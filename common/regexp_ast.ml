(* Abstract syntax tree for regular expressions *)

type converter = [ `Int 
		 | `Float
		 | `Option
		 | `Custom of MLast.expr 
		 | `Value of MLast.expr ]

module S = Set.Make (String)

let list_named_groups set = List.sort String.compare (S.elements set)

module Named_groups = 
struct
  module M = Map.Make (String)
  include M
  let list m =
    List.sort 
      (fun (a, _) (b, _) -> String.compare a b)
      (fold 
	 (fun key data accu -> 
	    let positions = 
	      List.sort
		(fun (loc, i, conv1) (loc, j, conv2) -> 
		   Pervasives.compare i j)
		data in
	    (key, positions) :: accu)
	 m [])

  let list_keys m = 
    List.sort String.compare (fold (fun key data accu -> key :: accu) m [])

  let keys m = fold (fun key data accu -> S.add key accu) m S.empty
  let equal m1 m2 = S.equal (keys m1) (keys m2)
  let inter m1 m2 = S.inter (keys m1) (keys m2)
  let union m1 m2 = S.union (keys m1) (keys m2)
  let diff m1 m2 = S.diff (keys m1) (keys m2)
end

type named_groups = (MLast.loc * int * converter option) list Named_groups.t


let add_new loc name conv group_num set =
  if Named_groups.mem name set then
    Messages.multiple_binding loc [name];
  Named_groups.add name [loc, group_num, conv] set

let add_new_group loc name conv group_num (groups, positions) =
  (add_new loc name conv group_num groups, positions)

let add_new_pos loc name group_num (groups, positions) =
  (groups, add_new loc name None group_num positions)

let merge_lists l1 l2 =
  let tbl = Hashtbl.create (List.length l1 + List.length l2) in
  let add l = 
    List.iter (fun ((_, n, conv) as x) -> Hashtbl.replace tbl n x) l in
  add l1;
  add l2;
  let l = Hashtbl.fold (fun _ x l -> x :: l) tbl [] in
  let cmp (_, x, _) (_, y, _) = compare x y in
  List.sort cmp l

let really_add name l2 set =
  try
    let l1 = Named_groups.find name set in
    Named_groups.add 
      name (merge_lists l1 l2)
      (Named_groups.remove name set)
  with Not_found ->
    Named_groups.add name l2 set

let merge set1 set2 =
  Named_groups.fold really_add set1 set2

type repetition_kind = 
    Star 
  | Option 
  | Plus
  | Range of (int * int option option)

type greediness = bool

type ast =
    Epsilon of MLast.loc
  | Characters of MLast.loc * Charset.t
  | Sequence of MLast.loc * ast * ast
  | Alternative of MLast.loc 
      * ast (* choice 1 *)
      * ast (* choice 2 *)
      * S.t (* group names *) 
      * S.t (* position names *)
  | Repetition of MLast.loc * (repetition_kind * greediness) * ast
  | Possessive of MLast.loc * ast
  | Bind of MLast.loc * ast * string * converter option
  | Bind_pos of MLast.loc * string
  | Backref of MLast.loc * string
  | Variable of MLast.loc * MLast.expr
  | Nocase_variable of MLast.loc * MLast.expr
  | Special of MLast.loc * string * (string * int option)
  | Lookahead of MLast.loc * bool * ast
  | Lookbehind of MLast.loc * bool * ast
  | Closed of ast

let rec loc_of_regexp = function
    Epsilon loc
  | Characters (loc, _)
  | Sequence (loc, _, _)
  | Alternative (loc, _, _, _, _)
  | Repetition (loc, _, _)
  | Possessive (loc, _)
  | Bind (loc, _, _, _)
  | Bind_pos (loc, _)
  | Backref (loc, _)
  | Variable (loc, _)
  | Nocase_variable (loc, _)
  | Special (loc, _, _)
  | Lookahead (loc, _, _)
  | Lookbehind (loc, _, _) -> loc
  | Closed ast -> loc_of_regexp ast

let rec bindings : ast -> S.t = function
    Bind (loc, e, s, conv) -> S.add s (bindings e)
  | Bind_pos _
  | Epsilon _
  | Characters _
  | Backref _
  | Variable _
  | Nocase_variable _
  | Special _ -> S.empty
  | Sequence (loc, e1, e2) -> S.union (bindings e1) (bindings e2)
  | Alternative (loc, e1, e2, set, pos_set) -> set
  | Repetition (loc, kind, e) -> bindings e
  | Possessive (loc, e)
  | Lookahead (loc, _, e)
  | Lookbehind (loc, _, e) -> bindings e
  | Closed e -> S.empty

let rec pos_bindings : ast -> S.t = function
    Bind_pos (loc, s) -> S.singleton s
  | Bind _
  | Epsilon _
  | Characters _
  | Backref _
  | Variable _
  | Nocase_variable _
  | Special _ -> S.empty
  | Sequence (loc, e1, e2) -> S.union (pos_bindings e1) (pos_bindings e2)
  | Alternative (loc, e1, e2, set, pos_set) -> pos_set
  | Repetition (loc, kind, e) -> pos_bindings e
  | Possessive (loc, e)
  | Lookahead (loc, _, e) 
  | Lookbehind (loc, _, e) -> pos_bindings e
  | Closed _ -> S.empty



let alternative loc e1 e2 =
  match e1, e2 with
      Characters (loc1, s1), Characters (loc2, s2) -> 
	Characters (loc, Charset.union s1 s2)
    | _ ->
	let b1 = bindings e1
	and b2 = bindings e2 in
	let pb1 = pos_bindings e1 
	and pb2 = pos_bindings e2 in
	Alternative (loc, e1, e2, S.union b1 b2, S.union pb1 pb2)

let rec repeat loc e (mini, maxoptopt) =
  if mini < 0 then
    Messages.invalid_range loc
  else
    match maxoptopt with
	None -> 
	  (match mini with
	       0 -> Epsilon loc
	     | n -> 
		 let rec loop i =
		   if i > 1 then
		     Sequence (loc, e, loop (i-1))
		   else e in
		 loop n)
      | Some (Some maxi) ->
	  let diff = maxi - mini in
	  if diff < 0 then Messages.invalid_range loc
	  else if diff = 0 then e
	  else 
	    let rec loop i =
	      alternative loc (Epsilon loc) 
		(if i > 1 then
		   (Sequence (loc, e, loop (i-1)))
		 else e) in
	    Sequence (loc, (repeat loc e (mini, None)), loop diff)
      | Some None ->
	  Sequence (loc, repeat loc e (mini, None), 
		    Repetition (loc, (Star, true), e))


let rec nocase = function
    Bind (loc, e, s, conv) -> Bind (loc, nocase e, s, conv)
  | Bind_pos _
  | Epsilon _
  | Backref _
  | Nocase_variable _
  | Special _ as e -> e
  | Characters (loc, charset) -> Characters (loc, Charset.nocase charset)
  | Sequence (loc, e1, e2) -> Sequence (loc, nocase e1, nocase e2)
  | Alternative (loc, e1, e2, ids, pos_ids) -> 
     Alternative (loc, nocase e1, nocase e2, ids, pos_ids)
  | Repetition (loc, kind, e) -> Repetition (loc, kind, nocase e)
  | Possessive (loc, e) -> Possessive (loc, nocase e)
  | Lookahead (loc, b, e) -> Lookahead (loc, b, nocase e)
  | Lookbehind (loc, b, e) -> Lookbehind (loc, b, nocase e)
  | Variable (loc, e) -> Nocase_variable (loc, e)
  | Closed ast -> Closed (nocase ast)


(* Miscellaneous functions *)

let explode s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := s.[i] :: !l
  done;
  !l

let of_string loc s =
  let l = explode s in
  match l with
      [c] -> Characters (loc, Charset.singleton c)
    | _ ->
	List.fold_right 
	  (fun c re -> 
	     Sequence (loc, (Characters (loc, Charset.singleton c)), re))
	  l (Epsilon loc)

let as_charset loc msg = function
    Characters (loc, set) -> set
  | _ -> Stdpp.raise_with_loc loc (Failure msg)

let rec warn_bindings w = function
    Bind (loc, e, s, conv) ->
      if w then Messages.not_visible loc [s] "context";
      warn_bindings w e
  | Bind_pos (loc, s) -> if w then Messages.not_visible loc [s] "context"
  | Epsilon _
  | Characters _
  | Backref _
  | Variable _
  | Nocase_variable _
  | Special _ -> ()
  | Sequence (loc, e1, e2) -> warn_bindings w e1; warn_bindings w e2
  | Alternative (loc, e1, e2, set, pos_set) -> 
      if w then
	(match list_named_groups (S.union set pos_set) with
	     [] -> ()
	   | ignored -> Messages.not_visible loc ignored "context")
  | Repetition (loc, kind, e) -> warn_bindings w e
  | Possessive (loc, e)
  | Lookahead (loc, _, e)
  | Lookbehind (loc, _, e) -> warn_bindings w e
  | Closed e -> warn_bindings true e

let warnings re =
  warn_bindings false re

