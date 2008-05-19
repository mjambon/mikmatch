(*pp camlp4orf *)
(* $Id$ *)

open Camlp4.PreCast

open Messages

(* General Camlp4 utilities *)

let list_of_comma_expr e =
  let rec aux e l =
    match e with
	<:expr< $e1$ , $e2$ >> -> aux e1 (aux e2 l)
      | <:expr< >> -> []
      | e -> [e]
  in
  aux e []

let list_of_comma_patt p =
  let rec aux p l =
    match p with
	<:patt< $p1$ , $p2$ >> -> aux p1 (aux p2 l)
      | <:patt< >> -> []
      | p -> [p]
  in
  aux p []

let list_of_semicolon_patt p =
  let rec aux p l =
    match p with
	<:patt< $p1$ ; $p2$ >> -> aux p1 (aux p2 l)
      | <:patt< >> -> []
      | p -> [p]
  in
  aux p []

let list_of_record p =
  List.map (
    function
	<:patt< $p1$ = $p2$ >> -> (p1, p2)
      | _ -> assert false
  ) (list_of_semicolon_patt p)


let comma_expr_of_list _loc l =
  List.fold_left (
    fun accu e -> <:expr< $accu$ , $e$ >>
  ) <:expr< >> l

let comma_patt_of_list _loc l =
  List.fold_left (
    fun accu p -> <:patt< $accu$ , $p$ >>
  ) <:patt< >> l

let semicolon_patt_of_list _loc l =
  List.fold_left (
    fun accu p -> <:patt< $accu$ ; $p$ >>
  ) <:patt< >> l

let record_of_list _loc l =
  semicolon_patt_of_list _loc
    (List.map (fun (p1, p2) -> <:patt< $p1$ = $p2$ >>) l)

let meta_bool = function
    true -> Ast.BTrue 
  | false -> Ast.BFalse

let binding_of_pair _loc (p, e) =
  <:binding< $p$ = $e$ >>

let pair_of_binding = function
    <:binding< $p$ = $e$ >> -> (p, e)
  | b -> 
      let _loc = Ast.loc_of_binding b in
      failure _loc "Failed assertion in Mm_util.pair_of_binding"


let match_case_of_tuple _loc (p, w, e) =
  match w with
      None -> <:match_case< $p$ -> $e$ >>
    | Some cond -> <:match_case< $p$ when $cond$ -> $e$ >>


let eval_string s = Camlp4.Struct.Token.Eval.string ~strict:() s

let eval_char s = Camlp4.Struct.Token.Eval.char s

