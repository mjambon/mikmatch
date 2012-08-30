(*pp camlp4orf *)
(* $Id$ *)

open Printf

open Camlp4.PreCast

open Mm_util
open Regexp_ast
open Constants
open Select_lib

(* Regular expression to match *)
type regexp_info = 
    { re_loc : Ast.loc;
      re_name : string; (* identifier of the compiled regexp *)
      re_num : int;
      re_args : regexp_args;
      re_source : regexp_source; (* string representation of the
				    regexp, with possible gaps *)
      re_groups : 
	named_groups (* names of substrings *)
      * named_groups (* names of positions *);

      re_postbindings : (string * Ast.expr) list;
      re_anchored : bool }


(* Recorded view pattern *)
type view_info = { view_loc : Ast.loc;
		   view_unique_name : string;
		   view_name_loc : Ast.loc; (* location of full name *)
		   view_name : string; (* base name *)
		   view_path : string list; (* module path w/o base name *)
		   view_num : int; (* number of this view pattern *)
		   view_arg : Ast.patt option }

type special_info = [ `Regexp of regexp_info | `View of view_info ]


(* Sets of strings with location *)

let compare_loc_string (_, s1) (_, s2) = String.compare s1 s2

module Names =
  Set.Make (struct
	      type t = Ast.loc * string
	      let compare = compare_loc_string
	    end)

let add_new _loc set x =
  if Names.mem x set then
    Messages.multiple_binding _loc [(snd x)];
  Names.add x set

let add_if_needed _loc set x = Names.add x set


let output_match_ref = ref (fun _ -> assert false)

let unloc l = List.map snd l

let pattify _loc l =
  debug "pattify";
  match List.map (fun (_loc, s) -> <:patt< $lid:s$ >>) l with
      [] -> <:patt< () >>
    | [p] -> p
    | pl ->
	let tup =
	  List.fold_left (fun tup p -> <:patt< $tup$ , $p$ >> )
	  <:patt< >> pl
	in
	<:patt< ( $tup:tup$ ) >>


let exprify _loc l =
  debug "exprify";
  match List.map (fun (_loc, s) -> <:expr< $lid:s$ >>) l with
      [] -> <:expr< () >>
    | [e] -> e
    | el -> 
	let tup =
	  List.fold_left (fun tup e -> <:expr< $tup$ , $e$ >> )
	  <:expr< >> el
	in
	<:expr< ( $tup:tup$ ) >>


let check_same_ids _loc e1 e2 =
  if not (Names.equal e1 e2) then
    let diff_elements = 
      unloc 
	(Names.elements 
	   (Names.diff (Names.union e1 e2) (Names.inter e1 e2))) in
    Messages.unbalanced_bindings _loc diff_elements

let check_different_ids _loc e1 e2 =
  let inter = Names.inter e1 e2 in
  if not (Names.equal inter Names.empty) then
    Messages.multiple_binding _loc (unloc (Names.elements inter))

let get_all_names re =
  let _loc = re.re_loc in
  let get_names x = 
    Named_groups.fold 
      (fun group_name _ accu ->
	 add_new _loc accu (_loc, group_name))
      x
      Names.empty in

  let (groups, positions) = re.re_groups in
  let group_names = get_names groups
  and position_names = get_names positions in
  check_different_ids _loc group_names position_names;
  Names.union group_names position_names

let list_all_names re = 
  Names.elements (get_all_names re)


(* Subpatterns with info about their variables *)
type sub_patt = 
    { sub_patt : Ast.patt;
      sub_names : Names.t;
      sub_specials : special_info list;
      sub_alternatives : (string (* name of the target *)
			  * sub_patt) list }


(* Options *)
let lib = ref Select_lib.dummy
let tailrec = ref true


let has_prefix ~prefix s =
  String.length s >= String.length prefix &&
  String.sub s 0 (String.length prefix) = prefix

let match_suffix ~suffix s =
  let len = String.length s in
  let slen = String.length suffix in
  if len >= slen &&
    String.sub s (len - slen) slen = suffix then 
      String.sub s 0 (len - slen)
  else 
    invalid_arg "match_suffix"
  

let is_reserved s = has_prefix ~prefix:reserved_prefix s

let classify_id s =
  if has_prefix ~prefix:reserved_prefix s then
    if has_prefix ~prefix:regexp_prefix s then `Regexp
    else if has_prefix ~prefix:view_prefix s then `View
    else assert false
  else `Other

let regexp_of_var var_name =
  match_suffix ~suffix:"_target" var_name

let view_of_var var_name =
  match_suffix ~suffix:"_target" var_name

let var_of_regexp re_name =
  re_name ^ "_target"

let var_of_view unique_view_name =
  unique_view_name ^ "_target"

let forbidden_type = reserved_prefix ^ "syntax_error"

let posix_regexps =
  List.map 
    (fun (name, set) -> (name, Characters (dummy_loc, set)))
    Charset.Posix.all


module Mikmatch_regexps =
struct
  let _loc = dummy_loc
  let set x = Characters (_loc, x)
  let cset s = set (Charset.of_string s)
  let opt x = Repetition (_loc, (Option, true), x)
  let plus x = Repetition (_loc, (Plus, true), x)
  let star x = Repetition (_loc, (Star, true), x)
  let lr f l = 
    match List.rev l with
	[] -> Epsilon _loc
      | last :: rl ->
	  List.fold_left
	    (fun accu x -> f x accu)
	    last
	    rl
  let seq l = lr (fun x y -> Sequence (_loc, x, y)) l
  let alt l = lr (fun x y -> Alternative (_loc, x, y, S.empty, S.empty)) l
  let explode s = 
    let l = ref [] in
    for i = String.length s - 1 downto 0 do
      l := set (Charset.singleton s.[i]) :: !l
    done;
    !l
  let string s = seq (explode s)

  open Charset.Posix

  let opt_sign = opt (cset "-+")
  let d = set digit

(*
RE int = ["-+"]? ( "0" ( ["xX"] xdigit+
		       | ["oO"] ['0'-'7']+
		       | ["bB"] ["01"]+ )
                 | digit+ )
*)

  let int = 
    seq [
      opt_sign;
      (alt [ 
	 seq [
	   (cset "0");
	   (alt [ 
	      seq [ (cset "xX"); (plus (set xdigit)) ];
	      seq [ (cset "oO"); (plus (set (Charset.range '0' '7'))) ];
	      seq [ (cset "bB"); (plus (cset "01")) ];
	    ]);
	 ];
	 plus d;
       ])
    ]

(*
RE float = 
  ["-+"]? 
     ( ( digit+ ("." digit* )? | "." digit+ ) (["eE"] ["+-"]? digit+ )?
       | "nan"~ 
       | "inf"~ )
*)

  let float =
    seq [
      opt_sign;
      alt [
	seq [
	  alt [
	    seq [
              plus d;
	      opt (seq [ cset "."; star d ])
	    ];
	    seq [ cset "."; plus d ];
	  ];
	  opt 
	    (seq [
	       cset "eE";
	       opt (cset "+-");
	       plus d;
	     ])
	];
	nocase (string "nan");
	nocase (string "inf");
      ]
    ]

  let all = 
    [ "int", int;
      "float", float ]
end


let named_regexps =
  (Hashtbl.create 13 : (string, Regexp_ast.ast) Hashtbl.t)

let fill_tbl tbl l =
  List.iter 
    (fun (key, data) -> Hashtbl.add tbl key data)
    l

let init_named_regexps () =
  fill_tbl named_regexps posix_regexps;
  fill_tbl named_regexps Mikmatch_regexps.all;
  fill_tbl named_regexps !(lib).predefined_regexps

let reset_named_regexps () =
  Hashtbl.clear named_regexps;
  fill_tbl named_regexps posix_regexps;
  fill_tbl named_regexps Mikmatch_regexps.all;
  fill_tbl named_regexps !(lib).predefined_regexps

let select_lib x =
  lib := x;
  reset_named_regexps ()


let views = Hashtbl.create 100

(* Global table where we store information about regexps 
   for later insertion or reuse. *)
let regexp_table = Hashtbl.create 100

(* Function that will fetch the str_item that should be inserted before
   the current str_item when it contains a reference to a regexp. *)
let get_regexp_str_item _loc name =
  let opt_re = 
    try Some (Hashtbl.find regexp_table name)
    with Not_found -> None
  in
  match opt_re with
      None -> None
    | Some re ->
	let _loc = dummy_loc in
	let expr = 
	  let compile =
	    if re.re_anchored then (!lib).compile_regexp_match
	    else (!lib).compile_regexp_search 
	  in
	  compile _loc re.re_args re.re_source in
	let regexp_def = <:str_item< value $lid:name$ = $expr$ >> in
	let str_item =
	  List.fold_left (
	    fun si (ident, e) ->
	      <:str_item< $si$ ; value $lid:ident$ = $e$ >>
	  ) regexp_def re.re_postbindings
	in
	Some str_item


(* Registration *)
let _ = Global_def.init get_regexp_str_item



let add_compiled_regexp ~anchored postbindings
  _loc name num re_args re_source named_groups =
  Hashtbl.add regexp_table 
    name
    { re_loc = _loc;
      re_num = num;
      re_name = name;
      re_source = re_source;
      re_args = re_args;
      re_groups = named_groups;
      re_anchored = anchored;
      re_postbindings = postbindings }


let add_view _loc unique_name num (name_loc, name, full_name) arg =
  Hashtbl.add views unique_name { view_loc = _loc;
				  view_num = num;
				  view_unique_name = unique_name;
				  view_name = name;
				  view_name_loc = name_loc;
				  view_path = full_name;
				  view_arg = arg }


let find_compiled_regexp _loc name =
  try Hashtbl.find regexp_table name
  with Not_found -> 
    Loc.raise _loc 
      (Invalid_argument ("find_compiled_regexp: " ^ name))

let find_view _loc name =
  try Hashtbl.find views name
  with Not_found -> 
    Loc.raise _loc 
      (Invalid_argument ("find_view: " ^ name))



let bind_target ?(force_string = false) _loc target =
  match target with
      <:expr< ( $tup:tup$ ) >> ->
		 let el = list_of_comma_expr tup in
		 let ids = List.map (fun _ -> new_target ()) el in
		 let idl = List.map 
			     (fun s -> <:expr< $lid:s$ >>)
			     ids in
		 let target = 
		   <:expr< ( $tup:comma_expr_of_list _loc idl$ ) >> in
		 let make_target e =
		   List.fold_right2 
		     (fun x id e ->
			<:expr< let $lid:id$ = $x$ in $e$ >>)
		     el
		     ids
		     e in
		   
		 (make_target, target)
    | x ->
	let id = new_target () in
	let target = <:expr< $lid:id$ >> in
	let make_target = 
	  if force_string then 
	    fun e -> <:expr< let $lid:id$ = ($x$ : string) in $e$ >>
	  else 
	    fun e -> <:expr< let $lid:id$ = $x$ in $e$ >> in
	(make_target, target)
		     


let match_failure _loc =
  <:expr< match () with [] >>




let keep patt = 
  let subpatt = { sub_patt = patt;
		  sub_names = Names.empty;
		  sub_specials = [];
		  sub_alternatives = [] } in
  (Names.empty, false, `Normal, subpatt)

let sum_kind kind1 kind2 =
  if kind1 = `Special || kind2 = `Special then `Special
  else `Normal

let names_from_list names _loc l get =
  List.fold_right
    (fun x (set, has_re, kind, spatts, subpatts, res) ->
       let p = get x in
       let (local_set, local_has_re, local_kind, subpatt) = names p in
       check_different_ids _loc local_set set;
       (Names.union local_set set, 
	(has_re || local_has_re),
	sum_kind kind local_kind,
	subpatt.sub_patt :: spatts, 
	subpatt.sub_alternatives @ subpatts,
	subpatt.sub_specials @ res))
    l
    (Names.empty, false, `Normal, [], [], [])
    
let rec names patt =
  debug "Match.names";
  let _loc = Ast.loc_of_patt patt in
  match patt with
    | <:patt< ( $p$ : $lid:s$ ) >> when s = forbidden_type -> 
      recons_patt1 _loc p (fun p -> p)

    | <:patt< ( $p$ : $t$ ) >> -> 
      recons_patt1 _loc p (fun p -> <:patt< ( $p$ : $t$ ) >>)
     
    | <:patt< $lid:id$ >> ->
      (match classify_id id with
	   `Regexp -> 
	     let re_name = regexp_of_var id in
	     let re = find_compiled_regexp _loc re_name in
	     let set = get_all_names re in
	     let subpatt = { sub_patt = patt;
			     sub_names = set;
			     sub_specials = [`Regexp re];
			     sub_alternatives = [] } in
	     (set, true, `Special, subpatt)
	 | `View -> 
	     let view_name = view_of_var id in
	     let view = find_view _loc view_name in
	     let set =
	       match view.view_arg with
		   None -> Names.empty
		 | Some p -> 
		     let (set, _, _, _) = names p in
		     set in
	     let subpatt = { sub_patt = patt;
			     sub_names = set;
			     sub_specials = [`View view];
			     sub_alternatives = [] } in
	     (set, true, `Special, subpatt)
	 | `Other ->
	     let set = Names.singleton (_loc, id) in
	     let subpatt = { sub_patt = patt;
			     sub_names = set;
			     sub_specials = [];
			     sub_alternatives = [] } in
	     (set, false, `Normal, subpatt))
      
    | <:patt< $id: _ $ >> -> 
      (* Should be something like A.x or A.B.x (at least one period) *)
      keep patt

    | <:patt< $anti:s$ >> -> 
      Messages.failure _loc ("don't know what to do with antiquotation " ^ s)
      
    | <:patt< $p1$ .. $p2$ >> -> 
      (recons_patt2 _loc p1 p2 
	 (fun p1 p2 -> <:patt< $p1$ .. $p2$ >>))

    | <:patt< $p1$ $p2$ >> ->
      (recons_patt2 _loc p1 p2 
	 (fun p1 p2 -> <:patt< $p1$ $p2$ >>))

    | <:patt< lazy $p$ >> ->
      (recons_patt1 _loc p
	 (fun p -> <:patt< lazy $p$ >>))

    | <:patt< ( $p1$ as $p2$ ) >> ->
      (recons_patt2 _loc p1 p2 
	 (fun p1 p2 -> 
	    <:patt< ( $p1$ as $p2$ ) >>))


    | <:patt< $p1$ | $p2$ >> -> 
      let (set1, has_re1, kind1, subpatt1) = names p1 in
      let (set2, has_re2, kind2, subpatt2) = names p2 in
      check_same_ids _loc set1 set2;
      let subpatt =
	if kind1 = `Special || kind2 = `Special then
	  let var_name = new_subpatt () in
	  let spatt = <:patt< $lid:var_name$ >> in
	  { sub_patt = spatt;
	    sub_names = set1;
	    sub_specials = [];
	    sub_alternatives = [ var_name, subpatt1; 
				 var_name, subpatt2 ] }
	else
	  { sub_patt = <:patt< $subpatt1.sub_patt$ | $subpatt2.sub_patt$ >>;
	    sub_names = subpatt1.sub_names;
	    sub_specials = [];
	    sub_alternatives = 
	      subpatt1.sub_alternatives @ subpatt2.sub_alternatives } in
      (set1, (has_re1 || has_re2), `Special, subpatt)
	
    | <:patt< { $p$ } >> ->
      let ppl = list_of_record p in 
      let (set, has_re, kind, spatts, l, res) = 
	names_from_list names _loc ppl snd in
      let fields = 
	List.map2 (fun (p1, p2) spatt -> (p1, spatt)) ppl spatts in
      let record_body = record_of_list _loc fields in
      let subpatt = 
	{ sub_patt = <:patt< { $record_body$ } >>;
	  sub_names = set;
	  sub_specials = res;
	  sub_alternatives = l } in
      (set, has_re, kind, subpatt)
	
    | <:patt< [| $p$ |] >> -> 
      let pl = list_of_semicolon_patt p in
      let (set, has_re, kind, spatts, l, res) = 
	names_from_list names _loc pl (fun x -> x) in
      let subpatt = 
	{ sub_patt = <:patt< [| $list:spatts$ |] >>;
	  sub_names = set;
	  sub_specials = res;
	  sub_alternatives = l } in
      (set, has_re, kind, subpatt)

    | <:patt< ( $tup:p$ ) >> -> 
      let pl = list_of_comma_patt p in
      let (set, has_re, kind, spatts, l, res) = 
	names_from_list names _loc pl (fun x -> x) in
      let tuple_body = comma_patt_of_list _loc spatts in
      let subpatt = 
	{ sub_patt = <:patt< ( $tup:tuple_body$ ) >>;
	  sub_names = set;
	  sub_specials = res;
	  sub_alternatives = l } in
      (set, has_re, kind, subpatt)

    | <:patt< $chr:_$ >>
    | <:patt< $int:_$ >>
    | <:patt< $str:_$ >>
    | <:patt< $flo:_$ >>
    | <:patt< _ >> -> keep patt

    | Ast.PaNativeInt (_, _) 
    | Ast.PaInt64 (_, _) 
    | Ast.PaInt32 (_, _) -> keep patt

    | Ast.PaVrn (_, _) 
    | Ast.PaTyp (_, _) -> keep patt

    | <:patt< $_$ , $_$ >> (* tuple items *)
    | <:patt< $_$ ; $_$ >> (* record or array items *)
    | <:patt< $_$ = $_$ >> (* record field *)
    | <:patt< >>
    | Ast.PaMod _    (* (module M) *)
    | Ast.PaOlb _    (* ?s or ?s:(p) *)
    | Ast.PaOlbi _   (* ?s:(p = e) or ?(p = e) *)
    | Ast.PaLab _ -> (* label *)
	Messages.invalid_pattern _loc

and recons_patt1 _loc p1 f =
  let (set1, has_re1, kind1, 
       { sub_patt = spatt1; 
	 sub_specials = res1; 
	 sub_alternatives = l1 }) = names p1 in
  let spatt = f spatt1 in
  let subpatt = { sub_patt = spatt;
		  sub_names = set1;
		  sub_specials = res1;
		  sub_alternatives = l1 } in
  (set1, has_re1, kind1, subpatt)

and recons_patt2 _loc p1 p2 f =
  let (set1, has_re1, kind1, 
       { sub_patt = spatt1; 
	 sub_specials = res1; 
	 sub_alternatives = l1 }) = names p1 in
  let (set2, has_re2, kind2, 
       { sub_patt = spatt2; 
	 sub_specials = res2; 
	 sub_alternatives = l2 }) = names p2 in
  check_different_ids _loc set1 set2;
  let kind = sum_kind kind1 kind2 in
  let spatt = f spatt1 spatt2 in
  let set = Names.union set1 set2 in
  let subpatt = { sub_patt = spatt;
		  sub_names = set;
		  sub_specials = res1 @ res2;
		  sub_alternatives = l1 @ l2 } in
  (set, (has_re1 || has_re2), kind, subpatt)


let id_list set = 
  List.sort (fun (_, a) (_, b) -> String.compare a b) (Names.elements set)

let expr_id_list l =
  List.map (fun (_loc, s) -> <:expr< $lid:s$ >>) l

let patt_id_list l =
  List.map (fun (_loc, s) -> <:patt< $lid:s$ >>) l

let expr_is_lid = function
    <:expr< $lid:_$ >> -> true
  | _ -> false

let patt_is_lid = function
    <:patt< $lid:_$ >> -> true
  | _ -> false



let match_case_of_pwe _loc p w e = 
  let we =
    match w with
	None -> <:expr< >>
      | Some e -> e
  in
  <:match_case< $p$ when $we$ -> $e$ >>

let match_case_of_tuple _loc (p, w, e) = 
  match_case_of_pwe _loc p w e

let match_case_of_tuple4 (_loc, p, w, e) =
  match_case_of_pwe _loc p w e


let simplify_match _loc target l default = 
  (* target is an expr that should not compute anything *)
  (* user-defined but unused expressions are kept for the type checker *)
  (* default normally raises a match_failure or reraises an exception *)
  match l with
      [] -> default

    | [ <:patt< _ >>, (None | Some <:expr< True >>), e ] -> e
    | [ <:patt< $lid:id$ >>, (None | Some <:expr< True >>), e ]
	when expr_is_lid target ->
	<:expr< let $lid:id$ = $target$ in $e$ >>
    | _ ->
	let l' = l @ [ <:patt< _ >>, None, default ] in
	let match_cases = List.map (match_case_of_tuple _loc) l' in
	<:expr< match $target$ with [ $list: match_cases$ ] >>


let rec patt_succeeds patt = (* always?, catch_all? *)
  match patt with
      <:patt< _ >> -> (true, true)
    | <:patt< $lid:_$ >> -> (true, false)
    | <:patt< ( $tup:tup$ ) >> ->
      let l = list_of_comma_patt tup in
      List.fold_left
	(fun (works_always, is_catch_all) p ->
	   let always, catch_all = patt_succeeds p in
	   (works_always && always, is_catch_all && catch_all))
	(false, true)
	l
    | _ -> (false, false) (* actually we could test if it is catch_all *)

let match_one_case _loc target patt success failure =
  let always_succeeds, is_catch_all = patt_succeeds patt in
  if is_catch_all then (success, false)
  else
    if always_succeeds then
      (<:expr< let $patt$ = $target$ in $success$ >>, false)
    else
      (<:expr< match $target$ with 
	   [ $patt$ when True -> $success$
	   | _ -> $failure$ ] >>,
	   true)


let make_get_re _loc re_name re_args =
  List.fold_left 
    (fun e (_, arg) -> 
       let _loc = Ast.loc_of_expr arg in
       <:expr< $e$ $arg$ >>)
    <:expr< $lid:re_name$ >>
    re_args


(*
let re_match re_list (expr, expr_may_fail) = 
  let result =
    List.fold_right
      (fun { re_loc = _loc;
	     re_name = re_name;
	     re_args = re_args;
	     re_groups = named_groups } success ->
	 let var_name = var_of_regexp re_name in
	 let target = <:expr< $lid:var_name$ >> in
	 let failure = raise_exit _loc in
	 let get_re = make_get_re _loc re_name re_args in
	 (!lib).match_and_bind 
	   _loc re_name get_re target named_groups success failure)
      re_list
      expr in
  let may_fail = expr_may_fail || re_list <> [] in
  (result, may_fail)
*)

let re_match re success =
  let { re_loc = _loc;
	re_name = re_name;
	re_args = re_args;
	re_groups = named_groups } = re in
  let var_name = var_of_regexp re_name in
  let target = <:expr< $lid:var_name$ >> in
  let failure = raise_exit _loc in
  let get_re = make_get_re _loc re_name re_args in
  (!lib).match_and_bind 
    _loc re_name get_re target named_groups success failure

let view_match x success =
  let { view_loc = _loc;
	view_unique_name = view_unique_name;
	view_name = view_name;
	view_name_loc = vloc;
	view_path = view_path;
	view_arg = arg } = x in
  let view_fun = 
    let _loc = vloc in
    let base =
      <:ident< $lid:"view_" ^ view_name$ >> in
    let id =
      List.fold_right (fun s r ->
			 <:ident< $uid:s$ . $r$ >>)
	view_path base in
    <:expr< $id:id$ >>
  in
  let var_name = var_of_view view_unique_name in
  let failure = raise_exit _loc in
  match arg with
      None ->
	(let target = 
	   let _loc = vloc in
	   let alpha = new_type_var () in
	   <:expr< ( $view_fun$ : '$alpha$ -> bool ) $lid:var_name$ >> in
	 <:expr< if $target$ then $success$
    	         else $failure$ >>)
    | Some patt ->
	let target = 
	  let _loc = vloc in
	  let alpha = new_type_var () in
	  let beta = new_type_var () in
	  <:expr< ( $view_fun$ : '$alpha$ -> option '$beta$ ) 
	              $lid:var_name$ >> in
	let cases =
	  [ (_loc, <:patt< Some $patt$ >>, None, success);
	    (_loc, <:patt< _ >>, None, failure) ] in
	!output_match_ref _loc target cases
	  

let special_match specials (expr, expr_may_fail) =
  let result =
    List.fold_right
      (fun special success ->
	 match special with
	     `Regexp re -> re_match re success
	   | `View x -> view_match x success)
      specials
      expr in
  let may_fail = expr_may_fail || specials <> [] in
  (result, may_fail)


let expand_subpatt _loc l after_success =
  match l with
      [] ->
	(* This case never raises Exit *)
	(after_success, false)

    | (_, { sub_names = set }) :: _ ->
	let vars = id_list set in
	let subresult = 
	  match vars with
	      [] -> <:expr< () >>
	    | [_loc,s] -> <:expr< $id: <:ident< $lid:s$ >> $ >>
	    | _ -> 
		let tup = comma_expr_of_list _loc (expr_id_list vars) in
		<:expr< ( $tup:tup$ ) >> in
	let rec expand _loc l =
	  match l with
	      [] -> subresult, false
	    | _ ->
		match
		  List.fold_right
		    (fun (var, subpatt) rest -> 
		       let patt = subpatt.sub_patt in
		       let l = subpatt.sub_alternatives in
		       let re_list = subpatt.sub_specials in
		       let (success, may_fail1) = 
			 special_match re_list (expand _loc l) in
		       let failure = <:expr< raise $expr_exit _loc$ >> in
		       let target = <:expr< $lid:var$ >> in
		       let (match_expr, may_fail2) = 
			 match_one_case _loc
			   target patt success failure in
		       
		       let may_fail_here = may_fail1 || may_fail2 in

		       match rest with
			   None -> 
			     (* succeed or raise Exit, which is a failure *)
			     Some (match_expr, may_fail_here)

			 | Some (real_rest, rest_may_fail) ->
			     let may_fail = may_fail_here || rest_may_fail in
			     (* catch Exit and try the next alternative *)
			     if may_fail then (* CHECK: was may_fail_here *)
			       Some (<:expr<
				     try $match_expr$
				     with [ $patt_exit _loc$ -> $real_rest$ ]
				     >>, rest_may_fail)
			     else (* can't fail, other tests are ignored *) 
			       Some (match_expr, false))
		    l
		    None
		with 
		    None -> (subresult, false)
		  | Some (e, may_fail) -> (e, may_fail) in

	let result_expr, may_fail =
	  match vars with
	      [] -> 
		let expanded_expr, may_fail = expand _loc l in
		<:expr< do { $expanded_expr$; $after_success$ } >>, may_fail
            | _ ->
		let p = 
		  match vars with 
		      [_loc,s] -> <:patt< $lid:s$ >>
		    | _ -> 
			let tup =
			  comma_patt_of_list _loc (patt_id_list vars) in
			<:patt< ( $tup:tup$ ) >> in
		
		let expanded_expr, may_fail = expand _loc l in
		<:expr< let $p$ = $expanded_expr$ in $after_success$ >>,
		may_fail in

        (* This whole expression raises Exit if the test fails *)
        (result_expr, may_fail) (* was: true *)
	

let extract_re cases =
  List.fold_right
    (fun (_loc, patt, w, e) (has_re1, accu) ->
       let (all_names, has_re2, kind, subpatt) = names patt in
       let more_cases = accu <> [] in
       ((has_re1 || has_re2),
	(_loc, subpatt, w, e, has_re2, more_cases) ::
	  accu))
    cases
    (false, [])



let force_when patt when_opt =
  let _loc = Ast.loc_of_patt patt in
  match when_opt with
      None -> Some <:expr< True >>
    | _ -> when_opt

let output_special_match _loc target_expr cases_with_re default_action =
  debug "output_special_match";
  let wrap_all = !(lib).wrap_match in
  let wrap = !(lib).wrap_user_case in
  let really_wrap = !(lib).really_wrap_user_case in
  let (clo, app) =
    if !tailrec then 
      (fun e -> let _loc = Ast.loc_of_expr e in <:expr< fun [ () -> $e$ ] >>),
      (fun e -> let _loc = Ast.loc_of_expr e in <:expr< $e$ () >>)
    else
      (fun e -> e),
      (fun e -> e) in
  
  let (make_target, target) = bind_target _loc target_expr in
  let raise_exit = <:expr< raise $expr_exit _loc$ >> in
  let (cases_without_regexp, final_expr) = 
    List.fold_right 
      (fun (_, subpatt, when_opt, user_expr, has_re, more_cases) 
	   (cases_without_regexp, match_next) ->
	     let patt = subpatt.sub_patt in
	     let subpatts = subpatt.sub_alternatives in
	     match has_re, when_opt, subpatts with

		 false, (None | Some <:expr< True >>), [] -> 

		   (((patt, force_when patt when_opt, wrap (clo user_expr)) :: 
			 cases_without_regexp), 
		      match_next)

	       | false, Some guard, [] when not really_wrap ->

		   (((patt, force_when patt when_opt, clo user_expr) :: 
			 cases_without_regexp), 
		      match_next)

	       | _ ->
		   let e4, e4_may_fail =
		     match when_opt with
			 None -> clo user_expr, false
		       | Some cond -> 
			   <:expr<
			   if $cond$ then $clo user_expr$ 
			   else $raise_exit$ >>, true in
		   let e3 = (wrap e4, e4_may_fail) in
		   let (e2, may_fail1) =
		     special_match subpatt.sub_specials e3 in
		   let (e1, subpatt_may_fail) =
		     expand_subpatt _loc subpatts e2 in
		   let success = e1 in
		   let failure = raise_exit in
		   let (this_match, may_fail2) =
		     match_one_case _loc target patt success failure in
		   let rematch =
		     simplify_match 
		       _loc target cases_without_regexp match_next in
		   let e =
		     if may_fail1 || may_fail2 || subpatt_may_fail 
		     then <:expr< 
		       try $this_match$
		       with [ $patt_exit _loc$ -> $rematch$ ] 
		       >>
		     else this_match in
		   ([], e))
      cases_with_re
      ([], (wrap (clo default_action))) in
  
  let full_expr =
    simplify_match _loc target cases_without_regexp final_expr in
  make_target (app (wrap_all full_expr))



let output_match _loc target_expr cases =
  match extract_re cases with
      false, _ -> (* change nothing *)
	let match_cases = 
	  List.map match_case_of_tuple4 cases
	in
	<:expr< match $target_expr$ with [ $list:match_cases$ ] >>

    | true, cases_with_re -> 
	output_special_match _loc target_expr cases_with_re (match_failure _loc)
	
let _ = output_match_ref := output_match


let output_try _loc e cases =
  match extract_re cases with
      false, _ -> (* change nothing *)
	let match_cases = List.map match_case_of_tuple4 cases in
	<:expr< try $e$ with [ $list:match_cases$ ] >>
    | true, cases_with_re -> 
	let exn = <:expr< $lid:any_exn$ >> in
	let default_action = <:expr< raise $exn$ >> in
	<:expr<
	try $e$ 
	with [ $lid:any_exn$ -> 
	       $output_special_match _loc exn cases_with_re default_action$ ] >>

let output_function _loc cases =
  debug "output_function";
  match extract_re cases with
      false, _ -> (* change nothing *)
	let match_cases = List.map match_case_of_tuple4 cases in
	<:expr< fun [ $list:match_cases$ ] >>
    | true, cases_with_re -> 
	let target = <:expr< $lid:any_target$ >> in
	<:expr<
	fun $lid:any_target$ ->
	  $output_special_match _loc 
	  target cases_with_re (match_failure _loc)$ >>


let pp_named_groups _loc (groups, positions) =
  debug "pp_named_groups";
  let l1 = Named_groups.list groups in
  let l2 = Named_groups.list positions in
  let l = l1 @ l2 in
  List.fold_right
    (fun (name, il) e -> 
       let el = 
	 List.fold_right
	   (fun (_loc, i, conv) e -> 
	      <:expr< [ $int:string_of_int i$ :: $e$ ] >>) 
	   il
	   <:expr< [ ] >> in
       <:expr< [ ( $str:String.escaped name$, $el$ ) :: $e$ ] >>)
    l
    <:expr< [ ] >>
let find_named_regexp _loc name =
  try Hashtbl.find named_regexps name
  with Not_found ->
    Messages.failure _loc
      ("Unbound regular expression " ^ name)


let handle_regexp_patt _loc re =
  warnings re;
  let (num, re_name) = new_regexp () in
  let var_name = var_of_regexp re_name in
  let (re_args, re_source, named_groups, postbindings) = 
    (!lib).process_regexp _loc ~sharing:false re re_name in
  add_compiled_regexp ~anchored:true postbindings
    _loc re_name num re_args re_source named_groups;
  <:patt< ( $lid:var_name$ : $lid: forbidden_type$ ) >>

let handle_view_patt _loc x =
  let (num, unique_view_name) = new_view () in
  let (name, arg) = x in
  let var_name = var_of_view unique_view_name in
  add_view _loc unique_view_name num name arg;
  <:patt< ( $lid:var_name$ : $lid: forbidden_type$ ) >>

let handle_special_patt _loc = function
    `Regexp re -> handle_regexp_patt _loc re
  | `View x -> handle_view_patt _loc x


let gen_handle_let_bindings ?in_expr _loc is_rec l =
  let rec check_one_patt = function
      <:patt< ( $lid:s$ : $lid: t$ ) >> as p
	when is_reserved s || t = forbidden_type -> 
			     Messages.misplaced_pattern p
    | <:patt< ( $tup:tup$ ) >> ->
      let pl = list_of_comma_patt tup in
      check_patts pl
    | _ -> () 
  and check_patts l = List.iter check_one_patt l in

  match l with
      [] -> 
	(match in_expr with
	     None -> `Str_item <:str_item< >>
	   | Some e -> `Expr e)

    | [ (<:patt< ( $lid:s$ : $lid: t$ ) >> as p, e) ]
	when not is_rec && (is_reserved s ||
			    t = forbidden_type) ->
	(match classify_id s with
	     `View ->
	       Messages.failure (Ast.loc_of_patt p)
	         "Views are not supported in this kind of pattern"
	   | `Other -> assert false
	   | `Regexp -> ());
	let names = lazy (list_all_names 
			    (find_compiled_regexp _loc (regexp_of_var s))) in
	let e2 = 
	  match in_expr with
	      None -> exprify _loc (Lazy.force names)
	    | Some e2 -> e2 in

	let cases = [ (_loc, p, None, e2) ] in
	let expr = output_match _loc e cases in
	if in_expr = None then
	  let p = pattify _loc (Lazy.force names) in
	  `Str_item <:str_item< value $p$ = $expr$ >>
	else
	  `Expr expr

    | l ->
	(* RE patterns in function arguments are not checked! *)
	check_patts (List.map fst l);
	match in_expr with
	    None -> 
	      let bindings = List.map (binding_of_pair _loc) l in
	      `Str_item 
	        <:str_item< value $rec: rec_flag is_rec$ $list:bindings$ >>
	  | Some e2 -> 
	      let bindings = List.map (binding_of_pair _loc) l in
	      `Expr <:expr< let $rec: rec_flag is_rec$ $list:bindings$ in
	                    $e2$ >>

let handle_let_bindings _loc is_rec l e2 =
  match gen_handle_let_bindings ~in_expr:e2 _loc is_rec l with
      `Expr e -> e
    | `Str_item _ -> assert false

let handle_value_bindings _loc is_rec l =
  match gen_handle_let_bindings _loc is_rec l with
      `Expr _ -> assert false
    | `Str_item x -> x


let let_try_in _loc o l e2 pwel =
  let f = <:expr< fun () -> $e2$ >> in
  let e = handle_let_bindings _loc (o <> None) l f in
  <:expr< (try $e$ 
           with [ $list:pwel$ ]) () >>

let get_re_source ~quote_expr ~nocasify accu =
  let rec compact args strings pieces = function
      `String s :: l -> 
	compact args (if s <> "" then s :: strings else strings) pieces l
    | `Var (e, nocase) :: l -> 
	let _loc = Ast.loc_of_expr e in
	let name = Constants.new_var () in
	let tail = 
	  if strings = [] then pieces
	  else `String (String.concat "" (List.rev strings)) :: pieces in
	let gap_expr =
	  let e' = <:expr< $lid:name$ >> in
	  if nocase then nocasify e'
	  else <:expr< $quote_expr$ $e'$ >> in
	let new_args = (name, e) :: args in
	compact new_args [] (`Expr gap_expr :: tail) l
    | [] -> 
	let final_pieces = 
	  if strings = [] then pieces
	  else
	    let s = String.concat "" (List.rev strings) in
	    if s <> "" then `String s :: pieces
	    else pieces in
	(List.rev args, List.rev final_pieces) in

  compact [] [] [] (List.rev !accu)

let compute_re_string _loc re_source =
  let get_expr = function
      `String s -> <:expr< $str: String.escaped s$ >>
    | `Expr e -> e in
  match re_source with
      [] -> <:expr< "" >>
    | [x] -> get_expr x
    | [first; second] -> 
	<:expr< $get_expr first$ ^ $get_expr second$ >>
    | _ ->
	let l =
	  List.fold_right 
	    (fun x tail -> <:expr< [ $get_expr x$ :: $tail$ ] >>) 
	    re_source
	    <:expr< [] >> in
	<:expr< String.concat "" $l$ >>


let protect mt e =
  if mt then
    let _loc = Ast.loc_of_expr e in
    <:expr< do { Mutex.lock mutex; 
		 try 
		   let result = $e$ in
	           do { Mutex.unlock mutex;
			result }
                 with exn ->
                   do { Mutex.unlock mutex; 
			raise exn } } >>
  else e

let get_re_fragments _loc re_source =
  match re_source with
      [] -> <:expr< "" >>
    | [`String s] -> <:expr< $str:String.escaped s$ >>
    | _ -> 
	List.fold_right
	  (fun x e ->
	     match x with
		 `String s -> 
		   <:expr< [ $str:String.escaped s$ :: $e$ ] >>
	       | _ -> e)
	  re_source <:expr< [] >>
