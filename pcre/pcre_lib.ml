(*pp camlp4orf *)


open Printf

open Camlp4.PreCast

open Mm_util
open Regexp_ast

let _ =
  Constants.mod_runtime := "Run_mikmatch_pcre";
  Constants.mod_runtime_mt := "Run_mikmatch_pcre"

(* output for PCRE: Perl Compatible Regular Expressions *)

let special_regexps = 
  let _loc = Constants.dummy_loc in
  [ "any",   (* any character except newline *)
    Special (_loc, ".", ("any", Some 1));
    
    "bol",   (* beginning of line *)
    Special (_loc, "(?:^|(?<=\n))", ("bol", Some 0));

    "eol",   (* end of line *)
    Special (_loc, "(?:$|(?=\n))", ("eol", Some 0));

    "bos",   (* beginning of string *)
    Special (_loc, "^", ("bos", Some 0));
    
    "eos",   (* end of string *)
    Special (_loc, "$", ("eos", Some 0)); ]


let string c = String.make 1 c

let quote_char c =
  match c with 
      '\\' | '^' | '$' | '.' | '[' | ']' | '|' 
    | '(' | ')' | '?' | '*' | '+' | '{' | '}' -> 
	let s = String.create 2 in
	s.[0] <- '\\'; s.[1] <- c; s
    | '\000' -> "\\000"
    | c -> string c

let quote_char_in_class c =
  match c with
      '\\' -> "\\\\"
    | ']' -> "\\]"
    | '-' -> "\\-"
    | '^' -> "\\^"
    | '\000' -> "\\000"
    | c -> string c


let reorder_charset l =
  if l = [] then
    invalid_arg "reorder_charset";
  List.sort Char.compare l

let compact l =
  let finish first last =
    match Char.code last - Char.code first with
	0 -> quote_char_in_class first
      | 1 -> (quote_char_in_class first) ^ (quote_char_in_class last)
      | _ -> (quote_char_in_class first) ^ "-" ^ (quote_char_in_class last) in

  let rec extend first last = 
    function 
	[] -> [finish first last]
      | c :: rest -> 
	  if Char.code c = Char.code last + 1 then 
	    extend first c rest
	  else
	    finish first last :: extend c c rest in

  match l with
      [] -> []
    | hd :: tl -> extend hd hd tl


let compact_charset _loc l =
  let sorted = List.sort Char.compare l in
  let (zero, nozero) =
    match sorted with
	'\000' :: rest -> ("\\000", rest)
      | l -> ("", l) in
  String.concat "" (zero :: compact nozero)


let rec rm_closed = function Closed ast -> rm_closed ast | ast -> ast


let add_const accu s =
  accu := `String s :: !accu
let add_var accu e =
  accu := `Var (e, false) :: !accu
let add_var_nocase accu e =
  accu := `Var (e, true) :: !accu

let rec to_string ?(top = false) ((last_group, named_groups) as groups) accu = 
  function
      Epsilon _loc -> groups
    | Special (_loc, s, _) -> add_const accu s; groups
    | Characters (_loc, set) -> 
	let l = Charset.list set in
	(match l with
	     [] -> groups
	   | [c] ->
	       add_const accu (quote_char c); 
	       groups
	   | _ ->
	       add_const accu (sprintf "[%s]" (compact_charset _loc l));
	       groups)
	  
    | Sequence (_loc, re1, re2) ->
	let groups = to_string groups accu re1 in
	to_string groups accu re2
	  
    | Alternative (_loc, re, Epsilon _, _, _) ->

	let must_group = 
	  not top && 
	  match rm_closed re with
	      Characters _ | Special _ | Bind _ | Alternative _ -> false
	    | _ -> true in
	if must_group then add_const accu "(?:";
	let (last_group, named_groups) as groups = 
	  to_string (last_group, named_groups) accu re in
	if must_group then add_const accu ")";
	add_const accu "?";
	groups

    | Alternative (_loc, re1, re2, _, _) ->

	let must_group = not top in
	if must_group then add_const accu "(?:";
	let (last_group, named_groups1) = 
	  to_string (last_group, named_groups) accu re1 in
	add_const accu "|";

	let (last_group, named_groups2) = 
	  to_string (last_group, named_groups) accu re2 in
	if must_group then add_const accu ")";

	let check_balance set1 set2 =
	  if not (Named_groups.equal set1 set2) then
	    (let missing = 
	       S.diff 
		 (Named_groups.union set1 set2) 
		 (Named_groups.inter set1 set2) in
	     Messages.unbalanced_bindings _loc (list_named_groups missing)) in

	let (groups1, positions1) = named_groups1
	and (groups2, positions2) = named_groups2 in
	check_balance groups1 groups2;
	check_balance positions1 positions2;
	  
	(last_group, (merge groups1 groups2, merge positions1 positions2))
	  
    | Repetition (_loc, (kind, greedy), re) ->
	let must_group =
	  not top && 
	  match rm_closed re with
	      Characters _ | Special _ | Bind _ 
	    | Alternative _ | Possessive _ -> false
	    | _ -> true in
	if must_group then
	  add_const accu "(?:";
	let groups = to_string (last_group, named_groups) accu re in
	if must_group then
	  add_const accu ")";
	let rec convert = function
	    Star -> "*"
	  | Plus -> "+"
	  | Option -> "?"
	  | Range (m, None) -> 
	      (match m with
		   1 -> ""
		 | _ -> sprintf "{%i}" m)
	  | Range (m, Some None) ->
	      (match m with
		   0 -> "*"
		 | 1 -> "+"
		 | _ -> sprintf "{%i,}" m)
	  | Range (m, Some (Some n)) -> 
	      if m = n then 
		convert (Range (m, None))
	      else if m = 0 && n = 1 then "?"
	      else sprintf "{%i,%i}" m n in
	let base_op = convert kind in
	add_const accu base_op;
	if not greedy && base_op <> "" then 
	  add_const accu "?";
	groups
	  
    | Possessive (_loc, re) ->
	add_const accu "(?>";
	let groups = to_string groups accu re in
	add_const accu ")";
	groups

    | Lookahead (_loc, positive, re) ->
	let start = if positive then "(?=" else "(?!" in
	add_const accu start;
	let groups = to_string groups accu re in
	add_const accu ")";
	groups

    | Lookbehind (_loc, positive, re) ->
	let start = if positive then "(?<=" else "(?<!" in
	add_const accu start;
	let groups = to_string groups accu re in
	add_const accu ")";
	groups

    | Bind (_loc, re, name, conv) -> 
	let last_group = succ last_group in
	let named_groups = 
	  add_new_group _loc name conv last_group named_groups in
	add_const accu "(";
	let groups = to_string (last_group, named_groups) accu re in
	add_const accu ")";
	groups

    | Bind_pos (_loc, name) -> 
	let last_group = succ last_group in
	let named_groups = add_new_pos _loc name last_group named_groups in
	add_const accu "()";
	(last_group, named_groups)

    | Backref (_loc, name) ->
	(try
	   match Named_groups.find name (fst named_groups) with
	       [] -> Messages.invalid_backref _loc name
	     | [(_, n, conv)] -> add_const accu (sprintf "\\%i" n); groups
	     | l -> 
		 add_const accu (sprintf "(?:%s)"
		   (String.concat "|"
		      (List.map (fun (_, n, conv) -> sprintf "\\%d" n) l)));
		 groups
	 with Not_found -> Messages.invalid_backref _loc name)

    | Variable (_loc, e) -> add_var accu e; groups
    | Nocase_variable (_loc, e) -> add_var_nocase accu e; groups
	    
    | Closed ast -> 
	let saved_named_groups = named_groups in
	let (last_group, named_groups) = to_string groups accu ast in
	(last_group, saved_named_groups)


(* Syntax expanders *)

open Constants

let nocasify e =
  let _loc = Ast.loc_of_expr e in
  <:expr< $uid: !mod_runtime$.nocase $e$ >>


let make_get_re_noargs _loc re_name re_args =
  let empty = <:expr< "" >> in
  let empty_args = List.map (fun (name, arg) -> (name, empty)) re_args in
  Match.make_get_re _loc re_name empty_args


let process_regexp _loc ~sharing re re_name =
  let accu = ref [] in
  let (last_group, named_groups) = 
    to_string ~top:true (0, (Named_groups.empty, Named_groups.empty)) 
      accu re in
  let re_args, re_source = 
    Match.get_re_source 
      ~quote_expr: <:expr< $uid: !mod_runtime$.quote_string >> 
      ~nocasify accu in
  let shared_id = shared re_name in
  let get_re_noargs = make_get_re_noargs _loc re_name re_args in
  let postbindings =
    if sharing then
      [ shared_id, <:expr< Pcre.make_ovector $get_re_noargs$ >>;
	subgroups2 re_name, <:expr< fst $lid:shared_id$ >>;
	shared_ovector re_name, <:expr< snd $lid:shared_id$ >> ]
    else [] in
  (re_args, re_source, named_groups, postbindings)


let raises_exn = function
    <:expr< raise $exn$ >> -> true
  | _ -> false

let string_match _loc re_name get_re target substrings pos success failure =
  let match_it = 
    <:expr< Pcre.exec ~rex:$get_re$ ~pos:$int:string_of_int pos$ $target$ >> in

  if raises_exn failure then (* shortcut *)
    <:expr< 
    let $lid:substrings$ = 
      try $match_it$
      with [ Not_found -> $failure$ ] in 
    $success$
    >>

  else
    <:expr<
    try
      let $lid:substrings$ = 
	try $match_it$
	with [ Not_found -> $raise_exit _loc$ ] in 
      $success$
    with
	[ $patt_exit _loc$ -> $failure$ ] >>


let matched_group _loc substrings n =
  <:expr< Pcre.get_substring $lid:substrings$ $int:string_of_int n$ >>

let matched_position _loc substrings n =
  <:expr< Pcre.get_substring_ofs $lid:substrings$ $int:string_of_int n$ >>



let compile_regexp_general ~anchored ~mt _loc re_args re_source =
  let default_flags = <:expr< [`DOLLAR_ENDONLY] >> in
  let anchored_flags = <:expr< [`ANCHORED; `DOLLAR_ENDONLY] >> in
  let flags = if anchored then anchored_flags else default_flags in
  let compile_string e =
    <:expr< Pcre.regexp ~flags:$flags$ $e$ >> in
  match re_args with
      [] -> 
	let re_string = Match.compute_re_string _loc re_source in
	compile_string re_string
    | _ ->
	let key = 
	  match re_args with
	      [name, _] -> <:expr< $id: <:ident< $lid:name$ >> $ >>
	    | _ -> 
		let expr_list = 
		  List.map (
		    fun (name, _) -> 
		      <:expr< $id: <:ident< $lid:name$ >> $ >>
		  ) re_args in
		let tup = comma_expr_of_list _loc expr_list in
		<:expr< ( $tup: tup$ ) >> in
	let compile =
	  let re_string = Match.compute_re_string _loc re_source in
	  compile_string re_string in
	    
	let find = 
	  Match.protect mt 
	  <:expr< $uid: !mod_runtime$.Mem.find tbl key >> in
	let add =
	  Match.protect mt 
	  <:expr< $uid: !mod_runtime$.Mem.unsafe_add tbl key data >> in

	let check_cache =
	  <:expr< 
	  let key = $key$ in
	  try $find$
	  with [ Not_found -> 
		   let data = $compile$ in
		   do { $add$;
			data } ] >> in

	let get_regexp =
	  List.fold_right 
	    (fun (argname, _) e -> <:expr< fun $lid:argname$ -> $e$ >>)
	    re_args
	    check_cache in

	let result =
	  <:expr< 
	  let tbl = $uid: !mod_runtime$.Mem.create 100 in 
	  $get_regexp$ >> in

	if mt then <:expr< let mutex = Mutex.create () in $result$ >>
	else result


let compile_regexp_match = compile_regexp_general ~anchored:true
let compile_regexp_search = compile_regexp_general ~anchored:false

let convert _loc conv e =
  match conv with
      None -> e
    | Some f ->
	match f with
	    `Int -> <:expr< Pervasives.int_of_string $e$ >>
	  | `Float -> <:expr< Pervasives.float_of_string $e$ >>
	  | `Option -> <:expr< let s = $e$ in 
	                       if s = "" then None 
			       else Some s >>
	  | `Custom f -> <:expr< $f$ $e$ >>
	  | `Value e' -> <:expr< $e'$ >>
    
let insert_bindings_poly 
  ?(skip_empty_captures = false) (* for compatibility with 
				    old versions of Pcre (before 2004-04-29) *)
  ?(get_fst = false)
  make_expr _loc substrings set e =
  Named_groups.fold
    (fun name l e -> 
       match l with
	   [] -> assert false
	 | (_loc, _, _) :: _ ->
	     let find_it =
	       List.fold_right 
		 (fun (_loc, n, conv) accu ->
		    let expr = make_expr _loc substrings n in
		    match accu with
			None -> Some (convert _loc conv expr)
		      | Some e ->
			  let result =
			    if skip_empty_captures then
			      <:expr<
			      try
				match $expr$ with 
				    [ "" -> raise Not_found
				    | s -> $convert _loc conv <:expr< s >>$ ]
			      with [ Not_found -> $e$ ] >>
			    else
			      <:expr< 
			      try $convert _loc conv expr$
			      with [ Not_found -> $e$ ] >> in
			  Some result)
		 l
		 None in
	     let result =
	       match find_it with
		   None -> assert false 
		 | Some e -> e in
	     let patt = 
	       if get_fst then <:patt< ( $lid:name$, _ ) >>
	       else <:patt< $lid:name$ >> in
	     <:expr< let $patt$ = $result$ in $e$ >>)
    set
    e
    
let insert_group_bindings = 
  insert_bindings_poly ~skip_empty_captures:true matched_group
let insert_position_bindings = 
  insert_bindings_poly ~get_fst:true matched_position

let insert_bindings _loc substrings (group_bindings, position_bindings) e =
  insert_group_bindings _loc substrings group_bindings 
    (insert_position_bindings _loc substrings position_bindings e)


let substrings_of_target target =
  match target with
      <:expr< $lid:s$ >> -> s ^ "_result"
    | _ -> assert false

let match_and_bind _loc re_name get_re target named_groups success failure =
  let substrings = substrings_of_target target in
  string_match _loc re_name get_re target substrings 0 
    (insert_bindings _loc substrings named_groups success)
    failure

let macro_replace_generic 
  f _loc (re_name : string) get_re target_name named_groups expr =
  let target = <:expr< $lid:target_name$ >> in
  let substrings = substrings_of_target target in
  <:expr<
  fun ?pos $lid:target_name$ ->
    Pcre.$lid: f$
    ~rex:$get_re$ 
    ?pos
    ~subst:(fun $lid:substrings$ -> 
	      $insert_bindings _loc substrings named_groups expr$)
    $target$ >>

let macro_replace = macro_replace_generic "substitute_substrings"
let macro_replace_first = macro_replace_generic "substitute_substrings_first"

let macro_match ?(ignore_bindings = false)
  _loc re_name get_re target_name named_groups expr =
  let target = <:expr< $lid:target_name$ >> in
  let substrings = substrings_of_target target in
  let sv = shared_ovector re_name in
  let sg2 = subgroups2 re_name in
  let result =
    if ignore_bindings then expr
    else insert_bindings _loc substrings named_groups expr in
  <:expr<
  fun ?(share = False) ?pos $lid:target_name$ ->
    let $lid:substrings$ =
      if not share then
	Pcre.exec ~rex:$get_re$ ?pos $target$
      else
	(Obj.magic 
	   ($target$,
	    do { Pcre.unsafe_pcre_exec 
		   (Obj.magic 0 : Pcre.irflag) 
		   $get_re$ (match pos with [ None -> 0 | Some n -> n]) 
		   $target$ $lid:sg2$
		   $lid:sv$ None;
		 $lid:sv$ }) : Pcre.substrings) in
    $result$ >>

let macro_search_first = macro_match

let macro_filter _loc re_name get_re target_name named_groups _ =
  let expr = <:expr< True >> in
  let e = 
    macro_match ~ignore_bindings:true
      _loc re_name get_re target_name named_groups expr in
  <:expr< fun ?share ?pos x -> try $e$ ?share ?pos x 
                               with [ Not_found -> False ] >>

let make_object _loc (group_bindings, position_bindings) =
  let all_ids = 
    Named_groups.list_keys group_bindings @ 
    Named_groups.list_keys position_bindings in
  let methods =
    List.fold_right
      (fun id accu -> <:class_str_item< method $id$ = $lid:id$ ; $accu$ >>)
      all_ids <:class_str_item< >>in
  <:expr< object $methods$ end >>

let macro_capture _loc re_name get_re target_name named_groups _ =
  let expr = make_object _loc named_groups in
  let e = macro_match _loc re_name get_re target_name named_groups expr in
  <:expr< fun ?share ?pos x -> try Some ($e$ ?share ?pos x)
                               with [ Not_found -> None ] >>

let macro_function 
  fun_name _loc (re_name : string) get_re target_name named_groups expr =
  let target = <:expr< $lid:target_name$ >> in
  let substrings = substrings_of_target target in
  <:expr<
  $uid: !mod_runtime$.$lid:fun_name$
    $get_re$
    (fun $lid:substrings$ -> 
       $insert_bindings _loc substrings named_groups expr$)
  >>


let macro_search = macro_function "search"
let macro_map = macro_function "map"
let macro_collect = macro_function "collect"

let macro_collectobj _loc re_name get_re target_name named_groups _ = 
  let e = make_object _loc named_groups in
  macro_function "collect" _loc re_name get_re target_name named_groups e

let macro_split _loc re_name get_re target_name named_groups _ =
  <:expr< $uid: !mod_runtime$.split $get_re$ >>

open Select_lib

let lib = { predefined_regexps = special_regexps;
	    unfold_range = false;
	    process_regexp = process_regexp;
	    compile_regexp_match = compile_regexp_match ~mt:false;
	    compile_regexp_search = compile_regexp_search ~mt:false;
	    match_and_bind = match_and_bind;
	    wrap_match = (fun e -> e);
	    wrap_user_case = (fun e -> e);
	    really_wrap_match = false;
	    really_wrap_user_case = false }

let lib_mt = { lib with 
		 compile_regexp_match = compile_regexp_match ~mt:true;
		 compile_regexp_search = compile_regexp_search ~mt:true }
