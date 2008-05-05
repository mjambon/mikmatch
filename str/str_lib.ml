(*pp camlp4o q_MLast.cmo -loc loc *)

open Constants

let _ =
  mod_runtime := "Run_micmatch_str";
  mod_runtime_mt := "Run_micmatch_str_mt"

let str_mutex = "str_mutex"


(* Emacs/Str syntax for regular expressions *)

open Printf

open Regexp_ast

let special_regexps = 
  let loc = Constants.dummy_loc in
  [ "bol", Special (loc, "^", ("bol", Some 0)); (* beginning of line *)
    "eol", Special (loc, "$", ("eol", Some 0)); (* end of line *)
    "bnd", Special (loc, "\\b", ("bnd", Some 0)); (* word boundary *)
    "any", Special (loc, ".", ("any", Some 1)); (* any character 
						   except newline *) ]

(*
  Note that the usual regexp special characters are not special inside
  a character set. A completely different set of special characters exists
  inside character sets: `]', `-' and `^'.

  To include a `]' in a character set, you must make it the first
  character. For example, `[]a]' matches `]' or `a'. To include a `-',
  write `-' as the first or last character of the set, or put it after
  a range. Thus, `[]-]' matches both `]' and `-'. 

  To include `^', make it other than the first character in the set.
*)

let string c = String.make 1 c

let quote_char = function
    '[' | ']' | '*' | '.' | '\\' | '?' | '+' | '^' | '$' as c ->
      let s = String.create 2 in
      s.[0] <- '\\'; s.[1] <- c; s
  | c -> string c


let reorder_charset l =
  if l = [] then
    invalid_arg "reorder_charset";
  List.sort 
    (fun c1 c2 ->
       if c1 = c2 then invalid_arg "reorder_charset: repeated char"
       else if c1 = ']' then -1
       else if c2 = ']' then 1
       else if c1 = '-' then 1
       else if c2 = '-' then -1
       else if c1 = '^' then 1
       else if c2 = '^' then -1
       else Char.compare c1 c2)
    l

let compact l =
  let finish first last =
    match Char.code last - Char.code first with
	0 -> string first
      | 1 -> string first ^ string last
      | _ -> string first ^ "-" ^ string last in

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


let compact_charset l =
  let rbracket = ref false
  and dash = ref false
  and caret = ref false in
  let normal = 
    List.filter (function
		     ']' -> rbracket := true; false
		   | '-' -> dash := true; false
		   | '^' -> caret := true; false
		   | _ -> true) l in
  let sorted = List.sort Char.compare normal in
  let special_tail = 
    let tail = if !dash then ["-"] else [] in
    if !caret then "^" :: tail else tail in
  let tail = compact sorted @ special_tail in
  if !rbracket then "]" :: tail else tail


let add_const accu s =
  accu := `String s :: !accu
let add_var accu e =
  accu := `Var (e, false) :: !accu
let add_var_nocase accu e =
  accu := `Var (e, true) :: !accu

let rec rm_closed = function Closed ast -> rm_closed ast | ast -> ast

let rec to_string ?(top = false) ((last_group, named_groups) as groups) accu = 
  function
      Epsilon loc -> groups
    | Special (loc, s, _) -> add_const accu s; groups
    | Characters (loc, set) -> 
	let l = Charset.list set in
	(match l with
	     [] -> groups
	   | [c] ->
	       add_const accu (quote_char c); 
	       groups
	   | _ ->
	       add_const accu "[";
	       List.iter (add_const accu) (compact_charset l);
	       add_const accu "]";
	       groups)
	  
    | Sequence (loc, re1, re2) ->
	let groups = to_string groups accu re1 in
	to_string groups accu re2
	  
    | Alternative (loc, re, Epsilon _, _, _) ->

	let must_group = 
	  not top && 
	  match rm_closed re with
	      Characters _ | Special _ | Bind _ | Alternative _ -> false
	    | _ -> true in
	let last_group = if must_group then succ last_group else last_group in
	if must_group then add_const accu "\\(";
	let (last_group, named_groups) as groups = 
	  to_string (last_group, named_groups) accu re in
	if must_group then add_const accu "\\)";
	add_const accu "?";
	groups

    | Alternative (loc, re1, re2, _, _) ->

	let must_group = not top in
	let last_group = if must_group then succ last_group else last_group in
	if must_group then add_const accu "\\(";
	let (last_group, named_groups1) = 
	  to_string (last_group, named_groups) accu re1 in
	add_const accu "\\|";

	let (last_group, named_groups2) = 
	  to_string (last_group, named_groups) accu re2 in
	if must_group then add_const accu "\\)";

	let check_balance set1 set2 =
	  if not (Named_groups.equal set1 set2) then
	    (let missing = 
	       S.diff 
		 (Named_groups.union set1 set2) 
		 (Named_groups.inter set1 set2) in
	     Messages.unbalanced_bindings loc (list_named_groups missing)) in

	let (groups1, positions1) = named_groups1
	and (groups2, positions2) = named_groups2 in
	check_balance groups1 groups2;
	check_balance positions1 positions2;	

	(last_group, (merge groups1 groups2, merge positions1 positions2))
	  
    | Repetition (loc, (Star, true), 
		  (Repetition (_, (Star, true), _) as re)) -> 
	to_string ~top groups accu re

    | Repetition (loc, kind, re) ->
	let must_group =
	  not top && 
	  match rm_closed re with
	      Characters _ | Special _ | Bind _ | Alternative _ -> false
	    | _ -> true in
	let last_group =
	  if must_group then
	    (add_const accu "\\(";
	     succ last_group)
	  else last_group in
	let groups = to_string (last_group, named_groups) accu re in
	if must_group then
	  add_const accu "\\)";
	let op = 
	  match kind with
	      (Star, true) -> "*"
	    | (Plus, true) -> "+"
	    | (Option, true) -> "?"
	    | _ -> assert false in
	add_const accu op;
	groups

    | Bind (loc, re, name, conv) -> 
	let last_group = succ last_group in
	let named_groups = 
	  add_new_group loc name conv last_group named_groups in
	add_const accu "\\(";
	let groups = to_string (last_group, named_groups) accu re in
	add_const accu "\\)";
	groups

    | Bind_pos (loc, name) -> 
	let last_group = succ last_group in
	let named_groups = add_new_pos loc name last_group named_groups in
	add_const accu "\\(\\)";
	(last_group, named_groups)

    | Backref (loc, name) ->
	(try
	   match Named_groups.find name (fst named_groups) with
	       [] -> Messages.invalid_backref loc name
	     | [(_, n, conv)] -> add_const accu (sprintf "\\%i" n); groups
	     | l -> 
		 add_const accu (sprintf "\\(%s\\)" 
		   (String.concat "\\|"
		      (List.map (fun (_, n, conv) -> sprintf "\\%i" n) l)));
		 (succ last_group, named_groups)
	 with Not_found -> Messages.invalid_backref loc name)
	    
    | Variable (loc, e) -> add_var accu e; groups
    | Nocase_variable (loc, e) -> add_var_nocase accu e; groups
	    
    | Closed ast -> 
	let saved_named_groups = named_groups in
	let (last_group, named_groups) = to_string groups accu ast in
	(last_group, saved_named_groups)

    | Possessive _ -> assert false
    | Lookahead _ -> assert false
    | Lookbehind _ -> assert false

let nocasify e =
  let loc = MLast.loc_of_expr e in
  <:expr< $uid: !mod_runtime$.nocase $e$ >>

let process_regexp loc ~sharing re re_name =
  let accu = ref [] in
  let (last_group, named_groups) = 
    to_string ~top:true (0, (Named_groups.empty, Named_groups.empty)) 
      accu re in
  let re_args, re_source = 
    Match.get_re_source ~quote_expr: <:expr< Str.quote >>
    ~nocasify accu in
  (re_args, re_source, named_groups, [])


(* Syntax expanders *)

open Constants

let expr_mutex loc = <:expr< $uid: !mod_runtime_mt$.$lid:str_mutex$ >>

let unlock loc =
  <:expr< Mutex.unlock $expr_mutex loc$ >>

let lock loc =
  <:expr< Mutex.lock $expr_mutex loc$ >>

let lock_unlock e =
  let loc = MLast.loc_of_expr e in
  <:expr< do { $lock loc$; 
	       try let x = $e$ in do { $unlock loc$; x }
               with [ exn -> do { $unlock loc$; raise exn } ] } >>

let unlock_lock e =
  let loc = MLast.loc_of_expr e in
  <:expr< do { $unlock loc$; 
	       try let x = $e$ in do { $lock loc$; x }
               with [ exn -> do { $lock loc$; raise exn } ] } >>

let string_match loc re_name get_re target pos =
  <:expr< Str.string_match $get_re$ $target$ $int:string_of_int pos$ >>

let matched_group loc n target =
  <:expr< Str.matched_group $int:string_of_int n$ $target$ >>

let matched_position loc n target =
  <:expr< Str.group_beginning $int:string_of_int n$ >>

let compile_regexp ~mt loc re_args re_source =
  let compile_string e =
    <:expr< Str.regexp $e$ >> in
    match re_args with
      [] -> 
	let re_string = Match.compute_re_string loc re_source in
	compile_string re_string
    | _ ->
	let key = 
	  match re_args with
	      [name, _] -> <:expr< $lid:name$ >>
	    | _ -> 
		let expr_list = 
		  List.map (fun (name, _) -> <:expr< $lid:name$ >>) re_args in
		<:expr< ( $list: expr_list$ ) >> in
	let compile =
	  let re_string = Match.compute_re_string loc re_source in
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

  

let convert loc conv e =
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

let insert_bindings_poly make_expr loc target set e =
  Named_groups.fold
    (fun name l e -> 
       match l with
	   [] -> assert false
	 | (loc, _, _) :: _ ->
	     let find_it =
	       List.fold_right 
		 (fun (loc, n, conv) accu ->
		    let expr = 
		      convert loc conv (make_expr loc n target) in
		    match accu with
			None -> Some expr
		      | Some e ->
			  Some <:expr< 
			  try $expr$ with [ Not_found -> $e$ ] >>)
		 l
		 None in
	     let result =
	       match find_it with
		   None -> assert false 
		 | Some e -> e in
	     <:expr< let $lid:name$ = $result$ in $e$ >>)
    set
    e

let insert_group_bindings = insert_bindings_poly matched_group
let insert_position_bindings = insert_bindings_poly matched_position

let insert_bindings loc target (group_bindings, position_bindings) e =
  insert_group_bindings loc target group_bindings 
    (insert_position_bindings loc target position_bindings e)


let match_and_bind loc re_name get_re target named_groups success failure =
  <:expr< 
  if $string_match loc re_name get_re target 0$
  then $insert_bindings loc target named_groups success$
  else $failure$ >>


let macro_replace loc re_name target_name named_groups expr =
  let target = <:expr< $lid:target_name$ >> in
  <:expr<
  fun $lid:target_name$ ->
    Str.global_substitute $lid:re_name$ 
    (fun _ -> $insert_bindings loc target named_groups expr$)
    $target$ >>


open Select_lib

let lib = { predefined_regexps = special_regexps;
	    unfold_range = true;
	    process_regexp = process_regexp;
	    compile_regexp_match = compile_regexp ~mt:false;
	    compile_regexp_search = compile_regexp ~mt:false;
	    match_and_bind = match_and_bind;
	    wrap_match = (fun e -> e);
	    wrap_user_case = (fun e -> e);
	    really_wrap_match = false;
	    really_wrap_user_case = false }

let lib_mt = { predefined_regexps = special_regexps;
	       unfold_range = true;
	       process_regexp = process_regexp;
	       compile_regexp_match = compile_regexp ~mt:true;
	       compile_regexp_search = compile_regexp ~mt:true;
	       match_and_bind = match_and_bind;
	       wrap_match = lock_unlock;
	       wrap_user_case = unlock_lock;
	       really_wrap_match = true;
	       really_wrap_user_case = true }
