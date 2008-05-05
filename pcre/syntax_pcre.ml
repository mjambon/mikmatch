(*pp $PP *)

open Printf

open Regexp_ast
open Syntax_common
open Select_lib
open Match

let expand_macro ?(sharing = false) ?(anchored = false) loc re e f =
  warnings re;
  let (num, re_name) = Constants.new_regexp () in
  let var_name = var_of_regexp re_name in
  let (re_args, re_source, named_groups, postbindings) = 
    (!lib).process_regexp ~sharing loc re re_name in
  let get_re = Match.make_get_re loc re_name re_args in
  add_compiled_regexp ~anchored postbindings
    loc re_name num re_args re_source named_groups;
  
  !(lib).wrap_match 
    (f loc re_name get_re var_name named_groups
       ((!lib).wrap_user_case e))

let check_assertion ~lookahead positive re =
  let rec check ~branched = function
      Bind (loc, e, name, conv) -> 
	if not positive then
	  Messages.not_visible loc [name] "negative assertion";
	check ~branched e
    | Bind_pos (loc, name) -> 
	if not positive then
	  Messages.not_visible loc [name] "negative assertion";
	0
    | Epsilon _ -> 0
    | Characters _ -> 1
    | Special (loc, s, (name, Some len)) -> len
    | Special (loc, s, (name, None)) -> 
	if not lookahead then
	  Messages.invalid_lookbehind loc
	    (sprintf "These patterns (%s)" name) ""
	else 0
    | Backref (loc, _) ->
	if not lookahead then
	  Messages.invalid_lookbehind loc "Backreferences" ""
	else 0
    | Variable (loc, _) | Nocase_variable (loc, _) -> 
	if not lookahead && branched then
	  Messages.invalid_lookbehind loc "Variables in optional branches" ""
	else 0
    | Sequence (loc, e1, e2) -> check ~branched e1 + check ~branched e2
    | Alternative (loc, e1, e2, _, _) -> 
	let len1 = check ~branched:true e1 in
	let len2 = check ~branched:true e2 in
	if not lookahead && len1 <> len2 then
	    Messages.invalid_lookbehind loc 
	      "Alternatives of different length" ""
	else max len1 len2
    | Repetition (loc, (kind, greediness), e) ->
	(match kind with
	     Range (x, None) -> x * check ~branched e
	   | _ -> 
	       if not lookahead then
		 Messages.invalid_lookbehind loc 
		   "Repetitions of variable length" ""
	       else check ~branched e)
    | Lookahead (loc, _, e) 
    | Lookbehind (loc, _, e) -> check ~branched e
    | Possessive (_, e)
    | Closed e -> check ~branched e in
  
  ignore (check ~branched:false re)


  

let lookahead loc bopt re =
  let positive = bopt = None in
  check_assertion ~lookahead:true positive re;
  Lookahead (loc, positive, if positive then re else Closed re)

let lookbehind loc bopt re =
  let positive = bopt = None in
  check_assertion ~lookahead:false positive re;
  Lookbehind (loc, positive, if positive then re else Closed re)

let extend_common ?(expr1_level = "expr1") () =
EXTEND
  Pcaml.expr: LEVEL $expr1_level$ [ 
    [ "RE_PCRE"; re = regexp -> 
	warnings re;
	let (re_args, re_source, named_groups, postbindings) = 
	  Pcre_lib.lib.process_regexp loc ~sharing:false re "" in
	
	let re_fragments = Match.get_re_fragments loc re_source in
  	<:expr< ( $re_fragments$, 
		  $pp_named_groups loc named_groups$ ) >>
	

    | "REPLACE"; re = regexp; "->"; e = Pcaml.expr LEVEL "top" ->
	expand_macro loc re e Pcre_lib.macro_replace

    | "SEARCH"; re = regexp; "->"; e = Pcaml.expr LEVEL "top" ->
	expand_macro loc re e Pcre_lib.macro_search

    | "MAP"; re = regexp; "->"; e = Pcaml.expr LEVEL "top" ->
	expand_macro loc re e Pcre_lib.macro_map

    | "COLLECT"; re = regexp; "->"; e = Pcaml.expr LEVEL "top" ->
	expand_macro loc re e Pcre_lib.macro_collect

    | "COLLECTOBJ"; re = regexp ->
	expand_macro loc re <:expr< assert false >> Pcre_lib.macro_collectobj

    | "SPLIT"; re = regexp ->
	expand_macro loc re <:expr< assert false >> Pcre_lib.macro_split

    | "REPLACE_FIRST"; re = regexp; "->"; e = Pcaml.expr LEVEL "top" ->
	expand_macro loc re e Pcre_lib.macro_replace_first

    | "SEARCH_FIRST"; re = regexp; "->"; e = Pcaml.expr LEVEL "top" ->
	expand_macro ~sharing:true loc re e Pcre_lib.macro_search_first

    | "MATCH"; re = regexp; "->"; e = Pcaml.expr LEVEL "top" ->
	expand_macro ~sharing:true ~anchored:true loc re e Pcre_lib.macro_match

    | "FILTER"; re = regexp ->
	expand_macro ~sharing:true ~anchored:true loc re 
	<:expr< assert false >> Pcre_lib.macro_filter

    | "CAPTURE"; re = regexp ->
	expand_macro ~sharing:true ~anchored:true loc re 
	<:expr< assert false >> Pcre_lib.macro_capture
    ]
  ];

  regexp: LEVEL "postop" [
    [ re = regexp; "*"; UIDENT "Lazy" -> 
	Repetition (loc, (Star, false), Closed re)
    | re = regexp; "+"; UIDENT "Lazy" ->
	Repetition (loc, (Plus, false), Closed re)
    | re = regexp; "?"; UIDENT "Lazy" ->
	Repetition (loc, (Option, false), Closed re)
    | r = regexp; "{"; (rng, rng_loc) = range; "}"; UIDENT "Lazy" -> 
	Repetition (loc, (Range rng, false), Closed r)
    | re = regexp; UIDENT "Possessive" ->
	Possessive (loc, re) ]
  ];

  regexp: LEVEL "simple" [
    [ "_" -> Characters (loc, Charset.full)
    | "<";
      x = OPT [ b1 = OPT UIDENT "Not"; re1 = regexp -> (b1, re1) ]; 
      y = OPT [ "."; 
		r2 = OPT [ b2 = OPT UIDENT "Not"; 
			   re2 = regexp -> (b2, re2) ] -> r2 ];
      ">" ->
	match x, y with
	    None, None
	  | None, Some None -> Epsilon loc
	  | None, Some (Some (b2, re2)) -> lookahead loc b2 re2
	  | Some (b1, re1), None -> lookahead loc b1 re1
	  | Some (b1, re1), Some None -> lookbehind loc b1 re1
	  | Some (b1, re1), Some (Some (b2, re2)) ->
	      Sequence (loc, lookbehind loc b1 re1, lookahead loc b2 re2) ]
  ];

END;;

let extend_regular () = extend_common ()
let extend_revised () = extend_common ~expr1_level:"top" ()

let _ =
  select_lib Pcre_lib.lib;

  Pcaml.add_option "-thread" 
    (Arg.Unit (fun () -> select_lib Pcre_lib.lib_mt))
    " option required for multithreaded programs";

  (match !Pcaml.syntax_name with
       "OCaml" -> extend_regular ()
     | _ -> extend_revised ())
