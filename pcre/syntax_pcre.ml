(*pp camlp4orf *)

open Printf

open Camlp4.PreCast
open Syntax

open Regexp_ast
open Syntax_common
open Select_lib
open Match

let expand_macro ?(sharing = false) ?(anchored = false) _loc re e f =
  warnings re;
  let (num, re_name) = Constants.new_regexp () in
  let var_name = var_of_regexp re_name in
  let (re_args, re_source, named_groups, postbindings) = 
    (!lib).process_regexp ~sharing _loc re re_name in
  let get_re = Match.make_get_re _loc re_name re_args in
  add_compiled_regexp ~anchored postbindings
    _loc re_name num re_args re_source named_groups;
  
  !(lib).wrap_match 
    (f _loc re_name get_re var_name named_groups
       ((!lib).wrap_user_case e))

let check_assertion ~lookahead positive re =
  let rec check ~branched = function
      Bind (_loc, e, name, conv) -> 
	if not positive then
	  Messages.not_visible _loc [name] "negative assertion";
	check ~branched e
    | Bind_pos (_loc, name) -> 
	if not positive then
	  Messages.not_visible _loc [name] "negative assertion";
	0
    | Epsilon _ -> 0
    | Characters _ -> 1
    | Special (_loc, s, (name, Some len)) -> len
    | Special (_loc, s, (name, None)) -> 
	if not lookahead then
	  Messages.invalid_lookbehind _loc
	    (sprintf "These patterns (%s)" name) ""
	else 0
    | Backref (_loc, _) ->
	if not lookahead then
	  Messages.invalid_lookbehind _loc "Backreferences" ""
	else 0
    | Variable (_loc, _) | Nocase_variable (_loc, _) -> 
	if not lookahead && branched then
	  Messages.invalid_lookbehind _loc "Variables in optional branches" ""
	else 0
    | Sequence (_loc, e1, e2) -> check ~branched e1 + check ~branched e2
    | Alternative (_loc, e1, e2, _, _) -> 
	let len1 = check ~branched:true e1 in
	let len2 = check ~branched:true e2 in
	if not lookahead && len1 <> len2 then
	    Messages.invalid_lookbehind _loc 
	      "Alternatives of different length" ""
	else max len1 len2
    | Repetition (_loc, (kind, greediness), e) ->
	(match kind with
	     Range (x, None) -> x * check ~branched e
	   | _ -> 
	       if not lookahead then
		 Messages.invalid_lookbehind _loc 
		   "Repetitions of variable length" ""
	       else check ~branched e)
    | Lookahead (_loc, _, e) 
    | Lookbehind (_loc, _, e) -> check ~branched e
    | Possessive (_, e)
    | Closed e -> check ~branched e in
  
  ignore (check ~branched:false re)


  

let lookahead _loc bopt re =
  let positive = bopt = None in
  check_assertion ~lookahead:true positive re;
  Lookahead (_loc, positive, if positive then re else Closed re)

let lookbehind _loc bopt re =
  let positive = bopt = None in
  check_assertion ~lookahead:false positive re;
  Lookbehind (_loc, positive, if positive then re else Closed re)

let seq _loc e = <:expr< do { $e$ } >>

let extend_common () =
  let expr_level = "top" in
EXTEND Gram
  expr: LEVEL $expr_level$ [ 
    [ "RE_PCRE"; re = regexp -> 
	warnings re;
	let (re_args, re_source, named_groups, postbindings) = 
	  Pcre_lib.lib.process_regexp _loc ~sharing:false re "" in
	
	let re_fragments = Match.get_re_fragments _loc re_source in
  	<:expr< ( $re_fragments$, 
		  $pp_named_groups _loc named_groups$ ) >>
	

    | "REPLACE"; re = regexp; "->"; e = sequence ->
	expand_macro _loc re (seq _loc e) Pcre_lib.macro_replace

    | "SEARCH"; re = regexp; "->"; e = sequence ->
	expand_macro _loc re (seq _loc e) Pcre_lib.macro_search

    | "MAP"; re = regexp; "->"; e = sequence ->
	expand_macro _loc re (seq _loc e) Pcre_lib.macro_map

    | "COLLECT"; re = regexp; "->"; e = sequence ->
	expand_macro _loc re (seq _loc e) Pcre_lib.macro_collect

    | "COLLECTOBJ"; re = regexp ->
	expand_macro _loc re <:expr< assert false >> Pcre_lib.macro_collectobj

    | "SPLIT"; re = regexp ->
	expand_macro _loc re <:expr< assert false >> Pcre_lib.macro_split

    | "REPLACE_FIRST"; re = regexp; "->"; e = sequence ->
	expand_macro _loc re (seq _loc e) Pcre_lib.macro_replace_first

    | "SEARCH_FIRST"; re = regexp; "->"; e = sequence ->
	expand_macro ~sharing:true _loc re (seq _loc e)
	  Pcre_lib.macro_search_first

    | "MATCH"; re = regexp; "->"; e = sequence ->
	expand_macro ~sharing:true ~anchored:true _loc re (seq _loc e)
	  Pcre_lib.macro_match

    | "FILTER"; re = regexp ->
	expand_macro ~sharing:true ~anchored:true _loc re 
	<:expr< assert false >> Pcre_lib.macro_filter

    | "CAPTURE"; re = regexp ->
	expand_macro ~sharing:true ~anchored:true _loc re 
	<:expr< assert false >> Pcre_lib.macro_capture
    ]
  ];

  regexp: LEVEL "postop" [
    [ re = regexp; "*"; UIDENT "Lazy" -> 
	Repetition (_loc, (Star, false), Closed re)
    | re = regexp; "+"; UIDENT "Lazy" ->
	Repetition (_loc, (Plus, false), Closed re)
    | re = regexp; "?"; UIDENT "Lazy" ->
	Repetition (_loc, (Option, false), Closed re)
    | r = regexp; "{"; (rng, rng_loc) = range; "}"; UIDENT "Lazy" -> 
	Repetition (_loc, (Range rng, false), Closed r)
    | re = regexp; UIDENT "Possessive" ->
	Possessive (_loc, re) ]
  ];

  regexp: LEVEL "simple" [
    [ "_" -> Characters (_loc, Charset.full)
    | "<";
      x = OPT [ b1 = OPT [ x = UIDENT "Not" -> x ];
		re1 = regexp -> (b1, re1) ]; 
      y = OPT [ "."; 
		r2 = OPT [ b2 = OPT [ x = UIDENT "Not" -> x ]; 
			   re2 = regexp -> (b2, re2) ] -> r2 ];
      ">" ->
	match x, y with
	    None, None
	  | None, Some None -> Epsilon _loc
	  | None, Some (Some (b2, re2)) -> lookahead _loc b2 re2
	  | Some (b1, re1), None -> lookahead _loc b1 re1
	  | Some (b1, re1), Some None -> lookbehind _loc b1 re1
	  | Some (b1, re1), Some (Some (b2, re2)) ->
	      Sequence (_loc, lookbehind _loc b1 re1, lookahead _loc b2 re2) ]
  ];

END;;

let extend_regular () = extend_common ()
let extend_revised () = extend_common ()

let _ =
  select_lib Pcre_lib.lib;

  Camlp4.Options.add "-thread" 
    (Arg.Unit (
       fun () -> 
	 select_lib Pcre_lib.lib_mt;
	 eprintf "Warning: -thread is deprecated.\n/%!"
     )
    )
    " Deprecated option that protects access to shared data with a mutex. \
      Currently only patterns containing @ are concerned.";

  (* How to test if the current syntax is the regular or revised one? *)
  extend_regular ()
