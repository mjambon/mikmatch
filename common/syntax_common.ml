(*pp camlp4orf *)


open Printf

open Camlp4.PreCast
open Syntax

open Mm_util
open Regexp_ast
open Select_lib
open Match

let regexp = Gram.Entry.mk "regexp";;
let regexp_match_case = Gram.Entry.mk "regexp_match_case";;
let range = Gram.Entry.mk "range";;

let extend_common () =
  (try DELETE_RULE Gram patt: LIDENT END
   with Not_found -> ());
  (try 
     DELETE_RULE Gram 
       expr: 
       "let"; OPT "rec"; LIST1 let_binding SEP "and"; 
       "in"; expr LEVEL "top" 
     END
   with Not_found -> ());
  
  (try
     DELETE_RULE Gram
       str_item: 
       "let"; OPT "rec"; LIST1 let_binding SEP "and"; 
       "in"; expr
     END
   with Not_found -> ());

EXTEND Gram
  GLOBAL: 
    str_item patt expr
    regexp regexp_match_case range;

  str_item: [
    [ "RE"; name = LIDENT; "="; re = regexp -> 
	warnings re;
	Hashtbl.add named_regexps name re;
      <:str_item< >> ]
  ];

  special_patt: [
    [ "RE"; re = regexp -> `Regexp re
    | "/"; re = regexp; "/" -> `Regexp re
    | "%"; name = uid_path;
      arg = OPT patt LEVEL "simple" -> `View (name, arg) ]
  ];

  uid_path: [
    [ l = LIST1 [ x = UIDENT -> x ] SEP "." -> 
	match List.rev l with 
	    basename :: modname -> (_loc, basename, List.rev modname)
	  | _ -> assert false ]
  ];

  patt: LEVEL "simple" [ 
    [ x = special_patt -> handle_special_patt _loc x ]
  ];

  expr: LEVEL "expr1" [
    [ "let"; o = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        e2 = expr LEVEL "top" ->
	  handle_let_bindings _loc (o <> None) (List.map pair_of_binding l) e2 ]
  ];

  expr: LEVEL "expr1" [
    [ "let"; LIDENT "view"; 
      name = UIDENT; "="; e1 = expr; "in"; e2 = expr ->
	<:expr< let $lid:"view_" ^ name$ = $e1$ in $e2$ >> ]
  ];

  expr: LEVEL "expr1" [
    [ "let"; "try"; o = OPT "rec"; l = LIST1 let_binding SEP "and"; 
      "in"; e2 = expr LEVEL "top";
      "with"; pwel = LIST1 lettry_case SEP "|" ->
	let_try_in _loc o (List.map pair_of_binding l) e2 pwel ]
  ];

  str_item: LEVEL "top" [
    [ "let"; o = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        e2 = expr ->
	  let e = 
	    handle_let_bindings _loc (o <> None) 
	      (List.map pair_of_binding l) e2 in
	  <:str_item< $exp:e$ >> ]
  ];

  str_item: LEVEL "top" [
    [ "let"; LIDENT "view"; name = UIDENT; "="; e1 = expr ->
	<:str_item< value $lid:"view_" ^ name$ = $e1$ >> ]
  ];

  str_item: LEVEL "top" [
    [ "let"; "try"; o = OPT "rec"; l = LIST1 let_binding SEP "and"; 
      "in"; e2 = expr;
      "with"; pwel = LIST1 lettry_case SEP "|" ->
	let e = let_try_in _loc o (List.map pair_of_binding l) e2 pwel in
	<:str_item< $exp:e$ >> ]
  ];

  lettry_case: [ 
    [ p = patt; 
      w = OPT [ "when"; e = expr -> e ]; "->"; 
      e = expr -> match_case_of_tuple _loc (p, w, <:expr< fun () -> $e$ >>) ]
  ];

  regexp_match_case: [ 
    [ x1 = patt; 
      w = OPT [ "when"; e = expr -> e ]; "->"; 
      x2 = expr ->
        (_loc, x1, w, x2) ]
  ];

  regexp: [
    [ r = regexp; "as"; i = LIDENT; 
      conv = 
	OPT [ ":"; s = LIDENT ->
	       (match s with
		    "int" -> `Int
		  | "float" -> `Float
		  | "option" -> `Option
		  | s -> Messages.invalid_converter _loc s)
	    | ":="; e = expr -> `Custom e
	    | "="; e = expr -> `Value e  ] ->
	Bind (_loc, r, i, conv) ]
  | [ r1 = regexp; "|"; r2 = regexp -> alternative _loc r1 r2 ]
  | [ r1 = regexp; r2 = regexp -> Sequence (_loc, r1, r2) ]

  | "postop" NONA
      [ r = regexp; "*" -> Repetition (_loc, (Star, true), Closed r)
      | r = regexp; "+" -> Repetition (_loc, (Plus, true), Closed r)
      | r = regexp; "?" -> Repetition (_loc, (Option, true), Closed r)
      | r = regexp; "~" -> nocase r
      | r = regexp; "{"; (rng, rng_loc) = range; "}" -> 
	  if !(lib).unfold_range then repeat rng_loc (Closed r) rng
	  else Repetition (_loc, (Range rng, true), Closed r) ]

  | "binop" LEFTA 
      [ r1 = regexp; "#"; r2 = regexp ->
	  let msg = " term is not a set of characters" in
	  let set1 = Regexp_ast.as_charset _loc ("left" ^ msg) r1 in
	  let set2 = Regexp_ast.as_charset _loc ("right" ^ msg) r2 in
	  Characters (_loc, Charset.diff set1 set2) ]

  | "preop" NONA 
      [ "!"; ident = LIDENT -> Backref (_loc, ident)
      | "@"; e = expr LEVEL "." -> Variable (_loc, e) ]

  | "simple" NONA 
      [ "["; set = charset; "]" -> Characters (_loc, set)
      | s = string -> s
      | name = LIDENT -> find_named_regexp _loc name
      | "%"; name = LIDENT -> Bind_pos (_loc, name)
      | "("; r = regexp; ")" -> r
      ]
  ];

  string: [
    [ s = STRING -> Regexp_ast.of_string _loc (eval_string s) ]
  | [ c = CHAR -> Characters (_loc, Charset.singleton (eval_char c)) ]
  ];

  charset: [
    [ "^"; x = charset -> Charset.complement x ]
  | [ c1 = CHAR; "-"; c2 = CHAR -> 
	Charset.range (eval_char c1) (eval_char c2) 

    | c = CHAR -> Charset.singleton (eval_char c)
    | s = STRING -> Charset.of_string (eval_string s)
    | name = LIDENT -> 
	Regexp_ast.as_charset _loc "not a set of characters" 
	  (find_named_regexp _loc name)
    | set1 = charset; set2 = charset -> Charset.union set1 set2
    ]
  ];

  range: [
    [ mini = INT;
      maxi = 
	OPT ["-"; maxi = 
	       OPT [ maxi = INT -> int_of_string maxi ] -> maxi] -> 
	  let mini = int_of_string mini in
	  (mini, maxi), _loc
    | mini = INT; "+" -> (int_of_string mini, Some None), _loc ]
  ];

  (* Reserved identifiers in patterns *)
  patt: LEVEL "simple" [ 
    [ s = LIDENT -> 
	if Match.is_reserved s then
	  Messages.reserved_identifier _loc Constants.reserved_prefix s
	else <:patt< $lid:s$ >> ]
  ];

END;;

let extend_regular () =
  extend_common ();
  (try
     DELETE_RULE Gram
       str_item: "let"; OPT "rec"; LIST1 let_binding SEP "and"
     END
   with Not_found -> ());

  EXTEND Gram
  expr: LEVEL "expr1" [
    [ "match"; target = expr; "with"; OPT "|";
      cases = LIST1 regexp_match_case SEP "|" -> 
	output_match _loc target cases
    | "try"; e = expr; "with"; OPT "|";
      cases = LIST1 regexp_match_case SEP "|" -> 
	output_try _loc e cases
    | "function"; OPT "|"; cases = LIST1 regexp_match_case SEP "|" ->
        output_function _loc cases ]
  ];

  str_item: LEVEL "top" [
    [ "let"; o = OPT "rec"; l = LIST1 let_binding SEP "and" ->
	handle_value_bindings _loc (o <> None) (List.map pair_of_binding l) ]
  ];
  END


let extend_revised () =
  extend_common ();

  (try
     DELETE_RULE Gram
       str_item: "value"; OPT "rec"; LIST1 let_binding SEP "and"
     END
   with Not_found -> ());

  EXTEND Gram
  expr: LEVEL "top" [
    [ "match"; target = expr; "with"; "[";
      cases = LIST1 regexp_match_case SEP "|"; "]" -> 
	output_match _loc target cases
    | "try"; e = expr; "with"; "[";
      cases = LIST1 regexp_match_case SEP "|"; "]" -> 
	output_try _loc e cases
    | "fun"; "["; cases = LIST1 regexp_match_case SEP "|"; "]" ->
        output_function _loc cases ]
  ];

  str_item: LEVEL "top" [
    [ "value"; o = OPT "rec"; l = LIST1 let_binding SEP "and" ->
	handle_value_bindings _loc (o <> None) (List.map pair_of_binding l) ]
  ];
  END


let () =
  init_named_regexps ();

  Camlp4.Options.add "-tailrec" 
    (Arg.Set tailrec)
    " produce code that preserves tail-recursivity (default)";

  Camlp4.Options.add "-direct" 
    (Arg.Clear tailrec)
    " produce code that does not try to preserve tail-recursivity";

  (match Id.name with
       "OCaml" -> extend_regular ()
     | _ -> extend_revised ())

