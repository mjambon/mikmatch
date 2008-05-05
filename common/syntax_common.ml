(*pp $PP *)


open Printf

open Regexp_ast
open Select_lib
open Match

let regexp = Grammar.Entry.create Pcaml.gram "regexp";;
let regexp_match_case = Grammar.Entry.create Pcaml.gram "regexp_match_case";;
let range = Grammar.Entry.create Pcaml.gram "range";;

let eval_string loc s =
(*#if 3.06 | 3.07pre | 3.07 | 3.07_5
  Token.eval_string s
#else*)
  Token.eval_string loc s

let extend_common () =
  (try DELETE_RULE Pcaml.patt: LIDENT END
   with Not_found -> ());
  (try 
     DELETE_RULE 
       Pcaml.expr: 
       "let"; OPT "rec"; LIST1 Pcaml.let_binding SEP "and"; 
       "in"; Pcaml.expr LEVEL "top" 
     END
   with Not_found -> ());
  
  (try
     DELETE_RULE 
       Pcaml.str_item: 
       "let"; OPT "rec"; LIST1 Pcaml.let_binding SEP "and"; 
       "in"; Pcaml.expr
     END
   with Not_found -> ());

EXTEND
  GLOBAL: regexp regexp_match_case range;

  Pcaml.str_item: [
    [ "RE"; name = LIDENT; "="; re = regexp -> 
	warnings re;
	Hashtbl.add named_regexps name re;
      <:str_item< declare end >> ]
  ];

  special_patt: [
    [ "RE"; re = regexp -> `Regexp re
    | "/"; re = regexp; "/" -> `Regexp re
    | "%"; name = uid_path;
      arg = OPT Pcaml.patt LEVEL "simple" -> `View (name, arg) ]
  ];

  uid_path: [
    [ l = LIST1 UIDENT SEP "." -> 
	match List.rev l with 
	    basename :: modname -> (loc, basename, List.rev modname)
	  | _ -> assert false ]
  ];

  Pcaml.patt: LEVEL "simple" [ 
    [ x = special_patt -> handle_special_patt loc x ]
  ];

  Pcaml.expr: LEVEL "expr1" [
    [ "let"; o = OPT "rec"; l = LIST1 Pcaml.let_binding SEP "and"; "in";
        e2 = Pcaml.expr LEVEL "top" ->
	  handle_let_bindings loc (o <> None) l e2 ]
  ];

  Pcaml.expr: LEVEL "expr1" [
    [ "let"; LIDENT "view"; 
      name = UIDENT; "="; e1 = Pcaml.expr; "in"; e2 = Pcaml.expr ->
	<:expr< let $lid:"view_" ^ name$ = $e1$ in $e2$ >> ]
  ];

  Pcaml.expr: LEVEL "expr1" [
    [ "let"; "try"; o = OPT "rec"; l = LIST1 Pcaml.let_binding SEP "and"; 
      "in"; e2 = Pcaml.expr LEVEL "top";
      "with"; pwel = LIST1 lettry_case SEP "|" ->
	let_try_in loc o l e2 pwel ]
  ];

  Pcaml.str_item: LEVEL "top" [
    [ "let"; o = OPT "rec"; l = LIST1 Pcaml.let_binding SEP "and"; "in";
        e2 = Pcaml.expr ->
	  let e = handle_let_bindings loc (o <> None) l e2 in
	  <:str_item< $exp:e$ >> ]
  ];

  Pcaml.str_item: LEVEL "top" [
    [ "let"; LIDENT "view"; name = UIDENT; "="; e1 = Pcaml.expr ->
	<:str_item< value $lid:"view_" ^ name$ = $e1$ >> ]
  ];

  Pcaml.str_item: LEVEL "top" [
    [ "let"; "try"; o = OPT "rec"; l = LIST1 Pcaml.let_binding SEP "and"; 
      "in"; e2 = Pcaml.expr;
      "with"; pwel = LIST1 lettry_case SEP "|" ->
	let e = let_try_in loc o l e2 pwel in
	<:str_item< $exp:e$ >> ]
  ];

  lettry_case: [ 
    [ p = Pcaml.patt; 
      w = OPT [ "when"; e = Pcaml.expr -> e ]; "->"; 
      e = Pcaml.expr -> (p, w, <:expr< fun () -> $e$ >>) ]
  ];

  regexp_match_case: [ 
    [ x1 = Pcaml.patt; 
      w = OPT [ "when"; e = Pcaml.expr -> e ]; "->"; 
      x2 = Pcaml.expr ->
        (loc, x1, w, x2) ]
  ];

  regexp: [
    [ r = regexp; "as"; i = LIDENT; 
      conv = 
	OPT [ ":"; s = LIDENT ->
	       (match s with
		    "int" -> `Int
		  | "float" -> `Float
		  | "option" -> `Option
		  | s -> Messages.invalid_converter loc s)
	    | ":="; e = Pcaml.expr -> `Custom e
	    | "="; e = Pcaml.expr -> `Value e  ] ->
	Bind (loc, r, i, conv) ]
  | [ r1 = regexp; "|"; r2 = regexp -> alternative loc r1 r2 ]
  | [ r1 = regexp; r2 = regexp -> Sequence (loc, r1, r2) ]

  | "postop" NONA
      [ r = regexp; "*" -> Repetition (loc, (Star, true), Closed r)
      | r = regexp; "+" -> Repetition (loc, (Plus, true), Closed r)
      | r = regexp; "?" -> Repetition (loc, (Option, true), Closed r)
      | r = regexp; "~" -> nocase r
      | r = regexp; "{"; (rng, rng_loc) = range; "}" -> 
	  if !(lib).unfold_range then repeat rng_loc (Closed r) rng
	  else Repetition (loc, (Range rng, true), Closed r) ]

  | "binop" LEFTA 
      [ r1 = regexp; "#"; r2 = regexp ->
	  let msg = " term is not a set of characters" in
	  let set1 = Regexp_ast.as_charset loc ("left" ^ msg) r1 in
	  let set2 = Regexp_ast.as_charset loc ("right" ^ msg) r2 in
	  Characters (loc, Charset.diff set1 set2) ]

  | "preop" NONA 
      [ "!"; ident = LIDENT -> Backref (loc, ident)
      | "@"; e = Pcaml.expr LEVEL "." -> Variable (loc, e) ]

  | "simple" NONA 
      [ "["; set = charset; "]" -> Characters (loc, set)
      | s = string -> s
      | name = LIDENT -> find_named_regexp loc name
      | "%"; name = LIDENT -> Bind_pos (loc, name)
      | "("; r = regexp; ")" -> r
      ]
  ];

  string: [
    [ s = STRING -> Regexp_ast.of_string loc (eval_string loc s) ]
  | [ c = CHAR -> Characters (loc, Charset.singleton (Token.eval_char c)) ]
  ];

  charset: [
    [ "^"; x = charset -> Charset.complement x ]
  | [ c1 = CHAR; "-"; c2 = CHAR -> 
	Charset.range (Token.eval_char c1) (Token.eval_char c2) 

    | c = CHAR -> Charset.singleton (Token.eval_char c)
    | s = STRING -> Charset.of_string (eval_string loc s)
    | name = LIDENT -> 
	Regexp_ast.as_charset loc "not a set of characters" 
	  (find_named_regexp loc name)
    | set1 = charset; set2 = charset -> Charset.union set1 set2
    ]
  ];

  range: [
    [ mini = INT;
      maxi = 
	OPT ["-"; maxi = 
	       OPT [ maxi = INT -> int_of_string maxi ] -> maxi] -> 
	  let mini = int_of_string mini in
	  (mini, maxi), loc
    | mini = INT; "+" -> (int_of_string mini, Some None), loc ]
  ];

  (* Reserved identifiers in patterns *)
  Pcaml.patt: LEVEL "simple" [ 
    [ s = LIDENT -> 
	if Match.is_reserved s then
	  Messages.reserved_identifier loc Constants.reserved_prefix s
	else <:patt< $lid:s$ >> ]
  ];

END;;

let extend_regular () =
  extend_common ();
  (try
     DELETE_RULE
       Pcaml.str_item: "let"; OPT "rec"; LIST1 Pcaml.let_binding SEP "and"
     END
   with Not_found -> ());

  EXTEND
  Pcaml.expr: LEVEL "expr1" [
    [ "match"; target = Pcaml.expr; "with"; OPT "|";
      cases = LIST1 regexp_match_case SEP "|" -> 
	output_match loc target cases
    | "try"; e = Pcaml.expr; "with"; OPT "|";
      cases = LIST1 regexp_match_case SEP "|" -> 
	output_try loc e cases
    | "function"; OPT "|"; cases = LIST1 regexp_match_case SEP "|" ->
        output_function loc cases ]
  ];

  Pcaml.str_item: LEVEL "top" [
    [ "let"; o = OPT "rec"; l = LIST1 Pcaml.let_binding SEP "and" ->
	handle_value_bindings loc (o <> None) l ]
  ];
  END


let extend_revised () =
  extend_common ();

  (try
     DELETE_RULE
       Pcaml.str_item: "value"; OPT "rec"; LIST1 Pcaml.let_binding SEP "and"
     END
   with Not_found -> ());

  EXTEND
  Pcaml.expr: LEVEL "top" [
    [ "match"; target = Pcaml.expr; "with"; "[";
      cases = LIST1 regexp_match_case SEP "|"; "]" -> 
	output_match loc target cases
    | "try"; e = Pcaml.expr; "with"; "[";
      cases = LIST1 regexp_match_case SEP "|"; "]" -> 
	output_try loc e cases
    | "fun"; "["; cases = LIST1 regexp_match_case SEP "|"; "]" ->
        output_function loc cases ]
  ];

  Pcaml.str_item: LEVEL "top" [
    [ "value"; o = OPT "rec"; l = LIST1 Pcaml.let_binding SEP "and" ->
	handle_value_bindings loc (o <> None) l ]
  ];
  END


let () =
  init_named_regexps ();

  Pcaml.add_option "-tailrec" 
    (Arg.Set tailrec)
    " produce code that preserves tail-recursivity (default)";

  Pcaml.add_option "-direct" 
    (Arg.Clear tailrec)
    " produce code that does not try to preserve tail-recursivity";

  (match !Pcaml.syntax_name with
       "OCaml" -> extend_regular ()
     | _ -> extend_revised ())

