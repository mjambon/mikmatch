(* $Id$ *)

open Printf

open Camlp4.PreCast

let warning _loc s =
  (* Format.err_formatter _loc;*)
  let label = if !Sys.interactive then "" else "Warning: " in
  Format.eprintf "%a:@.%s%s@." Loc.print _loc label s

let failure _loc s =
  (* does it print the error like Stdpp.raise_with_loc? *)
  Loc.raise _loc (Failure s)


let list = function
    [] -> ""
  | [s] -> s
  | l -> 
      let l' = List.rev l in
      String.concat ", " (List.rev (List.tl l')) ^ " and " ^ List.hd l'

let invalid_backref _loc name =
  failure _loc
    (sprintf "Invalid backreference %s" name)

let unbalanced_bindings _loc l =
  failure _loc
    (sprintf "Variable%s %s must occur on both sides of this | pattern"
       (if List.length l > 1 then "s" else "")
       (list l))

let multiple_binding _loc l =
  let s, are =
    if List.length l > 1 then "s", "are"
    else "", "is" in
  failure _loc
    (sprintf "Variable%s %s %s bound several times in this matching"
       s (list l) are)

let invalid_range _loc =
  failure _loc "Invalid range"

let invalid_pattern _loc =
  failure _loc "Invalid pattern"

let invalid_lookbehind _loc kind adjective =
  failure _loc 
    (sprintf "%s are disabled in %slookbehind assertions" kind adjective)

let not_visible _loc who where =
  let s, are = 
    if List.length who > 1 then "s", "are"
    else "", "is" in
  warning _loc 
    (sprintf "identifier%s %s %s not visible \
              out of this %s" 
       s (list who) are where)

let invalid_converter _loc name =
  failure _loc 
    (sprintf "%s is not a valid converter" name)

let reserved_identifier _loc prefix name =
  failure _loc 
    (sprintf "%s is a reserved identifier: use another prefix than %s" 
       name prefix)

let misplaced_pattern p =
  failure (Ast.loc_of_patt p)
    ("patterns of this kind cannot appear in this context. \
      Use match ... with if you are unsure.")

let cannot_delete_rule name =
  eprintf "Warning: Cannot delete rule %s\n%!" name
