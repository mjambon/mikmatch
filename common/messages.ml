open Printf

let list = function
    [] -> ""
  | [s] -> s
  | l -> 
      let l' = List.rev l in
      String.concat ", " (List.rev (List.tl l')) ^ " and " ^ List.hd l'

let invalid_backref loc name =
  Stdpp.raise_with_loc loc
    (Failure
       (sprintf "Invalid backreference %s" name))

let unbalanced_bindings loc l =
  Stdpp.raise_with_loc loc
    (Failure 
       (sprintf "Variable%s %s must occur on both sides of this | pattern"
	  (if List.length l > 1 then "s" else "")
	  (list l)))

let multiple_binding loc l =
  let s, are =
    if List.length l > 1 then "s", "are"
    else "", "is" in
  Stdpp.raise_with_loc loc
    (Failure 
       (sprintf "Variable%s %s %s bound several times in this matching"
	  s (list l) are))

let invalid_range loc =
  Stdpp.raise_with_loc loc (Failure "Invalid range")

let invalid_pattern loc =
  Stdpp.raise_with_loc loc (Failure "Invalid pattern")

let invalid_lookbehind loc kind adjective =
  Stdpp.raise_with_loc loc 
    (Failure 
       (sprintf "%s are disabled in %slookbehind assertions" kind adjective))

let not_visible loc who where =
  let warning = if !Sys.interactive then "" else "Warning: " in
  let s, are = 
    if List.length who > 1 then "s", "are"
    else "", "is" in
  !Pcaml.warning loc 
    (sprintf "%sidentifier%s %s %s not visible \
              out of this %s" 
       warning s (list who) are where)

let invalid_converter loc name =
  Stdpp.raise_with_loc loc 
    (Failure 
       (sprintf "%s is not a valid converter" name))

let reserved_identifier loc prefix name =
  Stdpp.raise_with_loc loc 
    (Failure 
       (sprintf "%s is a reserved identifier: use another prefix than %s" 
	  name prefix))

let misplaced_pattern p =
  Stdpp.raise_with_loc (MLast.loc_of_patt p)
    (Failure "patterns of this kind cannot appear in this context. \
              Use match ... with if you are unsure.")

