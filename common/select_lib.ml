open MLast

let not_implemented loc =
  Stdpp.raise_with_loc loc (Failure "not implemented")

let fail _ = failwith "not implemented"

type regexp_args = (string * MLast.expr) list
type regexp_source = [ `String of string | `Expr of MLast.expr ] list

type regexp_lib = 
    { predefined_regexps : (string * Regexp_ast.ast) list;
      unfold_range : bool;
      process_regexp : 
	loc -> sharing:bool -> Regexp_ast.ast -> string ->
	     regexp_args *
	     regexp_source *
	     (Regexp_ast.named_groups * Regexp_ast.named_groups) *
	     (string * expr) list;
      compile_regexp_match : loc -> regexp_args -> regexp_source -> expr;
      compile_regexp_search : loc -> regexp_args -> regexp_source -> expr;
      match_and_bind : 
	loc -> string -> expr -> expr -> 
	(Regexp_ast.named_groups * Regexp_ast.named_groups) -> 
	expr -> expr -> expr;
      wrap_match : expr -> expr;
      wrap_user_case : expr -> expr;
      really_wrap_match : bool;
      really_wrap_user_case : bool }

let dummy = 
  { predefined_regexps = [];
    unfold_range = false;
    process_regexp = not_implemented;
    compile_regexp_match = not_implemented;
    compile_regexp_search = not_implemented;
    match_and_bind = not_implemented;
    wrap_match = fail;
    wrap_user_case = fail;
    really_wrap_match = false;
    really_wrap_user_case = false }
