(* $Id$ *)

open Camlp4.PreCast

val init : (Ast.loc -> string -> Ast.str_item option) -> unit
(*
   [init get] registers a new filter that will operate after the parsing phase.
   The [get] function will be called to look up all expression lowercase 
   identifiers. It returns the structure items that are required by this
   expression. They will be inserted just before the current structure item,
   only once.

   Example:

   In your files that will extend the camlp4 preprocessor, define the
   following:


     let get _loc id = 
       match id with 
          "pi" -> Some <:str_item< value pi = acos (-1.) >>
        | _ -> None
     ;;

     let () = init get;;
     

   The preprocessed file:

     let x = 0.5 *. pi


   will be expanded into:

     let pi = acos (-1.)
     let x = 0.5 *. pi

*)

val init_from_table : (string, Ast.str_item) Hashtbl.t -> unit
(*
  Same as [init], but uses the given hash table for its lookups.
*)
