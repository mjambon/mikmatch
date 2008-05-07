(*pp camlp4orf *)

open Camlp4.PreCast
open Syntax

open Syntax_common
open Select_lib
open Match

let extend_common () =
EXTEND Gram
  Pcaml.expr: [
    [ "RE_STR"; re = regexp -> 
	Regexp_ast.warnings re;
	let (re_args, re_source, named_groups, postbindings) =
	  Str_lib.lib.process_regexp loc ~sharing:true re "" in
	
	let re_fragments = Match.get_re_fragments loc re_source in
	<:expr< ( $re_fragments$, 
		  $pp_named_groups loc named_groups$ ) >> ]
  ];

  Syntax_common.regexp: LEVEL "simple" [
    [ "_" -> Regexp_ast.Characters (loc, Charset.full) ]
  ];

END;;

let extend_regular () = extend_common ()
let extend_revised () = extend_common ()

let _ =
  select_lib Str_lib.lib;

  (* Keeping it for backwards compatibility *)
  Pcaml.add_option "-thread" 
    (Arg.Unit (
       fun () -> 
	 eprintf "Warning: the -thread option is no longer needed.\n/%!"
     ))
    " obsolete option, ignored";
  
  (match !Pcaml.syntax_name with
       "OCaml" -> extend_regular ()
     | _ -> extend_revised ())