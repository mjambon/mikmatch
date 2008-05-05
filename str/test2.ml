(* camlp4o -I . ./pa_micmatch.cma pr_o.cmo -thread test2.ml *)

match Some "x" with
    RE "y" -> "hello"
  | _ -> "bye"
