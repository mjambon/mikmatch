open Printf

ignore (FILTER "a")

open Set

module M =
struct
  open Map

  ignore (FILTER "b")

  open Hashtbl
end
