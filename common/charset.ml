module C = Set.Make (Char)

type t = C.t

let empty = C.empty

let add = C.add
let singleton = C.singleton
let union = C.union
let diff = C.diff

let add_range first last set =
  let r = ref set in
  for i = Char.code first to Char.code last do
    r := add (Char.chr i) !r
  done;
  !r

let range c1 c2 = add_range c1 c2 empty
let irange i j = range (Char.chr i) (Char.chr j)

let full = range '\000' '\255'
let full_for_C = C.remove '\000' full

let of_string s =
  let accu = ref C.empty in
  String.iter (fun c -> accu := C.add c !accu) s;
  !accu

let complement set = C.diff full set

let list = C.elements

let nocase set =
  C.fold 
    (fun c set -> 
       let c1 = Char.lowercase_ascii c 
       and c2 = Char.uppercase_ascii c in
       let set1 = C.add c1 set in
       if c1 <> c2 then C.add c2 set1
       else set1)
    set
    C.empty

module Posix =
struct
  let lower = range 'a' 'z'
  let upper = range 'A' 'Z'
  let ascii = range '\x00' '\x7F'
  let alpha = union lower upper
  let digit = range '0' '9'
  let alnum = union alpha digit
  let punct = of_string "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  let graph = union alnum punct
  let print = union (singleton ' ') graph
  let blank = of_string " \t"
  let cntrl = union (range '\x00' '\x1F') (singleton '\x7F')
  let xdigit = of_string "0123456789abcdefABCDEF"
  let space = of_string " \t\n\x0B\x0C\r"

  let all = [ "lower", lower;
	      "upper", upper;
	      "ascii", ascii;
	      "alpha", alpha;
	      "digit", digit;
	      "alnum", alnum;
	      "punct", punct;
	      "graph", graph;
	      "print", print;
	      "blank", blank;
	      "cntrl", cntrl;
	      "xdigit", xdigit;
	      "space", space; ]
end
