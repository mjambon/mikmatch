open Printf

(* Definition of regular expressions for further use *)
RE space = [' ' '\t' '\n' '\r']
RE not_space = _ # space
RE digit = ['0'-'9']
RE letter = ['A'-'Z' '_' 'a'-'z']
RE word = letter+

(*
(* Inclusion of file in the same syntax, e.g. a library of user-defined
   regular expressions. (known problem with error location) *)
 USE "my_regexps.ml" (* defines `word' and `digit' *)
*)

(* Extended pattern-matching in the following constructs:
   match ... with ...
   try ... with ...
   function ...
*)


(* Doesn't work. Don't know how to make it work. *)
(* Testing the Camlp4 support for stream parsers *)

let _ = match Stream.of_list [] with parser [< >] -> ()


let test expected a b c =
  printf "[case %i] " expected; flush stdout;
  (match a, b, c with
       None, (None | Some (RE space* )), None -> printf "case 1\n"
     | Some ({ contents = [| RE (word as x); _; y |]}),
       (Some ("test" as z | RE word space (word as z))),
       None -> printf "case 2: %S %S %S\n" x y z
     | _, _, Some (RE space* (['0'-'9']+ as n)) -> printf "case 3: %s\n" n
     | _ -> printf "case 4\n");
  flush stdout
  
let _ =
  printf "Tests (match ... with):\n"; flush stdout;
  test 1 None (Some "   ") None;

  test 2
    (Some (ref [| "alpha"; "beta"; "2 gamma" |]))
    (Some "Hello World!") 
    None;

  test 3 None None (Some "  123  ");

  test 4 (Some (ref [| |])) (Some "") (Some "abc")

let _ =
  match "" with
      (RE (("a" as a) | ("b" as a))) | a -> ()

let hello_who s =
  match s with
      RE _* ['h''H']"ello" ","? space* 
      ((word | space)* word as someone) -> String.capitalize someone
	
    | _ -> "nobody"

let _ =
  printf "Extraction of the recipient's name\n"; flush stdout;
  List.iter (fun s ->
	       printf "Hello who: %S\n" s;
	       printf " -> %S\n" (hello_who s);
	       flush stdout)
    [ "Hello World!";
      "First of all, hello everybody.";
      "*** hello world ***";
      "Hello, Caml riders!" ]

let _ =
  printf "Test (local and global bindings):\n"; flush stdout;
  match "" with
      (RE (word as x | space+ (word as x))* ) | _ -> 
	printf "Passed.\n"

let _ =
  printf "Test (repetition range + end of line):\n"; flush stdout;
  let f s =
    match s with
	RE '-'? digit{1-4} eol -> printf "%S has less than 5 digits.\n" s
      | RE '-'? digit{5+} eol -> printf "%S has at least 5 digits.\n" s
      | _ -> printf "%S is not a number.\n" s in
  List.iter f ["123"; "1234"; "12345"; "12345678"; "-1234"; "*1"; "1*";
	       "9\n*" ]

let test f (expected, s) =
  let (success, result) = f s in
  let passed = expected = success in
  if passed then (printf "[OK] %s%s\n" s 
		    (match result with None -> "" 
		       | Some x -> sprintf " -> %s"x);
		  flush stdout)
  else 
    (print_endline (s ^ "Failed"); flush stdout; failwith s)

let () =
  printf "Test (no case: the ~ operator):\n"; flush stdout;
  List.iter 
    (test (function
	       RE "hello"~ " World!" -> true, None
	     | _ -> false, None))
    [ true, "Hello World!";
      true, "hElLO World!";
      false, "hello WORLD!" ]

let () =
  printf "Test (try ... with):\n"; flush stdout;
  try failwith "Hello World!"
  with 
      Failure RE "Hello" space* (word as w) -> printf "OK: %s\n" w
    | Failure s -> printf "Failure: %s\n" s

let () =
  printf "Test (function ... -> ...):\n"; flush stdout;
  let f = 
    function
	RE "Hello" space* (word as w) -> printf "OK: %s\n" w
      | _ -> printf "Error\n" in
  f "Hello Everybody";
  f "Hello Caml!"

let () =
  printf "Test (backreferences):\n"; flush stdout;
  let f s =
    match s with 
	RE 
	  (digit+ as x | (word as x)) (* x = global id *)
	  (" " as sp !sp)*            (* sp = local id *)
	  !x -> true, Some x 
      | _ -> false, None in
  List.iter (test f)
    [ true, "123123";
      false, "123 123";
      true, "123  123";
      true, "aaaa";
      false, "abc";
      false, "ab1ab1" ]


let print_named_groups l =
  List.iter
    (fun (name, positions) ->
       printf "%s:" name;
       List.iter (fun i -> printf " %i" i) positions;
       printf "\n")
    l

(* Lower level feature: RE_PCRE returns the source of the regexp,
   to be used with specific compilation or search options. *)

let _ =
  let (src, named_groups) = 
    RE_PCRE 
      (("a"|"b"+)? digit{2}) as start 
      (space* word)+ ( digit{1} (word as last_word)
		     | digit{1} Lazy (word as last_word)
		     | digit{3} (word as last_word)) in
  
  printf "Regexp source: %S\n" src;
  printf "Named groups and their locations:\n";
  print_named_groups named_groups;
  flush stdout

let charset_union = RE_PCRE digit | space | "a" | ['A'-'Z']


(* Laziness *)

let _ =
  printf "Laziness and backreferences:\n"; flush stdout;
  let f = function
      RE _* Lazy
	"<" (_* Lazy as tag) ">" (_* Lazy as contents) "</" !tag ">" ->
	  sprintf " -> (%S, %S)" tag contents
    | s -> "" in
  List.iter (fun s -> 
	       printf "%S%s\n" s (f s);
	       flush stdout)
    [ "<a><b>hello</b></a><a></a>";
      "<hello><hello></hello></hello>";
      "<unmatched><a>text</a></b>" ]

let _ =
  printf "Possessiveness + backreferences + laziness
Take the first word, find its next occurence and return the text
in the middle:\n"; flush stdout;
  let f = function
      RE letter* Possessive as x (_* Lazy as y) !x -> sprintf " -> %S" y
    | _ -> "" in
  List.iter (fun s -> 
	       printf "%S%s\n" s (f s);
	       flush stdout)
    [ "abc,ab,abc,abc" ]



(* Macros *)


let swap =
  REPLACE 
    "(" space* (word as x) space* "," 
        space* (word as y) space* ")" -> "(" ^ y ^ "," ^ x ^ ")"

let swap =
  REPLACE_FIRST 
    "(" space* (word as x) space* "," 
        space* (word as y) space* ")" -> "(" ^ y ^ "," ^ x ^ ")"

let _ =
  let test s =
    let s1 = swap s in
    let s2 = swap s1 in
    printf "swap 0: %s\nswap 1: %s\nswap 2: %s\n" s s1 s2 in

  test "tuples: (x,y)/(1, 2)/(martin,    jambon)"


RE host = (['.' '-'] | letter | digit)+

let hide_email = 
  flush stdout;
  REPLACE "@" (host as host) -> "@" ^ (String.make (String.length host) '.')

let _ =
  let test s =
    printf "\
before: %s
after : %s
" s (hide_email s) in
  test "this is a list of email addresses: joe@sixpack.com, martin@home"


let _ =
  let i = ref 0 in
  let f = SEARCH "a" -> incr i in
  f "aaa";
  printf "This should be 3: %i\n" !i;
  flush stdout

let _ =
  let f = MAP ['a'-'z']+ as w -> `Word (String.capitalize w) in
  f "hello world!"

let _ =
  List.iter print_endline ((SPLIT ",") "a,b,c")

let _ =
  let l = 
    List.filter (FILTER _* ".ml" eos) (Array.to_list (Sys.readdir ".")) in
  printf "*.ml: %s\n%!" (String.concat " " l)


(* Sharing the subgroups array *)

let _ =
  let f ?share s = 
    try (MATCH "+" (print* as x) -> print_endline ("  Found " ^ x)) ?share s
    with Not_found -> print_endline "  Not found" in
  let g ?share () =
    print_endline "2 found:";
    flush stdout;
    List.iter
      (f ?share)
      [ "+a"; "b"; "+blop" ] in
  g ~share:true ();
  g ~share:false ()


(* Positions *)

let _ =
  match "a1234yz", 333 with 
      RE "a" as s ("bc" %pos | digit+ %pos), _ 
    | (s, pos) -> 
	printf "a = %s, 5 = %i\n%!" s pos;
	assert (s = "a" && pos = 5)

let _ =
  let find = 
    SEARCH_FIRST "(" %pos1 (_* Lazy as x) %pos2 ")" -> 
      printf "%s %i-%i\n%!" x pos1 pos2;
      assert (pos1 = 11 && pos2 = 17 && x = "result") in
  find "0123456789(result)..."


(* No real tuple (maybe not fixed yet) *)
let _ =
  match "abc", "def" with
      (RE _, RE _)
    | "abc", _ -> ignore `Case1
    | _ -> ignore `Case2


(* Assertions *)
let _ =
  let search = 
    SEARCH 
    <Not < _ > alpha{2}.> digit+ Possessive as m
    < %pos _ as x . Not (alpha | <Not "__"> "_"+ !m)+ > as n -> 
      printf "num = %s; pos = %i; x = %s\n%!" n pos x in
  search "abc1 23 456xyz 7. x8 33_33Y 34_35Y 36__36Y" 


let print_triplets_of_letters = SEARCH <alpha{3} as x> -> print_endline x
let _ = print_triplets_of_letters "Hello World!"

let _ = List.iter print_endline ((SPLIT "") "abc");;

RE test_warnings = 
  <Not "blop" as group_warning %pos_warning> ("a" as plus_warning)+

let _ = match "" with RE ("a" as star_warning)* test_warnings -> () | _ -> ()


(* Converters *)
let _ =
  let f s =
    printf "got it!\n";
    int_of_string s in
  let n =
    match "(123)", 456 with
	(RE "(" (digit+ as n := f) ")"), _ 
      | (RE _{1-3} as n = -1), _
      | (RE digit+ as n : int), _
      | _, n -> n in
  assert (n = 123);
  printf "123=%i\n%!" n


(* debugging *)
let _ =
  match "a" with
      (RE "b") | (RE "c") | (RE "a") -> ()
    | _ -> failwith "test failed"

let () = () in ();;



(* General syntax for local exception handling (let try ... in ... with ...) *)
let _ =
  try
    (let try x = ()
     and z = () in
       raise Exit
     with Exit -> assert false)
  with Exit -> 
    print_endline "OK for local exception handling (let-try-in-with)"
;;

let RE (_* as x) = "hello" in assert ("" <> "hello");;


(* Shortcut syntax *)
let _ =
  let RE (alpha+ as x) = "abc def" in
  assert (x = "abc");
  assert (x = "abc");
  print_endline "shortcut is OK"

(* Shortcut syntax with local exception handling *)
let _ =
  try
    (let try /"xy" as x/ = "xyz" in
       ignore x;
       raise Exit
     with Exit -> assert false)
  with Exit -> 
    print_endline "OK for local exception handling (RE)"

let /alpha+ space+ (alpha+ as x)/ = "xyz   abc  " in
assert (x = "abc");;


(* Similar tests for str_item's let in *)
let try /alpha+ space+ (alpha+ as x)/ = "" in 
  assert false
with Match_failure _ -> print_endline "OK for str_item let-try-in-with";;


(* Global value bindings *)
let /[digit "."]* as version/ = Sys.ocaml_version

RE int = digit+
let /(int as major : int) "." (int as minor : int) 
     ("." (int as patchlevel : int) | ("" as patchlevel = 0))
     ("+" (_* as additional_info) | ("" as additional_info))/ = 
  Sys.ocaml_version

let _ =
  printf "OCaml version: major=%i minor=%i patchlevel=%i additional-info=%S\n%!"
    major minor patchlevel additional_info


(* Parametrized regexps *)
let _ =
  let find s = 
    match "abcabcdefggghijkl" with
	RE _* Lazy ( @s+ as x) -> x
      | _ -> assert false in
  assert (find "abc" = "abcabc");
  assert (find "g" = "ggg")
  
let _ =
  let find_not_after x y =
    COLLECT < Not ( @x ) . > ":" @y "=" (alnum* as result) -> result in

  let text = "a:b=, xy:z=1, x:z=25, _:z=99" in
  assert (find_not_after "x" "z" text = ["1"; "99"]);
  assert (find_not_after "" "z" text = []);
  assert (find_not_after "a" "b" text = []);
  assert (find_not_after "a" "" text = []);
  assert (find_not_after "?" "b" text = [""])

let _ =
  let find_not_between ~before ~after ~label =
    COLLECT 
      < Not < ( @before ) . > @label "=" alnum* @after >
      @label "=" (alnum* as result) -> result in

  let text = "(field=12) (field=OK, field=) (field=yes" in
  assert (find_not_between ~before:"(" ~after:")" ~label:"field" text =
	    ["OK"; ""; "yes"])


let _ =
  let field key =
    printf "Case-insensitive search for field %S:\n%!" key;
    SEARCH @key~ "=" (alnum* as data) -> printf "  %s=%s\n%!" key data in

  let text = "hello name=Martin, AGE=27, Name=Jambon" in
  printf "Text: %S\n" text;
  field "name" text;
  field "age" text

(* Null character *)
let _ =
  match "" with
      / "\000abc" / -> assert false
    | _ -> ()

let _ =
  let zero_abc = "\000abc" in
  match "" with 
      / @zero_abc / -> assert false
    | _ -> ()

let ( % ) = (+)

let view T = fun x -> true
let view Lazy = fun x -> try Some (Lazy.force x) with _ -> None

let _ =
  match "a", lazy (1+1), lazy (3, lazy 4) with
      %T, %Lazy (2 as x), %Lazy (y, %Lazy z) -> 
	assert (x = 2);
	assert (y = 3);
	assert (z = 4);
	printf "Passed view test 1\n%!"
    | _ -> assert false



type 'a lazy_list = Empty | Cons of ('a * 'a lazy_list lazy_t)

let view Empty = 
    fun l ->
      try Lazy.force l = Empty
      with _ -> false

let view Cons = 
    fun l -> 
      try
	match Lazy.force l with 
	    Cons x -> Some x 
	  | Empty -> None
      with _ -> None

let _ =
  let l = lazy (Cons (1, lazy (Cons (2, lazy Empty)))) in
  match l with
      %Empty
    | %Cons (_, %Empty) -> assert false
    | %Cons (x1, %Cons (x2, %Empty)) ->
	assert (x1 = 1);
	assert (x2 = 2);
	printf "Passed view test 2\n%!"
    | _ -> assert false

let _ =
  let view XY = fun o -> Some (o#x, o#y) in
  let view S = fun o -> Some o#s in
  let o =
    (object 
       method x = 0 
       method y = 1 
       method s = "abc"
     end) in
  match o with
      %XY (1, _) -> assert false
    | %S / "A" / -> assert false
    | %S ( / upper as c / | / lower as c / as s) -> 
	assert (c = "a");
	assert (s = "abc");
	printf "Passed view test 3\n%!"

module Test =
struct
  let view Even = fun x -> x land 1 = 0
end

let _ =
  match 0 with
      %Test.Even -> printf "Passed view test 4\n%!"
    | _ -> assert false


let _ =
  let f = COLLECTOBJ (letter+ as x) (digit+ as y : int) in
  match List.map (fun x -> (x#x, x#y)) (f "ab12ER5") with
      [ ("ab", 12); ("ER", 5) ] -> printf "Passed COLLECTOBJ test\n%!"
    | _ -> assert false

let _ =
  match (CAPTURE (letter+ as x) (digit+ as y : int)) "ab12ER5" with
      Some o -> 
	assert (o#x = "ab" && o#y = 12); 
	printf "Passed CAPTURE test\n%!"
    | None -> assert false

let _ =
  match (SPLIT "x" ) "axbxc" with
      [ "a"; "b"; "c" ] ->
	printf "Passed basic SPLIT test\n%!"
    | _ -> assert false

let _ =
  match (SPLIT < "x" > ) "axbxc" with
      [ "a"; "xb"; "xc" ] ->
	printf "Passed zero-length SPLIT test (bug in versions <= 1.0.1)\n%!"
    | _ -> assert false
