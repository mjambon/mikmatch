open Printf

let _ = function RE "" -> ()

let x = 1 in ();;
let / alpha / = "a" in ();;

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
      (RE (("a" as a) | ("b" as a))) | a -> a

let hello_who s =
  match s with
      RE _* ['h''H']"ello" ","? space* 
      ((word | space)* word as someone) -> String.capitalize someone
	
    | _ -> "nobody"

let _ =
  printf "Extraction of the recipient's name\n";
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
      | RE '-'? digit{5-} eol -> printf "%S has at least 5 digits.\n" s
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

(* Lower level feature: RE_STR returns the source of the regexp,
   to be used with specific compilation or search options. *)

let _ =
  let (src, named_groups) = 
    RE_STR 
      (("a"|"b"+)? digit{2}) as start 
      (space* word)+ ( digit{1} (word as last_word)
		     | digit{3} (word as last_word)) in
  
  printf "Str regexp source: %S\n" src;
  printf "Named groups and their locations:\n";
  print_named_groups named_groups;
  flush stdout

let charset_union = RE_STR digit | space | "a" | ['A'-'Z']


let _ =
  printf "Debugging test 1:\n"; flush stdout;
  match ["Coucou Martin"] with
      [ (RE (word as x) space (word as y)) | ("zobi" as x as y) ]
    | ("abc" as x :: y :: _) -> printf "Trop cool x=%S y=%S\n" x y
    | _ -> printf "Bof bof...\n"

let _ =
  printf "Debugging test 2:\n"; flush stdout;
  match ["Hello"; "!"], 123 with
       "***" :: _, _ -> printf "Hop!\n"
	 
     | RE word as w :: RE _ as c :: _, (122|123) when w <> "Bye" -> 
	 printf "Cool: %S %S\n" w c
	   
     | [RE ""], _ -> printf "Glouglou\n"
     | _ -> printf "Sorry\n"

let _ =
  printf "Debugging test 3:\n"; flush stdout;
  (match "hello" with
       (RE ' '{10})
     | RE _* ' '{10} eol -> ()
     | _ -> ());
  printf "Passed.\n"

let _ =
  match Some "x" with
      Some ((RE "a") | ("b"|"c")) -> true
    | _ -> false

let _ =
  match "axxxxyz", 333 with 
      RE "a" as s ("bc" %pos | "x"+ %pos) (_* as s'), _ 
    | (s as s', pos) -> 
	printf "%s, %i, %s\n%!" s pos s'

let _ =
  match "123" with
      RE digit+ as n := fun _ -> 1 -> n
    | _ -> 2



(* Parametrized regexps *)
let _ =
  let find s = 
    match "abbcdefgghijkl" with
	RE _* @s @s -> assert true
      | _ -> assert false in
  find "b";
  find "g"

