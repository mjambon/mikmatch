module Text =
struct

exception Internal_exit

let iter_lines_of_channel f ic =
  try
    while true do
      let line =
	try input_line ic
	with End_of_file -> raise Internal_exit in
      f line
    done
  with Internal_exit -> ()


let iter_lines_of_file f file =
  let ic = open_in file in
  try 
    iter_lines_of_channel f ic;
    close_in ic
  with exn ->
    close_in_noerr ic;
    raise exn


let lines_of_channel ic =
  let l = ref [] in
  iter_lines_of_channel (fun line -> l := line :: !l) ic;
  List.rev !l

let lines_of_file file =
  let l = ref [] in
  iter_lines_of_file (fun line -> l := line :: !l) file;
  List.rev !l

let channel_contents ic =
  let len = 2048 in
  let buf = String.create len in
  let rec loop size accu =
    match input ic buf 0 len with
	0 -> (accu, size)
      | n when n = len -> loop (size + n) (String.copy buf :: accu)
      | n -> loop (size + n) (String.sub buf 0 n :: accu) in

  let accu, size = loop 0 [] in
  let result = String.create size in
  let rec loop2 last_pos = function
      [] -> assert (last_pos = 0)
    | s :: rest ->
	let len = String.length s in
	let pos = last_pos - len in
	String.blit s 0 result pos len;
	loop2 pos rest in
  loop2 size accu;
  result


let file_contents ?(bin = false) file =
  let ic = open_in file in
  let s = 
    try channel_contents ic
    with exn ->
      close_in_noerr ic; 
      raise exn in
  close_in ic;
  s

let save file data =
  let oc = open_out_bin file in
  (try
     output_string oc data;
   with exn -> 
     close_out_noerr oc;
     raise exn);
  close_out oc

let save_lines file lines =
  let oc = open_out_bin file in
  (try
     List.iter (fun s -> output_string oc s; output_char oc '\n') lines;
   with exn -> 
     close_out_noerr oc;
     raise exn);
  close_out oc


exception Skip

let rev_map f l =
  let rec loop f accu = function
      [] -> accu
    | hd :: tl -> 
	let accu' = 
	  try f hd :: accu
	  with Skip -> accu in
	loop f accu' tl in
  loop f [] l

let map f l =
  List.rev (rev_map f l)

let rec fold_left f accu l =
  match l with
      [] -> accu
    | hd :: tl -> 
	let accu' = 
	  try f accu hd
	  with Skip -> accu in
	fold_left f accu' tl

let rec rev_fold_right f l accu =
  match l with
      [] -> accu
    | hd :: tl -> 
	let accu' = 
	  try f hd accu
	  with Skip -> accu in
	rev_fold_right f tl accu'

let rec fold_right f l accu = 
  rev_fold_right f (List.rev l) accu

let map_lines_of_channel f ic =
  let l = ref [] in
  iter_lines_of_channel (fun line -> 
			   try l := f line :: !l
			   with Skip -> ()) ic;
  List.rev !l

let map_lines_of_file f file =
  let l = ref [] in
  iter_lines_of_file (fun line ->
			try l := f line :: !l
			with Skip -> ()) file;
  List.rev !l

end

module Fixed =
struct

let chop_spaces str =
  let len = String.length str in
  let rec getfirst n =
    if n = len then len
    else
      if String.unsafe_get str n = ' '
      then getfirst (n+1)
      else n
  and getlast n =
    if String.unsafe_get str n = ' '
    then getlast (n-1)
    else n in
  let first = getfirst 0 in
  if first = len then ""
  else 
    let last = getlast (len - 1) in
    String.sub str first (last-first+1)

let int s = int_of_string (chop_spaces s)
let float s = float_of_string (chop_spaces s)

end

module Directory =
struct

let list ?(absolute = false) ?path dir =
  let names =  Sys.readdir dir in
  Array.sort String.compare names;

  let make_path, path_maker =
    match absolute, path with
	false, None 
      | false, Some false -> false, (fun s -> s)
      |	false, Some true -> true, Filename.concat dir
      | true, Some true 
      | true, None -> 
	  let f =
	    if Filename.is_relative dir then
	      let cwd = Sys.getcwd () in
	      Filename.concat (Filename.concat cwd dir)
	    else Filename.concat dir in
	  true, f
      | true, Some false -> invalid_arg "Directory.list" in

  let paths = 
    if make_path then Array.map path_maker names
    else names in
  Array.to_list paths

let is_dir ?(nofollow = false) x =
  let stat = if nofollow then Unix.lstat else Unix.stat in
  try (stat x).Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error (Unix.ENOENT, _, _) -> 
    false (* may be a bad symbolic link if nofollow is false *)

end

module Glob =
struct
(* Filename globbing utility *)

(*
  Examples of use with mikmatch:

  let ml_files = list [FILTER _* ".ml" "i"? eos]
  let trash_files = 
    list [ FILTER ""; 
	   FILTER _* (".cm" ("i"|"o"|"x"|"a"|"xa") | ".o" | ".a") eos ]
*)

let filter_array f a =
  Array.fold_right (fun x l -> if f x then x :: l else l) a []

let rec scan_gen ~cons ~real_dir ~dir ?nofollow action path_filter =
  let real_real_dir = 
    if real_dir = "" then Filename.current_dir_name else real_dir in
  match path_filter with
      [] -> ()
    | [f] -> 
	List.iter (fun name -> action (cons dir name))
	  (filter_array f (Sys.readdir real_real_dir))
    | f :: subpath_filter -> 
	let filtered_files =
	  filter_array f (Sys.readdir real_real_dir) in
	List.iter
	  (fun name -> 
	     let subdir = cons dir name in
	     let real_subdir = Filename.concat real_dir name in
	     if Directory.is_dir ?nofollow real_subdir then 
	       scan_gen 
		 ~cons
		 ~real_dir:real_subdir
		 ~dir:subdir ?nofollow action subpath_filter)
	  filtered_files

let get_dir ~getcwd ~concat ~is_relative ~fun_name ~absolute ~path 
  ~relative_root root =
  match absolute, path with
      false, None 
    | false, Some false -> relative_root
    | false, Some true -> root
    | true, Some true 
    | true, None -> 
	if is_relative root then
	  let cwd = getcwd () in
	  concat cwd root
	else root
    | true, Some false -> invalid_arg fun_name

let scan ?(absolute = false) ?path ?(root = "") ?nofollow
  action path_filter = 
  let getcwd = Sys.getcwd in
  let cons = Filename.concat in
  let concat = Filename.concat in
  let is_relative = Filename.is_relative in
  let fun_name = "Glob.scan" in
  let relative_root = "" in
  let dir = 
    get_dir ~getcwd ~concat ~is_relative ~fun_name 
      ~absolute ~path ~relative_root root in
  scan_gen ~cons ~real_dir:root ~dir ?nofollow action path_filter


let lscan ?(rev = false) ?(absolute = false) ?path ?(root = []) ?nofollow 
  action path_filter = 
  let getcwd () = [Sys.getcwd ()] in
  let cons = (fun l s -> s :: l) in
  let concat = (fun root rel_path -> rel_path @ root) in
  let rec is_relative = function
      [] -> true
    | [x] -> Filename.is_relative x
    | x :: l -> is_relative l in
  let fun_name = "Glob.lscan" in
  let relative_root = [] in
  let rev_root = if rev then root else List.rev root in
  let rev_dir =
    get_dir ~getcwd ~concat ~is_relative ~fun_name 
      ~absolute ~path ~relative_root rev_root in
  let real_dir = List.fold_left Filename.concat "" (List.rev rev_dir) in
  let new_action =
    if rev then action
    else (fun l -> action (List.rev l)) in
  scan_gen ~cons ~real_dir ~dir:rev_dir ?nofollow new_action path_filter


let list_gen scan ?absolute ?path ?root ?nofollow ?(sort = true) path_filter =
  let l = ref [] in
  scan ?absolute ?path ?root ?nofollow (fun x -> l := x :: !l) path_filter;
  if sort then List.sort compare !l
  else !l

let list = list_gen scan
let llist ?rev = list_gen (lscan ?rev)
end
