(* $Id$ *)

exception Mikmatch_exit

open Pcre

let irflags = rflags []

external make_substrings : string * int array -> substrings = "%identity"

let search rex f ?(pos = 0) subj =
  let subgroup_offsets, offset_vector = make_ovector rex in
  let substrings = make_substrings (subj, offset_vector) in
  let subj_len = String.length subj in
  let rec loop cur_pos =
    if 
      try
	unsafe_pcre_exec 
	  irflags rex cur_pos subj subgroup_offsets offset_vector None; true
      with Not_found -> false
    then
      (f substrings;
       let first = offset_vector.(0) in
       let last = offset_vector.(1) in
       if first < subj_len then
	 loop (max (first + 1) last)) in
  loop pos

let scan ~full rex pos ~ftext ~fmatch subj =
  let subgroup_offsets, offset_vector = make_ovector rex in
  let substrings = make_substrings (subj, offset_vector) in
  let subj_len = String.length subj in
  let rec loop previous_last cur_pos =
    if 
      try
	unsafe_pcre_exec 
	  irflags rex cur_pos subj subgroup_offsets offset_vector None; true
      with Not_found -> 
	let last = String.length subj in
	if full || last > previous_last then
	  ftext (String.sub subj previous_last (last - previous_last));
	false
    then
      (let first = offset_vector.(0) in
       let last = offset_vector.(1) in
       if full || first > pos then
	 ftext (String.sub subj previous_last (first - previous_last));
       fmatch substrings;
       if first < subj_len then
	 loop last (max (first + 1) last)
       else if full then
	 ftext "") in
  loop pos pos

let map rex f ?(pos = 0) ?(full = true) subj =
  let l = ref [] in
  let ftext s = l := `Text s :: !l
  and fmatch substrings = l := f substrings :: !l in
  scan ~full rex pos ~ftext ~fmatch subj;
  List.rev !l

let collect rex f ?(pos = 0) subj =
  let l = ref [] in
  let f substrings = l := f substrings :: !l in
  search rex f ~pos subj;
  List.rev !l

let split rex ?(full = false) ?(pos = 0) subj =
  let l = ref [] in
  let ftext s = l := s :: !l
  and fmatch substrings = () in
  scan ~full rex pos ~ftext ~fmatch subj;
  List.rev !l

let bquote_char buf c =
  match c with 
      '\\' | '^' | '$' | '.' | '[' | ']' | '|' 
    | '(' | ')' | '?' | '*' | '+' | '{' | '}' -> Printf.bprintf buf "\\%c" c
    | '\000' -> Buffer.add_string buf "\\000"
    | c -> Buffer.add_char buf c

(* Pcre.quote does not escape null characters (which terminate C strings) *)
let quote_string s =
  let len = String.length s in
  let buf = Buffer.create (2 * len) in
  for i = 0 to len - 1 do
    bquote_char buf (String.unsafe_get s i)
  done;
  Buffer.contents buf

let nocase s =
  let len = String.length s in
  let buf = Buffer.create (2 * len) in
  for i = 0 to len - 1 do
    let c = s.[i] in
    let cl = Char.lowercase c
    and cu = Char.uppercase c in
    if cl <> cu then (* in this case, cl and cu are letters *)
      Printf.bprintf buf "[%c%c]" cl cu
    else
      bquote_char buf c
  done;
  Buffer.contents buf

module Mem =
struct
(* memoization table with periodic removal of old items *)

  type ('a, 'b) t = 
      { mutable date : float;
	mutable last_cleanup : float;
	opt_size : int;
	max_size : int;
	tbl : ('a, ('b * float ref)) Hashtbl.t }

  let create n =
    if n < 1 then invalid_arg "Memo.create"
    else
      { date = 0.;
	last_cleanup = 0.;
	opt_size = n;
	max_size = n + n;
	tbl = Hashtbl.create (n + n) }
	
  (* removal of anything which is too old *)
  let cleanup t =
    let t0 = t.last_cleanup in
    t.last_cleanup <- t.date;
    let tbl = t.tbl in
    let trash =
      Hashtbl.fold
	(fun key (data, last_access) trash ->
	   if !last_access < t0 then
	     key :: trash
	   else trash)
	tbl
	[] in
    List.iter (Hashtbl.remove tbl) trash
      
  (* unsafe addition of data (key should not be in the table) *)
  let unsafe_add t key data =
    let date = t.date +. 1. in
    t.date <- date;
    Hashtbl.add t.tbl key (data, ref date);
    let size = Hashtbl.length t.tbl in
    if size > t.max_size then
      cleanup t
    else
      if size = t.opt_size + 1 then
	t.last_cleanup <- t.date

  let add t key data =
    if Hashtbl.mem t.tbl key then
      invalid_arg "Memo.add"
    else
      unsafe_add t key data
	
  let find t key =
    let (data, last_access) = Hashtbl.find t.tbl key in
    let date = t.date in
    last_access := date;
    t.date <- date +. 1.;
    data
      
  let get t key lazy_data =
    try find t key
    with Not_found -> 
      let data = Lazy.force lazy_data in
      unsafe_add t key data;
      data
	
  let clear t =
    Hashtbl.clear t.tbl;
    t.date <- 0.;
    t.last_cleanup <- 0.

end
