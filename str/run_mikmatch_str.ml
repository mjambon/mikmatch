exception Mikmatch_exit

let nocase s =
  let len = String.length s in
  let buf = Buffer.create (4 * len) in
  for i = 0 to len - 1 do
    let c = s.[i] in
    let cl = Char.lowercase c
    and cu = Char.uppercase c in
    if cl <> cu then (* in this case, cl and cu are letters *)
      Printf.bprintf buf "[%c%c]" cl cu
    else
      Buffer.add_string buf (Str.quote (String.make 1 c))
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
