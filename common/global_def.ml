(*pp camlp4orf *)
(* $Id$ *)

open Camlp4.PreCast

let init get =
  let rewrite si0 =
    let _loc = Ast.loc_of_str_item si0 in
    let tbl = Hashtbl.create 10 in
    let accu = ref <:str_item< >> in
    let iter =
      (object
	 inherit Ast.map as super
	 method expr e =
	   let _loc = Ast.loc_of_expr e in
	   (match super#expr e with
	      | <:expr< $lid:id$ >> -> 
		(match get _loc id with
		     Some si ->
		       if not (Hashtbl.mem tbl id) then
			 (Hashtbl.add tbl id ();
			  accu := <:str_item< $!accu$ ; $si$ >>)
			   
		   | None -> ())
		  
	      | _ -> ());
	   e
       end)
    in
    ignore (iter # str_item si0);
    <:str_item< $!accu$ ; $si0$ >>
  in

  AstFilters.register_str_item_filter rewrite;
  AstFilters.register_topphrase_filter rewrite


let init_from_table tbl =
  let get _loc id = 
    try Some (Hashtbl.find tbl id)
    with Not_found -> None
  in
  init get
