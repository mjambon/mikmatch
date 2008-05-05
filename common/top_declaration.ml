let add, flush =
  let accu = ref [] in
  let add str_item = 
    accu := str_item :: !accu 
  in
  let flush () =
    let result = <:str_item< $ List.rev !accu $ >> in
    accu := [];
    result
  in
  add, flush


let install_syntax () =

  EXTEND Camlp4.PreCast.Gram
    
  GLOBAL: str_item;

  str_item: FIRST [
    [ si = NEXT -> add si; flush () ]
  ];

  END
