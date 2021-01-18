let print_location lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "file: %s, line %d, column %d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  
  
let () =
  let l = Lexing.from_string "{aaa: 10.0, bbb: null, ccc: {ddd: {}, eee: 100}}" in
  let p =
    try
      Parser.prog Lexer.token l
    with _ -> print_endline @@ print_location l; failwith "a" in
  print_endline @@ Util.show_object p
