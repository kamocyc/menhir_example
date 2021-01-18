let print_location lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "file: %s, line %d, column %d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  
  
let () =
  let lexbuf = Lexing.from_string "{aaa: 10.0, bbb: null, ccc: {ddd: {}, eee: 100}}" in
  (* let lexbuf = Lexing.from_string "{aaa: 10.0, bbb: null#, ccc: {ddd: {}, eee: 100}}" in *)
  (* let lexbuf = Lexing.from_string "{aaa: 10.0, bbb: null ccc: {ddd: {}, eee: 100}}" in *)
  let p =
    try Parser.prog Lexer.token lexbuf with
    | (Lexer.SyntaxError msg) as a ->
      print_endline msg;
      print_endline @@ print_location lexbuf;
      raise a
    | Parser.Error as b->
      print_string "Parse rrror: ";
      print_endline @@ print_location lexbuf;
      raise b in
  print_endline @@ Util.show_object p
