open Batteries

let parse ch = 
  let lexbuf = Lexing.from_channel ch in
  try Parse.program Lex.token lexbuf with
  | _ ->
    let p = lexbuf.lex_curr_p in
    Printf.sprintf
      "Unexpected Token at [%d,%d]"
      p.pos_lnum (p.pos_cnum - p.pos_bol)
    |> failwith
