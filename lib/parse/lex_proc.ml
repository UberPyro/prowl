(* open Batteries
open Lexing

let tabsize = ref 2

let advance lexbuf = 
  let lcp = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    lcp with
    pos_cnum = lcp.pos_cnum + !tabsize - 1
  }

let utf8encode s =
  let prefs = [|0x0; 0xc0; 0xe0|] in
  let s1 n = String.make 1 (Char.chr n) in

  let rec ienc k sofar resid =
    let bct = if k = 0 then 7 else 6 - k in
    if resid < 1 lsl bct then (s1 (prefs.(k) + resid)) ^ sofar
    else ienc (k + 1) (s1 (0x80 + resid mod 64) ^ sofar) (resid / 64) in
  
  ienc 0 "" (int_of_string ("0x" ^ s))

let decode =
    let re = Str.regexp "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]" in
    let subst = function
      | Str.Delim u -> utf8encode (String.sub u 2 4)
      | Str.Text t -> t in
    Str.full_split re
    %> List.map subst
    %> String.concat ""

let decode_char = 
  decode
  %> String.to_seq
  %> Seq.hd *)
