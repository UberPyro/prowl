open Batteries
open Lexing

open Util
open Ast

let tabsize = ref 2

type mode = Cat | Regex
let mode = ref Cat

let advance lexbuf = 
  let lcp = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    {lcp with pos_cnum = lcp.pos_cnum + !tabsize - 1}

let set_cat () = mode := Cat
let set_regex () = mode := Regex
let is_cat a b = 
  if !mode == Cat
  then a else b

let utf8encode s =
  let prefs = [|0x0; 0xc0; 0xe0|] in
  let s1 n = String.make 1 (Char.chr n) in

  let rec ienc k sofar resid =
    let bct = if k = 0 then 7 else 6 - k in
    if resid < 1 lsl bct then (s1 (prefs.(k) + resid)) ^ sofar
    else ienc (k + 1) (s1 (0x80 + resid mod 64) ^ sofar) (resid / 64) in
  
  ienc 0 "" (int_of_string ("0x" ^ s))

let decode s =
    let re = Str.regexp "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]" in
    let subst = function
      | Str.Delim u -> utf8encode (String.sub u 2 4)
      | Str.Text t -> t in
    String.concat "" (List.map subst (Str.full_split re s))

let decode_char = 
  decode
  >> String.to_seq
  >> Seq.hd

let parse_comb = Parse_comb.parse (Lex_comb.token (from_string s))

let parse_quant q g =
  begin match q with
    | "?" -> Opt
    | "+" -> Plus
    | "*" -> Star
    | _ -> failwith (Printf.sprintf "Unknown quantifier %s" q)
  end, begin match g with
    | "" -> Gre
    | "?" -> Rel
    | "+" -> Cut
    | _ -> failwith (Printf.sprintf "Unknown greediness %s" g)
  end

let parse_greed = function
  | "" -> Gre
  | "?" -> Rel
  | "+" -> Cut
  | _ -> failwith (Printf.sprintf "Unknown greediness %s" g)
