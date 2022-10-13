open Span

exception Prowl_error of string * span list

(* span ascription *)
let ascr node span = node, object
  method span = span
end

type 'a expr = 'a _expr * 'a
and 'a _expr = 
  | Cat of 'a word list
  | Bop of 'a expr * string * 'a expr
  | Uop of 'a expr * string
  | Let of [`Seq | `Rec] * string * string list * 'a expr * 'a expr
  | As of string list * 'a expr
and 'a word = 'a _word * 'a
and 'a _word = 
  | Int of int
  | Char of char
  | Quote of 'a expr
  | Group of 'a expr
  | List of 'a expr list
  | Id of string
  | SectLeft of string * 'a expr
  | SectRight of 'a expr * string
