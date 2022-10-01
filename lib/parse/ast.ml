open Batteries
open Lexing

type loc = position * position

type 'a wrap = <
  ast : 'a; 
>

type e = e wrap
and _e = w list list
and w = _w wrap
and _w = 
  | IntLit of int
  | CharLit of char
  | QuoteLit of e
  | ListLit of e list
  | Id of string

type s = s_ wrap
and s_ = string * e

type p = (string * e) list
