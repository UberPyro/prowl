type e = w list list
and w = 
  | IntLit of int
  | CharLit of char
  | QuoteLit of e
  | ListLit of e list
  | Id of string

type p = (string * e) list
