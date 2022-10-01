open Batteries
open Lexing

type loc = position * position

type 'a content = <
  ast : 'a;
  loc : loc;
>

let create ast loc = object
  method ast = ast
  method loc = loc
end

module type Wrapper = sig
  type 'a t
end

module Wrap(W : Wrapper) = struct

  type e = _e W.t
  and _e = w list list
  and w = _w W.t
  and _w = 
    | IntLit of int
    | CharLit of char
    | QuoteLit of e
    | ListLit of e list
    | Id of string
    | Expr of e

  type s = s_ W.t
  and s_ = string * e

  type p = (string * e) W.t list

end

module ContentAst = Wrap(struct
  type 'a t = 'a content
end)
