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
  type 'a t [@@deriving show]
end

module Wrap(W : Wrapper) = struct

  type e = _e W.t [@@deriving show]
  and _e = w list list
  and w = _w W.t
  and _w = 
    | IntLit of int
    | CharLit of char
    | QuoteLit of e
    | ListLit of e list
    | Id of string
    | Expr of e

  type p = (string * e) W.t list [@@deriving show]

end

module PPNotImplemented = struct
  let pp = failwith "pretty printing not implemented for content"
  let show = failwith "show not implemented for content"
end

module ContentAst = Wrap(struct
  type 'a t = 'a content
  include PPNotImplemented
end)

module Sast = Wrap(struct
  type 'a t = 'a [@@deriving show]
end)

module Simplify = struct
  let rec p y : Sast.p = List.map (fun x -> let a, b = x#ast in a, e b) y
  and e y = _e y#ast
  and _e y = List.map (List.map w) y
  and w y = _w y#ast
  and _w = function
    | ContentAst.QuoteLit z -> Sast.QuoteLit (e z)
    | ListLit z -> Sast.ListLit (List.map e z)
    | Expr z -> Expr (e z)
    | IntLit i -> Sast.IntLit i
    | CharLit c -> Sast.CharLit c
    | Id z -> Sast.Id z
end

let print_ast z = Sast.pp_p z % Simplify.p
let show_ast : <ast : string * ContentAst.e; loc : loc> list -> string = 
    Sast.show_p % Simplify.p
