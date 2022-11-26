open Batteries
open Lexing

open Util

type span = (int * int) * (int * int)

type 'a node = <
  ast : 'a;
  span : span;
>

type 'a expr_node = <
  ast : 'a;
  span : span;
  ty : Type.t;
>

type 'a mod_node = <
  ast : 'a;
  span : span;
  sign : Type.t Dict.t;
>

let lincol_of_pos p = p.pos_lnum, (p.pos_cnum - p.pos_bol)
let span_of_loc (p1, p2) = lincol_of_pos p1, lincol_of_pos p2

let node ast loc = object
  method ast = ast
  method span = span_of_loc loc
end

let exnode ast loc ty = object
  method ast = ast
  method span = span_of_loc loc
  method ty = ty
end

let modnode ast loc sign = object
  method ast = ast
  method span = span_of_loc loc
  method sign = sign
end
