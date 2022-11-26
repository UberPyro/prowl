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

let node ast span = object
  method ast = ast
  method span = span
end

let exnode ast span ty = object
  method ast = ast
  method span = span
  method ty = ty
end

let modnode ast span sign = object
  method ast = ast
  method span = span
  method sign = sign
end
