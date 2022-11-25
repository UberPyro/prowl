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
