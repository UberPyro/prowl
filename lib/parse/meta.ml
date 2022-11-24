type span = (int * int) * (int * int)

type 'a node = <
  ast : 'a;
  span : span;
>

type 'a expr_node = <
  ast : 'a;
  span : span;
  (* Need typing structures *)
>
