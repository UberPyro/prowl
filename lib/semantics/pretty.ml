open! Batteries
open Printf
open Uref

open System

open Util
open Ull

let rec pretty_value out = uget %> function
  | Lit Int -> fprintf out "z"
  | Lit String -> fprintf out "str"
  | Lit Void -> fprintf out "void"
  | Con (fn, Quote) -> 
    fprintf out "[";
    pretty_fn out fn;
    fprintf out "]"
  | Con (fn, List) -> 
    fprintf out "{";
    pretty_fn out fn;
    fprintf out "}"
  | Var k -> fprintf out "V%d" k

and pretty_stack out = uget %> function
  | UCons (u, us) -> 
    pretty_stack out us;
    fprintf out " ";
    pretty_value out u
  | USeq k -> fprintf out "'%d" k
  | UNil -> fprintf out "."

and pretty_costack out = uget %> function
  | UCons (u, us) -> 
    pretty_costack out us;
    fprintf out " | ";
    pretty_stack out u
  | USeq k -> fprintf out "\"%d" k
  | UNil -> fprintf out "$"

and pretty_fn out (i, o) = 
  pretty_costack out i;
  fprintf out " -- ";
  pretty_costack out o

let pretty_ctx out = 
  fprintf out "Context:\n";
  Ouro.to_list %> List.iter begin fun (s, (g, i, o)) -> 
    fprintf out "%s %s " s @@ if g then "=>" else "->";
    pretty_fn out (i, o);
    fprintf out "\n"
  end

let pretty_uctx out = 
  fprintf out "UVar Context:\n";
  Dict.to_list %> List.iter begin fun (s, v) -> 
    fprintf out "%s -> " s;
    pretty_value out v;
    fprintf out "\n"
  end

module Show = struct

  let sh f v = 
    let out = IO.output_string () in
    f out v;
    IO.close_out out
  
  let str_value x = sh pretty_value x
  let str_stack x = sh pretty_stack x
  let str_costack x = sh pretty_costack x
  let str_fn x = sh pretty_fn x
  let str_ctx x = sh pretty_ctx x
  let str_uctx x = sh pretty_uctx x

end
