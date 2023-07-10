open! Batteries
open Printf
open Uref

open System

open Util
open Ull

let rec pretty_value out = uget %> function
  | Lit Int -> fprintf out "z"
  | Lit String -> fprintf out "str"
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
  | USeq k -> fprintf out "s%d*" k
  | UNil -> fprintf out "."

and pretty_costack out = uget %> function
  | UCons (u, us) -> 
    pretty_costack out us;
    fprintf out " | ";
    pretty_stack out u
  | USeq k -> fprintf out "c%d+" k
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
