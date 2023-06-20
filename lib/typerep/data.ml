open! Batteries
open Uref

type 'a bref = 'a _bref uref
and 'a _bref = {
  dat : 'a;
  back : fn;
}

and fn = <
  dec : costack array;
  bot : costack;
  inc : costack array;
>

and 'a seq = 'a _seq bref
and 'a _seq = 
  | Null
  | Push of 'a seq * 'a
  | Next of int

and costack = stack seq
and stack = value seq

and value = _value bref
and _value = 
  | Lit of lit
  | Con of con * fn
  | Var of int

and lit = 
  | Int
  | String

and con = 
  | Quote
  | List

let mk_null_fn () = object (self)
  method private mk () = uref {
    dat = Null;
    back = self;
  }
  method dec = [|self#mk ()|]
  method bot = self#mk ()
  method inc = [|self#mk ()|]
end
