open! Batteries
open Uref

type 'a bref = 'a _bref uref
and 'a _bref = {
  dat : 'a;
  back : fn Lazy.t;
}

and fn = {
  dec : costack array;
  bot : costack;
  inc : costack array;
}

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

let mk_null_fn () = 
  let rec lazy_null_fn = 
    let mk_null_costack () = 
      uref {
        dat = Null;
        back = lazy_null_fn;
      } in
    (* each is only computed once *)
    lazy {
      dec = [|mk_null_costack ()|];
      bot = mk_null_costack ();
      inc = [|mk_null_costack ()|];
    } in
  Lazy.force lazy_null_fn

let mk_free_fn () = 
  let rec lazy_free_fn = 
    let mk_free_costack () = 
      uref {
        dat = Push (
          uref {
            dat = Next (unique ());
            back = lazy_free_fn;
          }, 
          uref {
            dat = Next (unique ());
            back = lazy_free_fn
          }
        );
        back = lazy_free_fn;
      } in
    lazy {
      dec = [|mk_free_costack ()|];
      bot = mk_free_costack ();
      inc = [|mk_free_costack ()|];
    } in
  Lazy.force lazy_free_fn
