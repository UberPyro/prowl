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

(* let rec mk_null_seq () = uref {
  dat = uref Null;
  back = 
} *)

(* let mk_free_fn () = 
  let rec fn = 
    let mk_unit_costack () = 
      (* uref {
        dat = Push (uref {dat = Next (unique ()); back = lazy fn}, uref {
          dat = uref {dat = Next (unique ()); back = lazy fn};
          back = lazy fn;
        });
        back = lazy fn;
      }  *)

      uref {
        back = lazy fn;
        dat = Null;
      }
    
    
    in
    {
      dec = [|mk_unit_costack ()|];
      bot = mk_unit_costack ();
      inc = [|mk_unit_costack ()|];
    } in
  fn *)
