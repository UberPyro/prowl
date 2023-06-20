open! Batteries
open Uref

module LazyList = struct
  include LazyList
  open struct
    type 'a t = 'a list [@@deriving show]
  end
  let pp f fmt = LazyList.to_list %> pp f fmt
end

let pp_uref fmt x y = fmt x (uget y)

type 'a bref = 'a _bref uref
and 'a _bref = {
  dat : 'a;
  back : fn LazyList.t;
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
  [@@deriving show]

let mk_null_fn () = 
  let rec lazy_null_fn = 
    lazy begin
      let mk_null_costack = 
        uref {
          dat = Null;
          back = lazy_null_fn;
        } in
      Cons (
        {
          dec = [|mk_null_costack|];
          bot = mk_null_costack;
          inc = [|mk_null_costack|];
        }, 
        LazyList.nil
      ) end in
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
    (* each uref should only be constructed once *)
    lazy begin Cons (
      {
        dec = [|mk_free_costack ()|];
        bot = mk_free_costack ();
        inc = [|mk_free_costack ()|];
      }, 
      LazyList.nil
    ) end in
  Lazy.force lazy_free_fn
