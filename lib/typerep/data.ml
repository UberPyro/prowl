open! Batteries
open Uref

let pp_uref fmt x y = fmt x (uget y)

type 'a bref = 'a _bref uref
and 'a _bref = {
  dat : 'a;
  back : fn Lazy.t array;
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
    [|lazy begin
      let mk_null_costack = 
        uref {
          dat = Null;
          back = lazy_null_fn;
        } in {
          dec = [|mk_null_costack|];
          bot = mk_null_costack;
          inc = [|mk_null_costack|];
      } end|] in
  Array.map Lazy.force lazy_null_fn

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
    [|lazy {
        dec = [|mk_free_costack ()|];
        bot = mk_free_costack ();
        inc = [|mk_free_costack ()|];
    }|] in
  Array.map Lazy.force lazy_free_fn

let append_ref_uniq x = 
  Array.append x % Array.filter begin fun z -> 
    Array.for_all (fun w -> z != w) x
  end

let backunite x sel' = 
  x |> unite ~sel:begin fun x y -> {
    dat = sel' x.dat y.dat;
    back = append_ref_uniq x.back y.back;
  } end

let rec unify_seq unify_elem x = backunite x % curry @@ function
  | Next _ as s, _ | _, (Next _ as s) -> s
  | Push (us, u) as s, Push (vs, v) -> 
    unify_elem u v;
    unify_seq unify_elem us vs;
    s
  | Push _, Null | Null, Push _ -> 
    failwith "Cannot unify differently sized sequences"
  | Null, Null -> Null
