open Batteries
open Uref

open Hir
open Type

type ty_meta = <ty : Costack.t * Costack.t>

let int i : ty_meta Hir.word = ascr (Int i) (lit (uref @@ Var.Mono "int"))
let char c : ty_meta Hir.word = ascr (Char c) (lit (uref @@ Var.Mono "char"))
let expr e = ascr e Costack.(fresh (), fresh ())
let _quote e : ty_meta Hir.word = 
  let i, o = (snd e)#ty in
  ascr (Quote e) (lit (uref @@ Var.Duo ("quote", i, o)))
let quote = expr % _quote
let _list es : ty_meta Hir.word = 
  let q = match es with
    | [] -> Costack.(fresh (), fresh ())
    | h :: _ -> 
      let i, o = (snd h)#ty in
      lit (uref @@ Var.Duo ("list", i, o)) in
  ascr (List es) q
let list = expr % _list
let id x = ascr (Id x) Costack.(fresh (), fresh ())
let builtin x ty : ty_meta Hir.word = ascr (Id x) ty
