open! Batteries
open Uref

type tlit = Int | String
type tcon = Quote | List

type v = v_contents uref
and v_contents = 
  | TLit of tlit
  | TCon of tcon * dc * dc
  | TMeta of int

and s = v Ull.t
and ds = s * s

and c = ds Ull.t
and dc = c * c


