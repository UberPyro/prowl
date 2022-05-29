open Format
open Lexing

type 'a loc = 'a * (position * position)

let span_flag = ref false
let span_of_pos p = p.pos_lnum, p.pos_cnum - p.pos_bol
let multispan_of_pos (p1, p2) = 
  let x1, y1 = span_of_pos p1 in
  let x2, y2 = span_of_pos p2 in
  x1, y1, x2, y2

let pp_loc c f (a, loc) = 
  c f a; 
  let x1, y1, x2, y2 = multispan_of_pos loc in
  begin 
    if !span_flag
    then Printf.sprintf " [%d:%d => %d:%d]" x1 y1 x2 y2
    else ""
  end |> pp_print_string f

type access_mod = Priv | Opaq | Pub [@@deriving show]

and sp = sp_t loc [@@deriving show]
and sp_t = 
  | SDef of string * ty
  | STy of string * string list * ty option
  | SData of string * string list * data
  [@@deriving show]

and ty = ty_t loc [@@deriving show]
and ty_t = ty_term * ty_term [@@deriving show]
and ty_term = ty_term_t loc [@@deriving show]
and ty_term_t = 
  | TId of string
  | TGen of string
  | TAccess of ty_term * string
  | TCat of ty_term list
  | TCapture of ty
  | TList of ty
  | TMap of ty_term * ty
  | TUnit
  | TVoid
  | TBin of ty list list
  | TSig of sp list
  | TMod of sp list
  | TImpl of string * ty
  [@@deriving show]

and data = data_t loc [@@deriving show]
and data_t = (ty_term list * string) list list [@@deriving show]

and s = s_t loc [@@deriving show]
and s_t = 
  | Def of access_mod * implicit * p * e * ty option
  | Open of implicit * e
  | Mix of e
  | Ty of access_mod * string * string list * ty option
  | Data of access_mod * string * string list * data
  [@@deriving show]

and implicit = bool

and greed = Gre | Rel | Cut [@@deriving show]
and quant = 
  | Opt
  | Plus
  | Star
  | Num of e
  | Min of e
  | Max of e
  | Range of e * e
  [@@deriving show]

and stack_comb = stack_comb_t loc [@@deriving show]
and stack_comb_t = 
  | Dup of int
  | Zap of int
  | Rot of int
  | Run of int
  [@@deriving show]

and e = e_t loc [@@deriving show]
and e_t = 
  | Id of string
  | Access of e * string
  | Get of e * e
  (* Todo: Data & Tuple access *)

  | Int of int
  | Flo of float
  | Char of char
  | Str of string
  | Unit

  | List of e list
  | Map of (e * e) list
  | Pair of e * e
  | Left of e
  | Right of e
  | EData of string
  | Prod of e list
  | Mod of s list
  | Impl of e
  | Capture of e

  | Sect of string
  | SectLeft of string * e
  | SectRight of e * string

  | Cat of e list
  | Bop of e * string * e
  | StackComb of stack_comb list

  | Let of (string * implicit * p * e) list * e
  | As of string * p * e

  | Quant of e * quant * greed
  | Case of (greed * e) list
  | Inv of e list
  | Span of e * e

  | Noncap of e
  | Cap of e
  | Atomic of e
  [@@deriving show]

and p = p_t loc [@@deriving show]
and p_t = 
  | PId of string
  | PAccess of p * string
  | PBlank
  | PCat of p list
  | PAsc of p * ty
  | PImpl of p * ty
  | POpen of implicit
  | PUse

  | PInt of int
  | PFlo of float
  | PStr of string
  | PChar of char
  | PUnit

  | PList of p list
  | PMap of (e * p) list
  | PPair of p * p
  | PLeft of p
  | PRight of p
  | PData of string
  | PProd of p list
  | PCapture of p

  | PBop of p * string * p
  [@@deriving show]

and program = access_mod * e [@@deriving show]
