open Sexplib.Conv

type spec = 
  | Spec of string * ty
  | SType of string * ty
  | SAbsTy of string
  | SData of string * data
  [@@deriving sexp]

and ty = constraints list * ty_body [@@deriving sexp]

and constraints = string * string list [@@deriving sexp]

and ty_body = 
  | TId of string
  | TSq of ty_body list
  | TFn of ty_body * ty_body
  | TArr of ty_body list
  | TQuoted of ty_body
  | TUnit
  | SMetaType of string
  | Sig of spec list
  | TWith of ty
  [@@deriving sexp]

and data = 
  | DVariant of (ty * string) list
  | DRecord of (string * ty) list
  | DPolyRecord of (string * ty) list
  [@@deriving sexp]

type annot = [`Annotation of ty]                        [@@deriving sexp]

type access_mods = [`Pub]                               [@@deriving sexp]
type ty_access_mods = [`Opaq | access_mods]             [@@deriving sexp]

type ty_imp_mods = [`Imp]                               [@@deriving sexp]
type imp_mods = [`Inst | ty_imp_mods]                   [@@deriving sexp]

type fn_mods = [`Rec | access_mods | imp_mods | annot]  [@@deriving sexp]
type val_mods = [`Rec | access_mods | imp_mods | annot] [@@deriving sexp]
type ty_mods = [`New | ty_access_mods | ty_imp_mods]    [@@deriving sexp]
type incl_mods = [`Inst]                                [@@deriving sexp]

type stmt = 
  | Fn of fn_mods list * pat * named_arg list * string * expr
  | Val of fn_mods list * pat * expr
  | Open of expr
  | Incl of incl_mods list * expr
  | Imp of expr
  | Ty of ty_mods list * string * ty
  | AbsTy of string
  | Data of ty_mods list * string * data
  | Begin of stmt list
  [@@deriving sexp]

and expr = 
  | Sq of expr list
  | Id of string
  | Spaced of string * expr
  | PSpaced of string * expr
  | Macro of string * expr
  | Let of stmt
  | To of ty
  | With of ty
  | Metatype of string
  | Mod of stmt list

  | Variant of string
  | PolyVariant of string
  | Record of (string * expr) list
  | PolyRecord of (string * expr) list
  | Tuple of expr list
  | StrLit of string
  | IntLit of int
  | FloatLit of float
  | CharLit of char
  | UnitLit

  | ArrayLit of expr list
  | FuncLit of (pat * expr) list
  | Quoted of expr

  | Bop of expr * bop * expr
  | Sect of bop
  | SectL of bop * expr
  | SectR of expr * bop
  | SectCmp of cmp

  | Lift
  | LiftA
  | LiftM
  | Fold
  | FoldL
  | FoldR
  | FilterMap
  | Dup
  | Mut
  [@@deriving sexp]

and bop = 
  | Add | Sub | Mul | Div | And | Or
  | Eq | Neq | Cmp
  | Cons | Snoc
  [@@deriving sexp]

and cmp = 
  | Gt | Lt | Ge | Le
  [@@deriving sexp]

and pat = pat_body * expr list [@@deriving sexp]

and pat_body = 
  | PId of string
  | WildCard
  | PActive of expr
  | PSq of pat_body list

  | IntPat of int
  | FloatPat of float
  | CharPat of char
  | StrPat of string
  | UnitPat

  | PVariant of string
  | PPolyVariant of string
  | PTuple of pat_body list
  | PRecord of (string * pat_body) list
  | PPolyRecord of (string * pat_body) list

  | PArr of pat_body list
  | PDict of (expr * pat_body) list
  | PQuoted of pat_body

  | PBop of pat_body * pbop * pat_body
  | PCmp of pcmp
  [@@deriving sexp]

and pbop = 
  | PCons
  | PSnoc
  | POr
  | PAnd
  [@@deriving sexp]

and pcmp = 
  | PLt | PGt | PLe | PGe
  | PEq | PNeq
  [@@deriving sexp]

and named_arg = string * expr option [@@deriving sexp]
