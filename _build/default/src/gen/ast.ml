type spec = 
  | Spec of string * ty
  | SType of string * ty
  | SAbsTy of string
  | SData of string * data

and ty = constraints list * ty_body

and constraints = string * string list

and ty_body = 
  | TId of string
  | TSq of ty_body list
  | TFn of ty_body * ty_body
  | TArr of ty_body list
  | TQuoted of ty_body
  | TUnit
  | SMetaType of string
  | Sig of spec list

and data = 
  | DVariant of (ty * string) list
  | DRecord of (string * ty) list
  | DPolyRecord of (string * ty) list

type annot = [`Annotation of ty]

type access_mods = [`Pub]
type ty_access_mods = [`Opaq | access_mods]

type ty_imp_mods = [`Imp]
type imp_mods = [`Inst | ty_imp_mods]

type fn_mods = [`Rec | access_mods | imp_mods | annot]
type val_mods = [`Rec | access_mods | imp_mods | annot]
type ty_mods = [`New | ty_access_mods | ty_imp_mods]
type incl_mods = [`Inst]

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

and expr = 
  | Sq of expr list
  | Id of string
  | Spaced of string * expr
  | PSpaced of string * expr
  | Macro of string * expr
  | Let of stmt
  | To of ty
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

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | And of expr * expr
  | Or of expr * expr

  | Eq of expr * expr
  | Neq of expr * expr
  | Cmp of expr * expr

  | Cons of expr * expr
  | Snoc of expr * expr
  | Lift
  | LiftA
  | LiftM
  | Fold
  | FoldL
  | FoldR
  | FilterMap
  | Dup
  | Mut

and pat = 
  | PId of string
  | PSq of pat list
  | PCons of pat * pat
  | PSnoc of pat * pat
  | Alternate of pat *pat
  | PTuple of pat list
  | PRecord of (string * pat) list
  | PPolyRecord of (string * pat) list
  | PArr of pat list
  | PDict of (expr * pat) list
  | PQuoted of pat
  | PAs of pat * pat

and named_arg = string * expr option
