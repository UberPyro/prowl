type lit = [
  | `int of int
  | `string of string
] [@@deriving show]

type word = [
  | lit
  | `id of string
] [@@deriving show]

type op = [
  | `gen | `fab | `elim | `exch
  | `swap | `unit | `call | `zap | `dup | `cat
  | `dis | `star | `mark

  | `eq | `neq | `lt | `gt | `le | `ge | `not
  | `add | `sub | `mul | `div | `rem | `neg
  | `concat | `mk
  | `parse | `show
] [@@deriving show]

type 'a dag = [ `dag of 'a ] [@@deriving show]
