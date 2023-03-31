type lit = [
  | `id of string
  | `int of int
  | `string of string
] [@@deriving show]

type op = [
  | `gen | `fab | `elim | `exch
  | `swap | `unit | `call | `zap | `dup
  | `cat

  | `eq | `neq | `lt | `gt | `le | `ge | `not
  | `add | `sub | `mul | `div | `rem | `neg
  | `concat | `mk
  | `parse | `show
]

type 'a dag = [ `dag of 'a ]
