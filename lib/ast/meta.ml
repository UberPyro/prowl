type t = 
  | Name of string
  | Id of int
  [@@deriving show, eq]

let name s = Name s
let num i = Id i
