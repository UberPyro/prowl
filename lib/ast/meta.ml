type t = 
  | Name of string
  | Id of int
  [@@deriving show, eq]

let c = ref (-1)
let uniq () = 
  incr c;
  Id !c

let name s = Name s
let num i = Id i
