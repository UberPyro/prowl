open! Batteries

type t = 
  | Name of string
  | Id of int
  [@@deriving show, eq]

exception OccursError of string

let name s = Name s
let num i = Id i

let c = ref (-1)
let fresh () = 
  incr c;
  num !c

let refresher () = object
  val tbl = Hashtbl.create 8
  method freshen (m : t) = 
    Hashtbl.find_option tbl m
    |> Option.default_delayed @@ fresh %> fun n -> 
      Hashtbl.add tbl m n;
      n
end
