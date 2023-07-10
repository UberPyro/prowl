open! Batteries

open struct
  type 'a t = (string * 'a) list [@@deriving show]
end

include Hashtbl.Make(struct include Hashtbl include String end)
let pp h fmt = to_list %> pp h fmt
