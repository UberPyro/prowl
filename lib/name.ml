open! Batteries

module M = struct
  type t = string [@@deriving show]
end

type t = int

let store : string DynArray.t = DynArray.create ()

let note s = 
  let x = DynArray.length store in
  DynArray.add store s;
  x

let show = DynArray.get store
let pp fmt = show %> M.pp fmt
