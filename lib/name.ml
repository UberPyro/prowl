open! Batteries

type t = int

let store : string DynArray.t = DynArray.create ()

let note s = 
  let x = DynArray.length store in
  DynArray.add store s;
  x

let view x = DynArray.get store x
