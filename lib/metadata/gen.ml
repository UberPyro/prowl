open! Batteries

module IM = Hashtbl.Make(struct
  include Hashtbl
  include Int
end)

let freshen (memo : int IM.t) i = 
  Option.default_delayed (fun () -> 
    let nu = unique () in
    IM.add memo i nu;
    nu) @@ IM.find_option memo i

let mk_cache () : int IM.t = IM.create 16
