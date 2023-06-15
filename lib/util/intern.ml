open! Batteries

module HDict = Hashtbl.Make(String)
module HMap = Hashtbl.Make(struct
  include Hashtbl
  include Int
end)

type t = {
  hdict : int HDict.t;
  hmap : string HMap.t;
}

let mk_intern () = {
  hdict = HDict.create 16;
  hmap = HMap.create 16;
}

let intern t str = 
  HDict.find_option t.hdict str
  |> Option.default_delayed @@ fun () -> 
    let nu = unique () in
    HDict.add t.hdict str nu;
    HMap.add t.hmap nu str;
    nu

let get_int t str = HDict.find t.hdict str
let get_str t int = HMap.find t.hmap int
