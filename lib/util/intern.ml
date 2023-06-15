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

let intern str t = 
  HDict.find_option t.hdict str
  |> Option.default_delayed @@ fun () -> 
    let nu = unique () in
    HDict.add t.hdict str nu;
    HMap.add t.hmap nu str;
    nu

let get_int str t = HDict.find t.hdict str
let get_str int t = HMap.find t.hmap int
