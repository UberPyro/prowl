open Batteries

open Ast
open Util

type file = 
  | File of string * string
  | Folder of string * file list

let rec load_file ?(path="") fn = 
  if Sys.is_directory (path ^ fn)
  then
    Sys.readdir (path ^ fn)
    |> Array.map (load_file ~path:(path ^ fn ^ "/"))
    |> Array.to_list
    |> fun x -> Folder (fn, x)
  else File (path, fn)

let rec def_of_file = function
  | File (path, fn) ->
    File.open_in (path ^ fn)
    |> Gen.parse
    |> fun (am, (_, loc as e1)) -> 
    Def (am, false, (PId (Filename.remove_extension fn), loc), e1, None)
  | Folder (sn, lst) -> 
    lst
    |> List.map (fun fn -> def_of_file fn, dum)
    |> fun def_lst -> 
    Def (Pub, false, (PId sn, dum), (Mod def_lst, dum), None)

let endow lib (am, e) = 
  lib
  |> load_file
  |> def_of_file
  |> function (Def (_, _, _, (_, loc as m), _)) -> am, (
    Cat [m; As ("", (POpen false, loc), e), loc], loc
  ) | _ -> failwith "Did not retrieve definition"
