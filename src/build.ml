open Batteries

open Ast

(* FIXME: Track & Include entire path!! *)

type file = 
  | File of string * string
  | Folder of string * string * file list

let rec load_file ?(path="/") fn = 
  if Sys.is_directory (path ^ fn)
  then
    Sys.readdir (path ^ fn)
    |> Array.map (load_file ~path:(path ^ fn ^ "/"))
    |> Array.to_list
    |> fun x -> Folder (path, fn, x)
  else File (path, fn)

let rec ast_of_file = function
  | File (path, fn) -> File.open_in (path ^ fn) |> Gen.parse
  | Folder (_, sfn, foln) -> (Pub, (Mod (List.map begin fun fn -> 
    let am, (_, loc as e) = ast_of_file fn in 
    Def (am, false, (PId sfn, loc), e, None), loc
  end foln), Interpret.dum))

let endow lib (am, (_, loc1 as e)) = 
  let (_, (_, loc as em)) = lib |> load_file |> ast_of_file in
  am, 
  (Let (["", false, (POpen false, loc), em], (Access (e, lib), loc1)), loc)
