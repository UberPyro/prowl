open Batteries

open Ast

type file = 
  | File of string
  | Folder of string * file list

let rec load_file fn = 
  if Sys.is_directory fn
  then
    Sys.readdir fn
    |> Array.map load_file
    |> Array.to_list
    |> fun x -> Folder (fn, x)
  else File fn

let rec ast_of_file = function
  | File fn -> File.open_in fn |> Gen.parse
  | Folder (sfn, foln) -> (Pub, (Mod (List.map begin fun fn -> 
    let am, (_, loc as e) = ast_of_file fn in 
    Def (am, false, (PId sfn, loc), e, None), loc
  end foln), Interpret.dum))
