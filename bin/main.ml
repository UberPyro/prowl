open Batteries

open Prowl
open Cli

open Gen

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | file :: _ -> File.open_in file |> parse |> ignore
