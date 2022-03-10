open Batteries
(* open Sexplib *)

open Lib

module S = BatOptParse.StdOpt
module O = BatOptParse.Opt
module P = BatOptParse.OptParser

let default_opt var const = O.{
    option_metavars = [];
    option_defhelp = None;
    option_get = (fun _ -> !var);
    option_set_value = (fun x -> var := Some x);
    option_set = (fun _ _ -> var := Some const)
  }
let sexp_out = default_opt (ref (Some false)) true
let op = 
  P.make
   ~prog:"Prowl Compiler"
   ()

let () = 
  P.add op
    ~help: "output AST in s-expressions"
    ~long_name: "sexp"
    sexp_out

let () = 
  match P.parse_argv op with
  | [] -> P.usage op ()
  | lst -> 
    (* let is_sexp = O.get sexp_out in *)
    List.map begin fun file -> 
      (* let name, _ = Filename.split_extension file in *)
      let ast = Gen.ast (File.open_in file) in
      (* if is_sexp then
        ast
        |> Ast.sexp_of_stmt
        |> Sexp.to_string_hum
        |> Enum.singleton
        |> File.write_lines (name ^ "_sexp.txt") *)
        ignore ast
    end lst |> ignore
