open! Batteries

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

let op = 
  P.make
    ~prog:"Prowl Interpreter"
    ()

let flag help long_name = 
  let st = default_opt (ref (Some false)) true in
  P.add op ~help ~long_name st;
  st

let flags help long_name short_name = 
  let st = default_opt (ref (Some false)) true in
  P.add op ~help ~long_name ~short_name st;
  st
