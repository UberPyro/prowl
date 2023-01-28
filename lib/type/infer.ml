open! Batteries

open Type
open Ast

let rec fix f n = f (fix f) n

let rec expr env (e_, _, io0) = match e_ with
  | Cat [] -> connect_self io0
  | Cat ((_, _, io) :: _ as es) -> 
    connect_in io0 io;
    List.iter (expr env) es; 
    es |> fix begin fun cat -> function
      | (_, _, io1) :: (_, _, io2) :: _ as t -> 
        connect io1 io2;
        cat t
      | [_, _, io] -> connect_out io0 io
      | [] -> failwith "Unreachable"
    end

  | _ -> failwith "todo"
