{ open Batteries
  open Lexing     }

num = 0 | [1-9][0-9]*

(* Lexes the stack combinators *)
rule comb = parse
  | "^" (num as n) {DUP n}
  | "_" (num as n) {ZAP n}
  | "%" (num as n) {ROT n}
  | "$" (num as n) {RUN n}
  | "^" {DUP 1}
  | "_" {ZAP 1}
  | "%" {ROT 1}
  | "$" {RUN 1}
