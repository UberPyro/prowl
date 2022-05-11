{ open Batteries

  open Parse_comb }

let num = '0' | ['1'-'9']['0'-'9']*

(* Lexes the stack combinators *)
rule token = parse
  | "^" (num as n) {DUP (int_of_string n)}
  | "_" (num as n) {ZAP (int_of_string n)}
  | "%" (num as n) {ROT (int_of_string n)}
  | "$" (num as n) {RUN (int_of_string n)}
  | "^" {DUP 1}
  | "_" {ZAP 1}
  | "%" {ROT 2}
  | "$" {RUN 1}
  | eof {EOF}
