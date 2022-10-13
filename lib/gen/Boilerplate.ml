(**
   Boilerplate to be used as a template when mapping the prowl CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_escape_sequence (env : env) (x : CST.escape_sequence) =
  (match x with
  | `Blank () -> todo env ()
  )

let map_op4 (env : env) (tok : CST.op4) =
  (* pattern [*\/%][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok

let map_op0 (env : env) (tok : CST.op0) =
  (* pattern \|[~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok

let rec map_comment (env : env) ((v1, v2, v3) : CST.comment) =
  let v1 = (* "/*" *) token env v1 in
  let v2 =
    (match v2 with
    | `Blank () -> todo env ()
    | `Comm x -> map_comment env x
    )
  in
  let v3 = (* "*/" *) token env v3 in
  todo env (v1, v2, v3)

let map_op3 (env : env) (tok : CST.op3) =
  (* pattern [+\-][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok

let map_op1 (env : env) (tok : CST.op1) =
  (* pattern [$&=][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok

let map_op5 (env : env) (tok : CST.op5) =
  (* pattern [\.\^#][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok

let map_uop (env : env) (tok : CST.uop) =
  (* pattern [~!?][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok

let map_id (env : env) (tok : CST.id) =
  (* pattern "[a-z](-?[A-Za-z0-9_'])*" *) token env tok

let map_op2 (env : env) (tok : CST.op2) =
  (* pattern [@:][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok

let map_int_ (env : env) (tok : CST.int_) =
  (* pattern 0|[1-9][0-9]* *) token env tok

let map_string_content (env : env) (x : CST.string_content) =
  (match x with
  | `SPACE tok -> (* " " *) token env tok
  | `LF tok -> (* "\n" *) token env tok
  | `HT tok -> (* "\t" *) token env tok
  | `Blank () -> todo env ()
  | `Esc_seq x -> map_escape_sequence env x
  )

let map_character_content (env : env) (x : CST.character_content) =
  (match x with
  | `Blank () -> todo env ()
  | `Esc_seq x -> map_escape_sequence env x
  )

let map_bop (env : env) (x : CST.bop) =
  (match x with
  | `Op0 tok ->
      (* pattern \|[~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok
  | `Op1 tok ->
      (* pattern [$&=][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok
  | `Op2 tok ->
      (* pattern [@:][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok
  | `Op3 tok ->
      (* pattern [+\-][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok
  | `Op4 tok ->
      (* pattern [*\/%][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok
  | `Op5 tok ->
      (* pattern [\.\^#][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env tok
  )

let rec map_expr (env : env) (x : CST.expr) =
  (match x with
  | `Rep1_word xs -> List.map (map_word env) xs
  | `Expr_op5_expr (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 =
        (* pattern [\.\^#][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env v2
      in
      let v3 = map_expr env v3 in
      todo env (v1, v2, v3)
  | `Expr_uop (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 =
        (* pattern [~!?][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env v2
      in
      todo env (v1, v2)
  | `Expr_op4_expr (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 =
        (* pattern [*\/%][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env v2
      in
      let v3 = map_expr env v3 in
      todo env (v1, v2, v3)
  | `Expr_op3_expr (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 =
        (* pattern [+\-][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env v2
      in
      let v3 = map_expr env v3 in
      todo env (v1, v2, v3)
  | `Expr_op2_expr (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 =
        (* pattern [@:][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env v2
      in
      let v3 = map_expr env v3 in
      todo env (v1, v2, v3)
  | `Expr_op1_expr (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 =
        (* pattern [$&=][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env v2
      in
      let v3 = map_expr env v3 in
      todo env (v1, v2, v3)
  | `Let_opt_rec_rep1_id_EQ_expr_in_expr (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = (* "let" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "rec" *) token env tok
        | None -> todo env ())
      in
      let v3 =
        List.map (token env (* pattern "[a-z](-?[A-Za-z0-9_'])*" *)) v3
      in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_expr env v5 in
      let v6 = (* "in" *) token env v6 in
      let v7 = map_expr env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `As_rep1_id_DASHGT_expr (v1, v2, v3, v4) ->
      let v1 = (* "as" *) token env v1 in
      let v2 =
        List.map (token env (* pattern "[a-z](-?[A-Za-z0-9_'])*" *)) v2
      in
      let v3 = (* "->" *) token env v3 in
      let v4 = map_expr env v4 in
      todo env (v1, v2, v3, v4)
  | `Expr_op0_expr (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 =
        (* pattern \|[~!@#$%^&*\-=+\.?:<>|/\\]* *) token env v2
      in
      let v3 = map_expr env v3 in
      todo env (v1, v2, v3)
  )

and map_word (env : env) (x : CST.word) =
  (match x with
  | `Int tok -> (* pattern 0|[1-9][0-9]* *) token env tok
  | `Char (v1, v2, v3) ->
      let v1 = (* "'" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_character_content env x
        | None -> todo env ())
      in
      let v3 = (* "'" *) token env v3 in
      todo env (v1, v2, v3)
  | `LBRACK_expr_RBRACK (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expr env v2 in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  | `Blank_expr_blank (v1, v2, v3) ->
      let v1 = blank env v1 in
      let v2 = map_expr env v2 in
      let v3 = blank env v3 in
      todo env (v1, v2, v3)
  | `LCURL_opt_COMMA_expr_rep_COMMA_expr_opt_COMMA_RCURL (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
      in
      let v3 = map_expr env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expr env v2 in
          todo env (v1, v2)
        ) v4
      in
      let v5 =
        (match v5 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
      in
      let v6 = (* "}" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `LBRACK_RBRACK (v1, v2) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = (* "]" *) token env v2 in
      todo env (v1, v2)
  | `Blank_blank (v1, v2) ->
      let v1 = blank env v1 in
      let v2 = blank env v2 in
      todo env (v1, v2)
  | `LCURL_RCURL (v1, v2) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = (* "}" *) token env v2 in
      todo env (v1, v2)
  | `Str (v1, v2, v3) ->
      let v1 = (* "\"" *) token env v1 in
      let v2 = List.map (map_string_content env) v2 in
      let v3 = (* "\"" *) token env v3 in
      todo env (v1, v2, v3)
  | `Id tok ->
      (* pattern "[a-z](-?[A-Za-z0-9_'])*" *) token env tok
  | `Blank_bop_expr_blank (v1, v2, v3, v4) ->
      let v1 = blank env v1 in
      let v2 = map_bop env v2 in
      let v3 = map_expr env v3 in
      let v4 = blank env v4 in
      todo env (v1, v2, v3, v4)
  | `Blank_expr_bop_blank (v1, v2, v3, v4) ->
      let v1 = blank env v1 in
      let v2 = map_expr env v2 in
      let v3 = map_bop env v3 in
      let v4 = blank env v4 in
      todo env (v1, v2, v3, v4)
  | `Blank_bop_blank (v1, v2, v3) ->
      let v1 = blank env v1 in
      let v2 = map_bop env v2 in
      let v3 = blank env v3 in
      todo env (v1, v2, v3)
  | `Blank_uop_blank (v1, v2, v3) ->
      let v1 = blank env v1 in
      let v2 =
        (* pattern [~!?][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env v2
      in
      let v3 = blank env v3 in
      todo env (v1, v2, v3)
  | `LBRACK_bop_expr_RBRACK (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_bop env v2 in
      let v3 = map_expr env v3 in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `LBRACK_expr_bop_RBRACK (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expr env v2 in
      let v3 = map_bop env v3 in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `LBRACK_bop_RBRACK (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_bop env v2 in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  | `LBRACK_uop_RBRACK (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (* pattern [~!?][~!@#$%^&*\-=+\.?:<>|/\\]* *) token env v2
      in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  )

let map_source_file (env : env) (opt : CST.source_file) =
  (match opt with
  | Some x -> map_expr env x
  | None -> todo env ())
