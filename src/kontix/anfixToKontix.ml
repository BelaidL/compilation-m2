(** This module implements a compiler from Anfix to Kontix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Anfix
module S = Source.AST
module Target = Kontix
module T = Target.AST

type environment = unit (* TODO *)

let initial_environment () = () (* TODO *)

(* Generate a fresh continuation function identifier.  *)
let fresh_cont_id () : T.function_identifier = failwith "TODO"

type val_def = S.function_identifier * S.expression

type fun_def = S.function_identifier * S.formals * S.expression

module VarSet = Set.Make (struct
    type t = T.identifier
    let compare = compare
  end)

let free_variables : S.expression -> VarSet.t = function
  | S.Simple _ -> failwith "TODO"
  | S.Let _ -> failwith "TODO"
  | S.IfThenElse _ -> failwith "TODO"
  | S.BinOp _ -> failwith "TODO"
  | S.BlockNew _ -> failwith "TODO"
  | S.BlockGet _ -> failwith "TODO"
  | S.BlockSet _ -> failwith "TODO"
  | S.FunCall _ -> failwith "TODO"
  | S.Print _ -> failwith "TODO"

let as_basicexpr : S.expression -> T.basicexpr option = function
  | S.Simple _ -> failwith "TODO"
  | S.Let _ -> failwith "TODO"
  | S.IfThenElse _ -> failwith "TODO"
  | S.BinOp _ -> failwith "TODO"
  | S.BlockNew _ -> failwith "TODO"
  | S.BlockGet _ -> failwith "TODO"
  | S.BlockSet _ -> failwith "TODO"
  | S.FunCall _ -> failwith "TODO"
  | S.Print _ -> failwith "TODO"

let translate_simplexpr : S.simplexpr -> T.basicexpr = function
  | S.Num _ -> failwith "TODO"
  | S.FunName _ -> failwith "TODO"
  | S.Var _ -> failwith "TODO"

let rec translate_expression :
  S.expression -> T.tailexpr * T.definition list = fun e ->
  match as_basicexpr e with
  | Some _ -> failwith "TODO"
  | None -> (
      match e with
      | S.Let _ -> failwith "TODO"
      | S.IfThenElse _ -> failwith "TODO"
      | S.Simple _ | S.BinOp _ | S.BlockNew _ | S.BlockGet _ | S.BlockSet _
      | S.FunCall _ | S.Print _ ->
          assert false
    )

let build_main (vdefs : val_def list) : (T.tailexpr * T.definition list) =
  failwith "TODO"

let translate_fun_def (fdef : fun_def) : T.definition list =
  failwith "TODO"

let split_program (prog : S.t) : fun_def list * val_def list =
  failwith "TODO"

let translate (p : S.t) env =
  let fdefs, vdefs = split_program p in
  let fdefs =
    fdefs
    |> List.map translate_fun_def
    |> List.flatten
  in
  let main, fdefs' = build_main vdefs in
  ((fdefs @ fdefs', main), env)
