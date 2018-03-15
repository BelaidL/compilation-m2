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

type val_def = S.identifier * S.expression

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
      | S.Let (x, e, e') ->
          let ce', kdefs' = translate_expression e' in (
            match as_basicexpr e with
            | Some e -> (T.TLet (x, e, ce'), kdefs')
            | None ->
                let kid = fresh_cont_id () in
                let fvs = (* The free variables of [e'] except [x] *)
                  free_variables e'
                  |> VarSet.remove x
                  |> VarSet.elements
                in
                let ce, kdefs = translate_expression e in
                let kdef = T.DefCont (kid, fvs, x, ce') in
                (T.TPushCont (kid, fvs, ce), kdef :: kdefs @ kdefs')
          )
      | S.IfThenElse _ -> failwith "TODO"
      | S.FunCall _ -> failwith "TODO"
      | S.Simple _ | S.BinOp _ | S.BlockNew _ | S.BlockGet _ | S.BlockSet _ |
        S.Print _ ->
          assert false
    )

(* Remark: At the moment, this function assumes that the order of the
   value definitions in [vdefs] is the same as in the input Anfix
   program, i.e. the function [split_program] preserves the order of
   value definitions.

   The idea of this function is to build an Anfix expression
   representing the Anfix top-level value definitions in [vdefs] and
   then compile this expression to Kontix.  More precisely:

   if [vdefs = [(x_1, e_1); ...; (x_(n-1), e_(n-1)); (x_n, e_n)]], then
   we build the following Anfix expression and compile it:

   let x_1 = e_1 in
   ...
   let x_(n-1) = e_(n-1) in
   e_n

*)
let build_main (vdefs : val_def list) : (T.tailexpr * T.definition list) =
  let main_expr =
    match List.rev vdefs with
    | [] -> failwith "No entry point"
    | (_, final_expr) :: vdefs ->
        List.fold_left (fun acc (x, e) -> S.Let (x, e, acc)) final_expr vdefs
  in
  translate_expression main_expr

let translate_fun_def (fdef : fun_def) : T.definition list =
  failwith "TODO"

(* Remark: With the current implementation of [build_main], the order of
   value definitions must be preserved by this function.  *)
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
