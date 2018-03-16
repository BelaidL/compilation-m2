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
let fresh_cont_id : unit -> T.function_identifier = Gensym.make "_K"

type val_def = S.identifier * S.expression

type fun_def = S.function_identifier * S.formals * S.expression

module VarSet = Set.Make (struct
    type t = T.identifier
    let compare = compare
  end)

let free_simple_variables : S.simplexpr -> VarSet.t = function
  | S.Var v -> VarSet.singleton v
  | _ -> VarSet.empty

let rec free_variables : S.expression -> VarSet.t = function
  | S.Simple e -> free_simple_variables e
  | S.Let (id,e1,e2) ->
     let freeV_e1, freeV_e2  = free_variables e1, free_variables e2 in
     VarSet.union freeV_e1 (VarSet.remove id freeV_e2)

  | S.IfThenElse (c,e1,e2) -> 
     let freeV_c, freeV_e1, freeV_e2 = 
       free_simple_variables c, free_variables e1, free_variables e2 in
     VarSet.union freeV_c (VarSet.union freeV_e1 freeV_e2)

  | S.BinOp (binop,e1,e2) -> 
     let freeVar_e1, freeVar_e2  = 
       free_simple_variables e1, free_simple_variables e2 in
     VarSet.union freeVar_e1 freeVar_e2
       
  | S.BlockNew n -> free_simple_variables n 
  | S.BlockGet (t,i) -> 
     let freeVar_t, freeVar_i = 
       free_simple_variables t, free_simple_variables i in
     VarSet.union freeVar_t freeVar_i

  | S.BlockSet (t,i,e) -> 
     let freeV_t, freeV_i, freeV_e = 
       free_simple_variables t,free_simple_variables i, free_simple_variables e
     in
     VarSet.union freeV_t (VarSet.union freeV_i freeV_e)

  | S.FunCall (f,args) ->
     let freeV_f, freeV_args = 
       free_simple_variables f, List.map free_simple_variables args in
     VarSet.union freeV_f (List.fold_left VarSet.union VarSet.empty freeV_args)

  | S.Print s -> VarSet.empty

let translate_simplexpr : S.simplexpr -> T.basicexpr = function
  | S.Num i -> T.Num i
  | S.FunName f -> T.FunName f
  | S.Var v -> T.Var v

let rec as_basicexpr : S.expression -> T.basicexpr option = function
  | S.Simple e -> Some(translate_simplexpr e)
  | S.Let (id,e1,e2) -> None
  | S.IfThenElse _ -> None
  | S.BinOp (binop,e1,e2) -> 
     let e1' = translate_simplexpr e1 in
     let e2' = translate_simplexpr e2 in
     Some(T.BinOp (binop,e1',e2'))

  | S.BlockNew n -> Some(T.BlockNew (translate_simplexpr n))
  | S.BlockGet (t,i) -> 
     let t' = translate_simplexpr t in
     let i' = translate_simplexpr i in
     Some(T.BlockGet (t',i'))

  | S.BlockSet (t,i,e) -> 
     let t' = translate_simplexpr t in
     let i' = translate_simplexpr i in
     let e' = translate_simplexpr e in
     Some(T.BlockSet (t',i',e'))

  | S.FunCall _ -> None
  | S.Print s -> Some(T.Print s)

let rec translate_expression :
  S.expression -> T.tailexpr * T.definition list = fun e ->
  match as_basicexpr e with
  | Some e -> (T.TContCall e, [])
  | None -> (
      match e with
      | S.Let (x, e, e') -> (
          match as_basicexpr e, as_basicexpr e' with
          | Some e, Some e' -> (T.TContCall (T.Let(x, e, e')), [])
          | Some e, None ->
              let ce', kdefs' = translate_expression e' in
              (T.TLet (x, e, ce'), kdefs')
          | _ ->
              let kid = fresh_cont_id () in
              let fvs = (* The free variables of [e'] except [x] *)
                free_variables e'
                |> VarSet.remove x
                |> VarSet.elements
              in
              let ce, kdefs = translate_expression e in
              let ce', kdefs' = translate_expression e' in
              let kdef = T.DefCont (kid, fvs, x, ce') in
              (T.TPushCont (kid, fvs, ce), kdef :: kdefs @ kdefs')
        )
      | S.IfThenElse _ -> failwith "TODO"
      | S.FunCall (e,args) -> (
        T.TFunCall (translate_simplexpr e,List.map translate_simplexpr args),[]
         )

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
  let fdefs = ExtStd.List.flat_map translate_fun_def fdefs in
  let main, fdefs' = build_main vdefs in
  ((fdefs @ fdefs', main), env)
