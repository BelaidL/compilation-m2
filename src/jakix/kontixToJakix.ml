(** This module implements a compiler from Kontix to Fopix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Kontix
module S = Source.AST
module Target = Jakix
module T = Target.AST
module Dispatcher = FopixToJavix.Dispatcher
module Labels = FopixToJavix.Labels

type environment = {
  nextvar          : int;
  variables        : (S.identifier * T.var) list;
  function_labels  : (S.function_identifier * T.label) list;
  (** [function_formals] maintains the relation between function identifiers
      and their formal arguments. *)
  function_formals : (S.function_identifier * S.formals) list;
  cont_stack       : (S.function_identifier * T.label) list;
  cont_args        : (S.function_identifier * (S.formal_env * S.identifier)) list;
}


module Env = 
struct
    include FopixToJavix.Env

    let bind_cont_args cont_id formals_env identifier env = {
      env with cont_args = (cont_id,(formals_env,identifier)) :: env.cont_args
    }


    let bind_cont_stack_label cont_id env = {
      env with cont_stack =
        (cont_id, fresh_function_label cont_id) :: env.cont_stack
    }

    let lookup_cont_args cont_id env = 
      List.assoc cont_id env.cont_args

    let current_cont env =
      match env.cont_stack with
      | [] -> None
      | cont :: conts -> Some cont

end
 
module Utils : sig 
    val translate_binop      : S.binop -> T.instruction
    val translate_cmpop      : S.binop -> T.cmpop
    val unlabelled_instr     : T.instruction -> T.labelled_instruction
    val unlabelled_instrs    : (T.instruction list) -> (T.labelled_instruction list)
    val labelled_instr       : T.label -> T.instruction -> T.labelled_instruction
    val labelled_instrs      : T.label -> (T.instruction list) -> 
(T.labelled_instruction list)

    val new_label            : string -> T.label
    val fun_epilog           : (T.labelled_instruction list)

end = struct

    let translate_binop   = FopixToJavix.translate_binop
    let translate_cmpop   = FopixToJavix.translate_cmpop
    let unlabelled_instr  = FopixToJavix.unlabelled_instr
    let unlabelled_instrs = FopixToJavix.unlabelled_instrs
    let labelled_instr    = FopixToJavix.labelled_instr
    let labelled_instrs   = FopixToJavix.labelled_instrs
    let new_label         = FopixToJavix.new_label
    let fun_epilog        = FopixToJavix.fun_epilog

end

(** Initially, the environment is empty. *)
let initial_environment () = {
  nextvar          = 0;
  variables        = [];
  function_labels  = [];
  function_formals = [];
  cont_stack       = [];
  cont_args        = [];
}

let basic_code code varSize stackSize = {
  T.classname = "Fopix";
  T.code = code;
  T.varsize = varSize;
  T.stacksize = stackSize;
}

let rec translate_basicexpr (expr: S.basicexpr) (env: environment) :
(T.labelled_instruction list) * environment = 
  match expr with
  | S.Num _ -> failwith "TODO"
  | S.FunName _ -> failwith "TODO"
  | S.Var _ -> failwith "TODO"
  | S.Let _ -> failwith "TODO"
  | S.IfThenElse _ -> failwith "TODO"
  | S.BinOp _ -> failwith "TODO"
  | S.BlockNew _ -> failwith "TODO"
  | S.BlockGet _ -> failwith "TODO"
  | S.BlockSet _ -> failwith "TODO"
  | S.Print _ -> failwith "TODO"

let rec translate_tailexpr (expr: S.tailexpr) (env: environment) : 
(T.labelled_instruction list) * environment = 
  match expr with
  | S.TLet _ -> failwith "TODO"
  | S.TIfThenElse _ -> failwith "TODO"
  | S.TPushCont _ -> failwith "TODO"
  | S.TFunCall _ -> failwith "TODO"
  | S.TContCall _ -> failwith "TODO"

let translate_def_fun : S.function_identifier -> S.formals -> S.tailexpr -> 
(T.labelled_instruction list) = failwith "TODO"

let translate_def_cont : S.function_identifier -> S.formal_env -> 
S.identifier -> S.tailexpr -> (T.labelled_instruction list) = failwith "TODO" 

let tarnslate_definition (def:S.definition) (env: environment) : 
(T.labelled_instruction list) * environment =
  match def with
  | S.DefFun _ -> failwith "TODO"
  | S.DefCont _ -> failwith "TODO"

let varAndStack_size (p: S.t) (env: environment) : int * int = failwith "TODO"

let rec translate (p : S.t) env : T.t * environment = 
  let defs,main = p in
  let main_code, env' = translate_tailexpr main env in
  let env',defs_instrs = (
    List.fold_left (fun (env,instrs) def -> 
      let instr_list,env = tarnslate_definition def env in
      (env, (instrs @ instr_list)) ) (env,[]) defs ) in

  let varSize, stackSize = varAndStack_size p env' in 
  let code = main_code @ defs_instrs @ Dispatcher.code () in
  (basic_code code varSize stackSize), env'
  
