(** This module implements a compiler from Kontix to Fopix. *)

let error msg = Error.error "compilation" Position.dummy msg

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


module Env : sig
  val lookup_variable : S.identifier -> environment -> T.var

  val lookup_last_var : environment -> T.var

  (** [lookup_function_label f env] returns the label of [f] in [env]. *)
  val lookup_function_label : S.identifier -> environment -> T.label

  (** [lookup_function_formals f env] returns the formal arguments of
      [f] in [env]. *)
  val lookup_function_formals :
    S.function_identifier -> environment -> S.formals

  val lookup_cont_args : S.function_identifier -> environment -> 
(S.formal_env * S.identifier)

  val bind_function_label : S.function_identifier -> environment -> environment

  val bind_function_formals :
    S.function_identifier -> S.formals -> environment -> environment

  val bind_variable : environment -> S.identifier -> T.var * environment

  val bind_cont_args : S.function_identifier -> S.formal_env -> 
S.identifier -> environment -> environment

  val bind_cont_stack_label : S.function_identifier -> environment -> 
environment

  val current_cont : environment -> (S.function_identifier * T.label) option

  val clear_all_variables : environment -> environment
 
end = struct

  let lookup_variable id env =
     try
       List.assoc id env.variables
     with Not_found -> error ("Variable " ^ id ^ " not found")
      
  let lookup_last_var env = T.Var (pred env.nextvar)

  let lookup_function_label f env =
    List.assoc f env.function_labels

  let lookup_function_formals f env =
    List.assoc f env.function_formals

  let lookup_cont_args cont_id env = 
    List.assoc cont_id env.cont_args

  let bind_variable env x =
    let v = T.Var env.nextvar in
    v,
    { env with
      nextvar = env.nextvar + 1;
      variables = (x,v) :: env.variables }

  let bind_function_label fun_id env = {
    env with
      function_labels =
      (fun_id, FopixToJavix.Env.fresh_function_label fun_id) :: 
        env.function_labels
  }

  let bind_function_formals fun_id formals env = {
    env with function_formals = (fun_id, formals) :: env.function_formals
  }
   

  let bind_cont_args cont_id formals_env identifier env = {
    env with cont_args = (cont_id,(formals_env,identifier)) :: env.cont_args
  }

  let bind_cont_stack_label cont_id env = {
    env with cont_stack = let label = lookup_function_label cont_id env in
                          (cont_id,label) :: env.cont_stack
  }

  let current_cont env =
    match env.cont_stack with
    | [] -> None
    | cont :: conts -> Some cont

  let clear_all_variables env = {env with variables = []; nextvar = 0}

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
(T.labelled_instruction list) = 
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
(T.labelled_instruction list) = 
  match expr with
  | S.TLet _ -> failwith "TODO"
  | S.TIfThenElse _ -> failwith "TODO"
  | S.TPushCont _ -> failwith "TODO"
  | S.TFunCall _ -> failwith "TODO"
  | S.TContCall _ -> failwith "TODO"

let translate_fun_body fun_id body env : (T.labelled_instruction list) = 
  Utils.unlabelled_instr (T.Comment ("Body of the function " ^ fun_id)) ::
  (translate_tailexpr body env)

let tarnslate_definition (def:S.definition) (env: environment) : 
(T.labelled_instruction list) * environment =
  match def with
  | S.DefFun _ -> failwith "TODO"
  | S.DefCont _ -> failwith "TODO"

let varAndStack_size (p: S.t) (env: environment) : int * int = failwith "TODO"

let rec translate (p : S.t) env : T.t * environment = 
  let defs,main = p in
  let main_code = translate_tailexpr main env in
  let env',defs_instrs = (
    List.fold_left (fun (env,instrs) def -> 
      let instr_list,env = tarnslate_definition def env in
      (env, (instrs @ instr_list)) ) (env,[]) defs ) in

  let varSize, stackSize = varAndStack_size p env' in 
  let code = main_code @ defs_instrs @ Dispatcher.code () in
  (basic_code code varSize stackSize), env'
  
