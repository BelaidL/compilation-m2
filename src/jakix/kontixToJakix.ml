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

  cont_args : (S.function_identifier * (S.formal_env * S.identifier)) list;
}


module Env : sig
  val lookup_variable : S.identifier -> environment -> T.var
  val lookup_last_var : environment -> T.var

  (** [lookup_function_label f env] returns the label of [f] in [env]. *)
  val lookup_function_label : S.identifier -> environment -> T.label

  (** [lookup_function_formals f env] returns the formal arguments
      of [f] in [env]. *)
  val lookup_function_formals : 
S.function_identifier -> environment -> S.formals

  val lookup_cont_args: S.function_identifier -> environment -> 
(S.formal_env * S.identifier)

  val bind_function_label : S.function_identifier -> environment -> environment

  val bind_function_formals :
    S.function_identifier -> S.formals -> environment -> environment

  val bind_variable : environment -> S.identifier -> T.var * environment

  val bind_cont_args : S.function_identifier -> S.formal_env -> S.identifier ->
environment -> environment

  val bind_cont_stack_label : 
S.function_identifier -> environment -> environment

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
  val translate_binop   : S.binop -> T.instruction
  val translate_cmpop   : S.binop -> T.cmpop
  val unlabelled_instr  : T.instruction -> T.labelled_instruction
  val unlabelled_instrs : (T.instruction list) -> (T.labelled_instruction list)
  val labelled_instr    : T.label -> T.instruction -> T.labelled_instruction
  val labelled_instrs   : 
T.label -> (T.instruction list) -> (T.labelled_instruction list)

  val new_label         : string -> T.label
  val fun_epilog        : (T.labelled_instruction list)
  val translate_binop_comp_with_new_label : (S.binop -> T.instruction * T.label)
  val translate_binop   : (S.binop -> T.instruction)
  val box_after         : T.instruction list -> T.instruction list
  val bipush_box        : (int -> T.instruction list)
  val unbox_after : T.labelled_instruction list -> T.labelled_instruction list
  val unbox_before      : T.instruction list -> T.instruction list
  val translate_Num     : int -> T.labelled_instruction list
  val translate_FunName : 
S.function_identifier -> T.label -> T.labelled_instruction list

  val translate_Var     : T.var -> T.labelled_instruction list
  val translate_Binop   : 
T.labelled_instruction list -> T.labelled_instruction list -> S.binop -> 
T.labelled_instruction list

  val translate_BlockNew: 
T.labelled_instruction list -> T.labelled_instruction list

  val translate_BlockGet: 
T.labelled_instruction list -> T.labelled_instruction list -> 
T.labelled_instruction list

  val translate_BlockSet: 
T.labelled_instruction list -> T.labelled_instruction list ->
T.labelled_instruction list -> T.labelled_instruction list

  val translate_Print   : string -> T.labelled_instruction list
  val save_return_address : T.label -> T.labelled_instruction list

end = struct
  let translate_binop   = FopixToJavix.translate_binop
  let translate_cmpop   = FopixToJavix.translate_cmpop
  let unlabelled_instr  = FopixToJavix.unlabelled_instr
  let unlabelled_instrs = FopixToJavix.unlabelled_instrs
  let labelled_instr    = FopixToJavix.labelled_instr
  let labelled_instrs   = FopixToJavix.labelled_instrs
  let new_label         = FopixToJavix.new_label
  let fun_epilog        = FopixToJavix.fun_epilog
  let translate_binop_comp_with_new_label = 
    FopixToJavix.translate_binop_comp_with_new_label
  
  let translate_binop   = FopixToJavix.translate_binop
  let box_after         = FopixToJavix.box_after
  let bipush_box        = FopixToJavix.bipush_box
  let unbox_after       = FopixToJavix.unbox_after
  let unbox_before      = FopixToJavix.unbox_before
  let translate_Num     = FopixToJavix.translate_Num
  let translate_FunName = FopixToJavix.translate_FunName
  let translate_Var     = FopixToJavix.translate_Var
  let translate_Binop   = FopixToJavix.translate_Binop
  let translate_BlockNew= FopixToJavix.translate_BlockNew
  let translate_BlockGet= FopixToJavix.translate_BlockGet
  let translate_BlockSet= FopixToJavix.translate_BlockSet
  let translate_Print   = FopixToJavix.translate_Print
  let save_return_address = FopixToJavix.save_return_address

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
  | S.Num i -> Utils.translate_Num i
  | S.FunName fun_id -> 
     let fun_label = Env.lookup_function_label fun_id env in
     Utils.translate_FunName fun_id fun_label

  | S.Var id -> 
     let v = Env.lookup_variable id env in
     Utils.translate_Var v

  | S.Let (id, expr, expr') -> 
     let var, env' = Env.bind_variable env id in
     let instrs = 
       translate_basicexpr expr env @ Utils.unlabelled_instrs [T.Astore var] in
     let instrs' = translate_basicexpr expr' env' in instrs @ instrs'

  | S.IfThenElse _ -> failwith "TODO"
  | S.BinOp (binop, left_expr, right_expr) -> 
     let insts_left = translate_basicexpr left_expr env in
     let insts_right = translate_basicexpr right_expr env in
     Utils.translate_Binop insts_left insts_right binop

  | S.BlockNew size_expr -> 
     let size = translate_basicexpr size_expr env in
     Utils.translate_BlockNew size

  | S.BlockGet (array_expr, index_expr) -> 
     let a_instrs = translate_basicexpr array_expr env in
     let i_instrs = translate_basicexpr index_expr env in
     Utils.translate_BlockGet a_instrs i_instrs

  | S.BlockSet (array_expr, index_expr, value_expr) -> 
     let a_instrs = translate_basicexpr array_expr env in
     let i_instrs = translate_basicexpr index_expr env in
     let v_instrs = translate_basicexpr value_expr env in
     Utils.translate_BlockSet a_instrs i_instrs v_instrs

  | S.Print s -> Utils.translate_Print s

let save_vars env = 
  Utils.unlabelled_instrs (
    T.Comment "Save variables onto the stack" ::
    List.map (fun (_,var) -> T.Aload var) env.variables
  )

let restore_vars env = 
  ExtStd.List.flat_map (fun (_,var) -> [T.Swap; T.Astore var])
    (List.rev env.variables)

let pass_fun_args args env =
  Utils.unlabelled_instr (T.Comment "Pass function arguments") ::
  ExtStd.List.flat_map (fun arg -> translate_basicexpr arg env) args

let call_fun fun_expr env = 
  match fun_expr with
  | S.FunName f ->
     Utils.unlabelled_instrs [T.Goto (Env.lookup_function_label f env)]

  | _ -> 
     Utils.unlabelled_instr (T.Comment "Compute the function to call") ::
     translate_basicexpr fun_expr env @
     Utils.unlabelled_instrs [T.Goto Dispatcher.label]

let translate_FunCall fun_expr args env =
  let return_label = Utils.new_label "return" in
     save_vars env @
     Utils.save_return_address return_label @
     pass_fun_args args env @
     call_fun fun_expr env @
     Utils.labelled_instrs return_label (
       T.Comment "Returned from the function call" ::
       restore_vars env
     )

let rec translate_tailexpr (expr: S.tailexpr) (env: environment) : 
(T.labelled_instruction list) = 
  match expr with
  | S.TLet (id, expr, expr') -> 
     let var, env' = Env.bind_variable env id in
     let instrs = 
       translate_basicexpr expr env @ Utils.unlabelled_instrs [T.Astore var] in
     let instrs' = translate_tailexpr expr' env' in instrs @ instrs'

  | S.TIfThenElse _ -> failwith "TODO"
  | S.TPushCont _ -> failwith "TODO"
  | S.TFunCall (fun_expr, args) -> 
     translate_FunCall fun_expr args env

  | S.TContCall b_expr -> 
     translate_basicexpr b_expr env  

let translate_fun_body fun_id body env : (T.labelled_instruction list) = 
  Utils.unlabelled_instr (T.Comment ("Body of the function " ^ fun_id)) ::
  (translate_tailexpr body env)

let store_fun_args formals env =
  List.fold_left (fun (instrs, env) formal ->
      let var, env = Env.bind_variable env formal in
      (T.Astore var :: instrs, env)
    ) ([], env) formals

let fun_prolog fun_id formals env =
  let instrs, env = store_fun_args formals (Env.clear_all_variables env) in
  (Utils.labelled_instrs (Env.lookup_function_label fun_id env) (
      T.Comment "Store the arguments in variables" ::
      instrs
    ),
   env)

let tarnslate_definition (def:S.definition) (env: environment) : 
(T.labelled_instruction list) * environment =
  match def with
  | S.DefFun (fun_id, formals, body) -> 
      let prolog, env' = fun_prolog fun_id formals env in
      (prolog @ translate_fun_body fun_id body env' @ Utils.fun_epilog, env)

  | S.DefCont (cont_id, formals, id, body) -> 
      let prolog, env' = fun_prolog cont_id (id :: formals) env in
      (prolog @ translate_fun_body cont_id body env' @ Utils.fun_epilog, env)

let varAndStack_size (p: S.t) (env: environment) : int * int = failwith "TODO"

let collect_function_info prog env =
  let collect_function_info env = function
    | S.DefFun (fun_id, formals, _) ->
        env
        |> Env.bind_function_label fun_id
        |> Env.bind_function_formals fun_id formals
    | S.DefCont (cont_id, formals_env, id, _) ->
        env
        |> Env.bind_function_label cont_id
        |> Env.bind_cont_args cont_id formals_env id
  in
  List.fold_left collect_function_info env prog

let rec translate (p : S.t) env : T.t * environment = 
  let cont_init,id_init = "_K00","identifier_init" in
  let defs,main = p in
  let defs',main' = 
(S.DefCont (cont_init, [],id_init ,main)) :: defs, (S.TPushCont (cont_init,[],main)) in

  let env = collect_function_info defs' env in

  let env',defs_instrs = (
    List.fold_left (fun (env,instrs) def -> 
      let instr_list,env = tarnslate_definition def env in
      (env, (instrs @ instr_list)) ) (env,[]) defs' ) in
  
  let main_code = translate_tailexpr main' env in
  
  (* let varSize, stackSize = varAndStack_size p env' in  *)
  let code = main_code @ defs_instrs @ Dispatcher.code () in
  (* (basic_code code varSize stackSize), env' *)
  (basic_code code 100 1000  ), env'
  
