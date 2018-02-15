(** This module implements a compiler from Fopix to Javix. *)

let error pos msg =
  Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Fopix
module Target = Javix

module S = Source.AST
module T = Target.AST

(** We will need the following pieces of information to be carrying
    along the translation: *)
type environment = {
  nextvar          : int;
  variables        : (S.identifier * T.var) list;
  function_labels  : (S.function_identifier * T.label) list;
  (** [function_formals] maintains the relation between function identifiers
      and their formal arguments. *)
  function_formals : (S.function_identifier * S.formals) list;
}

(** Initially, the environment is empty. *)
let initial_environment () = {
  nextvar          = 0;
  variables        = [];
  function_labels  = [];
  function_formals = [];
}

let lookup_variable id env = 
  try
    List.assoc id env.variables
  with Not_found -> failwith ("Error: Variable "^id^" note found")

let lookup_last_var env = T.Var (pred env.nextvar)

let bind_function_formals fun_id formals env = {
  env with function_formals = (fun_id, formals) :: env.function_formals
}

(** [lookup_function_label f env] returns the label of [f] in [env]. *)
let lookup_function_label f env =
  List.assoc f env.function_labels

(** [lookup_function_formals f env] returns the formal arguments of
    [f] in [env]. *)
let lookup_function_formals f env =
  List.assoc f env.function_formals

(** [fresh_function_label f] returns a fresh label starting with [f]
    that will be used for the function body instructions. *)
let fresh_function_label =
  let r = ref 0 in
  fun f ->
    incr r;
    T.Label (f ^ "_body_" ^ string_of_int !r)

let bind_function_label fun_id env = {
  env with
  function_labels = (fun_id, fresh_function_label fun_id) :: env.function_labels
}

let unlabelled_instr (instr : T.instruction) : T.labelled_instruction =
  (None, instr)

let unlabelled_instrs instrs = List.map unlabelled_instr instrs

let labelled_instr label instr = (Some label, instr)

(* Return a list of instructions with the given label on the first
   instruction.  *)
let labelled_instrs label = function
  | [] -> []
  | instr :: instrs -> labelled_instr label instr :: unlabelled_instrs instrs

(** Variables *)

(** [bind_variable env x] associates Fopix variable x to the next
    available Javix variable, and return this variable and the updated
    environment *)
let bind_variable env x =
  let v = T.Var env.nextvar in
  v,
  { env with
    nextvar = env.nextvar + 1;
    variables = (x,v) :: env.variables }

let clear_all_variables env = {env with variables = []; nextvar = 0}

(** For return addresses (or later higher-order functions),
    we encode some labels as numbers. These numbers could then
    be placed in the stack, and will be used in a final tableswitch
    We arbitrarily start these coding numbers at 1000, in order to
    easily distinguish them in javix code, adapt your tableswitch
    accordingly to use 1000 as base value. *)

module Labels :
 sig
   val encode : T.label -> int
   val all_encodings : unit -> (int * T.label) list
 end = struct
   let nextcode = ref 1000
   let allcodes = ref ([]:(int * T.label) list)
   let encode lab =
     let n = !nextcode in
     incr nextcode;
     allcodes := (n,lab) :: !allcodes;
     n
   let all_encodings () = !allcodes
 end

module Dispatcher : sig
  val label : T.label

  val code : unit -> T.labelled_instruction list
end = struct
  let label = T.Label "dispatch"

  let base_value = 1000

  let default_label = T.Label "crash"

  let code () =
    let labels =
      Labels.all_encodings ()
      |> List.filter (fun (_, label) -> label <> default_label)
      |> List.sort (fun (code, _) (code', _) -> compare code code')
      |> List.split
      |> snd
    in [
      labelled_instr default_label (T.Comment "Let's crash...");
      labelled_instr label T.Unbox;
      unlabelled_instr (T.Tableswitch (base_value, labels, default_label))
    ]
end

let ref_count = ref 0
let new_label name =
  incr ref_count; T.Label (name ^"_"^ string_of_int !ref_count)

let basic_program code =
  { T.classname = "Fopix";
    T.code = code;
    T.varsize = 100;
    T.stacksize = 10000; }

let translate_binop op = 
  let op' = (match op with
  | S.Add -> T.Add
  | S.Sub -> T.Sub
  | S.Mul -> T.Mul
  | S.Div -> T.Div
  | S.Mod -> T.Rem
  | _ -> failwith "Incorrect call. Binop is not an arithmetic operator") in
  T.Binop(op')

let translate_cmpop op = match op with
  | S.Eq -> T.Eq
  | S.Le -> T.Le
  | S.Lt -> T.Lt
  | S.Ge -> T.Ge
  | S.Gt -> T.Gt
  | _ -> failwith "Incorrect call. Binop is not a comparision operator"

let translate_binop_comp_with_new_label binop =
  let to_label = new_label "cmpop" in
  T.If_icmp (translate_cmpop binop, to_label)

let get_if_true_label_from_cond_codes cbs = 
  let last = List.nth cbs ((List.length cbs)-1) in match last with
  | _, inst -> ( 
     match inst with
     | T.If_icmp (_, label) -> label
     |_ -> failwith "Last instruction is not If_icmp"
  )

let box = [(None,T.Box)]
let unbox = [(None,T.Unbox)]

(* Idir: We translate a Fopix expression into a list of labelled Javix
   instructions.  *)
let rec translate_expression (expr : S.expression) (env : environment) :
  T.labelled_instruction list =
  match expr with
  (* Belaid: We use Box before Astore so we didn't use it after Bipush*)
  | S.Num i -> unlabelled_instrs [T.Bipush i; T.Box]

  (* Belaid: We do Unbox instruction after all Aload instructions because Binop need Int *)
  | S.Var id -> let v  = lookup_variable id env in 
                unlabelled_instrs [T.Aload v] 

  | S.FunName fun_id ->
      let fun_label = lookup_function_label fun_id env in
      unlabelled_instrs [
        T.Comment ("Push the encoded label of the function " ^ fun_id);
        T.Bipush (Labels.encode fun_label); T.Box]

  | S.Let (id, expr, expr') ->
     let (var, env') = bind_variable env id in
     let instrs = 
       translate_expression expr env @ unlabelled_instrs [T.Astore var] in
     let instrs' = translate_expression expr' env' in
     instrs @ instrs'

  | S.IfThenElse (cond_expr, then_expr, else_expr) ->
    let cond_codes = translate_expression cond_expr env in
    let then_codes = translate_expression then_expr env in
    let else_codes = translate_expression else_expr env in
    let if_true_label = get_if_true_label_from_cond_codes cond_codes in
    let close_label = new_label "close" in
    cond_codes @
    else_codes @
    unlabelled_instr (T.Goto close_label) ::
    [(Some (if_true_label), T.Comment "then_start")] @
    then_codes @
    [(Some (close_label), T.Comment "end_if")]

  | S.BinOp (binop, left_expr, right_expr) ->
    let insts_left = translate_expression left_expr env in
    let insts_right = translate_expression right_expr env in
    begin match binop with
      | S.Add | S.Sub | S.Mul | S.Div | S.Mod -> 
         let op = translate_binop binop in
         insts_left @ unbox @ insts_right @ unbox @ [unlabelled_instr op] @ box

      | S.Eq | S.Le | S.Lt | S.Ge | S.Gt ->
         let op = translate_binop_comp_with_new_label binop in 
         insts_left @ unbox @ insts_right @ unbox @ [unlabelled_instr op]
    end

  | S.BlockNew size_expr -> 
     let size = translate_expression size_expr env in
     [(None,T.Comment "builds an array of java Objects")] @
     size @ unbox @ unlabelled_instrs [T.Anewarray]

  | S.BlockGet (array_expr, index_expr) ->
     let a_instrs = translate_expression array_expr env in
     let i_instrs = translate_expression index_expr env in
     [(None,T.Comment "array access: array[index]")] @
     a_instrs @ [(None,T.Checkarray)] @ i_instrs @ 
     unbox @ unlabelled_instrs [T.AAload] 

  | S.BlockSet (array_expr, index_expr, value_expr) ->
     let a_instrs = translate_expression array_expr env in
     let i_instrs = translate_expression index_expr env in
     let v_instrs = translate_expression value_expr env in
     [(None,T.Comment "array modifiacation: array[index] = value")] @
     [(None,T.Bipush 0)] @ box @ a_instrs @ [(None,T.Checkarray)] 
     @ i_instrs @ unbox @ v_instrs @ unlabelled_instrs [T.AAstore]

  | S.FunCall (fun_expr, args) ->
      let pass_fun_args args env =
        unlabelled_instr (T.Comment "Pass function arguments") ::
        List.flatten (List.map (fun arg -> translate_expression arg env) args)
      in
      let save_vars env =
        List.map (fun (_, var) -> T.Aload var) env.variables
      in
      let restore_vars env =
        List.flatten (
          List.map (fun (_, var) -> [T.Swap; T.Astore var]) (List.rev env.variables)
        )
      in
      let return_label = new_label "return" in
      unlabelled_instrs (
        T.Comment "Save variables onto the stack" ::
        save_vars env
      ) @
      unlabelled_instrs [
        T.Comment "Save the return address";
        T.Bipush (Labels.encode return_label);
        T.Box
      ] @
      pass_fun_args args env @
      unlabelled_instr (T.Comment "Compute the function to call") ::
      translate_expression fun_expr env @
      unlabelled_instr (T.Goto Dispatcher.label) ::
      labelled_instrs return_label (
        T.Comment "Returned form the function call" ::
        restore_vars env
      )

  | S.Print s -> unlabelled_instrs [(T.Print s);T.Box]


(* Idir: We need to collect all the function labels in a first pass
   because all the functions are mutually recursive in Fopix.  *)
let collect_function_labels prog env =
  let collect_function_label env = function
    | S.DefFun (fun_id, _, _) -> bind_function_label fun_id env
    | S.DefVal _ -> env
  in
  List.fold_left collect_function_label env prog

let store_fun_args formals env =
  List.fold_left (fun (instrs, env) formal ->
      let var, env = bind_variable env formal in
      (T.Astore var :: instrs, env)
    ) ([], env) formals

let define_value  id expr env = 
      let var, env' = bind_variable env id in
      let instrs =
        (* Belaid: We do Box instruction before all Astore instruction if head stack is an Unboxed int*)
           translate_expression expr env @ unlabelled_instrs [T.Astore var]
      in
      (instrs, env')

(* Idir: We translate a Fopix definition into a list of labelled Javix
   instructions and produce a new environment.  *)
let translate_definition (definition : S.definition) (env : environment) :
  (T.labelled_instruction list * environment) =
  match definition with
  | S.DefVal (id, expr) ->
     define_value id expr env

  | S.DefFun (fun_id, formals, body) ->
      (* Idir: At the moment, I don't known what is the utility of
         [function_formals] in the environment...  
         Belaid: on aura besoin pour récupéré les argument d'une fonction
         et meme vérifié si le nombre d'arg et le mm ... dans FunCall*)

      let prolog, env' = 
        let env =
          env
          |> clear_all_variables
          |> bind_function_formals fun_id formals
        in
        store_fun_args formals env
      in
      let prolog =
        labelled_instrs (lookup_function_label fun_id env) (
          T.Comment "Store the arguments in variables" ::
          prolog
        )
      in
      let epilog =
        unlabelled_instrs [
          T.Comment "Return of the function";
          T.Swap;
          T.Goto Dispatcher.label
        ]
      in
      let instrs =
        prolog @
        unlabelled_instr (T.Comment ("Body of the function " ^ fun_id)) ::
        translate_expression body env' @
        epilog
      in
      (instrs, env)

let split_defs p =
  List.fold_right (fun def (vals, defs) ->
      match def with
      | S.DefVal _ -> (def :: vals, defs)
      | S.DefFun _ -> (vals, def :: defs)
    ) p ([], [])

let translate_definitions defs env =
  List.fold_left (fun (code, env) def ->
      let instrs, env = translate_definition def env in
      (code @ instrs, env)
    ) ([], env) defs

(** [translate p env] turns a Fopix program [p] into a Javix program
    using [env] to retrieve contextual information. *)
let translate (p : S.t) (env : environment) : T.t * environment =
  let vals, defs = split_defs p in
  let fun_codes, env =
    translate_definitions defs (collect_function_labels p env)
  in
  let main_code =
    let main_instrs, env = translate_definitions vals env in
    main_instrs @ unlabelled_instrs [
      T.Comment "Return the value of the last variable";
      T.Aload (lookup_last_var env);
      T.Unbox;
      T.Ireturn
    ]
  in
  let code = main_code @ fun_codes @ Dispatcher.code () in
  (basic_program code, env)

(** Remarks:
  - When using this compiler from fopix to javix, flap will
    produce some .j files.
    + Compile them to .class via: jasmin Foobar.j
    + Run them with: java -noverify Foobar

  - Final answer:
    your code should contain a final [Ireturn] that should
    return the value of the last DefVal (supposed to be
    an Integer).

  - Function Call Convention:
    + When a function starts, the stack should contain the
      return address (a label encoded as a number, see Labels.encode)
      then the n arguments of the function.
    + The function could freely use an modify any variable. So at least
      the variables that are reused after this call should have
      their contents saved in stack before the call and restored
      afterwards.
    + The function starts by moving its arguments from the stack to
      some variables.
    + When the function returns, the result should be on the top
      of the stack.

  - Boxing:
    The stack could contain both unboxed elements (Java int)
    or boxed elements (Java objects such as Integer or java arrays).
    We place into variables or in array cells only boxed values.
    The arithmetical operations (iadd, if_icmpeq, ...) only works
    on unboxed numbers.
    Conversion between int and Integer is possible via the
    Box and Unboxed pseudo-instructions (translated into correct
    calls to some ad-hoc methods we provide). You may try to
    do some obvious optimisations such as removing [Box;Unbox] or
    [Unbox;Box].

  - Tail-recursive calls : if the body of f ends with a call to
    another function g (which may be f itself in case of recursion),
    no need to save any variables, nor to push a new return address:
    just reuse the return address of the current call to f when
    jumping to g !

  - Variable size and stack size
    Your code should determine the number of variables used by the
    produced code. You might also try to compute the maximum
    stack size when the code is non-recursive or 100% tail-recursive.

*)
