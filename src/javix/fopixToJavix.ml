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

let lookup_last_var env = T.Var (pred env.nextvar)

let bind_function_label fun_id env = {
  env with function_labels = (fun_id, T.Label fun_id) :: env.function_labels
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

let unlabelled_instr (instr : T.instruction) : T.labelled_instruction =
  (None, instr)

let unlabelled_instrs instrs = List.map unlabelled_instr instrs

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
 end
=
 struct
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

  let default_label =
    let default_label = T.Label "default" in
    default_label |> Labels.encode |> ignore;
    default_label

  let code () =
    let labels =
      Labels.all_encodings ()
      |> List.sort (fun (c, _) (c', _) -> compare c c')
      |> List.split
      |> snd
    in [(
        Some label,
        T.Tableswitch (base_value, labels, default_label)
      )]
end

let basic_program code =
  { T.classname = "Fopix";
    T.code = code;
    T.varsize = 100;
    T.stacksize = 10000; }

(* Idir: We translate a Fopix expression into a list of labelled Javix
   instructions.  *)
let rec translate_expression (expr : S.expression) (env : environment) :
  T.labelled_instruction list =
  match expr with
  | S.Num i -> unlabelled_instrs [T.Bipush i; T.Box]

  | S.FunName fun_id -> failwith "Teammates! This is our job!"

  | S.Var id -> failwith "Teammates! This is our job!"

  | S.Let (id, expr, expr') -> failwith "Teammates! This is our job!"

  | S.IfThenElse (cond_expr, then_expr, else_expr) ->
      failwith "Teammates! This is our job!"

  | S.BinOp (binop, left_expr, right_expr) ->
      failwith "Teammates! This is our job!"

  | S.BlockNew size_expr -> failwith "Teammates! This is our job!"

  | S.BlockGet (array_expr, index_expr) ->
      failwith "Teammates! This is our job!"

  | S.BlockSet (array_expr, index_expr, value_expr) ->
      failwith "Teammates! This is our job!"

  | S.FunCall (fun_expr, args) -> failwith "Teammates! This is our job!"

(* Idir: We need to collect all the function labels in a first pass
   because all the functions are mutually recursive in Fopix.  *)
let collect_function_labels prog env =
  let collect_function_label env = function
    | S.DefFun (fun_id, _, _) -> bind_function_label fun_id env
    | S.DefVal _ -> env
  in
  List.fold_left collect_function_label env prog

(* Idir: We translate a Fopix definition into a list of labelled Javix
   instructions and produce a new environment.  *)
let translate_definition (definition : S.definition) (env : environment) :
  (T.labelled_instruction list * environment) =
  match definition with
  | S.DefVal (id, expr) ->
      let var, env = bind_variable env id in
      let instrs =
        translate_expression expr env @ unlabelled_instrs [T.Astore var]
      in
      (instrs, env)

  | S.DefFun (fun_id, formals, body) -> failwith "Teammates! This is our job!"

(** [translate p env] turns a Fopix program [p] into a Javix program
    using [env] to retrieve contextual information. *)
let translate (p : S.t) (env : environment) : T.t * environment =
  let instrs, env =
    List.fold_left (fun (code, env) def ->
        let instrs, env = translate_definition def env in
        (code @ instrs, env)
      ) ([], collect_function_labels p env) p
  in
  let code =
    instrs @ unlabelled_instrs [
      T.Comment "Return the value of the last variable";
      T.Aload (lookup_last_var env);
      T.Unbox;
      T.Ireturn
    ] @
    Dispatcher.code ()
  in
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
