open Error
open FopixAST

(** [error pos msg] reports runtime error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of fopi evaluates into a [value]. *)
type value =
  | VUnit
  | VInt      of int
  | VBool     of bool
  | VLocation of Memory.location
  | VFun      of function_identifier

let print_value = function
  | VInt x      -> string_of_int x
  | VBool true  -> "true"
  | VBool false -> "false"
  | VUnit       -> "()"
  | VLocation l -> Memory.print_location l
  | VFun f      -> f

type 'a coercion = value -> 'a option
let value_as_int      = function VInt x -> Some x | _ -> None
let value_as_bool     = function VBool x -> Some x | _ -> None
let value_as_location = function VLocation x -> Some x | _ -> None
let value_as_unit     = function VUnit -> Some () | _ -> None

type 'a wrapper = 'a -> value
let int_as_value x  = VInt x
let bool_as_value x = VBool x
let location_as_value x = VLocation x
let unit_as_value () = VUnit

(** Binary operators *)

let lift_binop coerce wrap op v1 v2 =
  match coerce v1, coerce v2 with
  | Some li, Some ri -> Some (wrap (op li ri))
  | _, _ -> None

let lift_arith_op op = lift_binop value_as_int int_as_value op
let lift_cmp_op op = lift_binop value_as_int bool_as_value op

let arith_op_of_symbol = function
  | Add -> ( + )
  | Sub -> ( - )
  | Div -> ( / )
  | Mul -> ( * )
  | Mod -> ( mod )
  | _ -> assert false

let cmp_op_of_symbol = function
  | Lt -> ( < )
  | Gt -> ( > )
  | Le -> ( <= )
  | Ge -> ( >= )
  | Eq -> ( = )
  | _ -> assert false

let evaluation_of_binary_symbol = function
  | (Add|Sub|Mul|Div|Mod) as s -> lift_arith_op (arith_op_of_symbol s)
  | (Lt|Gt|Le|Ge|Eq) as s -> lift_cmp_op (cmp_op_of_symbol s)

(** Execution environment *)

module Environment : sig
  type t
  val initial : t
  val bind    : t -> identifier -> value -> t
  exception UnboundIdentifier of identifier
  val lookup  : identifier -> t -> value
  val last    : t -> (identifier * value * t) option
  val print   : t -> string
end = struct
  type t = (identifier * value) list

  let initial = []

  let bind e x v = (x, v) :: e

  exception UnboundIdentifier of identifier

  let lookup x e =
    try
      List.assoc x e
    with Not_found ->
      raise (UnboundIdentifier x)

  let last = function
    | [] -> None
    | (x, v) :: e -> Some (x, v, e)

  let print_binding (x, v) =
    (* Identifiers starting with '_' are reserved for the compiler.
       Their values must not be observable by users. *)
    if x <> "_" && x.[0] = '_' then
      ""
    else
      x ^ " = " ^ print_value v

  let print env =
    String.concat "\n" (
      List.(filter (fun s -> s <> "") (map print_binding env))
    )

end

type runtime = {
  environment : Environment.t;
}

type observable = {
  new_environment : Environment.t;
}

let initial_runtime () = {
  environment = Environment.initial;
}

(** 640k ought to be enough for anybody -- B.G. *)
let memory : value Memory.t = Memory.create (640 * 1024)

let rec evaluate runtime ast =
  let runtime' = List.fold_left declaration runtime ast in
  (runtime', extract_observable runtime runtime')


and declaration runtime = function
  | DefVal (i, e) ->
    let v = expression runtime e in
    { environment = Environment.bind runtime.environment i v }
  | DefFun _ ->
    runtime

and expression runtime = function
  | Num n -> VInt n

  | FunName f -> VFun f

  | Var x -> Environment.lookup x runtime.environment

  | Def (x, ex, e) ->
    let v = expression runtime ex in
    let runtime =
     { environment = Environment.bind runtime.environment x v }
    in
    expression runtime e

  | IfThenElse (c, t, f) ->
    failwith "Student! This is your job!"

  | BinOp (Add|Sub|Mul|Div|Mod as op, e1, e2) ->
    binop runtime op e1 e2

  | BinOp _ | BlockNew _ | BlockGet _ | BlockSet _ ->
    failwith "Student! This is your job!"

  | FunCall (fexpr, args) ->
    failwith "Student! This is your job!"

and binop runtime op e1 e2 =
  let v1 = expression runtime e1 in
  let v2 = expression runtime e2 in
  match evaluation_of_binary_symbol op v1 v2 with
  | Some v -> v
  | None -> error [] "Invalid binary operation."

and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
        | None -> assert false (* Absurd. *)
        | Some (x, v, env') ->
          let new_environment = Environment.bind new_environment x v in
          substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.initial runtime.environment runtime'.environment
  }

let print_observable runtime observation =
  Environment.print observation.new_environment