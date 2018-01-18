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
  | VFun      of formals * expression

let print_value = function
  | VInt x      -> string_of_int x
  | VBool true  -> "true"
  | VBool false -> "false"
  | VUnit       -> "()"
  | VLocation l -> Memory.print_location l
  | VFun (fl, e) -> "<fun>"

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
  val update : t -> identifier -> value -> t
  val lookup  : identifier -> t -> value
  val last    : t -> (identifier * value * t) option
  val print   : t -> string
end = struct
  type t = (identifier * value) list

  let initial = []

  let bind e x v = (x, v) :: e

  exception UnboundIdentifier of identifier

  let rec update e x v = match e with
  | [] -> raise (UnboundIdentifier x)
  | (x', v')::e' -> if x' = x then (x,v)::e' else update e' x v
      
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

  | DefFun (id, fl, e) ->
      let new_run = defineFun runtime (id, fl, e) in
      defineFun new_run (id, fl, e) 

and defineFun run (id, fl, e) = 
      try
	{environment = Environment.update run.environment id (VFun (fl, e))}
      with Environment.UnboundIdentifier _ ->
	{environment = Environment.bind run.environment id (VFun (fl, e))}
      
and expression runtime = function
  | Num n -> VInt n

  | FunName f -> Environment.lookup f runtime.environment

  | Var x -> Environment.lookup x runtime.environment

  | Let (x, ex, e) ->
    let v = expression runtime ex in
    let runtime =
     { environment = Environment.bind runtime.environment x v }
    in
    expression runtime e

  | IfThenElse (c, t, f) ->
      let v = expression runtime c in
      begin match value_as_bool v with
      | None -> Printf.printf "Not a valid if condition" ; assert false
      | Some true  -> expression runtime t
      | Some false -> expression runtime f
      end

  | BinOp (Add|Sub|Mul|Div|Mod as op, e1, e2) ->
    binop runtime op e1 e2

  | BinOp (Lt|Gt|Le|Ge|Eq as op, e1, e2) ->
    binop runtime op e1 e2

  | BlockNew e ->
      let v = expression runtime e in
      begin match v with
      | VInt x -> let adr = Memory.allocate memory x (VInt 0) in VLocation adr
      | _ -> Printf.printf "Expr should be evaluate to int!";
	      assert false
      end
	
  | BlockGet (e1, e2) ->
      let v = expression runtime e2 in
      let v1 = expression runtime e1 in
      begin match v1 with
      | VLocation adr ->
      	  begin match v with
      	  | VInt x -> Memory.read (Memory.dereference memory adr) x
      	  | _      -> failwith "Incorrect address in VLocation of BlockSet"
      	  end
      | _ -> failwith "BlockSet must be an address"
      end   

  | BlockSet (e1, e2, e3) ->
      let v1 = expression runtime e1 in
      let v2 = expression runtime e2 in
      let v3 = expression runtime e3 in
      begin match v1 with
      | VLocation adr ->
	  begin match v2 with
	  | VInt x -> Memory.write (Memory.dereference memory adr) x v3; VUnit
	  | _      -> failwith "Incorrect address in VLocation of BlockSet"
	  end
      | _ -> failwith "BlockSet must be an address"
      end  

  | FunCall (fexpr, args) ->
    let vf = expression runtime fexpr in
    (*let f e = expression runtime e in
    let list_args = List.map f args in *)
    match vf with
    | _ -> failwith "TODO"
    

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
