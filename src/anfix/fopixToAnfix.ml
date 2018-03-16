(** Conversion from Fopix to Anfix *)

(** For the moment, we only handle Fopix code which is already
    in ANF form (i.e. all arguments of function call and binop
    are simple). This is used for parsing Anfix without defining
    a full parser (but reusing Fopix parser instead) *)

(** TODO: extend this code into a full Fopix to Anfix converter *)

module S=FopixAST
module T=AnfixAST

type environment = unit
let initial_environment () = ()

type defs = (T.identifier * T.expression) list

let fresh_identifier = Gensym.make "_x"

(* Return true iff the given Fopix expression is already simple.  *)
let is_simple = function
  | S.Num _ | S.FunName _ | S.Var _ -> true
  | _ -> false

(* Return a fresh identifier iff the Fopix expression [e] is simple.  *)
let fresh_identifier_opt e =
  if is_simple e then None else Some (fresh_identifier ())

let rec program l = List.map definition l

and definition = function
  | S.DefVal (i,e) -> T.DefVal (i,expr e)
  | S.DefFun (f,a,e) -> T.DefFun (f,a,expr e)

and simplexpr : S.expression -> T.simplexpr = function
  | S.Num n -> T.Num n
  | S.FunName f -> T.FunName f
  | S.Var x -> T.Var x
  | e -> failwith ("This expression should be simple:" ^
                     FopixPrettyPrinter.(to_string expression e))

and expr : S.expression -> T.expression = function
  | S.Num n -> T.Simple (T.Num n)
  | S.FunName f -> T.Simple (T.FunName f)
  | S.Var x -> T.Simple (T.Var x)
  | S.Let (x,e1,e2) -> T.Let (x, expr e1, expr e2)
  | S.IfThenElse (e1,e2,e3) -> 
      simplify_expr e1 (fun x -> T.IfThenElse(x, expr e2, expr e3))
  | S.BinOp (b,e1,e2) ->
      simplify_expr e1 (fun x1 -> 
          simplify_expr e2 (fun x2 ->
              T.BinOp(b, x1, x2)
          )
      )
  | S.BlockNew e ->
      simplify_expr e (fun size -> 
          T.BlockNew (size)
      )
  | S.BlockGet (e1,e2) -> 
      simplify_expr e1 (fun a ->
          simplify_expr e2 (fun i ->
              T.BlockGet (a,i)
          )
      )
  | S.BlockSet (e1,e2,e3) -> 
      simplify_expr e1 (fun a -> 
          simplify_expr e2 (fun i ->
              simplify_expr e3 (fun e ->
                  T.BlockSet (a,i,e)
              )
          )
      )
  | S.FunCall (e,el) ->
      simplify_expr e (fun f ->
          simplify_exprs el (fun xs ->
              T.FunCall (f, xs)
          )
      )
  | S.Print s -> T.Print s

(* Simplify the Fopix expression [e] (if necessary) and then build
   a more complex expression by applying [f] to the simplification of
   [e].  *)
and simplify_expr (e : S.expression) (f : T.simplexpr -> T.expression) :
  T.expression =
  match fresh_identifier_opt e with
  | None -> f (simplexpr e)
  | Some id -> T.Let (id, expr e, f (T.Var id))

(* Simplify the Fopix expressions [es] (if necessary) and then build
   a more complex expression by applying [f] to the simplifications of
   [es].  *)
and simplify_exprs (es : S.expression list)
    (f : T.simplexpr list -> T.expression) : T.expression =
  let ids = List.map fresh_identifier_opt es in
  let simplexprs = List.map2 (fun id e ->
      match id with
      | None -> simplexpr e
      | Some id -> T.Var id
    ) ids es
  in
  List.fold_right2 (fun id e acc ->
      match id with
      | None -> acc
      | Some id -> T.Let (id, expr e, acc)
    ) ids es (f simplexprs)
