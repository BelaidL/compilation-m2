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
  | S.IfThenElse (e1,e2,e3) -> T.IfThenElse (simplexpr e1, expr e2, expr e3)
  | S.BinOp (b,e1,e2) -> T.BinOp (b, simplexpr e1, simplexpr e2)
  | S.BlockNew e -> T.BlockNew (simplexpr e)
  | S.BlockGet (e1,e2) -> T.BlockGet (simplexpr e1, simplexpr e2)
  | S.BlockSet (e1,e2,e3) -> T.BlockSet (simplexpr e1,simplexpr e2,simplexpr e3)
  | S.FunCall (e,el) -> T.FunCall (simplexpr e, List.map simplexpr el)
