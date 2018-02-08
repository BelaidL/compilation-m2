(** Conversion from Anfix to a subset of Fopix *)

(** This is used for instance during printing *)

module S=AnfixAST
module T=FopixAST

let rec program l = List.map definition l

and definition = function
  | S.DefVal (i,e) -> T.DefVal (i, expr e)
  | S.DefFun (f,a,e) -> T.DefFun (f,a, expr e)

and expr = function
  | S.Simple e -> simple e
  | S.Let (x,e1,e2) -> T.Let (x,expr e1,expr e2)
  | S.IfThenElse (e1,e2,e3) -> T.IfThenElse (simple e1,expr e2,expr e3)
  | S.BinOp (b,e1,e2) -> T.BinOp (b,simple e1,simple e2)
  | S.BlockNew e -> T.BlockNew (simple e)
  | S.BlockGet (e1,e2) -> T.BlockGet (simple e1,simple e2)
  | S.BlockSet (e1,e2,e3) -> T.BlockSet (simple e1,simple e2,simple e3)
  | S.FunCall (e,el) -> T.FunCall (simple e, List.map simple el)

and simple = function
  | S.Num n -> T.Num n
  | S.FunName f -> T.FunName f
  | S.Var x -> T.Var x
