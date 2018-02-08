(** Conversion from Kontix to a subset of Fopix *)

(** This is used for instance during evaluation *)

module S=KontixAST
module T=FopixAST

let fresh_id =
  let r = ref (-1) in
  fun s -> incr r; s^(string_of_int !r)

let rec program (l,e) =
  List.map definition l @
  [T.DefFun ("_return_",["x";"E"],T.Var "x");
   T.DefVal ("res",tailexpr e)]

and definition = function
  | S.DefFun (f,a,e) -> T.DefFun (f,"K"::"E"::a,tailexpr e)
  | S.DefCont (f,ids,x,e) ->
     let id = fresh_id "__env" in
     T.DefFun (f,[x;id],untuple (T.Var id) 0 ("K"::"E"::ids) (tailexpr e))

and tuple ids =
  let id = fresh_id "__blk" in
  T.Let (id,T.BlockNew (T.Num (List.length ids)),
         settuple (T.Var id) 0 ids (T.Var id))

and settuple e0 i el e = match el with
  | [] -> e
  | id::el -> T.Let ("_",T.BlockSet(e0,T.Num i,T.Var id),
                     settuple e0 (i+1) el e)

and untuple ptr i ids e = match ids with
  | [] -> e
  | id::ids -> T.Let (id,T.BlockGet (ptr,T.Num i),
                      untuple ptr (i+1) ids e)

and tailexpr = function
  | S.TLet (x,e1,e2) -> T.Let (x, basicexpr e1, tailexpr e2)
  | S.TIfThenElse (e1,e2,e3) ->
     T.IfThenElse (basicexpr e1, tailexpr e2, tailexpr e3)
  | S.TFunCall (e,el) ->
     T.FunCall (basicexpr e, [T.Var "K"; T.Var "E"] @ List.map basicexpr el)
  | S.TContCall e ->
     T.FunCall (T.Var "K", [T.Var "E"; basicexpr e])
  | S.TPushCont (f,ids,e) ->
     T.Let ("E", tuple (["K";"E"]@ids),
       T.Let ("K", T.FunName f, tailexpr e))

and basicexpr = function
  | S.Num n -> T.Num n
  | S.FunName f -> T.FunName f
  | S.Var x -> T.Var x
  | S.Let (x,e1,e2) -> T.Let (x,basicexpr e1, basicexpr e2)
  | S.IfThenElse (e1,e2,e3) ->
     T.IfThenElse (basicexpr e1, basicexpr e2, basicexpr e3)
  | S.BinOp (o,e1,e2) -> T.BinOp (o, basicexpr e1, basicexpr e2)
  | S.BlockNew e -> T.BlockNew (basicexpr e)
  | S.BlockGet (e1,e2) -> T.BlockGet (basicexpr e1, basicexpr e2)
  | S.BlockSet (e1,e2,e3) ->
     T.BlockSet (basicexpr e1, basicexpr e2, basicexpr e3)
