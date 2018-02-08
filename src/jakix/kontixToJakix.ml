(** This module implements a compiler from Kontix to Fopix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Kontix
module S = Source.AST
module Target = Jakix
module T = Target.AST

type environment = unit (* TODO *)

let initial_environment () = () (* TODO *)

let rec translate (p : S.t) env = (failwith "TODO" : T.t * environment)
