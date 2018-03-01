(** * Call-with-current-continuation (callcc) en OCaml *)

(** Ressources générales:
    https://en.wikipedia.org/wiki/Call-with-current-continuation
    http://community.schemewiki.org/?call-with-current-continuation
    http://stackoverflow.com/questions/9250474/does-the-yin-yang-continuations-puzzle-make-sense-in-a-typed-language#9824303
    http://okmij.org/ftp/continuations/implementations.html
    http://okmij.org/ftp/continuations/against-callcc.html
*)


(** Implémentation de callcc en Ocaml (bytecode) :
    http://pauillac.inria.fr/~xleroy/software/ocaml-callcc-1.0.tar.gz *)
#directory "~letouzey/src/ocaml-callcc-1.0"
#load "callcc.cma";;
open Callcc;;

callcc;; (** ('a Callcc.cont -> 'a) -> 'a *)
throw;; (** 'a Callcc.cont -> 'a -> 'b *)

(** Callcc sans throw interne: résultat propagé comme d'habitude *)

let _ = callcc (fun k -> 5);;

(** Exemple simple de throw :
    le calcul actuel en cours est abandonné, et le second
    argument du throw devient le résultat du callcc *)

let _ = 1 + callcc (fun k -> 3 + (throw k 5) + 7)

(** Exceptions via callcc *)

let rec multlist l k = match l with
  | [] -> 1
  | x::l -> if x=0 then throw k 0 else x*multlist l k

let _ = callcc (multlist [1;3;5;0;7;9])
let _ = callcc (multlist [1;3;5;6;7;9])

(** Sortie prématurée d'un List.iter *)

let find f l =
  callcc (fun k ->
      List.iter (fun x -> if f x then throw k (Some x)) l;
      None)

let _ = find (fun x -> x mod 2 = 0) [1;2;3;4;5;6]

(** Backtrack via callcc *)

let find_next f l =
  callcc (fun k ->
    List.iter
      (fun x ->
        if f x then callcc (fun k' -> throw k (Some(x, k'))))
      l;
    None)

let (found, again) =
  match find_next (fun x -> x mod 2 = 0) [1;2;3;4;5;6] with
  | None -> raise Not_found
  | Some (x,k) -> (x,k)

let _ = throw again ()
let _ = throw again ()
let _ = throw again ()

(** Callcc + references : retour à de vieux calculs *)

let r = ref None

let _ = 1 + callcc (fun k -> r := Some k; 3)

let get_cont r = match !r with Some k -> k | None -> failwith "oups"

let _ = throw (get_cont r) 7
let _ = throw (get_cont r) 5

(** Callcc pour des boucles for ou while *)

let sum n =
  let n = ref n in
  let r = ref 0 in
  callcc
    (fun exit ->
     let again = ref exit in
     let () = callcc (fun k -> again := k) in
     if !n = 0 then throw exit ()
     else
       begin
         r := !r + !n;
         decr n;
         throw !again ();
       end
    );
  !r

let _ = sum 100;;

(** Coroutines *)

let coroutines () =
  callcc (fun init_k ->
    let curr_k = ref init_k in
    let communicate x =
      callcc (fun k -> let old_k = !curr_k in curr_k := k; throw old_k x) in
    let rec process1 n =
      if n >= 30 then (Printf.printf "1: finished\n%!"; 100)
      else (Printf.printf "1: received %d\n%!" n; process1(communicate(n+1)))
    and process2 n =
      if n >= 20 then (Printf.printf "2: finished\n%!"; 15)
      else (Printf.printf "2: received %d\n%!" n; process2(communicate(n+1)))
    in
    process1(callcc(fun start1 ->
      process2(callcc(fun start2 ->
        curr_k := start2; throw start1 0)))))

let _ = coroutines ()

(*
init_k : pas vraiment utilisé, juste là pour initialiser la reference
start1 = "process1(.)"
start2 = "process1(callcc(fun start1 -> process2(.)))"

curr_k := start2
throw start1 : calcul de process1(0) = process1(communicate(1))

dans communicate(1):
k = "process1(.)"
curr_k := k (qui vaut donc la même chose que start1)
throw start2 : calcul de
  process1(calcc(fun start1 -> process2(1)))
  = process1(calcc(fun start1 -> process2(communicate(2))))

dans communicate(2):
k = "process1(callcc(fun start1 -> process2(.)))" (bref, start2)
curr_k := start2
throw start1 : calcul de process1(2) = process1(communicate(3))

...

dans communicate(21):
curr_k := start1
et calcul de
  process1(callcc(fun start1 -> process2(21)))
  = process1(callcc(fun start1 -> 15))
  = process1(15)
  = process1(communicate(16))

dans communicate(16):
k = process1(.)
curr_k := start1
et throw start1, donc calcul de
 process1(16) = process1(communicate(17))

etc jusqu'à process1(30) qui répond 100

*)


(** YinYang example *)

exception Stop

(* Pour avoir une égalité 'a cont = 'a *)
type 'a cyclic_cont = Cont of 'a cyclic_cont Callcc.cont

let cthrow (Cont k) = throw k
let getcc () = callcc (fun k -> Cont k)

exception Stop

let count =
 let r = ref 0 in
 fun () -> incr r; if !r >= 30 then raise Stop

let newl k = print_newline (); count (); k
let star k = print_char '*'; k

let yin = newl (getcc ())
and yang = star (getcc ())
in cthrow yin yang;;


(** Version sans type spécialisé (nécessite -rectypes) *)

let yin = newl (callcc throw) in
let yang = star (callcc throw) in
yin yang


(*
 getcc donne une k0 avec
 k0 = "let yin = newl . in let yang = star (getcc ()) in cthrow yin yang"
 newl k0
\n
 repond k0 pour yin
 callcc donne une k1 avec
 k1 = "let yang = star . in cthrow k0 yang"
 star k1
*
 et répond k1 pour yang
 cthrow k0 k1
 = let yin = newl k1 in let yang = star (getcc ()) in cthrow yin yang
\n
 k1 pour yin
 k2 = "let yang = star . in cthrow k1 yang"
*
 k2 pour yang
 cthrow k1 k2
 = let yang = star k2 in throw k0 yang
*
 cthrow k0 k2
 = let yin = newl k2 in let yang = star (getcc ()) in cthrow yin yang
\n
  k2 pour yin
  k3 = "let yang = star . in throw k2 yang"
*
  throw k2 k3
  = let yang = star k3 in throw k1 yang
*
  throw k1 k3
  = let yang = star k3 in throw k0 yang
*
  throw k0 k3
  = let yin = newl k3 in let yang = star (getcc ()) in cthrow yin yang
\n
  k4 = "let yang = star . in throw k3 yang"
*
  cthrow k3 k4
*
  cthrow k2 k4
*
  cthrow k1 k4
*
  cthrow k0 k4
  = let yin = newl k4 in let yang = star (getcc ()) in cthrow yin yang
\n
  k5 = "let yang = star . in cthrow k4 yang
*
  cthrow k4 k5
  ...
*)

(** Remarque: une variante de callcc où la continuation
    est une vraie fonction: *)

let callcc_fun f = callcc (fun k -> f (throw k));;
(** val callcc_fun : (('a -> 'b) -> 'a) -> 'a *)

(** Ceux que la Logique intéressent reconnaîtront ce type:
    c'est la loi de Peirce ((A->B)->A)->A, un principe classique *)

(** Par exemple, un des premiers exemples de ce fichier devient: *)

let _ = 1 + callcc_fun (fun k -> 3 + k 5 + 7)


(** Défauts de callcc (envers de la médaille):
    - fuite mémoire
    - code incompréhensible
    - non-localité
    ...

    Variante : continuation délimitées (cf delimcc) *)
