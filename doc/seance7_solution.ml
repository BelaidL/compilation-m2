
(** Seance 7 : exos sur les exceptions *)

exception Zero

let rec loop i n t =
 if i >= n then 1
 else if t.(i) = 0 then raise Zero
 else t.(i) * loop (i+1) n t

let multab n t =
 try loop 0 n t
 with Zero -> 0

let _ = multab 4 [|1;2;3;4|];;
let _ = multab 4 [|1;2;0;4|];;

(** version CPS (en ocaml) *)

let rec loopcps i n t k k' =
 if i >= n then k 1
 else if t.(i) = 0 then k' ()
 else loopcps (i+1) n t (fun r -> k (t.(i) * r)) k'

let multabcps n t k =
 loopcps 0 n t k (fun () -> 0)

let _ = multabcps 4 [|1;2;3;4|] (fun r -> r);;
let _ = multabcps 4 [|1;2;0;4|] (fun r -> r);;

(** Exo 2 *)

exception Skip
exception Stop

let f = function 0 -> raise Stop | 13 -> raise Skip | x -> 2*x

let rec loop t n i =
  if i >= n then ()
  else try
          t.(i) <- f (t.(i));
          loop t n (i+1)
       with Skip -> loop t n (i+2)

let res =
 let t = [|1;13;0;4;0|] in
 try loop t 5 0; t.(4) with Stop -> 22

(** En cps *)

let f_cps x k k' = match x with
  | 0 -> k' Stop
  | 13 -> k' Skip
  | x -> k (2*x)

let rec loop_cps t n i k k' =
  if i >= n then k ()
  else f_cps
         (t.(i))
         (fun r -> t.(i) <- r; loop_cps t n (i+1) k k')
         (function Skip -> loop_cps t n (i+2) k k'
                 | e -> k' e)

let initcont r = r

let res_cps =
  let t = [|1;13;0;4;0|] in
  loop_cps t 5 0 (fun () -> initcont t.(4))
                 (function Stop -> initcont 0
                         | _ -> assert false)

(** Ici on peut facilement determiner que la ligne avec
    assert false ne servira jamais. Sinon on peut mettre
    à la place un traitement des exceptions "à toplevel",
    p.ex. un affichage de "Uncaught exception" comme OCaml *)

(** NB : ce qui précède est de la CPS "à la OCaml" (avec
    des fonctions anonymes accédant librement à toutes les
    variables de la fonction englobante. Mais on peut transformer
    ça en CPS "1er ordre" comme d'habitude :
     - continuations comme fonctions externes auxiliaires
     - environnements pour contenir les variables nécessaires
*)
