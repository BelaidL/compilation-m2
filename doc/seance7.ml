(* Exceptions simples *)

exception Zero

(* L'exception brise le flot actuel des fonctions.  Ici, on fait bien
   0 mult si le tab contient un 0.  Ce n'est pas vrai si on fait une
   version réc terminal.  *)
let rec loop i n t =
  if i >= n then 1
  else if t.(i) = 0 then raise Zero
  else t.(i) * loop (i + 1) n t

let multtab n t = try loop 0 n t with Zero -> 0

let test t = multtab (Array.length t) t


(* Version CPS *)

(* Idée: On utilise deux continuations -> une usuelle et l'autre en cas
   d'exception.  [k'] = exception handler.  Rq : En fait, on pourrait
   grouper les 2 continuations en une seule mais moins clair, lisible.
*)
let rec loop i n t k k' =
  if i >= n then k 1
  else if t.(i) = 0 then k' ()
  else loop (i + 1) n t (fun r -> k (t.(i) * r)) k'

(* Au lieu d'avoir un (fun () -> k 0), on pourrait avoir qqch comme ça :

     fun exn ->
       match exn with
       | Zero -> ...
       ...
       | _ -> ...
*)
let multtab n t k = loop 0 n t k (fun () -> k 0)

let test' t = multtab (Array.length t) t (fun x -> x)

let tests =
  let t = [|1; 2; 3|] in
  let t'= [|1; 0; 2|] in
  let res = test' t in
  let res' = test' t' in
  assert (res = test t);
  assert (res' = test t');
  (res, res')

(* Exceptions multiples *)
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

(* Version CPS *)
let f x k k' =
  if x = 0 then k' Stop
  else if x = 13 then k' Skip
  else k (2 * x)

let rec loop t n i k k' =
  if i >= n then k ()
  else
    f t.(i) (fun r ->
        t.(i) <- r;
        loop t n (i + 1) k k'
      ) (function
        | Skip -> loop t n (i + 1) k k'
        | e -> k' e
      )

let res =
  let t = [|1; 13; 0; 4; 0|] in
  loop t 5 0 (fun () -> t.(4)) (function
      | Stop -> 22
      | _ -> assert false       (* Ici c'est OK mais plus généralement,
                                   on renvoie un exn handler toplevel
                                   générique du genre 'print "Uncaught
                                   exception"'.  *)
    )

(* Bilan : On a vu une technique de compilation permettant de passer
   à un langage avec exceptions à un autre sans.  *)
