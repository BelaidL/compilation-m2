type 'a tree =
| Node of 'a tree * 'a tree
| Leaf of 'a

let rec tolist t = match t with
  | Leaf v -> [v]
  | Node (a, b) -> tolist a @ tolist b

let exemple =
  Node (Node (Leaf 1, Node (Leaf 2, Leaf 3)),
        Node (Node (Leaf 4, Leaf 5), Leaf 6))

let _ = tolist exemple


(* Version à moitié récursive terminale (donc non satisfaisante ici) *)

let rec tolist t acc = match t with
  | Leaf v -> v :: acc
  | Node (g,d) -> tolist g (tolist d acc)

let rec tolist_main t = tolist t []

let _ = tolist_main exemple

(* Version complètement tailrec, en utilisant une pile d'attente
   des sous-arbres gauche pas encore faits. Pour mieux voir ce qui
   se passe, faites un dessin ! *)

let rec tolist attente t acc = match t with
  | Leaf v ->
     (match attente with
      | [] -> v :: acc
      | g::attente -> tolist attente g (v::acc))
  | Node (g,d) -> tolist (g::attente) d acc

let tolist_main t = tolist [] t []

let _ = tolist_main exemple

(* Notez que la pile d'attente se comporte comme une continuation.
   Simplement, il s'agit de *données* à traiter ultérieument au lieu
   de *fonctions* faisant ce traitement ultérieur. *)

let rec prtree = function
  | Leaf v -> string_of_int v
  | Node (g,d) -> "("^prtree g^"#"^prtree d^")";;

let rec prlist l = String.concat "" (List.map string_of_int l);;

(* Utilisation de l'algo de CPS vu en cours sur le tolist initial.
   NB: on évalue ici les arguments de la gauche vers la droite
   (mais l'autre sens ne change pas grand-chose au final). *)

let rec tolist k e t =
  match t with
  | Leaf v -> k e [v]
  | Node (g,d) ->
     Printf.printf "tolist cont1 (.,.,%s) %s\n" (prtree d) (prtree g);
     Obj.magic tolist cont1 (k,e,d) g

and cont1 (k,e,d) lg =
  Printf.printf "tolist cont2 (.,.,%s) %s\n" (prlist lg) (prtree d);
  Obj.magic tolist cont2 (k,e,lg) d

and cont2 (k,e,lg) ld =
  Printf.printf "app %s %s\n" (prlist lg) (prlist ld);
  app k e lg ld

and app k e lg ld =
  match lg with
  | [] -> k e ld
  | x::lg' -> Obj.magic app cont3 (k,e,x) lg' ld

and cont3 (k,e,x) l = k e (x::l)

let tolist_main = tolist (fun _ x -> x) ()

let _ = tolist_main exemple;;

(* NB: Les printf sont là juste pour illustrer le comportement du code.
   Et les Obj.magic permettent d'éviter le typage OCaml,
   vu que nos fabrications d'environnement telles que (k,e,d) sont mal
   typés. Autre solution sinon : un type env = Vide | Push ... *)
