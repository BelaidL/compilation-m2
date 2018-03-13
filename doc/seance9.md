Notes de la séance 9 de Compil M2
=================================

# Trampoline

## Pourquoi ?

La technique du *trampoline* permet une compilation (relativement) efficace
de fonctions récursives terminales dans le cas de langages qui ne proposent pas
d'optimisation de ces fonctions (p.ex. C, Java, Python, JavaScript < 6, ...).
NB: en anglais on parle de langages sans TCO (Tail Call Optimization).

## Idée

La fonction récursive terminale devient une fonction
non-récursive où chaque sous-appel est remplacé par une donnée
indiquant quoi faire plus tard. Les réponses de la fonction récursive
d'origine sont aussi adaptées. C'est une forme très simplifiée de CPS.
Puis une boucle `while` répète cette fonction non-récursive tant
qu'il reste quelque-chose à faire.

## Exemple

```
(* code initial (recursif terminal) *)
let rec fact n acc =
  if n = 0 then acc
  else fact (n-1) (n*acc)

let fact_main n = fact n 1

(* type codant l'état actuel du calcul *)
type state =
 | Stop of int
 | Again of int * int  (* n, acc *)

let again = function Stop _ -> false | Again _ -> true
let get_res = function Stop r -> r | Again _ -> assert false

(* code dérecursifié *)
let fact_norec = function
| Stop _ -> assert false
| Again (n,acc) ->
  if n = 0 then Stop acc
  else Again (n-1, n*acc)

(* La boucle principale du trampoline *)
let fact_trampoline n =
  let state = ref (Again (n,1)) in
  while again !state do
    state := fact_norec !state
  done;
  get_res !state
```

Comme on peut le voir, ce style est un peu pénible en OCaml,
le type `state` nécessite un testeur et des accesseurs qu'on
peut d'habitude éviter en OCaml. Ceci dit, l'idée même de
trampoline est inutile en OCaml (qui sait très bien optimiser
les appels récursifs terminaux)...

## Fonctions récursives mutuelles

Dans le cas d'une fonction récursive simple, on a stocké
uniquement les prochains arguments. Cette technique se généralise
aux fonctions récursive mutuelles (`f` appelle `g`, qui appelle
`f`, etc). Dans ce cas, il faut aussi indiquer dans `Again`
quelle est la fonction suivante à lancer. Dans un langage
comme C on peut utiliser un pointeur de fonction, et le pointeur
`NULL` indique alors qu'on a fini. Dans d'autre langages
cela peut être une clôture, ou tout autre indicateur
(numéro ou type énuméré sur lequel on fait un `switch` ou
un `match` dans la boucle du trampoline). Voir les liens données en
référence ci-dessous pour plus de détails.

## Efficacité

Cette technique permet bien d'éviter une consommation de
pile proportionnelle à la profondeur d'appels récursifs. On évite donc
les soucis de stack overflow. Par contre cela peut rester sensiblement
plus lent qu'un véritable traitement natif des tail calls. Par exemple
on va allouer des données (les `Again` ci-dessous). Et pour les langages
qui passer par des clôtures, ce n'est pas gratuit...

## Quelques liens

  - [Lien wikipedia](https://en.wikipedia.org/wiki/Tail_call#Through_trampolining)
  - [Lien stackoverflow](http://stackoverflow.com/questions/189725/what-is-a-trampoline-function)
  - [Trampolines en Python](http://blog.moertel.com/posts/2013-06-12-recursion-to-iteration-4-trampolines.html)
