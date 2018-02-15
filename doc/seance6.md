
# Exercices pour Compilation Avancée (M2 2017)

## Passage non-trivial en récursif terminal

Ceci est une reprise d'un exercice de Programmation Fonctionnelle Avancée
(M1). Le code suivant transforme un arbre en liste:

```
  type 'a tree =
  | Node of 'a tree * 'a tree
  | Leaf of 'a;;

  let rec tolist t = match t with
  | Leaf v -> [v]
  | Node (a, b) -> tolist a @ tolist b;;

  let exemple = List.hd (tolist (Node (Leaf 1, Node (Leaf 2, Leaf 3))));;
```

1. Peut-on réécrire simplement la fonction `tolist` en récursif terminal ?
2. Utiliser l'algorithme de mise en CPS vu en cours, tout d'abord en OCaml.
3. Au fait, comment peut-on représenter ces arbres et listes en Fopix ?
   Que devient le `match` ci-dessus ? Et le `@` ?
4. Donner le code Kontix puis Jakix obtenu.
