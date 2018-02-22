
# Exercices pour Compilation Avancée

## Exceptions simples

Ecrire une fonction qui reçoit un tableau d'entiers `t` et sa longueur `n`,
et retourne la multiplication des entiers dans le tableau, en un seul passage
et en évitant de faire la moindre multiplication si 0 est dans le tableau.
Donner d'abord une version par exception, puis une version CPS.

## Exceptions multiples

Comment compiler le code suivant:

```
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
```
