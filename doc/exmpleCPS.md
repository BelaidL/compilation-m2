# Exemple CPS:
```ocaml
  type 'a tree = 
	|Leaf of 'a
	|Node of 'a tree * 'a tree

  let rec tree2list = function
	| Leaf x -> [x]
	| Node (g,d) -> tree2list g @ trre2list d
```	
**Recursion non terminal on veut la rendre terminal**

* deux methode:
 * 1ere methode:

```ocaml
  let rec tree2list t acc = 
	match t with
	| Leaf x -> x::acc
	| Node (g,d) -> tree2list g (tree2list d acc)
                  ---------    ---------------
                   acc term			tail i-c non terminal
```
  * 2eme methode:

```ocaml
	let rec tree2list attent: ('a tree list) t (acc: 'a list) (* exemple ci-desous *)
	match t with                                    (*    tolist []  		 t  []       *)
	| Node (g,d) -> tree2list (g::ettent) d acc     (*		tolist [t] 		 t2 []       *)
	| Leaf x 		 ->                                 (*		tolist [3;t1] (4) []       *)
		match attent with                             (*		tolist [t1] 	(3) [4]      *)
		| [] -> x::acc                                (*		tolist []			 t1 [3;4]    *)
		| g::attent -> tree2list attent g (x::acc)    (*		tolist [1] 		(2) [3;4]    *) 
	---------------------------------------------	  (*		tolist []			(1) [2;3;4]  *)
	|		l'exemple dessus est non terminal 			|   (*	  || *)
	---------------------------------------------		(*	 [1;2;3;4] (**deroulement**) *)
                                                                   (**arbre1**)
```

* L'exemple du cour CPS

(tjr l'xemple de tree2list)

```ocaml
  let rec concat l1 l2 = 
	match l1 with
	| [] -> l2
	| x::l1 -> x:: (concat l1 l2)
```
 * mise de concat en CPS

```ocaml
  let rec concat K E l1 l2 = 
	match l1 with
	| [] -> K E l1
	| x::l1' -> 
		let K,E = &aux,(K,E,x)
		in
		concat K E l1' l2

  let aux (K,E,x) r = K E (x::r)
```
 * mise en CPS de tree2list
```ocaml 
  let rec tolist K E t =
	match t with
	| Leaf x -> K E [x]
	| Node (g,d) -> 
		let K,E = &aux2,(K,E,d) in
		tolist K E g


  let aux2(K,E,d) rg = 
	 let K,E = &aux3,(K,E,rg)
	 in
	 tolist K E d

  let aux3(K,E,rg) rd = concat K E rg rd 
```

## Exemple

* arbre1:

```       t                         | tolist	 K0 E0 t```
```      / \                        | tolist &aux2 E1 t1```
```     t1  t2                      | tolist &aux2 E2 (1)```
```    /\   /\                      | aux2   E2 [1]```
```		1  2 3  4                     | tolist aux3 E3 (2)```
```                                 | aux3   E3 [2]```
```E0 : env vide initial              | concat aux2 E1 [1] [2]```
```E1 = (K0,E0,t2)                    | concat aux E4 [] [2]```
```E2 = (aux2,E1,(2))               | aux    E4 [2]```
```E3 = (aux2,E1,[1])               | aux2   E1 [1;2]```
```E4 = (aux2,E1,1)                 | tolist aux3 E5 t2```
```E5 = (K0,E0,[1;2])               | 		:```
```                                 | 		:```
```                                 | concat K0 E0 [1;2] [3;4]```
```                                 |			:```
```                                 |			.```
```                                 | K0 E0 ([1;2;3;4])```
