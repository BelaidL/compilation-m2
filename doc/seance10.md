Notes de la séance 10 de Compil M2
=================================

Au fait, n'oublier pas de calculer la hauteur de pile nécessaire
pour exécuter votre code kontix.


# Quelques optimisations possibles

## Limiter le nombre de variables locales crétines

En particulier, la phase de mise en Anfix peut introduire de nombres
variables intermédiaires utilisées une unique fois. Certaines sont
utiles pour le passage en continuation, mais pas toutes. Et dans
certains cas, c'est franchement pénalisant (p.ex. `if ...` ne pouvant
plus commencer par un binop en Anfix).

  - Repérer à la fin les variables "linéaires" et de les remplacer
    par leur définition. Condition: qu'il n'y a pas d'effet impératifs
    dedans (sinon déplacer un `BlockSet` peut changer le comportement
    du programme).

  - Idem pour variables "simples" (`let x = y in ...` ou `let x = 33 in ...`),
    même si elles apparaissent plusieurs fois.
 
  - Les variables qui sont à la fois "mortes" (zéros utilisations) et
    "pures" (sans effets de bord impératifs à la définition) peuvent
    aussi être enlevés.

## Limiter le nombre de fonctions et de sauts

  - Toujours privilégier un `goto label` plutôt qu'un `goto dispatch`
    si le label destination est connu statiquement.

  - Se débarasser des fonctions "mortes" (non appelées)

  - Pour une fonction appelée une unique fois, mettre son code
    directement à l'endroit de l'appel (*inlining*). Ca fait un saut
    d'économisé en récursif terminal, et deux si on était encore avec
    un appel général (nécessitant un saut au retour).

  - Plus généralement un des intérêts de la forme cps, c'est qu'on
    pourra essayer de mettre dans le code assembleur une fonction appelée
    juste après un de ses appelants (et tuer le saut correspondant).

  - L'idée de dépliage de fonctions (*inlining*) est plus général,
    et peut être utilisé même pour des fonctions utilisées plusieurs
    fois. Attention par contre à ne pas exploser la taille de votre
    code (ou à le rendre infini dans le cas de dépliage répété de code
    récursif). Il y a alors des compromis (ou heuristiques) à trouver,
    par exemple déplier les codes en dessous d'une certaine limite de taille.

## Optimisations par le petit bout de la lorgnette

En anglais on parle de *peephole optimisations*. C'est l'idée d'essayer
d'améliorer localement le code, en regadant juste quelques instructions,
sans beaucoup d'informations sur l'état de la machine quand elle arrivera
ici. Exemple:

  - `box; unbox` peut être supprimé
  - `unbox; box` aussi
  - `load k; store k` également
  - `store k; load k` par contre n'est pas toujours supprimable, ce n'est
    le cas que si on arrive à prouver que le contenu de cette variable k
    ne reservira pas.

## Limiter le nombre de mise en boîte (boxing/unboxing)

Au delà des cas très favorables où `unbox;box` ou `box;unbox` se suivent,
on peut aller plus loin dans l'analyse des variables (par typage), et décider
par exemple de réserver les premières variables de la JVM (p.ex. `v0..v3`)
pour des données dont on a pu prouver qu'elles seront entières, et donc
ne pas les boxer.