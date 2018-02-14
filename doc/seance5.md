Notes de la séance 5 de Compil M2
=================================

  - Visite guidée de Kontix et en particulier de son AST.
    Pour sa syntaxe concrète, voir les fichiers
    exemples [examples/fact.kx](../examples/fact.kx) et [examples/ack.kx](../examples/ack.kx).

  - Langage Jakix : un clone de Javix, pour éviter que flap n'ai
    plusieurs chemins de compilation pour un même langage destination.
    Les fichiers Jakix auront l'extension `.k`

  - Pourquoi passer par Anfix pour produire du Kontix ?
    Cela limite les points délicats lors du passage à Kontix,
    en concentrant la difficulté sur le traitement du `Let`
