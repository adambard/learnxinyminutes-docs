---
language: nix
filename: learn.nix
contributors:
    - ["Chris Martin", "http://chris-martin.org/"]
    - ["Rommel Martinez", "https://ebzzry.io"]
    - ["Javier Candeira", "https://candeira.com/"]
translators:
    - ["Loïc Reynier", "https://github.com/loicreynier"]
lang: fr-fr
---

Nix est un langage fonctionnel développé pour
le gestionnaire de paquet [Nix](https://nixos.org/nix/) et
[NixOS](https://nixos.org/).

Les expressions Nix peuvent être évaluées avec
[nix-instantiate](https://nixos.org/nix/manual/#sec-nix-instantiate)
ou [`nix repl`](https://nixos.org/nix/manual/#ssec-relnotes-2.0).

```nix
with builtins; [

  #  Commentaires
  #=========================================

  # Ceci est un commentaire en ligne.

  /* Ceci est un commentaire
     écrit sur plusieurs lignes. */


  #  Booléens
  #=========================================

  (true && false)               # Et
  #=> false

  (true || false)               # Ou
  #=> true

  (if 3 < 4 then "a" else "b")  # Test logique
  #=> "a"


  #  Entiers et nombres flottants
  #=========================================

  # Il y a deux types de nombres : les entiers et les flottants.

  1 0 42 (-3)       # Quelques exemples d'entiers

  123.43 .27e13     # Quelques exemples de nombre flottants

  # Les opérations conservent le type du nombre

  (4 + 6 + 12 - 2)  # Addition
  #=> 20
  (4 - 2.5)
  #=> 1.5

  (7 / 2)           # Division
  #=> 3
  (7 / 2.0)
  #=> 3.5


  #  Chaînes de caractères
  #=========================================

  "Les chaînes de caractères littérales sont écrites entre guillements."

  "
    Les chaînes de caractères littérales
    peuvent s'étendre sur
    plusieurs lignes
  "

  ''
    Ceci est ce qu'on appelle une
    "chaîne de caractères littérale indentée".
    Les espaces de début de lignes sont intelligemment supprimées.
  ''

  ''
    a
      b
  ''
  #=> "a\n  b"

  ("ab" + "cd")   # Concaténation de chaînes de caractères
  #=> "abcd"

  # L'antiquotation vous permet d'intégrer des valeurs
  # dans des chaînes de caracères.
  ("Votre répertoire personnel est ${getEnv "HOME"}")
  #=> "Votre répertoire personnel est /home/alice"


  #  Chemins
  #=========================================

  # Nix a un type de variable primitif pour les chemins.
  /tmp/tutorials/learn.nix

  # Un chemin relatif est résolu en un chemin absolu
  # au moment de l'évaluation.
  tutorials/learn.nix
  #=> /the-base-path/tutorials/learn.nix

  # Un chemin doit toujours contenir au moins une barre oblique (slash).
  # Un chemin relatif d'un fichier dans le répertoire courant
  # a donc besoin du préfixe `./`
  ./learn.nix
  #=> /the-base-path/learn.nix

  # L'opérateur `/` doit être entouré d'espaces afin d'être
  # traité comme une division
  7/2        # Ceci est un chemin
  (7 / 2)    # Ceci est une division entière


  #  Importations
  #=========================================

  # Un fichier nix contient une seule expression principale sans
  # variable libre.
  # Une expression importée s'évalue à la valeur du fichier importé.
  (import /tmp/foo.nix)

  # Les importations peuvent également être spécifiées par des chaînes
  # de caractères.
  (import "/tmp/foo.nix")

  # Les chemins doivent être absolus. Cependant comme les chemins
  # relatifs sont automatiquement résolus, ils peuvent être utilisés
  # pour l'importation.
  (import ./foo.nix)

  # Attention, cela ne se produit pas avec les chaînes de caractères.
  (import "./foo.nix")
  #=> error: string ‘foo.nix’ doesn't represent an absolute path


  #  Let
  #=========================================

  # Les blocs `let` permettent d'affecter des valeurs avec des variables.
  (let x = "a"; in
    x + x + x)
  #=> "aaa"

  # Les affectations peuvent se référer les unes aux autres, et leur ordre
  # n'a pas d'importance.
  (let y = x + "b";
       x = "a"; in
    y + "c")
  #=> "abc"


  # Les affectations d'un bloc fille écrasent les affections du bloc mère.
  (let a = 1; in
    let a = 2; in
      a)
  #=> 2


  #  Fonctions
  #=========================================

  (n: n + 1)      # Fonction qui ajoute 1

  ((n: n + 1) 5)  # Fonction précédente appliquée à 5
  #=> 6

  # Il n'y a pas de syntaxe pour nommer les fonctions,
  # mais elles peuvent affectée à une variable par un bloc `let`
  # comme toutes les autres valeurs.
  (let succ = (n: n + 1); in succ 5)
  #=> 6

  # Une fonction a exactement un seul argument.
  # Des fonctions à plusieurs arguments peuvent être construites
  # par imbrificication de fonctions.
  ((x: y: x + "-" + y) "a" "b")
  #=> "a-b"

  # Il est également possible d'avoir des arguments de fonction nommés.
  # Nous verrons comment après avoir introduit les ensembles.


  #  Listes
  #=========================================

  # Les listes sont indiquées par des crochets.

  (length [1 2 3 "x"])
  #=> 4

  ([1 2 3] ++ [4 5])
  #=> [1 2 3 4 5]

  (concatLists [[1 2] [3 4] [5]])
  #=> [1 2 3 4 5]

  (head [1 2 3])
  #=> 1
  (tail [1 2 3])
  #=> [2 3]

  (elemAt ["a" "b" "c" "d"] 2)
  #=> "c"

  (elem 2 [1 2 3])
  #=> true
  (elem 5 [1 2 3])
  #=> false

  (filter (n: n < 3) [1 2 3 4])
  #=> [ 1 2 ]


  #  Ensembles
  #=========================================

  # Un ensemble, ou "set", est un dictionnaire non ordonné avec des clés
  # en chaîne de caractères.
  { foo = [1 2]; bar = "x"; }

  # L'opérateur `.` extrait une valeur d'un ensemble.
  { a = 1; b = 2; }.a
  #=> 1

  # L'opérateur `?` teste si la clé est présente dans l'ensemble.
  ({ a = 1; b = 2; } ? a)
  #=> true
  ({ a = 1; b = 2; } ? c)
  #=> false

  # L'opérateur `//` fusionne deux ensembles.
  ({ a = 1; } // { b = 2; })
  #=> { a = 1; b = 2; }

  # Les valeurs de droite écrasent les valeurs de gauche.
  ({ a = 1; b = 2; } // { a = 3; c = 4; })
  #=> { a = 3; b = 2; c = 4; }

  # Le mot clé `rec` indique un ensemble récursif, ou "recursive set",
  # dans lequel les attributs peuvent se référer les uns aux autres.
  (let a = 1; in     { a = 2; b = a; }.b)
  #=> 1
  (let a = 1; in rec { a = 2; b = a; }.b)
  #=> 2

  # Les ensembles imbriqués peuvent être définis par morceaux.
  {
    a.b   = 1;
    a.c.d = 2;
    a.c.e = 3;
  }.a.c
  #=> { d = 2; e = 3; }

  # Les ensembles sont immuables, il est donc impossible de rédéfinir
  # un attribut :
  {
    a = { b = 1; };
    a.b = 2;
  }
  #=> attribute 'a.b' at (string):3:5 already defined at (string):2:11

  # Cependant un attribut d'un attribut de l'ensemble peut également
  # être défini par morceaux même si l'attribut père a été directement
  # défini.
  {
    a = { b = 1; };
    a.c = 2;
  }
  #=> { a = { b = 1; c = 2; }; }


  #  With
  #=========================================

  # Le corps d'un bloc `with` est évalué avec
  # les mappings d'un ensemble liés à des variables.
  (with { a = 1; b = 2; };
    a + b)
  # => 3

  # Les affectations d'un bloc fille écrasent les affections du bloc mère.
  (with { a = 1; b = 2; };
    (with { a = 5; };
      a + b))
  #=> 7

  # La première ligne du tutoriel commence par `with builtins;`
  # car `builtins` est un ensmble qui contient toutes les fonctions
  # de base (`length`, `head`, `tail`, `filter`, etc.). Cela permet
  # de ne pas avoir à écrire `builtins.length` au lieu de simplement
  # `length` par exemple.


  #  Modèles d'ensemble
  #=========================================

  # Les ensembles sont utiles pour passer plusieurs valeurs
  # à une fonction.
  (args: args.x + "-" + args.y) { x = "a"; y = "b"; }
  #=> "a-b"

  # On peut l'écrire plus clairement en utilisant des modèles d'ensemble,
  # ou "set patterns".
  ({x, y}: x + "-" + y) { x = "a"; y = "b"; }
  #=> "a-b"

  # Par défaut, le modèle échoue si l'ensemble contient des clés
  # supplémentaires.
  ({x, y}: x + "-" + y) { x = "a"; y = "b"; z = "c"; }
  #=> error: anonymous function called with unexpected argument ‘z’

  # L'ajout de `, ...` permet d'ignorer les clés supplémentaires.
  ({x, y, ...}: x + "-" + y) { x = "a"; y = "b"; z = "c"; }
  #=> "a-b"


  #  Erreurs
  #=========================================

  # `throw` provoque l'abandon de l'évaluation avec un message d'erreur.
  (2 + (throw "foo"))
  #=> error: foo

  # `tryEval` permet de capturer les erreurs.
  (tryEval 42)
  #=> { success = true; value = 42; }
  (tryEval (2 + (throw "foo")))
  #=> { success = false; value = false; }

  # `abort` est comme `throw`, mais l'erreur est alors fatale :
  # elle ne peut pas être capturée.
  (tryEval (abort "foo"))
  #=> error: evaluation aborted with the following error message: ‘foo’

  # `assert` s'évalue à la valeur donnée si le test est vrai;
  # sinon il lève une exception capturable.
  (assert 1 < 2; 42)
  #=> 42
  (assert 1 > 2; 42)
  #=> error: assertion failed at (string):1:1
  (tryEval (assert 1 > 2; 42))
  #=> { success = false; value = false; }


  #  Impureté
  #=========================================


  # La répétabilité des constructions étant critique pour le
  # gestionnaire de paquets Nix, la pureté fonctionnelle est
  # mise en avant dans le langage Nix. Cependant, il existe des
  # impuretés.

  # Vous pouvez vous référer aux variables d'environnement.
  (getEnv "HOME")
  #=> "/home/alice"


  # La fonction `trace` est utilisée pour le débogage.
  # Elle affiche le premier argument dans `stderr` et
  # évalue le second argument.
  (trace 1 2)
  #=> trace: 1
  #=> 2

  # Vous pouvez écrire des fichiers dans le magasin Nix (Nix store).
  # Bien qu'impur, c'est assez sûr car le nom du fichier est dérivé
  # du hachage de  son contenu. On peut lire des fichiers depuis n'importe où.
  # Dans cet exemple, on écrit un fichier dans le magasin, puis on le relit.
  (let filename = toFile "foo.txt" "hello!"; in
    [filename (builtins.readFile filename)])
  #=> [ "/nix/store/ayh05aay2anx135prqp0cy34h891247x-foo.txt" "hello!" ]

  # Il est également possible de télécharger des fichiers dans le magasin Nix.
  (fetchurl "https://example.com/package-1.2.3.tgz")
  #=> "/nix/store/2drvlh8r57f19s9il42zg89rdr33m2rm-package-1.2.3.tgz"

]
```

### Pour en savoir plus (anglais)

- [Nix Manual - Nix expression language](https://nixos.org/nix/manual/#ch-expression-language)
- [James Fisher - Nix by example - Part 1: The Nix expression language](https://medium.com/@MrJamesFisher/nix-by-example-a0063a1a4c55)
- [Susan Potter - Nix Cookbook - Nix By Example](https://ops.functionalalgebra.com/nix-by-example/)
- [Rommel Martinez - A Gentle Introduction to the Nix Family](https://web.archive.org/web/20210121042658/https://ebzzry.io/en/nix/#nix)
