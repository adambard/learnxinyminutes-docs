---
language: Haskell
contributors:
    - ["Adit Bhargava", "http://adit.io"]
translators:
    - ["David Baumgartner", "http://davidbaumgartner.ch"]    
lang: fr-fr
filename: learnhaskell-fr.hs
---

Haskell a été conçu pour être un langage fonctionnel pur et maniable. Il est connu pour ses monades et son système de types, mais je n'ai cesse d'y revenir pour son élégance. Pour moi, Haskell fait de la programmation une joie.

```haskell
-- Un commentaire en une ligne commence avec deux tirets.
{- Un commentaire sur plusieurs lignes peut être contenu dans
un bloc de cette façon.
-}

----------------------------------------------------
-- 1. Types de données primitifs et opérateurs
----------------------------------------------------

-- Vous avez les nombres
3 -- 3

-- Les maths sont comme vous vous y attendez
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- La division n'est pas entière par défaut
35 / 4 -- 8.75

-- division entière
35 `div` 4 -- 8

-- Les booléens sont primitifs
True
False

-- Opérations avec les booléens
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- Dans les exemples plus hauts, `not` est une fonction qui prend une valeur.
-- Haskell n'a pas besoin de parenthèses pour appeler une fonction... tous
-- les arguments sont juste listés après la fonction. Le schéma général est
-- donc :
-- func arg1 arg2 arg3...
-- Voyez la section sur les fonctions pour savoir comment écrire les vôtres.

-- Caractères et chaînes de caractère
"Ceci est une chaîne de caractère."
'a' -- caractère
'Vous ne pouvez pas utiliser des apostrophes pour les chaînes de caractère.' -- erreur !

-- Les chaînes peuvent être concaténées
"Hello " ++ "world!" -- "Hello world!"

-- Une chaîne de caractère est *réellement* une liste
"Ceci est une chaîne." !! 0 -- 'C'


----------------------------------------------------
-- Listes et tuples
----------------------------------------------------

-- Tous les éléments d'une liste doit avoir le même type.
-- les deux lignes suivantes sont semblables
[1, 2, 3, 4, 5]
[1..5]

-- Il y a aussi des listes infinies en Haskell !
[1..] -- une liste de tous les nombres naturels

-- Les listes infinies fonctionnent parce que Haskell est « paresseux »:
-- ça veut dire qu'il n'évalue que ce qui a besoin de l'être. Vous pouvez
-- donc vous demander le 1000e élément de votre liste et il vous le donnera :

[1..] !! 999 -- 1000

-- Et là, Haskell a évalué les éléments 1 à 1000 de la liste... mais le reste
-- de cette liste « infinie » n'existe pas encore ! En fait, Haskell ne va jamais 
-- le faire à moins qu'il ne le doive.

-- Adjoindre deux listes 
[1..5] ++ [6..10]

-- ajouter au début de la liste
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- l'indice d'une liste
[0..] !! 5 -- 5

-- d'autres opérations sur les listes
head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

--liste en compréhension
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

--avec un conditionnel
[x*2 | x <- [1..5], x*2 > 4] -- [6, 8, 10]

-- Chaque élément d'un tuple peut être d'un type différent, mais un
-- tuple a une longueur fixée.
-- Un tuple :
("haskell", 1)

-- accéder aux éléments d'un tuple
fst ("haskell", 1) -- "haskell"
snd ("haskell", 1) -- 1

----------------------------------------------------
-- 3. Functions
----------------------------------------------------
-- Une simple fonction qui prend deux paramètres
add a b = a + b

-- Notez que si vous utilisez ghci (l'interpréteur Haskell)
-- vous devrez utiliser `let`. Par exemple :
-- let add a b = a + b

-- Utiliser une fonction
add 1 2 -- 3

-- Vous pouvez également mettre le nom de la fonction entre les
-- deux arguments avec des accents graves :
1 `add` 2 -- 3

-- Vous pouvez également définir des fonctions qui n'ont pas de
-- lettres ! Ça vous laisse créer vos propres opérateurs ! Voilà 
-- un opérateur qui fait une division entière :
(//) a b = a `div` b
35 // 4 -- 8

-- Gardes : Une façon de gérer la valeur de vos arguments en amont
fib x
  | x < 2 = x
  | otherwise = fib (x - 1) + fib (x - 2)

-- Le filtrage par motif est similaire. Là, on a donné trois 
-- définitions différentes de `fib`. Haskell appellera automatiquement
-- la première fonction qui correspond au motif de la valeur.
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- Filtrage par motif sur un tuple.
foo (x, y) = (x + 1, y + 2)

-- Filtrage par motif sur des listes. Ici, `x` est le premier
-- élément de la liste, et `xs` le reste. On peut écrire notre
-- propre fonction `map` :
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

-- Les fonctions anonymes sont créées avec des barres obliques 
-- inverses, suivies de tous les arguments.
myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]

-- Une utilisation de fold (appelée `inject` dans quelques autres
-- langages) avec comme paramètre une fonction anonyme.
-- `foldl1` veut dire fold left -- soit littéralement pli gauche --
-- et utilise la première valeur de la liste comme accumulateur.
foldl1 (\acc x -> acc + x) [1..5] -- 15

----------------------------------------------------
-- 4. Plus de fonctions
----------------------------------------------------

-- curryfication : si vous n'appliquez pas tous les arguments à une
-- fonction, elle devient « curryfiée ». Ça veut dire qu'elle retourne
-- une fonction qui prend le reste des arguments.

add a b = a + b
foo = add 10 -- foo est une fonction qui prend un nombre et y ajoute 10
foo 5 -- 15

-- Une autre façon de l'écrire
foo = (+10)
foo 5 -- 15

-- Composition de fonctions
-- la fonction (.) enchaîne deux fonctions.
-- Par exemple, on a foo qui est une fonction qui prend une valeur, y ajoute
-- 10 et multiplie ce résultat par 5, et ensuite retourne la valeur finale.
foo = (*5) . (+10)

-- (5 + 10) * 5 = 75
foo 5 -- 75

-- fixation de priorité
-- Haskell a une autre fonction appelée `$`. Elle peut changer la priorité
-- de sorte que tout ce qu'il y a à sa gauche est calculé d'abord et ensuite 
-- appliqué à tout ce qu'il y a à droite. Vous pouvez utiliser `.` et `$` 
-- pour vous débarrasser de beaucoup de parenthèses :

-- avant
(even (fib 7)) -- False

-- ensuite
even . fib $ 7 -- False

----------------------------------------------------
-- 5. Signature de type
----------------------------------------------------

-- Haskell a un système de types très strict : par exemple, tout a un type.

-- Quelques types simples :
5 :: Integer
"hello" :: String
True :: Bool

-- Les fonctions ont également des types.
-- `not` prend un booléen et retourne un booléen.
-- not :: Bool -> Bool

-- Voilà une fonction qui prend deux paramètres.
-- add :: Integer -> Integer -> Integer

-- Quand vous définissez une valeur (souvenez-vous, tout est valeur en
-- Haskell), une bonne pratique est d'écrire son type explicitement
double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. Flux de contrôle et structures conditionnelles
----------------------------------------------------

-- structure conditionnelle if
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- les structures if peuvent être écrites sur plusieurs lignes
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- les structures case : voilà comment vous pourriez analyser les arguments de 
-- ligne de commande
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"


-- Haskell n'a pas de boucles parce qu'il utilise la récursion.
-- `map` applique une fonction sur chaque élément d'une liste

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- vous pouvez créer une fonction `for` en utilisant `map`
for array func = map func array

-- et l'utiliser
for [0..5] $ \i -> show i

-- nous aurions pu l'écrire également ainsi
for [0..5] show

-- vous pouvez utiliser foldl et foldr pour 
-- réduire une liste
-- foldl <fonction> <valeur initiale> <liste>
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- C'est donc la même chose que 
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl évalue de gauche à droite, foldr
-- de droite à gauche
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- Et c'est équivalent à
(2 * 3 + (2 * 2 + (2 * 1 + 4)))

----------------------------------------------------
-- 7. Types de données
----------------------------------------------------

-- Vous pouvez écrire vos propres types de données en Haskell

data Couleur = Rouge | Bleu | Vert

-- Et maintenant l'utiliser dans une fonction


say :: Couleur -> String
say Rouge = "Vous êtes Rouge !"
say Bleu = "Vous êtes Bleu !"
say Vert =  "Vous êtes Vert !"

-- Vos types peuvent également avoir des paramètres

data Maybe a = Nothing | Just a

-- Tous les exemples ci-dessous sont issus du type Maybe
Just "hello"    -- of type `Maybe String`
Just 1          -- of type `Maybe Int`
Nothing         -- of type `Maybe a` for any `a`

----------------------------------------------------
-- 8. Haskell IO
----------------------------------------------------

-- Tandis que l'IO ne peut pas être totalement expliqué pleinement
-- sans que les monades ne le soient, il n'est pas difficile
-- d'expliquer suffisamment pour commencer.

-- Quand un programme en Haskell est exécuté, la fonction `main`
-- est appelée. Il doit retourner une valeur de type `IO ()`.
-- Par exemple :

main :: IO ()
main = putStrLn $ "Bonjour, le ciel ! " ++ (say Blue) 
-- putStrLn a comme type String -> IO ()

-- La façon la plus simple pour faire de l'IO est de faire un programme 
-- fonction de String vers String. La fonction
--    interact :: (String -> String) -> IO ()
-- prend un texte, applique une fonction et affiche le résultat.

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- Vous pouvez considérer qu'une valeur de type `IO ()` représente
-- une séquence d'actions que l'ordinateur exécute, un peu comme 
-- dans un langage impératif. On peut utiliser la structure `do` 
-- pour enchaîner des actions. Par exemple :

sayHello :: IO ()
sayHello = do 
   putStrLn "Quel est ton nom ?"
   name <- getLine -- prend une ligne et assigne sa valeur à `name`
   putStrLn $ "Salut, " ++ name
   
-- Exercice : écrire votre propre version d'`interact` qui ne fait 
--           que de lire une ligne d'entrée.
   
-- Le code de `sayHello` ne sera jamais exécuté, cependant. La seule
-- action qui sera exécutée est la valeur de `main`.
-- Pour lancer `sayHello`, commentez l'ancienne définition de `main`
-- et remplacez-le par :
--   main = sayHello

-- Essaions de mieux comprendre comment la fonction `getLine` que 
-- nous venons d'utiliser. Son type est :
--    getLine :: IO String
-- vous pouvez considérer le type `IO a` comme un programme que
-- le programme va générer comme une valeur de type `a` quand
-- il sera exécuté. On peut l'enregistrer et la réutiliser en
-- utilisant `<-`. On peut aussi faire nos propres actions
-- de type `IO String` :

action :: IO String
action = do
   putStrLn "C'est une ligne. Heu"
   input1 <- getLine 
   input2 <- getLine
   -- Le type de la structure `do` est celui de sa dernière ligne.
   -- `return` n'est pas un mot clef, mais simplement une fonction.
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- On peut maintenant l'utiliser comme on a utilisé `getLine`
-- tout à l'heure

main'' = do
    putStrLn "Je vais afficher deux lignes !"
    result <- action 
    putStrLn result
    putStrLn "C'était tout !"

-- Le type `IO` est un exemple de « monade ». La façon dont Haskell utilise
-- une monade pour faire de l'IO lui permet d'être purement fonctionnel. N'importe
-- quelle fonction qui interagit avec le « monde extérieur » (c'est à dire fait de l'IO)
-- devient marqué comme `IO` dans la signature de son type. Ça nous montre
-- quelles fonctions sont « pures » (n'interagissent pas avec le monde extérieur
-- ou ne changent pas d'état) et quelles fonctions ne le sont pas.

-- C'est une fonctionnalité très puissante, car il est facile d'exécuter 
-- des fonctions pures simultanément, et donc la concurrence en Haskell
-- est très facile.


----------------------------------------------------
-- 9. Le REPL de Haskell
----------------------------------------------------

-- Lancer le REPL en tapant `ghci`.
-- Vous pouvez maintenant taper du code Haskell.
-- Toutes les nouvelles valeurs peuvent être crées 
-- avec `let` :

let foo = 5

-- Vous pouvez voir le type de n'importe quelle valeur avec `:t` :

>:t foo
foo :: Integer

-- Vous pouvez également lancer des actions de type `IO ()`

> sayHello
Quel est ton nom ?
Ami
Salut, Ami !

```

Et Haskell ne se limite pas à ça, on trouve encore par exemple les classes de types et les monades. Il y a beaucoup de raisons qui font que coder en Haskell est si *fun*. Je vous laisse avec un dernier exemple : une implémentation de quicksort :

```haskell
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```

Haskell facile à installer. Téléchargez-le [ici](http://www.haskell.org/platform/).

Vous pouvez trouver une approche beaucoup plus douce avec les excellents
[Learn you a Haskell](http://lyah.haskell.fr/) ou
[Real World Haskell (en)](http://book.realworldhaskell.org/).
