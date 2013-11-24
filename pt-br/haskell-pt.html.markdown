---
linguagem: haskell
tradutor/contribuidor:
    - ["Lucas Tonussi", "http://www.inf.ufsc.br/~tonussi/"]
---

As linguagens funcionais são linguagens de programação com base em avaliação
de funções matemáticas (expressões), evitando-se o conceito de mudança de
estado com alteração de dados. Neste aspecto, este paradigma é oposto ao
paradigma imperativo que se baseia em alterações de estados.

A programação funcional começou no cálculo lambda, que foi base teórica para
o desenvolvimento deste paradigma de programação.


```haskell
-- Para comentar a linha basta dois traços seguidos.

{- Abre chaves traço e traço fecha chaves cria um campo
   para comentário em múltiplas linhas.
-}

----------------------------------------------------
-- 1. Tipos Primitivos de Dados e Operadores
----------------------------------------------------

-- Numerais

0 -- 3
1 -- 1
2 -- 2 ...

-- Alguns Operadores Fundamentais

7 + 7 -- 7 mais 7
7 - 7 -- 7 menos 7
7 * 7 -- 7 vezes 7
7 / 7 -- 7 dividido por 7

-- Divisões não são inteiras, são fracionádas por padrão da linguagem
28736 / 82374 -- 0.3488479374559934


-- Divisão inteira
82374 `div` 28736 -- 2

-- Divisão modular
82374 `mod` 28736 -- 24902

-- Booleanos como tipo primitivo de dado
True -- Verdadeiro
False -- Falso

-- Operadores unitário
not True -- Nega uma verdade
not False -- Nega uma falácia


-- Operadores binários
7 == 7 -- 7 é igual a 7 ?
7 /= 7 -- 7 é diferente de 7 ?
7 < 7 -- 7 é menor que 7 ?
7 > 7 -- 7 é maior que 7 ?


{- Haskell é uma linguagem que tem uma sintáxe bastante familiar na
   matemática, por exemplo em chamadas de funções você tem:

   NomeFunção ArgumentoA ArgumentoB ArgumentoC ...
-}

-- Strings e Caractéres
"Texto entre abre áspas e fecha áspas define uma string"
'a' -- Caractere
'A' -- Caractere

'Strings entre aspas simples sobe um erro' -- Erro léxico!

-- Concatenação de Strings
"StringA" ++ "StringB" -- "StringAStringB"

-- Concatenação de Caracteres
"haskell" == ['h','a','s','k','e','l','l'] -- True
"haskell" == 'h':'a':'s':'k':'e':'l':'l':[] -- True

-- Você pode listar uma string pelos seus caractéres
"AbBbbcAbbcbBbcbcb" !! 0 -- 'A'
"AbBbbcAbbcbBbcbcb" !! 1 -- 'b'
"AbBbbcAbbcbBbcbcb" !! 2 -- 'B'

----------------------------------------------------
-- Listas e Túplas
----------------------------------------------------

-- A construção de uma lista precisa ser de elementos homogêneos
[1, 2, 3, 4, 5] -- Homogênea
[1, a, 2, b, 3] -- Heterogênea (Erro)

-- Haskell permite que você crie sequências
[1..5]

{- Haskell usa avaliação preguiçosa o que
   permite você ter listas "infinitas".
-}

-- Uma lista "infinita" cuja razão é 1
[1..]

-- O 777º elemento de uma lista de razão 1
[1..] !! 777 -- 778

-- União de listas [lista_0] ++ [lista_1] ++ [lista_i]
[1..5] ++ [6..10] ++ [1..4] -- [1,2,3,4,5,6,7,8,9,10,1,2,3,4]

-- Adiciona um cabeçalho a sua lista e desloca a cauda
0:[1..10] -- [0, 1, 2, 3, 4, 5]
'a':['a'..'e'] -- "aabcde"

-- Indexação em uma lista
[0..] !! 5 -- 5

-- Operadores de Listas usuais
head ['a'..'e'] -- Qual o cabeçalho da lista ?
tail ['a'..'e'] -- Qual a cauda da lista ?
init ['a'..'e'] -- Qual a lista menos o último elemento ?
last ['a'..'e'] -- Qual o último elemento ?

-- Compreensão de Lista (List Comprehension)

{- Uma lista pode ser especificada
   pela definição de eus elementos.
   A compreensão de listas é feita
   com um construtor de listas que
   utiliza conceitos  e notações
   da teoria dos conjuntos.

   Exemplo:

   A = { x**2 | X pertence aos Naturais && x é par}
-}

let par x = mod x 2 == 0
let constroi_lista = [x * x | x <- [9 ..39], par x]
-- [100,144,196,256,324,400,484,576,676,784,900,1024,1156,1296,1444]

par 4 -- True
par 3 -- False


-- Listas com regras
{- Para todo x se x é elemento da lista
   faça 2 vezes x mas componha a lista
   com apenas aqueles elementos cujo
   2*x é maior que 4
-}
[x*2 | x <- [1..5], x*2 > 4] -- [6, 8, 10]

-- Tuplas
("Q", "Gamma", "b", "Sigma", "delta", "q0", "F") -- 7-Tuple Turing Machine

-- Retirando da tupla

{- Com as funções fst (primeiro) e snd (segundo)
   você só pode enviar por parâmetro uma tupla
   bi-dimensional ou seja, 2 dimensões == (x,y)
-}

fst ((2,3), [2,3]) -- (2,3)
snd ((2,3), [4,3]) -- [4,3]


----------------------------------------------------
-- 3. Funções em Haskell
----------------------------------------------------

-- Uma função simples que toma duas variáveis
{- Haskell trabalha em cima de recursão
   Portanto certifique-se que você
   Entende como recurssão funciona.
-}

soma a b = a + b -- Função que vai em um programa.hs

{- Dentro do GHCi (Interpretador Haskell)
   Você terá que fazer da seguinte maneira-- Podemos criar nos

   Prelude> let soma a b = a + b
   Prelude> soma 7 7 -- 14
-}

let constroi_lista = [x * x | x <- [9 ..39], par x]

{- Você pode usar crases para chamar
   Funcões de maneira diferente
-}

7 `soma` 7 -- 14

{- Haskell permite que você crie os
   seus próprios operadores baseados
   nos já existendes
-}

let (~/\) a b = a `mod` b
15^13 ~/\ 432 -- 759375

-- Casamento de Padrões em Tuplas
coordenadas (x, y) = (x + 13, y - 31)

{- Haskell trabalha com casamento de padrões onde dada
   um conjunto de funções definidas de diferentes maneiras
   Haskell vai procurar por aquela que trabalha o seu tipo
   de entrada.
-}

-- Guardas "|" É um jeito simples de representar funções recursivas

let fatorial n | n == 0 = 1 | otherwise = fatorial (n - 1) * n -- Teste: fatorial 5

-- Ainda podemos fazer:

let fatorial 0 = 1
let fatorial n = fatorial (n - 1) * n

{- Podemos criar nossos próprios Mapeadores
   Onde `primeiro` é o primeiro elemento de
   uma lista é `resto`  é o resto da lista.
-}

mapa mapeador [] = []
mapa mapeador (primeiro : resto) = mapeador primeiro : (mapa mapeador resto)

{- Funções Anônimas são criadas com um `\` (barra invertida)
   seguido por seus argumentos!
-}
mapa (\primeiro -> primeiro + 2) [1..5] -- [3, 4, 5, 6, 7]

{- Usar "fold" (chamado `inject` em algumas outras línguagens) com
   uma função anônima.

   <foldl1> significa <fold left> E mapeia o primeiro valor
   da lista para ser o acumulador.
-}
foldl1 (\acc x -> acc + x) [1..5] -- 15
foldl1 (\x y -> (x+y)/5) [7..55] -- 13.6875

----------------------------------------------------
-- 4. Mais Funções
----------------------------------------------------

{- Currying: Se você não passar todos os argumentos
   para uma função, ela irá ser "currificada". O que
   significa que irá retornar a função que pega o resto
   dos elementos.
-}

soma a b = a + b
foo = soma 10 -- foo ganha a propriedade "currying"
foo 5 -- 15

-- Outra maneira
foo = (+10)
foo 5 -- 15

{- Composição de Funções
   O (.) encadeia funções! Por exemplo,
   aqui foo é uma função que recebe um valor.
   Ela soma 10 a ela, multiplica o resultado por 5
   e então retorna o resultado final.
-}
foo = (*5) . (+10)

-- (5 + 10) * 5 = 75
foo 5 -- 75

{- Concertando precedência:
   Haskell tem outra função chamada `$`. Isso altera a precedência
   de computação. Ou seja Haskell computa o que está sendo sinalizado com $
   da esquerda para a direita . You can use `.` and `$` to get rid of a lot
   of parentheses:
-}

-- Antes
(even (fatorial 3)) -- true

-- Depois
even . fatorial $ 3 -- true

----------------------------------------------------
-- 4. More functions
----------------------------------------------------

{- Mais Sobre Funções Currificadas: se você não passar
   todos os argumentos para uma função.
-}

add a b = a + b
foo = add 10 -- foo is now a function that takes a number and adds 10 to it
foo 5 -- 15

-- Another way to write the same thing
foo = (+10)
foo 5 -- 15

-- function composition
-- the (.) function chains functions together.
-- For example, here foo is a function that takes a value. It adds 10 to it,
-- multiplies the result of that by 5, and then returns the final value.
foo = (*5) . (+10)

-- (5 + 10) * 5 = 75
foo 5 -- 75

-- fixing precedence
-- Haskell has another function called `$`. This changes the precedence
-- so that everything to the left of it gets computed first and then applied
-- to everything on the right. You can use `.` and `$` to get rid of a lot
-- of parentheses:

-- before
(even (fib 7)) -- true

-- after
even . fib $ 7 -- true

----------------------------------------------------
-- 5. Type signatures
----------------------------------------------------

-- Haskell has a very strong type system, and everything has a type signature.

-- Some basic types:
5 :: Integer
"hello" :: String
True :: Bool

-- Functions have types too.
-- `not` takes a boolean and returns a boolean:
-- not :: Bool -> Bool

-- Here's a function that takes two arguments:
-- add :: Integer -> Integer -> Integer

-- When you define a value, it's good practice to write its type above it:
double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. Control Flow and If Statements
----------------------------------------------------

-- if statements
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- if statements can be on multiple lines too, indentation is important
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- case statements: Here's how you could parse command line arguments
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"

-- Haskell doesn't have loops because it uses recursion instead.
-- map applies a function over every element in an array

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- you can make a for function using map
for array func = map func array

-- and then use it
for [0..5] $ \i -> show i

-- we could've written that like this too:
for [0..5] show

-- You can use foldl or foldr to reduce a list
-- foldl <fn> <initial value> <list>
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- This is the same as
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl is left-handed, foldr is right-
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- This is now the same as
(2 * 3 + (2 * 2 + (2 * 1 + 4)))

----------------------------------------------------
-- 7. Data Types
----------------------------------------------------

-- Here's how you make your own data type in Haskell

data Color = Red | Blue | Green

-- Now you can use it in a function:


say :: Color -> String
say Red = "You are Red!"
say Blue = "You are Blue!"
say Green =  "You are Green!"

-- Your data types can have parameters too:

data Maybe a = Nothing | Just a

-- These are all of type Maybe
Just "hello"    -- of type `Maybe String`
Just 1          -- of type `Maybe Int`
Nothing         -- of type `Maybe a` for any `a`

----------------------------------------------------
-- 8. Haskell IO
----------------------------------------------------

-- While IO can't be explained fully without explaining monads,
-- it is not hard to explain enough to get going.

-- When a Haskell program is executed, the function `main` is
-- called. It must return a value of type `IO ()`. For example:

main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue)
-- putStrLn has type String -> IO ()

-- It is easiest to do IO if you can implement your program as
-- a function from String to String. The function
--    interact :: (String -> String) -> IO ()
-- inputs some text, runs a function on it, and prints out the
-- output.

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- You can think of a value of type `IO ()` as representing a
-- sequence of actions for the computer to do, much like a
-- computer program written in an imperative language. We can use
-- the `do` notation to chain actions together. For example:

sayHello :: IO ()
sayHello = do
   putStrLn "What is your name?"
   name <- getLine -- this gets a line and gives it the name "name"
   putStrLn $ "Hello, " ++ name

-- Exercise: write your own version of `interact` that only reads
--           one line of input.

-- The code in `sayHello` will never be executed, however. The only
-- action that ever gets executed is the value of `main`.
-- To run `sayHello` comment out the above definition of `main`
-- and replace it with:
--   main = sayHello

-- Let's understand better how the function `getLine` we just
-- used works. Its type is:
--    getLine :: IO String
-- You can think of a value of type `IO a` as representing a
-- computer program that will generate a value of type `a`
-- when executed (in addition to anything else it does). We can
-- store and reuse this value using `<-`. We can also
-- make our own action of type `IO String`:

action :: IO String
action = do
   putStrLn "This is a line. Duh"
   input1 <- getLine
   input2 <- getLine
   -- The type of the `do` statement is that of its last line.
   -- `return` is not a keyword, but merely a function
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- We can use this just like we used `getLine`:

main'' = do
    putStrLn "I will echo two lines!"
    result <- action
    putStrLn result
    putStrLn "This was all, folks!"

-- The type `IO` is an example of a "monad". The way Haskell uses a monad to
-- do IO allows it to be a purely functional language. Any function that
-- interacts with the outside world (i.e. does IO) gets marked as `IO` in its
-- type signature. This lets us reason about what functions are "pure" (don't
-- interact with the outside world or modify state) and what functions aren't.

-- This is a powerful feature, because it's easy to run pure functions
-- concurrently; so, concurrency in Haskell is very easy.


----------------------------------------------------
-- 9. O Haskell REPL (Read Eval Print Loop)
----------------------------------------------------

{- Digite dhci no seu terminal
   para começar o interpretador
   lembre-se que para definir
   funções e variáveis em haskell
   pelo interpretador você precisar
   iniciar com `let`
-}

Prelude> let foo = 1.4

-- Você pode ver o tipo de algo usando `:t`:

Prelude> :t foo
foo :: Double
```

----------------------------------------------------
-- 9. Mônadas
----------------------------------------------------




----------------------------------------------------
-- 10. Extra
----------------------------------------------------

Compilador e Interpretador Haskell

* [GHC](http://www.haskell.org/ghc/docs/latest/html/users_guide/index.html)
* [GHC/GHCi](http://www.haskell.org/haskellwiki/GHC)

Instale Haskell [Aqui!](http://www.haskell.org/platform/).

Aplicações Haskell Muito Interessantes:

* [Música e Som](http://www.haskell.org/haskellwiki/Applications_and_libraries/Music_and_sound)
* [Haskell SuperCollider Servidor](https://github.com/kaoskorobase/hsc3-server)
* [Haskell SuperCollider Cliente](http://hackage.haskell.org/package/hsc3)
* [Física e Matemática](http://www.haskell.org/haskellwiki/Applications_and_libraries/Mathematics)
* [Jogos](http://www.haskell.org/haskellwiki/Applications_and_libraries/Games)
* [Bio Informática](http://www.haskell.org/haskellwiki/Applications_and_libraries/Bioinformatics)
* [Muitos Outras Aplicações](http://www.haskell.org/haskellwiki/Libraries_and_tools)

Tutoriais:

* [Mapeadores](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/containers-0.3.0.0/Data-Map.html)
* [Aprenda Haskell!](http://haskell.tailorfontela.com.br/chapters)

Obtenha Também Haskell Wiki Book [Aqui!](https://en.wikibooks.org/wiki/Haskell)

Leia Sobre As Mônadas [Aqui!](http://www.haskell.org/haskellwiki/Monads)

Livro: Haskell Uma Abordagem Prática - Claudio Cesar de Sá e Márcio Ferreira da Silva
