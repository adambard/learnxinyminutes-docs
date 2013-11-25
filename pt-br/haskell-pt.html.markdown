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

mapa mapeador _ [] = []
mapa mapeador (primeiro : resto) = mapeador primeiro : (mapa mapeador resto)

{- Uma função anônima é uma função sem um nome.
   É uma abstração do cálculo lambda:

   \x -> x + 1
   λ.x (x + 1)

   Em Haskell Barra Invertida é um jeito para
   se escrever Lambda (λ). Uma ótima pedida
   Para entender Haskell e outras linguagens como Lisp
   É estudar Cálculo Lambda, é um entendimento matemático
   mais apurado. E do ponto de vista computacional é
   bastante interessante. Em EXTRAS você encontrará
   Links para aprender Cálculo Lambda.
-}

(\x -> x + 1) 4 -- 5


{- Algumas vezes é mais conveniente usar expressões lambda
   do que definir um nome para uma função. Na matemática
   Nomes são muito simbólicos. Isso acontece bastante
   quando você estiver trabalhando `map` ou `foldl` / `foldr`
-}

-- Sem usar expressões anônimas !
listaSomaUm lst = map somaUm' lst where somaUm' x = x + 1

-- Usando expressões anônimas !
listaSomaUm' lst = map (\x -> x + 1) lst

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
   da esquerda para a direita . Você pode usar `.` e `$` para se livrar
   de parentízação desnecessária.
-}

(even (fatorial 3)) -- true

-- Usando `.` e `$`
even . fatorial $ 3 -- true

----------------------------------------------------
-- 5. Tipos
----------------------------------------------------

-- Haskell é fortemente tipado e tudo tem uma assinatura típica.

-- Tipos Básicos:
460 :: Integer
"music" :: String
True :: Bool

{- Funções também tem tipos.
   `not` recebe um booleano e retorna um booleano:
   not :: Bool -> Bool
-}

{- Aqui temos uma função que recebe dois argumentos
   soma :: Integer -> Integer -> Integer
-}

{- Quando você define um valor em Haskell
   uma boa prática de programação é escrever
   o TIPO acima dessa mesma. Como segue:
-}

double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. Controle de Fluxo e IF-THEN-ELSE
----------------------------------------------------

-- Blocos IF-THEN-ELSE
let valor_alternado = if 144 `mod` 6 == 4 then "acertou" else "errou" -- errou

-- É legal identar quando você tem múltiplos branchs para acontecer

let valor_alternado = if 144 `mod` 6 == 4
                      then "acertou"
                      else "errou"

-- Blocos CASE

{- caso <argumento> seja :
        <ajuda>  -> mostra_ajuda
        <inicia> -> inicia_programa
        <_>      -> putStrLn "ExArgumentoInvalido"

    Onde `_` Significa Qualquer Outra Coisa.
-}


case args of
     "ajuda"  -> mostra_ajuda
     "inicia" -> inicia_programa
     _        -> putStrLn "ExArgumentoInvalido"

{- Haskell não funciona na base de loops pois ele é
   fortemente baseado em funcões recursivas e cálculo lambda

   Use `map` uma função build-in do interpretador
   para, por exemplo, mapear uma lista:
-}
map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- Você pode criar um FOR-LOOP usando map
let for array funcao = map funcao array
for [0..5] $ \i -> show i

-- Ou ainda (Pesquise sobre show em Haskell):
for [0..5] show


{- foldl computação é feita esquerda para direita
   foldr computação é feita  direita para esquerda

   Você pode usar foldl or foldr a fim de reduzir uma lista
   fold(l||r) <funcao> <valor inicial> <lista>
-}

-- Fold Left
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- Pensando Recursivamente Esquerda-Direita
(2 * (2 * (2 * 4 + 1) + 2) + 3) -- 43

-- Fold Right
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- Pensando Recursivamente Direita-Esquerda
(2 * 3 + (2 * 2 + (2 * 1 + 4)))

----------------------------------------------------
-- 7. Declaração de Dados
----------------------------------------------------

{- Vamos começar definindo um tipo de
   dado que é uma cor rgb então ela
   tem valores para vermelho azul e verde
   ela é composta desses 3 comprimentos
   Vamos usar `data` e `say` que são built-in:
   
   Haskell pede que você user letra
   maiuscula para tipos (types) ou classes (Class)
   
   Por favor, visite: http://www.haskell.org/haskellwiki/Type
   E de uma olhada na fórmula genérica de declaração de dados.
-}

data Cor = Vermelho | Azul | Verde

-- say :: Color -> String

let say Vermelho = "Vermelho"
let say Azul = "Azul"
let say Verde = "Verde"

{- O seu tipo de dados por receber parâmetros também
   vamos com um exemplo usando `data` e a Classe `Maybe`.
-}

data Maybe a = Nothing | Just a

-- Just e Nothing são todos derivadas de Maybe
Just "hello" -- tipo `Maybe String`
Just 1       -- tipo `Maybe Int`
Nothing      -- tipo `Maybe a` para algum `a`

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

----------------------------------------------------
-- 9. Mônadas
----------------------------------------------------

```


# Extra

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

Comunidade Haskell
* [Musica das Mônadas](http://www.haskell.org/haskellwiki/Music_of_monads)

Tutoriais:

* [Mapeadores](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/containers-0.3.0.0/Data-Map.html)
* [Aprenda Haskell!](http://haskell.tailorfontela.com.br/chapters)
* [Fundação Teórica da Linguagem Haskell](http://www.haskell.org/haskellwiki/Lambda_calculus)
* [Classe Maybe](http://www.haskell.org/haskellwiki/Maybe)
* [Zvon Referência Haskell](http://www.zvon.org/other/haskell/)

Obtenha Também Haskell Wiki Book [Aqui!](https://en.wikibooks.org/wiki/Haskell)

Leia Sobre As Mônadas [Aqui!](http://www.haskell.org/haskellwiki/Monads)

Livro: Haskell Uma Abordagem Prática - Claudio Cesar de Sá e Márcio Ferreira da Silva
