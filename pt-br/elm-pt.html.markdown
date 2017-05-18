---
language: Elm
contributors:
    - ["Max Goldstein", "http://maxgoldste.in/"]
translators:
    - ["Marcel dos Santos", "https://twitter.com/marcelgsantos"]
lang: pt-br
filename: learnelm-pt.elm
---

Elm é uma linguagem de programação funcional reativa que compila para (client-side)
JavaScript. Elm é estaticamente tipada, significando que o compilador captura a
maioria dos erros imediatamente e fornece uma mensagem de erro limpa e compreensível.
Elm é excelente para projetar interfaces de usuário e jogos para a web.


```haskell
-- Comentários de uma linha começam com dois traços.
{- Comentários de múltiplas linhas podem ser delimitados em um bloco como este.
{- Eles podem ser aninhados. -}
-}

{-- O Básico --}

-- Operações Aritméticas
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20

-- Cada número literal sem um ponto decimal pode ser um Int ou um Float.
33 / 2 -- 16.5 com divisão de ponto flutuante
33 // 2 -- 16 com divisão inteira

-- Exponenciação
5 ^ 2 -- 25

-- Booleanos
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- Strings e caracteres
"Esta é uma string porque ela utiliza aspas duplas."
'a' -- caracteres entre aspas simples

-- Strings podem ser anexadas.
"Olá " ++ "mundo!" -- "Olá mundo!"

{-- Listas, Tuplas e Registros --}

-- Cada elemento em uma lista deve ter o mesmo tipo.
["the", "quick", "brown", "fox"]
[1, 2, 3, 4, 5]
-- O segundo exemplo também pode ser escrito com dois pontos.
[1..5]

-- Junte listas da mesma forma que strings.
[1..5] ++ [6..10] == [1..10] -- True

-- Para adicionar um item utilize "cons".
0 :: [1..5] -- [0, 1, 2, 3, 4, 5]

-- A cabeça e a cauda de uma lista são retornadas como uma Maybe. Em vez de
-- verificar cada valor para ver se ele é nulo, você lida com os valores
-- faltantes explicitamente.
List.head [1..5] -- Just 1
List.tail [1..5] -- Just [2, 3, 4, 5]
List.head [] -- Nothing
-- List.functionName siginifica que a função faz parte do módulo List.

-- Cada elemento em uma tupla pode ser de um tipo diferente, mas uma tupla
-- tem um comprimento fixo.
("elm", 42)

-- Acesse os elementos de um par com as funções first e second.
-- (Este é um atalho; nós iremos para o "caminho real" em breve.)
fst ("elm", 42) -- "elm"
snd ("elm", 42) -- 42

-- Uma tupla vazia ou "unidade" às vezes é utilizada como um placeholder.
-- É o único valor de seu tipo, também chamado de "Unit".
()

-- Registros são como tuplas mas os campos possuem nomes. A ordem dos campos
-- não importa. Observe que os valores dos registros utilizam sinais de igual,
-- e não dois-pontos.
{ x = 3, y = 7 }

-- Acesse um campo com um ponto e o nome do campo.
{ x = 3, y = 7 }.x -- 3

-- Ou com uma função acessora, que é um ponto e o nome do próprio campo.
.y { x = 3, y = 7 } -- 7

-- Atualiza os campos de um registro. (Ele já deve ter os campos.)
{ person |
  name = "George" }

-- Atualiza vários campos de uma só vez utilizando os valores atuais.
{ particle |
  position = particle.position + particle.velocity,
  velocity = particle.velocity + particle.acceleration }

{-- Fluxo de Controle --}

-- Declarações if sempre devem ter um else e os valores devem ser do mesmo tipo.
if powerLevel > 9000 then
  "WHOA!"
else
  "meh"

-- Declarações if podem ser encadeadas.
if n < 0 then
  "n é negativo"
else if n > 0 then
  "n é positivo"
else
  "n é zero"

-- Utilize declarações case para casar padrões entre diferentes possibilidades.
case aList of
  [] -> "casa com uma lista vazia"
  [x]-> "casa com uma lista de exatamente um item, " ++ toString x
  x::xs -> "casa com uma lista de pelo menos um item cuja cabeça é " ++ toString x
-- O casamento do padrão acontece na ordem. Se colocarmos [x] por último, ele
-- nunca iria casar porque x::xs também casa (xs seria a lista vazia). Os
-- casamentos não "falham".
-- O compilador irá alertá-lo sobre casos faltantes ou extras.

-- Casa padrão com um Maybe.
case List.head aList of
  Just x -> "A cabeça é " ++ toString x
  Nothing -> "A lista estava vazia."

{-- Funções --}

-- A sintaxe do Elm é muito mínima, baseando-se principalmente em espaços em
-- branco em vez de parênteses e chaves. Não existe a palavra-chave "return".

-- Define uma função com seu nome, argumentos, um sinal de igual e o corpo.
multiply a b =
  a * b

-- Aplica (chama) uma função passando seus argumentos (vírgulas não necessárias).
multiply 7 6 -- 42

-- Aplica parcialmente uma função passando somente alguns de seus argumentos.
-- Dando, em seguida, um novo nome a função.
double =
  multiply 2

-- Constantes são semelhantes, exceto que não há argumentos.
answer =
  42

-- Passa funções como argumentos para outras funções.
List.map double [1..4] -- [2, 4, 6, 8]

-- Ou escreva uma função anônima.
List.map (\a -> a * 2) [1..4] -- [2, 4, 6, 8]

-- Você pode casar um padrão na definição de funções quando há somente um caso.
-- Esta função recebe uma tupla em vez de dois argumentos.
-- Esta é a maneira que você normalmente vai desempacotar/extrair valores de tuplas.
area (width, height) =
  width * height

area (6, 7) -- 42

-- Utilize chaves para casar o padrão de nomes de campos de um registro.
-- Utilize let para definir valores intermediários.
volume {width, height, depth} =
  let
    area = width * height
  in
    area * depth

volume { width = 3, height = 2, depth = 7 } -- 42

-- Funções podem ser recursivas.
fib n =
  if n < 2 then
    1
  else
    fib (n - 1) + fib (n - 2)

List.map fib [0..8] -- [1, 1, 2, 3, 5, 8, 13, 21, 34]

-- Outra função recursiva (utilize List.length em um código de verdade).
listLength aList =
  case aList of
    [] -> 0
    x::xs -> 1 + listLength xs

-- Chamadas de funções acontecem antes de qualquer operador infixo.
-- Os parênteses indicam a precendência.
cos (degrees 30) ^ 2 + sin (degrees 30) ^ 2 -- 1
-- Primeiro degrees é aplicada em 30, então o resultado é passado para as
-- funções de trigonometria, que então é elevado ao quadrado e, por fim, a
-- adição acontece.

{-- Tipos e Anotações de Tipos --}

-- O compilador irá inferir o tipo de cada valor em seu programa.
-- Tipos iniciam com letra maiúscula. Leia x : T como "x é do tipo T".
-- Alguns tipos comuns que você pode ver no REPL do Elm.
5 : Int
6.7 : Float
"hello" : String
True : Bool

-- Funções têm tipos também. Leia -> como "vai para". Pense no tipo mais à
-- direita como o tipo do valor de retorno e os outros como argumentos.
not : Bool -> Bool
round : Float -> Int

-- Quando você define um valor, é uma boa prática escrever seu tipo acima dele.
-- A anotação é uma forma de documentação, que é verifica pelo compilador.
double : Int -> Int
double x = x * 2

-- Argumentos de uma função são passados entre parênteses.
-- Tipos com letra minúscula são tipos variáveis: eles podem ser de qualquer
-- tipo, desde que cada chamada seja consistente.
List.map : (a -> b) -> List a -> List b
-- "List.map é do tipo a-vai-para-b, vai para lista de a e vai para lista de b."

-- Existem três tipos especiais com minúsculas: number, comparable e appendable.
-- Numbers permite que você utilize aritmética em Ints e Floats.
-- Comparable permite você ordenar números e strings, como a < b.
-- Appendable permite que coisas possam ser combinadas com a ++ b.

{-- Type Aliases e Union Types --}

-- Quando você escreve um registro ou uma tupla, seu tipo já existe.
-- (Observe que os tipos de um registro utilizam dois-pontos e os valores de um
-- registro utilizam igual.)
origin : { x : Float, y : Float, z : Float }
origin =
  { x = 0, y = 0, z = 0 }

-- Você pode dar um bom nome para tipos existentes com um type alias.
type alias Point3D =
  { x : Float, y : Float, z : Float }

-- Se você cria um alias para um registro, você pode usar o nome como uma
-- função construtora.
otherOrigin : Point3D
otherOrigin =
  Point3D 0 0 0

-- Mas ele ainda é do mesmo tipo, então você pode compará-los.
origin == otherOrigin -- True

-- Por outro lado, a definição de um union type cria um tipo que não existia
-- antes. Um union type é chamado assim porque ele pode ser uma de muitas
-- possibilidades. Cada uma das possibilidades é representada como uma "tag".
type Direction =
  North | South | East | West

-- As tags podem levar outros valores de tipos conhecidos. Isso pode trabalhar
-- recursivamente.
type IntTree =
  Leaf | Node Int IntTree IntTree
-- "Leaf" e "Node" são as tags. Tudo após uma tag é um tipo.

-- As tags podem ser usadas como valores ou funções.
root : IntTree
root =
  Node 7 Leaf Leaf

-- Union types (e type aliases) podem utilizar tipos variáveis.
type Tree a =
  Leaf | Node a (Tree a) (Tree a)
-- "O tipo árvore-de-a é uma folha ou um nó de a, árvore-de-a e árvore-de-a."

-- Casa padrão com union tags. As tags maiúsculas serão casadas de maneira exa-
-- ta. As variáveis minúsculas irão casar com qualquer coisa. Sublinhado também
-- casa com qualquer coisa, mas siginifica que você não o está utilizando.
leftmostElement : Tree a -> Maybe a
leftmostElement tree =
  case tree of
    Leaf -> Nothing
    Node x Leaf _ -> Just x
    Node _ subtree _ -> leftmostElement subtree

-- Isso é praticamente a própria linguagem. Agora vamos ver como organizar e
-- executar seu código.

{-- Módulos e Imports --}

-- As bibliotecas internas são organizadas em módulos, assim como quaisquer
-- bibliotecas de terceiros que você possa utilizar. Para grandes projetos,
-- você pode definir seus próprios módulos.

-- Coloque isso no topo do arquivo. Se for omitido, você está no Main.
module Name where

-- Por padrão, tudo é exportado. Você pode especificar as exportações de forma
-- explícita.
module Name (MyType, myValue) where

-- Um padrão comum é exportar um union type mas não suas tags. Isto é conhecido
-- como "tipo opaco" e é frequentemente utilizado em bibliotecas.

-- Importe código de outros módulos para utilizá-lo no seu código.
-- Coloque Dict no escopo para você poder chamar Dict.insert.
import Dict

-- Importe o módulo Dict e o tipo Dict para que suas anotações não tenham que
-- dizer Dict.Dict. Você ainda pode utilizar Dict.insert.
import Dict exposing (Dict)

-- Renomeie um import.
import Graphics.Collage as C

{-- Portas --}

-- Uma porta indica que você estará se comunicando com o mundo exterior.
-- Portas são permitidas somente no módulo Main.

-- Uma porta de entrada é apenas uma assinatura de tipo.
port clientID : Int

-- Uma porta de saída tem uma definição.
port clientOrders : List String
port clientOrders = ["Books", "Groceries", "Furniture"]

-- Não vamos entrar em detalhes, mas você configura callbacks no JavaScript
-- para enviar nas portas de entrada e receber nas portas de saída.

{-- Ferramentas de Linha de Comando --}

-- Compila um arquivo.
$ elm make MyFile.elm

-- A primeira vez que você fizer isso, o Elm instalará as bibliotecas internas
-- e criará o elm-package.json, onde a informação sobre seu projeto é mantida.

-- O reactor é um servidor que compila e roda seus arquivos.
-- Clique na chave ao lado dos nomes de arquivo para entrar no depurador de
-- viagem no tempo.
$ elm reactor

-- Teste expressões simples no Read-Eval-Print Loop.
$ elm repl

-- Pacotes são identificados pelo usuário e nome do repositório no GitHub.
-- Instale um novo pacote e registre-o no elm-package.json.
$ elm package install evancz/elm-html

-- Veja o que mudou entre as versões de um pacote.
$ elm package diff evancz/elm-html 3.0.0 4.0.2
-- O gerenciador de pacotes do Elm obriga o versionamento semântico, logo
-- mudanças de versões no minor nunca quebrará o seu build!
```

A linguagem Elm é supreendentemente pequena. Agora você pode olhar para quase
qualquer código-fonte em Elm e ter uma ideia aproximada do que está acontecendo.
No entanto, as possibilidades para código resistente a erros e de fácil
refatoração são infinitas!

Aqui estão algumas referências utéis.

* O [site do Elm](http://elm-lang.org/). Ele inclui:
  * Links para os [instaladores](http://elm-lang.org/install)
  * [Documentação](http://elm-lang.org/docs), incluindo [a referência de sintaxe](http://elm-lang.org/docs/syntax)
  * Muitos [exemplos](http://elm-lang.org/examples) úteis

* Documentação para as [bibliotecas internas do Elm](http://package.elm-lang.org/packages/elm-lang/core/latest/). Tome nota de:
  * [Basics](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics), que é importada por padrão
  * [Maybe](http://package.elm-lang.org/packages/elm-lang/core/latest/Maybe) e seu primo [Result](http://package.elm-lang.org/packages/elm-lang/core/latest/Result), comumente utilizados para valores faltantes e manipulação de erros
  * Estruturas de dados como [List](http://package.elm-lang.org/packages/elm-lang/core/latest/List), [Array](http://package.elm-lang.org/packages/elm-lang/core/latest/Array), [Dict](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict) e [Set](http://package.elm-lang.org/packages/elm-lang/core/latest/Set)
  * [Codificação](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode) e [decodificação](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode) JSON

* [A Arquitetura Elm](https://github.com/evancz/elm-architecture-tutorial#the-elm-architecture). Uma dissertação pelo criador do Elm com exemplos sobre como organizar código em componentes.

* A [lista de e-mail do Elm](https://groups.google.com/forum/#!forum/elm-discuss). Todos são amigáveis e solícitos.

* [Escopo em Elm](https://github.com/elm-guides/elm-for-js/blob/master/Scope.md#scope-in-elm) e [Como Ler uma Anotação de Tipo](https://github.com/elm-guides/elm-for-js/blob/master/How%20to%20Read%20a%20Type%20Annotation.md#how-to-read-a-type-annotation). Mais sobre guias da comunidade sobre o básico de Elm escrito por desenvolvedores JavaScript.

Saia e escreva algum código Elm!
