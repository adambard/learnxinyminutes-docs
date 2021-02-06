---
language: whip
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
  - ["Saurabh Sandav", "http://github.com/SaurabhSandav"]
author: Tenor Biel
author_url: http://github.com/L8D
translators:
  - ["Paulo Henrique Rodrigues Pinheiro", "https://github.com/paulohrpinheiro"]
lang: pt-br
filename: whip-pt.lisp
---

Whip é um dialeto de Lisp feito para construir scripts e trabalhar com
conceitos mais simples.
Ele também copia muitas funções e sintaxe de Haskell (uma linguagem não correlata)

Esse documento foi escrito pelo próprio autor da linguagem. Então é isso.

```scheme
; Comentário são como em Lisp. Pontos-e-vírgulas...

; A maioria das declarações de primeiro nível estão dentro de "listas"
; que nada mais são que coisas entre parênteses separadas por espaços em branco
nao_é_uma_lista
(uma lista)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Números, texto e operadores

; Whip tem um tipo numérico (que é um double de 64 bits IEE 754, do JavaScript)
3 ; => 3
1.5 ; => 1.5

; Funções são chamadas se elas são o primeiro elemento em uma lista
(funcao_chamada argumentos)

; A maioria das operações são feitas com funções
; Todas as funções aritméticas básicas são bem diretas
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2
; até mesmo o módulo
(% 9 4) ; => 1
; Divisão não inteira ao estilo JavaScript.
(/ 5 2) ; => 2.5

; Aninhamento de listas funciona como esperado.
(* 2 (+ 1 3)) ; => 8

; Há um tipo boleano.
true
false

; Textos são criados com ".
"Hello, world"

; Caracteres são criados com '.
'a'

; Para negação usa-se a função 'not'.
(not true) ; => false
(not false) ; => true

; Mas a maioria das funções não-haskell tem atalhos
; o atalho para "não" é um '!'.
(! (! true)) ; => true

; Igualdade é `equal` ou `=`.
(= 1 1) ; => true
(equal 2 1) ; => false

; Por exemplo, desigualdade pode ser verificada combinando as funções
;`not` e `equal`.
(! (= 2 1)) ; => true

; Mais comparações
(< 1 10) ; => true
(> 1 10) ; => false
; e suas contra partes para texto.
(lesser 1 10) ; => true
(greater 1 10) ; => false

; Texto pode ser concatenado com +.
(+ "Hello " "world!") ; => "Hello world!"

; Você pode usar as características comparativas do JavaScript.
(< 'a' 'b') ; => true
; ... e coerção de tipos
(= '5' 5)

; As funções `at` ou `@` acessarão caracteres de um texto, começando em 0.
(at 0 'a') ; => 'a'
(@ 3 "foobar") ; => 'b'

; Também existem as variáveis `null` e `undefined`.
null ; usada para indicar a ausência de algum valor
undefined ; usada para indicar que um valor não foi informado

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. Variáveis, matrizes e dicionários

; Variáveis são declaradas com as funções `def` ou `let`.
; Variáveis que não tiveram valor atribuído serão `undefined`.
(def some_var 5)
; `def` deixará a variável no contexto global.
; `let` deixará a variável no contexto local, e tem uma sintaxe estranha.
(let ((a_var 5)) (+ a_var 5)) ; => 10
(+ a_var 5) ; = undefined + 5 => undefined

; Matrizes são listas de valores de qualquer tipo.
; Elas basicamente são listas sem funções no início
(1 2 3) ; => [1, 2, 3] (sintaxe JavaScript)

; Dicionários em Whip são o equivalente a 'object' em JavaScript ou
; 'dict' em python ou 'hash' em Ruby: eles são uma coleção desordenada
de pares chave-valor.
{"key1" "value1" "key2" 2 3 3}

; Chaves podem ser apenas identificadores, números ou texto.
(def my_dict {my_key "my_value" "my other key" 4})
; Mas em Whip, dicionários são parceados como: valor, espaço, valor;
; com mais espaço entre cada. Então isso significa que
{"key" "value"
"another key"
1234
}
é avaliado da mesma forma que
{"key" "value" "another key" 1234}

; Dicionários podem ser acessados usando a função `at`
; (como em texto e listas)
(@ "my other key" my_dict) ; => 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. Lógica e controle de fluxo

; A função `if` é muito simples, ainda que muito diferente do que em muitas
linguagens imperativas.
(if true "returned if first arg is true" "returned if first arg is false")
; => "returned if first arg is true"

; E por conta do legado operador ternário
; `?` é o atalho não utilizado para `if`.
(? false true false) ; => false

; `both` é uma declaração lógica `and`, e `either` é o `or` lógico.
(both true true) ; => true
(both true false) ; => false
(either true false) ; => true
(either false false) ; => false
; E seus atalhos são
; & => both
; ^ => either
(& true true) ; => true
(^ false true) ; => true

;;;;;;;;;
; Lambdas

; Lambdas em Whip são declaradas com as funções `lambda` ou `->`.
; E funções são na verdade lambdas com nomes.
(def my_function (-> (x y) (+ (+ x y) 10)))
;         |       |    |          |
;         |       |    |    valor retornado (com escopo contento argumentos)
;         |       | argumentos
;         | declaração de funções lambda
;         |
;   nome do lambda a ser declarado

(my_function 10 10) ; = (+ (+ 10 10) 10) => 30

; Obviamente, todos os lambdas por definição são anônimos e
; tecnicamente sempre usados anonimamente. Redundância.
((lambda (x) x) 10) ; => 10

;;;;;;;;;;;;;;;;
; Comprehensions

; `range` or `..` geram uma lista dos números para
; cada número entre seus dois argumentos.
(range 1 5) ; => (1 2 3 4 5)
(.. 0 2)    ; => (0 1 2)

; `map` aplica seu primeiro argumento (que deve ser um lambda/função)
; a cada item dos argumentos seguintes (que precisa ser uma lista)
(map (-> (x) (+ x 1)) (1 2 3)) ; => (2 3 4)

; Reduce
(reduce + (.. 1 5))
; equivalente a
((+ (+ (+ 1 2) 3) 4) 5)

; Nota: map e reduce não possuem atalhos

; `slice` ou `\` é similar ao .slice() do JavaScript
; mas veja que ele pega uma lista como primeiro argumento, não o último.
(slice (.. 1 5) 2) ; => (3 4 5)
(\ (.. 0 100) -5) ; => (96 97 98 99 100)

; `append` ou `<<` são auto explicativos
(append 4 (1 2 3)) ; => (1 2 3 4)
(<< "bar" ("foo")) ; => ("foo" "bar")

; Length é auto explicativo.
(length (1 2 3)) ; => 3
(_ "foobar") ; => 6

;;;;;;;;;;;;;;;
; Delicadezas Haskell

; Primeiro item de uma lista
(head (1 2 3)) ; => 1
; Pega do segundo ao último elemento de uma lista
(tail (1 2 3)) ; => (2 3)
; Último item de uma lista
(last (1 2 3)) ; => 3
; Contrário de `tail`
(init (1 2 3)) ; => (1 2)
; Pega do primeiro até o elemento especificado da lista
(take 1 (1 2 3 4)) ; (1 2)
; Contrário de `take`
(drop 1 (1 2 3 4)) ; (3 4)
; Menor valor em uma lista
(min (1 2 3 4)) ; 1
; Maior valor em uma lista
(max (1 2 3 4)) ; 4
; Verifica se o valor está em uma lista ou objeto
(elem 1 (1 2 3)) ; true
(elem "foo" {"foo" "bar"}) ; true
(elem "bar" {"foo" "bar"}) ; false
; Inverte a ordem de uma lista
(reverse (1 2 3 4)) ; => (4 3 2 1)
; Verifica se o valor é par ou ímpar
(even 1) ; => false
(odd 1) ; => true
; Separa um texto cortando por espaço em branco
(words "foobar nachos cheese") ; => ("foobar" "nachos" "cheese")
; Junta lista de textos
(unwords ("foo" "bar")) ; => "foobar"
; Sucessor e predecessor
(pred 21) ; => 20
(succ 20) ; => 21
```

Para mais informação, verifique o [repositório](http://github.com/L8D/whip)
