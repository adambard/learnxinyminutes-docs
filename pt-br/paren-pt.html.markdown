---
language: Paren
filename: learnparen-pt.paren
contributors:
  - ["KIM Taegyoon", "https://github.com/kimtg"]
translators:
    - ["Claudson Martins", "https://github.com/claudsonm"]
lang: pt-br
---

[Paren](https://bitbucket.org/ktg/paren) é um dialeto do Lisp. É projetado para ser uma linguagem embutida.

Alguns exemplos foram retirados de <http://learnxinyminutes.com/docs/racket/>.

```scheme
;;; Comentários
# Comentários

;; Comentários de única linha começam com um ponto e vírgula ou cerquilha

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Tipos de Dados Primitivos e Operadores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Números
123 ; inteiro
3.14 ; double
6.02e+23 ; double
(int 3.14) ; => 3 : inteiro
(double 123) ; => 123 : double

;; O uso de funções é feito da seguinte maneira (f x y z ...)
;; onde f é uma função e x, y, z, ... são os operandos
;; Se você quiser criar uma lista literal de dados, use (quote) para impedir
;; que sejam interpretados
(quote (+ 1 2)) ; => (+ 1 2)
;; Agora, algumas operações aritméticas
(+ 1 1)  ; => 2
(- 8 1)  ; => 7
(* 10 2) ; => 20
(^ 2 3) ; => 8
(/ 5 2) ; => 2
(% 5 2) ; => 1
(/ 5.0 2) ; => 2.5

;;; Booleanos
true ; para verdadeiro
false ; para falso
(! true) ; => falso
(&& true false (prn "não chega aqui")) ; => falso
(|| false true (prn "não chega aqui")) ; => verdadeiro

;;; Caracteres são inteiros.
(char-at "A" 0) ; => 65
(chr 65) ; => "A"

;;; Strings são arrays de caracteres de tamanho fixo.
"Olá, mundo!"
"Sebastião \"Tim\" Maia"   ; Contra-barra é um caractere de escape
"Foo\tbar\r\n" ; Inclui os escapes da linguagem C: \t \r \n

;; Strings podem ser concatenadas também!
(strcat "Olá " "mundo!") ; => "Olá mundo!"

;; Uma string pode ser tratada como uma lista de caracteres
(char-at "Abacaxi" 0) ; => 65

;; A impressão é muito fácil
(pr "Isso é" "Paren. ") (prn "Prazer em conhecê-lo!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Variáveis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Você pode criar ou definir uma variável usando (set)
;; o nome de uma variável pode conter qualquer caracter, exceto: ();#"
(set alguma-variavel 5) ; => 5
alguma-variavel ; => 5

;; Acessar uma variável ainda não atribuída gera uma exceção
; x ; => Unknown variable: x : nil

;; Ligações locais: Utiliza cálculo lambda! 
;; 'a' e 'b' estão ligados a '1' e '2' apenas dentro de (fn ...)
((fn (a b) (+ a b)) 1 2) ; => 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Coleções
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Listas

;; Listas são estruturas de dados semelhantes a vetores. (A classe de comportamento é O(1).)
(cons 1 (cons 2 (cons 3 (list)))) ; => (1 2 3)
;; 'list' é uma variação conveniente para construir listas
(list 1 2 3) ; => (1 2 3)
;; Um quote também pode ser usado para uma lista de valores literais
(quote (+ 1 2)) ; => (+ 1 2)

;; Você ainda pode utilizar 'cons' para adicionar um item ao início da lista
(cons 0 (list 1 2 3)) ; => (0 1 2 3)

;; Listas são um tipo muito básico, portanto existe *enorme* funcionalidade
;; para elas, veja alguns exemplos:
(map inc (list 1 2 3))          ; => (2 3 4)
(filter (fn (x) (== 0 (% x 2))) (list 1 2 3 4))    ; => (2 4)
(length (list 1 2 3 4))     ; => 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Funções
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use 'fn' para criar funções.
;; Uma função sempre retorna o valor de sua última expressão
(fn () "Olá Mundo") ; => (fn () Olá Mundo) : fn

;; Use parênteses para chamar todas as funções, incluindo uma expressão lambda
((fn () "Olá Mundo")) ; => "Olá Mundo"

;; Atribuir uma função a uma variável
(set ola-mundo (fn () "Olá Mundo"))
(ola-mundo) ; => "Olá Mundo"

;; Você pode encurtar isso utilizando a definição de função açúcar sintático:
(defn ola-mundo2 () "Olá Mundo")

;; Os () acima é a lista de argumentos para a função
(set ola
  (fn (nome)
    (strcat "Olá " nome)))
(ola "Steve") ; => "Olá Steve"

;; ... ou equivalente, usando a definição açucarada:
(defn ola2 (nome)
  (strcat "Olá " name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Igualdade
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Para números utilize '=='
(== 3 3.0) ; => verdadeiro
(== 2 1) ; => falso

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Controle de Fluxo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Condicionais

(if true 				; Testa a expressão
    "isso é verdade"	; Então expressão
    "isso é falso") 	; Senão expressão
; => "isso é verdade"

;;; Laços de Repetição

;; O laço for é para número
;; (for SÍMBOLO INÍCIO FIM SALTO EXPRESSÃO ..)
(for i 0 10 2 (pr i "")) ; => Imprime 0 2 4 6 8 10
(for i 0.0 10 2.5 (pr i "")) ; => Imprime 0 2.5 5 7.5 10

;; Laço while
((fn (i)
  (while (< i 10)
    (pr i)
    (++ i))) 0) ; => Imprime 0123456789

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Mutação
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use 'set' para atribuir um novo valor a uma variável ou local
(set n 5) ; => 5
(set n (inc n)) ; => 6
n ; => 6
(set a (list 1 2)) ; => (1 2)
(set (nth 0 a) 3) ; => 3
a ; => (3 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Macros lhe permitem estender a sintaxe da linguagem.
;; Os macros no Paren são fáceis.
;; Na verdade, (defn) é um macro.
(defmacro setfn (nome ...) (set nome (fn ...)))
(defmacro defn (nome ...) (def nome (fn ...)))

;; Vamos adicionar uma notação infixa
(defmacro infix (a op ...) (op a ...))
(infix 1 + 2 (infix 3 * 4)) ; => 15

;; Macros não são higiênicos, você pode sobrescrever as variáveis já existentes!
;; Eles são transformações de códigos.
```
