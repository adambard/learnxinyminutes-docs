---
language: hy
filename: learnhy-pt.hy
contributors:
    - ["Abhishek L", "http://twitter.com/abhishekl"]
translators:
    - ["Miguel Araújo", "https://github.com/miguelarauj1o"]
lang: pt-br
---

Hy é um dialeto de Lisp escrito sobre Python. Isto é possível convertendo
código Hy em árvore sintática abstrata python (ast). Portanto, isto permite
hy chamar código python nativo e vice-versa.

Este tutorial funciona para hy ≥ 0.9.12

```clojure
;; Isso dá uma introdução básica em hy, como uma preliminar para o link abaixo
;; http://try-hy.appspot.com
;;
; Comentários em ponto-e-vírgula, como em outros LISPS

;; s-noções básicas de expressão
; programas Lisp são feitos de expressões simbólicas ou sexps que se assemelham
(some-function args)
; agora o essencial "Olá mundo"
(print "hello world")

;; Tipos de dados simples
; Todos os tipos de dados simples são exatamente semelhantes aos seus homólogos 
; em python que
42 ; => 42
3.14 ; => 3.14
True ; => True
4+10j ; => (4+10j) um número complexo

; Vamos começar com um pouco de aritmética muito simples
(+ 4 1) ;=> 5
; o operador é aplicado a todos os argumentos, como outros lisps
(+ 4 1 2 3) ;=> 10
(- 2 1) ;=> 1
(* 4 2) ;=> 8
(/ 4 1) ;=> 4
(% 4 2) ;=> 0 o operador módulo
; exponenciação é representado pelo operador ** como python
(** 3 2) ;=> 9
; formas aninhadas vão fazer a coisa esperada
(+ 2 (* 4 2)) ;=> 10
; também operadores lógicos e ou não e igual etc. faz como esperado
(= 5 4) ;=> False
(not (= 5 4)) ;=> True

;; variáveis
; variáveis são definidas usando SETV, nomes de variáveis podem usar utf-8, exceto
; for ()[]{}",'`;#|
(setv a 42)
(setv π 3.14159)
(def *foo* 42)
;; outros tipos de dados de armazenamento
; strings, lists, tuples & dicts
; estes são exatamente os mesmos tipos de armazenamento de python
"hello world" ;=> "hello world"
; operações de string funcionam semelhante em python
(+ "hello " "world") ;=> "hello world"
; Listas são criadas usando [], a indexação começa em 0
(setv mylist [1 2 3 4])
; tuplas são estruturas de dados imutáveis
(setv mytuple (, 1 2))
; dicionários são pares de valores-chave
(setv dict1 {"key1" 42 "key2" 21})
; :nome pode ser utilizado para definir palavras-chave em hy que podem ser utilizados para as chaves
(setv dict2 {:key1 41 :key2 20})
; usar 'get' para obter o elemento em um índice/key
(get mylist 1) ;=> 2
(get dict1 "key1") ;=> 42
; Alternativamente, se foram utilizadas palavras-chave que podem ser chamadas diretamente
(:key1 dict2) ;=> 41

;; funções e outras estruturas de programa
; funções são definidas usando defn, o último sexp é devolvido por padrão
(defn greet [name]
  "A simple greeting" ; uma docstring opcional
  (print "hello " name))

(greet "bilbo") ;=> "hello bilbo"

; funções podem ter argumentos opcionais, bem como argumentos-chave
(defn foolists [arg1 &optional [arg2 2]]
  [arg1 arg2])

(foolists 3) ;=> [3 2]
(foolists 10 3) ;=> [10 3]

; funções anônimas são criados usando construtores 'fn' ou 'lambda'
; que são semelhantes para 'defn'
(map (fn [x] (* x x)) [1 2 3 4]) ;=> [1 4 9 16]

;; operações de sequência
; hy tem algumas utils embutidas para operações de sequência, etc.
; recuperar o primeiro elemento usando 'first' ou 'car'
(setv mylist [1 2 3 4])
(setv mydict {"a" 1 "b" 2})
(first mylist) ;=> 1

; corte listas usando 'slice'
(slice mylist 1 3) ;=> [2 3]

; obter elementos de uma lista ou dict usando 'get'
(get mylist 1) ;=> 2
(get mydict "b") ;=> 2
; lista de indexação começa a partir de 0, igual em python
; assoc pode definir elementos em chaves/índices
(assoc mylist 2 10) ; faz mylist [1 2 10 4]
(assoc mydict "c" 3) ; faz mydict {"a" 1 "b" 2 "c" 3}
; há toda uma série de outras funções essenciais que torna o trabalho com 
; sequências uma diversão

;; Python interop
;; importação funciona exatamente como em python
(import datetime)
(import [functools [partial reduce]]) ; importa fun1 e fun2 do module1
(import [matplotlib.pyplot :as plt]) ; fazendo uma importação em foo como em bar
; todos os métodos de python embutidas etc. são acessíveis a partir hy
; a.foo(arg) is called as (.foo a arg)
(.split (.strip "hello world  ")) ;=> ["hello" "world"]

;; Condicionais
; (if condition (body-if-true) (body-if-false)
(if (= passcode "moria")
  (print "welcome")
  (print "Speak friend, and Enter!"))

; aninhe múltiplas cláusulas 'if else if' com cond
(cond
 [(= someval 42)
  (print "Life, universe and everything else!")]
 [(> someval 42)
  (print "val too large")]
 [(< someval 42)
  (print "val too small")])

; declarações de grupo com 'do', essas são executadas sequencialmente
; formas como defn tem um 'do' implícito
(do
 (setv someval 10)
 (print "someval is set to " someval)) ;=> 10

; criar ligações lexicais com 'let', todas as variáveis definidas desta forma
; tem escopo local
(let [[nemesis {"superman" "lex luther"
                "sherlock" "moriarty"
                "seinfeld" "newman"}]]
  (for [(, h v) (.items nemesis)]
	(print (.format "{0}'s nemesis was {1}" h v))))

;; classes
; classes são definidas da seguinte maneira
(defclass Wizard [object]
  [[--init-- (fn [self spell]
             (setv self.spell spell) ; init a mágica attr
             None)]
   [get-spell (fn [self]
              self.spell)]])

;; acesse hylang.org
```

### Outras Leituras

Este tutorial é apenas uma introdução básica para hy/lisp/python.

Docs Hy: [http://hy.readthedocs.org](http://hy.readthedocs.org)

Repo Hy no GitHub: [http://github.com/hylang/hy](http://github.com/hylang/hy)

Acesso ao freenode irc com #hy, hashtag no twitter: #hylang
