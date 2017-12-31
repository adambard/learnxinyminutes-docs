---
language: clojure
filename: learnclojure-pt.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Raphael Bezerra do Nascimento"]
lang: pt-br
---

Como todas as Lisps, a inerente [homoiconicity](https://en.wikipedia.org/wiki/Homoiconic)
do Clojure lhe dá acesso a toda a extensão da linguagem 
para escrever rotinas de geração de código chamados "macros". Macros fornecem uma poderosa forma de adequar a linguagem 
às suas necessidades.

Pórem Tenha cuidado. É considerado má pratica escrever uma macro quando uma função vai fazer. Use uma macro apenas 
quando você precisar do controle sobre quando ou se os argumentos para um formulário será avaliado.

Você vai querer estar familiarizado com Clojure. Certifique-se de entender tudo em 
[Clojure em Y Minutos](/docs/clojure/).

```clojure
;; Defina uma macro utilizando defmacro. Sua macro deve ter como saida uma lista que possa
;; ser avaliada como codigo Clojure.
;;
;; Essa macro é a mesma coisa que se você escrever (reverse "Hello World")
(defmacro my-first-macro []
  (list reverse "Hello World"))

;; Inspecione o resultado de uma macro utilizando macroexpand or macroexpand-1.
;;
;; Note que a chamada deve utilizar aspas simples.
(macroexpand '(my-first-macro))
;; -> (#<core$reverse clojure.core$reverse@xxxxxxxx> "Hello World")

;; Você pode avaliar o resultad de macroexpand diretamente:
(eval (macroexpand '(my-first-macro)))
; -> (\d \l \o \r \W \space \o \l \l \e \H)

;; mas você deve usar esse mais suscinto, sintax como de função:
(my-first-macro)  ; -> (\d \l \o \r \W \space \o \l \l \e \H)

;; Você pode tornar as coisas mais faceis pra você, utilizando a sintaxe de citação mais suscinta
;; para criar listas nas suas macros:
(defmacro my-first-quoted-macro []
  '(reverse "Hello World"))

(macroexpand '(my-first-quoted-macro))
;; -> (reverse "Hello World")
;; Note que reverse não é mais uma função objeto, mas um simbolo.

;; Macros podem ter argumentos.
(defmacro inc2 [arg]
  (list + 2 arg))

(inc2 2) ; -> 4

;; Mas se você tentar fazer isso com uma lista entre aspas simples, você vai receber um erro, por que o 
;; argumento irá entra aspas simples também. Para contornar isso, Clojure prover uma maneira de utilizar aspas simples 
;; em macros: `. Dentro `, você pode usar ~ para chegar ao escopo externo.
(defmacro inc2-quoted [arg]
  `(+ 2 ~arg))

(inc2-quoted 2)

;; Você pode usar os argumentos de destruturação habituais. Expandir lista de variaveis usando ~@
(defmacro unless [arg & body]
  `(if (not ~arg)
     (do ~@body))) ; Lembrar o do!

(macroexpand '(unless true (reverse "Hello World")))
;; ->
;; (if (clojure.core/not true) (do (reverse "Hello World")))

;; (unless) avalia e retorna seu corpo, se o primeiro argumento é falso.
;; caso contrario, retorna nil

(unless true "Hello") ; -> nil
(unless false "Hello") ; -> "Hello"

;; Usado sem cuidados, macros podem fazer muito mal por sobreporem suas variaveis
(defmacro define-x []
  '(do
     (def x 2)
     (list x)))

(def x 4)
(define-x) ; -> (2)
(list x) ; -> (2)

;;s Para evitar isso, use gensym para receber um identificador unico
(gensym 'x) ; -> x1281 (ou outra coisa)

(defmacro define-x-safely []
  (let [sym (gensym 'x)]
    `(do
       (def ~sym 2)
       (list ~sym))))

(def x 4)
(define-x-safely) ; -> (2)
(list x) ; -> (4)

;; Você pode usar # dentro de ` para produzir uma gensym para cada simbolo automaticamente
(defmacro define-x-hygenically []
  `(do
     (def x# 2)
     (list x#)))

(def x 4)
(define-x-hygenically) ; -> (2)
(list x) ; -> (4)

;; É típico o uso de funções de auxilio com macros. Vamos criar um pouco
;; Vamos criar um pouco para nos ajudar a suportar uma sintaxe aritmética inline (estupida)
(declare inline-2-helper)
(defn clean-arg [arg]
  (if (seq? arg)
    (inline-2-helper arg)
    arg))

(defn apply-arg
  "Given args [x (+ y)], return (+ x y)"
  [val [op arg]]
  (list op val (clean-arg arg)))

(defn inline-2-helper
  [[arg1 & ops-and-args]]
  (let [ops (partition 2 ops-and-args)]
    (reduce apply-arg (clean-arg arg1) ops)))

;; Podemos testar isso imediatamente, sem criar uma macro
(inline-2-helper '(a + (b - 2) - (c * 5))) ; -> (- (+ a (- b 2)) (* c 5))

; Entretanto, temos que tornar isso uma macro caso quisermos que isso seja rodado em tempo de compilação
(defmacro inline-2 [form]
  (inline-2-helper form)))

(macroexpand '(inline-2 (1 + (3 / 2) - (1 / 2) + 1)))
; -> (+ (- (+ 1 (/ 3 2)) (/ 1 2)) 1)

(inline-2 (1 + (3 / 2) - (1 / 2) + 1))
; -> 3 (Na verdade, 3N, desde que o numero ficou convertido em uma fração racional com /

```

### Leitura adicional

Escrevendo Macros de [Clojure para o Brave e True](http://www.braveclojure.com/)
[http://www.braveclojure.com/writing-macros/](http://www.braveclojure.com/writing-macros/)

Documentos oficiais 
[http://clojure.org/macros](http://clojure.org/macros)

Quando utilizar macros? 
[http://dunsmor.com/lisp/onlisp/onlisp_12.html](http://dunsmor.com/lisp/onlisp/onlisp_12.html)
