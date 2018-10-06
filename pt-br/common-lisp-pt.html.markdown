---
language: "Common Lisp"
filename: commonlisp-pt.lisp
contributors:
  - ["Paul Nathan", "https://github.com/pnathan"]
translators:
  - ["Édipo Luis Féderle", "https://github.com/edipofederle"]
lang: pt-br
---

ANSI Common Lisp é uma linguagem de uso geral, multi-paradigma, designada
para uma variedade de aplicações na indústria. É frequentemente citada
como uma linguagem de programação programável.


O ponto inicial clássico é [Practical Common Lisp e livremente disponível](http://www.gigamonkeys.com/book/)

Outro livro recente e popular é o
[Land of Lisp](http://landoflisp.com/).


```lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 0. Sintaxe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; "Form" Geral


;; Lisp tem dois pedaços fundamentais de sintaxe: o ATOM e S-expression.
;; Tipicamente, S-expressions agrupadas são chamadas de `forms`.


10  ; um atom; é avaliado para ele mesmo

:THING ;Outro atom; avaliado para o símbolo :thing.

t ; outro atom, denotado true.

(+ 1 2 3 4) ; uma s-expression

'(4 :foo  t)  ;outra s-expression


;;; Comentários

;; Comentários de uma única linha começam com ponto e vírgula; usar dois para
;; comentários normais, três para comentários de seção, e quadro para comentários
;; em nível de arquivo.

#| Bloco de comentário
   pode abranger várias linhas e...
    #|
       eles podem ser aninhados
    |#
|#

;;; Ambiente

;; Existe uma variedade de implementações; a maioria segue o padrão.
;; CLISP é um bom ponto de partida.

;; Bibliotecas são gerenciadas através do Quicklisp.org's Quicklisp sistema.

;; Common Lisp é normalmente desenvolvido com um editor de texto e um REPL
;; (Read Evaluate Print Loop) rodando ao mesmo tempo. O REPL permite exploração
;; interativa do programa como ele é "ao vivo" no sistema.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Tipos Primitivos e Operadores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Símbolos

'foo ; => FOO Perceba que um símbolo é automáticamente convertido para maiúscula.

;; Intern manualmente cria um símbolo a partir de uma string.

(intern "AAAA") ; => AAAA

(intern "aaa") ; => |aaa|

;;; Números
9999999999999999999999 ; inteiro
#b111                  ; binário => 7
#o111                  ; octal => 73
#x111                  ; hexadecimal => 273
3.14159s0              ; single
3.14159d0              ; double
1/2                    ; ratios
#C(1 2)                ; números complexos


;; Funções são escritas como (f x y z ...)
;; onde f é uma função e x, y, z, ... são operadores
;; Se você quiser criar uma lista literal de dados, use ' para evitar 
;; que a lista seja avaliada - literalmente, "quote" os dados.
'(+ 1 2) ; => (+ 1 2)
;; Você também pode chamar uma função manualmente:
(funcall #'+ 1 2 3) ; => 6
;; O mesmo para operações aritiméticas
(+ 1 1)              ; => 2
(- 8 1)              ; => 7
(* 10 2)             ; => 20
(expt 2 3)           ; => 8
(mod 5 2)            ; => 1
(/ 35 5)             ; => 7
(/ 1 3)              ; => 1/3
(+ #C(1 2) #C(6 -4)) ; => #C(7 -2)

                     ;;; Booleans
t                    ; para true (qualquer valor não nil é true)
nil                  ; para false - e para lista vazia
(not nil)            ; => t
(and 0 t)            ; => t
(or 0 nil)           ; => 0

                     ;;; Caracteres
#\A                  ; => #\A
#\λ                  ; => #\GREEK_SMALL_LETTER_LAMDA
#\u03BB              ; => #\GREEK_SMALL_LETTER_LAMDA

;;; String são arrays de caracteres com tamanho fixo.
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; barra é um escape de caracter

;; String podem ser concatenadas também!
(concatenate 'string "Hello " "world!") ; => "Hello world!"

;; Uma String pode ser tratada como uma sequência de caracteres
(elt "Apple" 0) ; => #\A

;; format pode ser usado para formatar strings
(format nil "~a can be ~a" "strings" "formatted")

;; Impimir é bastante fácil; ~% indica nova linha
(format t "Common Lisp is groovy. Dude.~%")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Variáveis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Você pode criar uma global (escopo dinâmico) usando defparameter
;; um nome de variável pode conter qualquer caracter, exceto: ()",'`;#|\

;; Variáveis de escopo dinâmico devem ter asteriscos em seus nomes!

(defparameter *some-var* 5)
*some-var* ; => 5

;; Você pode usar caracteres unicode também.
(defparameter *AΛB* nil)


;; Acessando uma variável anteriormente não ligada é um
;; comportamento não definido (mas possível). Não faça isso.

;; Ligação local: `me` é vinculado com "dance with you" somente dentro
;; de (let ... ). Let permite retornar o valor do último `form` no form let.

(let ((me "dance with you"))
  me)
;; => "dance with you"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Estruturas e Coleções
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Estruturas
(defstruct dog name breed age)
(defparameter *rover*
    (make-dog :name "rover"
              :breed "collie"
              :age 5))
*rover* ; => #S(DOG :NAME "rover" :BREED "collie" :AGE 5)

(dog-p *rover*) ; => t  ;; ewww)
(dog-name *rover*) ; => "rover"

;; Dog-p, make-dog, e dog-name foram todas criadas por defstruct!

;;; Pares
;; `cons' constroi pares, `car' and `cdr' extrai o primeiro
;; e o segundo elemento
(cons 'SUBJECT 'VERB) ; => '(SUBJECT . VERB)
(car (cons 'SUBJECT 'VERB)) ; => SUBJECT
(cdr (cons 'SUBJECT 'VERB)) ; => VERB

;;; Listas

;; Listas são estruturas de dados do tipo listas encadeadas, criadas com `cons'
;; pares e terminam `nil' (ou '()) para marcar o final da lista
(cons 1 (cons 2 (cons 3 nil))) ; => '(1 2 3)
;; `list' é um construtor conveniente para listas
(list 1 2 3) ; => '(1 2 3)
;; e a quote (') também pode ser usado para um valor de lista literal
'(1 2 3) ; => '(1 2 3)

;; Ainda pode-se usar `cons' para adicionar um item no começo da lista.
(cons 4 '(1 2 3)) ; => '(4 1 2 3)

;; Use `append' para - surpreendentemente - juntar duas listas
(append '(1 2) '(3 4)) ; => '(1 2 3 4)

;; Ou use concatenate -

(concatenate 'list '(1 2) '(3 4))

;; Listas são um tipo muito central, então existe uma grande variedade de
;; funcionalidades para eles, alguns exemplos:
(mapcar #'1+ '(1 2 3))             ; => '(2 3 4)
(mapcar #'+ '(1 2 3) '(10 20 30))  ; => '(11 22 33)
(remove-if-not #'evenp '(1 2 3 4)) ; => '(2 4)
(every #'evenp '(1 2 3 4))         ; => nil
(some #'oddp '(1 2 3 4))           ; => T
(butlast '(subject verb object))   ; => (SUBJECT VERB)


;;; Vetores

;; Vector's literais são arrays de tamanho fixo.
#(1 2 3) ; => #(1 2 3)

;; Use concatenate para juntar dois vectors
(concatenate 'vector #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)

;;; Arrays

;; Ambos vetores e strings são um caso especial de arrays.

;; 2D arrays

(make-array (list 2 2))

;; (make-array '(2 2)) também funciona.

; => #2A((0 0) (0 0))

(make-array (list 2 2 2))

; => #3A(((0 0) (0 0)) ((0 0) (0 0)))

;; Cuidado - os valores de inicialição padrões são
;; definidos pela implementção. Aqui vai como defini-lós.

(make-array '(2) :initial-element 'unset)

; => #(UNSET UNSET)

;; E, para acessar o element em 1,1,1 -
(aref (make-array (list 2 2 2)) 1 1 1)

; => 0

;;; Vetores Ajustáveis

;; Vetores ajustáveis tem a mesma representação impressa que os vectores
;;  de tamanho fixo
(defparameter *adjvec* (make-array '(3) :initial-contents '(1 2 3)
      :adjustable t :fill-pointer t))
      
*adjvec* ; => #(1 2 3)

;; Adicionando novo elemento
(vector-push-extend 4 *adjvec*) ; => 3

*adjvec* ; => #(1 2 3 4)



;;; Ingenuamente, conjuntos são apenas listas:

(set-difference '(1 2 3 4) '(4 5 6 7)) ; => (3 2 1)
(intersection '(1 2 3 4) '(4 5 6 7)) ; => 4
(union '(1 2 3 4) '(4 5 6 7))        ; => (3 2 1 4 5 6 7)
(adjoin 4 '(1 2 3 4))     ; => (1 2 3 4)

;; Mas você irá querer usar uma estrutura de dados melhor que uma lista encadeada.
;; para performance.

;;; Dicionários são implementados como hash tables

;; Cria um hash table
(defparameter *m* (make-hash-table))

;; seta um valor
(setf (gethash 'a *m*) 1)

;; Recupera um valor
(gethash 'a *m*) ; => 1, t

;; Detalhe - Common Lisp  tem multiplos valores de retorno possíveis. gethash
;; retorna t no segundo valor se alguma coisa foi encontrada, e nil se não.

;; Recuperando um valor não presente retorna nil
 (gethash 'd *m*) ;=> nil, nil

;; Você pode fornecer um valor padrão para uma valores não encontrados
(gethash 'd *m* :not-found) ; => :NOT-FOUND

;; Vamos tratas múltiplos valores de rotorno aqui.

(multiple-value-bind
      (a b)
    (gethash 'd *m*)
  (list a b))
; => (NIL NIL)

(multiple-value-bind
      (a b)
    (gethash 'a *m*)
  (list a b))
; => (1 T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Funções
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use `lambda' para criar funções anônimas
;; Uma função sempre retorna um valor da última expressão avaliada.
;; A representação exata impressão de uma função varia de acordo ...

(lambda () "Hello World") ; => #<FUNCTION (LAMBDA ()) {1004E7818B}>

;; Use funcall para chamar uma função lambda.
(funcall (lambda () "Hello World")) ; => "Hello World"

;; Ou Apply
(apply (lambda () "Hello World") nil) ; => "Hello World"

;; "De-anonymize" a função
(defun hello-world ()
   "Hello World")
(hello-world) ; => "Hello World"

;; O () acima é a lista de argumentos da função.
(defun hello (name)
   (format nil "Hello, ~a " name))

(hello "Steve") ; => "Hello, Steve"

;; Funções podem ter argumentos opcionais; eles são nil por padrão

(defun hello (name &optional from)
    (if from
        (format t "Hello, ~a, from ~a" name from)
        (format t "Hello, ~a" name)))

 (hello "Jim" "Alpacas") ;; => Hello, Jim, from Alpacas

;; E os padrões podem ser configurados...
(defun hello (name &optional (from "The world"))
   (format t "Hello, ~a, from ~a" name from))

(hello "Steve")
; => Hello, Steve, from The world

(hello "Steve" "the alpacas")
; => Hello, Steve, from the alpacas


;; E é claro, palavras-chaves são permitidas também... frequentemente mais
;; flexivel que &optional.

(defun generalized-greeter (name &key (from "the world") (honorific "Mx"))
    (format t "Hello, ~a ~a, from ~a" honorific name from))

(generalized-greeter "Jim")   ; => Hello, Mx Jim, from the world

(generalized-greeter "Jim" :from "the alpacas you met last summer" :honorific "Mr")
; => Hello, Mr Jim, from the alpacas you met last summer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Igualdade
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Common Lisp tem um sistema sofisticado de igualdade. Alguns são cobertos aqui.

;; Para número use `='
(= 3 3.0) ; => t
(= 2 1) ; => nil

;; para identidade de objeto (aproximadamente) use `eql`
(eql 3 3) ; => t
(eql 3 3.0) ; => nil
(eql (list 3) (list 3)) ; => nil

;; para listas, strings, e para pedaços de vetores use `equal'
(equal (list 'a 'b) (list 'a 'b)) ; => t
(equal (list 'a 'b) (list 'b 'a)) ; => nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Fluxo de Controle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Condicionais

(if t                ; testa a expressão
    "this is true"   ; então expressão
    "this is false") ; senão expressão
; => "this is true"

;; Em condicionais, todos valores não nulos são tratados como true
(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(GROUCHO ZEPPO)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'YEP

;; `cond' encadeia uma série de testes para selecionar um resultado
(cond ((> 2 2) (error "wrong!"))
      ((< 2 2) (error "wrong again!"))
      (t 'ok)) ; => 'OK

;; Typecase é um condicional que escolhe uma de seus cláusulas com base do tipo
;; do seu valor

(typecase 1
  (string :string)
  (integer :int))

; => :int

;;; Interação

;; Claro que recursão é suportada:

(defun walker (n)
  (if (zerop n)
      :walked
      (walker (1- n))))

(walker 5) ; => :walked

;; Na maioria das vezes, nós usamos DOTLISO ou LOOP

(dolist (i '(1 2 3 4))
  (format t "~a" i))

; => 1234

(loop for i from 0 below 10
      collect i)

; => (0 1 2 3 4 5 6 7 8 9)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Mutação
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use `setf' para atribuir um novo valor para uma variável existente. Isso foi
;; demonstrado anteriormente no exemplo da hash table.

(let ((variable 10))
    (setf variable 2))
 ; => 2


;; Um bom estilo Lisp é para minimizar funções destrutivas e para evitar 
;; mutação quando razoável.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Classes e Objetos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sem clases Animal, vamos usar os veículos de transporte de tração
;; humana mecânicos.

(defclass human-powered-conveyance ()
  ((velocity
    :accessor velocity
    :initarg :velocity)
   (average-efficiency
    :accessor average-efficiency
   :initarg :average-efficiency))
  (:documentation "A human powered conveyance"))

;; defcalss, seguido do nome, seguido por uma list de superclass,
;; seguido por um uma 'slot list', seguido por qualidades opcionais como
;; :documentation

;; Quando nenhuma lista de superclasse é setada, uma lista padrão para
;; para o objeto padrão é usada. Isso *pode* ser mudado, mas não até você
;; saber o que está fazendo. Olhe em Art of the Metaobject Protocol
;; para maiores informações.

(defclass bicycle (human-powered-conveyance)
  ((wheel-size
    :accessor wheel-size
    :initarg :wheel-size
    :documentation "Diameter of the wheel.")
   (height
    :accessor height
    :initarg :height)))

(defclass recumbent (bicycle)
  ((chain-type
    :accessor chain-type
    :initarg  :chain-type)))

(defclass unicycle (human-powered-conveyance) nil)

(defclass canoe (human-powered-conveyance)
  ((number-of-rowers
    :accessor number-of-rowers
    :initarg :number-of-rowers)))


;; Chamando DESCRIBE na classe human-powered-conveyance no REPL dá:

(describe 'human-powered-conveyance)

; COMMON-LISP-USER::HUMAN-POWERED-CONVEYANCE
;  [symbol]
;
; HUMAN-POWERED-CONVEYANCE names the standard-class #<STANDARD-CLASS
;                                                    HUMAN-POWERED-CONVEYANCE>:
;  Documentation:
;    A human powered conveyance
;  Direct superclasses: STANDARD-OBJECT
;  Direct subclasses: UNICYCLE, BICYCLE, CANOE
;  Not yet finalized.
;  Direct slots:
;    VELOCITY
;      Readers: VELOCITY
;      Writers: (SETF VELOCITY)
;    AVERAGE-EFFICIENCY
;      Readers: AVERAGE-EFFICIENCY
;      Writers: (SETF AVERAGE-EFFICIENCY)

;; Note o comportamento reflexivo disponível para você! Common Lisp é
;; projetada para ser um sistema interativo.

;; Para definir um métpdo, vamos encontrar o que nossa cirunferência da
;; roda da bicicleta usando a equação: C = d * pi

(defmethod circumference ((object bicycle))
  (* pi (wheel-size object)))

;; pi já é definido para a gente em Lisp!

;; Vamos supor que nós descobrimos que o valor da eficiência do número
;; de remadores em uma canoa é aproximadamente logarítmica. Isso provavelmente
;; deve ser definido no construtor / inicializador.

;; Veja como initializar sua instância após Common Lisp ter construído isso:

(defmethod initialize-instance :after ((object canoe) &rest args)
  (setf (average-efficiency object)  (log (1+ (number-of-rowers object)))))

;; Em seguida, para a construção de uma ocorrência e verificar a eficiência média ...

(average-efficiency (make-instance 'canoe :number-of-rowers 15))
; => 2.7725887




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Macros permitem que você estenda a sintaxe da lingaugem

;; Common Lisp não vem com um loop WHILE - vamos adicionar um.
;; Se obedecermos nossos instintos 'assembler', acabamos com:

(defmacro while (condition &body body)
    "Enquanto `condition` é verdadeiro, `body` é executado.

`condition` é testado antes de cada execução do `body`"
    (let ((block-name (gensym)))
        `(tagbody
           (unless ,condition
               (go ,block-name))
           (progn
           ,@body)
           ,block-name)))

;; Vamos dar uma olhada em uma versão alto nível disto:


(defmacro while (condition &body body)
    "Enquanto `condition` for verdadeira, `body` é executado.

`condition` é testado antes de cada execução do `body`"
  `(loop while ,condition
         do
         (progn
            ,@body)))

;; Entretanto, com um compilador moderno, isso não é preciso; o LOOP
;; 'form' compila igual e é bem mais fácil de ler.

;; Noteq ue ``` é usado , bem como `,` e `@`. ``` é um operador 'quote-type'
;; conhecido como 'quasiquote'; isso permite o uso de `,` . `,` permite "unquoting"
;; e variáveis. @ interpolará listas.

;; Gensym cria um símbolo único garantido que não existe em outras posições
;; o sistema. Isto é porque macros são expandidas em tempo de compilação e
;; variáveis declaradas na macro podem colidir com as variáveis usadas na
;; código regular.

;; Veja Practical Common Lisp para maiores informações sobre macros.
```


## Leitura Adicional

[Continua em frente com  Practical Common Lisp book.](http://www.gigamonkeys.com/book/)


## Créditos

Muitos  agradecimentos ao pessoal de Schema por fornecer um grande ponto de partida
o que facilitou muito a migração para Common Lisp.

- [Paul Khuong](https://github.com/pkhuong) pelas grandes revisões.
