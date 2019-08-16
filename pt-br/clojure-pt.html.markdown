---
language: clojure
filename: learnclojure-pt.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Mariane Siqueira Machado", "https://twitter.com/mariane_sm"]
lang: pt-br
---

Clojure é uma linguagem da família do Lisp desenvolvida para a JVM (máquina virtual Java). Possui uma ênfase muito mais forte em [programação funcional] (https://pt.wikipedia.org/wiki/Programa%C3%A7%C3%A3o_funcional) pura do que Common Lisp, mas inclui diversas utilidades [STM](https://en.wikipedia.org/wiki/Software_transactional_memory) para lidar com estado a medida que isso se torna necessário.

Essa combinação permite gerenciar processamento concorrente de maneira muito simples, e frequentemente de maneira automática.

(Sua versão de clojure precisa ser pelo menos 1.2)


```clojure
; Comentários começam por ponto e vírgula

; Clojure é escrito em "forms", os quais são simplesmente 
; listas de coisas dentro de parênteses, separados por espaços em branco.

; O "reader" (leitor) de Clojure presume que o primeiro elemento de 
; uma par de parênteses é uma função ou macro, e que os resto são argumentos.

: A primeira chamada de um arquivo deve ser ns, para configurar o namespace (espaço de nomes)
(ns learnclojure)

; Alguns exemplos básicos:

; str cria uma string concatenando seus argumentos
(str "Hello" " " "World") ; => "Hello World"

; Cálculos são feitos de forma direta e intuitiva
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2

; Você pode comparar igualdade utilizando =
(= 1 1) ; => true
(= 2 1) ; => false

; Negação para operações lógicas
(not true) ; => false

; Aninhar "forms" funciona como esperado
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

; Tipos
;;;;;;;;;;;;;

; Clojure usa os tipos de objetos de Java para booleanos, strings e números.
; Use `class` para inspecioná-los
(class 1) ; Literais Integer são java.lang.Long por padrão
(class 1.); Literais Float são java.lang.Double
(class ""); Strings são sempre com aspas duplas, e são java.lang.String
(class false) ; Booleanos são java.lang.Boolean
(class nil); O valor "null" é chamado nil

; Se você quiser criar um lista de literais, use aspa simples para 
; ela não ser avaliada
'(+ 1 2) ; => (+ 1 2)
; (que é uma abreviação de (quote (+ 1 2)))

; É possível avaliar uma lista com aspa simples
(eval '(+ 1 2)) ; => 3

; Coleções e sequências
;;;;;;;;;;;;;;;;;;;

; Listas são estruturas encadeadas, enquanto vetores são implementados como arrays.
; Listas e Vetores são classes Java também!
(class [1 2 3]); => clojure.lang.PersistentVector
(class '(1 2 3)); => clojure.lang.PersistentList

; Uma lista é escrita como (1 2 3), mas temos que colocar a aspa 
; simples para impedir o leitor (reader) de pensar que é uma função.
; Também, (list 1 2 3) é o mesmo que '(1 2 3)

; "Coleções" são apenas grupos de dados
; Listas e vetores são ambos coleções:
(coll? '(1 2 3)) ; => true
(coll? [1 2 3]) ; => true

; "Sequências" (seqs) são descrições abstratas de listas de dados.
; Apenas listas são seqs.
(seq? '(1 2 3)) ; => true
(seq? [1 2 3]) ; => false

; Um seq precisa apenas prover uma entrada quando é acessada.
; Portanto, já que seqs podem ser avaliadas sob demanda (lazy) -- elas podem definir séries infinitas:
(range 4) ; => (0 1 2 3)
(range) ; => (0 1 2 3 4 ...) (uma série infinita)
(take 4 (range)) ;  (0 1 2 3)

; Use cons para adicionar um item no início de uma lista ou vetor
(cons 4 [1 2 3]) ; => (4 1 2 3)
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; Conj adiciona um item em uma coleção sempre do jeito mais eficiente.
; Para listas, elas inserem no início. Para vetores, é inserido no final.
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; Use concat para concatenar listas e vetores
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; Use filter, map para interagir com coleções
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3]) ; => (2)

; Use reduce para reduzi-los
(reduce + [1 2 3 4])
; = (+ (+ (+ 1 2) 3) 4)
; => 10

; Reduce pode receber um argumento para o valor inicial
(reduce conj [] '(3 2 1))
; = (conj (conj (conj [] 3) 2) 1)
; => [3 2 1]

; Funções
;;;;;;;;;;;;;;;;;;;;;

; Use fn para criar novas funções. Uma função sempre retorna
; sua última expressão.
(fn [] "Hello World") ; => fn

; (É necessário colocar parênteses para chamá-los)
((fn [] "Hello World")) ; => "Hello World"

; Você pode atribuir valores a variáveis utilizando def
(def x 1)
x ; => 1

; Atribua uma função para uma var
(def hello-world (fn [] "Hello World"))
(hello-world) ; => "Hello World"

; Você pode abreviar esse processo usando defn
(defn hello-world [] "Hello World")

; O [] é uma lista de argumentos para um função.
(defn hello [name]
  (str "Hello " name))
(hello "Steve") ; => "Hello Steve"

; Você pode ainda usar essa abreviação para criar funcões:
(def hello2 #(str "Hello " %1))
(hello2 "Fanny") ; => "Hello Fanny"

; Vocé pode ter funções multi-variadic, isto é, com um número variável de argumentos
(defn hello3
  ([] "Hello World")
  ([name] (str "Hello " name)))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"

; Funções podem agrupar argumentos extras em uma seq
(defn count-args [& args]
  (str "You passed " (count args) " args: " args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"

; Você pode misturar argumentos regulares e argumentos em seq
(defn hello-count [name & args]
  (str "Hello " name ", you passed " (count args) " extra args"))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"


; Mapas
;;;;;;;;;;

; Hash maps e array maps compartilham uma mesma interface. Hash maps são mais
; rápidos para pesquisa mas não mantém a ordem da chave.
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; Arraymaps pode automaticamente se tornar hashmaps através da maioria das
; operações se eles ficarem grandes o suficiente, portanto não há necessida de 
; se preocupar com isso.

;Mapas podem usar qualquer valor que se pode derivar um hash como chave


; Mapas podem usar qualquer valor em que se pode derivar um hash como chave, 
; mas normalmente palavras-chave (keywords) são melhores.
; Keywords são como strings mas com algumas vantagens.
(class :a) ; => clojure.lang.Keyword

(def stringmap {"a" 1, "b" 2, "c" 3})
stringmap  ; => {"a" 1, "b" 2, "c" 3}

(def keymap {:a 1, :b 2, :c 3})
keymap ; => {:a 1, :c 3, :b 2}

; A propósito, vírgulas são sempre tratadas como espaçoes em branco e não fazem nada.

; Recupere o valor de um mapa chamando ele como uma função
(stringmap "a") ; => 1
(keymap :a) ; => 1

; Uma palavra-chave pode ser usada pra recuperar os valores de um mapa
(:b keymap) ; => 2

; Não tente isso com strings
;("a" stringmap)
; => Exception: java.lang.String cannot be cast to clojure.lang.IFn

; Buscar uma chave não presente retorna nil
(stringmap "d") ; => nil

; Use assoc para adicionar novas chaves para hash-maps
(def newkeymap (assoc keymap :d 4))
newkeymap ; => {:a 1, :b 2, :c 3, :d 4}

; Mas lembre-se, tipos em Clojure são sempre imutáveis!
keymap ; => {:a 1, :b 2, :c 3}

; Use dissoc para remover chaves
(dissoc keymap :a :b) ; => {:c 3}

; Conjuntos
;;;;;;

(class #{1 2 3}) ; => clojure.lang.PersistentHashSet
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}

; Adicione um membro com conj
(conj #{1 2 3} 4) ; => #{1 2 3 4}

; Remova um membro com disj
(disj #{1 2 3} 1) ; => #{2 3}

; Test por existência usando set como função:
(#{1 2 3} 1) ; => 1
(#{1 2 3} 4) ; => nil

; Existem muitas outras funções no namespace clojure.sets

; Forms úteis
;;;;;;;;;;;;;;;;;

; Construções lógicas em Clojure são como macros, e 
; se parecem com as demais
(if false "a" "b") ; => "b"
(if false "a") ; => nil

; Use let para criar um novo escopo associando sîmbolos a valores (bindings)
(let [a 1 b 2]
  (> a b)) ; => false

; Agrupe comandos juntos com "do"
(do
  (print "Hello")
  "World") ; => "World" (prints "Hello")

; Funções tem um do implícito
(defn print-and-say-hello [name]
  (print "Saying hello to " name)
  (str "Hello " name))
(print-and-say-hello "Jeff") ;=> "Hello Jeff" (prints "Saying hello to Jeff")

; Assim como let
(let [name "Urkel"]
  (print "Saying hello to " name)
  (str "Hello " name)) ; => "Hello Urkel" (prints "Saying hello to Urkel")

; Módulos
;;;;;;;;;;;;;;;

; Use "use" para poder usar todas as funções de um modulo
(use 'clojure.set)

; Agora nós podemos usar operações com conjuntos
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; Você pode escolher um subconjunto de funções para importar
(use '[clojure.set :only [intersection]])

; Use require para importar um módulo
(require 'clojure.string)

; Use / para chamar funções de um módulo
; Aqui, o módulo é clojure.string e a função é blank?
(clojure.string/blank? "") ; => true

; Você pode dar para um módulo um nome mais curto no import
(require '[clojure.string :as str])
(str/replace "This is a test." #"[a-o]" str/upper-case) ; => "THIs Is A tEst."
; (#"" denota uma expressão regular literal)

; Você pode usar require (e até "use", mas escolha require) de um namespace utilizando :require.
; Não é necessário usar aspa simples nos seus módulos se você usar desse jeito.
(ns test
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

; Java
;;;;;;;;;;;;;;;;;

; Java tem uma biblioteca padrão enorme e muito útil, 
; portanto é importante aprender como utiliza-la.

; Use import para carregar um modulo java
(import java.util.Date)

; Você pode importar usando ns também.
(ns test
  (:import java.util.Date
           java.util.Calendar))

; Use o nome da clase com um "." no final para criar uma nova instância
(Date.) ; <a date object>

; Use . para chamar métodos. Ou, use o atalho ".method"
(. (Date.) getTime) ; <a timestamp>
(.getTime (Date.)) ; exatamente a mesma coisa.

; Use / para chamar métodos estáticos
(System/currentTimeMillis) ; <a timestamp> (o módulo System está sempre presente)

; Use doto para pode lidar com classe (mutáveis) de forma mais tolerável
(import java.util.Calendar)
(doto (Calendar/getInstance)
  (.set 2000 1 1 0 0 0)
  .getTime) ; => A Date. set to 2000-01-01 00:00:00

; STM
;;;;;;;;;;;;;;;;;

; Software Transactional Memory é o mecanismo que Clojure usa para gerenciar
; estado persistente. Tem algumas construções em Clojure que o utilizam.

; O atom é o mais simples. Passe pra ele um valor inicial
(def my-atom (atom {}))

; Atualize o atom com um swap!.
; swap! pega uma função e chama ela com o valor atual do atom
; como primeiro argumento, e qualquer argumento restante como o segundo
(swap! my-atom assoc :a 1) ; Coloca o valor do átomo my-atom como o resultado de  (assoc {} :a 1)
(swap! my-atom assoc :b 2) ; Coloca o valor do átomo my-atom como o resultado de (assoc {:a 1} :b 2)

; Use '@' para desreferenciar um atom e acessar seu valor
my-atom  ;=> Atom<#...> (Retorna o objeto do Atom)
@my-atom ; => {:a 1 :b 2}

; Abaixo um contador simples usando um atom
(def counter (atom 0))
(defn inc-counter []
  (swap! counter inc))

(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)

@counter ; => 5

; Outras construção STM são refs e agents.
; Refs: http://clojure.org/refs
; Agents: http://clojure.org/agents
```

### Leitura adicional

Esse tutorial está longe de ser exaustivo, mas deve ser suficiente para que você possa começar.

Clojure.org tem vários artigos:
[http://clojure.org/](http://clojure.org/)

Clojuredocs.org tem documentação com exemplos para quase todas as funções principais (pertecentes ao core):
[http://clojuredocs.org/quickref/Clojure%20Core](http://clojuredocs.org/quickref/Clojure%20Core)

4Clojure é um grande jeito de aperfeiçoar suas habilidades em Clojure/Programação Funcional:
[http://www.4clojure.com/](http://www.4clojure.com/)

Clojure-doc.org tem um bom número de artigos para iniciantes:
[http://clojure-doc.org/](http://clojure-doc.org/)
