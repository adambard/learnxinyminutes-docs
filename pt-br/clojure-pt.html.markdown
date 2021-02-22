---
language: clojure
filename: learnclojure-pt.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Mariane Siqueira Machado", "https://twitter.com/mariane_sm"]
    - ["Ygor Sad", "https://github.com/ysads"]
lang: pt-br
---

Clojure é uma linguagem da família do Lisp desenvolvida para a JVM (máquina virtual Java). Possui uma ênfase muito mais forte em [programação funcional] (https://pt.wikipedia.org/wiki/Programa%C3%A7%C3%A3o_funcional) pura do que Common Lisp, mas inclui diversos recursos [STM](https://en.wikipedia.org/wiki/Software_transactional_memory) para lidar com estado e mutabilidade, caso isso seja necessário.

Essa combinação permite gerenciar processamento concorrente de maneira muito simples - frequentemente, de modo automático.

(Sua versão de clojure precisa ser pelo menos 1.2)


```clojure
; Comentários começam por ponto e vírgula

; Código Clojure é escrito em formas - 'forms', em inglês. Tais estruturas são
; simplesmente listas de valores encapsuladas dentro de parênteses, separados por
; espaços em branco.

; Ao interpretar um código em Clojure, o interpretador ou leitor - do inglês 'reader' - assume
; que o primeiro valor dentro de uma forma é uma função ou macro, de modo que os demais valores
; são seus argumentos. Isso se deve ao fato de que Clojure, por ser uma derivação de Lisp,
; usa notação prefixa (ou polonesa).

; Num arquivo, a primeira chamada deve ser sempre para a função ns,
; que é responsável por definir em qual namespace o código em questão
; deve ser alocado
(ns learnclojure)

; Alguns exemplos básicos:

; Aqui, str é uma função e "Olá" " " e "Mundo" são seus argumentos. O que ela faz é criar
; uma string concatenando seus argumentos.
(str "Olá" " " "Mundo") ; => "Olá Mundo"

; Note que espaços em branco separam os argumentos de uma função. Opcionalmente vírgulas
; podem ser usadas, se você quiser.
(str, "Olá", " ", "Mundo") ; => "Olá Mundo"

; As operações matemáticas básicas usam os operadores de sempre
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2

; Esses operadores aceitam um número arbitrário de argumentos
(+ 2 2 2) ; = 2 + 2 + 2 => 6
(- 5 1 1) ; = 5 - 1 - 1 => 3
(* 3 3 3 3) ; = 3 * 3 * 3 * 3 => 81

; Para verificar se dois valores são iguais, o operador = pode ser usado
(= 1 1) ; => true
(= 2 1) ; => false

; Para saber se dois valores são diferentes
(not= 1 2) ; => true
(not (= 1 2)) ; => true

; Conforme vimos acima, é possível aninhar duas formas
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2
(* (- 3 2) (+ 1 2)) ; = (3 - 2) * (1 + 2) => 3

; Se a leitura ficar comprometida, as fórmulas também podem ser escritas em múltiplas linhas
(* (- 3 2)
   (+ 1 2)) ; => 3
(*
 (- 3 2)
 (+ 1 2)) ; => 3


; Tipos
;;;;;;;;;;;;;

; Por ter interoperabilidade com Java, Clojure usa os tipos de objetos de Java para booleanos,
; strings e números. Para descobrir qual o tipo de um valor, você pode usar a função `class`:
(class 1234) ; Literais Integer são java.lang.Long por padrão
(class 1.50) ; Literais Float são java.lang.Double
(class "oi") ; Strings sempre usam aspas duplas e são java.lang.String
(class false) ; Booleanos são java.lang.Boolean

; Tenha cuidado, ao dividir valores inteiros:
(= (/ 1 2)
   (/ 1.0 2.0)) ; => false

(class (/ 1 2)) ; => clojure.lang.Ratio
(class (/ 1.0 2.0)) ; => java.lang.Double

; Aqui temos uma diferença em relação a Java, pois valores nulos são representados por `nil`
(class nil) ; nil


; Coleções e sequências
;;;;;;;;;;;;;;;;;;;

; Os dois tipos básicos de coleção são listas - "list" em inglês - e vetores - "vectors"
; no original. A principal diferença entre eles se
; dá pela implementação:
; - Vetores são implementados como arrays
; - Listas são listas ligadas
(class [1 2 3]) ; => clojure.lang.PersistentVector
(class '(1 2 3)) ; => clojure.lang.PersistentList

; Outra forma de declarar listas é usando a função list
(list 1 2 3) ; => '(1 2 3)

; Clojure classifica conjuntos de dados de duas maneiras

; "Coleções" são grupos simples de dados
; Tanto listas quanto vetores são coleções:
(coll? '(1 2 3)) ; => true
(coll? [1 2 3]) ; => true

; "Sequências" (seqs) são descrições abstratas de listas de dados.
; Sequências - ou seqs - são conjuntos de dados com avaliação "lazy"
; Apenas listas são seqs:
(seq? '(1 2 3)) ; => true
(seq? [1 2 3]) ; => false

; Ter avaliação lazy significa que uma seq somente precisa prover uma informação quando
; ela for requisitada. Isso permite às seqs representar listas infinitas.
(range) ; => (0 1 2 3 4 ...)
(cycle [1 2]) ; => (1 2 1 2 1 2 ...)
(take 4 (range)) ; => (0 1 2 3)

; A função cons é usada para adicionar um item ao início de uma lista ou vetor:
(cons 4 [1 2 3]) ; => (4 1 2 3)
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; Já conj adiciona um item em uma coleção sempre do jeito mais eficiente.
; Em listas, isso significa inserir no início. Já em vetores, ao final.
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; Concatenação de coleções pode ser feita usando concat. Note que ela sempre gera uma
; seq como resultado e está sujeita a problemas de perfomance em coleções grandes, por
; conta da natureza lazy das seqs.
(concat '(1 2) [3 4]) ; => (1 2 3 4)
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; Outra forma de concatenar coleções é usando into. Ela não está sujeita a problemas
; com a avaliação lazy, mas o resultado final da ordem e do tipo dos argumentos passados
(into [1 2] '(3 4)) ; => [1 2 3 4]
(into '(1 2) [3 4]) ; => (4 3 1 2)

; Note que em into a ordem dos parâmetros influencia a coleção final.
(into [1 2] '(3 4)) ; => (1 2 3 4)
(into '(1 2) [3 4]) ; => (4 3 1 2)

; As funções filter e map podem ser usadas para interagir com as coleções. Repare que
; elas sempre retornam seqs, independentemente do tipo do seu argumento.
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3 4]) ; => (2 4)

; Use reduce reduzir coleções a um único valor. Também é possível passar um argumento
; para o valor inicial das operações
(reduce + [1 2 3]) ; = (+ (+ (+ 1 2) 3) 4) => 10
(reduce + 10 [1 2 3 4]) ; = (+ (+ (+ (+ 10 1) 2) 3) 4) => 20
(reduce conj [] '(3 2 1)) ; = (conj (conj (conj [] 3) 2) 1) => [3 2 1]

; Reparou na semelhança entre listas e as chamadas de código Clojure? Isso se deve ao
; fato de que todo código clojure é escrito usando listas. É por isso que elas sempre
; são declaradas com o caracter ' na frente. Dessa forma o interpretador não tenta
; avaliá-las.
'(+ 2 3) ; cria uma lista com os elementos +, 2 e 3
(+ 2 3) ; o interpretador chama a função + passando como argumentos 2 e 3

; Note que ' é apenas uma abreviação para a função quote.
(quote (1 2 3)) ; => '(1 2 3)

; É possível passar uma lista para que o interpretador a avalie. Note que isso está
; sujeito ao primeiro elemento da lista ser um literal com um nome de uma função válida.
(eval '(+ 2 3)) ; => 5
(eval '(1 2 3)) ; dá erro pois o interpretador tenta chamar a função 1, que não existe


; Funções
;;;;;;;;;;;;;;;;;;;;;

; Use fn para criar novas funções. Uma função sempre retorna sua última expressão.
(fn [] "Olá Mundo") ; => fn

; Para executar suas funções, é preciso chamá-las, envolvendo-as em parênteses.
((fn [] "Olá Mundo")) ; => "Olá Mundo"

; Como isso não é muito prático, você pode nomear funções atribuindo elas a literais.
; Isso torna muito mais fácil chamá-las:
(def ola-mundo (fn [] "Olá Mundo")) ; => fn
(ola-mundo) ; => "Olá Mundo"

; Você pode abreviar esse processo usando defn:
(defn ola-mundo [] "Olá Mundo")

; Uma função pode receber uma lista de argumentos:
(defn ola
  [nome]
  (str "Olá " nome))
(ola "Jonas") ; => "Olá Jonas"

; É possível criar funções que recebam multivariadas, isto é, que aceitam números
; diferentes de argumentos:
(defn soma
  ([] 0)
  ([a] a)
  ([a b] (+ a b)))

(soma) ; => 0
(soma 1) ; => 1
(soma 1 2) ; => 3

; Funções podem agrupar argumentos extras em uma seq:
(defn conta-args
  [& args]
  (str "Você passou " (count args) " argumentos: " args))
(conta-args 1 2 3 4) ; => "Você passou 4 argumentos: (1 2 3 4)"

; Você pode misturar argumentos regulares e argumentos em seq:
(defn ola-e-conta
  [nome & args]
  (str "Olá " nome ", você passou " (count args) " argumentos extras"))
(ola-e-conta "Maria" 1 2 3 4) ; => "Olá Maria, você passou 4 argumentos extras"


; Nos exemplos acima usamos def para associar nomes a funções, mas poderíamos usá-lo
; para associar nomes a quaisquer valores:
(def xis :x)
xis ; => :x

; Inclusive, tais literais podem possuir alguns caracteres não usuais em outras linguagens:
(def *num-resposta* 42)
(def conexao-ativa? true)
(def grito-de-medo! "AAAAAAA")
(def ->vector-vazio [])

; É possível, inclusive, criar apelidos a nomes que já existem:
(def somar! soma)
(somar! 41 1) ; => 42 

; Uma forma rápida de criar funções é por meio de funções anônimas. Elas são ótimas
; para manipulação de coleções e seqs, já que podem ser passadas para map, filter
; e reduce. Nessas funções, % é substituído por cada um dos items na seq ou na coleção:
(filter #(not= % nil) ["Joaquim" nil "Maria" nil "Antônio"]) ; => ("Joaquim" "Maria" "Antônio")
(map #(* % (+ % 2)) [1 2]) ; => (3 8)


; Mapas
;;;;;;;;;;

; Existem dois tipos de mapas: hash maps e array maps. Ambos compartilham uma mesma
; interface e funções. Hash maps são mais rápidos para retornar dados, mas não mantém
; as chaves ordenadas.
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; Clojure converte automaticamente array maps em hash maps, por meio da maioria das
; funções de manipulação de mapas, caso eles fiquem grandes o suficiente. Não é
; preciso se preocupar com isso.

; Chaves podem ser qualquer valor do qual possa ser obtido um hash, mas normalmente
; usam-se keywords como chave, por possuírem algumas vantagens.
(class :a) ; => clojure.lang.Keyword

; Keywords são como strings, porém, duas keywords de mesmo valor são sempre armazenadas
; na mesma posição de memória, o que as torna mais eficientes.
(identical? :a :a) ; => true
(identical? (String. "a") (String. "a")) ; => false

(def mapa-strings {"a" 1 "b" 2 "c" 3})
mapa-strings ; => {"a" 1, "b" 2, "c" 3}

(def mapa-keywords {:a 1 :b 2 :c 3})
mapa-keywords ; => {:a 1, :c 3, :b 2}

; Você pode usar um mapa como função para recuperar um valor dele:
(mapa-strings "a") ; => 1
(mapa-keywords :a) ; => 1

; Se a chave buscada for uma keyword, ela também pode ser usada como função para recuperar 
; valores. Note que isso não funciona com strings.
(:b mapa-keywords) ; => 2
("b" mapa-strings) ; => java.lang.String cannot be cast to clojure.lang.IFn

; Se você buscar uma chave que não existe, Clojure retorna nil:
(mapa-strings "d") ; => nil

; Use assoc para adicionar novas chaves em um mapa.
(def mapa-keywords-estendido (assoc mapa-keywords :d 4))
mapa-keywords-estendido ; => {:a 1, :b 2, :c 3, :d 4}

; Mas lembre-se que tipos em Clojure são sempre imutáveis! Isso significa que o mapa
; inicial continua com as mesmas informações e um novo mapa, com mais dados, é criado
; a partir dele
mapa-keywords ; => {:a 1, :b 2, :c 3}

; assoc também pode ser usado para atualizar chaves:
(def outro-mapa-keywords (assoc mapa-keywords :a 0))
outro-mapa-keywords ; => {:a 0, :b 2, :c 3}

; Use dissoc para remover chaves
(dissoc mapa-keywords :a :b) ; => {:c 3}

; Mapas também são coleções - mas não seqs!
(coll? mapa-keywords) ; => true
(seq? mapa-keywords) ; => false

; É possível usar filter, map e qualquer outra função de coleções em mapas.
; Porém a cada iteração um vetor no formato [chave valor] vai ser passado como
; argumento. Por isso é conveniente usar funções anônimas.
(filter #(odd? (second %)) mapa-keywords) ; => ([:a 1] [:c 3])
(map #(inc (second %)) mapa-keywords) ; => (2 3 4)

; Conjuntos
;;;;;;

; Conjuntos são um tipo especial de coleções que não permitem elementos repetidos.
; Eles podem ser criados com #{} ou com a função set.
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}
(class #{1 2 3}) ; => clojure.lang.PersistentHashSet

; Note que nem sempre um set vai armazenar seus elementos na ordem esperada.
(def meu-conjunto #{1 2 3})
meu-conjunto ; => #{1 3 2}

; Adição funciona normalmente com conj.
(conj meu-conjunto 4) ; => #{1 4 3 2}

; Remoção, no entanto, precisa ser feita com disj:
(disj meu-conjunto 1) ; => #{3 2}

; Para saber se um elemento está em um conjunto, use-o como função. Nesse aspecto
; conjuntos funcionam de maneira semelhante a mapas.
(meu-conjunto 1) ; => 1
(meu-conjunto 4) ; => nil


; Condicionais e blocos
;;;;;;;;;;;;;;;;;

; Você pode usar um bloco let para criar um escopo local, no qual estarão disponíveis
; os nomes que você definir:
(let [a 1 b 2]
  (+ a b)) ; => 3

(let [cores {:yellow "Amarelo" :blue "Azul"}
      nova-cor :red
      nome-cor "Vermelho"]
  (assoc cores nova-cor nome-cor)) ; => {:yellow "Amarelo", :blue "Azul", :red "Vermelho"}

; Formas do tipo if aceitam três argumentos: a condição de teste, o comando a ser
; executado caso a condição seja positiva; e o comando para o caso de ela ser falsa.
(if true "a" "b") ; => "a"
(if false "a" "b") ; => "b"

; Opcionalmente você pode não passar o último argumento, mas se a condição for falsa
; o if vai retornar nil. 
(if false "a") ; => nil

; A forma if somente aceita um comando para ser executado em cada caso. Se você
; precisar executar mais comandos, você pode usar a função do:
(if true
  (do
    (print "Olá ")
    (print "Mundo"))) ; => escreve "Olá Mundo" na saída

; Se você só deseja tratar o caso de sua condição ser verdadeira, o comando when é
; uma alternativa melhor. Seu comportamento é idêntico a um if sem condição negativa.
; Uma de suas vantagens é permitir a execução de vários comandos sem exigir do:
(when true "a") ; => "a"
(when true
  (print "Olá ")
  (print "Mundo")) ; => também escreve "Olá Mundo" na saída

; Isso ocorre porque when possui um bloco do implícito. O mesmo se aplica a funções e
; comandos let:
(defn escreve-e-diz-xis
  [nome]
  (print "Diga xis, " nome)
  (str "Olá " nome))
(escreve-e-diz-xis "João") ;=> "Olá João", além de escrever "Diga xis, João" na saída.

(let [nome "Nara"]
  (print "Diga xis, " nome)
  (str "Olá " nome)) ;=> "Olá João", além de escrever "Diga xis, João" na saída.


; Módulos
;;;;;;;;;;;;;;;

; Você pode usar a função use para carregar todas as funções de um módulo.
(use 'clojure.set)

; Agora nós podemos usar operações de conjuntos definidas nesse módulo:
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; Isso porém não é uma boa prática pois dificulta saber de qual módulo cada função
; veio, além de expor o código a conflitos de nomes, caso dois módulos diferentes
; definam funções com o mesmo nome. A melhor forma de referenciar módulos é por meio
; de require:
(require 'clojure.string)

; Com isso podemos chamar as funções de clojure.string usando o operador /
; Aqui, o módulo é clojure.string e a função é blank?
(clojure.string/blank? "") ; => true

; Porém isso não é muito prático, por isso é possível dar para um nome mais curto para
; o módulo ao carregá-lo:
(require '[clojure.string :as str])
(str/replace "alguém quer teste?" #"[aeiou]" str/upper-case) ; => "AlgUém qUEr tEstE?"

; Nesse exemplo usamos também a construção #"", que delimita uma expressão regular.

; É possível carregar outros módulos direto na definição do namespace. Note que nesse
; contexto não é preciso usar ' antes do vetor que define a importação do módulo.
(ns test
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))


; Operadores thread
;;;;;;;;;;;;;;;;;

; Uma das funções mais interessantes de clojure são os operadores -> e ->> - respectivamente
; thread-first e thread-last macros. Elas permitem o encadeamento de chamadas de funções,
; sendo perfeitas para melhorar a legibilidade em transformações de dados.

; -> usa o resultado de uma chamada como o primeiro argumento da chamada à função seguinte:
(-> "  uMa   StRIng com! aLG_uNs  ##problemas. "
    (str/replace #"[!#_]" "")
    (str/replace #"\s+" " ")
    str/trim          ; se a função só aceitar um argumento, não é preciso usar parênteses
    (str/lower-case)) ; => "uma string com alguns problemas."

; Na thread uma string com vários problemas foi passada como primeiro argumento à função
; str/replace, que criou uma nova string, a partir da original, porém somente com caracteres
; alfabéticos. Essa nova string foi passada como primeiro argumento para a chamada str/replace
; seguinte, que criou uma nova string sem espaços duplos. Essa nova string foi então passada
; como primeiro argumento para str/trim, que removeu espaços de seu início e fim, passando essa
; última string para str/lower-case, que a converteu para caracteres em caixa baixa.

; ->> é equivalente a ->, porém o retorno de cada função é passado como último argumento da
; função seguinte. Isso é particularmente útil para lidar com seqs, já que as funções que
; as manipulam sempre as tomam como último argumento.
(->> '(1 2 3 4)
     (filter even?) ; => '(2 4)
     (map inc)      ; => '(3 5)
     (reduce *))    ; => 15


; Java
;;;;;;;;;;;;;;;;;

; A biblioteca padrão de Java é enorme e possui inúmeros algoritmos e estruturas de
; dados já implementados. Por isso é bastante conveniente saber como usá-la dentro
; de Clojure.

; Use import para carregar um módulo Java.
(import java.util.Date)

; Você pode importar classes Java dentro de ns também:
(ns test
  (:import java.util.Date
           java.util.Calendar
           java.util.ArrayList))

; Use o nome da clase com um "." no final para criar uma nova instância
(def instante (Date.))
(class instante) => ; java.util.Date

; Para chamar um método, use o operador . com o nome do método. Outra forma é
; usar simplesmente .<nome do método>
(. instante getTime) ; => retorna um inteiro representando o instante
(.getTime instante) ; => exatamente o mesmo que acima

; Para chamar métodos estáticos dentro de classes Java, use /
(System/currentTimeMillis) ; => retorna um timestamp

; Note que não é preciso importar o módulo System, pois ele está sempre presente

; Caso queira submeter uma instância de uma classe mutável a uma sequência de operações,
; você pode usar a função doto. Ela é funciona de maneira semelhante à função -> - ou
; thread-first -, exceto pelo fato de que ele opera com valores mutáveis.
(doto (java.util.ArrayList.)
  (.add 11)
  (.add 3)
  (.add 7)
  (java.util.Collections/sort)) ; => #<ArrayList [3, 7, 11]>


; STM
;;;;;;;;;;;;;;;;;

; Até aqui usamos def para associar nomes a valores. Isso, no entanto, possui algumas
; limitações, já que, uma vez definido essa associação, não podemos alterar o valor
; para o qual um nome aponta. Isso significa que nomes definidos com def não se
; comportam como as variáveis de outras linguagens.

; Para lidar com estado persistente e mutação de valores, Clojure usa o mecanismo Software
; Transactional Memory. O atom é o mais simples de todos. Passe pra ele um valor inicial e
; e ele criará um objeto que é seguro de atualizar:
(def atom-mapa (atom {}))

; Para acessar o valor de um atom, você pode usar a função deref ou o operador @: 
@atom-mapa ; => {}
(deref atom-mapa) ; => {}

; Para mudar o valor de um atom, você deve usar a função swap!
; O que ela faz é chamar a função passada usando o atom como seu primeiro argumento. Com
; isso, ela altera o valor do atom de maneira segura. 
(swap! atom-mapa assoc :a 1) ; Atribui a atom-mapa o resultado de (assoc {} :a 1)
(swap! atom-mapa assoc :b 2) ; Atribui a atom-mapa o resultado de (assoc {:a 1} :b 2)

; Observe que essas chamadas alteraram de fato o valor de atom-mapa. Seu novo valor é:
@atom-mapa ; => {:a 1 :b 2}

; Isso é diferente de fazer:
(def atom-mapa-2 (atom {}))
(def atom-mapa-3 (assoc @atom-mapa-2 :a 1))

; Nesse exemplo, atom-mapa-2 permanece com o seu valor original e é gerado um novo mapa,
; atom-mapa-3, que contém o valor de atom-mapa-2 atualizado. Note que atom-mapa-3 é um
; simples mapa, e não uma instância de um atom
@atom-mapa-2 ; => {}
atom-mapa-3 ; => {:a 1}

(class atom-mapa-2) ; => clojure.lang.Atom
(class atom-mapa-3) ; => clojure.lang.PersistentArrayMap

; A ideia é que o valor do atom só será atualizado se, após ser executada a função passada
; para swap!, o atom ainda estiver com o mesmo valor de antes. Isto é, se durante a execução
; da função alguém alterar o valor do atom, swap! reexecutará a função recebida usando o valor
; atual do átoma como argumento.

; Isso é ótimo em situações nas quais é preciso garantir a consistência de algum valor - tais
; como sistemas bancários e sites de compra. Para mais exemplos e informações sobre outras
; construções STM:

; Exemplos e aplicações: https://www.braveclojure.com/zombie-metaphysics/
; Refs: http://clojure.org/refs
; Agents: http://clojure.org/agents
```

### Leitura adicional

Esse tutorial está longe de ser completo, mas deve ser suficiente para que você possa dar seus primeiros passos em Clojure.
Caso queira aprender mais:

* clojure.org tem vários artigos:
[http://clojure.org/](http://clojure.org/)

* Brave Clojure possui um e-book que explora em profundidade diversos recursos de clojure, incluindo ótimos exemplos:
[https://www.braveclojure.com/](https://www.braveclojure.com/)

* clojuredocs.org tem documentação com exemplos para quase todas as funções principais (pertecentes ao core):
[http://clojuredocs.org/quickref/Clojure%20Core](http://clojuredocs.org/quickref/Clojure%20Core)

* 4clojure possui alguns problemas e desafios interessantes para quem quiser treinar clojure ou programação funcional:
[http://www.4clojure.com/](http://www.4clojure.com/)

* clojure-doc.org tem um bom número de artigos para iniciantes:
[http://clojure-doc.org/](http://clojure-doc.org/)

Clojure for the Brave and True é um livro de introdução ao Clojure e possui uma versão gratuita online:
[https://www.braveclojure.com/clojure-for-the-brave-and-true/](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
