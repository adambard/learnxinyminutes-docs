---
language: ruby
filename: learnruby-it.rb
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
  - ["Tristan Hume", "http://thume.ca/"]
  - ["Nick LaMuro", "https://github.com/NickLaMuro"]
  - ["Marcos Brizeno", "http://www.about.me/marcosbrizeno"]
  - ["Ariel Krakowski", "http://www.learneroo.com"]
  - ["Dzianis Dashkevich", "https://github.com/dskecse"]
  - ["Levi Bostian", "https://github.com/levibostian"]
  - ["Rahil Momin", "https://github.com/iamrahil"]
  - ["Gabriel Halley", "https://github.com/ghalley"]
  - ["Persa Zula", "http://persazula.com"]
  - ["Jake Faris", "https://github.com/farisj"]
  - ["Corey Ward", "https://github.com/coreyward"]
translators:
  - ["abonte", "https://github.com/abonte"]
lang: it-it   
---

```ruby
# Questo è un commento

# In Ruby, (quasi) tutto è un oggetto.
# Questo include i numeri...
3.class #=> Integer

# ...stringhe...
"Hello".class #=> String

# ...e anche i metodi!
"Hello".method(:class).class #=> Method

# Qualche operazione aritmetica di base
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2 ** 5 #=> 32
5 % 3 #=> 2

# Bitwise operators
3 & 5 #=> 1
3 | 5 #=> 7
3 ^ 5 #=> 6

# L'aritmetica è solo zucchero sintattico
# per chiamare il metodo di un oggetto
1.+(3) #=> 4
10.* 5 #=> 50
100.methods.include?(:/) #=> true

# I valori speciali sono oggetti
nil # equivalente a null in altri linguaggi
true # vero
false # falso

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Uguaglianza
1 == 1 #=> true
2 == 1 #=> false

# Disuguaglianza
1 != 1 #=> false
2 != 1 #=> true

# nil è l'unico valore, oltre a false, che è considerato 'falso'
!!nil   #=> false
!!false #=> false
!!0     #=> true
!!""    #=> true

# Altri confronti
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Operatori di confronto combinati (ritorna '1' quando il primo argomento è più
# grande, '-1' quando il secondo argomento è più grande, altrimenti '0')
1 <=> 10 #=> -1
10 <=> 1 #=> 1
1 <=> 1 #=> 0

# Operatori logici
true && false #=> false
true || false #=> true

# Ci sono versioni alternative degli operatori logici con meno precedenza.
# Sono usati come costrutti per il controllo di flusso per concatenare
# insieme statement finché uno di essi ritorna true o false.

# `do_something_else` chiamato solo se `do_something` ha successo.
do_something() and do_something_else()
# `log_error` è chiamato solo se `do_something` fallisce.
do_something() or log_error()

# Interpolazione di stringhe

placeholder = 'usare l\'interpolazione di stringhe'
"Per #{placeholder} si usano stringhe con i doppi apici"
#=> "Per usare l'interpolazione di stringhe si usano stringhe con i doppi apici"

# E' possibile combinare le stringhe usando `+`, ma non con gli altri tipi
'hello ' + 'world'  #=> "hello world"
'hello ' + 3 #=> TypeError: can't convert Fixnum into String
'hello ' + 3.to_s #=> "hello 3"
"hello #{3}" #=> "hello 3"

# ...oppure combinare stringhe e operatori
'ciao ' * 3 #=> "ciao ciao ciao "

# ...oppure aggiungere alla stringa
'ciao' << ' mondo' #=> "ciao mondo"

# Per stampare a schermo e andare a capo
puts "Sto stampando!"
#=> Sto stampando!
#=> nil

# Per stampare a schermo senza andare a capo
print "Sto stampando!"
#=> Sto stampando! => nil

# Variabili
x = 25 #=> 25
x #=> 25

# Notare che l'assegnamento ritorna il valore assegnato.
# Questo significa che è possibile effettuare assegnamenti multipli:
x = y = 10 #=> 10
x #=> 10
y #=> 10

# Per convenzione si usa lo snake_case per i nomi delle variabili
snake_case = true

# Usare nomi delle variabili descrittivi
path_to_project_root = '/buon/nome/'
m = '/nome/scadente/'

# I simboli sono immutabili, costanti riusabili rappresentati internamente da
# un valore intero. Sono spesso usati al posto delle stringhe per comunicare
# specifici e significativi valori.

:pendente.class #=> Symbol

stato = :pendente

stato == :pendente #=> true

stato == 'pendente' #=> false

stato == :approvato #=> false

# Le stringhe possono essere convertite in simboli e viceversa:
status.to_s #=> "pendente"
"argon".to_sym #=> :argon

# Arrays

# Questo è un array
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Gli array possono contenere diversi tipi di elementi
[1, 'hello', false] #=> [1, "hello", false]

# Gli array possono essere indicizzati
# Dall'inizio...
array[0] #=> 1
array.first #=> 1
array[12] #=> nil


# ...o dalla fine...
array[-1] #=> 5
array.last #=> 5

# With a start index and length
# ...o con un indice di inzio e la lunghezza...
array[2, 3] #=> [3, 4, 5]

# ...oppure con un intervallo.
array[1..3] #=> [2, 3, 4]

# Invertire l'ordine degli elementi di un array
a = [1,2,3]
a.reverse! #=> [3,2,1]

# Come per l'aritmetica,  l'accesso tramite [var]
# è solo zucchero sintattico
# per chiamare il metodo '[]'' di un oggetto
array.[] 0 #=> 1
array.[] 12 #=> nil

# Si può aggiungere un elemento all'array così
array << 6 #=> [1, 2, 3, 4, 5, 6]
# oppure così
array.push(6) #=> [1, 2, 3, 4, 5, 6]

# Controllare se un elemento esiste in un array
array.include?(1) #=> true

# Hash è un dizionario con coppie di chiave e valore
# Un hash è denotato da parentesi graffe:
hash = { 'colore' => 'verde', 'numero' => 5 }

hash.keys #=> ['colore', 'numero']

# E' possibile accedere all'hash tramite chiave:
hash['colore'] #=> 'verde'
hash['numero'] #=> 5

# Accedere all'hash con una chiave che non esiste ritorna nil:
hash['nothing here'] #=> nil

# Quando si usano simboli come chiavi di un hash, si possono utilizzare
# queste sintassi:

hash = { :defcon => 3, :action => true }
hash.keys #=> [:defcon, :action]
# oppure
hash = { defcon: 3, action: true }
hash.keys #=> [:defcon, :action]

# Controllare l'esistenza di una chiave o di un valore in un hash
new_hash.key?(:defcon) #=> true
new_hash.value?(3) #=> true

# Suggerimento: sia gli array che gli hash sono enumerabili!
# Entrambi possiedono metodi utili come each, map, count e altri.

# Strutture di controllo

#Condizionali
if true
  'if statement'
elsif false
  'else if, opzionale'
else
  'else, opzionale'
end

#Cicli
# In Ruby, i tradizionali cicli `for` non sono molto comuni. Questi semplici
# cicli, invece, sono implementati con un enumerable, usando `each`:
(1..5).each do |contatore|
  puts "iterazione #{contatore}"
end

# Esso è equivalente a questo ciclo, il quale è inusuale da vedere in Ruby:
for contatore in 1..5
  puts "iterazione #{contatore}"
end

# Il costrutto `do |variable| ... end` è chiamato 'blocco'. I blocchi
# sono simili alle lambda, funzioni anonime o closure che si trovano in altri
# linguaggi di programmazione. Essi possono essere passati come oggetti,
# chiamati o allegati come metodi.
# 
# Il metodo 'each' di un intervallo (range) esegue il blocco una volta
# per ogni elemento dell'intervallo.
# Al blocco è passato un contatore come parametro.

# E' possibile inglobare il blocco fra le parentesi graffe
(1..5).each { |contatore| puts "iterazione #{contatore}" }

# Il contenuto delle strutture dati può essere iterato usando "each".
array.each do |elemento|
  puts "#{elemento} è parte dell'array"
end
hash.each do |chiave, valore|
  puts "#{chiave} è #{valore}"
end

# If you still need an index you can use 'each_with_index' and define an index
# variable
# Se comunque si vuole un indice, si può usare "each_with_index" e definire
# una variabile che contiene l'indice
array.each_with_index do |elemento, indice|
  puts "#{elemento} è il numero #{index} nell'array"
end

contatore = 1
while contatore <= 5 do
  puts "iterazione #{contatore}"
  contatore += 1
end
#=> iterazione 1
#=> iterazione 2
#=> iterazione 3
#=> iterazione 4
#=> iterazione 5

# Esistono in Ruby ulteriori funzioni per fare i cicli,
# come per esempio 'map', 'reduce', 'inject' e altri.
# Nel caso di 'map', esso prende l'array sul quale si sta iterando, esegue
# le istruzioni definite nel blocco, e ritorna un array completamente nuovo.
array = [1,2,3,4,5]
doubled = array.map do |elemento|
  elemento * 2
end
puts doubled
#=> [2,4,6,8,10]
puts array
#=> [1,2,3,4,5]

# Costrutto "case"
grade = 'B'

case grade
when 'A'
  puts 'Way to go kiddo'
when 'B'
  puts 'Better luck next time'
when 'C'
  puts 'You can do better'
when 'D'
  puts 'Scraping through'
when 'F'
  puts 'You failed!'
else
  puts 'Alternative grading system, eh?'
end
#=> "Better luck next time"

# 'case' può usare anche gli intervalli
grade = 82
case grade
when 90..100
  puts 'Hooray!'
when 80...90
  puts 'OK job'
else
  puts 'You failed!'
end
#=> "OK job"

# Gestione delle eccezioni
begin
  # codice che può sollevare un eccezione
  raise NoMemoryError, 'Esaurita la memoria.'
rescue NoMemoryError => exception_variable
  puts 'NoMemoryError è stato sollevato.', exception_variable
rescue RuntimeError => other_exception_variable
  puts 'RuntimeError è stato sollvato.'
else
  puts 'Questo viene eseguito se nessuna eccezione è stata sollevata.'
ensure
  puts 'Questo codice viene sempre eseguito a prescindere.'
end

# Metodi

def double(x)
  x * 2
end

# Metodi (e blocchi) ritornano implicitamente il valore dell'ultima istruzione
double(2) #=> 4

# Le parentesi sono opzionali dove l'interpolazione è inequivocabile
double 3 #=> 6

double double 3 #=> 12

def sum(x, y)
  x + y
end

# Gli argomenit dei metodi sono separati dalla virgola
sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12

# yield
# Tutti i metodi hanno un implicito e opzionale parametro del blocco.
# Esso può essere chiamato con la parola chiave 'yield'.

def surround
  puts '{'
  yield
  puts '}'
end

surround { puts 'hello world' }

# {
# hello world
# }

# I blocchi possono essere convertiti in 'proc', il quale racchiude il blocco
# e gli permette di essere passato ad un altro metodo, legato ad uno scope
# differente o modificato. Questo è molto comune nella lista parametri del
# metodo, dove è frequente vedere il parametro '&block' in coda. Esso accetta
# il blocco, se ne è stato passato uno, e lo converte in un 'Proc'.
# Qui la denominazione è una convenzione; funzionerebbe anche con '&ananas'.
def guests(&block)
  block.class #=> Proc
  block.call(4)
end

# Il metodo 'call' del Proc è simile allo 'yield' quando è presente un blocco.
# Gli argomenti passati a 'call' sono inoltrati al blocco come argomenti:

guests { |n| "You have #{n} guests." }
# => "You have 4 guests."

# L'operatore splat ("*") converte una lista di argomenti in un array
def guests(*array)
  array.each { |guest| puts guest }
end

# Destrutturazione

# Ruby destruttura automaticamente gli array in assegnamento
# a variabili multiple:
a, b, c = [1, 2, 3]
a #=> 1
b #=> 2
c #=> 3

# In alcuni casi si usa l'operatore splat ("*") per destrutturare
# un array in una lista.
classifica_concorrenti = ["John", "Sally", "Dingus", "Moe", "Marcy"]

def migliore(primo, secondo, terzo)
  puts "I vincitori sono #{primo}, #{secondo}, e #{terzo}."
end

migliore *classifica_concorrenti.first(3)
#=> I vincitori sono John, Sally, e Dingus.

# The splat operator can also be used in parameters:
def migliore(primo, secondo, terzo, *altri)
  puts "I vincitori sono #{primo}, #{secondo}, e #{terzo}."
  puts "C'erano altri #{altri.count} partecipanti."
end

migliore *classifica_concorrenti 
#=> I vincitori sono John, Sally, e Dingus.
#=> C'erano altri 2 partecipanti.

# Per convenzione, tutti i metodi che ritornano un booleano terminano
# con un punto interrogativo
5.even? #=> false
5.odd? #=> true

# Per convenzione, se il nome di un metodo termina con un punto esclamativo,
# esso esegue qualcosa di distruttivo. Molti metodi hanno una versione con '!'
# per effettuare una modifiche, e una versione senza '!' che ritorna
# una versione modificata.
nome_azienda = "Dunder Mifflin"
nome_azienda.upcase #=> "DUNDER MIFFLIN"
nome_azienda #=> "Dunder Mifflin"
# Questa volta modifichiamo nome_azienda
nome_azienda.upcase! #=> "DUNDER MIFFLIN"
nome_azienda #=> "DUNDER MIFFLIN"

# Classi

# Definire una classe con la parola chiave class
class Umano

  # Una variabile di classe. E' condivisa da tutte le istance di questa classe.
  @@specie = 'H. sapiens'

  # Inizializzatore di base
  def initialize(nome, eta = 0)
    # Assegna il valore dell'argomento alla variabile dell'istanza "nome"
    @nome = nome
    # Se l'età non è fornita, verrà assegnato il valore di default indicato
    # nella lista degli argomenti
    @eta = eta
  end

  # Metodo setter di base
  def nome=(nome)
    @nome = nome
  end

  # Metodo getter di base
  def nome
    @nome
  end

  # Le funzionalità di cui sopra posso essere incapsulate usando
  # il metodo attr_accessor come segue
  attr_accessor :nome

  # Getter/setter possono anche essere creati individualmente
  attr_reader :nome
  attr_writer :nome

  # Un metodo della classe usa 'self' per distinguersi dai metodi dell'istanza.
  # Può essere richimato solo dalla classe, non dall'istanza.
  def self.say(msg)
    puts msg
  end

  def specie
    @@specie
  end
end


# Instanziare una classe
jim = Umano.new('Jim Halpert')

dwight = Umano.new('Dwight K. Schrute')

# Chiamiamo qualche metodo
jim.specie #=> "H. sapiens"
jim.nome #=> "Jim Halpert"
jim.nome = "Jim Halpert II" #=> "Jim Halpert II"
jim.nome #=> "Jim Halpert II"
dwight.specie #=> "H. sapiens"
dwight.nome #=> "Dwight K. Schrute"

# Chiamare un metodo della classe
Umano.say('Ciao') #=> "Ciao"

# La visibilità della variabile (variable's scope) è determinata dal modo
# in cui le viene assegnato il nome.
# Variabili che iniziano con $ hanno uno scope globale
$var = "Sono una variabile globale"
defined? $var #=> "global-variable"

# Variabili che inziano con @ hanno a livello dell'istanza
@var = "Sono una variabile dell'istanza"
defined? @var #=> "instance-variable"

# Variabili che iniziano con @@ hanno una visibilità a livello della classe
@@var = "Sono una variabile della classe"
defined? @@var #=> "class variable"

# Variabili che iniziano con una lettera maiuscola sono costanti
Var = "Sono una costante"
defined? Var #=> "constant"

# Anche una classe è un oggetto in ruby. Quindi la classe può avere
# una variabile dell'istanza. Le variabili della classe sono condivise
# fra la classe e tutti i suoi discendenti.

# Classe base
class Umano
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

# Classe derivata
class Lavoratore < Umano
end

Umano.foo #=> 0
Lavoratore.foo #=> 0

Umano.foo = 2 #=> 2
Lavoratore.foo #=> 2

# La variabile dell'istanza della classe non è condivisa dai discendenti.

class Umano
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(value)
    @bar = value
  end
end

class Dottore < Umano
end

Umano.bar #=> 0
Dottore.bar #=> nil

module EsempioModulo
  def foo
    'foo'
  end
end

# Includere moduli vincola i suoi metodi all'istanza della classe.
# Estendere moduli vincola i suoi metodi alla classe stessa.
class Persona
  include EsempioModulo
end

class Libro
  extend EsempioModulo
end

Persona.foo     #=> NoMethodError: undefined method `foo' for Person:Class
Persona.new.foo #=> 'foo'
Libro.foo       #=> 'foo'
Libro.new.foo   #=> NoMethodError: undefined method `foo'

# Callbacks sono eseguiti quand si include o estende un modulo
module ConcernExample
  def self.included(base)
    base.extend(ClassMethods)
    base.send(:include, InstanceMethods)
  end

  module ClassMethods
    def bar
      'bar'
    end
  end

  module InstanceMethods
    def qux
      'qux'
    end
  end
end

class Something
  include ConcernExample
end

Something.bar     #=> 'bar'
Something.qux     #=> NoMethodError: undefined method `qux'
Something.new.bar #=> NoMethodError: undefined method `bar'
Something.new.qux #=> 'qux'
```

## Ulteriori risorse

- [Learn Ruby by Example with Challenges](http://www.learneroo.com/modules/61/nodes/338) - Una variante di questa guida con esercizi nel browser.
- [An Interactive Tutorial for Ruby](https://rubymonk.com/) - Imparare Ruby attraverso una serie di tutorial interattivi.
- [Official Documentation](http://ruby-doc.org/core)
- [Ruby from other languages](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - Una passata [edizione libera](http://ruby-doc.com/docs/ProgrammingRuby/) è disponibile online.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - A community-driven Ruby coding style guide.
- [Try Ruby](http://tryruby.org) - Imparare le basi del linguaggio di programmazion Ruby, interattivamente nel browser.
