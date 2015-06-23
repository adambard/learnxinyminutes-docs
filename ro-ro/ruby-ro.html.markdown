---
language: ruby
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
  - ["Tristan Hume", "http://thume.ca/"]
  - ["Nick LaMuro", "https://github.com/NickLaMuro"]
  - ["Marcos Brizeno", "http://www.about.me/marcosbrizeno"]
translators:
  - ["Adrian Bordinc", "https://github.com/ellimist"]
filename: learnruby-ro.rb
lang: ro-ro
---

```ruby
# Acesta este un comentariu

=begin
Acesta este un comentariu pe mai multe linii
Nimeni nu le foloseste
Si nici tu nu ar trebui sa o faci
=end

# In primul rand: totul este un obiect

# Numerele sunt obiecte

3.class #=> Fixnum

3.to_s #=> "3"


# Aritmetica de baza
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Aritmetica este doar "zahar sintactic"
# pentru a putea chema metode pe un obiect
1.+(3) #=> 4
10.* 5 #=> 50

# Valorile speciale sunt obiecte
nil # Nimic
true # true
false # false

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Egalitate
1 == 1 #=> true
2 == 1 #=> false

# Inegalitate
1 != 1 #=> false
2 != 1 #=> true
!true  #=> false
!false #=> true

# Excluzand "false", "nil" este singura valoare "falsa"

!nil   #=> true
!false #=> true
!0     #=> false

# Mai multe comparatii
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Sirurule de caractere sunt obiecte

'Sunt un sir de caractere'.class #=> String
"Si eu sunt un sir de caractere".class #=> String

fi_inlocuit = "fi inlocuit"
"Pot #{fi_inlocuit} atunci cand folosesc dublu apostrof"
#=> "Pot fi inlocuit atunci cand folosesc dublu apostrof"


# Printeaza 
puts "Afisez rezultate!"

# Variabile
x = 25 #=> 25
x #=> 25

# Retineti faptul ca atribuire unei valori, o si returneaza pe aceasta
# Asta inseamna ca poti sa faci atribuire multipla:

x = y = 10 #=> 10
x #=> 10
y #=> 10

# Prin conventie se foloseste "snake_case" in denumirea variabilelor
snake_case = true

# Folositi nume descriptive pentru variablie
adresa_radacina_proiect = '/nume/bun/'
adresa = '/nume/nu atat de bun/'

# Simbolurile (sunt obiecte)
# Simbolurile sunt constante imutabile, reutilizabile, reprezentate intern 
# de o valoare numerica. Sunt deseori folosite in locul sirurilor de caractere 
# pentru a da un nume reprezentativ unei valori

:exemplu_simbol.class #=> Symbol

status = :exemplu_simbol

status == :exemplu_simbol #=> adevarat

status == 'exemplu_simbol' #=> fals

status == :aprobat #=> fals

# Vectori

# Acesta este un vector
vector = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Vectorii pot contine diferite tipuri de date

[1, "salut", false] #=> [1, "salut", false]

# Vectorii pot fi indexati
# de la inceput
vector[0] #=> 1
vector[12] #=> nil

# Ca si aritmetica, accessul [valoare]
# este doar "zahar sintactic"
# pentru a chema metoda [] a unui obiect
vector.[] 0 #=> 1
vector.[] 12 #=> nil

# De la sfarsit
vector[-1] #=> 5

# Cu un index de inceput si o lungime
vector[2, 3] #=> [3, 4, 5]

# Sau cu un interval
vector[1..3] #=> [2, 3, 4]

# Adauga elemente intr-un vector in felul urmator:
vector << 6 #=> [1, 2, 3, 4, 5, 6]

# Hash-urile sunt dictionarele din Ruby cu perechi cheie/valoare.
# Hash-urile sunt notate cu acolade
hash = {'culoare' => 'verde', 'numar' => 5}

hash.keys #=> ['culoare', 'numar']

# Poti lua valoare unui element dintr-un hash foarte rapid folosind cheia
hash['culoare'] #=> 'verde'
hash['numar'] #=> 5

# Incercand sa accesezi un element dintr-un hash 
# printr-o cheie care nu exista va returna "nil".
hash['nimic_aici'] #=> nil

# Incepand cu Ruby 1.9, este o sintaxa speciala 
# pentru atunci cand se folosesc simboluri drept chei:

hash_nou = { defcon: 3, actiune: true}

hash_now.keys #=> [:defcon, :actiune]

# Pont: Atat vectorii (Array) si hash-urile (Hash) sunt enumerabile (Enumerable)
# Ele impart o multime de metode utile precum each, map, count si altele


# Structuri de control

if true
  "instructiune if"
elsif false
  "else if, optional"
else
  "else, de asemenea optional"
end

for numar in 1..5
  puts "iteratia #{numar}"
end
#=> iteratia 1
#=> iteratia 2
#=> iteratia 3
#=> iteratia 4
#=> iteratia 5

# TOTUSI, Nici una nu foloseste instructiunea for
# In locul acesteia ar trebui sa folosesti metoda "each" si sa ii trimiti un block
# Un bloc este o bucata de cod pe care o poti trimite unei metode precum "each".
# Este analog pentru "lambda", functii anonime,
# sau closures in alte limbaje de programare.
#
# Metoda "each" a unui interval, ruleaza block-ul o data
# pentru fiecare element din interval.
# Block-ul primeste ca si parametru un index
# Invocand metoda "each" cu un block, arata in urmatorul fel:

(1..5).each do |index|
  puts "iteratia #{index}"
end
#=> iteratia 1
#=> iteratia 2
#=> iteratia 3
#=> iteratia 4
#=> iteratia 5

# Poti de asemenea sa pui block-ul intre acolade
(1..5).each {|index| puts "iteratia #{index}"}

# Continutul unei structuri de date poate fi parcurs folosind "each".
array.each do |element|
  puts "#{element} parte din vector"
end
hash.each do |cheie, valoare|
  puts "#{cheie} este #{valoare}"
end

index = 1
while index <= 5 do
  puts "iteratia #{index}"
  index += 1
end
#=> iteratia 1
#=> iteratia 2
#=> iteratia 3
#=> iteratia 4
#=> iteratia 5

nota = 'B'

case nota
when 'A'
  puts "Bravo pustiule!"
when 'B'
  puts "Mai mult noroc data viitoare"
when 'C'
  puts "Poti mai mult"
when 'D'
  puts "Incet, incet..."
when 'F'
  puts "Ai esuat!"
else
  puts "Sistem de notare alternativ?!"
end

# Functii

def dublu(x)
  x * 2
end

# Functille (si toate block-urile) 
# returneaza implicit valoarea ultimei instructiuni
dublu(2) #=> 4

# Parantezele sunt optionale cand rezultatul nu este ambiguu
dublu 3 #=> 6

dublu dublu 3 #=> 12

def suma(x,y)
  x + y
end

# Argumentele metodei sunt separate printr-o virgula
suma 3, 4 #=> 7

suma suma(3,4), 5 #=> 12

# yield
# Toate metodele au un parametru block, implicit si optional
# care poate fi invocat folosit cuvantul cheie 'yield'

def incercuieste
  puts "{"
  yield
  puts "}"
end

incercuieste { puts 'Salut Mihai!' }

# {
# Salut Mihai!
# }


# Poti trimite un block unei functii.
# "&" marcheaza o referinta trimisa unui block
def vizitatori(&block)
 block.call "un_parametru" 
end
 
# Poti trimite o lista de argumente, care va fi convertita intr-un vector (array).
# Pentru asta se foloseste ("*")
def vizitatori(*vector)
 vector.each { |vizitator| puts "#{vizitator}" }
end

# Defineste o clasa folosind cuvantul cheie "class"
class Om

  # O variabila apartinand clasei. Este folosita in toate instantele clasei
  @@specie = "H. sapiens"

  # Constructor
  def initialize(nume, varsta=0)
    # Atribuie argumentul, variabilei "nume", care apartine doar unei instante
    @nume = nume
    # Daca varsta nu este data, o sa ii atribuim valoarea implicita
    # din lista de argumente (0, in cazul nostru)
    @varsta = varsta
  end

  # Metoda pentru a seta valoarea unei variabile
  def nume=(nume)
    @nume = nume
  end

  # Metoda pentru a lua valoarea unei variabile
  def nume
    @nume
  end

  # Functionalitatea de mai sus poate fi obtinuta 
  # folosing metoda "attr_accessor" dupa cum urmeaza:
  attr_accessor :nume

  # Metodele pentru a lua si a seta valoarea unei variabile 
  # pot fi de asemenea obtinute individial:
  attr_reader :nume
  attr_writer :nume

  # O metoda apartinand unei clase foloseste "self" pentru a se diferentia 
  # de metodele unei instante ale clasei respective
  # Poate fi invocata doar pe clasa, si nu pe o instanta a acesteia
  def self.spune(msg)
    puts "#{msg}"
  end

  def specie
    @@specie
  end

end


# Creaza o instanta a unei clase
ion = Om.new("Ionut Popescu")

eugen = Om.new("Eugen Ionescu")

# Sa invocam niste metode
ion.specie #=> "H. sapiens"
ion.nume #=> "Ionut Popescu"
ion.nume = "Ionut Popescu JR." #=> "Ionut Popescu JR."
ion.nume #=> "Ionut Popescu JR."
eugen.specie #=> "H. sapiens"
eugen.nume #=> "Eugen Ionescu"

# Invoca o metoda a unei clase
Om.spune("Salut") #=> "Salut"


# Scopul unei variabile este definit de modul in care le numim
# Variabilele care incep cu $ au scop global
$var = "Sunt o variabila globala"
defined? $var #=> "global-variable"

# Variabilele care incep cu @ apartin unei instante
@var = "Sunt o variabila a unei instante"
defined? @var #=> "instance-variable"

# Variabilele care incep cu @@ apartin unei clase
@@var = "Sunt variabila unei clase"
defined? @@var #=> "class variable"

# Variabilele care incep cu litera mare sunt constante
Var = "Sunt o constanta"
defined? Var #=> "constant"

# Clasele sunt de asemenea obiecte in ruby. Astfel incat clasele 
# pot avea variabile care apartin unei instante
# O variabila care apartine unei clase poate fi accesata de toate 
# instantele acesteia si de clasele care o extind

# clasa parinte
class Om
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(valoare)
    @@foo = valoare
  end
end

#  clasa copil
class Muncitor < Om
end

Om.foo # 0
Muncitor.foo # 0

Om.foo = 2 # 2
Muncitor.foo # 2

# Variabilele care apartin unei instante ale unei clase, 
# nu sunt impartite de (copii acesteia) clasele care o extind
class Om
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(valoare)
    @bar = valoare
  end
end

class Doctor < Om
end

Om.bar # 0
Doctor.bar # nil

module ExempluModul
  def foo
    'foo'
  end
end

# Incluzand modulul instantei unui obiect
# Extinzand modulul unei instante ale unei clase

class Persoana
  include ExempluModul
end

class Carte
  extend ExempluModul
end

Persoana.foo     # => NoMethodError: undefined method `foo' for Persoana:Class
Persoana.new.foo # => 'foo'
Carte.foo       # => 'foo'
Carte.new.foo   # => NoMethodError: undefined method `foo'

# Callbacks atunci cand includerea si extinderea unui modul sunt executate

module ModulExempluCallBack
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

class CevaRelevant
  include ModulExempluCallBack
end

CevaRelevant.bar     # => 'bar'
CevaRelevant.qux     # => NoMethodError: undefined method `qux'
CevaRelevant.new.bar # => NoMethodError: undefined method `bar'
CevaRelevant.new.qux # => 'qux'
```
