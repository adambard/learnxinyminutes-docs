---
language: ruby
filename: learnruby-fi.rb
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
translators:
  - ["Oliver Vartiainen", "https://github.com/firoxer"]
lang: fi-fi
---

```ruby
# Tässä yhden rivin kommentti

=begin
Tässä usean rivin kommentti
Näitä ei kylläkään käytetä
Joten käytetään vastedes vain yksirivisiä
=end

# Tärkeintä on muistaa, että Rubyssa kaikki pohjautuu olioihin.

# Luvutkin ovat olioita:

3.class #=> Fixnum

3.to_s #=> "3"

# Peruslaskutoimituksia:
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2**5 #=> 32
5 % 3 #=> 2

# Bittioperaatioita:
3 & 5 #=> 1
3 | 5 #=> 7
3 ^ 5 #=> 6

# Laskutoimitukset ovat vain syntaksisokeria lukuolion laskumetodin kutsulle:
1.+(3) #=> 4
10.* 5 #=> 50

# Erityisarvotkin ovat olioita:

nil # vastaa joidenkin kielten "null"-arvoa
true # tosi
false # epätosi

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Samanvertaisuuden testaus:
1 == 1 #=> true
2 == 1 #=> false

# ...ja sama eriarvoisuudelle:
1 != 1 #=> false
2 != 1 #=> true

# "nil" ja "false" ovat ainoat epätodet arvot; kaikki muu ymmärretään todeksi:
!nil   #=> true
!false #=> true
!0     #=> false

# Lisää vertailuoperaatioita:
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Kahdensuuntainen vertailuoperaattori:
1 <=> 10 #=> -1
10 <=> 1 #=> 1
1 <=> 1 #=> 0

# Logiikkaoperaattorit:
true && false #=> false
true || false #=> true
!true #=> false

# Merkkipohjaisten logiikkaoperaattorien vaihtoehtona on sanalliset muodot,
# joilla on hyvin matala presedenssi. Niillä voi muokata ohjelman kulkua
# esimerkiksi väitelausekkeita ketjuttaen.

# Metodia `do_something_else` kutsutaan vain, jos `do_something` onnistuu:
do_something() and do_something_else()
# Metodia `log_error` kutsutaan vain, jos `do_something` epäonnistuu:
do_something() or log_error()

# Merkkijonot ovat olioita:

'Tässä on merkkijono'.class #=> String
"Rajaavat lainausmerkit voivat olla yksin- tai kaksinkertaisia".class #=> String

täyte = 'sisällyttää muita merkkijonoja'
"Kaksinkertaisilla lainausmerkeillä voi #{täyte}"
#=> "Kaksinkertaisilla lainausmerkeillä voi sisällyttää muita merkkijonoja"

# Yksinkertaisia lainausmerkkejä kannattaa silti suosia, sillä kaksinkertaiset
# merkit saattavat aiheuttaa turhia kielensisäisiä tarkistuksia.

# Merkkijonoja voi yhdistellä toisiinsa:
'hello ' + 'world'  #=> "hello world"

# ...mutta luvut vaativat ensin tyyppimuunnoksen:
'hello ' + 3 #=> TypeError: can't convert Fixnum into String
'hello ' + 3.to_s #=> "hello 3"

# Merkkijonoja voi soveltaa laskutoimituksiin... odotettavin seurauksin:
'hello ' * 3 #=> "hello hello hello "

# Merkkijonoa voi jatkaa toisella:
'hello' << ' world' #=> "hello world"

# Tulosteen luonti kera rivinvaihdon:
puts "I'm printing!"
#=> I'm printing!
#=> nil

# ...ja ilman rivinvaihtoa:
print "I'm printing!"
#=> I'm printing! => nil

# Muuttujien määrittely:
x = 25 #=> 25
x #=> 25

# Arvon asettaminen palauttaa arvon itsensä, joten usean muuttujan arvon
# yhtäaikainen määrittely käy vaivatta:
x = y = 10 #=> 10
x #=> 10
y #=> 10

# Muuttujien sanaerottimena käytetään alaviivaa:
snake_case = true

# Lisäksi Rubyssa suositaan ytimekkäitä nimiä:
path_to_project_root = '/good/name/'
path = '/bad/name/'

# Symbolit

# Symbolit ovat muuttumattomia, uudelleenkäytettäviä vakioita.
# Niitä käytetään merkkijonojen sijaan, kun tarkoitus on viitata arvoon,
# jolla on tietty, pysyvä merkitys:

:pending.class #=> Symbol

status = :pending

status == :pending #=> true

status == 'pending' #=> false

status == :approved #=> false

# Taulukot

# Tässä taulukko:
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Taulukko saa koostua erityyppisistä arvoista:
[1, 'hello', false] #=> [1, "hello", false]

# Taulukon alkioihin voi viitata järjestysnumerolla nollasta alkaen:
array[0] #=> 1
array.first #=> 1
array[12] #=> nil

# Kuten laskutoimituksissa nähty syntaksisokeri on myös taulukon alkioiden haku
# pohjimmiltaan vain taulukko-olioon kuuluvan "[]"-metodin kutsu:
array.[] 0 #=> 1
array.[] 12 #=> nil

# Haku käy myös lopustapäin:
array[-1] #=> 5
array.last #=> 5

# Alitaulukon haku käy indeksiparilla...
array[2, 3] #=> [3, 4, 5]

# ...tai määrittelemällä väli:
array[1..3] #=> [2, 3, 4]

# Taulukon voi kääntää:
a=[1,2,3]
a.reverse! #=> [3,2,1]

# Ja sitä voi jatkaa näin...
array << 6 #=> [1, 2, 3, 4, 5, 6]

# ...tai näin:
array.push(6) #=> [1, 2, 3, 4, 5, 6]

# Alkion olemassaolon tarkistus:
array.include?(1) #=> true

# Hashit eli assosiaatiotaulut ovat Rubyn tärkein avain-/arvoparirakenne.
# Hash luodaan aaltosulkeilla:
hash = { 'color' => 'green', 'number' => 5 }

hash.keys #=> ['color', 'number']

# Hash toimii erityisen nopeasti, kun haetaan arvoa avaimen perusteella:
hash['color'] #=> 'green'
hash['number'] #=> 5

# Jos hashistä ei löyty avainta vastaavaa arvoa, palautetaan nil-arvo:
hash['nothing here'] #=> nil

# Symbolihashin määrittelylle on oma syntaksinsa (alkaen Rubyn versiosta 1.9):
new_hash = { defcon: 3, action: true }
new_hash.keys #=> [:defcon, :action]

# Hashin avaimen ja arvon olemassaolon tarkistus:
new_hash.key?(:defcon) #=> true
new_hash.value?(3) #=> true

# Vinkki! Sekä taulukot että hashit sisältävät Enumerable-moduulin,
# johon kuuluu useita hyödyllisiä iterointimetodeja kuten .each, .map,
# .reduce ja .count

# Rakenteita

if true
  'if statement'
elsif false
  'else if, optional'
else
  'else, also optional'
end

for counter in 1..5
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# HUOMAA, että for-rakennetta kannattaa välttää, sillä Rubyssa suosittu
# each-metodi ajaa saman asian idiomaattisemmin. Each-metodi ottaa ainoana
# argumenttinaan lohkon. Lohkot toimivat pitkälti samoin kuin muiden kielten
# anonyymit funktiot, lambdat tai sulkeumat.

# Lukuvälit vastaavat each-metodiin, jolloin sille annettu lohko ajetaan
# kerran jokaiselle välin kokonaisluvulle.
# Lukuvälin each-rakenne lohkoineen näyttää tältä:

(1..5).each do |counter|
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# Lohkoa ympäröivät do/end-avainsanat voi korvata myös aaltosulkeilla:
(1..5).each { |counter| puts "iteration #{counter}" }

# Lukuvälien lisäksi myös tietorakenteita voidaan iteroida each-metodilla:
array.each do |element|
  puts "#{element} is part of the array"
end
hash.each do |key, value|
  puts "#{key} is #{value}"
end

# Taulukoita voi iteroida metodilla each_with_index, jolloin lohko saa
# argumenteikseen sekä alkion että indeksin:
array.each_with_index do |element, index|
  puts "#{element} is number #{index} in the array"
end

counter = 1
while counter <= 5 do
  puts "iteration #{counter}"
  counter += 1
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# Each-metodin lisäksi Rubyssa on useita muita iterointimetodeja kuten
# "map" ja "reduce". Näistä "map" kutsuttuna taulukolla ottaa argumentikseen
# lohkon, suorittaa sen kerran jokaiselle rakenteen jäsenelle, ja lopuksi
# palauttaa uuden taulukon, jonka jäsenet ovat lohkon suorituksen tuloksia.

array = [1, 2, 3, 4, 5]
doubled = array.map do |element|
  element * 2
end
puts doubled
#=> [2,4,6,8,10]
puts array
#=> [1,2,3,4,5]

# Case-rakenne siirtää ohjelman kulun yhdelle monista määritellyistä poluista:

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

# Case-rakenteessa voidaan hyödyntää lukuvälejä:
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

# Virheidenkäsittely:
begin
  # Seuraava koodinpätkä aiheuttaa NoMemoryError-poikkeuksen
  raise NoMemoryError, 'You ran out of memory.'
rescue NoMemoryError => exception_variable
  puts 'NoMemoryError was raised', exception_variable
rescue RuntimeError => other_exception_variable
  puts 'RuntimeError was raised now'
else
  puts 'This runs if no exceptions were thrown at all'
ensure
  puts 'This code always runs no matter what'
end

# Ylimmän näkyvyysalueen metodi näyttää itsenäiseltä funktiolta:
def double(x)
  x * 2
end

# Funktiot (ja lohkot) palauttavat implisiittisesti
# viimeiseksi ajamansa lausekkeen arvon:
double(2) #=> 4

# Metodikutsun argumentteja ympäröivät kaarisulkeet voi jättää pois,
# kunhan koodi ei muutu monitulkintaiseksi:

double 3 #=> 6

double double 3 #=> 12

def sum(x, y)
  x + y
end

# Argumentit erotetaan pilkuilla:

sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12

# Kaikilla metodeilla on implisiittinen lohkoparametri,
# joka voidaan suorittaa yield-avainsanalla:

def surround
  puts '{'
  yield
  puts '}'
end

surround { puts 'hello world' }

# {
# hello world
# }

# Metodille annetun lohkon voi nimetä parametrilistassa &-merkin avulla,
# minkä jälkeen se suoritetaan call-metodilla:
def guests(&block)
  block.call 'some_argument'
end

# Metodille voi antaa vaihtelevan määrän muuttujia. Ne siirretään taulukkoon,
# jolle annetaan parametrilistassa nimi \*-merkin avulla
def guests(*array)
  array.each { |guest| puts guest }
end

# Luokan määritys aloitetaan class-avainsanalla:

class Human

  # Tässä luokkamuuttuja, joka on yhteinen kaikille luokan olioille:
  @@species = 'H. sapiens'

  # Alustusmetodin määrittely:
  def initialize(name, age = 0)
	# name-oliomuuttujan arvon asetus metodille annetun name-muuttujan mukaan:
    @name = name

	# Jos tätä metodia kutsuessa jätetään toinen argumentti (age) antamatta,
	# saa se parametriluettelossa määritetyn arvon 0:
    @age = age
  end

  # Tyypillinen oliomuuttujan arvon asettava metodi:
  def name=(name)
    @name = name
  end

  # Tyypillinen oliomuuttujan arvon palauttava metodi:
  def name
    @name
  end

  # Edelliset kaksi metodia voi ilmaista idiomaattisemmin myös näin:
  attr_accessor :name

  # Lisäksi arvon palauttavan ja asettavan metodin voi määritellä erikseen:
  attr_reader :name
  attr_writer :name

  # Luokkametodeissa käytetään avainsanaa self erotuksena oliometodeista.
  # Luokkametodia voi kutsua vain luokalla itsellään, ei olioilla:
  def self.say(msg)
    puts msg
  end

  def species
    @@species
  end
end

# Olion luonti:

jim = Human.new('Jim Halpert')

dwight = Human.new('Dwight K. Schrute')

# Olion metodien kutsuja:
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# Luokkametodin kutsu:
Human.say('Hi') #=> "Hi"

# Muuttujan näkyvyysalueen voi määritellä etuliitteellä.

# $-alkuiset muuttujat ovat globaaleja:
$var = "I'm a global var"
defined? $var #=> "global-variable"

# @-alkuiset muuttujat kuuluvat oliolle,
# jonka näkyvyysalueella määrittely tehdään:
@var = "I'm an instance var"
defined? @var #=> "instance-variable"

# @@-alkuiset muuttujat kuuluvat vastaavasti näkyvyysalueensa luokalle:
@@var = "I'm a class var"
defined? @@var #=> "class variable"

# Isolla alkukirjaimella nimetyt muuttujat ovatkin vakioita:
Var = "I'm a constant"
defined? Var #=> "constant"

# Kuten odottaa saattaa, myös luokat itsessään ovat olioita.
# Siksi niille voi määritellä muuttujia, jotka ovat yhteisiä kaikille
# luokan ilmentymille ja perillisille.

# Tavallisen luokan määrittely:

class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

# Perillisluokan määrittely:

class Worker < Human
end

Human.foo # 0
Worker.foo # 0

Human.foo = 2 # 2
Worker.foo # 2

# Oliomuuttuja on kuitenkin olion oma eikä periydy:

class Human
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(value)
    @bar = value
  end
end

class Doctor < Human
end

Human.bar # 0
Doctor.bar # nil

module ModuleExample
  def foo
    'foo'
  end
end

# Moduulien lisääminen luokkaan "include"-avainsanalla siirtää moduulin metodit
# luokan ilmentymille, kun taas "extend" avainsana siirtää metodit
# luokalle itselleen:

class Person
  include ModuleExample
end

class Book
  extend ModuleExample
end

Person.foo     # => NoMethodError: undefined method `foo' for Person:Class
Person.new.foo # => 'foo'
Book.foo       # => 'foo'
Book.new.foo   # => NoMethodError: undefined method `foo'

# Callback-tyyppiset metodit suoritetaan moduulia sisällyttäessä:

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

Something.bar     # => 'bar'
Something.qux     # => NoMethodError: undefined method `qux'
Something.new.bar # => NoMethodError: undefined method `bar'
Something.new.qux # => 'qux'
```

## Lisämateriaalia englanniksi

- [Learn Ruby by Example with Challenges](http://www.learneroo.com/modules/61/nodes/338) - Selaimessa tehtäviä harjoituksia tämän dokumentin hengessä
- [An Interactive Tutorial for Ruby](https://rubymonk.com/)
- [Official Documentation](http://www.ruby-doc.org/core-2.1.1/) - Virallinen dokumentaatio
- [Ruby from other languages](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - Vanhempi, mutta [ilmainen painos](http://ruby-doc.com/docs/ProgrammingRuby/) on luettavissa netissä
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - Yhteisön luoma Ruby-tyyliopas
- [Try Ruby](http://tryruby.org) - Rubyn perusteet interaktiivisesti
