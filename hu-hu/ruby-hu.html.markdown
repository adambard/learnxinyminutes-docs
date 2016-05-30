---
language: ruby
lang: hu-hu
filename: learnruby-hu.rb
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
translators:
  - ["Zsolt Prontvai", "https://github.com/prozsolt"]
---

```ruby
# Ez egy komment

=begin
Ez egy többsoros komment
Senki sem használja
Neked sem kellene
=end

# Először is: Minden objektum

# A számok objektumok

3.class #=> Fixnum

3.to_s #=> "3"


# Néhány alapvető számtani művelet
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2**5 #=> 32

# A számtani művelet csak szintaktikus cukor
# az objektumon történő függvény hívásra
1.+(3) #=> 4
10.* 5 #=> 50

# A speciális értékek objektumok
nil # Nincs itt semmi látnivaló
true # igaz
false # hamis

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Egyenlőség
1 == 1 #=> true
2 == 1 #=> false

# Egyenlőtlenség
1 != 1 #=> false
2 != 1 #=> true

# A false-on kívül, nil az egyetlen hamis érték

!nil   #=> true
!false #=> true
!0     #=> false

# Még több összehasonlítás
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Logikai operátorok
true && false #=> false
true || false #=> true
!true #=> false

# A logikai operátoroknak alternatív verziójuk is van sokkal kisebb
# precedenciával. Ezeket arra szánták, hogy több állítást összeláncoljanak
# amíg egyikük igaz vagy hamis értékkel nem tér vissza.

# `csinalj_valami_mast` csak akkor fut le, ha `csinalj_valamit` igaz értékkel
# tért vissza.
csinalj_valamit() and csinalj_valami_mast()
# `log_error` csak akkor fut le, ha `csinalj_valamit` hamis értékkel
# tért vissza.
csinalj_valamit() or log_error()


# A sztringek objektumok

'Én egy sztring vagyok'.class #=> String
"Én is egy sztring vagyok".class #=> String

helykitolto = 'interpolációt használhatok'
"Sztring #{helykitolto}, ha dupla időzőjelben van a sztringem"
#=> "Sztring interpolációt használhatok, ha dupla időzőjelben van a sztringem"

# A szimpla idézőjelet preferáljuk, ahol csak lehet,
# mert a dupla idézőjel extra számításokat végez.

# Kombinálhatunk sztringeket, de nem számokkal
'hello ' + 'world'  #=> "hello world"
'hello ' + 3 #=> TypeError: can't convert Fixnum into String
'hello ' + 3.to_s #=> "hello 3"

# kiírás a kimenetre
puts "Írok"

# Változók
x = 25 #=> 25
x #=> 25

# Értékadás az adott értékkel tér vissza
# Ez azt jelenti, hogy használhatunk többszörös értékadást:

x = y = 10 #=> 10
x #=> 10
y #=> 10

# Konvencióból, snake_case változó neveket használj
snake_case = true

# Leíró változó neveket használj
ut_a_projekt_gyokerehez = '/jo/nev/'
ut = '/rossz/nev/'

# A szimbólumok (objektumok)
# A szimbólumok megváltoztathatatlan, újra felhasználható konstans,
# mely belsőleg egész számként reprezentált. Sokszor sztring helyett használják,
# hogy effektíven közvetítsünk konkrét, értelmes értékeket

:fuggoben.class #=> Symbol

statusz = :fuggoben

statusz == :fuggoben #=> true

statusz == 'fuggoben' #=> false

statusz == :jovahagyott #=> false

# Tömbök

# Ez egy tömb
tomb = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# A tömmbök különböző tipusú dolgokat tartalmazhat

[1, 'hello', false] #=> [1, "hello", false]

# Tömbök indexelhetőek
# Az elejéről
tomb[0] #=> 1
tomb[12] #=> nil

# Akárcsak a számtani műveletek [var] hozzáférés
# is csak szintaktikus cukor
# a [] függvény hívására az objektumon
tomb.[] 0 #=> 1
tomb.[] 12 #=> nil

# A végéről
tomb[-1] #=> 5

# Kezdőértékkel és hosszal
tomb[2, 3] #=> [3, 4, 5]

# Tömb megfordítása
a=[1,2,3]
a.reverse! #=> [3,2,1]

# Vagy tartománnyal
tomb[1..3] #=> [2, 3, 4]

# Így adhatunk a tömbhöz
tomb << 6 #=> [1, 2, 3, 4, 5, 6]
# Vagy így
tomb.push(6) #=> [1, 2, 3, 4, 5, 6]

# Ellenőrízük, hogy a tömb tartalmaz egy elemet
tomb.include?(1) #=> true

# Hash-ek a ruby elsődleges szótárjai kulcs/érték párokkal
# Hash-eket kapcsos zárójellel jelöljük
hash = { 'szin' => 'zold', 'szam' => 5 }

hash.keys #=> ['szin', 'szam']

# Hash-ekben könnyen kreshetünk a kulcs segítségével:
hash['szin'] #=> 'zold'
hash['szam'] #=> 5

# Nem létező kulcsra keresve nil-t kapunk:
hash['nincs itt semmi'] #=> nil

# Ruby 1.9-től, egy külnleges szintaxist is használhatunk a szimbólumot
# használunk kulcsnak

uj_hash = { defcon: 3, action: true }

uj_hash.keys #=> [:defcon, :action]

# Ellenőrizzük, hogy az adott kulcs és érték bene-e van a hash-ben
uj_hash.has_key?(:defcon) #=> true
uj_hash.has_value?(3) #=> true

# Tip: A tömbök és hash-ek is felsorolhatóak
# Sok közös függvényük van, akár az each, map, count, és több

# Kontroll Struktúrák

if true
  'ha állítás'
elsif false
  'különben ha, opcionális'
else
  'különben, szintén opcionális'
end

for szamlalo in 1..5
  puts "iteracio #{szamlalo}"
end
#=> iteracio 1
#=> iteracio 2
#=> iteracio 3
#=> iteracio 4
#=> iteracio 5

# HOWEVER, No-one uses for loops.
# Instead you should use the "each" method and pass it a block.
# A block is a bunch of code that you can pass to a method like "each".
# It is analogous to lambdas, anonymous functions or closures in other
# programming languages.
#
# The "each" method of a range runs the block once for each element of the range.
# The block is passed a counter as a parameter.
# Calling the "each" method with a block looks like this:

(1..5).each do |counter|
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# You can also surround blocks in curly brackets:
(1..5).each { |counter| puts "iteration #{counter}" }

# The contents of data structures can also be iterated using each.
array.each do |element|
  puts "#{element} is part of the array"
end
hash.each do |key, value|
  puts "#{key} is #{value}"
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

jegy = '4'

case jegy
when '5'
  puts 'Kitünő'
when '4'
  puts 'Jó'
when '3'
  puts 'Közepes'
when '2'
  puts 'Elégsége'
when '1'
  puts 'Elégtelen'
else
  puts 'Alternatív értékelés, hm?'
end
#=> "Jó"

# case-ek tartományokat is használhatnak
jegy = 82
case jegy
when 90..100
  puts 'Hurrá!'
when 80...90
  puts 'Jó munka'
else
  puts 'Megbuktál!'
end
#=> "Jó munka"

# kivétel kezelés:
begin
  # kód ami kivételt dobhat
  raise NoMemoryError, 'Megtelt a memória'
rescue NoMemoryError => kivetel_valtozo
  puts 'NoMemoryError-t dobott', kivetel_valtozo
rescue RuntimeError => mas_kivetel_valtozo
  puts 'RuntimeError dobott most'
else
  puts 'Ez akkor fut ha nem dob kivételt'
ensure
  puts 'Ez a kód mindenképpen lefut'
end

# Függvények

def ketszeres(x)
  x * 2
end

# Függvények (és egyébb blokkok) implicit viszatértnek az utolsó értékkel
ketszeres(2) #=> 4

# Zárójelezés opcionális, ha az eredmény félreérthetetlen
ketszeres 3 #=> 6

ketszeres ketszeres 3 #=> 12

def osszeg(x, y)
  x + y
end

# Függvény argumentumait vesszővel választjuk el.
osszeg 3, 4 #=> 7

osszeg osszeg(3, 4), 5 #=> 12

# yield
# Minden függvénynek van egy implicit, opcionális block paramétere
# 'yield' kulcsszóval hívhatjuk

def korulvesz
  puts '{'
  yield
  puts '}'
end

korulvesz { puts 'hello world' }

# {
# hello world
# }


# Fuggvénynek átadhatunk blokkot
# "&" jelöli az átadott blokk referenciáját
def vendegek(&block)
  block.call 'valami_argumentum'
end

# Argumentum lisát is átadhatunk, ami tömbé lesz konvertálva
# Erre való a splat operátor ("*")
def vendegek(*array)
  array.each { |vendeg| puts vendeg }
end

# Osztályt a class kulcsszóval definiálhatunk
class Ember

  # Az osztály változó. Az osztály minden példánnyával megvan osztva
  @@faj = 'H. sapiens'

  # Alap inicializáló
  def initialize(nev, kor = 0)
    # Hozzárendeli az argumentumot a "nev" példány változóhoz
    @nev = nev
    # Ha nem adtunk meg kort akkor az alapértemezet értéket fogja használni
    @kor = kor
  end

  # Alap setter függvény
  def nev=(nev)
    @nev = nev
  end

  # Alap getter függvény
  def nev
    @nev
  end

  # A fönti funkcionalítást az attr_accessor függvénnyel is elérhetjük
  attr_accessor :nev

  # Getter/setter függvények egyenként is kreálhatóak
  attr_reader :nev
  attr_writer :nev

  # Az osztály függvények "self"-et hasznalnak, hogy megkülönböztessék magukat a
  # példány függvényektől
  # Az osztályn hívhatóak, nem a példányon
  def self.mond(uzenet)
    puts uzenet
  end

  def faj
    @@faj
  end
end


# Példányosítsuk az osztályt
jim = Ember.new('Jim Halpert')

dwight = Ember.new('Dwight K. Schrute')

# Hívjunk meg pár függvényt
jim.faj #=> "H. sapiens"
jim.nev #=> "Jim Halpert"
jim.nev = "Jim Halpert II" #=> "Jim Halpert II"
jim.nev #=> "Jim Halpert II"
dwight.faj #=> "H. sapiens"
dwight.nev #=> "Dwight K. Schrute"

# Hívjuk meg az osztály függvényt
Ember.mond('Hi') #=> "Hi"

# Változók szókjait az elnevezésük definiálja
# $ kezdetű változók globálisak
$var = "Én egy globális változó vagyok"
defined? $var #=> "global-variable"

# Változók amik @-al kezdődnek példány szkópjuk van
@var = "Én egy példány változó vagyok"
defined? @var #=> "instance-variable"

# Változók amik @@-al kezdődnek példány szkópjuk van
@@var = "Én egy osztály változó vagyok"
defined? @@var #=> "class variable"

# Változók amik nagy betűvel kezdődnek a konstansok
Var = "Konstans vagyok"
defined? Var #=> "constant"

# Az osztály is objetum. Tehát az osztálynak lehet példány változója
# Az osztályváltozón osztozik minden pédány és leszármazott

# Ős osztály
class Ember
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(ertek)
    @@foo = ertek
  end
end

# Leszarmazott osztály
class Dolgozo < Ember
end

Ember.foo # 0
Dolgozo.foo # 0

Ember.foo = 2 # 2
Dolgozo.foo # 2

# Az osztálynak példány változóját nem látja az osztály leszármazottja.

class Ember
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(ertek)
    @bar = ertek
  end
end

class Doctor < Ember
end

Ember.bar # 0
Doctor.bar # nil

module ModulePelda
  def foo
    'foo'
  end
end

# Modulok include-olása a fügvényeiket az osztály példányaihoz köti.
# Modulok extend-elésa a fügvényeiket magához az osztályhoz köti.

class Szemely
  include ModulePelda
end

class Konyv
  extend ModulePelda
end

Szemely.foo     # => NoMethodError: undefined method `foo' for Szemely:Class
Szemely.new.foo # => 'foo'
Konyv.foo       # => 'foo'
Konyv.new.foo   # => NoMethodError: undefined method `foo'

# Callback-ek végrehajtódnak amikor include-olunk és extend-elünk egy modult

module ConcernPelda
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

class Valami
  include ConcernPelda
end

Valami.bar     # => 'bar'
Valami.qux     # => NoMethodError: undefined method `qux'
Valami.new.bar # => NoMethodError: undefined method `bar'
Valami.new.qux # => 'qux'
```

## Egyéb források

- [Learn Ruby by Example with Challenges](http://www.learneroo.com/modules/61/nodes/338)
- [Official Documentation](http://www.ruby-doc.org/core-2.1.1/)
- [Ruby from other languages](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - A régebbi [ingyenes változat](http://ruby-doc.com/docs/ProgrammingRuby/) elérhető online.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide)
