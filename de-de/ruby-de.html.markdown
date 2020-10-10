---
language: ruby
filename: ruby-de.rb
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
  - ["Jannik Siebert", "https://github.com/janniks"]
  - ["Keith Miyake", "https://github.com/kaymmm"]
translators:
  - ["Christian Albrecht", "https://github.com/coastalchief"]
  - ["Dennis Keller", "https://github.com/denniskeller"]
  - ["Paul Götze", "https://gitub.com/paulgoetze"]
lang: de-de
---

```ruby
# Das ist ein Kommentar

=begin
Das ist ein mehrzeiliger Kommentar.
Die Anfangszeile muss mit "=begin" beginnen
und die Endzeile muss mit "=end" beginnen. 

Alternativ kannst du jede Zeile in einem 
mehrzeiligen Kommentar mit dem # Zeichen beginnen. 
=end

# In Ruby ist (fast) alles ein Objekt.
# Das schließt Zahlen ein...
3.class #=> Integer

# ...und Zeichenketten (Strings)...
"Hallo".class #=> String

# ...und sogar Methoden!
"Hallo".method(:class).class #=> Method

# Simple Arithmetik
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2 ** 5 #=> 32
5 % 3 #=> 2

# Bitweise Operatoren
3 & 5 #=> 1
3 | 5 #=> 7
3 ^ 5 #=> 6

# Arithmetik ist aber eigentlich nur syntaktischer Zucker
# um eine Methode eines Objekts aufzurufen
1.+(3) #=> 4
10.* 5 #=> 50
100.methods.include?(:/) #=> true

## Spezielle Werte sind Objekte
nil # Equivalent zu null in anderen Sprachen
true # Wahrheitswert
false # Falschheitswert

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Gleicheit
1 == 1 #=> true
2 == 1 #=> false

# Ungleichheit
1 != 1 #=> false
2 != 1 #=> true

# Neben false selbst, ist nil der einzige andere 
# zu Falsch evaluierende Wert

!!nil   #=> false
!!false #=> false
!!0     #=> true
!!""    #=> true

# Weitere Vergleiche
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Kombinierter Vergleichsoperator (gibt `1` zurück wenn das erste Argument 
# größer ist, und `-1`, wenn das zweite Argument größer ist, sonst `0`)
1 <=> 10 #=> -1 (1 < 10)
10 <=> 1 #=> 1 (10 > 1)
1 <=> 1 #=> 0 (1 == 1)

### Logische Operatoren
true && false #=> false
true || false #=> true

# Es gibt alternative Versionen der logischen Operatoren mit niedrigerer
# Wertigkeit. Diese werden meistens zur Flusskontrolle eingesetzt, um
# verschiedenen Ausdrücke zu verketten bis einer true oder false zurück
# liefert.

# `do_something_else` wird nur ausgewertet wenn `do_something` true ist.
do_something() and do_something_else()
# `log_error` wird nur ausgewertet wenn `do_something` false ist.
do_something() or log_error()

# String Interpolation

placeholder = 'Ruby'
"Ich kann in #{placeholder} Platzhalter mit doppelten Anführungszeichen füllen."
#=> "Ich kann in Ruby Platzhalter mit doppelten Anführungszeichen füllen."

# Du kannst Strings mit `+` verbinden, nicht jedoch mit anderen Typen
'hallo ' + 'Welt'  #=> "hallo Welt"
'Hallo ' + 3 #=> TypeError: no implicit conversion of Integer into String
'hallo ' + 3.to_s #=> "hallo 3"
"hallo #{3}" #=> "hallo 3"

# ...oder Strings mit Operatoren kombinieren
'hallo ' * 3 #=> "hallo hallo hallo "

# ...oder Strings an andere Strings anhängen
'hallo' << ' Welt' #=> "hallo Welt"

# Du kannst Text mit einer neuen Zeile am Ende ausgeben
puts "Ich gebe Text aus!"
#=> Ich gebe Text aus!
#=> nil

# ...oder Text ohne einen Zeilenumbruch ausgeben
print "Ich gebe Text aus!"
#=> "Ich gebe Text aus!" => nil

# Variablen
x = 25 #=> 25
x #=> 25

# Beachte, dass Zuweisungen den zugewiesenen Wert zurückgeben.
# D.h. du kannst mehrfache Zuweisungen machen.

x = y = 10 #=> 10
x #=> 10
y #=> 10

# Nutze snake_case für Variablennamen.
snake_case = true

# Nutze verständliche Variablennamen.
path_to_project_root = '/guter/Name/'
m = '/schlechter/Name/'


# Symbole sind unveränderliche, wiederverwendbare Konstanten, welche intern
# als Integer repräsentiert werden. Sie werden häufig anstelle von Strings
# verwendet, um semantisch sinnvoll Werte zu übermitteln.
# Symbols werden mit dem Doppelpunkt gekennzeichnet.

:pending.class #=> Symbol

status = :pending

status == :pending #=> true

status == 'pending' #=> false

status == :approved #=> false

# Strings können in Symbole konvertiert werden und umgekehrt.
status.to_s #=> "pending"
"argon".to_sym #=> :argon

# Arrays

# Das ist ein Array.
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Array können verschiedene Typen beinhalten
[1, 'hello', false] #=> [1, "hello", false]

## Arrays könnenindiziert werden.

# Von vorne...
array[0] #=> 1
array.first #=> 1
array[12] #=> nil

# ...oder von hinten...
array[-1] #=> 5
array.last #=> 5

# ...oder mit einem Startindex und einer Länge...
array[2, 3] #=> [3, 4, 5]

# ...oder mit einem Range...
array[1..3] #=> [2, 3, 4]

# Du kanns ein Array umkehren.
# Gib ein neues Array mit umgkehrten Werten zurück
[1,2,3].reverse #=> [3,2,1]

# Kehre ein Array an Ort und Stelle um, um die Variable mit den 
# umgekehrten Werten zu aktualisieren.
a = [1,2,3]
a.reverse! #=> a==[3,2,1] wegen des Aufrufs von reverse mit Ausrufezeichens ('!')

# Wie bei der Arithmetik, ist Zugriff mit [index] nur 
# syntaktischer Zucker für den Aufruf der `[]` Methode auf dem Objekt.
array.[] 0 #=> 1
array.[] 12 #=> nil

# Du kannst Werte zu einem Array hinzufügen...
array << 6 #=> [1, 2, 3, 4, 5, 6]
# Oder so
array.push(6) #=> [1, 2, 3, 4, 5, 6]

# ...und testen ob ein Element schon vorhanden ist
array.include?(1) #=> true

# Hashes sind Rubys Hauptdatenstruktur for Schlüssel/Wert Paare.
# Hashes werden durch geschweifte Klammern gekennzeichnet.
hash = { 'Farbe' => 'grün', 'Nummer' => 5 }

hash.keys #=> ['farbe', 'nummer']

# Hashes can be quickly looked up by key.
hash['Farbe'] #=> "grün"
hash['Nummer'] #=> 5

# Abfragen eines nicht vorhandenen Schlüssels, gibt nil zurück. 
hash['nicht vorhanden'] #=> nil

# Wenn du Symbole als Schlüssel in einem Hash verwendest, kannst du
# eine alternative Syntax verwenden.
hash = { :defcon => 3, :action => true }
hash.keys #=> [:defcon, :action]

hash = { defcon: 3, action: true }
hash.keys #=> [:defcon, :action]

# Testen ob ein Schlüssel oder Wert im Hash existiert
hash.key?(:defcon) #=> true
hash.value?(3) #=> true

# Tipp: Arrays und Hashes sind Enumerables!
# Sie haben viele nützliche Methoden gemein, wie each, map, count, und andere.

# Kontrolstrukturen

# Bedingungen
if true
  'wenn Bedingung'
elsif false
  'sonst wenn, optional'
else
  'sonst, auch optional'
end

# Wenn eine Kontrollstruktur keinen Code-Block, sondern einen einzigen 
# Ausdruck ausführt, dann kannst du die nachgestellte if-Notation verwenden
warnings = ['Nachname fehlt', 'Adresse zu kurz']
puts("Vorhandene Warnungen:\n" + warnings.join("\n"))  if !warnings.empty?

# Formuliere die Bedingung um, wenn sich `unless` besser liest als `if`
puts("Vorhandene Warnungen:\n" + warnings.join("\n")) unless warnings.empty?

# Schleifen
# Traditionell ist das Benutzen von `for` Schleifen in Ruby eher unüblich.
# Stattdessen werden diese mit Hilfe von Enumerables implementiert, was mit
# dem Aufrufen von `each` einhergeht.
(1..5).each do |counter|
  puts "Iteration #{counter}"
end

# Was in etwa das selbe ist wie Folgendes (selten in Ruby zu sehen).
for counter in 1..5
  puts "Iteration #{counter}"
end

# Das `do |variable| ... end` Konstrukt wird `block` genannt. 
# Blocks sind vergleichbar mit Lambdas, anonymen Funktionen 
# oder Closures in anderen Programmiersprachen.
# Sie können als Objekte übergeben, aufgerufen oder als Methoden 
# zugewiesen werden.

# Die `each` Methode eines Ranges führt den Block einmal für jedes 
# Element des Ranges aus. 
# Dem Block wird eine counter Variable als Parameter übergeben.

# Du kannst einen Block auch mit geschweiften Klammern schreiben.
(1..5).each { |counter| puts "Iteration #{counter}" }

# Each kann auch über den Inhalt von Datenstrukturen iterieren.
array.each do |element|
  puts "#{element} is Teil des Arrays"
end

hash.each do |key, value|
  puts "#{key} ist #{value}"
end

# Um auf den Laufindex zuzugreifen kannst du `each_with_index` verwenden
# und eine index Variable definieren. 
array.each_with_index do |element, index|
  puts "#{element} ist Nummer #{index} im Array"
end

counter = 1
while counter <= 5 do
  puts "Iteration #{counter}"
  counter += 1
end
#=> Iteration 1
#=> Iteration 2
#=> Iteration 3
#=> Iteration 4
#=> Iteration 5

# Es gibt einige andere hilfreiche Schleifenfunktionen in Ruby.
# Wie etwa 'map', 'reduce', 'inject' und viele andere mehr. 
# Map zum Beispiel iteriert über das Array, führt für jedes Element 
# die Anweisungen aus, 
# die im Block definiert sind und gibt ein völlig neues Array zurück.
array = [1,2,3,4,5]
doubled = array.map do |element|
  element * 2
end
puts doubled
#=> [2,4,6,8,10]
puts array
#=> [1,2,3,4,5]

# Case Konstruct
grade = 'B'

case grade
when 'A'
  puts 'So wird’s gemacht'
when 'B'
  puts 'Viel Glück beim nächsten Mal'
when 'C'
  puts 'Das kannst du besser'
when 'D'
  puts 'Gerade so durch'
when 'F'
  puts 'Durchgefallen!'
else
  puts 'Anderes Bewertungssystem, was?'
end
#=> "Viel Glück beim nächsten Mal"

# Case kann auch Ranges benutzen
grade = 82
case grade
when 90..100
  puts 'Hurra!'
when 80...90
  puts 'OK gemacht'
else
  puts 'Durchgefallen!'
end
#=> "OK gemacht"

# Fehlerbehandlung
begin
  # Code der einen Fehler wirft...
  raise NoMemoryError, 'Dein Speicher ist voll.'
rescue NoMemoryError => exception_variable
  puts 'NoMemoryError ist aufgetreten', exception_variable
rescue RuntimeError => other_exception_variable
  puts 'RuntimeError ist aufgetreten'
else
  puts 'Das wird ausgeführt, wenn keine Fehler geworfen wurden'
ensure
  puts 'Dieser Code wird immer ausgeführt, egal was vorher passiert'
end

# Methoden

def double(x)
  x * 2
end

# Methoden (und Blocks) geben implizit den Wert des letzten Anweisung zurück.
double(2) #=> 4

# Klammern sind optional wenn die Anweisung dadurch nicht mehrdeutig wird.
double 3 #=> 6

double double 3 #=> 12

def sum(x, y)
  x + y
end

# Die Argumente einer Methode werden durch ein Komma getrennt.
sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12

# yield
# Alle Methoden haben implizit einen optionalen block Parameter.
# Dieser kann durch das Schlüsselwort 'yield' ausgeführt werden.
def surround
  puts '{'
  yield
  puts '}'
end

surround { puts 'hallo Welt' }

#=> {
#=> hallo Welt
#=> }

# Blocks können in ein 'Proc' Objekt umgewandelt werden. 
# Dieses ist eine Art Container um den Block und erlaubt ihn an eine 
# andere Methode zu übergeben, ihn in einen anderen Gültigkeitsbereicht 
# einzubinden oder ihn andersweitig zu verändern.
# Am häufigsten findet man dies bei Parameterlisten von Methoden, in Form 
# eines letzten '&block' Parameters, der den Block – wenn es einen gibt – 
# entgegen nimmt und ihn in ein 'Proc' umwandelt. Die Benennung '&block' ist 
# hier nur eine Konvention; es würde genauso mit '&pineapple' funktionieren. 
def guests(&block)
  block.class #=> Proc
  block.call(4)
end

# Die 'call' Methode eines Proc ist ganz ähnlich zum Aufruf von 'yield', wenn 
# ein Block vorhanden ist. Die Argumente, die 'call' übergeben werden, werden 
# als Argumente and den Block weitergereicht.

guests { |n| "Du hast #{n} Gäste." }
# => "Du hast 4 Gäste."

# Du kannst eine Liste von Argumenten übergeben, die dann in ein Array 
# umgewandelt werden. Dafür gibt es den splat-Operator (`*`).
def guests(*array)
  array.each { |guest| puts guest }
end

# Destrukturierung

# Ruby destrukturiert Arrays automatisch beim Zuweisen mehrerer Variablen.
a, b, c = [1, 2, 3]
a #=> 1
b #=> 2
c #=> 3

# In manchen Fällen will man den splat-Operator (`*`) verwenden um ein Array in
# eine Liste zu destrukturieren.
ranked_competitors = ["John", "Sally", "Dingus", "Moe", "Marcy"]

def best(first, second, third)
  puts "Gewinner sind #{first}, #{second} und #{third}."
end

best *ranked_competitors.first(3) #=> Gewinner sind John, Sally and Dingus.

# Der splat-Operator kann auch in Parametern verwendet werden.
def best(first, second, third, *others)
  puts "Gewinner sind #{first}, #{second} und #{third}."
  puts "Es gab #{others.count} andere Teilnehmer."
end

best *ranked_competitors 
#=> Gewinner sind John, Sally und Dingus.
#=> Es gab 2 andere Teilnehmer.

# Per Konvention enden alle Methoden, die einen Wahrheitswert zurück geben, mit einem
# Fragezeichen.
5.even? #=> false
5.odd? #=> true

# Wenn ein Methodenname mit einem Ausrufezeichen endet, dann tut diese Methode 
# per Konvention etwas Destruktives, wie z.B. das aufrufende Objekt zu 
# verändern.
# Viele Mehtoden haben eine !-Version um eine direkte Änderung zu machen und 
# eine Nicht-!-Version, die ein neues Objekt mit den Veränderungen zurück gibt.
company_name = "Dunder Mifflin"
company_name.upcase #=> "DUNDER MIFFLIN"
company_name #=> "Dunder Mifflin"
# Diesmal verändern wir company_name direkt.
company_name.upcase! #=> "DUNDER MIFFLIN"
company_name #=> "DUNDER MIFFLIN"

# Klassen

# Du kannst eine Klasse mit dem Schlüsselwort 'class' definieren.
class Human

  # Eine Klassenvariable. Sie wird von allen Instanzen einer Klasse geteilt.
  @@species = 'H. sapiens'

  # Konstruktor bzw. Initializer
  def initialize(name, age = 0)
    # Weise das Argument der Instanzvariable 'name' zu.
    @name = name
    # Wenn kein 'age' angegeben wurde wird der Standartwert aus der Argumentenlist verwendet.
    @age = age
  end

  # Setter Methode
  def name=(name)
    @name = name
  end

  # Getter Methode
  def name
    @name
  end

  # Getter & Setter können auch kürzer mit der attr_accessor Methode erstellt werden.
  attr_accessor :name

  # Getter & Setter Methoden können auch einzeln erstellt werden.
  attr_reader :name
  attr_writer :name

  # Eine Klassenmethode unterscheidet sich durch ein 'self' von einer 
  # Instanzmethode.
  # Sie kann nur auf der Klasse und nicht auf einer Instanz der Klasse 
  # aufgerufen werden.  
  def self.say(msg)
    puts msg
  end

  def species
    @@species
  end
end

# Instanziieren einer Klasse
jim = Human.new('Jim Halpert')
dwight = Human.new('Dwight K. Schrute')

# Du kannst die Methoden des erstellten Objekts aufrufen.
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# Aufrufen einer Klassenmethode
Human.say('Hi') #=> "Hi"

# Der Gültigkeitsbereich einer Variablen wird durch ihren Namen definiert.
# Variablen, die mit $ beginnen sind global gültig.
$var = "Ich bin eine globale Variable"
defined? $var #=> "global-variable"

# Variablen, die mit @ beginnen, sind innerhalb einer Instanz gültig.
@var = "Ich bin eine Instanzvariable"
defined? @var #=> "instance-variable"

# Variablen, die mit @@ beginnen, sind für die Klasse gültig.
@@var = "Ich bin eine Klassenvariable"
defined? @@var #=> "class variable"

# Variablen, die mit einem Großbuchstaben beginnen, sind Konstanten
Var = "Ich bin eine Konstante"
defined? Var #=> "constant"

# Class ist in Ruby auch ein Objekt. Deshalb kann eine Klasse Instanzvariablen
# haben. Eine Klassenvariable wird zwischen der Klasse und all ihren 
# Ableitungen geteilt.

# Basis Klasse
class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

# Abgeleitete Klasse
class Worker < Human
end

Human.foo #=> 0
Worker.foo #=> 0

Human.foo = 2
Worker.foo #=> 2

# Ableitungen einer Klasse haben keinen Zugriff auf eine Eine Klassen-Instanzvariable.
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

Human.bar #=> 0
Doctor.bar #=> nil

module ModuleExample
  def foo
    'foo'
  end
end

# Ein Einbinden (include) eines Moduls bindet seine Methoden an die Instanzen 
# der Klasse.
# Ein Erweitern (extend) eines Moduls bindet seine Methoden an die Klasse 
# selbst.
class Person
  include ModuleExample
end

class Book
  extend ModuleExample
end

Person.foo     #=> NoMethodError: undefined method `foo' for Person:Class
Person.new.foo #=> "foo"
Book.foo       #=> "foo"
Book.new.foo   #=> NoMethodError: undefined method `foo'


# Callbacks werden ausgeführt, wenn ein Modul eingebunden oder erweitert wird.
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

Something.bar     #=> "bar"
Something.qux     #=> NoMethodError: undefined method `qux'
Something.new.bar #=> NoMethodError: undefined method `bar'
Something.new.qux #=> "qux"
```

## Weitere Links

_(z.T. auf Englisch)_

- [Offizielle Ruby Website](https://www.ruby-lang.org/de/)
- [Learn Ruby by Example with Challenges](http://www.learneroo.com/modules/61/nodes/338) - Eine Variante dieses Dokuments mit in-Browser Challenges.
- [RubyMonk](https://rubymonk.com/) - Lerne Ruby mit einer Reihe interaktiver Tutorials.
- [Offizielle Dokumentation](http://ruby-doc.org/core)
- [Ruby from other languages](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - Eine ältere [freie Ausgabe](http://ruby-doc.com/docs/ProgrammingRuby/) ist online verfügbar.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - Ein von der Community erstellter Ruby coding style guide.
- [Try Ruby](http://tryruby.org) - Lerne die Grundlagen der Ruby Programmiersprache, interaktiv im Browser.
