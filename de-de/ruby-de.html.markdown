---
language: ruby
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
  - ["Christian Albrecht", "https://github.com/coastalchief"]
  - ["Dennis Keller", "https://github.com/denniskeller"]
filename: ruby-de.rb
lang: de-de
---

# Dies ist ein Kommentar

=begin
Dies sind multi-line
Kommentare. Niemand benutzt
die wirklich.
=end

# Objekte - Alles ist ein Objekt

## Zahlen sind Objekte
```
3.class #=> Fixnum
3.to_s #=> "3"
```

### Simple Arithmetik
```
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2**5 #=> 32
```

// Arithmetik ist aber eigentlich nur syntaktischer Zucker
// um eine Methode eines Objekt aufzurufen
```
1.+(3) #=> 4
10.* 5 #=> 50
```

## Special values sind Objekte
```
nil # Nothing to see here
true # truth
false # falsehood

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass
```

## Objektvergleiche
### Gleicheit
```
1 == 1 #=> true
2 == 1 #=> false
```
### Ungleichheit
```
1 != 1 #=> false
2 != 1 #=> true
```
### Neben false selbst, nil ist ein anderer 'falsey' Wert
```
!nil   #=> true
!false #=> true
!0     #=> false
```
### Weitere Vergleiche
```
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true
```
### Logische Operatoren
```
true && false #=> false
true || false #=> true
!true #=> false
```

Es gibt alternative Versionen der logischen Operatoren mit niedrigerer
Wertigkeit. Diese werden meistens bei Flow-Control eingesetzt, um
verschiedenen Ausdrücke zu verketten bis einer true oder false zurück
liefert.

#### and
##### `do_something_else` wird nur ausgewertet wenn `do_something` true ist.
do_something() and do_something_else()

#### or
#####`log_error` wird nur ausgewertet wenn `do_something` false ist.
do_something() or log_error()

## Strings sind Objekte
```
'I am a string'.class #=> String
"I am a string too".class #=> String


platzhalter = 'Ruby'
"Ich kann in #{placeholder} Platzhalter mit doppelten Anführungsstrichen füllen."
```
Einfache Anführungszeichen sollten bevorzugt werden.
Doppelte Anführungszeichen führen interne Berechnungen durch.

### Strings können verbunden werden, aber nicht mit Zahlen
```
'hello ' + 'world'  #=> "hello world"
'hello ' + 3 #=> TypeError: can't convert Fixnum into String
```
#### Zahl muss in String konvertiert werden
```
'hello ' + 3.to_s #=> "hello 3"
```
### Text ausgeben
```
puts "I'm printing!"
```
# Variablen
## Zuweisungen
### Diese Zuweisung gibt den zugeordneten Wert zurück
```
x = 25 #=> 25
x #=> 25
```
### Damit funktionieren auch mehrfache Zuweisungen
```
x = y = 10 #=> 10
x #=> 10
y #=> 10
```
## Benennung
### Konvention ist snake_case
```
snake_case = true
```
### Benutze verständliche Variablennamen
```
path_to_project_root = '/good/name/'
path = '/bad/name/'
```
# Symbols (sind auch Objekte)
Symbols sind unveränderliche, wiederverwendbare Konstanten, welche intern
als integer repräsentiert werden. Sie werden häufig anstelle von Strings
verwendet, um sinnvoll Werte zu übermitteln.
Symbols werden mit dem Doppelpunkt gekennzeichnet.

```
:pending.class #=> Symbol
status = :pending
status == :pending #=> true
status == 'pending' #=> false
status == :approved #=> false
```
# Arrays

## Ein Array anlegen
```
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]
```

## Array können verschiedene Typen beinhalten
```
[1, 'hello', false] #=> [1, "hello", false]
```

## Wie bei arithmetischen Ausdrücken auch wird beim Zugriff auf
## [0] eigentlich die Methode [] des Array Objekts aufgerufen.
```
array.[] 0 #=> 1
array.[] 12 #=> nil
```

## Arrays können von vorne indiziert werden
```
array[0] #=> 1
array[12] #=> nil
```

## Arrays können von hinten indiziert werden
```
array[-1] #=> 5
```

## Arrays können mit Start Index und Länge indiziert werden
```
array[2, 3] #=> [3, 4, 5]
```

## Arrays können mit einer Range indiziert werden
```
array[1..3] #=> [2, 3, 4]
```

## Einen Wert hinzufügen
```
array << 6 #=> [1, 2, 3, 4, 5, 6]
array.push(6) #=> [1, 2, 3, 4, 5, 6]
```

## Testen, ob ein Element schon vorhanden ist
```
array.include?(1) #=> true
```

# Hashes
Hashes sind das Hauptfeature um Key/Values zu speichern

## Ein Hash anlegen
```
hash = { 'color' => 'green', 'number' => 5 }
hash.keys #=> ['color', 'number']
```

## Wert per key herausfinden
```
hash['color'] #=> 'green'
hash['number'] #=> 5
hash['nothing here'] #=> nil
// Fragen an einen Hash nach einem Schlüssel, der nicht existiert, ruft nil hervor:
```

##  Symbols können auch keys sein
```
new_hash = { defcon: 3, action: true }
new_hash.keys #=> [:defcon, :action]
```

## Testen ob ein Key oder ein Value existiert
```
new_hash.has_key?(:defcon) #=> true
new_hash.has_value?(3) #=> true
```

### Tipp:  Arrays und Hashes sind Enumerable
### Und haben gemeinsame, hilfreiche Methoden wie:
### each, map, count, and more

# Kontrolstrukturen
## if
```
if true
  'if statement'
elsif false
  'else if, optional'
else
  'else, also optional'
end
```
## for - Allerdings werden for Schleifen nicht oft vewendet.
```
for counter in 1..5
  puts "iteration #{counter}"
end
```
## Stattdessen: "each" Methode und einen Bloch übergeben
Ein Block ist ein Codeteil, den man einer Methode übergeben kann
Ähnelt stark lambdas, anonymen Funktionen oder Closures in anderen
Programmiersprachen.

```
(1..5).each do |counter|
  puts "iteration #{counter}"
end
```

Die each Methode einer Range führt den Block für jedes Element der Range aus.

Dem Block wird ein "counter" parameter übergeben.

### Den Block kann man auch in geschweiften Klammern schreiben
```
(1..5).each { |counter| puts "iteration #{counter}" }
```

### Each kann auch über den Inhalt von Datenstrukturen iterieren
```
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
```

## case
```
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
=> "Better luck next time"
```

### Case können auch ranges
```
grade = 82
case grade
when 90..100
  puts 'Hooray!'
when 80...90
  puts 'OK job'
else
  puts 'You failed!'
end
=> "OK job"
```

# Exception handling:
```
begin
  # code here that might raise an exception
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
```
# Funktionen
```
def double(x)
  x * 2
end
```
## Funktionen (und Blocks)
## geben implizit den Wert des letzten Statements zurück
```
double(2) #=> 4
```

### Klammern sind optional wenn das Ergebnis nicht mehrdeutig ist
```
double 3 #=> 6
double double 3 #=> 12
def sum(x, y)
  x + y
end
```

### Methoden Parameter werden per Komma getrennt
```
sum 3, 4 #=> 7
sum sum(3, 4), 5 #=> 12
```

## yield
### Alle Methoden haben einen impliziten, optionalen block Parameter
### Dieser wird mit dem Schlüsselword "yield" aufgerufen
```
def surround
  puts '{'
  yield
  puts '}'
end
surround { puts 'hello world' }
```

## Einen Block kann man auch einer Methoden übergeben
### "&" kennzeichnet die Referenz zum übergebenen Block
```
def guests(&block)
  block.call 'some_argument'
end
```

### Eine Liste von Parametern kann man auch übergeben,
### Diese wird in ein Array konvertiert
### "*" kennzeichnet dies.
```
def guests(*array)
  array.each { |guest| puts guest }
end
```
# Klassen
## Werden mit dem class Schlüsselwort definiert
```
class Human
```

### Konstruktor bzw. Initializer
```
  def initialize(name, age = 0)
    # Assign the argument to the "name" instance variable for the instance
    @name = name
    # If no age given, we will fall back to the default in the arguments list.
    @age = age
  end
```

### setter Methode
```
  def name=(name)
    @name = name
  end
```
### getter Methode
```
  def name
    @name
  end
```

#### getter können mit der attr_accessor Methode vereinfacht definiert werden
```
  attr_accessor :name
  # Getter/setter methods can also be created individually like this
  attr_reader :name
  attr_writer :name
  # A class method uses self to distinguish from instance methods.
  # It can only be called on the class, not an instance.
  def self.say(msg)
    puts msg
  end
  def species
    @@species
  end
end
```

## Eine Klasse instanziieren
```
jim = Human.new('Jim Halpert')
dwight = Human.new('Dwight K. Schrute')
```

## Methodenaufrufe
```
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"
```

## Eine Klassenmethode aufrufen
```
Human.say('Hi') #=> "Hi"
```

## Variable Gültigkeit
### Variablen die mit "$" starten, gelten global
```
$var = "I'm a global var"
defined? $var #=> "global-variable"
```

### Variablen die mit "@" starten, gelten für die Instanz
```
@var = "I'm an instance var"
defined? @var #=> "instance-variable"
```

### Variablen die mit "@@" starten, gelten für die Klasse
```
@@var = "I'm a class var"
defined? @@var #=> "class variable"
```

### Variablen die mit einem Großbuchstaben anfangen, sind Konstanten
```
Var = "I'm a constant"
defined? Var #=> "constant"
```

## Class ist auch ein Objekt
### Hat also auch Instanzvariablen
### Eine Klassenvariable wird innerhalb der Klasse und Ableitungen geteilt.

### Basis Klasse
```
class Human
  @@foo = 0
  def self.foo
    @@foo
  end
  def self.foo=(value)
    @@foo = value
  end
end
```

### Abgeleitete Klasse
```
class Worker < Human
end
Human.foo # 0
Worker.foo # 0
Human.foo = 2 # 2
Worker.foo # 2
```

### Eine Klasseninstanzvariable wird nicht geteilt
```
class Human
  @bar = 0
  def self.bar
    @bar
  end
  def self.bar=(value)
    @bar = value
  end
end
```
```
class Doctor < Human
end
```
```
Human.bar # 0
Doctor.bar # nil
```
```
module ModuleExample
  def foo
    'foo'
  end
end
```
### Module einbinden, heisst ihre Methoden an die Instanzen der Klasse zu binden
### Module erweitern, heisst ihre Mothden an die Klasse selbst zu binden
```
class Person
  include ModuleExample
end
```
```
class Book
  extend ModuleExample
end
```
```
Person.foo     # => NoMethodError: undefined method `foo' for Person:Class
Person.new.foo # => 'foo'
Book.foo       # => 'foo'
Book.new.foo   # => NoMethodError: undefined method `foo'
```
### Callbacks werden ausgeführt, wenn ein Modul eingebunden oder erweitert wird
```
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
```
```
Something.bar     # => 'bar'
Something.qux     # => NoMethodError: undefined method `qux'
Something.new.bar # => NoMethodError: undefined method `bar'
Something.new.qux # => 'qux'
```

## Weiterführende Hinweise

//EN

- [Learn Ruby by Example with Challenges](http://www.learneroo.com/modules/61/nodes/338) - A variant of this reference with in-browser challenges.
- [Official Documentation](http://www.ruby-doc.org/core-2.1.1/)
- [Ruby from other languages](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - An older [free edition](http://ruby-doc.com/docs/ProgrammingRuby/) is available online.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - A community-driven Ruby coding style guide.
