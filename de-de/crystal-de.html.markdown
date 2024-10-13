---
language: crystal
contributors:
    - ["Vitalii Elenhaupt", "http://veelenga.com"]
    - ["Arnaud Fernandés", "https://github.com/TechMagister/"] 
translators:
    - ["caminsha", "https://github.com/caminsha"]
filename: learncrystal-de.cr
lang: de-de
---

```crystal
# Das ist ein Kommentar

# Alles ist ein Objekt
nil.class # => Nil
100.class # => Int32
true.class # => Bool

# Falschwerte sind: nil, false und Nullpointer
!nil    # => true    : Bool
!false  # => true    : Bool
!0      # => false   : Bool

# Integer

1.class # => Int32

# Fünf vorzeichenbehaftete Ganzzahlen
1_i8.class   # =>    Int8
1_i16.class  # =>    Int16
1_i32.class  # =>    Int32
1_i64.class  # =>    Int64
1_i128.class # =>    Int128

# Fünf vorzeichenlose Ganzzahlen
1_u8.class   # =>    UInt8
1_u16.class  # =>    UInt16
1_u32.class  # =>    UInt32
1_u64.class  # =>    UInt64
1_u128.class # =>    UInt128


2147483648.class            # => Int64
9223372036854775808.class   # => UInt64

# Binäre Zahlen
0b1101 # => 13  : Int32

# Oktalzahlen
0o123 # => 83   : Int32

# Hexadezimalzahlen
0xFE012D # => 16646445 : Int32
0xfe012d # => 16646445 : Int32

# Gleitkommazahlen (floats)

1.0.class   # => Float64

# Es gibt zwei Typen von Gleitkommazahlen

1.0_f32.class   # => Float32
1_f32.class     # => Float32

1e10.class      # => Float64
1.5e10.class    # => Float64
1.5e-7.class    # => Float64


# Chars (einzelne Zeichen)

'a'.class   # => Char

# Oktale Schreibweise
'\101'      # => 'A' : Char

# Unicode Schreibweise
'\u0041'    # => 'A' : Char

# Strings (Zeichenketten)
"s".class   # => String

# Strings sind unveränderlich
s = "hello, "   # => "hello, "          : String
s.object_id     # => 1234667712         : UInt64
s += "Crystal"  # => "hello, Crystal"   : String
s.object_id     # => 142528472          : UInt64

# Interpolation wird unterstützt
"sum = #{1 + 2}"    # => "sum = 3"  : String

# Mehrzeilige Strings
" Dies ist ein
    mehrzeiliger String."

# String mit doppeltem Anführungszeichen
%(hello "world")    # => "hello \"world\""

# Symbole
# Unveränderbare, wiederverwendbare Konstanten, welche intern als Int32 Integer
# Werte repräsentiert werden.
# Symbole werden oft anstelle von Strings verwendet, um bestimmte Werte zu bestimmen.

:symbol.class   # => Symbol

sentence = :question?   # :"question?" : Symbol

sentence = :question?       # => true   : Bool
sentence = :exclamation!    # => false  : Bool
sentence = "question?"      # => false  : Bool

# Arrays
[1, 2, 3].class         # => Array(Int32)
[1, "hello", 'x'].class # => Array(Int32 | String | Char)

# Leere Arrays sollten einen Typen definieren
[]                  # => Syntaxfehler: für leere Arrays,
                    # verwende `[] of ElementType`
[] of Int32         # => [] : Array(Int32)
Array(Int32).new    # => [] : Array(Int32)

# Arrays können indiziert werden
array = [1, 2, 3, 4, 5] # => [1, 2, 3, 4, 5] : Array(Int32)
array[0]                # => 1               : Int32
array[10]               # führt zu einem IndexError
array[-6]               # führt zu einem IndexError
array[10]?              # => nil             : (Int32 | Nil)
array[-6]?              # => nil             : (Int32 | Nil)

# Starte am Ende des Arrays
array[-1]               # => 5

# Mit einem Startindex und einer Länge
array[2, 4]             # => [3, 4, 5]

# oder mit einem Bereich
array[1..3]             # => [2, 3, 4]

# Füge etwas zu einem Array hinzu
array << 6              # => [1, 2, 3, 4, 5, 6]

# Entferne Einträge am Ende des Arrays
array.pop               # => 6
array                   # => [1, 2, 3, 4, 5]

# Entferne ersten Eintrag im Array
array.shift             # => 1
array                   # => [2, 3, 4, 5]

# Überprüfe, ob ein Element in einem Array existiert
array.includes? 3       # => true

# Spezielle Syntax für String-Arrays und Symbol-Arrays
%w(one two three)   # => ["one", "two", "three"]    : Array(String)
%i(one two three)   # 0> [:one, :two, :three]       : Array(Symbol)

# Es gibt auch für andere Arrays eine spezielle Syntax, wenn die Methoden
# `.new` und `#<<` definiert werden.
set = Set{1, 2, 3}  # => [1, 2, 3]
set.class           # => Set(Int32)

# Das obere ist äquivalent zu:
set = Set(typeof(1, 2, 3)).new
set << 1
set << 2
set << 3

# Hashes
{1 => 2, 3 => 4}.class      # => Hash(Int32, Int32)
{1 => 2, 'a' => 3}.class    # => Hash (Int32 | Char, Int32)

# Leere Hashes sollten einen Typen spezifizieren
{}                      # Syntaxfehler
{} of Int32 => Int32    # {}
Hash(Int32, Int32).new  # {}

# Hashes können schnell mit dem Key nachgeschaut werden
hash = {"color" => "green", "number" => 5}
hash["color"]           # => "green"
hash["no_such_key"]     # => Fehlender hash key: "no_such_key" (KeyError)
hash["no_such_key"]?    # => nil

# Überprüfe die Existenz eines Hashkeys
hash.has_key? "color"   # => true

# Spezielle Schreibweise für Symbol- und Stringkeys
{key1: 'a', key2: 'b'}      # {:key1 => 'a', :key2 => 'b'} 
{"key1": 'a', "key2": 'b'}  # {"key1" => 'a', "key2" => 'b'}

# Die spezielle Syntax für Hash-Literale gibt es auch für andere Typen, sofern
# diese die Methoden `.new` und `#[]=` Methoden definieren.
class MyType
    def []=(key, value)
        puts "do stuff"
    end
end

MyType{"foo" => "bar"}

# Das obere ist äquivalent zu:
tmp = MyType.new
tmp["foo"] = "bar"
tmp

# Ranges (Bereiche)
1..10                   # => Range(Int32, Int32)
Range.new(1,10).class   # => Range(Int32, Int32)

# Ranges können inklusiv oder exklusiv sein.
(3..5).to_a     # => [3, 4, 5]
(3...5).to_a    # => [3, 4]

# Überprüfe, ob ein Range einen Wert enthält oder nicht.
(1..8).includes? 2  # => true

# Tupel sind unveränderliche, Stack-zugewiese Folgen von Werten mit fester 
# Größe und möglicherweise unterschiedlichen Typen
{1, "hello", 'x'}.class # => Tuple(Int32, String, Char)

# Erhalte den Wert eines Tupels über den Index
tuple = {:key1, :key2}
tuple[1] # => :key2
tuple[2] # syntax error: Index out of bound

# Können auf mehrere Variablen erweitert werden
a, b, c = {:a, 'b', "c"}
a   # => :a
b   # => 'b'
c   # => "c"

# Procs repräsentieren ein Funktionspointer mit einem optionalen Kontext.
# Normalerweise wird ein Proc mit einem proc-Literal erstellt.
proc = ->(x : Int32) { x.to_s }
proc.class  # => Print(Int32, String)
# Außerdem kann man auch mit der Methode `new` ein Proc erstellen.
Proc(Int32, String).new { |x| x.to_s }

# Rufe ein Proc auf mit der Methode `call`
proc.call 10    # => "10"

# Kontrollstatements

if true
    "if statement"
elsif false
    "else-f, optional"
else
    "else, auch optional"
end

puts "if as a suffix" if true # => if as a suffix

# If als Ausdruck
a = if 2 > 1
        3
    else
        4
    end

a # => 3

# Bedingter ternärer Ausdruck
a = 1 > 2 ? 3 : 4   # => 4

# Case-Statement
cmd = "move"

action = case cmd
    when "create"
        "Creating..."
    when "copy"
        "Copying..."
    when "move"
        "Moving..."
    when "delete"
        "Deleting..."
end

action # => "Moving..."

# Schleifen
index = 0
while index <= 3
    puts "Index: #{index}"
    index += 1
end
# Index: 0
# Index: 1
# Index: 2
# Index: 3

index = 0
until index > 3
    puts "Index: #{index}"
    index += 1
end
# Index: 0
# Index: 1
# Index: 2
# Index: 3

# Der bevorzugte Weg, ist `each` zu verwenden.
(1..3).each do |index|
    puts "Index: #{index}"
end
# Index: 1
# Index: 2
# Index: 3

# Der Typ der Variablen hängt vom Typen innerhalb der Kontrollanweisung ab
if a < 3
    a = "hello"
else
    a = true
end
typeof a    # => (Bool | String)

if a && b
    # Hier wird garantiert, dass weder a noch b vom Typ Nil sind
end

if a.is_a? String
    a.class # => String
end

# Funktionen
def double(x)
    x * 2
end

# Funktionen geben implizit den Wert der letzten Anweisung zurück
# Dies ist auch bei anderen Blöcken der Fall.
double(2)       # => 4

# Klammern müssen nicht gesetzt werden, wenn der Aufruf eindeutig ist
double 3        # => 6
double double 3 # => 12

def sum(x, y)
    x + y
end

# Funktionsargumente werden mit einem Komma separiert.
sum 3, 4            # => 7

sum sum(3, 4), 5    # => 12

# yield
# Alle Methoden haben einen impliziten, optionalen Blockparameter.
# Dieser kann mit dem Schlüsselwort `yield` aufgerufen werden.

def surround
    puts '{'
    yield
    puts '}'
end

surround { puts "Hallo Welt" }

# {
# Hallo Welt 
# }

# Du kannst ein Block einer Funktion übergeben.
# "&" kennzeichnet eine Referenz zu einem übergebenen Block
def guests(&block)
    block.call "some_argument"
end

# Du kannst eine Liste von Argumenten mitgeben, welche zu einem Array
# umgewandelt werden.
# Hierfür ist der Splat-Operator ("*")
def guests(*array)
    array.each { |guest| puts guest }
end

# Wenn eine Methode ein Array zurückgibt, kann destrukturiende Zuordnung
# verwendet werden.
def foods
    ["pancake", "sandwich", "quesadilla"]
end
breakfast, lunch, dinner = foods
breakfast   # => "pancake"
dinner      # => "quesadilla"

# Gemäß der Konvention enden alle Methoden, welchen einen Boolean zurückgeben
# mit einem Fragezeichen.
5.even? # false
5.odd?  # true

# Und wenn eine Methode mit einem Ausrufezeichen endet, macht sie etwas
# destruktives. Zum Beispiel wird der Aufrufer verändert. Einige Methoden haben
# eine !-Version, um eine Änderung zu machen und eine Nicht-!-Version, welche
# lediglich eine neue veränderte Version zurückgibt.

company_name = "Dunder Mifflin"
company_name.gsub "Dunder", "Donald"    # => "Donald Mifflin"
company_name    # => "Dunder Mifflin"
company_name.gsub! "Dunder", "Donald"
company_name    # => "Donald Mifflin"

# definiere eine Klasse mit dem Schlüsselwort `class`.
class Human

# eine Klassenvariable. Diese wird mit allen Instanzen dieser Klasse geteilt.
    @@species = "H. sapiens"

    # type of name is String
    @name: String

    # Grundlegender Intialisierer
    # Weise das Argument der Instanz-Variable "name" zu
    # Wenn kein Alter angegeben wird, wird der Default (hier 0) genommen.
    def initialize(@name, @age = 0)
    end

    # Einfache Setter-Methode
    def name=(name)
        @name = name
    end

    # einfache Getter-Methode
    def name
        @name
    end

    # Die obere Funktionalität kann mit der property-Methode gekapselt werden:
    property :name

    # Getter/Setter-Methoden können auch individuell erstellt werden:
    getter :name
    setter :name
    
    # eine Klassenmethode verwendet `self` um sich von Instanzmethoden zu 
    # unterscheiden. Diese kann lediglich von einer Klasse aufgerufen werden,
    # nicht von einer Instanz.
    def self.say(msg)
        puts msg
    end

    def species
        @@species
    end
end


# Eine Klasse instanziieren
jim = Human.new("Jim Halpert")

dwight = Human.new("Dwight K. Schrute")

# Lass uns ein paar Methoden aufrufen
jim.species                     # => "H. sapiens"
jim.name                        # => "Jim Halpert"
jim.name = "Jim Halpert II"     # => "Jim Halpert II"
jim.name                        # => "Jim Halpert II"
dwight.species                  # => "H. sapiens"
dwight.name                     # => "Dwight K. Schrute"

# Rufe die Klassenmethode auf
Human.say("Hi")     # => gibt Hi aus und gibt `nil` zurück

# Variablen, welche mit @ starten, sind im Scope der Instanz
class TestClass
    @var = "Ich bin eine Instanzvariable"
end

# Variablen, welche mit @@ starten, sind im Scope der Klasse
class TestClass
    @@var = "Ich bin eine Klassenvariable"
end

# Variablen, welche mit einem Großbuchstaben starten, sind Konstanten.
Var = "Ich bin eine Konstante"
Var = "Ich kann nicht aktualisiert werden." # Die Konstante Var wurde bereits
                                            # initialisiert.

# In Crystal ist Class auch ein Objekt. Dadurch können Klassen Instanzvariablen
# haben. Klassenvariablen werden mit der Klasse und allen Subklassen geteilt.

# Basisklasse
class Human
    @@foo = 0

    def self.foo
        @@foo
    end

    def self.foo=(value)
        @@foo = value
    end
end

# abgeleitete Klasse
class Worker < Human
end

Human.foo       # => 0
Worker.foo      # => 0

Human.foo = 2   # => 2
Worker.foo      # => 0

Worker.foo = 3  # => 3
Human.foo       # => 2
Worker.foo      # => 3

module ModuleExample
    def foo
        "foo"
    end
end

# Wenn ein Modul mit include eingeschlossen wird, so werden die Methoden an die
# Instanzen gebunden.
# Wenn eine Klasse mit einem Modul erweitert wird, so werden die Methoden an die
# Klasse selbst gebunden.

class Person 
    include ModuleExample
end

class Book
    extend ModuleExample
end

Person.foo      # => undefinierte Methode 'foo' für Person:Class
Person.new.foo  # => 'foo'
Book.foo        # => 'foo'
Book.new.foo    # => undefinierte Methode für Book

# Ausnahmebehandlung

# Definiere eine neue Ausnahme
class MyException < Exception
end

# Definiere eine weitere Ausnahme
class MyAnotherException < Exception; end

ex = begin
    raise MyException.new
rescue ex1 : IndexError
    "ex1"
rescue ex2 : MyException | MyAnotherException
    "ex2"
rescue ex3 : Exception
    "ex3"
rescue ex4 # fange alle Ausnahmen ab
    "ex4"
end

ex # => "ex2"
```


## Weitere Unterlagen

- [offizielle Dokumentation, englisch](https://crystal-lang.org/)
