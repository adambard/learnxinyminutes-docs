---
language: crystal
filename: learncrystal-fr.cr
contributors:
    - ["Vitalii Elenhaupt", "http://veelenga.com"]
    - ["Arnaud Fernandés", "https://github.com/TechMagister/"]
translators:
    - ["Arnaud Fernandés", "http://github.com/TechMagister/"]
lang: fr-fr
---

```crystal

# Ceci est un commentaire

# Tout est object
nil.class  #=> Nil
100.class  #=> Int32
true.class #=> Bool

# Les valeurs fausses sont : nil, false et les pointeurs null
!nil   #=> true  : Bool
!false #=> true  : Bool
!0     #=> false : Bool

# Entiers

1.class #=> Int32

# Quatre types d'entiers signés
1_i8.class  #=> Int8
1_i16.class #=> Int16
1_i32.class #=> Int32
1_i64.class #=> Int64

# Quatre types d'entiers non signés
1_u8.class  #=> UInt8
1_u16.class #=> UInt16
1_u32.class #=> UInt32
1_u64.class #=> UInt64

2147483648.class          #=> Int64
9223372036854775808.class #=> UInt64

# Nombre en base binaire
0b1101 #=> 13 : Int32

# Nombre en base octale
0o123 #=> 83 : Int32

# Nombres hexadécimaux
0xFE012D #=> 16646445 : Int32
0xfe012d #=> 16646445 : Int32

# Nombres à virgule

1.0.class #=> Float64

# Il y a deux types de nombres à virgule
1.0_f32.class #=> Float32
1_f32.class   #=> Float32

1e10.class    #=> Float64
1.5e10.class  #=> Float64
1.5e-7.class  #=> Float64

# Caractères

'a'.class #=> Char

# Notation octale des caratères
'\101' #=> 'A' : Char

# Notation unicode
'\u0041' #=> 'A' : Char

# Chaînes de caratères

"s".class #=> String

# Les chaînes de caractères sont immuables
s = "hello, "  #=> "hello, "        : String
s.object_id    #=> 134667712        : UInt64
s += "Crystal" #=> "hello, Crystal" : String
s.object_id    #=> 142528472        : UInt64

# Interpolation
"sum = #{1 + 2}" #=> "sum = 3" : String

# Chaînes multilignes
"Ceci est une chaine sur
plusieurs lignes"

# Une autre notation pour les chaînes de caratères
# qui permet d'insérer des guillemets
%(hello "world") #=> "hello \"world\""

# Symboles
# Ils sont immuables et réutilisables, ils sont représentés en interne par
# un Int32. Ils sont souvent utilisés à la place des chaînes de caractères
# quand l'identité est plus importante que le contenu

:symbol.class #=> Symbol

sentence = :question?     # :"question?" : Symbol

sentence == :question?    #=> true  : Bool
sentence == :exclamation! #=> false : Bool
sentence == "question?"   #=> false : Bool

# Tableaux

[1, 2, 3].class         #=> Array(Int32)
[1, "hello", 'x'].class #=> Array(Int32 | String | Char)

# Un type doit être spécifié pour les tableaux vides
[]               # Syntax error: for empty arrays use '[] of ElementType'
[] of Int32      #=> [] : Array(Int32)
Array(Int32).new #=> [] : Array(Int32)

# Les tableaux peuvent être indexés
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5] : Array(Int32)
array[0]                #=> 1               : Int32
array[10]               # lève l'exception IndexError
array[-6]               # lève l'exception IndexError
array[10]?              #=> nil             : (Int32 | Nil)
array[-6]?              #=> nil             : (Int32 | Nil)

# À partir de la fin
array[-1] #=> 5

# Avec un index de début et la taille
array[2, 3] #=> [3, 4, 5]

# Ou avec un intervalle
array[1..3] #=> [2, 3, 4]

# L'ajout à un tableau peut se faire avec l'opérateur <<
array << 6  #=> [1, 2, 3, 4, 5, 6]

# Enlève la dernière entrée
array.pop #=> 6
array     #=> [1, 2, 3, 4, 5]

# Enlève la première entrée
array.shift #=> 1
array       #=> [2, 3, 4, 5]

# Vérifie si un élément est présent dans le tableau
array.includes? 3 #=> true

# Syntaxe spéciale pour un tableau de chaîne de caractères ou de symboles
%w(one two three) #=> ["one", "two", "three"] : Array(String)
%i(one two three) #=> [:one, :two, :three]    : Array(Symbol)

# Il y a une syntaxe spéciale pour les tableaux et autres types
# du moment qu'ils définissent une méthode .new et #<<
set = Set{1, 2, 3} #=> [1, 2, 3]
set.class          #=> Set(Int32)

# Ce qui est ci dessus est équivalent à :
set = Set(typeof(1, 2, 3)).new
set << 1
set << 2
set << 3

# Tableaux associatifs

{1 => 2, 3 => 4}.class   #=> Hash(Int32, Int32)
{1 => 2, 'a' => 3}.class #=> Hash(Int32 | Char, Int32)

# Un type doit être spécifié pour les tableaux associatifs vides
{}                     # Syntax error
{} of Int32 => Int32   # {}
Hash(Int32, Int32).new # {}

# L'accès se fait via une clé
hash = {"color" => "green", "number" => 5}
hash["color"]        #=> "green"
hash["no_such_key"]  #=> Missing hash key: "no_such_key" (KeyError)
hash["no_such_key"]? #=> nil

# Vérifie l'existence d'une clé
hash.has_key? "color" #=> true

# Notation spéciale pour les clés si ce sont des symboles
# ou chaînes de caractères
{key1: 'a', key2: 'b'}     # {:key1 => 'a', :key2 => 'b'}
{"key1": 'a', "key2": 'b'} # {"key1" => 'a', "key2" => 'b'}

# De même que pour les tableaux simples, une syntaxe spéciale
# existe du moment que le type définit une méthode .new et #[]=
class MyType
  def []=(key, value)
    puts "do stuff"
  end
end

MyType{"foo" => "bar"}

# Ce qui est équivalent à :
tmp = MyType.new
tmp["foo"] = "bar"
tmp

# Intervalle

1..10                  #=> Range(Int32, Int32)
Range.new(1, 10).class #=> Range(Int32, Int32)

# Ils peuvent être inclusifs ou exclusifs
(3..5).to_a  #=> [3, 4, 5]
(3...5).to_a #=> [3, 4]

# Vérifie si un intervalle contient une valeur
(1..8).includes? 2 #=> true

# les tuples sont fixés en taille, immuables et alloués sur la pile
{1, "hello", 'x'}.class #=> Tuple(Int32, String, Char)

# L'accès peut se faire en utilisant un index
tuple = {:key1, :key2}
tuple[1] #=> :key2
tuple[2] #=> syntax error : Index out of bound

# Ils peuvent être scindés en plusieurs variables
a, b, c = {:a, 'b', "c"}
a #=> :a
b #=> 'b'
c #=> "c"

# Les procédures ( Proc ) sont des pointeurs de fonction
# avec un contexte optionel. Ils sont généralement créés avec
# cette notation :
proc = ->(x : Int32) { x.to_s }
proc.class # Proc(Int32, String)
# Ou en utilisant la méthode new
Proc(Int32, String).new { |x| x.to_s }

# On les invoque avec la méthode call
proc.call 10 #=> "10"

# Contrôle de flux

if true
  "if statement"
elsif false
  "else-if, optional"
else
  "else, also optional"
end

puts "if as a suffix" if true

# Le si ( if ) peut être utilisé pour une déclaration
a = if 2 > 1
      3
    else
      4
    end

a #=> 3

# Opérateur ternaire
a = 1 > 2 ? 3 : 4 #=> 4

# Aiguillage à l'aide du mot clé "case"
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

action #=> "Moving..."

# Boucle
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

# Mais il est préférable d'utiliser #each
(1..3).each do |index|
  puts "Index: #{index}"
end
# Index: 0
# Index: 1
# Index: 2
# Index: 3

# Le type d'une variable dépend du type de l'expression
# dans la déclaration du if
if a < 3
  a = "hello"
else
  a = true
end
typeof a #=> (Bool | String)

if a && b
  # ici a et b ne sont pas null
end

if a.is_a? String
  a.class #=> String
end

# Fonctions

def double(x)
  x * 2
end

# Les fonctions et tous les blocs retournent la valeur de la dernière évaluation
double(2) #=> 4

# Les parenthèses sont optionnelle quand l'appel n'est pas ambigü
double 3 #=> 6

double double 3 #=> 12

def sum(x, y)
  x + y
end

# Les arguments sont séparés par une virgule
sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12

# yield
# Toutes les méthodes on un paramètre optionel et implicite de type bloc
# il peut être appelé avec le mot clé 'yield'
def surround
  puts '{'
  yield
  puts '}'
end

surround { puts "hello world" }

# {
# hello world
# }

# Un bloc peut être passé à une fonction
# Le "&" marque une référence à un bloc
def guests(&block)
  block.call "some_argument"
end

# Une liste d'arguments peut être donnée, qui sera convertie en tableau
# Pour cela, utilisez l'opérateur "*"
def guests(*array)
  array.each { |guest| puts guest }
end

# Si une méthode retourne un tableau, il peut être scindé
def foods
    ["pancake", "sandwich", "quesadilla"]
end
breakfast, lunch, dinner = foods
breakfast #=> "pancake"
dinner    #=> "quesadilla"

# Par convention, toutes les méthodes qui retournent un booléen
# finissent par un point d'interrogation
5.even? # false
5.odd?  # true

# Si une méthode finit avec un point d'exclamation, c'est qu'elle fait une
# opération destructrice. Quelques méthodes ont une version "!" pour faire
# des changements et une version non-"!" pour retourner une nouvelle version
company_name = "Dunder Mifflin"
company_name.gsub "Dunder", "Donald"  #=> "Donald Mifflin"
company_name  #=> "Dunder Mifflin"
company_name.gsub! "Dunder", "Donald"
company_name  #=> "Donald Mifflin"


# Les classes se définissent avec le mot clé "class"
class Human

  # Une variable de classe, partagée par toutes les instances
  @@species = "H. sapiens"

  # "name" est une chaine de caratère ( String )
  @name : String

  # Constructeur basique, assigne l'argument à la variable "name"
  # si l'age n'est pas donné, sa valeur sera de 0
  def initialize(@name, @age = 0)
  end

  # Mutateur
  def name=(name)
    @name = name
  end

  # Accesseur
  def name
    @name
  end

  # La macro "property" va générer les deux précédentes méthodes
  property :name

  # Les accesseurs/mutateurs peuvent aussi être créés individuellement
  getter :name
  setter :name

  # Une méthode de classe utilise "self" pour se distinguer d'une
  # méthode d'instance. Elle ne peut être appelée qu'à partir de la classe
  def self.say(msg)
    puts msg
  end

  def species
    @@species
  end
end


# Instantie une classe
jim = Human.new("Jim Halpert")

dwight = Human.new("Dwight K. Schrute")

# Appelons quelques méthodes
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# Appel de la méthode de classe
Human.say("Hi") #=> Affiche "Hi" et retourne nil

# Les variables qui commencent par @ ont une portée d'instance
class TestClass
  @var = "Je suis une variable d'instance"
end

# Les variables qui commencent par @@ ont une portée de classe
class TestClass
  @@var = "Je suis une variable de classe"
end
# Les constantes commencent par une lettre majuscule
Var = "Je suis constante"
Var = "impossible" # Already initialized constant Var

# La classe est aussi un objet
# Les variables de classe sont partagées avec les descendants

# Classe de base
class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

# Classe dérivée
class Worker < Human
end

Human.foo   #=> 0
Worker.foo  #=> 0

Human.foo = 2 #=> 2
Worker.foo    #=> 0

Worker.foo = 3 #=> 3
Human.foo   #=> 2
Worker.foo  #=> 3

module ModuleExample
  def foo
    "foo"
  end
end

# Inclure (include) des modules ajoute leurs méthodes aux instances
# Étendre (extend) ajoute les méthodes à la classe

class Person
  include ModuleExample
end

class Book
  extend ModuleExample
end

Person.foo     # => undefined method 'foo' for Person:Class
Person.new.foo # => 'foo'
Book.foo       # => 'foo'
Book.new.foo   # => undefined method 'foo' for Book


# Gestion des exceptions

# Définit un type d'exeption
class MyException < Exception
end

# Définit une autre exception
class MyAnotherException < Exception; end

ex = begin
   raise MyException.new
rescue ex1 : IndexError
  "ex1"
rescue ex2 : MyException | MyAnotherException
  "ex2"
rescue ex3 : Exception
  "ex3"
rescue ex4 # attrape toutes les autres exceptions
  "ex4"
end

ex #=> "ex2"

```

## Ressources additionnelles

- [Documentation Officielle (EN)](http://crystal-lang.org/)
