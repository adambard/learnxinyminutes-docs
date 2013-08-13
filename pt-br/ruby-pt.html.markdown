---
language: ruby
filename: learnruby.rb
contributors:
  - ["Bruno Henrique - Garu", "http://garulab.com"]
---

```ruby
# Isso é um comentario

=begin
This is a multiline comment
No-one uses them
You shouldn't either

Isso é um comentario multilinha
Ninguém os usa

=end

# First and foremost: Everything is an object.
# Primeiro e principal: Tudo é um objeto.

# Numbers are objects
# Números são objetos

3.class #=> Fixnum

3.to_s #=> "3"


# Some basic arithmetic
# Aritmética básica

1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Arithmetic is just syntactic sugar
# for calling a method on an object
# Arithmetic é apenas açúcar semântico 
# para chamar um métoddo de um objeto
1.+(3) #=> 4
10.* 5 #=> 50 

# Special values are objects
# Valores especiais são obejetos
nil # Nothing to see here 
nil # Nada para ver aqui
true # truth
true # verdadeiro
false # falsehood
false # falso

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Equality
# Igualdade
1 == 1 #=> true
2 == 1 #=> false

# Inequality
# Desigualdade
1 != 1 #=> false
2 != 1 #=> true
!true  #=> false
!false #=> true

# apart from false itself, nil is the only other 'falsey' value
# além de 'false', 'nil' é o único outro valor falso

!nil   #=> true
!false #=> true
!0     #=> false

# More comparisons
# Mais comparações
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Strings are objects
# Strings são obejetos

'I am a string'.class #=> String
'Eu sou uma string'.class #=> String
"I am a string too".class #=> String
"Eu também sou uma string".class #=> String

placeholder = "use string interpolation"
placeholder = "usar interpolação de string"
"I can #{placeholder} when using double quoted strings"
"Eu posso #{placeholder} quando estiver usando aspas duplas"
#=> "I can use string interpolation when using double quoted strings"
#=> "Eu posso usar insterpolação de string quando estiver usando aspas duplas"


# print to the output
# imprime para output (saida)
puts "I'm printing!"
puts "Estou imprimindo"

# Variables
# Variáveis
x = 25 #=> 25
x #=> 25

# Note that assignment returns the value assigned
# Note que uma atribuição retorna o valor atribuido
# This means you can do multiple assignment:
# Isso significa que você pode fazer multiplas atribuições:

x = y = 10 #=> 10
x #=> 10
y #=> 10

# By convention, use snake_case for variable names
# Por convenção, use snake_case para nomes de variáveis
snake_case = true

# Use descriptive variable names
# Use nomes de variáveis descrivos
path_to_project_root = '/good/name/'
caminho_para_a_raiz_do_projeto = '/bom/nome/'
path = '/bad/name/'
caminho = '/nome/ruim/'

# Symbols (are objects)
# Simbolos (são objetos)
# Symbols are immutable, reusable constants represented internally by an
# Simbolos são imultáveis, são constantes reutilizáveis representadadas internamente por um
# integer value. They're often used instead of strings to efficiently convey
# valor inteiro. Eles são frequentemente usados no lugar de strings para transmitir com eficiência os valores
# specific, meaningful values
# específicos e significativos

:pending.class #=> Symbol
:pendente.class #=> Symbol

status = :pending
status = :pendente

status == :pending #=> true
status == :pendente #=> true

status == 'pending' #=> false
status == 'pendente' #=> false

status == :approved #=> false
status == :aprovado #=> false

# Arrays

# This is an array
# Isso é um array
[1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Arrays can contain different types of items
# Arrays podem conter diferentes tipos de itens

array = [1, "hello", false] #=> => [1, "hello", false]
array = [1, "Oi", false] #=> => [1, "Oi", false]

# Arrays can be indexed
# Arrays podem ser indexados
# From the front
# a partir do começo
array[0] #=> 1
array[12] #=> nil

# Like arithmetic, [var] access
# Como aritimetica, o acesso via [var]
# is just syntactic sugar
# é apenas açúcar sintático
# for calling a method [] on an object
# para chamar o método [] de um objeto
array.[] 0 #=> 1
array.[] 12 #=> nil

# From the end
# a partir do final
array[-1] #=> 5

# With a start and end index
# Com um índice de começo e fim 
array[2, 4] #=> [3, 4, 5]

# Or with a range
# Ou com um intervalo de valores
array[1..3] #=> [2, 3, 4]

# Add to an array like this
# Adicionar a um array como este
array << 6 #=> [1, 2, 3, 4, 5, 6]

# Hashes are Ruby's primary dictionary with keys/value pairs.
# Hashes são dicionário com um par de chave(key)/valor(value)
# Hashes are denoted with curly braces:
# Hashes são simbolizados com chaves "{}"
hash = {'color' => 'green', 'number' => 5}
hash = {'cor' => 'verde', 'numero' => 5}

hash.keys #=> ['cor', 'numero']

# Hashes can be quickly looked up by key:
# Hashes podem ser rapidamente pesquisado pela chave (key)
hash['cor'] #=> 'verde'
hash['numero'] #=> 5

# Asking a hash for a key that doesn't exist returns nil:
# Procurar em um hash por uma chave que não existe retorna nil:
hash['nothing here'] #=> nil
hash['nada aqui'] #=> nil

# Iterate over hashes with the #each method:
# Interar sobre hashes com o método #each:
hash.each do |k, v|
  puts "#{k} is #{v}"
end

hash.each do |k, v|
  puts "#{k} é #{v}"
end

# Since Ruby 1.9, there's a special syntax when using symbols as keys:
# Desde o Ruby 1.9, temos uma sintaxe especial quando usamos simbolos como chaves (keys)

new_hash = { defcon: 3, action: true}
novo_hash = { defcon: 3, acao: true}

new_hash.keys #=> [:defcon, :action]
novo_hash.keys #=> [:defcon, :acao]

# Tip: Both Arrays and Hashes are Enumerable
# Dica: Tanto Arrays quanto Hashes são Enumerable
# They share a lot of useful methods such as each, map, count, and more
# Eles compartilham um monte de métodos úteis como each, map, count e mais

# Control structures
# Estruturas de controle

if true
  "if statement"
elsif false
 "else if, optional"
else
 "else, also optional"
end

if true
  "Se verdadeiro"
elsif false
 "else if, opicional"
else
 "else, também é opicional"
end

for counter in 1..5
  puts "iteration #{counter}"
end

for contador in 1..5
  puts "interação #{contador}"
end
#=> contador 1
#=> contador 2
#=> contador 3
#=> contador 4
#=> contador 5

# HOWEVER
# PORÉM
# No-one uses for loops
# Ninguém usa para loops
# Use `each` instead, like this:
# Use "each" em vez, dessa forma:

(1..5).each do |counter|
  puts "iteration #{counter}"
end

(1..5).each do |contador|
  puts "interação #{contador}"
end
#=> contador 1
#=> contador 2
#=> contador 3
#=> contador 4
#=> contador 5

counter = 1
while counter <= 5 do
  puts "iteration #{counter}"
  counter += 1
end

contador = 1
while contador <= 5 do
  puts "interação #{contador}"
  contador += 1
end
#=> contador 1
#=> contador 2
#=> contador 3
#=> contador 4
#=> contador 5

grade = 'B'

case grade
when 'A'
  puts "Way to go kiddo"
when 'B'
  puts "Better luck next time"
when 'C'
  puts "You can do better"
when 'D'
  puts "Scraping through"
when 'F'
  puts "You failed!"
else 
  puts "Alternative grading system, eh?"
end

grau = 'B'

case grau
when 'A'
  puts "Um longo caminho a percorrer pequeno gafanhoto"
when 'B'
  puts "Melhor sorte da próxima vez"
when 'C'
  puts "Você pode fazer melhor"
when 'D'
  puts "Scraping through"
when 'F'
  puts "Você falhou"
else 
  puts "Alternative grading system, eh?"
end

# Functions
# Funções

def dobrar(x)
  x * 2
end

# Functions (and all blocks) implcitly return the value of the last statement
# Funções (e todos os blocos) retornam implicitamente o valor da última linha
double(2) #=> 4
dobrar(2) #=> 4

# Parentheses are optional where the result is unambiguous
# Parênteses são opicionais onde o resultado é claro
double 3 #=> 6
dobrar 3 #=> 6

double double 3 #=> 12
dobrar dobrar 3 #=> 12

def sum(x,y)
  x + y
end

def somar(x,y)
  x + y
end

# Method arguments are separated by a comma
# Argumentos de métodos são separados por uma virgula
sum 3, 4 #=> 7
somar 3, 4 #=> 7

somar somar(3,4), 5 #=> 12

# yield
# All methods have an implicit, optional block parameter
# Todos os métodos possuem implicitamente um paramêntro opcional que é um bloco 
# it can be called with the 'yield' keyword
# ele pode ser chamado com a palavra chave 'yield'

def surround
  puts "{"
  yield
  puts "}"
end

surround { puts 'hello world' }


def ao_redor
  puts "{"
  yield
  puts "}"
end

ao_redor { puts 'Olá mundo' }

# {
# Olá mundo
# }


# Define a class with the class keyword
# Define uma classe com a palavra chave 'class'
class Human

  # A class variable. It is shared by all instances of this class.
  @@species = "H. sapiens"

  # Basic initializer
  def initialize(name, age=0)
    # Assign the argument to the "name" instance variable for the instance
    @name = name
    # If no age given, we will fall back to the default in the arguments list.
    @age = age
  end

  # Basic setter method
  def name=(name)
    @name = name
  end

  # Basic getter method
  def name
    @name
  end

  # A class method uses self to distinguish from instance methods.
  # It can only be called on the class, not an instance.
  def self.say(msg)
    puts "#{msg}"
  end

  def species
    @@species
  end

end


class Humano

  # Uma variavel de classe. Ela é compartilhada por todas as instancias dessa classe
  @@especies = "H. sapiens"

  # Inicialização básica (contructor)
  def initialize(nome, idade=0)
    # Atribui o argumento para a variavel de instacia "nome" do objeto
    @nome = nome
    # Se a idade não for passada, nós definimos um valor padrão na lista de argumentos
    @idade = idade
  end

  # Método básico para atribuir valor
  def nome=(nome)
    @nome = nome
  end

  # Método básico de resgatar valor
  def nome
    @nome
  end

  # Um método de classe usa a palavra chave self para se defenciar dos métodos de instancia.
  # Ele só pode ser chamado na classe, não na instancia
  def self.diz(msg)
    puts "#{msg}"
  end

  def especies
    @@especies
  end

end


# Instantiate a class
# Instaciando uma classe
jim = Human.new("Jim Halpert")
jim = Humano.new("Jim Halpert")

dwight = Human.new("Dwight K. Schrute")
dwight = Humano.new("Dwight K. Schrute")

# Let's call a couple of methods
# Vamos chamar um par de métodos
jim.species #=> "H. sapiens"
jim.especies #=> "H. sapiens"

jim.name #=> "Jim Halpert"
jim.nome #=> "Jim Halpert"

jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.nome = "Jim Halpert II" #=> "Jim Halpert II"

jim.name #=> "Jim Halpert II"
jim.nome #=> "Jim Halpert II"

dwight.species #=> "H. sapiens"
dwight.especies #=> "H. sapiens"

dwight.name #=> "Dwight K. Schrute"
dwight.nome #=> "Dwight K. Schrute"

# Call the class method
# Chamar o método de classe
Human.say("Hi") #=> "Hi"
Humano.diz("Oi") #=> "Oi"

# Class also is object in ruby. So class can have instance variables.
# Uma classe também é objeto em Ruby. Então uma classe pode possuir um variavel de instancia
# Class variable is shared among the class and all of its descendants.
# Variavies de classe são compartilhadas entre a classe e todos os seus descendentes.

# base class
class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end


# Classe base
class Humano
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

#  classe filha
class Trabalhador < Humano
end

Human.foo # 0
Humano.foo # 0
Worker.foo # 0
Trabalhador.foo # 0

Human.foo = 2 # 2
Humano.foo = 2 # 2
Worker.foo # 2
Trabalhador.foo # 2

# Class instance variable is not shared by the class's descendants.
# Uma variavel de instancia não é compartilhada por suas classes decendentes.

class Human
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(value)
    @bar = value
  end
end

class Humano
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

class Doutor < Humano
end

Humano.bar # 0
Doutor.bar # nil

```
