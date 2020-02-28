---
language: ruby
filename: learnruby-es.rb
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
translators:
    - ["Camilo Garrido", "http://www.twitter.com/hirohope"]
    - ["Erick Bernal", "http://www.twitter.com/billowkib"]
lang: es-es
---

```ruby
# Esto es un comentario

=begin
Este es un comentario multilínea
Nadie los usa.
Tu tampoco deberías
=end

# En primer lugar: Todo es un objeto

# Los números son objetos

3.class #=> Fixnum

3.to_s #=> "3"


# Un poco de aritmética básica
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2**5 #=> 32
5 % 3 #=> 2

# La aritmética es sólo azúcar sintáctico
# para llamar un método de un objeto
1.+(3) #=> 4
10.* 5 #=> 50

# Los valores especiales son objetos
nil # Nada que ver aqui
true # Verdadero
false # Falso

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Igualdad
1 == 1 #=> true
2 == 1 #=> false

# Desigualdad
1 != 1 #=> false
2 != 1 #=> true

# Además de 'false', 'nil' es otro valor falso

!nil   #=> true
!false #=> true
!0     #=> false

# Más comparaciones
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Operadores lógicos
true && false #=> false
true || false #=> true
!true #=> false

# Existen versiones alternativas de los operadores lógicos con menor prioridad
# Estos son usados como constructores controladores de flujo que encadenan
# sentencias hasta que una de ellas retorne verdadero o falso

# `has_otra_cosa` solo se llama si `has_algo` retorna verdadero.
has_algo() and has_otra_cosa()
# `registra_error` solo se llama si `has_algo` falla
has_algo() or registra_error()


# Los strings son objetos

'Soy un string'.class #=> String
"Soy un string también".class #=> String

referente = "usar interpolación de strings"
"Yo puedo #{referente} usando strings de comillas dobles"
#=> "Yo puedo usar interpolación de strings usando strings de comillas dobles"


# Imprime a la salida estándar
puts "¡Estoy imprimiendo!"

# Variables
x = 25 #=> 25
x #=> 25

# Nota que la asignación retorna el valor asignado
# Esto significa que puedes hacer múltiples asignaciones:

x = y = 10 #=> 10
x #=> 10
y #=> 10

# Por convención, usa snake_case para nombres de variables
snake_case = true

# Usa nombres de variables descriptivos
ruta_para_la_raiz_de_un_projecto = '/buen/nombre/'
ruta = '/mal/nombre/'

# Los símbolos (son objetos)
# Los símbolos son inmutables, constantes reusables representadas internamente por un
# valor entero. Son normalmente usados en vez de strings para expresar eficientemente
# valores específicos y significativos

:pendiente.class #=> Symbol

status = :pendiente

status == :pendiente #=> true

status == 'pendiente' #=> false

status == :aprobado #=> false

# Arreglos

# Esto es un arreglo
arreglo = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Arreglos pueden contener elementos de distintos tipos

[1, "hola", false] #=> => [1, "hola", false]

# Arreglos pueden ser indexados
# Desde el frente
arreglo[0] #=> 1
arreglo.first #=> 1
arreglo[12] #=> nil

# Al igual que en aritmética, el acceso como variable[índice]
# es sólo azúcar sintáctica
# para llamar el método [] de un objeto
arreglo.[] 0 #=> 1
arreglo.[] 12 #=> nil

# Desde el final
arreglo[-1] #=> 5
arreglo.last #=> 5

# Con un índice de inicio y longitud
arreglo[2, 3] #=> [3, 4, 5]

# Invertir un arreglo
a = [1, 2, 3]
a.reverse! #=> [3, 2, 1]

# O con rango
arreglo[1..3] #=> [2, 3, 4]

# Añade elementos a un arreglo así
arreglo << 6 #=> [1, 2, 3, 4, 5, 6]
# O así
arreglo.push(6) #=> [1, 2, 3, 4, 5, 6]

#Verifica si un elemento ya existe en ese arreglo
arreglo.include?(1) #=> true

# Hashes son los diccionarios principales de Ruby con pares llave/valor.
# Hashes se denotan con llaves:
hash = {'color' => 'verde', 'numero' => 5}

hash.keys #=> ['color', 'numero']

# Hashes pueden buscar rápidamente una llave:
hash['color'] #=> 'verde'
hash['numero'] #=> 5

# Preguntarle a un hash por una llave que no existe retorna 'nil':
hash['nada aqui'] #=> nil

# Desde Ruby 1.9, hay una sintaxis especial cuando se usa un símbolo como llave:

nuevo_hash = { defcon: 3, accion: true}

nuevo_hash.keys #=> [:defcon, :accion]

# Verifica la existencia de llaves y valores en el hash
new_hash.has_key?(:defcon) #=> true
new_hash.has_value?(3) #=> true

# Tip: Tanto los arreglos como los hashes son Enumerable (enumerables)
# Comparten muchos métodos útiles tales como 'each', 'map', 'count', y más

# Estructuras de Control

if true
  "declaracion 'if'"
elsif false
 "else if, opcional"
else
 "else, tambien opcional"
end

for contador in 1..5
  puts "iteracion #{contador}"
end
#=> iteracion 1
#=> iteracion 2
#=> iteracion 3
#=> iteracion 4
#=> iteracion 5

# SIN EMBARGO, nadie usa ciclos `for`
# En su lugar debes usar el método "each" y pasarle un block (bloque).
# Un bloque es un fragmento código que puedes pasar a métodos como `each`.
# Es símilar a las funciones lambda, funciones anónimas o `closures` en otros
# lenguajes de programación.
#
# El método `each` de un Range (rango) ejecuta el bloque una vez por cada elemento.
# Al bloque se le pasa un contador como parametro.
# Usar el método `each` con un bloque se ve así:

(1..5).each do |contador|
  puts "iteracion #{contador}"
end
#=> iteracion 1
#=> iteracion 2
#=> iteracion 3
#=> iteracion 4
#=> iteracion 5

# También puedes envolver el bloque entre llaves:
(1..5).each { |counter| puts "iteración #{contador}" }

#El contenido de las estructuras de datos en ruby puede ser iterado usando `each`.
arreglo.each do |elemento|
  puts "#{elemento} es parte del arreglo"
end
hash.each do |llave, valor|
  puts "#{llave} es #{valor}"
end

# Si aún necesitas un índice puedes usar "each_with_index" y definir una variable
# índice.
arreglo.each_with_index do |element, index|
  puts "#{element} tiene la posición #{index} en el arreglo"
end

contador = 1
while contador <= 5 do
  puts "iteracion #{contador}"
  contador += 1
end
#=> iteracion 1
#=> iteracion 2
#=> iteracion 3
#=> iteracion 4
#=> iteracion 5

# Hay una gran variedad de otras funciones iterativas útiles en Ruby,
# por ejemplo `map`, `reduce`,  `inject`, entre otras. Map, por ejemplo,
# toma el arreglo sobre el cuál está iterando, le hace cambios
# definidos en el bloque, y retorna un arreglo completamente nuevo.
arreglo = [1,2,3,4,5]
duplicado = array.map do |elemento|
  elemento * 2
end
puts duplicado
#=> [2,4,6,8,10]
puts array
#=> [1,2,3,4,5]

nota = 'B'

case nota
when 'A'
  puts "Muy bien muchacho"
when 'B'
  puts "Mejor suerte para la proxima"
when 'C'
  puts "Puedes hacerlo mejor"
when 'D'
  puts "Sobreviviendo"
when 'F'
  puts "¡Reprobaste!"
else
  puts "Sistema alternativo de notas, ¿eh?"
end
#=> "Mejor suerte para la proxima"

# Los casos también pueden usar rangos
nota = 82

case nota
when 90..100
  puts 'Excelente!'
when 80..100
  puts 'Buen trabajo'
else
  puts '¡Reprobaste!'
end
#=> "Buen trabajo"

# Manejo de excepciones
begin
  # código que podría causar excepción
  raise NoMemoryError, 'Se te acabó la memoria'
rescue NoMemoryError => variable_de_excepcion
  puts 'El error NoMemoryError ocurrió', variable_de_excepcion
rescue RuntimeError => otra_variable_de_excepcion
  puts 'El error RuntimeError ocurrió'
else
  puts 'Esto se ejecuta si ningun error ocurrió'
ensure
  puts 'Este código siempre se ejecuta, sin importar que'
end

# Funciones

def doble(x)
  x * 2
end

# Funciones (y todos los bloques) implícitamente retornan el valor de la última instrucción
doble(2) #=> 4

# Paréntesis son opcionales cuando el resultado no es ambiguo
doble 3 #=> 6

doble doble 3 #=> 12

def suma(x,y)
  x + y
end

# Arguméntos del método son separados por coma
suma 3, 4 #=> 7

suma suma(3,4), 5 #=> 12

# yield
# Todos los métodos tienen un parámetro bloque opcional e implícito
# puede llamarse con la palabra clave 'yield'

def alrededor
  puts "{"
  yield
  puts "}"
end

alrededor { puts 'hola mundo' }

# {
# hola mundo
# }

# Puedes pasar un bloque a una función
# '&' representa una referencia a un bloque
def visitantes(&bloque)
  bloque.call
end

# Puedes pasar una lista de argumentos, que serán convertidos en un arreglo
# Para eso sirve el operador ('*')
def visitantes(*arreglo)
  arreglo.each { |visitante| puts visitante }
end

# Define una clase con la palabra clave 'class'
class Humano

  # Una variable de clase. Es compartida por todas las instancias de la clase.
  @@species = "H. sapiens"

  # Inicializador Básico
  def initialize(nombre, edad=0)
    # Asigna el argumento a la variable de instancia 'nombre'
    @nombre = nombre
    # Si no dan edad, se usará el valor por defecto en la lista de argumentos.
    @edad = edad
  end

  # Método 'setter' (establecer) básico
  def nombre=(nombre)
    @nombre = nombre
  end

  # Método 'getter' (obtener) básico
  def nombre
    @nombre
  end

  # La funcionalidad anterior puede ser encapsulada usando el método attr_accessor
  # de la siguiente manera

  attr_accessor :name

  # Los métodos de tipo getter y setter también se pueden crear de manera individual
  # de la siguiente manera

  attr_reader :name
  attr_writer :name

  # Un método de clase usa 'self' (sí mismo) para distinguirse de métodos de instancia.
  # Sólo puede ser llamado en la clase, no por una instancia.
  def self.decir(mensaje)
    puts mensaje
  end

  def especie
    @@especie
  end
end


# Instancia una clase
jim = Humano.new("Jim Halpert")

dwight = Humano.new("Dwight K. Schrute")

# Llamemos un par de métodos
jim.especie #=> "H. sapiens"
jim.nombre #=> "Jim Halpert"
jim.nombre = "Jim Halpert II" #=> "Jim Halpert II"
jim.nombre #=> "Jim Halpert II"
dwight.especie #=> "H. sapiens"
dwight.nombre #=> "Dwight K. Schrute"

# Llama el método de clase
Humano.decir("Hi") #=> "Hi"

# El alcance de las variables es definido por la manera en que las nombramos.
# Las variables que inician con $ tienen un alcance global
$var = "Soy una variable global"
defined? $var #=> "global-variable"

# Las variables que empiezan con @ tienen un alcance de instancia
@var = "Soy una variable de instancia"
defined? @var #=> "instance-variable"

# Variables que empiezan con @@ tienen un alcance de clase
@@var = "Soy una variable de clase"
defined? @@var #=> "class variable"

# Las variables que empiezan con letra mayuscula son constantes
Var = "Soy una constante"
defined? Var #=> "constant"

# Las clases también son un objeto en ruby. Por lo cual, las clases también pueden tener variables de instancia.
# Variables de clase son compartidas a través de la clase y todos sus descendientes.

# clase base
class Humano
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(valor)
    @@foo = valor
  end
end

# clase derivada
class Trabajador < Humano
end

Humano.foo # 0
Trabajador.foo # 0

Humano.foo = 2 # 2
Trabajador.foo # 2

# Las variables de instancia de la clase no son compartidas por los descendientes de la clase.

class Humano
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(valor)
    @bar = valor
  end
end

class Doctor < Humano
end

Humano.bar # 0
Doctor.bar # nil

module ModuloEjemplo
  def foo
    'foo'
  end
end

# Al incluir un módulo sus métodos se comparten con las instancias de la clase
# Al extender un módulo sus métodos se comparten con la clase misma

class Persona
  include ModuloEjemplo
end

class Libro
  extend ModuloEjemplo
end

Persona.foo     # => NoMethodError: undefined method `foo' for Persona:Class
Persona.new.foo # => 'foo'
Libro.foo       # => 'foo'
Libro.new.foo   # => NoMethodError: undefined method `foo'

# Las llamadas de retorno (callbacks) son ejecutadas cuando se incluye o
# extiende un módulo
module EjemploConcern
  def self.incluido(base)
    base.extend(MetodosClase)
    base.send(:include, MetodosInstancia)
  end

  module MetodosClase
    def bar
      'bar'
    end
  end

  module MetodosInstancia
    def qux
      'qux'
    end
  end
end

class Algo
  include EjemploConcern
end

Algo.bar #=> 'bar'
Algo.qux #=> NoMethodError: undefined method `qux'
Algo.new.bar # => NoMethodError: undefined method `bar'
Algo.new.qux # => 'qux'
```

## Recursos adicionales
- [Aprende Ruby Mediante Ejemplo con Ejercicios](http://www.learneroo.com/modules/61/nodes/338) - Una variante de
esta referencia con ejercicios en navegador.
- [Documentación Oficial](http://www.ruby-doc.org/core-2.1.1/)
- [Ruby desde otros lenguajes](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programando Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - Una
[edición antigua](http://ruby-doc.com/docs/ProgrammingRuby/) gratuita disponible en línea.
- [Guía de estilo de Ruby](https://github.com/bbatsov/ruby-style-guide) - Guía de estilo creada por la comunidad.
