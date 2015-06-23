---
language: ruby
filename: learnruby-es.rb
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
translators:
    - ["Camilo Garrido", "http://www.twitter.com/hirohope"]
lang: es-es
---

```ruby
# Esto es un comentario

=begin
Este es un comentario multilínea
Nadie los usa.
Tu tampoco deberías
=end

# Lo primero y principal: Todo es un objeto

# Los números son objetos

3.class #=> Fixnum

3.to_s #=> "3"


# Un poco de aritmética básica
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

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
!true  #=> false
!false #=> true

# Además de 'false', 'nil' es otro valor falso

!nil   #=> true
!false #=> true
!0     #=> false

# Más comparaciones
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Los strings son objetos

'Soy un string'.class #=> String
"Soy un string también".class #=> String

referente = "usar interpolacion de strings"
"Yo puedo #{referente} usando strings de comillas dobles"
#=> "Yo puedo usar interpolacion de strings usando strings de comillas dobles"


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
# valor entero. Son usalmente usados en vez de strings para expresar eficientemente
# valores específicos y significativos

:pendiente.class #=> Symbol

status = :pendiente

status == :pendiente #=> true

status == 'pendiente' #=> false

status == :aprovado #=> false

# Arreglos

# Esto es un arreglo
[1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Arreglos pueden contener elementos de distintos tipos

arreglo = [1, "hola", false] #=> => [1, "hola", false]

# Arreglos pueden ser indexados
# Desde el frente
arreglo[0] #=> 1
arreglo[12] #=> nil

# Tal como la aritmética, el acceso como variable[índice]
# es sólo azúcar sintáctica
# para llamar el método [] de un objeto
arreglo.[] 0 #=> 1
arreglo.[] 12 #=> nil

# Desde el final
arreglo[-1] #=> 5

# Con un índice de inicio y final
arreglo[2, 4] #=> [3, 4, 5]

# O con rango
arreglo[1..3] #=> [2, 3, 4]

# Añade elementos a un arreglo así
arreglo << 6 #=> [1, 2, 3, 4, 5, 6]

# Hashes son los diccionarios principales de Ruby con pares llave/valor.
# Hashes se denotan con llaves:
hash = {'color' => 'verde', 'numero' => 5}

hash.keys #=> ['color', 'numero']

# Hashes pueden buscar rápidamente una llave:
hash['color'] #=> 'verde'
hash['numero'] #=> 5

# Preguntarle a un hash por una llave que no existe retorna 'nil':
hash['nada aqui'] #=> nil

# Itera sobre un hash con el método 'each':
hash.each do |k, v|
  puts "#{k} is #{v}"
end

# Desde Ruby 1.9, hay una sintaxis especial cuando se usa un símbolo como llave:

nuevo_hash = { defcon: 3, accion: true}

nuevo_hash.keys #=> [:defcon, :accion]

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

# Aunque
# Nadie usa los ciclos `for`
# Usa `each`, así:

(1..5).each do |contador|
  puts "iteracion #{contador}"
end
#=> iteracion 1
#=> iteracion 2
#=> iteracion 3
#=> iteracion 4
#=> iteracion 5

counter = 1
while counter <= 5 do
  puts "iteracion #{counter}"
  counter += 1
end
#=> iteracion 1
#=> iteracion 2
#=> iteracion 3
#=> iteracion 4
#=> iteracion 5

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

# Funciones

def doble(x)
  x * 2
end

# Funciones (y todos los bloques) implícitamente retornan el valor de la última instrucción
doble(2) #=> 4

# Paréntesis son opcionales cuando el resultado es ambiguo
doble 3 #=> 6

doble doble 3 #=> 12

def suma(x,y)
  x + y
end

# Arguméntos del método son separados por coma
suma 3, 4 #=> 7

suma suma(3,4), 5 #=> 12

# yield
# Todos los métodos tienen un parámetro de bloqueo opcional e implícitp
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

  # Un método de clase usa 'self' (sí mismo) para distinguirse de métodos de instancia.
  # Sólo puede ser llamado en la clase, no por una instancia.
  def self.decir(mensaje)
    puts "#{mensaje}"
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

Human.bar # 0
Doctor.bar # nil

```
