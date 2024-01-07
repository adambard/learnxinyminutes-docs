---
idioma: cristal
nombre de archivo: learncrystal-es.cr
colaboradores:
    - ["Vitalii Elenhaupt", "http://veelenga.com"]
    - ["Arnaud Fernandés", "https://github.com/TechMagister/"]
traductores:
    - ["Juan Vallejo", "https://github.com/ElCodigoDeNito"]
lang: es-es
---

```Cristal

# Este es un comentario

# Todo es objeto
nil.class # => Nil
100.clase # => Int32
true.class # => Bool

# Los valores falsos son: punteros nulos, falsos y nulos
! nil # => true: Bool
! false # => true: Bool
! 0 # => falso: Bool

# Enteros

1.clase # => Int32

# Cuatro tipos de enteros con signo
1_i8.class # => Int8
1_i16.class # => Int16
1_i32.class # => Int32
1_i64.class # => Int64

# Cuatro tipos de enteros sin signo
1_u8.class # => UInt8
1_u16.class # => UInt16
1_u32.class # => UInt32
1_u64.class # => UInt64
2147483648.clase # => Int64
9223372036854775808.class # => UInt64

# Número en base binaria
0b1101 # => 13: Int32

# Número en base octal
0o123 # => 83: Int32

# Números hexadecimales
0xFE012D # => 16646445: Int32
0xfe012d # => 16646445: Int32

# Números de comas

1.0.clase # => Float64

# Hay dos tipos de números decimales
1.0_f32.class # => Float32
1_f32.class # => Float32

1e10.class # => Float64
1.5e10.class # => Float64
1.5e-7.clase # => Float64

# Personajes

'a'.class # => Char

# Notación octal de caracteres
'\ 101' # => 'A': Char

# Notación Unicode
'\ u0041' # => 'A': Char

# Canales de personajes

"s" .class # => String

# Las cadenas son inmutables
s = "hola", # => "hola": cadena
s.object_id # => 134667712: UInt64
s + = "Crystal" # => "hola, Crystal": cadena
s.object_id # => 142528472: UInt64

# Interpolación
"sum = # {1 + 2}" # => "sum = 3": cadena

# Cadenas multilínea
"Esta es una cadena de
varias líneas "

# Otra notación para cadenas de caracteres
# que permite insertar comillas
% (hola "mundo") # => "hola \" mundo \ ""

# Símbolos
# Son inmutables y reutilizables, están representados internamente por
# un Int32. A menudo se usan en lugar de cadenas
# cuando la identidad es más importante que el contenido

: symbol.class # => Símbolo

oración =: pregunta? # :"¿pregunta?" : Símbolo

oración ==: pregunta? # => verdadero: Bool
oración ==: exclamación! # => falso: Bool
oración == "pregunta?" # => falso: Bool

# Tablas

[1, 2, 3] .class # => Array (Int32)
[1, "hola", 'x']. Clase # => Matriz (Int32 | String | Char)

# Se debe especificar un tipo para tablas vacías
[] # Error de sintaxis: para matrices vacías use '[] de ElementType'
[] de Int32 # => []: matriz (Int32)
Array (Int32) .new # => []: Array (Int32)

# Las matrices se pueden indexar
array = [1, 2, 3, 4, 5] # => [1, 2, 3, 4, 5]: matriz (Int32)
array [0] # => 1: Int32
array [10] # arroja la excepción IndexError
array [-6] # arroja la excepción IndexError
array [10]? # => nulo: (Int32 | Nulo)
array [-6]? # => nulo: (Int32 | Nulo)# Desde el final
matriz [-1] # => 5

# Con un índice de inicio y tamaño
matriz [2, 3] # => [3, 4, 5]

# O con un intervalo
matriz [1..3] # => [2, 3, 4]

# La adición a una tabla se puede hacer con el operador <<
matriz << 6 # => [1, 2, 3, 4, 5, 6]

# Elimina la última entrada
array.pop # => 6
matriz # => [1, 2, 3, 4, 5]

# Elimina la primera entrada
array.shift # => 1
matriz # => [2, 3, 4, 5]

# Compruebe si hay un elemento presente en la matriz
array.includes? 3 # => verdadero

# Sintaxis especial para una serie de cadenas de caracteres o símbolos
% w (uno dos tres) # => ["uno", "dos", "tres"]: matriz (cadena)
% i (uno dos tres) # => [: uno ,: dos ,: tres]: Matriz (Símbolo)

# Existe una sintaxis especial para matrices y otros tipos
# siempre que definan un método .new y # <<
set = Set {1, 2, 3} # => [1, 2, 3]
set.class # => Set (Int32)

# Lo que está arriba es equivalente a:
set = Set (typeof (1, 2, 3)) nuevo
establecer << 1
conjunto << 2
conjunto << 3# Tablas asociativas

{1 => 2, 3 => 4} .clase # => Hash (Int32, Int32)
{1 => 2, 'a' => 3} .class # => Hash (Int32 | Char, Int32)

# Se debe especificar un tipo para matrices asociativas vacías
{} # Error de sintaxis
{} de Int32 => Int32 # {}
Hash (Int32, Int32) .nuevo # {}

# El acceso es a través de una clave
hash = {"color" => "verde", "número" => 5}
hash ["color"] # => "verde"
hash ["no_such_key"] # => Falta la clave hash: "no_such_key" (KeyError)
desmenuzar [ "no_such_key"]? # => nulo

# Compruebe si hay una clave
hash.has_key? "color" # => verdadero

# Notación especial para teclas si son símbolos
# o cadenas
{clave1: 'a', clave2: 'b'} # {: clave1 => 'a' ,: clave2 => 'b'}
{"key1": 'a', "key2": 'b'} # {"key1" => 'a', "key2" => 'b'}

# Igual que para matrices simples, sintaxis especial
# existe mientras el tipo defina un método .new y # [] =
clase MyType
  def [] = (clave, valor)
    pone "hacer cosas"
  final
final

MyType {"foo" => "bar"}

# ¿Qué es equivalente a:
tmp = MyType.new
tmp ["foo"] = "bar"
tmp

# Intervalo

1..10 # => Rango (Int32, Int32)
Range.new (1, 10) .class # => Range (Int32, Int32)

# Pueden ser inclusivos o exclusivos
(3..5) .to_a # => [3, 4, 5]
(3 ... 5) .to_a # => [3, 4]

# Verificar si un intervalo contiene un valor
(1..8) · Incluye? 2 # => verdadero
# las tuplas son de tamaño fijo, inmutables y asignadas en la pila
{1, "hola", "x"}. Clase # => Tupla (Int32, String, Char)

# El acceso se puede hacer usando un índice
tupla = {: clave1 ,: clave2}
tupla [1] # =>: clave2
tupla [2] # => error de sintaxis: índice fuera del límite

# Se pueden dividir en varias variables
a, b, c = {: a, 'b', "c"}
a # =>: a
b # => 'b'
c # => "c"

# Los procedimientos (Proc) son punteros de función
# con un contexto opcional. Por lo general, se crean con
# esta notación:
proc = -> (x: Int32) {x.to_s}
proc.class # Proc (Int32, String)
# O usando el nuevo método
Proc (Int32, String) .new {| x | x.to_s}

# Los invocamos con el método de llamada
llamada a proceso 10 # => "10"

# Control de flujo

si es verdad
  "si la declaración"
elsif falso
  "else-if, opcional"
otro
  "más, también opcional"
final

pone "si como sufijo" si es verdadero

# El if (if) se puede usar para una declaración
a = si 2> 1
      3
    otro
      4
    final

a # => 3

# Operador ternario
a = 1> 2? 3: 4 # => 4

# Referencias usando la palabra clave "case"
cmd = "mover"

action = box cmd
  cuando "crear"
    "Crear ..."
  cuando "copiar"
    "Copiando ..."
  cuando "mover"
    "Moving ..."
  cuando "borrar"
    "Eliminación de ..."
final

action # => "Moviendo ..."# Loop
índice = 0
mientras que el índice <= 3
  pone "Índice: # {index}"
  índice + = 1
final
# Índice: 0
# Índice: 1
# Índice: 2
# Índice: 3

índice = 0
hasta índice> 3
  pone "Índice: # {index}"
  índice + = 1
final
# Índice: 0
# Índice: 1
# Índice: 2
# Índice: 3

# Pero es mejor usar #cada uno
(1..3) .cada do | index |
  pone "Índice: # {index}"
final
# Índice: 1
# Índice: 2
# Índice: 3

# El tipo de una variable depende del tipo de expresión
# en la declaración del if
si a <3
  a = "hola"
otro
  a = verdadero
final
typeof a # => (Bool | String)

si a && b
  # aquí ayb no son nulos
final

si a.is_a? cadena
  a.class # => String
final

# Funciones

doble def (x)
  x * 2
final

# Las funciones y todos los bloques devuelven el valor de la última evaluación
doble (2) # => 4

# Los paréntesis son opcionales cuando la llamada no es ambigua
doble 3 # => 6

doble doble 3 # => 12

suma def (x, y)
  x + y
final

# Los argumentos están separados por una coma
suma 3, 4 # => 7

suma total (3, 4), 5 # => 12

# rendimiento
# Todos los métodos tienen un parámetro opcional y un tipo de bloque implícito
# se puede llamar con la palabra clave 'rendimiento'
def surround
  pone '{'
  rendimiento
  pone '}'
finalsurround {pone "hola mundo"}

# {
# hola mundo
#}

# Se puede pasar un bloque a una función
# El "&" marca una referencia a un bloque
def invitados (y bloque)
  block.call "some_argument"
final

# Se puede dar una lista de argumentos, que se convertirán en una matriz
# Para hacer esto, use el operador "*"
def invitados (* array)
  array.each {| guest | pone invitado
final

# Si un método devuelve una matriz, se puede dividir
def alimentos
    ["panqueque", "emparedado", "quesadilla"]
final
desayuno, almuerzo, cena = comidas
desayuno # => "panqueque"
cena # => "quesadilla"

# Por convención, todos los métodos que devuelven un valor booleano
# termina con un signo de interrogación
5.even? # falso
5.odd? # verdadero

# Si un método termina con un signo de exclamación, significa que es
# operación destructiva. Algunos métodos tienen una versión "!" para hacer
# cambios y un no "!" para devolver una nueva versión
company_name = "Dunder Mifflin"
company_name.gsub "Dunder", "Donald" # => "Donald Mifflin"
company_name # => "Dunder Mifflin"
company_name.gsub! "Dunder", "Donald"
nombre_empresa # => "Donald Mifflin"


# Las clases se definen con la palabra clave "clase"
clase humana

  # Una variable de clase, compartida por todas las instancias
  @@ especies = "H. sapiens"

  # "nombre" es una cadena de caracteres (Cadena)
  @name: String

  # Constructor básico, asigna el argumento a la variable "nombre"
  # si no se especifica la edad, su valor será 0
  def inicializar (@name, @age = 0)
  final

  # Mutador
  def nombre = (nombre)
    @name = name
  final

  # Accesor
  nombre def
    @Name
  final

  # La macro "propiedad" generará los dos métodos anteriores
  propiedad: nombre

  # Los accesores / mutadores también se pueden crear individualmente
  getter: nombre
  setter: nombre

  # Un método de clase usa "self" para distinguirse de un
  # método de instancia. Ella solo puede ser llamada desde la clase
  def self.say (msg)
    pone msg
  final

  def especies
    @@ especies
  final
final


# Instanciar una clase
jim = Human.new ("Jim Halpert")

dwight = Human.new ("Dwight K. Schrute")

# Llamemos algunos métodos
jim.species # => "H. sapiens"
jim.name # => "Jim Halpert"
jim.name = "Jim Halpert II" # => "Jim Halpert II"
jim.name # => "Jim Halpert II"
dwight.species # => "H. sapiens"
dwight.name # => "Dwight K. Schrute"# Llamada del método de clase
Human.say ("Hola") # => Muestra "Hola" y devuelve nulo

# Las variables que comienzan con @ tienen un alcance de instancia
clase TestClass
  @var = "Soy una variable de instancia"
final

# Las variables que comienzan con @@ tienen un alcance de clase
clase TestClass
  @@ var = "Soy una variable de clase"
final
# Las constantes comienzan con mayúscula
Var = "Soy constante"
Var = "imposible" # constante ya inicializada Var

# La clase también es un objeto
# Las variables de clase se comparten con descendientes

# Clase básica
clase humana
  @@ foo = 0

  def self.foo
    @@ foo
  final

  def self.foo = (valor)
    @@ foo = valor
  final
final

# Clase derivada
Trabajador de clase <Humano
final

Human.foo # => 0
Worker.foo # => 0

Human.foo = 2 # => 2
Worker.foo # => 0

Worker.foo = 3 # => 3
Human.foo # => 2
Worker.foo # => 3

Módulo Módulo de ejemplo
  def foo
    "Foo"
  final
final

# Los módulos de inclusión (inclusión) agregan sus métodos a las instancias
# Extender agrega los métodos a la clase

Persona de clase
  incluye ModuleExample
final

Libro de clase
  extender ModuleExample
final

Person.foo # => método indefinido 'foo' para Persona: Clase
Person.new.foo # => 'foo'
Book.foo # => 'foo'
Book.new.foo # => método indefinido 'foo' para el Libro


# Manejo de excepciones

# Define un tipo de excepción
clase MyException <Excepción
final

# Definir otra excepción
clase MyAnotherException <Excepción; final

ex = comenzar
   elevar MyException.new
rescate ex1: IndexError
  "Ex1"
rescate ex2: MyException | MyAnotherException
  "Ex2"
rescate ex3: excepción
  "EX3"
Rescate ex4 # captura todas las demás excepciones
  "Ex4"
final

ex # => "ex2"

```

## Recursos adicionales

- [Documentación oficial (EN)] (http://crystal-lang.org/)