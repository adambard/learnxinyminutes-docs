---
language: julia
contributors:
    - ["Leah Hanson", "http://leahhanson.us"]
    - ["Guillermo Garza" ]
filename: learnjulia-es.jl
lang: es-es
---

Julia es un nuevo lenguaje funcional homoiconic enfocado en computación técnica.
Aunque que tiene todo el poder de macros homoiconic, funciones de primera
clase, y control de bajo nivel, Julia es tan fácil de aprender y utilizar como
Python.

Esto se basa en la versión de desarrollo actual de Julia, del 18 de octubre de
2013.

```ruby

# Comentarios de una línea comienzan con una almohadilla (o signo gato)

#= Commentarios multilinea pueden escribirse
   usando '#=' antes de el texto  y '=#' 
   después del texto. También se pueden anidar.
=#

####################################################
## 1. Tipos de datos primitivos y operadores.
####################################################

# Todo en Julia es una expresión.

# Hay varios tipos básicos de números.
3 # => 3 (Int64)
3.2 # => 3.2 (Float64)
2 + 1im # => 2 + 1im (Complex{Int64})
2//3 # => 2//3 (Rational{Int64})

# Todos los operadores infijos normales están disponibles.
1 + 1 # => 2
8 - 1 # => 7
10 * 2 # => 20
35 / 5 # => 7.0
5/2 # => 2.5 # dividir un Int por un Int siempre resulta en un Float
div (5, 2) # => 2 # para un resultado truncado, usa div
5 \ 35 # => 7.0
2 ^ 2 # => 4 # exponente, no es xor
12 % 10 # => 2

# Refuerza la precedencia con paréntesis
(1 + 3) * 2 # => 8

# Operadores a nivel de bit
~2 # => -3   # bitwise not
3 & 5 # => 1 # bitwise and
2 | 4 # => 6 # bitwise or
2 $ 4 # => 6 # bitwise xor
2 >>> 1 # => 1 # logical shift right
2 >> 1  # => 1 # arithmetic shift right
2 << 1  # => 4 # logical/arithmetic shift left

# Se puede utilizar la función bits para ver la representación binaria de un
# número.
bits(12345)
# => "0000000000000000000000000000000000000000000000000011000000111001"
bits(12345.0)
# => "0100000011001000000111001000000000000000000000000000000000000000"

# Valores 'boolean' (booleanos) son primitivos
true
false

# Operadores Boolean (booleanos)
!true # => false
!false # => true
1 == 1 # => true
2 == 1 # => false
1 != 1 # => false
2 != 1 # => true
1 < 10 # => true
1 > 10 # => false
2 <= 2 # => true
2 >= 2 # => true
# ¡Las comparaciones pueden ser concatenadas!
1 < 2 < 3 # => true
2 < 3 < 2 # => false

# Strings se crean con " 
"Esto es un string."

# Literales de caracteres se escriben con '
'a'

# Una string puede ser indexado como una array de caracteres
"Esto es un string."[1] # => 'E' # Índices en Julia empiezen del 1
# Sin embargo, esto no va a funcionar bien para strings UTF8, 
# Lo que se recomienda es la iteración (map, for, etc).

# $ puede ser utilizado para la interpolación de strings:
"2 + 2 = $(2 + 2)" # => "2 + 2 = 4"
# Se puede poner cualquier expresión de Julia dentro los paréntesis.

# Otro forma de formatear strings es el macro printf
@printf "%d es menor de %f" 4.5 5.3 # 5 es menor de 5.300000

# Imprimir es muy fácil
println("Soy Julia. ¡Encantado de conocerte!")

####################################################
## 2. Variables y Colecciones
####################################################

# No hay necesidad de declarar las variables antes de asignarlas.
una_variable = 5 # => 5
una_variable # => 5

# Acceder a variables no asignadas previamente es una excepción.
try
    otra_variable # => ERROR: some_other_var not defined
catch e
    println(e)
end

# Los nombres de variables comienzan con una letra.
# Después de eso, puedes utilizar letras, dígitos, guiones y signos de
# exclamación.
OtraVariable123! = 6 # => 6

# También puede utilizar caracteres unicode
☃ = 8 # => 8
# Estos son especialmente útiles para la notación matemática
2 * π # => 6.283185307179586

# Una nota sobre las convenciones de nomenclatura de Julia:
#
# * Los nombres de las variables aparecen en minúsculas, con separación de 
#   palabra indicado por underscore ('\ _'). 
#
# * Los nombres de los tipos comienzan con una letra mayúscula y separación de
#   palabras se muestra con CamelCase en vez de underscore.
#
# * Los nombres de las funciones y los macros están en minúsculas, sin
#   underscore.
#
# * Funciones que modifican sus inputs tienen nombres que terminan en!. Estos 
#   funciones a veces se llaman mutating functions o in-place functions.

# Los Arrays almacenan una secuencia de valores indexados entre 1 hasta n
a = Int64[] # => 0-element Int64 Array

# Literales de arrays 1-dimensionales se pueden escribir con valores separados
# por comas.
b = [4, 5, 6] # => 3-element Int64 Array: [4, 5, 6]
b[1] # => 4
b[end] # => 6

# Los arrays 2-dimensionales usan valores separados por espacios y filas
# separados por punto y coma.
matrix = [1 2; 3 4] # => 2x2 Int64 Array: [1 2; 3 4]

# Añadir cosas a la final de una lista con push! y append!
push!(a,1)     # => [1]
push!(a,2)     # => [1,2]
push!(a,4)     # => [1,2,4]
push!(a,3)     # => [1,2,4,3]
append!(a,b) # => [1,2,4,3,4,5,6]

# Eliminar de la final con pop
pop!(b)        # => 6 y b ahora es [4,5]

# Vamos a ponerlo de nuevo 
push!(b, 6)   # b es ahora [4,5,6] de nuevo.

a[1] # => 1 # recuerdan que los índices de Julia empiezan desde 1, no desde 0!

# end es una abreviatura para el último índice. Se puede utilizar en cualquier 
# expresión  de indexación
a[end] # => 6

# tambien hay shift y unshift
shift!(a) # => 1 y a es ahora [2,4,3,4,5,6]
unshift!(a,7) # => [7,2,4,3,4,5,6]

# Nombres de funciónes que terminan en exclamaciones indican que modifican 
# su argumento.
arr = [5,4,6] # => 3-element Int64 Array: [5,4,6]
sort(arr) # => [4,5,6]; arr es todavía [5,4,6]
sort!(arr) # => [4,5,6]; arr  es ahora [4,5,6]

# Buscando fuera de límites es un BoundsError
try
    a[0] # => ERROR: BoundsError() in getindex at array.jl:270
    a[end+1] # => ERROR: BoundsError() in getindex at array.jl:270
catch e
    println(e)
end

# Errors dan la línea y el archivo de su procedencia, aunque sea en el standard
# library. Si construyes Julia de source, puedes buscar en el source para
# encontrar estos archivos.

# Se puede inicializar arrays de un range
a = [1:5] # => 5-element Int64 Array: [1,2,3,4,5]

# Puedes mirar en ranges con sintaxis slice.
a[1:3] # => [1, 2, 3]
a[2:end] # => [2, 3, 4, 5]

# Eliminar elementos de una array por índice con splice!
arr = [3,4,5]
splice!(arr,2) # => 4 ; arr es ahora [3,5]

# Concatenar listas con append!
b = [1,2,3]
append!(a,b) # ahroa a es [1, 2, 3, 4, 5, 1, 2, 3]

# Comprueba la existencia en una lista con in
in(1, a) # => true

# Examina la longitud con length
length(a) # => 8

# Tuples son immutable.
tup = (1, 2, 3) # => (1,2,3) # un (Int64,Int64,Int64) tuple.
tup[1] # => 1
try:
    tup[1] = 3 # => ERROR: no method setindex!((Int64,Int64,Int64),Int64,Int64)
catch e
    println(e)
end

# Muchas funciones de lista también trabajan en las tuples
length(tup) # => 3
tup[1:2] # => (1,2)
in(2, tup) # => true

# Se puede desempacar tuples en variables
a, b, c = (1, 2, 3) # => (1,2,3)  # a is now 1, b is now 2 and c is now 3

# Los tuples se crean, incluso si se omite el paréntesis
d, e, f = 4, 5, 6 # => (4,5,6)

# Un tuple 1-elemento es distinto del valor que contiene
(1,) == 1 # => false
(1) == 1 # => true

# Mira que fácil es cambiar dos valores
e, d = d, e  # => (5,4) # d is now 5 and e is now 4


# Dictionaries almanecan mapeos
dict_vacio = Dict() # => Dict{Any,Any}()

# Se puede crear un dictionary usando un literal
dict_lleno = ["one"=> 1, "two"=> 2, "three"=> 3]
# => Dict{ASCIIString,Int64}

# Busca valores con []
dict_lleno["one"] # => 1

# Obtén todas las claves 
keys(dict_lleno)
# => KeyIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# Nota - claves del dictionary no están ordenados ni en el orden en que se
# insertan.

# Obtén todos los valores
values(dict_lleno)
# => ValueIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# Nota - Igual que el anterior en cuanto a ordenamiento de claves.

# Compruebe si hay existencia de claves en un dictionary con in y haskey
in(("uno", 1), dict_lleno) # => true
in(("tres", 3), dict_lleno) # => false
haskey(dict_lleno, "one") # => true
haskey(dict_lleno, 1) # => false

# Tratando de buscar una clave que no existe producirá un error
try
    dict_lleno["dos"] # => ERROR: key not found: dos in getindex at dict.jl:489
catch e
    println(e)
end

# Utilice el método get para evitar ese error proporcionando un valor
# predeterminado
# get(dictionary,key,default_value)
get(dict_lleno,"one",4) # => 1
get(dict_lleno,"four",4) # => 4

# Usa Sets para representar colecciones (conjuntos) de valores únicos, no
# ordenadas
conjunto_vacio = Set() # => Set{Any}()
# Iniciar una set de valores
conjunto_lleno = Set(1,2,2,3,4) # => Set{Int64}(1,2,3,4)

# Añadir más valores a un conjunto
push!(conjunto_lleno,5) # => Set{Int64}(5,4,2,3,1)
push!(conjunto_lleno,5) # => Set{Int64}(5,4,2,3,1)

# Compruebe si los valores están en el conjunto
in(2, conjunto_lleno) # => true
in(10, conjunto_lleno) # => false

# Hay funciones de intersección de conjuntos, la unión, y la diferencia.
conjunto_otro= Set(3, 4, 5, 6) # => Set{Int64}(6,4,5,3)
intersect(conjunto_lleno, conjunto_otro) # => Set{Int64}(3,4,5)
union(conjunto_lleno, conjunto_otro) # => Set{Int64}(1,2,3,4,5,6)
setdiff(Set(1,2,3,4),Set(2,3,5)) # => Set{Int64}(1,4)


####################################################
## 3. Control de Flujo
####################################################

# Hagamos una variable
una_variable = 5

# Aquí está una declaración de un 'if'. La indentación no es significativa en
# Julia
if una_variable > 10
    println("una_variable es completamente mas grande que 10.")
elseif una_variable < 10    # Este condición 'elseif' es opcional.
    println("una_variable es mas chica que 10.")
else                    # Esto también es opcional.
    println("una_variable es de hecho 10.")
end
# => imprime "una_variable es mas chica que 10."

# For itera sobre tipos iterables
# Tipos iterables incluyen Range, Array, Set, Dict, y String.
for animal=["perro", "gato", "raton"]
    println("$animal es un mamifero")
    # Se puede usar $ para interpolar variables o expresiónes en strings
end
# imprime:
#    perro es un mamifero
#    gato es un mamifero
#    raton es un mamifero

for a in ["perro"=>"mamifero","gato"=>"mamifero","raton"=>"mamifero"]
    println("$(a[1]) es un $(a[2])")
end
# imprime:
#    perro es un mamifero
#    gato es un mamifero
#    raton es un mamifero

for (k,v) in ["perro"=>"mamifero", "gato"=>"mamifero", "raton"=>"mamifero"]
    println("$k es un $v")
end
# imprime:
#    perro es un mamifero
#    gato es un mamifero
#    raton es un mamifero

# While itera hasta que una condición no se cumple.
x = 0
while x < 4
    println(x)
    x += 1  # versión corta de x = x + 1
end
# imprime:
#   0
#   1
#   2
#   3

# Maneja excepciones con un bloque try/catch
try
   error("ayuda")
catch e
   println("capturando $e")
end
# => capturando ErrorException("ayuda")


####################################################
## 4. Funciones
####################################################

# Usa 'function' para crear nuevas funciones

#function nombre(arglist)
#  cuerpo...
#end
function suma(x, y)
    println("x es $x e y es $y")

    # Las funciones devuelven el valor de su última declaración
    x + y
end

suma(5, 6) # => 11 # después de imprimir "x es 5 e y es de 6"

# Puedes definir funciones que toman un número variable de
# argumentos posicionales
function varargs(args...)
    return args
    # Usa la palabra clave return para devolver en cualquier lugar de la función
end
# => varargs (generic function with 1 method)

varargs(1,2,3) # => (1,2,3)

# El ... se llama un splat.
# Acabamos de utilizar lo en una definición de función.
# También se puede utilizar en una llamada de función,
# donde va splat un Array o el contenido de un Tuple en la lista de argumentos.
Set([1,2,3])    # => Set{Array{Int64,1}}([1,2,3]) # Produce un Set de Arrays
Set([1,2,3]...) # => Set{Int64}(1,2,3) # esto es equivalente a Set(1,2,3)

x = (1,2,3)     # => (1,2,3)
Set(x)          # => Set{(Int64,Int64,Int64)}((1,2,3)) # un Set de Tuples
Set(x...)       # => Set{Int64}(2,3,1)


# Puede definir funciones con argumentos posicionales opcionales
function defaults(a,b,x=5,y=6)
    return "$a $b y $x $y"
end

defaults('h','g') # => "h g y 5 6"
defaults('h','g','j') # => "h g y j 6"
defaults('h','g','j','k') # => "h g y j k"
try
    defaults('h') # => ERROR: no method defaults(Char,)
    defaults() # => ERROR: no methods defaults()
catch e
    println(e)
end

# Puedes definir funciones que toman argumentos de palabra clave
function args_clave(;k1=4,nombre2="hola") # note the ;
    return ["k1"=>k1,"nombre2"=>nombre2]
end

args_clave(nombre2="ness") # => ["nombre2"=>"ness","k1"=>4]
args_clave(k1="mine") # => ["k1"=>"mine","nombre2"=>"hola"]
args_clave() # => ["nombre2"=>"hola","k1"=>4]

# Puedes combinar todo tipo de argumentos en la misma función
function todos_los_args(arg_normal, arg_posicional_opcional=2; arg_clave="foo")
    println("argumento normal: $arg_normal")
    println("argumento optional: $arg_posicional_opcional")
    println("argumento de clave: $arg_clave")
end

todos_los_args(1, 3, arg_clave=4)
# imprime:
#   argumento normal: 1
#   argumento optional: 3
#   argumento de clave: 4

# Julia tiene funciones de primera clase
function crear_suma(x)
    suma = function (y)
        return x + y
    end
    return suma
end

# Esta es el sintaxis "stabby lambda" para crear funciones anónimas
(x -> x > 2)(3) # => true

# Esta función es idéntica a la crear_suma implementación anterior.
function crear_suma(x)
    y -> x + y
end

# También puedes nombrar la función interna, si quieres
function crear_suma(x)
    function suma(y)
        x + y
    end
    suma
end

suma_10 = crear_suma(10)
suma_10(3) # => 13


# Hay funciones integradas de orden superior
map(suma_10, [1,2,3]) # => [11, 12, 13]
filter(x -> x > 5, [3, 4, 5, 6, 7]) # => [6, 7]

# Podemos usar listas por comprensión para mapeos
[suma_10(i) for i=[1, 2, 3]] # => [11, 12, 13]
[suma_10(i) for i in [1, 2, 3]] # => [11, 12, 13]

####################################################
## 5. Tipos
####################################################

# Julia tiene sistema de tipos.
# Cada valor tiene un tipo y las variables no tienen propios tipos.
# Se puede utilizar la función `typeof` para obtener el tipo de un valor.
typeof(5) # => Int64

# Los tipos son valores de primera clase
typeof(Int64) # => DataType
typeof(DataType) # => DataType
# DataType es el tipo que representa los tipos, incluyéndose a sí mismo.

# Los tipos se usan para la documentación, optimizaciones, y envio.
# No están comprobados estáticamente.

# Los usuarios pueden definir tipos 
# Son como registros o estructuras en otros idiomas. 
# Nuevos tipos se definen utilizado la palabra clave `type`.

# type Nombre
#   field::OptionalType
#   ...
# end
type Tigre
  longituddecola::Float64
  colordelpelaje # no incluyendo una anotación de tipo es el mismo que `::Any`
end

# Los argumentos del constructor por default son las propiedades 
# del tipo, en el orden en que están listados en la definición
tigger = Tigre(3.5,"anaranjado") # => Tiger(3.5,"anaranjado")

# El tipo funciona como la función constructora de valores de ese tipo
sherekhan = typeof(tigger)(5.6,"fuego") # => Tiger(5.6,"fuego")


# Este estilo de tipos son llamados tipos concrete 
# Se pueden crear instancias, pero no pueden tener subtipos. 
# La otra clase de tipos es tipos abstractos (abstract types).

# abstract Nombre
abstract Gato # sólo un nombre y un punto en la jerarquía de tipos

# De los tipos Abstract no se pueden crear instancias, pero pueden tener
# subtipos.  Por ejemplo, Number es un tipo abstracto.
subtypes(Number) # => 6-element Array{Any,1}:
                 #     Complex{Float16}
                 #     Complex{Float32}
                 #     Complex{Float64}
                 #     Complex{T<:Real}
                 #     Real
subtypes(Gato) # => 0-element Array{Any,1}

# Cada tipo tiene un supertipo, utilice la función `súper` para conseguirlo.
typeof(5) # => Int64
super(Int64) # => Signed
super(Signed) # => Real
super(Real) # => Number
super(Number) # => Any
super(super(Signed)) # => Number
super(Any) # => Any
# Todo de estos tipos, a excepción de Int64, son abstractos.

# <: es el operador de subtipos
type Leon <: Gato # Leon es un subtipo de Gato
  color_de_crin
  rugido::String
end

# Se puede definir más constructores para su tipo.
# Sólo defina una función del mismo nombre que el tipo
# y llame a un constructor existente para obtener un valor del tipo correcto
Leon(rugido::String) = Leon("verde",rugido)
# Este es un constructor externo porque es fuera de la definición del tipo

type Pantera <: Gato # Pantera tambien es un a subtipo de Cat
  color_de_ojos
  Pantera() = new("verde")
    # Panteras sólo tendrán este constructor, y ningún constructor
    # predeterminado.
end
# Utilizar constructores internos, como Panther hace, te da control sobre cómo
# se pueden crear valores del tipo.  Cuando sea posible, debes utilizar
# constructores exteriores en lugar de los internos.

####################################################
## 6.  Envio múltiple
####################################################

# En Julia, todas las funciones nombradas son funciones genéricas.
# Esto significa que se construyen a partir de muchos métodos pequeños 
# Cada constructor de Leon es un método de la función genérica Leon.

# Por ejemplo, vamos a hacer un maullar función:

# Definiciones para Leon, Pantera, y Tigre
function maullar(animal::Leon)
  animal.rugido # acceso utilizando notación de puntos
end

function maullar(animal::Pantera)
  "grrr"
end

function maullar(animal::Tigre)
  "rawwwr"
end

# Prueba de la función maullar
maullar(tigger) # => "rawwr"
maullar(Leon("cafe","ROAAR")) # => "ROAAR"
maullar(Pantera()) # => "grrr"

# Revisar la jerarquía de tipos locales
issubtype(Tigre,Gato) # => false
issubtype(Leon,Gato) # => true
issubtype(Pantera,Gato) # => true

# Definición de una función que toma Gatos
function mascota(gato::Gato)
  println("El gato dice $(maullar(gato))")
end

mascota(Leon("42")) # => imprime "El gato dice 42"
try
    mascota(tigger) # => ERROR: no method mascota(Tigre))
catch e
    println(e)
end

# En los lenguajes orientados a objetos, expedición única es común. Esto
# significa que el método se recogió basándose en el tipo del primer argumento.
# En Julia, todos los tipos de argumentos contribuyen a seleccionar el mejor
# método.

# Vamos a definir una función con más argumentos, para que podamos ver la
# diferencia
function pelear(t::Tigre,c::Gato)
  println("¡El tigre $(t.colordelpelaje) gana!")
end
# => pelear (generic function with 1 method)

pelear(tigger,Pantera()) # => imprime ¡El tigre anaranjado gana!
pelear(tigger,Leon("ROAR")) # => ¡El tigre anaranjado gana!

# Vamos a cambiar el comportamiento cuando el Gato es específicamente un Leon
pelear(t::Tigre,l::Leon) = println("El león con melena $(l.color_de_crin) gana")
# => pelear (generic function with 2 methods)

pelear(tigger,Pantera()) # => imprime ¡El tigre anaranjado gana!
pelear(tigger,Leon("ROAR")) # => imprime El león con melena verde gana

# No necesitamos un tigre para poder luchar
pelear(l::Leon,c::Gato) = println("El gato victorioso dice $(maullar(c))")
# => fight (generic function with 3 methods)

pelear(Leon("balooga!"),Pantera()) # => imprime El gato victorioso dice grrr
try
  pelear(Pantera(),Leon("RAWR")) # => ERROR: no method pelear(Pantera, Leon))
catch
end

# Un metodo con el gato primero
pelear(c::Gato,l::Leon) = println("El gato le gana al León")
# Warning: New definition
#     pelear(Gato,Leon) at none:1
# is ambiguous with:
#     pelear(Leon,Gato) at none:1.
# To fix, define
#     pelear(Leon,Leon)
# before the new definition.
# pelear (generic function with 4 methods)

# Esta advertencia se debe a que no está claro que metodo de pelear será llamado
# en:
pelear(Leon("RAR"),Leon("cafe","rar")) # => imprime El gato victorioso dice rar
# El resultado puede ser diferente en otras versiones de Julia

pelear(l::Leon,l2::Leon) = println("Los leones llegan a un empate")
pelear(Leon("GR"),Leon("cafe","rar")) # => imprime Los leones llegan a un empate


# Un vistazo al nivel bajo
# Se puede echar un vistazo a la LLVM y el código ensamblador generado.

area_cuadrada(l) = l * l      # area_cuadrada (generic function with 1 method)

area_cuadrada(5) # => 25

# ¿Qué sucede cuando damos area_cuadrada diferentes argumentos?
code_native(area_cuadrada, (Int32,))  
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1              # Prologue
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    movsxd  RAX, EDI        # Fetch l from memory?
	#	    imul    RAX, RAX        # Square l and store the result in RAX
	#	    pop RBP                 # Restore old base pointer
	#	    ret                     # Result will still be in RAX

code_native(area_cuadrada, (Float32,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vmulss  XMM0, XMM0, XMM0  # Scalar single precision multiply (AVX)
	#	    pop RBP
	#	    ret

code_native(area_cuadrada, (Float64,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vmulsd  XMM0, XMM0, XMM0 # Scalar double precision multiply (AVX)
	#	    pop RBP
	#	    ret
	#	

# Ten en cuenta que Julia usará instrucciones de "floating point" si alguno de
# los argumentos son "floats"
# Vamos a calcular el área de un círculo
area_circulo(r) = pi * r * r     # circle_area (generic function with 1 method)
area_circulo(5)                  # 78.53981633974483

code_native(area_circulo, (Int32,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vcvtsi2sd   XMM0, XMM0, EDI          # Load integer (r) from memory
	#	    movabs  RAX, 4593140240              # Load pi
	#	    vmulsd  XMM1, XMM0, QWORD PTR [RAX]  # pi * r
	#	    vmulsd  XMM0, XMM0, XMM1             # (pi * r) * r
	#	    pop RBP
	#	    ret
	#

code_native(area_circulo, (Float64,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	    movabs  RAX, 4593140496
	#	Source line: 1
	#	    vmulsd  XMM1, XMM0, QWORD PTR [RAX]
	#	    vmulsd  XMM0, XMM1, XMM0
	#	    pop RBP
	#	    ret
	#	
```

## ¿Listo para más?

Puedes obtener muchos más detalles en [The Julia Manual](http://docs.julialang.org/en/latest/manual/)

El mejor lugar para obtener ayuda con Julia es el (muy amable) [lista de correos](https://groups.google.com/forum/#!forum/julia-users).

