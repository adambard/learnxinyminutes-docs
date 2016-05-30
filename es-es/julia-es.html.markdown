---
language: Julia
contributors:
    - ["Leah Hanson", "http://leahhanson.us"]
translators:
    - ["Guillermo Garza", "http://github.com/ggarza"]
    - ["Ismael Venegas Castelló", "https://github.com/Ismael-VC"]
filename: learnjulia-es.jl
lang: es-es
---

![JuliaLang](http://s13.postimg.org/z89djuwyf/julia_small.png)

[Julia](http://julialanges.github.io) es un [lenguaje de programación](http://es.wikipedia.org/wiki/Lenguaje_de_programaci%C3%B3n) [multiplataforma](http://es.wikipedia.org/wiki/Multiplataforma) y [multiparadigma](http://es.wikipedia.org/wiki/Lenguaje_de_programaci%C3%B3n_multiparadigma) de [tipado dinámico](http://es.wikipedia.org/wiki/Tipado_din%C3%A1mico), [alto nivel](http://es.wikipedia.org/wiki/Lenguaje_de_alto_nivel) y [alto desempeño](http://es.wikipedia.org/wiki/Computaci%C3%B3n_de_alto_rendimiento) para la computación [genérica](http://es.wikipedia.org/wiki/Lenguaje_de_programaci%C3%B3n_de_prop%C3%B3sito_general), [técnica y científica](http://es.wikipedia.org/wiki/Computaci%C3%B3n_cient%C3%ADfica),  con una sintaxis que es familiar para los usuarios de otros entornos de computación técnica y científica. Provee de un [sofisticado compilador JIT](http://es.wikipedia.org/wiki/Compilaci%C3%B3n_en_tiempo_de_ejecuci%C3%B3n), [ejecución distribuida y paralela](http://docs.julialang.org/en/release-0.3/manual/parallel-computing), [precisión numérica](http://julia.readthedocs.org/en/latest/manual/integers-and-floating-point-numbers) y de una [extensa librería con funciones matemáticas](http://docs.julialang.org/en/release-0.3/stdlib). La librería estándar, escrita casi completamente en Julia, también integra las mejores y más maduras librerías de C y Fortran para el [álgebra lineal](http://docs.julialang.org/en/release-0.3/stdlib/linalg), [generación de números aleatorios](http://docs.julialang.org/en/release-0.3/stdlib/numbers/?highlight=random#random-numbers), [procesamiento de señales](http://docs.julialang.org/en/release-0.3/stdlib/math/?highlight=signal#signal-processing), y [procesamiento de cadenas](http://docs.julialang.org/en/release-0.3/stdlib/strings). Adicionalmente, la comunidad de [desarrolladores de Julia](https://github.com/JuliaLang/julia/graphs/contributors) contribuye un número de [paquetes externos](http://pkg.julialang.org) a través del gestor de paquetes integrado de Julia a un paso acelerado. [IJulia](https://github.com/JuliaLang/IJulia.jl), una colaboración entre las comunidades de [IPython](http://ipython.org) y Julia, provee de una poderosa interfaz gráfica basada en el [navegador para Julia](https://juliabox.org).

En Julia los programas están organizados entorno al [despacho múltiple](http://docs.julialang.org/en/release-0.3/manual/methods/#man-methods); definiendo funciones y sobrecargándolas para diferentes combinaciones de tipos de argumentos, los cuales también pueden ser definidos por el usuario.

### ¡Prueba Julia ahora mismo!

* [TryJupyter](https://try.jupyter.org)
* [JuliaBox](https://juliabox.org)
* [SageMathCloud](https://cloud.sagemath.com)

### Resumen de Características:

* [Despacho múltiple](http://en.wikipedia.org/wiki/Multiple_dispatch): permite definir el comportamiento de las funciones a través de múltiples combinaciones de tipos de argumentos (**métodos**).
* Sistema de **tipado dinámico**: tipos para la documentación, la optimización y el despacho.
* [Buen desempeño](http://julialang.org/benchmarks), comparado al de lenguajes **estáticamente compilados** como C.
* [Gestor de paquetes](http://docs.julialang.org/en/release-0.3/stdlib/pkg) integrado.
* [Macros tipo Lisp](http://docs.julialang.org/en/release-0.3/manual/metaprogramming/#macros) y otras comodidades para la [meta programación](http://docs.julialang.org/en/release-0.3/manual/metaprogramming).
* Llamar funciones de otros lenguajes, mediante paquetes como: **Python** ([PyCall](https://github.com/stevengj/PyCall.jl)), [Mathematica](http://github.com/one-more-minute/Mathematica.jl), **Java** ([JavaCall](http://github.com/aviks/JavaCall.jl)), **R** ([Rif](http://github.com/lgautier/Rif.jl) y [RCall](http://github.com/JuliaStats/RCall.jl)) y **Matlab** ([MATLAB](http://github.com/JuliaLang/MATLAB.jl)).
* [Llamar funciones de C y Fortran](http://docs.julialang.org/en/release-0.3/manual/calling-c-and-fortran-code) **directamente**: sin necesidad de usar envoltorios u APIs especiales.
* Poderosas características de **línea de comandos** para [gestionar otros procesos](http://docs.julialang.org/en/release-0.3/manual/running-external-programs).
* Diseñado para la [computación paralela y distribuida](http://docs.julialang.org/en/release-0.3/manual/parallel-computing) **desde el principio**.
* [Corrutinas](http://en.wikipedia.org/wiki/Coroutine): hilos ligeros "**verdes**".
* Los [tipos definidos por el usuario](http://docs.julialang.org/en/release-0.3/manual/types) son tan **rápidos y compactos** como los tipos estándar integrados.
* [Generación automática de código](http://docs.julialang.org/en/release-0.3/stdlib/base/?highlight=%40code#internals) **eficiente y especializado** para diferentes tipos de argumentos.
* [Conversiones y promociones](http://docs.julialang.org/en/release-0.3/manual/conversion-and-promotion) para tipos numéricos y de otros tipos, **elegantes y extensibles**.
* Soporte eficiente para [Unicode](http://es.wikipedia.org/wiki/Unicode), incluyendo [UTF-8](http://es.wikipedia.org/wiki/UTF-8) pero sin limitarse solo a este.
* [Licencia MIT](https://github.com/JuliaLang/julia/blob/master/LICENSE.md): libre y de código abierto.

Esto se basa en la versión `0.3.11`.

```ruby
# Los comentarios de una línea comienzan con una almohadilla (o signo de gato).

#=
  Los comentarios multilínea pueden escribirse
  usando '#=' antes de el texto  y '=#'
  después del texto. También se pueden anidar.
=#


##############################################
# 1. Tipos de datos primitivos y operadores. #
##############################################

# Todo en Julia es una expresión (Expr).

# Hay varios tipos básicos de números.
3          # => 3          # Int64
3.2        # => 3.2        # Float64
2 + 1im    # => 2 + 1im    # Complex{Int64}
2 // 3     # => 2//3       # Rational{Int64}

# Todos los operadores infijos normales están disponibles.
1 + 1        # => 2
8 - 1        # => 7
10 * 2       # => 20
35 / 5       # => 7.0    # dividir un Int por un Int siempre resulta
                         # en un Float
5 / 2        # => 2.5
div(5, 2)    # => 2      # para un resultado truncado, usa la función div
5 \ 35       # => 7.0
2 ^ 2        # => 4      # exponente, no es XOR
12 % 10      # => 2

# Refuerza la precedencia con paréntesis.
(1 + 3) * 2    # => 8

# Operadores a nivel de bit.
~2         # => -3    # bitwise NOT
3 & 5      # => 1     # bitwise AND
2 | 4      # => 6     # bitwise OR
2 $ 4      # => 6     # bitwise XOR
2 >>> 1    # => 1     # desplazamiento lógico hacia la derecha
2 >> 1     # => 1     # desplazamiento aritmético hacia la derecha
2 << 1     # => 4     # desplazamiento lógico/aritmético hacia la izquierda

# Se puede utilizar la función bits para ver la representación
# binaria de un número.
bits(12345)
# => "0000000000000000000000000000000000000000000000000011000000111001"

bits(12345.0)
# => "0100000011001000000111001000000000000000000000000000000000000000"

# Los valores booleanos (Bool) son primitivos.
true     # => true
false    # => false

# Operadores booleanos.
!true     # => false
!false    # => true
1 == 1    # => true
2 == 1    # => false
1 != 1    # => false
2 != 1    # => true
1 < 10    # => true
1 > 10    # => false
2 <= 2    # => true
2 >= 2    # => true

# ¡Las comparaciones pueden ser concatenadas!
1 < 2 < 3    # => true
2 < 3 < 2    # => false

# Los literales de cadenas (String) se crean con la comilla doble: "
"Esto es una cadena."

# Los literales de caracteres (Char) se crean con la comilla simple: '
'a'

# Una cadena puede ser indexada como una arreglo de caracteres.
"Esto es un string."[1]    # => 'E'    # Los índices en Julia comienzan en: 1

# Sin embargo, esto no va a funcionar bien para las cadenas UTF8 (UTF8String),
# Lo que se recomienda es la iteración (map, for, etc).

# $ puede ser utilizado para la interpolación de cadenas, se puede poner
# cualquier expresión de Julia dentro los paréntesis.
"2 + 2 = $(2 + 2)"    # => "2 + 2 = 4"

# Otra forma para formatear cadenas es usando el macro printf.
@printf "%d es menor de %f\n" 4.5 5.3    # 5 es menor de 5.300000

# ¡Imprimir es muy fácil!
println("¡Hola Julia!")    # ¡Hola Julia!


##############################
# 2. Variables y Colecciones #
##############################

# No hay necesidad de declarar las variables antes de asignarlas.
una_variable = 5    # => 5
una_variable        # => 5

# Acceder a una variable no asignada previamente es una excepción.
try
    otra_variable    # ERROR: otra_variable not defined
catch e
    println(e)       # UndefVarError(:otra_variable)
end

# Los nombres de variables comienzan con una letra o guion bajo: _.
# Después de eso, puedes utilizar letras, dígitos, guiones bajos y signos de
# exclamación.
otraVariable_123! = 6    # => 6

# También puedes utilizar caracteres Unicode.
☃ = 8    # => 8

# Estos son especialmente útiles para la notación matemática
# (multiplicación implicita).
2π    # => 6.283185307179586

#=
  Una nota sobre las convenciones de nomenclatura de Julia:

  * Los nombres de las variables aparecen en minúsculas, con separación de
    palabra indicado por un guion bajo:

    otra_variable

  * Los nombres de los tipos comienzan con una letra mayúscula y separación de
    palabras se muestra con CamelCase en vez de guión bajo:

    OtroTipo

  * Los nombres de las funciones y los macros están en minúsculas, sin
    underscore:

    otromacro

  * Funciones que modifican sus entradas tienen nombres que terminan en: !.
    Estas funciones a veces se les llaman funciones transformadoras o
    funciones in situ:

    otra_funcion!
=#

# Los arreglos (Array) almacenan una secuencia de valores indexados de entre 1 hasta n.
a = Int64[]    # => 0-element Array{Int64,1}

# Los literales de arregos unidimensionales se pueden escribir con valores
# separados por comas.
b = [4, 5, 6]
#=
 => 3-element Array{Int64,1}:
     4
     5
     6
=#
b[1]      # => 4
b[end]    # => 6

# Los arreglos bidimensionales usan valores separados por espacios y filas
# separadas por punto y coma.
matrix = [1 2; 3 4]
#=
 => 2x2 Array{Int64,2}:
     1  2
     3  4
=#

# Añadir cosas al final de un arreglo con push! y append!.
push!(a, 1)      # => [1]
push!(a, 2)      # => [1,2]
push!(a, 4)      # => [1,2,4]
push!(a, 3)      # => [1,2,4,3]
append!(a, b)    # => [1,2,4,3,4,5,6]

# Eliminar del final con pop!.
pop!(b)    # => 6 y b ahora es: [4,5]

# Vamos a ponerlo de nuevo.
push!(b, 6)    # b es ahora [4,5,6] de nuevo

a[1]    # => 1    # recuerda, los índices de Julia empiezan desde 1, no desde 0!

# end es una abreviatura para el último índice. Se puede utilizar en cualquier
# expresión  de indexación.
a[end]    # => 6

# También hay shift! y unshift!.
shift!(a)         # => 1 y a es ahora: [2,4,3,4,5,6]
unshift!(a, 7)    # => [7,2,4,3,4,5,6]

# Los nombres de funciones que terminan en exclamaciones indican que modifican
# su o sus argumentos de entrada.
arr = [5, 4, 6]    # => 3-element Array{Int64,1}: [5,4,6]
sort(arr)          # => [4,5,6] y arr es todavía: [5,4,6]
sort!(arr)         # => [4,5,6] y arr es ahora: [4,5,6]

# Buscando fuera de límites es un BoundsError.
try
    a[0]          # ERROR: BoundsError() in getindex at array.jl:270
    a[end+1]      # ERROR: BoundsError() in getindex at array.jl:270
catch e
    println(e)    # BoundsError()
end

# Las excepciones y los errores dan la línea y el archivo de su procedencia,
# aunque provenga de la librería estándar. Si compilas Julia del código fuente,
# puedes buscar en el código para encontrar estos archivos.

# Se puede inicializar un arreglo con un rango (Range).
a = [1:5]    # => 5-element Array{Int64,1}: [1,2,3,4,5]

# Puedes mirar en los rangos con la sintaxis de rebanada.
a[1:3]       # => [1,2,3]
a[2:end]     # => [2,3,4,5]

# Eliminar elementos de un arreglo por índice con splice!
arr = [3, 4, 5]
splice!(arr, 2)    # => 4 y arr es ahora: [3,5]

# Concatenar arreglos con append!
b = [1, 2, 3]
append!(a, b)    # a ahora es: [1,2,3,4,5,1,2,3]

# Comprueba la existencia de un elemento en un arreglo con in.
in(1, a)    # => true

# Examina la longitud con length.
length(a)    # => 8

# Las tuplas (Tuple) son inmutables.
tup = (1, 2, 3)    # => (1,2,3)    # una tupla tipo (Int64,Int64,Int64)
tup[1]             # => 1

try:
    tup[1] = 3     # ERROR: no method setindex!((Int64,Int64,Int64),Int64,Int64)
catch e
    println(e)     # MethodError(setindex!,(:tup,3,1))
end

# Muchas funciones de arreglos también trabajan en con las tuplas.
length(tup)    # => 3
tup[1:2]       # => (1,2)
in(2, tup)     # => true

# Se pueden desempacar las tuplas en variables individuales.
a, b, c = (1, 2, 3)    # => (1,2,3)    # ahora a es 1, b es 2 y c es 3

# Los tuplas se crean, incluso si se omiten los paréntesis.
d, e, f = 4, 5, 6    # => (4,5,6)

# Una tupla de un elemento es distinta del valor que contiene.
(1,) == 1    # => false
(1) == 1     # => true

# Mira que fácil es cambiar dos valores!
e, d = d, e    # => (5,4)    # ahora d es 5 y e es 4

# Los diccionarios (Dict) son arreglos asociativos.
dicc_vacio = Dict()    # => Dict{Any,Any} with 0 entries

# Se puede crear un diccionario usando una literal.
dicc_lleno = ["uno" => 1, "dos" => 2, "tres" => 3]
#=
 => Dict{ASCIIString,Int64} with 3 entries:
      "tres" => 3
      "dos"  => 2
      "uno"  => 1
=#

# Busca valores con: [].
dicc_lleno["uno"]    # => 1

# Obtén todas las claves con.
keys(dicc_lleno)
#=
 => KeyIterator for a Dict{ASCIIString,Int64} with 3 entries. Keys:
      "tres"
      "dos"
      "uno"
=#

# Nota: los elementos del diccionario no están ordenados y no se guarda el orden
# en que se insertan.

# Obtén todos los valores.
values(dicc_lleno)
#=
 => ValueIterator for a Dict{ASCIIString,Int64} with 3 entries. Values:
      3
      2
      1
=#

# Nota: igual que el anterior en cuanto a ordenamiento de los elementos.

# Comprueba si una clave existe en un diccionario con in y haskey.
in(("uno", 1), dicc_lleno)     # => true
in(("tres", 3), dicc_lleno)    # => false

haskey(dicc_lleno, "uno")      # => true
haskey(dicc_lleno, 1)          # => false

# Tratar de obtener un valor con una clave que no existe producirá un error.
try
    # ERROR: key not found: cuatro in getindex at dict.jl:489
    dicc_lleno["cuatro"]
catch e
    println(e)    # KeyError("cuatro")
end

# Utiliza el método get para evitar este error proporcionando un valor
# predeterminado: get(diccionario, clave, valor_predeterminado).
get(dicc_lleno, "uno", 4)       # => 1
get(dicc_lleno, "cuatro", 4)    # => 4

# Usa conjuntos (Set) para representar colecciones de valores únicos, no
# ordenados.
conjunto_vacio = Set()    # => Set{Any}({})

# Iniciar una conjunto de valores.
conjunto_lleno = Set(1, 2, 2, 3, 4)    # => Set{Int64}({4,2,3,1})

# Añadir más valores a un conjunto.
push!(conjunto_lleno, 5)    # => Set{Int64}({4,2,3,5,1})
push!(conjunto_lleno, 5)    # => Set{Int64}({4,2,3,5,1})

# Comprobar si los valores están en el conjunto.
in(2, conjunto_lleno)     # => true
in(10, conjunto_lleno)    # => false

# Hay funciones de intersección, unión y diferencia de conjuntos.
otro_conjunto = Set(3, 4, 5, 6)             # => Set{Int64}({6,4,5,3})
intersect(conjunto_lleno, otro_conjunto)    # => Set{Int64}({3,4,5})
union(conjunto_lleno, otro_conjunto)        # => Set{Int64}({1,2,3,4,5,6})
setdiff(Set(1, 2, 3, 4), Set(2, 3, 5))      # => Set{Int64}({1,4})


#######################
# 3. Control de Flujo #
#######################

# Hagamos una variable.
una_variable = 5

# Aquí está la declaración de un if. La indentación no es significativa en
# Julia.
if una_variable > 10
    println("una_variable es completamente mayor que 10.")
elseif una_variable < 10                     # esta condición elseif es opcional
    println("una_variable es menor que 10.")
else                                         # esto también es opcional
    println("De echo una_variable es 10.")
end
# imprime: una_variable es menor que 10.

# El bucle for itera sobre tipos iterables, ie. Range, Array, Set,
# Dict y String.
for animal in ["perro", "gato", "ratón"]
    # Se puede usar $ para interpolar variables o expresiones en ls cadenas.
    println("$animal es un mamífero.")
end
#=
 imprime:
   perro es un mamífero.
   gato es un mamífero.
   ratón es un mamífero.
=#

for a in ["perro" => "mamífero", "gato" => "mamífero", "ratón" => "mamífero"]
    println("$(a[1]) es un $(a[2]).")
end
#=
 imprime:
   perro es un mamífero.
   gato es un mamífero.
   ratón es un mamífero.
=#

for (k,v) in ["perro"=>"mamífero", "gato"=>"mamífero", "ratón"=>"mamífero"]
    println("$k es un $v.")
end
#=
 imprime:
   perro es un mamífero.
   gato es un mamífero.
   ratón es un mamífero.
=#

# El bucle while itera hasta que una condición se deje de cumplir.
x = 0
while x < 4
    println(x)
    x += 1    # versión corta de: x = x + 1
end
#=
imprime:
  0
  1
  2
  3
=#

# Maneja excepciones con un bloque try/catch.
try    # intentar
   error("Ooops!")
catch e
   println("capturando: $e")    # capturando: ErrorException("Ooops!")
end


################
# 4. Funciones #
################

# Usa function para crear nuevas funciones.

#=
    function nombre(arglist)
        cuerpo...
    end
=#
function suma(x, y)
    println("x es $x e y es $y")

    # las funciones devuelven el valor de su última expresión
    x + y
end
# => suma (generic function with 1 method)

suma(5, 6)    # => 11    # después de imprimir: x es 5 e y es 6

# También puedes usar esta otra sintaxis para definir funciones!
resta(x, y) = x - y    # => resta (generic function with 1 method)

# Puedes definir funciones que toman un número variable de
# argumentos posicionales (el ... se llama un splat).
function varargs(args...)
    # Usa la palabra clave return para regresar desde cualquier
    # lugar de la función.
    return args
end
# => varargs (generic function with 1 method)

varargs(1, 2, 3)      # => (1,2,3)
varargs([1, 2, 3])    # => ([1,2,3],)

# Acabamos de utilizar el splat (...) en la definición de una función. También
# se puede utilizar al llamar a una función, donde se esparce un arreglo, tupla
# o en general una secuencia iterable en la tupla de argumentos.
varargs([1, 2, 3]...)    # => (1,2,3)    # igual que: varargs(1, 2, 3)

x = (1, 2, 3)    # => (1,2,3)
varargs(x)       # => ((1,2,3),)
varargs(x...)    # => (1,2,3)

varargs("abc"...)    # => ('a','b','c')

# Puedes definir funciones con argumentos posicionales opcionales.
function defaults(a, b, x=5, y=6)
    return "$a $b y $x $y"
end
# => defaults (generic function with 3 methods)

defaults('h', 'g')              # => "h g y 5 6"
defaults('h', 'g', 'j')         # => "h g y j 6"
defaults('h', 'g', 'j', 'k')    # => "h g y j k"

try
    defaults('h')    # ERROR: `defaults` has no method matching defaults(::Char)
    defaults()       # ERROR: `defaults` has no method matching defaults()
catch e
    println(e)       # MethodError(defaults,('h',))
end

# Puedes definir funciones que tomen argumentos de palabras clave.
function args_clave(;k1=4, nombre2="hola")    # nota el punto y coma: ;
    return ["k1" => k1, "nombre2" => nombre2]
end
# => args_clave (generic function with 1 method)

args_clave(nombre2="ness")    # => ["nombre2"=>"ness","k1"=>4]
args_clave(k1="mine")         # => ["k1"=>"mine","nombre2"=>"hola"]
args_clave()                  # => ["nombre2"=>"hola","k1"=>4]

# Puedes combinar todo tipo de argumentos en la misma función.
function todos_los_args(arg_posicional, arg_opcional=2; arg_clave="foo")
    println("argumento posicional: $arg_posicional")
    println("  argumento opcional: $arg_opcional")
    println("     argumento clave: $arg_clave")
end
# => todos_los_args (generic function with 2 methods)

# No se necesita punto y coma ; al llamar la función usando un argumento clave,
# esto solo es necesario en la definición de la función.
todos_los_args(1, 3, arg_clave=4)
#=
 imprime:
   argumento posicional: 1
     argumento opcional: 3
        argumento clave: 4
=#

# Julia tiene funciones de primera clase.
function crear_suma(x)
    suma = function (y)    # función anónima
        return x + y
    end
    return suma
end
# => crear_suma (generic function with 1 method)

# Esta es otra sintaxis (estilo cálculo lambda), para crear funciones anónimas.
(x -> x > 2)(3)    # => true

# Esta función es idéntica a la crear_suma implementación anterior.
crear_suma(x) = y -> x + y

# También puedes nombrar la función interna, si quieres.
function crear_suma(x)
    function suma(y)
        x + y
    end
    suma
end
# => crear_suma (generic function with 1 method)

suma_10 = crear_suma(10)    # => suma (generic function with 1 method)
suma_10(3)                  # => 13

# Hay funciones integradas de orden superior.
map(suma_10, [1, 2, 3])                # => [11,12,13]
filter(x -> x > 5, [3, 4, 5, 6, 7])    # => [6,7]

# Se puede pasar un bloque a las funciones cuyo primer argumento posicional
# es otra función, como en map y filter.
map([1, 2, 3]) do arr
    suma_10(arr)
end
#=
 => 3-element Array{Int64,1}:
     11
     12
     13
=#

filter([3, 4, 5, 6, 7]) do arr
    (x -> x > 5)(arr)
end
#=
 => 2-element Array{Int64,1}:
     6
     7
=#

# Podemos usar comprensiones de listas multidimensionales.
[suma_10(i) for i = [1, 2, 3]]     # => [11, 12, 13]    # 1D
[suma_10(i) for i in [1, 2, 3]]    # => [11, 12, 13]

[i*j for i = [1:3], j in [1:3]]    # 2D
#=
 => 3x3 Array{Int64,2}:
     1  2  3
     2  4  6
     3  6  9
=#

[i*j/k for i = [1:3], j = [1:3], k in [1:3]]    # 3D
#=
 => 3x3x3 Array{Float64,3}:
     [:, :, 1] =
      1.0  2.0  3.0
      2.0  4.0  6.0
      3.0  6.0  9.0

     [:, :, 2] =
      0.5  1.0  1.5
      1.0  2.0  3.0
      1.5  3.0  4.5

     [:, :, 3] =
      0.333333  0.666667  1.0
      0.666667  1.33333   2.0
      1.0       2.0       3.0
=#


############
# 5. Tipos #
############

# Cada valor tiene un tipo y las variables no tienen propios tipos.
# Se puede utilizar la función typeof para obtener el tipo de un valor.
typeof(5)    # => Int64    # en un sistema de 64 bits, de lo contrario: Int32

# Los tipos son valores de primera clase, DataType es el tipo que representa a
# los tipos, incluyéndose a sí mismo.
typeof(Int64)       # => DataType
typeof(DataType)    # => DataType

# Los tipos se usan para la documentación, para optimizaciones
# y el despacho múltiple. No están comprobados estáticamente.

# Los usuarios pueden definir sus propios tipos.
# Son como registros o estructuras en otros idiomas.
# Un nuevo tipos se define utilizado la palabra clave type.

# type Nombre
#   atributo::UnTipo    # las anotaciones de tipos son opcionales
#   ...
# end
type Tigre
    longitud_cola::Float64
    color_pelaje            # sin una anotación de tipo, es lo mismo que `::Any`
end

# Los argumentos del constructor por defecto son los atributos
# del tipo, en el orden en que están listados en la definición.
tigre = Tigre(3.5, "anaranjado")    # => Tigre(3.5,"anaranjado")

# El tipo funciona como método constructor para los valores de ese tipo.
sherekhan = typeof(tigre)(5.6, "fuego")    # => Tigre(5.6,"fuego")


# Este estilo de tipos son llamados tipos concretos.
# Se pueden crear instancias de estos, pero no pueden tener subtipos.
# La otra clase de tipos son los tipos abstractos.

# abstract Nombre
abstract Gato    # sólo un nombre y un punto en la jerarquía de tipos

# No se pueden crear instancias de los tipos abstractos, pero pueden tener
# subtipos. Por ejemplo, Number es un tipo abstracto.
subtypes(Number)
#=
 => 2-element Array{Any,1}:
     Complex{T<:Real}
     Real
=#

subtypes(Gato)    # => 0-element Array{Any,1}

# Cada tipo tiene un supertipo, utiliza la función súper para conseguirlo.
typeof(5)               # => Int64
super(Int64)            # => Signed
super(Signed)           # => Integer
super(Integer)          # => Real
super(Real)             # => Number
super(Number)           # => Any
super(super(Signed))    # => Real
super(Any)              # => Any

# Todos estos tipos, a excepción de Int64, son abstractos.

# <: es el operador de subtipos.
type Leon <: Gato    # Leon es un subtipo de Gato
    color_crin
    rugido::String
end

# Se pueden definir más constructores para un tipo.
# Sólo define una función del mismo nombre que el tipo y llama al constructor
# existente para obtener un valor del tipo correcto.

# Este es un constructor externo porque está fuera de la definición del tipo.
Leon(rugido::String) = Leon("verde", rugido)

type Pantera <: Gato    # Pantera también es un a subtipo de Gato
    color_ojos

    # Pantera sólo tendrá este constructor, y ningún constructor predeterminado.
    Pantera() = new("verde")
end

# Utilizar constructores internos, como se hace en Pantera, te da control sobre
# cómo se pueden crear valores de este tipo. Cuando sea posible, debes utilizar
# constructores externos en lugar de internos.


########################
# 6. Despacho Múltiple #
########################

# En Julia, todas las funciones nombradas son funciones genéricas.
# Esto significa que se construyen a partir de muchos métodos más pequeños.
# Cada constructor de Leon es un método de la función genérica Leon.

# Por ejemplo, vamos a hacer métodos para Leon, Pantera, y Tigre de una
# función genérica maullar:

# acceso utilizando notación de puntos
maullar(animal::Leon) = animal.rugido
# => maullar (generic function with 1 method)
maullar(animal::Pantera) = "grrr"
# => maullar (generic function with 2 methods)
maullar(animal::Tigre) = "rawwwr"
# => maullar (generic function with 3 methods)

# Se puede obtener una lista de métodos con la función methods.
methods(maullar)
#=
  # 3 methods for generic function "maullar":
  maullar(animal::Leon) at none:1
  maullar(animal::Pantera) at none:1
  maullar(animal::Tigre) at none:1
=#

# Prueba de la función maullar.
maullar(tigre)                    # => "rawwwr"
maullar(Leon("cafe", "ROAAR"))    # => "ROAAR"
maullar(Pantera())                # => "grrr"

# Revisar la jerarquía de tipos locales.
issubtype(Tigre, Gato)      # => false    # igual que: Tigre <: Gato
issubtype(Leon, Gato)       # => true     # igual que: Leon <: Gato
issubtype(Pantera, Gato)    # => true

# Definición de una función que acepta argumentos de tipo Gato.
mascota(gato::Gato) = println("El gato dice $(maullar(gato))")

mascota(Leon("42"))    # El gato dice 42

try
    mascota(tigre)    # ERROR: `mascota` has no method matching mascota(::Tigre)
catch e
    println(e)        # MethodError(mascota,(Tigre(3.5,"anaranjado"),))
end

# En los lenguajes orientados a objetos, el despacho simple es común. Esto
# significa que la implementación del método a llamar se selecciona en base
# al tipo del primer argumento.

# En Julia, los tipos de todos los argumentos contribuyen a seleccionar método
# más específico.

# Vamos a definir una función con más argumentos, para que podamos ver la
# diferencia
pelear(t::Tigre, c::Gato) = println("¡El tigre $(t.color_pelaje) gana!")
# => pelear (generic function with 1 method)

pelear(tigre, Pantera())       # ¡El tigre anaranjado gana!
pelear(tigre, Leon("ROAR"))    # ¡El tigre anaranjado gana!

# Vamos a cambiar el comportamiento cuando el Gato sea específicamente un Leon.
pelear(t::Tigre, l::Leon) = println("El león con melena $(l.color_crin) gana.")
# => pelear (generic function with 2 methods)

pelear(tigre, Pantera())       # ¡El tigre anaranjado gana!
pelear(tigre, Leon("ROAR"))    # El león con melena verde gana.

# No necesitamos un tigre para poder luchar.
pelear(l::Leon, c::Gato) = println("El gato victorioso dice $(maullar(c)).")
# => pelear (generic function with 3 methods)

methods(pelear)
#=
  # 3 methods for generic function "pelear":
  pelear(t::Tigre,l::Leon) at none:2
  pelear(t::Tigre,c::Gato) at none:1
  pelear(l::Leon,c::Gato) at none:2
=#

pelear(Leon("balooga!"), Pantera())    # El gato victorioso dice grrr.
try
    # ERROR: `pelear` has no method matching pelear(::Pantera, ::Leon)
    pelear(Pantera(),Leon("RAWR"))
catch    # no hacer nada con la excepción atrapada
end

# Un metodo con el tipo Gato primero.
pelear(c::Gato,l::Leon) = println("El gato le gana al León")
#=
  Warning: New definition
      pelear(Gato,Leon) at none:1
  is ambiguous with:
      pelear(Leon,Gato) at none:1.
  To fix, define
      pelear(Leon,Leon)
  before the new definition.
  pelear (generic function with 4 methods)
=#

# Esta advertencia se debe a que no está claro que método de pelear
# será llamado en:
pelear(Leon("RAR"),Leon("cafe","rar"))    # El gato victorioso dice rar.

# El resultado puede ser diferente en otras versiones de Julia
pelear(l::Leon,l2::Leon) = println("Los leones llegan a un empate")

pelear(Leon("GR"),Leon("cafe","rar"))    # Los leones llegan a un empate


################################
# 7. Un vistazo de bajo nivel. #
################################

# Se puede echar un vistazo al código IR de LLVM y al código
# ensamblador generado.
area_cuadrado(l) = l * l    # => area_cuadrado (generic function with 1 method)

area_cuadrado(5)    # => 25

# ¿Qué sucede cuando damos area_cuadrada diferentes tipos de argumentos?
code_native(area_cuadrado, (Int32,))
#=
    .section  __TEXT,__text,regular,pure_instructions
  Filename: none
  Source line: 1      # prólogo
    push  RBP
    mov RBP, RSP
  Source line: 1
    imul  RDI, RDI    # elevar l al cuadrado
    mov RAX, RDI      # almacenar el resultado en RAX
    pop RBP           # restaurar el puntero base anterior
    ret               # el resultado estará en RAX
=#

code_native(area_cuadrado, (Float32,))
#=
    .section  __TEXT,__text,regular,pure_instructions
  Filename: none
  Source line: 1
    push  RBP
    mov RBP, RSP
  Source line: 1
    mulss XMM0, XMM0    # multiplicación escalar de presición simple (AVX)
    pop RBP
    ret
=#

code_native(area_cuadrado, (Float64,))
#=
    .section  __TEXT,__text,regular,pure_instructions
  Filename: none
  Source line: 1
    push  RBP
    mov RBP, RSP
  Source line: 1
    mulsd XMM0, XMM0    # multiplicación escalar de presición doble (AVX)
    pop RBP
    ret
=#

# Ten en cuenta que Julia usará instrucciones de punto flotante si el tipo de
# alguno de los argumentos es flotante.

# Vamos a calcular el área de un círculo.
area_circulo(r) = π * r * r    # area_circulo (generic function with 1 method)
area_circulo(5)                # 78.53981633974483

code_native(area_circulo, (Int32,))
#=
    .section  __TEXT,__text,regular,pure_instructions
  Filename: none
  Source line: 1
    push  RBP
    mov RBP, RSP
  Source line: 1
    cvtsi2sd  XMM1, EDI            # cargar entero r de la memoria
    movabs  RAX, 4477117456        # cargar constante matemática π
    movsd XMM0, QWORD PTR [RAX]
    mulsd XMM0, XMM1               # π * r
    mulsd XMM0, XMM1               # (π * r) * r
    pop RBP
    ret
=#

code_native(area_circulo, (Float64,))
#=
    .section  __TEXT,__text,regular,pure_instructions
  Filename: none
  Source line: 1
    push  RBP
    mov RBP, RSP
    movabs  RAX, 4477120336
    movsd XMM1, QWORD PTR [RAX]
  Source line: 1
    mulsd XMM1, XMM0
    mulsd XMM1, XMM0
    movaps  XMM0, XMM1
    pop RBP
    ret
=#
```

![Julia-tan](http://s27.postimg.org/x37ndhz0j/julia_tan_small.png)

## ¿Listo para más?

Para más detalles, lee el [manual de Julia](http://docs.julialang.org/en/release-0.3).

El mejor lugar para obtener ayuda con Julia, es en su amigable [lista de correos](https://groups.google.com/forum/#!forum/julia-users).
