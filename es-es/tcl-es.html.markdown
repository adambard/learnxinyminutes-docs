---
language: Tcl
contributors:
    - ["Poor Yorick", "https://pooryorick.com/"]
translators:
    - ["Héctor Romojaro", "https://github.com/hromojaro"]
lang: es-es
filename: learntcl-es.tcl
---

Tcl fue creado por [John Ousterhout](https://wiki.tcl.tk/John%20Ousterout) como
un lenguaje reutilizable de scripting para herramientas de diseño de circuitos
de las que él era autor.  En 1997 recibió el 
[ACM Software System Award](https://en.wikipedia.org/wiki/ACM_Software_System_Award) 
por Tcl.   Tcl puede ser utilizado tanto como lenguaje de scripting embebido, 
como lenguaje de programación general.  Puede ser utilizado también como una
biblioteca portable de C, incluso en casos donde no se requieren capacidades
de scripting, ya que provee de estructuras de datos tales como cadenas (*string*)
de caracteres dinámicas, listas y tablas hash.  La biblioteca de C también
provee funcionalidad portable para cargar bibliotecas dinámicas, formato de
cadenas y conversión de código, operaciones sobre el sistema de ficheros,
operaciones de red y más.  Algunas características reseñables de Tcl:

* Conveniente API de red multiplataforma

* Sistema de ficheros totalmente virtualizado

* Canales apilables de E/S

* Asíncrono hasta el núcleo

* Corrutinas completas

* Un modelo de hebras reconocido como robusto y fácil de usar


Tcl tiene mucho en común con Lisp pero, en lugar de listas, Tcl utiliza cadenas
de caracteres como moneda de cambio del lenguaje.  Todos los valores son cadenas.
Una lista es una cadena con un formato definido, y el cuerpo de un procedimiento
(un script) es también una cadena en lugar de un bloque.  Para incrementar el
rendimiento, Tcl cachea internamente representaciones estructuradas de estos
valores.  Las rutinas con listas, por ejemplo, operan en la representación interna
en caché, y Tcl se ocupa de actualizar la representación en cadenas si es realmente
necesario en el script.  El diseño *copy-on-write* de Tcl permite a los autores
de scripts mover grandes volúmenes de datos sin incurrir en el consumo adicional
de memoria.  Los procedimientos son automáticamente compilados (*byte-compiled*)
a no ser que utilicen rutinas dinámicas como "uplevel", "upvar" o "trace".

Programar en Tcl es un placer.  Le resultará atractivo a hackers que encuentren
atractivo Lisp, Forth o Smalltalk, y a ingenieros y científicos que simplemente
quieren ponerse a trabajar con una herramienta que se doblega a su voluntad.  La
disciplina de exponer toda la funcionalidad programática como rutinas, incluyendo
cosas como iteraciones y operaciones matemáticas que normalmente están en la
sintaxis de otros lenguajes, permitiendo fundirse en el fondo de cualquier
funcionalidad específica del dominio que necesita un proyecto.  Su sintaxis,
incluso más simple que la de lisp, simplemente se quita de en medio.



```tcl
#! /bin/env tclsh

###############################################################################
## 1. Directrices
###############################################################################

# ¡Tcl no es ni Sh ni C!  Es necesario decirlo porque el entrecomillado estándar
# de shell casi funciona en Tcl, y es común que la gente empiece con Tcl e
# intente utilizar sintaxis de otros lenguajes.  Funciona al principio, pero
# rápidamente conduce a frustración cuando los scripts se vuelven más complejos.

# Las llaves son un mecanismo de entrecomillado, no de sintaxis para la construcción
# de bloques de código o listas.  Tcl no tiene ninguna de ellas.  Las llaves se
# usan para escapar caracteres especiales, lo que las hace apropiadas para 
# entrecomillar cuerpos de procedimientos y cadenas que deberían ser interpretadas
# como listas.


###############################################################################
## 2. Sintaxis
###############################################################################

# Un script consiste en comandos delimitados por saltos de línea o puntos y coma.
# Cada comando es una llamada a una rutina.  La primera palabra es el nombre de
# la rutina a llamar, y las siguientes palabras son argumentos de la rutina.
# Las palabras están delimitadas por espacios.  Puesto que cada argumento es una
# palabra en el comando, y una cadena de caracteres, puede no ser entrecomillada:
set part1 Sal
set part2 ut; set part3 ations


# el símbolo del dólar introduce la sustitución de variables:
set greeting $part1$part2$part3


# Cuando "set"recibe sólamente el nombre de una variable, devuelve su valor:
set part3 ;# Returns the value of the variable.


# Los corchetes delimitan un script que será evaluado y sustituido por su resultado:
set greeting $part1$part2[set part3]


# Un script incrustado puede estar compuesto de múltiples comandos, el último de
# los cuales devuelve el resultado de la sustitución:
set greeting $greeting[
    incr i
    incr i
    incr i
]
puts $greeting ;# La salida es "Salutations3"

# Cada palabra en un comando es una cadena, incluyendo el nombre de la rutina,
# así que se pueden utilizar sustituciones allí también. Dada esta asignación
# de variable,

set action pu

# los siguientes tres comandos son equivalentes:
puts $greeting
${action}ts $greeting 
[set action]ts $greeting


# La barra invertida suprime el significado especial de los caracteres:
set amount \$16.42


# La barra invertida añade significado especial a ciertos caracteres:
puts lots\nof\n\n\n\n\n\nnewlines


# Una palabra encerrada entre llaves no está sujeta a interpretación especial o
# sustitución, excepto que una barra invertida antes de una llave no cuenta al
# buscar la llave de cierre:
set somevar {
    This is a literal $ sign, and this \} escaped
    brace remains uninterpreted
}


# En una palabra delimitada por comillas dobles, los espacios pierden su significado
# especial:
set name Neo
set greeting "Hello, $name"


# Un nombre de variable puede ser cualquier cadena:
set {first name} New


# La forma de sustitución de variables utilizando llaves permite nombres de
# variable más complejos:
set greeting "Hello, ${first name}"


# "set" puede utilizarse siempre en lugar de la sustitución de variables, y permite
# utilizar cualquier nombre de variable:
set greeting "Hello, [set {first name}]"


# Para desempaquetar una lista en un el comando, se utiliza el operador de expansión,
# "{*}".  Estos dos comandos son equivalentes:
set name Neo
set {*}{name Neo}


# Un array es una variable especial que sirve como contenedor de otras variables.
set person(name) Neo
set person(destiny) {The One}
set greeting "Hello, $person(name)"


# "variable" se puede utilizar para declarar o asignar variables. Al contrario
# que "set", que utiliza el espacio de nombres global y el actual para resolver
# un nombre de variable, "variable" usa solamente el actual:
variable name New


# "namespace eval" crea un nuevo espacio de nombres en caso de no existir.
# Un espacio de nombres puede contener tanto rutinas como variables:
namespace eval people {
    namespace eval person1 {
        variable name Neo
    }
}


# Use dos o más ":" para delimitar componentes del espacio de nombres en nombres
# de variables:
namespace eval people {
    set greeting "Hello $person1::name"
}

# Dos o más ":" también delimitan componentes del espacio de nombres en nombres
# de rutinas:
proc people::person1::speak {} {
    puts {I am The One.}
}

# Nombres completos comienzan con dos ":":
set greeting "Hello $::people::person1::name"



###############################################################################
## 3. No más sintaxis
###############################################################################

# El resto de funcionalidades se implementa mediante rutinas.  Desde este punto,
# no hay nueva sintaxis.  Todo lo que queda para aprender Tcl es acerca del
# comportamiento de rutinas individuales y el significado que asignan a sus
# argumentos.



###############################################################################
## 4. Variables y espacios de nombres
###############################################################################

# Cada variable y cada rutina están asociadas a algún espacio de nombres

# Para terminar con un intérprete inútil, sólo hay que eliminar el espacio de
# nombres global.  No es algo muy útil, pero sirve para ilustrar la naturaleza
# de Tcl.  El nombre del espacio de nombres global es en realidad la cadena
# vacía, pero la única forma de representarlo es como un nombre completo. Para
# probarlo, se puede usar esta rutina.
proc delete_global_namespace {} {
    namespace delete ::
}

# Como "set" siempre mantiene su vista en los espacios de nombres global y actual,
# es más seguro utilizar "variable" para declarar o asignar un valor a una
# variable.  Si una variable llamada "nombre" ya existe en el espacio de nombres
# global, usar "set" asignará un valor a la variable local en lugar de a la
# variable del espacio de nombres actual, mientras que "variable" opera en el
# espacio de nombres actual solamente.
namespace eval people {
    namespace eval person1 {
        variable name Neo
    }
}

# Una vez que una variable es declarada en un espacio de nombres, [set] la vé
# en lugar de una variable de idéntico nombre en el espacio de nombres global:
namespace eval people {
    namespace eval person1 {
        variable name
        set name Neo
    }
}

# En cambio, si "set" tiene que crear una nueva variable, siempre lo hace en el
# espacio de nombres actual:
unset name
namespace eval people {
    namespace eval person1 {
        set name neo
    }

}
set people::person1::name


# Un nombre absoluto siempre comienza con el nombre del espacio de nombres global
# (cadena vacía), seguido de dos ":":
set ::people::person1::name Neo


# En el interior de un procedimiento, la variable enlaza una variable en el espacio
# de nombres actual en el ámbito local:
namespace eval people::person1 {
    proc fly {} {
        variable name
        puts "$name is flying!"
    }
}




###############################################################################
## 4. Rutinas incorporadas
###############################################################################

# Las operaciones matemáticas se pueden hacer con "expr":
set a 3
set b 4
set c [expr {$a + $b}]

# Como "expr" realiza sustituciones de variables por sí mismo, es necesario
# poner la expresión entre llaves para prevenir a Tcl sustituir las variables
# primero. Ver "http://wiki.tcl.tk/Brace%20your%20#%20expr-essions" para más
# detalles.


# "expr" entiende sustitución de variables y scripts:
set c [expr {$a + [set b]}]


# "expr" provee de un conjunto de funciones matemáticas:
set c [expr {pow($a,$b)}]


# Los operadores matemáticos están disponibles como rutinas en el espacio de
# nombres ::tcl::mathop
::tcl::mathop::+ 5 3

# Las rutinas pueden ser importadas desde otros espacios de nombres:
namespace import ::tcl::mathop::+
set result [+ 5 3]


# Los valores no numéricos deben ser entrecomillados, y los operadores como "eq"
# pueden utilizarse para restringir la operación a una comparación de cadenas:
set name Neo
expr {{Bob} eq $name}

# Los operadores generales recurren a la comparación de cadenas si una operación
# numérica no es factible.
expr {{Bob} == $name}


# "proc" crea nuevas rutinas:
proc greet name {
    return "Hello, $name!"
}

# Se pueden especificar múltiples parámetros:
proc greet {greeting name} {
    return "$greeting, $name!"
}


# Como se dijo antes, las llaves no construyen un bloque de código.  Cada valor,
# incluso el tercer argumento de "proc", es una cadena.  El comando anterior
# puede ser reescrito sin usar llaves:
proc greet greeting\ name return\ \"\$greeting,\ \$name!\"



# Cuando el último parámetro es el valor literal "args", todos los argumentos
# extra pasados a la rutina son recogidos en una lista y asignado a "args":
proc fold {cmd first args} {
    foreach arg $args {
        set first [$cmd $first $arg]
    }
    return $first
}
fold ::tcl::mathop::* 5 3 3 ;# ->  45


# La ejecución condicional se implementa como una rutina:
if {3 > 4} {
    puts {This will never happen}
} elseif {4 > 4} {
    puts {This will also never happen}
} else {
    puts {This will always happen}
}


# Los bucles se implementan como rutinas.  Los primer y tercer argumentos de "for"
# son tratados como scripts, mientras que el segundo lo es como una expresión:
set res 0
for {set i 0} {$i < 10} {incr i} {
    set res [expr {$res + $i}]
}
unset res


# El primer argumento de "while" se trata también como una expresión:
set i 0
while {$i < 10} {
    incr i 2
}


# Una lista es una cadena, y los elementos de la lista se delimitan con espacios
# en blanco:
set amounts 10\ 33\ 18
set amount [lindex $amounts 1]

# El espacio en blanco dentro de una lista debe ser entrecomillado:
set inventory {"item 1" item\ 2 {item 3}}


# Generalmente, es mejor idea usar rutinas de listas al modificarlas:
lappend inventory {item 1} {item 2} {item 3}


# Las llaves y barras invertidas pueden utilizarse para formatear valores más
# complejos en una lista.  Una lista parece un script, excepto en que el carácter
# de nueva línea y el ":" pierden su significado especial, y no hay sustitución
# de variable o scripts.  Esta característica hace Tcl homoicónico.  Hay tres
# elementos en la siguiente lista:
set values {

    one\ two

    {three four}

    five\{six

}


# Como, al igual que todos los valores, una lista es una cadena, operaciones de
# cadenas pueden ser realizadas sobre ellas, corriendo el riesgo de corromper
# el formato de la lista:
set values {one two three four}
set values [string map {two \{} $values] ;# $values is no-longer a \
    properly-formatted list


# La forma segura de conseguir una lista debidamente formateada es utilizando
# las rutinas propias de lista:
set values [list one \{ three four]
lappend values { } ;# add a single space as an item in the list


# Se puede utilizar "eval" para evaluar un valor como un script:
eval {
    set name Neo
    set greeting "Hello, $name"
}


# Una lista siempre puede ser pasada a "eval" como un script compuesto de un único
# comando:
eval {set name Neo}
eval [list set greeting "Hello, $name"]


# Por lo tanto, cuando se utiliza "eval", use "list" para construir el comando
# deseado:
set command {set name}
lappend command {Archibald Sorbisol}
eval $command


# Un error común es no usar funciones de listas al construir un comando:
set command {set name}
append command { Archibald Sorbisol}
try {
    eval $command ;# El error es que "set" tiene demasiados argumentos en \
        {set name Archibald Sorbisol}
} on error {result eoptions} {
    puts [list {received an error} $result]
}

# Este error puede ocurrir fácilmente con "subst":

set replacement {Archibald Sorbisol}
set command {set name $replacement}
set command [subst $command] 
try {
    eval $command ;# El mismo error que antes:  demasiados argumentos a "set" en \
        {set name Archibald Sorbisol}
} trap {TCL WRONGARGS} {result options} {
    puts [list {received another error} $result]
}


# "list" formatea correctamente un valor para su sustitución:
set replacement [list {Archibald Sorbisol}]
set command {set name $replacement}
set command [subst $command]
eval $command


# "list" se utiliza normalmente para formatear valores para su sustitución en
# scripts: Hay muchos ejemplos de esto más abajo.


# "apply" evalúa una lista de dos elementos como una rutina:
set cmd {{greeting name} {
    return "$greeting, $name!"
}}
apply $cmd Whaddup Neo

# Un tercer elemento puede ser utilizado para especificar el espacio de nombres
# donde aplicar la rutina:
set cmd [list {greeting name} {
    return "$greeting, $name!"
} [namespace current]]
apply $cmd Whaddup Neo


# "uplevel" evalúa un script en un nivel superior de la pila de llamadas:
proc greet {} {
    uplevel {puts "$greeting, $name"}
}

proc set_double {varname value} {
    if {[string is double $value]} {
        uplevel [list variable $varname $value]
    } else {
        error [list {not a double} $value]
    }
}


# "upvar" enlaza una variable en el nivel actual de la pila de llamadas a una
# variable en un nivel superior:
proc set_double {varname value} {
    if {[string is double $value]} {
        upvar 1 $varname var
        set var $value
    } else {
        error [list {not a double} $value]
    }
}


# Deshacerse de la rutina "while" incorporada, y utilizar "proc" para definir
# una nueva:
rename ::while {}
# la manipulación se deja como ejercicio:
proc while {condition script} {
    if {[uplevel 1 [list expr $condition]]} {
        uplevel 1 $script
        tailcall [namespace which while] $condition $script
    }
}


# "coroutine" crea una nueva pila de llamadas, una nueva rutina en la que
# introducir esa pila de llamadas, y luego llama a dicha rutina. "yield" suspende
# la evaluación en esa pila y devuelve el control a la pila que efectúa la llamada.
proc countdown count {
    # devuelve algo al creador de la corrutina, efectivamente pausando esta
    # pila de llamadas por ahora.
    yield [info coroutine]

    while {$count > 1} {
        yield [incr count -1]
    }
    return 0
}
coroutine countdown1 countdown 3
coroutine countdown2 countdown 5
puts [countdown1] ;# -> 2 
puts [countdown2] ;# -> 4 
puts [countdown1] ;# -> 1 
puts [countdown1] ;# -> 0 
catch {
    puts [coundown1] ;# -> invalid command name "countdown1"
} cres copts 
puts $cres
puts [countdown2] ;# -> 3 


# Pilas de corrutinas pueden cederse el control entre sí:

proc pass {whom args} {
    return [yieldto $whom {*}$args]
}

coroutine a apply {{} {
        yield
        set result [pass b {please pass the salt}]
        puts [list got the $result]
        set result [pass b {please pass the pepper}]
        puts [list got the $result]
}}

coroutine b apply {{} {
    set request [yield]
    while 1 {
        set response [pass c $request]
        puts [list [info coroutine] is now yielding]
        set request [pass a $response]
    }
}}

coroutine c apply {{} {
    set request [yield]
    while 1 {
        if {[string match *salt* $request]} {
            set request [pass b salt]
        } else {
            set request [pass b huh?]
        }
    }
}}

# Pon las cosas en marcha
a


```

## Reference

[Documentación oficial de Tcl](http://www.tcl.tk/man/tcl/)

[Tcl Wiki](http://wiki.tcl.tk)

[Tcl Subreddit](http://www.reddit.com/r/Tcl)
