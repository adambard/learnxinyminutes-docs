---
language: awk
filename: learnawk-es.awk
contributors:
    - ["Marshall Mason", "http://github.com/marshallmason"]
translators:
    - ["Hugo Guillén-Ramírez", "http://github.com/HugoGuillen"]
lang: es-es
---

AWK es una herramienta estándar en cada sistema UNIX compatible con POSIX.
Es como un Perl restringido, perfecto para tareas de procesamiento de texto y
otras necesidades de scripting. Tiene una sintaxis similar a C, pero sin 
puntos y comas, manejo manual de memoria y tipado estático. Puedes llamarlo 
desde un script de shell o usarlo como un lenguaje stand-alone para scripting.

¿Por qué elegir AWK sobre Perl? Principalmente, porque AWK es parte de UNIX.
Siempre puedes contar con él, mientras que el futuro de Perl está en duda. AWK
es más fácil de leer que Perl. Para scripts sencillos de procesamiento de texto,
particularmente si es para leer archivos línea a línea y dividir por
delimitadores, probablemente AWK es la herramienta correcta para el trabajo.

```awk
#!/usr/bin/awk -f

# Los comentarios tienen este aspecto.

# Los programas AWK son una colección de patrones y acciones. El patrón más
# importante es BEGIN. Las acciones van en bloques delimitados por llaves.

BEGIN {

    # BEGIN correrá al inicio del programa. Es donde pones todo el código
    # preliminar antes de procesar los archivos de texto. Si no tienes archivos
    # de texto, piensa en BEGIN como el punto de entrada principal del script.

    # Las variables son globales. Asígnalas o úsalas sin declararlas.
    count = 0

    # Los operadores son justo como en C (y amigos).
    a = count + 1
    b = count - 1
    c = count * 1
    d = count / 1
    e = count % 1 # módulo
    f = count ^ 1 # exponenciación

    a += 1
    b -= 1
    c *= 1
    d /= 1
    e %= 1
    f ^= 1

    # Incremento y decremento en uno
    a++
    b--

    # Como un operador prefijo, regresa el valor modificado
    ++a
    --b

    # Nota que no hay puntación para terminar las instrucciones

    # Instrucciones de control
    if (count == 0)
        print "Iniciando count en 0"
    else
        print "Eh?"

    # O puedes usar el operador ternario
    print (count == 0) ? "Iniciando count en 0" : "Eh?"

    # Bloques formados por múltiples líneas usan llaves
    while (a < 10) {
        print "La concatenación de strings se hace " " con series " 
	print " de" " strings separados por espacios"
        print a

        a++
    }

    for (i = 0; i < 10; i++)
        print "El viejo confiable ciclo for"

    # Los operaciones de comparación son estándar...
    a < b   # Menor que
    a <= b  # Menor o igual que
    a != b  # No igual
    a == b  # Igual
    a > b   # Mayor que
    a >= b  # Mayor o igual que

    # ...así como los operadores lógicos
    a && b  # AND
    a || b  # OR

    # Además están las expresiones regulares
    if ("foo" ~ "^fo+$")
        print "Fooey!"
    if ("boo" !~ "^fo+$")
        print "Boo!"

    # Arrays
    arr[0] = "foo"
    arr[1] = "bar"
    # Desafortunadamente no hay otra manera de inicializar un array.
    # Tienes que inicializar cada posición del array.

    # También hay arrays asociativos
    assoc["foo"] = "bar"
    assoc["bar"] = "baz"

    # Y arrays multidimensionales con limitaciones que no mencionaré aquí
    multidim[0,0] = "foo"
    multidim[0,1] = "bar"
    multidim[1,0] = "baz"
    multidim[1,1] = "boo"

    # Puedes probar pertenencia a un array
    if ("foo" in assoc)
        print "Fooey!"

    # También puedes usar el operador 'in' para iterar las claves de un array
    for (key in assoc)
        print assoc[key]

    # La terminal es un array especial llamado ARGV
    for (argnum in ARGV)
        print ARGV[argnum]

    # Puedes eliminar elementos de un array.
    # Esto es útil para prevenir que AWK suponga que algunos argumentos
    # son archivos por procesar.
    delete ARGV[1]

    # El número de argumentos de la terminal está en la variable ARGC
    print ARGC

    # AWK tiene tres categorías de funciones incluidas.
    # Demostraré esas funciones posteriormente.

    return_value = arithmetic_functions(a, b, c)
    string_functions()
    io_functions()
}

# Así se define una función
function arithmetic_functions(a, b, c,     localvar) {

    # Probablemente la parte más molesta de AWK es que no hay variables locales
    # Todo es global. No es problema en scripts pequeños, pero sí para
    # scripts más grandes.

    # Hay un work-around (mmm... hack). Los argumentos de las funciones son 
    # locales para la función, y AWK permite definir más argumentos de función
    # de los que necesita, por lo que define las variables locales en la 
    # declaración como en la función de arriba. Como convención, agrega
    # espacios en blanco para distinguir los parámetros de la función de las 
    # variables locales. En este ejemplo, a, b y c son parámetros y localvar es una 
    # variable local.

    # Ahora, a demostrar las funciones aritméticas

    # La mayoría de las implementaciones de AWK tienen funciones
    # trigonométricas estándar
    localvar = sin(a)
    localvar = cos(a)
    localvar = atan2(a, b) # arcotangente de b / a

    # Y cosas logarítmicas
    localvar = exp(a)
    localvar = log(a)

    # Raíz cuadrada
    localvar = sqrt(a)

    # Trucar un flotante a entero
    localvar = int(5.34) # localvar => 5

    # Números aleatorios
    srand() # La semilla es el argumento. Por defecto usa el tiempo del sistema
    localvar = rand() # Número aleatorio entre 0 y 1.

    # Y aquí se regresa el valor
    return localvar
}

function string_functions(    localvar, arr) {

    # AWK, being a string-processing language, has several string-related
    # functions, many of which rely heavily on regular expressions.

    # Search and replace, first instance (sub) or all instances (gsub)
    # Both return number of matches replaced
    localvar = "fooooobar"
    sub("fo+", "Meet me at the ", localvar) # localvar => "Meet me at the bar"
    gsub("e+", ".", localvar) # localvar => "m..t m. at th. bar"

    # Search for a string that matches a regular expression
    # index() does the same thing, but doesn't allow a regular expression
    match(localvar, "t") # => 4, since the 't' is the fourth character

    # Split on a delimiter
    split("foo-bar-baz", arr, "-") # a => ["foo", "bar", "baz"]

    # Other useful stuff
    sprintf("%s %d %d %d", "Testing", 1, 2, 3) # => "Testing 1 2 3"
    substr("foobar", 2, 3) # => "oob"
    substr("foobar", 4) # => "bar"
    length("foo") # => 3
    tolower("FOO") # => "foo"
    toupper("foo") # => "FOO"
}

function io_functions(    localvar) {

    # You've already seen print
    print "Hello world"

    # There's also printf
    printf("%s %d %d %d\n", "Testing", 1, 2, 3)

    # AWK doesn't have file handles, per se. It will automatically open a file
    # handle for you when you use something that needs one. The string you used
    # for this can be treated as a file handle, for purposes of I/O. This makes
    # it feel sort of like shell scripting:

    print "foobar" >"/tmp/foobar.txt"

    # Now the string "/tmp/foobar.txt" is a file handle. You can close it:
    close("/tmp/foobar.txt")

    # Here's how you run something in the shell
    system("echo foobar") # => prints foobar

    # Reads a line from standard input and stores in localvar
    getline localvar

    # Reads a line from a pipe
    "echo foobar" | getline localvar # localvar => "foobar"
    close("echo foobar")

    # Reads a line from a file and stores in localvar
    getline localvar <"/tmp/foobar.txt"
    close("/tmp/foobar.txt")
}

# As I said at the beginning, AWK programs consist of a collection of patterns
# and actions. You've already seen the all-important BEGIN pattern. Other
# patterns are used only if you're processing lines from files or standard
# input.
#
# When you pass arguments to AWK, they are treated as file names to process.
# It will process them all, in order. Think of it like an implicit for loop,
# iterating over the lines in these files. these patterns and actions are like
# switch statements inside the loop. 

/^fo+bar$/ {
    
    # This action will execute for every line that matches the regular
    # expression, /^fo+bar$/, and will be skipped for any line that fails to
    # match it. Let's just print the line:

    print

    # Whoa, no argument! That's because print has a default argument: $0.
    # $0 is the name of the current line being processed. It is created
    # automatically for you.

    # You can probably guess there are other $ variables. Every line is
    # implicitely split before every action is called, much like the shell
    # does. And, like the shell, each field can be access with a dollar sign

    # This will print the second and fourth fields in the line
    print $2, $4

    # AWK automatically defines many other variables to help you inspect and
    # process each line. The most important one is NF

    # Prints the number of fields on this line
    print NF

    # Print the last field on this line
    print $NF
}

# Every pattern is actually a true/false test. The regular expression in the
# last pattern is also a true/false test, but part of it was hidden. If you
# don't give it a string to test, it will assume $0, the line that it's
# currently processing. Thus, the complete version of it is this:

$0 ~ /^fo+bar$/ {
    print "Equivalent to the last pattern"
}

a > 0 {
    # This will execute once for each line, as long as a is positive
}

# You get the idea. Processing text files, reading in a line at a time, and
# doing something with it, particularly splitting on a delimiter, is so common
# in UNIX that AWK is a scripting language that does all of it for you, without
# you needing to ask. All you have to do is write the patterns and actions
# based on what you expect of the input, and what you want to do with it.

# Here's a quick example of a simple script, the sort of thing AWK is perfect
# for. It will read a name from standard input and then will print the average
# age of everyone with that first name. Let's say you supply as an argument the
# name of a this data file:
#
# Bob Jones 32
# Jane Doe 22
# Steve Stevens 83
# Bob Smith 29
# Bob Barker 72
#
# Here's the script:

BEGIN {

    # First, ask the user for the name
    print "What name would you like the average age for?"

    # Get a line from standard input, not from files on the command line
    getline name <"/dev/stdin"
}

# Now, match every line whose first field is the given name
$1 == name {

    # Inside here, we have access to a number of useful variables, already
    # pre-loaded for us:
    # $0 is the entire line
    # $3 is the third field, the age, which is what we're interested in here
    # NF is the number of fields, which should be 3
    # NR is the number of records (lines) seen so far
    # FILENAME is the name of the file being processed
    # FS is the field separator being used, which is " " here
    # ...etc. There are plenty more, documented in the man page.

    # Keep track of a running total and how many lines matched
    sum += $3
    nlines++
}

# Another special pattern is called END. It will run after processing all the
# text files. Unlike BEGIN, it will only run if you've given it input to
# process. It will run after all the files have been read and processed
# according to the rules and actions you've provided. The purpose of it is
# usually to output some kind of final report, or do something with the
# aggregate of the data you've accumulated over the course of the script.

END {
    if (nlines)
        print "The average age for " name " is " sum / nlines
}

```
Further Reading:

* [Awk tutorial](http://www.grymoire.com/Unix/Awk.html)
* [Awk man page](https://linux.die.net/man/1/awk)
* [The GNU Awk User's Guide](https://www.gnu.org/software/gawk/manual/gawk.html) GNU Awk is found on most Linux systems.
