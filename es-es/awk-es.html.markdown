---
category: tool
tool: awk
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
    localvar = atan2(b, a) # arcotangente de b / a

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

    # AWK tiene algunas funciones para procesamiento de strings,
    # y muchas dependen fuertemente en expresiones regulares.

    # Buscar y remplazar, primer instancia (sub) o todas las instancias (gsub)
    # Ambas regresan el número de matches remplazados.
    localvar = "fooooobar"
    sub("fo+", "Meet me at the ", localvar) # localvar => "Meet me at the bar"
    gsub("e+", ".", localvar) # localvar => "m..t m. at th. bar"

    # Buscar una cadena que haga match con una expresión regular
    # index() hace lo mismo, pero no permite expresiones regulares
    match(localvar, "t") # => 4, dado que 't' es el cuarto caracter

    # Separar con base en un delimitador
    split("foo-bar-baz", arr, "-") # a => ["foo", "bar", "baz"]

    # Otras funciones útiles
    sprintf("%s %d %d %d", "Testing", 1, 2, 3) # => "Testing 1 2 3"
    substr("foobar", 2, 3) # => "oob"
    substr("foobar", 4) # => "bar"
    length("foo") # => 3
    tolower("FOO") # => "foo"
    toupper("foo") # => "FOO"
}

function io_functions(    localvar) {

    # Ya has visto print
    print "Hello world"

    # También hay printf
    printf("%s %d %d %d\n", "Testing", 1, 2, 3)

    # AWK no tiene handles de archivos en sí mismo. Automáticamente abrirá un 
    # handle de archivo cuando use algo que necesite uno. El string que usaste 
    # para esto puede ser tratada como un handle de archivo para propósitos de I/O.
    # Esto lo hace similar al scripting de shell:

    print "foobar" >"/tmp/foobar.txt"

    # Ahora el string "/tmp/foobar.txt" es un handle. Puedes cerrarlo:
    close("/tmp/foobar.txt")

    # Aquí está como correr algo en el shell
    system("echo foobar") # => muestra foobar

    # Lee una línea de la entrada estándar (stdin) y lo guarda en localvar
    getline localvar

    # Lee una línea desde un pipe
    "echo foobar" | getline localvar # localvar => "foobar"
    close("echo foobar")

    # Lee una línea desde un archivo y la guarda en localvar
    getline localvar <"/tmp/foobar.txt"
    close("/tmp/foobar.txt")
}

# Como dije al inicio, los programas en AWK son una colección de patrones y 
# acciones. Ya conociste el patrón BEGIN. otros patrones sólo se usan si estás
# procesando líneas desde archivos o stdin.

# Cuando pasas argumentos a AWK, son tratados como nombres de archivos a 
# procesar. Los va a procesar todos, en orden. Imagínalos como un ciclo for 
# implícito, iterando sobre las líneas de estos archivos. Estos patrones y
# acciones son como instrucciones switch dentro del ciclo.

/^fo+bar$/ {
    
    # Esta acción se ejecutará por cada línea que haga match con la expresión
    # regular /^fo+bar$/, y será saltada por cualquier línea que no haga match.
    # Vamos a sólo mostrar la línea:

    print

    # ¡Wow, sin argumento! Eso es porque print tiene uno por defecto: $0.
    # $0 es el nombre de la línea actual que se está procesando.
    # Se crea automáticamente para ti.

    # Probablemente puedas adivinar que hay otras variables $. Cada línea es 
    # separada implícitamente antes de que se llame cada acción, justo como lo
    # hace shell. Y, como shell, cada campo puede ser accesado con $.

    # Esto mostrará el segundo y cuarto campos de la línea
    print $2, $4

    # AWK automáticamente define muchas otras variables que te ayudan a
    # inspeccionar y procesar cada línea. La más importante es NF

    # Imprime el número de campos de esta línea
    print NF

    # Imprime el último campo de esta línea
    print $NF
}

# Cada patrón es realmente un prueba de verdadero/falso. La expresión regular
# en el último patrón también es una prueba verdadero/falso, pero parte de eso
# estaba oculto. Si no le das un string a la prueba, supondrá $0, la línea que
# se está procesando. La versión completa de esto es:

$0 ~ /^fo+bar$/ {
    print "Equivalente al último patrón"
}

a > 0 {
    # Esto se ejecutará una vez por línea, mientras a sea positivo
}

# Y ya te das una idea. Procesar archivos de texto, leyendo una línea a la vez,
# y haciendo algo con ella, particularmente separando en un deliminator, es tan
# común en UNIX que AWK es un lenguaje de scripting que hace todo eso por ti
# sin que tengas que pedirlo. Basta con escribir los patrones y acciones 
# basados en lo que esperas de la entrada y lo quieras quieras hacer con ella.

# Aquí está un ejemplo de un script simple, para lo que AWK es perfecto.
# El script lee un nombre de stdin y muestra el promedio de edad para todos los
# que tengan ese nombre. Digamos que como argumento pasamos el nombre de un
# archivo con este contenido:
#
# Bob Jones 32
# Jane Doe 22
# Steve Stevens 83
# Bob Smith 29
# Bob Barker 72
#
# Éste es el script:

BEGIN {

    # Primero, pedir al usuario el nombre
    print "¿Para qué nombre quieres el promedio de edad?"

    # Recuperar una línea de stdin, no de archivos en la línea de comandos
    getline name <"/dev/stdin"
}

# Ahora, hacer match con cada línea cuyo primer campo es el nombre dado
$1 == name {

    # Aquí dentro tenemos acceso a variables útiles precargadas:
    # $0 es toda la línea
    # $3 es el tercer campo, la edad, que es lo que nos interesa
    # NF es el número de campos, que debe ser 3
    # NR es el número de registros (líneas) vistos hasta ahora
    # FILENAME es el nombre del archivo que está siendo procesado
    # FS es el campo separador, " " en este caso
    # Y muchas más que puedes conocer ejecutando 'man awk' en la terminal.

    # Llevar el registro de la suma y cuantas líneas han hecho match.
    sum += $3
    nlines++
}

# Otro patrón especial es END. Va a ejecutarse después de procesar todos los 
# archivos de texto. A diferencia de BEGIN, sólo se ejecuta si le das dado una
# entrada a procesar. Se ejecutará después de que todos los archivos hayan sido
# leídos y procesados según las reglas y acciones que programaste. El propósito
# es usualmente para mostrar un reporte final, o hacer algo con el agregado de
# los datos que has acumulado durante la ejecución del script.

END {
    if (nlines)
        print "La edad promedio para " name " es " sum / nlines
}

```
Más información:

* [Tutorial de AWK](http://www.grymoire.com/Unix/Awk.html)
* [Página man de AWK](https://linux.die.net/man/1/awk)
* [La guía del usuario de GNU Awk](https://www.gnu.org/software/gawk/manual/gawk.html): GNU Awk se encuentra en la mayoría de los sistemas Linux.
