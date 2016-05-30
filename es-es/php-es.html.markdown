---
language: PHP
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
    - ["Mario Pérez", "https://github.com/MarioPerezEsteso"]
lang: es-es
filename: learnphp-es.php
---

Este documento explica el funcionamiento de PHP 5+.

```php
<?php // El código PHP debe estar dentro de etiquetas <?php

// Si tu fichero php solo contiene código php, es una buena práctica
// omitir la etiqueta de cierre php para prevenir salidas acidentales.

// Dos barras comienzan un comentario de una línea.

# También lo hará una almohadilla, pero // es más común

/*
     Escribir texto entre una barra-asterisco y asterisco-barra
     crea un comentario multilínea.
*/

// Utiliza "echo" o "print" para imprimir por pantalla
print('Hola '); // Imprime "Hola " sin salto de línea

// () son opcionales para print y echo
echo "Mundo\n"; // Imprime "Mundo" con un salto de línea
// (todas las sentencias deben finalizar con un punto y coma)

// Cualquier cosa fuera de las etiquetas <?php se imprime automáticamente
?>
¡Hola Mundo de nuevo!
<?php


/************************************
 * Tipos y variables
 */

// Las variables comienzan con el símbolo $.
// Una variable válida comienza con una letra o guión bajo,
// seguida de cualquier cantidad de letras, números o guiones bajos.

// Las variables booleanas no distinguen entre mayúsculas o minúsculas
$boolean = true;  // o TRUE o True
$boolean = false; // o FALSE o False

// Enteros
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (un 0 al comienzo declara un número octal)
$int4 = 0x0F; // => 15 (un 0x al comienzo declara un hexadecimal)

// Floats (también conocidos como doubles)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Eliminar variable
unset($int1);

// Operaciones aritméticas
$suma        = 1 + 1; // 2
$diferencia  = 2 - 1; // 1
$producto    = 2 * 2; // 4
$cociente    = 2 / 1; // 2

// Operaciones aritméticas de escritura rápida
$numero = 0;
$numero += 1;      // Incrementa $numero en 1
echo $numero++;    // Imprime 1 (incremento después la evaluación)
echo ++$numero;    // Imprime 3 (incremento antes de la evaluación)
$numero /= $float; // Divide y asigna el cociente a $numero

// Las cadenas de caracteres deben declararse entre comillas simples
$sgl_quotes = '$String'; // => '$String'

// Evita utilizar comillas dobles excepto para embeber otras variables
$dbl_quotes = "This is a $sgl_quotes."; // => 'This is a $String.'

// Los caracteres especiales solo son válidos entre comillas dobles
$escaped   = "Esto contiene \t un caracter tabulador.";
$unescaped = 'Esto solo contiene una barra y una t: \t';

// Rodea una variable entre corchetes si es necesario
$dinero = "Tengo $${numero} en el banco.";

// Desde PHP 5.3, los nowdocs pueden ser utilizados para multilíneas no interpoladas
$nowdoc = <<<'END'
Multi line
string
END;

// Heredocs interpola cadenas de caracteres
$heredoc = <<<END
Multi line
$sgl_quotes
END;

// La concatenación de cadenas de caracteres se realiza con .
echo 'Esta cadena de caracteres ' . 'está concatenada';

// Las cadenas de caracteres pueden ser pasadas como parámetros en un echo
echo 'Multiples', 'Parametros', 'Validos';  // Devuelve 'MultiplesParametrosValidos'


/********************************
 * Constantes
 */

// Una constante se define utilizando define()
// y nunca puede ser cambiada en tiempo de ejecución

// un nombre válido para una constante debe comenzar con una letra o guión bajo,
// seguido por cualquier número de letras, números o guiones bajos.
define("FOO",     "algo");

// el acceso a una constante se puede realizar llamando a la variable elegida sin un símbolo de $
echo FOO; // Devuelve 'algo'
echo 'Esto imprime '.FOO;  // Devuelve 'Esto imprime algo'



/********************************
 * Arrays
 */

// Todos los arrays en PHP son asociativos (hashmaps),

// Los arrays asociativos son conocidos como hashmaps en algunos lenguajes.

// Funciona con todas las versiones de php
$asociativo = array('Uno' => 1, 'Dos' => 2, 'Tres' => 3);

// PHP 5.4 introdujo una nueva sintaxis
$asociativo = ['Uno' => 1, 'Dos' => 2, 'Tres' => 3];

echo $asociativo['Uno']; // imprime 1

// Lista literales implícitamente asignados con claves enteras
$array = ['Uno', 'Dos', 'Tres'];
echo $array[0]; // => "Uno"

// Añadir un elemento al final de un array
$array[] = 'Cuatro';
// o
array_push($array, 'Cinco');

// Eliminar un elemento de un array
unset($array[3]);

/********************************
 * Salidas por pantalla
 */

echo('¡Hola Mundo!');
// Imprime ¡Hola Mundo! en stdout.
// Stdout es la página web si se está ejecutando en un navegador.

print('!Hola Mundo!'); // Es lo mismo que echo

// No es necesario el paréntesis en echo y print
echo '¡Hola Mundo!';
print '¡Hola Mundo!';

$parrafo = 'parrafo';

echo 100;      // Haz echo de escalares directamente
echo $parrafo; // o de variables

// Si las etiquetas cortas estás configuradas y tu versión de PHP es
// la 5.4.0 o superior, puede utilizar la sintaxis abreviada de echo
?>
<p><?= $parrafo?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // $x ahora contiene el mismo valor que $y
$z = &$y;
// $z contiene ahora una referencia a $y. Un cambio en el valor de
// $z cambiará también el valor de $y, y viceversa.
// $x sin embargo, tendrá el valor original de $y

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0

// Dump muestra el tipo y valor de una variable en stdout
var_dump($z); // imprime int(0)

// Para mostrar el valor de una variable en un formato legible para humanos
print_r($array); // imprime: Array ( [0] => Uno [1] => Dos [2] => Tres )

/********************************
 * Lógica
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// assert lanza una advertencia si su argumento no es verdadero

// Estas comparaciones siempre serán verdaderas, incluso si los tipos no son los mismos.
assert($a == $b); // igualdad
assert($c != $a); // desigualdad
assert($c <> $a); // desigualdad alternativa
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// Los siguiente solo será verdadero si los valores coinciden y son del mismo tipo.
assert($c === $d);
assert($a !== $d);
assert(1 === '1');
assert(1 !== '1');

// Operador 'Spaceship' (desde PHP 7)
// Devuelve 0 si ambos valores son iguales
// Devuelve 1 si el valor de la izquierda es mayor
// Devuelve -1 si el valor de la derecha es mayor

$a = 100;
$b = 1000;

echo $a <=> $a; //  0 porque son iguales
echo $a <=> $b; // -1 porque $a < $b
echo $b <=> $a; //  1 porque $b > $a

// Las variables pueden ser convertidas entre tipos, dependiendo de su uso.

$entero = 1;
echo $entero + $entero; // => 2

$string = '1';
echo $string + $string; // => 2 (los strings son convertidos a enteros)

$string = 'uno';
echo $string + $string; // => 0
// Muestra un 0 porque el operador + no puede convertir la cadena de caracteres 'uno' a un número

// La conversión de tipos puede ser utilizada para tratar a una variable como otro tipo

$boolean = (boolean) 1; // => true

$cero = 0;
$boolean = (boolean) $cero; // => false

// También hay funciones dedicadas a la conversión de tipos
$entero = 5;
$string = strval($entero);

$var = null; // Valor nulo


/********************************
 * Estructuras de control
 */

if (true) {
    print 'He sido imprimido';
}

if (false) {
    print 'Yo no';
} else {
    print 'He sido imprimido';
}

if (false) {
    print 'No se imprime';
} elseif(true) {
    print 'Sí se imprime';
}

// operador ternario
print (false ? 'No se imprime' : 'Sí se imprime');

// atajo para el operador ternario desde PHP 5.3
// equivalente de "$x ? $x : 'Sí'""
$x = false;
print($x ?: 'Sí');

// operador 'no definido' desde php 7
$a = null;
$b = 'Imprime';
echo $a ?? 'a no está definido'; // imprime 'a no está definido'
echo $b ?? 'b no está definido'; // imprime 'Imprime'


$x = 0;
if ($x === '0') {
    print 'No imprime';
} elseif($x == '1') {
    print 'No imprime';
} else {
    print 'Imprime';
}



// Esta sintaxis alternativa se utiliza para plantillas:
?>

<?php if ($x): ?>
Esto se muestra si la evaluación es verdadera.
<?php else: ?>
En otro caso, se muestra esto.
<?php endif; ?>

<?php

// Utiliza el switch para tener algo más de lógica.
switch ($x) {
    case '0':
        print 'Switch does type coercion';
        break; // Debes incluir un break para no seguir con los casos 'Dos' y 'Tres'
    case 'Dos':
    case 'Tres':
        // Hacer algo si la variables es 'Dos' o 'Tres'
        break;
    default:
        // Hacer algo por defecto
}

// Los bucles While, do...while y for te serán familiares
$i = 0;
while ($i < 5) {
    echo $i++;
}; // Imprime "01234"

echo "\n";

$i = 0;
do {
    echo $i++;
} while ($i < 5); // Imprime "01234"

echo "\n";

for ($x = 0; $x < 10; $x++) {
    echo $x;
} // Imprime "0123456789"

echo "\n";

$ruedas = ['bicicleta' => 2, 'coche' => 4];

// Los bucles foreach pueden iterar por arrays
foreach ($ruedas as $numero_ruedas) {
    echo $numero_ruedas;
} // Imprime "24"

echo "\n";

// También se puede iterar sobre las claves, así como sobre los valores
foreach ($ruedas as $vehiculo => $numero_ruedas) {
    echo "Un $vehiculo tiene $numero_ruedas ruedas";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // Sale fuera del bucle while
    }
    echo $i++;
} // Imprime "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // Se salta esta iteración del bucle
    }
    echo $i;
} // Imprime "0124"


/********************************
 * Funciones
 */

// Define una función con "function":
function mi_funcion () {
    return 'Hola';
}

echo mi_funcion(); // => "Hola"

// Un nombre válido de función comienza con una letra o guión bajo, seguido de cualquier
// número de letras, números o guiones bajos.

function anadir ($x, $y = 1) { // $y es opcional y por defecto es 1
    $resultado = $x + $y;
    return $resultado;
}

echo anadir(4); // => 5
echo anadir(4, 2); // => 6

// $resultado no es accesible fuera de la función
// print $resultado; // Devuelve una advertencia.

// Desde PHP 5.3 se pueden declarar funciones anónimas
$inc = function ($x) {
    return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
    echo "$x - $y - $z";
}

// Las funciones pueden devolver funciones
function bar ($x, $y) {
    // Utiliza 'use' para meter variables de fuera de la función
    return function ($z) use ($x, $y) {
        foo($x, $y, $z);
    };
}

$bar = bar('A', 'B');
$bar('C'); // Imprime "A - B - C"

// Puedes llamar a funciones utilizando cadenas de caracteres
$nombre_funcion = 'add';
echo $nombre_funcion(1, 2); // => 3
// Es útil para determinarl qué función ejecutar.
// O, utiliza call_user_func(callable $callback [, $parameter [, ... ]]);


// Puedes obtener todos los parámetros pasados a una función
function parametros() {
    $numero_argumentos = func_num_args();
    if ($numero_argumentos > 0) {
        echo func_get_arg(0) . ' | ';
    }
    $args_array = func_get_args();
    foreach ($args_array as $key => $arg) {
        echo $key . ' - ' . $arg . ' | ';
    }
}

parametros('Hola', 'Mundo'); // Hola | 0 - Hola | 1 - Mundo |

// Desde PHP 5.6 se puede obtener un número variable de argumentos
function variable($palabra, ...$lista) {
	echo $palabra . " || ";
	foreach ($lista as $item) {
		echo $item . ' | ';
	}
}

variable("Separa", "Hola", "Mundo") // Separa || Hola | Mundo |

/********************************
 * Includes
 */

<?php
// Los ficheros PHP incluidos deben comenzar también con la etiqueta de <?php

include 'mi-fichero.php';
// El código de mi-fichero.php ya está disponible en el entorno actual.
// Si el fichero no puede ser incluido (por ejemplo porque no se ha encontrado),
// se muestra una advertencia.

include_once 'mi-fichero.php';
// Si el código del fichero mi-fichero.php ya ha sido incluido, ya no se
// incluirá de nuevo. Este previene errores por múltiples declaraciones.

require 'mi-fichero.php';
require_once 'mi-fichero.php';
// Es lo mismo que el include(), pero require() causará un error fatal si el archivo
// no ha podido ser incluido.

// Contenido de mi-include.php:
<?php

return 'Cualquier cosa.';
// acabar archivo

// Los include y require también pueden devolver un valor.
$valor = include 'mi-include.php';

// Los archivos son incluidos en función de la ruta data o, si ninguna ruta es
// especificada se utilizará la directiva de configuración de include_path. Si el
// fichero no se encuentra en el include_path, include comprobará la ruta del código
// que lo llama antes de fallar.
/* */

/********************************
 * Clases
 */

// Las clases son definidas con la palabra clave class

class MiClase
{
    const MI_CONSTANTE      = 'valor'; // Una constante

    static $staticVar   = 'static';

    // Las variables estáticas y su visibilidad
    public static $publicStaticVar = 'publicStatic';
    // Accesible solo dentro de su clase
    private static $privateStaticVar = 'privateStatic';
    // Accesible desde la clase y las subclases
    protected static $protectedStaticVar = 'protectedStatic';

    // Las propiedades deben declarar su visibilidad
    public $propiedad    = 'public';
    public $instanceProp;
    protected $prot = 'protected'; // Accesible desde la clase y las subclases
    private $priv   = 'private';   // Accesible solo desde la clase

    // Crear un constructor con __construct
    public function __construct($instanceProp) {
        // Accede a las variables de la instancia con $this
        $this->instanceProp = $instanceProp;
    }

    // Los métodos son declarados como funciones dentro de una clase
    public function miMetodo()
    {
        print 'MiClase';
    }

    // la palabra clave final hará una función no sobreescribible
    final function noMePuedesSobreEscribir()
    {
    }

/*
 * Declarar propiedades de clase o métodos como estáticos los hace accesibles sin
 * necesidad de instanciar la clase. Una propiedad declarada como estática no
 * puede ser accedida mediante una instancia de la clase, pero sí mediante un
 * método estático.
 */

    public static function miMetodoEstatico()
    {
        print 'Soy estático';
    }
}

// Las constantes de una clase siempre pueden ser accedidas estáticamente
echo MiClase::MI_CONSTANTE;    // Muestra 'valor';

echo MiClase::$staticVar;  // Muestra 'static';
MiClase::miMetodoEstatico(); // Muestra 'Soy estático';

// Instancia una clase usando new
$mi_clase = new MiClase('Una instancia');
// Los paréntesis son opcionales si no se pasa ningún argumento.

// Accede a los miembros de una clase utilizando ->
echo $mi_clase->propiedad;     // => "public"
echo $mi_clase->instanceProp; // => "Una instancia"
$mi_clase->miMetodo();        // => "MiClase"


// Extender clases utilizando "extends"
class MiOtraClase extends MiClase
{
    function imprimePropiedadProtegida()
    {
        echo $this->prot;
    }

    // Sobreescribe un método
    function miMetodo()
    {
        parent::miMetodo();
        print ' > MiOtraClase';
    }
}

$mi_otra_clase = new MiOtraClase('Propiedad de instancia');
$mi_otra_clase->imprimePropiedadProtegida(); // => Imprime "protected"
$mi_otra_clase->miMetodo();               // Imprime "MiClase > MiOtraClase"

final class NoMePuedesExtender
{
}

// Puedes utilizar "métodos mágicos" para crear los getters y setters
class MiClaseMapeada
{
    private $propiedad;

    public function __get($key)
    {
        return $this->$key;
    }

    public function __set($key, $value)
    {
        $this->$key = $value;
    }
}

$x = new MiClaseMapeada();
echo $x->propiedad; // Utilizará el método __get()
$x->propiedad = 'Algo'; // Utilizará el método __set()

// Las clases pueden ser abstractas (utilizando la palabra clave abstract) o
// implementando interfaces (utilizando la palabra clave implements).
// Una interfaz puede ser declarada con la palabra clave interface.

interface InterfazUno
{
    public function hazAlgo();
}

interface InterfazDos
{
    public function hazOtraCosa();
}

// las interfaces pueden ser extendidas
interface InterfazTres extends InterfazDos
{
    public function hazCualquierOtraCosa();
}

abstract class MiClaseAbstracta implements InterfazUno
{
    public $x = 'hazAlgo';
}

class MiOtraClase extends MiClaseAbstracta implements InterfazDos
{
    public function hazAlgo()
    {
        echo $x;
    }

    public function hazOtraCosa()
    {
        echo 'hazOtraCosa';
    }
}


// Las clases pueden implementar más de una interfaz
class CualquierOtraClase implements InterfazUno, InterfazDos
{
    public function hazAlgo()
    {
        echo 'hazAlgo';
    }

    public function hazOtraCosa()
    {
        echo 'hazOtraCosa';
    }
}


/********************************
 * Traits
 */

// Los traits están disponibles desde PHP 5.4.0 y son declarados utilizando "trait"

trait MiTrait
{
    public function miMetodoTrait()
    {
        print 'Tengo trait';
    }
}

class MiClaseTrait
{
    use MiTrait;
}

$cls = new MiClaseTrait();
$cls->miMetodoTrait(); // Imprime "Tengo trait"


/********************************
 * Namespaces
 */

// Esta sección está separada porque una declaración de namespace debe
// ser la primera sentencia en un archivo. Vamos a suponer que no es el caso

<?php

// Por defecto, las clases existen en el namespace global y pueden ser llamadas
// explícitamente con una contrabarra.

$cls = new \MiClase();



// Estableder el namespace para un archivo
namespace Mi\Namespace;

class MiClase
{
}

// (de otro archivo)
$cls = new Mi\Namespace\MiClase;

// O de otro namespace.
namespace Mi\Otro\Namespace;

use Mi\Namespace\MiClase;

$cls = new MiClase();

// O se puede asignar un ales al namespace

namespace Mi\Otro\Namespace;

use Mi\Namespace as OtroNamespace;

$cls = new OtroNamespace\MiClase();


/**********************
* Late Static Binding
*
*/

class ClasePadre {
    public static function quien() {
        echo "Soy una " . __CLASS__ . "\n";
    }
    public static function test() {
        // Auto referencia a la clase en la que el método está definido
        self::quien();
        // Referencia estáticamente a la clase donde el método ha sido llamado
        static::quien();
    }
}

ClasePadre::test();
/*
Soy una ClasePadre
Soy una ClasePadre
*/

class ClaseHija extends ClasePadre {
    public static function quien() {
        echo "Pero soy una " . __CLASS__ . "\n";
    }
}

ClaseHija::test();
/*
Soy una ClasePadre
Pero soy una ClaseHija
*/


/**********************
*  Manejo de errores
*  
*/

// Una simple gestión de errores puede ser realizada con un bloque try catch

try {
    // Haz algo
} catch (Exception $e) {
    // Maneja la excepción
}

// Cuando se utilicen bloques try catch en un entorno con namespaces hay que
// usar lo siguiente

try {
    // Haz algo
} catch (\Exception $e) {
    // Maneja la excepción
}

// Excepciones personalizadas

class MiExcepcion extends Exception {}

try {

    $condicion = true;

    if ($condicion) {
        throw new MiExcepcion('Ha pasado algo');
    }

} catch (MiExcepcion $e) {
    // Manejar la excepción
}

```

## Más información

Visita la [documentación oficial de PHP](http://www.php.net/manual/) para más referencias
y apoyo de la comunidad.

Si estás interesado en buenas prácticas, visita
[PHP The Right Way](http://www.phptherightway.com/).

Si vienes de un lenguaje con una buena gestión de paquetes, visita
[Composer](http://getcomposer.org/).

Para estándares comunes, visita el PHP Framework Interoperability Group
[PSR standards](https://github.com/php-fig/fig-standards).
