---
language: PHP
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
    - ["Ivan Alburquerque", "https://github.com/AlburIvan"]
filename: learnphp-es.php
lang: es-es
---

Este documento describe PHP 5+.

```php
<?php // El código PHP debe estar contenido dentro de la etiqueta <?php

// Si tu archivo php solo contiene código PHP, la mejor practica
// es omitir la etiqueta de cierre de php.

// Dos barras inclinadas comienzan un comentario de una linea.

# El numeral funciona de igual manera, pero // es mas común

/*
    Incluyendo el texto entre barra-asterisco y asterisco-barra
    lo hace un comentario de multiples lineas.
*/

// Use "echo" o "print" para imprimir una salida o resultado
print('Hello '); // Imprime "Hello " sin salto de linea

// los () son opcionales para print and echo
echo "World\n"; // Imprime "World" son un salto de linea
// (todas las declaraciones deben terminar con punto y coma)

// Todo lo que esté fuera de la etiqueta <?php será impreso automáticamente
?>
Hello World Again!
<?php


/************************************
 * Tipos y Variables
 */

// Las variables comienzan con el simbolo $.
// Un nombre de variable valido comienza con una letra o un guion bajo,
// seguido por cualquier numero de letras, números o guiones bajos.

// Los valores booleanos no son sensibles a mayúsculas y/o minúsculas
$boolean = true;  // TRUE o True
$boolean = false; // FALSE o False

// Enteros
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (un 0 al inicio denota números octales)
$int4 = 0x0F; // => 15 (un 0x al inicio denota un literal hexadecimal)

// Números flotantes (aka dobles)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Eliminar variables
unset($int1);

// Aritmética
$sum        = 1 + 1; // 2
$difference = 2 - 1; // 1
$product    = 2 * 2; // 4
$quotient   = 2 / 1; // 2

// Abreviaturas aritméticas
$number = 0;
$number += 1;      // Incrementa $number por 1
echo $number++;    // Imprime 1 (incrementa después de la evaluación)
echo ++$number;    // Imprime 3 (incrementa antes de la evaluación)
$number /= $float; // Divide y asigna la variable a $number

// Las cadenas deben ser incluidas en comillas simples;
$sgl_quotes = '$String'; // => '$String'

// Evita usar doble comillas exceptuando cuando se use variables
$dbl_quotes = "This is a $sgl_quotes."; // => 'This is a $String.'

// Los caracteres especiales solamente son escapados entre comillas dobles
$escaped   = "This contains a \t tab character.";
$unescaped = 'This just contains a slash and a t: \t';

// Incluya una variable entre llaves si es necesario
$money = "I have $${number} in the bank.";

// A partir de PHP 5.3, nowdocs se puede utilizar para  comentarios
// multi linea no interpoladas
$nowdoc = <<<'END'
Multi line
string
END;

// Heredocs hará cadenas interpoladas
$heredoc = <<<END
Multi line
$sgl_quotes
END;

// La concatenación de cadenas es mediante .
echo 'This string ' . 'is concatenated';


/********************************
 * Constantes
 */

// Una constante es definida usando define()
// y nunca podrá ser cambiada duranta la ejecución!

// Un nombre de constante valido comienza con una letra o guion bajo,
// seguido de cualquier número de letras, números o guiones bajos.
define("FOO",     "something");

// El acceso a las constantes es posible usando diréctamente el nombre escogido
echo 'This outputs '.FOO;


/********************************
 * Arreglos
 */

// Todos los arreglos en PHP son arreglos asociativos (hashmaps),

// arreglos asociativos, conocido como hashmaps en algunos lenguajes.

// Funciona con todas las versiones de PHP
$associative = array('One' => 1, 'Two' => 2, 'Three' => 3);

// PHP 5.4 introduce una nueva sintaxis
$associative = ['One' => 1, 'Two' => 2, 'Three' => 3];

echo $associative['One']; // imprime 1

// Las listas literales implicitamente asignan llaves de números enteros
$array = ['One', 'Two', 'Three'];
echo $array[0]; // => "One"

// Añade un elemento al final del arreglo
$array[] = 'Four';

// Elimina un elemento del arreglo
unset($array[3]);

/********************************
 * Salidas
 */

echo('Hello World!');
// Imprime Hello World! al stdout.
// Stdout es la página web, si es usado en un navegador.

print('Hello World!'); // Lo mismo que echo

// echo es en realidad un constructor de lenguaje,
// por lo tanto no es necesario el paréntesis.
echo 'Hello World!';
print 'Hello World!'; // print funciona de la misma manera

$paragraph = 'paragraph';

echo 100;        // Echo variables escalables directamente
echo $paragraph; // o variables

// Si las etiquetas cortas estan configuradas, o si tu versión de PHP es
// 5.4.0 o mayor, puedes usar la versión corta de la sintaxis de echo
?>
<p><?= $paragraph ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // $x ahora contiene el mismo valor que $y
$z = &$y;
// $z ahora contiene una referencia a $y. Cambiando el valor de
// $z cambiará el valor de $y de igual manera, y vice-versa.
// $x se quedará sin cambios como el valor original de $y

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0

// Vuelca el tipo y el valor de la variable al stdout
var_dump($z); // imprime int(0)

// Imprime variables al stdout en formato legible por humanos
print_r($array); // imprime: Array ( [0] => One [1] => Two [2] => Three )

/********************************
 * Lógica
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// assert lanza una advertencia si su argumento no es verdadero

// Estas comparaciones siempre serán verdaderas, incluso si los tipos 
// no son los mismos.
assert($a == $b); // igualdad
assert($c != $a); // desigualdad
assert($c <> $a); // desigualdad alternativa
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// Las siguientes solo serán verdaderas si los valores coinciden 
// y son del mismo tipo.
assert($c === $d);
assert($a !== $d);
assert(1 === '1');
assert(1 !== '1');

// Las variables pueden ser convertidas entre tipos, dependiendo del uso.

$integer = 1;
echo $integer + $integer; // => 2

$string = '1';
echo $string + $string; // => 2 (las cadenas son forzadas a enteros)

$string = 'one';
echo $string + $string; // => 0
// imprime 0 porque el operador + no puede convertir la cadena 'one' 
// a un número

// Type casting can be used to treat a variable as another type

$boolean = (boolean) 1; // => true

$zero = 0;
$boolean = (boolean) $zero; // => false

// Además hay funciones dedicadas para la conversion de tipos
$integer = 5;
$string = strval($integer);

$var = null; // valor nulo (Null)


/********************************
 * Estructuras de Control
 */

if (true) {
    print 'I get printed';
}

if (false) {
    print 'I don\'t';
} else {
    print 'I get printed';
}

if (false) {
    print 'Does not get printed';
} elseif(true) {
    print 'Does';
}

// operador ternario
print (false ? 'Does not get printed' : 'Does');

// acceso directo al operador ternario a partir de PHP 5.3
// equivalente al "$x ? $x : 'Does'""
$x = false;
print($x ?: 'Does');


// el operador de coalescencia nula (null coalesce) a partir de PHP 7
$a = null;
$b = 'Does print';
echo $a ?? 'a is not set'; // imprime 'a is not set'
echo $b ?? 'b is not set'; // imprime 'Does print'

$x = 0;
if ($x === '0') {
    print 'Does not print';
} elseif($x == '1') {
    print 'Does not print';
} else {
    print 'Does print';
}



// Esta es una sintaxis alternativa util para plantillas:
?>

<?php if ($x): ?>
This is displayed if the test is truthy.
<?php else: ?>
This is displayed otherwise.
<?php endif; ?>

<?php

// Usa switch para ahorrar lógica.
switch ($x) {
    case '0':
        print 'Switch does type coercion';
        break; // Debes incluir un break, de lo contrario caerás
               // en los cases 'two' y 'three'
    case 'two':
    case 'three':
        // Haz algo si $variable es 'two' or 'three'
        break;
    default:
        // Haz algo por defecto
}

// While, do...while y los ciclos for te resultarán familiares
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

$wheels = ['bicycle' => 2, 'car' => 4];

// Los ciclos Foreach pueden iterar sobre arreglos
foreach ($wheels as $wheel_count) {
    echo $wheel_count;
} // Imprime "24"

echo "\n";

// Puedes iterar sobre las llaves, así tambien como los valores
foreach ($wheels as $vehicle => $wheel_count) {
    echo "A $vehicle has $wheel_count wheels";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // Sale del ciclo while
    }
    echo $i++;
} // Imprime "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // Salta esta iteración en el ciclo
    }
    echo $i;
} // Imprime "0124"


/********************************
 * Funciones
 */

// Define una funcion con "function":
function my_function () {
  return 'Hello';
}

echo my_function(); // => "Hello"

// Un nombre de función valido comienza con una letra o un guion bajo,
// seguido de cualquier número de letras, números o guiones bajos.

function add ($x, $y = 1) { // $y es optional y tiene por defecto 1
  $result = $x + $y;
  return $result;
}

echo add(4); // => 5
echo add(4, 2); // => 6

// $result no es accesible fuera de la función
// imprime $result; // señala una advertencia.

// Desde PHP 5.3 puedes declarar funciones anónimas;
$inc = function ($x) {
  return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
  echo "$x - $y - $z";
}

// las Funciones pueden retornar funciones
function bar ($x, $y) {
  // Use 'use' para incluir variables de fuera
  return function ($z) use ($x, $y) {
    foo($x, $y, $z);
  };
}

$bar = bar('A', 'B');
$bar('C'); // Imprime "A - B - C"

// Puedes llamar funciones nombradas usando cadenas
$function_name = 'add';
echo $function_name(1, 2); // => 3
// Usado programaticamente para determinar que función usar.
// O, puedes usar call_user_func(callable $callback [, $parameter [, ... ]]);

/********************************
 * Inclusiones
 */

<?php
// Archivos incluidos con PHP tambien deben comenzar con la etiqueta de abierto.

include 'my-file.php';
// El código en my-file.php ahora esta disponible en el alcance actual.
// Si el archivo no puede ser incluido (e.g. file not found), una advertencia será emitida.

include_once 'my-file.php';
// Si el código en my-file.php ya ha sido incluido, no será incluido de nuevo.
// Esto previene errores por declaraciones multiples de clases

require 'my-file.php';
require_once 'my-file.php';
// Lo mismo que include(), exceptuando require() que causará un error fatal si
// el archivo no puede ser incluido.

// Contenido de my-include.php:
<?php

return 'Anything you like.';
// Final del archivo

// Includes y requires pueden tambien retornar un valor.
$value = include 'my-include.php';

// Los archivos incluidos basados en la ruta de archivo determinado o, 
// si no se da, la directiva de configuración include_path. Si el archivo
// no se encuentra en el include_path, include finalmente revisará
// el propio directorio del script que llama y el directorio de
// trabajo actual antes de fallar.

/* */

/********************************
 * Clases
 */

// Las clases son definidas con la palabra reservada class

class MyClass
{
    const MY_CONST      = 'value'; // Una constante

    static $staticVar   = 'static';

    // Las variables estáticas y su visibilidad
    public static $publicStaticVar = 'publicStatic';
    // Solo accesibles dentro de la clase
    private static $privateStaticVar = 'privateStatic';
    // Solo accesibles desde la clase y subclases
    protected static $protectedStaticVar = 'protectedStatic';

    // Las propiedades deben declarar su visibilidad
    public $property    = 'public';
    public $instanceProp;
    protected $prot = 'protected'; // solo accesible desde la clase y subclases
    private $priv   = 'private';   // solo accesible dentro de la clase

    // Crea un constructor con __construct
    public function __construct($instanceProp) {
        // accede a las variables de instancia con $this
        $this->instanceProp = $instanceProp;
    }

    // Los métodos son declarados como funciones dentro de las clases
    public function myMethod()
    {
        print 'MyClass';
    }

    // La palabra reservada final hará una funcion no sobreescribible
    final function youCannotOverrideMe()
    {
    }

/* 
* Declarando propiedades de las clases o métodos como estáticos los hace 
* accesibles sin la necesidad de instanciar la clase. las propiedades declaras
* como estáticas no pueden ser accesadas con una instancia del objeto de la clase (un método estático si puede)
*/


    public static function myStaticMethod()
    {
        print 'I am static';
    }
}

// las constantes de la clase siempre pueden ser accesadas estáticamente
echo MyClass::MY_CONST;    // Imprime 'value';

echo MyClass::$staticVar;  // Imprime 'static';
MyClass::myStaticMethod(); // Imprime 'I am static';

// Instancia una clase usando new
$my_class = new MyClass('An instance property');
// El paréntesis es opcional si no se usan argumentos.

// Accede a los miembros de una clase usando ->
echo $my_class->property;     // => "public"
echo $my_class->instanceProp; // => "An instance property"
$my_class->myMethod();        // => "MyClass"


// Extiende las clases usando "extends"
class MyOtherClass extends MyClass
{
    function printProtectedProperty()
    {
        echo $this->prot;
    }

    // Sobrescribe un método
    function myMethod()
    {
        parent::myMethod();
        print ' > MyOtherClass';
    }
}

$my_other_class = new MyOtherClass('Instance prop');
$my_other_class->printProtectedProperty(); // => Imprime "protected"
$my_other_class->myMethod();               // Imprime "MyClass > MyOtherClass"

final class YouCannotExtendMe
{
}

// Puedes usar "métodos mágicos" para crear métodos de obtención y establecimiento
// (getters y setters)
class MyMapClass
{
    private $property;

    public function __get($key)
    {
        return $this->$key;
    }

    public function __set($key, $value)
    {
        $this->$key = $value;
    }
}

$x = new MyMapClass();
echo $x->property; // Usará el método __get()
$x->property = 'Something'; // Usará el método __set()

// Las clases pueden ser abstractas (usando la palabra reservada abstract)
// o implementando unterfaces (usando la palabra reservada implements).
// Una interfaz es declarada con la palabra reservada interface.

interface InterfaceOne
{
    public function doSomething();
}

interface InterfaceTwo
{
    public function doSomethingElse();
}

// las interfaces pueden ser extendidas
interface InterfaceThree extends InterfaceTwo
{
    public function doAnotherContract();
}

abstract class MyAbstractClass implements InterfaceOne
{
    public $x = 'doSomething';
}

class MyConcreteClass extends MyAbstractClass implements InterfaceTwo
{
    public function doSomething()
    {
        echo $x;
    }

    public function doSomethingElse()
    {
        echo 'doSomethingElse';
    }
}


// Las clases pueden implementar más de una interfaz.
class SomeOtherClass implements InterfaceOne, InterfaceTwo
{
    public function doSomething()
    {
        echo 'doSomething';
    }

    public function doSomethingElse()
    {
        echo 'doSomethingElse';
    }
}


/********************************
 * Rasgos (Traits)
 */

// Los Rasgos están disponibles desde PHP 5.4.0 y son declarados usando "trait"

trait MyTrait
{
    public function myTraitMethod()
    {
        print 'I have MyTrait';
    }
}

class MyTraitfulClass
{
    use MyTrait;
}

$cls = new MyTraitfulClass();
$cls->myTraitMethod(); // Imprime "I have MyTrait"


/********************************
 * Espacio de Nombres (Namespaces)
 */

// Esta sección esta separada, porque una declaración de espacio de nombres
// debe ser la primera sentencia en un archivo. 
// Vamos a pretender que este no es el caso

<?php

// Por defecto las clases existen en un espacio de nombres global
// y pueden ser llamadas explicitamente con una barra invertida.

$cls = new \MyClass();



// Coloca el espacio de nombre para un archivo
namespace My\Namespace;

class MyClass
{
}

// (desde otro archivo)
$cls = new My\Namespace\MyClass;

// O desde dentro de otro espacio de nombre.
namespace My\Other\Namespace;

use My\Namespace\MyClass;

$cls = new MyClass();

// O puedes usar un alias para el espacio de nombre.

namespace My\Other\Namespace;

use My\Namespace as SomeOtherNamespace;

$cls = new SomeOtherNamespace\MyClass();

/**********************
*  Manejo de Errores
*  
*/

// manejos simples de errores pueden ser hechos con bloques try catch

try {
    // Haz algo
} catch ( Exception $e) {
    // Maneja la excepción
}

// Cuando se use bloques try catch en un ambiente con espacio de nombre, utilize

try { 
    // Haz algo
} catch (\Exception $e) { 
    // Maneja la excepción
}

// Excepciones personalizadas

class MyException extends Exception {}

try {
    
    $condition = true; 
    
    if ($condition) {
        throw new MyException('Something just happend');
    }
    
} catch (MyException $e) {
    // Maneja mi excepción
}

```

## Para más información

Visita la [documentación oficial de PHP](http://www.php.net/manual/) para referencia
y aportes de la comunidad.

Si estas interesado en las mejores practicas hasta la fecha, visita
[PHP The Right Way](http://www.phptherightway.com/).

Si vienes de un lenguaje con un buen manejador de paquetes, revisa
[Composer](http://getcomposer.org/).

Para los estandares comunes, visita el Grupo de Interoperatividad del Framework PHP
[PSR standards](https://github.com/php-fig/fig-standards).
