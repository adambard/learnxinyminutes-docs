---
language: PHP
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
    - ["Walter Cordero", "http://waltercordero.com"]    
filename: learnphp.php
---

Este documento esta basado en PHP 5+.

```php
<?php // El código de PHP debe estar entre los tags <?php

// Esto si y solo si tu archivo .php contiene codigo PHP, lo cual es una buena práctica
// para omitir el tag de cierre de php.

// Dos diagonales inician una línea sencilla de comentario.

# Asi que un hash (el símbolo de numeral) tambien es un comentario, esta es la forma más común

/*
    Envolviendo un texto entre diagonal-asterisco y asterisco-diagonal
    se crea un comentario multi-línea.
*/

// Usando "echo" o "print" para mostrar texto en pantalla
print('Hola '); // Imprime "Holla " sin salto de línea.

// Los paréntesis () son opcionales para las instrucciones print y echo
echo "Mundo\n"; // Imprime "Mundo" con un salto de línea
// (todas las declaraciones deben finalizar con punto y coma ";")

// Cualquier texto fuera de los tags de php, será impreso en pantalla de manera automática
?>
Hola Mundo, otra vez!
<?php


/************************************
 * Tipos y Varialbes
 */

// Las variables inician con el simbolo $.
// Todo nombre válido para una variable inicia con una letra o un guión bajo "_",
// seguido de un conjunto de letras, números o más guiones bajos.

// Los valores Booleanos son case-sensitive (sensibles a mayúsculas y minúsculas)
$booleano = true;  // o TRUE o True
$booleano = false; // o FALSE o False

// Números enteros
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (un 0 indica un número en base octal)
$int4 = 0x0F; // => 15 (un 0x denota una litearl hexadecimal)

// Número flotante (o números decimales)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Eliminar variables
unset($int1);

// Operaciones Aritméticas
$sum        = 1 + 1; // 2
$difference = 2 - 1; // 1
$product    = 2 * 2; // 4
$quotient   = 2 / 1; // 2

// Operaciones Aritméticas abreviadas
$number = 0;
$number += 1;      // Incrementa la variable $number en 1
echo $number++;    // Imprime 1 (Incrementa el valor despues de la evaluación)
echo ++$number;    // Imprime 3 (Incrementa antes de la evaluación)
$number /= $float; // Dividie y asigna el valor resultate a la variable $number

// Las cadenas de texto deben ser encerradas en comillas simples;
$sgl_quotes = '$String'; // => '$String'

// Se utilizan las comillas dobles ( " " ), cuando se desea imprimir el valor de una variable
// dentro del mismo texto
$dbl_quotes = "This is a $sgl_quotes."; // => 'This is a $String.'

// Los caracteres especiales se deben escribir unicamente en comillas dobles
$escaped   = "Este texto contiene \t un caracter tabulador.";
$unescaped = 'Este texto solo contiene una diagonal invertida y unta t: \t';

// Se pueden encerrar entre llaves "{}" de ser necesario
$money = "Yo tengo $${number} en el banco.";

// Desde PHP 5.3, nowdocs puede ser utilizado para interpolar multiples líneas
$nowdoc = <<<'END'
Multi line
string
END;

// Heredocs permite crear una interpolación de cadenas de texto
$heredoc = <<<END
Multi line
$sgl_quotes
END;

// La concatenación de texto se hace a través del punto "."
echo 'Esta es una cadena texto ' . 'concatenada';


/********************************
 * Constantes
 */

// Una constante es definida utilizando la expresión define()
// y nunca debe ser alterado su valor durante el tiempo de ejecución de la programa!

// Todo nombre válido para una constante inicia con una letra o un guión bajo "_",
// seguido de un conjunto de letras, números o más guiones bajos.
define("FOO",     "algo");

// para acceder al valor de una constante basta con escribir el nombre de la misma.
echo 'Esto imprime '.FOO;


/********************************
 * Arreglos
 */

// Todos los Arreglos en PHP son Arreglos Asociatios (hashmaps),

// Los Arreglos asociativos, son conocidos como HashMaps en otros lenguajes de programación

// Trabajan perfectamente en todas las versiones de PHP
$associative = array('One' => 1, 'Two' => 2, 'Three' => 3);

// PHP 5.4 introdujo una una sintaxis de trabajo con Arreglos
$associative = ['One' => 1, 'Two' => 2, 'Three' => 3];

echo $associative['One']; // Imprime 1

// Las listas literales, asignan implícitamente valores de llave enteros
$array = ['One', 'Two', 'Three'];
echo $array[0]; // => "One"

// Para agregar un eleemento al final de un Arreglo
$array[] = 'Four';

// Para remover cualquier elemento de un Arreglo
unset($array[3]);

/********************************
 * Salida de información
 */

echo('Hello World!');
// Imprime Hello World! en la pantalla del navegador.

print('Hello World!'); // El mismo efecto que la función echo

// echo es en la actualidad un lenguaje constructor, así que se pueden omitir los parentesis.
echo 'Hello World!';
print 'Hello World!'; // Al igual que print

$paragraph = 'paragraph';

echo 100;        // Echo imprime valores directamente
echo $paragraph; // o bien puede iprimir el valor de una variable

// Si se encuentra configurada la manera abreviada de la apertura de los tags en PHP
// en la versión 5.0.4 o mas alta, se puede utilizar la función abreviada de echo
?>
<p><?= $paragraph ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // La variable $x contiene el mismo valor que la variable $y
$z = &$y;
// La variable $z ahora contiene una referencia de $y. Cambiando el valor de
// $z devolvero el valor de $y tambien, y vice-versa.
// $x se conserva sin cambios con el valor original de $y

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0

// Dump muestra información sobre una variable y lo imprime en pantalla
var_dump($z); // Imprime int(0)

// Print_r se utiliza para mostrar la información en un formato entendible
print_r($array); // imprime: Array ( [0] => One [1] => Two [2] => Three )

/********************************
 * Logica
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// la funcion assert devuelve una alerta si el argumento no es verdadero

// Las siguientes comparaciones siempre seran verdades, siempre y cuando los tipos no sean los mismos
assert($a == $b); // igualdad
assert($c != $a); // no iguales
assert($c <> $a); // alternativa a no iguales
assert($a < $c); // menor que
assert($c > $b); // mayor que
assert($a <= $b); // menor o igual que
assert($c >= $d); // mayor o igual que

// Estas validaciones seran verdaderas univamente si son iguales en valor y en tipo de dato
assert($c === $d); // estrictamente iguales en tipo y valor
assert($a !== $d); // estrictamente diferentes en tipo y valor
assert(1 === '1');
assert(1 !== '1');

// Las variables pueden ser convertidas a otros tipos, dependiendo el uso que se les de.

$integer = 1;
echo $integer + $integer; // => 2

$string = '1';
echo $string + $string; // => 2 (los textos son convertidos a enteros ya que son números)

$string = 'one';
echo $string + $string; // => 0
// Imprime 0 porque el operador "+" no puede convertir el texto 'one' a un número válido

// La conversión de tipos puede ser utilizado para convertir una variable a otro tipo de dato

$boolean = (boolean) 1; // => true

$zero = 0;
$boolean = (boolean) $zero; // => false

// Estas son funciones dedicadas a la conversión de tipos
$integer = 5;
$string = strval($integer);

$var = null; // valor Nulo o Null


/********************************
 * Estructuras de control
 */

if (true) {
    print 'Yo imprimo valor';
}

if (false) {
    print 'Yo no imprimo';
} else {
    print 'Yo imprimo valor';
}

if (false) {
    print 'Esto no sera impreso';
} elseif(true) {
    print 'El valor sera impreso';
}

// Operador ternario
print (false ? 'Esto no sera impreso' : 'El valor sera impreo');

// La versión abreviada del operador ternario existe desde PHP 5.3
// Es el equivalente a "$x ? $x : 'El valor sera impreso'""
$x = false;
print($x ?: 'El valor sera impreso');

// el operador null se incorcopora desde PHP 7
$a = null;
$b = 'Imprime el valor';
echo $a ?? 'a no tiene valor'; // imrpime 'a no tiene valor'
echo $b ?? 'b no tiene valor'; // imprime 'Imprime el valor'


$x = 0;
if ($x === '0') {
    print 'esto no se imprime';
} elseif($x == '1') {
    print 'esto no se imprime';
} else {
    print 'imprime el valor';
}



// Esta alternativa de sintaxis es utilizada para templates:
?>

<?php if ($x): ?>
Este texto es mostrado si la evaluacion es verdadera.    
<?php else: ?>
de lo contrario se imprime este otro texto
<?php endif; ?>

<?php

// Use switch to save some logic.
// el uso de switch sirve para guardar cierta logica.
switch ($x) {
    case '0':
        print 'Switch hace imprimir la coercion';
        break; // Se debe incluir el operador break, o se ejecutara todo el resto del código
               // en este caso seguira evaluando el caso 'two' y 'three'
    case 'two':
    case 'three':
        // Imprime algo ya sea si el valor de $variable es 'two' o 'three'
        break;
    default:
        // Hace algo por defecto si ninguno de los casos evaluados se cumple
}

// Los ciclos While, do...while y for son muy familiares en funcionamiento
$i = 0;
while ($i < 5) {
    echo $i++;
}; // imprime "01234"

echo "\n";

$i = 0;
do {
    echo $i++;
} while ($i < 5); // imprime "01234"

echo "\n";

for ($x = 0; $x < 10; $x++) {
    echo $x;
} // imprime "0123456789"

echo "\n";

$wheels = ['bicycle' => 2, 'car' => 4];

// El ciclo Foreach puede interactuar sobre los arreglos
foreach ($wheels as $wheel_count) {
    echo $wheel_count;
} // imprime "24"

echo "\n";

// También se puede interactuar sobre las llaves y sobre los valores de los arreglos
foreach ($wheels as $vehicle => $wheel_count) {
    echo "A $vehicle has $wheel_count wheels";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // Sale fuera de el ciclo While
    }
    echo $i++;
} // imprime "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // Omite esta interacion de el cilco
    }
    echo $i;
} // imprime "0124"


/********************************
 * Functiones
 */

// las funciones se definen con la palabra 'function'
function my_function () {
  return 'Hello';
}

echo my_function(); // => "Hello"

// Un nombre válido para una función inicia con una letra o un guión bajo, seguido por un 
// número de letras, numeros o guiones bajos

function add ($x, $y = 1) { // $y es un parametro opcional y su valor por defecto es 1
  $result = $x + $y;
  return $result;
}

echo add(4); // => 5
echo add(4, 2); // => 6

// $result no es accesible fuera de la función donde esta declarada
// print $result // mostrará una alerta

// Desde PHP 5.3 se pueden declarar funciones anónimas;
$inc = function ($x) {
  return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
  echo "$x - $y - $z";
}

// Las funciones pueden retornar una función
function bar ($x, $y) {
  // Se utiliza 'use' para proporcionar fuera una variale
  return function ($z) use ($x, $y) {
    foo($x, $y, $z);
  };
}

$bar = bar('A', 'B');
$bar('C'); // Imprime "A - B - C"

// tambien se puede nombrar a las funciones utilizando cadenas de texto
$function_name = 'add';
echo $function_name(1, 2); // => 3
// Útil para determinar por medio de la programación que función se debe ejecutar.
// O, se puede utilizar call_user_func(callable $callback [, $parametros [, ... ]]);

/********************************
 * Inclusión de archivos
 */

<?php
// los archivos incluidos en un archivo PHP tambiend eben iniciar con la etiqueta de apertura

include 'my-file.php';
// El código dentro de my-file.php es accesible en el el entorno actual
// Si el archivo no puede ser incluido (e.j. Archivo no encontrado), una alerta es emitida
include_once 'my-file.php';
// Si el archivo my-file.php ha sido incluido en otra parte del codigo, esto hará
// que no sea incluido otra vez. Esto previne error de múltiples declaraciones de clases.

require 'my-file.php';
require_once 'my-file.php';
// Al igual que include(), a excepción que require() causara un error fatal si el
// archivo incluido no es encontrado.

// contenido de my-include.php:
<?php

return 'Cualquier valor u objeto que deba devolver.';
// fin del archivo

// Include y Require puede tambien devolver un valor
$value = include 'my-include.php';

// Los archivos incluidos se basan en la dirección que se le proporciona, si ninguna dirección es brindada
// la función include_path entra en acción y lo configura, Si el archivo no es encontrado
// la función indluce_path, incluirá al final una llamada a la función por mediode un script
// al directorio actual donde se encuentre trabajando.
/* */

/********************************
 * Definición de Clases
 */

// Las clases se definen con la palabra clave 'class'

class MyClass
{
    const MY_CONST      = 'value'; // representa una constante

    static $staticVar   = 'static';

    // variables Estáticas y su disponibilidad
    public static $publicStaticVar = 'publicStatic';
    // Accesible únicamente dentro de la clase
    private static $privateStaticVar = 'privateStatic';
    // Accesible dentro de la clase y las sub-clases
    protected static $protectedStaticVar = 'protectedStatic';

    // Se deben declarar tambien la disponibilidad de las propiedades
    public $property    = 'public';
    public $instanceProp;
    protected $prot = 'protected'; // Accesible desde la clase y las sub-clases
    private $priv   = 'private';   // Accesible únicamente dentro de la clase

    // Los constructores se crean con '__construct'
    public function __construct($instanceProp) {
        // Se accede a las variables a traves del operador $this
        $this->instanceProp = $instanceProp;
    }

    // Los métodos son declarados como funciones dentro de las clases
    public function myMethod()
    {
        print 'MyClass';
    }

    // La palabra clave 'final' crea una función que no se puede sobreescribir
    final function youCannotOverrideMe()
    {
    }

/*
 * Al declarar las propieades o los métodos como 'static' hace que estos sean accesible
 * sin necesidad de la instancia de una clase. Una propiedad declarada como 'static' no 
 * puede ser accedida con una instancia de una clase objeto (un metodo si puede ser accedido)
*/
    public static function myStaticMethod()
    {
        print 'I am static';
    }
}

// Las constantes de una Clases siempre pueden ser accedidas estáticamente
echo MyClass::MY_CONST;    // Imprime 'value';

echo MyClass::$staticVar;  // imprime 'static';
MyClass::myStaticMethod(); // imprime 'I am static';

// Se crean instancias de una clase utilizando la palabra 'new'
$my_class = new MyClass('Asigna el valor a una propiedad');
// Los paréntesis pueden ser opcionales sino se envía ningún argumento.

// Se accede a los miembros de las clases utilizando ->
echo $my_class->property;     // => "public"
echo $my_class->instanceProp; // => "Asigna el valor a una propiedad"
$my_class->myMethod();        // => "MyClass"

// Se crea herencia en una clase utilizando la palabra 'extends'
class MyOtherClass extends MyClass
{
    function printProtectedProperty()
    {
        echo $this->prot;
    }

    // Sobreescribe el metodo de la clase padre
    function myMethod()
    {
        parent::myMethod();
        print ' > MyOtherClass ';
    }
}

$my_other_class = new MyOtherClass('Instancia la propiedad');
$my_other_class->printProtectedProperty(); // => Imprime "protected"
$my_other_class->myMethod();               // Imprime "MyClass > MyOtherClass"

final class YouCannotExtendMe
{
}

// Se pueden crear "métodos mágicos" para crear getters y setters
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
echo $x->property; // Utilizará el metodo __get() 
$x->property = 'Something'; // Utilizará el método __set() 

// Una Clase puede ser abstracta (utilizando la palabra 'abstract') o
// puede implementar una interfaz (utilizando la palabra 'implements').
// Una interfaz es declarada utilizando la palabra clave 'interface'

interface InterfaceOne
{
    public function doSomething();
}

interface InterfaceTwo
{
    public function doSomethingElse();
}

// Las interfaces pueden ser heredadas
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

// Las Clases pueden implementar mas de una interfaz
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
 * Namespaces / Espacios de Nombre
 */


// Esta sección es un apartado difrente, ya que la declaración de un namespace
// debe estar primero en un archivo aparte.
<?php

// Por defecto, las clases existen en un namespace global, y pueden
// ser llamadas explícitamente con una diagonal inversa "\"

$cls = new \MyClass();

//Asigna el namespace para un archivo
namespace My\Namespace;

class MyClass
{
}

// (de otro archivo)
$cls = new My\Namespace\MyClass;

// Or de otro namespace
namespace My\Other\Namespace;

use My\Namespace\MyClass;

$cls = new MyClass();

// O bien se utilizar un "alias" para el namespace
namespace My\Other\Namespace;

use My\Namespace as SomeOtherNamespace;

$cls = new SomeOtherNamespace\MyClass();

*/

```
## Para más información

Visita el sitio oficial de PHP[official PHP documentation](http://www.php.net/manual/) encontrarás referencias
y una gran comunidad.

Si estas interesado en aprender las buenas prácticas, visita
[PHP The Right Way](http://www.phptherightway.com/).

Si quieres iniciar con un lenguaje con un buen manejo de paquetes, visita
[Composer](http://getcomposer.org/).

Para los estándares comunes, visita El grupo de Inteporalidad para los frameworks de PHP 
[PSR standards](https://github.com/php-fig/fig-standards).