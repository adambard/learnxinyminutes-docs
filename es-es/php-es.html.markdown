---
language: PHP
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
	- ["Adrián Carrascal", "https://github.com/acarrascalgarcia"]
lang: es-es
filename: learnphp-es.php
---


Este documento describe a PHP 5+.

```php
<?php // El código PHP debe estar encerrado entre las etiquetas <?php

// Si tu fichero php contiene únicamente código PHP, se recomienda
// omitir la etiqueta de cierre de php.

// Dos diagonales empiezan un comentario de linea.

# Lo mismo ocurrirá con numeral but // es mucho más común

/*
	 Encerrar el texto con diagonal-asterico y asterisco-diagonal
	 lo hace un comentario de varias lineas.
*/

// Use "echo" or "print" para imprimir
print('Hola '); // Imprime "Hola" sin salto de linea

// Los paréntesis "()" son opcionales en "print" y "echo"
echo "Mundo\n"; // Imprime "Mundo" con un salto de linea
// (Cada línea debe terminar con un punto y coma ";")

// Cualquier palabra afuera de las etiquetas <?php se imprime automáticamente
?>
¡Hola Mundo de nuevo!
<?php


/************************************
 * Tipos y variables
 */

// Las variables comienzan con el símbolo $.
// Un nombre valido de variable empieza con una letra o guíon bajo,
// seguido de cualquier cantidad de letras, números o guíones bajos.

// Los valores booleanos no son sensibles a mayúsculas y minúsculas.
$boolean = true;  // or TRUE or True
$boolean = false; // or FALSE or False

// Enteros
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (el 0 al inicio lo define como un número octal)
$int4 = 0x0F; // => 15 (el 0x al inicio lo define como un número hexadecimal)

// Flotantes (también conocidos como "Doubles")
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Eliminar variable
unset($int1);

// Aritméticas
$sum        = 1 + 1; // 2
$difference = 2 - 1; // 1
$product    = 2 * 2; // 4
$quotient   = 2 / 1; // 2

// Abreviaciones aritméticas
$number = 0;
$number += 1;      // Incrementa a $number en 1
echo $number++;    // Imprime 1 (incrementa después de la evaluación)
echo ++$number;    // Prints 3 (incrementa antes de la evaluación)
$number /= $float; // Divide and asigna el cociente a $number

// Los strings se deben encerrar en comillas simples;
$sgl_quotes = '$String'; // => '$String'

// Evita usar comillas dobles a no ser que hayan otras variables dentro.
$dbl_quotes = "Esto es un $sgl_quotes."; // => 'This is a $String.'

// Los caracteres especiales se escapan en comillas dobles
$escaped   = "Esto contiene un \t carácter de tabulación.";
$unescaped = 'Esto solo contiene una diagonal y una t: \t';

// Encierra una variable entre llaves si es necesario
$money = "Tengo $${number} en el banco.";

// Desde PHP 5.3, los nowdocs se puede usar para multi-liners interpolados
$nowdoc = <<<'END'
Multi linea
string
END;

// Los heredocs harán la interpolación de strings
$heredoc = <<<END
Multi linea
$sgl_quotes
END;

// La concatenación de strings se hace con
echo 'Este string ' . 'se concatenó';


/********************************
 * Constantes
 */

// Una constante se define usando define()
// y no se puede cambiar en tiempo de ejecución!

// Un nombre válido de constante empieza con una letra o un guíon bajo,
// seguido por cualquier cantidad de letras, números o guíones bajos.
define("FOO",     "algo");

// Acceder a una constante es posible a través del nombre elegido
echo 'Esto muestra '.FOO;


/********************************
 * Arrays
 */

// Todos los arrays en PHP son arrays asociativos (hashmaps),

// Arrays asociativos, hashmaps en algunos lenguajes.

// Trabaja con todas las versiones de PHP
$associative = array('Uno' => 1, 'Dos' => 2, 'Tres' => 3);

// PHP 5.4 introdujo una sintaxis nueva
$associative = ['Uno' => 1, 'Dos' => 2, 'Tres' => 3];

echo $associative['Uno']; // imprime 1

// 
$array = ['Uno', 'Dos', 'Tres'];
echo $array[0]; // => "Uno"

// Agrega un elemento al final del array
$array[] = 'Four';

// Elimina un elemento del array
unset($array[3]);

/********************************
 * Output
 */

echo('¡Hola Mundo!');
// Imprime ¡Hola Mundo! a stdout.
// Stdout es la página web si se está ejecutando en un 
// navegador.

print('¡Hola Mundo!'); // Igual que "echo"

// echo es realmente un constructor del lenguaje,
// así que también lo puedes hacer sin los paréntesis.
echo '¡Hola Mundo!';
print '¡Hola Mundo!'; // Este es el "print"

$paragraph = 'paragraph';

echo 100;        // Imprima variables escalares directamente
echo $paragraph; // o variables

// Si las etiquetas abreviadas de apertura se configuraron,
// o tu versión de PHP es 5.4.0 o mayor,
// puedes usar la sintaxis abreviada del echo
?>
<p><?= $paragraph ?></p>
<?php



```

## Más información

Visitar la [Documentación oficial de PHP](http://www.php.net/manual/) como referencia
y aportes de la comunidad.

If you're interested in up-to-date best practices, visit
[PHP The Right Way](http://www.phptherightway.com/).

If you're coming from a language with good package management, check out
[Composer](http://getcomposer.org/).

For common standards, visit the PHP Framework Interoperability Group's
[PSR standards](https://github.com/php-fig/fig-standards).