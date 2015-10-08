---
language: PHP
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
	- ["Adrián Carrascal", "https://github.com/acarrascalgarcia"]
lang: es-es
filename: learnphp.php
---


Este documento describe a PHP 5+.

```php
<?php // El código PHP debe estar encerrado entre las etiquetas <?php

// Si tu fichero php contiene únicamente código PHP, se recomienda
// omitir la etiqueta de cierre de php.

// Dos diagonales empiezan un comentario de linea.

# So will a hash (aka pound symbol) but // is more common

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
// Un nombre de variable válido empieza con una letra o guíon bajo,
// segudio de cualquier cantidad de letras, números o guíones bajos.

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
