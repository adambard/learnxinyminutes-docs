---
language: PHP
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
    - ["Jerodev", "https://github.com/jerodev"]
lang: nl-nl
filename: learnphp.php
---

Dit document beschrijft PHP 5+.

```php
<?php // PHP code bevindt zich steeds tussen <?php tags

// Indien je php bestand enkel php code bevat is het best
// om de sluit tag te laten vallen.

// Twee slashes starten een enkele commentaar regel

# Een hash teken doet dit ook maar // komt meer voor

/*
     Tekst tussen slash-asterisk en asterisk-slash
     creëert een commentaar over meerdere lijnen.
*/

// Gebruik "echo" of "print" om tekst naar het scherm te schrijven
print('Hallo '); // Dit print "Hallo " zonder een nieuwe lijn op het einde

// () zijn optioneel voor "print" en "echo"
echo "Wereld\n"; // Print "Wereld" met een nieuwe lijn op het einde
// (Iedere code regel moet afgesloten worden met een puntkomma)

// Alles buiten <?php tags wordt automatisch afgedrukt op het scherm
?>
Hallo Wereld Opnieuw!
<?php


/************************************
 * Types & Variables
 */

// Variabelen beginnen altijd met het $ symbool.
// Een geldige variable naam start altijd met een letter of een underscore,
// gevolgd door een reeks van nummers, letters of underscores.

// Boolean waardes zijn niet hoofdletter gevoellig
$boolean = true;  // of TRUE of True
$boolean = false; // of FALSE of False

// Integers
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (een 0 voor het nummer stelt een octaal nummer voor)
$int4 = 0x0F; // => 15 (een 0x voor het nummer stelt een hexadecimaal nummer voor)

// Kommagetallen (aka floats & doubles)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Variabelen verwijderen
unset($int1);

// Wiskundige formulles
$som        = 1 + 1; // 2
$verschil   = 2 - 1; // 1
$vermenigvuldiging    = 2 * 2; // 4
$deling   = 2 / 1; // 2

// Verkorte wiskundige formules
$nummer = 0;
$nummer += 1;      // $nummer verhogen met 1
echo $nummer++;    // Print 1 uit (Verhogen na afdrukken)
echo ++$nummer;    // Print 3 uit (Verhogen voor afdrukken)
$nummer /= $float; // Deel en steek het resultaat in $nummer

// Strings worden tussen enkele aanhalingstekens geplaatst
$enk_quotes = '$String'; // => '$String'

// Vermijd het gebruik van dubbele aanhalingstekens behalve om
// andere variabelen te gebruiken
$dub_quotes = "Dit is een $enk_quotes."; // => 'Dit is een $String.'

// Speciale karakters worden enkel voorgegaan door een backslash
// in dubbele quotes
$escaped   = "Dit bevat een \t tab karakter.";
$unescaped = 'Dit bevant enkel een slash en een t: \t';

// Plaats een variabele tussen accolades indien nodig
$geld = "Ik heb $${number} op mijn bankrekening.";

// Sinds PHP 5.3, kan nowdocs gebruikt worden voor strings
// met meerder lijnen.
$nowdoc = <<<'END'
Lijn1
Lijn 2
END;

// Strings worden geconcatineerd met een ‘.’ 
echo 'Deze string ' . 'is geconcatineerd';


/********************************
 * Constanten
 */

// Een constante wordt aangemaakt met de functie define()
// en kan nooit van waarde veranderen bij het runnen van de code

// Een geldige constante start met een letter of underscore gevolgd
// door een aantal letters, cijfers of underscores
define("FOO", "iets");

// De waarde van een constante kan opgeroepen worden via de naam
echo 'Dit print '.FOO;


/********************************
 * Arrays
 */

// Alle arrays in PHP zijn associative arrays.
// Associative arrays worden in andere programmeertalen ook
// hashmaps genoemd

// Dit werkt in all php versies
$associative = array('Een' => 1, 'Twee' => 2, ‘Drie' => 3);

// Deze syntax is beschikbaar vanaf PHP 5.4
$associative = [‘Een’ => 1, 'Twee’ => 2, 'Drie' => 3];

echo $associative['Een']; // print 1

// Lijsten krijgen automatisch integer sleutels
$array = [‘Een’, 'Twee', ‘Drie'];
echo $array[0]; // => "Een"

// Een nieuw element toevoegen aan het einde van een lijst
$array[] = 'vier';

// Een element wissen van de lijst
unset($array[3]);

/********************************
 * Output
 */

echo('Hallo Wereld!');
// Print “Hallo Wereld!” naar stdout.
// Stdout is de webpagina wanneer in een browser

print('Hallo Wereld!'); // Doet het zelfde als “echo”

// Echo en print werken ook zonder haakjes
echo 'Hallo Wereld!';
print 'Hallo Wereld!';

// Echo print ook variabelen
$paragraph = 'paragraph';
echo $paragraph; // or variables

$x = 1;
$y = 2;
$x = $y; // $x heeft nu de zelfde waarde als $y
$z = &$y;
// $z is nu een referentie naar $y. door de waarde van
// $z te veranderen veranderd $y ook, en vice-versa.
// $x zal altijd de originele waarde van $y behouden

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0

// Dumpt de waarde en het type van een variabele
var_dump($z); // print int(0)

// Print een array in een leesbaar formaat voor mensen
print_r($array); // print: Array ( [0] => One [1] => Two [2] => Three )

/********************************
 * Logica
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// Variables worden automatisch omgevormd naar een ander type,
// afhankelijk van het gebruik

$integer = 1;
echo $integer + $integer; // => 2

$string = '1';
echo $string + $string; // => 2 (strings worden omgevormd naar integers)

$string = 'een';
echo $string + $string; // => 0
// output is 0 want ‘een’ kan niet omgevormd worden naar een nummer

// Casting kan gebruikt worden om een variabele naar een ander type te forceren.

$boolean = (boolean) 1; // => true

$niets = 0;
$boolean = (boolean) $niets; // => false

// Voor de meeste types is ook een functie voorzien om te casten
$integer = 5;
$string = strval($integer);

$var = null; // Null waarde


/********************************
 * Controle Structuren
 */

if (true) {
    print 'Ik word geprint';
}

if (false) {
    print 'Ik niet';
} else {
    print 'Ik wordt geprint';
}

if (false) {
    print 'Wordt niet geprint';
} elseif(true) {
    print 'Deze wel';
}

// Verkorte versie
print (false ? 'Wordt niet geprint’' : 'Deze wel');

// Verkorte versie sinds PHP 5.3
// Dit is het zelfde als "$x ? $x : 'Deze wel'""
$x = false;
print($x ?: 'Deze wel');

// null coalesce operator since php 7
$a = null;
$b = 'Wordt geprint';
echo $a ?? 'a bestaat niet'; // prints 'a bestaat niet'
echo $b ?? 'b bestaat niet'; // prints 'Wordt geprint'


$x = 0;
if ($x === '0') {
    print 'Wordt niet geprint';
} elseif($x == '1') {
    print 'Wordt niet geprint';
} else {
    print ‘Wordt geprint';
}


// Gebruik een switch voor een kortere if structuur
switch ($x) {
    case '0':
        print 'Switch wisselt types';
        break; // Een break is verplicht of we gaan verder naar de volgende mogelijkheden

    case 'twe':
    case 'drie':
        // Doe iets als $x gelijk is aan ‘twee’ of ‘drie’
        break;
    default:
        // Doe iets als niets anders past
}

// While, do...while en for loops zijn net als in andere programmeertalen
$i = 0;
while ($i < 5) {
    echo $i++;
}; // Print "01234"

echo "\n";

$i = 0;
do {
    echo $i++;
} while ($i < 5); // Print "01234"

echo "\n";

for ($x = 0; $x < 10; $x++) {
    echo $x;
} // Print "0123456789"

echo "\n";

$wielen = [‘fiets’ => 2, ‘auto' => 4];

// Foreach lust kan over alle items in een array itereren
foreach ($wielen as $aantal_wielen) {
    echo $aantal_wielen;
} // Print "24"

echo "\n";

// Foreach kan itereren over sleutels en waardes van een array
foreach ($wheels as $voertuig => $aantal_wielen) {
    echo "Een $voertuig heeft $aantal_wielen wielen";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // Stop de lus hier
    }
    echo $i++;
} // Print "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // Sla deze stap in de lus over
    }
    echo $i;
} // Print "0124"
```

## More Information

Bezoek de [officiele PHP documentatie](http://www.php.net/manual/) voor meer informatie en community hulp

Als je geïntereseert bent in best practises, bezoek [PHP The Right Way](http://www.phptherightway.com/).

[Composer](http://getcomposer.org/) is de package manager voor alle php nodigheden.

Voor standaarden, bezoek de [PSR standards](https://github.com/php-fig/fig-standards) van dePHP Framework 
Interoperability groep

