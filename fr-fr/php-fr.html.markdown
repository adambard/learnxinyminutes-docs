---
language: PHP
filename: php-fr.php
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
    - ["Pascal Boutin", "http://pboutin.net/"]
    - ["Julien M'Poy", "https://github.com/groovytron"]
lang: fr-fr
---

Ce document décrit PHP 5+.

```php
 // Le code PHP doit être placé à l'intérieur de balises '<?php'

// Si votre fichier php ne contient que du code PHP, il est
// généralement recommandé de ne pas fermer la balise '?>'

// Deux barres obliques amorcent un commentaire simple.

# Le dièse aussi, bien que les barres obliques soient plus courantes

/*
    Les barres obliques et les astérisques peuvent être utilisés
    pour faire un commentaire multi-lignes.
*/

// Utilisez "echo" ou "print" afficher une sortie
print('Hello '); // Affiche "Hello " sans retour à la ligne

// Les parenthèses sont facultatives pour print et echo
echo "World\n"; // Affiche "World" avec un retour à la ligne

// toutes les instructions doivent se terminer par un point-virgule

// Tout ce qui se trouve en dehors des <?php ?> est automatiquement
// affiché en sortie
Hello World Again!
<?php


/************************************
 * Types & Variables
 */

// Les noms de variables débutent par le symbole $
// Un nom de variable valide commence par une lettre ou un souligné,
// suivi de n'importe quelle lettre, nombre ou de soulignés.

// Les valeurs booléennes ne sont pas sensibles à la casse
$boolean = true;  // ou TRUE ou True
$boolean = false; // ou FALSE ou False

// Entiers (integers)
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (un 0 devant la valeur désigne une valeur octale)
$int4 = 0x0F; // => 15 (un 0x devant la valeur désigne une valeur hexadécimale)

// Réels (floats, doubles)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Suppression d'une variable
unset($int1);

// Arithmétique
$sum        = 1 + 1; // 2 (addition)
$difference = 2 - 1; // 1 (soustraction)
$product    = 2 * 2; // 4 (produit)
$quotient   = 2 / 1; // 2 (division)

// Arithmétique (raccourcis)
$number = 0;
$number += 2;      // Incrémente $number de 2
echo $number++;    // Affiche 2 (incrémente après l'évaluation)
echo ++$number;    // Affiche 4 (incrémente avant l'évaluation)
$number /= $float; // Divise et assigne le quotient à $number

// Les chaînes de caractères (strings) doivent être à
// l'intérieur d'une paire d'apostrophes
$sgl_quotes = '$String'; // => '$String'

// Évitez les guillemets sauf pour inclure le contenu d'une autre variable
$dbl_quotes = "Ceci est une $sgl_quotes."; // => 'Ceci est une $String.'

// Les caractères spéciaux sont seulement échappés avec des guillemets
$escaped   = "Ceci contient \t une tabulation.";
$unescaped = 'Ceci contient juste un slash et un t: \t';

// En cas de besoin, placez la variable dans des accolades
$money = "J'ai $${number} sur mon compte en banque.";

// Depuis PHP 5.3, Nowdoc peut être utilisé pour faire des chaînes
// multi-lignes non-interprétées
$nowdoc = <<<'END'
String
mutli-lignes
END;

// Heredoc peut être utilisé pour faire des chaînes multi-lignes interprétées
$heredoc = <<<END
$sgl_quotes
multi-lignes
END;

// La concaténation de chaînes se fait avec un .
echo 'Cette string ' . 'est concatenée'; // => 'Cette string est concaténée'


/********************************
 * Constantes
 */

// Une constante est déclarée avec define()
// et ne peut jamais être changée durant l'exécution

// un nom valide de constante commence par une lettre ou un souligné,
// suivi de n'importe quelle lettre, nombre ou soulignés.
define("FOO",     "something");

// on peut accéder à une constante en utilisant directement son nom
echo 'Ceci affiche ' . FOO;


/********************************
 * Tableaux (array)
 */

// Tous les tableaux en PHP sont associatifs (hashmaps),

// Fonctionne dans toutes les versions de PHP
$associative = array('One' => 1, 'Two' => 2, 'Three' => 3);

// PHP 5.4 a introduit une nouvelle syntaxe
$associative = ['One' => 1, 'Two' => 2, 'Three' => 3];

echo $associative['One']; // affiche 1

// Dans une liste simple, l'index est automatiquement attribué en tant que clé
$array = ['One', 'Two', 'Three'];
echo $array[0]; // => "One"

// Ajoute un élément à la fin du tableau
$array[] = 'Four';

// Retrait d'un élément du tableau
unset($array[3]);

// Depuis PHP 7, il est possible de déclarer des tableaux constants en
// utilisant 'define'.
define('ANIMAUX', [
    'chien',
    'chat',
    'oiseau',
]);

/********************************
 * Affichage
 */

echo('Hello World!');
// Affiche Hello World! dans stdout.
// Stdout est la page web si on exécute depuis un navigateur.

print('Hello World!'); // Pareil à "écho"

// 'echo' et 'print' sont des language constructs.
// Il n'ont pas besoin de parenthèses car ils sont traités comme
// des opérateurs unaires.
echo 'Hello World!';
print 'Hello World!'; 

$paragraph = 'paragraphe';

echo 100;        // Affichez un scalaire directement
echo $paragraph; // ou des variables

// Si le raccourci de sortie est configuré, ou si votre version de PHP est
// 5.4.0+, vous pouvez utiliser ceci:
?>
<p><?= $paragraph ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // $x contient maintenant la même valeur que $y
$z = &$y;
// $z contient une référence vers $y. Changer la valeur de
// $z changerait également la valeur de $y, et vice-versa.
// $x resterait inchangé comme la valeur initiale de $y

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0

// Affiche le type et la valeur de la variable dans stdout
var_dump($z); // prints int(0)

// Affiche la variable dans stdout dans un format plus convivial
print_r($array); // prints: Array ( [0] => One [1] => Two [2] => Three )

/********************************
 * Logique
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// assert affiche un avertissement quand l'expression booléenne passée
// en argument n'est pas vraie.

// Ces comparaisons vont toujours être vraies, même si leurs
// types ne sont pas les mêmes.
assert($a == $b); // égalité
assert($c != $a); // inégalité
assert($c <> $a); // inégalité (moins courant)
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// Ces comparaisons vont seulement être vraies si les types concordent.
assert($c === $d);
assert($a !== $d);
assert(1 === '1');
assert(1 !== '1');

// Opérateur 'spaceship' depuis PHP 7
$a = 100;
$b = 1000;

echo $a <=> $a; // 0 car ils sont égaux
echo $a <=> $b; // -1 car $a < $b
echo $b <=> $a; // 1 car $b > $a

// Les variables peuvent être transtypées dépendamment de leur usage.

$integer = 1;
echo $integer + $integer; // => 2

$string = '1';
echo $string + $string; // => 2

$string = 'one';
echo $string + $string; // => 0
// Donne 0 car l'opérateur + ne peut pas transtyper la chaîne 'one' en un nombre

// On peut également transtyper manuellement pour utiliser
// une variable dans un autre type

$boolean = (boolean) 1; // => true

$zero = 0;
$boolean = (boolean) $zero; // => false

// Il y a également des fonctions dédiées pour transtyper
$integer = 5;
$string = strval($integer);

$var = null; // Valeur nulle


/********************************
 * Structures de contrôle
 */

if (true) {
    print 'Je suis affiché';
}

if (false) {
    print 'Je ne le suis pas';
} else {
    print 'Je suis affiché';
}

if (false) {
    print 'Je ne suis pas affiché';
} elseif (true) {
    print 'Je le suis';
}

// Opérateur ternaire
print (false ? 'N\'est pas affiché' : 'L\'est');

// Opérateur ternaire depuis PHP 5.3
// équivalent de $x ? $x : 'Does'
$x = false;
print($x ?: 'Does');

// depuis PHP 7, on peut facilement vérifier si une valeur est nulle
$a = null;
$b = 'Hello World';
echo $a ?? 'a is not set'; // Affiche 'a is not set'
echo $b ?? 'b is not set'; // Affiche 'Hello World'


$x = 0;
if ($x === '0') {
    print 'Pas affiché';
} elseif($x == '1') {
    print 'Pas affiché';
} else {
    print 'Affiché';
}


// Cette syntaxe alternative est particulièrement utile avec du HTML:
?>

<?php if ($x): ?>
<p>Ceci est affiché si $x est vrai</p>
<?php else: ?>
<p>Ceci est affiché si $x est faux</p>
<?php endif; ?>

<?php

// On peut également utiliser une condition multiple (switch case)
switch ($x) {
    case '0':
        print 'Les switch font du transtypage implicite';
        break; // Il est important de déclarer un 'break', sinon les cas
               // 'two' et 'three' seront évalués
    case 'two':
    case 'three':
        // Si $x == 'two' || $x == 'three'
        break;
    default:
        // Si aucun cas n'a été vrai
}

// Structures itératives (for, while, do while)
$i = 0;
while ($i < 5) {
    echo $i++;
}; // Affiche "01234"

echo "\n";

$i = 0;
do {
    echo $i++;
} while ($i < 5); // Affiche "01234"

echo "\n";

for ($x = 0; $x < 10; $x++) {
    echo $x;
} // Affiche "0123456789"

echo "\n";

$wheels = ['bicycle' => 2, 'car' => 4];

// Les boucles 'foreach' sont utiles pour parcourir les tableaux
foreach ($wheels as $wheel_count) {
    echo $wheel_count;
} // Affiche "24"

echo "\n";

// Il est également possible d'accéder aux clés du tableau
foreach ($wheels as $vehicle => $wheel_count) {
    echo "The $vehicle have $wheel_count wheels";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // Permet d'arrêter la boucle
    }
    echo $i++;
} // Affiche "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // Permet de passer immédiatement à l'itération suivante
    }
    echo $i;
} // Affiche "0124"


/********************************
 * Fonctions
 */

// On peut déclarer une fonction avec le mot clé 'function'
function my_function () {
  return 'Hello';
}

echo my_function(); // => "Hello"


// Un nom de fonction valide commence par une lettre ou un souligné,
// suivi de n'importe quelle lettre, nombre ou de soulignés.
// Les noms des arguments d'une fonction doivent respecter le même format que
// celui des variables.

function add ($x, $y = 1) { // $y est facultatif et sa valeur par défaut est 1
  $result = $x + $y;
  return $result;
}

echo add(4); // => 5
echo add(4, 2); // => 6

// $result n'est pas accessible en dehors de la fonction
// print $result; // Retourne un avertissement

// Depuis PHP 5.3 on peut déclarer des fonctions anonymes
$inc = function ($x) {
  return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
  echo "$x - $y - $z";
}

// Une fonction peut retourner une fonction
function bar ($x, $y) {
  // On peut utiliser 'use' pour passer des variables externes
  return function ($z) use ($x, $y) {
    foo($x, $y, $z);
  };
}

$bar = bar('A', 'B');
$bar('C'); // Affiche "A - B - C"

// On peut exécuter une fonction par son nom en chaîne de caractères
$function_name = 'add';
echo $function_name(1, 2); // => 3
// Utile pour déterminer par programmation quelle fonction exécuter.

// On peut également utiliser
call_user_func(callable $callback [, $parameter [, ... ]]);

/********************************
 * Insertions
 */

<?php
// Le PHP se trouvant dans un fichier inclus doit
// également commencer par une balise PHP.

include 'my-file.php';
// Le code se trouvant dans my-file.php est maintenant disponible dans
// le contexte courant. Si le fichier ne peut pas être inclus
// (ex. non trouvé), un avertissement sera émit.

include_once 'my-file.php';
// Si le code dans my-file.php a déjà été inclus ailleur, il ne va pas
// être inclus de nouveau.

require 'my-file.php';
require_once 'my-file.php';
// Même comportement que include() mais déclenche une érreur fatale si le fichier ne peux pas être inclus.

// Contenu de my-include.php:
<?php

return 'Anything you like.';
// Fin de my-include.php

// include() et require() peuvent également retourner une valeur
$value = include('my-include.php');

// Les fichiers sont inclus depuis le chemin donné ou, si aucun chemin n'est donné,
// la configuration 'include_path'. Si le fichier n'est pas trouvé dans le 'include_path',
// include va finalement vérifier dans le répertoire courant avant d'échouer.

/********************************
 * Classes
 */

// Les classes sont définies avec le mot clé 'class'

class MyClass
{
    const MY_CONST      = 'value'; // Une constante

    static $staticVar   = 'static';

    // Variables statiques et leur visibilité
    public static $publicStaticVar = 'publicStatic';
    // Accessible à l'intérieur de la classe seulement
    private static $privateStaticVar = 'privateStatic';
    // Accessible à l'intérieur de la classe et des classes enfants
    protected static $protectedStaticVar = 'protectedStatic';

    // Les attributs doivent définir leur visibilité
    public $property = 'public';
    public $instanceProp;
    protected $prot = 'protected';
    private $priv   = 'private';

    // Déclaration d'un constructeur avec __construct
    public function __construct($instanceProp) {
        // Access instance variables with $this
        $this->instanceProp = $instanceProp;
    }

    // Les méthodes sont déclarés par des fonctions au sein de la classe
    public function myMethod()
    {
        print 'MyClass';
    }

    // le mot clé 'final' rend la function impossible à surcharger
    final function youCannotOverrideMe()
    {
    }

/*
 * Les attributs et méthodes statiques peuvent être accédés sans devoir
 * instancier la classe. Les attributs statiques ne sont pas accessibles depuis
 * une instance, même si les méthodes statiques le sont.
 */

    public static function myStaticMethod()
    {
        print 'Je suis static';
    }
}

// Les constantes d'une classe peuvent toujours être utilisé de façon statique
echo MyClass::MY_CONST;    // Outputs 'value';

echo MyClass::$staticVar;  // Retourne 'static';
MyClass::myStaticMethod(); // Retourne 'Je suis static';

// On peut instancier une classe en utilisant le mot clé 'new'
$my_class = new MyClass('An instance property');

// On peut accéder aux attributs/méthodes d'une instance avec ->
echo $my_class->property;     // => "public"
echo $my_class->instanceProp; // => "An instance property"
$my_class->myMethod();        // => "MyClass"


// On peut hériter d'une classe en utilisant 'extends'
class MyOtherClass extends MyClass
{
    function printProtectedProperty()
    {
        echo $this->prot;
    }

    // Surcharge d'une méthode
    function myMethod()
    {
        parent::myMethod();
        print ' > MyOtherClass';
    }
}

$my_other_class = new MyOtherClass('Instance prop');
$my_other_class->printProtectedProperty(); // => Retourne "protected"
$my_other_class->myMethod();               // Retourne "MyClass > MyOtherClass"

// On peut empêcher qu'une classe soit héritée
final class YouCannotExtendMe
{
}

// On peut utiliser des "méthodes magiques" pour se faire des accesseurs
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
echo $x->property; // Va utiliser la méthode __get()
$x->property = 'Something'; // Va utiliser la méthode __set()

// Les classes peuvent être abstraites (en utilisant le mot clé 'abstract'), ou
// elle peuvent implémenter une interface (en utilisant le mot clé 'implements').

// Une interface peut être déclarée avec le mot clé 'interface'

interface InterfaceOne
{
    public function doSomething();
}

interface InterfaceTwo
{
    public function doSomethingElse();
}

// Les interfaces peuvent hériter d'autres interfaces
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


// Les classes peuvent implémenter plusieurs interfaces à la fois
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

// Il est possible de déclarer des classes internes anonymes depuis PHP 7

interface Logger {
    public function log(string $msg);
}

class Application {
    private $logger;

    public function getLogger(): Logger {
         return $this->logger;
    }

    public function setLogger(Logger $logger) {
         $this->logger = $logger;
    }
}

$app = new Application;

$app->setLogger(new class implements Logger {
    public function log(string $msg) {
        echo $msg;
    }
});

var_dump($app->getLogger()); // => 'object(class@anonymous)#2 (0) {}'


/********************************
 * Espaces de noms (namespaces)
 */

// Cette section est séparée, car une déclaration d'espace de nom doit être
// la première chose que l'on retrouve dans un fichier PHP,
// imaginons que c'est le cas

<?php

// Par défaut, les classes existent dans l'espace de nom global et peuvent
// être appelé explicitement avec un antislash.

$cls = new \MyClass();



// On peut spécifier l'espace de nom d'un fichier comme cela
namespace My\Namespace;

class MyClass
{
}

// (depuis un autre fichier...)
$cls = new My\Namespace\MyClass;

// Ou depuis un autre espace de nom
namespace My\Other\Namespace;

use My\Namespace\MyClass;

$cls = new MyClass();

// On peut également utiliser un alias sur un espace de nom

namespace My\Other\Namespace;

use My\Namespace as SomeOtherNamespace;

$cls = new SomeOtherNamespace\MyClass();

*/

```

## Pour plus d'informations

Visitez la [documentation officielle](http://www.php.net/manual/fr).

Si vous êtes intéressé par les bonnes pratiques, visitez
[PHP The Right Way](http://www.phptherightway.com/) (anglais seulement).

Si vous êtes habitué à utiliser de bons gestionaires de dépendances, regardez
[Composer](http://getcomposer.org/).

Pour consulter les standards, visitez "the PHP Framework Interoperability Groups"
[PSR standards](https://github.com/php-fig/fig-standards).
