---
language: php
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
    - ["Geoffrey Liu", "https://github.com/g-liu"]
filename: learnphp.php
---

Ce document decrit le PHP 5+.

```php
<?php // le code PHP doit être enfermé dans les balises «<?php»

// Si votre fichier php ne contient que du code PHP, il est practique
// d'omettrre la derniere balise «<?php»

// Deux barres obliques commen un commentaire d'une ligne

# Même avec un hashis, mais // est plus ordinaire

/*
    Mettre du texte dans un barre-oblique-etoile et etoile-barre-oblique
    le rend un commentaire de lignes multiples
*/

// Utilisez «echo» ou «print» pour rendre la sortie
print('Bonjour '); // Imprime «Bonjour » sans de saut de ligne

// Les parenthèses sont optionelles, en utilisant print et echo
echo "Tout le monde\n"; // Imprime «Tout le monde» avec un saut de ligne
// Tous les déclarations doit finir avec un point-virgule

// Anything outside <?php tags is echoed automatically
// N'importe quoi en dehors des balises «<?php» est «echo»é automatiquement
?>
Bonjour tout le monde à nouveau
<?php


/************************************
 * Types et variables
 */

// Variables commencent avec le symbole $
// Un nom de variable valide doit commencer par une lettre ou un trait de soulignement 
// suivi par un nombre quelconque de lettres, chiffres ou traits de soulignements.

// Les valeurs booléens sont insensibles à la casse
$boolean = true;  // ou TRUE ou True
$boolean = false; // ou FALSE ou False

// Les nombres entiers
$ent1 = 12;   // => 12
$ent2 = -12;  // => -12
$ent3 = 012;  // => 10 (un 0 au début indique un nombre octal)
$ent4 = 0x0F; // => 15 (un 0x au début indique un nombre hexadécimal littéral)

// Nombres à virgule flottante (alias les doubles)
$flot = 1.234;
$flot = 1.2e3;
$flot = 7E-10;

// Effacer un variable
unset($ent1)

// Arithmetique
$somme        = 1 + 1; // 2
$difference = 2 - 1; // 1
$produit    = 2 * 2; // 4
$quotient   = 2 / 1; // 2

// Arithmétique raccourci
$nombre = 0;
$nombre += 1;      // Incrémente $nombre by 1
echo $nombre++;    // Imprime 1 (incrémente apres l'evaluation)
echo ++$nombre;    // Prints 3 (incrément avant de l'evaluation)
$nombre /= $float; // Diviser et attribuer le quotient à $nombre

// Les strings doit être entre apostrophes
$apostrophes = '$String'; // => '$String'

// Evitez d'utiliser les guillemets excepté quand intégre d'autres variables
$guillemets = "C'est un $apostrphes."; // => 'C'est $String'.

// On n'échappe que les caractères spéciaux entre guillemets
$echappe   = "Ce contient un caractère \t de tabulation.";
$inechappe = 'Ce ne contient qu un barre oblique suivi par un t: \t';

// Mettre un variable entre accolades si nécessaire
$argent = "J'ai $${nombre} dans la banque.";

// Depuis PHP 5.3, on peut utiliser nowdocs pour les multi-lignes non interpolés
$nowdoc = <<<'END'
Multiples lignes
string
END;

// Les heredocs fera l'interpolation
$heredoc = <<<END
Multiples lignes
$apostrophes
END;

// String concatenation is done with .
// On utilise . (un point) pour enchaîner les strings
echo 'Ce string ' . 'est enchaîné';


/********************************
 * Les constantes
 */

// On utilise define() pour définer une constante
// La constante ne peut pas être changé pendant l'éxécution

// un nom de constante valide commence par une lettre ou un trait de soulignement, 
// suivi par un nombre quelconque de lettres, chiffres ou traits de soulignements.
define("FOO",     "quelque chose");

// accéder une constante avec le nom choisi
echo 'Ce sort '.FOO;


/********************************
 * Matrices
 */

// Tous les matrices en PHP sont des matrices associatifs (cartes hachage),
// Les matrices associatifs sont connue sous le nom «hash-maps»

// La suivante fonctionne avec tous les versions de PHP
$associatif = array('Un' => 1, 'Deux' => 2, 'Trois' => 3);

// La version 5.4 de PHP a introduit une nouvelle syntaxe
$associatif = ['Un' => 1, 'Deux' => 2, 'Trois' => 3];

echo $associatif['Un']; // imprime 1

// Les littéraux de liste attribuent implicitement les cléfs de nombres entiers
$matrice = ['Un', 'Deux', 'Trois'];
echo $matrice[0]; // => «Un»

// Ajouter un élément à la fin d'une matrice
$matrice[] = 'Four';

// Supprimer un élément de la matrice
unset($matrice[3]);

/********************************
 * La sortie
 */

echo('Bonjour tout le monde!');
// Imprime «Bonjour tout le monde!» à stdout.
// Si on utilise un navigateur Web, le stdout est le page Web.

print('Bonjour tout le monde!'); // Le même qu'echo

// echo est en fait une structure du langage, donc les parenthèses sont en option.
echo 'Bonjour tout le monde!';
print 'Bonjour tout le monde!'; // La même chose s'applique avec print

$paragraphe = 'paragraphe';

echo 100;        // Imprimer directement les variables scalaires
echo $paragraphe; // ...ou les variables

// Si les balises courtes ouvertes sont permis, ou votre version de PHP est
// 5.4.0 ou plus, vous pouvez utiliser la syntaxe suivante
?>
<p><?= $paragraphe ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // maintenant $x contient la même valeur qu'$y
$z = &$y;
// maintenant $z contient un référence à $y. Changer la valeur de
// $z va changer la valeur d'$y aussi, et vice-versa.
// $x reste inchangé comme la valeur originale d'$y.

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0

// Décharge le type et la valeur d'un variable à stdout
var_dump($z); // => "int(0)"

// Imprime la variable à stdout dans un format lisable
print_r($array); // imprime: Array ( [0] => Un [1] => Deux [2] => Trois )

/********************************
 * La logique
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// «assert» jete un avertissement si son argument n'est pas vrai (true)

// Ces comparaisons sera toujours vrai, même si les types ne seront pas pareilles.
assert($a == $b); // egalité
assert($c != $a); // inegalité
assert($c <> $a); // inegalité alternative
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// La suivante sera vrai si les valeurs correspondent et sont du même type.
assert($c === $d);
assert($a !== $d);
assert(1 === '1');
assert(1 !== '1');

// Variables peut être convertis entre types, selon leur usage.

$entier = 1;
echo $entier + $entier; // => 2

$string = '1';
echo $string + $string; // => 2 (les strings sont contraintés en nombres entiers)

$string = 'un';
echo $string + $string; // => 0
// Délivre en sortie 0 parce que l'opérateur + (signe plus) ne peut cataloguer le string 'un' dans un nombre


// La cataloguage (anglais: typecasting) peut être utilisé pour traiter une variable comme une autre type

$booleen = (boolean) 1; // => true

$zero = 0;
$booleen = (boolean) $zero; // => false

// Il existe aussi des fonctionnes dediées pour cataloguer la plupart de types
$entier = 5;
$string = strval($entier);

$var = null; // Valeur nul


/********************************
 * Les structures de contrôle
 */

if (true) {
    print 'Je suis imprimé';
}

if (false) {
    print 'Je ne suis pas imprimé';
} else {
    print 'mais je suis imprimé';
}

if (false) {
    print "N'est pas imprimé";
} elseif(true) {
    print 'est imprimé';
}

// l'operateur ternaire
print (false ? 'Nest pas imprimé' : 'est imprimé');

$x = 0;
if ($x === '0') {
    print "ne s'imprime pas";
} elseif($x == '1') {
    print "ne s'imprime pas";
} else {
    print "s'imprime";
}



// Cette syntaxe alternative est utile pour les modèles:
?>

<?php if ($x): ?>
Ce s'affiche si le test ci-dessus est vrai (anglais: «truthy»).
<?php else: ?>
Autrement, ce s'affiche.
<?php endif; ?>

<?php

// On utilise le mot-clé «switch» comme un alternatif aux «else if»s multiples
switch ($x) {
    case '0':
        print 'Switch fait la contrainte de type';
        break; // Vous devez incluer un «break», ou bien les cases
               // 'deux' et 'trois' va s'exécuter
    case 'two':
    case 'three':
        // Faire quelque chose si $variable est soit 'deux' oú 'trois'
        break;
    default:
        // Faire quelque chose par défaut
}

// Les loopings «while», «do...while» sont probablement familiers
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

$roues = ['velo' => 2, 'voiture' => 4];

// Les loopings peut itérer sur des matrices
foreach ($roues as $compte) {
    echo $compte;
} // Imprime "24"

echo "\n";

// Vous pouvez itérer sur des clés aussi bien que les valeurs
foreach ($roues as $vehicule => $compte) {
    echo "Un $vehicule possède $compte roues";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // Sortir du looping
    }
    echo $i++;
} // Imprime "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // Passer cette iteration du looping
    }
    echo $i;
} // Imprime "0124"


/********************************
 * Les fonctions
 */

// Définer une fonction avec «function»:
function ma_fonction () {
  return 'Allô';
}

echo ma_fonction(); // => "Allô"

// Un nom de fonction valide commence par une lettre ou un trait de soulignement, suivi d'un 
// nombre de lettres, chiffres ou traits de souligenment.

function ajouter ($x, $y = 1) { // $y est optionel et faire défaut à 1
  $resultat = $x + $y;
  return $resultat;
}

echo ajouter(4); // => 5
echo ajouter(4, 2); // => 6

// $resultat n'est pas accessible en dehors de la fonction
print $resultat; // Jete un avertissement

// Depuis PHP 5.3 vous pouvez déclarer les fonctions anonymes
$inc = function ($x) {
  return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
  echo "$x - $y - $z";
}

// Fonctions peut rendre des fonctions
function bar ($x, $y) {
  // Utiliser «use» pour apporter des variables du dehors
  return function ($z) use ($x, $y) {
    foo($x, $y, $z);
  };
}

$bar = bar('A', 'B');
$bar('C'); // Imprime "A - B - C"

// Vous pouvez appeler les fonctions nommées avec strings
$function_name = 'ajouter';
echo $function_name(1, 2); // => 3
// Utile pour déterminer par programme quelle fonctionne à exécuter.
// Ou, utiliser call_user_func(callable $callback [, $parameter [, ... ]]);

/********************************
 * Le mot-clé «includes»
 */

<?php
// Le code PHP dans les fichiers inclus doit commencer aussi avec un balise «<$php»

include 'mon-fichier.php';
// Maintenant, le code dans mon-fichier.php est disponible dans la portée actuelle
// Si le fichier ne peut pas être inclus (par example: fichier introuvable),
// un avertissement se jete.

include_once 'mon-fichier.php';
// Si le code dans mon-fichier.php a été inclus d'ailleurs, il ne sera pas
// inclus encore. Cela empêches les erreurs de declaration des multiples classes.

require 'mon-fichier.php';
require_once 'mon-fichier.php';
// La même chose qu'«include()», mais «require()» va causer une erreur fatale
// si le fichier ne peut pas être inclus.

// Contenu de mon-inclure.php:
<?php

return 'Anything you like.';
// La fin de fichier

// «include» et «require» peut rendre aussi une valeur.
$value = include 'my-inclure.php';
// Fichiers sont inclus selon du chemin du fichier donné ou, si aucun n'est donné, 
// la directive de configuration «include_path». Si le fichier n'est pas trouvé dans 
// l'«include_path», «include» va vérifier enfin dans le propre directoire du
// script d'appel et le directoire de travail courant avant d'échouer.

/********************************
 * Les classes
 */

// On les définit avec le mot-clé «class»

class MaClasse
{
    const MA_CONST      = 'valeur'; // A constant

    static $varStatique   = 'statique';

    // Les variables statiques et leur visibilité
    public static $varStatiquePublique = 'publique et statique';
    // Seulement accessible dans la classe
    private static $varStatiquePrive = 'privé et statique';
    // Accessible dans la classe et tous les sous-classes (anglais: subclasses)
    protected static $varStatiqueProtegee = 'protegé et statique';

    // Les propriétés doit déclarer leur visibilité
    public $propriete    = 'publique';
    public $propInstance;
    protected $prot = 'protégé'; // Accessible dans la classe et les sus-classes
    private $priv   = 'privé';   // Seulement accessible dans la classe

    // Créer un constructeur avec __construct
    public function __construct($propInstance) {
        // Accéder les variables instances avec $this
        $this->propInstance = $propInstance;
    }

    // On déclare des méthodes comme fonctions dans une classe
    public function maMethode()
    {
        print 'MaClasse';
    }

    // le mot-clé «final» rend une fonction insurchargeable
    final function vousNePouvezPasMEcraser()
    {
    }

/*
 * La déclaration des propriétés ou méthodes d'un classe comme statique les rend accessibles sans
 * le besoin pour un instanciation de la classe. Une propriété déclarée comme statique ne peut pas
 * être accedée avec un object de classe instancié (bien qu'une méthode statique le peut).
 */

    public static function maMethodeStatique()
    {
        print 'Je suis statique';
    }
}

echo MyClass::MA_CONST;    // Imprime 'valeur';
echo MyClass::$varStatique;  // Imprime 'statique';
MyClass::maMethodeStatique(); // Imprime 'Je suis statique';

// Instantiate classes using new
$my_class = new MyClass('Ma propriété instance');
// Les parenthèses sont optionelles si on ne le passe pas un argument.

// Accéder les membres de classe avec «->»
echo $my_class->propriete;     // => "publique"
echo $my_class->propInstance; // => "Ma propriété instance"
$my_class->maMethode();        // => "MaClasse"


// Étendre les classes avec «extends»
class MonAutreClasse extends MaClasse
{
    function rendreProprieteProtege()
    {
        echo $this->prot;
    }

    // Redéfinir une methode
    function maMethode()
    {
        parent::maMethode();
        print ' > MonAutreClasse';
    }
}

$my_other_class = new MyOtherClass("Prop d'instance");
$my_other_class->rendreProprieteProtege();  // => Imprime "protégé"
$my_other_class->maMethode();               // Imprime "MaClasse > MonAutreClasse"

final class VousNePouvezPasMEtendre
{
}

// Vous pouvez utiliser les méthodes magiques pour créer «getters» et «setters»
class MaClasseDeCarte
{
    private $propriete;

    public function __get($cle)
    {
        return $this->$cle;
    }

    public function __set($cle, $valeur)
    {
        $this->$cle = $valeur;
    }
}

$x = new MaClasseDeCarte();
echo $x->propriete; // Va utiliser la méthode «__get()»
$x->propriete = 'Quelque chose'; // Va utiliser la méthode «__set()»


// Les classes peut être abstraits (en utilisant le mot-clé «abstract») ou
// peut réaliser des interfaces (en utilisant le mot-clé «implements»).
// On déclare une interface avec le mot-clé «interface».

interface PremierInterface
{
    public function faireQuelqueChose();
}

interface DeuxiemeInterface
{
    public function faireAutreChose();
}

// interfaces can be extended
interface TroisiemeInterface extends DeuxiemeInterface
{
    public function faireUnAutreContrat();
}

abstract class MaClasseAbstraite implements PremierInterface
{
    public $x = 'faireQuelqueChose';
}

class MaClasseConcrete extends MaClasseAbstraite implements DeuxiemeInterface
{
    public function faireQuelqueChose()
    {
        echo $x;
    }

    public function faireAutreChose()
    {
        echo 'faireAutreChose';
    }
}


// Les classes peuvent réaliser plus d'une interface
class UneAutreClasses implements PremierInterface, DeuxiemeInterface
{
    public function faireQuelqueChose()
    {
        echo 'faireQuelqueChose';
    }

    public function faireAutreChose()
    {
        echo 'faireAutreChose';
    }
}


/********************************
 * Les traits
 */

// Les traits sont disponibles depuis PHP 5.4.0 et on les déclare avec «trait»
trait MonTrait
{
    public function maMethodeDeTrait()
    {
        print "J'ai mon trait";
    }
}

class MaClasseDeTrait
{
    use MonTrait;
}

$cls = new MaClasseDeTrait();
$cls->maMethodeDeTrait(); // Imprime "J'ai mon trait"


/********************************
 * Les espaces de noms
 */

// Cette section est séparée, car une déclaration d'espace de noms 
// doit être la première instruction dans un fichier. Imaginons que ce n'est pas le cas
<?php

// Par défaut, les classes existent dans l'espace de noms global, et peut 
// être appelé explicitement avec une barre oblique inverse, suivi par le nom de l'espace.

$cls = new \MyClass();



// Définir l'espace de nom pour un fichier
namespace Mon\Espace;

class maClasse
{
}

// (d'un autre fichier)
$cls = new Mon\Espace\MaClasse;

// Ou dans un autre espace de nom
namespace Mon\Autre\Espace;

use Mon\Espace\MaClasse;

$cls = new MaClasse();

// Ou vous pouvez créer un alias au espace de nom

namespace Mon\Autre\Espace;

use Mon\Espace as UnAutreEspace;

$cls = new UnAutreEspace\MaClasse();

*/

```

## Plus d'informations (pages suivantes en anglais)

Visitez la [documentation PHP officielle] (http://www.php.net/manual/) pour référence 
et commentaires de la communauté.

Si vous êtes intéressé par la mise à jour des meilleures pratiques, visite 
[PHP The Right Way] (http://www.phptherightway.com/).

Si vous venez d'une langue avec une bonne gestion de package, consultez
[Composer](http://getcomposer.org/).

Pour des normes communes, visitez le [PSR standards](https://github.com/php-fig/fig-standards) du PHP Framework Interoperability Group.
