---
language: php
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
    - ["Geoffrey Liu", "https://github.com/g-liu"]
filename: learnphp.php
---

Ce document décrit PHP 5+.

```php
<?php // Le code PHP doit être placés dans les balises <?php

// Si votre ficher PHP contient seulement le code PHP, il est mieux d'omettre la balise dernier.

// Deux slashes commencent un commentaire d'une ligne

# Un hashis fait la même, mais // est plus fréquente

/*
    Mettre du texte dans slash-étoile et étoile-slash
    rend un commentaire multi-ligne
*/

// Utilisez "echo" ou "print" pour rendre de la sortie
print('Bonjour '); // Rend "Bonjour " sans un saut de ligne

// () sont en option au lieu de print ou echo
echo "tout le monde\n"; // Rend "tout le monde" sans un saut de ligne
// (tous les déclarations doivent terminer avec un point-virgule)

// Rien de l'éxterieur des balises <?php est rendu automatiquement
?>
Bonjour tout le monde nouveau
<?php


/************************************
 * Types & Variables
 */

// Variables commencent avec le symbole $.
// Un nom de variable valide commence avec une lettre ou un souligné,
// suivie par un certain nombre de lettres, chiffres, ou soulignés

// Les booléens sont insensibles à la casse.
$boolean = true;  // ou TRUE ou True
$boolean = false; // ou FALSE ou False

// nombres entiers
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (un 0 au début signifie un nombre octal)
$int4 = 0x0F; // => 15 (un 0 au début signifie un nombre héxidecimal)

// Nombres à virgule flottante
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Effacer un variable
unset($int1)

// L'arithmétique
$somme        = 1 + 1; // 2
$différence = 2 - 1; // 1
$produit    = 2 * 2; // 4
$quotient   = 2 / 1; // 2

// L'arithmétique raccourci
$nombre = 0;
$nombre += 1;      // Incrémente $nombre par 1
echo $nombre++;    // Rend 1 (incrémente après l'évaluation)
echo ++$nombre;    // Rend 3 (incréments avant l'évaluation)
$nombre /= $float; // Divise et assigne le quotient à $nombre

// Strings doit être entre apostrophes
$apostrophes = '$String'; // => '$String'

// Évitez d'utiliser des guillemets à l'éxception d'incorporer d'autres variables
$guillemets = "This is a $apostrophes."; // => 'C'est un $String.'

// Les caractères spéciaux sont échappes seulement entre guillemets
$escaped   = "Ce contient un caractère \t -- un tabulation";
$unescaped = 'Ce ne contient qu un slash et un t: \t';

// Mettez un variable entre accolades, si nécessaire
$argent = "J'ai $${number} dans la banque.";

// Depuis PHP 5.3, on peut utiliser nowdocs pour les multi-ligneurs non interpolés
$nowdoc = <<<'END'
Multi ligne
string
END;

// Heredocs feront l'interpolation
$heredoc = <<<END
Multi line
$sgl_quotes
END;

// On fait l'enchaînement des strings avec . (période)
echo 'Ce string ' . 'est enchaîné';


/********************************
 * Constantes
 */

// On definit une constante avec define()
// Le constant ne peut pas être changé lors de l'éxecution!

// Un nom de constante valide commence avec un lettre ou un souligne,
// suivie par un certain nombre de lettres, chiffres ou soulignés.
define("FOO",     "quelquechose");

// Pour accéder une constante, utilisez directement le nom de la constante
echo 'Ce rend ' . FOO;


/********************************
 * Tableaux
 */

// Tous les tableaux en PHP sont associatifs (cartes hashages)

// Fonctionne avec tous les versions PHP
$associatif = array('Un' => 1, 'Deux' => 2, 'Trois' => 3);

// PHP 5.4 a introduit une nouvelle syntaxe
$associatif = ['Un' => 1, 'Deux' => 2, 'Trois' => 3];

echo $associatif['Un']; // rend 1

// List literals implicitly assign integer keys 
// Les littéraux assignent implicitement les clés entières
$tableau = ['Un', 'Deux', 'Trois'];
echo $tableau[0]; // => "Un"

// Add an element to the end of an array
// Ajouter un élément à la fin d'un tableau
$tableau[] = 'Four';

// Supprimer un élément d'un tableau
unset($tableau[3]);

/********************************
 * La sortie
 */

echo('Bonjour tout le monde!');
// Rendre "Bonjour tout le monde!" à stdout.
// Stdout est la page Web, si vous utilisez un navigateur Web.

print('Hello World!'); // Le même qu'echo

// echo est en fait un structure du langage, donc les parenthèses sont en option.
echo 'Bonjour tout le monde!';
print 'Bonjour tout le monde!'; // Alors est print

$paragraphe = 'paragraphe';

echo 100;        // Echo les variables scalaires directement
echo $paragraphe; // ou des variables

// Si les balises courts sont permis, ou votre version PHP est
// 5.4.0 ou plus, vous pouvez utiliser la syntaxe suivante, pour echo:
?>
<p><?= $paragraphe ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // maintenant $x contient la même valeur qu'$y
$z = &$y;
// maintenant $z contient une référence à $y. Si on change la valeur de
// $z, la valeur d'$y va changer aussi, et vice-versa.
// $x reste inchangé comme la valeur originale d'$y

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0

// Deverser à stdout la type et valeur de la variable
var_dump($z); // rend int(0)

// Imprimer à stdout la variable dans un format lisable
print_r($tableau); // rend: Array ( [0] => Un [1] => Deux [2] => Trois )

/********************************
 * La logique
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// assert jete un avertissement si son argument n'est pas "true"

// Ces comparaisons sera toujours "true", même les types ne sont pas pareilles.
assert($a == $b); // égal
assert($c != $a); // pas êgal
assert($c <> $a); // alternatif à pas êgal
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// La suivante sera "true" si les valeurs et les types sont pareilles.
assert($c === $d);
assert($a !== $d);
assert(1 === '1');
assert(1 !== '1');

// On peut convertir entre types les variables, selon leur usage.
$entier = 1;
echo $entier + $entier; // => 2

$string = '1';
echo $string + $string; // => 2 (strings sont contraints à des nombres entiers)

$string = 'un';
echo $string + $string; // => 0 (parce-que l'opérateur + ne peut pas contraindre le string 'un' à un certain nombre)

// Le cataloguage (type casting) peut être utiliser pour traiter une variable comme s'il était une autre type.

$boolean = (boolean) 1; // => true

$zero = 0;
$boolean = (boolean) $zero; // => false

// Il y a aussi les fonctionnes dédiées pour cataloguer la plupart de types
$integer = 5;
$string = strval($integer);

$var = null; // Valeur nul


/********************************
 * Les structures de contrôle
 */

if (true) {
    print 'je reçois imprimé';
}

if (false) {
    print 'je ne reçois pas imprimé';
} else {
    print 'Mais je le recois';
}

if (false) {
    print 'Nest pas imprimé';
} elseif(true) {
    print 'Est imprimé';
}

// ternary operator
print (false ? 'Nest pas imprimé' : 'Est imprimé');

$x = 0;
if ($x === '0') {
    print 'Ne pas imprimer';
} elseif($x == '1') {
    print 'Ne pas imprimer';
} else {
    print 'Est imprimé';
}


// Cette syntaxe alternative est utile pour les modèles:
?>

<?php if ($x): ?>
    Ce message est affiché si le test ci-dessus est vrai
<?php else: ?>
    Autrement, ce message est affiché
<?php endif; ?>

<?php

// L'usage de la déclaration switch au lieu d'if...else:
switch ($x) {
    case '0':
        print 'Switch fait la contrainte de type';
        break; // Le "break" est nécessaire, ou bien les cases
               // 'deux' et 'trois' s'exécuteront
    case 'deux':
    case 'trois':
        // Faire quelque chose si $variable est 'deux' ou 'trois'
        break;
    default:
        // Faire quelque chose par défaut
}

// les loopings while, do...while et for sont familiers
$i = 0;
while ($i < 5) {
    echo $i++;
}; // Rend "01234"

echo "\n";

$i = 0;
do {
    echo $i++;
} while ($i < 5); // Rend "01234"

echo "\n";

for ($x = 0; $x < 10; $x++) {
    echo $x;
} // Rend "0123456789"

echo "\n";

$roues = ['velo' => 2, 'voiture' => 4];

// Les loopings foreach peuvent itérer sur les tableaux
foreach ($roues as $compte) {
    echo $compte;
} // Rend "24"

echo "\n";

// Vous pouvez itérer sur les clés aussi bien que les valeurs
foreach ($roues as $vehicule => $compte) {
    echo "Un $vehicule possède $compte wheels";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // Sortir du looping while
    }
    echo $i++;
} // Rend "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // Passer cette itération du looping
    }
    echo $i;
} // Rend "0124"


/********************************
 * Les fonctionnes
 */

// Définissez une fonctionne avec "function":
function ma_fonctionne () {
  return 'Allô';
}

echo my_function(); // => "Allô"

// Un nom de fonctionne valide commence avec une lettre ou une souligné, suivit par n'importe quel
// lettres, chiffres ou soulignés.

function ajouter ($x, $y = 1) { // $y est en option et fait défaut à 1
  $resultat = $x + $y;
  return $resultat;
}

echo ajouter(4); // => 5
echo ajouter(4, 2); // => 6

// On ne peut pas accéder $resultat à l'éxterieur de la fonctionne
print $resultat; // Rend un avertissement

// Depuis PHP 5.3, vous pouvez déclarer les fonctionnes anonymes
$inc = function ($x) {
  return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
  echo "$x - $y - $z";
}

// Les fonctionnes peuvent remettre des fonctionnes
function bar ($x, $y) {
  // Utilisez 'use' d'apporter les variables du dehors
  return function ($z) use ($x, $y) {
    foo($x, $y, $z);
  };
}

$bar = bar('A', 'B');
$bar('C'); // Rends "A - B - C"

// Vous pouvez appeler les fonctionnes nommées avec strings
$function_name = 'ajouter';
echo $function_name(1, 2); // => 3

// Utile pour déterminer par programmation quelle fonctionne à éxécuter
// Ou, vous pouvez utiliser call_user_func(callable $callback [, $parameter [, ... ]]);

/********************************
 * Le mot-clé includes
 */

<?php
// Le PHP dans les fichers inclus doit commencer avec un balise d'ouverture PHP.

include 'mon-ficher.php';
// Le code dans mon-fichier.php est maintenant disponible dans la portée actuel.
// Si le fichier ne peut pas être inclus (e.g. fichier introuvable), un avertissement est jeté.

include_once 'mon-fichier.php';
// Si le code dans mon-fichier.php a été inclus ailleurs, il ne sera pas
// être inclus encore. Cela évite les erreurs de déclaration des classes multiples

require 'mon-fichier.php';
require_once 'mon-fichier.php';
// Même qu'include(), sauf que require() va causer une erreur fatale si le
// fichier ne peut pas être inclus.

// Les contients de mon-fichier.php:
<?php

return 'Tout ce que vous voulez.';
// Fin de fichier

// Includes et requires peut retourner aussi une valeur.
$value = include 'my-include.php';

// Les fichiers sont inclus selon le chemin du fichier ou, si aucun n'est donné,
// la directive de configuration include_path. Si le fichier n'est pas trouvé dans
// l'include_path, include va examiner enfin dans la propre dossier du script d'appel
// et le dossier de travail courant avant d'échouer

/********************************
 * Les classes
 */

// Les classes sont définis avec le mot-clé "class"

class MaClasse
{
    const MA_CONST      = 'valeur'; // Une constante

    static $varStatique   = 'statique'; // Une variable statique

    // La visibilité des variables statiques
    public static $varStatiquePublique = 'publiqueStatique';
    // Accessible seulement dans la classe
    private static $varStatiquePrive = 'privéStatique';
    // Accessible from the class and subclasses
    protected static $varStatiqueProtege = 'protégéStatique';

    // Les propriétés doivent déclarer leur visibilité
    public $propriete    = 'publique';
    public $propInstance;
    protected $prot  = 'protégé'; // Accessible dans la classe et les sous-classes
    private $prive   = 'privé';   // Accessible seulement dans la classe

    // Créez un constructeur avec __construct
    public function __construct($propInstance) {
        // Accédez les variables instances avec $this
        $this->propInstance = $propInstance;
    }

    // On déclare les méthodes comme les fonctionnes dans une classe
    public function maMethode()
    {
        print 'MaClasse';
    }

    // le mot-clé "final" rend une fonctionne inoutrepassable
    final function vousNePouvezPasMOutrepasser()
    {
    }

/*
 * La déclaration de "static" aux propriétés ou méthodes des classes leur rend accessible sans
 * le besoin d'instancier la classe. Une propriété déclaré comme "static" ne peut pas
 * être accédé avec un objet de classe instancié (bien qu'une méthode statique le peut).
 */

    public static function maMethodeStatique()
    {
        print 'Je suis statique';
    }
}

echo MaClasse::MA_CONST;    // Rend 'valeur';
echo MaClasse::$varStatique;  // Rend 'statique';
MaClasse::maMethodeStatique(); // Rend 'Je suis statique';

// Instancier les nouveaux classes avec le mot-clé "new"
$ma_classe = new MaClasse('Une propriété instance');
// Les parenthèses sont en option si il n'y a un argument

// Accédez les membres de classe avec "->"
echo $ma_classe->propriete;     // => "publique"
echo $ma_classe->propInstance; // => "Une propriété instance"
$ma_classe->maMethode();        // => "MyClasse"

// Étendez les classes avec "extends"
class MonAutreClasse extends MaClasse
{
    function rendreProprieteProtege()
    {
        echo $this->prot;
    }

    // Override a method
    function maMethode()
    {
        parent::maMethode();
        print ' > MonAutreClasse';
    }
}

$mon_autre_classe = new MonAutreClasse('Instance prop');
$mon_autre_classe->rendreProprieteProtege(); // => Prints "protégé"
$mon_autre_classe->maMethode();               // Prints "MaClasse > MonAutreClasse"

final class VouzNePouvezPasMEtendre
{
}

// Vous pouvez utiliser les "methodes magiques" pour créer les getters et les setters
class MaClasseCarte
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

$x = new MaClasseCarte();
echo $x->propriete; // Utilisera la methode __get()
$x->propriete = 'quelque chose'; // Utilisera la methode __set()

// Les classes peut être abstraits (avec le mot-clé "abstract") ou
// réaliser les interfaces (avec le mot-clé "implements").
// On déclare un interface avec le mot-clé "interface".

interface PremierInterface
{
    public function faireQuelqueChose();
}

interface DeuxiemeInterface
{
    public function faireAutreChose();
}

// les interfaces peut être étendu
interface TrosiemeInterface extends DeuxiemeInterface
{
    public function faireAutreContrat();
}

abstract class MaClasseAbstrait implements PremierInterface
{
    public $x = 'faireQuelqueChose';
}

class MaClasseConcret extends MaClasseAbstrait implements DeuxiemeInterface
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


// Les classes peut réaliser plus d'une interface
class AutreClasse implements PremierInterface, DeuxiemeInterface
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

// Les traits sont disponibles depuis PHP 5.4.0 est sont déclarées avec "trait"

trait MonTrait
{
    public function monMethodeTrait()
    {
        print 'Jai mon trait';
    }
}

class MaClasseTrait
{
    use MyTrait;
}

$cls = new MyTraitfulClass();
$cls->myTraitMethod(); // Rend "I have MyTrait"


/********************************
 * Les espaces de noms
 */

// Cette section est séparée, parce qu'un déclaration d'espace de nom
// doit être la première déclaration dans un fichier.
// Imaginons que ce n'est pas le cas

<?php

// Par défault, les classes existent dans l'espace de nom globale, et peut
// être appellé explicitement avec une barre oblique inverse.

$cls = new \MaClasse();



// Fixer l'espace de nom pour un fichier
namespace Mon\EspaceDeNom;

class MaClasse
{
}

// (d'un autre fichier)
$cls = new Mon\EspaceDeNom\MaClasse;

// Or from within another namespace.
// Ou à partir d'un autre espace de nom.
namespace Mon\Autre\EspaceDeNom;

use Mon\EspaceDeNom\MaClasse;

$cls = new MaClass();

// Ou vous pouvez faire un pseudonyme a l'espace de nom
namespace Mon\Autre\EspaceDeNom;

use Mon\EspaceDeNom as UneAutreEspaceDeNom;

$cls = new UneAutreEspaceDeNom\MaClasse();

*/

```

## Plus d'informations

Visitez la [documentation PHP officielle](http://www.php.net/manual/) pour référence 
et participation de la communauté.

Si vous êtes intéressé par la mise à jour des meilleures pratiques, visitez
[PHP The Right Way](http://www.phptherightway.com/).

Si vous venez d'une langue avec une bonne gestion de package, consultez
[Composer](http://getcomposer.org/).

Pour des normes communes, vizitez le PHP Framework Interoperability Group's
[PSR standards](https://github.com/php-fig/fig-standards).
