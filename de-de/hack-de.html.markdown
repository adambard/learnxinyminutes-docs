---
language: Hack
lang: de-de
contributors:
    - ["Stephen Holdaway", "https://github.com/stecman"]
    - ["David Lima", "https://github.com/davelima"]
translators:
    - ["Jerome Meinke", "https://github.com/jmeinke"]
filename: learnhack-de.hh
---

Hack ist eine von Facebook neu entwickelte Programmiersprache auf Basis von PHP.
Sie wird von der HipHop Virtual Machine (HHVM) ausgeführt. Die HHVM kann
aufgrund der Ähnlichkeit der Programmiersprachen nicht nur Hack, sondern auch
PHP-Code ausführen. Der wesentliche Unterschied zu PHP besteht in der statischen
Typisierung der Sprache, die eine wesentlich höhere Performance erlaubt.


Hier werden nur Hack-spezifische Eigenschaften beschrieben. Details über PHP's
Syntax findet man im [PHP Artikel](http://learnxinyminutes.com/docs/php/) dieser
Seite.

```php
<?hh

// Hack-Syntax ist nur für Dateien aktiv, die mit dem <?hh Prefix starten.
// Der <?hh Prefix kann nicht wie <?php mit HTML gemischt werden.
// Benutzung von "<?hh //strict" aktiviert den Strikt-Modus des Type-Checkers.


// Typisierung für Funktions-Argumente
function repeat(string $word, int $count)
{
    $word = trim($word);
    return str_repeat($word . ' ', $count);
}

// Typisierung für Rückgabewerte
function add(...$numbers) : int
{
    return array_sum($numbers);
}

// Funktionen ohne Rückgabewert, werden mit "void" typisiert
function truncate(resource $handle) : void
{
    // ...
}

// Typisierung unterstützt die explizit optionale Ein- / Ausgabe von "null"
function identity(?string $stringOrNull) : ?string
{
    return $stringOrNull;
}

// Typisierung von Klassen-Eigenschaften
class TypeHintedProperties
{
    public ?string $name;

    protected int $id;

    private float $score = 100.0;

    // Hack erfordert es, dass typisierte Eigenschaften (also "non-null")
    // einen Default-Wert haben oder im Konstruktor initialisiert werden.
    public function __construct(int $id)
    {
        $this->id = $id;
    }
}


// Kurzgefasste anonyme Funktionen (lambdas)
$multiplier = 5;
array_map($y ==> $y * $multiplier, [1, 2, 3]);


// Weitere, spezielle Felder (Generics)
// Diese kann man sich als ein zugreifbares Interface vorstellen
class Box<T>
{
    protected T $data;

    public function __construct(T $data) {
        $this->data = $data;
    }

    public function getData(): T {
        return $this->data;
    }
}

function openBox(Box<int> $box) : int
{
    return $box->getData();
}


// Formen
//
// Hack fügt das Konzept von Formen hinzu, wie struct-ähnliche arrays
// mit einer typ-geprüften Menge von Schlüsseln
type Point2D = shape('x' => int, 'y' => int);

function distance(Point2D $a, Point2D $b) : float
{
    return sqrt(pow($b['x'] - $a['x'], 2) + pow($b['y'] - $a['y'], 2));
}

distance(
    shape('x' => -1, 'y' => 5),
    shape('x' => 2, 'y' => 50)
);


// Typen-Definition bzw. Aliasing
//
// Hack erlaubt es Typen zu definieren und sorgt somit für bessere Lesbarkeit
newtype VectorArray = array<int, Vector<int>>;

// Ein Tupel mit zwei Integern
newtype Point = (int, int);

function addPoints(Point $p1, Point $p2) : Point
{
    return tuple($p1[0] + $p2[0], $p1[1] + $p2[1]);
}

addPoints(
    tuple(1, 2),
    tuple(5, 6)
);


// Erstklassige Aufzählungen (enums)
enum RoadType : int
{
    Road = 0;
    Street = 1;
    Avenue = 2;
    Boulevard = 3;
}

function getRoadType() : RoadType
{
    return RoadType::Avenue;
}


// Automatische Erstellung von Klassen-Eigenschaften durch Konstruktor-Argumente
//
// Wiederkehrende Definitionen von Klassen-Eigenschaften können durch die Hack-
// Syntax vermieden werden. Hack erlaubt es die Klassen-Eigenschaften über
// Argumente des Konstruktors zu definieren.
class ArgumentPromotion
{
    public function __construct(public string $name,
                                protected int $age,
                                private bool $isAwesome) {}
}

class WithoutArgumentPromotion
{
    public string $name;

    protected int $age;

    private bool $isAwesome;

    public function __construct(string $name, int $age, bool $isAwesome)
    {
        $this->name = $name;
        $this->age = $age;
        $this->isAwesome = $isAwesome;
    }
}


// Kooperatives Multitasking
//
// Die Schlüsselworte "async" and "await" führen Multitasking ein.
// Achtung, hier werden keine Threads benutzt, sondern nur Aktivität getauscht.
async function cooperativePrint(int $start, int $end) : Awaitable<void>
{
    for ($i = $start; $i <= $end; $i++) {
        echo "$i ";

        // Geben anderen Tasks die Möglichkeit aktiv zu werden
        await RescheduleWaitHandle::create(RescheduleWaitHandle::QUEUE_DEFAULT, 0);
    }
}

// Die Ausgabe von folgendem Code ist "1 4 7 2 5 8 3 6 9"
AwaitAllWaitHandle::fromArray([
    cooperativePrint(1, 3),
    cooperativePrint(4, 6),
    cooperativePrint(7, 9)
])->getWaitHandle()->join();


// Attribute
//
// Attribute repräsentieren eine Form von Metadaten für Funktionen.
// Hack bietet Spezial-Attribute, die nützliche Eigenschaften mit sich bringen.

// Das __Memoize Attribut erlaubt es die Ausgabe einer Funktion zu cachen.
<<__Memoize>>
function doExpensiveTask() : ?string
{
    return file_get_contents('http://example.com');
}

// Der Funktionsrumpf wird im Folgenden nur ein einziges mal ausgeführt:
doExpensiveTask();
doExpensiveTask();


// Das __ConsistentConstruct Attribut signalisiert dem type-checker, dass
// die Funktionsdeklaration von __construct für alle Unterklassen dieselbe ist.
<<__ConsistentConstruct>>
class ConsistentFoo
{
    public function __construct(int $x, float $y)
    {
        // ...
    }

    public function someMethod()
    {
        // ...
    }
}

class ConsistentBar extends ConsistentFoo
{
    public function __construct(int $x, float $y)
    {
        // Der Type-checker erzwingt den Aufruf des Eltern-Klassen-Konstruktors
        parent::__construct($x, $y);

        // ...
    }

    // Das __Override Attribut ist ein optionales Signal an den Type-Checker,
    // das erzwingt, dass die annotierte Methode die Methode der Eltern-Klasse
    // oder des Traits verändert.
    <<__Override>>
    public function someMethod()
    {
        // ...
    }
}

class InvalidFooSubclass extends ConsistentFoo
{
    // Wenn der Konstruktor der Eltern-Klasse nicht übernommen wird,
    // wird der Type-Checker einen Fehler ausgeben:
    //
    //  "This object is of type ConsistentBaz. It is incompatible with this object
    //   of type ConsistentFoo because some of their methods are incompatible"
    //
    public function __construct(float $x)
    {
        // ...
    }

    // Auch bei der Benutzung des __Override Attributs für eine nicht veränderte
    // Methode wird vom Type-Checker eine Fehler ausgegeben:
    //
    //  "InvalidFooSubclass::otherMethod() is marked as override; no non-private
    //   parent definition found or overridden parent is defined in non-<?hh code"
    //
    <<__Override>>
    public function otherMethod()
    {
        // ...
    }
}

// Ein Trait ist ein Begriff aus der objektorientierten Programmierung und
// beschreibt eine wiederverwendbare Sammlung von Methoden und Attributen,
// ähnlich einer Klasse.

// Anders als in PHP können Traits auch als Schnittstellen (Interfaces)
// implementiert werden und selbst Schnittstellen implementieren.
interface KittenInterface
{
    public function play() : void;
}

trait CatTrait implements KittenInterface
{
    public function play() : void
    {
        // ...
    }
}

class Samuel
{
    use CatTrait;
}


$cat = new Samuel();
$cat instanceof KittenInterface === true; // True

```

## Weitere Informationen

Die Hack [Programmiersprachen-Referenz](http://docs.hhvm.com/manual/de/hacklangref.php)
erklärt die neuen Eigenschaften der Sprache detailliert auf Englisch. Für
allgemeine Informationen kann man auch die offizielle Webseite [hacklang.org](http://hacklang.org/)
besuchen.

Die offizielle Webseite [hhvm.com](http://hhvm.com/) bietet Infos zum Download
und zur Installation der HHVM.

Hack's [nicht-untersützte PHP Syntax-Elemente](http://docs.hhvm.com/manual/en/hack.unsupported.php)
werden im offiziellen Handbuch beschrieben.
