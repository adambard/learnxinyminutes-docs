---
language: Hack
filename: learnhack-cs.hh
contributors:
    - ["Stephen Holdaway", "https://github.com/stecman"]
translators:
    - ["Vojta Svoboda", "https://github.com/vojtasvoboda/"]
lang: cs-cz
---

Hack je nadmnožinou PHP a běží v rámci virtuálního stroje zvaného HHVM. Hack 
dokáže skoro plně spolupracovat s existujícím PHP a přidává několik vylepšení, 
které známe ze staticky typovaných jazyků.

Níže jsou popsané pouze vlastnosti jazyka Hack. Detaily ohledně jazyka PHP a jeho
syntaxe pak najdete na těchto stránkách v samostatném 
[článku o PHP](http://learnxinyminutes.com/docs/php/).

```php
<?hh

// Hack je aktivní pouze pro soubory, které začínají <?hh.
// TODO <?hh soubory nemohou být jendoduše přeloženy v HTML tak jako <?php.
// Použitím značky <?hh //strict zapnete striktní mód typové kontroly.


// Typování skalární parametrů
function repeat(string $word, int $count)
{
    $word = trim($word);
    return str_repeat($word . ' ', $count);
}

// Typování návratových hodnot
function add(...$numbers) : int
{
    return array_sum($numbers);
}

// Funkce které nic nevrací jsou typované jako "void"
function truncate(resource $handle) : void
{
    // ...
}

// U typování musíme explicitně povolit prázdné (null) hodnoty
function identity(?string $stringOrNull) : ?string
{
    return $stringOrNull;
}

// Typování může být použito i na proměnné třídy
class TypeHintedProperties
{
    public ?string $name;
    
    protected int $id;

    private float $score = 100.0;

    // Typ proměnné si můžeme zadat přímo u definice proměnné v rámci třídy,
    // ale pak ho snadně přetížit v konstruktoru metody.
    public function __construct(int $id)
    {
        $this->id = $id;
    }
}


// Stručné anonymní funkce (lambda funkce)
$multiplier = 5;
array_map($y ==> $y * $multiplier, [1, 2, 3]);


// Generika (generické funkce)
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


// Tvary
// 
// Hack zavádí koncept tvaru pro definování strukturovaných polí s garantovanou
// typovou kontrolou pro klíče.
type Point2D = shape('x' => int, 'y' => int);

function distance(Point2D $a, Point2D $b) : float
{
    return sqrt(pow($b['x'] - $a['x'], 2) + pow($b['y'] - $a['y'], 2));
}

distance(
    shape('x' => -1, 'y' => 5),
    shape('x' => 2, 'y' => 50)
);


// Type aliasing
// 
// Hack přidává několik vylepšení pro lepší čitelnost komplexních typů
newtype VectorArray = array<int, Vector<int>>;

// Množina obsahující čísla
newtype Point = (int, int);

function addPoints(Point $p1, Point $p2) : Point
{
    return tuple($p1[0] + $p2[0], $p1[1] + $p2[1]);
}

addPoints(
    tuple(1, 2),
    tuple(5, 6)
);


// Výčtový typ
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


// Automatické nastavení proměnných třídy
// 
// Aby se nemuseli definovat proměnné třídy a její konstruktor,
// který pouze nastavuje třídní proměnné, můžeme v Hacku vše 
// definovat najednou.
class ArgumentPromotion
{
    public function __construct(public string $name,
                                protected int $age,
                                private bool $isAwesome) {}
}

// Takto by to vypadalo bez automatického nastavení proměnných
class WithoutArugmentPromotion
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


// Ko-operativní multi-tasking
// 
// Nová klíčová slova "async" and "await" mohou být použité pro spuštění mutli-taskingu
// Tato vlastnost ovšem zahrnuje vícevláknové zpracování, pouze povolí řízení přenosu
async function cooperativePrint(int $start, int $end) : Awaitable<void>
{
    for ($i = $start; $i <= $end; $i++) { 
        echo "$i ";

        // Dává ostatním úlohám šanci něco udělat
        await RescheduleWaitHandle::create(RescheduleWaitHandle::QUEUE_DEFAULT, 0);
    }
}

// Toto vypíše "1 4 7 2 5 8 3 6 9"
AwaitAllWaitHandle::fromArray([
    cooperativePrint(1, 3),
    cooperativePrint(4, 6),
    cooperativePrint(7, 9)
])->getWaitHandle()->join();


// Atributy
// 
// Atributy jsou určitou formou metadat pro funkce. Hack přidává některé vestavěné 
// atributy které aktivnují uživatečné chování funkcí.

// Speciální atribut __Memoize způsobí, že výsledek funkce je uložen do cache
<<__Memoize>>
function doExpensiveTask() : ?string
{
    return file_get_contents('http://example.com');
}

// Tělo funkce je v tomto případě vykonáno pouze jednou:
doExpensiveTask();
doExpensiveTask();


// Speciální atribut __ConsistentConstruct signalizuje typové kontrole Hacku, že
// zápis __construct bude stejný pro všechny podtřídy.
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
        // Typová kontrola Hacku zajistí volání konstruktoru rodičovské třídy
        parent::__construct($x, $y);

        // ...
    }

    // Anotace __Override je volitelný signál pro typovou kontrolu Hacku, že
    // tato metoda přetěžuje metodu rodičovské třídy, nebo traitu. Bez uvedení
    // této anotace vyhodí typová kontrola chybu.
    <<__Override>>
    public function someMethod()
    {
        // ...
    }
}

class InvalidFooSubclass extends ConsistentFoo
{
    // Nedodržení zápisu dle rodičovského konstruktoru způsobí syntaktickou chybu:
    //  
    //  "Tento objekt je typu ConsistentBaz a není kompatibilní v tímto objektem,
    //   který je typu ConsistentFoo protože některé jeho metody nejsou kompatibilní."
    //
    public function __construct(float $x)
    {
        // ...
    }

    // Použitím anotace __Override na nepřetíženou metodu způsobí chybu typové kontroly:
    //  
    //  "InvalidFooSubclass::otherMethod() je označená jako přetížená, ale nebyla nalezena
    //   taková rodičovská metoda, nebo rodič kterého přetěžujete není zapsán v <?hh kódu"
    //
    <<__Override>>
    public function otherMethod()
    {
        // ...
    }
}


// Traity mohou implementovat rozhraní, což standardní PHP neumí
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

## Více informací

Pro více informací navštivte [referenční příručku jazyka Hack](http://docs.hhvm.com/manual/en/hacklangref.php), 
kde se dozvíte více detailu a vylepšení, které jazyk Hack přidává do PHP, a nebo navštivte [oficiální stránky jazyka Hack](http://hacklang.org/)
pro obecné informace.

Pro instrukce k instalaci jazyka Hack navštivte [oficiální HHVM stránky](http://hhvm.com/).

Pro více informací ohledně zpětné kompatibility s PHP navštivte článek o [nepodporovaných PHP vlastnostech Hacku](http://docs.hhvm.com/manual/en/hack.unsupported.php).
