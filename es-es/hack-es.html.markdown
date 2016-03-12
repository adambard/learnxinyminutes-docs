---
language: Hack
contributors:
    - ["Stephen Holdaway", "https://github.com/stecman"]
    - ["David Lima", "https://github.com/davelima"]
translators:
    - ["César Suárez", "https://github.com/csuarez"]
lang: es-es
filename: learnhack-es.hh
---

Hack es un superconjunto de PHP que se ejecuta en una máquina virtual llamada HHVM. Hack es casi totalmente compatible con código PHP ya existente y añade varias características típicas de los lenguajes de programación estáticamente tipados.

En este artículo sólo se cubren las características específicas de Hack. Los detalles sobre la sintaxis de PHP están en el [artículo sobre PHP](http://learnxinyminutes.com/docs/php/) de esta misma web.

```php
<?hh

// La sintaxis de Hack sólo se habilita para los ficheros que comienzan con
// un marcador <?hh. Estos marcadores no pueden intercalarse con código HTML,
// tal como se puede hacer con <?php. Al usar el marcador "<?hh //strict" el
// comprobador de tipado en modo estricto se pone en modo estricto.

// Indicando el tipo de parámetros escalares
function repeat(string $word, int $count)
{
    $word = trim($word);
    return str_repeat($word . ' ', $count);
}

// Indicando el tipo que devuelve una función
function add(...$numbers) : int
{
    return array_sum($numbers);
}

// Las funciones que no devuelven nada usan el tipo "void"
function truncate(resource $handle) : void
{
    // ...
}

// Al determinar un tipo, hay que indicar explícitamente si permite el valor 
// NULL
function identity(?string $stringOrNull) : ?string
{
    return $stringOrNull;
}

// Se puede especificar el tipo de las propiedades de una clase
class TypeHintedProperties
{
    public ?string $name;
    
    protected int $id;

    private float $score = 100.0;

	// El comprobador de tipos de Hack fuerza que las propiedades tipadas
	// tengan un valor por defecto o que estén asignadas en el constructor
    public function __construct(int $id)
    {
        $this->id = $id;
    }
}


// Funciones anónimas concisas (lambdas)
$multiplier = 5;
array_map($y ==> $y * $multiplier, [1, 2, 3]);


// Genéricos
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


// Shapes
//
// Hack añade el concepto de shape para definir estructuras similares a
// vectores, pero con un conjunto de claves garantizado y tipado
type Point2D = shape('x' => int, 'y' => int);

function distance(Point2D $a, Point2D $b) : float
{
    return sqrt(pow($b['x'] - $a['x'], 2) + pow($b['y'] - $a['y'], 2));
}

distance(
    shape('x' => -1, 'y' => 5),
    shape('x' => 2, 'y' => 50)
);


// Alias de tipos
//
// Hack permite crear alias para hacer que los tipos complejos sean más legibles
newtype VectorArray = array<int, Vector<int>>;

// Una tupla que contiene dos enteros
newtype Point = (int, int);

function addPoints(Point $p1, Point $p2) : Point
{
    return tuple($p1[0] + $p2[0], $p1[1] + $p2[1]);
}

addPoints(
    tuple(1, 2),
    tuple(5, 6)
);


// Enumerados de primera clase
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


// Promoción de argumentos en constructores
//
// Para evitar repetir una y otra vez la definición de constructores que
// sólo asignan propiedades, Hack añade una sintaxis concisa para definir
// propiedades junto al constructor.
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


// Multitarea cooperativa
//
// "async" y "await" son dos palabras claves nuevas para realizar multi-tarea.
// Esto no implica que se usen hilos, sólo permiten transferir el control de la 
// ejecución.
{
    for ($i = $start; $i <= $end; $i++) { 
        echo "$i ";

        // Da a otras tareas la oportunidad de hacer algo
        await RescheduleWaitHandle::create(RescheduleWaitHandle::QUEUE_DEFAULT, 0);
    }
}

// Esto imprime "1 4 7 2 5 8 3 6 9"
AwaitAllWaitHandle::fromArray([
    cooperativePrint(1, 3),
    cooperativePrint(4, 6),
    cooperativePrint(7, 9)
])->getWaitHandle()->join();


// Atributos
//
// Los atributos son una especie de metadatos para funciones. Hack implementa
// algunos atributos especiales para introducir esta característica.

// El atributo especial __Memoize hace que el resultado de la función se cacheé.
<<__Memoize>>
function doExpensiveTask() : ?string
{
    return file_get_contents('http://example.com');
}

// Esta función se va a ejecutar sólo una vez:
doExpensiveTask();
doExpensiveTask();


// El atributo __ConsistentConstruct indica al comprobador de tipos de Hack que
// asegure que la signatura de __construct sea la misma para todas las
// subclases.
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
		 // El comprobador de tipos de Hack fuerza que los constructores de 
		 // los padres sean llamados.
        parent::__construct($x, $y);

        // ...
    }

    // La anotación __Override es un atributo opcional para que el comprobador
    // de tipos fuerce que ese método esté sobrecargando un método de un padre
    // o de un trait. Sino, fallará.
    <<__Override>>
    public function someMethod()
    {
        // ...
    }
}

class InvalidFooSubclass extends ConsistentFoo
{
	// Este constructor no coincide con el padre y causará el siguiente error:
	//
	//  "This object is of type ConsistentBaz. It is incompatible with this 
	//   object of type ConsistentFoo because some of their methods are 
	//   incompatible"
    public function __construct(float $x)
    {
        // ...
    }

    // Usando la anotación __Override en un método que no sobrecarga nada se
    // producirá el siguiente error:
    //
    //  "InvalidFooSubclass::otherMethod() is marked as override; no non-private
    //   parent definition found or overridden parent is defined in non-<?hh 
    //   code"
    <<__Override>>
    public function otherMethod()
    {
        // ...
    }
}


// Los traits pueden implementar interfaces (PHP no soporta esto).
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

## Más información

Para obtener una explicación más detallada de las características que añade Hack a PHP visita la página de [referencia de Hack](http://docs.hhvm.com/manual/en/hacklangref.php) o la [página oficial de Hack](http://hacklang.org/) para información de caracter más general.

Visita la [página oficial de HHVM](http://hhvm.com/) para ver las instrucciones de su instalación.

También puedes visitar la [sección de características de PHP no soportadas por Hack](http://docs.hhvm.com/manual/en/hack.unsupported.php) para más detalles sobre la retrocompatibilidad entre Hack y PHP.
