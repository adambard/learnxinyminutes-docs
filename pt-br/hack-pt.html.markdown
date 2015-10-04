---
language: Hack
contributors:
    - ["Stephen Holdaway", "https://github.com/stecman"]
    - ["David Lima", "https://github.com/davelima"]
translators:
    - ["David Lima", "https://github.com/davelima"]
lang: pt-br
filename: learnhack-pt.hh
---

Hack é uma linguagem baseada no PHP e roda numa máquina virtual chamada HHVM.
Hack é quase completamente interoperável com códigos PHP existentes e adiciona
alguns recursos úteis de linguagens estaticamente tipadas.

Somente recursos específicos da linguagem Hack serão abordados aqui. Detalhes
sobre a sintaxe do PHP estão disponíveis no
[artigo PHP](http://learnxinyminutes.com/docs/php/) neste site.

```php
<?hh

// A sintaxe do Hack é ativada apenas em arquivos que comecem com <?hh
// Marcadores <?hh não podem ser incluídos em páginas HTML, diferente de <?php.
// Usar o marcador "<?hh //strict" coloca o verificador de tipo no modo estrito.


// Indução de tipo de parâmetros escalares
function repeat(string $palavra, int $contagem)
{
    $palavra = trim($palavra);
    return str_repeat($palavra . ' ', $contagem);
}

// Indução de tipo para valores de retorno
function add(...$numeros) : int
{
    return array_sum($numeros);
}

// Funções que não retornam nada são induzidas com "void"
function truncate(resource $recurso) : void
{
    // ...
}

// Induções de tipo devem permitir nulos de forma explícita
function identity(?string $stringOuNulo) : ?string
{
    return $stringOuNulo;
}

// Induções de tipo podem ser especificadas em propriedades de classes
class PropriedadesComTipos
{
    public ?string $nome;
    
    protected int $id;

    private float $pontuacao = 100.0;

    // O verificador de tipos do Hack reforça que propriedades tipadas devem
    // ter um valor padrão ou devem ser definidos no construtor
    public function __construct(int $id)
    {
        $this->id = $id;
    }
}


// Funções anônimas (lambdas)
$multiplicador = 5;
array_map($y ==> $y * $multiplicador, [1, 2, 3]);


// Genéricos
class Caixa<T>
{
    protected T $dados;

    public function __construct(T $dados) {
        $this->dados = $dados;
    }

    public function pegaDados(): T {
        return $this->dados;
    }
}

function abreCaixa(Caixa<int> $caixa) : int
{
    return $caixa->pegaDados();
}


// Formas
//
// Hack adiciona o conceito de formas para definir arrays com uma estrutura
// e tipos de dados garantidos
type Point2D = shape('x' => int, 'y' => int);

function distancia(Point2D $a, Point2D $b) : float
{
    return sqrt(pow($b['x'] - $a['x'], 2) + pow($b['y'] - $a['y'], 2));
}

distancia(
    shape('x' => -1, 'y' => 5),
    shape('x' => 2, 'y' => 50)
);


// Pseudônimos de tipos
//
// Hack adiciona vários recursos para criação de pseudônimos, tornando tipos complexos
// mais fáceis de entender
newtype VectorArray = array<int, Vector<int>>;

// Um tuple contendo dois inteiros
newtype Point = (int, int);

function adicionaPontos(Point $p1, Point $p2) : Point
{
    return tuple($p1[0] + $p2[0], $p1[1] + $p2[1]);
}

adicionaPontos(
    tuple(1, 2),
    tuple(5, 6)
);


// enums em classes
enum TipoDePista : int
{
    Estrada = 0;
    Rua = 1;
    Alameda = 2;
    Avenida = 3;
}

function getTipoDePista() : TipoDePista
{
    return TipoDePista::Alameda;
}


// Especificação de argumentos no construtor (Argument Promotion)
// 
// Para evitar que propriedades sejam definidas em mais de um lugar, e
// construtores que só definem propriedades, o Hack adiciona uma sintaxe para
// definir as propriedades e o construtor ao mesmo tempo.
class ArgumentPromotion
{
    public function __construct(public string $nome,
                                protected int $idade,
                                private bool $legal) {}
}

class SemArgumentPromotion
{
    public string $nome;

    protected int $idade;

    private bool $legal;

    public function __construct(string $nome, int $idade, bool $legal)
    {
        $this->nome = $nome;
        $this->idade = $idade;
        $this->legal = $legal;
    }
}


// Multi-tarefas cooperativo
//
// Duas novas palavras-chave ("async" e "await") podem ser usadas para
// trabalhar com multi-tarefas.
// Obs. Isto não envolve threads - apenas permite a transferência de controle
async function printCooperativo(int $inicio, int $fim) : Awaitable<void>
{
    for ($i = $inicio; $i <= $fim; $i++) { 
        echo "$i ";

        // Permite que outras tarefas façam algo
        await RescheduleWaitHandle::create(RescheduleWaitHandle::QUEUE_DEFAULT, 0);
    }
}

// Imprime "1 4 7 2 5 8 3 6 9"
AwaitAllWaitHandle::fromArray([
    printCooperativo(1, 3),
    printCooperativo(4, 6),
    printCooperativo(7, 9)
])->getWaitHandle()->join();


// Atributos
//
// Atributos são uma forma de definir metadados para funções.
// Hack tem alguns atributos especiais que possuem comportamentos úteis.

// O atributo especial __Memoize faz com que o resultado da função fique em cache
<<__Memoize>>
function tarefaDemorada() : ?string
{
    return file_get_contents('http://exemplo.com');
}

// O corpo da função só é executado uma vez aqui:
tarefaDemorada();
tarefaDemorada();


// O atributo especial __ConsistentConstruct faz com que o Hack certifique-se
// de que a assinatura do construtor seja a mesma em todas as subclasses
<<__ConsistentConstruct>>
class FooConsistente
{
    public function __construct(int $x, float $y)
    {
        // ...
    }

    public function algumMetodo()
    {
        // ...
    }
}

class BarConsistente extends FooConsistente
{
    public function __construct(int $x, float $y)
    {
        // O verificador de tipos do Hack exige que os construtores pai
        // sejam chamados
        parent::__construct($x, $y);

        // ...
    }

    // A anotação __Override é uma anotação opcional que faz com que o
    // verificador de tipos do Hack sobrescreva um método em uma classe pai
    // ou um trait. Sem __Override, definir este método causará um erro,
    // pois ele já foi definido na classe pai (FooConsistente):
    <<__Override>>
    public function algumMetodo()
    {
        // ...
    }
}

class SubclasseFooInvalida extends FooConsistente
{
    // Caso o construtor não combine com o construtor da classe pai, o
    // verificador de tipos acusará um erro:
    //
    //  "Este objeto é incompatível com o objeto FooConsistente porque algum(ns)
    //   dos seus métodos são incompatíveis"
    //  
    public function __construct(float $x)
    {
        // ...
    }

    // Usar a anotação __Override em um método que não existe na classe pai
    // causará um erro do verificador de tipos:
    //  "SubclasseFooInvalida::outroMetodo() está marcada para sobrescrever;
    //   nenhuma definição não-privada foi encontrada ou a classe pai foi
    //   definida em código não-<?hh"
    //  
    <<__Override>>
    public function outroMetodo()
    {
        // ...
    }
}


// Traits podem implementar interfaces (não suportado pelo PHP)
interface InterfaceGatinho
{
    public function brinca() : void;
}

trait TraitGato implements InterfaceGatinho
{
    public function brinca() : void
    {
        // ...
    }
}

class Samuel
{
    use TraitGato;
}


$gato = new Samuel();
$gato instanceof InterfaceGatinho === true; // True

```

## Mais informações

Visite a [documentação do Hack](http://docs.hhvm.com/manual/en/hacklangref.php)
para ver explicações detalhadas dos recursos que Hack adiciona ao PHP, ou o [site oficial do Hack](http://hanlang.org/)
para outras informações.

Visite o [site oficial do HHVM](http://hhvm.com/) para aprender a instalar o HHVM.

Visite [este artigo](http://docs.hhvm.com/manual/en/hack.unsupported.php) para ver
os recursos do PHP que o Hack não suporta e ver incompatibilidades entre Hack e PHP.
