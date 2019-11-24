---
language: PHP
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
    - ["Abdala Cerqueira", "http://abda.la"]
    - ["Raquel Diniz", "http://twitter.com/raquelrdiniz"]
lang: pt-br
filename: php-pt.html.markdown
---

Este documento descreve PHP 5+.

```php
<?php // O código PHP deve estar incluso na tag <?php

// Se o arquivo PHP só contém código PHP, a melhor prática
// é omitir a tag de fechamento PHP.

// Duas barras iniciam o comentário de uma linha.

# O hash (conhecido como "pound symbol") também inicia, mas // é mais comum.

/*
     O texto envolto por barra-asterisco e asterisco-barra
     faz um comentário de múltiplas linhas.
*/

// Utilize "echo" ou "print" para imprimir a saída.
print('Olá '); // Imprime "Olá " sem quebra de linha.
print 'Olá '; // Não tem a necessidade de utilizar as chaves.

// () são opcionais para print e echo
echo "Mundo\n"; // Imprime "Mundo" com quebra de linha.
echo ("Mundo\n"); // Podemos tambem utilizar com chaves no echo.
// (Todas as declarações devem terminar com um ponto e vírgula.)

// Qualquer coisa fora da tag <?php é impresso automaticamente.
?>
Olá mundo novamente!
<?php


/************************************
 * Tipos e variáveis
 */

// Variáveis começam com o símbolo $.
// Um nome de variável válido se inicia com uma letra ou sublinhado,
// seguido por qualquer quantidade de letras, números ou sublinhados.

// Valores booleanos não diferenciam maiúsculo de minúsculo (case-insensitive).
$boolean = true;  // ou TRUE ou True
$boolean = false; // ou FALSE ou False

// Inteiros
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (um 0 denota um número octal)
$int4 = 0x0F; // => 15 (um 0x denota um literal hex)

// Flutuantes - Floats (aka doubles)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Excluir variável.
unset($int1);

// Aritmética
$soma      = 1 + 1; // 2
$diferenca = 2 - 1; // 1
$produto   = 2 * 2; // 4
$quociente = 2 / 1; // 2

// Taquigrafia aritmética
$numero = 0;
$numero += 1;      // Incrementa $number em 1
echo $numero++;    // Imprime 1 (incrementa após a avaliação)
echo ++$numero;    // Imprime 3 (incrementa antes da avaliação)
$numero /= $float; // Divide e atribui o quociente de $numero

// Strings podem ser colocadas entre aspas simples.
$sgl_quotes = '$String'; // => '$String'

// Evite o uso de aspas duplas, exceto para incorporar outras variáveis
$dbl_quotes = "Esta é uma $sgl_quotes."; // => 'Esta é uma $String.'

// Os caracteres especiais só são escapados entre aspas duplas.
$escapado    = "Este contém um \t caractere tab."; 
echo $escapado; //Imprime: Este contém um     caractere tab.
$naoescapado = 'Este contém somente a barra e o t: \t'; 
echo $naoescapado; //Imprime: Este contém somente a barra e o t: \t

// Coloque uma variável entre chaves se necessário.
$dinheiro = "Eu tenho $${numero} no banco.";

// Desde o PHP 5.3, nowdocs podem ser usados para múltiplas linhas sem análise
$nowdoc = <<<'FIM'
múltiplas linhas
string
FIM;

// Heredocs farão a análise
$heredoc = <<<FIM
múltiplas linhas
$sgl_quotes
FIM;

// Concatenação de string é feita com .
echo 'Esta string ' . 'é concatenada'; //Imprime: 'Esta string é concatenada'


/********************************
 * Constantes
 */

// Uma constante é definida usando define()
// e nunca pode ser mudada durante a execução!

// Um nome de constante válida começa com uma letra ou sublinhado,
// seguido por qualquer quantidade de letras, números ou sublinhados.
define("FOO",     "alguma coisa");

// Acesso a uma constante é possível usando diretamente o nome escolhido
echo 'Isto sairá '.FOO; //Imprime: Isto sairá alguma coisa


/********************************
 * Arrays
 */

// Todos os arrays em PHP são arrays associativos (hashmaps),

// Funciona com todas as versões do PHP
$associativo = array('Um' => 1, 'Dois' => 2, 'Tres' => 3);

// PHP 5.4 introduziu uma nova sintaxe
$associativo = ['Um' => 1, 'Dois' => 2, 'Tres' => 3];

echo $associativo['Um']; // Imprime 1.

// Uma lista de literais atribui chaves inteiras implicitamente
$array = ['Um', 'Dois', 'Tres'];
echo $array[0]; // Imprime => "Um"

// Adiciona um elemento no final do array
$array[] = 'Quatro';

// Remove um elemento do array.
unset($array[3]);

/********************************
 * Saída
 */

echo('Olá Mundo!');
// Imprime Olá Mundo! para stdout.
// Stdout é uma página web se executado em um navegador.

print('Olá Mundo!'); // O mesmo que o echo.

// echo é atualmente um construtor de linguagem, então você pode 
// remover os parênteses.
echo 'Olá Mundo!'; // Imprime: Olá Mundo!
print 'Olá Mundo!'; // O print também é - Imprime: Olá Mundo! 

$paragrafo = 'parágrafo';

echo 100;        // Imprime valores escalares diretamente
echo $paragrafo; // ou variáveis

// Se a abertura de tags curtas está configurada, ou sua versão do PHP é
// 5.4.0 ou maior, você pode usar a sintaxe de echo curto
?>
<p><?= $paragrafo ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // $x agora contém o mesmo valor de $y
$z = &$y;
// $z agora contém uma referência para $y. Mudando o valor de
// $z irá mudar o valor de $y também, e vice-versa.
// $x irá permanecer inalterado com o valor original de $y

echo $x; // Imprime => 2
echo $z; // Imprime => 2
$y = 0;
echo $x; // Imprime => 2
echo $z; // Imprime => 0

// Despeja tipos e valores de variável para o stdout
var_dump($z); // imprime int(0)

// Imprime variáveis para stdout em formato legível para humanos
print_r($array); // imprime: Array ( [0] => Um [1] => Dois [2] => Tres )

/********************************
 * Lógica
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// assert lança um aviso se o seu argumento não é verdadeiro

// Estas comparações serão sempre verdadeiras, mesmo que os tipos 
// não sejam os mesmos.
assert($a == $b); // igualdade
assert($c != $a); // desigualdade
assert($c <> $a); // alternativa para desigualdade
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// A seguir, só serão verdadeiras se os valores correspondem e são do mesmo tipo.
assert($c === $d);
assert($a !== $d);
assert(1 == '1');
assert(1 !== '1');

// As variáveis podem ser convertidas entre tipos, dependendo da sua utilização.

$inteiro = 1;
echo $inteiro + $inteiro; // Imprime => 2

$string = '1';
echo $string + $string; // Imprime => 2 (strings são coagidas para inteiros)

$string = 'one';
echo $string + $string; // Imprime => 0
// Imprime 0 porque o operador + não pode fundir a string 'um' para um número

// Tipo de fundição pode ser utilizado para tratar uma variável 
// como um outro tipo

$booleano = (boolean) 1; // => true

$zero = 0;
$booleano = (boolean) $zero; // => false

// Há também funções dedicadas para fundir a maioria dos tipos
$inteiro = 5;
$string = strval($inteiro);

$var = null; // valor Null


/********************************
 * Estruturas de controle
 */

if (true) {
    print 'Eu fico impresso';
}

if (false) {
    print 'Eu não\'t';
} else {
    print 'Eu fico impresso';
}

if (false) {
    print 'Não fica impresso';
} elseif(true) {
    print 'Fica';
}

// operadores ternários
print (false ? 'Não fica impresso' : 'Fica');

$x = 0;
if ($x === '0') {
    print 'Não imprime';
} elseif($x == '1') {
    print 'Não imprime';
} else {
    print 'Imprime';
}



// Esta sintaxe alternativa é útil para modelos (templates)
?>

<?php if ($x): ?>
Isto é exibido se o teste for verdadeiro.
<?php else: ?>
Isto é apresentado caso contrário.
<?php endif; ?>

<?php

// Use switch para salvar alguma lógica.
switch ($x) {
    case '0':
        print 'Switch faz coerção de tipo';
        break; // Você deve incluir uma pausa, ou você vai cair
               // no caso 'dois' e 'tres'
    case 'dois':
    case 'tres':
        // Faz alguma coisa, se a variável é 'dois' ou 'tres'
        break;
    default:
        // Faz algo por padrão
}

// While, do...while e for são repetições provavelmente familiares
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

$rodas = ['bicicleta' => 2, 'carro' => 4];

// Repetições foreach podem iterar sobre arrays
foreach ($rodas as $contador_rodas) {
    echo $contador_rodas;
} // Imprime "24"

echo "\n";

// Você pode iterar sobre as chaves, bem como os valores
foreach ($rodas as $veiculo => $contador_rodas) {
    echo "O $veiculo tem $contador_rodas rodas";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // Sai da repetição
    }
    echo $i++;
} // Imprime "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // Ignora esta iteração da repetição
    }
    echo $i;
} // Imprime "0124"


/********************************
 * Functions
 */

// Define a função com "function":
function minha_funcao () {
  return 'Olá';
}

echo minha_funcao(); // => "Olá"

// Um nome de função válido começa com uma letra ou sublinhado,
// seguido por qualquer quantidade de letras, números ou sublinhados.

function adicionar($x, $y = 1) { // $y é opcional e o valor padrão é 1
  $resultado = $x + $y;
  return $resultado;
}

echo adicionar(4); // => 5
echo adicionar(4, 2); // => 6

// $resultado não é acessível fora da função
// print $resultado; // Dá uma aviso.

// Desde o PHP 5.3 você pode declarar funções anônimas
$inc = function ($x) {
  return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
  echo "$x - $y - $z";
}

// Funções podem retornar funções
function bar ($x, $y) {
  // Utilize 'use' para trazer variáveis de fora
  return function ($z) use ($x, $y) {
    foo($x, $y, $z);
  };
}

$bar = bar('A', 'B');
$bar('C'); // Imprime "A - B - C"

// Você pode chamar funções nomeadas usando strings
$nome_funcao = 'add';
echo $nome_funcao(1, 2); // => 3
// Útil para dinamicamente determinar qual função será executada.
// Ou utilize call_user_func(callable $callback [, $parameter [, ... ]]);

/********************************
 * Includes (Incluir)
 */

<?php
// PHP dentro de arquivos incluídos também deve começar com uma tag 
// de abertura do PHP.

include 'meu-arquivo.php';
// O código meu-arquivo.php já está disponível no escopo atual.
// Se o arquivo não pode ser incluído (por exemplo, arquivo não encontrado), 
//um aviso é emitido.

include_once 'meu-arquivo.php';
// Se o código no meu-arquivo.php foi incluído em outro lugar, ele não vai
// ser incluído novamente. Isso evita vários erros de declaração de classe

require 'meu-arquivo.php';
require_once 'meu-arquivo.php';
// Faz o mesmo que o include(), exceto que o require() irá causar um erro fatal
// se o arquivo não puder ser incluído

// Conteúdo de meu-include.php:
<?php

return 'Qualquer coisa que você quiser.';
// Fim do arquivo

// Includes e requires também podem retornar um valor.
$valor = include 'meu-include.php';

// Arquivos são incluídos com base no caminho determinado ou,
// se este não for passado, com base na diretiva de configuração include_path.
// Se o arquivo não é encontrado no include_path, o include vai finalmente
// verificar no próprio diretório do script chamado e no diretório
// de trabalho atual antes de falhar.
/* */

/********************************
 * Classes
 */

// As classes são definidas com a palavra-chave class

class MinhaClasse
{
    const MINHA_CONST      = 'valor'; // Uma constante

    static $valorEstatico   = 'estatico';

    // Variáveis estáticas e sua visibilidade
    public static $valorEstaticoPublico = 'estaticoPublico';
    // Acessível somente dentro da classe
    private static $valorEstaticoPrivado = 'estaticoPrivado';
    // Acessível a partir da classe e subclasses
    protected static $valorEstaticoProtegido = 'estaticoProtegido';

    // Propriedades devem declarar a sua visibilidade
    public $propriedade    = 'publica';
    public $propInstancia;
    protected $prot = 'protegida'; // Acessível a partir da classe e subclasses
    private $priv   = 'privada';   // Acessível somente dentro da classe

    // Criar um construtor com o __construct
    public function __construct($propInstancia) {
        // Acesse variável de instância utilizando $this
        $this->propInstancia = $propInstancia;
    }

    // Métodos são declarados como funções dentro de uma classe
    public function meuMetodo()
    {
        print 'MinhaClasse';
    }

    //palavra-chave final faz uma função não poder ser sobrescrita
    final function voceNaoPodeMeSobrescrever()
    {
    }

/*
 * Declarando propriedades ou métodos de classe como estáticos faz deles 
 * acessíveis sem precisar instanciar a classe. A propriedade declarada
 * como estática não pode ser acessada com um objeto
 * instanciado da classe (embora métodos estáticos possam).
*/

    public static function meuMetodoEstatico()
    {
        print 'Eu sou estatico';
    }
}

echo MinhaClasse::MINHA_CONST;    // Imprime 'valor';
echo MinhaClasse::$valorEstatico; // Imprime 'estatico';
MinhaClasse::meuMetodoEstatico(); // Imprime 'Eu sou estatico';

// Instantiate classes using new
$minha_classe = new MinhaClasse('Uma propriedade de instância');
// Os parênteses são opcionais, se não passar algum argumento.

// Acesse membros da classe utilizando ->
echo $minha_classe->propriedade;  // => "publica"
echo $minha_classe->instanceProp; // => "Uma propriedade de instância"
$minha_classe->meuMetodo();       // => "MinhaClasse"


// Estender classes usando "extends"
class MinhaOutraClasse extends MinhaClasse
{
    function imprimePropriedadeProtegida()
    {
        echo $this->prot;
    }

    // Sobrescrever um método
    function meuMetodo()
    {
        parent::meuMetodo();
        print ' > MinhaOutraClasse';
    }
}

$minha_outra_classe = new MinhaOutraClasse('Propriedade de instância');
$minha_outra_classe->imprimePropriedadeProtegida(); // => Prints "protegida"
$minha_outra_classe->myMethod(); // Prints "MinhaClasse > MinhaOutraClasse"

final class VoceNaoPodeMeEstender
{
}

// Você pode usar "métodos mágicos" para criar getters e setters
class MinhaClasseMapa
{
    private $propriedade;

    public function __get($chave)
    {
        return $this->$chave;
    }

    public function __set($chave, $valor)
    {
        $this->$chave = $valor;
    }
}

$x = new MinhaClasseMapa();
echo $x->propriedade; // Irá usar o método __get()
$x->propriedade = 'Algo'; // Irá usar o método __set()

// Classes podem ser abstratas (usando a palavra-chave abstract) ou
// implementar interfaces (usando a palavra-chave implements).
// Uma interface é declarada com a palavra-chave interface.

interface InterfaceUm
{
    public function fazAlgo();
}

interface InterfaceDois
{
    public function fazOutraCoisa();
}

// interfaces podem ser estendidas
interface InterfaceTres extends InterfaceDois
{
    public function fazOutroContrato();
}

abstract class MinhaClasseAbstrata implements InterfaceUm
{
    public $x = 'fazAlgo';
}

class MinhaClasseConcreta extends MinhaClasseAbstrata implements InterfaceDois
{
    public function fazAlgo()
    {
        echo $x;
    }

    public function fazOutraCoisa()
    {
        echo 'fazOutraCoisa';
    }
}


// Classes podem implementar mais de uma interface
class UmaOutraClasse implements InterfaceUm, InterfaceDois
{
    public function fazAlgo()
    {
        echo 'fazAlgo';
    }

    public function fazOutraCoisa()
    {
        echo 'fazOutraCoisa';
    }
}


/********************************
 * Traits (Traços)
 */

// Traits estão disponíveis a partir de PHP 5.4.0 e 
// são declarados usando "trait"

trait MeuTraco
{
    public function meuMetodoDeTraco()
    {
        print 'Eu tenho MeuTraco';
    }
}

class MinhaClasseTracada
{
    use MeuTraco;
}

$cls = new MinhaClasseTracada();
$cls->meuMetodoDeTraco(); // Imprime "Eu tenho MeuTraco"


/********************************
 * Namespaces (Espaço nominal)
 */

// Esta seção é separada porque a declaração de espaços nominais
// deve ser a primeira instrução em um arquivo. Vamos fingir, aqui não é o caso

<?php

// Por padrão, as classes existem no espaço nominal global e podem
// ser explicitamente chamadas com uma barra invertida.

$cls = new \MinhaClasse();



// Definir o espaço nominal para o arquivo
namespace Meu\Espaconominal;

class MinhaClasse
{
}

// (de outro arquivo)
$cls = new Meu\Espaconominal\MinhaClasse;

//Ou de dentro de outro espaço nominal.
namespace Meu\Outro\Espaconominal;

use My\Espaconominal\MinhaClasse;

$cls = new MinhaClasse();

//Ou você pode usar como apelido de espaço nominal;

namespace Meu\Outro\Espaconominal;

use Meu\Espaconominal as OutroEspaconominal;

$cls = new OutroEspaconominal\MinhaClasse();

*/

```

## Mais informações

Visite a [documentação oficial do PHP](http://www.php.net/manual/) 
para referência e participação da comunidade.

Se você estiver interessado em melhores práticas atualizadas, visite
[PHP The Right Way](http://www.phptherightway.com/).

Se você está vindo de uma linguagem com bom gerenciamento de pacotes, confira
[Composer](http://getcomposer.org/).

Para os padrões comuns, visite o Grupo de Interoperabilidade de Framework PHP
[PSR standards](https://github.com/php-fig/fig-standards).
