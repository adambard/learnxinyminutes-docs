---
language: Solidity
filename: learnSolidity-br.sol
contributors:
  - ["Nemil Dalal", "https://www.nemil.com"]
  - ["Joseph Chow", ""]
translators:
    - ["João Farias", "http://thatsabug.com/"]
lang: pt-br
---

Solidity permite você programar para a [Ethereum]
(https://www.ethereum.org/), uma máquina virtual baseada na tecnologia blockhain
para criação e execução de contratos inteligentes, sem necessidade de partes
centralizadas ou de confiança.

Solidity é uma linguagem de contratos estaticamente tipaada com similaridade com
Javascript e C. Como objetos em programação orientada a objetos, cada contrato
possue variáveis de estado, funções e tipos de dados comuns. Funcionalidades
particulares de contratados incluem cláusuras modificadoras (guarda), notifica
dores de eventos para listerners e variáveis globais customizadas.


Exemplos de contratos Ethereum incluem crowdfunding, votações e audições cegas.

Erros em código Solidity causam grandes riscos e custos; portanto, você
deve ser muito cuidado com teste e divulgação. DEVIDO ÀS CONSTANTES MUDANÇAS
NO ETHEREUM, ESTE DOCUMENTOS PROVAVELMENTE NÃO ESTARÁ ATUALIZADO, VOCÊ DEVE
ACOMPANHAR A CHATROOM DO SOLIDITY E O BLOG DO ETHEREUM PARA NOTÍCIAS ATUALIZADAS.
TODO CÓDIGO AQUI É ENTREGUE COMO ESTÁ, COM SUBSTANCIAL RISCO DE ERRROS E PADRÕES
DE CÓDIGO DEPRECADOS.

Diferentemente de outros tipo de código, você também deve adicionar padrões
como pausa, deprecação e retração para reduzir riscos. Este documento discute
sintaxe, então, muito padrões populares são excluídos.

Como Solidity e Ethereum ainda estão sob desenvolvimento, funcionalidades beta
e experimentais são tipicamente marcadas e sujeitas à mudanças. Pull requests
são bem-vindos.

```javascript
// Primeiramente, um contrato de um Banco simples
// Permite depósitos, retiradas e checagens de saldo

// banco_simples.sol (note a extensão .sol)

/* **** INCICIO DO EXEMPLO **** */

// Declare a versão do compilador.
pragma solidity ^0.4.2;

// Inicie com comentários Natspec (as três barras)
// usados para documentação - e como dados descritivos para elementos/ação de UI

/// @title BancoSimples
/// @author nemild

/* 'contrato' tem similadirades com 'classes' em outras linguagens (variáveis de
class, herança, etc.) */

contract BancoSimples { // CamelCase
    // Declare variáveis de estado fora da função, para persistí-la durante a
    // duração do contrato

    // dicionário que mapeia endereços para saldos
    // tenha cuidado  sobre ataques de overflow com números

    mapping (address => uint) private saldos;

    // "private" significa que outros contratos não podem acessar saldos
    // diretamente, mas o dado ainda é visível para outras partes da blockchain

    address public dono;

    // ´public´ é legível (mas sem acesso de escrita) por usuários e contratos

    // Eventos - ações públicas para ouvintes externo
    event LogRealizacaoDeDeposito(address numeroDaConta, uint quantidade);

    // Construtor, pode receber uma ou várias variáveis; apenas uma opção é
    // permitidas

    function BancoSimples() {
        // msg dá detalhes sobre a mensagem mandada pelo contrato
        // msg.sender é um chamador do contrato (endereço do criador do
        // contrato)

        dono = msg.sender;
    }

    /// @notice Deposita ether no banco
    /// @return O saldo do usuário após o depósito

    function deposito() public returns (uint) {
        saldos[msg.sender] += msg.value;

        // Sem necessidade de "this." ou "self." para variáveis de estado
        // todos as variáveis são inciadas com seu valor default

        LogRealizacaoDeDeposito(msg.sender, msg.value); // dispara evento

        return saldos[msg.sender];
    }

    /// @notice Retira ether do banco
    /// @dev Isto não retorna nenhum ether excendente
    /// @param quantidadeDeRetirada quantidade que você quer retirar
    /// @return O saldo restante do usuário
    function retirada(uint quantidadeDeRetirada) public returns (uint saldoRestate) {
        if(saldos[msg.sender] >= quantidadeDeRetirada) {

            // Observe como deduzimos o saldo imediatamente, antes de enviar -
            // devido ao risco de uma chamada recursiva que permite o chamador
            // pedir um valor maior que seu saldo

            saldos[msg.sender] -= quantidadeDeRetirada;

            if (!msg.sender.send(quantidadeDeRetirada)) {
                // incremente de volta só se falhar, como pode estar enviando
                // para o contrato que substituiu 'enviar' no final
                // do recebimento
                saldos[msg.sender] += quantidadeDeRetirada;
            }
        }

        return saldos[msg.sender];
    }

    /// @notice Retorna o saldo
    /// @return O saldo do usuário
    // 'constant' evita que a função edite variáveis de estado
    // permite a função executar localmente/fora da blockchain
    function saldo() constant returns (uint) {
        return saldos[msg.sender];
    }

    // Função de fallback - Chamada se outras funções não forem chamadas ou
    // se ether sem dados forem enviados
    // Tipicamente chamada quando dados inválidos são enviados
    // Adicionada para que ether enviado neste contrato seja revertido se o
    // contrato falhar. Se não existisse, o dinheiro do enviante é transferido
    // para o contrato
    function () {
        throw; // 'throw' reverte o estao para antes da chamada
    }
}
// ** FIM DO EXEMPLO **

// Agora, o básico de Solidity


//1 TIPO DE DADOS E MÉTODOS ASSOCIADOS
// uint é usado para quantidade de moeda (não existem doubles ou floats)
// e para datas (no sistema de tempo Unix)

uint x;

// int de 256 bits, não pode ser mudado após sua instanciação
int constant a = 8;
int256 constant a = 8; // mesmo efeito, mas aqui os 256 bits são explícitos
uint constant VERSÃO_ID = 0x123A1; // uma constante hexadecimal

// com 'constant', o compilador substitui cada ocorrência com o valor

// Para int e uint, é possível determinar o espaço explicitamente, em intervalos
// de 8 a 256, e.g., int8, int16, int24
uint8 b;
int64 c;
uint248 e;

// Cuidado contra overflows, e proteja-se contra esse tipo de ataque

// Não há funções randômicas padrão, use outros contratos para este objetivo

// Casting de tipo
int x = int(b);

bool b = true; // ou então 'var b = true;' para inferição de tipo

// Endereços - comportam 20 bytes/160 bits endereços Ethereum
// Não permite operações aritiméticas
address public dono;

// Tipos de contas:
// Conta de contrato: endereço criado ao criar (função do endereço do criador,
// número da transação)
// Conta externa: (pessoa/entidade externa): endereç criado a partir de chave
// pública

// Adicione o campo 'public' para indicar visibilidade pública/externa
// um getter é automaticamente criado, mas NÃO um setter

// Todos os endereços podem enviar ether
dono.send(ALGUM_SALDO); // returna falso caso falhe
if (dono.send) {} // LEMBRE-SE: encapsule num 'if', dado que endereços de
// contrato tem funções executadas no envio e estas podem falhar
//Também certifique-se que os saldos deduzidos ANTES de tentar enviar, dado que
// há um risco de chamada recursiva que pode drenar um contrato

// pode sobrescrever seu próprio

// Pode verificar o saldo
dona.balance; // o saldo do dono (usuário ou contrato)

// Bytes permitidos de 1 a 32
byte a; // byte é o mesmo que bytes1
bytes2 b;
bytes32 c;

// Bytes dinamicamente criados

bytes m; // Um array especial, mesmo que byte[] (mas mais comprimido)

// Mais custoso que byte1-byte32, então, prefira estes quando possível

// mesmo que bytes, mas não permite verificar tamanho ou acesso por indíce (por
// enquanto)

string n = "oi"; // guardado em UTF8, note as aspas duplas, não simples

// funções de string serão adicionadas no futuro
// prefira bytes32/bytes, dado que UTF8 usa mais espaço

// Inferência de tipo
// var não infere tipos baseados na primeira atribuição,
// não pode ser usado em paramêtros de funções

var a = true;

// use com cuidado, inferência pode resultar em tipos errados
// e.g., um int8, quando um contador precisa de int16

// var pode ser usado para assinalar uma função a uma variável
function a(uint x) returns (uint) {
    return x * 2;
}
var f = a;
f(22); // chamada

// por padrão, todos os valores são inicializados com 0

// Delete pode ser chamada na maioria dos tipos
// (NÃO destroi o valor, mas retorna para o valor 0, o incial)

uint x = 5;

// Desestruturação/Tuplas
(x, y) = (2, 7); // assinada/troca múltiplos valores

// 2. ESTRUTURAS DE DADOS
// Arrays

bytes32[5] apelidos; // array estático
bytes32[] nomes; // array dinâmico
uint novoTamanho = nomes.push("João"); // adicionando retorna o novo tamanho do

// Tamanho
nomes.length; // pega o tamanho
nomes.length = 1; // tamanhos pode ser alterados (para arrays dinâmicos)

// arrays multidimensionais
uint x[][5]; // array com 5 arrays dinâmicos como elementos (ordem da maioria
// das linguagens)

// Dicionários (qualquer tipo para qualquer tipo)
mapping (string => uint) public saldos;
saldos["charles"] = 1;
console.log(saldos["ada"]); // é 0, toda chave não assinalada retorna zero
// 'public' permite o seguinte contrato
nomeDoContrato.saldos("charles"); // retorna 1
// 'public' cria um getter (mas não um setter) como o seguinte
function saldos(string _conta) returns (uint saldo) {
    return saldos[_conta];
}

// Mapeamentos aninhados
mapping (endereco => mapping (endereco => uint)) public guardioes;

// Para deletar
delete saldos["John"];
delete saldos; // assinala zero para todas as chaves

// Diferentemente de outras linguages, NÃO É POSSÍVEL iterar sobre todos os
// elementos de um mapeamento, sem saber previamente as chaves - é possível
// construir estruturas de dados personalizadas para fazer isso

// Structs e enums
struct Banco {
    address dono;
    uint saldo;
}
Banco b = Banco({
    dono: msg.sender,
    saldo: 5
});
// ou
Banco c = Banco(msg.sender, 5);

c.quantidade = 5; // cria novo valor
delete b;
// assinala todos os valores do enum para zero, exceto mapeamentos

// Enums
enum Estado { Criado, Travado, Inativo }; // geralmente usado para máquina de
// estados
Estado public estado; // Declara variável para enum
estado = Estado.Criado;
// enums podem ser explicitamente convertidas em ints
uint estadoCriado = uint(Estado.Criado); //  0

// Localização de dados: Memória vs. disco vs. pilha - todos os tipos complexos
// (arrays, structs) tem uma localização de dados
// 'memória' não é persistida, 'disco' é
// Padrão é 'disco' para variáveis locais e de estado; 'memória' para paramêtros
// de função. Pilha guarda pequena variáveis locais

// a maioria dos tipos podem ter sua localização de dados explicitamente assinalos

// 3. Operações simples
// Comparações, operadores binários e aritimétricos são providos
// exponenciação: **
// ou exclusivo: ^
// negação binária: ~

// 4. Variáveis Globais de nota
// ** this **
this; // endereço do contrato
// geralmente usado no final do contrato para enviar o saldo restante às partes
this.balance;
this.algumFuncao(); // chamada de função externa via call, não via jump interno

// ** msg - Mensagem corrente recebida pelo contrato ** **
msg.sender; // endereço do enviador
msg.value; // quantidade de ether provida para este contrato em wei
msg.data; // bytes, todos os dados da chamada
msg.gas; // gas restante

// ** tx - Esta transação **
tx.origin; // endereço do enviador da transação
tx.gasprice; // valor do gas da transação

// ** block - Informação do bloco corrente **
now; // tempo corrente (aproxiamdo), substituto para block.timestamp
//(usa tempo do Unix)
block.number; // número do bloco corrente
block.difficulty; // dificuldade do bloco corrente
block.blockhash(1); // retorna bytes32, só funciona para os 256 blocos mais
//recentes
block.gasLimit();

// ** storage - Hash de disco persistente **
storage['abc'] = 'def'; // mapea palavras de 256 bits em palavras de 256 bits


// 4. FUNÇÕES E MAIS
// A. Funções
// Funções simples
function incremento(uint x) returns (uint) {
    x += 1;
    return x;
}

// Funções podem retornar muito argumentos, e podem especificar argumentos
// retornados sem necessidade de explicitamente usar return
function incremento(uint x, uint y) returns (uint x, uint y) {
    x += 1;
    y += 1;
}
// Chamando a função anterior
uint (a,b) = incremento(1,1);

// 'constant' indica que uam função não altera variáveis persistidas
// Funções constantes são executadas localmente, não na blockchain
uint y;

function incremento(uint x) constant returns (uint x) {
    x += 1;
    y += 1; // Esta linha deve falhar
    // y é uma variável de estado e não pode ser alterada por uma função local
}

// 'Especificadores de visibilidade de funções'
// Estes podem substituitir 'constante', incluíndo:
// public - visbilidade externa e interna (padrão)
// private - apenas visível no contrato corrente
// internal - apenas visível no contrato corrente e seus derivados

// Functions hosteada - e pode ser assinalada para variável
function a() {
    var z = b;
    b();
}

function b() {

}

// Prefira loops sobre recursões (pilha de chamada é no máximo 1024)

// B. Eventos
// Eventos podem notificar partes externas; facilmente buscáveis e acessáveis
// de fora da blockchain (com clientes leves)
// tipicamente declarados após os parâmetros do contrato

// Tipicamente, com letra maiúscula - e adicione Log na frente para
// ser explicito e previnir confusão na chamada da função

// Declaração
event LogEnvio(address indexed de, address indexed para, uint quantidade);
// Observe a letra maíscula no início do nome

// Chamada
Envio(de, para, quantidade);

// Para partes externas (um contrato ou entidade externo), observe:
Coin.Envio().watch({}, '', function(erro, resultado) {
    if (!erro) {
        console.log("Moeda transferida: " + resultado.args.quantidade +
            " moedas enviadas de " + resultado.args.de +
            " para " + resultado.args.para + ".");
        console.log("Saldo atual:\n" +
            "Enviador: " + Coin.balances.call(resultado.args.de) +
            "Recebedor: " + Coin.balances.call(resultado.args.para));
    }
}
// Paradigma comum para um contrato depender de outro (e.g., um contrato que
// depende da taxa de troca provida por outro)

// C. ModifiCadores
// MOdificadores validam entradas de funções, como saldo mínimo e autorização
// do usuário; semelhantes a guardas em outras linguagens

// '_' (subtraço) geralmente incluído como última linha do corpo, indica que a
// função sendo chamada deve ser colocada ali
modifier apenasDepois(uint _tempo) { if (agora <= _tempo) throw; _ }
modifier apenasDono { if (msg.sender == dono) _ }
// geralmente usado para máquina de estado
modifier apenasSeEmEstado (Estado estadoCorrente)
{ if (estadoCorrente != Estado.A) _ }

// Concatenado logo após a chamada da função
function mudeDona(novoDono)
apenasDepois(algumTempo)
apenasDono()
apenasSeEmEstado(Estado.A)
{
    dono = novoDono;
}

// subtração pode ser incluído antes do final do corpo, mas retorno explícitos
// pode ser ignorado, então, tome cuidado
modifier chequeValor(uint quantidade) {
    _
    if (msg.value > quantidade) {
        uint quantidadeASerDevolvida = quantidade - msg.value;
        if (!msg.sender.send(quantidadeASerDevolvida)) {
            throw;
        }
    }
}

// 6. BRANCHES E LOOPS

// Todas as lógicas básicas de bloco funcionam - incluindo if/else,
// while, break, continue, return - mas não há switch

// A sintaxe é semelhante a Javascript, mas sem conversão de tipos
// de não-booleanos para booleanos (operadores de comparação precisam
// utilizar valores booleanos)

// Loops que dependem o comportamento do usuário exigem cuidado - dado
// que contratos tem uma quantidade máxima de gas para cada bloco de
// de código - falhas acontecerão caso ele seja excedido
// Por exemplo:
for(uint x = 0; x < listaDeEnderecoDeRefundo.length; x++) {
    if (!listaDeEnderecoDeRefundo[x].send(ALGUMA_QUANTIDADE)) {
       throw;
    }
}

// Dois erros acima:
// 1. Uma falha no enviar para o loop completamente, travando dinheiro
// 2. Este loop pode ser abitrariamente longo (basado na quando que
// o usuário precisa de refundo), portanto, pode falhar quando exceder
// a quantidade máxima de gas do bloco
// Ao invés disso, você deve deixar as pessoas retirarem
// individualmente de suas subcontas e marcarem a retirada


// 7. OBJETOS/CONTRATOS

// A. Chamando um contrato externo
contract FonteDeInformacoes {
    function info() returns (uint ret) { return 42; }
}

contract Consumidor {
    FonteDeInformacoes fonte; // aponta para um contrato na blockchain

    // Assinala variável para uma instância do contrato
    function setFonte(address endereco) {
        // Cast automático, cuidado; construtor não é chamado
        fonte = FonteDeInformacoes(endereco);
    }

    // Assinala variável para uma nova instância do contrato
    function createNewFeed() {
        fonte = new FonteDeInformacoes(); // nova instância criada
        // construtor é chamado
    }

    function chameFonte() {
        // último parenteses chama o contrato, podendo adicionar
        // opcionalmente valores ou gas
        fonte.info.value(10).gas(800)();
    }
}

// B. Herança

// Ordem importa, último contrato herdado (i.e., 'def') pode
// sobrescrever partes de contratos previamente herdados
contract MeuContratdo is abc, def("um argumento personalizado def") {

// sobrescrevendo função
    function z() {
        if (msg.sender == dono) {
            def.z(); // chama função sobrescrita de def
            super.z(); // chama função do pai imeadiato
        }
    }
}

// função abstrata
function umaFuncaoAbstrata(uint x);
// não pode ser compilada, usada em contratos base/abstratos que
// então, a implementam

// C. Import

import "filename";
import "github.com/ethereum/dapp-bin/library/iterable_mapping.sol";

// 'Import' está sobre desenvolvimento
// Atualmente não pode ser usada na linha de comando


// 8.OUTRAS PALAVRAS-CHAVE

// A. Throwing
// Throwing
throw; // reverte estado e dinheiro NÃO-USADO é devolvido ao usuários
// Atualmente não pode ser capturado

// Um padrão de design comum é:
if (!endereco.send(123)) {
    throw;
}

// B. Selfdestruct
// auto-destroe o contrato corrente, enviando fundos para o endereço
// (geralmente o criador)
selfdestruct(ALGUM_ENDERECO);

// remove disco/código dos blocos corrente e futuros
// ajuda clientes leves, mas dados persistidos continuam no blockchain

// Padrão comum, permite ao dono finalizar o contrato e receber fundos
// restantes
function remover() {
    if(msg.sender == criador) { // Apenas o criador do contrato pode
                                // fazer isso
        selfdestruct(criador); // Inativa o contrato e retorna os fundos
    }
}

// Talvez queria desativar o contrato manualmente, ao invés de usar
// selfdestruct (ether enviado para contratos selfdestructed é perdido)


// 9. NOTAS SOBRE DESIGN DE CONTRATOS

// A. Obfuscação
// Todas as variáveis são publicamente visíveis na blockchain, então
// qualquer coisa privada precisa ser obfuscada (e.g., hash com segredo)

// Passo-a-pass: 1. Comprometa-se com algo, 2. Revele compromisso
sha3("quantidade_de_lance", "algum segredo"); // compromisso

// chame a função reveal (revelar) do contrato no futuros
// mostrando o lance mais o segredo para foi hasheado com SHA3
reveal(100, "meuSegredo");

// B. Otimização de disco
// Escrever na blockchain pode ser caro, dado que os dados são guardados
// para sempre. É encorajado que contratos inteligentes usem memória (
// enventualmente, compilação será melhor, mas por enquanto é benéfico
// usar estruturas de dados simples - armazenando minimamente na
// blockchain)

// Custo pode ser alto para item como arrays multidimensionais
// (custo para guardar os dados - não declarar variáveis)

// C. Acesso de dados da blockchain

// Não pode restringir humanos ou computadores de ler os conteúdos
// de transações ou estado de transações

// Enquanto 'private' previne outros *contratos* de ler dados ]
// diretamente - qualquer outra parte ainda pode ler dados da blockchain

// Todos os dados são armazedos na blockchain, para que qualquer um
// possa observar dados antigos e mudanças

// D. Jobs Cron
// Contratos deve ser manualmente chamados para lidar com agendamentos
// baseados em tempo; podendo criar código externo para pingar
// regularmente ou prover incentivos (ether) para outros fazê-lo

// E. Padrão Observador
// O Padrão Observador permite que você registre um inscritor e
// registre uma função a ser chamada pelo Oráculo (nota, Oráculos pagam
// pela ação executada). Similarmente à subscrição em Pub/sub

// Este é um contrato abstrato, tanto as classes cliente como a
// servidor importam o cliente que deve ser implementado
contract AlgumOraculoCallback {
    function OraculoCallback(int _valor, uint _tempo, bytes32 info) external;
}

contract AlgumOráculo {
    AlgumOraculoCallback[] callbacks; // array com todos os inscritos

    // Registra inscrito
    function addInscrito(AlgumOraculoCallback a) {
        callbacks.push(a);
    }

    function notificar(valor, tempo, info) private {
        for(uint i = 0;i < callbacks.length; i++) {
            // todos os inscritos precisam implementar AlgumOraculoCallback
            callbacks[i].OraculoCallback(valor, tempo, info);
        }
    }

    function facaAlgo() public {
        // Código para fazer algo

        // Notifica todos os inscrito
        notificar(_valor, _tempo, _info);
    }
}

// Agora, seu contrato cliente pode addInscrito importando
// AlgumOraculoCallback e registrando algum Oráculo

// F. Máquinas de estado
// veja o exemplo abaixo para o enum Estado e o modificador noEstado

// *** EXEMPLO: Um exemplo de crowdfunding example (similar ao
// Kickstarter) ***
// ** INCIO DO EXEMPLO **

// FundadorDoCrowdFunding.sol

/// @title FundadorDoCrowdFunding
/// @author nemild
contract FundadorDoCrowdFunding {
    // Variáveis assinaladas na crição pelo criador
    address public criador;
    address public recipiente; // criador pode ser diferente do Recipiente
    uint public minALevantar; // requisito para pagar, pelo contrário
                              // os doadores recebem o dinheiro de volta
    string urlDaCampanha;
    byte constant versao = 1;

    // Estruturas de dados
    enum Estado {
        LevantandoFundos,
        RefundoExpirado,
        Sucesso
    }
    struct Contribuicao {
        uint quantidade;
        address contribuidor;
    }

    // Variáveis de Estado
    State public estado = Estado.LevantandoFundos; // incializado na criação
    uint public totalLevantado;
    uint public levantadoPor;
    uint public completadoEm;
    Contribution[] contribuidores;

    event LogRecebimentoDeFundos(address endereco,
                                 uint quantidade,
                                 uint totalAtual);
    event LogFundosPagos(address enderecoDoRecebedor);

    modifier noEstado(Estado _estado) {
        if (estado != _estado) throw;
        _
    }

    modifier eOCriador() {
        if (msg.sender != criador) throw;
        _
    }

    // Aguarda 6 meses após o final do contrato para destruí-lo
    modifier noFimDoContrato() {
    if(!((estado == Estado.RefundoExpirado || estado == Estado.Sucesso) &&
        completadoEm + 6 months < now)) {
            throw;
        }
        _
    }

    function FundadorDoCrowdFunding(
        uint tempoEmHorasParaFundraising,
        string _urlDaCampanha,
        address _recipiente,
        uint _minALevantar)
    {
        criador = msg.sender;
        recipiente = _recipiente;
        urlDaCampanha = _urlDaCampanha;
        minALevantar = _minALevantar;
        levantadoPor = now + (tempoEmHorasParaFundraising * 1 hours);
    }

    function contribuir()
    public
    noEstado(Estado.LevantandoFundos)
    {
        contribuidores.push(
            Contribuicao({
                quantidade: msg.value,
                contribuidor: msg.sender
            }) // use array, para podermos iterar
        );
        totalLevantado += msg.value;

        LogRecebimentoDeFundos(msg.sender, msg.value, totalRaised);

        verifiqueSeLevantamentoFoiCompletadoOuExpirado();
        return contribuicoes.length - 1; // retorna id
    }

    function verifiqueSeLevantamentoFoiCompletadoOuExpirado() {
        if (totalLevantado > minALevantar) {
            estado = Estado.Sucesso;
            pagar();

            // pode incentivar enviador que iniciou a mudanção de estado
        } else if ( now > levantadoPor )  {
            estado = Estado.RefundoExpirado; // backers podem coletar
                                             // o fundo chamando receberRefundo(id)
        }
        completadoEm = now;
    }

    function pagar()
    public
    emEstado(Estado.Sucesso)
    {
        if(!recipiente.send(this.balance)) {
            throw;
        }


        LogFundosPagos(fundRecipient);
    }

    function receberRefundo(id)
    public
    emEstado(Estado.RefundoExpirado)
    {
        if (contribuicoes.length <= id || id < 0 || contribuicoes[id].amount == 0 ) {
            throw;
        }

        uint quantidadeDeRefundo = contribuicoes[id].amount;
        contribuicoes[id].amount = 0;

        if(!contribuicoes[id].contribuidor.send(quantidadeParaEnviar)) {
            contribuicoes[id].amount = quantidadeParaEnviar;
            return false;
        }

      return true;
    }

    function removerContrato()
    public
    eOCriador()
    noFimDoContrato()
    {
        selfdestruct(msg.sender);
        // criador recebe todo o dinheiro restante{

    }

    function () { throw; }
}
// ** FIM DO EXEMPLO **

// 10. OUTRAS FUNÇÕES NATIVAS

// Unidades monetárias
// Moeda é definida usando wei, menor quantidade de ether
uint quantidadeMin = 1 wei;
uint a = 1 finney; // 1 ether == 1000 finney
// Para outras unidades, veja: http://ether.fund/tool/converter

// Unidades temporais
1 == 1 second // segundos
1 minutes == 60 seconds // Minutos

// Pode multiplicar uma variável de tempo, dado que unidades não são guardadas
// na variável
uint x = 5;
(x * 1 days); // 5 dias

// Cuidado com o salto de segundos / anos com declarações de igualdade para o tempo
// (em vez disso, prefira maior que / menor que)

// Criptografia
// Todas as string passadas são concatenadas antes de realizar hashing
sha3("ab", "cd");
ripemd160("abc");
sha256("def");

// 11. Segurança

// Bugs são desastrosos para contratos Ethereum - até padrões Solidity populares
// podem ser considerados anti-padrões

// Veja links para segurança no final deste documento

// 12. FUNÇÕES DE BAIXO NÍVELS
// call - baixo nível, geralmente não usada, não tem segurança de tipos
booleanSucesso = algumEnderecoDeContrato.call('nome_da_funcao', 'arg1', 'arg2');

// callcode - Código no endereço alvo executado no *contexto* do contrato
// de chamada. Fornece funcionalidade de biblioteca
algumEnderecoDeContrato.callcode('nome_da_funcao');


// 13. NOTAS DE ESTILO
// Baseado no guia de estilo PEP8 do Python

// Resumo rápido:
// 4 espaços para identação
// Duas linhas separam declaração de contratos (e outras declarações de alto nível)
// Evite espaços estranhos entre parênteses
// Pode omitir chaves curly para uma declaração de linha(if, for, etc)
// else deve ser colocado na própria linha


// 14. COMENTÁRIOS NATSPEC
// usado para documentação, comentários e UIs externos

// Contrato natspec - sempre acima da definição do contrato
/// @title Título do Contrato
/// @author Nome do autor

// Função natspec
/// @notice informações sobre o que funciona; mostrado quando a função é executada
/// @dev Documentação de função para desenvolvedor

// Parâmetro de função / valor de retorno  natspec
/// @param algumParametro Alguma descrição do que o parametro faz
/// @return Descrição do valor de retorno
```

## Recursos adicionais
- [Documetanção Solidity](https://solidity.readthedocs.org/en/latest/)
- [Guia de Estilo do Solidity](https://ethereum.github.io/solidity//docs/style-guide/):
 O guia de estilo Ethereum é derivado do guia de estilo do Python [pep8](https://www.python.org/dev/peps/pep-0008/).
- [Editor de Browser Solidity](http://chriseth.github.io/browser-solidity/)
- [Gitter Solidity Chat room](https://gitter.im/ethereum/solidity)
- [Estratégias de projeto modular para contratos Ethereum](https://docs.erisindustries.com/tutorials/solidity/)

## Contratos de Exemplo
- [Dapp Bin](https://github.com/ethereum/dapp-bin)
- [Solidity Baby Step Contracts](https://github.com/fivedogit/solidity-baby-steps/tree/master/contracts)
- [ConsenSys Contracts](https://github.com/ConsenSys/dapp-store-contracts)
- [State of Dapps](http://dapps.ethercasts.com/)

## Segurança
- [Thinking About Smart Contract Security](https://blog.ethereum.org/2016/06/19/thinking-smart-contract-security/)
- [Smart Contract Security](https://blog.ethereum.org/2016/06/10/smart-contract-security/)
- [Hacking Distributed Blog](http://hackingdistributed.com/)

## Informação excluída intencionalmente
- Libraries

## Estilo
- [PEP8](https://www.python.org/dev/peps/pep-0008/) é usado como guia de estilo,
 incluindo sua filosofia geral

## Editores
- [Vim Solidity](https://github.com/tomlion/vim-solidity)
- Snippets de Editores ([Ultisnips format](https://gist.github.com/nemild/98343ce6b16b747788bc))

## Trabalhos Futuros
- Novas palavras-chave: protected, inheritable
- Lista de padrões de design comuns (throttling, RNG, atualização de versão)
- Padrões anti-segurança comuns


Sinta-se a vontade para enviar um pull request com quaisquer edições - ou email
para nemild - / at- / gmail
