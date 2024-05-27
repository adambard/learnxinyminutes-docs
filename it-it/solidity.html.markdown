---
language: Solidity
filename: learnSolidity-it.sol
contributors:
  - ["Nemil Dalal", "https://www.nemil.com"]
  - ["Joseph Chow", ""]
  - ["Bhoomtawath Plinsut", "https://github.com/varshard"]
  - ["Shooter", "https://github.com/liushooter"]
  - ["Patrick Collins", "https://gist.github.com/PatrickAlphaC"]
translators:
    - ["Al", "http://github.com/al-ias"]
lang: it-it
---

Solidity permette di programmare su [Ethereum](https://www.ethereum.org/), una
macchina virtuale basata sulla blockchain che consente la creazione e
l'esecuzione degli smart contract senza che sia richiesta centralizzazione o
fiducia negli attori coinvolti.

Solidity è un linguaggio di programmazione di contratti tipizzato staticamente e
ha molte cose in comune con Javascript e C. Come per gli oggetti nella
programmazione ad oggetti, ogni contratto contiene variabili di stato, funzioni
e tipi di dato semplici. Tra le funzionalità specifiche dei contratti troviamo
le clausole (guardie) dei modifier, gli event notifier per i listener, e le
variabili globali custom.

Come esempi di contratti su Ethereum troviamo sistemi di crowdfunding, voto,
[finanza decentralizzata](https://defipulse.com/) e aste al buio.

Compiere errori nel codice Solidity può portare a rischi e costi alti, quindi
bisogna fare attenzione a testare e rilasciare le modifiche lentamente. A
CAUSA DEI CONTINUI CAMBIAMENTI DI ETHEREUM È IMPROBABILE CHE QUESTO DOCUMENTO
RESTI AGGIORNATO, QUINDI COSNIGLIAMO DI SEGUIRE LA CHAT ROOM DI SOLIDITY E IL
BLOG DI ETHEREUM PER TENERSI AGGIORNATI. TUTTO IL CODICE QUI PRESENTE E' FORNITO
COSÌ COM'È, CON ANNESSI RISCHI SOSTANZIALI DI ERRORI O PATTERN DI PROGRAMMAZIONE
DEPRECATI. 

A differenza di altri linguaggi, potresti aver bisogno di usare pattern di
pausing, deprecation e throttling usage per ridurre il rischio. Questo documento
tratta principalmene la sintassi e quindi esclude molti design pattern in voga.

Visto che Solidity e Ethereum sono in continuo sviluppo, le funzionalità
sperimentali o beta sono evidenziate e soggette a cambiamenti. Ogni Pull Request
è ben accetta.


# Lavorare con Remix e Metamask

Uno dei modi più semplici di scrivere, distribuire e testare il codice Solidity
è usare :

1. [L'ambiente di sviluppo online Remix](https://remix.ethereum.org/) 
2. [Il wallet Metamask](https://metamask.io/).

Per cominciare, [scarichiamo l'estesione per browser di Metamask](https://metamask.io/). 

Una volta installata, potremo iniziare ad usare Remix. Il codice seguente è
pre-inizializzato, ma prima di addentrarci, diamo un'occhiata a qualche
trucchetto per iniziare ad usare Remix. Carica tutto il necessario [clickando su questo link](https://remix.ethereum.org/#version=soljson-v0.6.6+commit.6c089d02.js&optimize=false&evmVersion=null&gist=f490c0d51141dd0515244db40bbd0c17&runs=200).

1. Scegli il compilatore per Solidity

![Solidity-in-remix](../images/solidity/remix-solidity.png)

2. Apri il file che si caricherà su quel link

![Solidity-choose-file](../images/solidity/remix-choose-file.png)

3. Compila il file

![Solidity-compile](../images/solidity/remix-compile.png)

4. Fai il deploy

![Solidity-deploy](../images/solidity/remix-deploy.png)

5. Smanetta con i contratti

![Solidity-deploy](../images/solidity/remix-interact.png)

Hai distribuito il tuo primo contratto! Congratulazioni!

Potrai testarlo e smanettare con le funzioni già definite. Dai un'occhiata ai
commenti per scoprire cosa fanno.


## Lavorare su una testnet

Distribuire e testare su una testnet è il modo più accurato per mettere alla
prova i tuoi smart contract in Solidity. Per farlo procuriamoci prima degli ETH
di test dalla testnet Kovan.

[Entra in questo Gitter Channel](https://gitter.im/kovan-testnet/faucet) e
scrivici l'indirizzo del tuo wallet Metamask.

Sul tuo Metamask, dovrai cambiare la testnet in `Kovan`.

![Solidity-in-remix](../images/solidity/metamask-kovan.png)

Riceverai degli Ethereum di test gratuiti. Abbiamo bisogno di Ethereum per
distribuire degli smart contract su una testnet.

Nell'esempio precedente non avevamo usato una testnet, ma avevamo distribuito
su un ambiente virtuale fittizio. Quando si lavora su una testnet, possiamo
davvero monitorare e interagire con i nostri contratti in maniera persistente.

Per distribuire su una testnet, allo step `#4 Fai il deploy`, cambia
l'`environment` selezionato in `injected web3`. In questo modo verrà usato
come network su cui fare il deploy qualsiasi network selezionato sul tuo
Metamask.

![Solidity-in-remix](../images/solidity/remix-testnet.png)

Per ora continua a usare la `Javascript VM` a meno che non ti sia detto di
cambiarla. Quando distribuisci su una testnet, Metamask aprirà un pop up che
ti chiederà di "confermare" la transazione. Premi `yes` e dopo un certo lasso
di tempo, ti apparirà la stessa interfaccia per il contratto nella parte
inferiore dello schermo.


```solidity
// Iniziamo con un semplice contratto su una Banca
// Permette di depositare, prelevare e fare l'estratto conto

// simple_bank.sol (nota l'estensione .sol)
/* **** INIZIO DELL'ESEMPIO **** */

// Dichiara la versione del compilatore per il file sorgente
pragma solidity ^0.6.6;

// Inizia con il commento Natspec (i tre slash)
// viene usato per la documentazione - e per i dati descrittivi per gli elementi
// dell'interfaccia utente / azioni

/// @title SimpleBank
/// @author nemild

/* 'contract' somiglia a 'class' in altri linguaggi (ha variabili di classe,
ereditarietà, etc.) */
contract SimpleBank { // CapWords
    // Dichiariamo le variabili di stato fuori dalle funzioni, persisteranno
    // durante tutta la vita del contratto

    // i dizionari mappano gli indirizzi con i saldi
    // fai sempre attenzione agli overflow attack che sfruttano i numeri
    mapping (address => uint) private balances;

    // "private" significa che che altri contratti non possono leggere i
    // saldi ma le informazioni restano visibili ad altri attori sulla blockchain

    address public owner;
    // 'public' lo rende leggibile dall'esterno (ma non modificabile) dagli
    // utenti e dai contratti

    // Gli 'event' pubblicano le azioni in modo che siano ascoltabili da
    // listener esterni
    event LogDepositMade(address accountAddress, uint amount);

    // I 'constructor' possono ricevere uno o più parametri; Si può
    // dichiarare un solo costruttore
    constructor() public {
        // 'msg' fornisce i dettagli sul messaggio che è stato mandato al contratto
        // 'msg.sender' è chi invoca il contratto (l'indirizzo di chi lo crea)
        owner = msg.sender;
    }

    /// @notice Deposita ether nella banca
    /// @return Il saldo dell'utente dopo che è stato effettualto il deposito
    function deposit() public payable returns (uint) {
        // Usiamo 'require' per testare gli input dell'utente, 'assert' per gli
        // invarianti interni. Qui ci assicuriamo di non avere a che fare con
        // un overflow
        require((balances[msg.sender] + msg.value) >= balances[msg.sender]);

        balances[msg.sender] += msg.value;
        // Non servono "this." o "self." con le variabili di stato
        // Tutti i valori iniziali delle variabili sono impostati automaticamente
        // al valore di default per quel tipo di dato

        emit LogDepositMade(msg.sender, msg.value); // Fa scattare l'evento

        return balances[msg.sender];
    }

    /// @notice Preleva ether dalla banca
    /// @dev Non restituisce gli ether inviati in eccesso
    /// @param withdrawAmount L'importo che si vuole ritirare
    /// @return remainingBal
    function withdraw(uint withdrawAmount) public returns (uint remainingBal) {
        require(withdrawAmount <= balances[msg.sender]);

        // Notiamo come per prima cosa scaliamo i soldi dal saldo, prima di
        // invarli. Ogni .transfer/.send in questo contratto può chiamare una 
        // funzione esterna. Questa cosa potrebbe permettere a chi invoca la
        // funzione di richiedere un importo maggiore del suo saldo usando
        // una chiamata ricorsiva. Miriamo ad aggiornare lo stato prima che sia
        // chiamata una funzione esterna, incluse .transfer/.send
        balances[msg.sender] -= withdrawAmount;

        // Qui lancia automaticamente un errore se fallisce, il che implica
        // che il saldo (non più aggiornato) viene ripristinato a prima della
        // transazione
        msg.sender.transfer(withdrawAmount);

        return balances[msg.sender];
    }

    /// @notice Recupera il saldo
    /// @return Il saldo dell'utente
    // 'view' (ex: constant) impedisce alle funzioni di modificare lo stato
    // delle variabili; consente alle le funzioni di essere disponibili in
    // locale/fuori dalla blockchain
    function balance() view public returns (uint) {
        return balances[msg.sender];
    }
}
// ** FINE DELL'ESEMPIO **


// Passiamo alle basi di Solidity

// 1. TIPI DI DATO E I LORO METODI
// uint viene usato per gli importi in valuta (non ci sono double o float)
// e per le date (in unix time)
uint x;

// int di 256 bit, non possono essere modificati dopo l'istanziazione
int constant a = 8;
int256 constant a = 8; // stesso effetto della riga prima, qui viene
// dichiarato esplicitamente che è di 256 bit
uint constant VERSION_ID = 0x123A1; // Una costante esadecimale
// con 'constant', il compilatore rimpiazza ogni occorrenza con il valore

// Tutte le variabili di stato (quelle fuori da una funzione)
// sono 'interne' di default e accessibili SOLO dall'interno del contratto 
// e da tutti contratti che le ereditano
// Bisogna usare esplicitamente 'public' per consentire l'accesso dai contratti 
// esterni
int256 public a = 8;

// Per int e uint possiamo esplicitamente assegnare una dimensione tra 8 e 256
// es. int8, int16, int24
uint8 b;
int64 c;
uint248 e;

// Attenzione a non andare in overflow e a proteggersi dagli attacchi che lo fanno
// Ad esempio per quanto rigrada l'addizione, conviene fare:
uint256 c = a + b;
assert(c >= a); // 'assert' testa gli invarianti interni; require viene usato 
// per gli input
// Per altri esempi di problemi comuni con le operazioni aritmentiche, dai una
// occhiata alla Zeppelin's SafeMath library
// https://github.com/OpenZeppelin/zeppelin-solidity/blob/master/contracts/math/SafeMath.sol


// Non ci sono funzioni random built-in, puoi ottenere un numero pseudo-casuale
// hashando l'ultimo blockhash, o ottenere un numero realmente casuale usando
// qualcosa come Chainlink VRF. 
// https://docs.chain.link/docs/get-a-random-number

// Conversione di tipo
int x = int(b);

bool b = true; // oppure 'var b = true;' per l'inferenza di tipo

// Indirizzi - contengono indirizzi Ethereum di 20 byte/160 bit
// Non sono consentite operazioni aritmetiche
address public owner;

// Tipi di account:
// Contract account: l'indirizzo viene impostato quando lo si crea (funzione con
// l'indirzzo di chi lo crea, il numero della transazione inviata)
// External Account: (persona/enitità esterna): l'indirizzo viene creato dalla
// chiave pubblica

// Aggiungi il campo 'public' per indicare che è pubblico/accessibile dall'esterno
// un getter viene creato automaticamente, ma NON un setter

// Si possono mandare ether a tutti gli indirizzi
owner.transfer(SOME_BALANCE); // fallisce e, in tal caso, ripristina
// lo stato precedente

// Possiamo anche usare la funzione di livello più basso .send, che restituisce
// false se fallisce
if (owner.send) {} // RICORDA: metti la send in un 'if' dato che gli indirizzi
// usati nei contratti hanno delle funzioni, che vengono eseguite quando viene
// fatta una send, che possono fallire.
// Inoltre fai attenzione a scalare i saldi PRIMA di provare a fare una send,
// dato il rischio di chiamate riscorsive che potrebbero prosciugare il contratto

// Possiamo controllare il saldo
owner.balance; // il saldo del propietario (utente o contratto)


// I Byte sono disposibili in dimensioni da 1 a 32
byte a; // 'byte' è la stessa cosa di 'bytes1'
bytes2 b;
bytes32 c;

// Byte con dimensione dinamica
bytes m; // Un array particolare, la stessa cosa dell'array 'byte[]' (ma scritto stringato)
// È più dispendioso di byte1-byte32, che di solito sono preferibili

// come bytes, ma non permette di accedere alla lunghezza o all'indice (per ora)
string n = "hello"; // salvato in UTF8, nota i doppi apici, non singoli
// le utility function per le stringhe saranno aggiunte in futuro
// sono preferibili bytes32/bytes, dato che UTF8 occupa più memoria

// Inferenza di tipo
// 'var' fa inferenza di tipo a seconda del primo assegnamento,
// non può essere usata tra i parametri di una funzione
var a = true;
// da usare con cautela, può inferire un tipo errato
// es. un int8 quando un contatore dev'essere un int16

// var può essere usata per assegnare una funzione ad una variabile
function a(uint x) returns (uint) {
    return x * 2;
}
var f = a;
f(22); // chiamata

// di default, tutte le variabili sono impostate a 0 durante l'istanziazione


// Delete può essere chiamato sulla maggior parte dei valori
// (NON distrugge il valore, ma lo setta a 0, il valore did default)
uint x = 5;


// Destructuring/Tuple
(x, y) = (2, 7); // assegna/scambia più valori


// 2. STRUTTURE DATI
// Array
bytes32[5] nicknames; // array statico
bytes32[] names; // array dinamico
uint newLength = names.push("John"); // aggiungere un elemento restituisce
// la nuova dimensione dell'array
// Dimesione
names.length; // ottenere la dimensione
names.length = 1; // la dimensione può essere assegnata (solo per gli array nello storage)

// array multidimensionali
uint[][5] x; // array con 5 array dinamici (ordine opposto rispetto ad
// altri linguaggi)

// Dizionari (da un tipo qualsiasi a un tipo qualsiasi)
mapping (string => uint) public balances;
balances["charles"] = 1;
// il risultato balances["ada"] è 0, tutte le chiavi non settate 
// restituiscono zero
// 'public' permette che si possa fare questo da un altro contratto:
contractName.balances("charles"); // restituisce 1
// 'public' ha creato getter (ma non un setter), come il seguente:
function balances(string _account) returns (uint balance) {
    return balances[_account];
}

//  Mapping annidati
mapping (address => mapping (address => uint)) public custodians;

// Fare una delete
delete balances["John"];
delete balances; // assegna 0 a tutti gli elementi

// Diversamente da altri linguaggi NON si può iterare tra gli elementi di un
// mapping senza conoscere le chiavi - ma si può costruire una struttura dati a monte
// che lo faccia

// Strutture dati
struct Bank {
    address owner;
    uint balance;
}
Bank b = Bank({
    owner: msg.sender,
    balance: 5
});
// oppure
Bank c = Bank(msg.sender, 5);

c.balance = 5; // imposta ad un nuovo valore
delete b;
// reimposta, imposta tutte le variabili della struttura a 0, tranne i mapping

// Enumerazioni
enum State { Created, Locked, Inactive }; // di solito si usano per gli automi a stati finiti
State public state; // Dichiara una variabile da un enum
state = State.Created;
// Le enum possono essere convertite esplicitamente in int
uint createdState = uint(State.Created); //  0

// Data location: Memory vs. storage vs. calldata - tutti i tipi complessi
// (array, struct) hanno una data location
// 'memory' non è persistente, 'storage' sì
// Il default è 'storage' per varibili locali e di stato; 
// 'memory' per i parametri delle funzioni
// Lo stack può contenere poche varaibili locali

// Per la maggior parte dei tipi, si può impostare esplicitamente
// quale data location usare


// 3. Operatori semplici
// Ci sono operatori logici, a bit e aritmetici
// Potenza: **
// Or esclusivo: ^
// Negazione bitwise: ~


// 4. Variabili globali degne di nota
// ** this **
this; // indirizzo del contratto
// di solito si usa per trasferire altrove il saldo rimanente 
// al termine della vita del contratto 
this.balance;
this.someFunction(); // invoca una funzione esterna tramite chiamata,
// non attraverso un salto interno

// ** msg - Il messaggio corrente ricevuto dal contratto **
msg.sender; // indirizzo di chi ha inviato msg
msg.value; // l'importo di ether forniti a questo contratto espresso in "wei",
// la funzione dovrebbe essere marcata come "payable"
msg.data; // in bytes, tutti gli argomenti del messaggio
msg.gas; // 'gas' restante

// ** tx - Questa transazione **
tx.origin; // l'indirizzo di chi ha avviato questa transazione
tx.gasprice; // il prezzo del "gas" per la transazione

// ** block - Informazioni sul blocco attuale **
now; // ora corrente (approssimatamente), alias di block.timestamp (in Unix time)
// Da notare come può essere manipolata dai miner, quindi da usare con cautela

block.number; // numero del blocco attuale
block.difficulty; // difficulty del blocco attuale
block.blockhash(1); // restituisce un bytes32, funziona solo per i 256 blocchi
// più recenti
block.gasLimit();

// ** storage - Memoria persistente (in hash) **
storage['abc'] = 'def'; // mappa da parole di 256 bit a parole di 256 bit


// 4. FUNZIONI E ALTRO
// A. Funzioni
// Una semplice funzione
function increment(uint x) returns (uint) {
    x += 1;
    return x;
}

// Le funzioni possono restituire molti valori,
// e visto che i valori di ritorno vengono dichiarati prima
// non è richiesta un'instruzione return esplicita
function increment(uint x, uint y) returns (uint x, uint y) {
    x += 1;
    y += 1;
}
// Chiama la funzione di cui sopra
uint (a,b) = increment(1,1);

// 'view' (un alias di 'constant')
// indica che la funzione non cambia / non può cambiare le variabili persistenti
// Le funzioni definite con view vengono eseguite localmente, non sulla blockchain
// N.B. la keyword constant sarà presto deprecata
uint y = 1;

function increment(uint x) view returns (uint x) {
    x += 1;
    y += 1; // questa riga fallirebbe
    // y è una variabile di stato, e non può essere cambiata in una funzione di view
}

// 'pure' è più restrittivo di 'view' o 'constant', e non
// permette nemmeno di leggere le varaibili di stato
// In realtà è più complicato, per approfondire su
// view/pure:
// http://solidity.readthedocs.io/en/develop/contracts.html#view-functions

// Modificatori di visibilità per le funzioni
// Possono essere messi vicino a 'view' e includono:
// public - visibile esternamente e internamente (di default per function)
// external - visible solo esternamente (comprese le chiamate fatte con this.)
// private - visibile solo dal contratto attuale
// internal - visibile solo dal contratto attuale, e da quelli che ne derivano

// Di solito è una buona idea marcare esplicitamente ogni funzione

// Le funzioni sono hoisted e si può assegnare una funzione ad una variabile
function a() {
    var z = b;
    b();
}

function b() {

}

// Tutte le funzioni che ricevono ether devono essere dichiarate come 'payable'
function depositEther() public payable {
    balances[msg.sender] += msg.value;
}


// I cicli sono da preferire alla ricorsione
// (la profondità massima dello stack è 1024)
// Inoltre, non impostare dei loop senza limiti,
// perchè potresti raggiungere il limite per il gas

// B. Eventi
// Gli eventi notificano a terze parti; è facile ispezionare e
// accedere agli eventi al di fuori della blockchain (con client leggeri);
// Tipicamente si dichiarano dopo i parametri del contratto

// Tipicamente, sono capitalized - si usa Log come prefisso per esplicitarli
// meglio ed evitare che si confondano con una chiamata a funzione

// Dichiarazione
event LogSent(address indexed from, address indexed to, uint amount); 
// Da notare le prime lettere maiuscole

// Chiamata
LogSent(from, to, amount);

/**

Una terza parte esterna (entità o contratto), può osservare usando
la libreria Javascript Web3:

// Quel che se segue è codice Javascript, non Solidity
Coin.LogSent().watch({}, '', function(error, result) {
    if (!error) {
        console.log("Trasferimento valuta: " + result.args.amount +
            " la valuta è stata mandata da " + result.args.from +
            " a " + result.args.to + ".");
        console.log("I saldi ora sono:\n" +
            "Mittente: " + Coin.balances.call(result.args.from) +
            "Destinatario: " + Coin.balances.call(result.args.to));
    }
}
**/

// È prassi che un contratto dipenda da un altro (es. che dipenda
// dai tassi di cambio forniti da un altro contratto)

// C. Modifier
// I modifier validano gli input per conto dele funzioni verificando ad esempio
// il saldo minimo o l'autenticazione dell'utente;
// sono simili alle calusole di guardia di altri linguaggi

// '_' (underscore) viene spesso posizionato nell'ultima riga del body, e indica
// che la funzione chiamata dev'essere posizionata lì
modifier onlyAfter(uint _time) { require (now >= _time); _; }
modifier onlyOwner { require(msg.sender == owner) _; }
// usate comunemente negli automi a stati finiti
modifier onlyIfStateA (State currState) { require(currState == State.A) _; }

// Si dichiarano appena dopo la definizione di una funzione
function changeOwner(newOwner)
onlyAfter(someTime)
onlyOwner()
onlyIfState(State.A)
{
    owner = newOwner;
}

// L'underscore può essere messo prima della fine del body,
// ma un'istruzione di ritorno esplicita lo salterebbe,
// quindi è da usare con cautela
modifier checkValue(uint amount) {
    _;
    if (msg.value > amount) {
        uint amountToRefund = amount - msg.value;
        msg.sender.transfer(amountToRefund);
    }
}


// 6. ISTRUZIONI CONDIZIONALI E CICLI

// Troviamo tutte le istruzioni condizionali di base - incluse if/else, for,
// while, break, continue e return - ma non c'è lo switch

// La sintassi è la stessa di javascript, ma non esiste la conversione di tipo
// in booleano dai non booleani (bisogna usare gli operatori logici per
// ottenere il valore boolean)

// Bisogna stare attenti i loop che iterano in base al comportamento
// dell'utente, dato che i contratti hanno un tetto massimo di gas
// per blocco di codice e falliranno se lo superano
// Ad esempio:
for(uint x = 0; x < refundAddressList.length; x++) {
    refundAddressList[x].transfer(SOME_AMOUNT);
}

// Ci sono due errori nel codice precedente:
// 1. Un fallimento su una transfer impedisce al loop di completare tutti
// i cicli, bloccando dei soldi;
// 2. Questo loop potrebbe essere arbitrariamente lungo (si basa sul numero
// degli utenti che hanno diritto al rimborso), quindi potrebbe fallire sempre
// se supera il tetto massimo di gas per blocco;
// Come soluzione, si permette agli utenti di prelevare
// individualmente dal loro subaccount e segnare il rimborso come riscosso
// Ad es. preferire pull payments ai push payment


// 7. OGGETTI/CONTRATTI

// A. Invocare un contratto esterno
contract InfoFeed {
    function info() payable returns (uint ret)  { return 42; }
}

contract Consumer {
    InfoFeed feed; // punta ad un contratto sulla blockchain

    // Imposta il feed sull'istanza del contratto esistente
    function setFeed(address addr) {
        // fare attenzione alla conversione di tipo automatica;
        // il costruttore non viene invocato
        feed = InfoFeed(addr);
    }

    // Imposta il feed ad una nuova istanza del contratto
    function createNewFeed() {
        feed = new InfoFeed(); // viene creata una nuova istanza;
        // viene invocato il costruttore
    }

    function callFeed() {
        // le parentesi finali invocano il contratto, opzionalmente si può
        // specificare un importo custom di ether o di gas
        feed.info.value(10).gas(800)();
    }
}

// B. ereditarietà

// Conta l'ordine, l'ultimo contratto ereditato (es. 'def') può andare
// in overriding su parti dei contratti precedentemente ereditati
contract MyContract is abc, def("a custom argument to def") {

// Funzione in overriding
    function z() {
        if (msg.sender == owner) {
            def.z(); // invoca la funzione overridden da def
            super.z(); // chiama la funzione overridden del padre
        }
    }
}

// Funzioni astratte
function someAbstractFunction(uint x);
// non possono essere compilate, vengono usate nei contratti base/astratti
// e poi verranno implementate

// C. Import

import "filename";
import "github.com/ethereum/dapp-bin/library/iterable_mapping.sol";


// 8. ALTRE KEYWORD

// A. Selfdestruct
// autodistrugge il contratto corrente, inviando i fondi ad un indirizzo
// (di solito il creatore)
selfdestruct(SOME_ADDRESS);

// rimuove il codice e quanto in memoria dal blocco corrente e da tutti i blocchi futuri
// aiuta ad alleggerire i client, ma le informazioni precedenti continueranno
// a persistere sulla blockchain

// È un pattern comune, permette al proprietario di terminare il contratto
// e ricevere i fondi rimasti
function remove() {
    if(msg.sender == creator) { // Solo il creatore del contratto può farlo
        selfdestruct(creator); // Cessa l'attività del contratto, trasferisce i fondi
    }
}

// Si potrebbe voler disattivare il contratto manualmente, anzichè usare una
// selfdestruct (gli ether inviati ad un contratto dopo una selfdestruct
// vengono persi)


// 9. NOTE SUL DESIGN DEI CONTRATTI

// A. Offruscamento
// Tutte le variabili sono pubblicamente visibili sulla blockchain, quindi 
// qualsiasi informazione privata ha bisogno di essere offruscata (es. hash con una
// chiave segreta)

// Passi: 1. Impegnarsi pagare una certa cifra, 2. Rivelare l'impegno preso
keccak256("una_puntata_d_asta", "un segreto"); // impegno

// in futuro, l'invocazione della funzione rivelatrice del contratto
// mostrerà la puntata con il segreto che produce lo SHA3
reveal(100, "ilMioSegreto");

// B. Ottimizzazione della memoria (storage)
// Scrivere dati sulla blockchain può essere costoso visto che vengono 
// conservati per sempre; siamo incoraggati ad usare la memoria in maniera
// scaltra (un giorno la compilazione migliorerà, ma per ora è vantaggioso
// pianificare le strutture dati da usare - e conservarne il minimo possibile
// sulla blockchain)

// I costi per conservare cose come array multidimensionali sono spesso alti
// (costa conservare dati - non dichiarare variabili parzialmente vuote)

// C. Accesso ai dati sulla blockchain
// Non si può impedire alle persone o ai computer di leggere il contenuto
// o lo stato delle transazioni

// Anche se 'private' non permette agli altri *contratti* di leggere alcune
// informazioni direttamente, qualsiasi altro attore può leggerle
// sulla blockchain

// Tutti i dati, dall'inizio, vegono conservati sulla blockchain e
// tutti possono accedere alle informazioni passate e ai cambiamenti futuri

// D. Oracle e dati esterni
// Gli oracle consentono di interagire con i tuoi smart contract
// al di fuori della blockchain. 
// Vengono usati per ricevere informazioni dal mondo reale, mandare
// richieste post al mondo reale o vice versa.

// Anche le implementazioni che sfruttano l'ora vengono fatte attraverso
// gli oracle, visto che i contratti devono essere chiamati direttamente e
// non possono fare una "subscribe" a un certo orario. 
// Data la decentralizzazione degli smart contract, vorrai ricevere informazioni
// in maniera decentralizzata, altrimenti rischi di ricreare l'accentramento
// che la progettazione degli smart contract si prefigge di prevenire.

// Il modo migliore di ottenere e usare dati decentralizzati già pronti
// è attraverso i Chainlink Data Feeds
// https://docs.chain.link/docs/get-the-latest-price
// Possiamo fare riferimento a certe informazioni della blockchain
// che sono già state aggregate da più fonti e ridistribuite on-chain,
// usandole come "banche dati" di fonti di informazione. 

// Puoi vedere altri esempi che effettuano chiamate alle API qui:
// https://docs.chain.link/docs/make-a-http-get-request

// E ovviamente puoi costruire la tua rete di orace, ma assicurati di sapere
// quant'è accentrata o decentralizzata la tua applicazione. 

// Mettere su una rete di oracle per conto tuo

// E. Cron Job
// I contratti devono essere chiamati manualmente per gestire lo scheduling
// in base all'orario; si può creare un codice esterno che li pinghi reglarmente
//  oppure fornire degli incentivi (ether) a qualcun'altro che lo faccia

// F. Pattern Observer
// Un pattern observer permette di iscriversi come osservatore e
// registrare una funzione che verrà chiamata dall'oracle
// (N.B. l'oracolo paga perchè sia eseguita quest'azione)
// Ci sono alcune somoglianze nella registrazione con Pub/sub

// Questo è un contratto astratto che importano sia il client che il server
// Il client dovrebbe implementarlo
contract SomeOracleCallback {
    function oracleCallback(int _value, uint _time, bytes32 info) external;
}

contract SomeOracle {
    SomeOracleCallback[] callbacks; // array di tutti gli osservatori iscritti

    // Osservatori iscritti
    function addSubscriber(SomeOracleCallback a) {
        callbacks.push(a);
    }

    function notify(value, time, info) private {
        for(uint i = 0;i < callbacks.length; i++) {
            // tutti gli osservatori iscritti dovranno implementare la oracleCallback
            callbacks[i].oracleCallback(value, time, info);
        }
    }

    function doSomething() public {
        // Codice che fa qualcosa

        // Notifica a tutti gli iscritti
        notify(_value, _time, _info);
    }
}

// Il contratto client può aggiungersi agli iscritti (con addSubscriber) 
// del contratto SomeOracle, importando SomeOracleCallback

// G. Automi a stati finiti
// vedi l'esempio sotto che usa enum per lo stato e il modifier inState
```

Prova l'esempio completo qui sotto [usando remix e la `Javascript VM`](https://remix.ethereum.org/#version=soljson-v0.6.6+commit.6c089d02.js&optimize=false&evmVersion=null&gist=3d12cd503dcedfcdd715ef61f786be0b&runs=200)

```solidity
// *** ESEMPIO: Un esempio di crowdfunding (molto simile a Kickstarter) ***
// ** START EXAMPLE **

// CrowdFunder.sol
pragma solidity ^0.6.6;

/// @title CrowdFunder
/// @author nemild
contract CrowdFunder {
    // Variabili impostate alla creazione dal creatore
    address public creator;
    address payable public fundRecipient; // il creatore può essere diverso
    // da chi riceve i fondi, che dev'essere payable
    uint public minimumToRaise; // è richiesto per chiedere il finanziamento,
    // altrimenti tutti ricevono un rimborso
    string campaignUrl;
    byte version = "1";

    // Strutture dati
    enum State {
        Fundraising,
        ExpiredRefund,
        Successful
    }
    struct Contribution {
        uint amount;
        address payable contributor;
    }

    // Variabili di stato
    State public state = State.Fundraising; // inizializzato alla creazione
    uint public totalRaised;
    uint public raiseBy;
    uint public completeAt;
    Contribution[] contributions;

    event LogFundingReceived(address addr, uint amount, uint currentTotal);
    event LogWinnerPaid(address winnerAddress);

    modifier inState(State _state) {
        require(state == _state);
        _;
    }

    modifier isCreator() {
        require(msg.sender == creator);
        _;
    }

    // Aspetta 24 settimane dopo l'ultimo cambio di stato prima di consentire
    // che in contratto venga distrutto
    modifier atEndOfLifecycle() {
    require(((state == State.ExpiredRefund || state == State.Successful) &&
        completeAt + 24 weeks < now));
        _;
    }

    function crowdFund(
        uint timeInHoursForFundraising,
        string memory _campaignUrl,
        address payable _fundRecipient,
        uint _minimumToRaise)
        public
    {
        creator = msg.sender;
        fundRecipient = _fundRecipient;
        campaignUrl = _campaignUrl;
        minimumToRaise = _minimumToRaise;
        raiseBy = now + (timeInHoursForFundraising * 1 hours);
    }

    function contribute()
    public
    payable
    inState(State.Fundraising)
    returns(uint256 id)
    {
        contributions.push(
            Contribution({
                amount: msg.value,
                contributor: msg.sender
            }) // usiamo un array per iterare
        );
        totalRaised += msg.value;

        emit LogFundingReceived(msg.sender, msg.value, totalRaised);

        checkIfFundingCompleteOrExpired();
        return contributions.length - 1; // restituisce l'id
    }

    function checkIfFundingCompleteOrExpired()
    public
    {
        if (totalRaised > minimumToRaise) {
            state = State.Successful;
            payOut();

            // qui si può incentivare chi ha provocato il cambiamento di stato
        } else if ( now > raiseBy )  {
            state = State.ExpiredRefund; // ora i finanziatori possono avere
            // il rimborso chiamando getRefund(id)
        }
        completeAt = now;
    }

    function payOut()
    public
    inState(State.Successful)
    {
        fundRecipient.transfer(address(this).balance);
        LogWinnerPaid(fundRecipient);
    }

    function getRefund(uint256 id)
    inState(State.ExpiredRefund)
    public
    returns(bool)
    {
        require(contributions.length > id && id >= 0 && contributions[id].amount != 0 );

        uint256 amountToRefund = contributions[id].amount;
        contributions[id].amount = 0;

        contributions[id].contributor.transfer(amountToRefund);

        return true;
    }

    function removeContract()
    public
    isCreator()
    atEndOfLifecycle()
    {
        selfdestruct(msg.sender);
        // il creatore riceve tutti i fondi che non sono stati riscossi
    }
}
// ** END EXAMPLE **
```

Qualche altra funzionalità.

```solidity
// 10. ATRE FUNZIONALITA' NATIVE

// Unità di valuta
// La valuta viene definita partendo dai wei, l'unità più piccola di Ether
uint minAmount = 1 wei;
uint a = 1 finney; // 1 ether == 1000 finney
// Per altre unità, vedi: http://ether.fund/tool/converter

// Unità temporali
1 == 1 second
1 minutes == 60 seconds

// Le unità temporali si possono moltiplicare, visto che non vegono salvate
// nelle variabili
uint x = 5;
(x * 1 days); // 5 giorni

// Attenzione ad usare l'operatore di uguaglianza con i secondi/anni bisestili
// (sono da preferire maggiore/minore di)

// Crittografia
// Tutte le stringhe che vengono passate vengono concatenate prima di
// calcolare l'hash
sha3("ab", "cd");
ripemd160("abc");
sha256("def");


// 11.SICUREZZA

// I bug possono essere disastrosi per i contratti Ethereum e anche 
// i pattern comuni di Solidity potrebbero riverlarsi degli antipattern

// Dai un'occhiata ai link sulla sicurezza alla fine di questo documento


// 12. FUNZIONI DI BASSO LIVELLO
// call - è di basso livello, non viene usata spesso, perchè non è type safe
successBoolean = someContractAddress.call('function_name', 'arg1', 'arg2');

// callcode - Il codice all'indirizzo target viene eseguito *contestualmente*
// alla chiamata del contratto
// fornisce le stesse funzionalità di una libreria
someContractAddress.callcode('function_name');


// 13. NOTE SULLO STILE
// Basate sulla guida allo stile PEP8 di Python
// Guida completa allo stile: http://solidity.readthedocs.io/en/develop/style-guide.html

// Riassunto veloce:
// 4 spazi per l'indentazione
// Due righe per separare la dichiarazione dei contratti
// (e altre dichirazioni top level)
// Evitare spazi ai lati interni delle parentesi tonde
// Si possono omettere le parentesi graffe per statement monolinea (if, for, etc)
// L'else dovrebbe essere posizionato su una riga a se

// 14. COMMENTI NATSPEC
// usati per la documentazione, commenti e UI esterne

// Natspec dei contratti - sempre sopra la definizione del contratto
/// @title Titolo del contratto
/// @author Nome dell'autore

// Natspec delle funzioni
/// @notice informazioni su quel che fa la funzione; mostrate quando la funzione viene eseguita
/// @dev Documentazione della funzione per sviluppatori

// Natspec dei parametri/valori di ritorno delle funzioni
/// @param someParam Una descrizione di quel che fa il parametro
/// @return Descrizione del valore di ritorno
```

## Risorse Aggiuntive
- [Documentazione di Solidity](https://solidity.readthedocs.org/en/latest/)
- [Tutorial Chainlink per Principianti](https://docs.chain.link/docs/beginners-tutorial)
- [Best Practice  per Smart Contract](https://github.com/ConsenSys/smart-contract-best-practices)
- [Superblocks Lab - Ambiente di sviluppo su browser per Solidity](https://lab.superblocks.com/)
- [EthFiddle - Il JsFiddle di Solidity](https://ethfiddle.com/)
- [Solidity Editor su Browser](https://remix.ethereum.org/)
- [Chat Room Gitter su Solidity](https://gitter.im/ethereum/solidity)
- [Stategie di Progettazione Modulare per Contratti Ethereum](https://docs.erisindustries.com/tutorials/solidity/)
- [Documentazione Chainlink](https://docs.chain.link/docs/getting-started)

## Framework di Sviluppo per Smart Contract
- [Hardhat](https://hardhat.org/)
- [Brownie](https://github.com/eth-brownie/brownie)
- [Truffle](https://www.trufflesuite.com/)

## Librerie importanti
- [Zeppelin](https://github.com/OpenZeppelin/openzeppelin-contracts): Librerie che offrono pattern comuni (crowdfuding, safemath, ecc)
- [Chainlink](https://github.com/smartcontractkit/chainlink): Codice che permette di interagire con dati esterni

## Contratti di esempio
- [Dapp Bin](https://github.com/ethereum/dapp-bin)
- [Esempi Defi](https://github.com/PatrickAlphaC/chainlink_defi)
- [Solidity per Contratti a Piccoli Passi](https://github.com/fivedogit/solidity-baby-steps/tree/master/contracts)
- [Contratti ConsenSys](https://github.com/ConsenSys/dapp-store-contracts)
- [Lo stato delle Dapp](http://dapps.ethercasts.com/)

## Sicurezza
- [Pensando Alla Sicurezza Degli Smart Contract](https://blog.ethereum.org/2016/06/19/thinking-smart-contract-security/)
- [Sicurezza Degli Smart Contract](https://blog.ethereum.org/2016/06/10/smart-contract-security/)
- [Blog Distribuito di Hacking](http://hackingdistributed.com/)

## Stile
- [Guida allo Stile di Solidity](http://solidity.readthedocs.io/en/latest/style-guide.html): La guida allo stile di Ethereum deriva in gran parte dalla guida allo stile [PEP 8](https://www.python.org/dev/peps/pep-0008/) di Python.

## Editor
- [Remix](https://remix.ethereum.org/)
- [Emacs Modalità Solidity](https://github.com/ethereum/emacs-solidity)
- [Vim Solidity](https://github.com/tomlion/vim-solidity)
- Snippet per gli Editor ([Ultisnips format](https://gist.github.com/nemild/98343ce6b16b747788bc))

## Cose da fare in futuro
- Nuove keyword: protected, inheritable
- Lista dei design pattern comuni (throttling, RNG, upgrade di versione)
-Anti patterns comuni sulla sicurezza

Sentiti libero di mandare una pull request con qualsiasi modifica - o scrivi una mail a nemild -/at-/ gmail
