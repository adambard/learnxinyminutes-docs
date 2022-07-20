---
language: Solidity
filename: learnSolidity.sol
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

A differenza di altri tipi di codice, potresti aver bisogno di usare pattern di
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

Hai distribuito il tuo primo contrattto! Congratulazioni!

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

Riceverai degli Ethereum di test gratuiti. Per distribuire degli smart contract
su una testnet abbiamo bisogno di Ethereum.

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


```javascript
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

    // I 'constructor' possono ricevere una o più parametri; Si può
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

// Attenzione a non andare in overflow, e a proteggersi dagli attacchi che lo fanno
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
// External Account: (persona/enitità esterna): l'indirizzo viene creato dala
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
// fatta una send, che possono fallire
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
enum State { Created, Locked, Inactive }; // di solito si usano per le macchine di stato
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


```
