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
    - ["Alias", "http://github.com/al-ias"]
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
```
