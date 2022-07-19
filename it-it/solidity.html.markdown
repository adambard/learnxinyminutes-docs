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

Rivecerai degli Ethereum di test gratuiti. Per distribuire degli smart contract
su una testnet abbiamo bisogno di Ethereum.

Nell'esempio precedente non avevamo usato una testnet, ma avevamo distribuito
su un ambiente virtuale fittizio. Quando si lavora su una testnet, possiamo
davvero monitorare e interagire con i nostri contratti in maniera persistente.

Per distribuire su una testnet, allo step `#4 Fai il deploy`, cambia
l'`environment` selezionato in `injected web3`. In questo modo verrà usato
come network su cui fare il deploy qualsiasi network selezionato sul tuo
Metamask.

![Solidity-in-remix](../images/solidity/remix-testnet.png)

Per ora continua a usare la `Javascript VM` a meno che non ti sia detto di cambiarla. Quando distribuisci su una testnet, Metamask aprirà un pop up che
ti chiederà di "confermare" la transazione. Premi `yes` e dopo un certo lasso
di tempo, ti apparirà la stessa interfaccia per il contratto nella parte
inferiore dello schermo.