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