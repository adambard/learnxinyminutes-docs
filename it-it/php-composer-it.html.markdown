---
category: tool
tool: composer
contributors:
    - ["Brett Taylor", "https://github.com/glutnix"]
translator:
    - ["Agostino Fiscale", "https://github.com/agostinofiscale"]
lang: it-it
filename: LearnComposer-it.sh
---

[Composer](https://getcomposer.org/) è uno strumento che ti aiuta a gestire le 
dipendenze in PHP. Ti permette di dichiarare le librerie utilizzate dal tuo 
progetto e di installarle/aggiornarle per te.

# Installazione

```sh
# Se installi l'eseguibile in una cartella...
curl -sS https://getcomposer.org/installer | php
# ...dovrai utilizzare questo approccio, invocando Composer in questo modo:
php composer.phar about

# Se installi l'eseguibile nella directory ~/bin/composer
# Nota: assicurati che ~/bin si trovi nella variabile di ambiente PATH
curl -sS https://getcomposer.org/installer | php -- --install-dir=~/bin --filename=composer
```

Gli utenti Windows possono seguire le istruzioni per [installarlo su Windows](https://getcomposer.org/doc/00-intro.md#installation-windows).

## Assicuriamoci che il tutto abbia funzionato correttamente

```sh
# Controlla la versione e la lista delle opzioni disponibili
composer

# Ottieni ulteriori informazioni riguardanti le opzioni
composer help require

# Controlla se Composer ha tutto il necessario per funzionare come si deve
# e se è aggiornato correttamente all'ultima versione disponibile.
composer diagnose
composer diag # alias

# Aggiorna Composer all'ultima versione disponibile
composer self-update
composer self # alias
```

# Cominciamo ad usare Composer

Composer memorizza le dipendenze necessarie nel file `composer.json`.
Puoi editare questo file manualmente, ma è meglio che lasci che Composer se ne 
occupi per te.

```sh
# Crea un nuovo progetto nella cartella attuale
composer init
# ti verranno chieste delle domande interrative riguardanti il tuo progetto. 
# Puoi evitare di rispondere almeno che tu non stia sviluppando altri progetti 
# che che possano dipendere da questo.

# Se il file composer.json esiste già, scarichera' le dipendenze necessarie
composer install

# Scarica le dipendenze necessarie per l'ambiente di produzione
composer install --no-dev

# Aggiunge una dipendenza per l'ambiente di produzione
composer require guzzlehttp/guzzle
# automaticamente selezionera' l'ultima versione, la scarichera' e la aggiungera'
# come dipendenza nell'apposito campo del file composer.json.

composer require guzzlehttp/guzzle:6.0.*
# scarichera' l'ultima versione disponibile corrispondente al pattern (es. 6.0.2)
# e lo aggiungera' come dipendenza nell'apposito campo del file composer.json.

composer require --dev phpunit/phpunit:~4.5.0
# aggiungera' la dipendenza nell'ambiente di sviluppo utilizzando l'ultima versione
# disponibile nel range >=4.5.0 e < 4.6.0.

composer require-dev phpunit/phpunit:^4.5.0
# aggiungera' la dipendenza nell'ambiente di sviluppo utilizzando l'ultima versione
# disponibile nel range >=4.5.0 and < 5.0.

# Per ulteriori dettagli riguardo le versioni, vedi [la documentazione di Composer sulle versioni](https://getcomposer.org/doc/articles/versions.md) per ulteriori dettagli

# Per vedere quali pacchetti sono installabili e quali sono gia' stati installati
composer show

# Per vedere solo quali pacchetti sono gia' stati installati
composer show --installed

# Per trovare una dipendenza con 'mailgun' nel suo nome o nella descrizione.
composer search mailgun
```

[Packagist.org](https://packagist.org/) è il repository principale per i pacchetti
di Composer.  Cerca qui pacchetti di terze-parti utili per il tuo progetto.

## `composer.json` vs `composer.lock`

Il file `composer.json` memorizza la versione che si preferisce per ogni dipendenza,
insieme ad altre informazioni.

Il file `composer.lock` memorizza quale versione è stata scaricata per ogni
dipendenza. Non editare mai questo file.

Se includi il file `composer.lock` nella tua repository git, ogni sviluppatore
andra' a installare la versione attualmente utilizzata dal tuo progetto per
ogni dipendenza. Anche quando una nuova versione è stata rilasciata, Composer 
andra' a installare la versione registrata nel file lock.

```sh
# Se vuoi aggiornare tutte le dipendenze all'ultima versione che corrisponde al pattern descritto
composer update

# Se vuoi scaricare l'ultima versione di una particolare dipendenza:
composer update phpunit/phpunit

# Se vuoi cambiare la versione di una una dipendenza, potresti dover rimuovere
# quello attualmente selezionato, per poi richiedere quello corretto successivamente,
# attraverso i seguenti comandi:
composer remove --dev phpunit/phpunit
composer require --dev phpunit/phpunit:^5.0
```

## Autoloader

Composer crea una classe autoloader che puoi richiamare nella tua applicazione.
Puoi creare instanze delle classi tramite il loro namespace.

```php
require __DIR__ . '/vendor/autoload.php';

$mailgun = new Mailgun\Mailgun("key");
```

### PSR-4 Autoloader

Puoi aggiungere i tuoi namespace all'autoloader.

Nel file `composer.json`, aggiungi il campo "autoload":

```json
{
  "autoload": {
    "psr-4": {"Acme\\": "src/"}
  }
}
```

Questo dira' all'autoloader di controllare ogni risorsa che corrisponde al 
namespace `\Acme\` all'interno della cartella `src`.

In alternativa puoi usare [PSR-0, una Classmap o una lista di file da includere](https://getcomposer.org/doc/04-schema.md#autoload).
Inoltre e' possibile anche utilizzare `autoload-dev` dedicato all'ambiente di sviluppo.

Quando aggiungi o modifichi una chiave, dovrai ricompilare l'autoload attraverso:

```sh
composer dump-autoload
composer dump # alias

# Ottimizza i pacchetti PSR0 e PSR4 per essere caricati anche con le classmap.
# Sara' lento, ma migliorera' le performance in produzione.
composer dump-autoload --optimize --no-dev
```

# La cache di Composer

```sh
# Composer utilizzera i pacchetti scaricati anche per i progetti futuri. Per evitare che accada:
composer clear-cache
```

# Problemi? 

```sh
composer diagnose
composer self-update
composer clear-cache
```

## Argomenti che non sono stati (ancora) discussi in questo tutorial

* Creare e distribuire pacchetti personali su Packagist.org o altrove
* Pre- e post- script hooks: eseguire operazioni quando vengono eseguiti degli eventi

### References

* [Composer - Dependency Manager for PHP](https://getcomposer.org/)
* [Packagist.org](https://packagist.org/)
