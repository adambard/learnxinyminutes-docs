---
category: tool
name: Vim
contributors:
    - ["RadhikaG", "https://github.com/RadhikaG"]
    - ["kaymmm", "https://github.com/kaymmm"]
translators:
    - ["valerioandreachiodi", "https://github.com/valerioandreachiodi"]
filename: LearnVim-it.txt
lang: it-it
---

[Vim](http://www.vim.org) (Vi IMproved) è un clone del popolare editor vi per Unix...
(e qui prosegui con tutto il testo tradotto che abbiamo fatto sopra)

[Vim](http://www.vim.org)
(Vi IMproved) è un clone del popolare editor vi per Unix. È un editor di
testo progettato per la velocità e l'aumento della produttività, ed è ubiquo nella
maggior parte dei sistemi basati su Unix. Ha numerosi scorciatoie da tastiera per
una navigazione rapida in punti specifici del file e per un editing veloce.

`vimtutor` è un'applicazione eccellente che ti insegna come usare `Vim`. Viene
fornita con il pacchetto vim. Dovresti essere in grado di eseguire "vimtutor"
dalla riga di comando per aprire questo tutor in italiano (se disponibile)
o tramite il comando `$ vimtutor it`.Ti guiderà
attraverso tutte le principali funzionalità di `vim`.

## Basi della navigazione in Vim

```
    vim <nomefile>    # Apre <nomefile> in vim
    :help <argomento> # Apre la documentazione di aiuto integrata su <argomento>
    :q                # Esci da vim
    :w                # Salva il file corrente
    :wq               # Salva il file ed esci da vim
    :q!               # Esci da vim senza salvare il file
                      # ! *forza* l'esecuzione di :q, quindi esce senza salvare
    ZQ                # Esci da vim senza salvare il file
    :x                # Salva il file (solo se modificato) ed esci da vim
    ZZ                # Salva il file (solo se modificato) ed esci da vim

    u                 # Annulla (Undo)
    CTRL+R            # Ripristina (Redo)

    h                 # Muovi a sinistra di un carattere
    j                 # Muovi giù di una riga
    k                 # Muovi su di una riga
    l                 # Muovi a destra di un carattere

    Ctrl+B            # Muovi indietro di una schermata intera
    Ctrl+F            # Muovi avanti di una schermata intera
    Ctrl+D            # Muovi avanti di 1/2 schermata
    Ctrl+U            # Muovi indietro di 1/2 schermata

    # Muoversi all'interno della riga

    0                 # Muovi all'inizio della riga
    $                 # Muovi alla fine della riga
    ^                 # Muovi al primo carattere non vuoto della riga

    # Cercare nel testo

    /parola           # Evidenzia tutte le occorrenze di parola dopo il cursore
    ?parola           # Evidenzia tutte le occorrenze di parola prima del cursore
    n                 # Muove il cursore alla successiva occorrenza dopo la ricerca
    N                 # Muove il cursore alla precedente occorrenza

    :%s/foo/bar/g     # Cambia 'foo' in 'bar' su ogni riga del file
    :s/foo/bar/g      # Cambia 'foo' in 'bar' sulla riga corrente
    :%s/\n/\r/g       # Sostituisce i caratteri di nuova riga con caratteri di nuova riga
    :'<,'>s/foo/bar/g # Cambia 'foo' in 'bar' su ogni riga della selezione visuale corrente

    # Saltare ai caratteri

    f<carattere>      # Salta avanti e atterra su <carattere>
    t<carattere>      # Salta avanti e atterra subito prima di <carattere>

    # Per esempio,
    f<                # Salta avanti e atterra su <
    t<                # Salta avanti e atterra subito prima di <

    # Muoversi per parola

    w                 # Muovi avanti di una parola
    b                 # Muovi indietro di una parola
    e                 # Muovi alla fine della parola corrente

    # Altri caratteri per muoversi

    gg                # Vai all'inizio del file
    G                 # Vai alla fine del file
    :NUM              # Vai alla riga numero NUM (NUM è un numero qualsiasi)
    H                 # Muovi all'inizio della schermata
    M                 # Muovi al centro della schermata
    L                 # Muovi alla fine della schermata

```

## Documentazione di aiuto

Vim ha una documentazione di aiuto integrata a cui si può accedere con `:help <argomento>`.
Per esempio `:help navigation` aprirà la documentazione su come navigare nel
tuo spazio di lavoro!

`:help` può anche essere usato senza opzioni. Questo aprirà un dialogo di aiuto
predefinito che mira a rendere l'approccio iniziale a vim più semplice!

## Modalità

Vim si basa sul concetto di **modalità**.

* Modalità Normale (Normal Mode) - vim si avvia in questa modalità, usata per navigare e scrivere comandi
* Modalità Inserimento (Insert Mode) - usata per apportare modifiche al file
* Modalità Visuale (Visual Mode) - usata per evidenziare testo ed eseguire operazioni su di esso
* Modalità Ex (Ex Mode) - usata per scendere in fondo con il prompt ':' per inserire comandi

```
    i                 # Mette vim in modalità inserimento, prima della posizione del cursore
    a                 # Mette vim in modalità inserimento, dopo la posizione del cursore
    v                 # Mette vim in modalità visuale
    :                 # Mette vim in modalità ex
    <esc>             # 'Esce' da qualunque modalità ti trovi, tornando in modalità Normale

    # Copiare e incollare testo
                      # Le operazioni usano il registro di vim per impostazione predefinita
                      # Pensalo come agli appunti privati di vim

                      # Yank ~ copia il testo nel registro di vim
    y                 # Copia (Yank) tutto ciò che è selezionato
    yy                # Copia la riga corrente

                      # Delete ~ copia il testo e lo elimina dal file
    d                 # Elimina tutto ciò che è selezionato
    dd                # Elimina la riga corrente

    p                 # Incolla il testo dal registro dopo la posizione corrente del cursore
    P                 # Incolla il testo dal registro prima della posizione corrente del cursore

    x                 # Elimina il carattere sotto la posizione corrente del cursore

```

## La 'Grammatica' di vim

Vim può essere pensato come un insieme di comandi in un
formato 'Verbo-Modificatore-Soggetto', dove:

* Verbo        - la tua azione
* Modificatore - come stai compiendo l'azione
* Soggetto     - l'oggetto su cui agisce la tua azione

Alcuni esempi importanti di 'Verbi', 'Modificatori' e 'Soggetti':

```
    # 'Verbi'

    d                 # Elimina (Delete)
    c                 # Cambia (Change)
    y                 # Copia (Yank)
    v                 # Seleziona visualmente

    # 'Modificatori'

    i                 # Dentro (Inside)
    a                 # Intorno (Around)
    NUM               # Numero (NUM è un numero qualsiasi)
    f                 # Cerca qualcosa e ci atterra sopra
    t                 # Cerca qualcosa e si ferma prima
    /                 # Trova una stringa dal cursore in poi
    ?                 # Trova una stringa prima del cursore

    # 'Soggetti'

    w                 # Parola (Word)
    s                 # Frase (Sentence)
    p                 # Paragrafo (Paragraph)
    b                 # Blocco (Block)

    # Esempi di 'frasi' o comandi

    d2w               # Elimina 2 parole
    cis               # Cambia all'interno della frase
    yip               # Copia all'interno del paragrafo (copia il paragrafo in cui ti trovi)
    ct<               # Cambia fino alla parentesi aperta
                      # Cambia il testo da dove sei fino alla prossima parentesi aperta
    d$                # Elimina fino alla fine della riga

```

## Alcune scorciatoie e trucchi

```
    >                 # Indenta la selezione di un blocco
    <                 # De-indenta la selezione di un blocco
    :earlier 15m      # Riporta il documento a come era 15 minuti fa
    :later 15m        # Inverte il comando precedente
    ddp               # Scambia la posizione di righe consecutive, dd poi p
    .                 # Ripeti l'azione precedente
    :w !sudo tee %    # Salva il file corrente come root
    :set syntax=c     # Imposta l'evidenziazione della sintassi per 'c'
    :sort             # Ordina tutte le righe
    :sort!            # Ordina tutte le righe al contrario
    :sort u           # Ordina tutte le righe e rimuovi i duplicati
    ~                 # Inverte maiuscole/minuscole del testo selezionato
    u                 # Testo selezionato in minuscolo
    U                 # Testo selezionato in maiuscolo
    J                 # Unisce la riga corrente con la riga successiva

    # Piegare (Fold) il testo
    zf                # Crea una piega dal testo selezionato
    zd                # Elimina la piega sulla riga corrente
    zD                # Elimina ricorsivamente le pieghe annidate o selezionate
    zE                # Elimina tutte le pieghe nella finestra
    zo                # Apri la piega corrente
    zO                # Apri ricorsivamente le pieghe annidate o selezionate
    zc                # Chiudi la piega corrente
    zC                # Chiudi ricorsivamente le pieghe annidate o selezionate
    zR                # Apri tutte le pieghe
    zM                # Chiudi tutte le pieghe
    za                # Alterna apertura/chiusura della piega corrente
    zA                # Alterna ricorsivamente apertura/chiusura delle pieghe annidate
    [z                # Muovi all'inizio della piega corrente
    ]z                # Muovi alla fine della piega corrente
    zj                # Muovi all'inizio della piega successiva
    zk                # Muovi alla fine della piega precedente

```

## Macro

Le macro sono fondamentalmente azioni registrabili.
Quando inizi a registrare una macro, questa registra **ogni** azione e comando
che usi, finché non interrompi la registrazione. Richiamando una macro, questa applica
esattamente la stessa sequenza di azioni e comandi di nuovo sulla selezione di testo.

```
    qa                # Inizia a registrare una macro chiamata 'a'
    q                 # Interrompi la registrazione
    @a                # Riproduci la macro

```

### Configurare ~/.vimrc

Il file .vimrc può essere usato per configurare Vim all'avvio.

Ecco un esempio di file ~/.vimrc:

```vim
" Esempio ~/.vimrc
" 2015.10

" Necessario perché vim sia iMproved
set nocompatible

" Determina il tipo di file dal nome per consentire l'auto-indentazione intelligente, ecc.
filetype indent plugin on

" Abilita l'evidenziazione della sintassi
syntax on

" Migliore completamento riga di comando
set wildmenu

" Usa la ricerca non sensibile alle maiuscole tranne quando si usano lettere maiuscole
set ignorecase
set smartcase

" Quando si apre una nuova riga e non è attiva l'indentazione specifica del file,
" mantieni la stessa indentazione della riga in cui ti trovi
set autoindent

" Visualizza i numeri di riga sulla sinistra
set number

" Opzioni di indentazione, modifica in base alla preferenza personale

" Numero di spazi visuali per TAB
set tabstop=4

" Numero di spazi nel TAB durante l'editing
set softtabstop=4

" Numero di spazi indentati quando si usano le operazioni di ri-indentazione (>> e <<)
set shiftwidth=4

" Converti i TAB in spazi
set expandtab

" Abilita tabulazione e spaziatura intelligente per indentazione e allineamento
set smarttab

```

### Riferimenti

[Vim | Home](http://www.vim.org/index.php)

Il manuale tradotto in italiano lo trovi [qui](https://sites.google.com/view/vimdoc-it/vim-9-1)

`$ vimtutor`  oppure se lo preferisci in italiano `$ vimtutor it`

[A vim Tutorial and Primer](https://danielmiessler.com/study/vim/) in inglese ma molto bello

#### qui sotto due riferimenti in inglese

[Quali sono gli angoli bui di Vim che tua madre non ti ha mai detto? (discussione su Stack Overflow)](http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about)

[Arch Linux Wiki](https://wiki.archlinux.org/index.php/Vim)

--- 
