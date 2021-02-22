---
category: tool
tool: vim
contributors:
    - ["RadhikaG", "https://github.com/RadhikaG"]
translators:
  - ["Rick Haan", "https://github.com/RickHaan"]
filename: learnvim-nl.yaml
lang: nl-nl
---

# Vim in het Nederlands

[Vim](http://www.vim.org)
(Vi IMproved) is een kopie van de populaire vi editor voor Unix. Het is
ontworpen voor snelheid, verhoogde productiviteit en is beschikbaar in de meeste
unix-gebaseerde systemen. Het heeft verscheidene toetscombinaties voor snelle
navigatie en aanpassingen in het doelbestand.

## De Basis van het navigeren in Vim

``` Vim
    vim <bestandsnaam> # Open <bestandsnaam> in vim
    :help <onderwerp>  # Open ingebouwde documentatie over <onderwerp> als
    deze bestaat
    :q               # Vim afsluiten
    :w               # Huidig bestand opslaan
    :wq              # Huidig bestand opslaan en vim afsluiten
    ZZ               # Huidig bestand opslaan en vim afsluiten
    :x               # Huidig bestand opslaan en vim afsluiten, verkorte versie
    :q!              # Afsluiten zonder opslaan
                     # ! *forceert* het normale afsluiten met :q

    u                # Ongedaan maken
    CTRL+R           # Opnieuw doen

    h                # Ga 1 karakter naar links
    j                # Ga 1 regel naar beneden
    k                # Ga 1 regel omhoog
    l                # Ga 1 karakter naar rechts

    Ctrl+B           # Ga 1 volledig scherm terug
    Ctrl+F           # Ga 1 volledig scherm vooruit
    Ctrl+D           # Ga 1/2 scherm vooruit
    Ctrl+U           # Ga 1/2 scherm terug

    # Verplaatsen over de regel

    0                # Verplaats naar het begin van de regel
    $                # Verplaats naar het eind van de regel
    ^                # Verplaats naar het eerste niet-lege karakter op de regel

    # Zoeken in de tekst

    /word            # Markeert alle voorvallen van 'word' na de cursor
    ?word            # Markeert alle voorvallen van 'word' voor de cursor
    n                # Verplaatst de cursor naar het volgende voorval van
    de zoekopdracht
    N                # Verplaatst de cursor naar het vorige voorval van
    de zoekopdracht

    :%s/foo/bar/g    # Verander 'foo' naar 'bar' op elke regel van het bestand
    :s/foo/bar/g     # Verander 'foo' naar 'bar' op de huidge regel in
    het bestand
    :%s/\n/\r/g      # Vervang nieuwe regel karakters met nieuwe regel karakters

    # Spring naar karakters

    f<character>     # Spring vooruit en land op <character>
    t<character>     # Spring vooruit en land net voor <character>

    # Bijvoorbeeld,
    f<               # Spring vooruit en land op <
    t<               # Spring vooruit en land net voor <

    # Verplaatsen per woord

    w                # Ga 1 woord vooruit
    b                # Ga 1 woord achteruit
    e                # Ga naar het einde van het huidige woord

    # Andere karakters om mee te verplaatsen

    gg               # Ga naar de bovenkant van het bestand
    G                # Ga naar de onderkant van het bestand
    :NUM             # Ga naar regel NUM (NUM is elk nummer)
    H                # Ga naar de bovenkant van het scherm
    M                # Ga naar het midden van het scherm
    L                # Ga naar de onderkant van het scherm
```

## Help documentatie

Vim heeft ingebouwde help documentatie dat benaderd kan worden met
`:help <onderwerp>`. Bijvoorbeeld `:help navigation` geeft documentatie weer hoe
door vim te navigeren. `:help` kan ook gebruikt worden zonder onderwerp. Dan wordt de standaard documentatie weergeven die bedoelt is om vim toegankelijker te maken.

## Modus

Vim is gebaseerd op het concept van **modus**.

* Command (opdracht) modus - Vim wordt opgestart in deze mode. Deze mode wordt
gebruikt om opdrachten te geven en te navigeren
* Insert (invoer) modus - Wordt gebruikt voor het aanpassen van het bestand
* Zichtbare (Visual) modus - Wordt gebruikt voor het markeren en bewerken van
tekst
* Ex modus - Wordt gebruikt voor het uitvoeren van opdrachten met `:`

``` Vim
    i                # Zet vim in de Command modus voor de cursor positie
    a                # Zet vim in de Insert modus na de cursor positie (append)
    v                # Zet vim in de Visual modus
    :                # Zet vim in de ex modus
    <esc>            # 'Escapes' vanuit elke modus naar de Command modus

    # Het kopiëren en plakken van tekst

    y                # Yank (kopieer) wat geselecteerd is
    yy               # Yank (kopieer) de huidige regel
    d                # Verwijder wat geselecteerd is
    dd               # Verwijder de huidige regel
    p                # Plak de huidige tekst op de cursor positie
    P                # Plak de huidige tekst voor de cursor positie
    x                # Verwijder karakter op cursor positie
```

## De 'gramatica' van vim

Vim kan aangeleerd worden als een set van acties in het 'Verb-Modifier-Noun'
formaat waar:

Verb (werkwoord) - De uit te voeren actie
Modifier (bijwoord) - Hoe de actie uitgevoerd dient te worden
Noun - Het object waarop de actie uitgevoerd wordt

Een paar belangrijke voorbeelden van 'Verbs', 'Modifiers', en 'Nouns' zijn:

``` Vim
    # 'Verbs'

    d       # Verwijder
    c       # Verander
    y       # Kopieer
    v       # Zichtbaar selecteren

    # 'Modifiers'

    i       # Binnen
    a       # Rondom
    NUM     # Elk nummer
    f       # Zoekt iets en selecteerd het
    t       # Zoekt iets en selecteerd het karakter voor het
    /       # Vindt een combinatie van tekens vanaf de cursor
    ?       # Vindt een combinatie van tekens voor de cursor

    # 'Nouns'

    w       # Woord
    s       # Zin
    p       # Paragraaf
    b       # Blok

    # Voorbeeld 'zinnen' of opdrachten

    d2w     # Verwijder twee woorden
    cis     # Verander in de zin
    yip     # Kopiereer in de paragraaf
    ct<     # Verander naar haakje openen
            # Verander de tekst vanaf de huidige positie tot het volgende haakje
            openen
    d$      # Verwijder tot het einde van de regel
```

## Een aantal afkortingen en trucs

``` Vim
    >               # Verspring de selectie met 1 blok
    <               # Verspring de selectie met 1 blok terug
    :earlier 15     # Zet het document terug naar de situatie van 15 minuten
    geleden
    :later 15       # Zet het document in de situatie 15 minuten in de toekomst
    (omgekeerde van de vorige opdracht)
    ddp             # Wissel de positie van opeenvolgende regels. dd daarna p
    .               # Herhaal de vorige opdracht
    :w !sudo tee%   # Sla het huidige bestand op als root
    :set syntax=c   # Stel syntax uitlichten in op 'c'
    :sort           # Sorteer alle regels
    :sort!          # Sorteer alle regels omgekeerd
    :sort u         # Sorteer alle regels en verwijder duplicaten
    ~               # Stel letter case in voor geselecteerde tekst
    u               # Verander de geselecteerde tekst naar kleine letters
    U               # Verander de geselecteerde tekst naar hoofdletters

    # Fold text
    zf              # Creeer een vouw op de geslecteerde tekst
    zo              # Open huidige vouw
    zc              # Sluit huidige vouw
    zR              # Open alle vouwen
    zM              # Sluit alle vouwen
```

## Macro's

Macro's zijn opgeslagen opdrachten. Wanneer je begint met het opnemen van een
macro dan worden **alle** acties opgenomen, totdat je stopt met opnemen. Als de
macro uitgevoerd wordt, worden alle acties in de zelfde volgorde als tijdens het
opnemen uitgevoerd.

``` Vim
    qa  # Start met het opnemen van de makro genaamd 'a'
    q   # Stop met opnemen
    @a  # Gebruik macro 'a'
```

## Configureren van .vimrc

Het .vimrc bestand kan gebruikt worden voor het opslaan van een
standaardconfiguratie van Vim. Het bestand wordt opgeslagen in de home map van de gebruiker. Hieronder staat een voorbeeld van een .vimrc bestand.

``` Vim
" Voorbeeld ~/.vimrc
" 2015.10

" In te stellen dat Vim niet samenwerkt met Vi
set nocompatible

" Stel in dat Vim kijkt naar de bestandstype voor syntax uitlichting en
automatish inspringen
filetype indent plugin on

" Zet inspringen aan
syntax on

" Betere opdracht regel aanvulling
set wildmenu

" Gebruik niet hoofdlettergevoelig zoeken.
set ignorecase
set smartcase

" Gebruik automatisch inspringen
set autoindent

" Geef regelnummers weer
set number

" Het aantal zichtbare spatie's per TAB
set tabstop=4

" Het aantal spatie's tijdens het aanpassen
set softtabstop=4

" Aantal spatie's wanneer (>> en <<) worden gebruikt

" Maak van TAB's spatie's
set expandtab

" Gebruik slimme tabs spatie's voor inspringen en uitlijnen
set smarttab
```

## Referenties (Engels)

[Vim | Home](http://www.vim.org/index.php)

`$ vimtutor`

[A vim Tutorial and Primer](https://danielmiessler.com/study/vim/)

[What are the dark corners of Vim your mom never told you about? (Stack Overflow thread)](http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about)

[Arch Linux Wiki](https://wiki.archlinux.org/index.php/Vim)