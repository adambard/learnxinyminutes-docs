---
category: tool
tool: vim
lang: de-de
contributors:
    - ["RadhikaG", "https://github.com/RadhikaG"]
translators:
    - ["caminsha", "https://github.com/caminsha"]
filename: LearnVim-de.txt
---


[Vim](http://www.vim.org)
(Vi IMproved) ist ein Klon von vi, dem bekannten Editor für Unix. Es ist ein
Texteditor, welcher mit Fokus auf Geschwindigkeit und Prouktivität entwickelt 
wurde.
Vim hat viele Keybindings für ein schnelles navigieren und schnelles bearbeiten
einer Datei.

## Grundlagen, um in Vim zu navigieren

```
    vim <filename>   # Öffne <filename> in Vim
    :help <topic>    # Open up built-in help docs about <topic> if any exists
    :help <topic>    # Öffne die eingebaute Hilfe zum Thema  <topic>, wenn
                     # es existiert
    :q               # Schließe vim
    :w               # Speichere diese Datei
    :wq              # Speichere diese Datei und schließe vim
    ZZ               # Speichere diese Datei und schließe vim
    :q!              # Schließe vim ohne die Datei zu speichern
                     # ! *zwingt* die Ausführung von :q,
                     # daher wird die Datei nicht gespeichert.
    ZQ               # Beende vim ohne die Datei zu speichern
    :x               # Speichere die Datei und beende vim
                     # Dies ist eine kürzere Version von :wq

    u                # Änderung rückgängig machen
    CTRL+R           # Änderung wiederherstellen

    h                # Den Cursor um ein Zeichen nach links bewegen
    j                # Den Cursor eine Zeile nach unten bewegen
    k                # Den Cursor eine Zeile nach oben bewegen
    l                # Den Cursor um ein Zeichen nach rechts bewegen

    Ctrl+B 	         # Gehe eine Bildschirmanzeige zurück
    Ctrl+F 	         # Gehe eine Bildschirmanzeige vorwärts
    Ctrl+D 	         # Gehe eine halbe Bildschirmanzeige vorwärts
    Ctrl+U           # Gehe eine halbe Bildschirmanzeige zurück

    # Navigieren innerhalb einer Zeile

    0                # Navigiere zum Anfang der Zeile
    $                # Navigiere zum Ende der Zeile
    ^                # Navigiere zum ersten Zeichen, welches kein Leerzeichen ist

    # Im Text suchen

    /word            # Hebt alle Ergebnisse nach dem Cursor hervor
    ?word            # Hebt alle Ergebnisse vor dem Cursor hervor
    n                # Bewegt den Cursor zum nächsten Ergebnis nach der Suche
    N                # Bewegt den Cursor zum vorherigen Ergebnis der Suche

    :%s/foo/bar/g    # Ersetze "foo" durch "bar" in allen Zeilen
    :s/foo/bar/g     # Ersetze "foo" durch "bar" in der aktuellen Zeile
    :%s/\n/\r/g      # Ersetze das newline-Zeichen bei allen Zeilen durch
                     # ein carriage return

    # Zu einzelnen Zeichen springen

    f<character>     # Springe vorwärts und auf dem Zeichen  <character>
    t<character>     # Springe vorwärts und lande vor dem Zeichen <character>

    # Zum Beispiel,
    f<               # Springe vorwärts und lande auf <
    t<               # Springe vorwärts und lande vor <

    # Wortweise navigieren

    w                # Springe um ein Wort vorwärts
    b                # Gehe ein Wort zurück
    e                # Springe zum Ende des aktuellen Wortes

    # Weitere Befehle, um zu navigieren

    gg               # Gehe an den Start der Datei
    G                # Gehe an das Ende der Datei
    :NUM             # Springe zur Zeile NUM (NUM kann eine beliebige Zahl sein)
    H                # Navigiere zum Start der aktuellen Bildschirmanzeige
    M                # Navigiere in die Mitte der aktuellen Bildschirmanzeige
    L                # Navigiere an das Ende der aktuellen Bildschirmanzeige
```

## Hilfsdokumente:

Vim hat eine eingebaute Dokumentation, welche mit `:help <topic>` aufgerufen
werden kann.
Zum Beispiel öffnet `:help navigation` die Dokumentation über das Navigieren

`:help` kann auch ohne ein Argument verwendet werden. Dies zeigt den Standard-
Hilfsdialog an, welcher den Start mit vim einfacher macht.
that aims to make getting started with vim more approachable!

## Modi:

Vim basiert auf dem Konzept von **modes**.

- Command Mode - Vim startet in diesem Modus, hier kann man navigieren und Befehle eingeben
- Insert Mode  - Wird verwendet, um Änderungen in der Datei zu machen.
- Visual Mode  - Wird verwendet, um Text zu markieren und Operationen durchzuführen
- Ex Mode      - Wird verwendet, um im ':'-Prompt Befehle einzugeben

```
    i                # Führt vim in den Insert Mode, vor der Cursorposition
    a                # Führt vim in den Insert Mode, nach der Cursorposition
    v                # Führt vim in den Visual Mode
    :                # Führt vim in den Ex Mode
    <esc>            # Führt zurück in den Command Mode, egal in welchem Mode
                     # man sich gerade befindet.

    # Kopieren und einfügen von Text

    y                # Kopiere alles, was im Moment ausgewählt ist
    yy               # Kopiert die aktuelle Zeile
    d                # Löscht alles, was im Moment ausgewählt ist
    dd               # Löscht die aktuelle Zeile
    p                # Fügt den kopierten Text nach dem Cursor ein
    P                # Fügt den kopierten Text vor dem Cursor ein
    x                # Löscht das Zeichen unter dem Cursor
```

## Die 'Grammatik' von Vim

Vim kann als Satz von Kommandos angesehen werden, welche im Format
'Verb-Modifier-Noun' sind. Hierbei gilt:

- Verb     - die Aktion, du machen willst
- Modifier - wie die Aktion gemacht wird
- Noun     - das Objekt, auf welchem die Aktion ausgeführt wird.

Einige wichtige Beispiele von 'Verb', 'Modifier' und 'Nouns':

```
    # 'Verb'

    d                # löschen
    c                # ändern
    y                # kopieren
    v                # visuelles auswählen

    # 'Modifiers'

    i                # innerhalb
    a                # außerhalb
    NUM              # Nummer (NUM kann irgendeine Zahl sein)
    f                # Sucht nach etwas und landet darauf
    t                # Sucht nach etwas und stoppt davor
    /                # Suche eine Zeichenfolge ab dem Cursor
    ?                # Suche eine Zeichenfolge vor dem Cursor

    # 'Nouns'

    w                # Wort
    s                # Satz
    p                # Abschnitt
    b                # Block

    # Beispielsätze resp. Kommandos

    d2w              # lösche zwei Wörter
    cis              # Ändere innerhalb des Satzes.
    yip              # Kopiere innerhalb des Abschnitts (kopiere den Abschnitt, 
                     # in welchem du bist)
    ct<              # Ändere bis zur spitzen Klammer
                     # Ändere den Text von deiner aktuellen Cursorposition bis
                     # zur nächsten spitzen Klammer
    d$               # Lösche bis zum Ende der Zeile
```

## Einige Shortcuts und Tricks

```
    >                # Rücke die Auswahl um einen Block ein
    <                # Lösche eine Einrückung der Auswahl
    :earlier 15m     # Stellt das Dokument so wieder her, wie es vor 15 
                     # Minuten war
    :later 15m       # den oberen Befehl rückgängig machen
    ddp              # Vertauschen zweier aufeinanderfolgenden Zeilen
                     # Zuerst dd, dann p
    .                # Wiederhole die vorherige Aktion
    :w !sudo tee %   # Speichere die Datei als Root
    :set syntax=c    # Stelle das Syntax-Highlighting für 'C' ein
    :sort            # Alle Zeilen sortieren
    :sort!           # Alle Zeilen rückwärts sortieren
    :sort u          # Alle Zeilen sortieren und Duplikate entfernen
    ~                # Umschalten der Groß-/Kleinschreibung des ausgewählten Textes
    u                # Ausgewählten Text zu Kleinschreibung ändern
    U                # Ausgewählten Text zu Großschreibung ändern
    
    # Text-Folding (Textfaltung)
    zf               # Erstelle eine Faltung des ausgewählten Textes
    zo               # Öffne die aktuelle Faltung
    zc               # Schließe die aktuelle Faltung
    zR               # Öffne alle Faltungen
    zM               # Schließe alle Faltungen
```

## Makros

Makros sind grundsätzlich einfach aufgezeichnete Aktionen
Wenn du mit dem Aufnehmen eines Makros beginnst, werden **alle** Aktionen und
Kommandos, welche du braucht, aufgenommen bis die Aufnahme gestoppt wird.
Wenn du ein Makro ausführst, werden exakt die gleichen Schritte gemacht.

```
    qa               # Starte das Aufnehmen des Makros 'a'
    q                # Beende das Aufnehmen
    @a               # Führe das Makro 'a' aus
```

### Konfigurieren mit ~/.vimrc

Die Datei .vimrc kann verwendet werden, um Vim beim Starten zu konfigurieren

Hier ist eine Beispiel ~/.vimrc Datei:

```
" Beispiel ~/.vimrc

" Erforderlich für vim, dass es iMproved ist.
set nocompatible

" Bestimme den Dateityp anhand des Namens, um ein intelligentes Einrücken etc.
" zu ermöglichen
filetype indent plugin on

" Aktiviere das Syntax-Highlighting
syntax on

" Bessere Kommandozeilen-Vervollständigung
set wildmenu

" Verwende die Suche ohne die Berücksichtigung der Groß-/Kleinschreibung, außer
" wenn mit Großbuchstaben gesucht wird.
set ignorecase
set smartcase

" Wenn eine neue Zeile erstellt wird und kein Dateispezifisches Einrücken
" aktiviert ist, behält die neue Zeile die gleiche Einrückung wie die aktuelle
" Zeile
set autoindent

" Zeige links die Zeilennummern an
set number

" Einrückungsoptionen, ändere diese nach deinen Vorlieben

" Anzahl sichtbarer Leerzeichen bei einem TAB
set tabstop=4

" Anzahl der Leerzeichen während des Bearbeitens bei einem TAB
set softtabstop=4

" Anzahl der Einrückungstiefe bei den Operationen (>> und <<)
set shiftwidth=4

" Konvertiere TABs zu Leerzeichen
set expandtab

" Enable intelligent tabbing and spacing for indentation and alignment
" Aktiviere intelligente Tabs und Leerzeichen bei der Einrückung und Ausrichtung
set smarttab
```

### Verweise

- [Vim | Homepage](http://www.vim.org/index.php)
- In der Shell eingeben: `vimtutor`
- [Ein vim Tutorial und Primer, englisch](https://danielmiessler.com/study/vim/)
- [Deutsches Arch Linux Wiki](https://wiki.archlinux.de/title/Vim)
- [Arch Linux Wiki, englisch (dafür ausführlicher)](https://wiki.archlinux.org/index.php/Vim)
- [What are the dark corners of Vim your mom never told you about? (Stack Overflow thread)](http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about)
