---
language: LOLCODE
filename: learnLOLCODE-de.lol
contributors:
    - ["abactel", "https://github.com/abactel"]
translators:
    - ["Henrik Jürges", "http://github.com/santifa"]
lang: de-de
---

LOLCODE ist eine esoterische Programmiersprache die die Sprache der [lolcats](https://upload.wikimedia.org/wikipedia/commons/a/ab/Lolcat_in_folder.jpg?1493656347257) nachahmt.

```
BTW Das ist ein Kommentar
BTW Das Programm muss mit `HAI <language version>` beginnen und mit `KTHXBYE` enden.

HAI 1.3
CAN HAS STDIO? BTW Standard Header importieren

OBTW
     ==========================================================================
     ============================== Grundlegendes =============================
     ==========================================================================
TLDR

BTW Texte anzeigen:
VISIBLE "HELLO WORLD"

BTW Variablen deklarieren:
I HAS A MESSAGE ITZ "CATZ ARE GOOD"
VISIBLE MESSAGE

OBTW
    Variablen sind dynamisch typisiert und der Typ muss nicht explizit
    angegeben werden. Die möglichen Typen sind:
TLDR

I HAS A STRING  ITZ "DOGZ ARE GOOOD" BTW Typ ist YARN
I HAS A INTEGER ITZ 42               BTW Typ ist NUMBR
I HAS A FLOAT   ITZ 3.1415           BTW Typ ist NUMBAR
I HAS A BOOLEAN ITZ WIN              BTW Typ ist TROOF
I HAS A UNTYPED                      BTW Typ ist NOOB

BTW Eingaben von Nutzern:
I HAS A AGE
GIMMEH AGE
BTW Die Variable wird als YARN gespeichert und kann in eine
BTW NUMBR konvertiert werden:
AGE IS NOW A NUMBR

OBTW
     ==========================================================================
     ================================== MATHE =================================
     ==========================================================================
TLDR

BTW LOLCODE benutzt polnische Notation für Mathe.

BTW grundlegende mathematische Notationen:

SUM OF 21 AN 33         BTW 21 + 33
DIFF OF 90 AN 10        BTW 90 - 10
PRODUKT OF 12 AN 13     BTW 12 * 13
QUOSHUNT OF 32 AN 43    BTW 32 / 43
MOD OF 43 AN 64         BTW 43 modulo 64
BIGGR OF 23 AN 53       BTW max(23, 53)
SMALLR OF 53 AN 45      BTW min(53, 45)

BTW binäre Notation:

BOTH OF WIN AN WIN           BTW und: WIN if x=WIN, y=WIN
EITHER OF FAIL AN WIN        BTW oder: FAIL if x=FAIL, y=FAIL
WON OF WIN AN FAIL           BTW exklusives oder: FAIL if x=y
NOT FAIL                     BTW unäre Negation: WIN if x=FAIL
ALL OF WIN AN WIN MKAY   BTW beliebige Stelligkeit bei AND
ANY OF WIN AN FAIL MKAY  BTW beliebige Stelligkeit bei OR

BTW Vergleiche:

BOTH SAEM "CAT" AN "DOG"             BTW WIN wenn x == y
DIFFRINT 732 AN 184                  BTW WIN wenn x != y
BOTH SAEM 12 AN BIGGR OF 12 AN 4     BTW x >= y
BOTH SAEM 43 AN SMALLR OF 43 AN 56   BTW x <= y
DIFFRINT 64 AN SMALLR OF 64 AN 2     BTW x > y
DIFFRINT 75 AN BIGGR OF 75 AN 643    BTW x < y

OBTW
     ==========================================================================
     ============================= Flusskontrolle =============================
     ==========================================================================
TLDR

BTW If/then Statement:
I HAS A ANIMAL
GIMMEH ANIMAL
BOTH SAEM ANIMAL AN "CAT", O RLY?
    YA RLY
        VISIBLE "YOU HAV A CAT"
    MEBBE BOTH SAEM ANIMAL AN "MAUS"
        VISIBLE "NOM NOM NOM. I EATED IT."
    NO WAI
        VISIBLE "AHHH IS A WOOF WOOF"
OIC

BTW Case Statement:
I HAS A COLOR
GIMMEH COLOR
COLOR, WTF?
    OMG "R"
        VISIBLE "RED FISH"
        GTFO
    OMG "Y"
        VISIBLE "YELLOW FISH"
        BTW Weil hier kein `GTFO` ist wird auch das nächste Statement überprüft
    OMG "G"
    OMG "B"
        VISIBLE "FISH HAS A FLAVOR"
        GTFO
    OMGWTF
        VISIBLE "FISH IS TRANSPARENT OHNO WAT"
OIC

BTW For Schleife:
I HAS A TEMPERATURE
GIMMEH TEMPERATURE
TEMPERATURE IS NOW A NUMBR
IM IN YR LOOP UPPIN YR ITERATOR TIL BOTH SAEM ITERATOR AN TEMPERATURE
    VISIBLE ITERATOR
IM OUTTA YR LOOP

BTW While Schleife:
IM IN YR LOOP NERFIN YR ITERATOR WILE DIFFRINT ITERATOR AN -10
    VISIBLE ITERATOR
IM OUTTA YR LOOP

OBTW
     =========================================================================
     ================================ Strings ================================
     =========================================================================
TLDR

BTW Zeilenumbrüche:
VISIBLE "FIRST LINE :) SECOND LINE"

BTW Tabulatoren:
VISIBLE ":>SPACES ARE SUPERIOR"

BTW Bell (macht beep):
VISIBLE "NXT CUSTOMER PLS :o"

BTW Anführungszeichen in Strings:
VISIBLE "HE SAID :"I LIKE CAKE:""

BTW Doppelpunkte in Strings :
VISIBLE "WHERE I LIVE:: CYBERSPACE"

OBTW
     =========================================================================
     =============================== Funktionen ==============================
     =========================================================================
TLDR

BTW Definieren einer neuen Funktion:
HOW IZ I SELECTMOVE YR MOVE BTW `MOVE` ist ein Argument
    BOTH SAEM MOVE AN "ROCK", O RLY?
        YA RLY
            VISIBLE "YOU HAV A ROCK"
        NO WAI
            VISIBLE "OH NO IS A SNIP-SNIP"
    OIC
    GTFO BTW Gibt NOOB zurück
IF U SAY SO

BTW Eine Funktion deklarieren und einen Wert zurückgeben:
HOW IZ I IZYELLOW
    FOUND YR "YELLOW"
IF U SAY SO

BTW Eine Funktion aufrufen:
I IZ IZYELLOW MKAY

KTHXBYE
```

## Weiterführende Informationen:

-   [LCI compiler](https://github.com/justinmeza/lci)
-   [Official spec](https://github.com/justinmeza/lolcode-spec/blob/master/v1.2/lolcode-spec-v1.2.md)
