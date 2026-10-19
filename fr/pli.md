---
translators:
    - ["Cyril Hansen"]
---

PL/I (*Programming Language One*, prononcé « PL one ») est né chez IBM
d'une ambition peu modeste pour les années 1960 : les scientifiques
avaient FORTRAN, les gestionnaires COBOL, et la programmation système
ses propres exigences. Pourquoi ne pas réunir tout cela dans un seul
langage ? PL/I mêle ainsi structures de contrôle, calcul décimal,
chaînes de caractères et traitement des exceptions.

Nous utiliserons ici **Digital Research PL/I-80** (1980), un compilateur
pour CP/M fondé sur le projet de sous-ensemble généraliste du PL/I ANSI
(*General-Purpose Subset G*). Une bonne partie du langage des grands
systèmes a survécu au passage sur 8 bits, même si le PL/I IBM complet
offre davantage de fonctionnalités.

Les commentaires du programme restent en ASCII pour faciliter sa
réutilisation avec les outils CP/M historiques.

```text
/* Commentaires : slash-etoile au debut, etoile-slash a la fin.
   PL/I ignore la casse, est en format libre et separe par des ';'. */

LEARN: PROCEDURE OPTIONS(MAIN);
    /* PL/I-80 permet des substitutions de texte a la compilation. */
    %REPLACE LIMIT BY 4;

    /* DCL abrege DECLARE. Les donnees se declarent explicitement. */
    DECLARE I FIXED BINARY(15);
    /* PL/I-80 impose STATIC avec INITIAL. */
    DECLARE TOTAL FIXED BINARY(15) STATIC INITIAL(0);
    DECLARE AMOUNT FIXED DECIMAL(7,2) STATIC INITIAL(12.50);
    DECLARE HALF FLOAT BINARY(24) STATIC INITIAL(0.5);
    DECLARE READY BIT(1) STATIC INITIAL('1'B);
    DECLARE LABEL_TEXT CHARACTER(12) STATIC INITIAL('hello');
    DECLARE MESSAGE CHARACTER(40) VARYING;
    DECLARE NUMBERS(LIMIT) FIXED BINARY(15);
    DECLARE GRID(0:2,1:3) FLOAT BINARY(24);
    DECLARE HEAP_PTR POINTER;
    DECLARE HEAP_VALUE FIXED BINARY(15) BASED(HEAP_PTR);
    DECLARE OUTPUT FILE;

    /* Une structure utilise des niveaux numeriques imbriques. */
    DECLARE 1 PERSON,
              2 PERSON_NAME CHARACTER(20),
              2 VISITS FIXED BINARY(15);

    /* PUT ecrit sur la console (SYSPRINT) par defaut. */
    PUT SKIP LIST('Hello, world!');

    /* = affecte une valeur. ** calcule une puissance. */
    I = 2 ** 3;                  /* 8 */
    TOTAL = I + 2;               /* 10 */
    AMOUNT = AMOUNT + 1.25;      /* 13.75 */

    PUT SKIP LIST('Total:', TOTAL);
    PUT SKIP LIST('Half:', HALF);

    /* EDIT formate la sortie : F(largeur, decimales). */
    PUT SKIP EDIT(AMOUNT) (F(7,2));

    /* Les chaines fixes sont completees par des espaces. */
    LABEL_TEXT = 'hi';

    /* Les chaines VARYING conservent leur longueur effective. */
    MESSAGE = 'PL/I' || ' on CP/M';  /* || concatene */
    PUT SKIP LIST(MESSAGE, LENGTH(MESSAGE));
    PUT SKIP LIST(SUBSTR(MESSAGE, 1, 4));
    SUBSTR(MESSAGE, 1, 4) = 'PL/1';

    /* Une comparaison donne BIT(1) ; & = ET, | = OU. */
    IF (TOTAL >= 10) & READY THEN DO;
        PUT SKIP LIST('Ready');
    END;
    ELSE DO;
        PUT SKIP LIST('Not ready');
    END;

    /* Les indices des tableaux commencent a 1 par defaut. */
    DO I = 1 TO LIMIT;
        NUMBERS(I) = I * I;
    END;

    DO I = 1 TO LIMIT;
        PUT SKIP LIST(I, NUMBERS(I));
    END;

    /* Autres variantes de boucle : BY et WHILE. */
    DO I = 2 TO 6 BY 2;
        PUT SKIP LIST('Even:', I);
    END;

    I = 3;
    DO WHILE (I > 0);
        I = I - 1;
    END;

    /* Tableaux numeriques et fonctions mathematiques, comme FORTRAN. */
    GRID(0,1) = SQRT(9.0);
    PUT SKIP LIST('Square root:', GRID(0,1));

    /* La memoire pointee peut survivre a son bloc d'origine. */
    ALLOCATE HEAP_VALUE SET(HEAP_PTR);
    HEAP_VALUE = 42;
    PUT SKIP LIST('Heap:', HEAP_PTR -> HEAP_VALUE);
    FREE HEAP_VALUE;
    HEAP_PTR = NULL();

    PERSON.PERSON_NAME = 'Ada';
    PERSON.VISITS = 2;
    PUT SKIP LIST(PERSON.PERSON_NAME, PERSON.VISITS);

    /* CALL appelle une procedure ; une fonction renvoie une valeur. */
    CALL SHOW_NUMBER(TOTAL);
    PUT SKIP LIST('5 factorial:', FACT(5));

    /* Sous PL/I-80, un GO TO permet de reprendre apres ZERODIVIDE. */
    ON ZERODIVIDE BEGIN;
        PUT SKIP LIST('Division by zero signalled');
        GO TO AFTER_ZERODIVIDE;
    END;

    SIGNAL ZERODIVIDE;

    AFTER_ZERODIVIDE:
    REVERT ZERODIVIDE;

    /* La lecture sur la console s'ecrit : GET LIST(I); */

    /* Fichier sequentiel CP/M (decommenter pour ecrire OUTPUT.DAT) :
       OPEN FILE(OUTPUT) STREAM OUTPUT;
       PUT FILE(OUTPUT) SKIP LIST('A line in a disk file');
       CLOSE FILE(OUTPUT);
    */

    STOP; /* Evite de poursuivre dans les corps de procedures. */

    SHOW_NUMBER: PROCEDURE(VALUE);
        DECLARE VALUE FIXED BINARY(15);
        PUT SKIP LIST('From procedure:', VALUE);
    END SHOW_NUMBER;

    /* Les procedures recursives exigent RECURSIVE. */
    FACT: PROCEDURE(N) RETURNS(FIXED BINARY(15)) RECURSIVE;
        DECLARE N FIXED BINARY(15);
        IF N <= 1 THEN RETURN(1);
        RETURN(N * FACT(N - 1));
    END FACT;
END LEARN;
```

Sur un système CP/M d'origine, enregistrez ce programme dans
`LEARN.PLI`, puis lancez `PLI LEARN`, `LINK LEARN` et `LEARN`.
Le compilateur produit un fichier objet `.REL` ; LINK-80 crée
un exécutable `.COM`. Ces commandes sont propres aux outils
Digital Research.

## Ce qui distingue PL/I

### Pointeurs et gestion explicite de la mémoire

PL/I laisse au programmeur une grande liberté avec la mémoire.
Un `POINTER` ne porte pas le type des données qu'il désigne.
C'est la déclaration `BASED` qui décrit comment interpréter la mémoire
à cette adresse, sans pour autant l'allouer. Dans l'exemple,
`ALLOCATE ... SET(pointer)` réserve une zone, et `FREE` la libère.
On peut nommer explicitement le pointeur avec `pointer -> object`,
ou l'indiquer dans la déclaration par `BASED(pointer)`.
`ADDR(variable)` permet aussi d'accéder à une zone déjà existante.

Cette liberté a sa contrepartie : pas de ramasse-miettes. Une allocation
`BASED` reste en place jusqu'à sa libération. Autre particularité :
PL/I-80 implante les variables `AUTOMATIC` ordinaires de façon statique,
sauf dans les procédures récursives. Mieux vaut donc savoir à qui
appartient chaque zone de mémoire.

### Arithmétique décimale et conversions automatiques

PL/I ne se limite pas aux entiers binaires et aux flottants.
Il sait aussi calculer en décimal à virgule fixe. Un
`FIXED DECIMAL(7,2)` convient ainsi aux montants monétaires :
`AMOUNT = AMOUNT + 1.25;` travaille en décimal, avec deux chiffres
après la virgule, sans l'approximation des flottants binaires.

Le compilateur se charge de nombreuses conversions entre types
numériques et chaînes. C'est commode, mais pas toujours sans perte :
**PL/I-80 convertit un `FIXED DECIMAL` en `FIXED BINARY` lorsque
ces types sont mélangés dans un calcul**. La partie fractionnaire
peut alors disparaître. Si la précision compte, utilisez des
opérandes décimaux compatibles ou les conversions explicites
`DECIMAL(...)`, `BINARY(...)` et `DIVIDE(...)`. 

### Héritage de FORTRAN

On reconnaît facilement la patte de FORTRAN : `FLOAT BINARY`,
l'opérateur de puissance `**`, les boucles `DO` avec compteur,
et les fonctions `SQRT`, `SIN` ou `LOG`. Les tableaux à plusieurs
dimensions acceptent des bornes explicites : `GRID(0:2,1:3)`
compte trois lignes et trois colonnes. PL/I abandonne le découpage
historique des sources FORTRAN en colonnes fixes, mais conserve
une grande partie de son vocabulaire numérique.

### Héritage de COBOL et écriture abrégée ou explicite

L'héritage de COBOL est tout aussi visible, mais ailleurs :
arithmétique décimale, structures hiérarchiques à membres numérotés,
sorties formatées `PUT EDIT`, fichiers en mode flux ou enregistrements.

PL/I permet aussi d'écrire court ou de tout expliciter.
`DCL RATE FIXED DEC(7,2);` signifie exactement la même chose que
`DECLARE RATE FIXED DECIMAL(7,2);`. De même,
`DCL TITLE CHAR(20) VAR;` équivaut à
`DECLARE TITLE CHARACTER(20) VARYING;`.
On rencontre également `PROC`/`PROCEDURE`, `PTR`/`POINTER`,
et `INIT`/`INITIAL`. Ce ne sont pas des dialectes différents :
les formes courtes économisent de la frappe, les formes complètes
facilitent la lecture des programmes anciens.

### Subset G et héritage des grands systèmes IBM

PL/I s'inscrivait dans l'ambition du System/360 : une famille de
machines capable de servir aussi bien le calcul scientifique que
la gestion. Le projet de sous-ensemble généraliste (*Subset G*)
en a retenu un noyau plus facile à porter. Digital Research suivait
ce projet dès 1980, avant la publication de la norme ANSI X3.74-1981,
révisée en 1987.

Que restait-il d'un langage de grands systèmes sur une machine 8 bits
sous CP/M ? Pas mal de choses : gestionnaires de conditions `ON`,
procédures imbriquées, liaison `ENTRY` et `EXTERNAL`, pointeurs,
arithmétique décimale, fichiers en mode flux et enregistrements.
Même les mots-clés restent contextuels : beaucoup peuvent aussi
servir de noms de variables.

Il y a bien sûr des limites. PL/I-80 omet `DEFINED`, `LIKE`,
`FLOAT DECIMAL`, les tableaux dynamiques et l'attribut de données
`PICTURE`. La version 1.3 a ajouté le format de sortie `P(...)`
pour `PUT EDIT`, mais pas l'attribut de données correspondant.
Le stockage `CONTROLLED` et le multitâche restent également absents.
C'est un véritable sous-ensemble de PL/I, pas une reproduction
miniature de toutes les fonctions du langage IBM.

## Pour aller plus loin

- [Manuel du langage PL/I-80 (Digital Research, décembre 1980)][language].
- [Guide d'applications PL/I-80 (Digital Research, 1980)][apps].
- [Compilateur PL/I Iron Spring pour Linux](https://www.iron-spring.com/)
- [IBM Research : histoire et caractéristiques du PL/I][history].

[history]: https://research.ibm.com/publications/the-early-history-and-characteristics-of-pli
[language]: https://ftpmirror.your.org/pub/misc/bitsavers/pdf/digitalResearch/pl1/PL1-80_Language_Manual_Dec80.pdf
[apps]: https://ftpmirror.your.org/pub/misc/bitsavers/pdf/digitalResearch/pl1/PL1-80_Applications_Guide_Dec80.pdf
