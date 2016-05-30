---
language: Tcl
contributors:
    - ["Poor Yorick", "http://pooryorick.com/"]
translators:
    - ["Martin Schimandl", "https://github.com/Git-Jiro"]
filename: learntcl-de.tcl
lang: de-de
---

Tcl wurde kreiert von [John Ousterhout](http://wiki.tcl.tk/John Ousterout) als
eine wiederverwendbare Script-Sprache für Chip-Design Werkzeuge die er kreiert
hat. Im Jahre 1997 wurde er mit dem [ACM Software System
Award](http://en.wikipedia.org/wiki/ACM_Software_System_Award) für Tcl
ausgezeichnet. Tcl kann sowohl als eingebettete Scipt-Sprache als auch als
allgemeine Programmier-Sprache verwendet werden. Tcl kann auch als portable
C-Bibliothek verwendet werden. Sogar in Fällen in denen die Script-Fähigkeiten
nicht nötig sind. Denn Tcl stellt Daten-Strukturen wie dynamische Zeichenketten,
Listen und Hash-Tabellen bereit. Die C-Bilbiothek stellt auch portable
Funktionen zur Verfügung: Laden von dynamischen Bibliotheken, Zeichenketten
formatierung und Code Konversion, Dateisystem Operationen, Netzwerk Operationen
und mehr.


Verschiedenste herausragende Fähigkeiten von Tcl:

* Praktische Cross-Platform Netzwerk-API

* Vollständig virtualisiertes Dateisystem

* Stapelbare I/O Kanäle

* Asynchron bis zum Kern

* Vollständige Ko-Routinen

* Robustes und einfach zu verwendendes Thread-Modell


Wenn Lisp ein Listen-Prozessor ist, dann ist TCl ein Zeichenketten-Prozessor.
Alle Werte sind Zeichenketten. Eine Liste ist ein Zeichenketten-Format. Eine
Prozedur-Definition ist ein Zeichenketten-Format. Um leistungsfähig zu sein,
werden Tcl-intern diese Zeichenketten in Strukutierter-Form gepuffert. Ein
Beispiel: Der "list" Befehl arbeitet mit diesen internen gepufferten 
Repräsentationen. Tcl kümmert sich selbständig darum die String-Repräsentationen
zu aktualisieren, falls dies im Skript benötigt werden sollten. Das Kopieren-
beim-Schreiben-Design von Tcl erlaubt es Skript-Authoren mit großen Daten-
Strukturen zu arbeiten ohne zuätzlichen Speicher-Overhead. Prozeduren werden
automatisch byte-kompiliert außer sie verwenden dynamsiche Befehle wie zum
Beispiel "uplevel", "upvar und "trace".

Es ist eine freude in Tcl zu programmieren. Hacker-Typen werden gefallen daran
finden, wenn sie Lisp, Forth oder Smalltalk interessant finden. Tcl wird auch
Ingenieuren und Wissenshaftlern gefallen die nur den Job erledigen wollen,
und zwar mit Werkzeugen die sich ihrem Willen anpassen. Bei Tcl ist jegliche
funktionalität in Befehlen ausgeführt, selbst Dinge wie Schleifen und
Mathematische-Funktionen die bei anderen Sprachen normalerweise Teil der Syntax
sind. Das erlaubt Tcl in den Hintergrund von Domänen spezischen Sprachen zu
treten die das jeweilige Projekt gerade benötigt. Die Tcl-Syntax ist sehr
leichtgewichtig. Sie ist selbst leichtgewichtiger als die Syntax von Lisp.
Tcl steht dir einfach nicht im Weg.


```tcl
#! /bin/env tclsh

################################################################################
## 1. Richtlinien 
################################################################################

# Tcl ist nicht Bash oder C! Das muss gesagt werden, denn standard Shell-Quoting
# funktioniert fast mit Tcl. Daher glauben viele sie können diese Syntax für
# Tcl übernehmen. Am Beginn funktioniert das meist, führt aber schnell zu 
# Frustrationen wenn die Skripte komplexer werden.

# Eckige-Klammern sind nur Quoting-Mechanismen, keine Code-Block-Konstruktoren
# und auch keine Listen-Konstruktoren. In Tcl gibt es diese beiden Dinge nicht.
# Eckige-Klammern werden verwendet um Spezial-Zeichen in Prozeduren zu escapen
# und in Zeichenketten die als Listen formattiert sind.

################################################################################
## 2. Syntax 
################################################################################

# Jede Zeile ist ein Befehl. Das erste Wort ist der Name des Befehls, jedes
# weitere Wort ist ein Argument des Befehls. Wörter sind begrenzt durch
# Leerzeichen. Da jedes Wort auch ein String ist, sind keine speziellen
# auszeichnungen wie Anführungs-Zeichen, Klammern oder Backslashes nötig.
# Selbst wenn Anführungs-Zeichen verwendet werden, denn sie sind ja keine
# String-Konstruktoren, sondern nur Escape-Zeichen.

set greeting1 Sal 
set greeting2 ut
set greeting3 ations


# Strichpunkte begrenzen auch Befehle
set greeting1 Sal; set greeting2 ut; set greeting3 ations 


# Das Dollar-Zeichen zeigt eine Variablen-Substitution an.
set greeting $greeting1$greeting2$greeting3


# Eckige-Klammern zeigen Befehls-Substitionen an. Das Ergebnis des Befehls wird an
# Stelle des Klammern-Ausdrucks eingefügt. Wenn man dem "set" Befehl nur den
# Namen einer Variablen übergibt, gibt er den Wert der Variablen zurück.
set greeting $greeting1$greeting2[set greeting3]


# Befehls-Substitution sollte eigentlich Script-Substitution heißen, denn ein
# komplettes Script, und nicht nur ein Befehl, kann zwischen die Eckigen-Klammern
# geschrieben werden. Der "incr" Befehl erhöht den Wert einer Variable um 1
# und gibt den neuen Wert der Variable zurück.
set greeting $greeting[
	incr i
	incr i
	incr i
]


# Der Backslash unterdrück die Bedeutung von Sonderzeichen
set amount \$16.42


# Der Backslash macht bestimmte Zeichen zu Sonderzeichen
puts lots\nof\n\n\n\n\n\nnewlines

# Ein Wort das in geschweiften Klammern eingeschlossen wurde ist von jeglichen
# speziellen Interpretationen ausgeschlossen. Eine Ausnahme bilden Backslashes
# vor geschweiften Klammern, hiermit wird die geschweifte Klammer von der Suche
# nach der schließenden geschweiften Klammer ausgeschlossen.
set somevar {
    Das ist ein literales $ Zeichen, diese geschweifte Klammer \} wird nicht
    als Ende interpretiert.
} 


# Bei einem Wort das in doppelten Anführungszeichen steht verlieren Leerzeichen
# ihre spezielle Bedeutung.
set name Neo
set greeting "Hallo, $name"


#Variablen-Namen können irgend eine Zeichenkette sein.
set {first name} New


# Die Geschweifte-Klammern-Form der Variablen-Substitution kann sehr komplexe
# Variblen-Namen handhaben.
set greeting "Hello, ${first name}"


# Der "set" Befehl kann immer anstatt einer Variablen-Substition verwendet
# werden.
set greeting "Hello, [set {first name}]"


# Mit dem Expansions-Operator "{*}" werden Wörter innerhalb eines Wortes wieder
# individuell als Teile des aktuellen Befehls behandelt.
set {*}{name Neo}

# Ist Äquivalent zu
set name Neo


# Ein Array ist eine spezielle Varible die also Kontainer für andere Variablen
# dient.
set person(name) Neo
set person(gender) male
set greeting "Hello, $person(name)"


# Ein Namensraum enthält Befehle und Variablen
namespace eval people {
    namespace eval person1 {
        variable name Neo
    }
}


#Der volle Name einer Variablen beihaltet den/die umschließenden
# Namensraum/Namensräume begrenzt durch zwei Doppelpunkte.
set greeting "Hello $people::person1::name"
```

```tcl
################################################################################
## 3. Einige Notizen 
################################################################################

# Jede weitere Funktion ist über Befehle implementiert. Von nun an kommt keine
# neue Syntax hinzu. Alles weitere das es über Tcl zu lernen gibt ist das
# Verhalten individueller Befehle und die bedeutung ihrer Argumente.


# Um einen Interpreter zu bekommen mit dem man nichts mehr machen kann, lösche
# einfach den globalen Namensraum. Das ist nicht sehr sinnvoll, zeigt aber die
# Natur von Tcl.
namespace delete ::


# Wegen des Verhaltens der Namens-Auflösung ist es sicherer den "variable"
# Befehl zu verwenden um in einem Namensraum einen Wert zu deklarieren oder
# zuzuweisen. Wenn eine Variable mit dem namen "name" bereits im globalen
# Namensraum existiert, bewirkt der "set" Befehl das der globalen Variable ein
# Wert zugewiesen wird, anstatt eine Variable im lokalen Namensraum zu erzeugen
namespace eval people {
    namespace eval person1 {
        variable name Neo
    }
}


# Es kann immer der vollständige Name einer Variable verwendet werden, falls
# gewünscht.
set people::person1::name Neo



################################################################################
## 4. Befehle 
################################################################################

# Berechnungen werde mit dem "expr" Befehl durchgeführt.
set a 3
set b 4
set c [expr {$a + $b}]

# Since "expr" performs variable substitution on its own, brace the expression
# to prevent Tcl from performing variable substitution first.  See

# Da der "expr" Befehl eigene Variablen-Substitutionen durchführt, setze den
# zu berechnenden Ausdruck in Eckige-Klammern. Das hindert Tcl daran Variablen-
# Substitutionen durchzuführen. Für Details siehe:
# "http://wiki.tcl.tk/Brace%20your%20#%20expr-essions"


# Der "expr" Befehl versteht Variablen- und Befehls-Substitutionen
set c [expr {$a + [set b]}]


# Der "expr" Befehl stellt Mathematische-Funktionen zur Verfügung.
set c [expr {pow($a,$b)}]


# Mathematische Operatoren sind als Befehle auch im Namensraum 
# ::tcl::mathop verfügbar.
::tcl::mathop::+ 5 3

# Befehle können aus anderen Namensräumen importiert werden.
namespace import ::tcl::mathop::+
set result [+ 5 3]


# Neu Befehle werden mit dem "proc" Befehl gebildet.
proc greet name {
    return "Hello, $name!"
}

#Es können mehrere Parameter spezifiziert werden.
proc greet {greeting name} {
    return "$greeting, $name!"
}


# Wie bereits erwähnt, geschwungene Klammern erzeugen keinen Code-Block.
# Jeder Wert, sogar das dritte Argument für den "proc" Befehl ist eine
# Zeichenkette. Der vorherige Befehl kann daher auch ohne
# geschwungene Klammern geschrieben werden:
proc greet greeting\ name return\ \"Hello,\ \$name!



# Wenn der letzte Parameter der literale Wert "args" ist, sammelt dieser Wert
# alle übrigen Argumente des Befehls ein wenn dieser aufgerufen wird.
proc fold {cmd args} {
    set res 0
    foreach arg $args {
        set res [$cmd $res $arg]
    }
}
fold ::tcl::mathop::* 5 3 3 ;# ->  45


# Bedingte Ausführung ist auch als Befehl implementiert
if {3 > 4} {
    puts {This will never happen}
} elseif {4 > 4} {
    puts {This will also never happen}
} else {
    puts {This will always happen}
}


# Auch Schleifen sind Befehle. Das erste, zweite und dritte Argument des "for"
# Befehls wird als mathematischer Ausdruck behandelt.
for {set i 0} {$i < 10} {incr i} {
    set res [expr {$res + $i}]
}


# Das erste Argument des "while" Befehls wird auch als mathematischer Ausdruck
# behandelt.
set i 0
while {$i < 10} {
    incr i 2
}


# Eine Liste ist eine speziell formatierte Zeichenkette. Im einfachsten Fall
# genügen Leerzeichen als Trennzeichen zwischen den einzelnen Werten.
set amounts 10\ 33\ 18 
set amount [lindex $amounts 1]


# Geschwungene Klammern und Backslashes können verwendet werden um komplexe
# Werte in einer Liste zu formatieren. Eine Liste sieht aus wie ein Skript,
# allerdings verlieren verlieren Zeilenumbrüche und Doppelüunkte ihre 
# besondere Bedeutung. Diese Funktionalität macht Tcl homoikonisch. Die
# folgende Liste enhtält drei Elemente.
set values {

    one\ two

    {three four}

    five\{six

}


# Da Listen auch Zeichenketten sind, kann man Zeichenketten-Operationen auf
# ihnen anwenden. Allerdings mit dem Risiko die Formatierung der Liste zu
# beschädigen.
set values {one two three four}
set values [string map {two \{} $values] ;# $values is no-longer a \
    properly-formatted listwell-formed list


# Der sicherste Weg korrekt formatierte Liste zu erzeugen, ist den "list"
# Befehl zu verwenden.
set values [list one \{ three four]
lappend values { } ;# Ein Leerzeichen als Element der Liste hinzufügen


# Mit "eval" können Werte als Skripts evaluiert weden.
eval {
    set name Neo
    set greeting "Hello, $name"
}


# Eine Liste kann immer an "eval" übergeben werden, solange die Liste einen
# einzigen Befehl entält.
eval {set name Neo}
eval [list set greeting "Hello, $name"]


# Daher: Wenn "eval" verwendet wird, verwende [list] um den gewünschten Befehl
# aufzubauen.
set command {set name}
lappend command {Archibald Sorbisol}
eval $command


# Es ist ein häufiger Fehler die Listen funktionen beim Aufbauen von Listen
# nicht zu verwenden.
set command {set name}
append command { Archibald Sorbisol}
eval $command ;# Hier passiert eine Fehler, denn der "set" Befehl hat nun zu \
    viele Argumente {set name Archibald Sorbisol}


# Dieser Fehler kann auch leicht beim "subst" Befehl passieren.
set replacement {Archibald Sorbisol}
set command {set name $replacement}
set command [subst $command] 
eval $command ;# The same error as before: too many arguments to "set" in \
    {set name Archibald Sorbisol}


# Die korrekte Vorgangsweise ist es den substituierten Wert mit dem "list"
# Befehl zu formatieren.
set replacement [list {Archibald Sorbisol}]
set command {set name $replacement}
set command [subst $command] 
eval $command


# Der "list" Befehl wird sehr häufig verwendet um Werte zu formatieren die
# in Tcl Skript Vorlagen substituiert werden. Es gibt dazu viele Beispiele,
# siehe unterhalb.


# Der "apply" Befehl evaluiert eine Zeichenkette als Befehl.
set cmd {{greeting name} {
    return "$greeting, $name!"
}}
apply $cmd Whaddup Neo


# Der "uplevel" Befehl evaluiert ein Skript in einem höher liegenden
Gültigkeitsbereich.
proc greet {} {
	uplevel {puts "$greeting, $name"}
}

proc set_double {varname value} {
	if {[string is double $value]} {
		uplevel [list variable $varname $value]
	} else {
		error [list {not a double} $value]
	}
}


# Der "upvar" Befehl verknüpft eine Variable im aktuellen Gültigkeitsbereich
# mit einer Variable in einem höher liegenden Gültigkeitsbereich.
proc set_double {varname value} {
	if {[string is double $value]} {
		upvar 1 $varname var
		set var $value
	} else {
		error [list {not a double} $value]
	}
}


# Werde den eingebauten "while" Befehl los.
rename ::while {}


# Definieren einen neuen "while" Befehl mit hilfe des "proc" Befehls.
# Ausführlichere Fehler-Behandlung wird dem Leser als Übung überlassen.
proc while {condition script} {
    if {[uplevel 1 [list expr $condition]]} {
        uplevel 1 $script
        tailcall [namespace which while] $condition $script
    }
}


# Der "coroutine" Befehl erzeugt einen separaten Call-Stack, zusammen mit einem
# Befehl um diesem Call-Stack zu verwenden. Der "yield" Befehl unterbricht
# die Ausführung des aktuellen Call-Stacks.
proc countdown {} {
	#send something back to the initial "coroutine" command
	yield

	set count 3 
	while {$count > 1} {
		yield [incr count -1]
	}
	return 0
}
coroutine countdown1 countdown
coroutine countdown2 countdown
puts [countdown 1] ;# -> 2 
puts [countdown 2] ;# -> 2 
puts [countdown 1] ;# -> 1 
puts [countdown 1] ;# -> 0 
puts [coundown 1] ;# -> invalid command name "countdown1"
puts [countdown 2] ;# -> 1 


```

## Referenzen

[Official Tcl Documentation](http://www.tcl.tk/man/tcl/)

[Tcl Wiki](http://wiki.tcl.tk)

[Tcl Subreddit](http://www.reddit.com/r/Tcl)
