---
category: tool
tool: make
contributors:
    - ["Robert Steed", "https://github.com/robochat"]
    - ["Stephan Fuhrmann", "https://github.com/sfuhrm"]
translators:
  - ["Martin Schimandl", "https://github.com/Git-Jiro"]
filename: Makefile-de
lang: de-de
---

Eine Makefile definiert einen Graphen von Regeln um ein Ziel (oder Ziele)
zu erzeugen. Es dient dazu, die geringste Menge an Arbeit zu verrichten um
ein Ziel in Einklang mit dem Quellcode zu bringen. Make wurde berühmterweise
von Stuart Feldman 1976 übers Wochenende geschrieben. Make ist noch immer
sehr verbreitet (vorallem im Unix Umfeld) obwohl es bereits sehr viel
Konkurrenz und Kritik zu Make gibt.

Es gibt eine Vielzahl an Varianten von Make, dieser Artikel beschäftigt sich
mit der Version GNU Make. Diese Version ist Standard auf Linux.

```make

# Kommentare können so geschrieben werden.

# Dateien sollten Makefile heißen, denn dann können sie als `make <ziel>`
# aufgerufen werden. Ansonsten muss `make -f "dateiname" <ziel>` verwendet
# werden.

# Warnung - Es sollten nur TABULATOREN zur Einrückung im Makefile verwendet
# werden. Niemals Leerzeichen!

#-----------------------------------------------------------------------
# Grundlagen
#-----------------------------------------------------------------------

# Eine Regel - Diese Regel wird nur abgearbeitet wenn die Datei file0.txt
# nicht existiert.
file0.txt:
	echo "foo" > file0.txt
	# Selbst Kommentare in der 'Rezept' Sektion werden an die Shell
	# weitergegeben. Versuche `make file0.txt` oder einfach `make`
	# die erste Regel ist die Standard-Regel.


# Diese Regel wird nur abgearbeitet, wenn file0.txt aktueller als file1.txt ist.
file1.txt: file0.txt
	cat file0.txt > file1.txt
	# Verwende die selben Quoting-Regeln wie die Shell
	@cat file0.txt >> file1.txt
	# @ unterdrückt die Ausgabe des Befehls an stdout.
	-@echo 'hello'
	# - bedeutet, dass Make die Abarbeitung fortsetzt auch wenn Fehler
    # passieren.
	# Versuche `make file1.txt` auf der Kommandozeile.

# Eine Regel kann mehrere Ziele und mehrere Voraussetzungen haben.
file2.txt file3.txt: file0.txt file1.txt
	touch file2.txt
	touch file3.txt

# Make wird sich beschweren, wenn es mehrere Rezepte für die gleiche Regel gibt.
# Leere Rezepte zählen nicht und können dazu verwendet werden weitere
# Voraussetzungen hinzuzufügen.

#-----------------------------------------------------------------------
# Phony-Ziele
#-----------------------------------------------------------------------

# Ein Phony-Ziel ist ein Ziel, das keine Datei ist.
# Es wird nie aktuell sein, daher wird Make immer versuchen, es abzuarbeiten
all: maker process

# Es ist erlaubt Dinge ausserhalb der Reihenfolge zu deklarieren.
maker:
	touch ex0.txt ex1.txt

# Um das Fehlschlagen von Phony-Regeln zu vermeiden wenn eine echte Datei den
# selben namen wie ein Phony-Ziel hat:
.PHONY: all maker process
# Das ist ein spezielles Ziel. Es gibt noch ein paar mehr davon.

# Eine Regel mit einem Phony-Ziel als Voraussetzung wird immer abgearbeitet
ex0.txt ex1.txt: maker

# Häufige Phony-Ziele sind: all make clean install ...

#-----------------------------------------------------------------------
# Automatische Variablen & Wildcards
#-----------------------------------------------------------------------

process: file*.txt	# Eine Wildcard um Dateinamen zu vergleichen
	@echo $^	# $^ ist eine Variable die eine Liste aller
			# Voraussetzungen enthält.
	@echo $@	# Namen des Ziels ausgeben.
	#(Bei mehreren Ziel-Regeln enthält $@ den Verursacher der Abarbeitung
	#der Regel.)
	@echo $<	# Die erste Voraussetzung aus der Liste
	@echo $?	# Nur die Voraussetzungen, die nicht aktuell sind.
	@echo $+	# Alle Voraussetzungen inklusive Duplikate (nicht wie Üblich)
	#@echo $|	# Alle 'order only' Voraussetzungen

# Selbst wenn wir die Voraussetzungen der Regel aufteilen, $^ wird sie finden.
process: ex1.txt file0.txt
# ex1.txt wird gefunden werden, aber file0.txt wird dedupliziert.

#-----------------------------------------------------------------------
# Muster
#-----------------------------------------------------------------------

# Mit Mustern kann man make beibringen wie Dateien in andere Dateien
# umgewandelt werden.

%.png: %.svg
	inkscape --export-png $^

# Muster-Vergleichs-Regeln werden nur abgearbeitet, wenn make entscheidet das
# Ziel zu erzeugen

# Verzeichnis-Pfade werden normalerweise bei Muster-Vergleichs-Regeln ignoriert.
# Aber make wird versuchen die am besten passende Regel zu verwenden.
small/%.png: %.svg
	inkscape --export-png --export-dpi 30 $^

# Make wird die letzte Version einer Muster-Vergleichs-Regel verwenden, die es
# findet.
%.png: %.svg
	@echo this rule is chosen

# Allerdings wird make die erste Muster-Vergleicher-Regel verwenden, die das
# Ziel erzeugen kann.
%.png: %.ps
	@echo this rule is not chosen if *.svg and *.ps are both present

# Make hat bereits ein paar eingebaute Muster-Vergleichs-Regelen. Zum Beispiel
# weiß Make wie man aus *.c Dateien *.o Dateien erzeugt.

# Ältere Versionen von Make verwenden möglicherweise Suffix-Regeln anstatt
# Muster-Vergleichs-Regeln.
.png.ps:
	@echo this rule is similar to a pattern rule.

# Aktivieren der Suffix-Regel
.SUFFIXES: .png

#-----------------------------------------------------------------------
# Variablen
#-----------------------------------------------------------------------
# auch Makros genannt.

# Variablen sind im Grunde genommen Zeichenketten-Typen.

name = Ted
name2="Sarah"

echo:
	@echo $(name)
	@echo ${name2}
	@echo $name    # Das funktioniert nicht, wird als $(n)ame behandelt.
	@echo $(name3) # Unbekannte Variablen werden als leere Zeichenketten behandelt.

# Es git 4 Stellen um Variablen zu setzen.
# In Reihenfolge der Priorität von höchster zu niedrigster:
# 1: Befehls-Zeilen Argumente
# 2: Makefile
# 3: Shell Umbebungs-Variablen - Make importiert diese automatisch.
# 3: MAke hat einige vordefinierte Variablen.

name4 ?= Jean
# Setze die Variable nur wenn es eine gleichnamige Umgebungs-Variable noch
# nicht gibt.

override name5 = David
# Verhindert, dass Kommando-Zeilen Argumente diese Variable ändern können.

name4 +=grey
# Werte an eine Variable anhängen (inkludiert Leerzeichen).

# Muster-Spezifische Variablen Werte (GNU Erweiterung).
echo: name2 = Sara # Wahr innerhalb der passenden Regel und auch innerhalb
	# rekursiver Voraussetzungen (ausser wenn es den Graphen zerstören
	# kann, wenn es zu kompilizert wird!)

# Ein paar Variablen, die von Make automatisch definiert werden.
echo_inbuilt:
	echo $(CC)
	echo ${CXX}
	echo $(FC)
	echo ${CFLAGS}
	echo $(CPPFLAGS)
	echo ${CXXFLAGS}
	echo $(LDFLAGS)
	echo ${LDLIBS}

#-----------------------------------------------------------------------
# Variablen 2
#-----------------------------------------------------------------------

# Der erste Typ von Variablen wird bei jeder Verwendung ausgewertet.
# Das kann aufwendig sein, daher exisitert ein zweiter Typ von Variablen.
# Diese werden nur einmal ausgewertet. (Das ist eine GNU make Erweiterung)

var := hello
var2 ::=  $(var) hello
#:= und ::= sind äquivalent.

# Diese Variablen werden prozedural ausgwertet (in der Reihenfolge in der sie
# auftauchen), die stehen daher im wiederspruch zum Rest der Sprache!

# Das funktioniert nicht
var3 ::= $(var4) and good luck
var4 ::= good night

#-----------------------------------------------------------------------
# Funktionen
#-----------------------------------------------------------------------

# Make verfügt über eine Vielzahl von Funktionen.

sourcefiles = $(wildcard *.c */*.c)
objectfiles = $(patsubst %.c,%.o,$(sourcefiles))

# Das Format ist $(func arg0,arg1,arg2...)

# Ein paar Beispiele
ls:	* src/*
	@echo $(filter %.txt, $^)
	@echo $(notdir $^)
	@echo $(join $(dir $^),$(notdir $^))

#-----------------------------------------------------------------------
# Direktiven
#-----------------------------------------------------------------------

# Inkludiere andere Makefile, sehr praktisch für platformspezifischen Code
include foo.mk

sport = tennis
# Konditionale kompiliereung
report:
ifeq ($(sport),tennis)
	@echo 'game, set, match'
else
	@echo "They think it's all over; it is now"
endif

# Es gibt auch ifneq, ifdef, ifndef

foo = true

ifdef $(foo)
bar = 'hello'
endif
```


### Mehr Resourcen

+ [gnu make documentation](https://www.gnu.org/software/make/manual/)
+ [software carpentry tutorial](http://swcarpentry.github.io/make-novice/)
+ learn C the hard way [ex2](http://c.learncodethehardway.org/book/ex2.html) [ex28](http://c.learncodethehardway.org/book/ex28.html)

