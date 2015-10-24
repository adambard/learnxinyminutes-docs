---
name: perl
category: language
language: perl
filename: learnperl-de.pl
contributors:
    - ["Korjavin Ivan", "http://github.com/korjavin"]
translators:
    - ["Martin Schimandl", "http://github.com/Git-Jiro"]
lang: de-de
---

Perl 5 ist eine sehr mächtige, funktionsreiche Programmiersprache mit über 25 Jahren Entwicklungsgeschichte.

Perl 5 läuft auf über 100 Platformen von portablen Geräten bis hin zu Mainframes. Perl 5 ist geeignet für Rapid-Prototyping und auch groß angelegte Entwicklungs-Projekte.

```perl
# Einzeilige Kommentare beginnen mit dem # Symbol.


#### Perl variablen typen

#  Variablen beginnen mit einem Sigil, das ist ein Symbol das den Typ anzeigt.
#  Ein erlaubter Variablen-Name beginnt mit einem Buchstaben oder einem
#  Unterstrich, gefolgt von beliebig vielen weiteren Buchstaben, Zahlen und
#  Unterstrichen.

### Perl hat drei Haupt-Typen von Variablen: $scalar, @array, und %hash.

## Scalare
#  Ein Scalar repräsentiert einen einzelnen Wert:
my $animal = "camel";
my $answer = 42;

# Scalare Werte könnne Zeichenketten, Ganzzahlen und Gleitkomma-Zahlen sein.
# Perl convertiert automatisch zwischen diesen Werten wenn nötig.

## Arrays
#  Ein Array repräsentiert eine Liste von Werten:
my @animals = ("camel", "llama", "owl");
my @numbers = (23, 42, 69);
my @mixed   = ("camel", 42, 1.23);



## Hashes
#   Ein Hash representiert ein Set von Schlüssel/Wert Paaren:

my %fruit_color = ("apple", "red", "banana", "yellow");

#  Man kann Leerzeichen und den "=>" Operator verwenden um sie schön darzustellen:

my %fruit_color = (
  apple  => "red",
  banana => "yellow",
);
# Scalare, Arrays und Hashes sind in perldata sehr genau dokumentiert.
# (perldoc perldata)

# Komplexere Daten-Typen können mit hilfe von Referenzen konstruiert werden.
# Dies erlaubt das erstellen von Listen und Hashes in Listen und Hashes.

#### Bedingte Ausführungs- und Schleifen-Konstrukte.

# Perl besitzt die üblichen Bedingte Ausführung- und Schleifen-Konstrukte

if ($var) {
  ...
} elsif ($var eq 'bar') {
  ...
} else {
  ...
}

unless (condition) {
  ...
}
# Dies ist die etwas leserliche Version von "if (!Bedingung)"

# Die Perl-Eigene Post-Bedingungs-Schreibweise
print "Yow!" if $zippy;
print "We have no bananas" unless $bananas;

#  while
while (condition) {
  ...
}


# Für Schleifen und Iterationen
for (my $i = 0; $i < $max; $i++) {
  print "index is $i";
}

for (my $i = 0; $i < @elements; $i++) {
  print "Current element is " . $elements[$i];
}

for my $element (@elements) {
  print $element;
}

# Implizite Iteration
for (@elements) {
  print;
}

# Die Perl-Eigene Post-Bedingungs-Schreibweise nochmals
print for @elements;

#### Reguläre Ausdrücke

# Die Unterstützung von Perl für reguläre Ausdrücke ist weit und tiefgreifend.
# Sie ist ausführlichst in perlrequick, perlretut und sonstwo dokumentiert.
# Die Kurzfassung:

# Einfaches Vergleichen
if (/foo/)       { ... }  # Wahr wenn "foo" in $_ enthalten ist
if ($a =~ /foo/) { ... }  # Wahr wenn "foo" in $a enthalten ist

# Einfache Substitution

$a =~ s/foo/bar/;         # Ersetzt foo mit bar in $a
$a =~ s/foo/bar/g;        # Ersetzt ALLE VORKOMMNISSE von foo mit bar in $a


#### Datien und Ein-/Ausgabe

# Dateien werden mit der "open()" Funktion zur Ein- oder Ausgabe geöffnet.

open(my $in,  "<",  "input.txt")  or die "Can't open input.txt: $!";
open(my $out, ">",  "output.txt") or die "Can't open output.txt: $!";
open(my $log, ">>", "my.log")     or die "Can't open my.log: $!";

# Von einem geöffneten Datei-Handle kann mit dem "<>" Operator gelesen werden.
# In einem Scalaren-Kontext liest man damit eine einzelnen Zeile vom Datei-Handle.
# In einem Listen-Kontext wird damit die komplette Datei eingelesen. Dabei
# entspricht jede Zeile einem Element der Liste:

my $line  = <$in>;
my @lines = <$in>;

#### Schreiben von Subroutinen

# Subroutinen schreiben ist einfach:

sub logger {
  my $logmessage = shift;

  open my $logfile, ">>", "my.log" or die "Could not open my.log: $!";

  print $logfile $logmessage;
}

# Nun könnne wir die Subroutine genau wie eine eingebaute Funktion verwenden:

logger("We have a logger subroutine!");
```

#### Verwenden von Perl Modulen

Perl Module lieferen eine Menge an Funktionen die dabei Helfen das Rad nicht neu erfinden zu müssen. Perl Module können von CPAN (http://www.cpan.org/) heruntergeladen werden. Einige populäre Module sind in der Perl Distribution selbst bereits enthalten.

Perlfaq enthält Fragen und Antworten zu häufig vorkommenden Aufgaben. Sehr oft sind auch Vorschläge enthalten welches CPAN module am besten geeignet ist.

#### Weiterführende Literatur

 - [perl-tutorial](http://perl-tutorial.org/)
 - [Learn at www.perl.com](http://www.perl.org/learn.html)
 - [perldoc](http://perldoc.perl.org/)
 - in Perl eingebaut : `perldoc perlintro`
