---
language: nix
filename: learnnix-de.nix
contributors:
    - ["Chris Martin", "http://chris-martin.org/"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
lang: de-de
---

Nix ist eine simple funktionale Programmiersprache, die für den
[Nix package manager](https://nixos.org/nix/) und
[NixOS](https://nixos.org/) entwickelt wurde.

Du kannst Nix Ausdrücke evaluieren mithilfe von
[nix-instantiate](https://nixos.org/nix/manual/#sec-nix-instantiate)
oder [`nix-repl`](https://github.com/edolstra/nix-repl).

```
with builtins; [

  #  Kommentare
  #=========================================

  # Inline Kommentare sehen so aus.

  /* Multizeilen Kommentare
     sehen so aus. */


  #  Booleans
  #=========================================

  (true && false)               # Und
  #=> false

  (true || false)               # Oder
  #=> true

  (if 3 < 4 then "a" else "b")  # Bedingungen
  #=> "a"


  #  Integers
  #=========================================

  # Integers sind die einzigen numerischen Typen.

  1 0 42 (-3)       # Einige integers

  (4 + 6 + 12 - 2)  # Addition
  #=> 20

  (7 / 2)           # Division
  #=> 3


  #  Strings
  #=========================================

  "String Literale sind in Anführungszeichen."

  "
    String Literale können mehrere
    Zeilen umspannen.
  "

  ''
    Dies wird als Literal mit eingerückten String bezeichnet.
    Es entfernt intelligent führende Leerzeichen.
  ''

  ''
    a
      b
  ''
  #=> "a\n  b"

  ("ab" + "cd")   # String Konkatenation
  #=> "abcd"

  # Mit Antiquotation kannst du Werte in Strings einbetten.
  ("Dein Homeverzeichnis ist ${getEnv "HOME"}")
  #=> "Dein Homeverzeichnis ist /home/alice"


  #  Paths
  #=========================================

  # Nix besitzt einen primitiven Datentyp für Pfade
  /tmp/tutorials/learn.nix

  # Ein relativer Pfad wird beim Parsing zu einem absoluten Pfad aufgelöst,
  # relativ zu der Datei in der es auftritt.
  tutorials/learn.nix
  #=> /the-base-path/tutorials/learn.nix

  # Ein Pfad muss mindestens einen Schrägstrich enthalten. Ein Pfad für eine
  # Datei im selben Verzeichnis benötigt ein ./ Präfix.
  ./learn.nix
  #=> /the-base-path/learn.nix

  # Der / Operator muss von Leerraum umgeben sein wenn du dividieren möchtest.
  7/2        # Das ist ein Pfadliteral
  (7 / 2)    # Das ist ein Integerliteral


  #  Importe
  #=========================================

  # Eine nix Datei besitzt einen einzelnen top-level Ausdruck mit keinen freien Variablen.
  # Ein Import-Ausdruck wird zum Wert der Datei, die importiert wird, ausgewertet.
  (import /tmp/foo.nix)

  # Importe können ebenso mit Strings spezifiziert werden.
  (import "/tmp/foo.nix")

  # Import Pfade müssen absolut sein. Pfadliterale
  # sind automatisch aufgelöst, das ist ein Ordnung.
  (import ./foo.nix)

  # Jedoch passiert dies nicht mit Strings.
  (import "./foo.nix")
  #=> error: string ‘foo.nix’ doesn't represent an absolute path


  #  Let
  #=========================================

  # `let` Blöcke erlauben es uns Werte zu Variablen zu binden.
  (let x = "a"; in
    x + x + x)
  #=> "aaa"

  # Bindungen können auf sich gegenseitig verweisen. Die Reihenfolge spielt
  # keine Rolle.
  (let y = x + "b";
       x = "a"; in
    y + "c")
  #=> "abc"

  # Innere Bindungen überschatten Äußere.
  (let a = 1; in
    let a = 2; in
      a)
  #=> 2


  #  Funktionen
  #=========================================

  (n: n + 1)      # Funktion, die 1 addiert

  ((n: n + 1) 5)  # Dieselbe Funktion angewendet auf 5.
  #=> 6

  # Es gibt keine spezielle Syntax für benannte Funktionen, aber sie
  # können mit `let` Blöcken, wie jeder andere Wert auch, gebunden werden.
  (let succ = (n: n + 1); in succ 5)
  #=> 6

  # Eine Funktion hat genau ein Argument.
  # Mehrere Argumente können erreicht werden mithilfe von Currying.
  ((x: y: x + "-" + y) "a" "b")
  #=> "a-b"

  # Benannte Funktionsargumente gibt es auch. Diese werden wir einführen, nachdem wir uns Sets
  # angeschaut haben.

  #  Listen
  #=========================================

  # Listen werden durch eckige Klammern gekennzeichnet.

  (length [1 2 3 "x"])
  #=> 4

  ([1 2 3] ++ [4 5])
  #=> [1 2 3 4 5]

  (concatLists [[1 2] [3 4] [5]])
  #=> [1 2 3 4 5]

  (head [1 2 3])
  #=> 1
  (tail [1 2 3])
  #=> [2 3]

  (elemAt ["a" "b" "c" "d"] 2)
  #=> "c"

  (elem 2 [1 2 3])
  #=> true
  (elem 5 [1 2 3])
  #=> false

  (filter (n: n < 3) [1 2 3 4])
  #=> [ 1 2 ]


  #  Sets
  #=========================================

  # Ein "Set" ist eine ungeordnete Zuordnung mit Stringschlüsseln.
  { foo = [1 2]; bar = "x"; }

  # Der . Operator nimmt einen Wert aus dem Set.
  { a = 1; b = 2; }.a
  #=> 1

  # Der ? Operator testet, ob der Schlüssel in dem Set vorhanden ist.
  ({ a = 1; b = 2; } ? a)
  #=> true
  ({ a = 1; b = 2; } ? c)
  #=> false

  # Der // Operator mergt zwei Sets.
  ({ a = 1; } // { b = 2; })
  #=> { a = 1; b = 2; }

  # Werte auf der rechten Seite überschreiben die Werte auf der linken Seite.
  ({ a = 1; b = 2; } // { a = 3; c = 4; })
  #=> { a = 3; b = 2; c = 4; }

  # Das Schlüsselwort rec bezeichenet ein "rekursives Set", in dem sich Attribute
  # aufeinander beziehen können.
  (let a = 1; in     { a = 2; b = a; }.b)
  #=> 1
  (let a = 1; in rec { a = 2; b = a; }.b)
  #=> 2

  # Verschachtelte Sets können stückweise definiert werden.
  {
    a.b   = 1;
    a.c.d = 2;
    a.c.e = 3;
  }.a.c
  #=> { d = 2; e = 3; }

  # Die Nachkommen eines Attributs können in diesem Feld nicht zugeordnet werden, wenn
  # das Attribut selbst nicht zugewiesen wurde.
  {
    a = { b = 1; };
    a.c = 2;
  }
  #=> error: attribute ‘a’ already defined


  #  With
  #=========================================

  # Der Körper eines Sets Blocks wird mit der Zuordnung eines Satzes an die Variablen gebunden.
  (with { a = 1; b = 2; };
    a + b)
  # => 3

  # Innere Bindungen überschatten äußere Bindungen.
  (with { a = 1; b = 2; };
    (with { a = 5; };
      a + b))
  #=> 7

  # Die erste Linie diese Tutorials startet mit "with builtins;",
  # weil builtins ein Set mit allen eingebauten
  # Funktionen (length, head, tail, filter, etc.) umfasst.
  # Das erspart uns beispielsweise "builtins.length" zu schreiben,
  # anstatt nur "length".


  #  Set patterns
  #=========================================

  # Sets sind nützlich, wenn du mehrere Werte einer Funktion
  # übergeben musst.
  (args: args.x + "-" + args.y) { x = "a"; y = "b"; }
  #=> "a-b"

  # Dies kann mit Hilfe von Set patterns deutlicher geschrieben werden.
  ({x, y}: x + "-" + y) { x = "a"; y = "b"; }
  #=> "a-b"

  # Standardmäßig schlägt das Muster bei Sets mit zusätzlichen Schlüsseln fehl.
  ({x, y}: x + "-" + y) { x = "a"; y = "b"; z = "c"; }
  #=> error: anonymous function called with unexpected argument ‘z’

  # Durch Hinzufügen von ", ..." können zusätzliche Schlüssel ignoriert werden.
  ({x, y, ...}: x + "-" + y) { x = "a"; y = "b"; z = "c"; }
  #=> "a-b"


  #  Errors
  #=========================================

  # `throw` bewirkt, dass die Auswertung mit einer Fehlermeldung abgebrochen wird.
  (2 + (throw "foo"))
  #=> error: foo

  # `tryEval` fängt geworfene Fehler.
  (tryEval 42)
  #=> { success = true; value = 42; }
  (tryEval (2 + (throw "foo")))
  #=> { success = false; value = false; }

  # `abort` ist ähnlich wie throw, aber es ist fatal. Es kann nicht gefangen werden.
  (tryEval (abort "foo"))
  #=> error: evaluation aborted with the following error message: ‘foo’

  # `assert` evaluiert zu dem gegebenen Wert, wenn die Bedingung wahr ist, sonst
  # löst es eine abfangbare Exception aus.
  (assert 1 < 2; 42)
  #=> 42
  (assert 1 > 2; 42)
  #=> error: assertion failed at (string):1:1
  (tryEval (assert 1 > 2; 42))
  #=> { success = false; value = false; }


  #  Impurity
  #=========================================

  # Da die Wiederholbarkeit von Builds für den Nix Packetmanager entscheidend ist,
  # werden in der Nix Sprache reine funktionale Elemente betont. Es gibt aber ein paar
  # unreine Elemente.
  # Du kannst auf Umgebungsvariablen verweisen.
  (getEnv "HOME")
  #=> "/home/alice"

  # Die trace Funktion wird zum Debugging verwendet. Sie gibt das erste Argument zu stderr aus
  # und evaluiert das zweite Argument.
  (trace 1 2)
  #=> trace: 1
  #=> 2

  # Du kannst Dateien in den Nix Store schreiben. Obwohl unrein, kannst du dir relativ sicher sein,
  # dass es sicher ist, da der Dateiname aus dem Hash des Inhalts abgeleitet wird.
  # Du kannst Dateien von überall lesen. In diesem Beispiel schreiben wir Dateien in den Store
  # und lesen wieder davon.
  (let filename = toFile "foo.txt" "hello!"; in
    [filename (builtins.readFile filename)])
  #=> [ "/nix/store/ayh05aay2anx135prqp0cy34h891247x-foo.txt" "hello!" ]

  # Außerdem können wir Dateien in den Nix Store herunterladen.
  (fetchurl "https://example.com/package-1.2.3.tgz")
  #=> "/nix/store/2drvlh8r57f19s9il42zg89rdr33m2rm-package-1.2.3.tgz"

]
```

### Weitere Ressourcen

* [Nix Manual - Nix expression language]
  (https://nixos.org/nix/manual/#ch-expression-language)

* [James Fisher - Nix by example - Part 1: The Nix expression language]
  (https://medium.com/@MrJamesFisher/nix-by-example-a0063a1a4c55)

* [Susan Potter - Nix Cookbook - Nix By Example]
  (https://ops.functionalalgebra.com/nix-by-example/)
