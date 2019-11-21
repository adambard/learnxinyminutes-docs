---
language: HQ9+
filename: hq9+-de.html
contributors:
    - ["Alexey Nazaroff", "https://github.com/rogaven"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
lang: de-de
---

HQ9+ ist eine Parodie auf esoterische Programmiersprachen und wurde von Cliff Biffle kreiert.
Die Sprache hat nur vier Befehle und ist nicht Turing-vollständig.

```
Es gibt nur vier Befehle, die durch die folgenden vier Zeichen dargestellt werden
H: druckt "Hello, world!"
Q: druckt den Quellcode des Programms (ein Quine)
9: druckt den Liedtext von "99 Bottles of Beer"
+: erhöhe den Akkumulator um Eins (Der Wert des Akkumulators kann nicht gelesen werden)
Jedes andere Zeichen wird ignoriert.

Ok. Lass uns ein Programm schreiben:
  HQ

Ergebnis:
  Hello world!
  HQ

HQ9+ ist zwar sehr simpel, es erlaubt aber dir Sachen zu machen, die in
anderen Sprachen sehr schwierig sind. Zum Beispiel druckt das folgende Programm
drei Mal Kopien von sich selbst auf den Bildschirm:
  QQQ
Dies druckt:
  QQQ
  QQQ
  QQQ
```

Und das ist alles. Es gibt sehr viele Interpreter für HQ9+.
Unten findest du einen von ihnen.

+ [One of online interpreters](https://almnet.de/esolang/hq9plus.php)
+ [HQ9+ official website](http://cliffle.com/esoterica/hq9plus.html)
