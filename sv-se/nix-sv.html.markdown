---
language: nix
filename: learn-sv.nix
contributors:
    - ["Chris Martin", "http://chris-martin.org/"]
translators:
    - ["Edward Tjörnhammar", "http://edwtjo.me"]
lang: sv-se
---

Nix är ett enkelt funktionelt språk utvecklat för
[Nix pakethanteraren](https://nixos.org/nix/) och
[NixOS](https://nixos.org/) linuxdistributionen.

Du kan utvärdera Nix uttryck genom att använda
[nix-instantiate](https://nixos.org/nix/manual/#sec-nix-instantiate)
eller [`nix-repl`](https://github.com/edolstra/nix-repl).

```
with builtins; [

  #  Kommentarer
  #=========================================

  # Inlinekommentarer ser ut såhär.

  /* Flerradskommentarer ser ut
     såhär. */


  #  Booleaner
  #=========================================

  (true && false)               # Och
  #=> false

  (true || false)               # Eller
  #=> true

  (if 3 < 4 then "a" else "b")  # Villkorlig
  #=> "a"


  #  Heltal
  #=========================================

  # Heltal är den enda numeriska typen.

  1 0 42 (-3)       # Några heltal

  (4 + 6 + 12 - 2)  # Addition
  #=> 20

  (7 / 2)           # Division
  #=> 3


  #  Strängar
  #=========================================

  "Stränglitteraler omgärdas av raka citationstecken."

  "
    Stränglitteraler kan sträcka sig
    över flera rader.
  "

  ''
    Detta kallas för en indenterad strängliteral, omgärdad av dubbla apostrofer
    Den plockar intelligent bort ledande blanktecken.
  ''

  ''
    a
      b
  ''
  #=> "a\n  b"

  ("ab" + "cd")   # Strängkonkatenering
  #=> "abcd"

  # Antikvotering låter dig bädda in språkvärden i strängar.
  ("Din hemkatalog är ${getEnv "HOME"}")
  #=> "Din hemkatalog är /home/alice"


  #  Sökvägar
  #=========================================

  # Nix har en primitiv, inbyggd, typ för sökvägar.
  /tmp/tutorials/learn.nix

  # Relativa sökvägar förenas med sökvägen till dess definerande fils sökväg
  # vid tolkningstillfället för att skapa dess absoluta sökväg.

  tutorials/learn.nix
  #=> /the-base-path/tutorials/learn.nix

  # En sökväg måste innehålla åtminstonde ett snedstreck, så en relativ sökväg
  # till en fil i samma katalog måste ges ett "./" prefix

  ./learn.nix
  #=> /the-base-path/learn.nix

  # Divisionsoperatorn / måste omges av blanksteg om man vill att det skall
  # tolkas som heltalsdivision

  7/2        # Detta är en sökväg
  (7 / 2)    # Detta är heltalsdivision


  #  Importer
  #=========================================

  # En nix fil innehåller ett enstaka topnivåuttryck utan fria variabler.
  # Ett importuttryck evalueras till värdet på filen som den importerar.
  (import /tmp/foo.nix)

  # Importer kan också specificeras med hjälp av strängar.
  (import "/tmp/foo.nix")

  # Importsökvägar måste vara absoluta. Sökvägslitteraler härleds vid
  # tolkningstillfället så följande är ok.
  (import ./foo.nix)

  # Men detta är inte något som sker med strängar.
  (import "./foo.nix")
  #=> error: string ‘foo.nix’ doesn't represent an absolute path


  #  Let
  #=========================================

  # `let` block tillåter oss att binda värden till namn.
  (let x = "a"; in
    x + x + x)
  #=> "aaa"

  # Bindingar kan referera till varandra och deras ordning sinsemellan spelar
  # ingen roll.
  (let y = x + "b";
       x = "a"; in
    y + "c")
  #=> "abc"

  # Innre bindningar skuggar utanpåliggande bindingar.
  (let a = 1; in
    let a = 2; in
      a)
  #=> 2


  #  Funktioner
  #=========================================

  (n: n + 1)      # En lambdafunktion som lägger till 1

  ((n: n + 1) 5)  # Samma funktion applicerad på 5
  #=> 6

  # Det finns ingen syntax för direkt namngivna funktioner, istället binder man
  # dessa med `let` block som andra värden.
  (let succ = (n: n + 1); in succ 5)
  #=> 6

  # En funktion är en lambda med en parameter. Flera parameterar kan ges med
  # hjälp av currying.
  ((x: y: x + "-" + y) "a" "b")
  #=> "a-b"

  # Vi kan också ha namngivna funktionsparametrar, vilket vi kommer komma till
  # senare, efter att vi introducerat attributset.

  #  Listor
  #=========================================

  # Listor noteras med hakparenteser.

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


  #  Mängder
  #=========================================

  # Ett attributset är en oordnad mappning av strängnycklar och värden.
  { foo = [1 2]; bar = "x"; }

  # Punktoperatorn . väljer ett värde från attributset:et
  { a = 1; b = 2; }.a
  #=> 1

  # Frågeoperatorn ? testar om en nyckel är närvarande i ett attributset
  ({ a = 1; b = 2; } ? a)
  #=> true
  ({ a = 1; b = 2; } ? c)
  #=> false

  # Snedstrecksoperatorn // slår ihop två attributset:ar.
  ({ a = 1; } // { b = 2; })
  #=> { a = 1; b = 2; }

  # Värden på höger skriver över värden till vänster.
  ({ a = 1; b = 2; } // { a = 3; c = 4; })
  #=> { a = 3; b = 2; c = 4; }

  # Recursionsnyckelordet rec noterar ett rekursivt attributset (en fixpunkt)
  # i vilket attributen kan referera till varandra.
  (let a = 1; in     { a = 2; b = a; }.b)
  #=> 1
  (let a = 1; in rec { a = 2; b = a; }.b)
  #=> 2

  # Nästlade attributset:ar kan definieras bit för bit.
  {
    a.b   = 1;
    a.c.d = 2;
    a.c.e = 3;
  }.a.c
  #=> { d = 2; e = 3; }

  # Ett attributsets barn kan inte tilldelas på detta vis om attributsetet
  # självt blivit direkt tilldelat.
  {
    a = { b = 1; };
    a.c = 2;
  }
  #=> error: attribute ‘a’ already defined


  #  Bindningsintroduktion, `with`
  #=========================================

  # Det attributset vilket återfinns i ett `with` uttryck kommer få sina
  # värdebindningar introducerade i efterkommande uttryck.
  (with { a = 1; b = 2; };
    a + b)
  # => 3

  # Innre bindningar skuggar yttre bindningar.
  (with { a = 1; b = 2; };
    (with { a = 5; };
      a + b))
  #=> 7

  # Första raden av detta exempel börjar med "with builtins;" eftersom builtins
  # är ett attributset innehållande alla inbyggda hjälpfunktioner såsom
  # (length, head, tail, filter, etc.). Detta sparar oss från att hela tiden
  # referera in i det attributset:et , alltså du kan använda bara "length"
  # istället för "builtins.length".


  #  Attributsetmönster
  #=========================================

  # Attributset är användbara när vi skall skicka med flera värden till en
  # funktion.
  (args: args.x + "-" + args.y) { x = "a"; y = "b"; }
  #=> "a-b"

  # Man kan använda attributsetmönster för ökad tydlighet.
  ({x, y}: x + "-" + y) { x = "a"; y = "b"; }
  #=> "a-b"

  # Attributmönster misslyckas dock om det medskickade attributmönstret
  # innehåller extra nycklar.
  ({x, y}: x + "-" + y) { x = "a"; y = "b"; z = "c"; }
  #=> error: anonymous function called with unexpected argument ‘z’

  # Genom att lägga till ", ..." kan vi ignorera ytterliggare nycklar.
  ({x, y, ...}: x + "-" + y) { x = "a"; y = "b"; z = "c"; }
  #=> "a-b"


  #  Felmeddelanden
  #=========================================

  # `throw` gör att programtolken gör abort med dess tillhörande felmeddelande
  causes evaluation to abort with an error message.
  (2 + (throw "foo"))
  #=> error: foo

  # `tryEval` fångar kastade fel `throw`.
  (tryEval 42)
  #=> { success = true; value = 42; }
  (tryEval (2 + (throw "foo")))
  #=> { success = false; value = false; }

  # `abort` fungerar som `throw`, men är kritiskt och kan inte fångas.
  (tryEval (abort "foo"))
  #=> error: evaluation aborted with the following error message: ‘foo’

  # `assert` utvärderas till det givna värdet om dess predikat är sant.
  # annars skickar den ett fångbart fel.
  (assert 1 < 2; 42)
  #=> 42
  (assert 1 > 2; 42)
  #=> error: assertion failed at (string):1:1
  (tryEval (assert 1 > 2; 42))
  #=> { success = false; value = false; }


  #  Orenhet
  #=========================================

  # Eftersom repeterbarhet för byggen är en kritisk egenskap för
  # Nix-pakethanteraren betonas funktionell renhet i Nix-programmeringsspråket.
  # Men med det sagt existerar det källor till orenhet

  # Man kan referera till miljövariabler.
  (getEnv "HOME")
  #=> "/home/alice"

  # `trace` funktionen används för att debugga. Den skriver ut första argumentet
  # till stderr och reduceras samtidigt till det andra argumentet.
  (trace 1 2)
  #=> trace: 1
  #=> 2

  # Man kan skriva filer till Nix-store, lagringsplatsen för alla Nix-uttryck.
  # Även om detta är orent beteende är det hyfsat säkert eftersom filens
  # lagringsplats är härledd från dess innehåll och beroenden. Man kan läsa
  # filer från precis överallt. I nedanstående exempel skriver vi en fil till
  # Nix-store och sedan läser tillbaka den.

  (let filename = toFile "foo.txt" "hello!"; in
    [filename (builtins.readFile filename)])
  #=> [ "/nix/store/ayh05aay2anx135prqp0cy34h891247x-foo.txt" "hello!" ]

  # Vi kan också ladda ned filer till Nix-store.
  (fetchurl "https://example.com/package-1.2.3.tgz")
  #=> "/nix/store/2drvlh8r57f19s9il42zg89rdr33m2rm-package-1.2.3.tgz"

]
```

### Vidare Läsning (eng)

* [Nix Manual - Nix expression language]
  (https://nixos.org/nix/manual/#ch-expression-language)

* [James Fisher - Nix by example - Part 1: The Nix expression language]
  (https://medium.com/@MrJamesFisher/nix-by-example-a0063a1a4c55)

* [Susan Potter - Nix Cookbook - Nix By Example]
  (http://funops.co/nix-cookbook/nix-by-example/)
