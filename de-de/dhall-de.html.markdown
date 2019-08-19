---
language: Dhall
contributors:
    - ["Gabriel Gonzalez", "http://www.haskellforall.com/"]
translators:
    - ["Profpatsch", "http://profpatsch.de"]
filename: learndhall-de.py
lang: de-de
---

Dhall ist eine programmierbare Konfigurationssprache und bietet eine
nicht-repetetive Alternative zu YAML.

Man kann Dhall sehen als: JSON + Funktionen + Typen + Importsystem

Obwohl Dhall programmierbar ist, ist die Sprache nicht
turingvollständig. Viele von Dhalls Features benutzen diese
Einschränkung, um stärkere Sicherheitsgarantien zu bieten und besseres
Tooling zu ermöglichen.

```haskell
-- einzeiliger Kommentar

{- mehrzeiliger Kommentar

   Unicode funktioniert 🙂

   Diese Datei ist eine valide Dhall-Expression und evaluiert zu einem
   großen Record, welcher die Ergebnisse jedes Schritts beinhaltet.

   Das Ergebnis kann angezeigt werden, indem man die Datei evaluiert:

       $ dhall --file learndhall.dhall

   {- Kommentare können verschachtelt sein -}
-}

let greeting = "Hallo, Welt!"

let fruits = "🍋🍓🍍🍉🍌"

let interpolation = "Ein paar leckere Früchte: ${fruits}"

let multilineText {- Inline-Kommentare funktionieren ebenfalls -} =
        ''
        In Multiline-Text-Literals wird Whitespace am Anfang der Zeile
        entfernt.

        Das bedeutet Text kann frei eingerückt oder ausgerückt werden,
        ohne dass sich der Inhalt des Strings ändert.

            Relative Einrückungen bleiben erhalten.
 
        Ansonsten wird das Text-Literal verbatim erhalten, ähnlich
        zu “literal”-Multiline-Strings in YAML.
        ''

let bool = True

-- Typannotationen für Bindings sind optional, aber hilfreich, also
-- benutzen wir sie hier.
let annotation : Bool = True

let renderedBool : Text = if bool then "True" else "False"

-- Natürliche Zahlen sind nicht-negativ und vorzeichenlos.
let naturalNumber : Natural = 42

-- Integer können negativ sein, brauchen aber ein explizites Vorzeichen.
let positiveInteger : Integer = +1

let negativeInteger : Integer = -12

let pi : Double = 3.14159265359

{- Identifier dürfen eine große Anzahl an verschiedenen Zeichen
   beinhalten (wie z.B. Anführungszeichen oder Whitespace), wenn man
   sie mit Backticks umschließt.
-}
let `Avogadro's Number` : Double = 6.0221409e+23

let origin : { x : Double, y : Double } = { x = 0.0, y = 0.0 }

let somePrimes : List Natural = [ 2, 3, 5, 7, 11 ]

{- Ein Schema ist das gleiche wie ein Typ.

   Typnamen beginnen konventionell mit einem Großbuchstaben, was
   jedoch nicht erzwungen wird.
-}
let Profile : Type
        = { person :
              { name : Text
              , age  : Natural
              }
          , address :
              { country : Text
              , state   : Text
              , city    : Text
              }
          }

let bernd : Profile =
        { person =
            { name = "Bernd Lauert"
            , age  = 67
            }
        , address =
            { country = "Deutschland"
            , state   = "Bayern"
            , city    = "Augsburg"
            }
        }

let augsburg : Text = bernd.address.city

{- Enum-Alternativen beginnen konventionell auch mit einem
   Großbuchstaben. Das wird ebenfalls nicht erzwungen.
-}
let DNA : Type = < Adenine | Cytosine | Guanine | Thymine >

let dnaSequence : List DNA = [ DNA.Thymine, DNA.Guanine, DNA.Guanine ]

let compactDNASequence : List DNA =
        let a = DNA.Adenine
        let c = DNA.Cytosine
        let g = DNA.Guanine
        let t = DNA.Thymine
        in  [ c, t, t, a, t, c, g, g, c ]

-- Enums werden transformiert, indem man einen Record mit einem Feld
-- pro Alternative angibt.
let theLetterG : Text =
            merge
            { Adenine  = "A"
            , Cytosine = "C"
            , Guanine  = "G"
            , Thymine  = "T"
            }
            DNA.Guanine

let presentOptionalValue : Optional Natural = Some 1

let absentOptionalValue : Optional Natural = None Natural

let points : List { x : Double, y : Double } =
        [ { x = 1.1, y = -4.2 }
        , { x = 4.4, y = -3.0 }
        , { x = 8.2, y = -5.5 }
        ]

{- `Natural -> List Natural` ist der Funktionstyp mit Eingabetyp
   `Natural` und Ausgabetyp `List Natural`.

   Alle Funktionen in Dhall sind Anonyme Funktionen (aka. „Lambdas“),
   denen man optional einen Namen geben kann.

   Die folgende Funktion beispielsweise ist äquivalent zu diesem
   Python Code:

       lambda n : [ n, n + 1 ]

   ... und diesem Javascript Code:

       function (n) { return [ n, n + 1 ]; }
-}
let exampleFunction : Natural -> List Natural =
        \(n : Natural) -> [ n, n + 1 ]

-- Dhall unterstützt auch Unicode-Syntax, aber dieses Tutorial nutzt
-- die ASCII-Syntax.
let unicodeFunction : Natural → List Natural =
        λ(n : Natural) → [ n, n + 1 ]

-- Funktionsargumente brauchen keine Klammern.
let exampleFunctionApplication : List Natural =
        exampleFunction 2

let functionOfMultipleArguments : Natural -> Natural -> List Natural =
        \(x : Natural) -> \(y : Natural) -> [ x, y ]

let functionAppliedToMultipleArguments : List Natural =
        functionOfMultipleArguments 2 3

{- Wie `exampleFunction`, aber wir geben dem Eingabetypen
   einen Namen, `n`.
-}
let namedArgumentType : forall (n : Natural) -> List Natural =
        \(n : Natural) -> [ n, n + 1 ]

{- Bekommt der Eingabetyp einen Namen, kann man ihn weiter hinten in
   der gleichen Typdefinition wiederverwenden.

   Das ermöglicht Funktionen, die mit mehr als einem Eingabetypen
   arbeiten können (aka. „polymorphe“ Funktionen).
-}
let duplicate : forall (a : Type) -> a -> List a =
        \(a : Type) -> \(x : a) -> [ x, x ] 

let duplicatedNumber : List Natural =
        duplicate Natural 2

let duplicatedBool : List Bool =
        duplicate Bool False

{- Die Sprache hat auch eine handvoll eingebauter polymorpher
   Funktionen, wie zum Beispiel:

       List/head : forall (a : Type) -> List a -> Optional a
-}
let firstPrime : Optional Natural = List/head Natural somePrimes

let functionOfARecord : { x : Natural, y : Natural } -> List Natural =
        \(args : { x : Natural, y : Natural }) -> [ args.x, args.y ]

let functionAppliedToARecord : List Natural =
        functionOfARecord { x = 2, y = 5 }

{- Alle Typkonversionen sind explizit.

   `Natural/show` ist eine eingebaute Funktion mit dem Typ:

       Natural/show : Natural -> Text

   ... welche `Natural`s in ihre `Text`-Repräsentation konvertiert.
-}
let typeConversion : Natural -> Text =
        \(age : Natural) -> "Ich bin ${Natural/show age} Jahre alt!"

-- Ein „Template“ ist einfach eine Funktion mit Ausgabetyp `Text`.
let mitLicense : { year : Natural, copyrightHolder : Text } -> Text =
        \(args : { year : Natural, copyrightHolder : Text }) ->
''
Copyright ${Natural/show args.year} ${args.copyrightHolder}

Permission is hereby granted, free of charge, to any person obtaining a copy of 
this software and associated documentation files (the "Software"), to deal in 
the Software without restriction, including without limitation the rights to 
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
of the Software, and to permit persons to whom the Software is furnished to do 
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all 
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
SOFTWARE.
''

-- Template-Instanziierung ist das gleiche wie Funktionsanwendung.
let templatedLicense : Text =
        mitLicense { year = 2019, copyrightHolder = "Jane Smith" }

{- Expressions können via URL importiert werden.

   Ähnlich wie in Bash kann man Code aus dem lokalen Dateisystem
   importieren (wird nicht gezeigt).

   Sicherheitsbewusste Nutzer können via URLs importierte Expressions
   mit einem semantischen Integritätscheck versehen („pinnen“).
   Für gepinnte Imports wird der Dhall-Interpreter jeden Versuch
   vereiteln, auf der Remote-Seite die Expression zu manipulieren.
   Jedoch werden Änderungen, die den Inhalt der importierten
   Expression nicht verändern trotzdem akzeptiert.
   
   Auf diese Weise gepinnte Expressions werden auch in einem
   Content-Adressable Store lokal gecached (standardmäßig in
   `~/.cache/dhall`).
-}
let Natural/sum : List Natural -> Natural =
      https://prelude.dhall-lang.org/Natural/sum
      sha256:33f7f4c3aff62e5ecf4848f964363133452d420dcde045784518fb59fa970037

let twentyEight : Natural = Natural/sum somePrimes

-- Ein „Paket“ ist einfach ein (möglicherweise verschachtelter)
-- Record, den man importiert.
let Prelude = https://prelude.dhall-lang.org/package.dhall

let false : Bool = Prelude.Bool.not True

-- Durch das Anhängen von `as Text` wird eine Datei verbatim
-- importiert und nicht als Dhall-Code interpretiert.
let sourceCode : Text = https://prelude.dhall-lang.org/Bool/not as Text

-- Environment-Variablen können auch imortiert werden.
let presentWorkingDirectory = env:PWD as Text

-- Mit `?` kann man eine “Fallback-Expression” angeben, für den Fall
-- dass ein Import fehlschlägt.
let home : Optional Text = Some env:HOME ? None Text

-- Fallback-Expressions können auch alternative Imports enthalten.
let possiblyCustomPrelude =
        env:DHALL_PRELUDE
      ? https://prelude.dhall-lang.org/package.dhall

{- Ein ausführliches Beispiel, welches mithilfe der
   `generate`-Funktion eine Konfiguration für 10 Build-User generiert:
 
       Prelude.List.generate
           : Natural -> forall (a : Type) -> (Natural -> a) -> List a
-}
let buildUsers =
        let makeUser = \(user : Text) ->
              let home       = "/home/${user}"
              let privateKey = "${home}/.ssh/id_ed25519"
              let publicKey  = "${privateKey}.pub"
              in  { home = home
                  , privateKey = privateKey
                  , publicKey = publicKey
                  }

        let buildUser =
                \(index : Natural) -> makeUser "build${Natural/show index}"

        let Config =
              { home : Text
              , privateKey : Text
              , publicKey : Text
              }

        in  Prelude.List.generate 10 Config buildUser

-- Alle Ergebnisse in einem großen Record
in  { greeting = greeting
    , fruits = fruits
    , interpolation = interpolation
    , multilineText = multilineText
    , bool = bool
    , annotation = annotation
    , renderedBool = renderedBool
    , naturalNumber = naturalNumber
    , positiveInteger = positiveInteger
    , negativeInteger = negativeInteger
    , pi = pi
    , `Avogadro's Number` = `Avogadro's Number`
    , origin = origin
    , somePrimes = somePrimes
    , bernd = bernd
    , augsburg = augsburg
    , dnaSequence = dnaSequence
    , compactDNASequence = compactDNASequence
    , theLetterG = theLetterG
    , presentOptionalValue = presentOptionalValue
    , absentOptionalValue = absentOptionalValue
    , points = points
    , exampleFunction = exampleFunction
    , unicodeFunction = unicodeFunction
    , exampleFunctionApplication = exampleFunctionApplication
    , functionOfMultipleArguments = functionOfMultipleArguments
    , functionAppliedToMultipleArguments = functionAppliedToMultipleArguments
    , namedArgumentType = namedArgumentType
    , duplicate = duplicate
    , duplicatedNumber = duplicatedNumber
    , duplicatedBool = duplicatedBool
    , firstPrime = firstPrime
    , functionOfARecord = functionOfARecord
    , functionAppliedToARecord = functionAppliedToARecord
    , typeConversion = typeConversion
    , mitLicense = mitLicense
    , templatedLicense = templatedLicense
    , twentyEight = twentyEight
    , false = false
    , sourceCode = sourceCode
    , presentWorkingDirectory = presentWorkingDirectory
    , home = home
    , buildUsers = buildUsers
    }
```

Mehr Infos und Lernmaterialien gibt es auf der offiziellen Website
(Englisch), auf der man Dhall auf im Browser ausprobieren kann:

* [https://dhall-lang.org](http://dhall-lang.org/)
