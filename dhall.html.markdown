---
language: Dhall
filename: learndhall.dhall
contributors:
    - ["Gabriel Gonzalez", "http://www.haskellforall.com/"]
---

Dhall is a programmable configuration language that provides a non-repetitive
alternative to YAML.

You can think of Dhall as: JSON + functions + types + imports

Note that while Dhall is programmable, Dhall is not Turing-complete.  Many
of Dhall's features take advantage of this restriction to provide stronger
safety guarantees and more powerful tooling.

```haskell
-- Single-line comment

{- Multi-line comment

   Unicode is fine 🙂

   This file is a valid Dhall expression that evaluates to a large record
   collecting the results of each step.

   You can view the results by interpreting the file:

       $ dhall --file learndhall.dhall

   {- Comments can be nested -}
-}

let greeting = "Hello, world!"

let fruits = "🍋🍓🍍🍉🍌"

let interpolation = "Enjoy some delicious fruit: ${fruits}"

let multilineText {- Inline comments work, too -} =
        ''
        Leading whitespace is stripped from multi-line text literals.

        That means you can freely indent or dedent a text literal without
        changing the result.

            Relative indentation within the literal is still preserved.

        Other than that, the text literal is preserved verbatim, similar to a
        "literal" YAML multiline string.
        ''

let bool = True

-- Type annotations on bindings are optional, but helpful, so we'll use them
let annotation : Bool = True

let renderedBool : Text = if bool then "True" else "False"

-- Natural numbers are non-negative and are unsigned
let naturalNumber : Natural = 42

-- Integers may be negative, but require an explicit sign, even if positive
let positiveInteger : Integer = +1

let negativeInteger : Integer = -12

let pi : Double = 3.14159265359

{- You can use a wider character range for identifiers (such as quotation
   marks and whitespace) if you quote them using backticks
-}
let `Avogadro's Number` : Double = 6.0221409e+23

let origin : { x : Double, y : Double } = { x = 0.0, y = 0.0 }

let somePrimes : List Natural = [ 2, 3, 5, 7, 11 ]

{- A schema is the same thing as a type

   Types begin with an uppercase letter by convention, but this convention is
   not enforced
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

let john : Profile =
        { person =
            { name = "John Doe"
            , age  = 67
            }
        , address =
            { country = "United States"
            , state   = "Pennsylvania"
            , city    = "Philadelphia"
            }
        }

let philadelphia : Text = john.address.city

{- Enum alternatives also begin with an uppercase letter by convention.  This
   convention is not enforced
-}
let DNA : Type = < Adenine | Cytosine | Guanine | Thymine >

let dnaSequence : List DNA = [ DNA.Thymine, DNA.Guanine, DNA.Guanine ]

let compactDNASequence : List DNA =
        let a = DNA.Adenine
        let c = DNA.Cytosine
        let g = DNA.Guanine
        let t = DNA.Thymine
        in  [ c, t, t, a, t, c, g, g, c ]

-- You can transform enums by providing a record with one field per alternative
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

{- `Natural -> List Natural` is the type of a function whose input type is a
   `Natural` and whose output type is a `List Natural`

   All functions in Dhall are anonymous functions (a.k.a. "lambdas"),
   which you can optionally give a name

   For example, the following function is equivalent to this Python code:

       lambda n : [ n, n + 1 ]

   ... and this JavaScript code:

       function (n) { return [ n, n + 1 ]; }
-}
let exampleFunction : Natural -> List Natural =
        \(n : Natural) -> [ n, n + 1 ]

-- Dhall also supports Unicode syntax, but this tutorial will stick to ASCII
let unicodeFunction : Natural → List Natural =
        λ(n : Natural) → [ n, n + 1 ]

-- You don't need to parenthesize function arguments
let exampleFunctionApplication : List Natural =
        exampleFunction 2

let functionOfMultipleArguments : Natural -> Natural -> List Natural =
        \(x : Natural) -> \(y : Natural) -> [ x, y ]

let functionAppliedToMultipleArguments : List Natural =
        functionOfMultipleArguments 2 3

{- Same as `exampleFunction` except we gave the function's input type a
   name: "n"
-}
let namedArgumentType : forall (n : Natural) -> List Natural =
        \(n : Natural) -> [ n, n + 1 ]

{- If you name a function's input type, you can use that name later within the
   same type

   This lets you write a function that works for more than one type of input
   (a.k.a. a "polymorphic" function)
-}
let duplicate : forall (a : Type) -> a -> List a =
        \(a : Type) -> \(x : a) -> [ x, x ] 

let duplicatedNumber : List Natural =
        duplicate Natural 2

let duplicatedBool : List Bool =
        duplicate Bool False

{- The language also has some built-in polymorphic functions, such as:

       List/head : forall (a : Type) -> List a -> Optional a
-}
let firstPrime : Optional Natural = List/head Natural somePrimes

let functionOfARecord : { x : Natural, y : Natural } -> List Natural =
        \(args : { x : Natural, y : Natural }) -> [ args.x, args.y ]

let functionAppliedToARecord : List Natural =
        functionOfARecord { x = 2, y = 5 }

{- All type conversions are explicit

   `Natural/show` is a built-in function of the following type:

       Natural/show : Natural -> Text

   ... that converts `Natural` numbers to their `Text` representation
-}
let typeConversion : Natural -> Text =
        \(age : Natural) -> "I am ${Natural/show age} years old!"

-- A "template" is the same thing as a function whose output type is `Text`
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

-- Template instantiation is the same thing as function application
let templatedLicense : Text =
        mitLicense { year = 2019, copyrightHolder = "Jane Smith" }

{- You can import expressions by URL

   Also, like Bash, you can import code from your local filesystem (not shown)

   Security-conscious users can pin remotely-imported expressions by adding a
   semantic integrity check.  The interpreter rejects any attempt to tamper with
   an expression pinned in this way.  However, behavior-preserving refactors
   of imported content will not perturb the hash.

   Imported expressions pinned in this way are also locally cached in a
   content-addressable store (typically underneath `~/.cache/dhall`)
-}
let Natural/sum : List Natural -> Natural =
      https://prelude.dhall-lang.org/Natural/sum
      sha256:33f7f4c3aff62e5ecf4848f964363133452d420dcde045784518fb59fa970037

let twentyEight : Natural = Natural/sum somePrimes

-- A "package" is the same thing as a (possibly nested) record that you can import
let Prelude = https://prelude.dhall-lang.org/package.dhall

let false : Bool = Prelude.Bool.not True

-- You can import the raw contents of a file by adding `as Text` to an import
let sourceCode : Text = https://prelude.dhall-lang.org/Bool/not as Text

-- You can import environment variables, too:
let presentWorkingDirectory = env:PWD as Text

-- You can provide a fallback expression if an import fails
let home : Optional Text = Some env:HOME ? None Text

-- Fallback expressions can contain alternative imports of their own
let possiblyCustomPrelude =
        env:DHALL_PRELUDE
      ? https://prelude.dhall-lang.org/package.dhall

{- Tie everything together by auto-generating configurations for 10 build users
   using the `generate` function:

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

-- Present all of the results in a final record
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
    , john = john
    , philadelphia = philadelphia
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

To learn more, visit the official website, which also lets you try the
language live in your browser:

* [https://dhall-lang.org](http://dhall-lang.org/)
