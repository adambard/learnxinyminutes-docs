---
name: Dhall
filename: learndhall.dhall
contributors:
    - ["Gabriel Gonzalez", "http://www.haskellforall.com/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---
Dhall은 YAML에 대한 반복적이지 않은 대안을 제공하는 프로그래밍 가능한 구성 언어입니다.

Dhall은 JSON + 함수 + 유형 + 가져오기라고 생각할 수 있습니다.

참고로 Dhall은 프로그래밍 가능하지만 튜링 완전하지는 않습니다. Dhall의 많은 기능은 더 강력한 안전 보장과 더 강력한 도구를 제공하기 위해 이 제한을 활용합니다.

```haskell
-- 한 줄 주석

{- 여러 줄 주석

   유니코드도 괜찮습니다 🙂

   이 파일은 각 단계의 결과를 수집하는 큰 레코드로 평가되는 유효한 Dhall 표현식입니다.

   파일을 해석하여 결과를 볼 수 있습니다:

       $ dhall --file learndhall.dhall

   {- 주석은 중첩될 수 있습니다 -}
-}

let greeting = "Hello, world!"

let fruits = "🍋🍓🍍🍉🍌"

let interpolation = "맛있는 과일을 즐기세요: ${fruits}"

let multilineText {- 인라인 주석도 작동합니다 -} =
        ''
        선행 공백은 여러 줄 텍스트 리터럴에서 제거됩니다.

        즉, 결과를 변경하지 않고 텍스트 리터럴을 자유롭게 들여쓰거나 내어쓸 수 있습니다.

            리터럴 내의 상대 들여쓰기는 여전히 유지됩니다.

        그 외에는 텍스트 리터럴이 "리터럴" YAML 여러 줄 문자열과 유사하게 그대로 유지됩니다.
        ''

let bool = True

-- 바인딩에 대한 유형 주석은 선택 사항이지만 유용하므로 사용하겠습니다.
let annotation : Bool = True

let renderedBool : Text = if bool then "True" else "False"

-- 자연수는 음수가 아니며 부호가 없습니다.
let naturalNumber : Natural = 42

-- 정수는 음수일 수 있지만, 양수이더라도 명시적인 부호가 필요합니다.
let positiveInteger : Integer = +1

let negativeInteger : Integer = -12

let pi : Double = 3.14159265359

{- 백틱을 사용하여 식별자(예: 따옴표 및 공백)에 대해 더 넓은 문자 범위를 사용할 수 있습니다.
-}
let `Avogadro's Number` : Double = 6.0221409e+23

let origin : { x : Double, y : Double } = { x = 0.0, y = 0.0 }

let somePrimes : List Natural = [ 2, 3, 5, 7, 11 ]

{- 스키마는 유형과 동일합니다.

   유형은 관례상 대문자로 시작하지만, 이 관례는 강제되지 않습니다.
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

{- Enum 대안은 관례상 대문자로 시작합니다. 이 관례는 강제되지 않습니다.
-}
let DNA : Type = < Adenine | Cytosine | Guanine | Thymine >

let dnaSequence : List DNA = [ DNA.Thymine, DNA.Guanine, DNA.Guanine ]

let compactDNASequence : List DNA =
        let a = DNA.Adenine
        let c = DNA.Cytosine
        let g = DNA.Guanine
        let t = DNA.Thymine
        in  [ c, t, t, a, t, c, g, g, c ]

-- 각 대안당 하나의 필드를 가진 레코드를 제공하여 열거형을 변환할 수 있습니다.
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

{- `Natural -> List Natural`은 입력 유형이 `Natural`이고
   출력 유형이 `List Natural`인 함수의 유형입니다.

   Dhall의 모든 함수는 익명 함수(일명 "람다")이며,
   선택적으로 이름을 지정할 수 있습니다.

   예를 들어, 다음 함수는 이 Python 코드와 동일합니다:

       lambda n : [ n, n + 1 ]

   ... 그리고 이 JavaScript 코드:

       function (n) { return [ n, n + 1 ]; }
-}
let exampleFunction : Natural -> List Natural =
        \(n : Natural) -> [ n, n + 1 ]

-- Dhall은 유니코드 구문도 지원하지만, 이 튜토리얼에서는 ASCII를 사용합니다.
let unicodeFunction : Natural → List Natural =
        λ(n : Natural) → [ n, n + 1 ]

-- 함수 인수를 괄호로 묶을 필요가 없습니다.
let exampleFunctionApplication : List Natural =
        exampleFunction 2

let functionOfMultipleArguments : Natural -> Natural -> List Natural =
        \(x : Natural) -> \(y : Natural) -> [ x, y ]

let functionAppliedToMultipleArguments : List Natural =
        functionOfMultipleArguments 2 3

{- `exampleFunction`과 동일하지만 함수의 입력 유형에 이름을 지정했습니다:
   "n"
-}
let namedArgumentType : forall (n : Natural) -> List Natural =
        \(n : Natural) -> [ n, n + 1 ]

{- 함수의 입력 유형에 이름을 지정하면 동일한 유형 내에서 나중에 해당 이름을 사용할 수 있습니다.

   이렇게 하면 둘 이상의 입력 유형에 대해 작동하는 함수를 작성할 수 있습니다.
   (일명 "다형성" 함수)
-}
let duplicate : forall (a : Type) -> a -> List a =
        \(a : Type) -> \(x : a) -> [ x, x ]

let duplicatedNumber : List Natural =
        duplicate Natural 2

let duplicatedBool : List Bool =
        duplicate Bool False

{- 언어에는 다음과 같은 일부 내장 다형성 함수도 있습니다:

       List/head : forall (a : Type) -> List a -> Optional a
-}
let firstPrime : Optional Natural = List/head Natural somePrimes

let functionOfARecord : { x : Natural, y : Natural } -> List Natural =
        \(args : { x : Natural, y : Natural }) -> [ args.x, args.y ]

let functionAppliedToARecord : List Natural =
        functionOfARecord { x = 2, y = 5 }

{- 모든 유형 변환은 명시적입니다.

   `Natural/show`는 다음 유형의 내장 함수입니다:

       Natural/show : Natural -> Text

   ... `Natural` 숫자를 `Text` 표현으로 변환합니다.
-}
let typeConversion : Natural -> Text =
        \(age : Natural) -> "저는 ${Natural/show age}살입니다!"

-- "템플릿"은 출력 유형이 `Text`인 함수와 동일합니다.
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

-- 템플릿 인스턴스화는 함수 적용과 동일합니다.
let templatedLicense : Text =
        mitLicense { year = 2019, copyrightHolder = "Jane Smith" }

{- URL로 표현식을 가져올 수 있습니다.

   또한 Bash와 마찬가지로 로컬 파일 시스템에서 코드를 가져올 수 있습니다(표시되지 않음).

   보안에 민감한 사용자는 의미론적 무결성 검사를 추가하여 원격으로 가져온 표현식을 고정할 수 있습니다. 이렇게 고정된 표현식을 조작하려는 시도는 인터프리터에서 거부됩니다. 그러나 가져온 콘텐츠의 동작 보존 리팩토링은 해시를 변경하지 않습니다.

   이렇게 고정된 가져온 표현식은 콘텐츠 주소 지정 가능 저장소(일반적으로 `~/.cache/dhall` 아래)에 로컬로 캐시됩니다.
-}
let Natural/sum : List Natural -> Natural =
      https://prelude.dhall-lang.org/Natural/sum
      sha256:33f7f4c3aff62e5ecf4848f964363133452d420dcde045784518fb59fa970037

let twentyEight : Natural = Natural/sum somePrimes

-- "패키지"는 가져올 수 있는 (중첩될 수 있는) 레코드와 동일합니다.
let Prelude = https://prelude.dhall-lang.org/package.dhall

let false : Bool = Prelude.Bool.not True

-- `as Text`를 가져오기에 추가하여 파일의 원시 내용을 가져올 수 있습니다.
let sourceCode : Text = https://prelude.dhall-lang.org/Bool/not as Text

-- 환경 변수도 가져올 수 있습니다:
let presentWorkingDirectory = env:PWD as Text

-- 가져오기가 실패할 경우 대체 표현식을 제공할 수 있습니다.
let home : Optional Text = Some env:HOME ? None Text

-- 대체 표현식은 자체적으로 대체 가져오기를 포함할 수 있습니다.
let possiblyCustomPrelude =
        env:DHALL_PRELUDE
      ? https://prelude.dhall-lang.org/package.dhall

{- `generate` 함수를 사용하여 10명의 빌드 사용자에 대한 구성을 자동 생성합니다:

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

-- 모든 결과를 최종 레코드에 표시합니다.
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

더 자세히 알아보려면 공식 웹사이트를 방문하십시오. 브라우저에서 언어를 직접 사용해 볼 수도 있습니다:

* [https://dhall-lang.org](http://dhall-lang.org/)