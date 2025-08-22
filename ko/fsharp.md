---
name: F#
contributors:
    - ["Scott Wlaschin", "http://fsharpforfunandprofit.com/"]
filename: learnfsharp.fs
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

F#은 범용 함수형/객체 지향 프로그래밍 언어입니다. 무료이며 오픈 소스이며 Linux, Mac, Windows 등에서 실행됩니다.

컴파일 타임에 많은 오류를 포착하는 강력한 타입 시스템을 가지고 있지만, 동적 언어처럼 읽히도록 타입 추론을 사용합니다.

F#의 구문은 C 스타일 언어와 다릅니다:

* 중괄호는 코드 블록을 구분하는 데 사용되지 않습니다. 대신 들여쓰기가 사용됩니다(Python과 유사).
* 매개변수를 구분하는 데 쉼표 대신 공백이 사용됩니다.

아래 코드를 사용해 보려면 [https://try.fsharp.org](https://try.fsharp.org)로 이동하여 대화형 REPL에 붙여넣을 수 있습니다.

```fsharp
// 한 줄 주석은 이중 슬래시를 사용합니다.
(* 여러 줄 주석은 (* . . . *) 쌍을 사용합니다.

- 여러 줄 주석 끝 - *)

// ================================================
// 기본 구문
// ================================================

// ------ "변수" (하지만 실제로는 아님) ------
// "let" 키워드는 (불변) 값을 정의합니다.
let myInt = 5
let myFloat = 3.14
let myString = "hello"           // 타입이 필요하지 않음에 유의하십시오.

// 변경 가능한 변수
let mutable a=3
a <- 4 // a는 이제 4입니다.

// 다소 변경 가능한 변수
// 참조 셀은 참조 의미 체계를 사용하여 변경 가능한 값을 만들 수 있는 저장 위치입니다.
// https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/reference-cells 참조
let xRef = ref 10
printfn "%d" xRef.Value // 10
xRef.Value <- 11
printfn "%d" xRef.Value // 11

let a=[ref 0; ref 1] // 다소 변경 가능한 목록
a[0].Value <- 2

// ------ 목록 ------
let twoToFive = [2; 3; 4; 5]     // 대괄호는 세미콜론 구분 기호가 있는 목록을 만듭니다.
let oneToFive = 1 :: twoToFive   // ::는 새 첫 번째 요소가 있는 목록을 만듭니다.
// 결과는 [1; 2; 3; 4; 5]입니다.
let zeroToFive = [0; 1] @ twoToFive   // @는 두 목록을 연결합니다.

// 중요: 쉼표는 구분 기호로 절대 사용되지 않으며 세미콜론만 사용됩니다!

// ------ 함수 ------
// "let" 키워드는 명명된 함수도 정의합니다.
let square x = x * x          // 괄호가 사용되지 않음에 유의하십시오.
square 3                      // 이제 함수를 실행합니다. 다시 말하지만, 괄호가 없습니다.

let add x y = x + y           // add (x,y)를 사용하지 마십시오! 완전히 다른 의미입니다.
add 2 3                       // 이제 함수를 실행합니다.

// 여러 줄 함수를 정의하려면 들여쓰기를 사용하십시오. 세미콜론이 필요하지 않습니다.
let evens list =
   let isEven x = x % 2 = 0   // "isEven"을 하위 함수로 정의합니다. 등호 연산자는 단일 문자 "="입니다.
   List.filter isEven list    // List.filter는 라이브러리 함수입니다.
                              // 두 개의 매개변수: 부울 함수와 작업할 목록

evens oneToFive               // 이제 함수를 실행합니다.

// 우선 순위를 명확히 하기 위해 괄호를 사용할 수 있습니다. 이 예제에서는
// 먼저 두 개의 인수로 "map"을 수행한 다음 결과에 대해 "sum"을 수행합니다.
// 괄호가 없으면 "List.map"이 List.sum에 인수로 전달됩니다.
let sumOfSquaresTo100 =
   List.sum ( List.map square [1..100] )

// "|>"를 사용하여 한 작업의 출력을 다음 작업으로 파이프할 수 있습니다.
// 데이터를 파이핑하는 것은 UNIX 파이프와 유사하게 F#에서 매우 일반적입니다.

// 다음은 파이프를 사용하여 작성된 동일한 sumOfSquares 함수입니다.
let sumOfSquaresTo100piped =
   [1..100] |> List.map square |> List.sum  // "square"는 이전에 정의되었습니다.

// "fun" 키워드를 사용하여 람다(익명 함수)를 정의할 수 있습니다.
let sumOfSquaresTo100withFun =
   [1..100] |> List.map (fun x -> x * x) |> List.sum

// F#에는 "return" 키워드가 없습니다. 함수는 항상
// 사용된 마지막 표현식의 값을 반환합니다.

// ------ 패턴 매칭 ------
// Match..with..는 매우 강력한 case/switch 문입니다.
let simplePatternMatch =
   let x = "a"
   match x with
    | "a" -> printfn "x is a"
    | "b" -> printfn "x is b"
    | _ -> printfn "x is something else"   // 밑줄은 무엇이든 일치합니다.

// F#은 기본적으로 null을 허용하지 않습니다 -- Option 타입을 사용해야 합니다.
// 그런 다음 패턴 매칭을 합니다.
// Some(..) 및 None은 Nullable 래퍼와 거의 유사합니다.
let validValue = Some(99)
let invalidValue = None

// 이 예제에서 match..with는 "Some"과 "None"을 일치시키고,
// 동시에 "Some"의 값을 풉니다.
let optionPatternMatch input =
   match input with
    | Some i -> printfn "input is an int=%d" i
    | None -> printfn "input is missing"

optionPatternMatch validValue
optionPatternMatch invalidValue

// ------ 인쇄 ------
// printf/printfn 함수는 C#의
// Console.Write/WriteLine 함수와 유사합니다.
printfn "Printing an int %i, a float %f, a bool %b" 1 2.0 true
printfn "A string %s, and something generic %A" "hello" [1; 2; 3; 4]

// 데이터를 문자열로 서식 지정하기 위한 sprintf/sprintfn 함수도 있습니다.
// C#의 String.Format과 유사합니다.

// ================================================
// 함수에 대한 추가 정보
// ================================================

// F#은 진정한 함수형 언어입니다 -- 함수는 일급
// 엔티티이며 강력한 구문을 만들기 위해 쉽게 결합할 수 있습니다.

// 모듈은 함수를 함께 그룹화하는 데 사용됩니다.
// 각 중첩 모듈에 대해 들여쓰기가 필요합니다.
module FunctionExamples =

    // 간단한 덧셈 함수 정의
    let add x y = x + y

    // 함수의 기본 사용법
    let a = add 1 2
    printfn "1 + 2 = %i" a

    // 매개변수를 "굽기" 위한 부분 적용
    let add42 = add 42
    let b = add42 1
    printfn "42 + 1 = %i" b

    // 함수를 결합하기 위한 구성
    let add1 = add 1
    let add2 = add 2
    let add3 = add1 >> add2
    let c = add3 7
    printfn "3 + 7 = %i" c

    // 고차 함수
    [1..10] |> List.map add3 |> printfn "new list is %A"

    // 함수 목록 등
    let add6 = [add1; add2; add3] |> List.reduce (>>)
    let d = add6 7
    printfn "1 + 2 + 3 + 7 = %i" d

// ================================================
// 목록 및 컬렉션
// ================================================

// 세 가지 유형의 정렬된 컬렉션이 있습니다:
// * 목록은 가장 기본적인 불변 컬렉션입니다.
// * 배열은 변경 가능하며 필요할 때 더 효율적입니다.
// * 시퀀스는 지연되고 무한합니다(예: 열거자).
//
// 다른 컬렉션에는 불변 맵 및 집합이 포함됩니다.
// بالإضافة إلى جميع مجموعات .NET القياسية

module ListExamples =

    // 목록은 대괄호를 사용합니다.
    let list1 = ["a"; "b"]
    let list2 = "c" :: list1    // ::는 앞에 추가하는 것입니다.
    let list3 = list1 @ list2   // @는 연결입니다.

    // 목록 이해(생성기라고도 함)
    let squares = [for i in 1..10 do yield i * i]

    // 소수 생성기
    // - 이것은 패턴 매칭 구문에 대한 짧은 표기법을 사용합니다.
    // - (p::xs)는 목록의 '첫 번째 :: 꼬리'이며, p :: xs로도 작성할 수 있습니다.
    //   이것은 'p'(목록의 첫 번째 항목)와 일치하고 xs는 목록의 나머지 부분임을 의미합니다.
    //   이것을 'cons 패턴'이라고 합니다.
    // - 재귀를 사용할 때 필요한 'rec' 키워드를 사용합니다.
    let rec sieve = function
        | (p::xs) -> p :: sieve [ for x in xs do if x % p > 0 then yield x ]
        | []      -> []
    let primes = sieve [2..50]
    printfn "%A" primes

    // 목록에 대한 패턴 매칭
    let listMatcher aList =
        match aList with
        | [] -> printfn "the list is empty"
        | [first] -> printfn "the list has one element %A " first
        | [first; second] -> printfn "list is %A and %A" first second
        | first :: _ -> printfn "the list has more than two elements, first element %A" first

    listMatcher [1; 2; 3; 4]
    listMatcher [1; 2]
    listMatcher [1]
    listMatcher []

    // 목록을 사용한 재귀
    let rec sum aList =
        match aList with
        | [] -> 0
        | x::xs -> x + sum xs
    sum [1..10]

    // -----------------------------------------
    // 표준 라이브러리 함수
    // -----------------------------------------

    // map
    let add3 x = x + 3
    [1..10] |> List.map add3

    // filter
    let even x = x % 2 = 0
    [1..10] |> List.filter even

    // 더 많음 -- 문서 참조

module ArrayExamples =

    // 배열은 대괄호와 막대를 사용합니다.
    let array1 = [| "a"; "b" |]
    let first = array1.[0]        // 점을 사용한 인덱스 액세스

    // 배열에 대한 패턴 매칭은 목록과 동일합니다.
    let arrayMatcher aList =
        match aList with
        | [| |] -> printfn "the array is empty"
        | [| first |] -> printfn "the array has one element %A " first
        | [| first; second |] -> printfn "array is %A and %A" first second
        | _ -> printfn "the array has more than two elements"

    arrayMatcher [| 1; 2; 3; 4 |]

    // 목록과 마찬가지로 표준 라이브러리 함수

    [| 1..10 |]
    |> Array.map (fun i -> i + 3)
    |> Array.filter (fun i -> i % 2 = 0)
    |> Array.iter (printfn "value is %i. ")


module SequenceExamples =

    // 시퀀스는 중괄호를 사용합니다.
    let seq1 = seq { yield "a"; yield "b" }

    // 시퀀스는 yield를 사용할 수 있으며
    // 하위 시퀀스를 포함할 수 있습니다.
    let strange = seq {
        // "yield"는 한 요소를 추가합니다.
        yield 1; yield 2;

        // "yield!"는 전체 하위 시퀀스를 추가합니다.
        yield! [5..10]
        yield! seq {
            for i in 1..10 do
              if i % 2 = 0 then yield i }}
    // test
    strange |> Seq.toList


    // 시퀀스는 "unfold"를 사용하여 만들 수 있습니다.
    // 다음은 피보나치 수열입니다.
    let fib = Seq.unfold (fun (fst,snd) ->
        Some(fst + snd, (snd, fst + snd))) (0,1)

    // test
    let fib10 = fib |> Seq.take 10 |> Seq.toList
    printf "first 10 fibs are %A" fib10


// ================================================
// 데이터 유형
// ================================================

module DataTypeExamples =

    // 모든 데이터는 기본적으로 불변입니다.

    // 튜플은 빠른 'n 쉬운 익명 유형입니다.
    // -- 쉼표를 사용하여 튜플을 만듭니다.
    let twoTuple = 1, 2
    let threeTuple = "a", 2, true

    // 패턴 매칭하여 풀기
    let x, y = twoTuple  // x = 1, y = 2로 설정

    // ------------------------------------
    // 레코드 유형에는 명명된 필드가 있습니다.
    // ------------------------------------

    // 중괄호가 있는 "type"을 사용하여 레코드 유형을 정의합니다.
    type Person = {First:string; Last:string}

    // 중괄호가 있는 "let"을 사용하여 레코드를 만듭니다.
    let person1 = {First="John"; Last="Doe"}

    // 패턴 매칭하여 풀기
    let {First = first} = person1    // first="John"으로 설정

    // ------------------------------------
    // 공용체 유형(변형이라고도 함)에는 선택 집합이 있습니다.
    // 한 번에 하나의 경우만 유효할 수 있습니다.
    // ------------------------------------

    // 막대/파이프가 있는 "type"을 사용하여 공용체 유형을 정의합니다.
    type Temp =
        | DegreesC of float
        | DegreesF of float

    // 경우 중 하나를 사용하여 하나를 만듭니다.
    let temp1 = DegreesF 98.6
    let temp2 = DegreesC 37.0

    // 모든 경우에 대해 패턴 매칭하여 풀기
    let printTemp = function
       | DegreesC t -> printfn "%f degC" t
       | DegreesF t -> printfn "%f degF" t

    printTemp temp1
    printTemp temp2

    // ------------------------------------
    // 재귀 유형
    // ------------------------------------

    // 유형은 하위 클래스를 만들지 않고도 복잡한 방식으로 재귀적으로 결합할 수 있습니다.
    type Employee =
      | Worker of Person
      | Manager of Employee list

    let jdoe = {First="John"; Last="Doe"}
    let worker = Worker jdoe

    // ------------------------------------
    // 유형을 사용한 모델링
    // ------------------------------------

    // 공용체 유형은 플래그를 사용하지 않고 상태를 모델링하는 데 적합합니다.
    type EmailAddress =
        | ValidEmailAddress of string
        | InvalidEmailAddress of string

    let trySendEmail email =
        match email with // 패턴 매칭 사용
        | ValidEmailAddress address -> ()   // 보내기
        | InvalidEmailAddress address -> () // 보내지 않기

    // 공용체 유형과 레코드 유형의 조합은
    // 도메인 기반 설계를 위한 훌륭한 기반을 제공합니다.
    // 도메인을 정확하게 반영하는 수백 개의 작은 유형을 만들 수 있습니다.

    type CartItem = { ProductCode: string; Qty: int }
    type Payment = Payment of float
    type ActiveCartData = { UnpaidItems: CartItem list }
    type PaidCartData = { PaidItems: CartItem list; Payment: Payment}

    type ShoppingCart =
        | EmptyCart  // 데이터 없음
        | ActiveCart of ActiveCartData
        | PaidCart of PaidCartData

    // ------------------------------------
    // 유형에 대한 내장 동작
    // ------------------------------------

    // 핵심 유형에는 코딩이 필요 없는 유용한 "기본" 동작이 있습니다.
    // * 불변성
    // * 디버깅 시 예쁜 인쇄
    // * 같음 및 비교
    // * 직렬화

    // %A를 사용한 예쁜 인쇄
    printfn "twoTuple=%A,\nPerson=%A,\nTemp=%A,\nEmployee=%A"
             twoTuple person1 temp1 worker

    // 내장된 같음 및 비교.
    // 다음은 카드 예제입니다.
    type Suit = Club | Diamond | Spade | Heart
    type Rank = Two | Three | Four | Five | Six | Seven | Eight
                | Nine | Ten | Jack | Queen | King | Ace

    let hand = [ Club, Ace; Heart, Three; Heart, Ace;
                 Spade, Jack; Diamond, Two; Diamond, Ace ]

    // 정렬
    List.sort hand |> printfn "sorted hand is (low to high) %A"
    List.max hand |> printfn "high card is %A"
    List.min hand |> printfn "low card is %A"


// ================================================
// 활성 패턴
// ================================================

module ActivePatternExamples =

    // F#에는 패턴이 동적으로 구문 분석되거나 감지될 수 있는 "활성 패턴"이라는 특수 유형의 패턴 매칭이 있습니다.

    // "바나나 클립"은 활성 패턴의 구문입니다.

    // 조건식에서 "else if" 대신 "elif"를 사용할 수 있습니다.
    // F#에서는 동일합니다.

    // 예를 들어, 문자 유형과 일치하는 "활성" 패턴을 정의합니다...
    let (|Digit|Letter|Whitespace|Other|) ch =
       if System.Char.IsDigit(ch) then Digit
       elif System.Char.IsLetter(ch) then Letter
       elif System.Char.IsWhiteSpace(ch) then Whitespace
       else Other

    // ... 그런 다음 구문 분석 논리를 훨씬 더 명확하게 만들기 위해 사용합니다.
    let printChar ch =
      match ch with
      | Digit -> printfn "%c is a Digit" ch
      | Letter -> printfn "%c is a Letter" ch
      | Whitespace -> printfn "%c is a Whitespace" ch
      | _ -> printfn "%c is something else" ch

    // 목록 인쇄
    ['a'; 'b'; '1'; ' '; '-'; 'c'] |> List.iter printChar

    // -----------------------------------
    // 활성 패턴을 사용한 FizzBuzz
    // -----------------------------------

    // 부분 일치 패턴도 만들 수 있습니다.
    // 정의에서 밑줄을 사용하고 일치하면 Some을 반환하십시오.
    let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
    let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None

    // 주 함수
    let fizzBuzz i =
      match i with
      | MultOf3 & MultOf5 -> printf "FizzBuzz, "
      | MultOf3 -> printf "Fizz, "
      | MultOf5 -> printf "Buzz, "
      | _ -> printf "%i, " i

    // test
    [1..20] |> List.iter fizzBuzz

// ================================================
// 간결함
// ================================================

module AlgorithmExamples =

    // F#은 신호/잡음비가 높으므로 코드는
    // 실제 알고리즘과 거의 같습니다.

    // ------ 예제: sumOfSquares 함수 정의 ------
    let sumOfSquares n =
       [1..n]              // 1) 1에서 n까지의 모든 숫자를 가져옵니다.
       |> List.map square  // 2) 각각을 제곱합니다.
       |> List.sum         // 3) 결과를 합산합니다.

    // test
    sumOfSquares 100 |> printfn "Sum of squares = %A"

    // ------ 예제: 정렬 함수 정의 ------
    let rec sort list =
       match list with
       // 목록이 비어 있으면
       | [] ->
            []                            // 빈 목록을 반환합니다.
       // 목록이 비어 있지 않으면
       | firstElem::otherElements ->      // 첫 번째 요소를 가져옵니다.
            let smallerElements =         // 더 작은 요소를 추출합니다.
                otherElements             // 나머지 요소에서
                |> List.filter (fun e -> e < firstElem)
                |> sort                   // 그리고 정렬합니다.
            let largerElements =          // 더 큰 요소를 추출합니다.
                otherElements             // 나머지 요소에서
                |> List.filter (fun e -> e >= firstElem)
                |> sort                   // 그리고 정렬합니다.
            // 3개 부분을 새 목록으로 결합하고 반환합니다.
            List.concat [smallerElements; [firstElem]; largerElements]

    // test
    sort [1; 5; 23; 18; 9; 1; 3] |> printfn "Sorted = %A"

// ================================================
// 비동기 코드
// ================================================

module AsyncExample =

    // F#에는 "파멸의 피라미드"를 만나지 않고도 비동기 코드를 지원하는 내장 기능이 있습니다.
    //
    // 다음 예제는 웹 페이지 집합을 병렬로 다운로드합니다.

    open System.Net
    open System
    open System.IO
    open Microsoft.FSharp.Control.CommonExtensions

    // URL의 내용을 비동기적으로 가져옵니다.
    let fetchUrlAsync url =
        async {   // "async" 키워드 및 중괄호
                  // "async" 개체를 만듭니다.
            let req = WebRequest.Create(Uri(url))
            use! resp = req.AsyncGetResponse()
                // use!는 비동기 할당입니다.
            use stream = resp.GetResponseStream()
                // "use"는 범위 끝에서 리소스에 대해 자동 close()를 트리거합니다.
            use reader = new IO.StreamReader(stream)
            let html = reader.ReadToEnd()
            printfn "finished downloading %s" url
            }

    // 가져올 사이트 목록
    let sites = ["http://www.bing.com";
                 "http://www.google.com";
                 "http://www.microsoft.com";
                 "http://www.amazon.com";
                 "http://www.yahoo.com"]

    // do it
    sites
    |> List.map fetchUrlAsync  // 비동기 작업 목록 만들기
    |> Async.Parallel          // 병렬로 실행할 작업 설정
    |> Async.RunSynchronously  // 시작

// ================================================
// .NET 호환성
// ================================================

module NetCompatibilityExamples =

    // F#은 C#이 할 수 있는 거의 모든 것을 할 수 있으며, .NET 또는 Mono 라이브러리와 원활하게 통합됩니다.

    // ------- 기존 라이브러리 함수 작업  -------

    let (i1success, i1) = System.Int32.TryParse("123");
    if i1success then printfn "parsed as %i" i1 else printfn "parse failed"

    // ------- 즉시 인터페이스 구현! ------- 

    // IDisposable을 구현하는 새 개체 만들기
    let makeResource name =
       { new System.IDisposable
         with member this.Dispose() = printfn "%s disposed" name }

    let useAndDisposeResources =
        use r1 = makeResource "first resource"
        printfn "using first resource"
        for i in [1..3] do
            let resourceName = sprintf "\tinner resource %d" i
            use temp = makeResource resourceName
            printfn "\tdo something with %s" resourceName
        use r2 = makeResource "second resource"
        printfn "using second resource"
        printfn "done."

    // ------- 객체 지향 코드 ------- 

    // F#은 또한 완전한 OO 언어입니다.
    // 클래스, 상속, 가상 메서드 등을 지원합니다.

    // 제네릭 유형이 있는 인터페이스
    type IEnumerator<'a> =
        abstract member Current : 'a
        abstract MoveNext : unit -> bool

    // 가상 메서드가 있는 추상 기본 클래스
    [<AbstractClass>]
    type Shape() =
        // 읽기 전용 속성
        abstract member Width : int with get
        abstract member Height : int with get
        // 비가상 메서드
        member this.BoundingArea = this.Height * this.Width
        // 기본 구현이 있는 가상 메서드
        abstract member Print : unit -> unit
        default this.Print () = printfn "I'm a shape"

    // 기본 클래스에서 상속하고 재정의하는 구체적인 클래스
    type Rectangle(x:int, y:int) =
        inherit Shape()
        override this.Width = x
        override this.Height = y
        override this.Print ()  = printfn "I'm a Rectangle"

    // test
    let r = Rectangle(2, 3)
    printfn "The width is %i" r.Width
    printfn "The area is %i" r.BoundingArea
    r.Print()

    // ------- 확장 메서드  -------

    // C#과 마찬가지로 F#은 확장 메서드로 기존 클래스를 확장할 수 있습니다.
    type System.String with
       member this.StartsWithA = this.StartsWith "A"

    // test
    let s = "Alice"
    printfn "'%s' starts with an 'A' = %A" s s.StartsWithA

    // ------- 이벤트  -------

    type MyButton() =
        let clickEvent = new Event<_>()

        [<CLIEvent>]
        member this.OnClick = clickEvent.Publish

        member this.TestEvent(arg) =
            clickEvent.Trigger(this, arg)

    // test
    let myButton = new MyButton()
    myButton.OnClick.Add(fun (sender, arg) ->
            printfn "Click event with arg=%O" arg)

    myButton.TestEvent("Hello World!")
```

## 추가 정보

F#에 대한 더 많은 데모를 보려면 [왜 F#을 사용해야 하는가](http://fsharpforfunandprofit.com/why-use-fsharp/) 시리즈를 참조하십시오.

[fsharp.org](http://fsharp.org/) 및 [dotnet의 F# 페이지](https://dotnet.microsoft.com/languages/fsharp)에서 F#에 대해 자세히 알아보십시오.