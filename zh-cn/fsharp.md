---
name: F#
contributors:
    - ["Scott Wlaschin", "http://fsharpforfunandprofit.com/"]
translators:
    - ["Endericedragon", "http://github.com/endericedragon"]
filename: learnfsharp.fs
---

F# 是一款通用的、函数式的面向对象语言。它既开源又免费，并且在Linux、Mac和Windows等多平台上均可运行。

这款语言拥有强大的类型系统，许多错误在编译器即可被发现并随之被修复。不过，由于 F# 采用了类型推理技术（译者注：不需手动指定，而是通过上下文自动推断变量的类型），在读代码时往往给人一种动态类型语言的错觉。

需要留意的是， F# 的语法和“类C语言”有较大不同：
- 使用**缩进**来组织代码块（类似Python那样），而不是大括号。
- 使用**空格**来分隔函数的各个参数，而不是逗号。

若您想尝试运行以下代码，看看效果如何的话，请移步[https://try.fsharp.org](https://try.fsharp.org)，将代码粘贴进交互式REPL界面运行吧。

```fsharp
// 单行注释以双斜杠开头
(* 多行注释以包裹在 (* , *) 之间
多行注释至此结束- *)

// ================================================
// 基础语法
// ================================================

// ------ "变量" (实际上默认不可变) ------
// "let" 关键字可定义一个（不可变的）变量
let myInt = 5
let myFloat = 3.14
let myString = "hello" // 注意，并不需要指定类型

// 可变变量用 "mutable" 标注
let mutable a=3
a <- 4 // a现在的值是4

// 稍有不同的可变变量
// Reference cell是一种容器，允许您使用引用语义来创建可变变量（create mutable values with reference semantics）
// 译者注：和Rust的RefCell类似，提供容器内部的可变性
// 详见 https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/reference-cells
let xRef = ref 10
printfn "%d" xRef.Value // 10
xRef.Value <- 11
printfn "%d" xRef.Value // 11

let a = [ref 0; ref 1] // 装有可变元素的数组
a[0].Value <- 2

// ------ 列表Lists ------
let twoToFive = [2; 3; 4; 5]     // 使用方括号创建列表
                                 // 元素间使用**分号**分隔
let oneToFive = 1 :: twoToFive   // :: 创建新列表，并将新元素添加到开头
// 结果为 [1; 2; 3; 4; 5]
let zeroToFive = [0; 1] @ twoToFive   // @ 可合并两个列表

// 重要提示：是使用分号来分隔语句，而不是逗号！！

// ------ 函数 ------
// "let" 关键字也可以用来定义函数
let square x = x * x          // 注意，没有使用圆括号
square 3                      // 注意，运行函数时也不需要圆括号

let add x y = x + y           // 切勿将参数写成 (x, y)！
                              // 这是元组Tuple的写法（稍后介绍），
                              // 和函数参数完全不同
add 2 3                       // 调用该函数

// 使用缩进来定义一个多行的复杂函数。不需要写分号
let evens list =
   let isEven x = x % 2 = 0   // 定义**子函数** "isEven"。
                              // 注意“相等”是使用单等号'='而不是双等号"=="
   List.filter isEven list    // List.filter 是个 F# 库函数，它有两个参数：
                              // - 返回值为boolean的函数
                              // - 待处理的列表

evens oneToFive               // 调用该函数

// 圆括号可用于显式地标注计算优先级。在下述示例代码中：
// 1. 首先进行 List.map 计算，该函数具有两个参数
// 2. 然后进行 List.sum 计算，将上一步的结果作为该步的参数
// 若不写圆括号，"List.map" 就会被当作参数传递给 List.sum

let sumOfSquaresTo100 =
   List.sum ( List.map square [1..100] )

// 使用管道操作符 "|>" ，可以将上一步的输出（译者注：即返回值）输送给下一步作为输入
// 管道操作在 F# 中很常见，其语义也与 UNIX 操作系统中的管道操作非常相似。

// 使用管道操作符，可将 sumOfSquares 函数重构为如下形式：
let sumOfSquaresTo100piped =
   [1..100] |> List.map square |> List.sum  // "square" 函数就是之前定义的，求平方的函数

// 匿名函数（或作lambda函数）可用 "fun" 关键字定义
let sumOfSquaresTo100withFun =
   [1..100] |> List.map (fun x -> x * x) |> List.sum

// F# 中没有"return" 关键字，因为函数总是返回最后一个表达式的值。

// ------ 模式匹配 ------
// 模式匹配 match..with.. 是个加强版的 case/switch 
let simplePatternMatch =
   let x = "a"
   match x with
    | "a" -> printfn "x is a"
    | "b" -> printfn "x is b"
    | _ -> printfn "x is something else"   // 下划线可匹配任意值

// F# 默认不支持空值（null），您必须使用Option类型以便随后进行模式匹配
// Some(..) 和 None 均为Option类型的一种，用于表示"可能为空值"的两种情况
let validValue = Some(99)
let invalidValue = None

// 在下列示例中，match..with 不仅匹配了 "Some" 和 "None" 两种情况，
// 还同时将 "Some" 中的内容给解包出来了
let optionPatternMatch input =
   match input with
    | Some i -> printfn "input is an int=%d" i
    | None -> printfn "input is missing"

optionPatternMatch validValue
optionPatternMatch invalidValue

// ------ 打印内容 ------
// printf/printfn 函数与 C# 中的 Console.Write/WriteLine 函数类似
printfn "Printing an int %i, a float %f, a bool %b" 1 2.0 true
printfn "A string %s, and something generic %A" "hello" [1; 2; 3; 4]

// 若要按一定格式生成字符串，可以使用 sprintf/sprintfn 函数，
// 它们与 C# 中的 String.Format 函数类似

// ================================================
// 函数漫谈
// ================================================

// F# 是函数式编程语言：函数是其一等公民，通过组合不同函数可以实现强大的功能

// 模块（Modules）可以将一系列函数组织到一起
// 每个模块使用标识符（Indentation）来唯一标记
module FunctionExamples =

    // 定义一个简单的求和函数
    let add x y = x + y

    // 函数的基础使用
    let a = add 1 2
    printfn "1 + 2 = %i" a

    // 函数的“部分应用”（partial application）可事先“固化”参数
    let add42 = add 42
    let b = add42 1
    printfn "42 + 1 = %i" b

    // 函数的组合（composition）可将多个函数“串接”起来
    let add1 = add 1
    let add2 = add 2
    let add3 = add1 >> add2
    let c = add3 7
    printfn "3 + 7 = %i" c

    // 高阶函数
    [1..10] |> List.map add3 |> printfn "new list is %A"

    // 函数列表，以及 List.reduce 函数
    let add6 = [add1; add2; add3] |> List.reduce (>>)
    let d = add6 7
    printfn "1 + 2 + 3 + 7 = %i" d

// ================================================
// 列表与集合（collection）
// ================================================

// F# 内置了三种有序集合：
// * 列表（List）是最基本的、不可变的集合类型
// * 数组（Arrays）是可变的，按需使用可令程序更高效
// * 序列（Sequences）是惰性的、无限的集合类型（例如枚举器enumerator）
//
// 除此以外，还有诸如不可变哈希表、不可变哈希集合、
// 以及所有 .NET 中定义的标准集合类型等

module ListExamples =

    // 列表使用方括号
    let list1 = ["a"; "b"]
    let list2 = "c" :: list1    // :: 将元素添加到开头
    let list3 = list1 @ list2   // @ 将两个列表连接起来

    // 列表推导式（list comprehensions），也被称为生成器（generators）
    let squares = [for i in 1..10 do yield i * i]

    // 素数生成器
    // - 本例采用了模式匹配的简写语法
    // - (p::xs) 匹配列表的 “第一个元素 :: 其余所有元素” ，写成 p :: xs也可以
    //   此时，p匹配到列表的第一个元素，而其余元素被xs匹配到
    //   这种写法被称为构造模式（cons pattern）
    // - 在写递归函数时，必须使用 "rec" 关键字
    let rec sieve = function
        | (p::xs) -> p :: sieve [ for x in xs do if x % p > 0 then yield x ]
        | []      -> []
    let primes = sieve [2..50]
    printfn "%A" primes

    // 对列表进行模式匹配
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

    // 接受列表作参数的递归函数
    let rec sum aList =
        match aList with
        | [] -> 0
        | x::xs -> x + sum xs
    sum [1..10]

    // -----------------------------------------
    // 列表的标准库函数
    // -----------------------------------------

    // map
    let add3 x = x + 3
    [1..10] |> List.map add3

    // filter
    let even x = x % 2 = 0
    [1..10] |> List.filter even

    // 还有很多，详见文档

module ArrayExamples =

    // arrays use square brackets with bar
    let array1 = [| "a"; "b" |]
    let first = array1.[0]        // indexed access using dot

    // pattern matching for arrays is same as for lists
    let arrayMatcher aList =
        match aList with
        | [| |] -> printfn "the array is empty"
        | [| first |] -> printfn "the array has one element %A " first
        | [| first; second |] -> printfn "array is %A and %A" first second
        | _ -> printfn "the array has more than two elements"

    arrayMatcher [| 1; 2; 3; 4 |]

    // Standard library functions just as for List

    [| 1..10 |]
    |> Array.map (fun i -> i + 3)
    |> Array.filter (fun i -> i % 2 = 0)
    |> Array.iter (printfn "value is %i. ")


module SequenceExamples =

    // sequences use curly braces
    let seq1 = seq { yield "a"; yield "b" }

    // sequences can use yield and
    // can contain subsequences
    let strange = seq {
        // "yield" adds one element
        yield 1; yield 2;

        // "yield!" adds a whole subsequence
        yield! [5..10]
        yield! seq {
            for i in 1..10 do
              if i % 2 = 0 then yield i }}
    // test
    strange |> Seq.toList


    // Sequences can be created using "unfold"
    // Here's the fibonacci series
    let fib = Seq.unfold (fun (fst,snd) ->
        Some(fst + snd, (snd, fst + snd))) (0,1)

    // test
    let fib10 = fib |> Seq.take 10 |> Seq.toList
    printf "first 10 fibs are %A" fib10


// ================================================
// Data Types
// ================================================

module DataTypeExamples =

    // All data is immutable by default

    // Tuples are quick 'n easy anonymous types
    // -- Use a comma to create a tuple
    let twoTuple = 1, 2
    let threeTuple = "a", 2, true

    // Pattern match to unpack
    let x, y = twoTuple  // sets x = 1, y = 2

    // ------------------------------------
    // Record types have named fields
    // ------------------------------------

    // Use "type" with curly braces to define a record type
    type Person = {First:string; Last:string}

    // Use "let" with curly braces to create a record
    let person1 = {First="John"; Last="Doe"}

    // Pattern match to unpack
    let {First = first} = person1    // sets first="John"

    // ------------------------------------
    // Union types (aka variants) have a set of choices
    // Only one case can be valid at a time.
    // ------------------------------------

    // Use "type" with bar/pipe to define a union type
    type Temp =
        | DegreesC of float
        | DegreesF of float

    // Use one of the cases to create one
    let temp1 = DegreesF 98.6
    let temp2 = DegreesC 37.0

    // Pattern match on all cases to unpack
    let printTemp = function
       | DegreesC t -> printfn "%f degC" t
       | DegreesF t -> printfn "%f degF" t

    printTemp temp1
    printTemp temp2

    // ------------------------------------
    // Recursive types
    // ------------------------------------

    // Types can be combined recursively in complex ways
    // without having to create subclasses
    type Employee =
      | Worker of Person
      | Manager of Employee list

    let jdoe = {First="John"; Last="Doe"}
    let worker = Worker jdoe

    // ------------------------------------
    // Modeling with types
    // ------------------------------------

    // Union types are great for modeling state without using flags
    type EmailAddress =
        | ValidEmailAddress of string
        | InvalidEmailAddress of string

    let trySendEmail email =
        match email with // use pattern matching
        | ValidEmailAddress address -> ()   // send
        | InvalidEmailAddress address -> () // don't send

    // The combination of union types and record types together
    // provide a great foundation for domain driven design.
    // You can create hundreds of little types that accurately
    // reflect the domain.

    type CartItem = { ProductCode: string; Qty: int }
    type Payment = Payment of float
    type ActiveCartData = { UnpaidItems: CartItem list }
    type PaidCartData = { PaidItems: CartItem list; Payment: Payment}

    type ShoppingCart =
        | EmptyCart  // no data
        | ActiveCart of ActiveCartData
        | PaidCart of PaidCartData

    // ------------------------------------
    // Built in behavior for types
    // ------------------------------------

    // Core types have useful "out-of-the-box" behavior, no coding needed.
    // * Immutability
    // * Pretty printing when debugging
    // * Equality and comparison
    // * Serialization

    // Pretty printing using %A
    printfn "twoTuple=%A,\nPerson=%A,\nTemp=%A,\nEmployee=%A"
             twoTuple person1 temp1 worker

    // Equality and comparison built in.
    // Here's an example with cards.
    type Suit = Club | Diamond | Spade | Heart
    type Rank = Two | Three | Four | Five | Six | Seven | Eight
                | Nine | Ten | Jack | Queen | King | Ace

    let hand = [ Club, Ace; Heart, Three; Heart, Ace;
                 Spade, Jack; Diamond, Two; Diamond, Ace ]

    // sorting
    List.sort hand |> printfn "sorted hand is (low to high) %A"
    List.max hand |> printfn "high card is %A"
    List.min hand |> printfn "low card is %A"


// ================================================
// Active patterns
// ================================================

module ActivePatternExamples =

    // F# has a special type of pattern matching called "active patterns"
    // where the pattern can be parsed or detected dynamically.

    // "banana clips" are the syntax for active patterns

    // You can use "elif" instead of "else if" in conditional expressions.
    // They are equivalent in F#

    // for example, define an "active" pattern to match character types...
    let (|Digit|Letter|Whitespace|Other|) ch =
       if System.Char.IsDigit(ch) then Digit
       elif System.Char.IsLetter(ch) then Letter
       elif System.Char.IsWhiteSpace(ch) then Whitespace
       else Other

    // ... and then use it to make parsing logic much clearer
    let printChar ch =
      match ch with
      | Digit -> printfn "%c is a Digit" ch
      | Letter -> printfn "%c is a Letter" ch
      | Whitespace -> printfn "%c is a Whitespace" ch
      | _ -> printfn "%c is something else" ch

    // print a list
    ['a'; 'b'; '1'; ' '; '-'; 'c'] |> List.iter printChar

    // -----------------------------------
    // FizzBuzz using active patterns
    // -----------------------------------

    // You can create partial matching patterns as well
    // Just use underscore in the definition, and return Some if matched.
    let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
    let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None

    // the main function
    let fizzBuzz i =
      match i with
      | MultOf3 & MultOf5 -> printf "FizzBuzz, "
      | MultOf3 -> printf "Fizz, "
      | MultOf5 -> printf "Buzz, "
      | _ -> printf "%i, " i

    // test
    [1..20] |> List.iter fizzBuzz

// ================================================
// Conciseness
// ================================================

module AlgorithmExamples =

    // F# has a high signal/noise ratio, so code reads
    // almost like the actual algorithm

    // ------ Example: define sumOfSquares function ------
    let sumOfSquares n =
       [1..n]              // 1) take all the numbers from 1 to n
       |> List.map square  // 2) square each one
       |> List.sum         // 3) sum the results

    // test
    sumOfSquares 100 |> printfn "Sum of squares = %A"

    // ------ Example: define a sort function ------
    let rec sort list =
       match list with
       // If the list is empty
       | [] ->
            []                            // return an empty list
       // If the list is not empty
       | firstElem::otherElements ->      // take the first element
            let smallerElements =         // extract the smaller elements
                otherElements             // from the remaining ones
                |> List.filter (fun e -> e < firstElem)
                |> sort                   // and sort them
            let largerElements =          // extract the larger ones
                otherElements             // from the remaining ones
                |> List.filter (fun e -> e >= firstElem)
                |> sort                   // and sort them
            // Combine the 3 parts into a new list and return it
            List.concat [smallerElements; [firstElem]; largerElements]

    // test
    sort [1; 5; 23; 18; 9; 1; 3] |> printfn "Sorted = %A"

// ================================================
// Asynchronous Code
// ================================================

module AsyncExample =

    // F# has built-in features to help with async code
    // without encountering the "pyramid of doom"
    //
    // The following example downloads a set of web pages in parallel.

    open System.Net
    open System
    open System.IO
    open Microsoft.FSharp.Control.CommonExtensions

    // Fetch the contents of a URL asynchronously
    let fetchUrlAsync url =
        async {   // "async" keyword and curly braces
                  // creates an "async" object
            let req = WebRequest.Create(Uri(url))
            use! resp = req.AsyncGetResponse()
                // use! is async assignment
            use stream = resp.GetResponseStream()
                // "use" triggers automatic close()
                // on resource at end of scope
            use reader = new IO.StreamReader(stream)
            let html = reader.ReadToEnd()
            printfn "finished downloading %s" url
            }

    // a list of sites to fetch
    let sites = ["http://www.bing.com";
                 "http://www.google.com";
                 "http://www.microsoft.com";
                 "http://www.amazon.com";
                 "http://www.yahoo.com"]

    // do it
    sites
    |> List.map fetchUrlAsync  // make a list of async tasks
    |> Async.Parallel          // set up the tasks to run in parallel
    |> Async.RunSynchronously  // start them off

// ================================================
// .NET compatibility
// ================================================

module NetCompatibilityExamples =

    // F# can do almost everything C# can do, and it integrates
    // seamlessly with .NET or Mono libraries.

    // ------- work with existing library functions  -------

    let (i1success, i1) = System.Int32.TryParse("123");
    if i1success then printfn "parsed as %i" i1 else printfn "parse failed"

    // ------- Implement interfaces on the fly! -------

    // create a new object that implements IDisposable
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

    // ------- Object oriented code -------

    // F# is also a fully fledged OO language.
    // It supports classes, inheritance, virtual methods, etc.

    // interface with generic type
    type IEnumerator<'a> =
        abstract member Current : 'a
        abstract MoveNext : unit -> bool

    // abstract base class with virtual methods
    [<AbstractClass>]
    type Shape() =
        // readonly properties
        abstract member Width : int with get
        abstract member Height : int with get
        // non-virtual method
        member this.BoundingArea = this.Height * this.Width
        // virtual method with base implementation
        abstract member Print : unit -> unit
        default this.Print () = printfn "I'm a shape"

    // concrete class that inherits from base class and overrides
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

    // ------- extension methods  -------

    // Just as in C#, F# can extend existing classes with extension methods.
    type System.String with
       member this.StartsWithA = this.StartsWith "A"

    // test
    let s = "Alice"
    printfn "'%s' starts with an 'A' = %A" s s.StartsWithA

    // ------- 事件  -------

    type MyButton() =
        let clickEvent = new Event<_>()

        [<CLIEvent>]
        member this.OnClick = clickEvent.Publish

        member this.TestEvent(arg) =
            clickEvent.Trigger(this, arg)

    // 测试效果
    let myButton = new MyButton()
    myButton.OnClick.Add(fun (sender, arg) ->
            printfn "Click event with arg=%O" arg)

    myButton.TestEvent("Hello World!")
```

## 更多信息

欲参阅更多 F# 示例代码，请移步 [why use F#](http://fsharpforfunandprofit.com/why-use-fsharp/) 系列文章。

欲更深入了解 F# ，请移步 [fsharp.org](http://fsharp.org/) 与 [dotnet's F# page](https://dotnet.microsoft.com/languages/fsharp)。
