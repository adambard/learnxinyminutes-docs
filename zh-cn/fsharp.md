---
name: F#
contributors:
    - ["Scott Wlaschin", "http://fsharpforfunandprofit.com/"]
translators:
    - ["Endericedragon", "http://github.com/endericedragon"]
filename: learnfsharp.fs
---

F# 是一款通用的、函数式的面向对象语言。它既开源又免费，并且在Linux、Mac和Windows等多平台上均可运行。

这款语言拥有强大的类型系统，许多错误在编译期间即可被发现并随之被修复。不过，由于 F# 采用了类型推理技术（译者注：不需手动指定，而是通过上下文自动推断变量的类型），在读代码时往往给人一种动态类型语言的错觉。

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

let a = [ref 0; ref 1] // 装有可变元素的列表
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

    // 数组（Array）用 [| 和 |] 包裹
    let array1 = [| "a"; "b" |]
    let first = array1.[0]        // 用 . 来索引元素

    // 数组和列表的模式匹配语法完全相同
    let arrayMatcher aList =
        match aList with
        | [| |] -> printfn "the array is empty"
        | [| first |] -> printfn "the array has one element %A " first
        | [| first; second |] -> printfn "array is %A and %A" first second
        | _ -> printfn "the array has more than two elements"

    arrayMatcher [| 1; 2; 3; 4 |]

    // Array的标准库函数和List也几乎一样

    [| 1..10 |]
    |> Array.map (fun i -> i + 3)
    |> Array.filter (fun i -> i % 2 = 0)
    |> Array.iter (printfn "value is %i. ")


module SequenceExamples =

    // 序列（Sequence）用大括号包裹
    let seq1 = seq { yield "a"; yield "b" }

    // 序列可以使用 "yield" 关键字，也可以包含子序列
    let strange = seq {
        // "yield" 向序列中增添1个元素
        yield 1; yield 2;

        // "yield!" 则是增添一个子序列
        yield! [5..10]
        yield! seq {
            for i in 1..10 do
              if i % 2 = 0 then yield i }}
    // 试试看吧！
    strange |> Seq.toList


    // 序列可以通过 "Seq.unfold" 函数来创建
    // 下例演示如何用这个函数创建斐波那契数列
    let fib = Seq.unfold (fun (fst,snd) ->
        Some(fst + snd, (snd, fst + snd))) (0,1)

    // 试试看吧！
    let fib10 = fib |> Seq.take 10 |> Seq.toList
    printf "first 10 fibs are %A" fib10


// ================================================
// 数据类型
// ================================================

module DataTypeExamples =

    // 默认情况下，所有的数据类型均是不可变的

    // 元组（Tuple）是一种简单快捷的匿名类型
    // -- 使用逗号即可创建元组
    let twoTuple = 1, 2
    let threeTuple = "a", 2, true

    // 同样，使用模式匹配来解包元组
    let x, y = twoTuple  // x, y分别被赋值为1, 2

    // ------------------------------------
    // 记录（Record）类型由命名域构成（译者注：类似class的成员变量）
    // 译者注：由于“记录”一词听起来像动词，下文将以英文原文Record来指代
    // ------------------------------------

    // 使用 "type" 关键字和大括号来定义Record类型
    type Person = {First:string; Last:string}

    // 使用 "let" 关键字和大括号来创建Record实例
    let person1 = {First="John"; Last="Doe"}

    // 同样，使用模式匹配来解包Record实例
    let {First = first} = person1    // first被赋值为"John"

    // ------------------------------------
    // 联合类型（Union或Variants，类似于Rust中的枚举类型）拥有一系列的取值选项。其实例仅能从中取其一。
    // 译者注：由于联合类型与Rust中的枚举类型（enum）很相似，故后文将以“枚举类型”指代之
    // ------------------------------------

    // 使用 "type" 关键字和竖线/管道符来定义枚举类型
    type Temp =
        | DegreesC of float
        | DegreesF of float

    // 使用一个选项来创建枚举实例
    let temp1 = DegreesF 98.6
    let temp2 = DegreesC 37.0

    // 模式匹配可以解包枚举实例
    let printTemp = function
       | DegreesC t -> printfn "%f degC" t
       | DegreesF t -> printfn "%f degF" t

    printTemp temp1
    printTemp temp2

    // ------------------------------------
    // 递归类型
    // ------------------------------------

    // 类型可以通过递归组合成复杂的类型，无需创建子类
    type Employee =
      | Worker of Person
      | Manager of Employee list // 译者注：这儿发生了递归定义

    let jdoe = {First="John"; Last="Doe"}
    let worker = Worker jdoe

    // ------------------------------------
    // 使用枚举类型建模
    // ------------------------------------

    // 枚举类型非常适合用于表述某种状态，再也不需要用数字等标志位（flag）来表征状态啦！
    type EmailAddress =
        | ValidEmailAddress of string   // 状态：合法邮件地址
        | InvalidEmailAddress of string // 状态：不合法邮件地址

    let trySendEmail email =
        match email with // 使用模式匹配
        | ValidEmailAddress address -> ()   // 可以发送
        | InvalidEmailAddress address -> () // 不能发送

    // 组合使用枚举类型和Record类型为“域驱动的软件设计”（Domain Driven Design）提供了良好基础。
    // 您可以定义数以百计的类型，每一种都精准地反映着一个域（Domain）

    type CartItem = { ProductCode: string; Qty: int }
    type Payment = Payment of float
    type ActiveCartData = { UnpaidItems: CartItem list }
    type PaidCartData = { PaidItems: CartItem list; Payment: Payment}

    type ShoppingCart =
        | EmptyCart  // 空购物车，没有数据
        | ActiveCart of ActiveCartData
        | PaidCart of PaidCartData

    // ------------------------------------
    // 数据类型的内置行为
    // ------------------------------------

    // 核心数据类型提供了开箱即用的默认行为与性质，无需额外编码。
    // * 不可变性
    // * 漂亮的打印输出，在debug时尤其好用
    // * 相等性与可比性
    // * 可序列化性

    // 使用 %A 来输出复杂数据类型
    printfn "twoTuple=%A,\nPerson=%A,\nTemp=%A,\nEmployee=%A"
             twoTuple person1 temp1 worker

    // 下列扑克牌示例展示了 F# 中内置的相等性与可比较性
    type Suit = Club | Diamond | Spade | Heart
    type Rank = Two | Three | Four | Five | Six | Seven | Eight
                | Nine | Ten | Jack | Queen | King | Ace

    let hand = [ Club, Ace; Heart, Three; Heart, Ace;
                 Spade, Jack; Diamond, Two; Diamond, Ace ]

    // 排序
    List.sort hand |> printfn "sorted hand is (low to high) %A"
    List.max hand |> printfn "high card is %A"
    List.min hand |> printfn "low card is %A"


// ================================================
// 主动模式（Active patterns）
// ================================================

module ActivePatternExamples =

    // F# 中，有一种被称为“主动模式”的特殊模式匹配
    // 它可以在模式匹配中动态解析或检测模式（pattern）。
    // 译者注：译者个人倾向于把此所谓“主动模式”理解为，自定义某个数据类型在接受模式匹配时，会返回哪些情况

    // 主动匹配的语法形似 "banana clips" （译者注：确实不知道banana clips是啥意思）

    // 您可以用elif代替else if，它们在 F# 中完全等价

    // 下列示例使用主动模式去匹配不同类型的字符...
    let (|Digit|Letter|Whitespace|Other|) ch =
       if System.Char.IsDigit(ch) then Digit
       elif System.Char.IsLetter(ch) then Letter
       elif System.Char.IsWhiteSpace(ch) then Whitespace
       else Other

    // ...然后使用它，可以看到解析逻辑十分清晰
    let printChar ch =
      match ch with
      | Digit -> printfn "%c is a Digit" ch
      | Letter -> printfn "%c is a Letter" ch
      | Whitespace -> printfn "%c is a Whitespace" ch
      | _ -> printfn "%c is something else" ch

    // 用主动模式处理并打印一个列表
    ['a'; 'b'; '1'; ' '; '-'; 'c'] |> List.iter printChar

    // -----------------------------------
    // 用主动模式实现FizzBuzz
    // -----------------------------------

    // 您也可以在主动模式中实现“部分匹配”
    // 只需在定义中使用下划线，并在匹配成功时返回 Some 即可
    let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
    let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None

    // fizzbuzz的主函数
    let fizzBuzz i =
      match i with
      | MultOf3 & MultOf5 -> printf "FizzBuzz, "
      | MultOf3 -> printf "Fizz, "
      | MultOf5 -> printf "Buzz, "
      | _ -> printf "%i, " i

    // 试试看吧！
    [1..20] |> List.iter fizzBuzz

// ================================================
// 简明的 F#
// ================================================

module AlgorithmExamples =

    // F# 代码的“信噪比”很高，阅读代码时很容易就能弄明白算法的意图

    // ------ 例子: 计算平方和 ------
    let sumOfSquares n =
       [1..n]              // 1) 取从 1 到 n 的所有整数
       |> List.map square  // 2) 给每个整数求平方
       |> List.sum         // 3) 将上一步的结果求和

    // 试试看吧！
    sumOfSquares 100 |> printfn "Sum of squares = %A"

    // ------ 例子: 排序 ------
    // 译者注：下列示例实现的是朴素的快速排序算法
    let rec sort list =
       match list with
       // 若 list 是空表...
       | [] ->
            []                            // ...则返回空表
       // 否则...
       | firstElem::otherElements ->      // 取其第一个元素
            let smallerElements =         // 从剩余元素中提取比它小的
                otherElements
                |> List.filter (fun e -> e < firstElem)
                |> sort                   // 并排序
            let largerElements =          // 同理，从剩余元素中提取比它大的
                otherElements
                |> List.filter (fun e -> e >= firstElem)
                |> sort                   // 并排序
            // 最后，将这三部分组合起来，返回一个新的列表
            List.concat [smallerElements; [firstElem]; largerElements]

    // 试试看吧！
    sort [1; 5; 23; 18; 9; 1; 3] |> printfn "Sorted = %A"

// ================================================
// 异步编程
// ================================================

module AsyncExample =

    // F# 内置了对异步编程的支持
    // 从而规避了缩进地狱问题（"pyramid of doom"）
    // 下列示例展示了如何使用异步编程，实现同时下载多个网页

    open System.Net
    open System
    open System.IO
    open Microsoft.FSharp.Control.CommonExtensions

    // 异步地访问URL，并获取其内容
    let fetchUrlAsync url =
        async {   // "async" 关键词和大括号将创建一个异步对象 （"async" object）
            let req = WebRequest.Create(Uri(url))
            use! resp = req.AsyncGetResponse()
                // use! 的意思是异步赋值，类似于JavaScript的await
            use stream = resp.GetResponseStream()
                // "use" 会让资源在当前作用域结束时自动 close()
            use reader = new IO.StreamReader(stream)
            let html = reader.ReadToEnd()
            printfn "finished downloading %s" url
        }

    // 准备以下网页喂给爬虫
    let sites = ["http://www.bing.com";
                 "http://www.google.com";
                 "http://www.microsoft.com";
                 "http://www.amazon.com";
                 "http://www.yahoo.com"]

    // 来试试看吧！
    sites
    |> List.map fetchUrlAsync  // 把每个URL都包装成一个异步任务
    |> Async.Parallel          // 令所有任务并发地运行
    |> Async.RunSynchronously  // 开始运行

// ================================================
// .NET 兼容性
// ================================================

module NetCompatibilityExamples =

    // C#能干的活，F# 基本上都能做。同时，F# 还无缝集成了 .Net 或 Mono 的库
    // 译者注：Mono库（Mono Libraries）的含义不太确定

    // ------- 使用已有的库函数  -------

    let (i1success, i1) = System.Int32.TryParse("123");
    if i1success then printfn "parsed as %i" i1 else printfn "parse failed"

    // ------- 简简单单实现接口（interface） -------

    // 创建一个实现了 IDisposable 的对象
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

    // ------- 面向对象编程 -------

    // F# 对面向对象编程的支持也很成熟，支持类、接口、虚函数等

    // 带有泛型的接口
    type IEnumerator<'a> =
        abstract member Current : 'a
        abstract MoveNext : unit -> bool

    // 带有虚函数的抽象基类
    [<AbstractClass>]
    type Shape() =
        // 只读的成员变量
        abstract member Width : int with get
        abstract member Height : int with get
        // 非虚函数
        member this.BoundingArea = this.Height * this.Width
        // 虚函数，且带有默认实现
        abstract member Print : unit -> unit
        default this.Print () = printfn "I'm a shape"

    // 具象子类，继承上述抽象基类，并覆写（override）了其中的函数
    type Rectangle(x:int, y:int) =
        inherit Shape()
        override this.Width = x
        override this.Height = y
        override this.Print ()  = printfn "I'm a Rectangle"

    // 试试看吧！
    let r = Rectangle(2, 3)
    printfn "The width is %i" r.Width
    printfn "The area is %i" r.BoundingArea
    r.Print()

    // ------- 扩展已有类的方法  -------

    // 与 C# 一样，F# 也可以使用扩展语法来扩展已有类的方法
    type System.String with
       member this.StartsWithA = this.StartsWith "A"

    // 试试看吧！
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
