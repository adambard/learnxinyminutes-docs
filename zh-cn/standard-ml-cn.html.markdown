---
language: "Standard ML"
contributors:
    - ["Simon Shine", "http://shine.eu.org/"]
    - ["David Pedersen", "http://lonelyproton.com/"]
    - ["James Baker", "http://www.jbaker.io/"]
    - ["Leo Zovic", "http://langnostic.inaimathi.ca/"]
filename: standard-ml-cn.html
translators:
   - ["Buqian Zheng", "https://github.com/zhengbuqian"]
lang: zh-cn
---

Standard ML是一门拥有类型推断和一些副作用的函数式编程语言。学习Standard ML的一些
难点在于：递归、模式匹配和类型推断（猜测正确的类型但是决不允许隐式类型转换）。与Haskell的
不同之处在于Standard ML拥有引用，允许对变量进行更新。

```ocaml
(* Standard ML的注释以 (* 开头，以 *) 结束。注释可以嵌套，也就意味着所有的 (* 标签都
   需要由一个 *) 结束。这条注释就是两个嵌套的注释的例子。*)

(* 一个Standard ML程序包括声明，例如值声明： *)
val rent = 1200
val phone_no = 5551337
val pi = 3.14159
val negative_number = ~15  (* 是的，一元运算符用波浪符号`~`表示 *)

(* 你当然也可以显示的声明类型，但这并不是必须的，因为ML会自动推断出值的类型。*)
val diameter = 7926 : int
val e = 2.718 : real
val name = "Bobby" : string

(* 同样重要的还有函数： *)
fun is_large(x : int) = if x > 37 then true else false

(* 浮点数被叫做实数： "real". *)
val tau = 2.0 * pi         (* 两个real可以相乘 *)
val twice_rent = 2 * rent  (* 两个int也可以相乘 *)
(* val meh = 1.25 * 10 *)  (* 但是你不能让int和real相乘。 *)
val yeh = 1.25 * (Real.fromInt 10) (* ...除非你显示的把一个转换为另一个*)

(* +, - 和 * 被重载过，所以可以作用于int和real。*)
(* 但是除法有单独的运算符： *)
val real_division = 14.0 / 4.0  (* 结果是 3.5 *)
val int_division  = 14 div 4    (* 结果是 3， 向下取整 *)
val int_remainder = 14 mod 4    (* 结果是 2， 因为 3*4 = 12 *)

(* ~ 有时其实是函数 (比如被放在变量前面的时候) *)
val negative_rent = ~(rent)  (* 即使rent是"real"也正确 *)

(* 当然也有布尔值和相关的运算符 *)
val got_milk = true
val got_bread = false
val has_breakfast = got_milk andalso got_bread  (* 'andalso' 是运算符 *)
val has_something = got_milk orelse got_bread   (* 'orelse' 是运算符 *)
val is_sad = not(has_something)                 (* not 是一个函数 *)

(* 很多值都可以用判等性运算符进行比较： = 和 <> *)
val pays_same_rent = (rent = 1300)  (* false *)
val is_wrong_phone_no = (phone_no <> 5551337)  (* false *)

(* <> 运算符就是其他大部分语言里的 != 。 *)
(* 'andalso' 和 'orelse' 在很多其他语言里被叫做 && 和 || 。 *)

(* 实际上，上面大部分的圆括号都是不需要的。比如表达上面内容的一些不同的方式： *)
fun is_large x = x > 37  
val is_sad = not has_something
val pays_same_rent = rent = 1300  (* 看起来很奇怪，但是就是这样的。 *)
val is_wrong_phone_no = phone_no <> 5551337
val negative_rent = ~rent  (* ~ rent (注意空格) 也正确 *)

(* 圆括号大部分时候用来把东西们组合在一起 *)
val some_answer = is_large (5 + 5)      (* 没有圆括号的话会出错！ *)
(* val some_answer = is_large 5 + 5 *)  (* 会被理解为： (is_large 5) + 5. 错了！ *)

(* 除了boolean, int, real，Standard ML也有char和string *)
val foo = "Hello, World!\n"  (* \n是换行的转移字符 *)
val one_letter = #"a"        (* 这种酷炫的语法表示一个字符a *)

val combined = "Hello " ^ "there, " ^ "fellow!\n"  (* 拼接字符串 *)

val _ = print foo       (* 你可以打印一些东西，这儿我们队打印的结果并不感兴趣，因此 *)
val _ = print combined  (* 用 _ 把结果丢掉了 *)
(* val _ = print one_letter *)  (* 只有字符串才能被这样打印 *)


val bar = [ #"H", #"e", #"l", #"l", #"o" ]  (* SML 也有列表！ *)
(* val _ = print bar *)  (* 然而列表和string是不同的 *)

(* 当然这二者可以相互转换。String是一个库，implode和size是这个库里接受string作为
   参数的函数。*)
val bob = String.implode bar          (* 结果是 "Hello" *)
val bob_char_count = String.size bob  (* 结果是 5 *)
val _ = print (bob ^ "\n")            (* 为了好看加了个换行符 *)

(* 列表可以包含任意类型的元素 *)
val numbers = [1, 3, 3, 7, 229, 230, 248]  (* : int list *)
val names = [ "Fred", "Jane", "Alice" ]    (* : string list *)

(* 列表甚至可以包含列表！ *)
val groups = [ [ "Alice", "Bob" ],
               [ "Huey", "Dewey", "Louie" ],
               [ "Bonnie", "Clyde" ] ]     (* : string list list *)

val number_count = List.length numbers     (* 结果是 7 *)

(* 你可以使用 :: 操作符把单个值放到同样类型列表的最前面。
   :: 叫做con操作符（名字来自Lisp） *)
val more_numbers = 13 :: numbers  (* 结果是 [13, 1, 3, 3, 7, ...] *)
val more_groups  = ["Batman","Superman"] :: groups

(* 拥有同样类型元素的列表可以使用 @ 操作符连接起来 *)
val guest_list = [ "Mom", "Dad" ] @ [ "Aunt", "Uncle" ]

(* 使用 :: 操作符也能完成这项工作。但是这有点绕，因为左手边必须是单个元素
   而右边必须是这种元素的列表 *)
val guest_list = "Mom" :: "Dad" :: [ "Aunt", "Uncle" ]
val guest_list = "Mom" :: ("Dad" :: ("Aunt" :: ("Uncle" :: [])))

(* 如果你有很多同样类型的列表，也可以整个拼接成一个。 *)
val everyone = List.concat groups  (* [ "Alice", "Bob", "Huey", ... ] *)

(* 列表可以包含任意（无限）数量的元素 *)
val lots = [ 5, 5, 5, 6, 4, 5, 6, 5, 4, 5, 7, 3 ]  (* still just an int list *)

(* 但是列表只能包含一种类型的元素 *)
(* val bad_list = [ 1, "Hello", 3.14159 ] : ??? list *)

(* 而元组Tuples则可以包含有限固定数量的不同类型的元素 *)
val person1 = ("Simon", 28, 3.14159)  (* : string * int * real *)

(* 你甚至可以让列表和元组相互嵌套 *)
val likes = [ ("Alice", "ice cream"),
              ("Bob",   "hot dogs"),
              ("Bob",   "Alice") ]     (* : (string * string) list *)

val mixup = [ ("Alice", 39),
              ("Bob",   37),
              ("Eve",   41) ]  (* : (string * int) list *)

val good_bad_stuff =
  (["ice cream", "hot dogs", "chocolate"],
   ["liver", "paying the rent" ])           (* : string list * string list *)

(* 记录Record是每个位置带名字的元组 *)

val rgb = { r=0.23, g=0.56, b=0.91 } (* : {b:real, g:real, r:real} *)

(* 使用Record时不需要提前声明每个位置的名字。 有不同名字的Record属于不同的类型
   即使他们的值的类型是相同的。比如说：*)
val Hsl = { H=310.3, s=0.51, l=0.23 } (* : {H:real, l:real, s:real} *)
val Hsv = { H=310.3, s=0.51, v=0.23 } (* : {H:real, s:real, v:real} *)

(* ...如果你想判断 `Hsv = Hsl` 或者 `rgb = Hsl` 的话，会得到一个类型错误。虽然他们都包含3个
   real，但是由于名字不同，其类型也不同。 *)

(* 可以使用 # 符号取出元组的值 *)

val H = #H Hsv (* : real *)
val s = #s Hsl (* : real *)

(* 函数！ *)
fun add_them (a, b) = a + b    (* 一个简单的加法函数 *)
val test_it = add_them (3, 4)  (* 结果是 7 *)

(* 复杂函数通常会为了可读性写成多行 *)
fun thermometer temp =
    if temp < 37
    then "Cold"
    else if temp > 37
         then "Warm"
         else "Normal"

val test_thermo = thermometer 40  (* 结果是 "Warm" *)

(* if 实际上是表达式而不是声明。一个函数体只可以包含一个表达式。但是还是有一些小技巧
   让一个函数做更多的事。 *)

(* 函数也可以使用调用自己的结果 (递归！) *)
fun fibonacci n =
    if n = 0 then 0 else                   (* 终止条件 *)
    if n = 1 then 1 else                   (* 终止条件 *)
    fibonacci (n - 1) + fibonacci (n - 2)  (* 递归 *)

(* 有的时候，手写出递归函数的执行过程能帮助理解递归概念：

 fibonacci 4
   ~> fibonacci (4 - 1) + fibonacci (4 - 2)
   ~> fibonacci 3 + fibonacci 2
   ~> (fibonacci (3 - 1) + fibonacci (3 - 2)) + fibonacci 2
   ~> (fibonacci 2 + fibonacci 1) + fibonacci 2
   ~> ((fibonacci (2 - 1) + fibonacci (2 - 2)) + fibonacci 1) + fibonacci 2
   ~> ((fibonacci 1 + fibonacci 0) + fibonacci 1) + fibonacci 2
   ~> ((1 + fibonacci 0) + fibonacci 1) + fibonacci 2
   ~> ((1 + 0) + fibonacci 1) + fibonacci 2
   ~> (1 + fibonacci 1) + fibonacci 2
   ~> (1 + 1) + fibonacci 2
   ~> 2 + fibonacci 2
   ~> 2 + (fibonacci (2 - 1) + fibonacci (2 - 2))
   ~> 2 + (fibonacci (2 - 1) + fibonacci (2 - 2))
   ~> 2 + (fibonacci 1 + fibonacci 0)
   ~> 2 + (1 + fibonacci 0)
   ~> 2 + (1 + 0)
   ~> 2 + 1
   ~> 3  第四个斐波那契数

 *)

(* 函数不能改变它引用的值。它只能暂时的使用同名的新变量来覆盖这个值。也就是说，变量其实是
   常数，只有在递归的时候才表现的比较像变量。因此，变量也被叫做值绑定。举个例子： *)

val x = 42
fun answer(question) =
    if question = "What is the meaning of life, the universe and everything?"
    then x
    else raise Fail "I'm an exception. Also, I don't know what the answer is."
val x = 43
val hmm = answer "What is the meaning of life, the universe and everything?"
(* 现在 hmm 的值是 42。  这是因为函数 answer 引用的x是函数定义之前的x。 *)

(* 函数通过接受一个元组来接受多个参数。 *)
fun solve2 (a : real, b : real, c : real) =
    ((~b + Math.sqrt(b * b - 4.0 * a * c)) / (2.0 * a),
     (~b - Math.sqrt(b * b - 4.0 * a * c)) / (2.0 * a))

(* 有时候同样的计算会被计算多次，因此把结果保存下来以重复使用是很有必要的。
   这时可以使用 let 绑定。 *)
fun solve2 (a : real, b : real, c : real) =
    let val discr  = b * b - 4.0 * a * c
        val sqr = Math.sqrt discr
        val denom = 2.0 * a
    in ((~b + sqr) / denom,
        (~b - sqr) / denom)
    end

(* 模式匹配是函数式编程的一个精巧的部分，它是实现 if 的另一种方式。  
   斐波那契函数可以被重写为如下方式： *)
fun fibonacci 0 = 0  (* 终止条件 *)
  | fibonacci 1 = 1  (* 终止条件 *)
  | fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  (* 递归 *)

(* 模式匹配也可以用于比如元组、列表和记录的复合类型。"fun solve2 (a, b, c) = ..."
   的写法实际上也是对于一个三元素元组的模式匹配。类似但是比较不直观的是你也可以从列表的开头
   对列表元素进行匹配。 *)
fun first_elem (x::xs) = x
fun second_elem (x::y::xs) = y
fun evenly_positioned_elems (odd::even::xs) = even::evenly_positioned_elems xs
  | evenly_positioned_elems [odd] = []  (* 终止条件：丢弃结果 *)
  | evenly_positioned_elems []    = []  (* 终止条件 *)

(* 匹配记录的时候，比如使用每个位置的名字，每个位置的值都需要绑定，但是顺序并不重要。 *)

fun rgbToTup {r, g, b} = (r, g, b)    (* fn : {b:'a, g:'b, r:'c} -> 'c * 'b * 'a *)
fun mixRgbToTup {g, b, r} = (r, g, b) (* fn : {b:'a, g:'b, r:'c} -> 'c * 'b * 'a *)

(* 如果传入参数 {r=0.1, g=0.2, b=0.3}，上面的两个函数都会返回 (0.1, 0.2, 0.3)。
   但是传入参数 {r=0.1, g=0.2, b=0.3, a=0.4} 的话则会得到类型错误 *)

(* 高阶函数： 可以接受其他函数作为参数的函数
   函数只不过是另一种类型的值，不需要依附与一个名字而存在。
   没有名字的函数被叫做匿名函数或者lambda表达式或者闭包（因为匿名函数也依赖于词法作用域）*)
val is_large = (fn x => x > 37)
val add_them = fn (a,b) => a + b
val thermometer =
    fn temp => if temp < 37
               then "Cold"
               else if temp > 37
                    then "Warm"
                    else "Normal"

(* 下面的代码就是用了匿名函数，结果是 "ColdWarm" *)
val some_result = (fn x => thermometer (x - 5) ^ thermometer (x + 5)) 37

(* 这是一个作用于列表的高阶函数 *)
(* map f l
       把f从左至右作用于l的每一个元素，并返回结果组成的列表。 *)
val readings = [ 34, 39, 37, 38, 35, 36, 37, 37, 37 ]  (* 先定义一个列表 *)
val opinions = List.map thermometer readings (* 结果是 [ "Cold", "Warm", ... ] *)

(* filter 函数用于筛选列表 *)
val warm_readings = List.filter is_large readings  (* 结果是 [39, 38] *)

(* 你也可以创建自己的高阶函数。函数也可以通过 curry 来接受多个参数。
   从语法上来说，curry就是使用空格来分隔参数，而不是逗号和括号。 *)
fun map f [] = []
  | map f (x::xs) = f(x) :: map f xs

(* map 的类型是 ('a -> 'b) -> 'a list -> 'b list ，这就是多态。 *)
(* 'a 被叫做类型变量 *)


(* 函数可以被声明为中缀的。 *)
val plus = add_them   (* plus 现在和 add_them 是同一个函数。 *)
infix plus            (* plus 现在是一个中缀操作符。 *)
val seven = 2 plus 5  (* seven 现在被绑定上了 7 *)

(* 函数也可以在声明之前就声明为中缀 *)
infix minus
fun x minus y = x - y (* 这样有点不容易判断哪个是参数。 *)
val four = 8 minus 4  (* four 现在被绑定上了 4 *)

(* 中缀函数/操作符也可以使用 'op' 函数变回前缀函数。 *)
val n = op + (5, 5)   (* n is now 10 *)

(* 'op' 在结合高阶函数的时候非常有用，因为高阶函数接受的是函数而不是操作符作为参数。
   大部分的操作符其实都是中缀函数。 *)
(* foldl f init [x1, x2, ..., xn]
       返回
       f(xn, ...f(x2, f(x1, init))...)
       或者如果列表为空时返回 init *)
val sum_of_numbers = foldl op+ 0 [1, 2, 3, 4, 5]


(* 可以很方便的使用 datatype 定义或简单或复杂的数据结构。 *)
datatype color = Red | Green | Blue

(* 这个函数接受 color 之一作为参数。 *)
fun say(col) =
    if col = Red then "You are red!" else
    if col = Green then "You are green!" else
    if col = Blue then "You are blue!" else
    raise Fail "Unknown color"

val _ = print (say(Red) ^ "\n")

(* datatype 经常和模式匹配一起使用。 *)
fun say Red   = "You are red!"
  | say Green = "You are green!"
  | say Blue  = "You are blue!"
  | say _     = raise Fail "Unknown color"


(* 一个二叉树 datatype *)
datatype 'a btree = Leaf of 'a
                  | Node of 'a btree * 'a * 'a btree (* 三个参数的构造器 *)

(* 一颗二叉树： *)
val myTree = Node (Leaf 9, 8, Node (Leaf 3, 5, Leaf 7))

(* 画出来应该是这个样子：

           8
          / \
 leaf -> 9   5
            / \
   leaf -> 3   7 <- leaf
 *)

(* 这个函数计算所有节点值的和。 *)
fun count (Leaf n) = n
  | count (Node (leftTree, n, rightTree)) = count leftTree + n + count rightTree

val myTreeCount = count myTree  (* myTreeCount is now bound to 32 *)


(* 异常！ *)
(* 使用关键字 'raise' 来抛出异常： *)
fun calculate_interest(n) = if n < 0.0
                            then raise Domain
                            else n * 1.04

(* 使用 "handle" 关键字来处理异常 *)
val balance = calculate_interest ~180.0
              handle Domain => ~180.0    (* x 现在的值是 ~180.0 *)

(* 某些异常还包含额外信息 *)
(* 一些内建异常的例子： *)
fun failing_function []    = raise Empty  (* 空列表异常 *)
  | failing_function [x]   = raise Fail "This list is too short!"
  | failing_function [x,y] = raise Overflow  (* 用作计算 *)
  | failing_function xs    = raise Fail "This list is too long!"

(* 使用 'handle' 时也可以使用模式匹配来保证异常都被处理。 *)
val err_msg = failing_function [1,2] handle Fail _ => "Fail was raised"
                                          | Domain => "Domain was raised"
                                          | Empty  => "Empty was raised"
                                          | _      => "Unknown exception"

(* err_msg 的值会是 "Unknown exception" 
   因为 Overflow 没有在模式中列出，因此匹配到了通配符_。 *)

(* 我们也可以定义自己的异常 *)
exception MyException
exception MyExceptionWithMessage of string
exception SyntaxError of string * (int * int)

(* 文件读写！ *)
(* 把一首诗写进文件： *)
fun writePoem(filename) =
    let val file = TextIO.openOut(filename)
        val _ = TextIO.output(file, "Roses are red,\nViolets are blue.\n")
        val _ = TextIO.output(file, "I have a gun.\nGet in the van.\n")
    in TextIO.closeOut(file)
    end

(* 把一首诗读进一个字符串列表： *)
fun readPoem(filename) =
    let val file = TextIO.openIn filename
        val poem = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in String.tokens (fn c => c = #"\n") poem
    end

val _ = writePoem "roses.txt"
val test_poem = readPoem "roses.txt"  (* gives [ "Roses are red,",
                                                 "Violets are blue.",
                                                 "I have a gun.",
                                                 "Get in the van." ] *)

(* 我们还可以创建指向值的引用，引用可以被更新。 *)
val counter = ref 0 (* 使用 ref 函数创建一个引用。 *)

(* 使用赋值运算符给引用复制 *)
fun set_five reference = reference := 5

(* 使用解引用运算符得到引用的值 *)
fun equals_five reference = !reference = 5

(* 递归很复杂的时候，也可以使用 while 循环 *)
fun decrement_to_zero r = if !r < 0
                          then r := 0
                          else while !r >= 0 do r := !r - 1

(* 这将会返回 unit （也就是什么都没有，一个0元素的元组） *)

(* 要返回值，可以使用分号来分开表达式。 *)
fun decrement_ret x y = (x := !x - 1; y)
```

## 阅读更多

* 安装交互式编译器 (REPL)，如：
  [Poly/ML](http://www.polyml.org/),
  [Moscow ML](http://mosml.org),
  [SML/NJ](http://smlnj.org/).
* 上Coursera上的课程 [Programming Languages](https://www.coursera.org/course/proglang).
* 购买 Larry C. Paulson 写的 *ML for the Working Programmer* 书。
* 使用 [StackOverflow's sml 标签](http://stackoverflow.com/questions/tagged/sml).
