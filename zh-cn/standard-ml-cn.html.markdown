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

(* You can put single values in front of lists of the same kind using
   the :: operator, called "the cons operator" (known from Lisp). *)
val more_numbers = 13 :: numbers  (* gives [13, 1, 3, 3, 7, ...] *)
val more_groups  = ["Batman","Superman"] :: groups

(* Lists of the same kind can be appended using the @ ("append") operator *)
val guest_list = [ "Mom", "Dad" ] @ [ "Aunt", "Uncle" ]

(* This could have been done with the "cons" operator.  It is tricky because the
   left-hand-side must be an element whereas the right-hand-side must be a list
   of those elements. *)
val guest_list = "Mom" :: "Dad" :: [ "Aunt", "Uncle" ]
val guest_list = "Mom" :: ("Dad" :: ("Aunt" :: ("Uncle" :: [])))

(* If you have many lists of the same kind, you can concatenate them all *)
val everyone = List.concat groups  (* [ "Alice", "Bob", "Huey", ... ] *)

(* A list can contain any (finite) number of values *)
val lots = [ 5, 5, 5, 6, 4, 5, 6, 5, 4, 5, 7, 3 ]  (* still just an int list *)

(* Lists can only contain one kind of thing... *)
(* val bad_list = [ 1, "Hello", 3.14159 ] : ??? list *)


(* Tuples, on the other hand, can contain a fixed number of different things *)
val person1 = ("Simon", 28, 3.14159)  (* : string * int * real *)

(* You can even have tuples inside lists and lists inside tuples *)
val likes = [ ("Alice", "ice cream"),
              ("Bob",   "hot dogs"),
              ("Bob",   "Alice") ]     (* : (string * string) list *)

val mixup = [ ("Alice", 39),
              ("Bob",   37),
              ("Eve",   41) ]  (* : (string * int) list *)

val good_bad_stuff =
  (["ice cream", "hot dogs", "chocolate"],
   ["liver", "paying the rent" ])           (* : string list * string list *)


(* Records are tuples with named slots *)

val rgb = { r=0.23, g=0.56, b=0.91 } (* : {b:real, g:real, r:real} *)

(* You don't need to declare their slots ahead of time. Records with
   different slot names are considered different types, even if their
   slot value types match up. For instance... *)

val Hsl = { H=310.3, s=0.51, l=0.23 } (* : {H:real, l:real, s:real} *)
val Hsv = { H=310.3, s=0.51, v=0.23 } (* : {H:real, s:real, v:real} *)

(* ...trying to evaluate `Hsv = Hsl` or `rgb = Hsl` would give a type
   error. While they're all three-slot records composed only of `real`s,
   they each have different names for at least some slots. *)

(* You can use hash notation to get values out of tuples. *)

val H = #H Hsv (* : real *)
val s = #s Hsl (* : real *)

(* Functions! *)
fun add_them (a, b) = a + b    (* A simple function that adds two numbers *)
val test_it = add_them (3, 4)  (* gives 7 *)

(* Larger functions are usually broken into several lines for readability *)
fun thermometer temp =
    if temp < 37
    then "Cold"
    else if temp > 37
         then "Warm"
         else "Normal"

val test_thermo = thermometer 40  (* gives "Warm" *)

(* if-sentences are actually expressions and not statements/declarations.
   A function body can only contain one expression.  There are some tricks
   for making a function do more than just one thing, though. *)

(* A function can call itself as part of its result (recursion!) *)
fun fibonacci n =
    if n = 0 then 0 else                   (* Base case *)
    if n = 1 then 1 else                   (* Base case *)
    fibonacci (n - 1) + fibonacci (n - 2)  (* Recursive case *)

(* Sometimes recursion is best understood by evaluating a function by hand:

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
   ~> 3  which is the 4th Fibonacci number, according to this definition

 *)

(* A function cannot change the variables it can refer to.  It can only
   temporarily shadow them with new variables that have the same names.  In this
   sense, variables are really constants and only behave like variables when
   dealing with recursion.  For this reason, variables are also called value
   bindings. An example of this: *)

val x = 42
fun answer(question) =
    if question = "What is the meaning of life, the universe and everything?"
    then x
    else raise Fail "I'm an exception. Also, I don't know what the answer is."
val x = 43
val hmm = answer "What is the meaning of life, the universe and everything?"
(* Now, hmm has the value 42.  This is because the function answer refers to
   the copy of x that was visible before its own function definition. *)


(* Functions can take several arguments by taking one tuples as argument: *)
fun solve2 (a : real, b : real, c : real) =
    ((~b + Math.sqrt(b * b - 4.0 * a * c)) / (2.0 * a),
     (~b - Math.sqrt(b * b - 4.0 * a * c)) / (2.0 * a))

(* Sometimes, the same computation is carried out several times. It makes sense
   to save and re-use the result the first time. We can use "let-bindings": *)
fun solve2 (a : real, b : real, c : real) =
    let val discr  = b * b - 4.0 * a * c
        val sqr = Math.sqrt discr
        val denom = 2.0 * a
    in ((~b + sqr) / denom,
        (~b - sqr) / denom)
    end


(* Pattern matching is a funky part of functional programming.  It is an
   alternative to if-sentences.  The fibonacci function can be rewritten: *)
fun fibonacci 0 = 0  (* Base case *)
  | fibonacci 1 = 1  (* Base case *)
  | fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  (* Recursive case *)

(* Pattern matching is also possible on composite types like tuples, lists and
   records. Writing "fun solve2 (a, b, c) = ..." is in fact a pattern match on
   the one three-tuple solve2 takes as argument. Similarly, but less intuitively,
   you can match on a list consisting of elements in it (from the beginning of
   the list only). *)
fun first_elem (x::xs) = x
fun second_elem (x::y::xs) = y
fun evenly_positioned_elems (odd::even::xs) = even::evenly_positioned_elems xs
  | evenly_positioned_elems [odd] = []  (* Base case: throw away *)
  | evenly_positioned_elems []    = []  (* Base case *)

(* When matching on records, you must use their slot names, and you must bind
   every slot in a record. The order of the slots doesn't matter though. *)

fun rgbToTup {r, g, b} = (r, g, b)    (* fn : {b:'a, g:'b, r:'c} -> 'c * 'b * 'a *)
fun mixRgbToTup {g, b, r} = (r, g, b) (* fn : {b:'a, g:'b, r:'c} -> 'c * 'b * 'a *)

(* If called with {r=0.1, g=0.2, b=0.3}, either of the above functions
   would return (0.1, 0.2, 0.3). But it would be a type error to call them
   with {r=0.1, g=0.2, b=0.3, a=0.4} *)

(* Higher order functions: Functions can take other functions as arguments.
   Functions are just other kinds of values, and functions don't need names
   to exist.  Functions without names are called "anonymous functions" or
   lambda expressions or closures (since they also have a lexical scope). *)
val is_large = (fn x => x > 37)
val add_them = fn (a,b) => a + b
val thermometer =
    fn temp => if temp < 37
               then "Cold"
               else if temp > 37
                    then "Warm"
                    else "Normal"

(* The following uses an anonymous function directly and gives "ColdWarm" *)
val some_result = (fn x => thermometer (x - 5) ^ thermometer (x + 5)) 37

(* Here is a higher-order function that works on lists (a list combinator) *)
(* map f l
       applies f to each element of l from left to right, 
       returning the list of results. *)
val readings = [ 34, 39, 37, 38, 35, 36, 37, 37, 37 ]  (* first an int list *)
val opinions = List.map thermometer readings (* gives [ "Cold", "Warm", ... ] *)

(* And here is another one for filtering lists *)
val warm_readings = List.filter is_large readings  (* gives [39, 38] *)

(* You can create your own higher-order functions, too.  Functions can also take
   several arguments by "currying" them. Syntax-wise this means adding spaces
   between function arguments instead of commas and surrounding parentheses. *)
fun map f [] = []
  | map f (x::xs) = f(x) :: map f xs

(* map has type ('a -> 'b) -> 'a list -> 'b list and is called polymorphic. *)
(* 'a is called a type variable. *)


(* We can declare functions as infix *)
val plus = add_them   (* plus is now equal to the same function as add_them *)
infix plus            (* plus is now an infix operator *)
val seven = 2 plus 5  (* seven is now bound to 7 *)

(* Functions can also be made infix before they are declared *)
infix minus
fun x minus y = x - y (* It becomes a little hard to see what's the argument *)
val four = 8 minus 4  (* four is now bound to 4 *)

(* An infix function/operator can be made prefix with 'op' *)
val n = op + (5, 5)   (* n is now 10 *)

(* 'op' is useful when combined with high order functions because they expect
   functions and not operators as arguments. Most operators are really just
   infix functions. *)
(* foldl f init [x1, x2, ..., xn]
       returns
       f(xn, ...f(x2, f(x1, init))...)
       or init if the list is empty. *)
val sum_of_numbers = foldl op+ 0 [1, 2, 3, 4, 5]


(* Datatypes are useful for creating both simple and complex structures *)
datatype color = Red | Green | Blue

(* Here is a function that takes one of these as argument *)
fun say(col) =
    if col = Red then "You are red!" else
    if col = Green then "You are green!" else
    if col = Blue then "You are blue!" else
    raise Fail "Unknown color"

val _ = print (say(Red) ^ "\n")

(* Datatypes are very often used in combination with pattern matching *)
fun say Red   = "You are red!"
  | say Green = "You are green!"
  | say Blue  = "You are blue!"
  | say _     = raise Fail "Unknown color"


(* Here is a binary tree datatype *)
datatype 'a btree = Leaf of 'a
                  | Node of 'a btree * 'a * 'a btree (* three-arg constructor *)

(* Here is a binary tree *)
val myTree = Node (Leaf 9, 8, Node (Leaf 3, 5, Leaf 7))

(* Drawing it, it might look something like...

           8
          / \
 leaf -> 9   5
            / \
   leaf -> 3   7 <- leaf
 *)

(* This function counts the sum of all the elements in a tree *)
fun count (Leaf n) = n
  | count (Node (leftTree, n, rightTree)) = count leftTree + n + count rightTree

val myTreeCount = count myTree  (* myTreeCount is now bound to 32 *)


(* Exceptions! *)
(* Exceptions can be raised/thrown using the reserved word 'raise' *)
fun calculate_interest(n) = if n < 0.0
                            then raise Domain
                            else n * 1.04

(* Exceptions can be caught using "handle" *)
val balance = calculate_interest ~180.0
              handle Domain => ~180.0    (* x now has the value ~180.0 *)

(* Some exceptions carry extra information with them *)
(* Here are some examples of built-in exceptions *)
fun failing_function []    = raise Empty  (* used for empty lists *)
  | failing_function [x]   = raise Fail "This list is too short!"
  | failing_function [x,y] = raise Overflow  (* used for arithmetic *)
  | failing_function xs    = raise Fail "This list is too long!"

(* We can pattern match in 'handle' to make sure
   a specfic exception was raised, or grab the message *)
val err_msg = failing_function [1,2] handle Fail _ => "Fail was raised"
                                          | Domain => "Domain was raised"
                                          | Empty  => "Empty was raised"
                                          | _      => "Unknown exception"

(* err_msg now has the value "Unknown exception" because Overflow isn't
   listed as one of the patterns -- thus, the catch-all pattern _ is used. *)

(* We can define our own exceptions like this *)
exception MyException
exception MyExceptionWithMessage of string
exception SyntaxError of string * (int * int)

(* File I/O! *)
(* Write a nice poem to a file *)
fun writePoem(filename) =
    let val file = TextIO.openOut(filename)
        val _ = TextIO.output(file, "Roses are red,\nViolets are blue.\n")
        val _ = TextIO.output(file, "I have a gun.\nGet in the van.\n")
    in TextIO.closeOut(file)
    end

(* Read a nice poem from a file into a list of strings *)
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

(* We can create references to data which can be updated *)
val counter = ref 0 (* Produce a reference with the ref function *)

(* Assign to a reference with the assignment operator *)
fun set_five reference = reference := 5

(* Read a reference with the dereference operator *)
fun equals_five reference = !reference = 5

(* We can use while loops for when recursion is messy *)
fun decrement_to_zero r = if !r < 0
                          then r := 0
                          else while !r >= 0 do r := !r - 1

(* This returns the unit value (in practical terms, nothing, a 0-tuple) *)

(* To allow returning a value, we can use the semicolon to sequence evaluations *)
fun decrement_ret x y = (x := !x - 1; y)
```

## Further learning

* Install an interactive compiler (REPL), for example
  [Poly/ML](http://www.polyml.org/),
  [Moscow ML](http://mosml.org),
  [SML/NJ](http://smlnj.org/).
* Follow the Coursera course [Programming Languages](https://www.coursera.org/course/proglang).
* Get the book *ML for the Working Programmer* by Larry C. Paulson.
* Use [StackOverflow's sml tag](http://stackoverflow.com/questions/tagged/sml).
