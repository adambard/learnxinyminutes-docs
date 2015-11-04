---
language: Haskell 
filename: learn-haskell-zh.hs
contributors:
    - ["Adit Bhargava", "http://adit.io"]
translators:
    - ["Peiyong Lin", ""]
    - ["chad luo", "http://yuki.rocks"]
lang: zh-cn
---

Haskell 是一门实用的函数式编程语言，因其 Monads 与类型系统而闻名。而我使用它则是因为它异常优雅。用 Haskell 编程令我感到非常快乐。

```haskell
-- 单行注释以两个减号开头
{- 多行注释像这样
    被一个闭合的块包围
-}

----------------------------------------------------
-- 1. 简单的数据类型和操作符
----------------------------------------------------

-- 数字
3 -- 3
-- 数学计算
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- 默认除法不是整除
35 / 4 -- 8.75

-- 整除
35 `div` 4 -- 8

-- 布尔值
True
False

-- 布尔操作
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- 在上面的例子中，`not` 是一个接受一个参数的函数。
-- Haskell 不需要括号来调用函数，所有的参数都只是在函数名之后列出来
-- 因此，通常的函数调用模式是：
--   func arg1 arg2 arg3...
-- 你可以查看函数部分了解如何自行编写。

-- 字符串和字符
"This is a string." -- 字符串
'a' -- 字符
'对于字符串你不能使用单引号。' -- 错误！

-- 连接字符串
"Hello " ++ "world!" -- "Hello world!"

-- 一个字符串是一系列字符
['H', 'e', 'l', 'l', 'o'] -- "Hello"
"This is a string" !! 0 -- 'T'


----------------------------------------------------
-- 列表和元组
----------------------------------------------------

-- 一个列表中的每一个元素都必须是相同的类型。
-- 下面两个列表等价
[1, 2, 3, 4, 5]
[1..5]

-- 区间也可以这样
['A'..'F'] -- "ABCDEF"

-- 你可以在区间中指定步进
[0,2..10] -- [0, 2, 4, 6, 8, 10]
[5..1] -- 这样不行，因为 Haskell 默认递增
[5,4..1] -- [5, 4, 3, 2, 1]

-- 列表下标
[0..] !! 5 -- 5

-- 在 Haskell 你可以使用无限列表
[1..] -- 一个含有所有自然数的列表

-- 无限列表的原理是，Haskell 有“惰性求值”。
-- 这意味着 Haskell 只在需要时才会计算。
-- 所以当你获取列表的第 1000 项元素时，Haskell 会返回给你：
[1..] !! 999 -- 1000
-- Haskell 计算了列表中第 1 至 1000 项元素，但这个无限列表中剩下的元素还不存在。
-- Haskell 只有在需要时才会计算它们。

-- 连接两个列表
[1..5] ++ [6..10]

-- 往列表头增加元素
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- 其它列表操作
head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

-- 列表推导 (list comprehension)
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- 附带条件
[x*2 | x <-[1..5], x*2 > 4] -- [6, 8, 10]

-- 元组中的每一个元素可以是不同类型，但是一个元组的长度是固定的
-- 一个元组
("haskell", 1)

-- 获取元组中的元素（例如，一个含有 2 个元素的元祖）
fst ("haskell", 1) -- "haskell"
snd ("haskell", 1) -- 1

----------------------------------------------------
-- 3. 函数
----------------------------------------------------

-- 一个接受两个变量的简单函数
add a b = a + b

-- 注意，如果你使用 ghci (Hakell 解释器)，你需要使用 `let`，也就是
-- let add a b = a + b

-- 调用函数
add 1 2 -- 3

-- 你也可以使用反引号中置函数名：
1 `add` 2 -- 3

-- 你也可以定义不带字母的函数名，这样你可以定义自己的操作符。
-- 这里有一个做整除的操作符
(//) a b = a `div` b
35 // 4 -- 8

-- Guard：一个在函数中做条件判断的简单方法
fib x
  | x < 2 = x
  | otherwise = fib (x - 1) + fib (x - 2)

-- 模式匹配与 Guard 类似。
-- 这里给出了三个不同的 fib 定义。
-- Haskell 会自动调用第一个符合参数模式的声明
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- 元组的模式匹配
foo (x, y) = (x + 1, y + 2)

-- 列表的模式匹配
-- 这里 `x` 是列表中第一个元素，`xs` 是列表剩余的部分。
-- 我们可以实现自己的 map 函数：
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

-- 匿名函数带有一个反斜杠，后面跟着所有的参数
myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]

-- 在 fold（在一些语言称 为`inject`）中使用匿名函数
-- foldl1 意味着左折叠 (fold left), 并且使用列表中第一个值作为累加器的初始值。
foldl1 (\acc x -> acc + x) [1..5] -- 15

----------------------------------------------------
-- 4. 其它函数
----------------------------------------------------

-- 部分调用
-- 如果你调用函数时没有给出所有参数，它就被“部分调用”。
-- 它将返回一个接受余下参数的函数。
add a b = a + b
foo = add 10 -- foo 现在是一个接受一个数并对其加 10 的函数
foo 5 -- 15

-- 另一种等价写法
foo = (+10)
foo 5 -- 15

-- 函列表合
-- (.) 函数把其它函数链接到一起。
-- 例如，这里 foo 是一个接受一个值的函数。
-- 它对接受的值加 10，并对结果乘以 5，之后返回最后的值。
foo = (*5) . (+10)

-- (5 + 10) * 5 = 75
foo 5 -- 75

-- 修正优先级
-- Haskell 有另外一个函数 `$` 可以改变优先级。
-- `$` 使得 Haskell 先计算其右边的部分，然后调用左边的部分。
-- 你可以使用 `$` 来移除多余的括号。

-- 修改前
(even (fib 7)) -- False

-- 修改后
even . fib $ 7 -- False

-- 等价地
even $ fib 7 -- False

----------------------------------------------------
-- 5. 类型声明
----------------------------------------------------

-- Haskell 有一个非常强大的类型系统，一切都有一个类型声明。

-- 一些基本的类型：
5 :: Integer
"hello" :: String
True :: Bool

-- 函数也有类型
-- `not` 接受一个布尔型返回一个布尔型
-- not :: Bool -> Bool

-- 这是接受两个参数的函数
-- add :: Integer -> Integer -> Integer

-- 当你定义一个值，声明其类型是一个好做法
double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. 控制流和 If 语句
----------------------------------------------------

-- if 语句：
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- if 语句也可以有多行，注意缩进：
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- case 语句
-- 解析命令行参数：
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"

-- Haskell 没有循环，它使用递归
-- map 对一个列表中的每一个元素调用一个函数
map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- 你可以使用 map 来编写 for 函数
for array func = map func array

-- 调用
for [0..5] $ \i -> show i

-- 我们也可以像这样写
for [0..5] show

-- 你可以使用 foldl 或者 foldr 来分解列表
-- foldl <fn> <initial value> <list>
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- 等价于
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl 从左开始，foldr 从右
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- 现在它等价于
(2 * 3 + (2 * 2 + (2 * 1 + 4)))

----------------------------------------------------
-- 7. 数据类型
----------------------------------------------------

-- 在 Haskell 中声明你自己的数据类型：
data Color = Red | Blue | Green

-- 现在你可以在函数中使用它：
say :: Color -> String
say Red = "You are Red!"
say Blue = "You are Blue!"
say Green =  "You are Green!"

-- 你的数据类型也可以有参数：
data Maybe a = Nothing | Just a

-- 这些都是 Maybe 类型：
Just "hello"    -- `Maybe String` 类型
Just 1          -- `Maybe Int` 类型
Nothing         -- 对任意 `a` 为 `Maybe a` 类型

----------------------------------------------------
-- 8. Haskell IO
----------------------------------------------------

-- 虽然不解释 Monads 就无法完全解释 IO，但大致了解并不难。

-- 当执行一个 Haskell 程序时，函数 `main` 就被调用。
-- 它必须返回一个类型 `IO ()` 的值。例如：
main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue) 
-- putStrLn 的类型是 String -> IO ()

-- 如果你的程序输入 String 返回 String，那样编写 IO 是最简单的。
-- 函数
--    interact :: (String -> String) -> IO ()
-- 输入一些文本，对其调用一个函数，并打印输出。

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- 你可以认为一个 `IO ()` 类型的值是表示计算机做的一系列操作，类似命令式语言。
-- 我们可以使用 `do` 声明来把动作连接到一起。
-- 举个列子
sayHello :: IO ()
sayHello = do 
   putStrLn "What is your name?"
   name <- getLine -- 这里接受一行输入并绑定至 "name"
   putStrLn $ "Hello, " ++ name
   
-- 练习：编写只读取一行输入的 `interact`
   
-- 然而，`sayHello` 中的代码将不会被执行。唯一被执行的动作是 `main` 的值。
-- 为了运行 `sayHello`，注释上面 `main` 的定义，替换为：
--   main = sayHello

-- 让我们来更进一步理解刚才所使用的函数 `getLine` 是怎样工作的。它的类型是：
--    getLine :: IO String
-- 你可以认为一个 `IO a` 类型的值代表了一个运行时会生成一个 `a` 类型值的程序。
-- （可能伴随其它行为）
-- 我们可以通过 `<-` 保存和重用这个值。
-- 我们也可以实现自己的 `IO String` 类型函数：
action :: IO String
action = do
   putStrLn "This is a line. Duh"
   input1 <- getLine 
   input2 <- getLine
   -- `do` 语句的类型是它的最后一行
   -- `return` 不是关键字，只是一个普通函数
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- 我们可以像调用 `getLine` 一样调用它
main'' = do
    putStrLn "I will echo two lines!"
    result <- action 
    putStrLn result
    putStrLn "This was all, folks!"

-- `IO` 类型是一个 "Monad" 的例子。
-- Haskell 通过使用 Monad 使得其本身为纯函数式语言。
-- 任何与外界交互的函数（即 IO）都在它的类型声明中标记为 `IO`。
-- 这告诉我们什么样的函数是“纯洁的”(不与外界交互，不修改状态) ，
-- 什么样的函数不是 “纯洁的”。
-- 这个功能非常强大，因为纯函数并发非常容易，由此在 Haskell 中做并发非常容易。

----------------------------------------------------
-- 9. Haskell REPL
----------------------------------------------------

-- 键入 `ghci` 开始 REPL。
-- 现在你可以键入 Haskell 代码。
-- 任何新值都需要通过 `let` 来创建
let foo = 5

-- 你可以通过命令 `:t` 查看任何值的类型
>:t foo
foo :: Integer

-- 你也可以运行任何 `IO ()`类型的动作
> sayHello
What is your name?
Friend!
Hello, Friend!

```

Haskell 还有许多内容，包括类型类 (typeclasses) 与 Monads。这些都是令 Haskell 编程非常有趣的好东西。我们最后给出 Haskell 的一个例子，一个快速排序的实现：

```haskell
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```

安装 Haskell 很简单。你可以[从这里获得](http://www.haskell.org/platform/)。

你可以从优秀的
[Learn you a Haskell](http://learnyouahaskell.com/) 或者
[Real World Haskell](http://book.realworldhaskell.org/)
找到更平缓的入门介绍。
