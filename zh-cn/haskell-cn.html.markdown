---
language: haskell 
filename: learn-haskell-zh.hs
contributors:
    - ["Adit Bhargava", "http://adit.io"]
translators:
    - ["Peiyong Lin", ""]
lang: zh-cn
---

Haskell 被设计成一种实用的纯函数式编程语言。它因为 monads 及其类型系统而出名，但是我回归到它本身因为。Haskell 使得编程对于我而言是一种真正的快乐。

```haskell
-- 单行注释以两个破折号开头
{- 多行注释像这样
   被一个闭合的块包围
-}

----------------------------------------------------
-- 1. 简单的数据类型和操作符
----------------------------------------------------

-- 你有数字
3 -- 3
-- 数学计算就像你所期待的那样
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- 默认除法不是整除
35 / 4 -- 8.75

-- 整除
35 `div` 4 -- 8

-- 布尔值也简单
True
False

-- 布尔操作
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- 在上述的例子中，`not` 是一个接受一个值的函数。
-- Haskell 不需要括号来调用函数。。。所有的参数
-- 都只是在函数名之后列出来。因此，通常的函数调用模式是：
-- func arg1 arg2 arg3...
-- 查看关于函数的章节以获得如何写你自己的函数的相关信息。

-- 字符串和字符
"This is a string."
'a' -- 字符
'对于字符串你不能使用单引号。' -- 错误！

-- 连结字符串
"Hello " ++ "world!" -- "Hello world!"

-- 一个字符串是一系列字符
"This is a string" !! 0 -- 'T'


----------------------------------------------------
-- 列表和元组
----------------------------------------------------

-- 一个列表中的每一个元素都必须是相同的类型
-- 下面两个列表一样
[1, 2, 3, 4, 5]
[1..5]

-- 在 Haskell 你可以拥有含有无限元素的列表
[1..] -- 一个含有所有自然数的列表

-- 因为 Haskell 有“懒惰计算”，所以无限元素的列表可以正常运作。这意味着
-- Haskell 可以只在它需要的时候计算。所以你可以请求
-- 列表中的第1000个元素，Haskell 会返回给你

[1..] !! 999 -- 1000

-- Haskell 计算了列表中 1 - 1000 个元素。。。但是
-- 这个无限元素的列表中剩下的元素还不存在！ Haskell 不会
-- 真正地计算它们知道它需要。

<FS>- 连接两个列表
[1..5] ++ [6..10]

-- 往列表头增加元素
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- 列表中的下标
[0..] !! 5 -- 5

-- 更多列表操作
head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

-- 列表推导
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- 附带条件
[x*2 | x <-[1..5], x*2 > 4] -- [6, 8, 10]

-- 元组中的每一个元素可以是不同类型的，但是一个元组
-- 的长度是固定的
-- 一个元组
("haskell", 1)

-- 获取元组中的元素
fst ("haskell", 1) -- "haskell"
snd ("haskell", 1) -- 1

----------------------------------------------------
-- 3. 函数
----------------------------------------------------
-- 一个接受两个变量的简单函数
add a b = a + b

-- 注意，如果你使用 ghci (Hakell 解释器)
-- 你将需要使用 `let`，也就是
-- let add a b = a + b

-- 使用函数
add 1 2 -- 3

-- 你也可以把函数放置在两个参数之间
-- 附带倒引号：
1 `add` 2 -- 3

-- 你也可以定义不带字符的函数！这使得
-- 你定义自己的操作符！这里有一个操作符
-- 来做整除
(//) a b = a `div` b
35 // 4 -- 8

-- 守卫：一个简单的方法在函数里做分支
fib x
  | x < 2 = x
  | otherwise = fib (x - 1) + fib (x - 2)

-- 模式匹配是类型的。这里有三种不同的 fib 
-- 定义。Haskell 将自动调用第一个
-- 匹配值的模式的函数。
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- 元组的模式匹配：
foo (x, y) = (x + 1, y + 2)

-- 列表的模式匹配。这里 `x` 是列表中第一个元素，
-- 并且 `xs` 是列表剩余的部分。我们可以写
-- 自己的 map 函数：
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

-- 编写出来的匿名函数带有一个反斜杠，后面跟着
-- 所有的参数。
myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]

-- 使用 fold （在一些语言称为`inject`）随着一个匿名的
-- 函数。foldl1 意味着左折叠(fold left), 并且使用列表中第一个值
-- 作为累加器的初始化值。
foldl1 (\acc x -> acc + x) [1..5] -- 15

----------------------------------------------------
-- 4. 更多的函数
----------------------------------------------------

-- 柯里化(currying)：如果你不传递函数中所有的参数，
-- 它就变成“柯里化的”。这意味着，它返回一个接受剩余参数的函数。

add a b = a + b
foo = add 10 -- foo 现在是一个接受一个数并对其加 10 的函数
foo 5 -- 15

-- 另外一种方式去做同样的事
foo = (+10)
foo 5 -- 15

-- 函数组合
-- (.) 函数把其它函数链接到一起
-- 举个列子，这里 foo 是一个接受一个值的函数。它对接受的值加 10，
-- 并对结果乘以 5，之后返回最后的值。
foo = (*5) . (+10)

-- (5 + 10) * 5 = 75
foo 5 -- 75

-- 修复优先级
-- Haskell 有另外一个函数称为 `$`。它改变优先级
-- 使得其左侧的每一个操作先计算然后应用到
-- 右侧的每一个操作。你可以使用 `.` 和 `$` 来除去很多
-- 括号：

-- before
(even (fib 7)) -- true

-- after
even . fib $ 7 -- true

----------------------------------------------------
-- 5. 类型签名
----------------------------------------------------

-- Haskell 有一个非常强壮的类型系统，一切都有一个类型签名。

-- 一些基本的类型：
5 :: Integer
"hello" :: String
True :: Bool

-- 函数也有类型。
-- `not` 接受一个布尔型返回一个布尔型：
-- not :: Bool -> Bool

-- 这是接受两个参数的函数：
-- add :: Integer -> Integer -> Integer

-- 当你定义一个值，在其上写明它的类型是一个好实践：
double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. 控制流和 If 语句
----------------------------------------------------

-- if 语句
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- if 语句也可以有多行，缩进是很重要的
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- case 语句：这里是你可以怎样去解析命令行参数
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"

-- Haskell 没有循环因为它使用递归取代之。
-- map 应用一个函数到一个数组中的每一个元素

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- 你可以使用 map 来编写 for 函数
for array func = map func array

-- 然后使用它
for [0..5] $ \i -> show i

-- 我们也可以像这样写：
for [0..5] show

-- 你可以使用 foldl 或者 foldr 来分解列表
-- foldl <fn> <initial value> <list>
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- 这和下面是一样的
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl 是左手边的，foldr 是右手边的-
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- 这和下面是一样的
(2 * 3 + (2 * 2 + (2 * 1 + 4)))

----------------------------------------------------
-- 7. 数据类型
----------------------------------------------------

-- 这里展示在 Haskell 中你怎样编写自己的数据类型

data Color = Red | Blue | Green

-- 现在你可以在函数中使用它：


say :: Color -> String
say Red = "You are Red!"
say Blue = "You are Blue!"
say Green =  "You are Green!"

-- 你的数据类型也可以有参数：

data Maybe a = Nothing | Just a

-- 类型 Maybe 的所有
Just "hello"    -- of type `Maybe String`
Just 1          -- of type `Maybe Int`
Nothing         -- of type `Maybe a` for any `a`

----------------------------------------------------
-- 8. Haskell IO
----------------------------------------------------

-- 虽然在没有解释 monads 的情况下 IO不能被完全地解释，
-- 着手解释到位并不难。

-- 当一个 Haskell 程序被执行，函数 `main` 就被调用。
-- 它必须返回一个类型 `IO ()` 的值。举个列子：

main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue) 
-- putStrLn has type String -> IO ()

-- 如果你能实现你的程序依照函数从 String 到 String，那样编写 IO 是最简单的。
-- 函数
--    interact :: (String -> String) -> IO ()
-- 输入一些文本，在其上运行一个函数，并打印出输出

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- 你可以考虑一个 `IO()` 类型的值，当做一系列计算机所完成的动作的代表，
-- 就像一个以命令式语言编写的计算机程序。我们可以使用 `do` 符号来把动作链接到一起。
-- 举个列子：

sayHello :: IO ()
sayHello = do 
   putStrLn "What is your name?"
   name <- getLine -- this gets a line and gives it the name "input"
   putStrLn $ "Hello, " ++ name
   
-- 练习：编写只读取一行输入的 `interact`
   
-- 然而，`sayHello` 中的代码将不会被执行。唯一被执行的动作是 `main` 的值。
-- 为了运行 `sayHello`，注释上面 `main` 的定义，并代替它：
--   main = sayHello

-- 让我们来更好地理解刚才所使用的函数 `getLine` 是怎样工作的。它的类型是：
--    getLine :: IO String
-- 你可以考虑一个 `IO a` 类型的值，代表一个当被执行的时候
-- 将产生一个 `a` 类型的值的计算机程序（除了它所做的任何事之外）。我们可以保存和重用这个值通过 `<-`。
-- 我们也可以写自己的 `IO String` 类型的动作：

action :: IO String
action = do
   putStrLn "This is a line. Duh"
   input1 <- getLine 
   input2 <- getLine
   -- The type of the `do` statement is that of its last line.
   -- `return` is not a keyword, but merely a function 
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- 我们可以使用这个动作就像我们使用 `getLine`:

main'' = do
    putStrLn "I will echo two lines!"
    result <- action 
    putStrLn result
    putStrLn "This was all, folks!"

-- `IO` 类型是一个 "monad" 的例子。Haskell 使用一个 monad 来做 IO的方式允许它是一门纯函数式语言。
-- 任何与外界交互的函数(也就是 IO) 都在它的类型签名处做一个 `IO` 标志
-- 着让我们推出 什么样的函数是“纯洁的”(不与外界交互，不修改状态) 和 什么样的函数不是 “纯洁的”

-- 这是一个强有力的特征，因为并发地运行纯函数是简单的；因此，Haskell 中并发是非常简单的。


----------------------------------------------------
-- 9. The Haskell REPL
----------------------------------------------------

-- 键入 `ghci` 开始 repl。
-- 现在你可以键入 Haskell 代码。
-- 任何新值都需要通过 `let` 来创建：

let foo = 5

-- 你可以查看任何值的类型，通过命令 `:t`：

>:t foo
foo :: Integer

-- 你也可以运行任何 `IO ()`类型的动作

> sayHello
What is your name?
Friend!
Hello, Friend!

```

还有很多关于 Haskell，包括类型类和 monads。这些是使得编码 Haskell 是如此有趣的主意。我用一个最后的 Haskell 例子来结束：一个 Haskell 的快排实现：

```haskell
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```

安装 Haskell 是简单的。你可以从[这里](http://www.haskell.org/platform/)获得它。

你可以从优秀的
[Learn you a Haskell](http://learnyouahaskell.com/) 或者
[Real World Haskell](http://book.realworldhaskell.org/)
找到优雅不少的入门介绍。
