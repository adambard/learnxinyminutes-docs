---
language: wolfram
contributors:
  - ["hyphz", "http://github.com/hyphz/"]
translators:
  - ["wuyudi", "http://github.com/wuyudi/"]
filename: learnwolfram-cn.nb
lang: zh-cn
---

Wolfram 语言是最初在 Mathematica 中使用的底层语言，但现在可用于多种场合。

Wolfram 语言有几个界面。

- Raspberry Pi 上的命令行内核界面（就叫 _The Wolfram Language_），它是交互式运行的，不能产生图形输入。
- _Mathematica_ 是一个丰富的文本/数学编辑器，内置交互式的 Wolfram: 在 "代码单元 "上按 shift+Return 键可以创建一个输出单元，输出结果，这不是动态的。
- _Wolfram Workbench_，这是 Eclipse 与 Wolfram 语言后端的界面。

本例中的代码可以在任何界面中输入，并使用 Wolfram Workbench 进行编辑。直接加载到 Mathematica 中可能会很不方便，因为该文件不包含单元格格式化信息（这将使该文件作为文本阅读时变得一团糟）--它可以被查看/编辑，但可能需要一些设置。

```mathematica
(* 这是一个注释 *)

(* 在Mathematica中，您可以创建一个文本单元格，用排版好的文本和图像来注释您的代码，而不是使用这些注释 *)

(* 输入一个表达式返回结果 *)
2*2              (* 4 *)
5+8              (* 13 *)

(* 调用函数 *)
(* 注意，函数名（和其他所有东西）是区分大小写的 *)
Sin[Pi/2]        (* 1 *)

(* 带有一个参数的函数调用的替代语法 *)
Sin@(Pi/2)       (* 1 *)
(Pi/2) // Sin    (* 1 *)

(* WL 中的每一种语法都有一些等价的函数调用，即万物皆函数 *)
Times[2, 2]      (* 4 *)
Plus[5, 8]       (* 13 *)

(* 第一次使用一个变量定义它，并使其成为全局变量 *)
x = 5            (* 5 *)
x == 5           (* 返回真，C-style 的赋值和相等测试 *)
x                (* 5 *)
x = x + 5        (* 10 *)
x                (* 10 *)
Set[x, 20]       (* 当我说万物皆函数时 我是认真的 *)
x                (* 20 *)

(* 因为WL是基于计算机代数系统的, *)
(* 可以使用未定义的变量，它们只是阻碍了计算 *)
cow + 5          (* 5 + cow, cow 是未定义的，所以无法进一步计算 *)
cow + 5 + 10     (* 15 + cow, 它将尽力计算 *)
%                (* 15 + cow, % 获取最后一次返回的内容 *)
% - cow          (* 15, 未定义的变量cow被消去 *)
moo = cow + 5    (* 小心，moo 现在是一个表达式，不是一个数字! *)

(* 定义一个函数 *)
Double[x_] := x * 2    (*   注意 := 防止对RHS进行即时计算。
                            而 x 后面的 _ 表示没有模式匹配的约束条件*)
Double[10]             (* 20 *)
Double[Sin[Pi/2]]      (* 2 *)
Double @ Sin @ (Pi/2)  (* 2, @-syntax 避免了闭括号的队列 *)
(Pi/2) // Sin // Double(* 2, //-syntax 按执行顺序列举了函数 *)

(* 对于命令式编程，使用 ; 来分隔语句。 *)
(* 丢弃 LHS 的任何输出并运行 RHS *)
MyFirst[] := (Print@"Hello"; Print@"World")  (* 请注意，外侧的小括号是关键
                                                 ;的优先级低于:= *)
MyFirst[]                                    (* 你好，世界 *)

(* C-Style 的 For 循环，注：在 Mathematica 中使用 For 循环是低效的 *)
PrintTo[x_] := For[y=0, y<x, y++, (Print[y])]  (* 开始，测试，自增，循环体 *)
PrintTo[5]                                     (* 0 1 2 3 4 *)

(* While 循环 *)
x = 0; While[x < 2, (Print@x; x++)]     (* 带测试和程序体的 while 循环 *)

(* If 和 条件 *)
x = 8; If[x==8, Print@"Yes", Print@"No"]   (* Condition, true case, else case *)
Switch[x, 2, Print@"Two", 8, Print@"Yes"]  (* 值匹配风格切换 *)
Which[x==2, Print@"No", x==8, Print@"Yes"] (* Elif风格切换 *)

(* 除参数外的变量默认为全局变量，即使在函数内部也是如此 *)
y = 10             (* 10, 全局变量 y *)
PrintTo[5]         (* 0 1 2 3 4 *)
y                  (* 5, 全局的 y 被 PrintTo 内的循环计数器所占用 *)
x = 20             (* 20, 全局变量 x *)
PrintTo[5]         (* 0 1 2 3 4 *)
x                  (* 20, PrintTo 中的 x 是一个参数，并自动局部化 *)

(* 局部变量使用 Module 元函数声明 *)
(* 带本地变量的版本 *)
BetterPrintTo[x_] := Module[{y}, (For[y=0, y<x, y++, (Print@y)])]
y = 20             (* 全局变量 y *)
BetterPrintTo[5]   (* 0 1 2 3 4 *)
y                  (* 20, 那更好 *)

(* 实际上，模块允许我们声明任何我们喜欢的作用域 *)
Module[{count}, count=0;        (* 声明这个变量count的作用域 *)
  (IncCount[] := ++count);      (* 这些函数都在该作用域内 *)
  (DecCount[] := --count)]
count              (* count - 全局变量 count 未定义 *)
IncCount[]         (* 1, 使用作用域内的counter变量 *)
IncCount[]         (* 2, incCount 升级了它 *)
DecCount[]         (* 1, decCount 也是 *)
count              (* count - 这个名字依旧不是全局变量 *)

(* Lists *)
myList = {1, 2, 3, 4}     (* {1, 2, 3, 4} *)
myList[[1]]               (* 1 - 注意列表索引从 1 开始，而不是从 0 开始。 *)
Map[Double, myList]       (* {2, 4, 6, 8} - 函数式列表的 map 函数 *)
Double /@ myList          (* {2, 4, 6, 8} - 以上的缩略语法 *)
Scan[Print, myList]       (* 1 2 3 4 - 命令式循环 *)
Fold[Plus, 0, myList]     (* 10 (0+1+2+3+4) *)
FoldList[Plus, 0, myList] (* {0, 1, 3, 6, 10} - 存值的 Fold *)
Append[myList, 5]         (* {1, 2, 3, 4, 5} - 注意 myList 没有更新 *)
Prepend[myList, 5]        (* {5, 1, 2, 3, 4} - 如果想让 myList 更新，就加上 "myList = " *)
Join[myList, {3, 4}]      (* {1, 2, 3, 4, 3, 4} *)
myList[[2]] = 5          (* {1, 5, 3, 4} - 这确实更新了myList *)

(* 关联，又称字典/哈希值 *)
myHash = <|"Green" -> 2, "Red" -> 1|>   (* 创建一个关联 *)
myHash[["Green"]]                       (* 2, 使用 *)
myHash[["Green"]] := 5                  (* 5, 更新 *)
myHash[["Puce"]] := 3.5                 (* 3.5, 扩展 *)
KeyDropFrom[myHash, "Green"]            (* 抹去绿色的键 *)
Keys[myHash]                            (* {Red, Puce} *)
Values[myHash]                          (* {1, 3.5} *)

(* 做 Wolfram 的演示时 你不能不展示这个 *)
Manipulate[y^2, {y, 0, 20}] (* 返回一个反应式的用户界面，显示y^2
                               并允许通过滑块在 0-20 之间调整y。
                               只适用于图形前端 *)
```

## 想多来点?

- [Wolfram Language Documentation Center](http://reference.wolfram.com/language/)
