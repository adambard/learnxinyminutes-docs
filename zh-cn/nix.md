---
name: Nix
filename: learnnix.nix
contributors:
    - ["Chris Martin", "http://chris-martin.org/"]
    - ["Rommel Martinez", "https://ebzzry.io"]
    - ["Javier Candeira", "https://candeira.com/"]
translators:
  - ["Smile", "https://wiki.dev-hub.top"]
---

Nix 是一个为 [Nix 包管理器](https://nixos.org/nix/) 和 [NixOS](https://nixos.org/) 开发的简单函数式语言。

你可以使用 [nix-instantiate](https://nixos.org/nix/manual/#sec-nix-instantiate) 或 [`nix repl`](https://nixos.org/nix/manual/#ssec-relnotes-2.0) 来评估 Nix 表达式。

```nix
with builtins; [

  #  注释
  #=========================================

  # 行内注释看起来像这样。

  /* 多行注释
     看起来像这样。 */


  #  布尔值
  #=========================================

  (true && false)               # 与
  #=> false

  (true || false)               # 或
  #=> true

  (if 3 < 4 then "a" else "b")  # 条件
  #=> "a"


  #  整数和浮点数
  #=========================================

  # 有两种数值类型：整数和浮点数

  1 0 42 (-3)       # 一些整数

  123.43 .27e13     # 一对浮点数

  # 运算将保留数值类型

  (4 + 6 + 12 - 2)  # 加法
  #=> 20
  (4 - 2.5)
  #=> 1.5

  (7 / 2)           # 除法
  #=> 3
  (7 / 2.0)
  #=> 3.5


  #  字符串
  #=========================================

  "字符串字面量用双引号括起来。"

  "
    字符串字面量可以跨越
    多行。
  "

  ''
    这被称为“缩进字符串”字面量。
    它智能地去除前导空白。
  ''

  ''
    a
      b
  ''
  #=> "a\n  b"

  ("ab" + "cd")   # 字符串连接
  #=> "abcd"

  # 反引号允许你在字符串中嵌入值。
  ("Your home directory is ${getEnv "HOME"}")
  #=> "Your home directory is /home/alice"


  #  路径
  #=========================================

  # Nix 有一个用于路径的原始数据类型。
  /tmp/tutorials/learn.nix

  # 相对路径在解析时相对于它出现的文件解析为绝对路径。
  tutorials/learn.nix
  #=> /the-base-path/tutorials/learn.nix

  # 路径必须至少包含一个斜杠，因此同一目录中的文件的相对路径需要一个 ./ 前缀，
  ./learn.nix
  #=> /the-base-path/learn.nix

  # 如果你希望 / 运算符表示除法，它必须被空格包围。

  7/2        # 这是一个路径字面量
  (7 / 2)    # 这是整数除法


  #  导入
  #=========================================

  # 一个 nix 文件包含一个没有自由变量的顶级表达式。一个导入表达式评估为它导入的文件的值。
  (import /tmp/foo.nix)

  # 导入也可以通过字符串指定。
  (import "/tmp/foo.nix")

  # 导入路径必须是绝对路径。路径字面量会自动解析，因此这没问题。
  (import ./foo.nix)

  # 但字符串不会这样。
  (import "./foo.nix")
  #=> 错误：字符串‘foo.nix’不代表绝对路径


  #  Let
  #=========================================

  # `let` 块允许我们将值绑定到变量。
  (let x = "a"; in
    x + x + x)
  #=> "aaa"

  # 绑定可以相互引用，并且它们的顺序无关紧要。
  (let y = x + "b";
       x = "a"; in
    y + "c")
  #=> "abc"

  # 内部绑定会覆盖外部绑定。
  (let a = 1; in
    let a = 2; in
      a)
  #=> 2


  #  函数
  #=========================================

  (n: n + 1)      # 添加 1 的函数

  ((n: n + 1) 5)  # 同一个函数，应用于 5
  #=> 6

  # 没有命名函数的语法，但它们可以像任何其他值一样通过 `let` 块绑定。
  (let succ = (n: n + 1); in succ 5)
  #=> 6

  # 一个函数恰好有一个参数。
  # 多个参数可以通过柯里化实现。
  ((x: y: x + "-" + y) "a" "b")
  #=> "a-b"

  # 我们还可以有命名函数参数，
  # 我们将在引入集合后再讨论。


  #  列表
  #=========================================

  # 列表用方括号表示。

  (length [1 2 3 "x"])
  #=> 4

  ([1 2 3] ++ [4 5])
  #=> [1 2 3 4 5]

  (concatLists [[1 2] [3 4] [5]])
  #=> [1 2 3 4 5]

  (head [1 2 3])
  #=> 1
  (tail [1 2 3])
  #=> [2 3]

  (elemAt ["a" "b" "c" "d"] 2)
  #=> "c"

  (elem 2 [1 2 3])
  #=> true
  (elem 5 [1 2 3])
  #=> false

  (filter (n: n < 3) [1 2 3 4])
  #=> [ 1 2 ]


  #  集合
  #=========================================

  # “集合”是一个无序的字符串键映射。
  { foo = [1 2]; bar = "x"; }

  # . 运算符从集合中提取一个值。
  { a = 1; b = 2; }.a
  #=> 1

  # ? 运算符测试一个键是否存在于集合中。
  ({ a = 1; b = 2; } ? a)
  #=> true
  ({ a = 1; b = 2; } ? c)
  #=> false

  # // 运算符合并两个集合。
  ({ a = 1; } // { b = 2; })
  #=> { a = 1; b = 2; }

  # 右边的值覆盖左边的值。
  ({ a = 1; b = 2; } // { a = 3; c = 4; })
  #=> { a = 3; b = 2; c = 4; }

  # rec 关键字表示一个“递归集合”，
  # 其中属性可以相互引用。
  (let a = 1; in     { a = 2; b = a; }.b)
  #=> 1
  (let a = 1; in rec { a = 2; b = a; }.b)
  #=> 2

  # 嵌套集合可以分段定义。
  {
    a.b   = 1;
    a.c.d = 2;
    a.c.e = 3;
  }.a.c
  #=> { d = 2; e = 3; }

  # 集合是不可变的，因此你不能重新定义一个属性：
  {
    a = { b = 1; };
    a.b = 2;
  }
  #=> 属性 'a.b' 在 (string):3:5 处已在 (string):2:11 处定义

  # 然而，即使属性本身已被直接赋值，属性的集合成员也可以分段定义。
  {
    a = { b = 1; };
    a.c = 2;
  }
  #=> { a = { b = 1; c = 2; }; }


  #  With
  #=========================================

  # `with` 块的主体在集合的映射绑定到变量时进行评估。
  (with { a = 1; b = 2; };
    a + b)
  # => 3

  # 内部绑定会覆盖外部绑定。
  (with { a = 1; b = 2; };
    (with { a = 5; };
      a + b))
  #=> 7

  # 本教程的第一行以 "with builtins;" 开始，
  # 因为 builtins 是一个包含所有内置函数（length、head、tail、filter 等）的集合。
  # 这使我们不必写 "builtins.length" 而只需写 "length"。


  #  集合模式
  #=========================================

  # 当我们需要将多个值传递给函数时，集合很有用。
  (args: args.x + "-" + args.y) { x = "a"; y = "b"; }
  #=> "a-b"

  # 这可以用集合模式更清晰地写出来。
  ({x, y}: x + "-" + y) { x = "a"; y = "b"; }
  #=> "a-b"

  # 默认情况下，模式在包含额外键的集合上失败。
  ({x, y}: x + "-" + y) { x = "a"; y = "b"; z = "c"; }
  #=> error: anonymous function called with unexpected argument ‘z’

  # 添加 ", ..." 允许忽略额外的键。
  ({x, y, ...}: x + "-" + y) { x = "a"; y = "b"; z = "c"; }
  #=> "a-b"

  # 整个集合可以使用 `@` 绑定到一个变量
  (args@{x, y}: args.x + "-" + args.y) { x = "a"; y = "b"; }
  #=> "a-b"

  #  错误
  #=========================================

  # `throw` 导致评估中止并显示错误消息。
  (2 + (throw "foo"))
  #=> error: foo

  # `tryEval` 捕获抛出的错误。
  (tryEval 42)
  #=> { success = true; value = 42; }
  (tryEval (2 + (throw "foo")))
  #=> { success = false; value = false; }

  # `abort` 类似于 throw，但它是致命的；它无法被捕获。
  (tryEval (abort "foo"))
  #=> error: evaluation aborted with the following error message: ‘foo’

  # `assert` 如果为真，则评估为给定值；
  # 否则它会抛出一个可捕获的异常。
  (assert 1 < 2; 42)
  #=> 42
  (assert 1 > 2; 42)
  #=> error: assertion failed at (string):1:1
  (tryEval (assert 1 > 2; 42))
  #=> { success = false; value = false; }


  #  不纯性
  #=========================================

  # 因为构建的可重复性对 Nix 包管理器至关重要，
  # 函数式纯度在用于描述 Nix 包的 Nix 语言中得到了强调。
  # 但也有一些不纯之处。

  # 您可以引用环境变量。
  (getEnv "HOME")
  #=> "/home/alice"

  # trace函数用于调试。它将第一个参数打印到stderr，并计算第二个参数。
  (trace 1 2)
  #=> trace: 1
  #=> 2

  # 您可以将文件写入Nix存储。尽管不纯，但这相当安全，因为文件名是从其内容的哈希中派生的。您可以从任何地方读取文件。在此示例中，我们将文件写入存储，然后再读出。
  (let filename = toFile "foo.txt" "hello!"; in
    [filename (readFile filename)])
  #=> [ "/nix/store/ayh05aay2anx135prqp0cy34h891247x-foo.txt" "hello!" ]

  # 我们还可以将文件下载到Nix存储中。
  (fetchurl "https://example.com/package-1.2.3.tgz")
  #=> "/nix/store/2drvlh8r57f19s9il42zg89rdr33m2rm-package-1.2.3.tgz"

]
```

### 进一步阅读

* [Nix Manual - Nix表达式语言](https://nixos.org/nix/manual/#ch-expression-language)
* [James Fisher - Nix 示例 - Part 1: The Nix expression language](https://medium.com/@MrJamesFisher/nix-by-example-a0063a1a4c55)
* [Susan Potter - Nix Cookbook - Nix 示例](https://ops.functionalalgebra.com/nix-by-example/)
* [Zero to Nix - Nix教程](https://zero-to-nix.com/)
* [Rommel Martinez - Nix 家族简介](https://web.archive.org/web/20210121042658/https://ebzzry.io/en/nix/#nix)
