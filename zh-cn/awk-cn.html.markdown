---
category: tool
tool: awk
contributors:
     - ["Marshall Mason", "http://github.com/marshallmason"]
translators:
    - ["Tian Zhipeng", "https://github.com/tianzhipeng-git"]
filename: learnawk-cn.awk
lang: zh-cn
---

AWK是POSIX兼容的UNIX系统中的标准工具. 它像简化版的Perl, 非常适用于文本处理任务和其他脚本类需求.
它有着C风格的语法, 但是没有分号, 没有手动内存管理, 没有静态类型. 
他擅长于文本处理, 你可以通过shell脚本调用AWK, 也可以用作独立的脚本语言.

为什么使用AWK而不是Perl, 大概是因为AWK是UNIX的一部分, 你总能依靠它, 而Perl已经前途未卜了.
AWK比Perl更易读. 对于简单的文本处理脚本, 特别是按行读取文件, 按分隔符分隔处理, AWK极可能是正确的工具.

```awk
#!/usr/bin/awk -f

# 注释使用井号

# AWK程序由一系列 模式(patterns) 和 动作(actions) 组成. 
# 最重要的模式叫做 BEGIN. 动作由大括号包围.
BEGIN {

    # BEGIN在程序最开始运行. 在这里放一些在真正处理文件之前的准备和setup的代码.
    # 如果没有文本文件要处理, 那就把BEGIN作为程序的主入口吧.

    # 变量是全局的. 直接赋值使用即可, 无需声明.
    count = 0

    # 运算符和C语言系一样
    a = count + 1
    b = count - 1
    c = count * 1
    d = count / 1 # 整数除法
    e = count % 1 # 取余
    f = count ^ 1 # 取幂

    a += 1
    b -= 1
    c *= 1
    d /= 1
    e %= 1
    f ^= 1

    # 自增1, 自减1
    a++
    b--

    # 前置运算, 返回增加之后的值
    ++a
    --b

    # 注意, 不需要分号之类的标点来分隔语句

    # 控制语句
    if (count == 0)
        print "Starting with count of 0"
    else
        print "Huh?"

    # 或者三目运算符
    print (count == 0) ? "Starting with count of 0" : "Huh?"

    # 多行的代码块用大括号包围
    while (a < 10) {
        print "String concatenation is done" " with a series" " of"
            " space-separated strings"
        print a

        a++
    }

    for (i = 0; i < 10; i++)
        print "Good ol' for loop"

    # 标准的比较运算符
    a < b   # 小于
    a <= b  # 小于或等于
    a != b  # 不等于
    a == b  # 等于
    a > b   # 大于
    a >= b  # 大于或等于

    # 也有逻辑运算符
    a && b  # 且
    a || b  # 或

    # 并且有超实用的正则表达式匹配
    if ("foo" ~ "^fo+$")
        print "Fooey!"
    if ("boo" !~ "^fo+$")
        print "Boo!"

    # 数组
    arr[0] = "foo"
    arr[1] = "bar"
    # 不幸的是, 没有其他方式初始化数组. 必须像这样一行一行的赋值.

    # 关联数组, 类似map或dict的用法.
    assoc["foo"] = "bar"
    assoc["bar"] = "baz"

    # 多维数组. 但是有一些局限性这里不提了.
    multidim[0,0] = "foo"
    multidim[0,1] = "bar"
    multidim[1,0] = "baz"
    multidim[1,1] = "boo"

    # 可以检测数组包含关系
    if ("foo" in assoc)
        print "Fooey!"

    # 可以使用in遍历数组
    for (key in assoc)
        print assoc[key]

    # 命令行参数是一个叫ARGV的数组
    for (argnum in ARGV)
        print ARGV[argnum]

    # 可以从数组中移除元素
    # 在 防止awk把文件参数当做数据来处理 时delete功能很有用.
    delete ARGV[1]

    # 命令行参数的个数是一个叫ARGC的变量
    print ARGC

    # AWK有很多内置函数, 分为三类, 会在接下来定义的各个函数中介绍.

    return_value = arithmetic_functions(a, b, c)
    string_functions()
    io_functions()
}

# 定义函数
function arithmetic_functions(a, b, c,     d) {

    # 或许AWK最让人恼火的地方是没有局部变量, 所有东西都是全局的, 
    # 对于短的脚本还好, 对于长一些的就会成问题.

    # 这里有一个技巧, 函数参数是对函数局部可见的, 并且AWK允许定义多余的参数, 
    # 因此可以像上面那样把局部变量插入到函数声明中. 
    # 为了方便区分普通参数(a,b,c)和局部变量(d), 可以多键入一些空格.

    # 现在介绍数学类函数

    # 多数AWK实现中包含标准的三角函数
    localvar = sin(a)
    localvar = cos(a)
    localvar = atan2(a, b) # arc tangent of b / a

    # 对数
    localvar = exp(a)
    localvar = log(a)

    # 平方根
    localvar = sqrt(a)

    # 浮点型转为整型
    localvar = int(5.34) # localvar => 5

    # 随机数
    srand() # 接受随机种子作为参数, 默认使用当天的时间
    localvar = rand() # 0到1之间随机

    # 函数返回
    return localvar
}

function string_functions(    localvar, arr) {

    # AWK, 作为字符处理语言, 有很多字符串相关函数, 其中大多数都严重依赖正则表达式.

    # 搜索并替换, 第一个出现的 (sub) or 所有的 (gsub)
    # 都是返回替换的个数
    localvar = "fooooobar"
    sub("fo+", "Meet me at the ", localvar) # localvar => "Meet me at the bar"
    gsub("e+", ".", localvar) # localvar => "m..t m. at th. bar"

    # 搜索匹配正则的字符串
    # index() 也是搜索, 不支持正则
    match(localvar, "t") # => 4, 't'在4号位置. 
    # (译者注: awk是1开始计数的,不是常见的0-base)

    # 按分隔符分隔
    split("foo-bar-baz", arr, "-") # a => ["foo", "bar", "baz"]

    # 其他有用的函数
    sprintf("%s %d %d %d", "Testing", 1, 2, 3) # => "Testing 1 2 3"
    substr("foobar", 2, 3) # => "oob"
    substr("foobar", 4) # => "bar"
    length("foo") # => 3
    tolower("FOO") # => "foo"
    toupper("foo") # => "FOO"
}

function io_functions(    localvar) {

    # 你已经见过的print函数
    print "Hello world"

    # 也有printf
    printf("%s %d %d %d\n", "Testing", 1, 2, 3)

    # AWK本身没有文件句柄, 当你使用需要文件的东西时会自动打开文件, 
    # 做文件I/O时, 字符串就是打开的文件句柄. 这看起来像Shell
    print "foobar" >"/tmp/foobar.txt"

    # 现在"/tmp/foobar.txt"字符串是一个文件句柄, 你可以关闭它
    close("/tmp/foobar.txt")

    # 在shell里运行一些东西
    system("echo foobar") # => prints foobar

    # 从标准输入中读一行, 并存储在localvar中
    getline localvar

    # 从管道中读一行, 并存储在localvar中
    "echo foobar" | getline localvar # localvar => "foobar"
    close("echo foobar")

    # 从文件中读一行, 并存储在localvar中
    getline localvar <"/tmp/foobar.txt"
    close("/tmp/foobar.txt")
}

# 正如开头所说, AWK程序由一系列模式和动作组成. 你已经看见了重要的BEGIN pattern, 
# 其他的pattern在你需要处理来自文件或标准输入的的数据行时才用到.
# 
# 当你给AWK程序传参数时, 他们会被视为要处理文件的文件名, 按顺序全部会处理. 
# 可以把这个过程看做一个隐式的循环, 遍历这些文件中的所有行.
# 然后这些模式和动作就是这个循环里的switch语句一样

/^fo+bar$/ {
    
    # 这个动作会在匹配这个正则(/^fo+bar$/)的每一行上执行. 不匹配的则会跳过.
    # 先让我们打印它:
    print

    # 哦, 没有参数, 那是因为print有一个默认参数 $0.
    # $0 是当前正在处理的行, 自动被创建好了.

    # 你可能猜到有其他的$变量了. 
    # 每一行在动作执行前会被分隔符分隔. 像shell中一样, 每个字段都可以用$符访问

    # 这个会打印这行的第2和第4个字段
    print $2, $4

    # AWK自动定义了许多其他的变量帮助你处理行. 最常用的是NF变量
    # 打印这一行的字段数
    print NF

    # 打印这一行的最后一个字段
    print $NF
}

# 每一个模式其实是一个true/false判断, 上面那个正则其实也是一个true/false判断, 只不过被部分省略了.
# 没有指定时默认使用当前处理的整行($0)进行匹配. 因此, 完全版本是这样:

$0 ~ /^fo+bar$/ {
    print "Equivalent to the last pattern"
}

a > 0 {
    # 只要a是整数, 这块会在每一行上执行.
}

# 就是这样, 处理文本文件, 一次读一行, 对行做一些操作. 
# 按分隔符分隔, 这在UNIX中很常见, awk都帮你做好了.
# 你所需要做的是基于自己的需求写一些模式和动作.

# 这里有一个快速的例子, 展示了AWK所擅长做的事.
# 它从标准输入读一个名字, 打印这个first name下所有人的平均年龄.
# 示例数据:
#
# Bob Jones 32
# Jane Doe 22
# Steve Stevens 83
# Bob Smith 29
# Bob Barker 72
#
# 示例脚本:

BEGIN {

    # 首先, 问用户要一个名字
    print "What name would you like the average age for?"

    # 从标准输入获取名字
    getline name <"/dev/stdin"
}

# 然后, 用给定的名字匹配每一行的第一个字段.
$1 == name {

    # 这里我们要使用几个有用的变量, 已经提前为我们加载好的:
    # $0 是整行
    # $3 是第三个字段, 就是我们所感兴趣的年龄
    # NF 字段数, 这里是3
    # NR 至此为止的行数
    # FILENAME 在处理的文件名
    # FS 在使用的字段分隔符, 这里是空格" "
    # ...等等, 还有很多, 在帮助文档中列出.

    # 跟踪 总和以及行数
    sum += $3
    nlines++
}

# 另一个特殊的模式叫END. 它会在处理完所有行之后运行. 不像BEGIN, 它只会在有输入的时候运行.
# 它在所有文件依据给定的模式和动作处理完后运行, 目的通常是输出一些最终报告, 做一些数据聚合操作.

END {
    if (nlines)
        print "The average age for " name " is " sum / nlines
}

```
更多:

* [Awk 教程](http://www.grymoire.com/Unix/Awk.html)
* [Awk 手册](https://linux.die.net/man/1/awk)
* [The GNU Awk 用户指南](https://www.gnu.org/software/gawk/manual/gawk.html) GNU Awk在大多数Linux中预装
