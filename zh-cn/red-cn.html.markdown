---
name: Red
category: language
language: Red
filename: LearnRed-zh.red
contributors:
    - ["Arnold van Hofwegen", "https://github.com/iArnold"]
translators:
    - ["Limo Saplf", "https://github.com/saplf"]
lang: zh-cn
---

Red 的编写是出于工作需要，该语言的作者想要使用 REBOL，但它有许多缺陷。
当时 REBOL 还没有开源，由于它是一门解释型语言，这就意味着它比编译型语言效率低。

Red 使用 C 语言级别的 Red/System，是一门涉及所有编程领域的语言。
Red 基于 REBOL 编写，它继承了 REBOL 的灵活性，同时也包含了许多 C 语言能做的底层实现。

Red 将会成为世界上第一门全栈式编程语言，这意味着它可以完成几乎所有的编程任务，从底层到抽象，无需其他工具的参与。
而且，Red 支持交叉编译，任意两个平台之间，不需要任何 GCC 之类的工具链的支持。
所有的工作，仅仅需要一个不到 1 MB 的二进制可执行文件。

准备好你的 Red 第一课了吗？

```red
所有 header 之前的文字都是注释，只要你不使用 "red" 关键字，其中的 "r" 大写。
这是词法分析器的一个缺陷，所以大多数时候，你都应该直接以 header 开始程序或者脚本的编写。

red 脚本的 header 由关键字，首字母大写的 "red" 开始，后跟一个空格，再跟一对方括号 []。
方括号里可以写上一些关于这段脚本或者程序的相关信息：
作者，文件名，版本号，license，程序功能的简介，它依赖的其他文件。
red/System 的 header 和 red header 类似，仅仅是说明 "red/System" 而非 "red"。


Red []

; 这是一条行注释

print "Hello Red World"    ; 另一条注释

comment {
    这是多行注释。
    你刚刚看到的就是 Red 版的 Hello World。
}

; 程序的入口就是第一句可执行的代码
; 不需要把它放在 'main' 函数里

; 变量名以一个字母开始，可以包含数字，
; 只包含 A ~ F 字符和数字的变量名不能以 'h' 结尾，
; 因为这是 Red 和 Red/System 中十六进制数字的表达方式。

; 给变量赋值使用冒号 ":"
my-name: "Red"
reason-for-using-the-colon: {使用冒号作为赋值符号
 是为了能够让 "=" 能够用来作为比较符号，这本来就是 "="
 存在的意义！还记得上学时学的，y = x + 1 、 x = 1，
 以及推导出的 y = 2 吗？
}
is-this-name-valid?: true

; 用 print 打印输出，prin 打印不带换行的输出

prin "我的名字是 " print my-name
; 我的名字是 Red

print ["我的名字是 " my-name lf]
; 我的名字是 Red

; 注意到了吗：语句没有以分号结尾 ;-)

;
; 数据类型
;
; 如果你了解 Rebol，你可能就会注意到它有许多数据类型。
; Red 并没有囊括它所有的类型，但由于 Red 想要尽可能的
; 接近 Rebol，所以它也会有很多数据类型。
; 类型以叹号结尾，但要注意，变量名也是可以以叹号结尾的。
; 一些内置类型有 integer! string! block!

; 使用变量前需要声明吗？
; Red 能够分辨什么时候使用什么变量，变量声明并非必要的。
; 一般认为，声明变量是较好的编码实践，但 Red 并不会强制这点。
; 你可以声明一个变量然后指定它的类型，而一个变量的类型就
; 指定了它的字节大小。

; integer! 类型的变量通常是 4 字节，32位
my-integer: 0
; Red 的整型包含符号，暂时不支持无符号类型，但以后会支持的。

; 怎样判断一个变量的类型？
type? my-integer
; integer!

; 一个变量的初始化可以使用另一个同样刚刚初始化的变量：
i2: 1 + i1: 1

; 算数运算符
i1 + i2 ; 3
i2 - i1 ; 1
i2 * i1 ; 2
i1 / i2 ; 0 (0.5，但截取为 0)

; 比较运算符都差不多，但和其他语言不一样的是相等的比较，
; Red 使用单个的 '='。
; Red 有一个类似 boolean 的类型，它的值是 true 和 false，
; 但也可以使用 on/off 或者 yes/on

3 = 2  ; false
3 != 2 ; true
3 > 2  ; true
3 < 2  ; false
2 <= 2 ; true
2 >= 2 ; true

;
; 控制流
;
; if
; 如果给定的条件为 true 则执行一段代码块。
; if 没有返回值，所以不能用作表达式。
if a < 0 [print "a 是负值"]

; either
; 如果给定的条件为 true 则执行一段代码块，否则就
; 执行另一段可选的代码块。如果两个代码块中最后一个表达式
; 的类型相同， either 就可以用作表达式。
either a > 0 [
    msg: "正值"
][
    either a = 0 [
        msg: "零"
    ][
        msg: "负值"
    ]
]
print ["a 是 " msg lf]

; 还可以有另一种写法
; （因为两条路径的返回值相同，所以可以这么写）：

msg: either a > 0 [
    "正值"
][
    either a = 0 [
        "零"
    ][
        "负值"
    ]
]
print ["a 是 " msg lf]

; util
; 循环执行一段代码块，直到满足给定的条件为止。
; util 没有返回值，所以它不能用在表示式中。
c: 5
util [
    prin "o"
    c: c - 1
    c = 0    ; 终止循环的条件
]
; 输出：ooooo
; 需要注意的是，即使条件从一开始就不满足，
; 这个循环也至少会执行一次。

; while
; 当满足给定的条件，就执行一段代码。
; while 没有返回值，不能用在表达式中。
c: 5
while [c > 0][
    prin "o"
    c: c - 1
]
; 输出：ooooo

;
; 函数
;
; 函数示例
twice: function [a [integer!] /one return: [integer!]][
        c: 2
        a: a * c
        either one [a + 1][a]
]
b: 3
print twice b      ; 输出 6

; 使用 #include 和 %文件名 来导入外部文件
#include %includefile.red
; 现在就可以使用 includefile.red 中的函数了。

```

## 更进一步

Red 相关的源码信息在 [Red 语言主页](http://www.red-lang.org)。

源代码的 [github 库](https://github.com/red/red)。

Red/System 特性在 [这里](http://static.red-lang.org/red-system-specs-light.html)。

想要了解更多关于 Rebol 和 Red 的信息，加入 [Gitter 聊天室](https://gitter.im/red/red)。如果你无法加入，也可以给我们发[邮件](mailto:red-langNO_SPAM@googlegroups.com)。

也可以在 [Stack Overflow](stackoverflow.com/questions/tagged/red) 上查阅、提交问题。

也许你现在就要试一试 Red ？可以在线尝试 [try Rebol and Red site](http://tryrebol.esperconsultancy.nl)。

你也可以通过学习一些 [Rebol](http://www.rebol.com/docs.html) 来学习 Red。
