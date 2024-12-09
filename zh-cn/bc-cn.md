---
language: bc
contributors:
    - ["Btup"]
translators:
    - ["CloneWith", "https://github.com/CloneWith"]
filename: learnbc.bc
lang: zh-cn
---
```bc
/*这是一条
多行注释。*/
# 这（在 GNU bc 中）也是一条（单行）注释！

    /*1. 变量与指令结构*/
num = 45 /*所有变量都只以双精度浮点数形式存储，
    并且 bc 不支持直接存储字符串常量。*/
num = 45; /*每个语句后可以添加
    一个英文分号，也可以不加。*/
/*语句块使用 {} 运算符表示（与 C 语言相似）：*/
while(num < 50) {
    num += 1 /*等价于 num=num+1。
    a = a op b 等价于 a op= b。*/
}
/*也有 ++（自加）与 --（自减）运算符。*/
/*有三个特殊变量：
scale: 定义双精度浮点数字的比例。
ibase: 定义输入数值的基数。
obase: 定义输出数值的基数。*/
/*If 语句：*/
hour = read() /*输入一个数字*/

if(hour < 12) { /*运算符的用法与 C 语言类似。*/
    print "Good morning\n" /*“print”输出字符串或变量，用英文逗号分隔。*/
} else if(hour == 12) {
    print "Hello\n"
    /*字符串中的转义序列以反斜杠 \ 开头。
    为了讲述清楚，以下为 bc 中常用转义序列表：
    \b: 退格
    \c: 硬回车
    \n: 换行符
    \t: 制表符
    \\: 反斜杠*/
} else {
    print "Good afternoon\n"
}

/*像 C 语言一样，只有 0 定义为假（false）。*/
num = 0
if(!num) {print "false\n"}

/*与 C 语言不同，bc 没有 ?: 运算符。例如，
 这个代码块会导致出错：
a = (num) ? 1 : 0
但是你可以模拟一个：*/
a = (num) && (1) || (0) /*&& 代表“与”，|| 代表“或”*/

/*循环语句*/
num = 0
for(i = 1; i <= 100; i++) {/*与 C 语言中的循环类似。*/
    num += i
}

    /*2.函数与数组*/
define fac(n) { /*使用“define”定义函数。*/
    if(n == 1 || n == 0) {
        return 1 /*返回一个数值*/
    }
    return n * fac(n - 1) /*可以使用递归*/
}

/*不可使用闭包与匿名函数。*/

num = fac(4) /*24*/

/*这是局部变量的示例：*/
define x(n) {
    auto x
    x = 1
    return n + x
}
x(3) /*4*/
print x /*看起来无法在函数外访问 x。*/
/*数组与 C 语言中的等同。*/
for(i = 0; i <= 3; i++) {
    a[i] = 1
}
/*这样访问它：*/
print a[0], " ", a[1], " ", a[2], " ", a[3], "\n"
quit /*添加这行代码，确保程序退出。
这行代码可写可不写。*/
```

请享用这个简单的计算器吧！（或者确切地讲，这个编程语言。）

本程序全部使用 GNU bc 语言编写。要运行程序，请使用 ```bc learnbc.bc```。
