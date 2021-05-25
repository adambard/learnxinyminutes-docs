---
category: tool
tool: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
    - ["Denis Arh", "https://github.com/darh"]
    - ["akirahirose", "https://twitter.com/akirahirose"]
    - ["Anton Strömkvist", "http://lutic.org/"]
    - ["Rahil Momin", "https://github.com/iamrahil"]
    - ["Gregrory Kielian", "https://github.com/gskielian"]
    - ["Etan Reisner", "https://github.com/deryni"]
translators:
    - ["Jinchang Ye", "https://github.com/Alwayswithme"]
    - ["Chunyang Xu", "https://github.com/XuChunyang"]
filename: LearnBash-cn.sh
lang: zh-cn
---

Bash 是一个为 GNU 计划编写的 Unix shell，是 Linux 和 macOS 下的默认 shell。
以下大多数例子可以作为脚本的一部分运行，也可直接在 shell 下交互执行。

[更多信息](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# 脚本的第一行叫 shebang，用来告知系统如何执行该脚本:
# 参见： http://en.wikipedia.org/wiki/Shebang_(Unix)
# 如你所见，注释以 # 开头，shebang 也是注释。

# 显示 “Hello world!”
echo Hello world!

# 每一句指令以换行或分号隔开：
echo 'This is the first line'; echo 'This is the second line'

# 声明一个变量：
Variable="Some string"

# 下面是错误的做法：
Variable = "Some string"
# Bash 会把 Variable 当做一个指令，由于找不到该指令，因此这里会报错。

# 也不可以这样：
Variable= 'Some string'
# Bash 会认为 'Some string' 是一条指令，由于找不到该指令，这里再次报错。
# （这个例子中 'Variable=' 这部分会被当作仅对 'Some string' 起作用的赋值。）

# 使用变量：
echo $Variable
echo "$Variable"
echo '$Variable'
# 当你赋值 (assign) 、导出 (export)，或者以其他方式使用变量时，变量名前不加 $。
# 如果要使用变量的值， 则要加 $。
# 注意： ' (单引号) 不会展开变量（即会屏蔽掉变量）。


# 在变量内部进行字符串代换
echo ${Variable/Some/A}
# 会把 Variable 中首次出现的 "some" 替换成 “A”。

# 变量的截取
Length=7
echo ${Variable:0:Length}
# 这样会仅返回变量值的前7个字符

# 变量的默认值
echo ${Foo:-"DefaultValueIfFooIsMissingOrEmpty"}
# 对 null (Foo=) 和空串 (Foo="") 起作用； 零（Foo=0）时返回0
# 注意这仅返回默认值而不是改变变量的值

# 内置变量：
# 下面的内置变量很有用
echo "Last program return value: $?"
echo "Script's PID: $$"
echo "Number of arguments: $#"
echo "Scripts arguments: $@"
echo "Scripts arguments separated in different variables: $1 $2..."

# 读取输入：
echo "What's your name?"
read Name # 这里不需要声明新变量
echo Hello, $Name!

# 通常的 if 结构看起来像这样：
# 'man test' 可查看更多的信息
if [ $Name -ne $USER ]
then
    echo "Your name isn't your username"
else
    echo "Your name is your username"
fi

# 根据上一个指令执行结果决定是否执行下一个指令
echo "Always executed" || echo "Only executed if first command fails"
echo "Always executed" && echo "Only executed if first command does NOT fail"

# 在 if 语句中使用 && 和 || 需要多对方括号
if [ $Name == "Steve" ] && [ $Age -eq 15 ]
then
    echo "This will run if $Name is Steve AND $Age is 15."
fi

if [ $Name == "Daniya" ] || [ $Name == "Zach" ]
then
    echo "This will run if $Name is Daniya OR Zach."
fi

# 表达式的格式如下:
echo $(( 10 + 5 ))

# 与其他编程语言不同的是，bash 运行时依赖上下文。比如，使用 ls 时，列出当前目录。
ls

# 指令可以带有选项：
ls -l # 列出文件和目录的详细信息

# 前一个指令的输出可以当作后一个指令的输入。grep 用来匹配字符串。
# 用下面的指令列出当前目录下所有的 txt 文件：
ls -l | grep "\.txt"

# 重定向输入和输出（标准输入，标准输出，标准错误）。
# 以 ^EOF$ 作为结束标记从标准输入读取数据并覆盖 hello.py :
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF

# 重定向可以到输出，输入和错误输出。
python hello.py < "input.in"
python hello.py > "output.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# > 会覆盖已存在的文件， >> 会以累加的方式输出文件中。
python hello.py >> "output.out" 2>> "error.err"

# 覆盖 output.out , 追加 error.err 并统计行数
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

# 运行指令并打印文件描述符 （比如 /dev/fd/123）
# 具体可查看： man fd
echo <(echo "#helloworld")

# 以 "#helloworld" 覆盖 output.out:
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out >/dev/null

# 清理临时文件并显示详情（增加 '-i' 选项启用交互模式）
rm -v output.out error.err output-and-error.log

# 一个指令可用 $( ) 嵌套在另一个指令内部：
# 以下的指令会打印当前目录下的目录和文件总数
echo "There are $(ls | wc -l) items here."

# 反引号 `` 起相同作用，但不允许嵌套
# 优先使用 $(  ).
echo "There are `ls | wc -l` items here."

# Bash 的 case 语句与 Java 和 C++ 中的 switch 语句类似:
case "$Variable" in
    # 列出需要匹配的字符串
    0) echo "There is a zero.";;
    1) echo "There is a one.";;
    *) echo "It is not null.";;
esac

# 循环遍历给定的参数序列:
# 变量$Variable 的值会被打印 3 次。
for Variable in {1..3}
do
    echo "$Variable"
done

# 或传统的 “for循环” ：
for ((a=1; a <= 3; a++))
do
    echo $a
done

# 也可以用于文件
# 用 cat 输出 file1 和 file2 内容
for Variable in file1 file2
do
    cat "$Variable"
done

# 或作用于其他命令的输出
# 对 ls 输出的文件执行 cat 指令。
for Output in $(ls)
do
    cat "$Output"
done

# while 循环：
while [ true ]
do
    echo "loop body here..."
    break
done

# 你也可以使用函数
# 定义函数：
function foo ()
{
    echo "Arguments work just like script arguments: $@"
    echo "And: $1 $2..."
    echo "This is a function"
    return 0
}

# 更简单的方法
bar ()
{
    echo "Another way to declare functions!"
    return 0
}

# 调用函数
foo "My name is" $Name

# 有很多有用的指令需要学习:
# 打印 file.txt 的最后 10 行
tail -n 10 file.txt
# 打印 file.txt 的前 10 行
head -n 10 file.txt
# 将 file.txt 按行排序
sort file.txt
# 报告或忽略重复的行，用选项 -d 打印重复的行
uniq -d file.txt
# 打印每行中 ',' 之前内容
cut -d ',' -f 1 file.txt
# 将 file.txt 文件所有 'okay' 替换为 'great', （兼容正则表达式）
sed -i 's/okay/great/g' file.txt
# 将 file.txt 中匹配正则的行打印到标准输出
# 这里打印以 "foo" 开头, "bar" 结尾的行
grep "^foo.*bar$" file.txt
# 使用选项 "-c" 统计行数
grep -c "^foo.*bar$" file.txt
# 如果只是要按字面形式搜索字符串而不是按正则表达式，使用 fgrep (或 grep -F)
fgrep "^foo.*bar$" file.txt 


# 以 bash 内建的 'help' 指令阅读 Bash 自带文档：
help
help help
help for
help return
help source
help .

# 用 man 指令阅读相关的 Bash 手册
apropos bash
man 1 bash
man bash

# 用 info 指令查阅命令的 info 文档 （info 中按 ? 显示帮助信息）
apropos info | grep '^info.*('
man info
info info
info 5 info

# 阅读 Bash 的 info 文档：
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
