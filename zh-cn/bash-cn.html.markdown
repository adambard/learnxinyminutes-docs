---
category: tool
tool: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
translators:
    - ["Chunyang Xu", "https://github.com/XuChunyang"]
filename: LearnBash-cn.sh
lang: zh-cn
---

Bash 是一个为 GNU 计划编写的 Unix shell，是 Linux 和 Mac OS X 下的默认 shell。
以下大多数例子可以作为脚本的一部分运行，也可直接在 shell 下交互执行。

[更多信息](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# 脚本的第一行叫 shebang，用来告知系统如何执行该脚本:
# 参见： http://en.wikipedia.org/wiki/Shebang_(Unix)
# 如你所见，注释以 # 开头，shebang 也是注释。

# 显示 “Hello world!”
echo Hello, world!

# 每一句指令以换行或分号隔开：
echo 'This is the first line'; echo 'This is the second line'

# 声明一个变量：
VARIABLE="Some string"

# 下面是错误的做法：
VARIABLE = "Some string"
# Bash 会把 VARIABLE 当做一个指令，由于找不到该指令，因此这里会报错。


# 使用变量：
echo $VARIABLE
echo "$VARIABLE"
echo '$VARIABLE'
# 当你赋值 (assign) 、导出 (export)，或者以其他方式使用变量时，变量名前不加 $。
# 如果要使用变量的值， 则要加 $。
# 注意： ' (单引号) 不会展开变量（即会屏蔽掉变量）。


# 在变量内部进行字符串代换
echo ${VARIABLE/Some/A}
# 会把 VARIABLE 中首次出现的 "some" 替换成 “A”。

# 内置变量：
# 下面的内置变量很有用
echo "Last program return value: $?"
echo "Script's PID: $$"
echo "Number of arguments: $#"
echo "Scripts arguments: $@"
echo "Scripts arguments separeted in different variables: $1 $2..."

# 读取输入：
echo "What's your name?"
read NAME # 这里不需要声明新变量
echo Hello, $NAME!

# 通常的 if 结构看起来像这样：
# 'man test' 可查看更多的信息
if [ $NAME -ne $USER ]
then
    echo "Your name is you username"
else
    echo "Your name isn't you username"
fi

# 根据上一个指令执行结果决定是否执行下一个指令
echo "Always executed" || echo "Only executed if first command fail"
echo "Always executed" && echo "Only executed if first command does NOT fail"

# 表达式的格式如下:
echo $(( 10 + 5 ))

# 与其他编程语言不同的是，bash 运行时依赖上下文。比如，使用 ls 时，列出当前目录。
ls

# 指令可以带有选项：
ls -l # 列出文件和目录的详细信息

# 前一个指令的输出可以当作后一个指令的输入。grep 用来匹配字符串。
# 用下面的指令列出当前目录下所有的 txt 文件：
ls -l | grep "\.txt"

# 重定向可以到输出，输入和错误输出。
python2 hello.py < "input.in"
python2 hello.py > "output.out"
python2 hello.py 2> "error.err"
# > 会覆盖已存在的文件， >> 会以累加的方式输出文件中。

# 一个指令可用 $( ) 嵌套在另一个指令内部：
# 以下的指令会打印当前目录下的目录和文件总数
echo "There are $(ls | wc -l) items here."

# Bash 的 case 语句与 Java 和 C++ 中的 switch 语句类似:
case "$VARIABLE" in
    # 列出需要匹配的字符串
    0) echo "There is a zero.";;
    1) echo "There is a one.";;
    *) echo "It is not null.";;
esac

# 循环遍历给定的参数序列:
# 变量$VARIABLE 的值会被打印 3 次。
# 注意 ` ` 和 $( ) 等价。seq 返回长度为 3 的数组。
for VARIABLE in `seq 3`
do
    echo "$VARIABLE"
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
foo "My name is" $NAME

# 有很多有用的指令需要学习:
tail -n 10 file.txt
# 打印 file.txt 的最后 10 行
head -n 10 file.txt
# 打印 file.txt 的前 10 行
sort file.txt
# 将 file.txt 按行排序
uniq -d file.txt
# 报告或忽略重复的行，用选项 -d 打印重复的行
cut -d ',' -f 1 file.txt
# 打印每行中 ',' 之前内容
```
