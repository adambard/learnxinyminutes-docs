---
contributors:
    - ["Jack Kuan", "https://github.com/kjkuan"]
    - ["Azeem Sajid", "https://github.com/iamazeem"]
translators:
    - ["muzimu", "https://github.com/muzimu"]
---

`jq` 是一个用于转换 JSON 输入并生成 JSON 输出的工具。作为一种
编程语言，jq 支持布尔表达式和算术表达式、对象与数组索引；
它还提供条件语句、函数，甚至异常处理……等等。掌握 jq 后，你可以轻松编写
能够对 JSON 文档执行复杂查询、查找答案、生成报告，或生成供其他程序继续
处理的另一个 JSON 文档的小程序。

> **注意**：本指南演示如何从命令行使用 jq，
> 具体来说，运行环境是 Bash shell。

```bash
# 从命令行运行 jq 时，可以将 jq 程序代码作为
# `jq` 所有选项之后的第一个参数。我们通常用
# 单引号（`'`）包裹 jq 程序，防止命令行
# shell 对其进行特殊解释。
#
jq -n '# 注释以 # 开头，持续到行尾。
       # -n 选项将输入设为 `null`，并阻止 `jq`
       # 从外部来源读取输入。
'

# 输出：
# null


# 默认情况下，jq 从 *STDIN* 读取一串 JSON 输入（值）。它
# 使用命令行指定的 jq 程序（过滤器）处理每个输入，
# 然后将程序处理每个输入得到的输出打印到
# *STDOUT*。
#
echo '
  "hello" 123 [
    "one",
    "two",
    "three"
  ]
  { "name": "jq" }
' |
 jq '.  # <-- 这里的 jq 程序是单个点（.），称为 identity
        # 运算符，表示当前输入。
'

# 输出：
# "hello"
# 123
# [
#   "one",
#   "two",
#   "three"
# ]
# {
#   "name": "jq"
# }


# 注意，jq 默认会美化打印输出，因此，将数据通过管道传给
# `jq` 是格式化某个返回 JSON 的 REST API 端点响应的简单方法。比如：
# `curl -s https://freegeoip.app/json/ | jq`


# 除了用 jq 程序处理每个 JSON 输入外，也可以
# 让 jq 将它们汇集成一个数组。
#
echo '1 "two" 3' | jq -s .

# 输出：
# [
#   1,
#   "two",
#   3
# ]


# 或者，将每一行都当作字符串处理。
#
(echo line 1; echo line 2) | jq -R .

# 输出：
# "line 1"
# "line 2"


# 也可以组合 -s 和 -R，将输入行汇集为一个字符串。
#
(echo line 1; echo line 2) | jq -sR .

# 输出：
# "line 1\nline2\n"


# 输入也可以来自命令行指定的 JSON 文件：
#
echo '"hello"' > hello.json
jq . hello.json

# 输出：
# "hello"


# 可以使用 `--arg` 选项向 jq 程序传入一个值。
# 下面的例子将值 `123` 绑定到变量名 `val`。
# 之后可以用 `$val` 引用该变量。
#
jq -n --arg val 123 '$val'  # $val 是字符串 "123"

# 输出：
# "123"


# 如果需要传入 JSON 值，请使用 `--argjson`
#
jq -n --argjson val 123 '$val'  # $val 是数字

# 输出：
# 123


# 使用 `--arg` 或 `--argjson` 可以方便地根据
# 现有输入构建 JSON 输出。
#
jq --arg text "$(date; echo "Have a nice day!")" -n '{ "today": $text }'

# 输出：
# {
#   "today": "Sun Apr 10 09:53:07 PM EDT 2022\nHave a nice day!"
# }


# 除了将值作为 JSON 输出，还可以使用 `-r` 选项打印
# 不加引号/不转义的字符串值。非字符串值仍会按
# JSON 打印。
#
echo '"hello" 2 [1, "two", null] {}' | jq -r .

# 输出：
# hello
# 2
# [
#   1,
#   "two",
#   null
# ]
# {}


# 在 jq 字符串中，可以使用 `\(expr)` 将 `expr` 的输出替换到
# 外层字符串中。
#
jq -rn '"1 + 2 = \(1+2)"'

# 输出：
# 1 + 2 = 3


# `-r` 选项最适合生成要继续在 shell 管道中处理的文本输出，
# 尤其适合与带有插值字符串且以 `@sh` 前缀
# 运算符开头的字符串结合使用。
#
# `@sh` 运算符会用单引号转义字符串中 `\(...)` 的输出，
# 这样每个生成的 `\(...)` 字符串都能被 shell 当作一个单独的
# 单词/标记/参数求值，而不会受到特殊
# 解释。
#
env_vars=$(
    echo '{"var1": "value one", "var2": "value\ntwo"}' \
     |
    jq -r '
      "export " + @sh "var1=\(.var1) var2=\(.var2)"
      #                     ^^^^^^^^      ^^^^^^^^
      #                  "'value one'"  "'value\ntwo'"
      #
      # 注意：这里的 +（加号）运算符用于拼接字符串。
    '
)
echo "$env_vars"
eval "$env_vars"
declare -p var1 var2

# 输出：
# export var1='value one' var2='value
# two'
# declare -- var1="value one"
# declare -- var2="value
# two"

# 还有其他字符串 `@prefix` 运算符（例如 @base64、@uri、@csv……），
# 可能也很有用。详情请参阅 `man jq`。


# jq 中的逗号（`,`）运算符会计算每个操作数并生成多个
# 输出：
#
jq -n '"one", 2, ["three"], {"four": 4}'

# 输出：
# "one"
# 2
# [
#   "three"
# ]
# {
#   "four": 4
# }


# 任意 JSON 值都是有效的 jq 表达式，其求值结果就是该 JSON 值
# 本身。
#
jq -n '1, "one", [1, 2], {"one": 1}, null, true, false'

# 输出：
# 1
# "one"
# [
#   1,
#   2
# ]
# {
#   "one": 1
# }
# null
# true
# false


# 在需要 JSON 值的地方都可以使用 jq 表达式，甚至可以用作对象
# 的键。（对象键或值可能需要加括号）
#
jq -n '[2*3, 8-1, 16/2], {("tw" + "o"): (1 + 1)}'

# 输出：
# [
#   6,
#   7,
#   8
# ]
# {
#   "two": 2
# }


# 作为简写，如果 JSON 对象的键看起来像有效标识符（匹配
# 正则表达式 `^[a-zA-Z_][a-zA-Z_0-9]*$`），可以省略引号。
#
jq -n '{ key_1: "value1" }'

# 如果省略 JSON 对象键的值，jq 会使用该键在当前
# 输入中查找对应值：（`... | ...` 的含义见下一个例子）
#
jq -n '{c: 3} | {a: 1, "b", c}'

# 输出：
# {
#   "a": 1,
#   "b": null,
#   "c": 3
# }


# jq 程序通常写成一系列由管道（`|`）运算符
# 连接的表达式（过滤器）。该运算符会把左侧过滤器的输出
# 作为右侧过滤器的输入。
#
jq -n '1 | . + 2 | . + 3'  # 第一个点是 1；第二个点是 3

# 输出：
# 6

# 如果表达式求值产生多个输出，jq 会遍历这些输出，
# 将每个输出沿管道传递下去，并最终生成多个
# 输出。
#
jq -n '1, 2, 3 | ., 4 | .'

# 输出：
# 1
# 4
# 2
# 4
# 3
# 4

# 上一个例子中的数据流可以这样表示：
# （数字前的 `*` 表示当前输出）
#
# *1,  2,  3 | *1,  4 | *1
#  1,  2,  3 |  1, *4 | *4
#  1, *2,  3 | *2,  4 | *2
#  1,  2,  3 |  2, *4 | *4
#  1,  2, *3 | *3,  4 | *3
#  1,  2,  3 |  3, *4 | *4
#
# 换句话说，上面例子的求值过程与以下其他编程语言中的代码
# 非常相似：
#
# Python：
#
#   for first_dot in 1, 2, 3:
#       for second_dot in first_dot, 4:
#           print(second_dot)
#
# Ruby：
#
#   [1, 2, 3].each do |dot|
#     [dot, 4].each { |dot| puts dot }
#   end
#
# JavaScript：
#
#   [1, 2, 3].forEach(dot => {
#       [dot, 4].forEach(dot => console.log(dot))
#   })
#


# 下面是一些使用表达式后的 `[expr]` 运算符进行数组索引和
# 对象属性查找的例子。如果 `expr` 是数字，则进行数组索引查找；
# 否则它应当是字符串，此时进行对象属性查找：
# 数组索引查找
#
jq -n '[2, {"four": 4}, 6][1 - 1]' # => 2
jq -n '[2, {"four": 4}, 6][0]'     # => 2
jq -n '[2, {"four": 4}, 6] | .[0]' # => 2

# 由于查找本身就是表达式，因此可以将多个查找串联起来。
#
jq -n '[2, {"four": 4}, 6][1]["fo" + "ur"]' # => 4

# 对象属性还可以使用 `.key` 简写。
#
jq -n '[2, {"four": 4}, 6][1].four'  # => 4

# 如果键不是有效标识符，请使用 `."key"`。
#
jq -n '[2, {"f o u r": 4}, 6][1]."f o u r"' # => 4

# 找不到索引时，数组索引查找返回 null。
#
jq -n '[2, {"four": 4}, 6][99]' # => null

# 找不到键时，对象属性查找返回 null。
#
jq -n '[2, {"four": 4}, 6][1].whatever' # => null

# 可以使用替代运算符 `//` 提供默认值，
# 当左操作数的结果为 `null` 或 `false` 时生效。
#
jq -n '.unknown_key // 7' # => 7

# 如果查找运算符（`[expr]`）之前的值既不是数组
# 也不是对象，就会收到错误：
#
jq -n '123 | .[0]'     # => jq: error (at <unknown>): Cannot index number with number
jq -n '"abc" | .name'  # => jq: error (at <unknown>): Cannot index string with string "name"
jq -n '{"a": 97} | .[0]'    # => jq: error (at <unknown>): Cannot index object with number
jq -n '[89, 64] | .["key"]' # => jq: error (at <unknown>): Cannot index array with string "key"

# 不过，可以在查找后追加 `?`，让 jq 在发生此类错误时
# 返回 `empty`。
#
jq -n '123 | .[0]?'    # 没有输出，因为结果是 empty。
jq -n '"abc" | .name?' # 没有输出，因为结果是 empty。

# 替代运算符（`//`）对 `empty` 也有效：
#
jq -n '123 | .[0]? // 99'           # => 99
jq -n '"abc" | .name? // "unknown"' # => "unknown"

# 注意：`empty` 实际上是 jq 的一个内置函数。
# 结合前面嵌套循环的说明，
# `empty` 类似于某些编程语言中跳过当前循环迭代的
# `continue` 或 `next` 关键字。


# 字符串和数组可以使用与 Python 相同的切片语法（`[i:j]`，但不支持
# 步长）和语义：
#
#                0   1    2    3    4   5 …… 无限
#        数组 = ["a", "b", "c", "d"]
# -无限 …… -4  -3   -2   -1
#
jq -n '["Peter", "Jerry"][1]'            # => "Jerry"
jq -n '["Peter", "Jerry"][-1]'           # => "Jerry"
jq -n '["Peter", "Jerry", "Tom"][1:]'    # => ["Jerry", "Tom"]
jq -n '["Peter", "Jerry", "Tom"][:1+1]'  # => ["Peter", "Jerry"]
jq -n '["Peter", "Jerry", "Tom"][1:99]'  # => ["Jerry", "Tom"]


# 如果省略查找索引或键，jq 会遍历
# 集合，并在每次迭代中生成一个输出值。
#
# 以下例子会产生相同的输出。
#
echo 1 2 3 | jq .
jq -n '1, 2, 3'
jq -n '[1, 2, 3][]'
jq -n '{a: 1, b: 2, c: 3}[]'

# 输出：
# 1
# 2
# 3


# 可以根据多个输出构建数组。
#
jq -n '{values: [{a: 1, b: 2, c: 3}[] | . * 2]}'

# 输出：
# {
#   "values": [
#     2,
#     4,
#     6
#   ]
# }


# 如果不将多个输出包含起来，最终就会得到多个
# 输出。
#
jq -n '{values: ({a: 1, b: 2, c: 3}[] | . * 2)}'

# 输出：
# {
#   "values": 2
# }
# {
#   "values": 4
# }
# {
#   "values": 6
# }


# jq 中的条件语句 `if ... then ... else ... end` 是一个表达式，因此
# `then` 部分和 `else` 部分都必须存在。在 jq 中，只有
# `null` 和 `false` 两个值为假，其他所有值都为真。
#
jq -n 'if 1 > 2 | not and 1 <= 2 then "Makes sense" else "WAT?!" end'

# 输出
# "Makes sense"

# 注意，`not` 是一个不接受参数的内置函数，
# 所以这里把它作为过滤器来否定输入值。
# 稍后会介绍函数。

# 另一个使用条件语句的例子：
#
jq -n '1, 2, 3, 4, 5 | if . % 2 != 0 then . else empty end'

# 输出
# 1
# 3
# 5

# 上面的 `empty` 是一个不接受参数且
# 不生成输出的内置函数。下面再看看其他内置函数。

# 上面的条件语句例子也可以使用内置函数 `select/1` 编写，
# （`/1` 表示该函数需要的参数数量）。
#
jq -n '1, 2, 3, 4, 5 | select(. % 2 != 0)'  # 注意：% 表示余数。

# 输出
# 1
# 3
# 5


# jq 中的函数参数按名称传递，也就是说，参数不会在调用点求值，
# 而是被视为一个 lambda 表达式，调用点的上下文会作为
# 其中用于解析表达式内变量和函数引用的作用域。
#
# 在上面的例子中，传给 `select/1` 的参数是表达式 `. % 2 != 0`，
# 而不是 `true` 或 `false`；后者只有在布尔表达式先求值再传入
# 函数时才会出现。
#


# 内置函数 `range/1`、`range/2` 和 `range/3` 会生成
# 指定范围内的整数。
#
jq -n '[range(3)]'         # => [0, 1, 2]
jq -n '[range(0; 4)]'      # => [0, 1, 2, 3]
jq -n '[range(2; 10; 2)]'  # => [2, 4, 6, 8]

# 注意：使用 `;`（分号）分隔函数参数。


# `map/1` 函数会将给定表达式应用于
# 当前输入（数组）的每个元素，并输出一个新数组。
#
jq -n '[range(1; 6) | select(. % 2 != 0)] | map(. * 2)'

# 输出：
# [
#   2,
#   6,
#   10
# ]

# 不使用 `select/1` 和 `map/1`，上面的例子也可以写成：
#
jq -n '[range(1; 6) | if . % 2 != 0 then . else empty end | . * 2]'


# `keys/0` 返回当前输入的键数组。对于对象，
# 对象的属性名；对于数组，它们是
# 数组索引。
#
jq -n '[range(2; 10; 2)] | keys'   # => [0, 1, 2, 3]
jq -n '{a: 1, b: 2, c: 3} | keys'  # => ["a", "b", "c"]

# `values/0` 返回当前输入的值数组。对于对象，
# 对象的属性值；对于数组，它们是
# 数组元素。
#
jq -n '[range(2; 10; 2)] | values'   # => [2, 4, 6, 8]
jq -n '{a: 1, b: 2, c: 3} | values'  # => [1, 2, 3]


# `to_entries/0` 返回当前输入对象的键值对象数组。
#
jq -n '{a: 1, b: 2, c: 3} | to_entries'

# 输出：
# [
#   {
#     "key": "a",
#     "value": 1
#   },
#   {
#     "key": "b",
#     "value": 2
#   },
#   {
#     "key": "c",
#     "value": 3
#   }
# ]


# 下面演示如何利用目前学到的内容，将对象属性转换为环境变量。
#
env_vars=$(
    jq -rn '{var1: "1 2  3   4", var2: "line1\nline2\n"}
            | to_entries[]
            | "export " + @sh "\(.key)=\(.value)"
           '
)
eval "$env_vars"
declare -p var1 var2

# 输出：
# declare -x var1="1 2  3   4"
# declare -x var2="line1
# line2
# "


# `from_entries/0` 与 `to_entries/0` 相反：它接收一个
# 键值对象数组，并根据这些对象的 `key` 和 `value` 属性
# 生成包含键和值的对象。
#
# 当需要遍历对象的每个属性并对其进行处理时，
# 它与 `to_entries/0` 搭配使用非常方便。
#
jq -n '{a: 1, b: 2, c: 3} | to_entries | map(.value *= 2) | from_entries'

# 输出：
# {
#   "a": 2,
#   "b": 4,
#   "c": 6
# }


# 上面的例子还可以使用内置函数 `with_entries/1` 进一步简写：
#
jq -n '{a: 1, b: 2, c: 3} | with_entries(.value *= 2)'


# `group_by/1` 根据当前输入（数组）生成一个由分组（数组）组成的数组。
# 分类方式是将表达式参数应用于输入数组的每个成员。
#
# 看一个人为构造的例子（注意，`tostring`、`tonumber`、
# `length` 和 `max` 都是 jq 内置函数。可以在 jq 手册中
# 查阅它们的说明）：
#
# 生成一些随机数。
numbers=$(echo $RANDOM{,,,,,,,,,,,,,,,,,,,,})
#
# 将这些数字传给 jq，进行分组、计算平均值，最后生成报告。
#
echo $numbers | jq -rs '  # 将数字汇集到一个数组中。
[
  [ map(tostring)          # 将其转换为字符串数组。
    | group_by(.[0:1])     # 按数字的第一位分组。
    | .[]                  # 遍历数组的数组（各分组）。
    | map(tonumber)        # 将每个分组转换回数字数组。
  ] # 最后，将所有分组放入一个数组。

  | sort_by([length, max]) # 按分组大小排序。
    # 如果两个分组大小相同，则数字最大
    # 的分组排在后面。

  | to_entries[]           # 枚举数组，生成键值对象。
  |                        # 对每个对象生成两行：
  "Group \(.key): \(.value | sort | join(" "))"   + "\n" +
  "Average: \(      .value | (add / length)  )"

] # 将分组和平均值行放入数组。
  # 用分隔线（短横线）连接数组元素，生成报告。
| join("\n" + "-"*78 + "\n")
'

# 输出：
#
# Group 0: 3267
# Average: 3267
# ------------------------------------------------------------------------------
# Group 1: 7854
# Average: 7854
# ------------------------------------------------------------------------------
# Group 2: 4415 4447
# Average: 4431
# ------------------------------------------------------------------------------
# Group 3: 681 6426
# Average: 3553.5
# ------------------------------------------------------------------------------
# Group 4: 21263 21361 21801 21832 22947 23523 29174
# Average: 23128.714285714286
# ------------------------------------------------------------------------------
# Group 5: 10373 12698 13132 13924 17444 17963 18934 18979
# Average: 15430.875


# 内置函数 `add/1` 会将值数组“归约”为单个值。
# 可以把它理解为在集合的每个值之间插入 `+` 运算符。
# 下面是一些例子：
#
jq -n '[1, 2, 3, 4, 5] | add'  # => 15
jq -n '["a", "b", "c"] | add'  # => "abc"

# `+` 会拼接数组
jq -n '[["a"], ["b"], ["c"]] | add'

# 输出：
# [
#   "a",
#   "b",
#   "c"
# ]

# `+` 会以非递归方式合并对象。
jq -n '[{a: 1, b: {c: 3}}, {b: 2, c: 4}] | add'

# 输出：
# {
#   "a": 1,
#   "b": 2,
#   "c": 4
# }


# jq 提供了一种特殊语法，可以将某个表达式产生的
# 多个输出归约为单个值。
# 形式如下：
#
#   reduce outputs_expr as $var (initial_value; reduction_expr)
#
# 示例：
#
jq -n 'reduce range(1; 6) as $i (0; . + $i)'             # => 15
jq -n 'reduce (1, 2, 3, 4, 5) as $i (0; . + $i)'         # => 15
jq -n '[1, 2, 3, 4, 5] | reduce .[] as $i (0; . + $i)'   # => 15
jq -n '["a", "b", "c"] | reduce .[] as $i (""; . + $i)'  # => "abc"

# 注意，`reduction_expr` 中的 `.` 起初是 `initial_value`，
# 随后会随着遍历 `outputs_expr` 的值，变成应用 `reduction_expr`
# 后得到的结果。表达式：
#
#    reduce (1, 2, 3, 4, 5) as $i (0; . + $i)
#
# 可以看作执行：
#
#    0 + 1 | . + 2 | . + 3 | . + 4 | . + 5
#


# 对两个对象使用 `*` 运算符时，会递归合并二者。
# 因此，要递归合并 JSON 对象，可以将 `reduce`
# 与 `*` 运算符结合使用。例如：
#
echo '
  {"a": 1,  "b": {"c": 3}}
  {         "b": {"d": 4}}
  {"a": 99, "e": 5       }
' | jq -s 'reduce .[] as $m ({}; . * $m)'

# 输出：
# {
#   "a": 99,
#   "b": {
#     "c": 3,
#     "d": 4
#   },
#   "e": 5
# }


# jq 使用 `expr as $var` 的形式进行变量绑定，将 `expr` 的值
# 绑定到 `$var`，并且 `$var` 不可变。此外，
# `... as ...` 不会改变下一个过滤器的输入；它在过滤器管道中的引入
# 只是为了建立值到变量的绑定，
# 其作用域会延伸到定义之后的过滤器。
# （也就是说，要查找变量的定义，只需从使用它的表达式开始，沿过滤器链向左
# 扫描，直到找到定义。）
#
jq -rn '[1, 2, 3, 4, 5]
        | (.[0] + .[-1])      as $sum     # 始终给绑定表达式 `expr` 加上 ( )，避免意外结果。
        | ($sum * length / 2) as $result  # 此时的当前输入仍然是初始数组。
        | "The result is: \($result)"     # 同样如此。
'

# 输出：
# The result is: 15


# 使用 `expr as $var` 形式时，如果 `expr` 生成多个值，
# jq 会依次遍历这些值，并将每个值绑定到 `$var`，
# 供管道的其余部分使用。
#
jq -rn 'range(2; 4) as $i
        | range(1; 6) as $j
          | "\($i) * \($j) = \($i * $j)"
'

# 输出：
# 2 * 1 = 2
# 2 * 2 = 4
# 2 * 3 = 6
# 2 * 4 = 8
# 2 * 5 = 10
# 3 * 1 = 3
# 3 * 2 = 6
# 3 * 3 = 9
# 3 * 4 = 12
# 3 * 5 = 15


# 有时，将初始输入绑定到程序开头的一个变量很有用，
# 这样就能在管道后续部分引用它。
#
jq -rn "$(cat <<'EOF'
    {lookup:  {a: 1, b: 2, c: 3},
     bonuses: {a: 5, b: 2, c: 9}
    }
    | . as $doc
    | .bonuses
    | to_entries[]
    | "\(.key)'s total is \($doc.lookup[.key] + .value)"
EOF
)"

# 输出：
# a's total is 6
# b's total is 4
# c's total is 12


# jq 支持在变量绑定时进行解构。这样可以从数组或对象中提取值，
# 并将它们绑定到变量。
#
jq -n '[range(5)] | . as [$first, $second] | $second'

# 输出：
# 1

jq -n '{ name: "Tom", numbers: [1, 2, 3], age: 32}
       | . as {
            name: $who,                  # 将 .name 绑定到 $who
            $name,                       # `name: $name` 的简写
            numbers: [$first, $second],
         }
       | $name, $second, $first, $who
'

# 输出：
# "Tom"
# 2
# 1
# "Tom"


# 在 jq 中，可以通过赋值运算符 `=` 将值赋给数组索引或对象键。
# 赋值运算符两侧接收相同的当前输入，
# 赋值表达式本身的求值结果也是当前输入。换句话说，赋值表达式的求值
# 只产生副作用，
# 不会生成新的输出。
#
jq -n '.a = 1 | .b = .a + 1'  # => {"a": 1, "b": 2}

# 注意，由于使用了 `jq -n`，输入是 `null`，因此第一个过滤器中的 `.` 是 `null`；
# 在 `null` 下给键赋值会将其变成包含该键的对象。这个相同的输入（现在是对象）
# 随后通过管道传给下一个过滤器，
# 后者把 `b` 键设为 `a` 键的值加 `1`，结果为 `2`。
#

# 另一个例子：
#
jq -n '.a=1, .a.b=2'   # => {"a": 1} {"a": {"b": 2}}

# 上面的例子会生成两个对象，因为两个赋值都
# 以 `null` 作为输入，且逗号运算符的每个操作数
# 都会独立求值。还可以看到，生成嵌套对象非常简单。
#


# 除了赋值运算符，jq 还提供以下运算符：
# `+=`、`-=`、`*=` 和 `/=` 等。基本上，`a op= b` 是
# `a = a op b` 的简写，适合根据对象属性或数组元素的当前值
# 进行更新。示例：
#
jq -n '.a.b.c = 3 | .a.b.c = .a.b.c + 1' # => {"a": {"b": {"c": 4}}}
jq -n '.a.b.c = 3 | .a.b.c += 1'         # => {"a": {"b": {"c": 4}}}


# 要删除值，请使用 `del/1`，它接收一个路径表达式，用于指定
# 要删除内容的位置。示例：
#
jq -n '{a: 1, b: {c: 2}, d: [3, 4, 5]} | del(.b.c, .d[1]) | .b.x = 6'

# 输出：
# {
#   "a": 1,
#   "b": {
#     "x": 6
#   },
#   "d": [
#     3,
#     5
#   ]
# }


# 除了使用 jq 的内置函数外，还可以定义自己的函数。
# 实际上，许多内置函数本身就是用 jq 定义的（参见文档末尾
# 指向 jq 内置函数的链接）。
#
jq -n '
    def my_select(expr): if expr then . else empty end;
    def my_map(expr): [.[] | expr];
    def sum: reduce .[] as $x (0; . + $x);
    def my_range($from; $to):
        if $from >= $to then
            empty
        else
            $from, my_range($from + 1; $to)
        end
    ;
    [my_range(1; 6)] | my_map(my_select(. % 2 != 0)) | sum
'

# 输出：
# 9

# 关于函数定义的一些说明：
#
# - 函数通常在开头定义，以便整个 jq 程序的其余部分都能使用它们。
#
# - 每个函数定义都应以 `;`（分号）结尾。
#
# - 也可以在函数内部定义函数，不过这里没有展示。
#
# - 函数参数用 `;`（分号）分隔。这与调用函数时传递多个参数的方式一致。
#
# - 函数可以调用自身；实际上，jq 支持 TCO（尾调用优化）。
#
# - `def f($a; $b): ...;` 是以下形式的简写：`def f(a; b): a as $a | b as $b | ...`
```

## 延伸阅读

- [jq 手册](https://jqlang.github.io/jq/manual/)
- [语言描述](https://github.com/jqlang/jq/wiki/jq-Language-Description)
- [示例手册](https://github.com/jqlang/jq/wiki/Cookbook)
- [builtin.jq](https://github.com/jqlang/jq/blob/master/src/builtin.jq)
