---
language: yaml
contributors:
  - ["Adam Brenecki", "https://github.com/adambrenecki"]
translators:
  - ["Zach Zhang", "https://github.com/checkcheckzz"]
  - ["Jiang Haiyun", "https://github.com/haiiiiiyun"]
filename: learnyaml-cn.yaml
lang: zh-cn
---

YAML 是一个数据序列化语言，被设计成人类直接可写可读的。

它是 JSON 的严格超集，增加了语法显著换行符和缩进，就像 Python。但和 Python 不一样，
YAML 根本不容许文字制表符。


```yaml
# YAML 中的注解看起来像这样。

################
# 标量类型     #
################

# 我们的根对象 (它们在整个文件里延续) 将会是一个映射，
# 它等价于在别的语言里的一个字典，哈西表或对象。
key: value
another_key: Another value goes here.
a_number_value: 100
# 如果你想将数字 1 作为值，你必须要将它括在引号中。
# 不然 YAML 解析器会假定它是一个布尔值 true。
scientific_notation: 1e+12
boolean: true
null_value: null
key with spaces: value
# 注意到字符串不需要被括在引号中。但是，它们可以被括起来。
"Keys can be quoted too.": "Useful if you want to put a ':' in your key."

# 多行字符串既可以写成像一个'文字块'(使用 |)，
# 或像一个'折叠块'(使用 '>')。
literal_block: |
    This entire block of text will be the value of the 'literal_block' key,
    with line breaks being preserved.

    The literal continues until de-dented, and the leading indentation is
    stripped.

        Any lines that are 'more-indented' keep the rest of their indentation -
        these lines will be indented by 4 spaces.
folded_style: >
    This entire block of text will be the value of 'folded_style', but this
    time, all newlines will be replaced with a single space.

    Blank lines, like above, are converted to a newline character.

        'More-indented' lines keep their newlines, too -
        this text will appear over two lines.

####################
# 集合类型         #
####################

# 嵌套是通过缩进完成的。
a_nested_map:
    key: value
    another_key: Another Value
    another_nested_map:
        hello: hello

# 映射的键值不必是字符串。
0.25: a float key

# 键值也可以是复合型的，比如多行对象
# 我们用 ? 后跟一个空格来表示一个复合键的开始。
? |
    This is a key
    that has multiple lines
: and this is its value

# YAML 也允许使用复杂键语法表示序列间的映射关系。
# 但有些语言的解析器可能会不支持。
# 一个例子：
? - Manchester United
  - Real Madrid
: [ 2001-01-01, 2002-02-02 ]

# 序列 (等价于列表或数组) 看起来像这样：
a_sequence:
    - Item 1
    - Item 2
    - 0.5 # 序列可以包含不同类型。
    - Item 4
    - key: value
      another_key: another_value
    -
        - This is a sequence
        - inside another sequence

# 因为 YAML 是 JSON 的超集，你也可以写 JSON 风格的映射和序列：
json_map: {"key": "value"}
json_seq: [3, 2, 1, "takeoff"]

#######################
# 其余的 YAML 特性    #
#######################

# YAML 还有一个方便的特性叫 '锚'，它能让你很容易在文档中进行文本复用。
# 如下两个键会有相同的值：
anchored_content: &anchor_name This string will appear as the value of two keys.
other_anchor: *anchor_name

# 锚也可被用来复制/继承属性
base: &base
    name: Everyone has same name

foo: &foo
    <<: *base
    age: 10

bar: &bar
    <<: *base
    age: 20

# foo 和 bar 将都含有 name: Everyone has same name

# YAML 还有标签，你可以用它显示地声明类型。
explicit_string: !!str 0.5
# 一些解析器实现特定语言的标签，就像这个针对 Python 的复数类型。
python_complex_number: !!python/complex 1+2j

# 我们也可以在 YAML 的复合键中使用特定语言的标签
? !!python/tuple [5, 7]
: Fifty Seven
# 将会是 Python 中的  {(5, 7): 'Fifty Seven'}

####################
# 其余的 YAML 类型 #
####################

# 除了字符串和数字，YAML 还能理解其它标量。
# ISO 格式的日期和日期时间文本也可以被解析。
datetime: 2001-12-15T02:59:43.1Z
datetime_with_spaces: 2001-12-14 21:59:43.10 -5
date: 2002-12-14

# 这个 !!binary 标签表明这个字符串实际上
# 是一个用 base64 编码表示的二进制 blob。
gif_file: !!binary |
    R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
    OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
    +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
    AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=

# YAML 还有一个集合类型，它看起来像这样：
set:
    ? item1
    ? item2
    ? item3

# 像 Python 一样，集合仅是值为 null 的映射；上面的集合等价于：
set2:
    item1: null
    item2: null
    item3: null
```

### 更多资源

+ [YAML official website](http://yaml.org/)
+ [Online YAML Validator](http://codebeautify.org/yaml-validator)
