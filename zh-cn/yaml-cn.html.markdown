---
language: yaml
contributors:
  - ["Adam Brenecki", "https://github.com/adambrenecki"]
translators:
  - ["Zach Zhang", "https://github.com/checkcheckzz"]
filename: learnyaml-cn.yaml
lang: zh-cn
---

YAML是一个数据序列化语言，被设计成人类直接可写可读的。

它是JSON的严格超集，增加了语法显著换行符和缩进，就像Python。但和Python不一样，
YAML根本不容许文字制表符。


```yaml
# YAML中的注解看起来像这样。

################
# 标量类型 #
################

# 我们的根对象 (它们在整个文件里延续) 将会是一个地图，
# 它等价于在别的语言里的一个字典，哈西表或对象。
key: value
another_key: Another value goes here.
a_number_value: 100
scientific_notation: 1e+12
boolean: true
null_value: null
key with spaces: value
# 注意到字符串不需要被引用。但是，它们可以被引用。
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
# 集合类型 #
####################

# 嵌套是通过缩进完成的。
a_nested_map:
    key: value
    another_key: Another Value
    another_nested_map:
        hello: hello

# 地图不用有字符串键值。
0.25: a float key

# 键值也可以是多行对象，用?表明键值的开始。
? |
    This is a key
    that has multiple lines
: and this is its value

# YAML也容许键值是集合类型，但是很多语言将会抱怨。

# 序列 (等价于表或数组) 看起来像这样：
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

# 因为YAML是JSON的超集，你也可以写JSON风格的地图和序列：
json_map: {"key": "value"}
json_seq: [3, 2, 1, "takeoff"]

#######################
# 其余的YAML特点 #
#######################

# YAML还有一个方便的特点叫'锚'，它让你简单地在整个文件里重复内容。
# 两个键值将会有相同的值：
anchored_content: &anchor_name This string will appear as the value of two keys.
other_anchor: *anchor_name

# YAML还有标签，你可以用它显示地声明类型。
explicit_string: !!str 0.5
# 一些解析器实现特定语言的标签，就像这个为了Python的复数类型。
python_complex_number: !!python/complex 1+2j

####################
# 其余的YAML类型 #
####################

# 字符串和数字不是仅有的YAML可以理解的标量。
# ISO 格式的日期和日期时间文字也是可以被解析的。
datetime: 2001-12-15T02:59:43.1Z
datetime_with_spaces: 2001-12-14 21:59:43.10 -5
date: 2002-12-14

# 这个!!binary标签表明一个字符串实际上是一个二进制blob的base64编码表示。
gif_file: !!binary |
    R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
    OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
    +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
    AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=

# YAML还有一个集合类型，它看起来像这样：
set:
    ? item1
    ? item2
    ? item3

# 像Python一样，集合仅是有null数值的地图；上面的集合等价于：
set2:
    item1: null
    item2: null
    item3: null
```
