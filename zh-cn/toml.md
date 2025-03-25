---
name: TOML
filename: learntoml.toml
contributors:
  - ["Alois de Gouvello", "https://github.com/aloisdg"]
translators:
  - ["linglilongyi", "https://github.com/aloisdg/linglilongyi"]
---

TOML 全称为Tom 的（语义）明显、（配置）最小化的语言。（Tom's Obvious, Minimal Language）. 它是一种数据序列化语言，设计为一种最小的配置文件格式，由于明显的语义而易于阅读。

它是 YAML 和 JSON 的替代方案。它的目标是比 JSON 更人性化，并且比 YAML 更简单。TOML 旨在无歧义地映射到哈希表。TOML 应该很容易解析为各种语言的数据结构。

本文档遵循 [TOML v1.0.0](https://toml.io/en/v1.0.0). 未来的[更改](https://github.com/toml-lang/toml/blob/main/CHANGELOG.md)预计会很小且向后兼容。

```toml
# TOML 里的注释长这样。

################
# 标量类型 #
################

# 我们的根对象 (整个文档中持续有效) 是一组映射,
# 等价于其它语言中的一个字典、哈希表或对象.

# 键、等号和值必须在同一行
# (不过有些值可以跨多行).
key = "value"
string = "hello"
number = 42
float = 3.14
boolean = true
dateTime = 1979-05-27T07:32:00-08:00
scientificNotation = 1e+12
"key can be quoted" = true # ' 和 " 都可以
"unquoted key may contain" = "键名允许字母, 数字, 下划线和 连词号"
other_kêys = "规范中允许特殊字符，但大部分实现上并不允许它们"

# 裸键不能为空，但空引号键是允许的。
"" = "blank"     # 允许但不鼓励
'' = 'blank'     # 允许但不鼓励

##########
# 字符串 #
##########

# 所有字符串都只能包含有效的 UTF-8 字符
# 我们可以转义字符，其中一些字符具有简便转义写法。
# 例如, \t 添加制表符. 请参阅规范以获取所有内容.
basicString = "被引号包围. \"我也可以被引\". 名字\tJos"

multiLineString = """
用三连双引号包围
在两侧并允许新行。"""

literalString = '单引号引用的字符串不允许转义字符'

multiLineLiteralString = '''
用三连单引号包围
在两侧并允许新行，同样不允许转义。
紧随开头引号的那个换行会被去除。
   其它空白和换行
   会被原样保留. #! 会被保留？
'''

# 二进制内容建议使用 base64 编码, 其它内容使用 ASCII 或 UTF8
# 编码. 编码的处理方式是各个应用特异的。

###########
# 整数 #
###########

## 整数以 +、- 符号为前缀或无前缀。
## 不允许使用前导零。
## 允许使用十六进制、八进制和二进制形式。
## 如果无法无损表现某个整数，则必须抛出错误。
int1 = +42
int2 = 0
int3 = -21
int4 = 0xdeadbeef
int5 = 0o755
int6 = 0b11011100
integerRange = 64

## 可以用下划线分隔增强可读性。每个
## 下划线两侧必须至少有一个数字.
int4 = 5_349_221
int5 = 1_2_3_4_5     # 允许但不鼓励

#########
# 浮点数 #
#########

# 一个浮点数由一个整数部分（遵从与十进制整数值相同的规则）后跟上一个小数部分和/或一个指数部分组成。
flt1 = 3.1415
flt2 = -5e6
flt3 = 6.626E-34

###########
# 布尔值 #
###########

bool1 = true
bool2 = false
boolMustBeLowercase = true

############
# 时间 #
############

date1 = 1979-05-27T07:32:00Z # UTC time, 遵循 RFC 3339/ISO 8601 规范
date2 = 1979-05-26T15:32:00+08:00 # 带有 RFC 3339 日期时刻中的时区偏移量
date3 = 1979-05-27T07:32:00 # 省略时区偏移量
date4 = 1979-05-27 # 省略时区偏移量或时间

####################
# 集合类型 #
####################

#########
# 数组 #
#########

array1 = [ 1, 2, 3 ]
array2 = [ "逗号", "分隔" ]
array3 = [ "不要混合", "不同", "类型" ]
array4 = [ [ 1.2, 2.4 ], ["所有", '字符串', """都是相同""", '''类型'''] ]
array5 = [
  "空白", "会被", "忽略"
]

#########
# 表 #
#########

# 表（也被称为哈希表或字典）是键值对的集合。
# 它们由表头定义，连同方括号作为单独的行出现。
# 空表是允许的，只要里面没有键值对就行了。 
[table]

# 在它下方，直至下一个表头或文件结束，都是这个表的键值对。
# 表不保证保持键值对的指定顺序。
[table-1]
key1 = "some string"
key2 = 123

[table-2]
key1 = "another string"
key2 = 456

# 裸键中禁止使用点，因为点用于表示嵌套表。
# 表名的规则与键名相同。 
[dog."tater.man"]
type = "pug"

# 等价于 JSON:
# { "dog": { "tater.man": { "type": "pug" } } }

# 键名周围的空格会被忽略。
# 然而，最佳实践还是不要有任何多余的空白。 
[a.b.c]            # this is best practice
[ d.e.f ]          # same as [d.e.f]
[ j . "ʞ" . 'l' ]  # same as [j."ʞ".'l']

# 你不必层层完整地写出你不想写的所有途径的父表。
# TOML 知道该怎么办。 
# [x] you
# [x.y] don't
# [x.y.z] need these
[x.y.z.w] # for this to work

# 只要父表还没有被直接定义，你依然可以
# 定义父表及其未定义的键值对。
[a.b]
c = 1

[a]
d = 2

# 等价于 JSON:
# { "a": {"b": {"c": 1}, "d": 2 } }

# 你不能重复定义一个表。这样做是非法的。 

# 不要这么做
[a]
b = 1

[a]
c = 2

# 也不要这么做
[a]
b = 1

[a.b]
c = 2

# 所有表名都不能为空
[]     # 非法的
[a.]   # 非法的
[a..b] # 非法的
[.b]   # 非法的
[.]    # 非法的

################
# 内联表 #
################

inlineTables = { areEnclosedWith = "{ 和 }", a = { b = { c = { d = 1 } } } }
point = { x = 1, y = 2 }
usingMultiple = {
  lines = "不鼓励换行使用内联表!",
  instead = "用普通的 TOML 表",
}

###################
# 表数组 #
###################

# 表数组通过把表名写在双方括号里的表头来表示。
# 每一个用双方括号括起的表都是表数组中的一个元素
# 这些表按出现顺序插入该数组。 

[[products]]
name = "表数组"
sku = 738594937
emptyTableAreAllowed = true

[[products]]

[[products]]
name = "Nail"
sku = 284758393
color = "gray"
```

等价于 JSON:

```json
{ 
  "products": [
    {
      "name": "array of table",
      "sku": 7385594937,
      "emptyTableAreAllowed": true
    },
    {},
    {
      "name": "Nail",
      "sku": 284758393,
      "color": "gray"
    }
  ]
}
```

```toml
# 你可以创建嵌套的表数组. 每个被双括号括起的
# 子表会从属于在其上最近的表。

[[fruit]]
  name = "apple" # I am a property in fruit table/map

  [fruit.geometry]
    shape = "round"
    note = "I am a property in geometry table/map"

  [[fruit.color]]
    name = "red"
    note = "I am an array item in apple fruit's table/map"

  [[fruit.color]]
    name = "green"
    note = "I am in the same array as red"

[[fruit]]
  name = "banana"

  [[fruit.color]]
    name = "yellow"
    note = "I am an array item in banana fruit's table/map"
```

等价于 JSON:

```
{
  "fruit": [
    {
      "name": "apple",
      "geometry": { "shape": "round", "note": "..."},
      "color": [
        { "name": "red", "note": "..." },
        { "name": "green", "note": "..." }
      ]
    },
    {
      "name": "banana",
      "color": [
        { "name": "yellow", "note": "..." }
      ]
    }
  ]
}
```

### 进一步阅读

+ [TOML 官方仓库](https://github.com/toml-lang/toml)
