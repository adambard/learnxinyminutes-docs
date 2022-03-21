---
language: Python
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
translators:
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Maple", "https://github.com/mapleincode"]
filename: learnpython-cn.py
lang: zh-cn

---

Python 是由吉多·范罗苏姆(Guido Van Rossum)在 90 年代早期设计。
它是如今最常用的编程语言之一。它的语法简洁且优美，几乎就是可执行的伪代码。

欢迎大家斧正。英文版原作 Louie Dinh [@louiedinh](http://twitter.com/louiedinh)
邮箱 louiedinh [at] [谷歌的信箱服务]。中文翻译 Geoff Liu。

注意：这篇教程是基于 Python 3 写的。如果你想学旧版 Python 2，我们特别有[另一篇教程](http://learnxinyminutes.com/docs/pythonlegacy/)。

```python
# 用井字符开头的是单行注释

""" 多行字符串用三个引号
    包裹，也常被用来做多
    行注释
"""

####################################################
## 1. 原始数据类型和运算符
####################################################

# 整数
3  # => 3

# 算术没有什么出乎意料的
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20

# 但是除法例外，会自动转换成浮点数
35 / 5  # => 7.0
5 / 3   # => 1.6666666666666667

# 整数除法的结果都是向下取整
5 // 3       # => 1
5.0 // 3.0   # => 1.0 # 浮点数也可以
-5 // 3      # => -2
-5.0 // 3.0  # => -2.0

# 浮点数的运算结果也是浮点数
3 * 2.0 # => 6.0

# 模除
7 % 3 # => 1
# i % j 结果的正负符号会和 j 相同，而不是和 i 相同
-7 % 3 # => 2

# x 的 y 次方
2**4 # => 16

# 用括号决定优先级
1 + 3 * 2    # => 7
(1 + 3) * 2  # => 8

# 布尔值 (注意: 首字母大写)
True   # => True
False  # => False

# 用 not 取非
not True   # => False
not False  # => True

# 逻辑运算符，注意 and 和 or 都是小写
True and False # => False
False or True # => True

# True 和 False 实质上就是数字 1 和0
True + True # => 2
True * 8    # => 8
False - 5   # => -5

# 数值与 True 和 False 之间的运算
0 == False # => True
2 == True # => False
1 == True # => True
-5 != False # => True

# 使用布尔逻辑运算符对数字类型的值进行运算时，会把数值强制转换为布尔值进行运算
# 但计算结果会返回它们的强制转换前的值
# 注意不要把 bool(ints) 与位运算、and/or (&, |) 混淆
bool(0)     # => False
bool(4)     # => True
bool(-6)    # => True
0 and 2     # => 0
-5 or 0     # => -5

# 用==判断相等
1 == 1  # => True
2 == 1  # => False

# 用!=判断不等
1 != 1  # => False
2 != 1  # => True

# 比较大小
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# 判断一个值是否在范围里
1 < 2 and 2 < 3  # => True
2 < 3 and 3 < 2  # => False
# 大小比较可以连起来！
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# (is 对比 ==) is 判断两个变量是否引用同一个对象,
# 而 == 判断两个对象是否含有相同的值
a = [1, 2, 3, 4]  # 变量 a 是一个新的列表, [1, 2, 3, 4]
b = a             # 变量 b 赋值了变量 a 的值
b is a            # => True, a 和 b 引用的是同一个对象
b == a            # => True, a 和 b 的对象的值相同
b = [1, 2, 3, 4]  # 变量 b 赋值了一个新的列表, [1, 2, 3, 4]
b is a            # => False, a 和 b 引用的不是同一个对象
b == a            # => True, a 和 b 的对象的值相同


# 创建字符串可以使用单引号（'）或者双引号（"）
"这是个字符串"
'这也是个字符串'

# 字符串可以使用加号连接成新的字符串
"Hello " + "world!"  # => "Hello world!"
# 非变量形式的字符串甚至可以在没有加号的情况下连接
"Hello " "world!"    # => "Hello world!"

# 字符串可以被当作字符列表
"Hello world!"[0]  # => 'H'

# 你可以获得字符串的长度
len("This is a string")  # => 16

# 你可以使用 f-strings 格式化字符串（python3.6+）
name = "Reiko"
f"She said her name is {name}." # => "She said her name is Reiko"
# 你可以在大括号内几乎加入任何 python 表达式，表达式的结果会以字符串的形式返回
f"{name} is {len(name)} characters long." # => "Reiko is 5 characters long."

# 用 .format 来格式化字符串
"{} can be {}".format("strings", "interpolated")
# 可以重复参数以节省时间
"{0} be nimble, {0} be quick, {0} jump over the {1}".format("Jack", "candle stick")
# => "Jack be nimble, Jack be quick, Jack jump over the candle stick"
# 如果不想数参数，可以用关键字
"{name} wants to eat {food}".format(name="Bob", food="lasagna") 
# => "Bob wants to eat lasagna"

# 如果你的 Python3 程序也要在 Python2.5 以下环境运行，也可以用老式的格式化语法
"%s can be %s the %s way" % ("strings", "interpolated", "old")

# None是一个对象
None  # => None

# 当与 None 进行比较时不要用 ==，要用 is。is 是用来比较两个变量是否指向同一个对象。
"etc" is None  # => False
None is None  # => True

# None，0，空字符串，空列表，空字典，空元组都算是 False
# 所有其他值都是 True
bool(0)  # => False
bool("")  # => False
bool([]) # => False
bool({}) # => False
bool(()) # => False


####################################################
## 2. 变量和集合
####################################################

# print是内置的打印函数
print("I'm Python. Nice to meet you!")

# 默认情况下，print 函数会在输出结果后加入一个空行作为结尾
# 可以使用附加参数改变输出结尾
print("Hello, World", end="!")  # => Hello, World!

# 可以很简单的从终端获得输入数据
input_string_var = input("Enter some data: ") # 返回字符串数值

# 在给变量赋值前不用提前声明
# 传统的变量命名是小写，用下划线分隔单词
some_var = 5
some_var  # => 5

# 访问未赋值的变量会抛出异常
# 参考流程控制一段来学习异常处理
some_unknown_var  # 抛出NameError

# 用列表 (list) 储存序列
li = []
# 创建列表时也可以同时赋给元素
other_li = [4, 5, 6]

# 用append在列表最后追加元素
li.append(1)    # li现在是[1]
li.append(2)    # li现在是[1, 2]
li.append(4)    # li现在是[1, 2, 4]
li.append(3)    # li现在是[1, 2, 4, 3]
# 用pop从列表尾部删除
li.pop()        # => 3 且li现在是[1, 2, 4]
# 把3再放回去
li.append(3)    # li变回[1, 2, 4, 3]

# 列表存取跟数组一样
li[0]  # => 1
# 取出最后一个元素
li[-1]  # => 3

# 越界存取会造成 IndexError
li[4]  # 抛出 IndexError

# 列表有切割语法
li[1:3]    # => [2, 4]
# 取尾
li[2:]     # => [4, 3]
# 取头
li[:3]     # => [1, 2, 4]
# 隔一个取一个
li[::2]    # =>[1, 4]
# 倒排列表
li[::-1]   # => [3, 4, 2, 1]
# 可以用三个参数的任何组合来构建切割
# li[始:终:步伐]

# 简单的实现了单层数组的深度复制
li2 = li[:]  # => li2 = [1, 2, 4, 3] ，但 (li2 is li) 会返回 False

# 用 del 删除任何一个元素
del li[2]   # li 现在为 [1, 2, 3]

# 删除第一个匹配的元素
li.remove(2)  # li 现在为 [1, 3]
li.remove(2)  # 抛出错误 ValueError: 2 is not in the list

# 在指定索引处插入一个新的元素
li.insert(1, 2)  # li is now [1, 2, 3] again

# 获得列表第一个匹配的值的索引
li.index(2)  # => 1
li.index(4)  # 抛出一个 ValueError: 4 is not in the list

# 列表可以相加
# 注意：li 和 other_li 的值都不变
li + other_li   # => [1, 2, 3, 4, 5, 6]

# 用 "extend()" 拼接列表
li.extend(other_li)   # li 现在是[1, 2, 3, 4, 5, 6]

# 用 "in" 测试列表是否包含值
1 in li   # => True

# 用 "len()" 取列表长度
len(li)   # => 6


# 元组类似列表，但是不允许修改
tup = (1, 2, 3)
tup[0]   # => 1
tup[0] = 3  # 抛出 TypeError

# 如果元素数量为 1 的元组必须在元素之后加一个逗号
# 其他元素数量的元组，包括空元组，都不需要
type((1))   # => <class 'int'>
type((1,))  # => <class 'tuple'>
type(())    # => <class 'tuple'>

# 列表允许的操作元组大多都可以
len(tup)   # => 3
tup + (4, 5, 6)   # => (1, 2, 3, 4, 5, 6)
tup[:2]   # => (1, 2)
2 in tup   # => True

# 可以把元组合列表解包，赋值给变量
a, b, c = (1, 2, 3)     # 现在 a 是 1，b 是 2，c 是 3
# 也可以做扩展解包
a, *b, c = (1, 2, 3, 4)  # 现在 a 是 1, b 是 [2, 3]， c 是 4
# 元组周围的括号是可以省略的
d, e, f = 4, 5, 6 # 元组 4, 5, 6 通过解包被赋值给变量 d, e, f
# 交换两个变量的值就这么简单
e, d = d, e     # 现在 d 是 5，e 是 4


# 字典用来存储 key 到 value 的映射关系
empty_dict = {}
# 初始化的字典
filled_dict = {"one": 1, "two": 2, "three": 3}

# 字典的 key 必须为不可变类型。 这是为了确保 key 被转换为唯一的哈希值以用于快速查询
# 不可变类型包括整数、浮点、字符串、元组
invalid_dict = {[1,2,3]: "123"}  # => 抛出 TypeError: unhashable type: 'list'
valid_dict = {(1,2,3):[1,2,3]}   # 然而 value 可以是任何类型

# 用[]取值
filled_dict["one"]   # => 1

# 用 keys 获得所有的键。
# 因为 keys 返回一个可迭代对象，所以我们需要把它包在 "list()" 里才能转换为列表。
# 我们下面会详细介绍可迭代。
# 注意: 对于版本 < 3.7 的 python, 字典的 key 的排序是无序的。你的运行结果
# 可能与下面的例子不符，但是在 3.7 版本，字典中的项会按照他们被插入到字典的顺序进行排序
list(filled_dict.keys())  # => ["three", "two", "one"] Python 版本 <3.7
list(filled_dict.keys())  # => ["one", "two", "three"] Python 版本 3.7+

# 用 "values()" 获得所有的值。跟 keys 一样也是可迭代对象，要使用 "list()" 才能转换为列表。
# 注意: 排序顺序和 keys 的情况相同。

list(filled_dict.values())  # => [3, 2, 1] Python 版本 < 3.7
list(filled_dict.values())  # => [1, 2, 3] Python 版本 3.7+


# 用in测试一个字典是否包含一个键
"one" in filled_dict   # => True
1 in filled_dict   # => False

# 访问不存在的键会导致 KeyError
filled_dict["four"]   # KeyError

# 用 "get()" 来避免KeyError
filled_dict.get("one")      # => 1
filled_dict.get("four")     # => None
# 当键不存在的时候 "get()" 方法可以返回默认值
filled_dict.get("one", 4)   # => 1
filled_dict.get("four", 4)  # => 4

# "setdefault()" 方法只有当键不存在的时候插入新值
filled_dict.setdefault("five", 5)  # filled_dict["five"] 设为5
filled_dict.setdefault("five", 6)  # filled_dict["five"] 还是5

# 字典赋值
filled_dict.update({"four":4}) # => {"one": 1, "two": 2, "three": 3, "four": 4}
filled_dict["four"] = 4        # 另一种赋值方法

# 用 del 删除项
del filled_dict["one"]  # 从 filled_dict 中把 one 删除


# 用 set 表达集合
empty_set = set()
# 初始化一个集合，语法跟字典相似。
some_set = {1, 1, 2, 2, 3, 4}   # some_set现在是 {1, 2, 3, 4}

# 类似字典的 keys，set 的元素也必须是不可变类型
invalid_set = {[1], 1}  # => Raises a TypeError: unhashable type: 'list'
valid_set = {(1,), 1}

# 可以把集合赋值于变量
filled_set = some_set

# 为集合添加元素
filled_set.add(5)   # filled_set 现在是 {1, 2, 3, 4, 5}
# set 没有重复的元素
filled_set.add(5)   # filled_set 依然是 {1, 2, 3, 4, 5}

# "&" 取交集
other_set = {3, 4, 5, 6}
filled_set & other_set   # => {3, 4, 5}

# "|" 取并集
filled_set | other_set   # => {1, 2, 3, 4, 5, 6}

# "-" 取补集
{1, 2, 3, 4} - {2, 3, 5}   # => {1, 4}

# "^" 取异或集（对称差）
{1, 2, 3, 4} ^ {2, 3, 5}  # => {1, 4, 5}

# 判断左边的集合是否是右边集合的超集
{1, 2} >= {1, 2, 3} # => False

# 判断左边的集合是否是右边集合的子集
{1, 2} <= {1, 2, 3} # => True

# in 测试集合是否包含元素
2 in filled_set   # => True
10 in filled_set   # => False

# 单层集合的深度复制
filled_set = some_set.copy()  # filled_set 是 {1, 2, 3, 4, 5}
filled_set is some_set        # => False

####################################################
## 3. 流程控制和迭代器
####################################################

# 先随便定义一个变量
some_var = 5

# 这是个if语句。注意缩进在Python里是有意义的！
# 缩进要使用 4 个空格而不是 tabs。
# 这段代码会打印 "some_var is smaller than 10"
if some_var > 10:
    print("some_var is totally bigger than 10.")
elif some_var < 10:    # elif 语句是可选的
    print("some_var is smaller than 10.")
else:                  # else 也是可选的
    print("some_var is indeed 10.")


"""
用 for 循环语句遍历列表
打印:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # 你可以使用 format() 格式化字符串并插入值
    print("{} is a mammal".format(animal))

"""
"range(number)" 返回数字列表从 0 到 number 的数字
打印:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)
    
"""
"range(lower, upper)" 会返回一个包含从 lower 到 upper 的数字迭代器
prints:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
"range(lower, upper, step)" 会返回一个，从 lower 到 upper、并且间隔值为 step 的迭代器。
如果 step 未传入则会使用默认值 1
prints:
    4
    6
"""
for i in range(4, 8, 2):
    print(i)

"""
遍历列表，并且同时返回列表里的每一个元素的索引和数值。
prints:
    0 dog
    1 cat
    2 mouse
"""
animals = ["dog", "cat", "mouse"]
for i, value in enumerate(animals):
    print(i, value)

"""
while 循环直到条件不满足
打印:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # x = x + 1 的简写


# 用 try/except 块处理异常状况
try:
    # 用 raise 抛出异常
    raise IndexError("This is an index error")
except IndexError as e:
    pass    						 # pass 是无操作，但是应该在这里处理错误
except (TypeError, NameError):
    pass    						 # 可以同时处理不同类的错误
else:                    # else语句是可选的，必须在所有的except之后
    print("All good!")   # 只有当try运行完没有错误的时候这句才会运行
finally:								 # 在任何情况下都会执行
 		print("We can clean up resources here")

# 你可以使用 with 语句来代替 try/finally 对操作进行结束的操作
with open("myfile.txt") as f:
    for line in f:
        print(line)
        
# 写入文件
contents = {"aa": 12, "bb": 21}
with open("myfile1.txt", "w+") as file:
    file.write(str(contents))        # 写入字符串到文件

with open("myfile2.txt", "w+") as file:
    file.write(json.dumps(contents)) # 写入对象到文件

# Reading from a file
with open('myfile1.txt', "r+") as file:
    contents = file.read()           # 从文件读取字符串
print(contents)
# print: {"aa": 12, "bb": 21}

with open('myfile2.txt', "r+") as file:
    contents = json.load(file)       # 从文件读取 json 对象
print(contents)
# print: {"aa": 12, "bb": 21}


# Python 提供一个叫做可迭代 (iterable) 的基本抽象。一个可迭代对象是可以被当作序列
# 的对象。比如说上面 range 返回的对象就是可迭代的。

filled_dict = {"one": 1, "two": 2, "three": 3}
our_iterable = filled_dict.keys()
print(our_iterable) # => dict_keys(['one', 'two', 'three'])，是一个实现可迭代接口的对象

# 可迭代对象可以遍历
for i in our_iterable:
    print(i)    # 打印 one, two, three

# 但是不可以随机访问
our_iterable[1]  # 抛出TypeError

# 可迭代对象知道怎么生成迭代器
our_iterator = iter(our_iterable)

# 迭代器是一个可以记住遍历的位置的对象
# 用 "next()" 获得下一个对象
next(our_iterator)  # => "one"

# 再一次调取 "next()" 时会记得位置
next(our_iterator)  # => "two"
next(our_iterator)  # => "three"

# 当迭代器所有元素都取出后，会抛出 StopIteration
next(our_iterator) # 抛出 StopIteration

# 我们可以通过遍历还访问所有的值，实际上，for 内部实现了迭代
our_iterator = iter(our_iterable)
for i in our_iterator:
    print(i)  # 依次打印 one, two, three

# 可以用 list 一次取出迭代器或者可迭代对象所有的元素
list(filled_dict.keys())  # => 返回 ["one", "two", "three"]
list(our_iterator)  # => 返回 [] 因为迭代的位置被保存了


####################################################
## 4. 函数
####################################################

# 用def定义新函数
def add(x, y):
    print("x is {} and y is {}".format(x, y))
    return x + y    # 用 return 语句返回

# 调用函数
add(5, 6)   # => 打印 "x is 5 and y is 6" 并且返回 11

# 也可以用关键字参数来调用函数
add(y=6, x=5)   # 关键字参数可以用任何顺序


# 我们可以定义一个可变参数函数
def varargs(*args):
    return args

varargs(1, 2, 3)   # => (1, 2, 3)


# 我们也可以定义一个关键字可变参数函数
def keyword_args(**kwargs):
    return kwargs

# 我们来看看结果是什么：
keyword_args(big="foot", loch="ness")   # => {"big": "foot", "loch": "ness"}


# 这两种可变参数可以混着用
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) prints:
    (1, 2)
    {"a": 3, "b": 4}
"""

# 调用可变参数函数时可以做跟上面相反的，用 * 展开元组，用 ** 展开字典。
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)   # 相当于 all_the_args(1, 2, 3, 4)
all_the_args(**kwargs)   # 相当于 all_the_args(a=3, b=4)
all_the_args(*args, **kwargs)   # 相当于 all_the_args(1, 2, 3, 4, a=3, b=4)

# 使用返回多个数值（返回值为元组类型）
def swap(x, y):
    return y, x  # 用不带括号的元组的格式来返回多个数值
                 # (注意: 括号不需要加，但是也可以加)

x = 1
y = 2
x, y = swap(x, y)     # => x = 2, y = 1
# (x, y) = swap(x,y)  # 同上，括号不需要加，但是也可以加

    
# 函数作用域
x = 5

def setX(num):
    # 局部作用域的 x 和全局域的 x 是不同的
    x = num # => 43
    print (x) # => 43

def setGlobalX(num):
    global x
    print (x) # => 5
    x = num   # 现在全局域的 x 被赋值
    print (x) # => 6

setX(43)
setGlobalX(6)


# 函数在 Python 是一等公民
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# 也有匿名函数
(lambda x: x > 2)(3)                  # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# 内置的高阶函数
list(map(add_10, [1, 2, 3]))          # => [11, 12, 13]
list(map(max, [1, 2, 3], [4, 2, 1]))  # => [4, 2, 3]

list(filter(lambda x: x > 5, [3, 4, 5, 6, 7]))  # => [6, 7]

# 用列表推导式可以简化映射和过滤。列表推导式的返回值是另一个列表。
[add_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]   # => [6, 7]

# 你也可以用这种方式实现对集合和字典的构建
{x for x in 'abcddeef' if x not in 'abc'}  # => {'d', 'e', 'f'}
{x: x**2 for x in range(5)}  # => {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}


####################################################
## 5. 模块
####################################################

# 导入模块
import math
print(math.sqrt(16))  # => 4.0

# 你可以导入模块中具体的函数
from math import ceil, floor
print(ceil(3.7))   # => 4.0
print(floor(3.7))  # => 3.0

# 你可以导入模块中的所有的函数
# 警告: 此操作不推荐
from math import *

# 你可以对模块名进行简化
import math as m
math.sqrt(16) == m.sqrt(16)  # => True

# Python 模块实质上是 Python 文件
# 你可以自己编写自己的模块，然后导入
# 模块的名称和文件名相同

# 你可以用 "dir()" 查看模块中定义的函数和字段
import math
dir(math)

# 当你的脚本文件所在的文件夹也包含了一个名为 math.py 的 Python 文件
# 这个 math.p 文件会被代替引入，而不是引入 Python 內建模块中的 math
# 出现这个情况的原因是本地文件夹的引入优先级要比 Python 內建库引入优先级要高


####################################################
## 6. 类
####################################################

# 我们使用 "class" 语句来创建类
class Human:

    # 一个类的字段。 这个字段共享给这个类的所有实例。
    species = "H. sapiens"

    # 构造方法，当实例被初始化时被调用。注意名字前后的双下划线，这是表明这个属
    # 性或方法对 Python 有特殊意义，但是允许用户自行定义。
    # 方法(可能是对象或者属性) 类似: __init__, __str__,__repr__ etc
    # 都是特殊的方法
    # 你自己取名时不应该用这种格式
    def __init__(self, name):
        # 将参数赋值给实例的 name 字段
        self.name = name

        # 初始化属性
        self._age = 0

    # 实例方法，第一个参数总是self，也就是这个实例对象
    def say(self, msg):
        print("{name}: {message}".format(name=self.name, message=msg))

    # 另一个实例方法
    def sing(self):
        return 'yo... yo... microphone check... one two... one two...'

    # 类方法，被所有此类的实例共用。
    # 第一个参数是这个类对象。
    @classmethod
    def get_species(cls):
        return cls.species

    # 静态方法。调用时没有实例或类的绑定。
    @staticmethod
    def grunt():
        return "*grunt*"

    # property 有点类似 getter
    # 它把方法 age() 转换为同名并且只读的属性
    # 通常情况下，可以不需要编写复杂的 getter 和 setter。
    @property
    def age(self):
        return self._age

    # 允许属性被修改
    @age.setter
    def age(self, age):
        self._age = age

    # 允许属性被删除
    @age.deleter
    def age(self):
        del self._age

# 当 Python 解释器在读取源文件的时候，就会执行文件中所有的代码
# 对 __name__ 的检查可以保证这块代码只会在执行这个模块是住程序情况下被运行（而不是在引用时运行）
if __name__ == '__main__':
    # 
    i = Human(name="Ian")
    i.say("hi")                     # "Ian: hi"
    j = Human("Joel")
    j.say("hello")                  # "Joel: hello"
    # i 和 j 都是 Human 实例化后的对象，换一句话说，它们都是 Human 实例

    # 运行类方法 (classmethod)
    i.say(i.get_species())          # "Ian: H. sapiens"
    # 修改共享的类属性
    Human.species = "H. neanderthalensis"
    i.say(i.get_species())          # => "Ian: H. neanderthalensis"
    j.say(j.get_species())          # => "Joel: H. neanderthalensis"

    # 运行静态方法 (staticmethod)
    print(Human.grunt())            # => "*grunt*"

    # 实例上也可以执行静态方法
    print(i.grunt())                # => "*grunt*"

    # 更新实例的属性
    i.age = 42
    # 访问实例的属性
    i.say(i.age)                    # => "Ian: 42"
    j.say(j.age)                    # => "Joel: 0"
    # 删除实例的属性
    del i.age
    # i.age                         # => 这会抛出一个错误: AttributeError


####################################################
## 6.1 类的继承
####################################################

# 继承机制允许子类可以继承父类上的方法和变量。
# 我们可以把 Human 类作为一个基础类或者说叫做父类，
# 然后定义一个名为 Superhero 的子类来继承父类上的比如 "species"、 "name"、 "age" 的属性
# 和比如 "sing" 、"grunt" 这样的方法，同时，也可以定义它自己独有的属性

# 基于 Python 文件模块化的特点，你可以把这个类放在独立的文件中，比如说，human.py。

# 要从别的文件导入函数，需要使用以下的语句
# from "filename-without-extension" import "function-or-class"

from human import Human

# 指定父类作为类初始化的参数
class Superhero(Human):
		
    # 如果子类需要继承所有父类的定义，并且不需要做任何的修改，
    # 你可以直接使用 "pass" 关键字（并且不需要其他任何语句）
    # 但是在这个例子中会被注释掉，以用来生成不一样的子类。
    # pass

    # 子类可以重写父类定义的字段
    species = 'Superhuman'
		
    # 子类会自动的继承父类的构造函数包括它的参数，但同时，子类也可以新增额外的参数或者定义，
    # 甚至去覆盖父类的方法比如说构造函数。
    # 这个构造函数从父类 "Human" 上继承了 "name" 参数，同时又新增了 "superpower" 和
    # "movie" 参数:
    def __init__(self, name, movie=False,
                 superpowers=["super strength", "bulletproofing"]):

        # 新增额外类的参数
        self.fictional = True
        self.movie = movie
        # 注意可变的默认值，因为默认值是共享的
        self.superpowers = superpowers
				
        # "super" 函数让你可以访问父类中被子类重写的方法
        # 在这个例子中，被重写的是 __init__ 方法
        # 这个语句是用来运行父类的构造函数:
        super().__init__(name)

    # 重写父类中的 sing 方法
    def sing(self):
        return 'Dun, dun, DUN!'

    # 新增一个额外的方法
    def boast(self):
        for power in self.superpowers:
            print("I wield the power of {pow}!".format(pow=power))


if __name__ == '__main__':
    sup = Superhero(name="Tick")

    # 检查实例类型
    if isinstance(sup, Human):
        print('I am human')
    if type(sup) is Superhero:
        print('I am a superhero')

    # 获取方法解析顺序 MRO，MRO 被用于 getattr() 和 super()
    # 这个字段是动态的，并且可以被修改
    print(Superhero.__mro__)    # => (<class '__main__.Superhero'>,
                                # => <class 'human.Human'>, <class 'object'>)

    # 调用父类的方法并且使用子类的属性
    print(sup.get_species())    # => Superhuman

    # 调用被重写的方法
    print(sup.sing())           # => Dun, dun, DUN!

    # 调用 Human 的方法
    sup.say('Spoon')            # => Tick: Spoon

    # 调用 Superhero 独有的方法
    sup.boast()                 # => I wield the power of super strength!
                                # => I wield the power of bulletproofing!

    # 继承类的字段
    sup.age = 31
    print(sup.age)              # => 31

    # Superhero 独有的字段
    print('Am I Oscar eligible? ' + str(sup.movie))


####################################################
## 6.2 多重继承
####################################################

# 定义另一个类
# bat.py
class Bat:

    species = 'Baty'

    def __init__(self, can_fly=True):
        self.fly = can_fly

    # 这个类同样有 say 的方法
    def say(self, msg):
        msg = '... ... ...'
        return msg

    # 新增一个独有的方法
    def sonar(self):
        return '))) ... ((('

if __name__ == '__main__':
    b = Bat()
    print(b.say('hello'))
    print(b.fly)

# 现在我们来定义一个类来同时继承 Superhero 和 Bat
# superhero.py
from superhero import Superhero
from bat import Bat

# 定义 Batman 作为子类，来同时继承 SuperHero 和 Bat
class Batman(Superhero, Bat):

    def __init__(self, *args, **kwargs):
        # 通常要继承属性，你必须调用 super:
        # super(Batman, self).__init__(*args, **kwargs)
        # 然而在这里我们处理的是多重继承，而 super() 只会返回 MRO 列表的下一个基础类。
        # 因此，我们需要显示的调用初始类的 __init___
        # *args 和 **kwargs 传递参数时更加清晰整洁，而对于父类而言像是 “剥了一层洋葱”
        Superhero.__init__(self, 'anonymous', movie=True,
                           superpowers=['Wealthy'], *args, **kwargs)
        Bat.__init__(self, *args, can_fly=False, **kwargs)
        # 重写了 name 字段
        self.name = 'Sad Affleck'

    def sing(self):
        return 'nan nan nan nan nan batman!'


if __name__ == '__main__':
    sup = Batman()

    # 获取方法解析顺序 MRO，MRO 被用于 getattr() 和 super()
    # 这个字段是动态的，并且可以被修改
    print(Batman.__mro__)       # => (<class '__main__.Batman'>,
                                # => <class 'superhero.Superhero'>,
                                # => <class 'human.Human'>,
                                # => <class 'bat.Bat'>, <class 'object'>)

    # 调用父类的方法并且使用子类的属性
    print(sup.get_species())    # => Superhuman

    # 调用被重写的类
    print(sup.sing())           # => nan nan nan nan nan batman!

    # 调用 Human 上的方法，(之所以是 Human 而不是 Bat)，是因为继承顺序起了作用
    sup.say('I agree')          # => Sad Affleck: I agree

    # 调用仅存在于第二个继承的父类的方法
    print(sup.sonar())          # => ))) ... (((

    # 继承类的属性
    sup.age = 100
    print(sup.age)              # => 100

    # 从第二个类上继承字段，并且其默认值被重写
    print('Can I fly? ' + str(sup.fly)) # => Can I fly? False


####################################################
## 7. 高级用法
####################################################

# 用生成器(generators)方便地写惰性运算
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# 生成器只有在需要时才计算下一个值。它们每一次循环只生成一个值，而不是把所有的
# 值全部算好。
#
# range的返回值也是一个生成器，不然一个1到900000000的列表会花很多时间和内存。
#
# 如果你想用一个Python的关键字当作变量名，可以加一个下划线来区分。
range_ = range(1, 900000000)
# 当找到一个 >=30 的结果就会停
# 这意味着 `double_numbers` 不会生成大于30的数。
for i in double_numbers(range_):
    print(i)
    if i >= 30:
        break
# 你也可以把一个生成器推导直接转换为列表
values = (-x for x in [1,2,3,4,5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]


# 装饰器(decorators)
# 这个例子中，beg装饰say
# beg会先调用say。如果返回的say_please为真，beg会改变返回的字符串。
from functools import wraps


def beg(target_function):
    @wraps(target_function)
    def wrapper(*args, **kwargs):
        msg, say_please = target_function(*args, **kwargs)
        if say_please:
            return "{} {}".format(msg, "Please! I am poor :(")
        return msg

    return wrapper


@beg
def say(say_please=False):
    msg = "Can you buy me a beer?"
    return msg, say_please


print(say())  # Can you buy me a beer?
print(say(say_please=True))  # Can you buy me a beer? Please! I am poor :(
```



## 想继续学吗？

### 在线免费材料（英文）

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com/)
* [Ideas for Python Projects](http://pythonpracticeprojects.com/)
* [The Official Docs](https://docs.python.org/3/)
* [Hitchhiker’s Guide to Python](https://docs.python-guide.org/en/latest/)
* [Python Course](https://www.python-course.eu/)
* [Free Interactive Python Course](http://www.kikodo.io/)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)
* [A curated list of awesome Python frameworks, libraries and software](https://github.com/vinta/awesome-python)
* [30 Python Language Features and Tricks You May Not Know About](https://sahandsaba.com/thirty-python-language-features-and-tricks-you-may-not-know.html)
* [Official Style Guide for Python](https://www.python.org/dev/peps/pep-0008/)
* [Python 3 Computer Science Circles](https://cscircles.cemc.uwaterloo.ca/)
* [Dive Into Python 3](https://www.diveintopython3.net/index.html)
* [A Crash Course in Python for Scientists](https://nbviewer.jupyter.org/gist/anonymous/5924718)
* [Python Tutorial for Intermediates](https://pythonbasics.org/)
* [Build a Desktop App with Python](https://pythonpyqt.com/)

### 书籍（也是英文）

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)

