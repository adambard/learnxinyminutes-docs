"""
---
language: python3
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["Zachary Ferguson", "http://github.com/zfergus2"]
    - ["evuez", "http://github.com/evuez"]
translators:
    - ["kakakaya", "https://github.com/kakakaya"]
filename: learnpython3.py
---

90年代の初め、Guido Van RossumによってPythonは作成されました。現在となっては、最も有名な言語の1つです。
私は構文の明快さによって、Pythonと恋に落ちました。
以下は基本的に実行可能な疑似コードです。

フィードバッグは大歓迎です! [@louiedinh](http://twitter.com/louiedinh) または louiedinh [at] [google's email service] にご連絡下さい!

Note: この記事はPython 3に内容を絞っています。もし古いPython 2.7を学習したいなら、 [こちら](http://learnxinyminutes.com/docs/python/) をご確認下さい。

```python
"""

# 1行のコメントは番号記号(#)から始まります。

""" 複数行の文字は、"を3つ繋げることで
    書くことができます。
    また、これはコメントとしてもよく使われます。
"""

####################################################
# 1. プリミティブ型と演算子
####################################################

# 数字です
3  # => 3

# 四則演算はあなたの期待通りに動きます。
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20
35 / 5  # => 7.0

# 整数除算の結果は、正負に関わらず小数の切り捨てが行われます。
5 // 3       # => 1
5.0 // 3.0   # => 1.0 # 浮動小数点でも同様に動作します。
-5 // 3      # => -2
-5.0 // 3.0  # => -2.0

# 除算の結果は常に浮動小数点になります。
10.0 / 3  # => 3.3333333333333335

# 剰余の計算
7 % 3  # => 1

# 冪乗 (x**y, x の y 乗)
2**4  # => 16

# 括弧により、計算の順番を優先させられます。
(1 + 3) * 2  # => 8

# 真偽値はプリミティブ型です(大文字から始まっていることに注意!)
True
False

# not で真偽を反転させられます。
not True   # => False
not False  # => True

# ブール演算
# 注意: "and" と "or" は小文字です
True and False  # => False
False or True   # => True

# 整数でブール演算をするときのメモ
0 and 2     # => 0
-5 or 0     # => -5
0 == False  # => True
2 == True   # => False
1 == True   # => True

# 値が等しいか確認するには ==
1 == 1  # => True
2 == 1  # => False

# 値が等しくないか確認するには !=
1 != 1  # => False
2 != 1  # => True

# 他の比較方法
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# 比較は連結させられます!
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# (is vs. ==)
# "is" は、2つの変数が同一のオブジェクトを参照しているか確認します。
# 一方 "==" は、それぞれが参照する2つのオブジェクトが同じ値を持つか確認します。
a = [1, 2, 3, 4]  # a は新しいリストの [1, 2, 3, 4] を指します。
b = a             # b は a が指すリストを指します。
b is a            # => True, a と b は同一のオブジェクトを参照しています。
b == a            # => True, a と b が参照するオブジェクトの値は等しいです。
b = [1, 2, 3, 4]  # b は新しいリストの [1, 2, 3, 4] を指します。
b is a            # => False, a と b は別々のオブジェクトを参照しています。
b == a            # => True, a と b が参照するオブジェクトの値は等しいです。

# " または ' を使って文字列を作成します。
"This is a string."
'This is also a string.'

# 文字列も加算をすることができます!でも、あまり行わないように。
"Hello " + "world!"  # => "Hello world!"
# '+' を使わなくても連結はできます。
"Hello " "world!"    # => "Hello world!"

# 文字列は文字のリストであるかのように扱うことができます。
"This is a string"[0]  # => 'T'

# 文字列の長さを得るにはこのようにします。
len("This is a string")  # => 16

# .format で文字列のフォーマットを行えます
"{} can be {}".format("Strings", "interpolated")  # => "Strings can be interpolated"

# 入力を減らすために、フォーマットするときに引数を繰り返し使うことができます。
"{0} be nimble, {0} be quick, {0} jump over the {1}".format("Jack", "candle stick")
# => "Jack be nimble, Jack be quick, Jack jump over the candle stick"

# 引数の順番を数えるのがお嫌い？キーワード引数をどうぞ。
"{name} wants to eat {food}".format(name="Bob", food="lasagna")  # => "Bob wants to eat lasagna"

# もし Python 3 のコードを Python 2.5以下でも使う必要があるなら、
# 旧式のフォーマット方法を使うこともできます。
"%s can be %s the %s way" % ("Strings", "interpolated", "old")  # => "Strings can be interpolated the old way"


# None はオブジェクトです(大文字からです!)
None  # => None

# オブジェクトがNoneであるか確認するのに "==" 演算子を使わないように。
# 代わりに "is" を使いましょう。オブジェクトの素性を確認できます。
"etc" is None  # => False
None is None   # => True

# None や 0 、空の 文字列/リスト/辞書/タプル は全て False として評価されます。
# 他の全ての値は True になります。
bool(0)   # => False
bool("")  # => False
bool([])  # => False
bool({})  # => False
bool(())  # => False

####################################################
# 2. Variables and Collections
####################################################

# Python にはprint関数があります。
print("I'm Python. Nice to meet you!")  # => I'm Python. Nice to meet you!

# 標準では、print関数は最後に改行を出力します。
# この動作を変更するためには、オプション引数を利用します。
print("Hello, World", end="!")  # => Hello, World!

# コンソールから入力を得るための簡単な例
input_string_var = input("Enter some data: ")  # 入力を文字列として返します
# Note: Python の初期のバージョンでは、 input() は raw_input() という名前で存在します。

# 変数に代入する前に宣言する必要はありません。
# 慣例的に、小文字でアンダースコア区切り ( lower_case_with_underscores ) の変数が使われます。
some_var = 5
some_var  # => 5

# 代入されていない変数へのアクセスは例外を引き起こします。
# See Control Flow to learn more about exception handling. TODO
some_unknown_var  # NameError を送出します

# ifは式として使用できます。
# C言語の「?:(三項演算子)」に対応する例:
"yahoo!" if 3 > 2 else 2  # => "yahoo!"

# リストは順序を保存します。
li = []
# 値の入っているリストも作成できます。
other_li = [4, 5, 6]

# append により、リストの末尾にものを入れられます。
li.append(1)    # li is now [1]
li.append(2)    # li is now [1, 2]
li.append(4)    # li is now [1, 2, 4]
li.append(3)    # li is now [1, 2, 4, 3]
# pop でリストの末尾から取り除けます。
li.pop()                        # => 3 and li is now [1, 2, 4]
# 元に戻しましょう!
li.append(3)                    # li is now [1, 2, 4, 3] again.

# 配列のように、リストにアクセスできます。
li[0]                           # => 1
# 最後の要素を参照できます。
li[-1]                          # => 3

# 範囲外の要素を参照すると IndexError になります。
li[4]                           # IndexError が発生します

# スライス構文により範囲を参照できます。
li[1:3]                         # => [2, 4]
# 先端を取り除く
li[2:]                          # => [4, 3]
# 末尾を取り除く
li[:3]                          # => [1, 2, 4]
# 1つ飛ばしで選択する
li[::2]                         # =>[1, 4]
# 反転したリストを得る
li[::-1]                        # => [3, 4, 2, 1]
# これらの任意の組み合わせにより、より複雑なスライスを作ることができます。
# li[start:end:step]

# スライスにより、深いコピーを1階層分行うことができます。
li2 = li[:]          # => li2 = [1, 2, 4, 3] だが、 (li2 is li) はFalseになる。

# "del"によりリストから任意の要素を削除できます。
del li[2]                       # li は [1, 2, 3] になりました。

# "remove"で最初に出現する要素を削除できます。
li.remove(2)                    # li は [1, 3] になりました。
li.remove(2)                    # 2はリストの中に存在しないので、 ValueError が発生します。

# 要素を好きなところに挿入できます。
li.insert(1, 2)                 # li は [1, 2, 3] に戻りました。

# "index"で引数の要素が最初に出現する場所のインデックスを得られます。
li.index(2)                     # => 1
li.index(4)                     # 4はリストの中に存在しないので、 ValueError が発生します。

# リスト同士を足すこともできます。
# Note: li と other_li の値は変更されません。
li + other_li                   # => [1, 2, 3, 4, 5, 6]

# "extend()"で他のリストを連結することができます。
li.extend(other_li)             # li は [1, 2, 3, 4, 5, 6] になります。

# リストの中に値が存在するか、 "in" で確認できます。
1 in li                         # => True

# 長さは "len()" で確認できます。
len(li)                         # => 6


# タプルはリストのようなものですが、不変であるという違いがあります。
tup = (1, 2, 3)
tup[0]                      # => 1
tup[0] = 3                  # 内容を変更しようとすると TypeError が発生します。

# 長さが1のタプルを作成するには、要素の後にカンマを付ける必要があります。
# しかし、それ以外の長さなら、例え長さが0でもそのようにする必要はありません。
type((1))                       # => <class 'int'>
type((1,))                      # => <class 'tuple'>
type(())                        # => <class 'tuple'>

# 大抵のリスト操作はタプルでも行うことができます。
len(tup)                        # => 3
tup + (4, 5, 6)                 # => (1, 2, 3, 4, 5, 6)
tup[:2]                         # => (1, 2)
2 in tup                        # => True

# タプルやリストから複数の変数に代入することができます。
a, b, c = (1, 2, 3)             # a, b, c にはそれぞれ 1, 2, 3 が代入されました。
# 拡張記法もあります。
a, *b, c = (1, 2, 3, 4)         # a は 1 、 b は [2, 3] 、c は4 になります。
# 括弧を作成しなくてもデフォルトでタプルが作成されます。
d, e, f = 4, 5, 6
# 2つの変数を交換するのがどれほど簡単か見てみましょう。
e, d = d, e                     # d は 5 、 e は e になります。


# 辞書はマップ(キーと値の組み合わせ)を保存できます。
empty_dict = {}
# 値が入っている辞書を直接作成することもできます。
filled_dict = {"one": 1, "two": 2, "three": 3}

# キーは不変の型である必要があります。
# これは、高速化のため、キーを定数のハッシュ値に変換できるようにするためです。
# 不変の型の例として、int、float、string、tupleなどが上げられます。
invalid_dict = {[1, 2, 3]: "123"}  # => list はハッシュ化できないので、 TypeError が発生します。
valid_dict = {(1, 2, 3): [1, 2, 3]}  # 一方、キーに対応する値はどのような型でも利用できます。

# [] で 値を取り出せます。
filled_dict["one"]              # => 1

# "keys()"により、全てのキーを反復可能な形式で取り出せます。
# これをリストにするために、"list()"で囲んでいます。これについては後程解説します。
# Note: 辞書のキーの順番は考慮されていません。実行した結果がこれと異なる場合があります。
list(filled_dict.keys())        # => ["three", "two", "one"]

# "values()"により、全ての値を反復可能な形式で取り出せます。
# 前と同じように、これをリストにするために、"list()"で囲んでいます。
# Note: 辞書の値の順番は考慮されていません。実行した結果がこれと異なる場合があります。
list(filled_dict.values())      # => [3, 2, 1]


# "in" により、辞書のキーが存在するか確認できます。
"one" in filled_dict            # => True
1 in filled_dict                # => False

# 存在しないキーで辞書を参照すると KeyError になります。
filled_dict["four"]             # KeyError

# "get()" メソッドを使うことで KeyError を回避できます。
filled_dict.get("one")          # => 1
filled_dict.get("four")         # => None
# get ではキーが存在しなかったときのデフォルト値を指定できます。
filled_dict.get("one", 4)       # => 1
filled_dict.get("four", 4)      # => 4

# "setdefault()" で、キーが存在しなかった場合のみ、値を設定できます。
filled_dict.setdefault("five", 5)  # filled_dict["five"] は 5 になりました。
filled_dict.setdefault("five", 6)  # filled_dict["five"] は 5 のままです。

# 辞書にマップを追加する
filled_dict.update({"four": 4})  # => {"one": 1, "two": 2, "three": 3, "four": 4}
# filled_dict["four"] = 4        # 辞書に追加する別の方法

# del により辞書からキーを削除できます。
del filled_dict["one"]          # "one" キーを辞書から削除します。

# Python 3.5 以降では、追加の値を取り出す方法があります。
{'a': 1, **{'b': 2}}            # => {'a': 1, 'b': 2}
{'a': 1, **{'a': 2}}            # => {'a': 2}


# set では集合を表現できます。
empty_set = set()
# 集合を一連の値で初期化する例です。辞書に似ていますね？ごめんなさい。
some_set = {1, 1, 2, 2, 3, 4}   # some_set is now {1, 2, 3, 4}

# 辞書のキーのように、集合の値は不変である必要があります。
invalid_set = {[1], 1}         # => list はハッシュ化できないので、 TypeError が送出されます。
valid_set = {(1,), 1}

# 新しい値を集合にセットできます。
filled_set = some_set

# 集合に新しい要素を追加できます。
filled_set.add(5)               # filled_set は {1, 2, 3, 4, 5} になりました。

# & により、集合同士の共通部分が得られます。
other_set = {3, 4, 5, 6}
filled_set & other_set          # => {3, 4, 5}

# | により、集合同士の合併が得られます。
filled_set | other_set          # => {1, 2, 3, 4, 5, 6}

# - により、集合同士の差集合が得られます。
{1, 2, 3, 4} - {2, 3, 5}        # => {1, 4}

# ^ により、集合同士の対象差が得られます。
{1, 2, 3, 4} ^ {2, 3, 5}        # => {1, 4, 5}

# 左の集合が右の集合の上位集合であるか確認。
{1, 2} >= {1, 2, 3}             # => False

# 左の集合が右の集合の部分集合であるか確認。
{1, 2} <= {1, 2, 3}             # => True

# in により値が集合の中に存在するか確認できます。
2 in filled_set                 # => True
10 in filled_set                # => False



####################################################
## 3. Control Flow and Iterables
####################################################

# Let's just make a variable
some_var = 5

# Here is an if statement. Indentation is significant in python!
# prints "some_var is smaller than 10"
if some_var > 10:
    print("some_var is totally bigger than 10.")
elif some_var < 10:    # This elif clause is optional.
    print("some_var is smaller than 10.")
else:                  # This is optional too.
    print("some_var is indeed 10.")


"""
For loops iterate over lists
prints:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # You can use format() to interpolate formatted strings
    print("{} is a mammal".format(animal))

"""
"range(number)" returns an iterable of numbers
from zero to the given number
prints:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
"range(lower, upper)" returns an iterable of numbers
from the lower number to the upper number
prints:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
"range(lower, upper, step)" returns an iterable of numbers
from the lower number to the upper number, while incrementing
by step. If step is not indicated, the default value is 1.
prints:
    4
    6
"""
for i in range(4, 8, 2):
    print(i)
"""

While loops go until a condition is no longer met.
prints:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # Shorthand for x = x + 1

# Handle exceptions with a try/except block
try:
    # Use "raise" to raise an error
    raise IndexError("This is an index error")
except IndexError as e:
    pass                 # Pass is just a no-op. Usually you would do recovery here.
except (TypeError, NameError):
    pass                 # Multiple exceptions can be handled together, if required.
else:                    # Optional clause to the try/except block. Must follow all except blocks
    print("All good!")   # Runs only if the code in try raises no exceptions
finally:                 #  Execute under all circumstances
    print("We can clean up resources here")

# Instead of try/finally to cleanup resources you can use a with statement
with open("myfile.txt") as f:
    for line in f:
        print(line)

# Python offers a fundamental abstraction called the Iterable.
# An iterable is an object that can be treated as a sequence.
# The object returned the range function, is an iterable.

filled_dict = {"one": 1, "two": 2, "three": 3}
our_iterable = filled_dict.keys()
print(our_iterable)  # => dict_keys(['one', 'two', 'three']). This is an object that implements our Iterable interface.

# We can loop over it.
for i in our_iterable:
    print(i)  # Prints one, two, three

# However we cannot address elements by index.
our_iterable[1]  # Raises a TypeError

# An iterable is an object that knows how to create an iterator.
our_iterator = iter(our_iterable)

# Our iterator is an object that can remember the state as we traverse through it.
# We get the next object with "next()".
next(our_iterator)  # => "one"

# It maintains state as we iterate.
next(our_iterator)  # => "two"
next(our_iterator)  # => "three"

# After the iterator has returned all of its data, it gives you a StopIterator Exception
next(our_iterator)  # Raises StopIteration

# You can grab all the elements of an iterator by calling list() on it.
list(filled_dict.keys())  # => Returns ["one", "two", "three"]


####################################################
## 4. Functions
####################################################

# Use "def" to create new functions
def add(x, y):
    print("x is {} and y is {}".format(x, y))
    return x + y  # Return values with a return statement

# Calling functions with parameters
add(5, 6)  # => prints out "x is 5 and y is 6" and returns 11

# Another way to call functions is with keyword arguments
add(y=6, x=5)  # Keyword arguments can arrive in any order.

# You can define functions that take a variable number of
# positional arguments
def varargs(*args):
    return args

varargs(1, 2, 3)  # => (1, 2, 3)

# You can define functions that take a variable number of
# keyword arguments, as well
def keyword_args(**kwargs):
    return kwargs

# Let's call it to see what happens
keyword_args(big="foot", loch="ness")  # => {"big": "foot", "loch": "ness"}


# You can do both at once, if you like
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) prints:
    (1, 2)
    {"a": 3, "b": 4}
"""

# When calling functions, you can do the opposite of args/kwargs!
# Use * to expand tuples and use ** to expand kwargs.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)            # equivalent to foo(1, 2, 3, 4)
all_the_args(**kwargs)         # equivalent to foo(a=3, b=4)
all_the_args(*args, **kwargs)  # equivalent to foo(1, 2, 3, 4, a=3, b=4)

# Returning multiple values (with tuple assignments)
def swap(x, y):
    return y, x  # Return multiple values as a tuple without the parenthesis.
                 # (Note: parenthesis have been excluded but can be included)

x = 1
y = 2
x, y = swap(x, y)     # => x = 2, y = 1
# (x, y) = swap(x,y)  # Again parenthesis have been excluded but can be included.

# Function Scope
x = 5

def set_x(num):
    # Local var x not the same as global variable x
    x = num    # => 43
    print (x)  # => 43

def set_global_x(num):
    global x
    print (x)  # => 5
    x = num    # global var x is now set to 6
    print (x)  # => 6

set_x(43)
set_global_x(6)


# Python has first class functions
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# There are also anonymous functions
(lambda x: x > 2)(3)                  # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# There are built-in higher order functions
list(map(add_10, [1, 2, 3]))          # => [11, 12, 13]
list(map(max, [1, 2, 3], [4, 2, 1]))  # => [4, 2, 3]

list(filter(lambda x: x > 5, [3, 4, 5, 6, 7]))  # => [6, 7]

# We can use list comprehensions for nice maps and filters
# List comprehension stores the output as a list which can itself be a nested list
[add_10(i) for i in [1, 2, 3]]         # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]  # => [6, 7]

# You can construct set and dict comprehensions as well.
{x for x in 'abcddeef' if x not in 'abc'}  # => {'d', 'e', 'f'}
{x: x**2 for x in range(5)}  # => {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}


####################################################
## 5. Modules
####################################################

# You can import modules
import math
print(math.sqrt(16))  # => 4.0

# You can get specific functions from a module
from math import ceil, floor
print(ceil(3.7))   # => 4.0
print(floor(3.7))  # => 3.0

# You can import all functions from a module.
# Warning: this is not recommended
from math import *

# You can shorten module names
import math as m
math.sqrt(16) == m.sqrt(16)  # => True

# Python modules are just ordinary python files. You
# can write your own, and import them. The name of the
# module is the same as the name of the file.

# You can find out which functions and attributes
# defines a module.
import math
dir(math)

# If you have a Python script named math.py in the same
# folder as your current script, the file math.py will
# be loaded instead of the built-in Python module.
# This happens because the local folder has priority
# over Python's built-in libraries.


####################################################
## 6. Classes
####################################################

# We use the "class" operator to get a class
class Human:

    # A class attribute. It is shared by all instances of this class
    species = "H. sapiens"

    # Basic initializer, this is called when this class is instantiated.
    # Note that the double leading and trailing underscores denote objects
    # or attributes that are used by python but that live in user-controlled
    # namespaces. Methods(or objects or attributes) like: __init__, __str__,
    # __repr__ etc. are called magic methods (or sometimes called dunder methods)
    # You should not invent such names on your own.
    def __init__(self, name):
        # Assign the argument to the instance's name attribute
        self.name = name

        # Initialize property
        self.age = 0

    # An instance method. All methods take "self" as the first argument
    def say(self, msg):
        print ("{name}: {message}".format(name=self.name, message=msg))

    # Another instance method
    def sing(self):
        return 'yo... yo... microphone check... one two... one two...'

    # A class method is shared among all instances
    # They are called with the calling class as the first argument
    @classmethod
    def get_species(cls):
        return cls.species

    # A static method is called without a class or instance reference
    @staticmethod
    def grunt():
        return "*grunt*"

    # A property is just like a getter.
    # It turns the method age() into an read-only attribute
    # of the same name.
    @property
    def age(self):
        return self._age

    # This allows the property to be set
    @age.setter
    def age(self, age):
        self._age = age

    # This allows the property to be deleted
    @age.deleter
    def age(self):
        del self._age


# When a Python interpreter reads a source file it executes all its code.
# This __name__ check makes sure this code block is only executed when this
# module is the main program.
if __name__ == '__main__':
    # Instantiate a class
    i = Human(name="Ian")
    i.say("hi")                     # "Ian: hi"
    j = Human("Joel")
    j.say("hello")                  # "Joel: hello"
    # i and j are instances of type Human, or in other words: they are Human objects

    # Call our class method
    i.say(i.get_species())          # "Ian: H. sapiens"
    # Change the shared attribute
    Human.species = "H. neanderthalensis"
    i.say(i.get_species())          # => "Ian: H. neanderthalensis"
    j.say(j.get_species())          # => "Joel: H. neanderthalensis"

    # Call the static method
    print(Human.grunt())            # => "*grunt*"
    print(i.grunt())                # => "*grunt*"

    # Update the property for this instance
    i.age = 42
    # Get the property
    i.say(i.age)                    # => 42
    j.say(j.age)                    # => 0
    # Delete the property
    del i.age
    # i.age                         # => this would raise an AttributeError


####################################################
## 6.1 Multiple Inheritance
####################################################

# Another class definition
class Bat:

    species = 'Baty'

    def __init__(self, can_fly=True):
        self.fly = can_fly

    # This class also has a say method
    def say(self, msg):
        msg = '... ... ...'
        return msg

    # And its own method as well
    def sonar(self):
        return '))) ... ((('

if __name__ == '__main__':
    b = Bat()
    print(b.say('hello'))
    print(b.fly)

# To take advantage of modularization by file you could place the classes above in their own files,
# say, human.py and bat.py

# to import functions from other files use the following format
# from "filename-without-extension" import "function-or-class"

# superhero.py
from human import Human
from bat import Bat

# Batman inherits from both Human and Bat
class Batman(Human, Bat):

    # Batman has its own value for the species class attribute
    species = 'Superhero'

    def __init__(self, *args, **kwargs):
        # Typically to inherit attributes you have to call super:
        #super(Batman, self).__init__(*args, **kwargs)      
        # However we are dealing with multiple inheritance here, and super()
        # only works with the next base class in the MRO list.
        # So instead we explicitly call __init__ for all ancestors.
        # The use of *args and **kwargs allows for a clean way to pass arguments,
        # with each parent "peeling a layer of the onion".
        Human.__init__(self, 'anonymous', *args, **kwargs)
        Bat.__init__(self, *args, can_fly=False, **kwargs)
        # override the value for the name attribute
        self.name = 'Sad Affleck'

    def sing(self):
        return 'nan nan nan nan nan batman!'


if __name__ == '__main__':
    sup = Batman()

    # Instance type checks
    if isinstance(sup, Human):
        print('I am human')
    if isinstance(sup, Bat):
        print('I am bat')
    if type(sup) is Batman:
        print('I am Batman')

    # Get the Method Resolution search Order used by both getattr() and super().
    # This attribute is dynamic and can be updated
    print(Batman.__mro__)       # => (<class '__main__.Batman'>, <class 'human.Human'>, <class 'bat.Bat'>, <class 'object'>)

    # Calls parent method but uses its own class attribute
    print(sup.get_species())    # => Superhero

    # Calls overloaded method
    print(sup.sing())           # => nan nan nan nan nan batman!

    # Calls method from Human, because inheritance order matters
    sup.say('I agree')          # => Sad Affleck: I agree

    # Call method that exists only in 2nd ancestor
    print(sup.sonar())          # => ))) ... (((

    # Inherited class attribute
    sup.age = 100
    print(sup.age)

    # Inherited attribute from 2nd ancestor whose default value was overridden.
    print('Can I fly? ' + str(sup.fly))



####################################################
## 7. Advanced
####################################################

# Generators help you make lazy code.
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# Generators are memory-efficient because they only load the data needed to
# process the next value in the iterable. This allows them to perform
# operations on otherwise prohibitively large value ranges.
# NOTE: `range` replaces `xrange` in Python 3.
for i in double_numbers(range(1, 900000000)):  # `range` is a generator.
    print(i)
    if i >= 30:
        break

# Just as you can create a list comprehension, you can create generator
# comprehensions as well.
values = (-x for x in [1,2,3,4,5])
for x in values:
    print(x)  # prints -1 -2 -3 -4 -5 to console/terminal

# You can also cast a generator comprehension directly to a list.
values = (-x for x in [1,2,3,4,5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]


# Decorators
# In this example `beg` wraps `say`. If say_please is True then it
# will change the returned message.
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


print(say())                 # Can you buy me a beer?
print(say(say_please=True))  # Can you buy me a beer? Please! I am poor :(
```

## Ready For More?

### Free Online

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [Ideas for Python Projects](http://pythonpracticeprojects.com)
* [The Official Docs](http://docs.python.org/3/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Course](http://www.python-course.eu/index.php)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)
* [A curated list of awesome Python frameworks, libraries and software](https://github.com/vinta/awesome-python)
* [30 Python Language Features and Tricks You May Not Know About](http://sahandsaba.com/thirty-python-language-features-and-tricks-you-may-not-know.html)
* [Official Style Guide for Python](https://www.python.org/dev/peps/pep-0008/)
* [Python 3 Computer Science Circles](http://cscircles.cemc.uwaterloo.ca/)
* [Dive Into Python 3](http://www.diveintopython3.net/index.html)
* [A Crash Course in Python for Scientists](http://nbviewer.jupyter.org/gist/anonymous/5924718)
