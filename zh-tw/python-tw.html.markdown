---
language: python
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
    - ["Amin Bandali", "http://aminbandali.com"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["evuez", "http://github.com/evuez"]
translators:
    - ["Michael Yeh", "https://github.com/hinet60613"]
filename: learnpython.py
lang: zh-tw
---

Python是在1990年代早期由Guido Van Rossum創建的。它是現在最流行的程式語言之一。我愛上Python是因為他極為清晰的語法，甚至可以說它就是可執行的虛擬碼。

非常歡迎各位給我們任何回饋！ 你可以在[@louiedinh](http://twitter.com/louiedinh) 或 louiedinh [at] [google's email service]聯絡到我。

註： 本篇文章適用的版本為Python 2.7，但大部分的Python 2.X版本應該都適用。 Python 2.7將會在2020年停止維護，因此建議您可以從Python 3開始學Python。
Python 3.X可以看這篇[Python 3 教學 (英文)](http://learnxinyminutes.com/docs/python3/).

讓程式碼同時支援Python 2.7和3.X是可以做到的，只要引入
 [`__future__` imports](https://docs.python.org/2/library/__future__.html) 模組.
 `__future__` 模組允許你撰寫可以在Python 2上執行的Python 3程式碼，詳細訊息請參考Python 3 教學。

```python

# 單行註解從井字號開始

""" 多行字串可以用三個雙引號
    包住，不過通常這種寫法會
    被拿來當作多行註解
"""

####################################################
## 1. 原始型別與運算元
####################################################

# 你可以使用數字
3  # => 3

# 還有四則運算
1 + 1  # => 2
8 - 1  # => 7
10 * 2  # => 20
35 / 5  # => 7

# 除法比較麻煩，除以整數時會自動捨去小數位。
5 / 2  # => 2

# 要做精確的除法，我們需要浮點數
2.0     # 浮點數
11.0 / 4.0  # => 2.75 精確多了！

# 整數除法的無條件捨去對正數或負數都適用
5 // 3     # => 1
5.0 // 3.0 # => 1.0 # 浮點數的整數也適用
-5 // 3  # => -2
-5.0 // 3.0 # => -2.0

# 我們可以用除法模組(參考第六節:模組)，讓
# 單一斜線代表普通除法，而非無條件捨去
from __future__ import division
11/4    # => 2.75  ...普通除法
11//4   # => 2 ...無條件捨去

# 取餘數
7 % 3 # => 1

# 指數 (x的y次方)
2**4 # => 16

# 括號即先乘除後加減
(1 + 3) * 2  # => 8

# 布林運算
# 注意 "and" 和 "or" 的大小寫
True and False #=> False
False or True #=> True

# 用整數與布林值做運算
0 and 2 #=> 0
-5 or 0 #=> -5
0 == False #=> True
2 == True #=> False
1 == True #=> True

# 用not取反向
not True  # => False
not False  # => True

# 等於判斷是用 ==
1 == 1  # => True
2 == 1  # => False

# 不等於判斷是用 !=
1 != 1  # => False
2 != 1  # => True

# 更多比較
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# 比較是可以串接的
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# 字串用單引號 ' 或雙引號 " 建立
"This is a string."
'This is also a string.'

# 字串相加會被串接再一起
"Hello " + "world!"  # => "Hello world!"
# 不用加號也可以做字串相加
"Hello " "world!"  # => "Hello world!"

# ... 也可以做相乘
"Hello" * 3  # => "HelloHelloHello"

# 字串可以被視為字元的陣列
"This is a string"[0]  # => 'T'

# 字串的格式化可以用百分之符號 %
# 儘管在Python 3.1後這個功能被廢棄了，並且在
# 之後的版本會被移除，但還是可以了解一下
x = 'apple'
y = 'lemon'
z = "The items in the basket are %s and %s" % (x,y)

# 新的格式化方式是使用format函式
# 這個方式也是較為推薦的
"{} is a {}".format("This", "placeholder")
"{0} can be {1}".format("strings", "formatted")
# 你也可以用關鍵字，如果你不想數你是要用第幾個變數的話
"{name} wants to eat {food}".format(name="Bob", food="lasagna")

# 無(None) 是一個物件
None  # => None

# 不要用等於符號 "==" 對 無(None)做比較
# 用 "is" 
"etc" is None  # => False
None is None  # => True

# 'is' 運算元是用來識別物件的。對原始型別來說或許沒什麼用，
# 但對物件來說是很有用的。

# 任何物件都可以被當作布林值使用
# 以下的值會被視為是False :
#    - 無(None)
#    - 任何型別的零 (例如: 0, 0L, 0.0, 0j)
#    - 空序列 (例如: '', (), [])
#    - 空容器 (例如: {}, set())
#    - 自定義型別的實體，且滿足某些條件
#      請參考文件: https://docs.python.org/2/reference/datamodel.html#object.__nonzero__
#
# 其餘的值都會被視為True (用bool()函式讓他們回傳布林值).
bool(0)  # => False
bool("")  # => False


####################################################
## 2. 變數與集合
####################################################

# Python的輸出很方便
print "I'm Python. Nice to meet you!" # => I'm Python. Nice to meet you!

# 從命令列獲得值也很方便
input_string_var = raw_input("Enter some data: ") # 資料會被視為字串存進變數
input_var = input("Enter some data: ") # 輸入的資料會被當作Python程式碼執行
# 注意: 請謹慎使用input()函式
# 註: 在Python 3中，input()已被棄用，raw_input()已被更名為input()

# 使用變數前不需要先宣告
some_var = 5    # 方便好用
lower_case_with_underscores
some_var  # => 5

# 存取沒有被賦值的變數會造成例外
# 請參考錯誤流程部分做例外處理
some_other_var  # 造成 NameError

# if可以當判斷式使用
# 相當於C語言中的二元判斷式
"yahoo!" if 3 > 2 else 2  # => "yahoo!"

# 串列型態可以儲存集合
li = []
# 你可以預先填好串列內容
other_li = [4, 5, 6]

# 用append()在串列後新增東西 append
li.append(1)    # 此時 li 內容為 [1]
li.append(2)    # 此時 li 內容為 [1, 2]
li.append(4)    # 此時 li 內容為 [1, 2, 4]
li.append(3)    # 此時 li 內容為 [1, 2, 4, 3]
# 用pop()移除串列尾端的元素
li.pop()        # => 3 and li is now [1, 2, 4]
# 然後再塞回去
li.append(3)    # li is now [1, 2, 4, 3] again.

# 你可以像存取陣列一樣的存取串列
li[0]  # => 1
# 用等號 = 給串列中特定索引的元素賦值
li[0] = 42
li[0]  # => 42
li[0] = 1  # 註: 將其設定回原本的值
# 用 -1 索引值查看串列最後一個元素
li[-1]  # => 3

# 存取超過範圍會產生IndexError
li[4]  # Raises an IndexError

# 你可以用切片語法來存取特定範圍的值
# (相當於數學中的左閉右開區間，即包含最左邊界，但不包含右邊界)
li[1:3]  # => [2, 4]
# 略過開頭元素
li[2:]  # => [4, 3]
# 略過結尾元素
li[:3]  # => [1, 2, 4]
# 每隔兩個元素取值
li[::2]   # =>[1, 4]
# 串列反轉
li[::-1]   # => [3, 4, 2, 1]
# 你可以任意組合來達到你想要的效果
# li[開始索引:結束索引:間隔]

# 用 "del" 從串列中移除任意元素
del li[2]   # 現在 li 內容為 [1, 2, 3]

# 你可以做串列相加
li + other_li   # => [1, 2, 3, 4, 5, 6]
# 註: li 及 other_li 沒有被更動

# 用 "extend()" 做串列串接
li.extend(other_li)   # 現在 li 內容為 [1, 2, 3, 4, 5, 6]

# 移除特定值的第一次出現
li.remove(2)  # 現在 li 內容為 [1, 3, 4, 5, 6]
li.remove(2)  # 2 不在串列中，造成 ValueError

# 在特定位置插入值
li.insert(1, 2)  # 現在 li 內容再次回復為 [1, 2, 3, 4, 5, 6]

# 取得特定值在串列中第一次出現的位置
li.index(2)  # => 1
li.index(7)  # 7 不在串列中，造成 ValueError

# 用 "in" 檢查特定值是否出現在串列中
1 in li   # => True

# 用 "len()" 取得串列長度
len(li)   # => 6


# Tuples are like lists but are immutable.
tup = (1, 2, 3)
tup[0]   # => 1
tup[0] = 3  # Raises a TypeError

# You can do all those list thingies on tuples too
len(tup)   # => 3
tup + (4, 5, 6)   # => (1, 2, 3, 4, 5, 6)
tup[:2]   # => (1, 2)
2 in tup   # => True

# You can unpack tuples (or lists) into variables
a, b, c = (1, 2, 3)     # a is now 1, b is now 2 and c is now 3
d, e, f = 4, 5, 6       # you can leave out the parentheses
# Tuples are created by default if you leave out the parentheses
g = 4, 5, 6             # => (4, 5, 6)
# Now look how easy it is to swap two values
e, d = d, e     # d is now 5 and e is now 4


# Dictionaries store mappings
empty_dict = {}
# Here is a prefilled dictionary
filled_dict = {"one": 1, "two": 2, "three": 3}

# Look up values with []
filled_dict["one"]   # => 1

# Get all keys as a list with "keys()"
filled_dict.keys()   # => ["three", "two", "one"]
# Note - Dictionary key ordering is not guaranteed.
# Your results might not match this exactly.

# Get all values as a list with "values()"
filled_dict.values()   # => [3, 2, 1]
# Note - Same as above regarding key ordering.

# Check for existence of keys in a dictionary with "in"
"one" in filled_dict   # => True
1 in filled_dict   # => False

# Looking up a non-existing key is a KeyError
filled_dict["four"]   # KeyError

# Use "get()" method to avoid the KeyError
filled_dict.get("one")   # => 1
filled_dict.get("four")   # => None
# The get method supports a default argument when the value is missing
filled_dict.get("one", 4)   # => 1
filled_dict.get("four", 4)   # => 4
# note that filled_dict.get("four") is still => None
# (get doesn't set the value in the dictionary)

# set the value of a key with a syntax similar to lists
filled_dict["four"] = 4  # now, filled_dict["four"] => 4

# "setdefault()" inserts into a dictionary only if the given key isn't present
filled_dict.setdefault("five", 5)  # filled_dict["five"] is set to 5
filled_dict.setdefault("five", 6)  # filled_dict["five"] is still 5


# Sets store ... well sets (which are like lists but can contain no duplicates)
empty_set = set()
# Initialize a "set()" with a bunch of values
some_set = set([1, 2, 2, 3, 4])   # some_set is now set([1, 2, 3, 4])

# order is not guaranteed, even though it may sometimes look sorted
another_set = set([4, 3, 2, 2, 1])  # another_set is now set([1, 2, 3, 4])

# Since Python 2.7, {} can be used to declare a set
filled_set = {1, 2, 2, 3, 4}   # => {1, 2, 3, 4}

# Add more items to a set
filled_set.add(5)   # filled_set is now {1, 2, 3, 4, 5}

# Do set intersection with &
other_set = {3, 4, 5, 6}
filled_set & other_set   # => {3, 4, 5}

# Do set union with |
filled_set | other_set   # => {1, 2, 3, 4, 5, 6}

# Do set difference with -
{1, 2, 3, 4} - {2, 3, 5}   # => {1, 4}

# Do set symmetric difference with ^
{1, 2, 3, 4} ^ {2, 3, 5}  # => {1, 4, 5}

# Check if set on the left is a superset of set on the right
{1, 2} >= {1, 2, 3} # => False

# Check if set on the left is a subset of set on the right
{1, 2} <= {1, 2, 3} # => True

# Check for existence in a set with in
2 in filled_set   # => True
10 in filled_set   # => False


####################################################
## 3. Control Flow
####################################################

# Let's just make a variable
some_var = 5

# Here is an if statement. Indentation is significant in python!
# prints "some_var is smaller than 10"
if some_var > 10:
    print "some_var is totally bigger than 10."
elif some_var < 10:    # This elif clause is optional.
    print "some_var is smaller than 10."
else:           # This is optional too.
    print "some_var is indeed 10."


"""
For loops iterate over lists
prints:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # You can use {0} to interpolate formatted strings. (See above.)
    print "{0} is a mammal".format(animal)

"""
"range(number)" returns a list of numbers
from zero to the given number
prints:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
"range(lower, upper)" returns a list of numbers
from the lower number to the upper number
prints:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print i

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
    print x
    x += 1  # Shorthand for x = x + 1

# Handle exceptions with a try/except block

# Works on Python 2.6 and up:
try:
    # Use "raise" to raise an error
    raise IndexError("This is an index error")
except IndexError as e:
    pass    # Pass is just a no-op. Usually you would do recovery here.
except (TypeError, NameError):
    pass    # Multiple exceptions can be handled together, if required.
else:   # Optional clause to the try/except block. Must follow all except blocks
    print "All good!"   # Runs only if the code in try raises no exceptions
finally: #  Execute under all circumstances
    print "We can clean up resources here"

# Instead of try/finally to cleanup resources you can use a with statement
with open("myfile.txt") as f:
    for line in f:
        print line

####################################################
## 4. Functions
####################################################

# Use "def" to create new functions
def add(x, y):
    print "x is {0} and y is {1}".format(x, y)
    return x + y    # Return values with a return statement

# Calling functions with parameters
add(5, 6)   # => prints out "x is 5 and y is 6" and returns 11

# Another way to call functions is with keyword arguments
add(y=6, x=5)   # Keyword arguments can arrive in any order.


# You can define functions that take a variable number of
# positional args, which will be interpreted as a tuple if you do not use the *
def varargs(*args):
    return args

varargs(1, 2, 3)   # => (1, 2, 3)


# You can define functions that take a variable number of
# keyword args, as well, which will be interpreted as a dict if you do not use **
def keyword_args(**kwargs):
    return kwargs

# Let's call it to see what happens
keyword_args(big="foot", loch="ness")   # => {"big": "foot", "loch": "ness"}


# You can do both at once, if you like
def all_the_args(*args, **kwargs):
    print args
    print kwargs
"""
all_the_args(1, 2, a=3, b=4) prints:
    (1, 2)
    {"a": 3, "b": 4}
"""

# When calling functions, you can do the opposite of args/kwargs!
# Use * to expand positional args and use ** to expand keyword args.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)   # equivalent to foo(1, 2, 3, 4)
all_the_args(**kwargs)   # equivalent to foo(a=3, b=4)
all_the_args(*args, **kwargs)   # equivalent to foo(1, 2, 3, 4, a=3, b=4)

# you can pass args and kwargs along to other functions that take args/kwargs
# by expanding them with * and ** respectively
def pass_all_the_args(*args, **kwargs):
    all_the_args(*args, **kwargs)
    print varargs(*args)
    print keyword_args(**kwargs)

# Function Scope
x = 5

def set_x(num):
    # Local var x not the same as global variable x
    x = num # => 43
    print x # => 43

def set_global_x(num):
    global x
    print x # => 5
    x = num # global var x is now set to 6
    print x # => 6

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
(lambda x: x > 2)(3)   # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1) # => 5

# There are built-in higher order functions
map(add_10, [1, 2, 3])   # => [11, 12, 13]
map(max, [1, 2, 3], [4, 2, 1])   # => [4, 2, 3]

filter(lambda x: x > 5, [3, 4, 5, 6, 7])   # => [6, 7]

# We can use list comprehensions for nice maps and filters
[add_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]   # => [6, 7]


####################################################
## 5. Classes
####################################################

# We subclass from object to get a class.
class Human(object):

    # A class attribute. It is shared by all instances of this class
    species = "H. sapiens"

    # Basic initializer, this is called when this class is instantiated.
    # Note that the double leading and trailing underscores denote objects
    # or attributes that are used by python but that live in user-controlled
    # namespaces. You should not invent such names on your own.
    def __init__(self, name):
        # Assign the argument to the instance's name attribute
        self.name = name

        # Initialize property
        self.age = 0


    # An instance method. All methods take "self" as the first argument
    def say(self, msg):
        return "{0}: {1}".format(self.name, msg)

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


# Instantiate a class
i = Human(name="Ian")
print i.say("hi")     # prints out "Ian: hi"

j = Human("Joel")
print j.say("hello")  # prints out "Joel: hello"

# Call our class method
i.get_species()   # => "H. sapiens"

# Change the shared attribute
Human.species = "H. neanderthalensis"
i.get_species()   # => "H. neanderthalensis"
j.get_species()   # => "H. neanderthalensis"

# Call the static method
Human.grunt()   # => "*grunt*"

# Update the property
i.age = 42

# Get the property
i.age # => 42

# Delete the property
del i.age
i.age  # => raises an AttributeError


####################################################
## 6. Modules
####################################################

# You can import modules
import math
print math.sqrt(16)  # => 4

# You can get specific functions from a module
from math import ceil, floor
print ceil(3.7)  # => 4.0
print floor(3.7)   # => 3.0

# You can import all functions from a module.
# Warning: this is not recommended
from math import *

# You can shorten module names
import math as m
math.sqrt(16) == m.sqrt(16)   # => True
# you can also test that the functions are equivalent
from math import sqrt
math.sqrt == m.sqrt == sqrt  # => True

# Python modules are just ordinary python files. You
# can write your own, and import them. The name of the
# module is the same as the name of the file.

# You can find out which functions and attributes
# defines a module.
import math
dir(math)


####################################################
## 7. Advanced
####################################################

# Generators help you make lazy code
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# A generator creates values on the fly.
# Instead of generating and returning all values at once it creates one in each
# iteration.  This means values bigger than 15 wont be processed in
# double_numbers.
# Note xrange is a generator that does the same thing range does.
# Creating a list 1-900000000 would take lot of time and space to be made.
# xrange creates an xrange generator object instead of creating the entire list
# like range does.
# We use a trailing underscore in variable names when we want to use a name that
# would normally collide with a python keyword
xrange_ = xrange(1, 900000000)

# will double all numbers until a result >=30 found
for i in double_numbers(xrange_):
    print i
    if i >= 30:
        break


# Decorators
# in this example beg wraps say
# Beg will call say. If say_please is True then it will change the returned
# message
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


print say()  # Can you buy me a beer?
print say(say_please=True)  # Can you buy me a beer? Please! I am poor :(
```

## Ready For More?

### Free Online

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [The Official Docs](http://docs.python.org/2/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Module of the Week](http://pymotw.com/2/)
* [A Crash Course in Python for Scientists](http://nbviewer.ipython.org/5920182)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)

### Dead Tree

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)
