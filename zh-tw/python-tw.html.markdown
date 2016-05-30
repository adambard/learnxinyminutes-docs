---
language: python
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
    - ["Amin Bandali", "http://aminbandali.com"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["evuez", "http://github.com/evuez"]
translators:
    - ["Michael Yeh", "https://hinet60613.github.io/"]
filename: learnpython-tw.py
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

# 用括號改變運算順序
(1 + 3) * 2  # => 8

# 布林運算
# 注意 "and" 和 "or" 要用小寫
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

# 對沒有被賦值的變數取值會造成例外
# 請參考錯誤流程部分做例外處理
some_other_var  # 造成 NameError

# if可以當判斷式使用
# 相當於C語言中的二元判斷式
"yahoo!" if 3 > 2 else 2  # => "yahoo!"

# 串列型態可以儲存集合
li = []
# 你可以預先填好串列內容
other_li = [4, 5, 6]

# 用append()在串列後新增東西
li.append(1)    # 此時 li 內容為 [1]
li.append(2)    # 此時 li 內容為 [1, 2]
li.append(4)    # 此時 li 內容為 [1, 2, 4]
li.append(3)    # 此時 li 內容為 [1, 2, 4, 3]
# 用pop()移除串列尾端的元素
li.pop()        # => 3 ，此時 li 內容為 [1, 2, 4]
# 然後再塞回去
li.append(3)    # 此時 li 內容再次為 [1, 2, 4, 3]

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


# 元組(Tuple，以下仍用原文)類似於串列，但是它是不可改變的
tup = (1, 2, 3)
tup[0]   # => 1
tup[0] = 3  # 產生TypeError

# 能對串列做的東西都可以對tuple做
len(tup)   # => 3
tup + (4, 5, 6)   # => (1, 2, 3, 4, 5, 6)
tup[:2]   # => (1, 2)
2 in tup   # => True

# 你可以把tuple拆開並分別將值存入不同變數
a, b, c = (1, 2, 3)     # a 現在是 1, b 現在是 2, c 現在是 3
d, e, f = 4, 5, 6       # 也可以不寫括號
# 如果不加括號，預設會產生tuple
g = 4, 5, 6             # => (4, 5, 6)
# 你看，交換兩個值很簡單吧
e, d = d, e     # 此時 d 的值為 5 且 e 的值為 4


# 字典(Dictionary)用來儲存映射關係
empty_dict = {}
# 你可以對字典做初始化
filled_dict = {"one": 1, "two": 2, "three": 3}

# 用 [] 取值
filled_dict["one"]   # => 1

# 用 "keys()" 將所有的Key輸出到一個List中
filled_dict.keys()   # => ["three", "two", "one"]
# 註: 字典裡key的排序是不固定的
# 你的執行結果可能與上面不同
# 譯註: 只能保證所有的key都有出現，但不保證順序

# 用 "values()" 將所有的Value輸出到一個List中
filled_dict.values()   # => [3, 2, 1]
# 註: 同上，不保證順序

# 用 "in" 來檢查指定的Key是否在字典中
"one" in filled_dict   # => True
1 in filled_dict   # => False

# 查詢不存在的Key會造成KeyError
filled_dict["four"]   # KeyError

# 用 "get()" 來避免KeyError
# 若指定的Key不存在的話會得到None
filled_dict.get("one")   # => 1
filled_dict.get("four")   # => None
# "get()" 函式支援預設值，當找不到指定的值時，會回傳指定的預設值
filled_dict.get("one", 4)   # => 1
filled_dict.get("four", 4)   # => 4
# 注意此時 filled_dict.get("four") 仍然為 None
# (get()此時並沒有產生出任何的值)

# 像操作list一樣，對指定的Key賦值
filled_dict["four"] = 4  # 此時 filled_dict["four"] => 4

# "setdefault()" 只在指定的Key不存在時才會將值插入dictionary
filled_dict.setdefault("five", 5)  # filled_dict["five"] 被指定為 5
filled_dict.setdefault("five", 6)  # filled_dict["five"] 仍保持 5


# 集合(Set)被用來儲存...集合。
# 跟串列(List)有點像，但集合內不會有重複的元素
empty_set = set()
# 初始化 "set()" 並給定一些值
some_set = set([1, 2, 2, 3, 4])   # 現在 some_set 為 set([1, 2, 3, 4])，注意重複的元素只有一個會被存入

# 一樣，不保證順序，就算真的有照順序排也只是你運氣好
another_set = set([4, 3, 2, 2, 1])  # another_set 現在為 set([1, 2, 3, 4])

# 從 Python 2.7 開始，可以使用大括號 {} 來宣告Set
filled_set = {1, 2, 2, 3, 4}   # => {1, 2, 3, 4}

# 加入更多元素進入Set
filled_set.add(5)   # filled_set is now {1, 2, 3, 4, 5}

# 用 & 來對兩個集合取交集
other_set = {3, 4, 5, 6}
filled_set & other_set   # => {3, 4, 5}

# 用 | 來對兩個集合取聯集
filled_set | other_set   # => {1, 2, 3, 4, 5, 6}

# 用 - 來將第二個集合內有的元素移出第一個集合
{1, 2, 3, 4} - {2, 3, 5}   # => {1, 4}

# 用 ^ 來對兩個集合取差集
{1, 2, 3, 4} ^ {2, 3, 5}  # => {1, 4, 5}

# 檢查左邊是否為右邊的母集
{1, 2} >= {1, 2, 3} # => False

# 檢查左邊是否為右邊的子集
{1, 2} <= {1, 2, 3} # => True

# 用 in 來檢查某元素是否存在於集合內
2 in filled_set   # => True
10 in filled_set   # => False


####################################################
## 3. 控制流程
####################################################

# 首先，先宣告一個變數
some_var = 5

# 這邊是 if 判斷式。注意，縮排對Python是很重要的。
# 下面應該會印出 "some_var is smaller than 10"
if some_var > 10:
    print "some_var is totally bigger than 10."
elif some_var < 10:    # elif 可有可無
    print "some_var is smaller than 10."
else:           # else 也可有可無
    print "some_var is indeed 10."


"""
For 迴圈會遞迴整的List
下面的程式碼會輸出:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # 你可以用{0}來組合0出格式化字串 (見上面.)
    print "{0} is a mammal".format(animal)

"""
"range(number)" 回傳一個包含從0到給定值的數字List，
下面的程式碼會輸出:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
"range(lower, upper)" 回傳一個包含從給定的下限
到給定的上限的數字List
下面的程式碼會輸出:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print i

"""
While迴圈會執行到條件不成立為止
下面的程式碼會輸出:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # x = x + 1 的簡寫

# 用try/except處理例外

# 適用Python 2.6及以上版本
try:
    # 用 "raise" 來發起例外
    raise IndexError("This is an index error")
except IndexError as e:
    pass    # 毫無反應，就只是個什麼都沒做的pass。通常這邊會讓你做對例外的處理
except (TypeError, NameError):
    pass    # 有需要的話，多種例外可以一起處理
else:   # else 可有可無，但必須寫在所有的except後
    print "All good!"   # 只有在try的時候沒有產生任何except才會被執行
finally: # 不管什麼情況下一定會被執行
    print "We can clean up resources here"

# 除了try/finally以外，你可以用 with 來簡單的處理清理動作
with open("myfile.txt") as f:
    for line in f:
        print line

####################################################
## 4. 函式
####################################################

# 用 "def" 來建立新函式
def add(x, y):
    print "x is {0} and y is {1}".format(x, y)
    return x + y    # 用 "return" 來回傳值

# 用參數來呼叫函式
add(5, 6)   # => 輸出 "x is 5 and y is 6" 並回傳 11

# 你也可以寫上參數名稱來呼叫函式
add(y=6, x=5)   # 這種狀況下，兩個參數的順序並不影響執行


# 你可以定義接受多個變數的函式，用*來表示參數tuple
def varargs(*args):
    return args

varargs(1, 2, 3)   # => (1, 2, 3)


# 你可以定義接受多個變數的函式，用**來表示參數dictionary
def keyword_args(**kwargs):
    return kwargs

# 呼叫看看會發生什麼事吧
keyword_args(big="foot", loch="ness")   # => {"big": "foot", "loch": "ness"}


# 如果你想要，你也可以兩個同時用
def all_the_args(*args, **kwargs):
    print args
    print kwargs
"""
all_the_args(1, 2, a=3, b=4) prints:
    (1, 2)
    {"a": 3, "b": 4}
"""

# 呼叫函式時，你可以做反向的操作
# 用 * 將變數展開為順序排序的變數
# 用 ** 將變數展開為Keyword排序的變數
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)   # 等同於 foo(1, 2, 3, 4)
all_the_args(**kwargs)   # 等同於 foo(a=3, b=4)
all_the_args(*args, **kwargs)   # 等同於 foo(1, 2, 3, 4, a=3, b=4)

# 你可以把args跟kwargs傳到下一個函式內
# 分別用 * 跟 ** 將它展開就可以了
def pass_all_the_args(*args, **kwargs):
    all_the_args(*args, **kwargs)
    print varargs(*args)
    print keyword_args(**kwargs)

# 函式範圍
x = 5

def set_x(num):
    # 區域變數 x 和全域變數 x 不是同一個東西
    x = num # => 43
    print x # => 43

def set_global_x(num):
    global x
    print x # => 5
    x = num # 全域變數 x 在set_global_x(6)被設定為 6 
    print x # => 6

set_x(43)
set_global_x(6)

# Python有一級函式
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# 也有匿名函式
(lambda x: x > 2)(3)   # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1) # => 5

# 還有內建的高階函式
map(add_10, [1, 2, 3])   # => [11, 12, 13]
map(max, [1, 2, 3], [4, 2, 1])   # => [4, 2, 3]

filter(lambda x: x > 5, [3, 4, 5, 6, 7])   # => [6, 7]

# 我們可以用List列表的方式對map和filter等高階函式做更有趣的應用
[add_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]   # => [6, 7]


####################################################
## 5. 類別
####################################################

# 我們可以由object繼承出一個新的類別
class Human(object):

    # 類別的參數，被所有這個類別的實體所共用
    species = "H. sapiens"

    # 基礎建構函式，當class被實體化的時候會被呼叫
    # 注意前後的雙底線
    # 代表此物件或屬性雖然在使用者控制的命名空間內，但是被python使用
    def __init__(self, name):
        # 將函式引入的參數 name 指定給實體的 name 參數
        self.name = name

        # 初始化屬性
        self.age = 0


    # 一個實體的方法(method)。 所有的method都以self為第一個參數
    def say(self, msg):
        return "{0}: {1}".format(self.name, msg)

    # 一個類別方法會被所有的實體所共用
    # 他們會以類別為第一參數的方式被呼叫
    @classmethod
    def get_species(cls):
        return cls.species

    # 靜態方法
    @staticmethod
    def grunt():
        return "*grunt*"

    # 屬性就像是用getter取值一樣
    # 它將方法 age() 轉為同名的、只能讀取的屬性
    @property
    def age(self):
        return self._age

    # 這樣寫的話可以讓屬性被寫入新的值
    @age.setter
    def age(self, age):
        self._age = age

    # 這樣寫的話允許屬性被刪除
    @age.deleter
    def age(self):
        del self._age


# 將類別實體化
i = Human(name="Ian")
print i.say("hi")     # prints out "Ian: hi"

j = Human("Joel")
print j.say("hello")  # prints out "Joel: hello"

# 呼叫類別方法
i.get_species()   # => "H. sapiens"

# 更改共用的屬性
Human.species = "H. neanderthalensis"
i.get_species()   # => "H. neanderthalensis"
j.get_species()   # => "H. neanderthalensis"

# 呼叫靜態方法
Human.grunt()   # => "*grunt*"

# 更新屬性
i.age = 42

# 取得屬性
i.age # => 42

# 移除屬性
del i.age
i.age  # => raises an AttributeError


####################################################
## 6. 模組
####################################################

# 你可以引入模組來做使用
import math
print math.sqrt(16)  # => 4
                     # math.sqrt()為取根號

# 你可以只從模組取出特定幾個函式
from math import ceil, floor
print ceil(3.7)  # => 4.0
print floor(3.7)   # => 3.0

# 你可以將所有的函式從模組中引入
# 注意：不建議這麼做
from math import *

# 你可以用 as 簡寫模組名稱
import math as m
math.sqrt(16) == m.sqrt(16)   # => True
# 你也可以測試函示是否相等
from math import sqrt
math.sqrt == m.sqrt == sqrt  # => True

# Python的模組就只是一般的Python檔。
# 你可以自己的模組自己寫、自己的模組自己引入
# 模組的名稱和檔案名稱一樣

# 你可以用dir()來查看有哪些可用函式和屬性
import math
dir(math)


####################################################
## 7. 進階
####################################################

# 產生器(Generator)可以讓你寫更懶惰的程式碼
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# 產生器可以讓你即時的產生值
# 不是全部產生完之後再一次回傳，產生器會在每一個遞迴時
# 產生值。 這也意味著大於15的值不會在double_numbers中產生。
# 這邊，xrange()做的事情和range()一樣
# 建立一個 1-900000000 的List會消耗很多時間和記憶體空間
# xrange() 建立一個產生器物件，而不是如range()建立整個List
# 我們用底線來避免可能和python的關鍵字重複的名稱
xrange_ = xrange(1, 900000000)

# 下面的程式碼會把所有的值乘以兩倍，直到出現大於30的值
for i in double_numbers(xrange_):
    print i
    if i >= 30:
        break


# 裝飾子
# 在這個範例中，beg會綁在say上
# Beg會呼叫say。 如果say_please為True的話，它會更改回傳的訊息
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

## 準備好學更多了嗎?

### 線上免費資源

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [The Official Docs](http://docs.python.org/2/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Module of the Week](http://pymotw.com/2/)
* [A Crash Course in Python for Scientists](http://nbviewer.ipython.org/5920182)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)

### 或買本書?

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)
