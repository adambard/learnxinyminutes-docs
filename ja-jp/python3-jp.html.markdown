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
filename: learnpython3-jp.py
lang: ja-jp
---

90年代の初め、Guido Van RossumによってPythonは作成されました。現在となっては、最も有名な言語の1つです。
私は構文の明快さによって、Pythonと恋に落ちました。
以下は基本的に実行可能な疑似コードです。

フィードバッグは大歓迎です! [@louiedinh](http://twitter.com/louiedinh) または louiedinh [at] [google's email service] にご連絡下さい!

Note: この記事はPython 3に内容を絞っています。もし古いPython 2.7を学習したいなら、 [こちら](http://learnxinyminutes.com/docs/python/) をご確認下さい。

```python

# 1行のコメントは番号記号(#)から始まります。

""" 複数行の文字は、"を3つ繋げることで
    書くことができます。
    また、これはコメントとしてもよく使われます。
"""

####################################################
# 1. プリミティブ型と演算子
####################################################

# 数字です
3                               # => 3

# 四則演算はあなたの期待通りに動きます。
1 + 1                           # => 2
8 - 1                           # => 7
10 * 2                          # => 20
35 / 5                          # => 7.0

# 整数除算の結果は、正負に関わらず小数の切り捨てが行われます。
5 // 3                          # => 1
5.0 // 3.0                      # => 1.0 # 浮動小数点でも同様に動作します。
-5 // 3                         # => -2
-5.0 // 3.0                     # => -2.0

# 除算の結果は常に浮動小数点になります。
10.0 / 3                        # => 3.3333333333333335

# 剰余の計算
7 % 3                           # => 1

# 冪乗 (x**y, x の y 乗)
2**4                            # => 16

# 括弧により、計算の順番を優先させられます。
(1 + 3) * 2                     # => 8

# 真偽値はプリミティブ型です(大文字から始まっていることに注意!)
True
False

# not で真偽を反転させられます。
not True                        # => False
not False                       # => True

# ブール演算
# 注意: "and" と "or" は小文字です
True and False                  # => False
False or True                   # => True

# 整数でブール演算をするときのメモ
0 and 2                         # => 0
-5 or 0                         # => -5
0 == False                      # => True
2 == True                       # => False
1 == True                       # => True

# 値が等しいか確認するには ==
1 == 1                          # => True
2 == 1                          # => False

# 値が等しくないか確認するには !=
1 != 1                          # => False
2 != 1                          # => True

# 他の比較方法
1 < 10                          # => True
1 > 10                          # => False
2 <= 2                          # => True
2 >= 2                          # => True

# 比較は連結させられます!
1 < 2 < 3                       # => True
2 < 3 < 2                       # => False

# (is vs. ==)
# "is" は、2つの変数が同一のオブジェクトを参照しているか確認します。
# 一方 "==" は、それぞれが参照する2つのオブジェクトが同じ値を持つか確認します。
a = [1, 2, 3, 4]     # a は新しいリストの [1, 2, 3, 4] を指します。
b = a                # b は a が指すリストを指します。
b is a               # => True, a と b は同一のオブジェクトを参照しています。
b == a               # => True, a と b が参照するオブジェクトの値は等しいです。
b = [1, 2, 3, 4]     # b は新しいリストの [1, 2, 3, 4] を指します。
b is a               # => False, a と b は別々のオブジェクトを参照しています。
b == a               # => True, a と b が参照するオブジェクトの値は等しいです。

# " または ' を使って文字列を作成します。
"This is a string."
'This is also a string.'

# 文字列も加算をすることができます!でも、あまり行わないように。
"Hello " + "world!"             # => "Hello world!"
# '+' を使わなくても連結はできます。
"Hello " "world!"               # => "Hello world!"

# 文字列は文字のリストであるかのように扱うことができます。
"This is a string"[0]           # => 'T'

# 文字列の長さを得るにはこのようにします。
len("This is a string")         # => 16

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
None                            # => None

# オブジェクトがNoneであるか確認するのに "==" 演算子を使わないように。
# 代わりに "is" を使いましょう。オブジェクトの素性を確認できます。
"etc" is None                   # => False
None is None                    # => True

# None や 0 、空の 文字列/リスト/辞書/タプル は全て False として評価されます。
# 他の全ての値は True になります。
bool(0)                         # => False
bool("")                        # => False
bool([])                        # => False
bool({})                        # => False
bool(())                        # => False

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
some_var                        # => 5

# 代入されていない変数へのアクセスは例外を引き起こします。
# 例外の取り扱いについては、3章の制御の流れをご確認ください。
some_unknown_var                # NameError を送出します

# ifは式として使用できます。
# C言語の「?:(三項演算子)」に対応する例:
"yahoo!" if 3 > 2 else 2        # => "yahoo!"

# リストは順序を保存します。
li = []
# 値の入っているリストも作成できます。
other_li = [4, 5, 6]

# append により、リストの末尾にものを入れられます。
li.append(1)                    # li is now [1]
li.append(2)                    # li is now [1, 2]
li.append(4)                    # li is now [1, 2, 4]
li.append(3)                    # li is now [1, 2, 4, 3]
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
# 3. 制御の流れとiterable
####################################################

# まずは変数を作りましょう。
some_var = 5

# これはif文です。インデントがPythonでは特徴的ですね!
# 以下の例では"some_var is smaller than 10"と出力されます。
if some_var > 10:
    print("some_var is totally bigger than 10.")
elif some_var < 10:             # この elif 節はオプションです。
    print("some_var is smaller than 10.")
else:                           # この else 節もオプションです。
    print("some_var is indeed 10.")


"""
for ループはリストの要素を反復することができます。
出力:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # format() を使って文字列に変数を挿入して出力できます。
    print("{} is a mammal".format(animal))

"""
"range(数値)" は、ゼロから与えられた数値までのiterableを返します。
出力:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
"range(lower, upper)" は、 lower の数値から upper の数値までのiterableを返します。
upper の数値は含まれません。
出力:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
"range(lower, upper, step)" は、lower の数値から upper の数値までが、
step 刻みで表現されるiterableを返します
step が与えられない場合、デフォルトは1になります。
出力:
    4
    6
"""
for i in range(4, 8, 2):
    print(i)
"""

while によるループは条件が成立しなくなるまで実行されます。
出力:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1                      # x = x + 1 の省略記法

# try/except ブロックにより、例外を扱う
try:
    # "raise" により例外を発生させます。
    raise IndexError("This is an index error")
except IndexError as e:
    pass  # pass は、何もしないという命令(no-op)に相当します。普通、ここで例外に対処します。
except (TypeError, NameError):
    pass                 # もし必要なら、複数の種類の例外を一緒に処理できます。
else:  # try/except ブロックへのオプションの節。他の全てのexceptブロックより後に置かなければなりません。
    print("All good!")        # tryで例外が発生しなかった場合のみ実行されます。
finally:  # 例外が発生したか、しなかったか、どのような例外だったかに関らず実行されます。
    print("We can clean up resources here")

# try/finallyでリソースの始末をする代わりに、 with 文を使うこともできます。
with open("myfile.txt") as f:
    for line in f:
        print(line)

# Pythonは、iterableと呼ばれる基本的な抽象化が提供しています。
# iterableは、シーケンスとして取り扱えるオブジェクトです。
# range関数で返されるオブジェクトもiterableの一種です。
filled_dict = {"one": 1, "two": 2, "three": 3}
our_iterable = filled_dict.keys()
print(our_iterable)  # => dict_keys(['one', 'two', 'three']). これはiterableインタフェースを実装するオブジェクトです。

# iterableでループを行うことができます。
for i in our_iterable:
    print(i)                    # Prints one, two, three

# しかし、インデックスで要素を参照することはできません。
our_iterable[1]                 # TypeError が発生します。

# iterableは、iteratorの作り方がわかるオブジェクトです。
our_iterator = iter(our_iterable)

# iterator は、要素を取り出したときの状態を覚えるオブジェクトです。
# "next()"により次の要素を取り出せます。
next(our_iterator)              # => "one"

# 反復(iterate)する度に、状態を更新します。
next(our_iterator)              # => "two"
next(our_iterator)              # => "three"

# iteratorが自身の持つ全てのデータを返したあとは、 StopIteration 例外を発生させます。
next(our_iterator)              # StopIteration が発生します。

# "list()"を呼ぶことにより、iteratorの全ての要素を得られます。
list(filled_dict.keys())        # => ["one", "two", "three"]


####################################################
# 4. 関数
####################################################

# 新しい関数を作成するには "def" を使います。
def add(x, y):
    print("x is {} and y is {}".format(x, y))
    return x + y                # return 文で値を返します。

# 引数付きで関数を呼んでみましょう。
add(5, 6)                    # => "x is 5 and y is 6" と出力し、 11 を返します。

# キーワード引数で関数を呼ぶこともできます。
add(y=6, x=5)                   # キーワード引数を使うと任意の順番で引数を指定できます。


# 可変数の位置引数を持つ関数を定義できます。
def varargs(*args):
    return args

varargs(1, 2, 3)  # => (1, 2, 3)


# 可変数のキーワード引数を持つ関数を定義できます。
def keyword_args(**kwargs):
    return kwargs

# 何が起こるか、試してみましょう
keyword_args(big="foot", loch="ness")  # => {"big": "foot", "loch": "ness"}


# お望みなら、両方一気にやることもできます。
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) prints:
    (1, 2)
    {"a": 3, "b": 4}
"""

# 関数を呼ぶとき、 args/kwargs の逆のことをすることができます!
# * を使ってタプルを展開したり、 ** を使って辞書を展開できます。
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)             # foo(1, 2, 3, 4) に対応します。
all_the_args(**kwargs)          # foo(a=3, b=4) に対応します。
all_the_args(*args, **kwargs)   # foo(1, 2, 3, 4, a=3, b=4) に対応します。


# タプルで複数の値を返す
def swap(x, y):    # 括弧を使わずに、複数の値をタプルとして返すことができます。
    return y, x    # (Note: 括弧は使わなくてもいいですが、使うこともできます。)


x = 1
y = 2
x, y = swap(x, y)               # => x = 2, y = 1
# (x, y) = swap(x,y)  # このように、括弧は使っても使わなくてもいいです。


# 関数のスコープ
x = 5


def set_x(num):
    # ローカル変数の x はグローバル変数の x とは異なります
    x = num                     # => 43
    print(x)                    # => 43


def set_global_x(num):
    global x
    print(x)                    # => 5
    x = num                     # グローバル変数の x に 6 が代入されました。
    print(x)                    # => 6

set_x(43)
set_global_x(6)


# Pythonは第一級関数をサポートします。
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)                       # => 13

# 無名関数もサポートしています。
(lambda x: x > 2)(3)                 # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# 高階関数も組込まれています。
list(map(add_10, [1, 2, 3]))         # => [11, 12, 13]
list(map(max, [1, 2, 3], [4, 2, 1]))  # => [4, 2, 3]

list(filter(lambda x: x > 5, [3, 4, 5, 6, 7]))  # => [6, 7]

# map や filter の代わりに、リスト内包表記を使うことができます。
# リスト内包表記は、出力を別のリスト内包表記にネストさせることができます。
[add_10(i) for i in [1, 2, 3]]        # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]  # => [6, 7]

# 集合(set)や辞書も内包表記ができます。
{x for x in 'abcddeef' if x not in 'abc'}  # => {'d', 'e', 'f'}
{x: x**2 for x in range(5)}                # => {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}


####################################################
# 5. モジュール
####################################################

# Pythonではモジュールをインポートできます。
import math
print(math.sqrt(16))            # => 4.0

# モジュールの中から特定の関数をインポートすることもできます。
from math import ceil, floor
print(ceil(3.7))                # => 4.0
print(floor(3.7))               # => 3.0

# 全部の関数をモジュールからインポートすることができます。
# Warning: この方法は推奨されません。
from math import *

# 短い名前でモジュールをインポートすることができます。
import math as m
math.sqrt(16) == m.sqrt(16)     # => True

# Pythonのモジュールは実際には単なるPythonのファイルです。
# 自分で書くことも、インポートすることもできます。
# ファイル名がそのままモジュール名になります。

# モジュールで定義されている関数と属性を調べることができます。
import math
dir(math)

# もし、現在書いているスクリプトと同じフォルダに「math.py」という
# Pythonのスクリプトが存在する場合、そのmath.pyが
# 組み込みのPythonモジュールの代わりに読み込まれるでしょう。
# これは、ローカルのフォルダはPythonの組み込みライブラリよりも
# 優先度が高いため発生するのです。


####################################################
# 6. クラス
####################################################

# クラスを作成するために、"class"という演算子を使います。
class Human:

    # クラスの属性です。このクラスの全てのインスタンスで共有されます。
    species = "H. sapiens"

    # 標準的なイニシャライザで、このクラスがインスタンスを作成するときは毎回呼ばれます。
    # 2つのアンダースコアがオブジェクトや属性の前後についているとき、これらはPythonによって利用され、
    # ユーザーの名前空間には存在しないということに注意してください。
    # __init__ や __str__ 、 __repr__ のようなメソッド(やオブジェクト、属性)は、
    # magic methods (または dunder methods)と呼ばれます。
    # このような名前を自分で発明しないほうがよいでしょう。
    def __init__(self, name):
        # 引数をインスタンスのname属性に設定します。
        self.name = name

        # プロパティの初期化
        self.age = 0

    # インスタンスメソッド。全てのメソッドは"self"を最初の引数に取ります。
    def say(self, msg):
        print("{name}: {message}".format(name=self.name, message=msg))

    # 別のインスタンスメソッドの例。
    def sing(self):
        return 'yo... yo... microphone check... one two... one two...'

    # クラスメソッドは全てのインスタンスで共有されます。
    # クラスメソッドではクラスを最初の引数として呼ばれます。
    @classmethod
    def get_species(cls):
        return cls.species

    # スタティックメソッドはクラスやインスタンスを参照せずに呼ばれます。
    @staticmethod
    def grunt():
        return "*grunt*"

    # プロパティはgetterのようなものです。
    # age() メソッドを同名の読取専用属性に変換します。
    @property
    def age(self):
        return self._age

    # プロパティを設定できるようにします。
    @age.setter
    def age(self, age):
        self._age = age

    # プロパティを削除できるようにします。
    @age.deleter
    def age(self):
        del self._age


# Pythonインタプリタがソースファイルを読み込んだとき、全てのコードを実行します。
# この __name__ による確認により、このモジュールがメインのプログラムである場合にのみ、
# このコードブロックが実行されるようにします。
if __name__ == '__main__':
    # クラスのインスタンスを作成します。
    i = Human(name="Ian")
    i.say("hi")                 # "Ian: hi"
    j = Human("Joel")
    j.say("hello")              # "Joel: hello"
    # i と j はHumanのインスタンスです。別の言葉で言うなら、これらはHumanのオブジェクトです。

    # クラスメソッドを呼んでみましょう。
    i.say(i.get_species())      # "Ian: H. sapiens"
    # 共有属性を変更してみましょう。
    Human.species = "H. neanderthalensis"
    i.say(i.get_species())      # => "Ian: H. neanderthalensis"
    j.say(j.get_species())      # => "Joel: H. neanderthalensis"

    # スタティックメソッドを呼んでみましょう。
    print(Human.grunt())            # => "*grunt*"
    print(i.grunt())                # => "*grunt*"

    # インスタンスのプロパティを更新してみましょう。
    i.age = 42
    # プロパティを取得してみましょう。
    i.say(i.age)                    # => 42
    j.say(j.age)                    # => 0
    # プロパティを削除してみましょう。
    del i.age
    # i.age                         # => AttributeError が発生します。


####################################################
# 6.1 多重継承
####################################################

# 別のクラスを定義します。
class Bat:

    species = 'Baty'

    def __init__(self, can_fly=True):
        self.fly = can_fly

    # このクラスも say メソッドを持ちます。
    def say(self, msg):
        msg = '... ... ...'
        return msg

    # 同様に、独自のメソッドも与えましょう。
    def sonar(self):
        return '))) ... ((('

if __name__ == '__main__':
    b = Bat()
    print(b.say('hello'))
    print(b.fly)

# ファイル単位のモジュール化を利用するために、上記のクラスを別々のファイルに配置することができます。
# ここでは、human.pyとbat.pyを作成してみましょう。

# 他のファイルから関数をインポートするために、次のような形式を利用してください。
# from "拡張子無しのファイル名" import "関数またはクラス"

# superhero.py
from human import Human
from bat import Bat


# BatmanはHumanとBatの両方を継承します。
class Batman(Human, Bat):

    # Batmanは species のクラス属性に独自の値を持ちます。
    species = 'Superhero'

    def __init__(self, *args, **kwargs):
        # 通常、属性を継承するにはsuper()を呼び出します。
        #     super(Batman, self).__init__(*args, **kwargs)
        # しかし、ここでは多重継承を行っているので、 super() はMRO(メソッド解決順序)の次の基本クラスにのみ動作します。
        # なので、全ての祖先に対して明示的に __init__ を呼ぶことにします。
        # *args と **kwargs を使うことで、それぞれの継承元が
        # たまねぎの皮を剥がすごとく、引数を用いることができます。
        Human.__init__(self, 'anonymous', *args, **kwargs)
        Bat.__init__(self, *args, can_fly=False, **kwargs)
        # 名前の属性の値を上書きします。
        self.name = 'Sad Affleck'

    def sing(self):
        return 'nan nan nan nan nan batman!'


if __name__ == '__main__':
    sup = Batman()

    # インスタンスの型を調べてみましょう。
    if isinstance(sup, Human):
        print('I am human')
    if isinstance(sup, Bat):
        print('I am bat')
    if type(sup) is Batman:
        print('I am Batman')

    # getattr() や super() の両方で使われるMROを取得します。
    # この属性は動的であり、更新が可能です。
    print(Batman.__mro__)  # => (<class '__main__.Batman'>, <class 'human.Human'>, <class 'bat.Bat'>, <class 'object'>)

    # 親メソッドを呼び出しますが、独自のクラス属性を参照します。
    print(sup.get_species())    # => Superhero

    # オーバーロードされたメソッドを呼び出します。
    print(sup.sing())           # => nan nan nan nan nan batman!

    # 継承順により、Humanから継承されたメソッドを呼び出します。
    sup.say('I agree')          # => Sad Affleck: I agree

    # 2番目の先祖にのみ存在するメソッドを呼び出してみます。
    print(sup.sonar())          # => ))) ... (((

    # 継承されたクラス属性
    sup.age = 100
    print(sup.age)

    # デフォルト値が上書きされて、2番目の先祖から継承された属性
    print('Can I fly? ' + str(sup.fly))


####################################################
# 7. 発展的内容
####################################################

# ジェネレータは遅延をするコードの作成に役立ちます。
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# 次の値を処理するのに必要なデータしか読み込まないので、ジェネレータはメモリをあまり消費しません。
# この性質により、他の方法では非常に多くのメモリを消費するような操作が可能になります。
for i in double_numbers(range(1, 900000000)):  # `range` もジェネレータの1つです。
    print(i)
    if i >= 30:
        break

# リスト内包表記のように、ジェネータ内包表記を作成することもできます。
values = (-x for x in [1, 2, 3, 4, 5])
for x in values:
    print(x)                    # prints -1 -2 -3 -4 -5

# ジェネレータ内包表記から直接リストを作成することもできます。
values = (-x for x in [1, 2, 3, 4, 5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]

# デコレータ
# この例では`beg` が `say` を `wraps`します。
# もし say_please が True なら、出力が変更されます。
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
