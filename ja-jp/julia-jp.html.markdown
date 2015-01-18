---
language: Julia
contributors:
    - ["Leah Hanson", "http://leahhanson.us"]
translators:
    - ["Yuichi Motoyama", "https://github.com/yomichi"]
filename: learnjulia-jp.jl
lang: ja-jp
---

Julia は科学技術計算向けに作られた、同図像性を持った(homoiconic) プログラミング言語です。
マクロによる同図像性や第一級関数などの抽象化機能の恩恵を受けつつ、低階層をも扱えますが、
それでいてPython 並に学習しやすく、使いやすい言語となっています。

この文章は、Julia の2013年10月18日現在の開発バージョンを元にしています。

```ruby

# ハッシュ（シャープ）記号から改行までは単一行コメントとなります。
#= 複数行コメントは、
   '#=' と '=#' とで囲むことで行えます。
   #= 
   入れ子構造にすることもできます。
   =#
=#

####################################################
## 1. 基本的な型と演算子
####################################################

# Julia ではすべて式となります。

# 基本となる数値型がいくつかあります。
3 # => 3 (Int64)
3.2 # => 3.2 (Float64)
2 + 1im # => 2 + 1im (Complex{Int64})
2//3 # => 2//3 (Rational{Int64})

# 一般的な中置演算子が使用可能です。
1 + 1 # => 2
8 - 1 # => 7
10 * 2 # => 20
35 / 5 # => 7.0
5 / 2 # => 2.5 # 整数型同士の割り算の結果は、浮動小数点数型になります
div(5, 2) # => 2 # 整数のまま割り算するには、 div を使います
5 \ 35 # => 7.0
2 ^ 2 # => 4 # べき乗です。排他的論理和ではありません
12 % 10 # => 2

# 丸括弧で演算の優先順位をコントロールできます
(1 + 3) * 2 # => 8

# ビット演算
~2 # => -3   # ビット反転
3 & 5 # => 1 # ビット積
2 | 4 # => 6 # ビット和
2 $ 4 # => 6 # ビット排他的論理和
2 >>> 1 # => 1 # 右論理シフト
2 >> 1  # => 1 # 右算術シフト
2 << 1  # => 4 # 左シフト

# bits 関数を使うことで、数の二進表現を得られます。
bits(12345)
# => "0000000000000000000000000000000000000000000000000011000000111001"
bits(12345.0)
# => "0100000011001000000111001000000000000000000000000000000000000000"

# ブール値が用意されています
true
false

# ブール代数
!true # => false
!false # => true
1 == 1 # => true
2 == 1 # => false
1 != 1 # => false
2 != 1 # => true
1 < 10 # => true
1 > 10 # => false
2 <= 2 # => true
2 >= 2 # => true
# 比較演算子をつなげることもできます
1 < 2 < 3 # => true
2 < 3 < 2 # => false

# 文字列は " で作れます
"This is a string."

# 文字リテラルは ' で作れます
'a'

# 文字列は文字の配列のように添字アクセスできます
"This is a string"[1] # => 'T' # Julia では添字は 1 から始まります
# ただし、UTF8 文字列の場合は添字アクセスではうまくいかないので、
# イテレーションを行ってください(map 関数や for ループなど)

# $ を使うことで、文字列に変数や、任意の式を埋め込めます。
"2 + 2 = $(2 + 2)" # => "2 + 2 = 4"

# 他にも、printf マクロを使うことでも変数を埋め込めます。
@printf "%d is less than %f" 4.5 5.3 # 5 is less than 5.300000

# 出力も簡単です
println("I'm Julia. Nice to meet you!")

####################################################
## 2. 変数と配列、タプル、集合、辞書
####################################################

# 変数の宣言は不要で、いきなり変数に値を代入・束縛できます。
some_var = 5 # => 5
some_var # => 5

# 値に束縛されていない変数を使おうとするとエラーになります。
try
    some_other_var # => ERROR: some_other_var not defined
catch e
    println(e)
end

# 変数名は数字や記号以外の文字から始めます。
# その後は、数字やアンダースコア(_), 感嘆符(!)も使えます。
SomeOtherVar123! = 6 # => 6

# Unicode 文字も使えます。
☃ = 8 # => 8
# ギリシャ文字などを使うことで数学的な記法が簡単にかけます。
2 * π # => 6.283185307179586

# Julia における命名習慣について:
#
# * 変数名における単語の区切りにはアンダースコアを使っても良いですが、
#   使わないと読みにくくなる、というわけではない限り、
#   推奨はされません。
#
# * 型名は大文字で始め、単語の区切りにはキャメルケースを使います。
#
# * 関数やマクロの名前は小文字で書きます。
#   単語の分かち書きにはアンダースコアをつかわず、直接つなげます。
#
# * 内部で引数を変更する関数は、名前の最後に ! をつけます。
#   この手の関数は、しばしば「破壊的な関数」とか「in-place な関数」とか呼ばれます。


# 配列は、1 から始まる整数によって添字付けられる、値の列です。
a = Int64[] # => 0-element Int64 Array

# 一次元配列（列ベクトル）は、角括弧 [] のなかにカンマ , 区切りで値を並べることで作ります。
b = [4, 5, 6] # => 3-element Int64 Array: [4, 5, 6]
b[1] # => 4
b[end] # => 6

# 二次元配列は、空白区切りで作った行を、セミコロンで区切ることで作ります。
matrix = [1 2; 3 4] # => 2x2 Int64 Array: [1 2; 3 4]

# 配列の末尾に値を追加するには push! を、
# 他の配列を結合するには append! を使います。
push!(a,1)     # => [1]
push!(a,2)     # => [1,2]
push!(a,4)     # => [1,2,4]
push!(a,3)     # => [1,2,4,3]
append!(a,b) # => [1,2,4,3,4,5,6]

# 配列の末尾から値を削除するには pop! を使います。
pop!(b)        # => 6 and b is now [4,5]

# 一旦元に戻しておきましょう。
push!(b,6)   # b is now [4,5,6] again.

a[1] # => 1 # Julia では添字は0 ではなく1 から始まること、お忘れなく!

# end は最後の添字を表す速記法です。
# 添字を書く場所ならどこにでも使えます。
a[end] # => 6

# 先頭に対する削除・追加は shift!, unshift! です。
shift!(a) # => 1 and a is now [2,4,3,4,5,6]
unshift!(a,7) # => [7,2,4,3,4,5,6]

# ! で終わる関数名は、その引数を変更するということを示します。
arr = [5,4,6] # => 3-element Int64 Array: [5,4,6]
sort(arr) # => [4,5,6]; arr is still [5,4,6]
sort!(arr) # => [4,5,6]; arr is now [4,5,6]

# 配列の範囲外アクセスをすると BoundsError が発生します。
try
    a[0] # => ERROR: BoundsError() in getindex at array.jl:270
    a[end+1] # => ERROR: BoundsError() in getindex at array.jl:270
catch e
    println(e)
end

# エラーが発生すると、どのファイルのどの行で発生したかが表示されます。
# 標準ライブラリで発生したものでもファイル名と行数が出ます。
# ソースからビルドした場合など、標準ライブラリのソースが手元にある場合は
# base/ ディレクトリから探し出して見てください。

# 配列は範囲オブジェクトから作ることもできます。
a = [1:5] # => 5-element Int64 Array: [1,2,3,4,5]

# 添字として範囲オブジェクトを渡すことで、
# 配列の部分列を得ることもできます。
a[1:3] # => [1, 2, 3]
a[2:end] # => [2, 3, 4, 5]

# 添字を用いて配列から値の削除をしたい場合は、splice! を使います。
arr = [3,4,5]
splice!(arr,2) # => 4 ; arr is now [3,5]

# 配列の結合は append! です。
b = [1,2,3]
append!(a,b) # Now a is [1, 2, 3, 4, 5, 1, 2, 3]

# 配列内に指定した値があるかどうかを調べるのには in を使います。
in(1, a) # => true

# length で配列の長さを取得できます。
length(a) # => 8

# 変更不可能 (immutable) な値の組として、タプルが使えます。
tup = (1, 2, 3) # => (1,2,3) # an (Int64,Int64,Int64) tuple.
tup[1] # => 1
try:
    tup[1] = 3 # => ERROR: no method setindex!((Int64,Int64,Int64),Int64,Int64)
catch e
    println(e)
end

# 配列に関する関数の多くが、タプルでも使えます。
length(tup) # => 3
tup[1:2] # => (1,2)
in(2, tup) # => true

# タプルから値をばらして(unpack して) 複数の変数に代入できます。
a, b, c = (1, 2, 3) # => (1,2,3)  # a is now 1, b is now 2 and c is now 3

# 丸括弧なしでもタプルになります。
d, e, f = 4, 5, 6 # => (4,5,6)

# ひとつの値だけからなるタプルは、その値自体とは区別されます。
(1,) == 1 # => false
(1) == 1 # => true

# 値の交換もタプルを使えば簡単です。
e, d = d, e  # => (5,4) # d is now 5 and e is now 4


# 辞書 (Dict) は、値から値への変換の集合です。
empty_dict = Dict() # => Dict{Any,Any}()

# 辞書型リテラルは次のとおりです。
filled_dict = ["one"=> 1, "two"=> 2, "three"=> 3]
# => Dict{ASCIIString,Int64}

# [] を使ったアクセスができます。
filled_dict["one"] # => 1

# すべての鍵（添字）は keys で得られます。
keys(filled_dict)
# => KeyIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# 必ずしも辞書に追加した順番には並んでいないことに注意してください。

# 同様に、values はすべての値を返します。
values(filled_dict)
# => ValueIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# 鍵と同様に、必ずしも辞書に追加した順番には並んでいないことに注意してください。

# in や haskey を使うことで、要素や鍵が辞書の中にあるかを調べられます。
in(("one", 1), filled_dict) # => true
in(("two", 3), filled_dict) # => false
haskey(filled_dict, "one") # => true
haskey(filled_dict, 1) # => false

# 存在しない鍵を問い合わせると、エラーが発生します。
try
    filled_dict["four"] # => ERROR: key not found: four in getindex at dict.jl:489
catch e
    println(e)
end

# get 関数を使い、鍵がなかった場合のデフォルト値を与えておくことで、
# このエラーを回避できます。
get(filled_dict,"one",4) # => 1
get(filled_dict,"four",4) # => 4

# 集合 (Set) は一意な値の、順序付けられていない集まりです。
empty_set = Set() # => Set{Any}()
# 集合の初期化
filled_set = Set(1,2,2,3,4) # => Set{Int64}(1,2,3,4)

# 集合への追加
push!(filled_set,5) # => Set{Int64}(5,4,2,3,1)

# in で、値が既に存在するかを調べられます。
in(2, filled_set) # => true
in(10, filled_set) # => false

# 積集合や和集合、差集合を得る関数も用意されています。
other_set = Set(3, 4, 5, 6) # => Set{Int64}(6,4,5,3)
intersect(filled_set, other_set) # => Set{Int64}(3,4,5)
union(filled_set, other_set) # => Set{Int64}(1,2,3,4,5,6)
setdiff(Set(1,2,3,4),Set(2,3,5)) # => Set{Int64}(1,4)


####################################################
## 3. 制御構文
####################################################

# まずは変数を作ります。
some_var = 5

# if 構文です。Julia ではインデントに意味はありません。
if some_var > 10
    println("some_var is totally bigger than 10.")
elseif some_var < 10    # elseif 節は省略可能です。
    println("some_var is smaller than 10.")
else                    # else 節も省略可能です。
    println("some_var is indeed 10.")
end
# => "some var is smaller than 10" と出力されます。

# for ループによって、反復可能なオブジェクトを走査できます。
# 反復可能なオブジェクトの型として、
# Range, Array, Set, Dict, String などがあります。
for animal=["dog", "cat", "mouse"]
    println("$animal is a mammal")
    # $ を使うことで文字列に変数の値を埋め込めます。
    # You can use $ to interpolate variables or expression into strings
end
# prints:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

# for = の代わりに for in を使うこともできます
for animal in ["dog", "cat", "mouse"]
    println("$animal is a mammal")
end
# prints:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

# 辞書ではタプルが返ってきます。
for a in ["dog"=>"mammal","cat"=>"mammal","mouse"=>"mammal"]
    println("$(a[1]) is a $(a[2])")
end
# prints:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

# タプルのアンパック代入もできます。
for (k,v) in ["dog"=>"mammal","cat"=>"mammal","mouse"=>"mammal"]
    println("$k is a $v")
end
# prints:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

# while ループは、条件式がtrue となる限り実行され続けます。
x = 0
while x < 4
    println(x)
    x += 1  # Shorthand for x = x + 1
end
# prints:
#   0
#   1
#   2
#   3

# 例外は try/catch で捕捉できます。
try
   error("help")
catch e
   println("caught it $e")
end
# => caught it ErrorException("help")


####################################################
## 4. 関数
####################################################

# function キーワードを次のように使うことで、新しい関数を定義できます。
#function name(arglist)
#  body...
#end
function add(x, y)
    println("x is $x and y is $y")

    # 最後に評価された式の値が、関数全体の返り値となります。
    x + y
end

add(5, 6) # => 11 after printing out "x is 5 and y is 6"

# 可変長引数関数も定義できます。
function varargs(args...)
    return args
    # return キーワードを使うことで、好きな位置で関数から抜けられます。
end
# => varargs (generic function with 1 method)

varargs(1,2,3) # => (1,2,3)

# ... はsplat と呼ばれます
# （訳注：「ピシャッという音（名詞）」「衝撃で平らにする（動詞）」）
# 今回は関数定義で使いましたが、関数呼び出しに使うこともできます。
# その場合、配列やタプルの要素を開いて、複数の引数へと割り当てることとなります。
Set([1,2,3])    # => Set{Array{Int64,1}}([1,2,3]) # 「整数の配列」の集合
Set([1,2,3]...) # => Set{Int64}(1,2,3) # 整数の集合

x = (1,2,3)     # => (1,2,3)
Set(x)          # => Set{(Int64,Int64,Int64)}((1,2,3)) # タプルの集合
Set(x...)       # => Set{Int64}(2,3,1)


# 引数に初期値を与えることで、オプション引数をもった関数を定義できます。
function defaults(a,b,x=5,y=6)
    return "$a $b and $x $y"
end

defaults('h','g') # => "h g and 5 6"
defaults('h','g','j') # => "h g and j 6"
defaults('h','g','j','k') # => "h g and j k"
try
    defaults('h') # => ERROR: no method defaults(Char,)
    defaults() # => ERROR: no methods defaults()
catch e
    println(e)
end

# キーワード引数を持った関数も作れます。
function keyword_args(;k1=4,name2="hello") # ; が必要なことに注意
    return ["k1"=>k1,"name2"=>name2]
end

keyword_args(name2="ness") # => ["name2"=>"ness","k1"=>4]
keyword_args(k1="mine") # => ["k1"=>"mine","name2"=>"hello"]
keyword_args() # => ["name2"=>"hello","k1"=>4]

# もちろん、これらを組み合わせることもできます。
function all_the_args(normal_arg, optional_positional_arg=2; keyword_arg="foo")
    println("normal arg: $normal_arg")
    println("optional arg: $optional_positional_arg")
    println("keyword arg: $keyword_arg")
end

all_the_args(1, 3, keyword_arg=4)
# prints:
#   normal arg: 1
#   optional arg: 3
#   keyword arg: 4

# Julia では関数は第一級関数として、値として扱われます。
function create_adder(x)
    adder = function (y)
        return x + y
    end
    return adder
end

# ラムダ式によって無名関数をつくれます。
(x -> x > 2)(3) # => true

# 先ほどの create_adder と同じもの
function create_adder(x)
    y -> x + y
end

# 中の関数に名前をつけても構いません。
function create_adder(x)
    function adder(y)
        x + y
    end
    adder
end

add_10 = create_adder(10)
add_10(3) # => 13


# いくつかの高階関数が定義されています。
map(add_10, [1,2,3]) # => [11, 12, 13]
filter(x -> x > 5, [3, 4, 5, 6, 7]) # => [6, 7]

# map の代わりとしてリスト内包表記も使えます。
[add_10(i) for i=[1, 2, 3]] # => [11, 12, 13]
[add_10(i) for i in [1, 2, 3]] # => [11, 12, 13]

####################################################
## 5. 型
####################################################

# Julia ではすべての値にひとつの型がついています。
# 変数に、ではなくて値に、です。
# typeof 関数を使うことで、値が持つ型を取得できます。
typeof(5) # => Int64

# 型自身もまた、第一級の値であり、型を持っています。
typeof(Int64) # => DataType
typeof(DataType) # => DataType
# DataType は型を表現する型であり、DataType 自身もDataType 型の値です。

# 型はドキュメント化や最適化、関数ディスパッチのために使われます。
# 静的な型チェックは行われません。

# 自分で新しい型を定義することもできます。
# 他の言語で言う、構造体やレコードに近いものになっています。
# 型定義には type キーワードを使います。
# type Name
#   field::OptionalType
#   ...
# end
type Tiger
  taillength::Float64
  coatcolor # 型注釈を省略した場合、自動的に :: Any として扱われます。
end

# 型を定義すると、その型のプロパティすべてを、定義した順番に
# 引数として持つデフォルトコンストラクタが自動的に作られます。
tigger = Tiger(3.5,"orange") # => Tiger(3.5,"orange")

# 型名がそのままコンストラクタ名（関数名）となります。
sherekhan = typeof(tigger)(5.6,"fire") # => Tiger(5.6,"fire")

# このような、構造体スタイルの型は、具体型(concrete type)と呼ばれます。
# 具体型はインスタンス化可能ですが、派生型(subtype)を持つことができません。
# 具体型の他には抽象型(abstract type)があります。

# abstract Name
abstract Cat # 型の階層図の途中の一点を指し示す名前となります。

# 抽象型はインスタンス化できませんが、派生型を持つことができます。
# 例えば、 Number は以下の派生型を持つ抽象型です。
subtypes(Number) # => 6-element Array{Any,1}:
                 #     Complex{Float16}
                 #     Complex{Float32}
                 #     Complex{Float64}
                 #     Complex{T<:Real}
                 #     ImaginaryUnit
                 #     Real
subtypes(Cat) # => 0-element Array{Any,1}

# すべての型は、直接的にはただひとつの基本型(supertype) を持ちます。
# super 関数でこれを取得可能です。
typeof(5) # => Int64
super(Int64) # => Signed
super(Signed) # => Real
super(Real) # => Number
super(Number) # => Any
super(super(Signed)) # => Number
super(Any) # => Any
# Int64 を除き、これらはすべて抽象型です。

# <: は派生形を表す演算子です。
# これを使うことで派生型を定義できます。
type Lion <: Cat # Lion は 抽象型 Cat の派生型
  mane_color
  roar::String
end

# 型名と同じ名前の関数を定義し、既に存在するコンストラクタを呼び出して、
# 必要とする型の値を返すことによって、
# デフォルトコンストラクタ以外のコンストラクタを作ることができます。

Lion(roar::String) = Lion("green",roar)
# 型定義の外側で定義されたコンストラクタなので、外部コンストラクタと呼ばれます。

type Panther <: Cat # Panther も Cat の派生型
  eye_color
  Panther() = new("green")
  # Panther は内部コンストラクタとしてこれのみを持ち、
  # デフォルトコンストラクタを持たない
end
# 内部コンストラクタを使うことで、どのような値が作られるのかをコントロールすることができます。
# 出来る限り、外部コンストラクタを使うべきです。

####################################################
## 6. 多重ディスパッチ
####################################################

# Julia では、すべての名前付きの関数は総称的関数(generic function) です。
# これは、関数はいくつかの細かいメソッドの集合である、という意味です。
# 例えば先の Lion 型のコンストラクタ Lion は、Lion という関数の1つのメソッドです。

# コンストラクタ以外の例をみるために、新たに meow 関数を作りましょう。

# Lion, Panther, Tiger 型それぞれに対する meow 関数のメソッド定義
function meow(animal::Lion)
  animal.roar # 型のプロパティには . でアクセスできます。
end

function meow(animal::Panther)
  "grrr"
end

function meow(animal::Tiger)
  "rawwwr"
end

# meow 関数の実行
meow(tigger) # => "rawwr"
meow(Lion("brown","ROAAR")) # => "ROAAR"
meow(Panther()) # => "grrr"

# 型の階層関係を見てみましょう
issubtype(Tiger,Cat) # => false
issubtype(Lion,Cat) # => true
issubtype(Panther,Cat) # => true

# 抽象型 Cat の派生型を引数にとる関数
function pet_cat(cat::Cat)
  println("The cat says $(meow(cat))")
end

pet_cat(Lion("42")) # => prints "The cat says 42"
try
    pet_cat(tigger) # => ERROR: no method pet_cat(Tiger,)
catch e
    println(e)
end

# オブジェクト指向言語では、一般的にシングルディスパッチが用いられます。
# つまり、関数に複数あるメソッドのうちにどれが呼ばれるかは、
# その第一引数（もしくは、 . や -> の前にある値の型）によってのみ決定されます。
# 一方でJulia では、すべての引数の型が、このメソッド決定に寄与します。

# 多変数関数を定義して、この辺りを見て行きましょう。
function fight(t::Tiger,c::Cat)
  println("The $(t.coatcolor) tiger wins!")
end
# => fight (generic function with 1 method)

fight(tigger,Panther()) # => prints The orange tiger wins!
fight(tigger,Lion("ROAR")) # => prints The orange tiger wins!

# 第二引数の Cat が実際は Lion だった時に、挙動が変わるようにします。
fight(t::Tiger,l::Lion) = println("The $(l.mane_color)-maned lion wins!")
# => fight (generic function with 2 methods)

fight(tigger,Panther()) # => prints The orange tiger wins!
fight(tigger,Lion("ROAR")) # => prints The green-maned lion wins!

# 別に Tiger だけが戦う必要もないですね。
fight(l::Lion,c::Cat) = println("The victorious cat says $(meow(c))")
# => fight (generic function with 3 methods)

fight(Lion("balooga!"),Panther()) # => prints The victorious cat says grrr
try
  fight(Panther(),Lion("RAWR")) # => ERROR: no method fight(Panther,Lion)
catch
end

# 第一引数にも Cat を許しましょう。
fight(c::Cat,l::Lion) = println("The cat beats the Lion")
# => Warning: New definition
#    fight(Cat,Lion) at none:1
# is ambiguous with
#    fight(Lion,Cat) at none:2.
# Make sure
#    fight(Lion,Lion)
# is defined first.
#fight (generic function with 4 methods)

# 警告が出ましたが、これは次の対戦で何が起きるのかが不明瞭だからです。
fight(Lion("RAR"),Lion("brown","rarrr")) # => prints The victorious cat says rarrr
# Julia のバージョンによっては、結果が違うかもしれません。

fight(l::Lion,l2::Lion) = println("The lions come to a tie")
fight(Lion("RAR"),Lion("brown","rarrr")) # => prints The lions come to a tie


# Julia が生成する LLVM 内部表現や、アセンブリを調べることもできます。

square_area(l) = l * l      # square_area (generic function with 1 method)

square_area(5) #25

# square_area に整数を渡すと何が起きる？
code_native(square_area, (Int32,))  
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1              # Prologue
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    movsxd  RAX, EDI        # l を取得
	#	    imul    RAX, RAX        # l*l を計算して RAX に入れる
	#	    pop RBP                 # Base Pointer を元に戻す
	#	    ret                     # 終了。RAX の中身が結果

code_native(square_area, (Float32,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vmulss  XMM0, XMM0, XMM0  # 単精度浮動小数点数演算 (AVX)
	#	    pop RBP
	#	    ret

code_native(square_area, (Float64,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vmulsd  XMM0, XMM0, XMM0 # 倍精度浮動小数点数演算 (AVX)
	#	    pop RBP
	#	    ret
	#	

# Julia では、浮動小数点数と整数との演算では
# 自動的に浮動小数点数用の命令が生成されることに注意してください。
# 円の面積を計算してみましょう。
circle_area(r) = pi * r * r     # circle_area (generic function with 1 method)
circle_area(5)                  # 78.53981633974483

code_native(circle_area, (Int32,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vcvtsi2sd   XMM0, XMM0, EDI          # Load integer (r) from memory
	#	    movabs  RAX, 4593140240              # Load pi
	#	    vmulsd  XMM1, XMM0, QWORD PTR [RAX]  # pi * r
	#	    vmulsd  XMM0, XMM0, XMM1             # (pi * r) * r
	#	    pop RBP
	#	    ret
	#

code_native(circle_area, (Float64,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	    movabs  RAX, 4593140496
	#	Source line: 1
	#	    vmulsd  XMM1, XMM0, QWORD PTR [RAX]
	#	    vmulsd  XMM0, XMM1, XMM0
	#	    pop RBP
	#	    ret
	#	
```

## より勉強するために

[公式ドキュメント](http://docs.julialang.org/en/latest/manual/) (英語)にはより詳細な解説が記されています。

Julia に関して助けが必要ならば、[メーリングリスト](https://groups.google.com/forum/#!forum/julia-users) が役に立ちます。
みんな非常に親密に教えてくれます。

