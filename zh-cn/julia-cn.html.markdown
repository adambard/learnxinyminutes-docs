---
language: Julia
filename: learn-julia-zh.jl
contributors:
    - ["Jichao Ouyang", "http://oyanglul.us"]
translators:
    - ["Jichao Ouyang", "http://oyanglul.us"]
    - ["woclass", "https://github.com/inkydragon"]
lang: zh-cn
---

```julia
# 单行注释只需要一个井号
#= 多行注释
   只需要以 '#=' 开始 '=#' 结束
   还可以嵌套.
=#

####################################################
## 1. 原始类型与操作符
####################################################

# Julia 中一切皆为表达式

# 这是一些基本数字类型
typeof(3)       # => Int64
typeof(3.2)     # => Float64
typeof(2 + 1im) # => Complex{Int64}
typeof(2 // 3)  # => Rational{Int64}

# 支持所有的普通中缀操作符
1 + 1      # => 2
8 - 1      # => 7
10 * 2     # => 20
35 / 5     # => 7.0
10 / 2     # => 5.0  # 整数除法总是返回浮点数
div(5, 2)  # => 2    # 使用 div 可以获得整除的结果
5 \ 35     # => 7.0
2^2        # => 4    # 幂运算，不是异或 (xor)
12 % 10    # => 2

# 用括号提高优先级
(1 + 3) * 2 # => 8

# 位操作符
~2         # => -3 # 按位非 (not)
3 & 5      # => 1  # 按位与 (and)
2 | 4      # => 6  # 按位或 (or)
xor(2, 4)  # => 6  # 按位异或 (xor)
2 >>> 1    # => 1  # 逻辑右移
2 >> 1     # => 1  # 算术右移
2 << 1     # => 4  # 逻辑/算术左移

# 可以用函数 bitstring 查看二进制数。
bitstring(12345)
# => "0000000000000000000000000000000000000000000000000011000000111001"
bitstring(12345.0)
# => "0100000011001000000111001000000000000000000000000000000000000000"

# 布尔值是原始类型
true
false

# 布尔操作符
!true   # => false
!false  # => true
1 == 1  # => true
2 == 1  # => false
1 != 1  # => false
2 != 1  # => true
1 < 10  # => true
1 > 10  # => false
2 <= 2  # => true
2 >= 2  # => true

# 链式比较
1 < 2 < 3 # => true
2 < 3 < 2 # => false

# 字符串可以由 " 创建
"This is a string."

# 字符字面量可用 ' 创建
'a'

# 可以像取数组取值一样用 index 取出对应字符
ascii("This is a string")[1]  # => 'T' 
# Julia 的 index 从 1 开始 :(
# 但是对 UTF-8 无效,
# 因此建议使用遍历器 (map, for loops, 等).

# $ 可用于字符插值:
"2 + 2 = $(2 + 2)" # => "2 + 2 = 4"
# 可以将任何 Julia 表达式放入括号。

# 另一种输出格式化字符串的方法是使用标准库 Printf 中的 Printf 宏
using Printf
@printf "%d is less than %f\n" 4.5 5.3  # => 5 is less than 5.300000

# 打印字符串很容易
println("I'm Julia. Nice to meet you!")

# 字符串可以按字典序进行比较
"good" > "bye" # => true
"good" == "good" # => true
"1 + 2 = 3" == "1 + 2 = $(1 + 2)" # => true

####################################################
## 2. 变量与集合
####################################################

# 给变量赋值就是声明变量
some_var = 5 # => 5
some_var # => 5

# 访问未声明变量会抛出异常
try
    some_other_var # => ERROR: UndefVarError: some_other_var not defined
catch e
    println(e)
end

# 变量名必须以下划线或字母开头
# 之后任何字母，数字，下划线，叹号都是合法的。
SomeOtherVar123! = 6 # => 6

# 甚至可以用 unicode 字符
☃ = 8 # => 8
# 用数学符号非常方便
2 * π # => 6.283185307179586

# 注意 Julia 的命名规约:
#
# * 变量名为小写，单词之间以下划线连接 "_" 。
#
# * 类型名以大写字母开头，单词以 CamelCase 方式连接。
#
# * 函数与宏的名字小写，无下划线。
#
# * 会改变输入的函数名末位为 !。
#   这类函数有时被称为 mutating functions 或 in-place functions.

# 数组存储一列值，index 从 1 开始
a = Int64[]  # => 0-element Array{Int64,1}

# 一维数组可以以逗号分隔值的方式声明
b = [4, 5, 6]  # => 3-element Array{Int64,1}: [4, 5, 6]
b = [4; 5; 6]  # => 3-element Array{Int64,1}: [4, 5, 6]
b[1]    # => 4
b[end]  # => 6

# 二维数组以分号分隔维度
matrix = [1 2; 3 4]  # => 2×2 Array{Int64,2}: [1 2; 3 4]

# 指定数组的类型
b = Int8[4, 5, 6]  # => 3-element Array{Int8,1}: [4, 5, 6]

# 使用 push! 和 append! 往数组末尾添加元素
push!(a, 1)    # => [1]
push!(a, 2)    # => [1,2]
push!(a, 4)    # => [1,2,4]
push!(a, 3)    # => [1,2,4,3]
append!(a, b)  # => [1,2,4,3,4,5,6]

# 用 pop 弹出尾部的元素
pop!(b)  # => 6
b # => [4,5]

# 再放回去
push!(b, 6)  # => [4,5,6]
b # => [4,5,6]

a[1] # => 1 #  永远记住 Julia 的引索从 1 开始！而不是 0！

# 用 end 可以直接取到最后索引. 可用作任何索引表达式
a[end] # => 6

# 数组还支持 popfirst! 和 pushfirst!
popfirst!(a)  # => 1 
a # => [2,4,3,4,5,6]
pushfirst!(a, 7)  # => [7,2,4,3,4,5,6]
a # => [7,2,4,3,4,5,6]

# 以叹号结尾的函数名表示它会改变参数的值
arr = [5,4,6]  # => 3-element Array{Int64,1}: [5,4,6]
sort(arr)   # => [4,5,6]
arr         # => [5,4,6]
sort!(arr)  # => [4,5,6]
arr         # => [4,5,6]

# 数组越界会抛出 BoundsError
try
    a[0] 
    # => ERROR: BoundsError: attempt to access 7-element Array{Int64,1} at 
    # index [0]
    # => Stacktrace:
    # =>  [1] getindex(::Array{Int64,1}, ::Int64) at .\array.jl:731
    # =>  [2] top-level scope at none:0
    # =>  [3] ...
    # => in expression starting at ...\LearnJulia.jl:188
    a[end + 1] 
    # => ERROR: BoundsError: attempt to access 7-element Array{Int64,1} at 
    # index [8]
    # => Stacktrace:
    # =>  [1] getindex(::Array{Int64,1}, ::Int64) at .\array.jl:731
    # =>  [2] top-level scope at none:0
    # =>  [3] ...
    # => in expression starting at ...\LearnJulia.jl:196
catch e
    println(e)
end

# 报错时错误会指出出错的文件位置以及行号，标准库也一样
# 你可以在 Julia 安装目录下的 share/julia 文件夹里找到这些标准库

# 可以用 range 初始化数组
a = [1:5;]  # => 5-element Array{Int64,1}: [1,2,3,4,5]

# 可以切割数组
a[1:3] # => [1, 2, 3]
a[2:end] # => [2, 3, 4, 5]

# 用 splice! 切割原数组
arr = [3,4,5]
splice!(arr, 2) # => 4 
arr # => [3,5]

# 用 append! 连接数组
b = [1,2,3]
append!(a, b) # => [1, 2, 3, 4, 5, 1, 2, 3]
a # => [1, 2, 3, 4, 5, 1, 2, 3]

# 检查元素是否在数组中
in(1, a) # => true

# 用 length 获得数组长度
length(a) # => 8

# 元组(Tuples)是不可变的
tup = (1, 2, 3)  # => (1,2,3)
typeof(tup) # => Tuple{Int64,Int64,Int64}
tup[1] # => 1
try
    tup[1] = 3  
    # => ERROR: MethodError: no method matching 
    # setindex!(::Tuple{Int64,Int64,Int64}, ::Int64, ::Int64)
catch e
    println(e)
end

# 大多数组的函数同样支持元组
length(tup) # => 3
tup[1:2]    # => (1,2)
in(2, tup)  # => true

# 可以将元组的元素解包赋给变量
a, b, c = (1, 2, 3)  # => (1,2,3)  
a # => 1
b # => 2
c # => 3

# 不用括号也可以
d, e, f = 4, 5, 6  # => (4,5,6)
d # => 4
e # => 5
f # => 6

# 单元素 tuple 不等于其元素值
(1,) == 1 # => false
(1) == 1  # => true

# 交换值
e, d = d, e  # => (5,4) 
d # => 5
e # => 4


# 字典Dictionaries store mappings
empty_dict = Dict()  # => Dict{Any,Any} with 0 entries

# 也可以用字面量创建字典
filled_dict = Dict("one" => 1, "two" => 2, "three" => 3)
# => Dict{String,Int64} with 3 entries:
# =>  "two" => 2, "one" => 1, "three" => 3

# 用 [] 获得键值
filled_dict["one"] # => 1

# 获得所有键
keys(filled_dict)
# => Base.KeySet for a Dict{String,Int64} with 3 entries. Keys:
# =>  "two", "one", "three"
# 注意，键的顺序不是插入时的顺序

# 获得所有值
values(filled_dict)
# => Base.ValueIterator for a Dict{String,Int64} with 3 entries. Values: 
# =>  2, 1, 3
# 注意，值的顺序也一样

# 用 in 检查键值是否已存在，用 haskey 检查键是否存在
in(("one" => 1), filled_dict)  # => true
in(("two" => 3), filled_dict)  # => false
haskey(filled_dict, "one")     # => true
haskey(filled_dict, 1)         # => false

# 获取不存在的键的值会抛出异常
try
    filled_dict["four"]  # => ERROR: KeyError: key "four" not found
catch e
    println(e)
end

# 使用 get 可以提供默认值来避免异常
# get(dictionary,key,default_value)
get(filled_dict, "one", 4)   # => 1
get(filled_dict, "four", 4)  # => 4

# Sets 表示无序不可重复的值的集合
empty_set = Set()  # => Set(Any[])
# 初始化一个带初值的 Set
filled_set = Set([1, 2, 2, 3, 4])  # => Set([4, 2, 3, 1])

# 新增值
push!(filled_set, 5)  # => Set([4, 2, 3, 5, 1])

# 检查 Set 中是否存在某值
in(2, filled_set)   # => true
in(10, filled_set)  # => false

# 交集，并集，差集
other_set = Set([3, 4, 5, 6])         # => Set([4, 3, 5, 6])
intersect(filled_set, other_set)      # => Set([4, 3, 5])
union(filled_set, other_set)          # => Set([4, 2, 3, 5, 6, 1])
setdiff(Set([1,2,3,4]), Set([2,3,5])) # => Set([4, 1])


####################################################
## 3. 控制语句
####################################################

# 声明一个变量
some_var = 5

# 这是一个 if 语句块，其中的缩进不是必须的
if some_var > 10
    println("some_var is totally bigger than 10.")
elseif some_var < 10    # elseif 是可选的
    println("some_var is smaller than 10.")
else                    # else 也是可选的
    println("some_var is indeed 10.")
end
# => some_var is smaller than 10.


# For 循环遍历
# 可迭代的类型包括：Range, Array, Set, Dict 和 AbstractString
for animal = ["dog", "cat", "mouse"]
    println("$animal is a mammal")
    # 你可以用 $ 将变量或表达式插入字符串中 
end
# => dog is a mammal
# => cat is a mammal
# => mouse is a mammal

# 你也可以不用 '=' 而使用 'in'
for animal in ["dog", "cat", "mouse"]
    println("$animal is a mammal")
end
# => dog is a mammal
# => cat is a mammal
# => mouse is a mammal

for pair in Dict("dog" => "mammal", "cat" => "mammal", "mouse" => "mammal")
    from, to = pair
    println("$from is a $to")
end
# => mouse is a mammal
# => cat is a mammal
# => dog is a mammal
# 注意！这里的输出顺序和上面的不同

for (k, v) in Dict("dog" => "mammal", "cat" => "mammal", "mouse" => "mammal")
    println("$k is a $v")
end
# => mouse is a mammal
# => cat is a mammal
# => dog is a mammal

# While 循环
let x = 0
    while x < 4
        println(x)
        x += 1  # x = x + 1 的缩写
    end
end
# => 0
# => 1
# => 2
# => 3

# 用 try/catch 处理异常
try
    error("help")
catch e
    println("caught it $e")
end
# => caught it ErrorException("help")


####################################################
## 4. 函数
####################################################

# 用关键字 'function' 可创建一个新函数
#function name(arglist)
#  body...
#end
function add(x, y)
    println("x is $x and y is $y")

    # 最后一行语句的值为返回
    x + y
end

add(5, 6) # => 在 "x is 5 and y is 6" 后会打印 11

# 还可以定义接收可变长参数的函数
function varargs(args...)
    return args
    # 关键字 return 可在函数内部任何地方返回
end
# => varargs (generic function with 1 method)

varargs(1,2,3) # => (1,2,3)

# 省略号 ... 被称为 splat.
# 刚刚用在了函数定义中
# 还可以用在函数的调用
# Array 或者 Tuple 的内容会变成参数列表
Set([1,2,3])    # => Set{Array{Int64,1}}([1,2,3]) # 获得一个 Array 的 Set
Set([1,2,3]...) # => Set{Int64}(1,2,3) # 相当于 Set(1,2,3)

x = (1,2,3)     # => (1,2,3)
Set(x)          # => Set{(Int64,Int64,Int64)}((1,2,3)) # 一个 Tuple 的 Set
Set(x...)       # => Set{Int64}(2,3,1)


# 可定义可选参数的函数
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

# 还可以定义键值对的参数
function keyword_args(;k1=4,name2="hello") # note the ;
    return ["k1"=>k1,"name2"=>name2]
end

keyword_args(name2="ness") # => ["name2"=>"ness","k1"=>4]
keyword_args(k1="mine") # => ["k1"=>"mine","name2"=>"hello"]
keyword_args() # => ["name2"=>"hello","k1"=>4]

# 可以组合各种类型的参数在同一个函数的参数列表中
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

# Julia 有一等函数
function create_adder(x)
    adder = function (y)
        return x + y
    end
    return adder
end

# 这是用 "stabby lambda syntax" 创建的匿名函数
(x -> x > 2)(3) # => true

# 这个函数和上面的 create_adder 一模一样
function create_adder(x)
    y -> x + y
end

# 你也可以给内部函数起个名字
function create_adder(x)
    function adder(y)
        x + y
    end
    adder
end

add_10 = create_adder(10)
add_10(3) # => 13


# 内置的高阶函数有
map(add_10, [1,2,3]) # => [11, 12, 13]
filter(x -> x > 5, [3, 4, 5, 6, 7]) # => [6, 7]

# 还可以使用 list comprehensions 替代 map
[add_10(i) for i=[1, 2, 3]] # => [11, 12, 13]
[add_10(i) for i in [1, 2, 3]] # => [11, 12, 13]

####################################################
## 5.  类型
####################################################

# Julia 有类型系统
# 所有的值都有类型；但变量本身没有类型
# 你可以用 `typeof` 函数获得值的类型
typeof(5) # => Int64

# 类型是一等值
typeof(Int64) # => DataType
typeof(DataType) # => DataType
# DataType 是代表类型的类型，也代表他自己的类型

# 类型可用作文档化，优化，以及调度
# 并不是静态检查类型

# 用户还可以自定义类型
# 跟其他语言的 records 或 structs 一样
# 用 `type` 关键字定义新的类型

# type Name
#   field::OptionalType
#   ...
# end
type Tiger
  taillength::Float64
  coatcolor # 不附带类型标注的相当于 `::Any`
end

# 构造函数参数是类型的属性
tigger = Tiger(3.5,"orange") # => Tiger(3.5,"orange")

# 用新类型作为构造函数还会创建一个类型
sherekhan = typeof(tigger)(5.6,"fire") # => Tiger(5.6,"fire")

# struct 类似的类型被称为具体类型
# 他们可被实例化但不能有子类型
# 另一种类型是抽象类型

# abstract Name
abstract Cat # just a name and point in the type hierarchy

# 抽象类型不能被实例化，但是可以有子类型
# 例如，Number 就是抽象类型
subtypes(Number) # => 6-element Array{Any,1}:
                 #     Complex{Float16}
                 #     Complex{Float32}
                 #     Complex{Float64}
                 #     Complex{T<:Real}
                 #     ImaginaryUnit
                 #     Real
subtypes(Cat) # => 0-element Array{Any,1}

# 所有的类型都有父类型; 可以用函数 `super` 得到父类型.
typeof(5) # => Int64
super(Int64) # => Signed
super(Signed) # => Real
super(Real) # => Number
super(Number) # => Any
super(super(Signed)) # => Number
super(Any) # => Any
# 所有这些类型，除了 Int64, 都是抽象类型.

# <: 是类型集成操作符
type Lion <: Cat # Lion 是 Cat 的子类型
  mane_color
  roar::String
end

# 可以继续为你的类型定义构造函数
# 只需要定义一个同名的函数
# 并调用已有的构造函数设置一个固定参数
Lion(roar::String) = Lion("green",roar)
# 这是一个外部构造函数，因为他再类型定义之外

type Panther <: Cat # Panther 也是 Cat 的子类型
  eye_color
  Panther() = new("green")
  # Panthers 只有这个构造函数，没有默认构造函数
end
# 使用内置构造函数，如 Panther，可以让你控制
# 如何构造类型的值
# 应该尽可能使用外部构造函数而不是内部构造函数

####################################################
## 6. 多分派
####################################################

# 在Julia中, 所有的具名函数都是类属函数
# 这意味着他们都是有很大小方法组成的
# 每个 Lion 的构造函数都是类属函数 Lion 的方法

# 我们来看一个非构造函数的例子

# Lion, Panther, Tiger 的 meow 定义为
function meow(animal::Lion)
  animal.roar # 使用点符号访问属性
end

function meow(animal::Panther)
  "grrr"
end

function meow(animal::Tiger)
  "rawwwr"
end

# 试试 meow 函数
meow(tigger) # => "rawwr"
meow(Lion("brown","ROAAR")) # => "ROAAR"
meow(Panther()) # => "grrr"

# 再看看层次结构
issubtype(Tiger,Cat) # => false
issubtype(Lion,Cat) # => true
issubtype(Panther,Cat) # => true

# 定义一个接收 Cats 的函数
function pet_cat(cat::Cat)
  println("The cat says $(meow(cat))")
end

pet_cat(Lion("42")) # => prints "The cat says 42"
try
    pet_cat(tigger) # => ERROR: no method pet_cat(Tiger,)
catch e
    println(e)
end

# 在面向对象语言中，通常都是单分派
# 这意味着分派方法是通过第一个参数的类型决定的
# 在Julia中, 所有参数类型都会被考虑到

# 让我们定义有多个参数的函数，好看看区别
function fight(t::Tiger,c::Cat)
  println("The $(t.coatcolor) tiger wins!")
end
# => fight (generic function with 1 method)

fight(tigger,Panther()) # => prints The orange tiger wins!
fight(tigger,Lion("ROAR")) # => prints The orange tiger wins!

# 让我们修改一下传入具体为 Lion 类型时的行为
fight(t::Tiger,l::Lion) = println("The $(l.mane_color)-maned lion wins!")
# => fight (generic function with 2 methods)

fight(tigger,Panther()) # => prints The orange tiger wins!
fight(tigger,Lion("ROAR")) # => prints The green-maned lion wins!

# 把 Tiger 去掉
fight(l::Lion,c::Cat) = println("The victorious cat says $(meow(c))")
# => fight (generic function with 3 methods)

fight(Lion("balooga!"),Panther()) # => prints The victorious cat says grrr
try
  fight(Panther(),Lion("RAWR")) # => ERROR: no method fight(Panther,Lion)
catch
end

# 在试试让 Cat 在前面
fight(c::Cat,l::Lion) = println("The cat beats the Lion")
# => Warning: New definition
#    fight(Cat,Lion) at none:1
# is ambiguous with
#    fight(Lion,Cat) at none:2.
# Make sure
#    fight(Lion,Lion)
# is defined first.
#fight (generic function with 4 methods)

# 警告说明了无法判断使用哪个 fight 方法
fight(Lion("RAR"),Lion("brown","rarrr")) # => prints The victorious cat says rarrr
# 结果在老版本 Julia 中可能会不一样

fight(l::Lion,l2::Lion) = println("The lions come to a tie")
fight(Lion("RAR"),Lion("brown","rarrr")) # => prints The lions come to a tie


# Under the hood
# 你还可以看看 llvm 以及生成的汇编代码

square_area(l) = l * l      # square_area (generic function with 1 method)

square_area(5) #25

# 给 square_area 一个整形时发生什么
code_native(square_area, (Int32,))
    #       .section    __TEXT,__text,regular,pure_instructions
    #   Filename: none
    #   Source line: 1              # Prologue
    #       push    RBP
    #       mov RBP, RSP
    #   Source line: 1
    #       movsxd  RAX, EDI        # Fetch l from memory?
    #       imul    RAX, RAX        # Square l and store the result in RAX
    #       pop RBP                 # Restore old base pointer
    #       ret                     # Result will still be in RAX

code_native(square_area, (Float32,))
    #       .section    __TEXT,__text,regular,pure_instructions
    #   Filename: none
    #   Source line: 1
    #       push    RBP
    #       mov RBP, RSP
    #   Source line: 1
    #       vmulss  XMM0, XMM0, XMM0  # Scalar single precision multiply (AVX)
    #       pop RBP
    #       ret

code_native(square_area, (Float64,))
    #       .section    __TEXT,__text,regular,pure_instructions
    #   Filename: none
    #   Source line: 1
    #       push    RBP
    #       mov RBP, RSP
    #   Source line: 1
    #       vmulsd  XMM0, XMM0, XMM0 # Scalar double precision multiply (AVX)
    #       pop RBP
    #       ret
    #
# 注意 只要参数中又浮点类型，Julia 就使用浮点指令
# 让我们计算一下圆的面积
circle_area(r) = pi * r * r     # circle_area (generic function with 1 method)
circle_area(5)                  # 78.53981633974483

code_native(circle_area, (Int32,))
    #       .section    __TEXT,__text,regular,pure_instructions
    #   Filename: none
    #   Source line: 1
    #       push    RBP
    #       mov RBP, RSP
    #   Source line: 1
    #       vcvtsi2sd   XMM0, XMM0, EDI          # Load integer (r) from memory
    #       movabs  RAX, 4593140240              # Load pi
    #       vmulsd  XMM1, XMM0, QWORD PTR [RAX]  # pi * r
    #       vmulsd  XMM0, XMM0, XMM1             # (pi * r) * r
    #       pop RBP
    #       ret
    #

code_native(circle_area, (Float64,))
    #       .section    __TEXT,__text,regular,pure_instructions
    #   Filename: none
    #   Source line: 1
    #       push    RBP
    #       mov RBP, RSP
    #       movabs  RAX, 4593140496
    #   Source line: 1
    #       vmulsd  XMM1, XMM0, QWORD PTR [RAX]
    #       vmulsd  XMM0, XMM1, XMM0
    #       pop RBP
    #       ret
    #
```
