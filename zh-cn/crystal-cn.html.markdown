---
language: crystal
filename: learncrystal-cn.cr
contributors:
    - ["Vitalii Elenhaupt", "http://veelenga.com"]
    - ["Arnaud Fernandés", "https://github.com/TechMagister/"]
translators:
    - ["Xuty", "https://github.com/xtyxtyx"]
lang: zh-cn
---

```crystal

# 这是一行注释

# 一切都是对象(object)
nil.class  #=> Nil
100.class  #=> Int32
true.class #=> Bool

# nil, false 以及空指针是假值(falsey values)
!nil   #=> true  : Bool
!false #=> true  : Bool
!0     #=> false : Bool

# 整数类型

1.class #=> Int32

# 四种有符号整数
1_i8.class  #=> Int8
1_i16.class #=> Int16
1_i32.class #=> Int32
1_i64.class #=> Int64

# 四种无符号整数
1_u8.class  #=> UInt8
1_u16.class #=> UInt16
1_u32.class #=> UInt32
1_u64.class #=> UInt64

2147483648.class          #=> Int64
9223372036854775808.class #=> UInt64

# 二进制数
0b1101 #=> 13 : Int32

# 八进制数
0o123 #=> 83 : Int32

# 十六进制数
0xFE012D #=> 16646445 : Int32
0xfe012d #=> 16646445 : Int32

# 浮点数类型

1.0.class #=> Float64

# Crystal中有两种浮点数
1.0_f32.class #=> Float32
1_f32.class   #=> Float32

1e10.class    #=> Float64
1.5e10.class  #=> Float64
1.5e-7.class  #=> Float64

# 字符类型

'a'.class #=> Char

# 八进制字符
'\101' #=> 'A' : Char

# Unicode字符
'\u0041' #=> 'A' : Char

# 字符串

"s".class #=> String

# 字符串不可变(immutable)
s = "hello, "  #=> "hello, "        : String
s.object_id    #=> 134667712        : UInt64
s += "Crystal" #=> "hello, Crystal" : String
s.object_id    #=> 142528472        : UInt64

# 支持字符串插值(interpolation)
"sum = #{1 + 2}" #=> "sum = 3" : String

# 多行字符串
"这是一个
   多行字符串"

# 书写带有引号的字符串
%(hello "world") #=> "hello \"world\""

# 符号类型
# 符号是不可变的常量，本质上是Int32类型
# 符号通常被用来代替字符串，来高效地传递特定的值

:symbol.class #=> Symbol

sentence = :question?     # :"question?" : Symbol

sentence == :question?    #=> true  : Bool
sentence == :exclamation! #=> false : Bool
sentence == "question?"   #=> false : Bool

# 数组类型(Array)

[1, 2, 3].class         #=> Array(Int32)
[1, "hello", 'x'].class #=> Array(Int32 | String | Char)

# 必须为空数组指定类型
[]               # Syntax error: for empty arrays use '[] of ElementType'
[] of Int32      #=> [] : Array(Int32)
Array(Int32).new #=> [] : Array(Int32)

# 数组可以通过下标访问
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5] : Array(Int32)
array[0]                #=> 1               : Int32
array[10]               # raises IndexError
array[-6]               # raises IndexError
array[10]?              #=> nil             : (Int32 | Nil)
array[-6]?              #=> nil             : (Int32 | Nil)

# 使用负位置编号，从后往前访问数组
array[-1] #=> 5

# With a start index and size
# 使用起始位置编号+大小
array[2, 3] #=> [3, 4, 5]

# 使用范围(range)访问数组
array[1..3] #=> [2, 3, 4]

# 向尾部添加元素
array << 6  #=> [1, 2, 3, 4, 5, 6]

# 删除尾部元素
array.pop #=> 6
array     #=> [1, 2, 3, 4, 5]

# 删除首部元素
array.shift #=> 1
array       #=> [2, 3, 4, 5]

# 检查元素是否存在与数组之中
array.includes? 3 #=> true

# 一种特殊语法，用来创建字符串数组或符号数组
%w(one two three) #=> ["one", "two", "three"] : Array(String)
%i(one two three) #=> [:one, :two, :three]    : Array(Symbol)

# 对于定义了`new`和`#<<`方法的类，可以用以下语法创建新对象
set = Set{1, 2, 3} #=> [1, 2, 3]
set.class          #=> Set(Int32)

# 以下代码与上方等同
set = Set(typeof(1, 2, 3)).new
set << 1
set << 2
set << 3

# 哈希表类型(Hash)

{1 => 2, 3 => 4}.class   #=> Hash(Int32, Int32)
{1 => 2, 'a' => 3}.class #=> Hash(Int32 | Char, Int32)

# 必须为空哈希表指定类型
{}                     # Syntax error
{} of Int32 => Int32   # {}
Hash(Int32, Int32).new # {}

# 可以使用键(key)快速查询哈希表
hash = {"color" => "green", "number" => 5}
hash["color"]        #=> "green"
hash["no_such_key"]  #=> Missing hash key: "no_such_key" (KeyError)
hash["no_such_key"]? #=> nil

# 检查某一键哈希表中是否存在
hash.has_key? "color" #=> true

# 对于定义了`#[]=`方法的类，可以使用以下语法创建对象
class MyType
  def []=(key, value)
    puts "do stuff"
  end
end

MyType{"foo" => "bar"}

# 以上与下列代码等同
tmp = MyType.new
tmp["foo"] = "bar"
tmp

# 范围类型(Range)

1..10                  #=> Range(Int32, Int32)
Range.new(1, 10).class #=> Range(Int32, Int32)

# 包含或不包含端点
(3..5).to_a  #=> [3, 4, 5]
(3...5).to_a #=> [3, 4]

# 检查某一值是否在范围内
(1..8).includes? 2 #=> true

# 元组类型(Tuple)

# 元组类型尺寸固定，不可变，储存在栈中
# 元组可以有不同类型的对象组成
{1, "hello", 'x'}.class #=> Tuple(Int32, String, Char)

# 使用下标访问元组
tuple = {:key1, :key2}
tuple[1] #=> :key2
tuple[2] #=> syntax error : Index out of bound

# 将元组中的元素赋值给变量
a, b, c = {:a, 'b', "c"}
a #=> :a
b #=> 'b'
c #=> "c"

# 命名元组类型(NamedTuple)

tuple = {name: "Crystal", year: 2011} # NamedTuple(name: String, year: Int32)
tuple[:name] # => "Crystal" (String)
tuple[:year] # => 2011      (Int32)

# 命名元组的键可以是字符串常量
{"this is a key": 1} # => NamedTuple("this is a key": Int32)

# 过程类型(Proc)
# 过程代表一个函数指针，以及可选的上下文(闭包)
# 过程通常使用字面值创建
proc = ->(x : Int32) { x.to_s }
proc.class # Proc(Int32, String)

# 或者使用`new`方法创建
Proc(Int32, String).new { |x| x.to_s }

# 使用`call`方法调用过程
proc.call 10 #=> "10"

# 控制语句(Control statements)

if true
  "if 语句"
elsif false
  "else-if, 可选"
else
  "else, 同样可选"
end

puts "可以将if后置" if true

# 将if作为表达式
a = if 2 > 1
      3
    else
      4
    end

a #=> 3

# 条件表达式
a = 1 > 2 ? 3 : 4 #=> 4

# `case`语句
cmd = "move"

action = case cmd
  when "create"
    "Creating..."
  when "copy"
    "Copying..."
  when "move"
    "Moving..."
  when "delete"
    "Deleting..."
end

action #=> "Moving..."

# 循环
index = 0
while index <= 3
  puts "Index: #{index}"
  index += 1
end
# Index: 0
# Index: 1
# Index: 2
# Index: 3

index = 0
until index > 3
  puts "Index: #{index}"
  index += 1
end
# Index: 0
# Index: 1
# Index: 2
# Index: 3

# 更好的做法是使用`each`
(0..3).each do |index|
  puts "Index: #{index}"
end
# Index: 0
# Index: 1
# Index: 2
# Index: 3

# 变量的类型取决于控制语句中表达式的类型
if a < 3
  a = "hello"
else
  a = true
end
typeof a #=> (Bool | String)

if a && b
  # 此处`a`与`b`均为Nil
end

if a.is_a? String
  a.class #=> String
end

# 函数(Functions)

def double(x)
  x * 2
end

# 函数(以及所有代码块)均将最末尾表达式的值作为返回值
double(2) #=> 4

# 在没有歧义的情况下，括号可以省略
double 3 #=> 6

double double 3 #=> 12

def sum(x, y)
  x + y
end

# 使用逗号分隔参数
sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12

# yield
# 所有函数都有一个默认生成、可选的代码块(block)参数
# 在函数中可以使用yield调用此代码块

def surround
  puts '{'
  yield
  puts '}'
end

surround { puts "hello world" }

# {
# hello world
# }


# 可将代码块作为参数传给函数
# "&" 表示对代码块参数的引用
def guests(&block)
  block.call "some_argument"
end

# 使用星号"*"将参数转换成元组
def guests(*array)
  array.each { |guest| puts guest }
end

# 如果函数返回数组，可以将其解构
def foods
    ["pancake", "sandwich", "quesadilla"]
end
breakfast, lunch, dinner = foods
breakfast #=> "pancake"
dinner    #=> "quesadilla"

# 按照约定，所有返回布尔值的方法都以问号结尾
5.even? # false
5.odd?  # true

# 以感叹号结尾的方法，都有一些破坏性(destructive)行为，比如改变调用接收者(receiver)
# 对于某些方法，带有感叹号的版本将改变调用接收者，而不带有感叹号的版本返回新值
company_name = "Dunder Mifflin"
company_name.gsub "Dunder", "Donald"  #=> "Donald Mifflin"
company_name  #=> "Dunder Mifflin"
company_name.gsub! "Dunder", "Donald"
company_name  #=> "Donald Mifflin"


# 使用`class`关键字来定义类(class)
class Human

  # 类变量，由类的所有实例所共享
  @@species = "H. sapiens"

  # `name`的类型为`String`
  @name : String

  # 构造器方法(initializer)
  # 其中@name、@age为简写，相当于
  #
  # def initialize(name, age = 0)
  #   @name = name
  #   @age = age
  # end
  #
  # `age`为可选参数，如果未指定，则使用默认值0
  def initialize(@name, @age = 0)
  end

  # @name的setter方法
  def name=(name)
    @name = name
  end

  # @name的getter方法
  def name
    @name
  end

  # 上述getter与setter的定义可以用property宏简化
  property :name

  # 也可用getter与setter宏独立创建getter与setter
  getter :name
  setter :name

  # 此处的`self.`使`say`成为类方法
  def self.say(msg)
    puts msg
  end

  def species
    @@species
  end
end


# 将类实例化
jim = Human.new("Jim Halpert")

dwight = Human.new("Dwight K. Schrute")

# 调用一些实例方法
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# 调用类方法
Human.say("Hi") #=> 输出 Hi ，返回 nil

# 带有`@`前缀的变量为实例变量
class TestClass
  @var = "I'm an instance var"
end

# 带有`@@`前缀的变量为类变量
class TestClass
  @@var = "I'm a class var"
end
# 首字母大写的变量为常量
Var = "这是一个常量"
Var = "无法再次被赋值" # 常量`Var`已经被初始化

# 在crystal中类也是对象(object)，因此类也有实例变量(instance variable)
# 类变量的定义由类以及类的派生类所共有，但类变量的值是独立的

# 基类
class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

# 派生类
class Worker < Human
end

Human.foo   #=> 0
Worker.foo  #=> 0

Human.foo = 2 #=> 2
Worker.foo    #=> 0

Worker.foo = 3 #=> 3
Human.foo   #=> 2
Worker.foo  #=> 3

module ModuleExample
  def foo
    "foo"
  end
end

# include <Module> 将模块(module)中的方法添加为实例方法
# extend <Module>  将模块中的方法添加为类方法

class Person
  include ModuleExample
end

class Book
  extend ModuleExample
end

Person.foo     # => undefined method 'foo' for Person:Class
Person.new.foo # => 'foo'
Book.foo       # => 'foo'
Book.new.foo   # => undefined method 'foo' for Book


# 异常处理

# 定义新的异常类(exception)
class MyException < Exception
end

# 再定义一个异常类
class MyAnotherException < Exception; end

ex = begin
   raise MyException.new
rescue ex1 : IndexError
  "ex1"
rescue ex2 : MyException | MyAnotherException
  "ex2"
rescue ex3 : Exception
  "ex3"
rescue ex4 # 捕捉任何类型的异常
  "ex4"
end

ex #=> "ex2"

```

## 参考资料

- [官方网站](https://crystal-lang.org/)
- [官方文档](https://crystal-lang.org/docs/overview/)
- [在线运行代码](https://play.crystal-lang.org/#/cr)
- [Github仓库](https://github.com/crystal-lang/crystal)
