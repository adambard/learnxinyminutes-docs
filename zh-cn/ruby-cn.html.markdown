---
language: ruby
filename: learnruby-zh.rb
lang: zh-cn
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
  - ["lidashuang", "https://github.com/lidashuang"]
  - ["ftwbzhao", "https://github.com/ftwbzhao"]
translators:
  - ["Lin Xiangyu", "https://github.com/oa414"]
  - ["Jiang Haiyun", "https://github.com/haiiiiiyun"]
---

```ruby
# 这是单行注释

=begin
这是多行注释
没人用这个
你也不该用
=end

# 首先，也是最重要的，所有东西都是对象

# 数字是对象

3.class #=> Fixnum

3.to_s #=> "3"


# 一些基本的算术符号
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2**5 #=> 32
5 % 3 #=> 2

# 位运算符
3 & 5 #=> 1
3 | 5 #=> 7
3 ^ 5 #=> 6

# 算术符号只是语法糖而已
# 实际上是调用对象的方法
1.+(3) #=> 4
10.* 5 #=> 50 

# 特殊的值也是对象
nil # 相当于其它语言中的 null
true # 真
false # 假

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# 相等运算符
1 == 1 #=> true
2 == 1 #=> false

# 不相等运算符
1 != 1 #=> false
2 != 1 #=> true

# 除了false自己，nil是唯一的另一个值为false的对象

!nil   #=> true
!false #=> true
!0     #=> false

# 更多比较
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true


# 组合比较运算符
1 <=> 10 #=> -1
10 <=> 1 #=> 1
1 <=> 1 #=> 0

# 逻辑运算符
true && false #=> false
true || false #=> true
!true #=> false

# 也有优先级更低的逻辑运算符
# 它们用于控制流结构中，用来串接语句，直到返回true或false。

# `do_something_else` 只当 `do_something` 返回true时才会被调用
do_something() and do_something_else()
# `log_error` 只当 `do_something` 返回false时才会被调用
do_something() or log_error()


# 字符串是对象

'I am a string'.class #=> String
"I am a string too".class #=> String

placeholder = "use string interpolation"
"I can #{placeholder} when using double quoted strings"
#=> "I can use string interpolation when using double quoted strings"

# 尽可能优先使用单引号的字符串
# 双引号的字符串会进行一些额外的内部处理

# 合并字符串，但不能和数字合并
'hello ' + 'world'  #=> "hello world"
'hello ' + 3 #=> TypeError: can't convert Fixnum into String
'hello ' + 3.to_s #=> "hello 3"

# 合并字符串及其运算符
'hello ' * 3 #=> "hello hello hello "

# 字符串追加
'hello' << ' world' #=> "hello world"

# 打印输出，并在末尾加换行符
puts "I'm printing!"
#=> I'm printing!
#=> nil

# 打印输出，不加换行符
print "I'm printing!"
#=> I'm printing! => nil

# 变量
x = 25 #=> 25
x #=> 25

# 注意赋值语句返回了赋的值
# 这意味着你可以用多重赋值语句

x = y = 10 #=> 10
x #=> 10
y #=> 10

# 按照惯例，使用类似snake_case风格的变量名
snake_case = true

# 使用有意义的变量名
path_to_project_root = '/good/name/'
path = '/bad/name/'

# 符号（Symbols，也是对象)
# 符号是不可变的，内部用整数值表示的可重用的常数
# 通常用它代替字符串来有效地表示有意义的值

:pending.class #=> Symbol

status = :pending

status == :pending #=> true

status == 'pending' #=> false

status == :approved #=> false

# 数组

# 这是一个数组
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# 数组可以包含不同类型的元素

[1, "hello", false] #=> [1, "hello", false]

# 数组可以被索引
# 从前面开始
array[0] #=> 1
array[12] #=> nil

# 像运算符一样，[var] 形式的访问
# 也只是语法糖
# 实际上是调用对象的 [] 方法
array.[] 0 #=> 1
array.[] 12 #=> nil

# 从尾部开始
array[-1] #=> 5
array.last #=> 5

# 同时指定开始的位置和长度
array[2, 3] #=> [3, 4, 5]

# 将数组逆序
a=[1,2,3]
a.reverse! #=> [3,2,1]

# 或者指定一个区间
array[1..3] #=> [2, 3, 4]

# 像这样往数组增加一个元素
array << 6 #=> [1, 2, 3, 4, 5, 6]
# 或者像这样
array.push(6) #=> [1, 2, 3, 4, 5, 6]

# 检查元素是否包含在数组中
array.include?(1) #=> true

# 哈希表是 Ruby 的主要键/值对表示法
# 哈希表由大括号表示
hash = {'color' => 'green', 'number' => 5}

hash.keys #=> ['color', 'number']

# 哈希表可以通过键快速地查询
hash['color'] #=> 'green'
hash['number'] #=> 5

# 查询一个不存在地键将会返回nil
hash['nothing here'] #=> nil

# 从Ruby 1.9开始，用符号作为键的时候有特别的记号表示:

new_hash = { defcon: 3, action: true }

new_hash.keys #=> [:defcon, :action]

# 小贴士：数组和哈希表都是可枚举的
# 它们共享一些有用的方法，比如each,map,count等等

# 控制流

if true
  "if statement"
elsif false
 "else if, optional"
else
 "else, also optional"
end

for counter in 1..5
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5


# 但是，没有人用for循环。
# 你应该使用"each"方法，然后再传给它一个块。
# 所谓块就是可以传给像"each"这样的方法的代码段。
# 它类似于其它语言中的lambdas, 匿名函数或闭包。
#
# 区间上的"each"方法会对区间中的每个元素运行一次块代码。
# 我们将counter作为一个参数传给了块。
# 调用带有块的"each"方法看起来如下：

(1..5).each do |counter|
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# 你也可以将块包含在一个大括号中：
(1..5).each { |counter| puts "iteration #{counter}" }

# 数据结构中的内容也可以使用each来遍历。
array.each do |element|
  puts "#{element} is part of the array"
end
hash.each do |key, value|
  puts "#{key} is #{value}"
end

# 如果你还需要索引值，可以使用"each_with_index"，并且定义
# 一个索引变量
array.each_with_index do |element, index|
  puts "#{element} is number #{index} in the array"
end

counter = 1
while counter <= 5 do
  puts "iteration #{counter}"
  counter += 1
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# Ruby 中还有很多有用的循环遍历函数，
# 如"map","reduce","inject"等等。
# 以map为例，它会遍历数组，并根据你在
# 块中定义的逻辑对它进行处理，然后返回
# 一个全新的数组。
array = [1,2,3,4,5]
doubled = array.map do |element|
  element * 2
end
puts doubled
#=> [2,4,6,8,10]
puts array
#=> [1,2,3,4,5]

grade = 'B'

case grade
when 'A'
  puts "Way to go kiddo"
when 'B'
  puts "Better luck next time"
when 'C'
  puts "You can do better"
when 'D'
  puts "Scraping through"
when 'F'
  puts "You failed!"
else 
  puts "Alternative grading system, eh?"
end
#=> "Better luck next time"

# case也可以用区间
grade = 82
case grade
when 90..100
  puts 'Hooray!'
when 80...90
  puts 'OK job'
else
  puts 'You failed!'
end
#=> "OK job"

# 异常处理：
begin
  # 这里的代码可能会抛出异常
  raise NoMemoryError, 'You ran out of memory.'
rescue NoMemoryError => exception_variable
  puts 'NoMemoryError was raised', exception_variable
rescue RuntimeError => other_exception_variable
  puts 'RuntimeError was raised now'
else
  puts 'This runs if no exceptions were thrown at all'
ensure
  puts 'This code always runs no matter what'
end

# 函数

def double(x)
  x * 2
end

# 函数 (以及所有的块) 隐式地返回最后语句的值
double(2) #=> 4

# 当不存在歧义的时候括号是可有可无的
double 3 #=> 6

double double 3 #=> 12

def sum(x,y)
  x + y
end

# 方法的参数通过逗号分隔
sum 3, 4 #=> 7

sum sum(3,4), 5 #=> 12

# yield
# 所有的方法都有一个隐式的，可选的块参数
# 可以用 'yield' 关键字调用

def surround
  puts "{"
  yield
  puts "}"
end

surround { puts 'hello world' }

# {
# hello world
# }

# 可以向函数传递一个块
# "&"标记传递的块是一个引用
def guests(&block)
  block.call 'some_argument'
end

# 可以传递多个参数，这些参数会转成一个数组，
# 这也是使用星号符 ("*") 的原因：
def guests(*array)
  array.each { |guest| puts guest }
end

# 如果函数返回一个数组，在赋值时可以进行拆分：
def foods
    ['pancake', 'sandwich', 'quesadilla']
end
breakfast, lunch, dinner = foods
breakfast #=> 'pancake'
dinner #=> 'quesadilla'

# 按照惯例，所有返回布尔值的方法都以?结尾
5.even? # false
5.odd? # true

# 如果方法名末尾有!，表示会做一些破坏性的操作，比如修改调用者自身。
# 很多方法都会有一个!的版本来进行修改，和一个非!的版本
# 只用来返回更新了的结果
company_name = "Dunder Mifflin"
company_name.upcase #=> "DUNDER MIFFLIN"
company_name #=> "Dunder Mifflin"
company_name.upcase! # we're mutating company_name this time!
company_name #=> "DUNDER MIFFLIN"


# 用class关键字定义一个类
class Human

  # 一个类变量，它被这个类的所有实例变量共享
  @@species = "H. sapiens"
  
  # 基本构造函数
  def initialize(name, age = 0)
    # 将参数值赋给实例变量"name"
    @name = name
    # 如果没有给出age,那么会采用参数列表中的默认值
    @age = age
  end
  
  # 基本的setter方法
  def name=(name)
    @name = name
  end
  
  # 基本地getter方法
  def name
    @name
  end
  
  # 以上的功能也可以用下面的attr_accessor来封装
  attr_accessor :name
  
  # Getter/setter方法也可以像这样单独创建
  attr_reader :name
  attr_writer :name
  
  # 类方法通过使用self与实例方法区别开来。
  # 它只能通过类来调用，不能通过实例调用。
  def self.say(msg)
    puts "#{msg}"
  end
  
  def species
    @@species
  end
end


# 初始化一个类
jim = Human.new("Jim Halpert")

dwight = Human.new("Dwight K. Schrute")

# 让我们来调用一些方法
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# 调用类方法
Human.say('Hi') #=> "Hi"

# 变量的作用域由它们的名字格式定义
# 以$开头的变量具有全局域
$var = "I'm a global var"
defined? $var #=> "global-variable"

# 以@开头的变量具有实例作用域
@var = "I'm an instance var"
defined? @var #=> "instance-variable"

# 以@@开头的变量具有类作用域
@@var = "I'm a class var"
defined? @@var #=> "class variable"

# 以大写字母开头的变量是常数
Var = "I'm a constant"
defined? Var #=> "constant"

# 类也是对象。因此类也可以有实例变量。
# 类变量在类以及其继承者之间共享。

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

Human.foo # 0
Worker.foo # 0

Human.foo = 2 # 2
Worker.foo # 2

# 类实例变量不能在继承类间共享。

class Human
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(value)
    @bar = value
  end
end

class Doctor < Human
end

Human.bar # 0
Doctor.bar # nil

module ModuleExample
  def foo
    'foo'
  end
end

# '包含'模块后，模块的方法会绑定为类的实例方法
# '扩展'模块后，模块的方法会绑定为类方法

class Person
  include ModuleExample
end

class Book
  extend ModuleExample
end

Person.foo     # => NoMethodError: undefined method `foo' for Person:Class
Person.new.foo # => 'foo'
Book.foo       # => 'foo'
Book.new.foo   # => NoMethodError: undefined method `foo'

# 当包含或扩展一个模块时，相应的回调代码会被执行。

module ConcernExample
  def self.included(base)
    base.extend(ClassMethods)
    base.send(:include, InstanceMethods)
  end

  module ClassMethods
    def bar
      'bar'
    end
  end

  module InstanceMethods
    def qux
      'qux'
    end
  end
end

class Something
  include ConcernExample
end

Something.bar     # => 'bar'
Something.qux     # => NoMethodError: undefined method `qux'
Something.new.bar # => NoMethodError: undefined method `bar'
Something.new.qux # => 'qux'
```


## 其它资源

- [Learn Ruby by Example with Challenges](http://www.learneroo.com/modules/61/nodes/338) - A variant of this reference with in-browser challenges.
- [An Interactive Tutorial for Ruby](https://rubymonk.com/) - Learn Ruby through a series of interactive tutorials.
- [Official Documentation](http://ruby-doc.org/core)
- [Ruby from other languages](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - An older [free edition](http://ruby-doc.com/docs/ProgrammingRuby/) is available online.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - A community-driven Ruby coding style guide.
- [Try Ruby](http://tryruby.org) - Learn the basic of Ruby programming language, interactive in the browser.
