---
language: ruby
filename: learnruby-zh.rb
lang: zh-cn
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
translators:
  - ["Lin Xiangyu", "https://github.com/oa414"]
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

# 算术符号只是语法糖而已
# 实际上是调用对象的方法
1.+(3) #=> 4
10.* 5 #=> 50 

# 特殊的只也是对象
nil # 空
true # 真
false # 假

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# 相等运算符
1 == 1 #=> true
2 == 1 #=> false

# 不等运算符
1 != 1 #=> false
2 != 1 #=> true
!true  #=> false
!false #=> true

# 除了false自己，nil是唯一的值为false的对象 

!nil   #=> true
!false #=> true
!0     #=> false

# 更多比较
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# 字符串是对象

'I am a string'.class #=> String
"I am a string too".class #=> String

placeholder = "use string interpolation"
"I can #{placeholder} when using double quoted strings"
#=> "I can use string interpolation when using double quoted strings"


# 输出值
puts "I'm printing!"

# 变量
x = 25 #=> 25
x #=> 25

# 注意赋值语句返回了赋的值
# 这意味着你可以用多重赋值语句

x = y = 10 #=> 10
x #=> 10
y #=> 10

# 按照惯例，用 snake_case 作为变量名
snake_case = true

# 使用具有描述性的运算符
path_to_project_root = '/good/name/'
path = '/bad/name/'

# 符号（Symbols，也是对象)
# 符号是不可变的，内部用整数类型表示的可重用的值。
# 通常用它代替字符串来有效地表示有意义的值。


:pending.class #=> Symbol

status = :pending

status == :pending #=> true

status == 'pending' #=> false

status == :approved #=> false

# 数组

# 这是一个数组
[1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# 数组可以包含不同类型的元素

array = [1, "hello", false] #=> => [1, "hello", false]

# 数组可以被索引
# 从前面开始
array[0] #=> 1
array[12] #=> nil

# 像运算符一样，[var]形式的访问
# 也就是一个语法糖
# 实际上是调用对象的[] 方法
array.[] 0 #=> 1
array.[] 12 #=> nil

# 从尾部开始
array[-1] #=> 5

# 同时指定开始的位置和结束的位置
array[2, 4] #=> [3, 4, 5]

# 或者指定一个范围
array[1..3] #=> [2, 3, 4]

# 像这样往数组增加一个元素
array << 6 #=> [1, 2, 3, 4, 5, 6]

# 哈希表是Ruby的键值对的基本数据结构
# 哈希表由大括号定义
hash = {'color' => 'green', 'number' => 5}

hash.keys #=> ['color', 'number']

# 哈希表可以通过键快速地查询
hash['color'] #=> 'green'
hash['number'] #=> 5

# 查询一个不存在地键将会返回nil
hash['nothing here'] #=> nil

# 用 #each 方法来枚举哈希表:
hash.each do |k, v|
  puts "#{k} is #{v}"
end

# 从Ruby 1.9开始, 用符号作为键的时候有特别的记号表示:

new_hash = { defcon: 3, action: true}

new_hash.keys #=> [:defcon, :action]

# 小贴士：数组和哈希表都是可枚举的
# 它们可以共享一些有用的方法，比如each, map, count, 和more

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

# 然而
# 没人用for循环
#  用`each`来代替，就像这样

(1..5).each do |counter|
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

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

# 函数

def double(x)
  x * 2
end

# 函数 (以及所有的方法块) 隐式地返回了最后语句的值
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
# 所有的方法都有一个隐式的块参数
# 可以用yield参数调用

def surround
  puts "{"
  yield
  puts "}"
end

surround { puts 'hello world' }

# {
# hello world
# }


# 用class关键字定义一个类
class Human

     # 一个类变量，它被这个类地所有实例变量共享
    @@species = "H. sapiens"

    # 构造函数
    def initialize(name, age=0)
        # 将参数name的值赋给实例变量@name
        @name = name
        # 如果没有给出age, 那么会采用参数列表中地默认地值
        @age = age
    end

    # 基本的 setter 方法
    def name=(name)
        @name = name
    end

    # 基本地 getter 方法
    def name
        @name
    end

    # 一个类方法以self.开头
    # 它可以被类调用，但不能被类的实例调用
    def self.say(msg)
       puts "#{msg}"
    end

    def species
        @@species
    end

end


# 类的例子
jim = Human.new("Jim Halpert")

dwight = Human.new("Dwight K. Schrute")

# 让我们来调用一些方法
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# 调用对象的方法
Human.say("Hi") #=> "Hi"

```
