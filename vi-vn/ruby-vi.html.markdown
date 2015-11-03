---
language: ruby
filename: learnruby.rb
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
  - ["Tristan Hume", "http://thume.ca/"]
  - ["Nick LaMuro", "https://github.com/NickLaMuro"]
  - ["Marcos Brizeno", "http://www.about.me/marcosbrizeno"]
  - ["Ariel Krakowski", "http://www.learneroo.com"]
  - ["Dzianis Dashkevich", "https://github.com/dskecse"]
  - ["Levi Bostian", "https://github.com/levibostian"]
  - ["Rahil Momin", "https://github.com/iamrahil"]
  - ["Vinh Nguyen", "http://rubydaily.net"]
lang: vi-vn

---

```ruby
# Đây là một comment

=begin
Đây là một comment nhiều dòng
Không ai dùng cách này
Bạn không nên dùng
=end

# Đầu tiên và quan trọng nhất: Mọi thứ là đối tượng.

# Các con số là các đối tượng.

3.class #=> Fixnum

3.to_s #=> "3"


# Một vài bài toán số học căn bản
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2**5 #=> 32

# Số học vừa là các cú pháp  thân thiện cho việc gọi
# một hàm trên một đối tượng
1.+(3) #=> 4
10.* 5 #=> 50

# Các giá trị đặc biệt là các đối tượng
nil # Ở đây không có gì để xem
true # luôn đúng
false # luôn sai

nil.class #=> Lớp Nil
true.class #=> Lớp True
false.class #=> Lớp False

# So sánh bằng
1 == 1 #=> true
2 == 1 #=> false

# So sánh không bằng
1 != 1 #=> false
2 != 1 #=> true

# Ngoài chính false, thì nil là một giá trị khác của false

!nil   #=> true
!false #=> true
!0     #=> false

# Các loại so sánh khác
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Các toán tử logic
true && false #=> false
true || false #=> true
!true #=> false


# Có các cách khác của các toán tử logic với mức thấp hơn
# Chúng được sử dụng như các cấu trúc điều khiển luồng nối các mệnh đề
# với nhau cho đến khi một trong số chúng trả về đúng hoặc sai.

# `do_something_else` chỉ được gọi nếu như hàm  `do_something` thành công.
do_something() and do_something_else()
# `log_error` chỉ được gọi nếu hàm `do_something` không thành công.
do_something() or log_error()


# Các chuỗi là các đối tượng

'I am a string'.class #=> String
"I am a string too".class #=> String

placeholder = 'use string interpolation'
"I can #{placeholder} when using double quoted strings"
#=> "I can use string interpolation when using double quoted strings"

# Nên đưa các chuỗi vào trong dấu nháy đơn
# Ngoài ra dấu nháy kép được sử dụng trong tính toán.

# Nối các chuỗi, nhưng không nối với các số.
'hello ' + 'world'  #=> "hello world"
'hello ' + 3 #=> TypeError: can't convert Fixnum into String
'hello ' + 3.to_s #=> "hello 3"

# Xuất ra ngoài màn hình
puts "I'm printing!"

# Các biến
x = 25 #=> 25
x #=> 25

# Chú ý về việc gán các giá trị được trả về vào biến.
# Điều này có nghĩa là bạn có thể gán nhiều biến.

x = y = 10 #=> 10
x #=> 10
y #=> 10

# Theo quy ước, dùng snake_case cho các tên của biến.
snake_case = true

# Dùng để mô tả tên các biến
path_to_project_root = '/good/name/'
path = '/bad/name/'

# Ký tự (là các đối tượng)
# Các ký tự là bất biến, như các biến hằng số chỉ đến các số nguyên.
# Chúng thường xuyên được sử dụng thay cho các chuỗi để chuyển đổi các giá
# trị hiệu quả.

:pending.class #=> Symbol

status = :pending

status == :pending #=> true

status == 'pending' #=> false

status == :approved #=> false

# Các mảng

# Đây là một mảng
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Các mảng có thể chứa nhiều phần tử khác nhau

[1, 'hello', false] #=> [1, "hello", false]

# Có thể truy cập các giá trị của mảng thông qua các chỉ mục
array[0] #=> 1
array[12] #=> nil

# Giống như số học, sử dụng [biến] là một cú pháp thông dụng
array.[] 0 #=> 1
array.[] 12 #=> nil

# Lấy phần tử cuối cùng
array[-1] #=> 5

# Bắt đầu từ chỉ mục và số phần tử cần lấy
array[2, 3] #=> [3, 4, 5]

# Đảo ngược một mảng
a=[1,2,3]
a.reverse! #=> [3,2,1]

# Lấy một khoảng
array[1..3] #=> [2, 3, 4]

# Thêm phần tử vào mảng bằng cách này
array << 6 #=> [1, 2, 3, 4, 5, 6]
# Hoặc cách này
array.push(6) #=> [1, 2, 3, 4, 5, 6]

# Kiểm tra phần tử có tồn tại trong mảng
array.include?(1) #=> true

# Băm là phần chính của Ruby với các cặp khoá/giá trị
# Băm được biểu thị bằng dấu ngoặc nhọn:
hash = { 'color' => 'green', 'number' => 5 }

hash.keys #=> ['color', 'number']

# Băm có thể được truy cập nhanh chóng thông qua khoá
hash['color'] #=> 'green'
hash['number'] #=> 5

# Khoá không tồn tại sẽ trả về nil
hash['nothing here'] #=> nil

# Kể từ Ruby bản 1.9, đây là một cú pháp đặc biệt, sử dụng symbol như khoá

new_hash = { defcon: 3, action: true }

new_hash.keys #=> [:defcon, :action]

# Kiểm tra khoá hoặc giá trị có tồn tại hay không
new_hash.has_key?(:defcon) #=> true
new_hash.has_value?(3) #=> true

# Mẹo: Cả Mảng và Băm đều là Enumberable
# Chúng cùng chia sẻ rất nhiều phương thức hữu ích như each, map, count...

# Cấu trúc điều khiển

if true
  'if statement'
elsif false
  'else if, optional'
else
  'else, also optional'
end

for counter in 1..5
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# TUY NHIÊN, không ai sử dụng vòng lặp  for.
# Thay vào đó, ban nên dùng phương thức "each" và truyền vào đó một khối.
# Một khối là một loạt các mã mà bạn có thể truyền
# cho một phương thức giống như each.
# Nó tương tự với lambda,  các hàm ẩn danh hoặc closures trong các ngôn ngữ
# lập trình khác.
#
# Phương thức "each" cho một khoản sẽ chạy qua từng phần tử của khoảng đó.
# Khối được truyền vào là một số đếm như là tham số.
# Gọi một method "each" với một khối sẽ trông như thế này:

(1..5).each do |counter|
  puts "iteration #{counter}"
end
#=> iteration 1
#=> iteration 2
#=> iteration 3
#=> iteration 4
#=> iteration 5

# Bạn cũng có thể bao khối trong các dấu ngoặc nhọn.
(1..5).each { |counter| puts "iteration #{counter}" }

# Các nội dung của cấu trúc dữ liệu cũng có thể được lặp bằng each.
array.each do |element|
  puts "#{element} is part of the array"
end
hash.each do |key, value|
  puts "#{key} is #{value}"
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

grade = 'B'

case grade
when 'A'
  puts 'Way to go kiddo'
when 'B'
  puts 'Better luck next time'
when 'C'
  puts 'You can do better'
when 'D'
  puts 'Scraping through'
when 'F'
  puts 'You failed!'
else
  puts 'Alternative grading system, eh?'
end
#=> "Better luck next time"

# Cases cũng được dùng cho các dãy
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

# Xử lý ngoại lệ:
begin
  # Code ở đây có thể sẽ đưa ra một ngoại lệ.
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

# Hàm

def double(x)
  x * 2
end

# Hàm (và tất cả các khối) được mặc định giá trị trả về ở mệnh đề cuối.
double(2) #=> 4

# Dấu ngoặc là một tuỳ chọn cho một kết quả rõ ràng.
double 3 #=> 6

double double 3 #=> 12

def sum(x, y)
  x + y
end

# Các đối số được chia cắt bởi dấu phẩy.
sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12

# yield
# Tất cả các hàm có thể có một tham số tuỳ chọn.
# Nó có thể được gọi với từ khóa "yield".
def surround
  puts '{'
  yield
  puts '}'
end

surround { puts 'hello world' }

# {
# hello world
# }


# Bạn có thể truyền một khối đến một hàm
# Dấu "&" được đánh dấu đến một khối
def guests(&block)
  block.call 'some_argument'
end

# Bạn có thể truyền một danh sách các tham số, nó sẽ được chuyển thành mảng.
# Thông qua việc sử dụng dấu *.
def guests(*array)
  array.each { |guest| puts guest }
end

# Định nghĩ một lớp thông qua từ khoá class.
class Human

  # Một biến class. Nó được chia sẽ cho tất cả các instance của lớp này.
  @@species = 'H. sapiens'

  # Các khởi tạo căn bản
  def initialize(name, age = 0)
    # Gán đối số đến biến instance "name"
    @name = name
    # Nếu không có age, sẽ lấy giá trị mặc định trong danh sách đối số.
    @age = age
  end

  # Hàm nhập giá trị căn bản
  def name=(name)
    @name = name
  end

  # Hàm lấy giá trị căn bản
  def name
    @name
  end

  # Các hàm trên có thể được gọn lại bằng cách dùng hàm attr_accessor
  attr_accessor :name

  # Các hàm nhận/lấy cũng có thể được tạo riêng như sau:
  attr_reader :name
  attr_writer :name

  # Một hàm lớp dùng self để phân biệt với hàm instance.
  # Nó chỉ có thể được gọi trên lớp.
  def self.say(msg)
    puts msg
  end

  def species
    @@species
  end
end


# Khởi tạo một lớp
jim = Human.new('Jim Halpert')

dwight = Human.new('Dwight K. Schrute')

# Hãy gọi một cặp các hàm.
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# Gọi một hàm lớp
Human.say('Hi') #=> "Hi"

# Phạm vi của biến được định nghĩa bởi cách chúng ta đặt tên cho chúng.
# Các biến bắt đầu với dấu $ là biến toàn cục.
$var = "I'm a global var"
defined? $var #=> "global-variable"

# Các biến bắt đầu với dấu @ là biến phạm vi.
@var = "I'm an instance var"
defined? @var #=> "instance-variable"

# Các biến bắt đầu với dấu @@ có pham vi là trong một lớp.
@@var = "I'm a class var"
defined? @@var #=> "class variable"

# Các biến bắt đầu với ký tự viết hoa là biến hằng.
Var = "I'm a constant"
defined? Var #=> "constant"

# Lớp cũng là một đối tượng trong Ruby. Bởi vậy lớp có các biến instance.
# Biến lớp được chia sẽ trong lớp và các lớp kế thừa nó.

# Lớp cơ sở
class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

# Lớp kế thừa
class Worker < Human
end

Human.foo # 0
Worker.foo # 0

Human.foo = 2 # 2
Worker.foo # 2

# Các biến lớp instance không được chia sẽ trong lớp kế thừa.

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

# Include một module sẽ đưa các hàm của module thành instances của lớp.
# Extend một module sẽ đưa các hàm của module thành các biến của lớp.

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

# Hàm hồi quy được thực hiện khi include và extend một module.

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

## Các nguồn tham khảo thêm.

- [Learn Ruby by Example with Challenges](http://www.learneroo.com/modules/61/nodes/338) - A variant of this reference with in-browser challenges.
- [Official Documentation](http://www.ruby-doc.org/core-2.1.1/)
- [Ruby from other languages](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - An older [free edition](http://ruby-doc.com/docs/ProgrammingRuby/) is available online.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - A community-driven Ruby coding style guide.
