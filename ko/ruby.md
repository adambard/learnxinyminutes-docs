---
name: Ruby
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
  - ["Gabriel Halley", "https://github.com/ghalley"]
  - ["Persa Zula", "http://persazula.com"]
  - ["Jake Faris", "https://github.com/farisj"]
  - ["Corey Ward", "https://github.com/coreyward"]
  - ["Jannik Siebert", "https://github.com/janniks"]
  - ["Keith Miyake", "https://github.com/kaymmm"]
---

```ruby
# 이것은 주석입니다

=begin
이것은 여러 줄 주석입니다.
시작 줄은 "=begin"으로 시작해야 합니다
그리고 끝 줄은 "=end"로 시작해야 합니다.

이렇게 하거나, 여러 줄 주석의
각 줄을 # 문자로 시작할 수 있습니다.
=end

# Ruby에서는 (거의) 모든 것이 객체입니다.
# 이것은 숫자도 포함합니다...
3.class #=> Integer

# ...그리고 문자열도...
"Hello".class #=> String

# ...심지어 메서드도!
"Hello".method(:class).class #=> Method

# 몇 가지 기본 산술
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2 ** 5 #=> 32
5 % 3 #=> 2

# 비트 연산자
3 & 5 #=> 1
3 | 5 #=> 7
3 ^ 5 #=> 6

# 산술은 단지 문법적 설탕일 뿐입니다
# 객체에 대한 메서드 호출을 위한
1.+(3) #=> 4
10.* 5 #=> 50
100.methods.include?(:/) #=> true

# 특수 값은 객체입니다
nil # 다른 언어의 null과 동일
true # 참
false # 거짓

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# 동등성
1 == 1 #=> true
2 == 1 #=> false

# 부등성
1 != 1 #=> false
2 != 1 #=> true

# false 자체를 제외하고, nil은 유일한 다른 '거짓' 값입니다

!!nil   #=> false
!!false #=> false
!!0     #=> true
!!""    #=> true

# 더 많은 비교
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# 결합 비교 연산자 (첫 번째 인수가 더 크면 `1`을,
# 두 번째 인수가 더 크면 `-1`을, 그렇지 않으면 `0`을 반환)
1 <=> 10 #=> -1 (1 < 10)
10 <=> 1 #=> 1 (10 > 1)
1 <=> 1 #=> 0 (1 == 1)

# 논리 연산자
true && false #=> false
true || false #=> true

# 훨씬 낮은 우선순위를 가진 논리 연산자의 대체 버전이 있습니다.
# 이것들은 그들 중 하나가 참 또는 거짓을 반환할 때까지 문장을
# 함께 연결하는 흐름 제어 구문으로 사용하기 위한 것입니다.

# `do_something_else`는 `do_something`이 성공하는 경우에만 호출됩니다.
do_something() and do_something_else()
# `log_error`는 `do_something`이 실패하는 경우에만 호출됩니다.
do_something() or log_error()

# 문자열 보간

placeholder = '문자열 보간 사용'
"큰따옴표 문자열을 사용할 때 #{placeholder}할 수 있습니다"
#=> "큰따옴표 문자열을 사용할 때 문자열 보간 사용 할 수 있습니다"

# `+`를 사용하여 문자열을 결합할 수 있지만, 다른 유형과는 결합할 수 없습니다
'hello ' + 'world'  #=> "hello world"
'hello ' + 3 #=> TypeError: no implicit conversion of Integer into String
'hello ' + 3.to_s #=> "hello 3"
"hello #{3}" #=> "hello 3"

# ...또는 문자열과 연산자 결합
'hello ' * 3 #=> "hello hello hello "

# ...또는 문자열에 추가
'hello' << ' world' #=> "hello world"

# 끝에 개행 문자를 사용하여 출력에 인쇄할 수 있습니다
puts "인쇄 중입니다!"
#=> 인쇄 중입니다!
#=> nil

# ...또는 개행 문자 없이 출력에 인쇄
print "인쇄 중입니다!"
#=> "인쇄 중입니다!" => nil

# 변수
x = 25 #=> 25
x #=> 25

# 할당은 할당된 값을 반환한다는 점에 유의하십시오.
# 이것은 다중 할당을 할 수 있음을 의미합니다.

x = y = 10 #=> 10
x #=> 10
y #=> 10

# 관례적으로, 변수 이름에는 snake_case를 사용합니다.
snake_case = true

# 설명적인 변수 이름 사용
path_to_project_root = '/good/name/'
m = '/bad/name/'

# 심볼은 정수 값으로 내부적으로 표현되는 불변의 재사용 가능한 상수입니다.
# 특정 의미 있는 값을 효율적으로 전달하기 위해 문자열 대신 자주 사용됩니다.

:pending.class #=> Symbol

status = :pending

status == :pending #=> true

status == 'pending' #=> false

status == :approved #=> false

# 문자열은 심볼로, 심볼은 문자열로 변환할 수 있습니다.
status.to_s #=> "pending"
"argon".to_sym #=> :argon

# 배열

# 이것은 배열입니다.
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# 배열은 다른 유형의 항목을 포함할 수 있습니다.
[1, 'hello', false] #=> [1, "hello", false]

# 따옴표 대신 %w를 선호할 수 있습니다
%w[foo bar baz] #=> ["foo", "bar", "baz"]

# 배열은 인덱싱할 수 있습니다.
# 앞에서부터...
array[0] #=> 1
array.first #=> 1
array[12] #=> nil

# ...또는 뒤에서부터...
array[-1] #=> 5
array.last #=> 5

# ...또는 시작 인덱스와 길이로...
array[2, 3] #=> [3, 4, 5]

# ...또는 범위로...
array[1..3] #=> [2, 3, 4]

# 배열을 뒤집을 수 있습니다.
# 뒤집힌 값으로 새 배열 반환
[1,2,3].reverse #=> [3,2,1]
# 변수를 뒤집힌 값으로 업데이트하기 위해 배열을 제자리에서 뒤집기
a = [1,2,3]
a.reverse! #=> a==[3,2,1] bang ('!') 호출 때문에

# 산술과 마찬가지로 [var] 액세스는 단지 문법적 설탕일 뿐입니다
# 객체에서 '[]' 메서드를 호출하기 위한
array.[] 0 #=> 1
array.[] 12 #=> nil

# 배열에 추가할 수 있습니다...
array << 6 #=> [1, 2, 3, 4, 5, 6]
# 또는 이렇게
array.push(6) #=> [1, 2, 3, 4, 5, 6]

# ...그리고 항목이 배열에 있는지 확인
array.include?(1) #=> true

# 해시는 키/값 쌍이 있는 Ruby의 기본 사전입니다.
# 해시는 중괄호로 표시됩니다.
hash = { 'color' => 'green', 'number' => 5 }

hash.keys #=> ['color', 'number']

# 해시는 키로 빠르게 조회할 수 있습니다.
hash['color'] #=> "green"
hash['number'] #=> 5

# 존재하지 않는 키에 대해 해시를 요청하면 nil이 반환됩니다.
hash['nothing here'] #=> nil

# 해시에서 키에 심볼을 사용할 때 대체 구문을 사용할 수 있습니다.

hash = { :defcon => 3, :action => true }
hash.keys #=> [:defcon, :action]

hash = { defcon: 3, action: true }
hash.keys #=> [:defcon, :action]

# 해시에서 키와 값의 존재 확인
hash.key?(:defcon) #=> true
hash.value?(3) #=> true

# 팁: 배열과 해시는 모두 Enumerable입니다!
# each, map, count 등과 같은 많은 유용한 메서드를 공유합니다.

# 제어 구조

# 조건문
if true
  'if 문'
elsif false
  'else if, 선택 사항'
else
  'else, 또한 선택 사항'
end

# 조건이 코드 블록이 아닌 단일 문장의 호출을 제어하는 경우
# 후위-if 표기법을 사용할 수 있습니다.
warnings = ['Patronimic is missing', 'Address too short']
puts("Some warnings occurred:\n" + warnings.join("\n"))  if !warnings.empty?

# `if`보다 `unless`가 더 잘 들리면 조건을 재구성하십시오.
puts("Some warnings occurred:\n" + warnings.join("\n"))  unless warnings.empty?

# 루프
# Ruby에서는 전통적인 `for` 루프가 그다지 일반적이지 않습니다. 대신, 이러한
# 기본 루프는 `each`에 의존하는 enumerable을 사용하여 구현됩니다.
(1..5).each do |counter|
  puts "iteration #{counter}"
end

# 이것은 대략 다음과 같으며 Ruby에서는 보기 드문 경우입니다.
for counter in 1..5
  puts "iteration #{counter}"
end

# 위의 `do |variable| ... end` 구문은 '블록'이라고 합니다. 블록은
# 다른 프로그래밍 언어의 람다, 익명 함수 또는 클로저와 유사합니다.
# 객체로 전달하거나, 호출하거나, 메서드로 첨부할 수 있습니다.
#
# 범위의 'each' 메서드는 범위의 각 요소에 대해 블록을 한 번 실행합니다.
# 블록은 매개변수로 카운터를 전달받습니다.

# 중괄호로 블록을 둘러쌀 수도 있습니다.
(1..5).each { |counter| puts "iteration #{counter}" }

# 데이터 구조의 내용은 each를 사용하여 반복할 수도 있습니다.
array.each do |element|
  puts "#{element} is part of the array"
end
hash.each do |key, value|
  puts "#{key} is #{value}"
end

# 여전히 인덱스가 필요한 경우 'each_with_index'를 사용하고 인덱스
# 변수를 정의할 수 있습니다.
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

# Ruby에는 다른 유용한 반복 함수가 많이 있습니다.
# 예를 들어: 'map', 'reduce', 'inject' 등 목록이 계속됩니다.
# Map은 예를 들어 반복하는 배열을 가져와
# 블록에 정의된 대로 무언가를 수행하고 완전히 새로운 배열을 반환합니다.
array = [1,2,3,4,5]
doubled = array.map do |element|
  element * 2
end
puts doubled
#=> [2,4,6,8,10]
puts array
#=> [1,2,3,4,5]

# 또 다른 유용한 구문은 .map(&:method)입니다.
a = ["FOO", "BAR", "BAZ"]
a.map { |s| s.downcase } #=> ["foo", "bar", "baz"]
a.map(&:downcase) #=> ["foo", "bar", "baz"]

# Case 구문
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

# Case는 범위를 사용할 수도 있습니다.
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

# 예외 처리
begin
  # 예외를 발생시킬 수 있는 코드
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

# 메서드

def double(x)
  x * 2
end

# 메서드(및 블록)는 마지막 문의 값을 암시적으로 반환합니다.
double(2) #=> 4

# 해석이 명확한 경우 괄호는 선택 사항입니다.
double 3 #=> 6

double double 3 #=> 12

def sum(x, y)
  x + y
end

# 메서드 인수는 쉼표로 구분됩니다.
sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12

# yield
# 모든 메서드에는 암시적이고 선택적인 블록 매개변수가 있습니다.
# 'yield' 키워드로 호출할 수 있습니다.
def surround
  puts '{'
  yield
  puts '}'
end

surround { puts 'hello world' }

#=> {
#=> hello world
#=> }

# 블록은 'proc' 객체로 변환될 수 있으며, 이는 블록을 래핑하고
# 다른 메서드에 전달하거나, 다른 범위에 바인딩하거나,
# 다른 방식으로 조작할 수 있도록 합니다. 이것은 메서드 매개변수 목록에서
# 가장 일반적이며, 블록이 주어지면 블록을 수락하고
# 'Proc'으로 변환하는 후행 '&block' 매개변수를 자주 볼 수 있습니다.
# 여기서 이름 지정은 관례이며, '&pineapple'과도 잘 작동합니다.
def guests(&block)
  block.class #=> Proc
  block.call(4)
end

# Proc의 'call' 메서드는 블록이 있을 때 'yield'를 호출하는 것과 유사합니다.
# 'call'에 전달된 인수는 블록에 인수로 전달됩니다.

guests { |n| "You have #{n} guests." }
# => "You have 4 guests."

# 배열로 변환될 인수 목록을 전달할 수 있습니다.
# 그것이 스플랫 연산자("*")의 용도입니다.
def guests(*array)
  array.each { |guest| puts guest }
end

# 약식 블록 구문도 있습니다. 모든 배열 항목에 대해 간단한
# 메서드를 호출해야 할 때 가장 유용합니다.
upcased = ['Watch', 'these', 'words', 'get', 'upcased'].map(&:upcase)
puts upcased
#=> ["WATCH", "THESE", "WORDS", "GET", "UPCASED"]

sum = [1, 2, 3, 4, 5].reduce(&:+)
puts sum
#=> 15

# 구조 분해

# Ruby는 여러 변수에 할당할 때 배열을 자동으로 구조 분해합니다.
a, b, c = [1, 2, 3]
a #=> 1
b #=> 2
c #=> 3

# 어떤 경우에는 스플랫 연산자: `*`를 사용하여 배열을
# 목록으로 구조 분해하라는 메시지를 표시해야 합니다.
ranked_competitors = ["John", "Sally", "Dingus", "Moe", "Marcy"]

def best(first, second, third)
  puts "Winners are #{first}, #{second}, and #{third}."
end

best *ranked_competitors.first(3) #=> Winners are John, Sally, and Dingus.

# 스플랫 연산자는 매개변수에서도 사용할 수 있습니다.
def best(first, second, third, *others)
  puts "Winners are #{first}, #{second}, and #{third}."
  puts "There were #{others.count} other participants."
end

best *ranked_competitors
#=> Winners are John, Sally, and Dingus.
#=> There were 2 other participants.

# 관례적으로, 부울을 반환하는 모든 메서드는 물음표로 끝납니다.
5.even? #=> false
5.odd? #=> true

# 관례적으로, 메서드 이름이 느낌표로 끝나면 수신자를
# 변경하는 것과 같은 파괴적인 작업을 수행합니다. 많은 메서드에는
# 변경을 위한 ! 버전과 변경된 새 버전을 반환하는
# 비-! 버전이 있습니다.
company_name = "Dunder Mifflin"
company_name.upcase #=> "DUNDER MIFFLIN"
company_name #=> "Dunder Mifflin"
# 이번에는 company_name을 변경합니다.
company_name.upcase! #=> "DUNDER MIFFLIN"
company_name #=> "DUNDER MIFFLIN"

# 클래스

# 'class' 키워드로 클래스를 정의할 수 있습니다.
class Human

  # 클래스 변수. 이 클래스의 모든 인스턴스에서 공유됩니다.
  @@species = 'H. sapiens'

  # 기본 초기화자
  def initialize(name, age = 0)
    # 인수를 인스턴스의 'name' 인스턴스 변수에 할당합니다.
    @name = name
    # 나이가 주어지지 않으면 인수 목록의 기본값으로 돌아갑니다.
    @age = age
  end

  # 기본 설정자 메서드
  def name=(name)
    @name = name
  end

  # 기본 접근자 메서드
  def name
    @name
  end

  # 위의 기능은 다음과 같이 attr_accessor 메서드를 사용하여 캡슐화할 수 있습니다.
  attr_accessor :name

  # 접근자/설정자 메서드는 다음과 같이 개별적으로 만들 수도 있습니다.
  attr_reader :name
  attr_writer :name

  # 클래스 메서드는 인스턴스 메서드와 구별하기 위해 self를 사용합니다.
  # 클래스에서만 호출할 수 있으며 인스턴스에서는 호출할 수 없습니다.
  def self.say(msg)
    puts msg
  end

  def species
    @@species
  end
end

# 클래스 인스턴스화
jim = Human.new('Jim Halpert')
dwight = Human.new('Dwight K. Schrute')

# 생성된 객체의 메서드를 호출할 수 있습니다.
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# 클래스 메서드 호출
Human.say('Hi') #=> "Hi"

# 변수의 범위는 이름을 지정하는 방식으로 정의됩니다.
# $로 시작하는 변수는 전역 범위를 가집니다.
$var = "I'm a global var"
defined? $var #=> "global-variable"

# @로 시작하는 변수는 인스턴스 범위를 가집니다.
@var = "I'm an instance var"
defined? @var #=> "instance-variable"

# @@로 시작하는 변수는 클래스 범위를 가집니다.
@@var = "I'm a class var"
defined? @@var #=> "class variable"

# 대문자로 시작하는 변수는 상수입니다.
Var = "I'm a constant"
defined? Var #=> "constant"

# 클래스는 ruby에서 객체이기도 합니다. 따라서 클래스는 인스턴스 변수를 가질 수 있습니다.
# 클래스 변수는 클래스와 모든 하위 클래스에서 공유됩니다.

# 기본 클래스
class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

# 파생 클래스
class Worker < Human
end

Human.foo #=> 0
Worker.foo #=> 0

Human.foo = 2
Worker.foo #=> 2

# 클래스 인스턴스 변수는 클래스의 하위 클래스에서 공유되지 않습니다.
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

Human.bar #=> 0
Doctor.bar #=> nil

module ModuleExample
  def foo
    'foo'
  end
end

# 모듈을 포함하면 해당 메서드가 클래스 인스턴스에 바인딩됩니다.
# 모듈을 확장하면 해당 메서드가 클래스 자체에 바인딩됩니다.
class Person
  include ModuleExample
end

class Book
  extend ModuleExample
end

Person.foo     #=> NoMethodError: undefined method `foo' for Person:Class
Person.new.foo #=> "foo"
Book.foo       #=> "foo"
Book.new.foo   #=> NoMethodError: undefined method `foo'

# 모듈을 포함하고 확장할 때 콜백이 실행됩니다.
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

Something.bar     #=> "bar"
Something.qux     #=> NoMethodError: undefined method `qux'
Something.new.bar #=> NoMethodError: undefined method `bar'
Something.new.qux #=> "qux"
```

## 추가 자료

- [Ruby 대화형 튜토리얼](https://rubymonk.com/) - 일련의 대화형 튜토리얼을 통해 Ruby를 배우십시오.
- [공식 문서](http://ruby-doc.org/core)
- [다른 언어에서 온 Ruby](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - 이전 [무료 버전](http://ruby-doc.com/docs/ProgrammingRuby/)은 온라인에서 사용할 수 있습니다.
- [Ruby 스타일 가이드](https://github.com/bbatsov/ruby-style-guide) - 커뮤니티 기반 Ruby 코딩 스타일 가이드.
- [Try Ruby](https://try.ruby-lang.org/) - 브라우저에서 대화형으로 Ruby 프로그래밍 언어의 기본을 배우십시오.
