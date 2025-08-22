---
name: Crystal
filename: learncrystal.cr
contributors:
    - ["Vitalii Elenhaupt", "http://veelenga.com"]
    - ["Arnaud Fernandés", "https://github.com/TechMagister/"]
    - ["Valentin Baca", "https://github.com/valbaca/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

```crystal
# 이것은 주석입니다.

# 모든 것은 객체입니다.
nil.class  #=> Nil
100.class  #=> Int32
true.class #=> Bool

# 거짓 값은 nil, false 및 null 포인터입니다.
!nil   #=> true  : Bool
!false #=> true  : Bool
!0     #=> false : Bool

# 정수

1.class #=> Int32

# 5가지 부호 있는 정수 유형
1_i8.class   #=> Int8
1_i16.class  #=> Int16
1_i32.class  #=> Int32
1_i64.class  #=> Int64
1_i128.class #=> Int128

# 5가지 부호 없는 정수 유형
1_u8.class   #=> UInt8
1_u16.class  #=> UInt16
1_u32.class  #=> UInt32
1_u64.class  #=> UInt64
1_u128.class #=> UInt128

2147483648.class          #=> Int64
9223372036854775808.class #=> UInt64

# 이진수
0b1101 #=> 13 : Int32

# 8진수
0o123 #=> 83 : Int32

# 16진수
0xFE012D #=> 16646445 : Int32
0xfe012d #=> 16646445 : Int32

# 부동 소수점

1.0.class #=> Float64

# 두 가지 부동 소수점 유형이 있습니다.
1.0_f32.class #=> Float32
1_f32.class   #=> Float32

1e10.class    #=> Float64
1.5e10.class  #=> Float64
1.5e-7.class  #=> Float64

# 문자는 'a' 쌍의 작은따옴표를 사용합니다.

'a'.class #=> Char

# 문자는 32비트 유니코드입니다.
'あ' #=> 'あ' : Char

# 유니코드 코드 포인트
'\u0041' #=> 'A' : Char

# 문자열은 " 쌍의 큰따옴표를 사용합니다.

"s".class #=> String

# 문자열은 변경할 수 없습니다.
s = "hello, "  #=> "hello, "        : String
s.object_id    #=> 134667712        : UInt64
s += "Crystal"
s              #=> "hello, Crystal" : String
s.object_id    #=> 142528472        : UInt64

# 보간 지원
"sum = #{1 + 2}" #=> "sum = 3" : String

# 여러 줄 문자열
"This is
   multiline string" #=> "This is\n   multiline string"


# 큰따옴표가 있는 문자열
%(hello "world") #=> "hello \"world\""

# 기호
# 변경할 수 없고 재사용 가능한 상수이며 내부적으로 Int32 정수 값으로 표현됩니다.
# 효율적으로 특정 의미 있는 값을 전달하기 위해 문자열 대신 자주 사용됩니다.

:symbol.class #=> Symbol

sentence = :question?     # :"question?" : Symbol

sentence == :question?    #=> true  : Bool
sentence == :exclamation! #=> false : Bool
sentence == "question?"   #=> false : Bool

# 배열

[1, 2, 3].class         #=> Array(Int32)
[1, "hello", 'x'].class #=> Array(Char | Int32 | String)

# 빈 배열은 유형을 지정해야 합니다.
[]               # 구문 오류: 빈 배열의 경우 '[] of ElementType'을 사용하십시오.
[] of Int32      #=> [] : Array(Int32)
Array(Int32).new #=> [] : Array(Int32)

# 배열은 인덱싱할 수 있습니다.
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5] : Array(Int32)
array[0]                #=> 1               : Int32
array[10]               # IndexError 발생
array[-6]               # IndexError 발생
array[10]?              #=> nil             : (Int32 | Nil)
array[-6]?              #=> nil             : (Int32 | Nil)

# 끝에서부터
array[-1] #=> 5

# 시작 인덱스와 크기
array[2, 3] #=> [3, 4, 5]

# 또는 범위로
array[1..3] #=> [2, 3, 4]

# 배열에 추가
array << 6  #=> [1, 2, 3, 4, 5, 6]

# 배열 끝에서 제거
array.pop #=> 6
array     #=> [1, 2, 3, 4, 5]

# 배열 시작에서 제거
array.shift #=> 1
array       #=> [2, 3, 4, 5]

# 배열에 항목이 있는지 확인
array.includes? 3 #=> true

# 문자열 배열 및 기호 배열에 대한 특수 구문
%w(one two three) #=> ["one", "two", "three"] : Array(String)
%i(one two three) #=> [:one, :two, :three]    : Array(Symbol)

# 다른 유형의 해시 리터럴 구문도 있습니다. .new 및 #<< 메서드를 정의하는 한
set = Set{1, 2, 3} #=> Set{1, 2, 3}
set.class          #=> Set(Int32)

# 위는 다음과 동일합니다.
set = Set(typeof(1, 2, 3)).new #=> Set{} : Set(Int32)
set << 1                       #=> Set{1} : Set(Int32)
set << 2                       #=> Set{1, 2} : Set(Int32)
set << 3                       #=> Set{1, 2, 3} : Set(Int32)

# 해시

{1 => 2, 3 => 4}.class   #=> Hash(Int32, Int32)
{1 => 2, 'a' => 3}.class #=> Hash(Char| Int32, Int32)

# 빈 해시는 유형을 지정해야 합니다.
{}                     # 구문 오류: 빈 해시의 경우 '{} of KeyType => ValueType'을 사용하십시오.
{} of Int32 => Int32   # {} : Hash(Int32, Int32)
Hash(Int32, Int32).new # {} : Hash(Int32, Int32)

# 해시는 키로 빠르게 조회할 수 있습니다.
hash = {"color" => "green", "number" => 5}
hash["color"]        #=> "green"
hash["no_such_key"]  #=> 해시 키 없음: "no_such_key" (KeyError)
hash["no_such_key"]? #=> nil

# 반환 값의 유형은 모든 키 유형을 기반으로 합니다.
hash["number"] #=> 5 : (Int32 | String)

# 해시에서 키 존재 여부 확인
hash.has_key? "color" #=> true

# 기호 및 문자열 키에 대한 특수 표기법
{key1: 'a', key2: 'b'}     # {:key1 => 'a', :key2 => 'b'}
{"key1": 'a', "key2": 'b'} # {"key1" => 'a', "key2" => 'b'}

# 다른 유형의 해시 리터럴 구문도 있습니다. .new 및 #[]= 메서드를 정의하는 한
class MyType
  def []=(key, value)
    puts "do stuff"
  end
end

MyType{"foo" => "bar"}

# 위는 다음과 동일합니다.
tmp = MyType.new
tmp["foo"] = "bar"
tmp

# 범위

1..10                  #=> Range(Int32, Int32)
Range.new(1, 10).class #=> Range(Int32, Int32)

# 포함 또는 배타적일 수 있습니다.
(3..5).to_a  #=> [3, 4, 5]
(3...5).to_a #=> [3, 4]

# 범위에 주어진 값이 포함되는지 여부 확인
(1..8).includes? 2 #=> true

# 튜플은 고정 크기, 불변, 스택 할당된 값 시퀀스로, 유형이 다를 수 있습니다.
{1, "hello", 'x'}.class #=> Tuple(Int32, String, Char)

# 튜플의 값을 인덱스로 액세스
tuple = {:key1, :key2}
tuple[1] #=> :key2
tuple[2] #=> 오류: Tuple(Symbol, Symbol)에 대한 인덱스 범위를 벗어났습니다(2는 -2..1에 없음).

# 여러 변수로 확장할 수 있습니다.
a, b, c = {:a, 'b', "c"}
a #=> :a
b #=> 'b'
c #=> "c"

# Procs는 선택적 컨텍스트(클로저 데이터)가 있는 함수 포인터를 나타냅니다.
# 일반적으로 proc 리터럴로 생성됩니다.
proc = ->(x : Int32) { x.to_s }
proc.class # Proc(Int32, String)
# 또는 새 메서드 사용
Proc(Int32, String).new { |x| x.to_s }

# call 메서드로 proc 호출
proc.call 10 #=> "10"

# 제어문

if true
  "if statement"
elsif false
  "else-if, optional"
else
  "else, also optional"
end

puts "if as a suffix" if true

# 표현식으로서의 if
a = if 2 > 1
      3
    else
      4
    end

a #=> 3

# 삼항 if
a = 1 > 2 ? 3 : 4 #=> 4

# Case 문
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

# 루프
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

# 하지만 선호되는 방법은 each를 사용하는 것입니다.
(1..3).each do |index|
  puts "Index: #{index}"
end
# Index: 1
# Index: 2
# Index: 3

# 제어문에서 변수의 유형은 표현식의 유형에 따라 달라집니다.
if a < 3
  a = "hello"
else
  a = true
end
typeof(a) #=> (Bool | String)

if a && b
  # 여기서는 a와 b 모두 nil이 아님이 보장됩니다.
end

if a.is_a? String
  a.class #=> String
end

# 함수

def double(x)
  x * 2
end

# 함수(및 모든 블록)는 암시적으로 마지막 문의 값을 반환합니다.
double(2) #=> 4

# 호출이 명확한 경우 괄호는 선택 사항입니다.
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

surround { puts "hello world" }

# {
# hello world
# }


# 함수에 블록을 전달할 수 있습니다.
# "&"는 전달된 블록에 대한 참조를 표시합니다.
def guests(&block)
  block.call "some_argument"
end

# 인수의 목록을 전달할 수 있으며, 이는 배열로 변환됩니다.
# 이것이 splat 연산자("*")의 목적입니다.
def guests(*array)
  array.each { |guest| puts guest }
end

# 메서드가 배열을 반환하면 구조 분해 할당을 사용할 수 있습니다.
def foods
    ["pancake", "sandwich", "quesadilla"]
end
breakfast, lunch, dinner = foods
breakfast #=> "pancake"
dinner    #=> "quesadilla"

# 관례적으로 부울을 반환하는 모든 메서드는 물음표로 끝납니다.
5.even? # false
5.odd?  # true

# 또한 관례적으로 메서드가 느낌표로 끝나면 수신자를 변경하는 것과 같이 파괴적인 작업을 수행합니다.
# 일부 메서드에는 변경을 수행하는 ! 버전과
# 새 변경된 버전을 반환하는 비-! 버전이 있습니다.
fruits = ["grapes", "apples", "bananas"]
fruits.sort  #=> ["apples", "bananas", "grapes"]
fruits       #=> ["grapes", "apples", "bananas"]
fruits.sort! #=> ["apples", "bananas", "grapes"]
fruits       #=> ["apples", "bananas", "grapes"]

# 그러나 일부 변경 메서드는 !로 끝나지 않습니다.
fruits.shift #=> "apples"
fruits       #=> ["bananas", "grapes"]

# class 키워드로 클래스를 정의합니다.
class Human

  # 클래스 변수. 이 클래스의 모든 인스턴스에서 공유됩니다.
  @@species = "H. sapiens"

  # 인스턴스 변수. name의 유형은 String입니다.
  @name : String

  # 기본 초기화자
  # 인수를 인스턴스의 "name" 인스턴스 변수에 할당합니다.
  # 나이가 주어지지 않으면 인수 목록의 기본값으로 대체됩니다.
  def initialize(@name, @age = 0)
  end

  # 기본 setter 메서드
  def name=(name)
    @name = name
  end

  # 기본 getter 메서드
  def name
    @name
  end

  # 위의 기능은 property 메서드를 사용하여 다음과 같이 캡슐화할 수 있습니다.
  property :name

  # Getter/setter 메서드는 다음과 같이 개별적으로 만들 수도 있습니다.
  getter :name
  setter :name

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
jim = Human.new("Jim Halpert")

dwight = Human.new("Dwight K. Schrute")

# 몇 가지 메서드를 호출해 보겠습니다.
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# 클래스 메서드 호출
Human.say("Hi") #=> Hi를 인쇄하고 nil을 반환합니다.

# @로 시작하는 변수는 인스턴스 범위입니다.
class TestClass
  @var = "I'm an instance var"
end

# @@로 시작하는 변수는 클래스 범위입니다.
class TestClass
  @@var = "I'm a class var"
end
# 대문자로 시작하는 변수는 상수입니다.
Var = "I'm a constant"
Var = "can't be updated" # 오류: 이미 초기화된 상수 Var

# 클래스도 Crystal의 객체입니다. 따라서 클래스는 인스턴스 변수를 가질 수 있습니다.
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

# 모듈을 포함하면 해당 메서드가 클래스 인스턴스에 바인딩됩니다.
# 모듈을 확장하면 해당 메서드가 클래스 자체에 바인딩됩니다.

class Person
  include ModuleExample
end

class Book
  extend ModuleExample
end

Person.foo     # => Person:Class에 대한 정의되지 않은 메서드 'foo'
Person.new.foo # => 'foo'
Book.foo       # => 'foo'
Book.new.foo   # => Book에 대한 정의되지 않은 메서드 'foo'


# 예외 처리

# 새 예외 정의
class MyException < Exception
end

# 다른 예외 정의
class MyAnotherException < Exception; end

ex = begin
   raise MyException.new
rescue ex1 : IndexError
  "ex1"
rescue ex2 : MyException | MyAnotherException
  "ex2"
rescue ex3 : Exception
  "ex3"
rescue ex4 # 모든 종류의 예외를 잡습니다.
  "ex4"
end

ex #=> "ex2"
```

## 추가 자료

- [공식 문서](https://crystal-lang.org/)