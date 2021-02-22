---
language: crystal
filename: learncrystal-ru.cr
contributors:
    - ["Vitalii Elenhaupt", "http://veelenga.com"]
    - ["Arnaud Fernandés", "https://github.com/TechMagister/"]
translators:
    - ["Den Patin", "https://github.com/denpatin"]
lang: ru-ru
---

```crystal
# — так начинается комментарий


# Всё является объектом
nil.class  #=> Nil
100.class  #=> Int32
true.class #=> Bool

# Возвращают false только nil, false и пустые указатели
!nil   #=> true  : Bool
!false #=> true  : Bool
!0     #=> false : Bool


# Целые числа

1.class #=> Int32

# Четыре типа целых чисел со знаком
1_i8.class  #=> Int8
1_i16.class #=> Int16
1_i32.class #=> Int32
1_i64.class #=> Int64

# Четыре типа целых чисел без знака
1_u8.class  #=> UInt8
1_u16.class #=> UInt16
1_u32.class #=> UInt32
1_u64.class #=> UInt64

2147483648.class          #=> Int64
9223372036854775808.class #=> UInt64

# Двоичные числа
0b1101 #=> 13 : Int32

# Восьмеричные числа
0o123 #=> 83 : Int32

# Шестнадцатеричные числа
0xFE012D #=> 16646445 : Int32
0xfe012d #=> 16646445 : Int32

# Числа с плавающей точкой

1.0.class #=> Float64

# Два типа чисел с плавающей запятой
1.0_f32.class #=> Float32
1_f32.class   #=> Float32

1e10.class    #=> Float64
1.5e10.class  #=> Float64
1.5e-7.class  #=> Float64


# Символьные литералы

'a'.class #=> Char

# Восьмеричный код символа
'\101' #=> 'A' : Char

# Код символа Unicode
'\u0041' #=> 'A' : Char


# Строки

"s".class #=> String

# Строки неизменяемы
s = "hello, "  #=> "hello, "        : String
s.object_id    #=> 134667712        : UInt64
s += "Crystal" #=> "hello, Crystal" : String
s.object_id    #=> 142528472        : UInt64

# Поддерживается интерполяция строк
"sum = #{1 + 2}" #=> "sum = 3" : String

# Поддерживается многострочность
"This is
   multiline string"

# Строка с двойными кавычками
%(hello "world") #=> "hello \"world\""


# Символы — константы без значения, определяемые только именем. Часто
# используются вместо часто используемых строк для лучшей производительности.
# На внутреннем уровне они представлены как Int32.

:symbol.class #=> Symbol

sentence = :question?     # :"question?" : Symbol

sentence == :question?    #=> true  : Bool
sentence == :exclamation! #=> false : Bool
sentence == "question?"   #=> false : Bool


# Массивы

[1, 2, 3].class         #=> Array(Int32)
[1, "hello", 'x'].class #=> Array(Int32 | String | Char)

# При объявлении пустого массива необходимо указать тип его элементов
[]               # Syntax error: for empty arrays use '[] of ElementType'
[] of Int32      #=> [] : Array(Int32)
Array(Int32).new #=> [] : Array(Int32)

# Элементы внутри массива имеют свои индексы
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5] : Array(Int32)
array[0]                #=> 1               : Int32
array[10]               # raises IndexError
array[-6]               # raises IndexError
array[10]?              #=> nil             : (Int32 | Nil)
array[-6]?              #=> nil             : (Int32 | Nil)

# Можно получать элементы по индексу с конца
array[-1] #=> 5

# С начала и с указанием размера итогового массива
array[2, 3] #=> [3, 4, 5]

# Или посредством указания диапазона
array[1..3] #=> [2, 3, 4]

# Добавление в массив
array << 6  #=> [1, 2, 3, 4, 5, 6]

# Удаление элемента из конца массива
array.pop #=> 6
array     #=> [1, 2, 3, 4, 5]

# Удаление элемента из начала массива
array.shift #=> 1
array       #=> [2, 3, 4, 5]

# Проверка на наличие элемента в массиве
array.includes? 3 #=> true

# Синтаксический сахар для массива строк и символов
%w(one two three) #=> ["one", "two", "three"] : Array(String)
%i(one two three) #=> [:one, :two, :three]    : Array(Symbol)

# Массивоподобный синтаксис используется и для других типов, только если для
# них определены методы .new и #<<
set = Set{1, 2, 3} #=> [1, 2, 3]
set.class          #=> Set(Int32)

# Вышеприведенное эквивалентно следующему
set = Set(typeof(1, 2, 3)).new
set << 1
set << 2
set << 3


# Хэши

{1 => 2, 3 => 4}.class   #=> Hash(Int32, Int32)
{1 => 2, 'a' => 3}.class #=> Hash(Int32 | Char, Int32)

# При объявлении пустого хэша необходимо указать типы ключа и значения
{}                     # Syntax error
{} of Int32 => Int32   # {}
Hash(Int32, Int32).new # {}

# Значения в хэше легко найти по ключу
hash = {"color" => "green", "number" => 5}
hash["color"]        #=> "green"
hash["no_such_key"]  #=> Missing hash key: "no_such_key" (KeyError)
hash["no_such_key"]? #=> nil

# Проверка наличия ключа в хэше
hash.has_key? "color" #=> true

# Синтаксический сахар для символьных и строковых ключей
{key1: 'a', key2: 'b'}     # {:key1 => 'a', :key2 => 'b'}
{"key1": 'a', "key2": 'b'} # {"key1" => 'a', "key2" => 'b'}

# Хэшеподобный синтаксис используется и для других типов, только если для них
# определены методы .new и #[]=
class MyType
  def []=(key, value)
    puts "do stuff"
  end
end

MyType{"foo" => "bar"}

# Вышеприведенное эквивалентно следующему
tmp = MyType.new
tmp["foo"] = "bar"
tmp


# Диапазоны

1..10                  #=> Range(Int32, Int32)
Range.new(1, 10).class #=> Range(Int32, Int32)

# Включающий и исключающий диапазоны
(3..5).to_a  #=> [3, 4, 5]
(3...5).to_a #=> [3, 4]

# Проверка на вхождение в диапазон
(1..8).includes? 2 #=> true


# Кортежи
# Неизменяемые последовательности фиксированного размера, содержащие,
# как правило, элементы разных типов

{1, "hello", 'x'}.class #=> Tuple(Int32, String, Char)

# Доступ к элементам осуществляется по индексу
tuple = {:key1, :key2}
tuple[1] #=> :key2
tuple[2] #=> syntax error : Index out of bound

# Элементы кортежей можно попарно присвоить переменным
a, b, c = {:a, 'b', "c"}
a #=> :a
b #=> 'b'
c #=> "c"


# Процедуры
# Указатели на функцию с необязательным содержимым (замыкание).
# Обычно создаётся с помощью специального литерала ->

proc = ->(x : Int32) { x.to_s }
proc.class # Proc(Int32, String)
# Или посредством метода .new
Proc(Int32, String).new { |x| x.to_s }

# Вызываются посредством метода .call
proc.call 10 #=> "10"


# Управляющие операторы

if true
  "if statement"
elsif false
  "else-if, optional"
else
  "else, also optional"
end

puts "if as a suffix" if true

# if как часть выражения
a = if 2 > 1
      3
    else
      4
    end

a #=> 3

# Тернарный if
a = 1 > 2 ? 3 : 4 #=> 4

# Оператор выбора
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


# Циклы

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

# Но лучше использовать each
(1..3).each do |index|
  puts "Index: #{index}"
end
# Index: 1
# Index: 2
# Index: 3

# Тип переменной зависит от типа выражения
if a < 3
  a = "hello"
else
  a = true
end
typeof a #=> (Bool | String)

if a && b
  # здесь гарантируется, что и a, и b — не nil
end

if a.is_a? String
  a.class #=> String
end


# Методы

def double(x)
  x * 2
end

# Методы (а также любые блоки) всегда возвращают значение последнего выражения
double(2) #=> 4

# Скобки можно опускать, если вызов метода не вносит двусмысленности
double 3 #=> 6

double double 3 #=> 12

def sum(x, y)
  x + y
end

# Параметры методов перечисляются через запятую
sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12


# yield

# У всех методов есть неявный необязательный параметр блока, который можно
# вызвать ключевым словом yield

def surround
  puts '{'
  yield
  puts '}'
end

surround { puts "hello world" }

# {
# hello world
# }

# Методу можно передать блок
# & — ссылка на переданный блок
def guests(&block)
  block.call "some_argument"
end

# Методу можно передать список параметров, доступ к ним будет как к массиву
# Для этого используется оператор *
def guests(*array)
  array.each { |guest| puts guest }
end

# Если метод возвращает массив, можно попарно присвоить значение каждого из его
# элементов переменным
def foods
    ["pancake", "sandwich", "quesadilla"]
end
breakfast, lunch, dinner = foods
breakfast #=> "pancake"
dinner    #=> "quesadilla"

# По соглашению название методов, возвращающих булево значение, должно
# оканчиваться вопросительным знаком
5.even? # false
5.odd?  # true

# Если название метода оканчивается восклицательным знаком, по соглашению это
# означает, что метод делает что-то необратимое, например изменяет получателя.
# Некоторые методы имеют две версии: "опасную" версию с !, которая что-то
# меняет, и "безопасную", которая просто возвращает новое значение
company_name = "Dunder Mifflin"
company_name.gsub "Dunder", "Donald"  #=> "Donald Mifflin"
company_name  #=> "Dunder Mifflin"
company_name.gsub! "Dunder", "Donald"
company_name  #=> "Donald Mifflin"


# Классы
# Определяются с помощью ключевого слова class

class Human

  # Переменная класса является общей для всех экземпляров этого класса
  @@species = "H. sapiens"

  # Объявление типа переменной name экземпляра класса
  @name : String

  # Базовый конструктор
  # Значением первого параметра инициализируем переменную @name.
  # То же делаем и со вторым параметром — переменная @age. В случае, если мы
  # не передаём второй параметр, для инициализации @age будет взято значение
  # по умолчанию (в данном случае — 0)
  def initialize(@name, @age = 0)
  end

  # Базовый метод установки значения переменной
  def name=(name)
    @name = name
  end

  # Базовый метод получения значения переменной
  def name
    @name
  end

  # Синтаксический сахар одновременно для двух методов выше
  property :name

  # А также по отдельности
  getter :name
  setter :name

  # Метод класса определяется ключевым словом self, чтобы его можно было
  # различить с методом экземпляра класса. Такой метод можно вызвать только
  # на уровне класса, а не экземпляра.
  def self.say(msg)
    puts msg
  end

  def species
    @@species
  end
end


# Создание экземпляра класса
jim = Human.new("Jim Halpert")

dwight = Human.new("Dwight K. Schrute")

# Вызов методов экземпляра класса
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# Вызов метода класса
Human.say("Hi") #=> выведет "Hi" и вернёт nil

# Переменные экземпляра класса (@) видно только в пределах экземпляра
class TestClass
  @var = "I'm an instance var"
end

# Переменные класса (@) видны как в экземплярах класса, так и в самом классе
class TestClass
  @@var = "I'm a class var"
end

# Переменные с большой буквы — это константы
Var = "I'm a constant"
Var = "can't be updated" # Error: already initialized constant Var

# Примеры

# Базовый класс
class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

# Класс-потомок
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

# Подключение модуля в класс добавляет его методы в экземпляр класса
# Расширение модуля добавляет его методы в сам класс

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


# Обработка исключений

# Создание пользовательского типа исключения
class MyException < Exception
end

# Ещё одного
class MyAnotherException < Exception; end

ex = begin
   raise MyException.new
rescue ex1 : IndexError
  "ex1"
rescue ex2 : MyException | MyAnotherException
  "ex2"
rescue ex3 : Exception
  "ex3"
rescue ex4 # без указания конкретного типа исключения будут "отлавливаться" все
  "ex4"
end

ex #=> "ex2"

```

## Дополнительная информация

### На русском

- [Официальная документация](http://ru.crystal-lang.org/docs/)

### На английском

- [Official Documentation](http://crystal-lang.org/)
