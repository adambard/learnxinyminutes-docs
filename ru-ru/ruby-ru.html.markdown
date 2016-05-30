---
language: ruby
lang: ru-ru
filename: learnruby-ru.rb
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
  - ["Tristan Hume", "http://thume.ca/"]
  - ["Nick LaMuro", "https://github.com/NickLaMuro"]
translators:
  - ["Alexey Makarov", "https://github.com/Anakros"]
---

```ruby
# Это комментарий

=begin
Это многострочный комментарий
Никто их не использует
И они не рекомендуются к использованию
=end

# Первое и самое главное: Всё является объектом.

# Числа это объекты

3.class #=> Fixnum

3.to_s #=> "3"


# Немного простой арифметики
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Арифметика -- это синтаксический сахар
# над вызовом метода для объекта
1.+(3) #=> 4
10.* 5 #=> 50

# Логические величины -- это объекты
nil # Здесь ничего нет
true # истина
false # ложь

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Операция равенства
1 == 1 #=> true
2 == 1 #=> false

# Операция неравенства
1 != 1 #=> false
2 != 1 #=> true
!true  #=> false
!false #=> true

# nil -- имеет такое же логическое значение, как и false

!nil   #=> true
!false #=> true
!0     #=> false

# Больше операций сравнения
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Строки -- это объекты

'Я строка'.class #=> String
"Я тоже строка".class #=> String

placeholder = "использовать интерполяцию строк"
"Я могу #{placeholder}, когда создаю строку с двойными кавычками"
#=> "Я могу использовать интерполяцию строк,
# когда создаю строку с двойными кавычками"


# печатать в стандартный вывод
puts "Я печатаюсь!"

# Переменные
x = 25 #=> 25
x #=> 25

# Присваивание значения возвращает то самое присвоенное значение.
# Это позволяет делать множественные присваивания:

x = y = 10 #=> 10
x #=> 10
y #=> 10

# По соглашению, используйте snake_case для имён переменных
snake_case = true

# Используйте подробные имена для переменных
# Но не переборщите!
path_to_project_root = '/good/name/'
path = '/bad/name/'

# Идентификаторы (тоже объекты)

# Идентификаторы -- это неизменяемые, многоразовые константы. 
# Для каждого идентификатора (кроме текста) сохраняется цифровой хэш.
# При последующем использовании идентификатора, заместо создания нового объекта,
# будет найден уже существующий по цифровому хэшу.
# Они часто используются вместо строк для ускорения работы приложений

:pending.class #=> Symbol

status = :pending

status == :pending #=> true

status == 'pending' #=> false

status == :approved #=> false

# Массивы

# Это массив
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Массив может содержать различные типы значений

[1, "hello", false] #=> [1, "hello", false]

# Значение в массиве можно получить по индексу с левой границы
array[0] #=> 1
array[12] #=> nil

# Как и арифметика, доступ к значению в массиве
# это синтаксический сахар над вызовом метода для объекта
array.[] 0 #=> 1
array.[] 12 #=> nil

# Также, можно получить по индексу с правой границы
array[-1] #=> 5

# С заданными левой и правой границами индексов
array[2, 4] #=> [3, 4, 5]

# Или с использованием диапазона значений
array[1..3] #=> [2, 3, 4]

# Вот так можно добавить значение в массив
array << 6 #=> [1, 2, 3, 4, 5, 6]

# Хэши -- это массив пар "ключ => значение".
# Хэши объявляются с использованием фигурных скобок:
hash = {'color' => 'green', 'number' => 5}

hash.keys #=> ['color', 'number']
hash.values #=> ['green', 5]

# Значение в хэше легко может быть найдено по ключу:
hash['color'] #=> 'green'
hash['number'] #=> 5

# Поиск по ключу, которого в хэше нет вернёт nil:
hash['nothing here'] #=> nil

# начиная с Ruby 1.9, существует специальный синтаксис 
# при использовании идентификаторов как ключей хэша:

new_hash = { defcon: 3, action: true}

new_hash.keys #=> [:defcon, :action]

# Массивы и Хэши -- перечисляемые типы данных
# У них есть много полезных методов, например: each, map, count и другие

# Управление ходом выполнения (Управляющие структуры)

if true
  "Если истина"
elsif false
  "Иначе, если ложь (опционально)"
else
  "Во всех других случаях"
end

for counter in 1..5
  puts "итерация #{counter}"
end
#=> итерация 1
#=> итерация 2
#=> итерация 3
#=> итерация 4
#=> итерация 5

# Однако, никто не использует "for" для циклов.
# Вместо него Вы должны использовать метод "each" вместе с блоком кода.
#
# Блок кода -- это один из вариантов создания замыканий (лямбды,
# анонимные функции).
# Блок может только передаваться методу, сам по себе он существовать не может.
# "for" не имеет своей области видимости и все переменные, объявленные в нём
# будут доступны отовсюду. "each" вместе с блоком создаёт свою область видимости

# Метод "each" для диапазона значений запускает блок кода один раз
# для каждого из значений диапазона
# Блок передаёт счётчик (counter) в качестве параметра.
# Вызов метода "each" с блоком выглядит следующим образом:

(1..5).each do |counter|
  puts "итерация #{counter}"
end
#=> итерация 1
#=> итерация 2
#=> итерация 3
#=> итерация 4
#=> итерация 5

# Вы также можете ограничивать блоки фигурными скобками:
(1..5).each {|counter| puts "итерация #{counter}"}

# Содержимое структурных данных также можно перебирать используя "each":
array.each do |element|
  puts "#{element} -- часть массива"
end
hash.each do |key, value|
  puts "#{key} -- это #{value}"
end

counter = 1
while counter <= 5 do
  puts "итерация #{counter}"
  counter += 1
end
#=> итерация 1
#=> итерация 2
#=> итерация 3
#=> итерация 4
#=> итерация 5

grade = 'B'

case grade
when 'A'
  puts "Так держать, детка!"
when 'B'
  puts "Тебе повезёт в следующий раз"
when 'C'
  puts "Ты можешь сделать лучше"
when 'D'
  puts "Выскоблил последнее"
when 'F'
  puts "Ты провалился!"
else
  puts "Альтернативная система оценок, да?"
end

# Функции

def double(x)
  x * 2
end

# Функции (и все блоки) неявно возвращают значение последней операции
double(2) #=> 4

# Скобки необязательны, если возвращаемый результат однозначен
double 3 #=> 6

double double 3 #=> 12

def sum(x,y)
  x + y
end

# Аргументы метода разделены запятой
sum 3, 4 #=> 7

sum sum(3,4), 5 #=> 12

# yield
# Все методы имеют неявный, опциональный параметр,
# который может быть вызван с помощью инструкции "yield"

def surround
  puts "{"
  yield
  puts "}"
end

surround { puts 'hello world' }

# {
# hello world
# }


# Определение класса с помощью ключевого слова "class"
class Human

  # Переменная класса, она является общей для всех экземпляров класса
  @@species = "H. sapiens"

  # Базовый метод-конструктор
  def initialize(name, age=0)
    # Присвоить аргумент "name" переменной "name" экземпляра класса
    @name = name
    # Если аргумент "age" не задан,
    # мы используем значение по умолчанию из списка аргументов
    @age = age
  end

  # Базовый метод установки значения для переменной (setter)
  def name=(name)
    @name = name
  end

  # Базовый метод получения значения переменной (getter)
  def name
    @name
  end

  # Метод класса определяется с ключевым словом "self",
  # чтобы можно было отличить его от метода экземпляра класса.
  # Он может быть вызван только на уровне класса, но не экземпляра.
  def self.say(msg)
    puts "#{msg}"
  end

  def species
    @@species
  end

end


# Создание экземпляра класса
jim = Human.new("Jim Halpert")

dwight = Human.new("Dwight K. Schrute")

# Давайте вызовем несколько методов
jim.species #=> "H. sapiens"
jim.name #=> "Jim Halpert"
jim.name = "Jim Halpert II" #=> "Jim Halpert II"
jim.name #=> "Jim Halpert II"
dwight.species #=> "H. sapiens"
dwight.name #=> "Dwight K. Schrute"

# Вызов метода класса
Human.say("Hi") #=> "Hi"

# Область видимости переменной определяется тем, как мы даём имя переменной.
# Переменные, имя которых начинается с "$" имеют глобальную область видимости
$var = "I'm a global var"
defined? $var #=> "global-variable"

# Переменная экземпляра класса, она видна только в экземпляре
@var = "I'm an instance var"
defined? @var #=> "instance-variable"

# Переменная класса, видна для всех экземпляров этого класса и в самом классе
@@var = "I'm a class var"
defined? @@var #=> "class variable"

# Имена переменных с большой буквы используются для создания констант
Var = "I'm a constant"
defined? Var #=> "constant"

# Класс тоже объект в Ruby. Класс может иметь переменные экземпляра.
# Переменная класса доступна в классе, его экземплярах и его потомках.

# Пример класса
class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

#  Производный класс (класс-потомок)
class Worker < Human
end

Human.foo # 0
Worker.foo # 0

Human.foo = 2 # 2
Worker.foo # 2

# Переменная экземпляра класса недоступна в потомках этого класса.

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

# Включение модулей в класс добавляет их методы в экземпляр класса
# Или в сам класс, зависит только от метода подключения
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

# Коллбэки при подключении модуля

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
