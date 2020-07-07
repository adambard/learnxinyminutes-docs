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
  - ["Vasiliy Petrov", "https://github.com/Saugardas"]
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
2**5 #=> 32
5 % 3 #=> 2

# Побитовые операторы
3 & 5 #=> 1
3 | 5 #=> 7
3 ^ 5 #=> 6

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

# nil -- имеет такое же логическое значение, как и false

!nil   #=> true
!false #=> true
!0     #=> false

# Больше операций сравнения
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Оператор сравнения <=>
1 <=> 10 #=> -1
10 <=> 1 #=> 1
1 <=> 1 #=> 0

# Булевы операторы
true && false #=> false
true || false #=> true
!true #=> false

# Существуют альтернативные версии логических операторов с гораздо меньшим
# приоритетом. Они используются для связывания операций, пока одна из них
# не вернёт false или true

# `do_something_else` будет вызван если `do_something` вернёт истинное значение
do_something() and do_something_else()
# `log_error` будет вызван если `do_something` вернёт (nil/false)
do_something() or log_error()


# Строки -- это объекты

'Я строка'.class #=> String
"Я тоже строка".class #=> String

placeholder = "использовать интерполяцию строк"
"Я могу #{placeholder}, когда создаю строку с двойными кавычками"
#=> "Я могу использовать интерполяцию строк,
# когда создаю строку с двойными кавычками"

# Конкатенация строк
'hello ' + 'world' #=> "hello world"
'hello ' + 3 #=> TypeError: can't convert Fixnum into String
'hello ' + 3.to_s #=> "hello 3"

# Умножение строк
'hello ' * 3 #=> "hello hello hello "

# Добавление к строке
'hello' << ' world' #=> "hello world"

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
array.first #=> 1
array[12] #=> nil

# Как и арифметика, доступ к значению в массиве
# это синтаксический сахар над вызовом метода для объекта
array.[] 0 #=> 1
array.[] 12 #=> nil

# Также, можно получить по индексу с правой границы
array[-1] #=> 5
array.last #=> 5

# Задавая индекс и количество элементов
array[0,2] #=> [1, 2]
array[0,999] #=> [1, 2, 3, 4, 5]

# Или с использованием диапазона значений
array[1..3] #=> [2, 3, 4]

# Перестановка элементов в обратном порядке
a = [1, 2, 3]
a.reverse #=> [3, 2, 1]

# Вот так можно добавить значение в массив
array << 6 #=> [1, 2, 3, 4, 5, 6]
# Или так
array.push(6) #=> [1, 2, 3, 4, 5, 6]

# Проверка включения элемента в массив
array.include?(1) #=> true

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

# Проверка существования ключа и значения в хеше
new_hash.key?(:defcon) #=> true
new_hash.value?(3) #=> true

# Массивы и Хэши -- перечисляемые типы данных
# У них есть много полезных методов, например: each, map, count и другие

# Управление ходом выполнения (Управляющие структуры)

# Условия
if true
  'Если истина'
elsif false
  'Иначе, если ложь (опционально)'
else
  'Во всех других случаях (тоже опционально)'
end

# Если условие контролирует выполнение не блока кода, а единственного выражения,
# можно использовать постфиксную запись условного оператора
warnings = ['Отсутствует отчество', 'Слишком короткий адрес']
puts("Обратите внимание:\n" + warnings.join("\n"))  if !warnings.empty?

# Иногда условие лучше звучит с `unless`, чем с `if`
puts("Обратите внимание:\n" + warnings.join("\n"))  unless warnings.empty?

# Циклы
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
(1..5).each { |counter| puts "итерация #{counter}" }

# Содержимое структурных данных также можно перебирать используя "each":
array.each do |element|
  puts "#{element} -- часть массива"
end
hash.each do |key, value|
  puts "#{key} -- это #{value}"
end

# Если вам нужен индекс вы можете использовать "each_with_index"
# В этом случае индекс будет начинаться с 0
array.each_with_index do |element, index|
  puts "#{element} is number #{index} in the array"
end

# Если индекс должен начинаться с произвольного значения,
# используйте "each.with_index"
[:q, :w, :e].each.with_index(100) do |element, index|
  puts "#{element} -> #{index}"
end
#=> :q -> 100
#=> :w -> 101
#=> :e -> 102

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

# Существует большое количество других полезных функций,
# например "map", "reduce", "inject", и так далее. Например, "map"
# выполняет связанный с ним блок для каждого элемента перечисляемого объекта,
# возвращая массив результатов.
array = [1, 2, 3, 4, 5]
doubled = array.map do |element|
  element * 2
end
puts doubled
#=> [2, 4, 6, 8, 10]
puts array
#=> [1, 2, 3, 4, 5]

grade = 'B'

case grade
when 'A'
  puts 'Так держать, детка!'
when 'B'
  puts 'Тебе повезёт в следующий раз'
when 'C'
  puts 'Ты можешь сделать лучше'
when 'D'
  puts 'Выскоблил последнее'
when 'F'
  puts 'Ты провалился!'
else
  puts 'Альтернативная система оценок, да?'
end
#=> 'Тебе повезёт в следующий раз'

# в when также можно использовать диапазоны
grade = 82
case grade
when 90..100
  puts 'Ура!'
when 80...90
  puts 'Хорошая работа!'
else
  puts 'Вы не справились!'
end
#=> 'Хорошая работа!'

# Обработка исключений
begin
  # здесь код, который может вызвать исключение
  raise NoMemoryError, 'У вас закончилась память.'
rescue NoMemoryError => exception_variable
  puts 'Был вызван NoMemoryError', exception_variable
rescue RuntimeError => other_exception_variable
  puts 'Был вызван RuntimeError'
else
  puts 'Этот код будет выполнятся, если исключения не были вызваны'
ensure
  puts 'Этот код выполняется всегда'
end
#=> Был вызван NoMemoryError
#=> У вас закончилась память.
#=> Этот код выполняется всегда

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


# Вы можете передать блок методу
# "&" отмечает ссылку на переданный блок
def guests(&block)
  block.call 'some_argument'
end

# Чтобы метод принимал произвольное количество аргументов, спереди
# одного из параметров ставится префикс "*"
def method(first, *rest)
  p rest
end
method(1, 2, 3, 4) #=> [2, 3, 4]

# Если метод возвращает массив. можно использовать множественное присваивание
def foods
  ['pancake', 'sandwich', 'quesadilla']
end
breakfast, lunch, dinner = foods
breakfast #=> 'pancake'
dinner #=> 'quesadilla'

# По соглашению, все методы, возвращающие булево значение
# оканчиваются символом "?"
5.even? #=> false
5.odd? #=> true

# Если метод заканчивается восклицательным знаком, значит он делает что-то
# опасное или необратимое, например изменяет внутреннее состояние объекта.
# Многие из таких методов-мутаторов часто имеют "безопасную" версию без "!"
# которая возвращает новое значение
company_name = "Dunder Mifflin"
company_name.upcase #=> "DUNDER MIFFLIN"
company_name #=> "Dunder Mifflin"
company_name.upcase! # Изменяем зачение company_name!
company_name #=> "DUNDER MIFFLIN"


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

  # Тоже самое можно определить с помощью attr_accessor
  attr_accessor :name

  # Также можно создать методы только для записи или чтения
  attr_reader :name
  attr_writer :name

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
