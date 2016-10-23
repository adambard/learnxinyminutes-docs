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
  - ["Gabriel Halley", "https://github.com/ghalley"]
  - ["Persa Zula", "http://persazula.com"]
  - ["Jake Faris", "https://github.com/farisj"]
translators:
  - ["Serhii Maksymchuk", "https://github.com/Serg-Maximchuk"]
lang: uk-ua
---

Rubi — це інтерпретована, повністю об'єктно-орієнтована мова програмування з чіткою динамічною типізацією.

```ruby
# Це коментар

=begin
Це багаторядковий коментар
Ніхто їх не використовує
Тобі теж не варто
=end

# В першу чергу: все являється об’єктом.

# Числа це об’єкти

3.class #=> Fixnum

3.to_s #=> "3"


# Базова арифметика
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2**5 #=> 32
5 % 3 #=> 2

# Побітові оператори
3 & 5 #=> 1
3 | 5 #=> 7
3 ^ 5 #=> 6

# Арифметика це просто синтаксичний цукор
# для виклику методу об’єкта
1.+(3) #=> 4
10.* 5 #=> 50

# Спеціальні значення теж об’єкти
nil # еквівалентно null в інших мовах
true # істина
false # хиба

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Рівність
1 == 1 #=> true
2 == 1 #=> false

# Нерівність
1 != 1 #=> false
2 != 1 #=> true

# Окрім власне false, nil це ще одне "хибне" значення

!nil   #=> true
!false #=> true
!0     #=> false

# Інші порівняння
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Комбінований оператор порівняння
1 <=> 10 #=> -1
10 <=> 1 #=> 1
1 <=> 1 #=> 0

# Логічні оператори
true && false #=> false
true || false #=> true
!true #=> false

# Є альтернативні версії логічних операторів з набагато меншим
# прецедентом. Вони використовуються в конструкціях з контролем
# виконання ланцюга виразів які повертають булевий результат.

# `do_something_else` викликається лише якщо `do_something` повертає true.
do_something() and do_something_else()
# `log_error` викликається лише якщо `do_something` повертає false.
do_something() or log_error()


# Strings це об’єкти

'Я — рядок'.class #=> String
"Я теж рядок".class #=> String

placeholder = 'використовувати інтерполяцію рядків'
"Я можу #{placeholder} коли користуюсь рядками в подвійних лапках"
#=> "Я можу використовувати інтерполяцію рядків коли користуюсь рядками в подвійних лапках"

# Варто надавати перевагу рядкам в одинарних лапках де це можливо
# Рядки в подвійних лапках викликають додаткові внутрішні обчислення

# Об’єднуйте рядки, але не з числами
'hello ' + 'world'  #=> "hello world"
'hello ' + 3 #=> TypeError: can't convert Fixnum into String
'hello ' + 3.to_s #=> "hello 3"

# Об’єднуйте рядки з операторами
'hello ' * 3 #=> "hello hello hello "

# Додавання до рядка
'hello' << ' world' #=> "hello world"

# Вивести рядок з переходом на новий рядок вкінці
puts "Я надрукований!"
#=> Я надрукований!
#=> nil

# Вивести рядок без переходу на новий
print "Я надрукований!"
#=> Я надрукований! => nil

# Змінні
x = 25 #=> 25
x #=> 25

# Зверніть увагу, оператор присвоєння повертає присвоєне значення
# Отже можна робити одночасне присвоєння кількох змінних:
x = y = 10 #=> 10
x #=> 10
y #=> 10

# Для назв змінних використовується зміїний_регістр
snake_case = true

# Використовуйте назви змінних які їх характеризують 
path_to_project_root = '/good/name/'
path = '/bad/name/'

# Символи (теж об’єкти)
# Символи є незмінними константами багаторазового використання, внутрішньо
# представлені цілочисельним значенням. Вони часто використовуються замість
# рядків щоб ефективно передати конкретні, значущі значення.

:pending.class #=> Symbol

status = :pending

status == :pending #=> true

status == 'pending' #=> false

status == :approved #=> false

# Масиви

# Ось масив
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Масиви можуть містити елементи різних типів
[1, 'hello', false] #=> [1, "hello", false]

# Масиви можуть бути проіндексовані
# З початку
array[0] #=> 1
array.first #=> 1
array[12] #=> nil

# Як і арифметика, доступ до елемента масиву у вигляді [індекс] — це лише
# синтаксичний цукор виклику методу [] об’єкта
array.[] 0 #=> 1
array.[] 12 #=> nil

# З кінця
array[-1] #=> 5
array.last #=> 5

# З початковим індексом та довжиною
array[2, 3] #=> [3, 4, 5]

# Реверс масиву
a=[1,2,3]
a.reverse! #=> [3,2,1]

# Елементи масиву за діапазоном індексів
array[1..3] #=> [2, 3, 4]

# Додавати елементи в масив можна так:
array << 6 #=> [1, 2, 3, 4, 5, 6]
# Або так:
array.push(6) #=> [1, 2, 3, 4, 5, 6]

# Перевірити чи масив містить елемент
array.include?(1) #=> true

# Хеш — це масив пар ключ/значення.
# Хеш оголошується з використанням фігурних дужок:
hash = { 'color' => 'green', 'number' => 5 }

hash.keys #=> ['color', 'number']

# Значення в хеші може бути швидко знайдене за ключем:
hash['color'] #=> 'green'
hash['number'] #=> 5

# Запит значення за неіснуючим ключем повертає nil:
hash['nothing here'] #=> nil

# Починаючи з Ruby 1.9 з’явився спеціальний синтаксис при використанні
# символів в якості ключів:
new_hash = { defcon: 3, action: true }

new_hash.keys #=> [:defcon, :action]

# Перевірка наявності ключів та значень в хеші
new_hash.key?(:defcon) #=> true
new_hash.value?(3) #=> true

# Хеші та масиви — перелічувальні типи даних
# Вони мають багато корисних методів, такі як each, map, count, та інші.

# Оператор вибору "if"
if true
  'якщо'
elsif false
  'інакше якщо, опціонально'
else
  'інакше, також опціонально'
end

# Оператор циклу "for"
for counter in 1..5
  puts "ітерація #{counter}"
end
#=> ітерація 1
#=> ітерація 2
#=> ітерація 3
#=> ітерація 4
#=> ітерація 5

# Проте, ніхто не використовує "for" в циклах.
# Замість цього варто використовувати метод "each" і передати йому блок.
# Блок — це відокремлений код який можна передати в метод, наприклад в "each".
# Це аналог лямбда-виразів, анонімних функцій або замикань в інших мовах програмування.

# Метод "each" для діапазону запускає блок один раз для кожного елементу діапазону.
# Лічильник передається блоку в якості аргументу.

# Виклик методу "each" з блоком виглядає наступним чином:
(1..5).each do |counter|
  puts "ітерація #{counter}"
end
#=> ітерація 1
#=> ітерація 2
#=> ітерація 3
#=> ітерація 4
#=> ітерація 5

# Також можна загорнути блок в фігурні дужки:
(1..5).each { |counter| puts "ітерація #{counter}" }

# Вміст структур даних також може бути ітерований використовуючи метод "each":
array.each do |element|
  puts "#{element} є елементом масиву"
end
hash.each do |key, value|
  puts "#{key} є #{value}"
end

# Якщо є необхідність індексувати ітерацію, можна використати метод "each_with_index":
array.each_with_index do |element, index|
  puts "#{element} під номером #{index} в масиві"
end

# Оператор циклу "while"
counter = 1
while counter <= 5 do
  puts "ітерація #{counter}"
  counter += 1
end
#=> ітерація 1
#=> ітерація 2
#=> ітерація 3
#=> ітерація 4
#=> ітерація 5

# Є й інші корисні функції для циклів, такі як "map", "reduce",
# "inject" та інші. Наприклад "map" в циклі проходить по масиву,
# виконує над елементами операцію(-ї) в блоці і повертає абсолютно
# новий масив.
array = [1,2,3,4,5]
doubled = array.map do |element|
  element * 2
end
puts doubled
#=> [2,4,6,8,10]
puts array
#=> [1,2,3,4,5]

# Оператор множинного вибору
grade = 'B'

case grade
when 'A'
  puts 'Відмінно!'
when 'B'
  puts 'Пощастить наступного разу'
when 'C'
  puts 'Ти можеш краще'
when 'D'
  puts 'Майже четвірка'
when 'E'
  puts 'Випросив'
when 'F'
  puts 'Не здав!'
else
  puts 'Інша система оцінювання, так?'
end
#=> "Пощастить наступного разу"

# Оператор "case" також може використовувати діапазон:
grade = 82
case grade
when 90..100
  puts 'Ура!'
when 80...90
  puts 'Хороша робота'
when 60...80
  puts 'Ну, хоч щось'
else
  puts 'Не здав!'
end
#=> "Хороша робота"

# Обробка вийнятків:
begin
  # код з можливим вийнятком
  raise NoMemoryError, 'Ви використали всю пам’ять.'
rescue NoMemoryError => exception_variable
  puts 'Помилка NoMemoryError', exception_variable
rescue RuntimeError => other_exception_variable
  puts 'Помилка RuntimeError'
else
  puts 'Цей код запуститься якщо вийнятків не було взагалі'
ensure
  puts 'Цей код запуститься в будь-якому випадку'
end

# Функції

def double(x)
  x * 2
end

# Функції (і всі блоки) неявно повертають значення останнього виразу
double(2) #=> 4

# Дужки не є обов’язковими якщо результат недвозначний
double 3 #=> 6

double double 3 #=> 12

def sum(x, y)
  x + y
end

# Аргументи методів розділяються комою
sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12

# yield
# Всі методи мають неявний, опціональний параметр, який
# можна викликат за допомогою ключового слова 'yield'
def surround
  puts '{'
  yield
  puts '}'
end

surround { puts 'привіт світ' }

# {
# привіт світ
# }


# Ви можете передати блок у функцію
# "&" позначає посилання на блок
def guests(&block)
  block.call 'some_argument'
end

# Ви можете передати список аргументів, вони будуть перетворені в масив
# Для цього існує оператор ("*")
def guests(*array)
  array.each { |guest| puts guest }
end

# Якщо метод повертає масив, ви можете використати деструктуризацію
def foods
    ['млинці', 'бутерброд', 'лазанья']
end
breakfast, lunch, dinner = foods
breakfast #=> 'млинці'
dinner #=> 'лазанья'

# Зазвичай методи які повертають булевий результат закінчуються знаком питання
5.even? # false
5.odd? # true

# І якщо метод закінчується знаком оклику, то він робить щось деструктивне 
# типу зміни отриманого аргументу. Багато методів мають версію з "!" які
# змінюють аргумент, та без "!" що повертають новий об’єкт
company_name = "Дандер Міфлін"
company_name.upcase #=> "ДАНДЕР МІФЛІН"
company_name #=> "Дандер Міфлін"
company_name.upcase! # змінна company_name зміниться цього разу!
company_name #=> "ДАНДЕР МІФЛІН"


# Клас оголошується ключовим словом class
class Human

  # Змінна класу. Вона поширюється на всі екземпляри цього класу.
  @@species = 'Homo sapiens'

  # Основний ініціалізатор
  def initialize(name, age = 0)
    # Призначення аргументу "name" до однойменної змінної екземпляру
    @name = name
    # Якщо аргумент "age" не заданий, то йому буде присвоєно дефолтне значення
    # зі списку аргументів
    @age = age
  end

  # Метод-сетер
  def name=(name)
    @name = name
  end

  # Метод-ґетер
  def name
    @name
  end

  # Функціональність вище може бути інкапсульована використовуючи метод attr_accessor:
  attr_accessor :name

  # Ґетери і сетери можуть бути створені індивідуально, наприклад:
  attr_reader :name
  attr_writer :name

  # Метод класу позначається ключовим словом "self", щоб відрізнити від
  # методів екземпляра класу.
  # Він може бути викликаний лише в класі, але не в екземплярі.
  def self.say(msg)
    puts msg
  end

  def species
    @@species
  end
end


# Ініціалізуємо клас
jim = Human.new('Джим Галперт')

dwight = Human.new('Дуайт Шрут')

# Викличемо кілька методів
jim.species #=> "Homo sapiens"
jim.name #=> "Джим Галперт"
jim.name = "Джим Галперт II" #=> "Джим Галперт II"
jim.name #=> "Джим Галперт II"
dwight.species #=> "Homo sapiens"
dwight.name #=> "Дуайт Шрут"

# Викликати метод класу
Human.say('Привіт') #=> "Привіт"

# Область видимості змінних визначається способом оголошення імені змінної.
# Змінні, що починаються на "$" мають глобальну область видимості.
$var = "Я глобальна змінна"
defined? $var #=> "global-variable"

# Зміннні, що опчинають на "@" мають область видимості екзкмпляра
@var = "Я змінна екземпляра"
defined? @var #=> "instance-variable"

# Змінні, що починаються на "@@" мають область видимості класу
@@var = "Я змінна класу"
defined? @@var #=> "class variable"

# Змінні, що починаються з великої букви, є константами
Var = "Я константа"
defined? Var #=> "constant"

# Клас теж об’єкт. Тому клас може мати змінні екземпляра.
# Змінна класу поширюється між класом і всіма його нащадками.

# Базовий клас
class Human
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

# Похідний клас (нащадок)
class Worker < Human
end

Human.foo # 0
Worker.foo # 0

Human.foo = 2 # 2
Worker.foo # 2

# Змінна екземпляра класу не поширюється між класами-нащадками.
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

# Включення модулів додає їхні методи до екземплярів класу
# Розширення модулів додає їхні методи в сам клас

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

# Колбек виконується при включенні і розширенні модуля

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

## Додаткові ресурси

- [Learn Ruby by Example with Challenges](http://www.learneroo.com/modules/61/nodes/338) - A variant of this reference with in-browser challenges.
- [An Interactive Tutorial for Ruby](https://rubymonk.com/) - Learn Ruby through a series of interactive tutorials.
- [Official Documentation](http://ruby-doc.org/core)
- [Ruby from other languages](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - An older [free edition](http://ruby-doc.com/docs/ProgrammingRuby/) is available online.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - A community-driven Ruby coding style guide.
- [Try Ruby](http://tryruby.org) - Learn the basic of Ruby programming language, interactive in the browser.
