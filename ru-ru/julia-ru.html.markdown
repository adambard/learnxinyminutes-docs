---
language: Julia
contributors:
    - ["Leah Hanson", "http://leahhanson.us"]
translators:
    - ["Sergey Skovorodkin", "https://github.com/skovorodkin"]
filename: learnjulia-ru.jl
lang: ru-ru
---

Julia — гомоиконный функциональный язык программирования для технических расчётов.
Несмотря на полную поддержку гомоиконных макросов, функций первого класса и конструкций управления низкого уровня, этот язык так же прост в изучении и применении, как и Python.

Документ описывает текущую dev-версию Julia от 18-о октября 2013 года.

```ruby

# Однострочные комментарии начинаются со знака решётки.

####################################################
## 1. Примитивные типы данных и операторы
####################################################

# Всё в Julia — выражение.

# Простые численные типы
3 # => 3 (Int64)
3.2 # => 3.2 (Float64)
2 + 1im # => 2 + 1im (Complex{Int64})
2//3 # => 2//3 (Rational{Int64})

# Доступны все привычные инфиксные операторы
1 + 1 # => 2
8 - 1 # => 7
10 * 2 # => 20
35 / 5 # => 7.0
5 / 2 # => 2.5 # деление Int на Int всегда возвращает Float
div(5, 2) # => 2 # для округления к нулю используется div
5 \ 35 # => 7.0
2 ^ 2 # => 4 # возведение в степень
12 % 10 # => 2

# С помощью скобок можно изменить приоритет операций
(1 + 3) * 2 # => 8

# Побитовые операторы
~2 # => -3   # НЕ (NOT)
3 & 5 # => 1 # И (AND)
2 | 4 # => 6 # ИЛИ (OR)
2 $ 4 # => 6 # сложение по модулю 2 (XOR)
2 >>> 1 # => 1 # логический сдвиг вправо
2 >> 1  # => 1 # арифметический сдвиг вправо
2 << 1  # => 4 # логический/арифметический сдвиг влево

# Функция bits возвращает бинарное представление числа
bits(12345)
# => "0000000000000000000000000000000000000000000000000011000000111001"
bits(12345.0)
# => "0100000011001000000111001000000000000000000000000000000000000000"

# Логические значения являются примитивами
true
false

# Булевы операторы
!true # => false
!false # => true
1 == 1 # => true
2 == 1 # => false
1 != 1 # => false
2 != 1 # => true
1 < 10 # => true
1 > 10 # => false
2 <= 2 # => true
2 >= 2 # => true
# Сравнения можно объединять цепочкой
1 < 2 < 3 # => true
2 < 3 < 2 # => false

# Строки объявляются с помощью двойных кавычек — "
"This is a string."

# Символьные литералы создаются с помощью одинарных кавычек — '
'a'

# Строки индексируются как массивы символов
"This is a string"[1] # => 'T' # Индексы начинаются с единицы
# Индексирование не всегда правильно работает для UTF8-строк,
# поэтому рекомендуется использовать итерирование (map, for-циклы и т.п.).

# Для строковой интерполяции используется знак доллара ($):
"2 + 2 = $(2 + 2)" # => "2 + 2 = 4"
# В скобках можно использовать любое выражение языка.

# Другой способ форматирования строк — макрос printf
@printf "%d is less than %f" 4.5 5.3 # 5 is less than 5.300000

####################################################
## 2. Переменные и коллекции
####################################################

# Вывод
println("I'm Julia. Nice to meet you!")

# Переменные инициализируются без предварительного объявления
some_var = 5 # => 5
some_var # => 5

# Попытка доступа к переменной до инициализации вызывает ошибку
try
    some_other_var # => ERROR: some_other_var not defined
catch e
    println(e)
end

# Имена переменных начинаются с букв.
# После первого символа можно использовать буквы, цифры, 
# символы подчёркивания и восклицательные знаки.
SomeOtherVar123! = 6 # => 6

# Допустимо использование unicode-символов
☃ = 8 # => 8
# Это особенно удобно для математических обозначений
2 * π # => 6.283185307179586

# Рекомендации по именованию:
# * имена переменных в нижнем регистре, слова разделяются символом 
#   подчёркивания ('\_');
#
# * для имён типов используется CamelCase;
#
# * имена функций и макросов в нижнем регистре
#   без разделения слов символом подчёркивания;
#
# * имя функции, изменяющей переданные ей аргументы (in-place function),
#   оканчивается восклицательным знаком.

# Массив хранит последовательность значений, индексируемых с единицы до n:
a = Int64[] # => пустой массив Int64-элементов

# Одномерный массив объявляется разделёнными запятой значениями.
b = [4, 5, 6] # => массив из трёх Int64-элементов: [4, 5, 6]
b[1] # => 4
b[end] # => 6

# Строки двумерного массива разделяются точкой с запятой.
# Элементы строк разделяются пробелами.
matrix = [1 2; 3 4] # => 2x2 Int64 Array: [1 2; 3 4]

# push! и append! добавляют в список новые элементы
push!(a,1)     # => [1]
push!(a,2)     # => [1,2]
push!(a,4)     # => [1,2,4]
push!(a,3)     # => [1,2,4,3]
append!(a,b) # => [1,2,4,3,4,5,6]

# pop! удаляет из списка последний элемент
pop!(b)        # => возвращает 6; массив b снова равен [4,5]

# Вернём 6 обратно
push!(b,6)   # b снова [4,5,6].

a[1] # => 1 # индексы начинаются с единицы!

# Последний элемент можно получить с помощью end
a[end] # => 6

# Операции сдвига
shift!(a) # => 1 and a is now [2,4,3,4,5,6]
unshift!(a,7) # => [7,2,4,3,4,5,6]

# Восклицательный знак на конце названия функции означает,
# что функция изменяет переданные ей аргументы.
arr = [5,4,6] # => массив из 3 Int64-элементов: [5,4,6]
sort(arr) # => [4,5,6]; но arr равен [5,4,6]
sort!(arr) # => [4,5,6]; а теперь arr — [4,5,6]

# Попытка доступа за пределами массива выбрасывает BoundsError
try
    a[0] # => ERROR: BoundsError() in getindex at array.jl:270
    a[end+1] # => ERROR: BoundsError() in getindex at array.jl:270
catch e
    println(e)
end

# Вывод ошибок содержит строку и файл, где произошла ошибка,
# даже если это случилось в стандартной библиотеке.
# Если вы собрали Julia из исходных кодов, 
# то найти эти файлы можно в директории base.

# Создавать массивы можно из последовательности
a = [1:5] # => массив из 5 Int64-элементов: [1,2,3,4,5]

# Срезы
a[1:3] # => [1, 2, 3]
a[2:] # => [2, 3, 4, 5]
a[2:end] # => [2, 3, 4, 5]

# splice! удаляет элемент из массива
# Remove elements from an array by index with splice!
arr = [3,4,5]
splice!(arr,2) # => 4 ; arr теперь равен [3,5]

# append! объединяет списки
b = [1,2,3]
append!(a,b) # теперь a равен [1, 2, 3, 4, 5, 1, 2, 3]

# Проверка на вхождение
in(1, a) # => true

# Длина списка
length(a) # => 8

# Кортеж — неизменяемая структура.
tup = (1, 2, 3) # => (1,2,3) # кортеж (Int64,Int64,Int64).
tup[1] # => 1
try:
    tup[1] = 3 # => ERROR: no method setindex!((Int64,Int64,Int64),Int64,Int64)
catch e
    println(e)
end

# Многие функции над списками работают и для кортежей
length(tup) # => 3
tup[1:2] # => (1,2)
in(2, tup) # => true

# Кортежи можно распаковывать в переменные
a, b, c = (1, 2, 3) # => (1,2,3)  # a = 1, b = 2 и c = 3

# Скобки из предыдущего примера можно опустить
d, e, f = 4, 5, 6 # => (4,5,6)

# Кортеж из одного элемента не равен значению этого элемента
(1,) == 1 # => false
(1) == 1 # => true

# Обмен значений
e, d = d, e  # => (5,4) # d = 5, e = 4


# Словари содержат ассоциативные массивы
empty_dict = Dict() # => Dict{Any,Any}()

# Для создания словаря можно использовать литерал
filled_dict = ["one"=> 1, "two"=> 2, "three"=> 3]
# => Dict{ASCIIString,Int64}

# Значения ищутся по ключу с помощью оператора []
filled_dict["one"] # => 1

# Получить все ключи
keys(filled_dict)
# => KeyIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# Заметьте, словарь не запоминает порядок, в котором добавляются ключи.

# Получить все значения.
values(filled_dict)
# => ValueIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# То же касается и порядка значений.

# Проверка вхождения ключа в словарь
in(("one", 1), filled_dict) # => true
in(("two", 3), filled_dict) # => false
haskey(filled_dict, "one") # => true
haskey(filled_dict, 1) # => false

# Попытка обратиться к несуществующему ключу выбросит ошибку
try
    filled_dict["four"] # => ERROR: key not found: four in getindex at dict.jl:489
catch e
    println(e)
end

# Используйте метод get со значением по умолчанию, чтобы избежать этой ошибки
# get(dictionary,key,default_value)
get(filled_dict,"one",4) # => 1
get(filled_dict,"four",4) # => 4

# Для коллекций неотсортированных уникальных элементов используйте Set
empty_set = Set() # => Set{Any}()
# Инициализация множества
filled_set = Set(1,2,2,3,4) # => Set{Int64}(1,2,3,4)

# Добавление элементов
push!(filled_set,5) # => Set{Int64}(5,4,2,3,1)

# Проверка вхождения элементов во множество
in(2, filled_set) # => true
in(10, filled_set) # => false

# Функции для получения пересечения, объединения и разницы.
other_set = Set(3, 4, 5, 6) # => Set{Int64}(6,4,5,3)
intersect(filled_set, other_set) # => Set{Int64}(3,4,5)
union(filled_set, other_set) # => Set{Int64}(1,2,3,4,5,6)
setdiff(Set(1,2,3,4),Set(2,3,5)) # => Set{Int64}(1,4)


####################################################
## 3. Поток управления
####################################################

# Создадим переменную
some_var = 5

# Выражение if. Отступы не имеют значения.
if some_var > 10
    println("some_var is totally bigger than 10.")
elseif some_var < 10    # Необязательная ветка elseif.
    println("some_var is smaller than 10.")
else                    # else-ветка также опциональна.
    println("some_var is indeed 10.")
end
# => prints "some var is smaller than 10"


# Цикл for проходит по итерируемым объектам
# Примеры итерируемых типов: Range, Array, Set, Dict и String.
for animal=["dog", "cat", "mouse"]
    println("$animal is a mammal")
    # Для вставки значения переменной или выражения в строку используется $
end
# Выведет:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

# Другой вариант записи.
for animal in ["dog", "cat", "mouse"]
    println("$animal is a mammal")
end
# Выведет:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

for a in ["dog"=>"mammal","cat"=>"mammal","mouse"=>"mammal"]
    println("$(a[1]) is a $(a[2])")
end
# Выведет:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

for (k,v) in ["dog"=>"mammal","cat"=>"mammal","mouse"=>"mammal"]
    println("$k is a $v")
end
# Выведет:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

# Цикл while выполняется до тех пор, пока верно условие
x = 0
while x < 4
    println(x)
    x += 1  # Короткая запись x = x + 1
end
# Выведет:
#   0
#   1
#   2
#   3

# Обработка исключений
try
   error("help")
catch e
   println("caught it $e")
end
# => caught it ErrorException("help")


####################################################
## 4. Функции
####################################################

# Для определения новой функции используется ключевое слово 'function'
#function имя(аргументы)
#  тело...
#end
function add(x, y)
    println("x is $x and y is $y")

    # Функция возвращает значение последнего выражения
    x + y
end

add(5, 6) # => Вернёт 11, напечатав "x is 5 and y is 6"

# Функция может принимать переменное количество позиционных аргументов.
function varargs(args...)
    return args
    # для возвращения из функции в любом месте используется 'return'
end
# => varargs (generic function with 1 method)

varargs(1,2,3) # => (1,2,3)

# Многоточие (...) — это splat.
# Мы только что воспользовались им в определении функции.
# Также его можно использовать при вызове функции,
# где он преобразует содержимое массива или кортежа в список аргументов.
Set([1,2,3])    # => Set{Array{Int64,1}}([1,2,3]) # формирует множество массивов
Set([1,2,3]...) # => Set{Int64}(1,2,3) # эквивалентно Set(1,2,3)

x = (1,2,3)     # => (1,2,3)
Set(x)          # => Set{(Int64,Int64,Int64)}((1,2,3)) # множество кортежей
Set(x...)       # => Set{Int64}(2,3,1)


# Опциональные позиционные аргументы
function defaults(a,b,x=5,y=6)
    return "$a $b and $x $y"
end

defaults('h','g') # => "h g and 5 6"
defaults('h','g','j') # => "h g and j 6"
defaults('h','g','j','k') # => "h g and j k"
try
    defaults('h') # => ERROR: no method defaults(Char,)
    defaults() # => ERROR: no methods defaults()
catch e
    println(e)
end

# Именованные аргументы
function keyword_args(;k1=4,name2="hello") # обратите внимание на ;
    return ["k1"=>k1,"name2"=>name2]
end

keyword_args(name2="ness") # => ["name2"=>"ness","k1"=>4]
keyword_args(k1="mine") # => ["k1"=>"mine","name2"=>"hello"]
keyword_args() # => ["name2"=>"hello","k2"=>4]

# В одной функции можно совмещать все виды аргументов
function all_the_args(normal_arg, optional_positional_arg=2; keyword_arg="foo")
    println("normal arg: $normal_arg")
    println("optional arg: $optional_positional_arg")
    println("keyword arg: $keyword_arg")
end

all_the_args(1, 3, keyword_arg=4)
# Выведет:
#   normal arg: 1
#   optional arg: 3
#   keyword arg: 4

# Функции в Julia первого класса 
function create_adder(x)
    adder = function (y)
        return x + y
    end
    return adder
end

# Анонимная функция
(x -> x > 2)(3) # => true

# Эта функция идентичная предыдущей версии create_adder
function create_adder(x)
    y -> x + y
end

# Если есть желание, можно воспользоваться полным вариантом
function create_adder(x)
    function adder(y)
        x + y
    end
    adder
end

add_10 = create_adder(10)
add_10(3) # => 13


# Встроенные функции высшего порядка
map(add_10, [1,2,3]) # => [11, 12, 13]
filter(x -> x > 5, [3, 4, 5, 6, 7]) # => [6, 7]

# Списковые сборки
[add_10(i) for i=[1, 2, 3]] # => [11, 12, 13]
[add_10(i) for i in [1, 2, 3]] # => [11, 12, 13]

####################################################
## 5. Типы
####################################################

# Julia has a type system.
# Каждое значение имеет тип, но переменные не определяют тип значения.
# Функция `typeof` возвращает тип значения.
typeof(5) # => Int64

# Types are first-class values
# Типы являются значениями первого класса
typeof(Int64) # => DataType
typeof(DataType) # => DataType
# Тип DataType представляет типы, включая себя самого.

# Типы используются в качестве документации, для оптимизации и организации.
# Статически типы не проверяются.

# Пользователь может определять свои типы
# Типы похожи на структуры в других языках
# Новые типы определяются с помощью ключевого слова `type`

# type Name
#   field::OptionalType
#   ...
# end
type Tiger
  taillength::Float64
  coatcolor # отсутствие типа равносильно `::Any`
end

# Аргументы конструктора по умолчанию — свойства типа
# в порядке их определения.
tigger = Tiger(3.5,"orange") # => Tiger(3.5,"orange")

# Тип объекта по сути является конструктором значений такого типа
sherekhan = typeof(tigger)(5.6,"fire") # => Tiger(5.6,"fire")

# Эти типы, похожие на структуры, называются конкретными.
# Можно создавать объекты таких типов, но не их подтипы.
# Другой вид типов — абстрактные типы.

# abstract Name
abstract Cat # просто имя и точка в иерархии типов

# Объекты абстрактных типов создавать нельзя, 
# но зато от них можно наследовать подтипы.
# Например, Number — это абстрактный тип.
subtypes(Number) # => 6 элементов в массиве Array{Any,1}:
                 #     Complex{Float16}
                 #     Complex{Float32}
                 #     Complex{Float64}
                 #     Complex{T<:Real}
                 #     ImaginaryUnit
                 #     Real
subtypes(Cat) # => пустой массив Array{Any,1}

# У всех типов есть супертип. Для его определения есть функция `super`.
typeof(5) # => Int64
super(Int64) # => Signed
super(Signed) # => Real
super(Real) # => Number
super(Number) # => Any
super(super(Signed)) # => Number
super(Any) # => Any
# Все эти типы, за исключением Int64, абстрактные.

# Для создания подтипа используется оператор <:
type Lion <: Cat # Lion — это подтип Cat
  mane_color
  roar::String
end

# У типа может быть несколько конструкторов.
# Для создания нового определите функцию с именем, как у типа,
# и вызовите имеющийся конструктор.
Lion(roar::String) = Lion("green",roar)
# Мы создали внешний (т.к. он находится вне определения типа) конструктор.

type Panther <: Cat # Panther — это тоже подтип Cat
  eye_color

  # Определим свой конструктор вместо конструктора по умолчанию
  Panther() = new("green")
end
# Использование внутренних конструкторов позволяет
# определять, как будут создаваться объекты типов.
# Но по возможности стоит пользоваться внешними конструкторами.

####################################################
## 6. Мультиметоды
####################################################

# Все именованные функции являются generic-функциями,
# т.е. все они состоят из разных методов.
# Каждый конструктор типа Lion — это метод generic-функции Lion.

# Приведём пример без использования конструкторов, создадим функцию meow

# Определения Lion, Panther и Tiger
function meow(animal::Lion)
  animal.roar # доступ к свойству типа через точку
end

function meow(animal::Panther)
  "grrr"
end

function meow(animal::Tiger)
  "rawwwr"
end

# Проверка
meow(tigger) # => "rawwr"
meow(Lion("brown","ROAAR")) # => "ROAAR"
meow(Panther()) # => "grrr"

# Вспомним иерархию типов
issubtype(Tiger,Cat) # => false
issubtype(Lion,Cat) # => true
issubtype(Panther,Cat) # => true

# Определим функцию, принимающую на вход объекты типа Cat
function pet_cat(cat::Cat)
  println("The cat says $(meow(cat))")
end

pet_cat(Lion("42")) # => выведет "The cat says 42"
try
    pet_cat(tigger) # => ERROR: no method pet_cat(Tiger,)
catch e
    println(e)
end

# В объектно-ориентированных языках распространена одиночная диспетчеризация —
# подходящий метод выбирается на основе типа первого аргумента.
# В Julia все аргументы участвуют в выборе нужного метода.

# Чтобы понять разницу, определим функцию с несколькими аргументами.
function fight(t::Tiger,c::Cat)
  println("The $(t.coatcolor) tiger wins!")
end
# => fight (generic function with 1 method)

fight(tigger,Panther()) # => выведет The orange tiger wins!
fight(tigger,Lion("ROAR")) # => выведет The orange tiger wins!

# Переопределим поведение функции, если Cat-объект является Lion-объектом
fight(t::Tiger,l::Lion) = println("The $(l.mane_color)-maned lion wins!")
# => fight (generic function with 2 methods)

fight(tigger,Panther()) # => выведет The orange tiger wins!
fight(tigger,Lion("ROAR")) # => выведет The green-maned lion wins!

# Драться можно не только с тиграми!
fight(l::Lion,c::Cat) = println("The victorious cat says $(meow(c))")
# => fight (generic function with 3 methods)

fight(Lion("balooga!"),Panther()) # => выведет The victorious cat says grrr
try
  fight(Panther(),Lion("RAWR")) # => ERROR: no method fight(Panther,Lion)
catch
end

# Вообще, пускай кошачьи могут первыми проявлять агрессию
fight(c::Cat,l::Lion) = println("The cat beats the Lion")
# => Warning: New definition
#    fight(Cat,Lion) at none:1
# is ambiguous with
#    fight(Lion,Cat) at none:2.
# Make sure
#    fight(Lion,Lion)
# is defined first.
#fight (generic function with 4 methods)

# Предупреждение говорит, что неясно, какой из методов вызывать:
fight(Lion("RAR"),Lion("brown","rarrr")) # => выведет The victorious cat says rarrr
# Результат может оказаться разным в разных версиях Julia

fight(l::Lion,l2::Lion) = println("The lions come to a tie")
fight(Lion("RAR"),Lion("brown","rarrr")) # => выведет The lions come to a tie


# Под капотом
# Язык позволяет посмотреть на сгенерированные ассемблерный и LLVM-код.

square_area(l) = l * l      # square_area (generic function with 1 method)

square_area(5) #25

# Что происходит, когда мы передаём функции square_area целое число?
code_native(square_area, (Int32,))  
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1              # Вводная часть
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    movsxd  RAX, EDI        # 
	#	    imul    RAX, RAX        # 
	#	    pop RBP                 #
	#	    ret                     #

code_native(square_area, (Float32,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vmulss  XMM0, XMM0, XMM0  # Произведение чисел одинарной точности (AVX)
	#	    pop RBP
	#	    ret

code_native(square_area, (Float64,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vmulsd  XMM0, XMM0, XMM0 # Произведение чисел двойной точности (AVX)
	#	    pop RBP
	#	    ret
	#	
# Если хотя бы один из аргументов является числом с плавающей запятой,
# то Julia будет использовать соответствующие инструкции.
# Вычислим площать круга
circle_area(r) = pi * r * r     # circle_area (generic function with 1 method)
circle_area(5)                  # 78.53981633974483

code_native(circle_area, (Int32,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vcvtsi2sd   XMM0, XMM0, EDI          # Загрузить целое число (r)
	#	    movabs  RAX, 4593140240              # Загрузить pi
	#	    vmulsd  XMM1, XMM0, QWORD PTR [RAX]  # pi * r
	#	    vmulsd  XMM0, XMM0, XMM1             # (pi * r) * r
	#	    pop RBP
	#	    ret
	#

code_native(circle_area, (Float64,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	    movabs  RAX, 4593140496
	#	Source line: 1
	#	    vmulsd  XMM1, XMM0, QWORD PTR [RAX]
	#	    vmulsd  XMM0, XMM1, XMM0
	#	    pop RBP
	#	    ret
	#
```

## Что дальше?

Для более подробной информации читайте [документацию по языку](http://docs.julialang.org/en/latest/manual/)

Если вам нужна помощь, задавайте вопросы в [списке рассылки](https://groups.google.com/forum/#!forum/julia-users).
