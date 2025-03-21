---
filename: haskell.hs
contributors:
    - ["Adit Bhargava", "http://adit.io"]
translators:
    - ["Aleksey Pirogov", "http://astynax.github.io"]
---

Haskell разрабатывался, как чистый функциональный язык программирования, применимый на практике. Язык известен благодаря своей системе типов, и "знаменит" благодаря монадам. [Меня][author] же Haskell заставляет возвращаться к себе снова и снова именно своей элегантностью и [я][author] получаю истинное удовольствие, программируя на Haskell.

```haskell
-- Однострочные комментарии начинаются с двух дефисов
{- Многострочный комментарий
заключается в пару фигурных скобок с дефисами с внутренней стороны.
-}

-------------------------------------------------------
-- 1. Примитивные типы и простейшие операции над ними
-------------------------------------------------------

-- Числа объявляются просто
3 -- 3

-- Арифметика тоже выглядит вполне ожидаемо
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- Операция деления всегда возвращает действительное число
35 / 4 -- 8.75

-- Делим нацело так
35 `div` 4 -- 8

-- Булевы значения - тоже примитивные значения
True
False

-- Булева алгебра
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- В примере выше `not`, это функция, принимающая один аргумент.
-- При вызове функции в Haskell список аргументов
-- не нужно заключать в скобки - аргументы просто
-- перечисляются через пробелы сразу после имени функции.
-- Т.о. типичный вызов выглядит так:
-- func arg1 arg2 arg3...
-- Ниже же будет показано, как определять свои функции.

-- Строки и символы
"Это строка."
'ы' -- а это символ
'Нельзя заключать длинные строки в одинарные кавычки.' -- ошибка!

-- Строки можно конкатенировать
"Привет" ++ ", Мир!" -- "Привет, Мир!"

-- При этом строки - это просто списки символов!
"Я - строка!" !! 0 -- 'Я'


----------------------------------------------------
-- 2. Списки и Кортежи
----------------------------------------------------

-- Все элементы списка в Haskell
-- должны иметь один и тот же тип.

-- Эти два списка - эквивалентны:
[1, 2, 3, 4, 5]
[1..5]

-- Haskell позволяет определять даже бесконечные списки!
[1..] -- список всех натуральных чисел!

-- Бесконечные списки возможно в Haskell потому, что он "ленив".
-- В Haskell все вычисления производятся тогда и только тогда,
-- когда их результат потребуется.
-- Эта стратегия так и называется - "lazy evaluation".
-- Скажем, если вам нужен тысячный элемент из
-- списка натуральных чисел (бесконечного) и вы напишете так:

[1..] !! 999 -- 1000

-- То Haskell вычислит элементы этого списка от 1 до 1000...
-- ... и остановится, ведь последующие элементы пока не нужны.
-- Это значит, что остальные элементы нашего
-- "бесконечного" списка не будут вычисляться! По крайней мере,
-- пока не понадобятся и они.

-- Списки можно объединять
[1..5] ++ [6..10]

-- И добавлять значения в начало
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- А можно обратиться по индексу
[0..] !! 5 -- 5

-- Вот ещё несколько функций, часто используемых со списками
head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

-- list comprehensions - "формулы" для описания списков
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- можно указать условие попадания элементов в список
[x*2 | x <- [1..5], x*2 > 4] -- [6, 8, 10]

-- Списки могут даже состоять из других списков
[[1,2,3],[4,5,6]] !! 1 !! 2 -- 6 (вторая строка, третий столбец)

-- Кортежи позволяют своим элементам иметь различные типы,
-- но при этом кортежи имеют фиксированную длину.
-- Кортеж:
("haskell", 1)

-- Часто кортежи из двух элементов называются "парами".
-- Элементы пары можно получать так:
fst ("haskell", 1) -- "haskell"
snd ("haskell", 1) -- 1

----------------------------------------------------
-- 3. Функции
----------------------------------------------------
-- Простая функция, принимающая два аргумента
add a b = a + b

-- Внимание!
-- Если вы используете ghci (интерактивный интерпретатор Haskell),
-- вам нужно использовать ключевое слово `let`, примерно так:
-- let add a b = a + b

-- Вызовем нашу функцию
add 1 2 -- 3

-- Функцию можно поместить между первым и вторым аргументами,
-- если заключить её имя в обратные кавычки
1 `add` 2 -- 3

{- Вы можете также определять функции, имя которых
вообще не содержит букв! Таки функции и называются "операторами",
и, да, вы можете определять свои операторы!
Скажем, оператор целочисленного деления можно определить так -}
(//) a b = a `div` b
35 // 4 -- 8
{- Здесь оператор заключен в скобки - как говорят,
поставлен в префиксную позицию.
В префиксной позиции оператор можно не только определять,
но и вызывать -}
(+) 1 2 -- 3

-- Охранные выражения (guards) порой удобны,
-- если наша функция ветвится
fib x
  | x < 2 = x
  | otherwise = fib (x - 1) + fib (x - 2)

{- Сопоставление с образцом (pattern matching)
чем-то напоминает охранные выражения.
Здесь мы видим три определения функции fib.
При вызове функции по имени Haskell использует
первое определение, к образцу которого
"подойдет" набор аргументов -}
fib 1 = 1
fib 2 = 1
fib x = fib (x - 1) + fib (x - 2)

-- Pattern matching для кортежей выглядит так
foo (x, y) = (x + 1, y + 2)

{- Pattern matching для списков устроен чуть сложнее.
Пусть `x` - первый элемент списка, а `xs` - остальные элементы.
Тогда операции `head` и `tail` могут быть определены так -}
myHead (x:xs) = x
myTail (x:xs) = xs

-- Функцию отображения мы можем написать так
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

-- При сопоставлении происходит привязка
-- элементов значения с именами в образце
fstPlusThird (a : _ : b : _) = a + b
fstPlusThird [1,2,3,4,5] -- 4
-- Значения, для которых вместо имени указано `_`,
-- игнорируются. Это удобно, когда важен сам факт
-- совпадения образца
oneElem [_] = True
oneElem _ = False

startsWith x (y:_) = x == y
startsWith _ _ = False

startsWith 'H' "Hello!" -- True
startsWith 'H' "hello!" -- False

{- Обратите внимание на тот факт,
что первый аргумент нашей функции `myMap` - тоже функция!
Функции, подобно `myMap`, принимающие другие функции
в качестве параметров, или, скажем, возвращающие функции
в качестве результата, называются
Функциями Высших Порядков (ФВП, High Order Functions, HOF)
-}

-- Вместе с ФВП часто используются анонимные функции
myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]
-- Такие функции описываются в виде
-- \arg1 arg1 .. -> expression

-- Популярные в других языках ФВП присутствуют и в Haskell
map (\x -> x * 10) [1..5] -- [10, 20, 30, 40, 50]
filter (\x -> x > 2) [1..5] -- [3, 4, 5]

{- Функция свертки
(она же `reduce` или `inject` в других языках)
в Haskell представлены функциями `foldr` и `foldl`.
Суть свертки можно представить так:

foldl f x0 [x1,x2,x3] -> (f (f (f x0 x1) x2) x3)
foldr f x0 [x1,x2,x3] -> (f x1 (f x2 (f x3 x0)))

Здесь x0 - начальное значения так называемого "аккумулятора"
-}
-- Эти два вызова дают одинаковый результат
foldr (\x acc -> acc + x) 0 [1..5] -- 15
foldl (\acc x -> acc + x) 0 [1..5] -- 15
-- Тут можно даже заменить анонимную функцию на оператор
foldr (+) 0 [1..5] -- 15
foldl (+) 0 [1..5] -- 15

-- Зато здесь разница видна
foldr (\x acc -> (x + 10) : acc) [] [1..3] -- [11, 12, 13]
foldl (\acc x -> (x + 10) : acc) [] [1..3] -- [13, 12, 11]

{- Часто в качестве начального значения
удобно брать крайнее значение списка (крайнее слева или справа).
Для этого есть пара функций - `foldr1` и `foldl1`  -}
foldr1 (+) [1..5] -- 15
foldl1 (+) [1..5] -- 15

----------------------------------------------------
-- 4. Больше о функциях
----------------------------------------------------

{- Каррирование (currying)
Если в Haskell при вызове функции передать не все аргументы,
Функция становится "каррированой" - результатом вызова станет
новая функция, которая при вызове и примет оставшиеся аргументы -}

add a b = a + b
foo = add 10 -- теперь foo будет принимать число
             -- и добавлять к нему 10
foo 5 -- 15

-- Для операторов можно "опустить" любой из двух аргументов
-- Используя этот факт можно определить
-- функцию `foo` из кода выше несколько иначе
foo = (+10)
foo 5 -- 15

-- Поупражняемся
map (10-) [1..3] -- [9, 8, 7]
filter (<5) [1..10] -- [1, 2, 3, 4]

{- Композиция функций
Функция (.) соединяет пару функций в цепочку.
К примеру, можно соединить функцию, добавляющую 10,
с функцией, умножающей на 5 -}
foo = (*5) . (+10)

-- (5 + 10) * 5 = 75
foo 5 -- 75

{- Управление приоритетом вычисления
В Haskell есть функция `$`, которая применяет
свой первый аргумент ко второму с наименьшим приоритетом
(обычное применение функций имеет наивысший приоритет)
Эта функция часто позволяет избежать использования
"лишних" скобок -}
head (tail (tail "abcd")) -- 'c'
head $ tail $ tail "abcd" -- 'c'
-- того же эффекта иногда можно достичь использованием композиции
(head . tail . tail) "abcd" -- 'c'
head . tail . tail $ "abcd" -- 'c'
{- Тут стоит сразу запомнить, что композиция функций
возвращает именно новую функцию, как в последнем примере.
Т.е. можно делать так -}
third = head . tail . tail
-- но не так
third = head $ tail $ tail -- (head (tail (tail))) - ошибка!

----------------------------------------------------
-- 5. Сигнатуры типов
----------------------------------------------------

{- Haskell обладает очень сильной системой типов.
И типизация в Haskell - строгая. Каждое выражение имеет тип,
который может быть описан сигнатурой.
Сигнатура записывается в форме
expression :: type signature
-}

-- Типы примитивов
5 :: Integer
"hello" :: String
True :: Bool

{- Функции тоже имеют тип
`not` принимает булево значение и возвращает булев результат
not :: Bool -> Bool

Вот функция двух аргументов
add :: Integer -> Integer -> Integer

Тут то мы и видим предпосылки к каррированию: тип
на самом деле выглядит так (скобки просто обычно опускаются)
add :: (Integer -> Integer) -> Integer
т.е. функция принимает аргумент,
и возвращает функцию от второго аргумента! -}

-- Считается хорошим тоном указывать сигнатуру определений,
-- которые доступны другим разработчикам (публичны). Пример:
double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. Управление потоком исполнения
----------------------------------------------------

-- Выражение `if`
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- Выражение `if` можно записать и в несколько строк.
-- Соблюдайте отступы!
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- Так как `if` - выражение, ветка `else` обязательна!
-- И более того, результаты выражений в ветках `then` и `else`
-- должны иметь одинаковый тип!

-- `case`-выражение выглядит так
case args of -- парсим аргументы командной строки
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"

-- При вычислении результата `case`-выражения производится
-- сопоставление с образцом:
fib x = case x of
          1 -> 1
          2 -> 1
          _ -> fib (x - 1) + fib (x - 2)

-- В Haskell нет циклов - вместо них используются рекурсия,
-- отображение, фильтрация и свертка (map/filter/fold)
map (*2) [1..5] -- [2, 4, 6, 8, 10]

for array func = map func array
for [0..3] $ \i -> show i -- ["0", "1", "2", "3"]
for [0..3] show           -- ["0", "1", "2", "3"]

----------------------------------------------------
-- 7. Пользовательские типы данных
----------------------------------------------------

-- Создадим свой Haskell-тип данных

data Color = Red | Blue | Green

-- Попробуем использовать

say :: Color -> String
say Red   = "You are Red!"
say Blue  = "You are Blue!"
say Green = "You are Green!"

-- Типы могут иметь параметры (параметры типов)

data Maybe a = Nothing | Just a

-- Все эти выражения имеют тип `Maybe`
Just "hello"    -- :: `Maybe String`
Just 1          -- :: `Maybe Int`
Nothing         -- :: `Maybe a` для любого `a`

-- Типы могут быть достаточно сложными
data Figure = Rectangle (Int, Int) Int Int
            | Square (Int, Int) Int
            | Point (Int, Int)

area :: Figure -> Int
area (Point     _)     = 0
area (Square    _ s)   = s * s
area (Rectangle _ w h) = w * h

----------------------------------------------------
-- 8. Ввод-вывод в Haskell
----------------------------------------------------

-- Полноценно объяснить тему ввода-вывода невозможно
-- без объяснения монад, но для использования в простых случаях
-- вводного описания будет достаточно.

-- Когда программа на Haskell выполняется,
-- вызывается функция с именем `main`.
-- Эта функция должна вернуть значение типа `IO ()`
-- Например

main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue)
-- `putStrLn` имеет тип `String -> IO ()`

-- Проще всего реализовать программу с вводом-выводом (IO),
-- если вы реализуете функцию с типом `String -> String`.
-- Далее ФВП
--    interact :: (String -> String) -> IO ()
-- сделает всё за нас!

countLines :: String -> String
countLines = show . length . lines
-- здесь `lines` разделяет строку на список строк
-- по символу перевода строки

main' :: IO ()
main' = interact countLines

{- Вы можете думать о типе `IO ()`,
как о некотором представлении последовательности
действий, которые должен совершить компьютер.
Такое представление напоминает программу
на императивном языке программирования. Для описания
такой последовательности используется `do`-нотация -}

sayHello :: IO ()
sayHello = do
   putStrLn "What is your name?"
   name <- getLine -- запрашиваем строку и связываем с "name"
   putStrLn $ "Hello, " ++ name

-- Упражнение:
--     напишите свою реализацию функции `interact`,
--     которая запрашивает и обрабатывает только одну строку

{- Код функции `sayHello` не будет исполняться
при её определении. Единственное место, где IO-действия
могут быть произведены - функция `main`!
Чтобы эта программа выполнила действия в функции `sayHello`,
закомментируйте предыдущее определение функции `main`
и добавьте новое определение:

main = sayHello -}

{- Давайте подробнее рассмотрим, как работает функция `getLine`
Её тип:
   getLine :: IO String
Вы можете думать, что значение типа `IO a` представляет
собой компьютерную программу, в результате выполнения которой
генерируется значение типа `a`, в дополнение
к остальным эффектам, производимым при выполнении - таким как
печать текста на экран. Это значение типа `a` мы можем
сохранить с помощью оператора `<-`. Мы даже можем реализовать
свое действие, возвращающее значение: -}

action :: IO String
action = do
   putStrLn "This is a line. Duh"
   input1 <- getLine
   input2 <- getLine
   -- Тип блока `do` будет соответствовать типу последнего
   -- выполненного в блоке выражения.
   -- Заметим, что `return` - не ключевое слово, а функция
   -- типа `a -> IO a`
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- Теперь это действие можно использовать вместо `getLine`:

main'' = do
    putStrLn "I will echo two lines!"
    result <- action
    putStrLn result
    putStrLn "This was all, folks!"

{- Тип `IO` - пример "монады". Языку Haskell нужны монады,
чтобы оставаться преимущественно чистым функциональным языком.
Любые функции, взаимодействующие с внешним миром
(производящие ввод-вывод) имеют `IO` в своих сигнатурах.
Это позволяет судить о функции как о "чистой" - такая не будет
производить ввод-вывод. В ином случая функция - не "чистая".

Такой подход позволяет очень просто разрабатывать многопоточные
программы - чистые функции, запущенные параллельно
не будут конфликтовать между собой в борьбе за ресурсы. -}

----------------------------------------------------
-- 9. Haskell REPL
----------------------------------------------------

{- Интерактивная консоль Haskell запускается командой `ghci`.
Теперь можно вводить строки кода на Haskell.
Связывание значений с именами производится
с помощью выражения `let`: -}

let foo = 5

-- Тип значения или выражения можно узнать
-- с помощью команды `:t`:

>:t foo
foo :: Integer

-- Также можно выполнять действия с типом `IO ()`

> sayHello
What is your name?
Friend!
Hello, Friend!
```

Многое о Haskell, например классы типов и монады невозможно уместить в столь короткую статью. Огромное количество очень интересных идей лежит в основе языка, и именно благодаря этому фундаменту на языке так приятно писать код. Позволю себе привести ещё один маленький пример кода на Haskell - реализацию быстрой сортировки:

```haskell
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```

Haskell прост в установке, забирайте [здесь](http://www.haskell.org/platform/) и пробуйте! Это же так интересно!.

Более глубокое погрузиться в язык позволят прекрасные книги
[Learn you a Haskell](http://learnyouahaskell.com/) и
[Real World Haskell](http://book.realworldhaskell.org/).

[author]: http://adit.io "имеется в виду автор оригинального текста Adit Bhargava *(примечание переводчика)*"
