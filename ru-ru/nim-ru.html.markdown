---
language: Nim
filename: learnNim-ru.nim
contributors:
    - ["Jason J. Ayala P.", "http://JasonAyala.com"]
    - ["Dennis Felsing", "http://felsin9.de/nnis/"]
translators:
    - ["Nomadic", "https://github.com/n0madic"]
lang: ru-ru
---

Nim (ранее известный, как Nimrod) — язык программирования со статической
типизацией, поддерживающий процедурный, объектно-ориентированный,
функциональный и обобщённый стили программирования.

Nim эффективный, выразительный и элегантный.

```nim
var                     # Объявление (и присваивание) переменных,
  letter: char = 'n'    # с указанием типа или без
  lang = "N" & "im"
  nLength : int = len(lang)
  boat: float
  truth: bool = false

let            # Используйте let *сразу* для объявления и связывания переменных.
  legs = 400   # legs неизменяемый.
  arms = 2_000 # Символ _ игнорируется и удобен для длинных чисел.
  aboutPi = 3.15

const            # Константы вычисляются во время компиляции. Это обеспечивает
  debug = true   # производительность и полезно в выражениях этапа компиляции.
  compileBadCode = false

when compileBadCode:            # `when` это `if` этапа компиляции.
  legs = legs + 1               # Эта ошибка никогда не будет скомпилирована.
  const input = readline(stdin) # Значения констант должны быть известны во
                                # время компиляции.

discard 1 > 2 # Примечание. Компилятор будет жаловаться, если результат
              # выражения не используется. `discard` обходит это.

discard """
Это может использоваться как многострочный комментарий.
Или для не поддающегося синтаксическому анализу, сломанного кода
"""

#
# Структуры данных
#

# Кортежи

var
  child: tuple[name: string, age: int]   # Кортежи определяют *как* имя поля
  today: tuple[sun: string, temp: float] # так *и* порядок полей.

child = (name: "Rudiger", age: 2) # Присвоить все сразу литералом ()
today.sun = "Overcast"            # или отдельно по полям.
today.temp = 70.1

# Последовательности

var
  drinks: seq[string]

drinks = @["Water", "Juice", "Chocolate"] # @[V1,..,Vn] является литералом
                                          # последовательности

drinks.add("Milk")

if "Milk" in drinks:
  echo "We have Milk and ", drinks.len - 1, " other drinks"

let myDrink = drinks[2]

#
# Определение типов
#

# Определение собственных типов позволяет компилятору работать на вас.
# Это то, что делает статическую типизацию мощной и полезной.

type
  Name = string # Псевдоним типа дает вам новый тип, который равнозначен
  Age = int     # старому типу, но более нагляден.
  Person = tuple[name: Name, age: Age] # Определение структур данных.
  AnotherSyntax = tuple
    fieldOne: string
    secondField: int

var
  john: Person = (name: "John B.", age: 17)
  newage: int = 18 # Было бы лучше использовать Age, чем int

john.age = newage # Но это все же работает, потому что int и Age синонимы.

type
  Cash = distinct int    # `distinct` делает новый тип несовместимым с его
  Desc = distinct string # базовым типом.

var
  money: Cash = 100.Cash # `.Cash` преобразует int в наш тип
  description: Desc  = "Interesting".Desc

when compileBadCode:
  john.age  = money        # Error! age is of type int and money is Cash
  john.name = description  # Компилятор говорит: "Нельзя!"

#
# Дополнительные типы и структуры данных
#

# Перечисления позволяют типу иметь одно из ограниченного числа значений

type
  Color = enum cRed, cBlue, cGreen
  Direction = enum # Альтернативный формат
    dNorth
    dWest
    dEast
    dSouth
var
  orient = dNorth # `orient` имеет тип Direction, со значением `dNorth`
  pixel = cGreen # `pixel` имеет тип Color, со значением `cGreen`

discard dNorth > dEast # Перечисления обычно являются "порядковыми" типами

# Поддиапазоны определяют ограниченный допустимый диапазон

type
  DieFaces = range[1..20] # Допустимым значением являются только int от 1 до 20
var
  my_roll: DieFaces = 13

when compileBadCode:
  my_roll = 23 # Error!

# Arrays

type
  RollCounter = array[DieFaces, int]  # Массивы фиксированной длины и
  DirNames = array[Direction, string] # индексируются любым порядковым типом.
  Truths = array[42..44, bool]
var
  counter: RollCounter
  directions: DirNames
  possible: Truths

possible = [false, false, false] # Массивы создаются литералом [V1,..,Vn]
possible[42] = true

directions[dNorth] = "Ahh. The Great White North!"
directions[dWest] = "No, don't go there."

my_roll = 13
counter[my_roll] += 1
counter[my_roll] += 1

var anotherArray = ["Default index", "starts at", "0"]

# Доступны другие структуры данных, в том числе таблицы, множества,
# списки, очереди и crit-bit деревья.
# http://nim-lang.org/docs/lib.html#collections-and-algorithms (EN)

#
# IO и поток управления выполнением
#

# `case`, `readLine()`

echo "Read any good books lately?"
case readLine(stdin)
of "no", "No":
  echo "Go to your local library."
of "yes", "Yes":
  echo "Carry on, then."
else:
  echo "That's great; I assume."

# `while`, `if`, `continue`, `break`

import strutils as str # http://nim-lang.org/docs/strutils.html (EN)
echo "I'm thinking of a number between 41 and 43. Guess which!"
let number: int = 42
var
  raw_guess: string
  guess: int
while guess != number:
  raw_guess = readLine(stdin)
  if raw_guess == "": continue # Пропустить эту итерацию
  guess = str.parseInt(raw_guess)
  if guess == 1001:
    echo("AAAAAAGGG!")
    break
  elif guess > number:
    echo("Nope. Too high.")
  elif guess < number:
    echo(guess, " is too low")
  else:
    echo("Yeeeeeehaw!")

#
# Итерации (циклы)
#

for i, elem in ["Yes", "No", "Maybe so"]: # Или просто `for elem in`
  echo(elem, " is at index: ", i)

for k, v in items(@[(person: "You", power: 100), (person: "Me", power: 9000)]):
  echo v

let myString = """
an <example>
`string` to
play with
""" # Многострочная "сырая" строка

for line in splitLines(myString):
  echo(line)

for i, c in myString:       # Индекс и символ. Или `for j in` только для символов
  if i mod 2 == 0: continue # Компактная форма `if`
  elif c == 'X': break
  else: echo(c)

#
# Процедуры
#

type Answer = enum aYes, aNo

proc ask(question: string): Answer =
  echo(question, " (y/n)")
  while true:
    case readLine(stdin)
    of "y", "Y", "yes", "Yes":
      return Answer.aYes  # Перечисления могут быть квалифицированы
    of "n", "N", "no", "No":
      return Answer.aNo
    else: echo("Please be clear: yes or no")

proc addSugar(amount: int = 2) = # Значение поумолчанию 2, ничего не возвращает
  assert(amount > 0 and amount < 9000, "Crazy Sugar")
  for a in 1..amount:
    echo(a, " sugar...")

case ask("Would you like sugar in your tea?")
of aYes:
  addSugar(3)
of aNo:
  echo "Oh do take a little!"
  addSugar()
# Здесь нет необходимости в `else`. Возможны только `yes` и `no`.

#
# FFI (интерфейс внешних функций)
#

# Так как Nim компилируется в C, то FFI делается очень просто:

proc strcmp(a, b: cstring): cint {.importc: "strcmp", nodecl.}

let cmp = strcmp("C?", "Easy!")
```

Кроме того, Nim выделяется среди себе подобных метапрограммированием,
производительностью, функциями этапа компиляции.

## Дальнейшее чтение (EN)

* [Домашняя страница](http://nim-lang.org)
* [Скачать](http://nim-lang.org/download.html)
* [Сообщество](http://nim-lang.org/community.html)
* [FAQ](http://nim-lang.org/question.html)
* [Документация](http://nim-lang.org/documentation.html)
* [Руководство](http://nim-lang.org/docs/manual.html)
* [Стандартная библиотека](http://nim-lang.org/docs/lib.html)
* [Rosetta Code](http://rosettacode.org/wiki/Category:Nim)
