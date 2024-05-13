---
language: Nim
filename: learnNim-jp.nim
contributors:
    - ["Jason J. Ayala P.", "http://JasonAyala.com"]
    - ["Dennis Felsing", "https://dennis.felsing.org"]
translators:
    - ["Seiichi Ariga", "https://github.com/s-ariga"]
lang: ja-jp
---

Nim (元 Nimrod)は、静的型付けの命令型言語です。ランタイムの効率を損なうこと
なく、プログラマーに恩恵を与えてくれます。

Nimは、効率的で、表現力があり、エレガントです。

```nim
# 単一行コメントは # で開始

#[
  複数行コメントです。
  Nimでは、複数行コメントはネスト可能で、#[で始まり、
  ... そして終了は ]#
]#

discard """
これも複数行コメントとして機能します。
あるいは、解析不能のコードです。
"""



var                     # 変数宣言 (そして割当て)
  letter: char = 'n'    # 型注釈ありとなし
  lang = "N" & "im"
  nLength: int = len(lang)
  boat: float
  truth: bool = false

let            # letで変数を*1回(イミュータブル)*束縛します
  legs = 400   # legsはイミュータブル
  arms = 2_000 # _ は無視され、長い数値を読みやすくします
  aboutPi = 3.15

const            # 定数はコンパイル時に評価されます。
  debug = true   # これにより実行速度を高めます。
  compileBadCode = false

when compileBadCode:            # `when`はコンパイル時の`if`です。
  legs = legs + 1               # この部分はコンパイルされません。ed.
  const input = readline(stdin) # 定数値はコンパイル時に決まっていなければ
                                # なりません。

discard 1 > 2 # Note: コンパイラーはある式の結果が使われていないと警告を
              # 表示します。`discard`により、これを回避できます。


#
# データ構造
#

# タプル

var
  child: tuple[name: string, age: int]   # タプルにはフィールド名
  today: tuple[sun: string, temp: float] # *そして*順序の*両方*があります

child = (name: "Rudiger", age: 2) # ()リテラルで両方同時に割当て
today.sun = "Overcast"            # あるいは、個別のフィールドに割当て
today.temp = 70.1

# シーケンス

var
  drinks: seq[string]

drinks = @["Water", "Juice", "Chocolate"] # シーケンスリテラルは@[V1,..,Vn]

drinks.add("Milk")

if "Milk" in drinks:
  echo "We have Milk and ", drinks.len - 1, " other drinks"

let myDrink = drinks[2]

#
# 型の定義
#

# あなた自身の型を定義することで、コンパイラーにより多くの仕事をさせられます。
# それにより静的型付けがパワフルで便利なものになります。

type
  Name = string # 型エイリアスは、既存の型と置き換え可能でありながら、
  Age = int     # より記述的な型を提供します。
  Person = tuple[name: Name, age: Age] # データ構造も定義できます。
  AnotherSyntax = tuple
    fieldOne: string
    secondField: int

var
  john: Person = (name: "John B.", age: 17)
  newage: int = 18 # ここはint型よりもAge型を使ったほうが良いでしょう。

john.age = newage # intとAgeは同じ型なので、これで動作します。

type
  Cash = distinct int    # `distinct`を使うと、新しい型と
  Desc = distinct string # 元の型の互換性がなくなります。

var
  money: Cash = 100.Cash  # `.Cash`がint型をわれわれ独自の型に
                          #変換しています。
  description: Desc  = "Interesting".Desc

when compileBadCode:
  john.age  = money        # エラー! ageはint型でmoneyはCash型です。
  john.name = description  # コンパイル時のエラーになります。

#
# さらに型とデータ構造
#

# 列挙は型にいくつかの値の中から1つの値をとることを許します。

type
  Color = enum cRed, cBlue, cGreen
  Direction = enum  # 別の書き方
    dNorth
    dWest
    dEast
    dSouth
var
  orient = dNorth # `orient`はDirection型で値は`dNorth`
  pixel = cGreen  # `pixel`はColor型で値は`cGreen`

discard dNorth > dEast # 列挙には通常、順序があります。

# サブレンジは有効な値の範囲を限定します。

type
  DieFaces = range[1..20] # 1から20のintだけが有効な値です。
var
  my_roll: DieFaces = 13

when compileBadCode:
  my_roll = 23 # エラー!

# 配列

type
  RollCounter = array[DieFaces, int]  # 配列の長さは固定で、順序のある型の
  DirNames = array[Direction, string] # どれかをインデックスとします。
  Truths = array[42..44, bool]
var
  counter: RollCounter
  directions: DirNames
  possible: Truths

possible = [false, false, false] # [V1,..,Vn]で配列を作れます。
possible[42] = true

directions[dNorth] = "Ahh. The Great White North!"
directions[dWest] = "No, don't go there."

my_roll = 13
counter[my_roll] += 1
counter[my_roll] += 1

var anotherArray = ["Default index", "starts at", "0"]

# 他にも表、集合、リスト、キュー、Crit-bit treeなどのデータ構造があります。
# http://nim-lang.org/docs/lib.html#collections-and-algorithms

#
# IOと制御
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

import strutils as str # http://nim-lang.org/docs/strutils.html
echo "I'm thinking of a number between 41 and 43. Guess which!"
let number: int = 42
var
  raw_guess: string
  guess: int
while guess != number:
  raw_guess = readLine(stdin)
  if raw_guess == "": continue # この繰り返しを飛ばす。
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
# 繰返し
#

for i, elem in ["Yes", "No", "Maybe so"]: # または単に`for elem in`
  echo(elem, " is at index: ", i)

for k, v in items(@[(person: "You", power: 100), (person: "Me", power: 9000)]):
  echo v

let myString = """
an <example>
`string` to
play with
""" # 複数行の生文字列

for line in splitLines(myString):
  echo(line)

for i, c in myString:       # インデックスと文字。
                            # あるいは`for j in`で文字だけ。
  if i mod 2 == 0: continue # `if`構文の簡易版
  elif c == 'X': break
  else: echo(c)

#
# プロシージャ
#

type Answer = enum aYes, aNo

proc ask(question: string): Answer =
  echo(question, " (y/n)")
  while true:
    case readLine(stdin)
    of "y", "Y", "yes", "Yes":
      return Answer.aYes  # 列挙に限定することができます。
    of "n", "N", "no", "No":
      return Answer.aNo
    else: echo("Please be clear: yes or no")

proc addSugar(amount: int = 2) = # amountのデフォルトは2、戻り値はなし
  assert(amount > 0 and amount < 9000, "Crazy Sugar")
  for a in 1..amount:
    echo(a, " sugar...")

case ask("Would you like sugar in your tea?")
of aYes:
  addSugar(3)
of aNo:
  echo "Oh do take a little!"
  addSugar()
# ここで可能な値は`yes`か`no`だけなので、`else`は必要ない

#
# FFI
#

# NimはCへとコンパイルされるので、容易にFFIができる:

proc strcmp(a, b: cstring): cint {.importc: "strcmp", nodecl.}

let cmp = strcmp("C?", "Easy!")
```

これらの他に、Nimはほかの言語と比較してメタプログラミング、
実行時パフォーマンス、コンパイル時の機能で特長があります。

## 参考

* [Home Page](http://nim-lang.org)
* [Download](http://nim-lang.org/download.html)
* [Community](http://nim-lang.org/community.html)
* [FAQ](http://nim-lang.org/question.html)
* [Documentation](http://nim-lang.org/documentation.html)
* [Manual](http://nim-lang.org/docs/manual.html)
* [Standard Library](http://nim-lang.org/docs/lib.html)
* [Rosetta Code](http://rosettacode.org/wiki/Category:Nim)
