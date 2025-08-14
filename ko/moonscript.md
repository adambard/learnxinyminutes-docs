---
name: MoonScript
contributors:
  - ["RyanSquared", "https://ryansquared.github.io/"]
  - ["Job van der Zwan", "https://github.com/JobLeonard"]
filename: moonscript.moon
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

MoonScript는 Lua로 컴파일되는 동적 스크립팅 언어입니다. 가장 빠른 스크립팅 언어 중 하나의 강력한 기능과 풍부한 기능 세트를 제공합니다.

모든 플랫폼에 대한 공식 설치 가이드는 [MoonScript 웹사이트](https://moonscript.org/)를 참조하십시오.

```moon
-- 두 개의 대시는 주석을 시작합니다. 주석은 줄 끝까지 이어질 수 있습니다.
-- Lua로 트랜스파일된 MoonScript는 주석을 유지하지 않습니다.

-- 참고로, MoonScript는 Lua처럼 'do', 'then' 또는 'end'를 사용하지 않고
-- 대신 Python과 매우 유사한 들여쓰기 구문을 사용합니다.

--------------------------------------------------
-- 1. 할당
--------------------------------------------------

hello = "world"
a, b, c = 1, 2, 3
hello = 123 -- 위의 `hello`를 덮어씁니다.

x = 0
x += 10 -- x = x + 10

s = "hello "
s ..= "world" -- s = s .. "world"

b = false
b and= true or false -- b = b and (true or false)

--------------------------------------------------
-- 2. 리터럴 및 연산자
--------------------------------------------------

-- 리터럴은 Lua에서와 거의 똑같이 작동합니다. 문자열은
-- \ 없이 줄 중간에서 끊을 수 있습니다.

some_string = "exa
mple" -- local some_string = "exa\nmple"

-- 문자열에는 보간된 값, 즉 평가된 다음
-- 문자열 내에 배치되는 값을 포함할 수도 있습니다.

some_string = "This is an #{some_string}" -- 'This is an exa\nmple'이 됩니다

--------------------------------------------------
-- 2.1. 함수 리터럴
--------------------------------------------------

-- 함수는 화살표를 사용하여 작성됩니다:

my_function = -> -- `function() end`로 컴파일됨
my_function() -- 빈 함수 호출

-- 함수는 괄호를 사용하지 않고 호출할 수 있습니다. 괄호는 여전히
-- 다른 함수보다 우선순위를 갖기 위해 사용할 수 있습니다.

func_a = -> print "Hello World!"
func_b = ->
	value = 100
	print "The value: #{value}"

-- 함수에 매개변수가 필요 없는 경우 `()` 또는 `!`로 호출할 수 있습니다.

func_a!
func_b()

-- 함수는 화살표 앞에 괄호로 묶인 인수 이름 목록을
-- 사용하여 인수를 사용할 수 있습니다.

sum = (x, y)-> x + y -- 마지막 표현식이 함수에서 반환됩니다.
print sum(5, 10)

-- Lua에는 첫 번째 인수를 'self' 객체처럼 객체로 함수에
-- 보내는 관용구가 있습니다. 얇은 화살표(->) 대신 굵은 화살표(=>)를
-- 사용하면 자동으로 `self` 변수가 생성됩니다. `@x`는 `self.x`의
-- 약어입니다.

func = (num)=> @value + num

-- 기본 인수는 함수 리터럴과 함께 사용할 수도 있습니다:

a_function = (name = "something", height=100)->
	print "Hello, I am #{name}.\nMy height is #{height}."

-- 기본 인수는 Lua로 트랜스파일될 때 함수 본문에서
-- 계산되므로 이전 인수를 참조할 수 있습니다.

some_args = (x = 100, y = x + 1000)-> print(x + y)

--------------------------------------------------
-- 고려 사항
--------------------------------------------------

-- 마이너스 기호는 단항 부정 연산자와 이항
-- 뺄셈 연산자라는 두 가지 역할을 합니다. 가능한 충돌을 피하기 위해
-- 이항 연산자 사이에는 항상 공백을 사용하는 것이 좋습니다.

a = x - 10 --  a = x - 10
b = x-10 -- b = x - 10
c = x -y -- c = x(-y)
d = x- z -- d = x - z

-- 변수와 문자열 리터럴 사이에 공백이 없으면 함수
-- 호출이 다음 표현식보다 우선합니다:

x = func"hello" + 100 -- func("hello") + 100
y = func "hello" + 100 -- func("hello" + 100)

-- 함수에 대한 인수는 들여쓰기된 한 여러 줄에 걸쳐 있을 수 있습니다.
-- 들여쓰기는 중첩될 수도 있습니다.

my_func 5, -- my_func(5, 8, another_func(6, 7, 9, 1, 2), 5, 4)로 호출됨
	8, another_func 6, 7, --
		9, 1, 2,		  -- another_func(6, 7, 9, 1, 2)로 호출됨
	5, 4

-- 함수가 블록 시작 부분에서 사용되면 들여쓰기는
-- 블록에서 사용되는 들여쓰기 수준과 다를 수 있습니다:

if func 1, 2, 3, -- func(1, 2, 3, "hello", "world")로 호출됨
		"hello",
		"world"
	print "hello"

--------------------------------------------------
-- 3. 테이블
--------------------------------------------------

-- 테이블은 Lua처럼 중괄호로 정의됩니다:

some_values = {1, 2, 3, 4}

-- 테이블은 쉼표 대신 줄 바꿈을 사용할 수 있습니다.

some_other_values = {
	5, 6
	7, 8
}

-- 할당은 `=` 대신 `:`으로 수행됩니다:

profile = {
	name: "Bill"
	age: 200
	"favorite food": "rice"
}

-- `key: value` 테이블의 경우 중괄호를 생략할 수 있습니다.

y = type: "dog", legs: 4, tails: 1

profile =
	height: "4 feet",
	shoe_size: 13,
	favorite_foods: -- 중첩 테이블
		foo: "ice cream",
		bar: "donuts"

my_function dance: "Tango", partner: "none" -- :( forever alone

-- 변수로 구성된 테이블은 변수와 동일한 이름을
-- 접두사 연산자로 `:`를 사용하여 사용할 수 있습니다.

hair = "golden"
height = 200
person = {:hair, :height}

-- Lua에서와 같이 키는 `[]`를 사용하여 문자열이 아니거나
-- 숫자가 아닌 값일 수 있습니다.

t =
	[1 + 2]: "hello"
	"hello world": true -- `[]` 없이 문자열 리터럴을 사용할 수 있습니다.

--------------------------------------------------
-- 3.1. 테이블 컴프리헨션
--------------------------------------------------

-- 리스트 컴프리헨션

-- 모든 항목이 두 배인 리스트의 복사본을 만듭니다. 별표를 사용하여
-- 변수 이름이나 테이블 앞에 붙이면 테이블의 값을 반복할 수 있습니다.

items = {1, 2, 3, 4}
doubled = [item * 2 for item in *items]
-- `when`을 사용하여 값을 포함할지 여부를 결정합니다.

slice = [item for item in *items when item > 1 and item < 3]

-- 리스트 컴프리헨션 내부의 `for` 절은 연결될 수 있습니다.

x_coords = {4, 5, 6, 7}
y_coords = {9, 2, 3}

points = [{x,y} for x in *x_coords for y in *y_coords]

-- 숫자 for 루프는 컴프리헨션에서도 사용할 수 있습니다:

evens = [i for i=1, 100 when i % 2 == 0]

-- 테이블 컴프리헨션은 매우 유사하지만 `{`와 `}`를 사용하고
-- 각 반복에 대해 두 개의 값을 사용합니다.

thing = color: "red", name: "thing", width: 123
thing_copy = {k, v for k, v in pairs thing}

-- 테이블은 `unpack`을 사용하여 배열의 키-값 쌍에서 "평탄화"될 수 있습니다.
-- 두 값을 모두 반환하여 첫 번째는 키로, 두 번째는
-- 값으로 사용합니다.

tuples = {{"hello", "world"}, {"foo", "bar"}}
table = {unpack tuple for tuple in *tuples}

-- 슬라이싱은 배열의 특정 섹션만 반복하는 데 사용할 수 있습니다.
-- 반복을 위해 `*` 표기법을 사용하지만 `[start, end, step]`을 추가합니다.

-- 다음 예는 이 구문이 `for` 루프뿐만 아니라
-- 모든 컴프리헨션에서도 사용할 수 있음을 보여줍니다.

for item in *points[1, 10, 2]
	print unpack item

-- 원하지 않는 값은 생략할 수 있습니다.
-- 단계가 포함되지 않은 경우 두 번째 쉼표는 필요하지 않습니다.

words = {"these", "are", "some", "words"}
for word in *words[,3]
	print word

--------------------------------------------------
-- 4. 제어 구조
--------------------------------------------------

have_coins = false
if have_coins
	print "Got coins"
else
	print "No coins"

-- 한 줄 `if`에 `then` 사용
if have_coins then "Got coins" else "No coins"

-- `unless`는 `if`의 반대입니다.
unless os.date("%A") == "Monday"
	print "It is not Monday!"

-- `if`와 `unless`는 표현식으로 사용할 수 있습니다.
is_tall = (name)-> if name == "Rob" then true else false
message = "I am #{if is_tall "Rob" then "very tall" else "not so tall"}"
print message -- "I am very tall"

-- `if`, `elseif`, `unless`는 할당과 표현식을 평가할 수 있습니다.
if x = possibly_nil! -- `x`를 `possibly_nil()`로 설정하고 `x`를 평가
	print x

-- 조건문은 문장 앞뿐만 아니라 뒤에도 사용할 수 있습니다. 이것은
-- "라인 데코레이터"라고 합니다.

is_monday = os.date("%A") == "Monday"
print("It IS Monday!") if isMonday
print("It is not Monday..") unless isMonday
--print("It IS Monday!" if isMonday) -- 문장이 아니므로 작동하지 않음

--------------------------------------------------
-- 4.1 루프
--------------------------------------------------

for i = 1, 10
	print i

for i = 10, 1, -1 do print i -- 한 줄 루프에 `do` 사용.

i = 0
while i < 10
	continue if i % 2 == 0 -- continue 문; 루프의 나머지 부분을 건너뜁니다.
	print i

-- 루프는 조건문처럼 라인 데코레이터로 사용할 수 있습니다.
print "item: #{item}" for item in *items

-- 루프를 표현식으로 사용하면 배열 테이블이 생성됩니다. 블록의
-- 마지막 문은 표현식으로 강제 변환되어 테이블에 추가됩니다.
my_numbers = for i = 1, 6 do i -- {1, 2, 3, 4, 5, 6}

-- 값을 필터링하려면 `continue` 사용
odds = for i in *my_numbers
	continue if i % 2 == 0 -- 컴프리헨션의 `when`과 반대로 작동!
	i -- 홀수일 때만 반환 테이블에 추가됨

-- `for` 루프는 함수 마지막 문일 때 `nil`을 반환합니다.
-- 테이블을 생성하려면 명시적 `return`을 사용하십시오.
print_squared = (t) -> for x in *t do x*x -- `nil` 반환
squared = (t) -> return for x in *t do x*x -- 새 제곱 테이블 반환

-- 다음은 `(t) -> [i for i in *t when i % 2 == 0]`와 동일하게 작동합니다.
-- 하지만 리스트 컴프리헨션은 더 나은 코드를 생성하고 더 읽기 쉽습니다!

filter_odds = (t) ->
	return for x in *t
		if x % 2 == 0 then x else continue
evens = filter_odds(my_numbers) -- {2, 4, 6}

--------------------------------------------------
-- 4.2 Switch 문
--------------------------------------------------

-- Switch 문은 동일한 값에 대해 여러 `if` 문을 작성하는
-- 약식 방법입니다. 값은 한 번만 평가됩니다.

name = "Dan"

switch name
	when "Dave"
		print "You are Dave."
	when "Dan"
		print "You are not Dave, but Dan."
	else
		print "You are neither Dave nor Dan."

-- Switch는 표현식으로 사용할 수 있을 뿐만 아니라 여러 값을
-- 비교할 수도 있습니다. 값이 단일 표현식인 경우 `when` 절과
-- 같은 줄에 있을 수 있습니다.

b = 4
next_even = switch b
	when 1 then 2
	when 2, 3 then 4
	when 4, 5 then 6
	else error "I can't count that high! D:"

--------------------------------------------------
-- 5. 객체 지향 프로그래밍
--------------------------------------------------

-- 클래스는 식별자 뒤에 `class` 키워드를 사용하여 생성되며,
-- 일반적으로 CamelCase를 사용하여 작성됩니다. 클래스에 특정한 값은
-- `self.value` 대신 식별자로 @를 사용할 수 있습니다.

class Inventory
	new: => @items = {}
	add_item: (name)=> -- 클래스에 굵은 화살표 사용에 유의!
		@items[name] = 0 unless @items[name]
		@items[name] += 1

-- 클래스 내부의 `new` 함수는 클래스의 인스턴스가
-- 생성될 때 호출되므로 특별합니다.

-- 클래스의 인스턴스를 만드는 것은 클래스를 함수로 호출하는 것만큼
-- 간단합니다. 클래스 내부의 함수를 호출하려면 \를 사용하여
-- 인스턴스와 호출하는 함수를 구분합니다.

inv = Inventory!
inv\add_item "t-shirt"
inv\add_item "pants"

-- new() 함수가 아닌 클래스에 정의된 값은 모든
-- 인스턴스에서 공유됩니다.

class Person
	clothes: {}
	give_item: (name)=>
		table.insert @clothes name

a = Person!
b = Person!

a\give_item "pants"
b\give_item "shirt"

-- "pants"와 "shirt" 모두 출력

print item for item in *a.clothes

-- 클래스 인스턴스는 인스턴스를 생성한 클래스 객체와
-- 동일한 값 `.__class`를 가집니다.

assert(b.__class == Person)

-- `=` 연산자를 사용하여 클래스 본문에 선언된 변수는 지역 변수이므로
-- 이러한 "비공개" 변수는 현재 범위 내에서만 액세스할 수 있습니다.

class SomeClass
	x = 0
	reveal: ->
		x += 1
		print x

a = SomeClass!
b = SomeClass!
print a.x -- nil
a.reveal! -- 1
b.reveal! -- 2

--------------------------------------------------
-- 5.1 상속
--------------------------------------------------

-- `extends` 키워드를 사용하여 다른 클래스에서 속성과 메서드를
-- 상속받을 수 있습니다.

class Backpack extends Inventory
	size: 10
	add_item: (name)=>
		error "backpack is full" if #@items > @size
		super name -- `name`으로 Inventory.add_item 호출.

-- `new` 메서드가 추가되지 않았으므로 `Inventory`의 `new` 메서드가
-- 대신 사용됩니다. `Inventory`의 생성자를 계속 사용하면서
-- 생성자를 사용하려면 `new()` 중에 마법의 `super` 함수를
-- 사용할 수 있습니다.

-- 클래스가 다른 클래스를 확장하면 부모 클래스(있는 경우)의
-- `__inherited` 메서드를 호출합니다. 항상 부모와
-- 자식 객체로 호출됩니다.

class ParentClass
	@__inherited: (child)=>
		print "#{@__name} was inherited by #{child.__name}"
	a_method: (a, b) => print a .. ' ' .. b

-- 'ParentClass was inherited by MyClass' 출력

class MyClass extends ParentClass
	a_method: =>
		super "hello world", "from MyClass!"
		assert super == ParentClass

--------------------------------------------------
-- 6. 범위
--------------------------------------------------

-- 모든 값은 기본적으로 지역 변수입니다. `export` 키워드를 사용하여
-- 변수를 전역 값으로 선언할 수 있습니다.

export var_1, var_2
var_1, var_3 = "hello", "world" -- var_3는 지역 변수, var_1은 아님.

export this_is_global_assignment = "Hi!"

-- 클래스 앞에 `export`를 붙여 전역 클래스로 만들 수도 있습니다.
-- 또는 `export ^`를 사용하여 모든 CamelCase 변수를 자동으로 내보내거나
-- `export *`를 사용하여 모든 값을 내보낼 수 있습니다.

-- `do`를 사용하면 지역 변수가 필요할 때 수동으로 범위를 만들 수 있습니다.

do
	x = 5
print x -- nil

-- 여기서는 `do`를 표현식으로 사용하여 클로저를 만듭니다.

counter = do
	i = 0
	->
		i += 1
		return i

print counter!  -- 1
print counter!  -- 2

-- `local` 키워드를 사용하여 할당되기 전에
-- 변수를 정의할 수 있습니다.

local var_4
if something
	var_4 = 1
print var_4 -- `var_4`가 `if` 범위가 아닌 이 범위에서 설정되었으므로 작동합니다.

-- `local` 키워드를 사용하여 기존 변수를 가릴 수도 있습니다.

x = 10
if false
	local x
	x = 12
print x -- 10

-- `local *`를 사용하여 모든 변수를 미리 선언합니다.
-- 또는 `local ^`를 사용하여 모든 CamelCase 값을 미리 선언합니다.

local *

first = ->
	second!

second = ->
	print data

data = {}

--------------------------------------------------
-- 6.1 가져오기
--------------------------------------------------

-- 테이블의 값은 `import` 및 `from` 키워드를 사용하여
-- 현재 범위로 가져올 수 있습니다. `import` 목록의 이름은
-- 모듈 함수인 경우 `\`를 앞에 붙일 수 있습니다.

import insert from table -- local insert = table.insert
import \add from state: 100, add: (value)=> @state + value
print add 22

-- 테이블과 마찬가지로 `import` 목록에서 쉼표를 제외하여
-- 더 긴 가져오기 항목 목록을 허용할 수 있습니다.

import
	asdf, gh, jkl
	antidisestablishmentarianism
	from {}

--------------------------------------------------
-- 6.2 With
--------------------------------------------------

-- `with` 문을 사용하여 클래스 또는 객체의 인스턴스에서
-- 값을 빠르게 호출하고 할당할 수 있습니다.

file = with File "lmsi15m.moon" -- `file`은 `set_encoding()`의 값입니다.
	\set_encoding "utf8"

create_person = (name, relatives)->
	with Person!
		.name = name
		\add_relative relative for relative in *relatives
me = create_person "Ryan", {"sister", "sister", "brother", "dad", "mother"}

with str = "Hello" -- 표현식으로 할당! :D
	print "original: #{str}"
	print "upper: #{\upper!}"

--------------------------------------------------
-- 6.3 구조 분해
--------------------------------------------------

-- 구조 분해는 배열, 테이블 및 중첩된 테이블을 가져와
-- 지역 변수로 변환할 수 있습니다.

obj2 =
	numbers: {1, 2, 3, 4}
	properties:
		color: "green"
		height: 13.5

{numbers: {first, second}, properties: {:color}} = obj2

print first, second, color -- 1 2 green

-- `first`와 `second`는 배열이므로 [1]과 [2]를 반환하지만,
-- `:color`는 `color: color`와 같으므로 `color` 값으로
-- 설정됩니다.

-- 구조 분해는 `import` 대신 사용할 수 있습니다.

{:max, :min, random: rand} = math -- math.random을 rand로 이름 변경

-- 구조 분해는 할당이 가능한 모든 곳에서 수행할 수 있습니다.

for {left, right} in *{{"hello", "world"}, {"egg", "head"}}
	print left, right
```

## 추가 자료

- [언어 가이드](https://moonscript.org/reference/)
- [온라인 컴파일러](https://moonscript.org/compiler/)
