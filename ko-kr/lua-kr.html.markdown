---
language: Lua
category: language
contributors:
    - ["Tyler Neylon", "http://tylerneylon.com/"]
translators:
    - ["wikibook", "http://wikibook.co.kr"]
lang: ko-kr
filename: learnlua-kr.lua
---

```lua
-- 대시 두 개는 한 줄짜리 주석을 의미합니다.

--[[
     [와 ]를 두 개씩 추가하면 여러 줄 주석이 됩니다.
--]]

----------------------------------------------------
-- 1. 변수와 흐름 제어
----------------------------------------------------

num = 42  -- 모든 숫자는 double입니다.
-- 놀랄 필요는 없습니다. 64비트 double은 
-- 정확한 int 값을 저장하기 위해 52비트로 구성돼 
-- 있습니다. 52비트 이하의 int 값에 대해서는 
-- 장비 정밀도와 관련된 문제가 생기지 않습니다.

s = 'walternate'  -- 파이썬과 같은 불변 문자열
t = "큰따옴표를 써도 됩니다"
u = [[ 이중 대괄호는
       여러 줄 문자열을
       나타냅니다.]]
t = nil  -- 미정의 t. 루아는 가비지 컬렉션을 지원합니다.

-- 블록은 do/end와 같은 키워드로 나타냅니다:
while num < 50 do
  num = num + 1  --  ++나 += 유형의 연산자는 쓸 수 없습니다.
end

-- If 절:
if num > 40 then
  print('40 이상')
elseif s ~= 'walternate' then  -- ~=은 '같지 않다'입니다.
  -- 동일성 검사는 파이썬과 마찬가지로 ==입니다. 
  -- 문자열에도 쓸 수 있습니다.
  io.write('not over 40\n')  -- 기본적으로 stdout에 씁니다.
else
  -- 변수는 기본적으로 전역 변수입니다.
  thisIsGlobal = 5  -- 낙타 표기법이 일반적입니다.

  -- 변수를 지역 변수로 만드는 방법은 다음과 같습니다:
  local line = io.read()  -- 다음 stdin 줄을 읽습니다

  -- 문자열 연결에는 .. 연산자를 씁니다:
  print('겨울이 오고 있습니다, ' .. line)
end

-- 미정의 변수는 nil을 반환합니다.
-- 다음 코드를 실행해도 오류가 나지 않습니다:
foo = anUnknownVariable  -- 이제 foo는 nil입니다.

aBoolValue = false

-- nil과 false만이 거짓값입니다; 0과 ''은 참입니다!
if not aBoolValue then print('twas false') end

-- 'or'와 'and'는 단축 평가(short-circuit)됩니다.
-- 다음 코드는 C/자바스크립트의 a?b:c 연산자와 비슷합니다:
ans = aBoolValue and 'yes' or 'no'  --> 'no'

karlSum = 0
for i = 1, 100 do  -- 범위에는 마지막 요소도 포함됩니다.
  karlSum = karlSum + i
end

-- 카운트 다운을 할 때는 "100, 1, -1"을 범위로 씁니다.
fredSum = 0
for j = 100, 1, -1 do fredSum = fredSum + j end

-- 일반적으로 범위는 begin, end[, step]입니다.

-- 또 다른 반복문 구문은 다음과 같습니다:
repeat
  print('미래의 방식')
  num = num - 1
until num == 0


----------------------------------------------------
-- 2. 함수
----------------------------------------------------

function fib(n)
  if n < 2 then return n end
  return fib(n - 2) + fib(n - 1)
end

-- 클로저와 익명 함수도 사용할 수 있습니다:
function adder(x)
  -- 반환된 함수는 adder가 호출될 때 생성되고 x의
  -- 값이 유지됩니다:
  return function (y) return x + y end
end
a1 = adder(9)
a2 = adder(36)
print(a1(16))  --> 25
print(a2(64))  --> 100

-- 반환문, 함수 호출, 할당문은 길이가 다른
-- 값의 리스트에 대해서도 모두 동작합니다.
-- 리스트에 값이 더 적을 때는 nil이 할당/반환되고
-- 리스트에 값이 더 많을 때는 나머지 값은 버려집니다.

x, y, z = 1, 2, 3, 4
-- 이제 x = 1, y = 2, z = 3이고 4는 버려집니다.

function bar(a, b, c)
  print(a, b, c)
  return 4, 8, 15, 16, 23, 42
end

x, y = bar('zaphod')  --> "zaphod  nil nil"가 출력
-- 이제 x = 4, y = 8이고 15~42의 값은 버려집니다.

-- 함수는 일급 객체이고, 지역/전역 유효범위를 가질
-- 수 있습니다. 아래의 두 함수는 같습니다:
function f(x) return x * x end
f = function (x) return x * x end

-- 그리고 아래의 두 함수도 마찬가지입니다:
local function g(x) return math.sin(x) end
local g; g  = function (x) return math.sin(x) end
-- 'local g'라고 선언하면 g를 지역 함수로 만듭니다.

-- 그나저나 삼각 함수는 라디안 단위로 동작합니다.

-- 함수를 호출할 때 문자열 매개변수를 하나만 전달한다면
-- 괄호를 쓰지 않아도 됩니다:
print 'hello'  -- 잘 동작합니다.


----------------------------------------------------
-- 3. 테이블
----------------------------------------------------

-- 테이블 = 루아의 유일한 복합 자료구조로서, 연관 배열입니다.
-- PHP의 배열이나 자바스크립트의 객체와 비슷하며,
-- 리스트로도 사용할 수 있는 해시 기반의 딕셔너리입니다.

-- 테이블을 딕셔너리/맵으로 사용하기:

-- 딕셔너리 리터럴은 기본적으로 문자열 키를 가집니다:
t = {key1 = 'value1', key2 = false}

-- 문자열 키에는 자바스크립트와 유사한 점 표기법을 쓸 수 있습니다:
print(t.key1)  -- 'value1'을 출력.
t.newKey = {}  -- 새 키/값 쌍을 추가.
t.key2 = nil   -- 테이블에서 key2를 제거.

-- (nil이 아닌) 값을 키로 사용하는 리터럴 표기법:
u = {['@!#'] = 'qbert', [{}] = 1729, [6.28] = 'tau'}
print(u[6.28])  -- "tau"가 출력

-- 키 매칭은 기본적으로 숫자와 문자열에 대해서는 값으로 하지만
-- 테이블에 대해서는 식별자로 합니다.
a = u['@!#']  -- Now a = 'qbert'.
b = u[{}]     -- We might expect 1729, but it's nil:
a = u['@!#']  -- 이제 a는 'qbert'입니다.
b = u[{}]     -- 1729를 예상했겠지만 nil입니다:
-- 탐색이 실패하기 때문에 b는 nil입니다. 탐색이 실패하는 이유는
-- 사용된 키가 원본 값을 저장할 때 사용한 키와 동일한 객체가 아니기
-- 때문입니다. 따라서 문자열 및 숫자가 좀 더 이식성 있는 키입니다.

-- 테이블 하나를 매개변수로 취하는 함수를 호출할 때는 괄호가 필요하지 않습니다:
function h(x) print(x.key1) end
h{key1 = 'Sonmi~451'}  -- 'Sonmi~451'를 출력.

for key, val in pairs(u) do  -- 테이블 순회
  print(key, val)
end

-- _G는 모든 전역 멤버에 대한 특별한 테이블입니다.
print(_G['_G'] == _G)  -- 'true'가 출력

-- 테이블을 리스트/배열로 사용하기:

-- 리스트 리터럴은 암묵적으로 int 키로 설정됩니다:
v = {'value1', 'value2', 1.21, 'gigawatts'}
for i = 1, #v do  -- #v는 리스트 v의 크기입니다.
  print(v[i])  -- 인덱스가 1에서 시작합니다!! 제정신이 아닙니다!
end
-- 'list'는 실제 타입이 아닙니다. v는 연속된 정수형 키가 포함된
-- 테이블이고 리스트로 취급될 뿐입니다.

----------------------------------------------------
-- 3.1 메타테이블과 메타메서드
----------------------------------------------------

-- 테이블은 테이블에 연산자 오버로딩을 가능하게 하는 메타테이블을
-- 가질 수 있습니다. 나중에 메타테이블이 어떻게 자바스크립트 
-- 프로토타입과 같은 행위를 지원하는지 살펴보겠습니다.

f1 = {a = 1, b = 2}  -- 분수 a/b를 표현
f2 = {a = 2, b = 3}

-- 다음 코드는 실패합니다:
-- s = f1 + f2

metafraction = {}
function metafraction.__add(f1, f2)
  sum = {}
  sum.b = f1.b * f2.b
  sum.a = f1.a * f2.b + f2.a * f1.b
  return sum
end

setmetatable(f1, metafraction)
setmetatable(f2, metafraction)

s = f1 + f2  -- f1의 메타테이블을 대상으로 __add(f1, f2)를 호출

-- f1과 f2는 자바스크립트의 프로토타입과 달리 각 메타테이블에 대한 
-- 키가 없어서 getmetatable(f1)과 같이 받아와야 합니다.
-- 메타테이블은 __add 같은 루아가 알고 있는 키가 지정된 일반 테이블입니다.

-- 그렇지만 다음 줄은 s가 메타테이블을 가지고 있지 않기 때문에 실패합니다.
-- t = s + s
-- 아래와 같이 클래스와 유사한 패턴은 이러한 문제가 발생하지 않습니다.

-- 메타테이블에 대한 __index는 점을 이용한 탐색을 오버로드합니다:
defaultFavs = {animal = 'gru', food = 'donuts'}
myFavs = {food = 'pizza'}
setmetatable(myFavs, {__index = defaultFavs})
eatenBy = myFavs.animal  -- 동작합니다! 고마워요, 메타테이블!

-- 직접적인 메타테이블 탐색이 실패할 경우 메타테이블의 __index 값을 이용해
-- 재시도하고, 이런 과정이 반복됩니다.

-- __index 값은 좀 더 세분화된 탐색을 위해 function(tbl, key)가
-- 될 수도 있습니다.

-- __index, __add, ...의 값을 메타메서드라고 합니다.
-- 다음은 메타메서드를 가진 테이블의 전체 목록입니다.

-- __add(a, b)                     for a + b
-- __sub(a, b)                     for a - b
-- __mul(a, b)                     for a * b
-- __div(a, b)                     for a / b
-- __mod(a, b)                     for a % b
-- __pow(a, b)                     for a ^ b
-- __unm(a)                        for -a
-- __concat(a, b)                  for a .. b
-- __len(a)                        for #a
-- __eq(a, b)                      for a == b
-- __lt(a, b)                      for a < b
-- __le(a, b)                      for a <= b
-- __index(a, b)  <fn이나 테이블>  for a.b
-- __newindex(a, b, c)             for a.b = c
-- __call(a, ...)                  for a(...)

----------------------------------------------------
-- 3.2 클래스 형태의 테이블과 상속
----------------------------------------------------

-- 루아에는 클래스가 내장돼 있지 않으며, 테이블과 메타테이블을
-- 이용해 클래스를 만드는 다양한 방법이 있습니다.

-- 다음 예제에 대한 설명은 하단을 참조합니다.

Dog = {}                                   -- 1.

function Dog:new()                         -- 2.
  newObj = {sound = 'woof'}                -- 3.
  self.__index = self                      -- 4.
  return setmetatable(newObj, self)        -- 5.
end

function Dog:makeSound()                   -- 6.
  print('I say ' .. self.sound)
end

mrDog = Dog:new()                          -- 7.
mrDog:makeSound()  -- 'I say woof'         -- 8.

-- 1. Dog는 클래스처럼 동작합니다. 실제로는 테이블입니다.
-- 2. function 테이블명:fn(...)은
--    function 테이블명.fn(self, ...)과 같습니다.
--    :는 self라는 첫 번째 인자를 추가할 뿐입니다.
--    self가 값을 어떻게 얻는지 궁금하다면 아래의 7과 8을 읽어보세요.
-- 3. newObj는 Dog 클래스의 인스턴스가 됩니다.
-- 4. self = 인스턴스화되는 클래스.
--    주로 self = Dog이지만 상속을 이용하면 이것을 바꿀 수 있습니다. 
--    newObj의 메타테이블과 self의 __index를 모두 self에 설정하면
--    newObj가 self의 함수를 갖게 됩니다.
-- 5. 참고: setmetatable은 첫 번째 인자를 반환합니다.
-- 6. :는 2에서 설명한 것과 같이 동작하지만 이번에는 self가 
--    클래스가 아닌 인스턴스라고 예상할 수 있습니다.
-- 7. Dog.new(Dog)과 같으므로 new()에서는 self = Dog입니다.
-- 8. mrDog.makeSound(mrDog)과 같으므로 self = mrDog입니다.

----------------------------------------------------

-- 상속 예제:

LoudDog = Dog:new()                           -- 1.

function LoudDog:makeSound()
  s = self.sound .. ' '                       -- 2.
  print(s .. s .. s)
end

seymour = LoudDog:new()                       -- 3.
seymour:makeSound()  -- 'woof woof woof'      -- 4.

-- 1. LoudDog은 Dog의 메서드와 변수를 갖게 됩니다.
-- 2. self는 new()에서 'sound' 키를 가집니다. 3을 참고하세요.
-- 3. LoudDog.new(LoudDog)과 같고, LoudDog은 'new' 키가 없지만
--    메타테이블에서 __index = Dog이기 때문에 Dog.new(LoudDog)으로
--    변환됩니다.
--    결과: seymour의 메타테이블은 LoudDog이고 LoudDog.__index는
--    LoudDog입니다. 따라서 seymour.key는 seymour.key, 
--    LoudDog.key, Dog.key와 같을 것이며, 지정한 키에 어떤 테이블이
--    오든 상관없을 것입니다.
-- 4. 'makeSound' 키는 LoudDog에서 발견할 수 있습니다. 
--    이것은 LoudDog.makeSound(seymour)와 같습니다.

-- 필요할 경우, 하위 클래스의 new()는 기반 클래스의 new()와 유사합니다.
function LoudDog:new()
  newObj = {}
  -- newObj를 구성
  self.__index = self
  return setmetatable(newObj, self)
end

----------------------------------------------------
-- 4. 모듈
----------------------------------------------------


--[[ 여기서 주석을 제거하면 이 스크립트의 나머지 부분은 
--   실행 가능한 상태가 됩니다.
```

```lua
-- mod.lua 파일의 내용이 다음과 같다고 가정해 봅시다.
local M = {}

local function sayMyName()
  print('이소룡')
end

function M.sayHello()
  print('안녕하세요')
  sayMyName()
end

return M

-- 또 다른 파일에서는 mod.lua의 기능을 이용할 수 있습니다.
local mod = require('mod')  -- mod.lua 파일을 실행

-- require는 모듈을 포함시키는 표준화된 방법입니다.
-- require는 다음과 같이 동작합니다:     (캐싱돼 있지 않을 경우. 하단 참조)
-- mod.lua가 함수의 본문처럼 되므로 mod.lua 안의 지역 멤버는
-- 밖에서 볼 수 없습니다.

-- 다음 코드가 동작하는 것은 mod가 mod.lua의 M과 같기 때문입니다.
mod.sayHello()  -- 이소룡 씨에게 인사를 건넵니다.

-- 다음 코드를 실행하면 오류가 발생합니다.
-- sayMyName는 mod.lua 안에서만 존재하기 때문입니다:
mod.sayMyName()  -- 오류

-- require의 반환값은 캐싱되므로 require를 여러 번 실행해도
-- 파일은 최대 한 번만 실행됩니다.

-- mod2.lua에 "print('Hi')"가 들어 있다고 가정해 봅시다.
local a = require('mod2')  -- Hi!를 출력
local b = require('mod2')  -- print를 실행하지 않음. a=b

-- dofile은 require와 비슷하지만 캐싱을 하지 않습니다:
dofile('mod2')  --> Hi!
dofile('mod2')  --> Hi! (require와 달리 다시 한번 실행됨)

-- loadfile은 루아 파일을 읽어들이지만 실행하지는 않습니다
f = loadfile('mod2')  -- f()를 호출해야 mod2.lua가 실행됩니다.

-- loadstring은 문자열에 대한 loadfile입니다.
g = loadstring('print(343)')  -- 함수를 반환합니다.
g()  -- 343이 출력됩니다. 그전까지는 아무것도 출력되지 않습니다.

--]]

```

## 참고자료

루아를 배우는 일이 흥미진진했던 이유는 <a href="http://love2d.org/">Love 2D 게임 엔진</a>을 이용해 
게임을 만들 수 있었기 때문입니다. 이것이 제가 루아를 배운 이유입니다.

저는 <a href="http://nova-fusion.com/2012/08/27/lua-for-programmers-part-1/">BlackBulletIV의 "프로그래머를 위한 루아"</a>로
시작했습니다. 그다음으로 공식 <a href="http://www.lua.org/pil/contents.html">"프로그래밍 루아"</a> 책을 읽었습니다.
그렇게 루아를 배웠습니다.

lua-users.org에 있는 <a href="http://lua-users.org/files/wiki_insecure/users/thomasl/luarefv51.pdf">짧은 루아 레퍼런스</a>를
읽어두면 도움될지도 모르겠습니다.

여기서는 표준 라이브러리에 관해서는 다루지 않았습니다.

* <a href="http://lua-users.org/wiki/StringLibraryTutorial">string 라이브러리</a>
* <a href="http://lua-users.org/wiki/TableLibraryTutorial">table 라이브러리</a>
* <a href="http://lua-users.org/wiki/MathLibraryTutorial">math 라이브러리</a>
* <a href="http://lua-users.org/wiki/IoLibraryTutorial">io 라이브러리</a>
* <a href="http://lua-users.org/wiki/OsLibraryTutorial">os 라이브러리</a>

그나저나 이 파일 전체는 유효한 루아 프로그램입니다. 이 파일을
learn.lua로 저장한 후 "lua learn.lua"를 실행해 보세요!

이 글은 tylerneylon.com에 처음으로 써본 글이며, 
<a href="https://gist.github.com/tylerneylon/5853042">GitHub의 Gist</a>에서도 확인할 수 있습니다.
루아로 즐거운 시간을 보내세요!
