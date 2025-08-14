---
name: MiniScript
contributors:
    - ["Joe Strout", "https://github.com/JoeStrout"]
filename: miniscript.ms
---

**MiniScript**는 게임 및 기타 소프트웨어에 쉽게 내장되도록 설계된 간단한 스크립팅 언어입니다. 명령줄에서 사용하거나 [Soda](https://github.com/JoeStrout/soda) 또는 [Mini Micro](https://miniscript.org/MiniMicro)를 통해 크로스 플랫폼 게임 개발 환경으로 사용할 수도 있습니다.

MiniScript를 시작하는 쉬운 방법은 서버에서 MiniScript 코드를 실행하는 [Try-It! 페이지](https://miniscript.org/tryit/)를 이용하는 것입니다. 그러나 이 페이지의 코드는 2000자로 제한됩니다. (아래 튜토리얼 스크립트는 Try-It! 페이지에서 실행되도록 2048자 이하의 블록으로 나뉘어 있습니다.)

Try-It! 페이지를 넘어설 준비가 되면, 다음 단계는 명령줄과 프로그램 모두에서 MiniScript를 사용하는 무료 가상 컴퓨터인 [Mini Micro](https://miniscript.org/MiniMicro)를 다운로드하는 것입니다. 해당 환경에서 프롬프트에 **edit**를 입력하여 코드를 편집한 다음, 편집기에서 실행 버튼을 클릭하여 실행하십시오.

```
print "Hello world"

// MiniScript는 매우 구문이 가볍습니다. 위 print 문에 괄호가
// 필요하지 않다는 점에 유의하십시오. 주석은 //로 시작하여
// 줄 끝까지 이어집니다. MiniScript는 대소문자를 구분합니다.

// 흐름 제어
// 어떤 조건에 따라 다른 작업을 수행하려면 if 블록을 사용하십시오.
// 0개 이상의 else if 블록과 선택적인 else 블록 하나를 포함할 수 있습니다.
if 2+2 == 4 then
   print "수학이 작동합니다!"
else if pi > 3 then
   print "파이는 맛있습니다"
else if "a" < "b" then
   print "정렬할 수 있습니다"
else
   print "마지막 기회"
end if

// 반복
// MiniScript에는 while 루프와 for 루프 두 가지 반복 구문만 있습니다.
// 조건이 참인 동안 반복하려면 while 블록을 사용하십시오.
s = "Spam"
while s.len < 50
    s = s + ", spam"
end while
print s + " and spam!"

// for 루프는 range 함수로 쉽게 만들 수 있는 리스트를 포함하여
// 모든 리스트를 반복할 수 있습니다.
for i in range(10, 1)
    print i + "..."
end for
print "발사!"

// 루프 내에서 유용한 두 가지 추가 키워드가 있습니다. break 문은
// 가장 가까운 while 또는 for 루프를 빠져나옵니다. continue 문은
// 현재 반복의 나머지 부분을 건너뛰고 루프의 맨 위로 점프합니다.
for i in range(1,100)
    if i % 3 == 0 then continue  // 3의 배수 건너뛰기
    if i^2 > 200 then break  // i^2이 200을 초과하면 중지
    print i + "의 제곱은 " + i^2
end for
```

### 숫자

```
// 모든 숫자는 전체 정밀도 형식으로 저장됩니다. 숫자는 또한
// 참(1)과 거짓(0)을 나타내며, 이에 대한 내장 키워드
// (true 및 false)가 있습니다.
a = 7
b = 3
ultimateAnswer = 42
pi = 3.14159
n = true
m = false
print ultimateAnswer + ", " + pi + ", " + n + ", " + m

// 숫자는 다음 연산자를 지원합니다:
print "기본 수학:"
print a + b     // 덧셈
print a - b     // 뺄셈
print a * b     // 곱셈
print a / b     // 나눗셈
print a % b     // 모듈로 (나머지)
print a ^ b     // 거듭제곱

print "논리:"
print n and m   // 논리 "and"
print n or m    // 논리 "or"
print not n     // 논리 부정

print "비교:"
print a == b    // 동등성 테스트 (여기서는 =가 아닌 ==에 유의!)
print a != b    // 부등
print a > b     // 보다 큼
print a >= b    // 보다 크거나 같음
print a < b     // 보다 작음
print a <= b    // 보다 작거나 같음
```

### 문자열

```
// 텍스트는 유니코드 문자의 문자열로 저장됩니다. 따옴표로
// 둘러싸서 문자열을 작성합니다. 문자열에 인용 부호를 포함해야
// 하는 경우 두 번 입력하십시오.
print "Hello, ""Bob""."
a = "Hello"
b = "Spam"

// 문자열은 다음 연산자를 지원합니다:
print "문자열 ""수학"":"
print a + b     // 문자열 연결
print b - "m"   // 문자열 빼기 (자르기)
print b * 4     // 문자열 복제
print a / 2     // 문자열 나누기

print "비교:"
print a == b    // 동등성 테스트 (여기서는 =가 아닌 ==에 유의!)
print a != b    // 부등
print a > b     // 보다 큼
print a >= b    // 보다 크거나 같음
print a < b     // 보다 작음
print a <= b    // 보다 작거나 같음

// 문자열의 인덱싱 및 슬라이싱은 대괄호 안에 인덱스(또는 두 개)를
// 사용하여 수행됩니다. 앞에서부터 세려면 0부터 시작하는 인덱스를 사용하고,
// 뒤에서부터 세려면 음수 인덱스를 사용합니다. 콜론으로 구분된
// 두 개의 인덱스로 슬라이스(부분 문자열)를 가져옵니다. 둘 중 하나를 생략하여
// 슬라이스를 문자열의 시작 또는 끝까지 확장할 수 있습니다.
print "인덱싱 및 슬라이싱:"
print a[0]      // 0부터 시작하는 문자 가져오기 ("H")
print a[1]      // 두 번째 문자 가져오기 ("e")
print a[-1]     // 음수는 뒤에서부터 셉니다 ("o")
print a[1:4]    // 1부터 4 미만까지 슬라이스 가져오기 ("ell")
print a[1:-1]   // 위와 같지만 음수 인덱스 사용
print a[1:]     // 1부터 끝까지 슬라이스 가져오기 ("ello")
print a[:2]     // 처음부터 2 미만까지 슬라이스 가져오기 ("He")

// MiniScript의 문자열은 불변입니다. 문자열에 접근하여
// 포함된 문자를 변경할 수는 없지만(항상 다른 문자로
// 새 문자열을 만들 수는 있습니다).
```

### 리스트

```
// 리스트는 모든 유형의 값으로 구성된 순서 있는 시퀀스입니다.
// for 루프로 리스트를 반복하거나 .indexes를 사용하여
// 인덱스를 반복할 수 있습니다.
x = ["alpha", "beta", "gamma", "delta"]
for item in x
    print item
end for
for i in x.indexes
    print "x[" + i + "] is " + x[i]
end for

// 리스트의 인덱싱 및 슬라이싱은 문자열과 정확히 같습니다.
// 앞에서부터 세려면 0부터 시작하는 인덱스를 사용하고, 뒤에서부터
// 세려면 음수를 사용합니다. 콜론으로 구분된 두 개의 인덱스로
// 리스트의 슬라이스(부분 집합)를 가져옵니다. 둘 중 하나를 생략하여
// 슬라이스를 리스트의 시작 또는 끝까지 확장할 수 있습니다.
print x[0]      // alpha
print x[-1]     // delta
print x[1:3]    // [beta, gamma]
print x[2:]     // [gamma, delta]
print x[:-1]    // [alpha, beta, gamma]

// 리스트는 다음 연산자를 지원합니다:
y = ["a", "be", "ce", "de"]
print "리스트 ""수학"":"
print x + y     // 리스트 연결
print y * 3     // 리스트 복제
print x / 2     // 리스트 나누기

print "비교:"
print x == y    // 동등성 테스트 (여기서는 =가 아닌 ==에 유의!)
print x != y    // 부등
```

### 맵

```
// 맵은 고유한 키와 연관된 값의 집합입니다. 맵은
// 데이터 레코드, 객체, 희소 배열 등을 나타내는 데 사용되는
// 매우 강력하고 다재다능한 데이터 유형입니다.
// 중괄호로 맵을 만들고 대괄호로 단일 값을 가져오거나
// 설정합니다. 키와 값은 모든 유형이 될 수 있습니다.
// (맵의 컨텍스트에서 "키"와 "인덱스"는 같은 의미입니다.)
m = {1:"one", 2:"two"}
print m[1]      // one
m[2] = "dos"    // 인덱스 2와 연관된 값 변경
print m[2]      // dos

// 키(인덱스)가 유효한 변수 이름이 되는 문자열인 특수한 경우,
// 대괄호 구문 대신 점 구문이 있습니다. 맵과 점(마침표) 뒤에
// 따옴표 없이 키를 넣으십시오.
m.pi = 3.14     // m["pi"] = 3.14와 동일
print m["pi"]   // 3.14
m["e"] = 2.72   // m.e = 2.72와 동일
print m.e       // 2.72

// 맵은 + 연산자만 지원하며, 두 맵의 모든 키/값 쌍을
// 하나로 결합합니다.
m1 = {1:"one", 2:"two"}
m2 = {2:"dos", 3:"tres"}
print m1 + m2   // 맵 연결

// for 루프로 맵의 키/값 쌍을 반복할 수 있습니다.
// 각 반복에서 변수는 "key"와 "value" 인덱스가 있는
// 작은 맵이 됩니다.
for kv in m1+m2
    print kv.key + " -> " + kv.value
end for

// 맵의 키/값 쌍 순서는 정의되지 않습니다.
// 맵을 인쇄하거나 반복할 때 특정 순서로 나타날 것이라고
// 가정해서는 안 됩니다.
```

### 함수

```
// miniscript에서 함수는 function...end function 블록으로 만듭니다.
// 대부분의 경우 나중에 호출할 수 있도록 결과를 변수에 할당합니다.
// 함수가 결과를 반환해야 하는 경우 return 키워드로 수행합니다.
rollDie = function
    return ceil(rnd * 6)  // 1-6 사이의 난수 반환
end function
print rollDie
print rollDie

// 매개변수가 필요한 경우 function 키워드 뒤에 괄호 안에 넣습니다.
// 매개변수는 기본값을 가질 수 있습니다.
roll = function(numberOfDice, sides=6)
    sum = 0
    for i in range(1, numberOfDice)
        sum = sum + ceil(rnd * sides)
    end for
    return sum
end function
print roll(2)     // 6면체 주사위 2개 굴리기
print roll(2,20)  // 20면체 주사위 2개 굴리기

// 변수는 MiniScript에서 기본적으로 항상 지역 변수입니다.
// 위 함수의 변수 i와 sum은 함수 외부에서 액세스할 수 없으며
// 함수가 반환되는 즉시 사라집니다. (변수 범위에 대해서는 나중에 자세히 설명합니다.)

// 괄호는 (1) 함수에 인수(매개변수 값)를 전달하고 있고
// (2) 결과를 더 큰 문의 일부로 사용하는 경우에만 필요합니다.
// 위의 첫 번째 예제에서 rollDie는 인수를 전달하지 않았기 때문에
// 괄호가 필요하지 않았습니다. 다음은 내장 print 함수와 같이
// 그 자체로 문으로 사용되어 괄호가 필요하지 않은 함수의 예입니다.
doRoll = function(numberOfDice, sides=6)
    print "Rolling " + numberOfDice + "d" + sides + "..."
    sum = 0
    for i in range(1, numberOfDice)
        roll = ceil(rnd * sides)
        print "You rolled a " + roll + "."
        sum = sum + roll
    end for
    print "Your total is: " + sum
end function
doRoll 3         // 3d6 굴리기 -- 괄호 필요 없음
doRoll 3, 8      // 여기도 마찬가지지만 3d8 굴리기

// 함수를 호출하지 않고 참조해야 하는 경우 @ 연산자를 사용할 수 있습니다.
f = @doRoll      // f가 doRoll과 동일한 함수를 참조하도록 함
f 2,4            // 2d4 굴리기
```

### 클래스와 객체

```
// MiniScript는 프로토타입 기반 상속을 사용합니다. 클래스나 객체는
// 부모 클래스를 가리키는 특수 __isa 항목이 있는 맵일 뿐입니다.
// 이것은 new 연산자를 사용할 때 자동으로 설정됩니다.
Shape = {}            // 기본 클래스 만들기
Shape.sides = 0       // 기본적으로 0개의 변을 가짐

Square = new Shape    // Shape의 서브클래스 Square 만들기
Square.sides = 4      // 변의 수 재정의

x = new Square        // Square 클래스의 인스턴스 만들기
print x.sides         // 4, x는 Square이고 Square.sides는 4이므로

// 메서드는 클래스(맵)에 저장된 함수일 뿐입니다. 이들은
// 다른 값과 마찬가지로 __isa 체인을 통해 상속됩니다.
// 메서드 내에서 self 키워드는 메서드가 호출된 객체(점 구문 사용)를
// 참조합니다. 이것이 객체의 데이터나 메서드를 참조하는 방법입니다.
Shape.describe = function
    print
    print "This is a " + self.sides + "-sided shape."
end function
x.describe            // This is a 4-sided shape.

// 메서드는 (다시 값처럼) 재정의될 수 있습니다. 서브클래스/인스턴스
// 메서드에서 super를 사용하여 상속 체인의 다음 버전 메서드를
// 호출하면서도 self를 이 메서드가 호출된 객체에 바인딩된 상태로
// 유지할 수 있습니다.
Square.describe = function
    super.describe    // 먼저 표준 설명 수행
    print "It looks very squarish."  // 그런 다음 이것을 추가
end function
x.describe
```

### 변수 범위에 대한 추가 정보

```
// MiniScript의 변수 할당은 항상 지역 변수, 즉
// 할당을 포함하는 함수 내에서만 존재하는 변수를 만들거나
// 업데이트합니다. 단, 점 구문을 사용하여 다른 범위를 지정하는 경우는
// 예외입니다.
x = 42      // x라는 전역 변수
f = function
    x = 1234   // x라는 지역 변수 만들기
    print "Inside the function, x is now " + x
end function
f
print "Outside the function, x is " + x

// 위 예에서 함수 내의 x에 대한 할당은
// 우연히 같은 이름을 가졌음에도 불구하고 전역 x 값에
// 영향을 미치지 않습니다. (이것은 의도하지 않은 부작용을
// 피하는 데 도움이 되므로 좋은 점입니다.) 전역 변수는
// 일반적으로 권장되지 않지만, 함수 내에서 업데이트해야 하는 경우
// "globals." 접두사를 사용하여 그렇게 할 수 있습니다.
f = function
    print "Using the globals prefix..."
    globals.x = 1234   // 전역 변수 x 업데이트
    print "Inside the function, x is now " + x
end function
f
print "Outside the function, x is " + x

// 이것은 클래스 메서드와 함께 사용되는 "self." 접두사와 매우 유사합니다.
// 두 경우 모두 변수에 더 구체적인 범위를 제공합니다(실제로는
// 점 구문으로 인덱싱할 맵을 지정하는 것일 뿐입니다).

// 그러나 중요한 차이점이 있습니다. 변수를 할당하는 것이 아니라
// 읽을 때, 변수 이름이 지역 변수 중에서 발견되지 않으면
// MiniScript는 자동으로 해당 이름의 전역 변수를 찾습니다.
// 따라서 변수를 읽을 때는 "globals." 접두사가 필요하지 않지만
// 할당할 때는 필요합니다.
count = 0
addToCount = function(amount=1)
    globals.count = count + amount
end function
addToCount
addToCount
print "count is now: " + count

// 위 addToCount 함수에서 할당의 왼쪽에는 globals 접두사가
// 필요한 반면, 오른쪽에는 전역 값을 읽기만 하므로
// 필요하지 않다는 점에 유의하십시오.
```

### 유용한 내장 메서드

```
// 내장 메서드는 MiniScript나 그 환경에 내장된 메서드입니다.
// 특정 MiniScript 환경(예: Mini Micro, Soda, 명령줄 MiniScript,
// MiniScript를 내장 언어로 사용하는 일부 게임 등)은 아마도
// 추가 내장 함수를 추가할 것입니다. 그러나 항상 사용할 수 있어야 하는
// 약 50개의 핵심 내장 함수가 있습니다.

// 가장 일반적으로 사용되는 몇 가지에 대한 빠른 데모입니다.
print abs(-42)         // 절대값
print pi               // 파이 값 가져오기 (예, 내장되어 있습니다!)
print cos(pi)          // 코사인
print sqrt(100)        // 제곱근
print round(pi, 2)     // 반올림 (소수점 2자리까지)
print char(65)         // 유니코드 문자 65 가져오기

print
s = "Hello world!"
print s.upper          // 대문자로 변환
print s.len            // 길이 가져오기 (문자 수)
print s.replace("Hello", "Heya")  // 문자열 대체
print s.split(" ")     // 공백으로 분할하여 리스트 만들기
print s.remove("l")    // "l"의 첫 번째 항목 제거

print
a = range(2,15,3)      // 리스트 만들기: 2부터 15까지, 3단계씩
print "a: " + a
print "a.len:" + a.len // 길이 가져오기 (값 수)
print "a.sum:" + a.sum // 모든 값을 더한 합계 가져오기
print a.pop            // 마지막 값 꺼내기
print a.pull           // 첫 번째 값 꺼내기
print "popped and pulled: " + a
a.push 99              // 끝에 새 항목 추가
a.insert 0, 101        // 인덱스 0에 새 항목 삽입
print "after push and insert: " + a
a.remove 2             // 리스트에서 인덱스 2 제거
print "after remove 2: " + a
s = a.join("#")        // #으로 값을 결합하여 문자열 만들기
print s

print
m = {"one": "uno", "two": "dos", "three": "tres"}
print m.hasIndex("one") // 키 존재 여부 확인
print m.indexes         // 모든 인덱스 가져오기
print m.values          // 모든 값 가져오기
m.remove "two"          // 인덱스 (및 해당 값) 제거
print m
```

## 더 읽을거리

* [MiniScript.org 웹사이트](https://miniscript.org/) — MiniScript 세계의 중심
* [MiniScript 빠른 참조](https://miniscript.org/files/MiniScript-QuickRef.pdf) — 이 튜토리얼을 한 페이지로
* [MiniScript 사용자 매뉴얼](https://miniscript.org/files/MiniScript-Manual.pdf) — 더 심층적인 문서
* [MiniScript 위키](https://miniscript.org/wiki/) — 커뮤니티 기반 문서
