# python.md (번역)

---
name: Python
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["Zachary Ferguson", "http://github.com/zfergus2"]
    - ["evuez", "http://github.com/evuez"]
    - ["Rommel Martinez", "https://ebzzry.io"]
    - ["Roberto Fernandez Diaz", "https://github.com/robertofd1995"]
    - ["caminsha", "https://github.com/caminsha"]
    - ["Stanislav Modrak", "https://stanislav.gq"]
    - ["John Paul Wohlscheid", "https://gitpi.us"]
filename: learnpython.py
---

Python은 90년대 초에 Guido van Rossum에 의해 만들어졌습니다. 지금은 가장 인기 있는 언어 중 하나입니다. 저는 Python의 구문적 명확성에 반했습니다. 기본적으로 실행 가능한 의사 코드입니다.

```python
# 한 줄 주석은 숫자 기호로 시작합니다.

""" 여러 줄 문자열은
    세 개의 "를 사용하여 작성할 수 있으며, 종종
    문서로 사용됩니다.
"""

####################################################
## 1. 기본 데이터 타입 및 연산자
####################################################

# 숫자가 있습니다.
3  # => 3

# 수학은 예상대로입니다.
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20
35 / 5  # => 7.0

# 내림 나눗셈은 음의 무한대 방향으로 반올림합니다.
5 // 3       # => 1
-5 // 3      # => -2
5.0 // 3.0   # => 1.0  # 부동 소수점에서도 작동합니다.
-5.0 // 3.0  # => -2.0

# 나눗셈의 결과는 항상 부동 소수점입니다.
10.0 / 3  # => 3.3333333333333335

# 나머지 연산
7 % 3   # => 1
# i % j는 C와 달리 j와 같은 부호를 가집니다.
-7 % 3  # => 2

# 거듭제곱 (x**y, x의 y 제곱)
2**3  # => 8

# 괄호로 우선순위 강제
1 + 3 * 2    # => 7
(1 + 3) * 2  # => 8

# 불리언 값은 기본 타입입니다 (참고: 대소문자 구분)
True   # => True
False  # => False

# not으로 부정
not True   # => False
not False  # => True

# 불리언 연산자
# "and"와 "or"은 대소문자를 구분합니다.
True and False  # => False
False or True   # => True

# True와 False는 실제로 1과 0이지만 다른 키워드를 사용합니다.
True + True  # => 2
True * 8     # => 8
False - 5    # => -5

# 비교 연산자는 True와 False의 숫자 값을 봅니다.
0 == False   # => True
2 > True     # => True
2 == True    # => False
-5 != False  # => True

# None, 0, 빈 문자열/리스트/딕셔너리/튜플/세트는 모두 False로 평가됩니다.
# 다른 모든 값은 True입니다.
bool(0)      # => False
bool("")     # => False
bool([])     # => False
bool({})     # => False
bool(())     # => False
bool(set())  # => False
bool(4)      # => True
bool(-6)     # => True

# int에 불리언 논리 연산자를 사용하면 평가를 위해 불리언으로 캐스팅되지만,
# 캐스팅되지 않은 값이 반환됩니다. bool(ints)와 비트
# and/or (&,|)를 혼동하지 마십시오.
bool(0)   # => False
bool(2)   # => True
0 and 2   # => 0
bool(-5)  # => True
bool(2)   # => True
-5 or 0   # => -5

# 같음은 == 입니다.
1 == 1  # => True
2 == 1  # => False

# 같지 않음은 != 입니다.
1 != 1  # => False
2 != 1  # => True

# 더 많은 비교
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# 값이 범위에 있는지 확인
1 < 2 and 2 < 3  # => True
2 < 3 and 3 < 2  # => False
# 연쇄를 사용하면 더 보기 좋습니다.
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# (is vs. ==) is는 두 변수가 동일한 객체를 참조하는지 확인하지만, ==는
# 가리키는 객체가 동일한 값을 갖는지 확인합니다.
a = [1, 2, 3, 4]  # a를 새 목록 [1, 2, 3, 4]로 지정
b = a             # b를 a가 가리키는 것으로 지정
b is a            # => True, a와 b는 동일한 객체를 참조
b == a            # => True, a와 b의 객체는 동일
b = [1, 2, 3, 4]  # b를 새 목록 [1, 2, 3, 4]로 지정
b is a            # => False, a와 b는 동일한 객체를 참조하지 않음
b == a            # => True, a와 b의 객체는 동일

# 문자열은 " 또는 '로 생성됩니다.
"This is a string."
'This is also a string.'

# 문자열도 추가할 수 있습니다.
"Hello " + "world!"  # => "Hello world!"
# 문자열 리터럴(변수는 아님)은 '+'를 사용하지 않고 연결할 수 있습니다.
"Hello " "world!"    # => "Hello world!"

# 문자열은 문자 목록처럼 처리할 수 있습니다.
"Hello world!"[0]  # => 'H'

# 문자열의 길이를 찾을 수 있습니다.
len("This is a string")  # => 16

# Python 3.6부터 f-문자열 또는 서식화된 문자열 리터럴을 사용할 수 있습니다.
name = "Reiko"
f"She said her name is {name}."  # => "She said her name is Reiko"
# 이 중괄호 안의 모든 유효한 Python 표현식은 문자열로 반환됩니다.
f"{name} is {len(name)} characters long."  # => "Reiko is 5 characters long."

# None은 객체입니다.
None  # => None

# 객체를 None과 비교하기 위해 등호 "==" 기호를 사용하지 마십시오.
# 대신 "is"를 사용하십시오. 이것은 객체 ID의 동등성을 확인합니다.
"etc" is None  # => False
None is None   # => True

####################################################
## 2. 변수 및 컬렉션
####################################################

# Python에는 print 함수가 있습니다.
print("I'm Python. Nice to meet you!")  # => I'm Python. Nice to meet you!

# 기본적으로 print 함수는 끝에 줄 바꿈도 출력합니다.
# 끝 문자열을 변경하려면 선택적 인수 end를 사용하십시오.
print("Hello, World", end="!")  # => Hello, World!

# 콘솔에서 입력 데이터를 간단하게 가져오는 방법
input_string_var = input("Enter some data: ")  # 데이터를 문자열로 반환

# 선언은 없고 할당만 있습니다.
# 변수 명명 규칙은 snake_case 스타일입니다.
some_var = 5
some_var  # => 5

# 이전에 할당되지 않은 변수에 액세스하면 예외가 발생합니다.
# 예외 처리에 대해 자세히 알아보려면 제어 흐름을 참조하십시오.
some_unknown_var  # NameError 발생

# if는 표현식으로 사용할 수 있습니다.
# C의 '?:' 삼항 연산자와 동일합니다.
"yay!" if 0 > 1 else "nay!"  # => "nay!"

# 리스트는 시퀀스를 저장합니다.
li = []
# 미리 채워진 리스트로 시작할 수 있습니다.
other_li = [4, 5, 6]

# append로 리스트 끝에 항목 추가
li.append(1)    # li는 이제 [1]입니다.
li.append(2)    # li는 이제 [1, 2]입니다.
li.append(4)    # li는 이제 [1, 2, 4]입니다.
li.append(3)    # li는 이제 [1, 2, 4, 3]입니다.
# pop으로 끝에서 제거
li.pop()        # => 3이고 li는 이제 [1, 2, 4]입니다.
# 다시 넣자
li.append(3)    # li는 이제 다시 [1, 2, 4, 3]입니다.

# 다른 배열처럼 리스트에 액세스
li[0]   # => 1
# 마지막 요소 보기
li[-1]  # => 3

# 범위를 벗어나면 IndexError입니다.
li[4]  # IndexError 발생

# 슬라이스 구문으로 범위를 볼 수 있습니다.
# 시작 인덱스는 포함되고 끝 인덱스는 포함되지 않습니다.
# (수학적인 분들을 위해 닫힌/열린 범위입니다.)
li[1:3]   # 인덱스 1에서 3까지의 목록 반환 => [2, 4]
li[2:]    # 인덱스 2부터 시작하는 목록 반환 => [4, 3]
li[:3]    # 처음부터 인덱스 3까지의 목록 반환  => [1, 2, 4]
li[::2]   # 2의 단계 크기로 요소를 선택하는 목록 반환 => [1, 4]
li[::-1]  # 역순으로 목록 반환 => [3, 4, 2, 1]
# 고급 슬라이스를 만들기 위해 이들의 조합을 사용하십시오.
# li[start:end:step]

# 슬라이스를 사용하여 한 단계 깊은 복사본 만들기
li2 = li[:]  # => li2 = [1, 2, 4, 3]이지만 (li2 is li)는 false가 됩니다.

# "del"로 리스트에서 임의의 요소 제거
del li[2]  # li는 이제 [1, 2, 3]입니다.

# 값의 첫 번째 발생 제거
li.remove(2)  # li는 이제 [1, 3]입니다.
li.remove(2)  # 2가 목록에 없으므로 ValueError 발생

# 특정 인덱스에 요소 삽입
li.insert(1, 2)  # li는 이제 다시 [1, 2, 3]입니다.

# 인자와 일치하는 첫 번째 항목의 인덱스 가져오기
li.index(2)  # => 1
li.index(4)  # 4가 목록에 없으므로 ValueError 발생

# 리스트를 추가할 수 있습니다.
# 참고: li와 other_li의 값은 수정되지 않습니다.
li + other_li  # => [1, 2, 3, 4, 5, 6]

# "extend()"로 리스트 연결
li.extend(other_li)  # 이제 li는 [1, 2, 3, 4, 5, 6]입니다.

# "in"으로 리스트에 존재 여부 확인
1 in li  # => True

# "len()"으로 길이 검사
len(li)  # => 6


# 튜플은 리스트와 같지만 불변입니다.
tup = (1, 2, 3)
tup[0]      # => 1
tup[0] = 3  # TypeError 발생

# 길이가 1인 튜플은 마지막 요소 뒤에 쉼표가 있어야 하지만
# 0을 포함한 다른 길이의 튜플은 그렇지 않습니다.
type((1))   # => <class 'int'>
type((1,))  # => <class 'tuple'>
type(())    # => <class 'tuple'>

# 튜플에서도 대부분의 리스트 작업을 수행할 수 있습니다.
len(tup)         # => 3
tup + (4, 5, 6)  # => (1, 2, 3, 4, 5, 6)
tup[:2]          # => (1, 2)
2 in tup         # => True

# 튜플(또는 리스트)을 변수로 언패킹할 수 있습니다.
a, b, c = (1, 2, 3)  # a는 이제 1, b는 이제 2, c는 이제 3입니다.
# 확장 언패킹도 할 수 있습니다.
a, *b, c = (1, 2, 3, 4)  # a는 이제 1, b는 이제 [2, 3], c는 이제 4입니다.
# 괄호를 생략하면 기본적으로 튜플이 생성됩니다.
d, e, f = 4, 5, 6  # 튜플 4, 5, 6이 변수 d, e, f로 언패킹됩니다.
# 각각 d = 4, e = 5, f = 6이 되도록
# 이제 두 값을 바꾸는 것이 얼마나 쉬운지 보십시오.
e, d = d, e  # d는 이제 5, e는 이제 4입니다.


# 딕셔너리는 키에서 값으로의 매핑을 저장합니다.
empty_dict = {}
# 미리 채워진 딕셔너리
filled_dict = {"one": 1, "two": 2, "three": 3}

# 딕셔너리의 키는 불변 타입이어야 합니다. 이는 키가
# 빠른 조회를 위해 상수 해시 값으로 변환될 수 있도록 하기 위함입니다.
# 불변 타입에는 int, float, string, tuple이 포함됩니다.
invalid_dict = {[1,2,3]: "123"}  # => TypeError: unhashable type: 'list' 발생
valid_dict = {(1,2,3):[1,2,3]}   # 값은 모든 타입이 될 수 있습니다.

# []로 값 조회
filled_dict["one"]  # => 1

# "keys()"로 모든 키를 반복 가능한 객체로 가져오기. list()로 호출을 래핑해야
# 목록으로 변환됩니다. 나중에 이에 대해 이야기하겠습니다. 참고 - Python
# 3.7 미만 버전의 경우 딕셔너리 키 순서가 보장되지 않습니다. 결과가
# 아래 예제와 정확히 일치하지 않을 수 있습니다. 그러나 Python 3.7부터는 딕셔너리
# 항목이 딕셔너리에 삽입된 순서를 유지합니다.
list(filled_dict.keys())  # => ["three", "two", "one"] in Python <3.7
list(filled_dict.keys())  # => ["one", "two", "three"] in Python 3.7+


# "values()"로 모든 값을 반복 가능한 객체로 가져오기. 다시 한 번 list()로 래핑해야
# 반복 가능한 객체에서 벗어날 수 있습니다. 참고 - 키 순서에 대한 위와 동일합니다.
list(filled_dict.values())  # => [3, 2, 1]  in Python <3.7
list(filled_dict.values())  # => [1, 2, 3] in Python 3.7+

# "in"으로 딕셔너리에 키 존재 여부 확인
"one" in filled_dict  # => True
1 in filled_dict      # => False

# 존재하지 않는 키를 조회하면 KeyError입니다.
filled_dict["four"]  # KeyError

# KeyError를 피하기 위해 "get()" 메서드 사용
filled_dict.get("one")      # => 1
filled_dict.get("four")     # => None
# get 메서드는 값이 없을 때 기본 인수를 지원합니다.
filled_dict.get("one", 4)   # => 1
filled_dict.get("four", 4)  # => 4

# "setdefault()"는 주어진 키가 없는 경우에만 딕셔너리에 삽입합니다.
filled_dict.setdefault("five", 5)  # filled_dict["five"]가 5로 설정됨
filled_dict.setdefault("five", 6)  # filled_dict["five"]는 여전히 5입니다.

# 딕셔너리에 추가
filled_dict.update({"four":4})  # => {"one": 1, "two": 2, "three": 3, "four": 4}
filled_dict["four"] = 4         # 딕셔너리에 추가하는 다른 방법

# del로 딕셔너리에서 키 제거
del filled_dict["one"]  # filled dict에서 "one" 키 제거

# Python 3.5부터 추가 언패킹 옵션을 사용할 수도 있습니다.
{"a": 1, **{"b": 2}}  # => {'a': 1, 'b': 2}
{"a": 1, **{"a": 2}}  # => {'a': 2}


# 세트는... 음, 세트를 저장합니다.
empty_set = set()
# 여러 값으로 세트 초기화
some_set = {1, 1, 2, 2, 3, 4}  # some_set은 이제 {1, 2, 3, 4}입니다.

# 딕셔너리의 키와 마찬가지로 세트의 요소는 불변이어야 합니다.
invalid_set = {[1], 1}  # => TypeError: unhashable type: 'list' 발생
valid_set = {(1,), 1}

# 세트에 항목 하나 더 추가
filled_set = some_set
filled_set.add(5)  # filled_set은 이제 {1, 2, 3, 4, 5}입니다.
# 세트에는 중복 요소가 없습니다.
filled_set.add(5)  # 이전과 같이 {1, 2, 3, 4, 5}로 유지됩니다.

# &로 세트 교집합
other_set = {3, 4, 5, 6}
filled_set & other_set  # => {3, 4, 5}

# |로 세트 합집합
filled_set | other_set  # => {1, 2, 3, 4, 5, 6}

# -로 세트 차집합
{1, 2, 3, 4} - {2, 3, 5}  # => {1, 4}

# ^로 세트 대칭 차집합
{1, 2, 3, 4} ^ {2, 3, 5}  # => {1, 4, 5}

# 왼쪽 세트가 오른쪽 세트의 상위 집합인지 확인
{1, 2} >= {1, 2, 3}  # => False

# 왼쪽 세트가 오른쪽 세트의 하위 집합인지 확인
{1, 2} <= {1, 2, 3}  # => True

# in으로 세트에 존재 여부 확인
2 in filled_set   # => True
10 in filled_set  # => False

# 한 단계 깊은 복사본 만들기
filled_set = some_set.copy()  # filled_set은 {1, 2, 3, 4, 5}입니다.
filled_set is some_set        # => False


####################################################
## 3. 제어 흐름 및 반복 가능 객체
####################################################

# 변수를 만들어 봅시다.
some_var = 5

# if 문입니다. Python에서는 들여쓰기가 중요합니다!
# 관례는 탭이 아닌 4개의 공백을 사용하는 것입니다.
# "some_var is smaller than 10"을 출력합니다.
if some_var > 10:
    print("some_var is totally bigger than 10.")
elif some_var < 10:    # 이 elif 절은 선택 사항입니다.
    print("some_var is smaller than 10.")
else:                  # 이것도 선택 사항입니다.
    print("some_var is indeed 10.")

# Match/Case — Python 3.10에 도입됨
# 여러 패턴에 대해 값을 비교하고 일치하는 케이스 블록을 실행합니다.

command = "run"

match command:
    case "run":
        print("The robot started to run 🏃‍♂️")
    case "speak" | "say_hi":  # 여러 옵션 (OR 패턴)
        print("The robot said hi 🗣️")
    case code if command.isdigit():  # 조건부
        print(f"The robot execute code: {code}")
    case _:  # _는 절대 실패하지 않는 와일드카드입니다 (default/else와 같음)
        print("Invalid command ❌")

# 출력: "the robot started to run 🏃‍♂️"

"""
For 루프는 리스트를 반복합니다.
출력:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # format()을 사용하여 서식화된 문자열을 보간할 수 있습니다.
    print("{} is a mammal".format(animal))

"""
"range(number)"는 0부터 주어진 숫자
(제외)까지의 숫자 반복 가능 객체를 반환합니다.
출력:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
"range(lower, upper)"는 아래 숫자에서 위 숫자까지의
숫자 반복 가능 객체를 반환합니다.
출력:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
"range(lower, upper, step)"는 아래 숫자에서 위 숫자까지
step만큼 증가하면서 숫자 반복 가능 객체를 반환합니다.
step이 표시되지 않으면 기본값은 1입니다.
출력:
    4
    6
"""
for i in range(4, 8, 2):
    print(i)

"""
리스트를 반복하여 각 리스트 항목의 인덱스와 값을 모두 검색합니다.
    0 dog
    1 cat
    2 mouse
"""
animals = ["dog", "cat", "mouse"]
for i, value in enumerate(animals):
    print(i, value)

"""
While 루프는 조건이 더 이상 충족되지 않을 때까지 계속됩니다.
출력:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # x = x + 1의 약어

# try/except 블록으로 예외 처리
try:
    # "raise"를 사용하여 오류 발생
    raise IndexError("This is an index error")
except IndexError as e:
    pass                 # 이것은 삼가십시오. 복구를 제공하십시오 (다음 예제).
except (TypeError, NameError):
    pass                 # 여러 예외를 공동으로 처리할 수 있습니다.
else:                    # try/except 블록의 선택적 절. 모든
                         # except 블록을 따라야 합니다.
    print("All good!")   # try의 코드가 예외를 발생시키지 않을 때만 실행됩니다.
finally:                 # 모든 상황에서 실행
    print("We can clean up resources here")

# 리소스 정리를 위해 try/finally 대신 with 문을 사용할 수 있습니다.
with open("myfile.txt") as f:
    for line in f:
        print(line)

# 파일에 쓰기
contents = {"aa": 12, "bb": 21}
with open("myfile1.txt", "w") as file:
    file.write(str(contents))        # 파일에 문자열 쓰기

import json
with open("myfile2.txt", "w") as file:
    file.write(json.dumps(contents))  # 파일에 객체 쓰기

# 파일에서 읽기
with open("myfile1.txt") as file:
    contents = file.read()           # 파일에서 문자열 읽기
print(contents)
# 출력: {"aa": 12, "bb": 21}

with open("myfile2.txt", "r") as file:
    contents = json.load(file)       # 파일에서 json 객체 읽기
print(contents)
# 출력: {"aa": 12, "bb": 21}


# Python은 반복 가능이라는 기본 추상화를 제공합니다.
# 반복 가능 객체는 시퀀스로 처리할 수 있는 객체입니다.
# range 함수에서 반환된 객체는 반복 가능 객체입니다.

filled_dict = {"one": 1, "two": 2, "three": 3}
our_iterable = filled_dict.keys()
print(our_iterable)  # => dict_keys(['one', 'two', 'three']). 이것은
                     # Iterable 인터페이스를 구현하는 객체입니다.

# 반복할 수 있습니다.
for i in our_iterable:
    print(i)  # one, two, three 출력

# 그러나 인덱스로 요소에 주소를 지정할 수는 없습니다.
our_iterable[1]  # TypeError 발생

# 반복 가능 객체는 반복자를 만드는 방법을 아는 객체입니다.
our_iterator = iter(our_iterable)

# 반복자는 반복하면서 상태를 기억할 수 있는 객체입니다.
# "next()"로 다음 객체를 가져옵니다.
next(our_iterator)  # => "one"

# 반복하면서 상태를 유지합니다.
next(our_iterator)  # => "two"
next(our_iterator)  # => "three"

# 반복자가 모든 데이터를 반환한 후에는
# StopIteration 예외를 발생시킵니다.
next(our_iterator)  # StopIteration 발생

# 반복할 수도 있습니다. 사실 "for"는 암시적으로 이 작업을 수행합니다!
our_iterator = iter(our_iterable)
for i in our_iterator:
    print(i)  # one, two, three 출력

# list() 호출로 반복 가능 객체 또는 반복자의 모든 요소를 가져올 수 있습니다.
list(our_iterable)  # => ["one", "two", "three"] 반환
list(our_iterator)  # => [] 반환, 상태가 저장되었기 때문


####################################################
## 4. 함수
####################################################

# "def"를 사용하여 새 함수 생성
def add(x, y):
    print("x is {} and y is {}".format(x, y))
    return x + y  # return 문으로 값 반환

# 매개변수로 함수 호출
add(5, 6)  # => "x is 5 and y is 6"을 출력하고 11을 반환

# 함수를 호출하는 다른 방법은 키워드 인수입니다.
add(y=6, x=5)  # 키워드 인수는 순서에 상관없이 도착할 수 있습니다.

# 가변 개수의 위치 인수를 받는 함수를
# 정의할 수 있습니다.
def varargs(*args):
    return args

varargs(1, 2, 3)  # => (1, 2, 3)

# 가변 개수의 키워드 인수를 받는 함수를
# 정의할 수도 있습니다.
def keyword_args(**kwargs):
    return kwargs

# 어떻게 되는지 보기 위해 호출해 봅시다.
keyword_args(big="foot", loch="ness")  # => {"big": "foot", "loch": "ness"}


# 원한다면 한 번에 둘 다 할 수 있습니다.
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) 출력:
    (1, 2)
    {"a": 3, "b": 4}
"""

# 함수를 호출할 때 args/kwargs의 반대 작업을 수행할 수 있습니다!
# *를 사용하여 args(튜플)를 확장하고 **를 사용하여 kwargs(딕셔너리)를 확장합니다.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)            # 동등: all_the_args(1, 2, 3, 4)
all_the_args(**kwargs)         # 동등: all_the_args(a=3, b=4)
all_the_args(*args, **kwargs)  # 동등: all_the_args(1, 2, 3, 4, a=3, b=4)

# 여러 값 반환 (튜플 할당 사용)
def swap(x, y):
    return y, x  # 괄호 없이 튜플로 여러 값 반환
                 # (참고: 괄호는 제외되었지만 포함될 수 있음)

x = 1
y = 2
x, y = swap(x, y)     # => x = 2, y = 1
# (x, y) = swap(x,y)  # 다시 괄호 사용은 선택 사항입니다.

# 전역 범위
x = 5

def set_x(num):
    # 지역 범위 시작
    # 지역 변수 x는 전역 변수 x와 다릅니다.
    x = num    # => 43
    print(x)   # => 43

def set_global_x(num):
    # global은 특정 변수가 전역 범위에 있음을 나타냅니다.
    global x
    print(x)   # => 5
    x = num    # 전역 변수 x가 이제 6으로 설정됨
    print(x)   # => 6

set_x(43)
set_global_x(6)
"""
출력:
    43
    5
    6
"""


# Python에는 일급 함수가 있습니다.
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# 중첩된 함수의 클로저:
# nonlocal 키워드를 사용하여 내부 함수에서 선언되어서는 안 되는 중첩된 범위의 변수로 작업할 수 있습니다.
def create_avg():
    total = 0
    count = 0
    def avg(n):
        nonlocal total, count
        total += n
        count += 1
        return total/count
    return avg
avg = create_avg()
avg(3)  # => 3.0
avg(5)  # (3+5)/2 => 4.0
avg(7)  # (8+7)/3 => 5.0

# 익명 함수도 있습니다.
(lambda x: x > 2)(3)                  # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# 내장 고차 함수가 있습니다.
list(map(add_10, [1, 2, 3]))          # => [11, 12, 13]
list(map(max, [1, 2, 3], [4, 2, 1]))  # => [4, 2, 3]

list(filter(lambda x: x > 5, [3, 4, 5, 6, 7]))  # => [6, 7]

# 멋진 맵과 필터를 위해 리스트 내포를 사용할 수 있습니다.
# 리스트 내포는 출력을 리스트로 저장합니다(자체적으로 중첩될 수 있음).
[add_10(i) for i in [1, 2, 3]]         # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]  # => [6, 7]

# 세트 및 딕셔너리 내포도 구성할 수 있습니다.
{x for x in "abcddeef" if x not in "abc"}  # => {'d', 'e', 'f'}
{x: x**2 for x in range(5)}  # => {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}


####################################################
## 5. 모듈
####################################################

# 모듈을 가져올 수 있습니다.
import math
print(math.sqrt(16))  # => 4.0

# 모듈에서 특정 함수를 가져올 수 있습니다.
from math import ceil, floor
print(ceil(3.7))   # => 4
print(floor(3.7))  # => 3

# 모듈에서 모든 함수를 가져올 수 있습니다.
# 경고: 권장하지 않습니다.
from math import *

# 모듈 이름을 단축할 수 있습니다.
import math as m
math.sqrt(16) == m.sqrt(16)  # => True

# Python 모듈은 일반 Python 파일일 뿐입니다.
# 직접 작성하고 가져올 수 있습니다. 모듈의 이름은
# 파일 이름과 동일합니다.

# 모듈에 정의된 함수와 속성을
# 찾을 수 있습니다.
import math
dir(math)

# 현재 스크립트와 동일한 폴더에 math.py라는
# Python 스크립트가 있는 경우, 내장 Python 모듈 대신
# math.py 파일이 로드됩니다.
# 이것은 로컬 폴더가 Python의 내장 라이브러리보다
# 우선순위가 높기 때문에 발생합니다.


####################################################
## 6. 클래스
####################################################

# "class" 문을 사용하여 클래스를 만듭니다.
class Human:

    # 클래스 속성입니다. 이 클래스의 모든 인스턴스에서 공유됩니다.
    species = "H. sapiens"

    # 기본 초기화자, 이 클래스가 인스턴스화될 때 호출됩니다.
    # 이중 선행 및 후행 밑줄은 Python에서 사용되지만
    # 사용자 제어 네임스페이스에 있는 객체 또는 속성을 나타냅니다.
    # __init__, __str__, __repr__ 등과 같은 메서드(또는 객체 또는 속성)는
    # 특수 메서드(때로는 dunder 메서드라고도 함)라고 합니다.
    # 이러한 이름을 직접 만들어서는 안 됩니다.
    def __init__(self, name):
        # 인수를 인스턴스의 이름 속성에 할당
        self.name = name

        # 속성 초기화
        self._age = 0   # 선행 밑줄은 "age" 속성이
                        # 내부적으로 사용되도록 의도되었음을 나타냅니다.
                        # 이것이 강제될 것이라고 의존하지 마십시오. 다른 개발자에게 힌트입니다.

    # 인스턴스 메서드입니다. 모든 메서드는 "self"를 첫 번째 인수로 받습니다.
    def say(self, msg):
        print("{name}: {message}".format(name=self.name, message=msg))

    # 다른 인스턴스 메서드
    def sing(self):
        return "yo... yo... microphone check... one two... one two..."

    # 클래스 메서드는 모든 인스턴스에서 공유됩니다.
    # 호출하는 클래스를 첫 번째 인수로 사용하여 호출됩니다.
    @classmethod
    def get_species(cls):
        return cls.species

    # 정적 메서드는 클래스 또는 인스턴스 참조 없이 호출됩니다.
    @staticmethod
    def grunt():
        return "*grunt*"

    # 속성은 getter와 같습니다.
    # age() 메서드를 동일한 이름의 읽기 전용 속성으로 바꿉니다.
    # 하지만 Python에서는 사소한 getter와 setter를 작성할 필요가 없습니다.
    @property
    def age(self):
        return self._age

    # 이렇게 하면 속성을 설정할 수 있습니다.
    @age.setter
    def age(self, age):
        self._age = age

    # 이렇게 하면 속성을 삭제할 수 있습니다.
    @age.deleter
    def age(self):
        del self._age


# Python 인터프리터가 소스 파일을 읽을 때 모든 코드를 실행합니다.
# 이 __name__ 검사는 이 코드 블록이 이 모듈이
# 주 프로그램일 때만 실행되도록 합니다.
if __name__ == "__main__":
    # 클래스 인스턴스화
    i = Human(name="Ian")
    i.say("hi")                     # "Ian: hi"
    j = Human("Joel")
    j.say("hello")                  # "Joel: hello"
    # i와 j는 Human 타입의 인스턴스입니다. 즉, Human 객체입니다.

    # 클래스 메서드 호출
    i.say(i.get_species())          # "Ian: H. sapiens"
    # 공유 속성 변경
    Human.species = "H. neanderthalensis"
    i.say(i.get_species())          # => "Ian: H. neanderthalensis"
    j.say(j.get_species())          # => "Joel: H. neanderthalensis"

    # 정적 메서드 호출
    print(Human.grunt())            # => "*grunt*"

    # 정적 메서드는 인스턴스에서도 호출할 수 있습니다.
    print(i.grunt())                # => "*grunt*"

    # 이 인스턴스의 속성 업데이트
    i.age = 42
    # 속성 가져오기
    i.say(i.age)                    # => "Ian: 42"
    j.say(j.age)                    # => "Joel: 0"
    # 속성 삭제
    del i.age
    # i.age                         # => AttributeError 발생


####################################################
## 6.1 상속
####################################################

# 상속을 통해 부모 클래스에서 메서드와
# 변수를 상속하는 새로운 자식 클래스를 정의할 수 있습니다.

# 위에서 정의한 Human 클래스를 기본 또는 부모 클래스로 사용하여
# 자식 클래스인 Superhero를 정의할 수 있습니다. Superhero는 "species",
# "name", "age"와 같은 변수와 "sing", "grunt"와 같은 메서드를
# Human 클래스에서 상속하지만, 자신만의 고유한 속성을 가질 수도 있습니다.

# 파일별 모듈화를 활용하려면 위 클래스를
# human.py와 같은 자체 파일에 배치할 수 있습니다.

# 다른 파일에서 함수를 가져오려면 다음 형식을 사용하십시오.
# from "파일이름-확장자없음" import "함수-또는-클래스"

from human import Human


# 부모 클래스를 클래스 정의의 매개변수로 지정
class Superhero(Human):

    # 자식 클래스가 부모의 모든 정의를 수정 없이
    # 상속해야 하는 경우 "pass" 키워드를 사용할 수 있습니다(다른 것은 없음).
    # 하지만 이 경우 고유한 자식 클래스를 허용하기 위해 주석 처리되었습니다.
    # pass

    # 자식 클래스는 부모의 속성을 재정의할 수 있습니다.
    species = "Superhuman"

    # 자식은 부모 클래스의 생성자를 자동으로 상속하며
    # 인수도 포함하지만, 추가 인수를 정의하거나 정의하고
    # 클래스 생성자와 같은 메서드를 재정의할 수도 있습니다.
    # 이 생성자는 "Human" 클래스에서 "name" 인수를 상속하고
    # "superpower" 및 "movie" 인수를 추가합니다.
    def __init__(self, name, movie=False,
                 superpowers=["super strength", "bulletproofing"]):

        # 추가 클래스 속성 추가:
        self.fictional = True
        self.movie = movie
        # 기본값이 공유되므로 가변 기본값에 유의하십시오.
        self.superpowers = superpowers

        # "super" 함수를 사용하면 자식에 의해 재정의된
        # 부모 클래스의 메서드에 액세스할 수 있습니다. 이 경우 __init__ 메서드입니다.
        # 이것은 부모 클래스 생성자를 호출합니다.
        super().__init__(name)

    # sing 메서드 재정의
    def sing(self):
        return "Dun, dun, DUN!"

    # 추가 인스턴스 메서드 추가
    def boast(self):
        for power in self.superpowers:
            print("I wield the power of {pow}!".format(pow=power))


if __name__ == "__main__":
    sup = Superhero(name="Tick")

    # 인스턴스 타입 확인
    if isinstance(sup, Human):
        print("I am human")
    if type(sup) is Superhero:
        print("I am a superhero")

    # getattr() 및 super()에서 사용하는 "메서드 확인 순서" 가져오기
    # (속성 또는 메서드를 검색하는 클래스 순서)
    # 이 속성은 동적이며 업데이트할 수 있습니다.
    print(Superhero.__mro__)    # => (<class '__main__.Superhero'>,
                                # => <class 'human.Human'>, <class 'object'>)

    # 부모 메서드를 호출하지만 자체 클래스 속성을 사용
    print(sup.get_species())    # => Superhuman

    # 재정의된 메서드 호출
    print(sup.sing())           # => Dun, dun, DUN!

    # Human에서 메서드 호출
    sup.say("Spoon")            # => Tick: Spoon

    # Superhero에만 존재하는 메서드 호출
    sup.boast()                 # => I wield the power of super strength!
                                # => I wield the power of bulletproofing!

    # 상속된 클래스 속성
    sup.age = 31
    print(sup.age)              # => 31

    # Superhero 내에만 존재하는 속성
    print("Am I Oscar eligible? " + str(sup.movie))

####################################################
## 6.2 다중 상속
####################################################


# 다른 클래스 정의
# bat.py
class Bat:

    species = "Baty"

    def __init__(self, can_fly=True):
        self.fly = can_fly

    # 이 클래스에도 say 메서드가 있습니다.
    def say(self, msg):
        msg = "... ... ..."
        return msg

    # 그리고 자신만의 메서드도 있습니다.
    def sonar(self):
        return "))) ... ((("


if __name__ == "__main__":
    b = Bat()
    print(b.say("hello"))
    print(b.fly)


# 그리고 Superhero와 Bat에서 상속하는 또 다른 클래스 정의
# superhero.py
from superhero import Superhero
from bat import Bat

# Batman을 Superhero와 Bat 모두에서 상속하는 자식으로 정의
class Batman(Superhero, Bat):

    def __init__(self, *args, **kwargs):
        # 일반적으로 속성을 상속하려면 super를 호출해야 합니다.
        # super(Batman, self).__init__(*args, **kwargs)
        # 하지만 여기서는 다중 상속을 다루고 있으며, super()는
        # MRO 목록의 다음 기본 클래스에서만 작동합니다.
        # 따라서 대신 모든 조상에 대해 명시적으로 __init__을 호출합니다.
        # *args와 **kwargs를 사용하면 각 부모가 "양파 껍질을 벗기는"
        # 방식으로 인수를 깔끔하게 전달할 수 있습니다.
        Superhero.__init__(self, "anonymous", movie=True,
                           superpowers=["Wealthy"], *args, **kwargs)
        Bat.__init__(self, *args, can_fly=False, **kwargs)
        # name 속성 값 재정의
        self.name = "Sad Affleck"

    def sing(self):
        return "nan nan nan nan nan batman!"


if __name__ == "__main__":
    sup = Batman()

    # 메서드 확인 순서
    print(Batman.__mro__)     # => (<class '__main__.Batman'>,
                              # => <class 'superhero.Superhero'>,
                              # => <class 'human.Human'>,
                              # => <class 'bat.Bat'>, <class 'object'>)

    # 부모 메서드를 호출하지만 자체 클래스 속성을 사용
    print(sup.get_species())  # => Superhuman

    # 재정의된 메서드 호출
    print(sup.sing())         # => nan nan nan nan nan batman!

    # 상속 순서가 중요하므로 Human에서 메서드 호출
    sup.say("I agree")        # => Sad Affleck: I agree

    # 두 번째 조상에만 존재하는 메서드 호출
    print(sup.sonar())        # => ))) ... (((

    # 상속된 클래스 속성
    sup.age = 100
    print(sup.age)            # => 100

    # 기본값이 재정의된 두 번째 조상에서 상속된 속성
    print("Can I fly? " + str(sup.fly))  # => Can I fly? False


####################################################
## 7. 고급
####################################################

# 제너레이터는 게으른 코드를 만드는 데 도움이 됩니다.
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# 제너레이터는 반복 가능한 객체에서 다음 값을 처리하는 데 필요한
# 데이터만 로드하기 때문에 메모리 효율적입니다.
# 이를 통해 금지적으로 큰 값 범위에 대한 작업을 수행할 수 있습니다.
# 참고: `range`는 Python 3에서 `xrange`를 대체합니다.
for i in double_numbers(range(1, 900000000)):  # `range`는 제너레이터입니다.
    print(i)
    if i >= 30:
        break

# 리스트 내포를 만들 수 있는 것처럼 제너레이터
# 내포도 만들 수 있습니다.
values = (-x for x in [1,2,3,4,5])
for x in values:
    print(x)  # 콘솔/터미널에 -1 -2 -3 -4 -5 출력

# 제너레이터 내포를 리스트로 직접 캐스팅할 수도 있습니다.
values = (-x for x in [1,2,3,4,5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]


# 데코레이터는 구문 설탕의 한 형태입니다.
# 투박한 구문을 달성하면서 코드를 더 쉽게 읽을 수 있도록 합니다.

# 래퍼는 데코레이터의 한 유형입니다.
# 기존 함수를 수정할 필요 없이 로깅을 추가하는 데 정말 유용합니다.

def log_function(func):
    def wrapper(*args, **kwargs):
        print("Entering function", func.__name__)
        result = func(*args, **kwargs)
        print("Exiting function", func.__name__)
        return result
    return wrapper

@log_function               # 동등:
def my_function(x,y):       # def my_function(x,y):
    """Adds two numbers together."""
    return x+y              #   return x+y
                            # my_function = log_function(my_function)
# 데코레이터 @log_function은 my_function에 대한 함수 정의를 읽기 시작할 때
# 이 함수가 log_function으로 래핑될 것임을 알려줍니다.
# 함수 정의가 길면 정의 끝에 있는 데코레이션되지 않은
# 할당을 구문 분석하기 어려울 수 있습니다.

my_function(1,2)  # => "Entering function my_function"
                  # => "3"
                  # => "Exiting function my_function"

# 하지만 문제가 있습니다.
# my_function에 대한 정보를 얻으려고 하면 어떻게 될까요?

print(my_function.__name__)  # => 'wrapper'
print(my_function.__doc__)  # => None (래퍼 함수에는 docstring이 없음)

# 데코레이터가 my_function = log_function(my_function)과 동일하기 때문에
# my_function에 대한 정보를 래퍼 정보로 대체했습니다.

# functools를 사용하여 이 문제를 해결하십시오.

from functools import wraps

def log_function(func):
    @wraps(func)  # 이것은 docstring, 함수 이름, 인수 목록 등이 모두
                  # 래핑된 함수에 복사되도록 보장합니다 - 래퍼 정보로 대체되는 대신
    def wrapper(*args, **kwargs):
        print("Entering function", func.__name__)
        result = func(*args, **kwargs)
        print("Exiting function", func.__name__)
        return result
    return wrapper

@log_function
def my_function(x,y):
    """Adds two numbers together."""
    return x+y

my_function(1,2)  # => "Entering function my_function"
                  # => "3"
                  # => "Exiting function my_function"

print(my_function.__name__)  # => 'my_function'
print(my_function.__doc__)  # => 'Adds two numbers together.'
```

### 무료 온라인

* [파이썬으로 지루한 작업 자동화하기](https://automatetheboringstuff.com)
* [공식 문서](https://docs.python.org/3/)
* [파이썬을 위한 히치하이커 안내서](https://docs.python-guide.org/)
* [파이썬 코스](https://www.python-course.eu)
* [파이썬 첫걸음](https://realpython.com/learn/python-first-steps/)
* [엄선된 멋진 파이썬 프레임워크, 라이브러리 및 소프트웨어 목록](https://github.com/vinta/awesome-python)
* [파이썬 공식 스타일 가이드](https://peps.python.org/pep-0008/)
* [파이썬 3 컴퓨터 과학 서클](https://cscircles.cemc.uwaterloo.ca/)
* [파이썬 3으로 뛰어들기](https://www.diveintopython3.net/)
* [중급자를 위한 파이썬 튜토리얼](https://pythonbasics.org/)
* [파이썬으로 데스크톱 앱 만들기](https://pythonpyqt.com/)
