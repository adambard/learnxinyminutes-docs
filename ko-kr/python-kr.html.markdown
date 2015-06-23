---
language: python
category: language
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
filename: learnpython-ko.py
translators:
    - ["wikibook", "http://wikibook.co.kr"]
lang: ko-kr
---

파이썬은 귀도 반 로섬이 90년대에 만들었습니다. 파이썬은 현존하는 널리 사용되는 언어 중 하나입니다.
저는 문법적 명료함에 반해 파이썬을 사랑하게 됐습니다. 파이썬은 기본적으로 실행 가능한 의사코드입니다.

피드백 주시면 정말 감사하겠습니다! [@louiedinh](http://twitter.com/louiedinh)나
louiedinh [at] [구글의 이메일 서비스]를 통해 저에게 연락하시면 됩니다.

참고: 이 글은 구체적으로 파이썬 2.7에 해당하는 내용을 담고 있습니다만
파이썬 2.x에도 적용할 수 있을 것입니다. 파이썬 3을 다룬 튜토리얼도 곧 나올 테니 기대하세요!

```python
# 한 줄짜리 주석은 해시로 시작합니다.
""" 여러 줄 문자열은 "를 세 개 써서 시작할 수 있고,
    주석으로 자주 사용됩니다.
"""

####################################################
## 1. 기본 자료형과 연산자
####################################################

# 숫자
3 #=> 3

# 수학 연산은 예상하신 대로입니다.
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# 나눗셈은 약간 까다롭습니다. 정수로 나눈 다음 결과값을 자동으로 내림합니다.
5 / 2 #=> 2

# 나눗셈 문제를 해결하려면 float에 대해 알아야 합니다.
2.0     # 이것이 float입니다.
11.0 / 4.0 #=> 2.75 훨씬 낫네요

# 괄호를 이용해 연산자 우선순위를 지정합니다.
(1 + 3) * 2 #=> 8

# 불린(Boolean) 값은 기본형입니다.
True
False

# not을 이용해 부정합니다.
not True #=> False
not False #=> True

# 동일성 연산자는 ==입니다.
1 == 1 #=> True
2 == 1 #=> False

# 불일치 연산자는 !=입니다.
1 != 1 #=> False
2 != 1 #=> True

# 그밖의 비교 연산자는 다음과 같습니다.
1 < 10 #=> True
1 > 10 #=> False
2 <= 2 #=> True
2 >= 2 #=> True

# 비교 연산을 연결할 수도 있습니다!
1 < 2 < 3 #=> True
2 < 3 < 2 #=> False

# 문자열은 "나 '로 생성합니다.
"This is a string."
'This is also a string.'

# 문자열도 연결할 수 있습니다!
"Hello " + "world!" #=> "Hello world!"

# 문자열은 문자로 구성된 리스트로 간주할 수 있습니다.
"This is a string"[0] #=> 'T'

# %는 다음과 같이 문자열을 형식화하는 데 사용할 수 있습니다:
"%s can be %s" % ("strings", "interpolated")

# 문자열을 형식화하는 새로운 방법은 format 메서드를 이용하는 것입니다.
# 이 메서드를 이용하는 방법이 더 선호됩니다.
"{0} can be {1}".format("strings", "formatted")
# 자릿수를 세기 싫다면 키워드를 이용해도 됩니다.
"{name} wants to eat {food}".format(name="Bob", food="lasagna")

# None은 객체입니다.
None #=> None

# 객체와 None을 비교할 때는 동일성 연산자인 `==`를 사용해서는 안 됩니다.
# 대신 `is`를 사용하세요.
"etc" is None #=> False
None is None  #=> True

# 'is' 연산자는 객체의 식별자를 검사합니다.
# 기본형 값을 다룰 때는 이 연산자가 그다지 유용하지 않지만
# 객체를 다룰 때는 매우 유용합니다.

# None, 0, 빈 문자열/리스트는 모두 False로 평가됩니다.
# 그밖의 다른 값은 모두 True입니다
0 == False  #=> True
"" == False #=> True


####################################################
## 2. 변수와 컬렉션
####################################################

# 뭔가를 출력하는 것은 상당히 쉽습니다.
print "I'm Python. Nice to meet you!"


# 변수에 값을 할당하기 전에 변수를 반드시 선언하지 않아도 됩니다.
some_var = 5    # 명명관례는 '밑줄이_포함된_소문자'입니다.
some_var #=> 5

# 미할당된 변수에 접근하면 예외가 발생합니다.
# 예외 처리에 관해서는 '제어 흐름'을 참고하세요.
some_other_var  # 이름 오류가 발생

# 표현식으로도 사용할 수 있습니다.
"yahoo!" if 3 > 2 else 2 #=> "yahoo!"

# 리스트는 순차 항목을 저장합니다.
li = []
# 미리 채워진 리스트로 시작할 수도 있습니다.
other_li = [4, 5, 6]

# append를 이용해 리스트 끝에 항목을 추가합니다.
li.append(1)    #li는 이제 [1]입니다.
li.append(2)    #li는 이제 [1, 2]입니다.
li.append(4)    #li는 이제 [1, 2, 4]입니다.
li.append(3)    #li는 이제 [1, 2, 4, 3]입니다.
# pop을 이용해 끝에서부터 항목을 제거합니다.
li.pop()        #=> 3이 반환되고 li는 이제 [1, 2, 4]입니다.
# 다시 넣어봅시다
li.append(3)    # li는 이제 다시 [1, 2, 4, 3]가 됩니다.

# 배열에서 했던 것처럼 리스트에도 접근할 수 있습니다.
li[0] #=> 1
# 마지막 요소를 봅시다.
li[-1] #=> 3

# 범위를 벗어나서 접근하면 IndexError가 발생합니다.
li[4] # IndexError가 발생

# 슬라이스 문법을 통해 범위를 지정해서 값을 조회할 수 있습니다.
# (이 문법을 통해 간편하게 범위를 지정할 수 있습니다.)
li[1:3] #=> [2, 4]
# 앞부분을 생략합니다.
li[2:] #=> [4, 3]
# 끝부분을 생략합니다.
li[:3] #=> [1, 2, 4]

# del로 임의의 요소를 제거할 수 있습니다.
del li[2] # li is now [1, 2, 3]

# 리스트를 추가할 수도 있습니다.
li + other_li #=> [1, 2, 3, 4, 5, 6] - 참고: li와 other_li는 그대로 유지됩니다.

# extend로 리스트를 연결합니다.
li.extend(other_li) # 이제 li는 [1, 2, 3, 4, 5, 6]입니다.

# in으로 리스트 안에서 특정 요소가 존재하는지 확인합니다.
1 in li #=> True

# len으로 길이를 검사합니다.
len(li) #=> 6

# 튜플은 리스트와 비슷하지만 불변성을 띱니다.
tup = (1, 2, 3)
tup[0] #=> 1
tup[0] = 3  # TypeError가 발생

# 튜플에 대해서도 리스트에서 할 수 있는 일들을 모두 할 수 있습니다.
len(tup) #=> 3
tup + (4, 5, 6) #=> (1, 2, 3, 4, 5, 6)
tup[:2] #=> (1, 2)
2 in tup #=> True

# 튜플(또는 리스트)을 변수로 풀 수 있습니다.
a, b, c = (1, 2, 3)     # 이제 a는 1, b는 2, c는 3입니다
# 괄호를 빼면 기본적으로 튜플이 만들어집니다.
d, e, f = 4, 5, 6
# 이제 두 값을 바꾸는 게 얼마나 쉬운지 확인해 보세요.
e, d = d, e     # 이제 d는 5이고 e는 4입니다.

# 딕셔너리는 매핑을 저장합니다.
empty_dict = {}
# 다음은 값을 미리 채운 딕셔너리입니다.
filled_dict = {"one": 1, "two": 2, "three": 3}

# []를 이용해 값을 조회합니다.
filled_dict["one"] #=> 1

# 모든 키를 리스트로 구합니다.
filled_dict.keys() #=> ["three", "two", "one"]
# 참고 - 딕셔너리 키의 순서는 보장되지 않습니다.
# 따라서 결과가 이와 정확히 일치하지 않을 수도 있습니다.

# 모든 값을 리스트로 구합니다.
filled_dict.values() #=> [3, 2, 1]
# 참고 - 키 순서와 관련해서 위에서 설명한 내용과 같습니다.

# in으로 딕셔너리 안에 특정 키가 존재하는지 확인합니다.
"one" in filled_dict #=> True
1 in filled_dict #=> False

# 존재하지 않는 키를 조회하면 KeyError가 발생합니다.
filled_dict["four"] # KeyError

# get 메서드를 이용하면 KeyError가 발생하지 않습니다.
filled_dict.get("one") #=> 1
filled_dict.get("four") #=> None
# get 메서드는 값이 누락된 경우 기본 인자를 지원합니다.
filled_dict.get("one", 4) #=> 1
filled_dict.get("four", 4) #=> 4

# setdefault 메서드는 딕셔너리에 새 키-값 쌍을 추가하는 안전한 방법입니다.
filled_dict.setdefault("five", 5) #filled_dict["five"]는 5로 설정됩니다.
filled_dict.setdefault("five", 6) #filled_dict["five"]는 여전히 5입니다.


# 세트는 집합을 저장합니다.
empty_set = set()
# 다수의 값으로 세트를 초기화합니다.
some_set = set([1,2,2,3,4]) # 이제 some_set는 set([1, 2, 3, 4])입니다.

# 파이썬 2.7부터는 {}를 세트를 선언하는 데 사용할 수 있습니다.
filled_set = {1, 2, 2, 3, 4} # => {1 2 3 4}

# 세트에 항목을 추가합니다.
filled_set.add(5) # 이제 filled_set는 {1, 2, 3, 4, 5}입니다.

# &을 이용해 교집합을 만듭니다.
other_set = {3, 4, 5, 6}
filled_set & other_set #=> {3, 4, 5}

# |를 이용해 합집합을 만듭니다.
filled_set | other_set #=> {1, 2, 3, 4, 5, 6}

# -를 이용해 차집합을 만듭니다.
{1,2,3,4} - {2,3,5} #=> {1, 4}

# in으로 세트 안에 특정 요소가 존재하는지 검사합니다.
2 in filled_set #=> True
10 in filled_set #=> False


####################################################
## 3. 제어 흐름
####################################################

# 변수를 만들어 봅시다.
some_var = 5

# 다음은 if 문입니다. 파이썬에서는 들여쓰기가 대단히 중요합니다!
# 다음 코드를 실행하면 "some_var is smaller than 10"가 출력됩니다.
if some_var > 10:
    print "some_var is totally bigger than 10."
elif some_var < 10:    # elif 절은 선택사항입니다.
    print "some_var is smaller than 10."
else:           # 이 부분 역시 선택사항입니다.
    print "some_var is indeed 10."


"""
for 루프는 리스트를 순회합니다.
아래 코드는 다음과 같은 내용을 출력합니다:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # %로 형식화된 문자열에 값을 채워넣을 수 있습니다.
    print "%s is a mammal" % animal
    
"""
`range(number)`는 숫자 리스트를 반환합니다.
이때 숫자 리스트의 범위는 0에서 지정한 숫자까지입니다.
아래 코드는 다음과 같은 내용을 출력합니다:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
while 루프는 조건이 더는 충족되지 않을 때까지 진행됩니다.
prints:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # x = x + 1의 축약형

# try/except 블록을 이용한 예외 처리

# 파이썬 2.6 및 상위 버전에서 동작하는 코드
try:
    # raise를 이용해 오류를 발생시킵니다
    raise IndexError("This is an index error")
except IndexError as e:
    pass    # pass는 단순 no-op 연산입니다. 보통 이곳에 복구 코드를 작성합니다.


####################################################
## 4. 함수
####################################################

# 새 함수를 만들 때 def를 사용합니다.
def add(x, y):
    print "x is %s and y is %s" % (x, y)
    return x + y    # return 문을 이용해 값을 반환합니다.

# 매개변수를 전달하면서 함수를 호출
add(5, 6) #=> "x is 5 and y is 6"가 출력되고 11이 반환됨

# 함수를 호출하는 또 다른 방법은 키워드 인자를 지정하는 방법입니다.
add(y=6, x=5)   # 키워드 인자는 순서에 구애받지 않습니다.

# 위치 기반 인자를 임의 개수만큼 받는 함수를 정의할 수 있습니다.
def varargs(*args):
    return args

varargs(1, 2, 3) #=> (1,2,3)


# 키워드 인자를 임의 개수만큼 받는 함수 또한 정의할 수 있습니다.
def keyword_args(**kwargs):
    return kwargs

# 이 함수를 호출해서 어떤 일이 일어나는지 확인해 봅시다.
keyword_args(big="foot", loch="ness") #=> {"big": "foot", "loch": "ness"}

# 원한다면 한 번에 두 가지 종류의 인자를 모두 받는 함수를 정의할 수도 있습니다.
def all_the_args(*args, **kwargs):
    print args
    print kwargs
"""
all_the_args(1, 2, a=3, b=4)를 실행하면 다음과 같은 내용이 출력됩니다:
    (1, 2)
    {"a": 3, "b": 4}
"""

# 함수를 호출할 때 varargs/kwargs와 반대되는 일을 할 수 있습니다!
# *를 이용해 튜플을 확장하고 **를 이용해 kwargs를 확장합니다.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args) # foo(1, 2, 3, 4)와 같음
all_the_args(**kwargs) # foo(a=3, b=4)와 같음
all_the_args(*args, **kwargs) # foo(1, 2, 3, 4, a=3, b=4)와 같음

# 파이썬에는 일급 함수가 있습니다
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3) #=> 13

# 게다가 익명 함수도 있습니다.
(lambda x: x > 2)(3) #=> True

# 내장된 고차 함수(high order function)도 있습니다.
map(add_10, [1,2,3]) #=> [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7]) #=> [6, 7]

# 맵과 필터에 리스트 조건 제시법(list comprehensions)을 사용할 수 있습니다.
[add_10(i) for i in [1, 2, 3]]  #=> [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5] #=> [6, 7]

####################################################
## 5. 클래스
####################################################

# 클래스를 하나 만들기 위해 특정 객체의 하위 클래스를 만들 수 있습니다.
class Human(object):

    # 클래스 속성은 이 클래스의 모든 인스턴스에서 공유합니다.
    species = "H. sapiens"

    # 기본 초기화자
    def __init__(self, name):
        # 인자를 인스턴스의 name 속성에 할당합니다.
        self.name = name

    # 모든 인스턴스 메서드에서는 self를 첫 번째 인자로 받습니다.
    def say(self, msg):
       return "%s: %s" % (self.name, msg)

    # 클래스 메서드는 모든 인스턴스에서 공유합니다.
    # 클래스 메서드는 호출하는 클래스를 첫 번째 인자로 호출됩니다.
    @classmethod
    def get_species(cls):
        return cls.species

    # 정적 메서드는 클래스나 인스턴스 참조 없이도 호출할 수 있습니다.
    @staticmethod
    def grunt():
        return "*grunt*"


# 클래스 인스턴스화
i = Human(name="Ian")
print i.say("hi")     # "Ian: hi"가 출력됨

j = Human("Joel")
print j.say("hello")  # "Joel: hello"가 출력됨

# 클래스 메서드를 호출
i.get_species() #=> "H. sapiens"

# 공유 속성을 변경
Human.species = "H. neanderthalensis"
i.get_species() #=> "H. neanderthalensis"
j.get_species() #=> "H. neanderthalensis"

# 정적 메서드를 호출
Human.grunt() #=> "*grunt*"


####################################################
## 6. 모듈
####################################################

# 다음과 같이 모듈을 임포트할 수 있습니다.
import math
print math.sqrt(16) #=> 4

# 모듈의 특정 함수를 호출할 수 있습니다.
from math import ceil, floor
print ceil(3.7)  #=> 4.0
print floor(3.7) #=> 3.0

# 모듈의 모든 함수를 임포트할 수 있습니다.
# Warning: this is not recommended
from math import *

# 모듈 이름을 축약해서 쓸 수 있습니다.
import math as m
math.sqrt(16) == m.sqrt(16) #=> True

# 파이썬 모듈은 평범한 파이썬 파일에 불과합니다.
# 직접 모듈을 작성해서 그것들을 임포트할 수 있습니다. 
# 모듈의 이름은 파일의 이름과 같습니다.

# 다음과 같은 코드로 모듈을 구성하는 함수와 속성을 확인할 수 있습니다.
import math
dir(math)


```

## 더 배울 준비가 되셨습니까?

### 무료 온라인 참고자료

* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [The Official Docs](http://docs.python.org/2.6/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Module of the Week](http://pymotw.com/2/)

### 파이썬 관련 도서

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)
