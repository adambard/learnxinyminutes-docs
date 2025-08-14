# ldpl.md (번역)

---
name: LDPL
filename: learnLDPL.ldpl
contributors:
    - ["Martín del Río", "https://github.com/lartu"]
    - ["John Paul Wohlscheid", "https://github.com/JohnBlood"]
---

**LDPL**은 C++로 트랜스파일되는 강력한 오픈 소스 프로그래밍 언어로,
처음부터 과도하게 표현력이 풍부하고, 읽기 쉽고, 빠르고, 배우기 쉽도록 설계되었습니다.
COBOL과 같은 오래된 프로그래밍 언어와 유사하게 평이한 영어를 모방하여
누구나 이해할 수 있기를 바라는 마음으로 만들어졌습니다. 매우 이식성이 뛰어나고
다양한 아키텍처와 운영 체제에서 실행되며, UTF-8을
즉시 지원합니다.

[여기에서 더 읽어보세요.](https://github.com/lartu/ldpl)

```coffeescript
# 이것은 LDPL의 한 줄 주석입니다.
# LDPL에는 여러 줄 주석이 없습니다.

# LDPL은 대소문자를 구분하지 않는 언어입니다: dIsPlaY와 DISPLAY는 동일한
# 문장이며, foo와 FOO는 동일한 변수를 명명합니다.

# LDPL 소스 파일은 DATA 섹션과 PROCEDURE 섹션의 두 섹션으로 나뉩니다.

DATA:
# DATA 섹션 내에서 변수가 선언됩니다.

myNumber is number          # 실수를 정의합니다.
myString is text            # 문자열을 정의합니다.
myList is number list       # 숫자 목록을 정의합니다.
myMap  is number map        # 숫자 맵을 정의합니다.

# LDPL은 네 가지 데이터 타입을 이해합니다: 두 가지 스칼라 타입 (NUMBER, TEXT)
# 및 두 가지 컨테이너 타입 (LISTs 및 MAPs).
# LISTs는 TEXT LISTs 또는 NUMBER LISTs가 될 수 있으며, MAPs는
# TEXT MAPs 및 NUMBER MAPs가 될 수 있습니다. 더 큰 데이터 타입을 만들기 위해
# 많은 컨테이너를 연결할 수도 있습니다.
textListList is text list list
myMulticontainer is number list list map
# 숫자 목록의 목록의 목록의 맵을 정의합니다.

PROCEDURE:
# PROCEDURE 섹션 내에서 코드가 작성됩니다.

store -19.2 in myNumber         # 값을 할당하려면 STORE 문을 사용합니다.
store "Hi there" in myString    # 변수에.
push 890 to myList # 목록에 값을 추가하려면 PUSH - TO를 사용합니다.
push 100 to myList
push 500 to myList
store 45 in myMap:"someIndex" # 컨테이너를 인덱싱하려면 : 연산자를 사용합니다.

push list to textListList # 목록 목록에 빈 목록을 푸시합니다.
push "LDPL is nice!" to textListList:0 #푸시된 목록에 텍스트를 푸시합니다.

display "Hello World!" # 값을 출력하려면 DISPLAY 문을 사용합니다.
# display 문은 공백으로 구분된 여러 값을 받을 수 있습니다.
display crlf "How are you today?" myNumber myString crlf
# CRLF는 LDPL의 표준 줄 바꿈 값입니다.
display textListList:0:0 " Isn't it?" crlf

# LDPL의 IF 문은 매우 장황합니다:
if myNumber is equal to -19.2 and myList:0 is less than 900 then
    display "Yes!" crlf
else if myMap:"someIndex" is not equal to 45 then
    display "This is an else if!" crlf
else
    display "Else!" crlf
end if
# 유효한 LDPL 비교 연산자는 다음과 같습니다.
# - IS EQUAL TO
# - IS NOT EQUAL TO
# - IS LESS THAN
# - IS GREATER THAN
# - IS LESS THAN OR EQUAL TO
# - IS GREATER THAN OR EQUAL TO
if "Hi there!" is not equal to "Bye bye!" then
    display "Yep, those weren't equal." crlf
end if
# LDPL은 일반적으로 인라인 표현식을 이해하지 못하므로
# 다음과 같은 작업을 수행할 수 없습니다.
# if myNumber - 9 * 2 is equal to 10 then
# 그렇게 하면 LDPL이 컴퓨터에 불을 지르고 화면을 터뜨릴 것입니다.

# WHILE 루프는 동일한 규칙을 따릅니다.
store 0 in myNumber
while myNumber is less than 10 do
    display "Loop number " myNumber "..." crlf
    in myNumber solve myNumber + 1 # 이렇게 수학을 할 수 있습니다.
repeat
# 다른 언어와 마찬가지로 루프 내에서 'break' 및 'continue'를 사용할 수 있습니다.

# LDPL에는 FOR 루프와 FOR EACH 루프도 있습니다.
for myNumber from 0 to 100 step 2 do
    display myNumber crlf
repeat

for each myNumber in myList do
    display myNumber
repeat

display "Enter your name: "
accept myString # 사용자가 값을 입력하도록 하려면 ACCEPT를 사용합니다.
display "Hi there, " myString crlf
display "How old are you?: "
accept myNumber
if myNumber is greater than 200 then
    display "Woah, you are so old!" crlf
end if

wait 1000 milliseconds # 프로그램을 1초 동안 일시 중지합니다.

# 수학을 좀 해보자
store 1.2 in myNumber
in myNumber solve myNumber * (10 / 7.2) # 연산자는 공백으로 구분됩니다.
floor myNumber
display myNumber crlf
get random in myNumber # 0과 1 사이의 난수를 얻습니다.
                       # 그리고 myNumber에 저장합니다.

# LDPL의 함수는 서브 프로시저라고 합니다. 서브 프로시저는 소스
# 파일과 마찬가지로 섹션으로 나뉩니다. 서브 프로시저에서 발견되는 섹션은
# PARAMETERS 섹션, LOCAL DATA 섹션 및 PROCEDURE 섹션입니다.
# PROCEDURE 섹션을 제외한 모든 섹션은 사용하지 않는 경우 건너뛸 수 있습니다.
# PARAMETERS 또는 LOCAL DATA 섹션이 사용되지 않는 경우 PROCEDURE
# 키워드를 생략할 수 있습니다.
sub myFunction
    parameters:
        a is number # LDPL은 참조에 의한 전달입니다.
        b is number
        result is number # 따라서 매개변수를 통해 값을 반환할 수 있습니다.
    local data:
        c is number
    procedure:
        get random in c
        in result solve a + b * c
end sub

sub sayHello
    display "Hi there!" crlf
    return
    display "This won't be displayed :("
end sub

call myFunction with 1 2 myNumber
display myNumber crlf
call sayHello
call sayBye # 서브 프로시저는 선언되기 전에 호출될 수 있습니다.

sub sayBye
    display "Bye!"
end sub

# LDPL의 가장 큰 특징 중 하나는 자신만의
# 문장을 만들 수 있다는 것입니다.

create statement "say hi" executing sayHello
say hi

create statement "random add $ and $ in $" executing myFunction
random add 1 and 2 in myNumber
display myNumber crlf

exit
```

## 더 읽을거리

 * [LDPL 문서](https://docs.ldpl-lang.org)
