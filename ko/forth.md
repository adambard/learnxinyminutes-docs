---
name: Forth
contributors:
    - ["Horse M.D.", "http://github.com/HorseMD/"]
filename: learnforth.fs
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Forth는 1970년대에 Charles H. Moore가 만들었습니다. 명령형, 스택 기반 언어 및 프로그래밍 환경으로, Open Firmware와 같은 프로젝트에서 사용됩니다. NASA에서도 사용됩니다.

참고: 이 문서는 주로 Forth의 Gforth 구현에 중점을 두지만, 여기에 작성된 대부분의 내용은 다른 곳에서도 작동해야 합니다.

```forth
\ 이것은 주석입니다.
( 이것도 주석이지만 단어를 정의할 때만 사용됩니다. )

\ --------------------------------- 전제 조건 ----------------------------------

\ Forth의 모든 프로그래밍은 매개변수 스택(더 일반적으로는 "스택"이라고 함)을 조작하여 수행됩니다.
5 2 3 56 76 23 65    \ ok

\ 이러한 숫자는 왼쪽에서 오른쪽으로 스택에 추가됩니다.
.s    \ <7> 5 2 3 56 76 23 65 ok

\ Forth에서 모든 것은 단어 또는 숫자입니다.

\ ------------------------------ 기본 산술 ------------------------------

\ 산술(실제로 데이터를 필요로 하는 대부분의 단어)은 스택의 데이터를 조작하여 작동합니다.
5 4 +    \ ok

\ `.`는 스택에서 최상위 결과를 팝합니다:
.    \ 9 ok

\ 산술의 추가 예:
6 7 * .        \ 42 ok
1360 23 - .    \ 1337 ok
12 12 / .      \ 1 ok
13 2 mod .     \ 1 ok

99 negate .    \ -99 ok
-99 abs .      \ 99 ok
52 23 max .    \ 52 ok
52 23 min .    \ 23 ok

\ ----------------------------- 스택 조작 -----------------------------

\ 당연히 스택으로 작업할 때 유용한 몇 가지 메서드가 필요합니다:

3 dup -          \ 최상위 항목 복제 (1번째가 이제 2번째와 같음): 3 - 3
2 5 swap /       \ 최상위 항목을 두 번째 요소와 교환:        5 / 2
6 4 5 rot .s     \ 최상위 3개 요소 회전:                   4 5 6
4 0 drop 2 /     \ 최상위 항목 제거 (화면에 인쇄하지 않음):  4 / 2
1 2 3 nip .s     \ 두 번째 항목 제거 (drop과 유사):    1 3

\ ---------------------- 더 고급 스택 조작 ----------------------

1 2 3 4 tuck   \ 최상위 항목을 두 번째 슬롯 아래에 복제:      1 2 4 3 4 ok
1 2 3 4 over   \ 두 번째 항목을 맨 위로 복제:             1 2 3 4 3 ok
1 2 3 4 2 roll \ 해당 위치의 항목을 맨 위로 *이동*:      1 3 4 2 ok
1 2 3 4 2 pick \ 해당 위치의 항목을 맨 위로 *복제*: 1 2 3 4 2 ok

\ 스택 인덱스를 참조할 때 0부터 시작합니다.

\ ------------------------------ 단어 만들기 --------------------------------

\ `:` 단어는 Forth를 `;` 단어를 볼 때까지 컴파일 모드로 설정합니다.
: square ( n -- n ) dup * ;    \ ok
5 square .                     \ 25 ok

\ 단어가 무엇을 하는지 볼 수도 있습니다:
see square     \ : square dup * ; ok

\ -------------------------------- 조건문 --------------------------------

\ -1 == true, 0 == false. 그러나 0이 아닌 값은 일반적으로 참으로 처리됩니다:
42 42 =    \ -1 ok
12 53 =    \ 0 ok

\ `if`는 컴파일 전용 단어입니다. `if` <할 일> `then` <나머지 프로그램>.
: ?>64 ( n -- n ) dup 64 > if ." Greater than 64!" then ; \ ok
100 ?>64                                                  \ Greater than 64! ok

\ Else:
: ?>64 ( n -- n ) dup 64 > if ." Greater than 64!" else ." Less than 64!" then ;
100 ?>64    \ Greater than 64! ok
20 ?>64     \ Less than 64! ok

\ ------------------------------------ 루프 -----------------------------------

\ `?do`도 컴파일 전용 단어입니다.
: myloop ( -- ) 5 0 ?do cr ." Hello!" loop ; \ ok
myloop
\ Hello!
\ Hello!
\ Hello!
\ Hello!
\ Hello! ok

\ `?do`는 스택에 두 개의 숫자를 예상합니다: 끝 숫자(제외)와 시작 숫자(포함).

\ `i`를 사용하여 루프를 돌면서 인덱스 값을 얻을 수 있습니다:
: one-to-12 ( -- ) 12 0 do i . loop ;     \ ok
one-to-12                                 \ 0 1 2 3 4 5 6 7 8 9 10 11 ok

\ `do`는 비슷하게 작동하지만, 시작과 끝이 정확히 같으면 산술 언더플로가 발생할 때까지 영원히 반복됩니다.
: loop-forever 1 1 do i square . loop ;     \ ok
loop-forever                                \ 1 4 9 16 25 36 49 64 81 100 ...

\ `+loop`로 "단계"를 변경합니다:
: threes ( n n -- ) ?do i . 3 +loop ;    \ ok
15 0 threes                             \ 0 3 6 9 12 ok

\ `begin` <할 일> <플래그> `until`을 사용한 무한 루프:
: death ( -- ) begin ." Are we there yet?" 0 until ;    \ ok

\ ---------------------------- 변수 및 메모리 ----------------------------

\ `variable`을 사용하여 `age`를 변수로 선언합니다.
variable age    \ ok

\ 그런 다음 `!` 단어로 age에 21을 씁니다.
21 age !    \ ok

\ 마지막으로 `@` "읽기" 단어를 사용하여 변수를 인쇄할 수 있습니다. 이 단어는 값을 스택에 추가하거나, 한 번에 읽고 인쇄하는 `?`를 사용할 수 있습니다.
age @ .    \ 21 ok
age ?      \ 21 ok

\ 상수는 매우 유사하지만 메모리 주소를 신경 쓰지 않습니다:
100 constant WATER-BOILING-POINT    \ ok
WATER-BOILING-POINT .               \ 100 ok

\ ----------------------------------- 배열 -----------------------------------

\ 배열을 만드는 것은 변수와 유사하지만 더 많은 메모리를 할당해야 합니다.

\ `2 cells allot`을 사용하여 길이가 3인 배열을 만들 수 있습니다:
variable mynumbers 2 cells allot    \ ok

\ 모든 값을 0으로 초기화합니다.
mynumbers 3 cells erase    \ ok

\ 또는 `fill`을 사용할 수 있습니다:
mynumbers 3 cells 0 fill

\ 또는 위의 모든 것을 건너뛰고 특정 값으로 초기화할 수 있습니다:
create mynumbers 64 , 9001 , 1337 , \ ok (마지막 `,`가 중요합니다!)

\ ...와 동일합니다:

\ 각 인덱스에 수동으로 값 쓰기:
64 mynumbers 0 cells + !      \ ok
9001 mynumbers 1 cells + !    \ ok
1337 mynumbers 2 cells + !    \ ok

\ 특정 배열 인덱스에서 값 읽기:
0 cells mynumbers + ?    \ 64 ok
1 cells mynumbers + ?    \ 9001 ok

\ 배열 조작을 위한 도우미 단어를 만들어 약간 단순화할 수 있습니다:
: of-arr ( n n -- n ) cells + ;    \ ok
mynumbers 2 of-arr ?               \ 1337 ok

\ 쓰기에도 사용할 수 있습니다:
20 mynumbers 1 of-arr !    \ ok
mynumbers 1 of-arr ?       \ 20 ok

\ ------------------------------ 반환 스택 ------------------------------

\ 반환 스택은 단어가 다른 단어(예: 루프)를 실행할 때 포인터를 보유하는 데 사용됩니다.

\ 이미 반환 스택의 맨 위를 복제하는 `i`를 보았습니다. `i`는 `r@`와 동일합니다.
: myloop ( -- ) 5 0 do r@ . loop ;    \ ok

\ 읽기뿐만 아니라 반환 스택에 추가하고 제거할 수도 있습니다:
5 6 4 >r swap r> .s    \ 6 5 4 ok

\ 참고: Forth는 단어 포인터에 반환 스택을 사용하므로 `>r` 뒤에는 항상 `r>`가 와야 합니다.

\ ------------------------- 부동 소수점 연산 --------------------------

\ 대부분의 Forth는 부동 소수점 연산 사용을 피하는 경향이 있습니다.
8.3e 0.8e f+ f.    \ 9.1 ok

\ 일반적으로 부동 소수점을 다룰 때 단어 앞에 'f'를 붙입니다:
variable myfloatingvar    \ ok
4.4e myfloatingvar f!     \ ok
myfloatingvar f@ f.       \ 4.4 ok

\ --------------------------------- 최종 참고 사항 --------------------------------

\ 존재하지 않는 단어를 입력하면 스택이 비워집니다. 그러나 특별히 이를 위한 단어도 있습니다:
clearstack

\ 화면 지우기:
page

\ Forth 파일 로드:
\ s" forthfile.fs" included

\ Forth의 사전에 있는 모든 단어를 나열할 수 있습니다(하지만 목록이 매우 큽니다!):
\ words

\ Gforth 종료:
\ bye
```

## 추가 자료

* [Starting Forth](http://www.forth.com/starting-forth/)
* [Simple Forth](http://www.murphywong.net/hello/simple.htm)
* [Thinking Forth](http://thinking-forth.sourceforge.net/)