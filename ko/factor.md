---
name: Factor
contributors:
    - ["hyphz", "http://github.com/hyphz/"]
filename: learnfactor.factor
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---
Factor는 Slava Pestov가 만든 Forth를 기반으로 한 현대적인 스택 기반 언어입니다.

이 파일의 코드는 Factor에 입력할 수 있지만, 어휘 및 가져오기 헤더가 시작을 완전히 혼란스럽게 만들기 때문에 직접 가져올 수는 없습니다.

```factor
! 이것은 주석입니다.

! Forth와 마찬가지로 모든 프로그래밍은 스택을 조작하여 수행됩니다.
! 리터럴 값을 명시하면 스택에 푸시됩니다.
5 2 3 56 76 23 65    ! 출력은 없지만 스택은 대화형 모드에서 인쇄됩니다.

! 이러한 숫자는 왼쪽에서 오른쪽으로 스택에 추가됩니다.
! .s는 스택을 비파괴적으로 인쇄합니다.
.s     ! 5 2 3 56 76 23 65

! 산술은 스택의 데이터를 조작하여 작동합니다.
5 4 +    ! 출력 없음

! `.`는 스택에서 최상위 결과를 팝하고 인쇄합니다.
.    ! 9

! 산술의 추가 예:
6 7 * .        ! 42
1360 23 - .    ! 1337
12 12 / .      ! 1
13 2 mod .     ! 1

99 neg .       ! -99
-99 abs .      ! 99
52 23 max .    ! 52
52 23 min .    ! 23

! 스택을 조작하기 위해 여러 단어가 제공되며, 이를 통칭하여 셔플 단어라고 합니다.

3 dup -          ! 최상위 항목 복제 (1번째가 이제 2번째와 같음): 3 - 3
2 5 swap /       ! 최상위 항목을 두 번째 요소와 교환:        5 / 2
4 0 drop 2 /     ! 최상위 항목 제거 (화면에 인쇄하지 않음):  4 / 2
1 2 3 nip .s     ! 두 번째 항목 제거 (drop과 유사):    1 3
1 2 clear .s     ! 전체 스택 지우기
1 2 3 4 over .s  ! 두 번째 항목을 맨 위로 복제: 1 2 3 4 3
1 2 3 4 2 pick .s ! 세 번째 항목을 맨 위로 복제: 1 2 3 4 2 3

! 단어 만들기
! `:` 단어는 `;` 단어를 볼 때까지 Factor를 컴파일 모드로 설정합니다.
: square ( n -- n ) dup * ;    ! 출력 없음
5 square .                     ! 25

! 단어가 무엇을 하는지 볼 수도 있습니다.
! \는 단어의 평가를 억제하고 대신 스택에 식별자를 푸시합니다.
\ square see    ! : square ( n -- n ) dup * ;

! 만들 단어의 이름 뒤에 대괄호 사이의 선언은 스택 효과를 제공합니다.
! 선언 내에서 원하는 이름을 사용할 수 있습니다:
: weirdsquare ( camel -- llama ) dup * ;

! 단어의 스택 효과와 개수가 일치하는 경우:
: doubledup ( a -- b ) dup dup ; ! 오류: 스택 효과 선언이 잘못되었습니다.
: doubledup ( a -- a a a ) dup dup ; ! 확인
: weirddoubledup ( i -- am a fish ) dup dup ; ! 또한 확인

! Factor가 Forth와 다른 점은 인용 부호를 사용하는 것입니다.
! 인용 부호는 값으로 스택에 푸시되는 코드 블록입니다.
! [는 인용 부호 모드를 시작하고 ]는 종료합니다.
[ 2 + ]       ! 2를 더하는 인용 부호가 스택에 남습니다.
4 swap call . ! 6

! 따라서 고차 단어입니다. 수많은 고차 단어입니다.
2 3 [ 2 + ] dip .s      ! 최상위 스택 값을 팝하고, 인용 부호를 실행하고, 다시 푸시합니다: 4 3
3 4 [ + ] keep .s       ! 최상위 스택 값을 복사하고, 인용 부호를 실행하고, 복사본을 푸시합니다: 7 4
1 [ 2 + ] [ 3 + ] bi .s ! 각 인용 부호를 최상위 값에 대해 실행하고, 두 결과를 모두 푸시합니다: 3 4
4 3 1 [ + ] [ + ] bi .s ! bi의 인용 부호는 스택의 더 깊은 곳에서 값을 가져올 수 있습니다: 4 5 ( 1+3 1+4 )
1 2 [ 2 + ] bi@ .s      ! 첫 번째 및 두 번째 값에 대해 인용 부호를 실행합니다.
2 [ + ] curry           ! 지정된 값을 인용 부호의 시작 부분에 주입합니다: [ 2 + ]가 스택에 남습니다.

! 조건문
! 내장 값 f를 제외한 모든 값은 참입니다.
! 내장 값 t는 존재하지만 사용이 필수는 아닙니다.
! 조건문은 위의 조합기와 같은 고차 단어입니다.

5 [ "Five is true" . ] when                     ! Five is true
0 [ "Zero is true" . ] when                     ! Zero is true
f [ "F is true" . ] when                        ! 출력 없음
f [ "F is false" . ] unless                     ! F is false
2 [ "Two is true" . ] [ "Two is false" . ] if   ! Two is true

! 기본적으로 조건문은 테스트 중인 값을 소비하지만, 별표가 붙은 변형은
! 참인 경우 그대로 둡니다:

5 [ . ] when*      ! 5
f [ . ] when*      ! 출력 없음, 빈 스택, f는 거짓이므로 소비됩니다.


! 루프
! 짐작하셨겠지만.. 이것들도 고차 단어입니다.

5 [ . ] each-integer               ! 0 1 2 3 4
4 3 2 1 0 5 [ + . ] each-integer   ! 0 2 4 6 8
5 [ "Hello" . ] times              ! Hello Hello Hello Hello Hello

! 다음은 목록입니다:
{ 2 4 6 8 }                        ! 한 항목으로 스택에 들어갑니다.

! 목록을 반복합니다:
{ 2 4 6 8 } [ 1 + . ] each          ! 3 5 7 9 인쇄
{ 2 4 6 8 } [ 1 + ] map             ! 스택에 { 3 5 7 9 }를 남깁니다.

! 목록을 줄이거나 빌드하는 루프:
{ 1 2 3 4 5 } [ 2 mod 0 = ] filter  ! 인용 부호가 참을 산출하는 목록 멤버만 유지합니다: { 2 4 }
{ 2 4 6 8 } 0 [ + ] reduce .        ! 함수형 언어의 "fold"와 같습니다: 20 인쇄 (0+2+4+6+8)
{ 2 4 6 8 } 0 [ + ] accumulate . .  ! reduce와 같지만 중간 값을 목록에 유지합니다: { 0 2 6 12 }를 인쇄한 다음 20을 인쇄합니다.
1 5 [ 2 * dup ] replicate .         ! 인용 부호를 5번 반복하고 결과를 목록에 수집합니다: { 2 4 8 16 32 } 인쇄
1 [ dup 100 < ] [ 2 * dup ] produce ! 첫 번째 인용 부호가 거짓을 반환할 때까지 두 번째 인용 부호를 반복하고 결과를 수집합니다: { 2 4 8 16 32 64 128 }

! 다른 모든 방법이 실패하면 범용 while 루프:
1 [ dup 10 < ] [ "Hello" . 1 + ] while  ! "Hello"를 10번 인쇄합니다.
                                        ! 예, 읽기 어렵습니다.
                                        ! 이것이 모든 변형 루프가 있는 이유입니다.

! 변수
! 일반적으로 Factor 프로그램은 모든 데이터를 스택에 유지해야 합니다.
! 명명된 변수를 사용하면 리팩토링이 더 어려워집니다(그리고 Factor라고 불리는 데는 이유가 있습니다).
! 전역 변수, 꼭 필요한 경우:

SYMBOL: name            ! name을 식별 단어로 생성합니다.
"Bob" name set-global   ! 출력 없음
name get-global .       ! "Bob"

! 명명된 지역 변수는 확장으로 간주되지만 사용할 수 있습니다.
! 인용 부호에서..
[| m n                  ! 인용 부호는 최상위 두 스택 값을 m과 n으로 캡처합니다.
 | m n + ]              ! 읽기

! 또는 단어에서..
:: lword ( -- )           ! 어휘 변수 확장을 호출하려면 이중 콜론 참고
   2 :> c                 ! 불변 변수 c를 선언하여 2를 보유합니다.
   c . ;                  ! 인쇄

! 이 방법으로 선언된 단어에서 스택 선언의 입력 쪽은
! 의미가 있으며 스택 값이 캡처되는 변수 이름을 제공합니다.
:: double ( a -- result ) a 2 * ;

! 변수는 이름이 느낌표로 끝나는 경우 변경 가능으로 선언됩니다.
:: mword2 ( a! -- x y )   ! 스택의 맨 위를 변경 가능한 변수 a에 캡처합니다.
   a                      ! a 푸시
   a 2 * a!               ! a에 2를 곱하고 결과를 a에 다시 저장합니다.
   a ;                    ! a의 새 값을 푸시합니다.
5 mword2                  ! 스택: 5 10

! 목록 및 시퀀스
! 위에서 목록을 스택에 푸시하는 방법을 보았습니다.

0 { 1 2 3 4 } nth         ! 목록의 특정 멤버에 액세스: 1
10 { 1 2 3 4 } nth        ! 오류: 시퀀스 인덱스가 범위를 벗어났습니다.
1 { 1 2 3 4 } ?nth        ! 인덱스가 범위 내에 있으면 nth와 동일: 2
10 { 1 2 3 4 } ?nth       ! 범위 밖에 있으면 오류 없음: f

{ "at" "the" "beginning" } "Append" prefix    ! { "Append" "at" "the" "beginning" }
{ "Append" "at" "the" } "end" suffix          ! { "Append" "at" "the" "end" }
"in" 1 { "Insert" "the" "middle" } insert-nth ! { "Insert" "in" "the" "middle" }
"Concat" "enate" append                       ! "Concatenate" - 문자열도 시퀀스입니다.
"Concatenate" "Reverse " prepend              ! "Reverse Concatenate"
{ "Concatenate " "seq " "of " "seqs" } concat ! "Concatenate seq of seqs"
{ "Connect" "subseqs" "with" "separators" } " " join  ! "Connect subseqs with separators"

! 그리고 메타를 원한다면 인용 부호는 시퀀스이며 분해할 수 있습니다..
0 [ 2 + ] nth                              ! 2
1 [ 2 + ] nth                              ! +
[ 2 + ] \ - suffix                         ! 인용 부호 [ 2 + - ]
```

## 추가 자료

* [Factor 문서](http://docs.factorcode.org/content/article-help.home.html)