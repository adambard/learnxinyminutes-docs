---
name: LOLCODE
filename: learnLOLCODE.lol
contributors:
    - ["abactel", "https://github.com/abactel"]
---

LOLCODE는 [롤캣](https://upload.wikimedia.org/wikipedia/commons/a/ab/Lolcat_in_folder.jpg?1493656347257)의 말투를 닮도록 설계된 난해한 프로그래밍 언어입니다.

```
BTW 이건 한 줄 주석임
BTW 모든 코드는 `HAI <언어 버전>`으로 시작해서 `KTHXBYE`로 끝나야 함

HAI 1.3
CAN HAS STDIO? BTW 표준 헤더 가져오는 중

OBTW
     ==========================================================================
     ================================= 기초 ===================================
     ==========================================================================
TLDR

BTW 텍스트 표시하기:
VISIBLE "HELLO WORLD"

BTW 변수 선언하기:
I HAS A MESSAGE ITZ "CATZ ARE GOOD"
VISIBLE MESSAGE

OBTW
    (이건 코드 블록임.) 변수는 동적 타입이라서 타입을 선언할 필요 없음.
    변수 타입은 내용에 따라 정해짐. 타입 종류는 다음과 같음:
TLDR

I HAS A STRING  ITZ "DOGZ ARE GOOOD" BTW 타입은 YARN (실)
I HAS A INTEGER ITZ 42               BTW 타입은 NUMBR (숫자)
I HAS A FLOAT   ITZ 3.1415           BTW 타입은 NUMBAR (소수)
I HAS A BOOLEAN ITZ WIN              BTW 타입은 TROOF (진실)
I HAS A UNTYPED                      BTW 타입은 NOOB (뉴비)

BTW 사용자 입력 받기:
I HAS A AGE
GIMMEH AGE
BTW 변수는 YARN으로 저장됨. NUMBR로 바꾸려면:
AGE IS NOW A NUMBR

OBTW
     ==========================================================================
     ================================== 수학 ==================================
     ==========================================================================
TLDR

BTW LOLCODE는 폴란드 표기법 스타일의 수학을 씀.

BTW 기본 수학 표기법:

SUM OF 21 AN 33         BTW 21 + 33
DIFF OF 90 AN 10        BTW 90 - 10
PRODUKT OF 12 AN 13     BTW 12 * 13
QUOSHUNT OF 32 AN 43    BTW 32 / 43
MOD OF 43 AN 64         BTW 43 modulo 64
BIGGR OF 23 AN 53       BTW max(23, 53)
SMALLR OF 53 AN 45      BTW min(53, 45)

BTW 2진 표기법:

BOTH OF WIN AN WIN           BTW 그리고: x=WIN, y=WIN이면 WIN
EITHER OF FAIL AN WIN        BTW 또는: x=FAIL, y=FAIL이면 FAIL
WON OF WIN AN FAIL           BTW xor: x=y이면 FAIL
NOT FAIL                     BTW 단항 부정: x=FAIL이면 WIN
ALL OF WIN AN WIN MKAY   BTW 무한 인수 AND
ANY OF WIN AN FAIL MKAY  BTW 무한 인수 OR

BTW 비교:

BOTH SAEM "CAT" AN "DOG"             BTW x == y 이면 WIN
DIFFRINT 732 AN 184                  BTW x != y 이면 WIN
BOTH SAEM 12 AN BIGGR OF 12 AN 4     BTW x >= y
BOTH SAEM 43 AN SMALLR OF 43 AN 56   BTW x <= y
DIFFRINT 64 AN SMALLR OF 64 AN 2     BTW x > y
DIFFRINT 75 AN BIGGR OF 75 AN 643    BTW x < y

OBTW
     ==========================================================================
     ============================== 흐름 제어 ==============================
     ==========================================================================
TLDR

BTW If/then 문:
I HAS A ANIMAL
GIMMEH ANIMAL
BOTH SAEM ANIMAL AN "CAT", O RLY?
    YA RLY
        VISIBLE "고양이가 있군요"
    MEBBE BOTH SAEM ANIMAL AN "MAUS"
        VISIBLE "냠냠냠. 내가 먹었음."
    NO WAI
        VISIBLE "으악 멍멍이다"
OIC

BTW Case 문:
I HAS A COLOR
GIMMEH COLOR
COLOR, WTF?
    OMG "R"
        VISIBLE "빨간 물고기"
        GTFO
    OMG "Y"
        VISIBLE "노란 물고기"
        BTW `GTFO`가 없어서 다음 문장도 테스트될 거임
    OMG "G"
    OMG "B"
        VISIBLE "물고기는 맛이 있음"
        GTFO
    OMGWTF
        VISIBLE "물고기가 투명해 오노 왓"
OIC

BTW For 반복문:
I HAS A TEMPERATURE
GIMMEH TEMPERATURE
TEMPERATURE IS NOW A NUMBR
IM IN YR LOOP UPPIN YR ITERATOR TIL BOTH SAEM ITERATOR AN TEMPERATURE
    VISIBLE ITERATOR
IM OUTTA YR LOOP

BTW While 반복문:
IM IN YR LOOP NERFIN YR ITERATOR WILE DIFFRINT ITERATOR AN -10
    VISIBLE ITERATOR
IM OUTTA YR LOOP

OBTW
     =========================================================================
     =============================== 문자열 ================================
     =========================================================================
TLDR

BTW 줄바꿈:
VISIBLE "첫 번째 줄 :) 두 번째 줄"

BTW 탭:
VISIBLE ":>공백이 최고임"

BTW 벨 (삑 소리 남):
VISIBLE "다음 손님 오세요 :o"

BTW 리터럴 큰따옴표:
VISIBLE "그가 말했음 :"나는 케이크를 좋아해:""

BTW 리터럴 콜론:
VISIBLE "내가 사는 곳:: 사이버 공간"

OBTW
     =========================================================================
     =============================== 함수 ===============================
     =========================================================================
TLDR

BTW 새 함수 선언하기:
HOW IZ I SELECTMOVE YR MOVE BTW `MOVE`는 인수임
    BOTH SAEM MOVE AN "ROCK", O RLY?
        YA RLY
            VISIBLE "바위가 있군요"
        NO WAI
            VISIBLE "오노 가위-가위다"
    OIC
    GTFO BTW 이건 NOOB를 반환함
IF U SAY SO

BTW 함수 선언하고 값 반환하기:
HOW IZ I IZYELLOW
    FOUND YR "YELLOW"
IF U SAY SO

BTW 함수 호출하기:
I IZ IZYELLOW MKAY

KTHXBYE
```

## 더 읽을거리:

- [LCI 컴파일러](https://github.com/justinmeza/lci)
- [공식 사양](https://github.com/justinmeza/lolcode-spec/blob/master/v1.2/lolcode-spec-v1.2.md)
