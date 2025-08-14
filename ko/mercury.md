---
name: Mercury
contributors:
    - ["Julian Fondren", "https://mercury-in.space/"]
---

Mercury는 Prolog, ML, Haskell의 영향을 받은 엄격하고 순수한 함수형/논리형 프로그래밍 언어입니다.

```prolog
% 퍼센트 기호는 한 줄 주석을 시작합니다.

    % foo(Bar, Baz)
    %
    % 문서 주석은 설명하는 내용 앞에 들여쓰기됩니다.
:- pred foo(bar::in, baz::out) is det.

% 모든 최상위 구문 요소는 '.'(마침표)로 끝납니다.

% Mercury 용어는 술어 논리에서 유래했습니다. 대략적으로:

% | Mercury               | C                            |
% |                       |                              |
% | Goal (목표)           | statement (문장)             |
% | expression (표현식)   | expression (표현식)          |
% | predicate rule (술어 규칙) | void function (void 함수) |
% | function rule (함수 규칙) | function (함수)              |
% | head (of a rule) (헤드) | function name and parameters |
% | body (of a rule) (본문) | function body (함수 본문)    |
% | fact (사실)           | (rule without a body) (본문 없는 규칙) |
% | pred/func declaration | function signature (함수 시그니처) |
% | A, B (논리곱)         | A && B                       |
% | A ; B (논리합)        | if (A) {} else if (B) {}     |

% 몇 가지 사실:
man(socrates).  % "소크라테스는 사람이라는 사실입니다"
man(plato).
man(aristotle).

% 규칙:
mortal(X) :- man(X).  % "X가 사람이면 X는 필멸자라는 규칙입니다."
%            ^^^^^^-- 규칙의 본문
%         ^^-- 화살표 <--, 본문에서 헤드를 가리킴
%^^^^^^^^-- 규칙의 헤드
% 이것은 또한 규칙을 정의하는 단일 절입니다.

% X가 대문자라는 것으로 변수임을 알 수 있습니다.
% socrates가 소문자라는 것으로 항임을 알 수 있습니다.

% 'socrates'가 정의되지 않으면 오류입니다. 타입을 가져야 합니다:

% 선언은 ':-'로 시작합니다.
:- type people
    --->    socrates
    ;       plato
    ;       aristotle
    ;       hermes.
    %<--첫 번째 탭 정지 (4칸 탭 사용)
            %<--세 번째 탭 정지 (---> 다음 첫 번째)

:- pred man(people).  % 규칙과 사실도 타입이 필요합니다.

% 규칙의 모드는 사용 방법을 알려줍니다.
:- mode man(in) is semidet.  % man(plato)는 성공합니다. man(hermes)는 실패합니다.
:- mode man(out) is multi.   % man(X)는 X를 socrates, plato, aristotle 중 하나에 바인딩합니다.

% semidet 술어는 테스트와 같습니다. 값을 반환하지는 않지만
% 성공하거나 실패할 수 있으며, 백트래킹이나 논리합 또는
% 조건문의 다른 쪽을 트리거합니다.

% 'is semidet'은 모드의 결정성을 제공합니다. 다른 결정성:
% | 실패할 수 있는가? | 해 없음     | 1개      | 1개 이상    |
% |                   |             |          |             |
% | 아니요            | 오류        | det      | multi       |
% | 예                | 실패        | semidet  | nondet      |

:- pred mortal(people::in) is semidet.  % 하나의 선언에 타입/모드

% 이 규칙의 본문은 두 개의 논리곱 A, B, C로 구성됩니다.
% 이 규칙은 A, B, C가 모두 참일 때 참입니다.
% age(P)가 16을 반환하면 실패합니다.
% alive(P)가 실패하면 실패합니다.
:- type voter(people::in) is semidet.
voter(P) :-
    alive(P),
    registered(P, locale(P)),
    age(P) >= 18.  % age/1은 함수이고, int.>=는 연산자로 사용되는 함수입니다.

% "P는 살아있고, P의 지역에 등록되어 있으며, P의 나이가
% 18세 이상이면 유권자입니다."

% 여기에 사용된 >=는 기본적으로 가져오지 않는 'int' 모듈에서 제공됩니다.
% Mercury는 매우 작은 'Prelude'('builtin' 모듈)를 가지고 있습니다.
% 리스트 리터럴을 사용하려면 'list' 모듈도 가져와야 합니다.
```

완전한 실행 가능 예제. 'types.m' 파일에 있으며 'mmc --make types'로 컴파일합니다.

```prolog
:- module types.
:- interface.
:- import_module io.  % io.io 타입에 필요...
% main/2는 보통 'det'입니다. 스레딩 및 예외는 'cc_multi'가 필요합니다.
:- pred main(io::di, io::uo) is cc_multi.  % 프로그램 진입점
:- implementation.
:- import_module int, float, string, list, bool, map, exception.

% 열거형.
:- type days
    --->    sunday
    ;       monday
    ;       tuesday
    ;       wednesday
    ;       thursday
    ;       friday
    ;       saturday.

% 식별 합집합, ML의 데이터 타입과 유사.
:- type payment_method
    --->    cash(int)
    ;       credit_card(
                name :: string,         % 이름 있는 필드
                cc_number :: string,
                cvv :: int,
                expiration :: string
            )
    ;       crypto(coin_type, wallet, amount).

:- type coin_type
    --->    etherium
    ;       monero.  % "다른 코인도 사용 가능"

% 타입 별칭.
:- type wallet == string.
:- type amount == int.

% !IO는 io.io 인수의 쌍입니다.
% I/O를 수행하기 위해 I/O를 수행하는 모든 것에 전달하십시오.
% 그렇지 않으면 순수하지 않은 많은 함수가 !IO를 사용하여 'I/O 상태에 연결'할 수 있습니다.
main(!IO) :-
    Ints = [
        3,
        1 + 1,
        8 - 1,
        10 * 2,
        35 / 5,
        5 / 2,      % 절단 나눗셈
        int.div(5, 2),  % 내림 나눗셈
        div(5, 2),  % (타입으로 인해 모호하지 않음)
        5 `div` 2,  % (모든 이진 함수는 ``를 사용하여 연산자가 될 수 있음)
        7 `mod` 3,  % 내림 나눗셈의 모듈로
        7 `rem` 3,  % 절단 나눗셈의 나머지
        2 `pow` 4,  % 2의 4제곱
        (1 + 3) * 2,    % 괄호는 일반적인 의미를 가짐

        2 >> 3,     % 비트 단위 오른쪽 시프트
        128 << 3,   % 비트 단위 왼쪽 시프트
        \ 0,        % 비트 단위 보수
        5 /\ 1,     % 비트 단위 AND
        5 \/ 1,     % 비트 단위 OR
        5 `xor` 3,  % 비트 단위 XOR

        max_int,
        min_int,

        5 `min` 3,  % ( 5 > 3 이면 3 아니면 5 )
        5 `max` 3
    ],
    Bools = [
        yes,
        no
        % Mercury에서는 제어 흐름이 불리언 표현식 대신
        % semidet 목표에 의해 이루어지므로 불리언은 훨씬 덜 중요합니다.
    ],
    Strings = [
        "이것은 문자열입니다",
        "문자열은 이중화를 통해 \"\" 포함된 큰따옴표를 가질 수 있습니다",
        "문자열은 일반적인 이스케이프 \u4F60\u597D\n을 지원합니다",
        % 문자열의 암시적 연결 없음: "concat:" "together"
        "하지만 string.++ 연산자 " ++ "를 사용할 수 있습니다",

        % 두 번째 매개변수는 list(string.poly_type)입니다.
        % s/1은 문자열을 받아 poly_type을 반환하는 함수입니다.
        % i/1은 int를, f/1은 float를, c/1은 char를 받습니다.
        string.format("안녕하세요, %d번째 %s\n", [i(45), s("세상")])
    ],

    % 'map'과 'list'와 같은 순수 함수형 타입으로 시작하십시오!
    % 배열과 해시 테이블도 사용할 수 있지만, 사용하려면
    % Mercury에 대해 훨씬 더 많이 알아야 합니다.
    get_map1(Map1),
    get_map2(Map2),

    % list.foldl에는 *많은* 변형이 있습니다.
    % 이것은 리스트의 각 X에 대해 io.print_line(X, !IO)를 호출합니다.
    foldl(io.print_line, Ints, !IO),
    foldl(io.print_line, Bools, !IO),
    foldl(io.print_line, Strings, !IO),
    io.print_line(Map1, !IO),
    % ( Cond 이면 ThenGoal 아니면 ElseGoal )
    % Cond에서 I/O 허용 안 됨: I/O는 실패할 수 없습니다!
    ( if Map2^elem(42) = Elem then
        io.print_line(Elem, !IO)
    else % 항상 필요
        true  % 아무것도 하지 않고 성공 ('fail'과 반대)
    ),

    % 예외 처리:
    ( try [io(!IO)] ( % io/1 매개변수 필요, 그렇지 않으면 여기서 I/O 허용 안 됨
        io.print_line(received(cash(1234)), !IO),
        io.print_line(received(crypto(monero, "invalid", 123)), !IO)
    ) then
        io.write_string("모든 결제 수락됨\n", !IO) % 절대 도달하지 않음
    catch "monero not yet supported" -> % 매우 구체적인 catch!
        io.write_string("monero 결제 실패\n", !IO)
    ).

:- pred get_map1(map(string, int)::out) is det.
get_map1(!:Map) :-  % 헤드의 !:Map은 최종 (자유, 바인딩되지 않은) Map입니다.
    !:Map = init,   % 본문의 !:Map은 다음 Map입니다.
    det_insert("hello", 1, !Map),  % Map 변수 쌍
    det_insert("world", 2, !Map),

    % 현재 (바인딩된) Map의 디버그 인쇄
    % 다른 [Params]는 런타임 또는 컴파일 타임 플래그에 따라 선택 사항으로 만들 수 있습니다.
    trace [io(!IO)] (io.print_line(!.Map, !IO)),

    det_insert_from_corresponding_lists(K, V, !Map),
    % 이 코드는 K와 V가 사용되기 전에 정의되도록 재정렬되었습니다.
    K = ["more", "words", "here"],
    V = [3, 4, 5].

:- pred get_map2(map(int, bool)::out) is det.
get_map2(Map) :-
    det_insert(42, yes, map.init, Map).

:- func received(payment_method) = string.
received(cash(N)) = string.format("%d달러 받음", [i(N)]).
received(credit_card(_, _, _, _)) = "신용카드 받음".  % _는 버리는 값
received(crypto(Type, _Wallet, Amount)) = S :-  % _Wallet은 이름 있는 버리는 값
    ( % case/switch 구조
        Type = etherium,
        S = string.format("%d ETH 받는 중", [i(Amount)])
    ;
        Type = monero,
        throw("monero는 아직 지원되지 않음")  % 문자열을 페이로드로 사용하는 예외
    ).
```

## 빠르네요! 더 원하시나요?

### 추가 튜토리얼

* [Mercury 튜토리얼](https://mercurylang.org/documentation/papers/book.pdf) (pdf 링크) - 더 여유로운 속도의 더 전통적인 튜토리얼
* [Mercury Crash Course](https://mercury-in.space/crash.html) - Q&A 형식의 밀도 높은 예제 중심 튜토리얼
* [GitHub Wiki 튜토리얼](https://github.com/Mercury-Language/mercury/wiki/Tutorial)
* [Getting Started with Mercury](https://bluishcoder.co.nz/2019/06/23/getting-started-with-mercury.html) - 설치 및 첫 단계

### 문서

* 언어 매뉴얼, 사용자 가이드, 라이브러리 참조는 모두 다음에 있습니다.
  [mercurylang.org](https://mercurylang.org/documentation/documentation.html)
