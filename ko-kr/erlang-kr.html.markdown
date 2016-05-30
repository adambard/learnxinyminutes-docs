---
language: erlang
contributors:
    - ["Giovanni Cappellotto", "http://www.focustheweb.com/"]
filename: learnerlang-kr.erl
translators:
    - ["Taesung Jung", "https://github.com/tsj"]
lang: ko-kr
---

```erlang
% 퍼센트 기호는 한 줄 주석을 시작한다.

%% 두 개의 퍼센트 문자는 함수의 주석에 사용된다.

%%% 세 개의 퍼센트 문자는 모듈의 주석에 사용된다.

% Erlang에선 3가지 유형의 문장 부호를 사용한다.
% 쉼표(`,`)는 함수 호출에서 인수, 데이터 생성자(constructors), 패턴을 구분한다.
% 마침표(`.`)(다음에 오는 공백)는 셸에서 함수 전체와 식을 구분한다.
% 세미콜론(`;`)은 절을 구분한다. 몇 가지 문맥(contexts)에서 절이 발견된다:
% 함수 정의와 `case`, `if`, `try..catch`, 그리고 `receive` 식


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1. 변수와 패턴 매칭
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Erlang에서 새로운 변수는 `=` 문장에 의해 바인딩 된다.
Num = 42.  % 모든 변수 이름은 반드시 대문자로 시작해야 한다.

% Erlang은 단일 할당 변수(single-assignment variables)를 가진다;
% 만약 다른 값을 `Num` 변수에 할당하려고 시도하면 오류가 발생한다.
Num = 43. % ** 예외 오류: 우변의 값 43과 매칭되지 않음

% 대부분 언어에서 `=`는 할당문을 나타낸다. 그러나 Erlang에서 
% `=`는 패턴 매칭 연산자를 나타낸다. 비어 있는 변수가 `=` 연산자의 좌변에
% 사용되면 바인드(할당) 된다, 그러나 바인드 변수가 좌변에 사용된 경우에
% 다음 행동은 그 바인드 변수가 관측된다.
% `Lhs = Rhs`의 진짜 의미: 우변(`Rhs`)을 평가하고, 그리고
% 그 결과를 좌변(`Lhs`)의 패턴과 매치시켜라.
Num = 7 * 6.

% 부동 소수점 수.
Pi = 3.14159.

% Atom은 숫자가 아닌 서로 다른 상숫값을 표현하는 데 사용한다. Atom은
% 소문자로 시작하고, 연속적인 영숫자(alphanumeric) 문자나 밑줄(`_`) 또는
% 골뱅이(`@`) 기호가 따라온다.
Hello = hello.
OtherNode = example@node.

% 영숫자 값이 아닌 Atom은 작은따옴표로 묶여서 작성될 수 있다.
AtomWithSpace = 'some atom with space'.

% Tuple은 C의 struct와 비슷하다.
Point = {point, 10, 45}.

% Tuple에서 어떤 값을 추출하려면, 패턴 매칭 연산자 `=`를 사용한다.
{point, X, Y} = Point.  % X = 10, Y = 45

% 관심 없는 변수를 위해 자리 표시자(placeholder) `_`를 사용할 수 있다.
% 기호 `_`는 익명 변수(anonymous variable)라 부른다. 일반적인 변수들과
% 다르게 같은 패턴에서 여러 번 나오더라도 동일한 값으로 바인드되지 않아도 된다.
Person = {person, {name, {first, joe}, {last, armstrong}}, {footsize, 42}}.
{_, {_, {_, Who}, _}, _} = Person.  % Who = joe

% List를 만들기 위해서 List의 원소는 대괄호([])로 둘러싸고 쉼표(,)로 구분한다.
% List의 각각의 원소는 어떤 타입도 가능하다.
% List의 첫 번째 원소는 List의 HEAD이다. 만약 List의 HEAD를 제거하면,
% 남은 부분은 List의 TAIL이라 부른다.
ThingsToBuy = [{apples, 10}, {pears, 6}, {milk, 3}].

% 만약 `T`가 List이면, `[H|T]`도 HEAD가 `H`이고 TAIL이 `T`인 List이다.
% 세로 막대(`|`)는 List의 HEAD와 TAIL을 분리한다. `[]`는 빈 List다.
% List의 원소들은 패턴 매칭 연산으로 추출할 수 있다.
% 만약 비어있지 않은 List `L`이 있을 때, `[X|Y] = L` 식의 `X`와 `Y`가
% 바인드되지 않은 변수이면, List의 HEAD는 X에 그리고 TAIL은 Y로 추출된다.
[FirstThing|OtherThingsToBuy] = ThingsToBuy.
% FirstThing = {apples, 10}
% OtherThingsToBuy = [{pears, 6}, {milk, 3}]

% Erlang에는 문자열(String)이 없다. 문자열은 사실 정수의 List일 뿐이다.
% 문자열은 큰따옴표(`"`)로 묶인다.
Name = "Hello".
[72, 101, 108, 108, 111] = "Hello".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2. 순차 프로그래밍
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Erlang에서 Module은 코드의 기본 단위이다. 우리가 작성한 모든 함수는
% Module에 담긴다. Module은 확장자가 `.erl`인 파일에 저장된다.
% 코드가 실행되기 전에 Module은 컴파일되어야 한다. 컴파일된 Module은
% `.beam` 확장자를 가진다.
-module(geometry).
-export([area/1]). % Module로부터 내보내진(exported) 함수의 List

% 함수 `area`는 두 개의 절로 구성된다. 절은 세미콜론(`;`)으로 구분되며,
% 마지막 절은 마침표-공백(dot-whitespace)으로 끝난다.
% 각 절은 서문(head)과 본문(body)을 가진다. 서문은 함수의 이름에 이어서
% 패턴이(괄호 속에) 따라온다. 본문은 연속적인 식으로 구성되고,
% 연속적인 식은 서문의 패턴과 호출한 인수가 성공적으로 매치되면 평가된다.
% 패턴은 함수 정의가 나타나는 순서대로 매치된다.
area({rectangle, Width, Ht}) -> Width * Ht;
area({circle, R})            -> 3.14159 * R * R.

% geometry.erl 파일의 코드 컴파일
c(geometry).  % {ok,geometry}

% 호출하려는 함수를 정확히 알아내기 위해 함수 이름을 Module 이름과 함께
% 명시하는 것이 필요하다.
geometry:area({rectangle, 10, 5}).  % 50
geometry:area({circle, 1.4}).  % 6.15752

% Erlang에서, 같은 Module에 이름이 같고 Arity(인수의 갯수)가 다른
% 두 함수는 전혀 다른 함수를 나타낸다.
-module(lib_misc).
-export([sum/1]). % Arity가 1인 내보내진(export) 함수 `sum`
                  % 하나의 인수만 받음: 정수의 List
sum(L) -> sum(L, 0).
sum([], N)    -> N;
sum([H|T], N) -> sum(T, H+N).

% Fun은 "익명(anonymous)" 함수다. 이름이 없어서 이렇게 부른다.
% 그러나, 변수에 할당될 수 있다.
Double = fun(X) -> 2 * X end. % `Double`은 익명 함수를 가리킨다:
                              % #Fun<erl_eval.6.17052888>
Double(2).  % 4

% 함수는 인수로 Fun을 받거나, Fun을 반환할 수 있다.
Mult = fun(Times) -> ( fun(X) -> X * Times end ) end.
Triple = Mult(3).
Triple(5).  % 15

% List 해석(List comprehensions)은 Fun, Map, Filter 없이 List를 만드는 식이다.
% 표기법 `[F(X) || X <- L]`은 `F(X)`의 List라는 의미이다.
% 이때 `X`는 List `L`로부터 가져온다.
L = [1,2,3,4,5].
[2 * X || X <- L].  % [2,4,6,8,10]
% List 해석은 Generator와 생성된 값들의 부분 집합을 선택하는 Filter를 가질 수 있다.
EvenNumbers = [N || N <- [1, 2, 3, 4], N rem 2 == 0]. % [2, 4]

% Guard는 패턴 매칭의 능력을 향상시키는데 사용할 수 있는 구조다.
% Guard를 사용하면, 패턴에 있는 변수에 대해 간단한 검사와 비교를 수행할 수 있다.
% 함수 정의의 서문(head)에 `when` 키워드로 시작되는 Guard를 사용할 수도 있고,
% 또는 식이 허용되는 언어의 어떤 곳에도 사용될 수 있다.
max(X, Y) when X > Y -> X;
max(X, Y) -> Y.

% Guard는 쉼표(`,`)로 구분된 연속된 Guard 식이다.
% 모든 Guard 식 `GuardExpr1`, `GuardExpr2`, ..., `GuardExprN`이
% `true`로 평가된다면, Guard `GuardExpr1`, `GuardExpr2`, ..., `GuardExprN`는
% 참이다.
is_cat(A) when is_atom(A), A =:= cat -> true;
is_cat(A) -> false.
is_dog(A) when is_atom(A), A =:= dog -> true;
is_dog(A) -> false.

% `=:=` 연산자는 여기서 자세히 다루지 않을 것이다; 두 개의 Erlang 식의 값이 같고
% *그리고* 같은 타입인지 검사하는 데 사용된다고만 알면 된다.
% `==` 연산자의 작동과 대조할 것:
1 + 2 =:= 3.   % true
1 + 2 =:= 3.0. % false
1 + 2 ==  3.0. % true

% 연속적인 Guard는 단일 Guard 또는 세미콜론(`;`)으로 구분된 연속된 Guard다.
% Guard `G1; G2; ...; Gn` 중에 적어도 하나의 Guard가 `true`로 평가된다면,
% 연속적인 Guard `G1; G2; ...; Gn`는 참이다.
is_pet(A) when is_atom(A), (A =:= dog);(A =:= cat) -> true;
is_pet(A)                                          -> false.

% 주의: 모든 유효한 Erlang 식이 Guard 식으로 사용될 수 있는 것은 아니다;
% 특히, 함수 `is_cat`과 `is_dog`는 `is_pet`의 정의 안에 있는 
% 연속적인 Guard 사이에 사용될 수 없다.
% 연속적인 Guard에 허용되는 식의 자세한 설명은 Erlang 레퍼런스 메뉴얼
% [section](http://erlang.org/doc/reference_manual/expressions.html#id81912)
% 을 참조하라.

% Record는 Tuple 안에 이름과 특정 요소를 연결하는 방법을 제공한다.
% Record 정의는 Erlang 소스 코드 파일에 포함되거나 Erlang 소스 코드 파일에
% 포함될 수 있는 확장자가 `.hrl`인 파일에 집어넣을 수 있다.
-record(todo, {
  status = reminder,  % 기본 값
  who = joe,
  text
}).

% Record를 사용할 수 있기 전에 Record 정의를 반드시 셸로 읽어 들여야 한다.
% 셸로 읽어 들이기 위해 셸 함수 `rr`(read records의 약자)을 사용한다.
rr("records.hrl").  % [todo]

% Record 생성과 수정
X = #todo{}.
% #todo{status = reminder, who = joe, text = undefined}
X1 = #todo{status = urgent, text = "Fix errata in book"}.
% #todo{status = urgent, who = joe, text = "Fix errata in book"}
X2 = X1#todo{status = done}.
% #todo{status = done, who = joe, text = "Fix errata in book"}

% `case` 식
% `filter`는 List `L`의 원소 `X` 중에서 `P(X)`가 참인 모든 `X`의 List를 반환한다.
filter(P, [H|T]) ->
  case P(H) of
    true -> [H|filter(P, T)];
    false -> filter(P, T)
  end;
filter(P, []) -> [].
filter(fun(X) -> X rem 2 == 0 end, [1, 2, 3, 4]). % [2, 4]

% `if` 식.
max(X, Y) ->
  if
    X > Y -> X;
    X < Y -> Y;
    true -> nil
  end.

% 주의: 적어도 if 식의 Guard 중의 하나는 반드시 `true`로 평가되어야 한다. 
% 그렇지 않으면 예외가 발생한다.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 3. 예외
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 예외는 내부에 에러가 생겼거나 명시적으로 `throw(Exception)`,
% `exit(Exception)` 또는 `erlang:error(Exception)`를 호출하면
% 시스템에 의해 발생한다.
generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> erlang:error(a).

% Erlang은 예외를 잡는 두 가지 방법을 가지고 있다. 한 가지는
% 예외를 발생시키는 함수의 호출 부분을 `try...catch` 식으로 감싸는 것이다.
catcher(N) ->
  try generate_exception(N) of
    Val -> {N, normal, Val}
  catch
    throw:X -> {N, caught, thrown, X};
    exit:X -> {N, caught, exited, X};
    error:X -> {N, caught, error, X}
  end.

% 다른 방법은 그 호출 부분을 `catch` 식으로 감싸는 것이다.
% 예외를 잡았을 때, 그 예외는 오류를 설명하는 Tuple로 변환된다.
catcher(N) -> catch generate_exception(N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4. 병행성
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Erlang은 병행성을 위해 Actor 모델을 사용한다. Erlang에서 병행 프로그램을
% 작성하는 데 필요한 모든 것은 3가지 기본 형식(primitivies)이다:
% 프로세스 생성, 메시지 보내기, 메시지 받기

% 새로운 프로세스를 시작하기 위해, 함수를 인수로 받는 `spawn` 함수를 사용한다.

F = fun() -> 2 + 2 end. % #Fun<erl_eval.20.67289768>
spawn(F). % <0.44.0>

% `spawn`은 pid(프로세스 식별자)를 반환한다. 이 pid를 프로세스로
% 메시지를 보내는 데 사용할 수 있다. 메시지 전달을 위해, `!` 연산자를 사용한다.
% 위의 기능이 유용하려면, 메시지를 받을 수 있어야 한다. 메시지를 받는 것은
% `receive` 메커니즘을 사용한다.

-module(calculateGeometry).
-compile(export_all).
calculateArea() ->
    receive
      {rectangle, W, H} ->
        W * H;
      {circle, R} ->
        3.14 * R * R;
      _ ->
        io:format("We can only calculate area of rectangles or circles.")
    end.

% Module을 컴파일하고 셸에서 `calculateArea`를 평가한 프로세스를 생성한다.
c(calculateGeometry).
CalculateArea = spawn(calculateGeometry, calculateArea, []).
CalculateArea ! {circle, 2}. % 12.56000000000000049738

% 셸도 마찬가지로 프로세스이다. 현재 pid를 얻기 위해서 `self`를 사용할 수 있다.
self(). % <0.41.0>

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5. EUnit과 테스트
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EUnit의 테스트 생성기(generators)와 assert 매크로를 이용해
% 단위 테스트를 작성할 수 있다.
-module(fib).
-export([fib/1]).
-include_lib("eunit/include/eunit.hrl").

fib(0) -> 1;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).

fib_test_() ->
    [?_assert(fib(0) =:= 1),
     ?_assert(fib(1) =:= 1),
     ?_assert(fib(2) =:= 2),
     ?_assert(fib(3) =:= 3),
     ?_assert(fib(4) =:= 5),
     ?_assert(fib(5) =:= 8),
     ?_assertException(error, function_clause, fib(-1)),
     ?_assert(fib(31) =:= 2178309)
    ].

% EUnit은 Erlang 셸에서 테스트를 실행할 수 있게 
% 자동으로 test() 함수를 내보낸다(export).
fib:test()

% Erlang의 유명한 빌드 툴인 Rebar는 EUnit과 호환된다.
% ```
% rebar eunit
% ```

```

## 참조

* ["Learn You Some Erlang for great good!"](http://learnyousomeerlang.com/)
* ["Programming Erlang: Software for a Concurrent World" by Joe Armstrong](http://pragprog.com/book/jaerlang/programming-erlang)
* [조 암스트롱, 김석준 역, "프로그래밍 얼랭: Software for a Concurrent World", 인사이트](http://ebook.insightbook.co.kr/book/23)
* [Erlang/OTP Reference Documentation](http://www.erlang.org/doc/)
* [Erlang - Programming Rules and Conventions](http://www.erlang.se/doc/programming_rules.shtml)
