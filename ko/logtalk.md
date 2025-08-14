---
name: Logtalk
filename: learnlogtalk.lgt
contributors:
    - ["Paulo Moura", "http://github.com/pmoura"]
---

Logtalk는 선언적 프로그래밍 기능을 손상시키지 않으면서 최신 코드 캡슐화 및 코드 재사용 메커니즘으로 Prolog를 확장하고 활용하는 객체 지향 논리 프로그래밍 언어입니다. Logtalk는 이식성이 뛰어난 코드로 구현되었으며 대부분의 최신 표준 준수 Prolog 구현을 백엔드 컴파일러로 사용할 수 있습니다.

이 튜토리얼은 적절한 크기를 유지하기 위해 독자가 Prolog에 대한 실무 지식을 가지고 있으며 Logtalk 객체 지향 기능을 설명하는 데 편향되어 있다고 가정합니다.

# 구문

Logtalk는 원활한 학습 곡선과 넓은 이식성을 위해 몇 가지 연산자와 지시어를 추가한 표준 Prolog 구문을 사용합니다. 한 가지 중요한 결과는 Prolog 코드를 거의 또는 전혀 변경하지 않고 객체에 쉽게 캡슐화할 수 있다는 것입니다. 또한 Logtalk는 대부분의 Prolog 모듈을 Logtalk 객체로 투명하게 해석할 수 있습니다.

주요 연산자는 다음과 같습니다:

* `::/2` - 객체에 메시지 보내기
* `::/1` - _self_에 메시지 보내기 (즉, 처리 중인 메시지를 받은 객체에)
* `^^/1` - _super_ 호출 (상속되거나 가져온 술어의)

가장 중요한 엔티티 및 술어 지시어 중 일부는 다음 섹션에서 소개됩니다.

# 엔티티와 역할

Logtalk는 _객체_, _프로토콜_, _카테고리_를 일급 엔티티로 제공합니다. 엔티티 간의 관계는 _코드 재사용 패턴_과 엔티티가 수행하는 _역할_을 정의합니다. 예를 들어, 한 객체가 다른 객체를 _인스턴스화_할 때 첫 번째 객체는 인스턴스 역할을 하고 두 번째 객체는 클래스 역할을 합니다. 두 객체 간의 _확장_ 관계는 두 객체 모두 프로토타입 역할을 하며, 그중 하나가 다른 하나, 즉 부모 프로토타입을 확장함을 의미합니다.

# 객체 정의하기

객체는 술어 선언과 정의를 캡슐화합니다. 객체는 동적으로 생성될 수 있지만 일반적으로 정적이며 소스 파일에 정의됩니다. 단일 소스 파일에는 여러 엔티티 정의가 포함될 수 있습니다. 리스트 멤버 공개 술어를 정의하는 간단한 객체:

```logtalk
:- object(list).

	:- public(member/2).
	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.
```

# 소스 파일 컴파일 및 로드

위의 `list` 객체 코드가 `list.lgt` 파일에 저장되어 있다고 가정하면, `logtalk_load/1` 내장 술어 또는 그 약어인 `{}/1`을 사용하여 파일 경로를 인수로 전달하여 컴파일하고 로드할 수 있습니다(확장자는 생략 가능).

```logtalk
?- {list}.
yes
```

일반적으로 엔티티는 다른 소스 파일(예: 라이브러리 엔티티)에 정의된 엔티티에 대한 종속성을 가질 수 있습니다. 파일과 모든 종속성을 로드하려면, 애플리케이션에 필요한 모든 파일을 로드하는 _로더_ 파일을 정의하는 것이 좋습니다. 로더 파일은 일반적으로 `loader.lgt`라는 이름의 소스 파일이며, 이식성 및 표준 준수를 위해 일반적으로 `initialization/1` 지시어에서 `logtalk_load/1-2` 내장 술어를 호출합니다. 로더 파일은 모든 라이브러리, 도구 및 예제에 제공됩니다.

# 객체에 메시지 보내기

`::/2` 중위 연산자는 객체에 메시지를 보내는 데 사용됩니다. Prolog에서와 같이 대체 솔루션을 위해 백트랙할 수 있습니다:

```logtalk
?- list::member(X, [1,2,3]).
X = 1 ;
X = 2 ;
X = 3
yes
```

캡슐화가 적용됩니다. 술어는 _public_, _protected_ 또는 _private_으로 선언될 수 있습니다. 범위 지시어가 없는 경우 _local_일 수도 있습니다. 예:

```logtalk
:- object(scopes).

	:- private(bar/0).
	bar.

	local.

:- end_object.
```

객체가 `scopes.lgt` 파일에 저장되어 있다고 가정합니다:

```logtalk
?- {scopes}.
yes

?- catch(scopes::bar, Error, true).
Error = error(
	permission_error(access, private_predicate, bar/0),
	logtalk(scopes::bar, user)
)
yes

?- catch(scopes::local, Error, true).
Error = error(
	existence_error(predicate_declaration, local/0),
	logtalk(scopes::local, user)
)
yes
```

메시지의 술어가 객체에 대해 알려지지 않은 경우(객체가 수행하는 역할이 조회 절차를 결정함)에도 오류가 발생합니다. 예:

```logtalk
?- catch(scopes::unknown, Error, true).
Error = error(
	existence_error(predicate_declaration, unknown/0),
	logtalk(scopes::unknown, user)
)
yes
```

미묘한 점은 술어 범위 지시어는 술어 _호출_ 의미를 지정하며 _정의_ 의미가 아니라는 것입니다. 예를 들어, 클래스 역할을 하는 객체가 술어를 private으로 선언하면, 해당 술어는 서브클래스와 인스턴스에서 정의될 수 있지만, 해당 인스턴스에서는 클래스 _에서만_ 호출할 수 있습니다.

# 프로토콜 정의 및 구현

프로토콜에는 여러 객체 및 카테고리에서 구현할 수 있는 술어 선언이 포함됩니다:

```logtalk
:- protocol(listp).

	:- public(member/2).

:- end_protocol.

:- object(list,
	implements(listp)).

	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.
```

프로토콜 술어의 범위는 protected 또는 private 구현을 사용하여 제한할 수 있습니다. 예:

```logtalk
:- object(stack,
	implements(private::listp)).

:- end_object.
```

실제로 모든 엔티티 관계(엔티티 열기 지시어에서)는 public(기본값), protected 또는 private으로 한정될 수 있습니다.

# 프로토타입

다른 객체와 _인스턴스화_ 또는 _특수화_ 관계가 없는 객체는 프로토타입 역할을 합니다. 프로토타입은 다른 객체, 즉 부모 프로토타입을 _확장_할 수 있습니다.

```logtalk
% 우리의 프로토타입 코끼리, clyde
:- object(clyde).

	:- public(color/1).
	color(grey).

	:- public(number_of_legs/1).
	number_of_legs(4).

:- end_object.

% 또 다른 코끼리 fred는 clyde와 같지만 흰색입니다.
:- object(fred,
	extends(clyde)).

	color(white).

:- end_object.
```

프로토타입 역할을 하는 객체로 전송된 메시지에 응답할 때, 우리는 메시지를 확인하고 먼저 프로토타입 자체에서 답을 찾고, 찾지 못하면 부모 프로토타입에 위임합니다:

```logtalk
?- fred::number_of_legs(N).
N = 4
yes

?- fred::color(C).
C = white
yes
```

해당 술어가 선언되고(송신자가 범위 내에 있는 경우) 메시지가 유효하지만, 술어가 정의되지 않은 경우 오류를 발생시키는 대신 실패합니다. 이것을 _폐쇄 세계 가정_이라고 합니다. 예를 들어, `foo.lgt` 파일에 저장된 다음 객체를 고려하십시오:

```logtalk
:- object(foo).

	:- public(bar/0).

:- end_object.
```

파일을 로드하고 `bar/0` 술어를 호출하려고 하면 예상대로 실패합니다. 이것은 _알려지지 않은_ 술어를 호출하는 것과는 다르며, 오류가 발생합니다:

```logtalk
?- {foo}.
yes

?- foo::bar.
no

?- catch(foo::baz, Error, true).
Error = error(
	existence_error(predicate_declaration, baz/0),
	logtalk(foo::baz, user)
)
yes
```

# 클래스와 인스턴스

클래스 및/또는 인스턴스 역할을 하는 객체를 정의하려면, 객체는 다른 객체와 최소한 하나의 인스턴스화 또는 특수화 관계를 가져야 합니다. 클래스를 인스턴스로도 봐야 할 때 메타클래스 역할을 하는 객체를 사용할 수 있습니다. 다음 예제를 사용하여 런타임에 새 객체를 동적으로 생성하는 방법을 설명합니다:

```logtalk
% 인스턴스에 대한 new/2 술어를 정의하는 간단하고 일반적인 메타클래스
:- object(metaclass,
	instantiates(metaclass)).

	:- public(new/2).
	new(Instance, Clauses) :-
		self(Class),
		create_object(Instance, [instantiates(Class)], [], Clauses).

:- end_object.

% 인스턴스에 대한 age/1 및 name/1 술어를 정의하는 간단한 클래스
:- object(person,
	instantiates(metaclass)).

	:- public([
		age/1, name/1
	]).

	% age/1의 기본값
	age(42).

:- end_object.

% person 클래스의 정적 인스턴스
:- object(john,
	instantiates(person)).

	name(john).
	age(12).

:- end_object.
```

인스턴스 역할을 하는 객체로 전송된 메시지에 응답할 때, 클래스에서 시작하여 필요한 경우 클래스 슈퍼클래스까지 올라가서 메시지를 확인합니다. 메시지가 유효하다고 가정하면, 인스턴스 자체에서 시작하여 답을 찾습니다:

```logtalk
?- person::new(Instance, [name(paulo)]).
Instance = o1
yes

?- o1::name(Name).
Name = paulo
yes

?- o1::age(Age).
Age = 42
yes

?- john::age(Age).
Age = 12
yes
```

# 카테고리

카테고리는 모든 객체로 가져올 수 있는 _단일_ 기능을 구현하는 _응집력 있는_ 술어 선언 및 정의 집합을 캡슐화하는 데 사용되는 세분화된 코드 재사용 단위입니다. 따라서 카테고리는 프로토콜의 이중 개념으로 볼 수 있습니다. 다음 예에서는 자동차 엔진을 나타내는 카테고리를 정의한 다음 자동차 객체로 가져옵니다:

```logtalk
% 엔진 특성을 설명하는 프로토콜
:- protocol(carenginep).

	:- public([
		reference/1,
		capacity/1,
		cylinders/1,
		horsepower_rpm/2,
		bore_stroke/2,
		fuel/1
	]).

:- end_protocol.

% 카테고리로 정의된 일반적인 엔진
:- category(classic,
	implements(carenginep)).

	reference('M180.940').
	capacity(2195).
	cylinders(6).
	horsepower_rpm(94, 4800).
	bore_stroke(80, 72.8).
	fuel(gasoline).

:- end_category.

% 이전 엔진의 개조 버전
:- category(sport,
	extends(classic)).

	reference('M180.941').
	horsepower_rpm(HP, RPM) :-
		^^horsepower_rpm(ClassicHP, ClassicRPM),	% "super" 호출
		HP is truncate(ClassicHP*1.23),
		RPM is truncate(ClassicRPM*0.762).

:- end_category.

% 엔진(및 기타 구성 요소)으로 일부 자동차 "조립"을 시작할 수 있습니다.
:- object(sedan,
	imports(classic)).

:- end_object.

:- object(coupe,
	imports(sport)).

:- end_object.
```

카테고리는 독립적으로 컴파일되므로 객체 재컴파일 없이 가져온 카테고리를 간단히 업데이트하여 가져오는 객체를 업데이트할 수 있습니다. 카테고리는 또한 _런타임 투명성_을 제공합니다. 즉, 카테고리 프로토콜은 카테고리를 가져오는 객체의 프로토콜에 추가됩니다:

```logtalk
?- sedan::current_predicate(Predicate).
Predicate = reference/1 ;
Predicate = capacity/1 ;
Predicate = cylinders/1 ;
Predicate = horsepower_rpm/2 ;
Predicate = bore_stroke/2 ;
Predicate = fuel/1
yes
```

# 핫 패치

카테고리는 객체를 핫 패치하는 데에도 사용할 수 있습니다. 카테고리는 객체에 새 술어를 추가하거나 객체 술어 정의를 대체할 수 있습니다. 예를 들어, 다음 객체를 고려하십시오:

```logtalk
:- object(buggy).

	:- public(p/0).
	p :- write(foo).

:- end_object.
```

객체가 `p/0` 메시지를 받았을 때 잘못된 문자열을 인쇄한다고 가정합니다:

```logtalk
?- {buggy}.
yes

?- buggy::p.
foo
yes
```

객체 소스 코드를 사용할 수 없고 객체 코드를 실행하는 애플리케이션을 수정해야 하는 경우, 버그가 있는 술어를 수정하는 카테고리를 간단히 정의할 수 있습니다:

```logtalk
:- category(patch,
	complements(buggy)).

	% 수정된 p/0 정의
	p :- write(bar).

:- end_category.
```

실행 중인 애플리케이션에 카테고리를 컴파일하고 로드한 후에는 다음을 얻게 됩니다:

```logtalk
?- set_logtalk_flag(complements, allow).
yes

?- {patch}.
yes

?- buggy::p.
bar
yes
```

핫 패치는 캡슐화를 강제로 깨뜨리므로, `complements` 컴파일러 플래그를 (전역적으로 또는 객체별로) 설정하여 허용, 제한 또는 방지할 수 있습니다.

# 매개변수 객체 및 카테고리

객체 및 카테고리는 식별자로 원자 대신 복합 항을 사용하여 매개변수화할 수 있습니다. 객체 및 카테고리 매개변수는 모든 캡슐화된 술어와 공유되는 _논리 변수_입니다. 기하학적 원에 대한 예:

```logtalk
:- object(circle(_Radius, _Color)).

	:- public([
		area/1, perimeter/1
	]).

	area(Area) :-
		parameter(1, Radius),
		Area is pi*Radius*Radius.

	perimeter(Perimeter) :-
		parameter(1, Radius),
		Perimeter is 2*pi*Radius.

:- end_object.
```

매개변수 객체는 다른 객체와 마찬가지로 사용되며, 일반적으로 메시지를 보낼 때 매개변수 값을 제공합니다:

```logtalk
?- circle(1.23, blue)::area(Area).
Area = 4.75291
yes
```

매개변수 객체는 또한 술어 집합을 일반 Prolog 술어와 연결하는 간단한 방법을 제공합니다. Prolog 사실은 매개변수 객체의 식별자와 동일한 함자 및 인수를 가질 때 _매개변수 객체 프록시_로 해석될 수 있습니다. 프록시 작업을 위한 편리한 구문이 제공됩니다. 예를 들어, `circle/2` 술어에 대한 다음 절을 가정합니다:

```logtalk
circle(1.23, blue).
circle(3.71, yellow).
circle(0.39, green).
circle(5.74, black).
circle(8.32, cyan).
```

이러한 절이 로드되면, 모든 원의 면적 목록을 쉽게 계산할 수 있습니다:

```logtalk
?- findall(Area, {circle(_, _)}::area(Area), Areas).
Areas = [4.75291, 43.2412, 0.477836, 103.508, 217.468]
yes
```

`{Goal}::Message` 구문은 `Goal`을 증명하고(아마도 그 안의 변수를 인스턴스화함), 결과 항에 `Message`를 보냅니다.

# 이벤트와 모니터

Logtalk는 이벤트와 해당 이벤트에 대한 모니터를 정의하여 _이벤트 기반 프로그래밍_을 지원합니다. 이벤트는 단순히 객체에 메시지를 보내는 것입니다. 메시지 보내기를 원자적 활동으로 해석하면 _before_ 이벤트와 _after_ 이벤트가 인식됩니다. 이벤트 모니터는 이벤트 핸들러 술어 `before/3` 및 `after/3`을 정의하고, 이벤트를 모니터와 연결하는 시스템 전체 이벤트 레지스트리를 쿼리, 등록 및 삭제할 수 있습니다. 예를 들어, `::/2` 제어 구문을 사용하여 전송되는 모든 메시지에 대한 간단한 추적기는 다음과 같이 정의할 수 있습니다:

```logtalk
:- object(tracer,
	implements(monitoring)).    % 이벤트 핸들러를 위한 내장 프로토콜

	:- initialization(define_events(_, _, _, _, tracer)).

	before(Object, Message, Sender) :-
		write('call: '), writeq(Object), write(' <-- '), writeq(Message),
		write(' from '), writeq(Sender), nl.

	after(Object, Message, Sender) :-
		write('exit: '), writeq(Object), write(' <-- '), writeq(Message),
		write(' from '), writeq(Sender), nl.

:- end_object.
```

앞서 정의한 `tracer` 객체와 `list` 객체가 컴파일되고 로드되었다고 가정하면, 메시지를 보내 이벤트 핸들러의 동작을 관찰할 수 있습니다:

```logtalk
?- set_logtalk_flag(events, allow).
yes

?- list::member(X, [1,2,3]).

call: list <-- member(X, [1,2,3]) from user
exit: list <-- member(1, [1,2,3]) from user
X = 1 ;
exit: list <-- member(2, [1,2,3]) from user
X = 2 ;
exit: list <-- member(3, [1,2,3]) from user
X = 3
yes
```

`define_events/5` 및 `abolish_events/5` 내장 술어를 호출하여 런타임에 이벤트를 동적으로 설정하고 삭제할 수 있습니다.

이벤트 기반 프로그래밍은 _계산적 리플렉션_의 한 형태로 볼 수 있습니다. 그러나 이벤트는 `::/2` 메시지 전송 제어 구문을 사용할 때만 생성된다는 점에 유의하십시오.

# 람다 표현식

Logtalk는 람다 표현식을 지원합니다. 람다 매개변수는 `(>>)/2` 중위 연산자를 사용하여 람다에 연결된 목록을 사용하여 표현됩니다. 라이브러리 `meta`를 사용한 몇 가지 간단한 예:

```logtalk
?- {meta(loader)}.
yes

?- meta::map([X,Y]>>(Y is 2*X), [1,2,3], Ys).
Ys = [2,4,6]
yes
```

커링도 지원됩니다:

```logtalk
?- meta::map([X]>>([Y]>>(Y is 2*X)), [1,2,3], Ys).
Ys = [2,4,6]
yes
```

람다 자유 변수는 확장된 구문 `{Free1, ...}/[Parameter1, ...]>>Lambda`를 사용하여 표현할 수 있습니다.

# 매크로

소스 파일의 항과 목표는 텀 확장 및 목표 확장 규칙을 정의하는 _후크 객체_를 지정하여 컴파일 타임에 _확장_될 수 있습니다. 예를 들어, `source.lgt` 파일에 저장된 다음 간단한 객체를 고려하십시오:

```logtalk
:- object(source).

	:- public(bar/1).
	bar(X) :- foo(X).

	foo(a). foo(b). foo(c).

:- end_object.
```

`foo/1` 로컬 술어에 대한 절과 호출을 확장하는 `my_macros.lgt` 파일에 저장된 다음 후크 객체를 가정합니다:

```logtalk
:- object(my_macros,
	implements(expanding)).    % 술어 확장을 위한 내장 프로토콜

	term_expansion(foo(Char), baz(Code)) :-
		char_code(Char, Code). % 표준 내장 술어

	goal_expansion(foo(X), baz(X)).

:- end_object.
```

매크로 파일을 로드한 후, `hook` 컴파일러 플래그를 사용하여 소스 파일을 확장할 수 있습니다:

```logtalk
?- logtalk_load(my_macros), logtalk_load(source, [hook(my_macros)]).
yes

?- source::bar(X).
X = 97 ;
X = 98 ;
X = 99
true
```

Logtalk 라이브러리는 다른 워크플로를 사용하여 후크 객체를 결합하는 지원을 제공합니다(예: 확장 파이프라인 정의).

# 추가 정보

자세한 내용은 [Logtalk 웹사이트](http://logtalk.org)를 방문하십시오.
