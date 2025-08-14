# tailspin.md (번역)

---
name: Tailspin
filename: learntailspin.tt
contributors:
    - ["Torbjörn Gannholm", "https://github.com/tobega/"]

---

**Tailspin**은 파이프라인에서 값의 스트림으로 작동합니다. 종종
프로그램이 기계이고 입력 데이터가 프로그램이라고 느낄 수 있습니다.

Tailspin이 주류가 되거나 프로덕션 준비가 될 가능성은 낮지만,
좋은 방향으로 프로그래밍에 대한 생각을 바꿀 것입니다.

```c
// 줄 끝까지 주석

// ->로 구분된 단계가 있는 파이프라인에서 데이터 처리
// 문자열 리터럴은 작은따옴표로 구분됩니다.
// 느낌표(!)는 싱크 또는 파이프의 끝을 나타냅니다.
// OUT은 표준 출력 객체이고, ::write는 출력을 쓰는 메시지입니다.
'Hello, World!' -> !OUT::write

// 문자열에 입력하여 줄 바꿈 출력 (여러 줄 문자열)
'
' -> !OUT::write
// 또는 $#와 ; 사이에 줄 바꿈(10)에 대한 10진수 유니코드 값 출력
'$#10;' -> !OUT::write

// 불변의 명명된 값 정의. 값 구문은 매우 리터럴합니다.
def names: ['Adam', 'George', 'Jenny', 'Lucy'];

// 각 이름을 처리하기 위해 목록 스트리밍. $를 사용하여 값을 가져오는 것에 유의하십시오.
// 파이프라인의 현재 값은 항상 $입니다.
// 문자열 보간은 $로 시작하여 ;로 끝납니다.
$names... -> 'Hello $;!
' -> !OUT::write

// 보간에 스트리밍하고 보간을 중첩할 수도 있습니다.
// 괄호를 사용한 목록 인덱싱 및 슬라이스 추출에 유의하십시오.
// ~를 사용하여 범위에 대한 배타적 경계를 나타내는 것에 유의하십시오.
// 'Hello Adam, George, Jenny and Lucy!'를 출력합니다.
'Hello $names(first);$names(first~..~last)... -> ', $;'; and $names(last);!
' -> !OUT::write

// 다른 사람에게 다른 말을 조건부로 합니다.
// 매처(조건식)는 꺾쇠괄호로 구분됩니다.
// 위에서 아래로 평가되는 매처 집합은 템플릿(함수)에 있어야 합니다.
// 여기서는 \(에서 \)로 구분된 인라인 템플릿입니다.
// 리터럴 ' 및 $를 얻기 위해 이중 '' 및 $$를 사용하는 것에 유의하십시오.
$names... -> \(
  when <='Adam'> do 'What''s up $;?' !
  when <='George'> do 'George, where are the $$10 you owe me?' !
  otherwise 'Hello $;!' !
\) -> '$;$#10;' -> !OUT::write

// 템플릿(함수)을 정의할 수도 있습니다.
// 단독 !는 제어를 반환하지 않고 호출 파이프라인으로 값을 내보냅니다.
// #은 매처에 의해 일치될 값을 보냅니다.
// 템플릿은 항상 하나의 입력 값을 받고 0개 이상의 출력을 내보냅니다.
templates collatz-sequence
  when <..0> do 'The start seed must be a positive integer' !
  when <=1> do $!
// ?(에서 )까지는 계산된 값을 일치시킬 수 있습니다. "and"로 연결할 수 있습니다.
  when <?($ mod 2 <=1>)> do
    $ !
    3 * $ + 1 -> #
  otherwise
    $ !
    $ ~/ 2 -> #
end collatz-sequence

// 한 줄에 공백으로 구분된 임의의 시작점에서 콜라츠 시퀀스
1000 -> SYS::randomInt -> $ + 1 -> collatz-sequence -> '$; ' -> !OUT::write
'
' -> !OUT::write

// 인덱싱된 목록 템플릿으로 한 줄에 10개씩 서식이 지정된 콜라츠 시퀀스
// 대괄호는 묶인 파이프라인 결과의 목록을 생성합니다.
// \[i](에서 \)까지는 목록의 각 값에 적용할 템플릿을 정의하며,
// i(또는 선택한 식별자)는 인덱스를 보유합니다.
[1000 -> SYS::randomInt -> $ + 1 -> collatz-sequence]
-> \[i](
  when <=1|?($i mod 10 <=0>)> do '$;$#10;' !
  otherwise '$; ' !
\)... -> !OUT::write

// 범위에는 선택적 보폭이 있을 수 있습니다.
def odd-numbers: [1..100:2];

// 로컬에서 가변 상태 사용. 템플릿당 하나의 변수, 항상 @라고 함
templates product
  @: $(first);
  $(first~..last)... -> @: $@ * $;
  $@ !
end product

$odd-numbers(6..8) -> product -> !OUT::write
'
' -> !OUT::write

// 프로세서 객체를 사용하여 가변 상태 유지.
// 외부 @는 내부 컨텍스트에서 이름으로 참조해야 합니다.
// 싱크 템플릿은 출력이 없으며 접두사 !로 호출됩니다.
// 소스 템플릿은 입력이 없으며 접두사 $로 호출됩니다.
processor Product
  @: 1;
  sink accumulate
    @Product: $@Product * $;
  end accumulate
  source result
    $@Product !
  end result
end Product

// 프로세서는 생성자 템플릿입니다. 이것은 $(입력 없음)으로 호출됩니다.
def multiplier: $Product;

// ::로 메시지를 보내 객체 템플릿 호출
1..7 -> !multiplier::accumulate
-1 -> !multiplier::accumulate
$multiplier::result -> 'The product is $;
' -> !OUT::write

// 수집기 인터페이스를 구현하는 프로세서에 대한 구문 설탕
1..7 -> ..=Product -> 'The collected product is $;$#10;' -> !OUT::write

// 유한한 값 집합에 대해 심볼 집합(본질적으로 열거형)을 정의할 수 있습니다.
data colour #{green, red, blue, yellow}

// 프로세서 타입 상태를 사용하여 상태를 깔끔하게 모델링합니다.
// 마지막으로 명명된 가변 상태 값 집합이 타입 상태를 결정합니다.
processor Lamp
  def colours: $;
  @Off: 0;
  state Off
    source switchOn
      @On: $@Off mod $colours::length + 1;
      'Shining a $colours($@On); light$#10;' !
    end switchOn
  end Off
  state On
    source turnOff
      @Off: $@On;
      'Lamp is off$#10;' !
    end turnOff
  end On
end Lamp

def myLamp: [colour#green, colour#blue] -> Lamp;

$myLamp::switchOn -> !OUT::write // Shining a green light
$myLamp::turnOff -> !OUT::write  // Lamp is off
$myLamp::switchOn -> !OUT::write // Shining a blue light
$myLamp::turnOff -> !OUT::write  // Lamp is off
$myLamp::switchOn -> !OUT::write // Shining a green light

// 정규식을 사용하여 문자열 테스트
['banana', 'apple', 'pear', 'cherry']... -> \(
  when <'.*a.*'> do '$; contains an ''a''' !
  otherwise '$; has no ''a''' !
\) -> '$;
' -> !OUT::write

// 정규식과 정의된 규칙으로 작곡가를 사용하여 문자열 구문 분석
composer parse-stock-line
  {inventory-id: <INT> (<WS>), name: <'\w+'> (<WS>), currency: <'.{3}'>,
    unit-price: <INT> (<WS>?) <parts>?}
  rule parts: associated-parts: [<part>+]
  rule part: <'[A-Z]\d+'> (<=','>?)
end parse-stock-line

'705 gizmo EUR5 A67,G456,B32' -> parse-stock-line -> !OUT::write
// {associated-parts: [A67, G456, B32], currency: EUR,
//     inventory-id: 705, name: gizmo, unit-price: 5}
'
' -> !OUT::write

// 문자열을 스트리밍하여 글리프로 분할합니다.
// 목록은 인덱스 배열로 인덱싱/슬라이스할 수 있습니다.
// ['h','e','l','l','o']를 출력하고, 배열/목록 인덱싱은 1부터 시작합니다.
['abcdefghijklmnopqrstuvwxyz'...] -> $([8,5,12,12,15]) -> !OUT::write
'
' -> !OUT::write

// 위에서는 원시 문자열만 사용했습니다.
// 문자열은 태그로 결정되는 다른 타입을 가질 수 있습니다.
// 더 넓은 타입 경계가 설정되지 않은 한 다른 타입을 비교하는 것은 오류입니다.
// 타입 경계는 ´´로 주어지며 ''는 태그가 있거나 원시인 모든 문자열 값을 의미합니다.
templates get-string-type
  when <´''´ '.*'> do '$; is a raw string' !
  when <´''´ id´'\d+'> do '$; is a numeric id string' !
  when <´''´ =id´'foo'> do 'id foo found' !
  when <´''´ id´'.*'> do '$; is an id' !
  when <´''´ name´'.+'> do '$; is a name' !
  otherwise '$; is not a name or id, nor a raw string' !
end get-string-type

[name´'Anna', 'foo', id´'789', city´'London', id´'xzgh', id´'foo']...
-> get-string-type -> '$;
' -> !OUT::write

// 숫자는 원시, 태그 또는 측정 단위를 가질 수 있습니다.
// 타입 ..은 태그, 측정 또는 원시인 모든 숫자 값입니다.
templates get-number-type
  when <´..´ =inventory-id´86> do 'inventory-id 86 found' !
  when <´..´ inventory-id´100..> do '$; is an inventory-id >= 100' !
  when <´..´ inventory-id´0..|..inventory-id´0> do '$; is an inventory-id' !
  when <´..´ 0"m"..> do '$; is an m-measure >= 0"m"' !
  when <´..´ ..0|0..> do '$; is a raw number' !
  otherwise '$; is not a positive m-measure nor an inventory-id, nor raw' !
end get-number-type

[inventory-id´86, inventory-id´6, 78"m", 5"s", 99, inventory-id´654]...
-> get-number-type -> '$;
' -> !OUT::write

// 측정 단위는 산술에 사용할 수 있으며, "1"은 스칼라 단위입니다.
// 측정 단위를 혼합할 때는 결과 측정 단위로 캐스팅해야 합니다.
4"m" + 6"m" * 3"1" -> ($ ~/ 2"s")"m/s" -> '$;
' -> !OUT::write

// 태그가 있는 식별자는 산술에 사용될 때 원시 숫자로 만들어야 합니다.
// 그런 다음 원하는 경우 결과를 태그가 있는 식별자로 다시 캐스팅할 수 있습니다.
inventory-id´300 -> inventory-id´($::raw + 1) -> get-number-type -> '$;
' -> !OUT::write

// 필드는 기본적으로 원시 문자열 또는 숫자를 태그하여 자동으로 타입을 지정합니다.
// 필드에 잘못된 타입을 할당할 수 없습니다.
def item: { inventory-id: 23, name: 'thingy', length: 12"m" };

'Field inventory-id $item.inventory-id -> get-number-type;
' -> !OUT::write
'Field name $item.name -> get-string-type;
' -> !OUT::write
'Field length $item.length -> get-number-type;
' -> !OUT::write

// 타입을 정의하고 타입 테스트로 사용할 수 있습니다. 이것은 또한 필드를 정의합니다.
// 표준 플레이트 필드에 비표준 플레이트를 할당하는 것은 오류입니다.
data standard-plate <'[A-Z]{3}[0-9]{3}'>

[['Audi', 'XYZ345'], ['BMW', 'I O U']]... -> \(
  when <?($(2) <standard-plate>)> do {make: $(1), standard-plate: $(2)}!
  otherwise {make: $(1), vanity-plate: $(2)}!
\) -> '$;
' -> !OUT::write

// 유니온 타입을 정의할 수 있습니다.
data age <"years"|"months">

[ {name: 'Cesar', age: 20"years"},
  {name: 'Francesca', age: 19"years"},
  {name: 'Bobby', age: 11"months"}]...
-> \(
// 구조에 대한 조건부 테스트는 필드 테스트와 함께 리터럴처럼 보입니다.
  when <{age: <13"years"..19"years">}> do '$.name; is a teenager'!
  when <{age: <"months">}> do '$.name; is a baby'!
// 모든 경우를 처리할 필요는 없습니다. 'Cesar'는 그냥 무시됩니다.
\) -> '$;
' -> !OUT::write

// 배열/목록 인덱스는 기본적으로 1부터 시작하지만 선택할 수 있습니다.
// 슬라이스는 실제 배열과 겹치는 부분을 반환합니다.
[1..5] -> $(-2..2) -> '$;
' -> !OUT::write // [1,2] 출력
0:[1..5] -> $(-2..2) -> '$;
' -> !OUT::write // [1,2,3] 출력
-2:[1..5] -> $(-2..2) -> '$;
' -> !OUT::write // [1,2,3,4,5] 출력

// 배열은 측정 단위 또는 태그가 있는 식별자의 인덱스를 가질 수 있습니다.
def game-map: 0"y":[
  1..5 -> 0"x":[
    1..5 -> level´1:[
      1..3 -> {
        level: $,
        terrain-id: 6 -> SYS::randomInt,
        altitude: (10 -> SYS::randomInt)"m"
      }
    ]
  ]
];

// 프로젝션(인덱싱)은 여러 차원에 걸쳐 있을 수 있습니다.
$game-map(3"y"; 1"x"..3"x"; level´1; altitude:) -> '$;
' -> !OUT::write // 세 개의 고도 값 목록을 제공합니다.

// 통계를 얻기 위해 평탄화하고 그룹화 프로젝션을 수행합니다.
// Count 및 Max는 내장된 수집기 프로세서입니다.
[$game-map... ... ...] -> $(collect {
      occurences: Count,
      highest-on-level: Max&{by: :(altitude:), select: :(level:)}
    } by $({terrain-id:}))
-> !OUT::write
'
' -> !OUT::write

// 관계는 구조/레코드의 집합입니다.
// 여기서는 모든 고유한 {level:, terrain-id:, altitude:} 조합을 얻습니다.
def location-types: {|$game-map... ... ...|};

// 프로젝션은 구조를 다시 매핑할 수 있습니다. §는 상대 접근자입니다.
$location-types({terrain-id:, foo: §.level::raw * §.altitude})
-> '$;
' -> !OUT::write

// 관계형 대수 연산자는 관계에 사용할 수 있습니다.
($location-types join {| {altitude: 3"m"} |})
-> !OUT::write
'
' -> !OUT::write

// 이항 연산에 대한 자체 연산자 정의
operator (left dot right)
  $left -> \[i]($ * $right($i)!\)... -> ..=Sum&{of: :()} !
end dot

([1,2,3] dot [2,5,8]) -> 'dot product: $;
' -> !OUT::write

// 템플릿 동작을 변경하기 위해 매개변수 제공
templates die-rolls&{sides:}
  1..$ -> $sides::raw -> SYS::randomInt -> $ + 1 !
end die-rolls

[5 -> die-rolls&{sides: 4}] -> '$;
' -> !OUT::write

// 템플릿을 매개변수로 전달하고, 일부 매개변수를 미리 채울 수 있습니다.
source damage-roll&{first:, second:, third:}
  (1 -> first) + (1 -> second) + (1 -> third) !
end damage-roll

$damage-roll&{first: die-rolls&{sides:4},
  second: die-rolls&{sides:6}, third: die-rolls&{sides:20}}
-> 'Damage done is $;
' -> !OUT::write

// 인라인으로 테스트 작성. 명령줄에서 --test 플래그로 실행
// 매처의 ~는 "not"을 의미하며,
// 배열 내용 매처는 < 1 및 > 4인 요소를 일치시킵니다.
test 'die-rolls'
  assert [100 -> die-rolls&{sides: 4}] <~[<..~1|4~..>]> 'all rolls 1..4'
end 'die-rolls'

// 테스트에 수정된 모듈 제공 (테스트 더블 또는 모의 객체라고도 함)
// IN은 표준 입력 객체이고 ::lines는 모든 줄을 가져옵니다.
source read-numbers
  $IN::lines -> #
  when <'\d+'> do $!
end read-numbers

test 'read numbers from input'
  use shadowed core-system/
    processor MockIn
      source lines
        [
          '12a',
          '65',
          'abc'
        ]... !
      end lines
    end MockIn
    def IN: $MockIn;
  end core-system/
  assert $read-numbers <=65> 'Only 65 is read'
end 'read numbers from input'

// 바이트 배열로 작업할 수 있습니다.
composer hexToBytes
  <HEX>
end hexToBytes

'1a5c678d' -> hexToBytes -> ($ and [x 07 x]) -> $(last-1..last) -> '$;
' -> !OUT::write // 0005 출력
```

## 더 읽을거리

- [Tailspin 메인 사이트](https://github.com/tobega/tailspin-v0/)
- [Tailspin 언어 참조](https://github.com/tobega/tailspin-v0/blob/master/TailspinReference.md)
