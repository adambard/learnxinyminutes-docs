---
name: Raku
filename: learnraku.raku
contributors:
    - ["vendethiel", "http://github.com/vendethiel"]
    - ["Samantha McVey", "https://cry.nu"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Raku (이전 Perl 6)는 최소한 향후 100년을 위해 만들어진 매우 유능하고 기능이 풍부한 프로그래밍 언어입니다.

주요 Raku 컴파일러는 [Rakudo](http://rakudo.org)라고 불리며, JVM 및 [MoarVM](http://moarvm.com)에서 실행됩니다.

메타 노트:

* 파운드 기호(`#`)는 문장과 노트에 사용되지만, Pod 스타일 주석(아래에서 자세히 설명)은 편리할 때마다 사용됩니다.
* `# OUTPUT:`은 모든 표준 스트림에 대한 명령의 출력을 나타내는 데 사용됩니다. 출력에 줄 바꿈이 있으면 `␤` 기호로 표시됩니다. 출력은 항상 꺾쇠 괄호(`«` 및 `»`)로 묶입니다.
* `#=>`는 표현식의 값, 서브루틴의 반환 값 등을 나타냅니다. 경우에 따라 값은 주석과 함께 제공됩니다.
* 백틱은 언어 구문을 텍스트와 구별하고 강조하는 데 사용됩니다.

```perl6
####################################################
# 0. 주석
####################################################

# 한 줄 주석은 파운드 기호로 시작합니다.

#`( 여러 줄 주석은 #`와 인용 구문을 사용합니다.
  (), [], {}, 「」 등은 작동합니다.
)

=for comment
주석을 포함하기 위해 여러 줄 주석과 동일한 구문을 사용하십시오.
for #`(각 요소에 대해) @array {
    put #`(또는 요소 인쇄) $_ #`(줄 바꿈 포함);
}

# Pod 스타일 주석도 사용할 수 있습니다. 예를 들어:

=comment 알고리즘에 대해 여기에 더 추가하십시오.

=comment Pod 주석은 문서의 문서를 작성하는 데 좋습니다.

=begin comment
이 주석은 여러 줄입니다.

빈 줄도 여기에 있을 수 있습니다!
=end comment

####################################################
# 1. 변수
####################################################

# Raku에서 `my` 키워드를 사용하여 어휘 변수를 선언합니다:
my $variable;

# Raku에는 스칼라, 배열, 해시의 세 가지 기본 변수 유형이 있습니다.

#
# 1.1 스칼라
#

# 스칼라는 단일 값을 나타냅니다. `$` 시길로 시작합니다:
my $str = 'String';

# 큰따옴표는 보간을 허용합니다(나중에 설명):
my $str2 = "$str";

# 변수 이름은 작은따옴표와 대시를 포함할 수 있지만 끝날 수는 없으며,
# 밑줄을 포함할 수 있습니다(그리고 끝날 수 있습니다):
my $person's-belongings = 'towel'; # 이것은 작동합니다!

my $bool = True;             # `True`와 `False`는 Raku의 부울 값입니다.
my $inverse = !$bool;        # 접두사 `!` 연산자로 부울을 반전합니다.
my $forced-bool = so $str;   # 그리고 접두사 `so` 연산자를 사용할 수 있습니다.
$forced-bool = ?$str;   # 또는 `?`를 사용하여 피연산자를 부울로 변환합니다.

#
# 1.2 배열 및 목록
#

# 배열은 여러 값을 나타냅니다. 배열 변수는 `@` 시길로 시작합니다. 목록과 달리 배열은 변경 가능합니다.

my @array = 'a', 'b', 'c';
# 다음과 동일합니다:
my @letters = <a b c>;
# 이전 문에서 공백으로 구분된 단어 배열에 대해 quote-words(`<>`) 용어를 사용합니다.
# Perl의 qw 또는 Ruby의 %w와 유사합니다.

@array = 1, 2, 4;

# 배열 인덱스는 0부터 시작합니다. 여기서는 세 번째 요소에 액세스합니다.
say @array[2]; # OUTPUT: «4␤»

say "배열을 보간하려면 []를 사용하십시오: @array[]";
# OUTPUT: «배열을 보간하려면 []: 1 2 3␤»

@array[0]    = -1;     # 배열 인덱스에 새 값 할당
@array[0, 1] = 5, 6;   # 여러 값 할당

my @keys = 0, 2;
@array[@keys] = @letters; # 인덱스 값을 포함하는 배열을 사용하여 할당
say @array;               # OUTPUT: «a 6 b␤»

#
# 1.3 해시 또는 키-값 쌍.
#

# 해시는 키와 값 쌍의 집합입니다. `key => value` 구문을 사용하여 `Pair` 객체를 구성할 수 있습니다.
# 해시 테이블은 조회 속도가 매우 빠르며, 정렬되지 않은 상태로 저장됩니다. 키는 해시 컨텍스트에서 "평탄화"되고 중복된 키는 중복 제거됩니다.
my %hash = 'a' => 1, 'b' => 2;

# fat comma(`=>`)를 사용하면 키가 자동으로 인용됩니다. 후행 쉼표는 괜찮습니다.
%hash = a => 1, b => 2, ;

# 해시가 배열과 다르게 내부적으로 저장되더라도,
# Raku는 짝수 배열에서 해시를 쉽게 만들 수 있도록 합니다:
my %hash = <key1 value1 key2 value2>;          # 또는:
my %hash = "key1", "value1", "key2", "value2";

%hash = key1 => 'value1', key2 => 'value2'; # 위와 동일한 결과

# "콜론 쌍" 구문도 사용할 수 있습니다. 이 구문은 특히 나중에 설명할 명명된 매개변수에 유용합니다.
%hash = :n(2),    # `n => 2`와 동일
        :is-even, # `:is-even(True)` 또는 `is-even => True`와 동일
        :!is-odd, # `:is-odd(False)` 또는 `is-odd => False`와 동일
;
# `:`(예: `:is-even`) 및 `:!`(예: `:!is-odd`) 구문은 각각 `True` 및 `False` 단축키로 알려져 있습니다.

# 아래 예제에서 보여주듯이, {}를 사용하여 키에서 값을 가져올 수 있습니다.
# 공백이 없는 문자열인 경우 실제로 quote-words 연산자(`<>`)를 사용할 수 있습니다.
# Perl과 달리 Raku는 bareword를 사용하지 않으므로 `{key1}`은 작동하지 않습니다.
say %hash{'n'};     # OUTPUT: «2␤», 키 'n'과 연결된 값을 가져옵니다.
say %hash<is-even>; # OUTPUT: «True␤», 키 'is-even'과 연결된 값을 가져옵니다.

####################################################
# 2. 서브루틴
####################################################

# 서브루틴 또는 대부분의 다른 언어에서 함수라고 부르는 것은 `sub` 키워드로 생성됩니다.
sub say-hello { say "Hello, world" }

# (타입이 지정된) 인수를 제공할 수 있습니다. 지정된 경우, 타입은 컴파일 타임에 확인됩니다(가능한 경우). 그렇지 않으면 런타임에 확인됩니다.
sub say-hello-to( Str $name ) {
    say "Hello, $name !";
}

# 서브루틴은 블록의 마지막 값을 반환합니다. 마찬가지로 마지막 표현식의 세미콜론은 생략할 수 있습니다.
sub return-value { 5 }
say return-value;      # OUTPUT: «5␤»

sub return-empty { }
say return-empty;      # OUTPUT: «Nil␤»

# 일부 제어 흐름 구조는 값을 생성합니다. 예를 들어 `if`:
sub return-if {
	if True { "Truthy" }
}
say return-if;         # OUTPUT: «Truthy␤»

# `for`와 같이 일부는 그렇지 않습니다:
sub return-for {
    for 1, 2, 3 { 'Hi' }
}
say return-for;        # OUTPUT: «Nil␤»

# 위치 인수는 기본적으로 필수입니다. 선택 사항으로 만들려면 매개변수 이름 뒤에 `?`를 사용하십시오.

# 다음 예제에서 `with-optional` 서브루틴은 인수가 전달되지 않으면 `(Any)`(Perl의 null과 유사한 값)를 반환합니다. 그렇지 않으면 인수를 반환합니다.
sub with-optional( $arg? ) {
    $arg;
}
with-optional;     # Any 반환
with-optional();   # Any 반환
with-optional(1);  # 1 반환

# 또한 전달되지 않을 때 기본값을 제공할 수 있습니다. 이렇게 하면 해당 매개변수가 선택 사항이 됩니다. 필수 매개변수는 선택 사항 매개변수보다 먼저 와야 합니다.

# `greeting` 서브루틴에서 `$type` 매개변수는 선택 사항입니다.
sub greeting( $name, $type = "Hello" ) {
  say "$type, $name!";
}

greeting("Althea");                 # OUTPUT: «Hello, Althea!␤»
greeting("Arthur", "Good morning"); # OUTPUT: «Good morning, Arthur!␤»

# 해시와 유사한 구문(통합 구문!)을 사용하여 명명된 매개변수를 선언하고 서브루틴에 명명된 인수를 전달할 수도 있습니다.
# 기본적으로 명명된 매개변수는 선택 사항이며 `Any`로 기본 설정됩니다.
sub with-named( $normal-arg, :$named ) {
	say $normal-arg + $named;
}
with-named(1, named => 6); # OUTPUT: «7␤»

# 여기에 주의해야 할 한 가지 함정이 있습니다: 키를 인용하면 Raku는 컴파일 타임에 키를 볼 수 없으며, 단일 `Pair` 객체가 위치 매개변수로 사용됩니다. 이는 함수 서브루틴 `with-named(1, 'named' => 6);`이 실패함을 의미합니다.
with-named(2, :named(5));  # OUTPUT: «7␤»

# 명명된 매개변수를 강제하려면 매개변수에 `!`를 추가할 수 있습니다.
# 이는 필수 매개변수를 선택 사항으로 만드는 `?`의 반대입니다.

sub with-mandatory-named( :$str! )  {
    say "$str!";
}
with-mandatory-named(str => "My String"); # OUTPUT: «My String!␤»
# with-mandatory-named;   # 런타임 오류: "필수 명명된 매개변수가 전달되지 않았습니다."
# with-mandatory-named(3);# 런타임 오류: "너무 많은 위치 매개변수가 전달되었습니다."

# 서브루틴이 명명된 부울 인수를 취하는 경우, 이전에 논의한 것과 동일한 "짧은 부울" 해시 구문을 사용할 수 있습니다.
sub takes-a-bool( $name, :$bool ) {
    say "$name takes $bool";
}
takes-a-bool('config', :bool);  # OUTPUT: «config takes True␤»
takes-a-bool('config', :!bool); # OUTPUT: «config takes False␤»

# 서브루틴을 호출할 때 괄호를 생략할 수 있으므로,
# 인수가 없는 서브루틴 호출과 코드 객체를 구별하려면 `&`를 사용해야 합니다.

# 예를 들어, 이 예제에서는 `&`를 사용하여 `say-hello` 서브루틴(즉, 서브루틴의 코드 객체)을 변수에 저장해야 합니다. 서브루틴 호출이 아닙니다.
my &s = &say-hello;
my &other-s = sub { say "Anonymous function!" }

# 서브루틴은 "slurpy" 매개변수를 가질 수 있습니다. 이는 "얼마나 많은지 상관없는" 매개변수라고 할 수 있습니다.
# 이를 위해 `*@`(slurpy)를 사용해야 합니다. 이는 "나머지 모든 것을 가져옵니다".
# slurpy 매개변수 *앞에는* 원하는 만큼의 매개변수를 가질 수 있지만, *뒤에는* 가질 수 없습니다.
sub as-many($head, *@rest) {
    @rest.join(' / ') ~ " !";
}
say as-many('Happy', 'Happy', 'Birthday');          # OUTPUT: «Happy / Birthday !␤»
say as-many('Happy', ['Happy', 'Birthday'], 'Day'); # OUTPUT: «Happy / Birthday / Day !␤»

# 스플랫(*)은 그 앞에 있는 매개변수를 소비하지 않았다는 점에 유의하십시오.

# Raku에는 두 가지 다른 slurpy 매개변수 변형이 있습니다. 이전 것(즉, `*@`), 평탄화된 slurpy로 알려져 있으며, 전달된 인수를 평탄화합니다. 다른 두 가지는 `**@` 및 `+@`로, 각각 평탄화되지 않은 slurpy 및 "단일 인수 규칙" slurpy로 알려져 있습니다. 평탄화되지 않은 slurpy는 목록 인수를 평탄화하지 않습니다(또는 Iterable 인수).
sub b(**@arr) { @arr.perl.say };
b(['a', 'b', 'c']);             # OUTPUT: «[["a", "b", "c"],]»
b(1, $('d', 'e', 'f'), [2, 3]); # OUTPUT: «[1, ("d", "e", "f"), [2, 3]]»
b(1, [1, 2], ([3, 4], 5));      # OUTPUT: «[1, [1, 2], ([3, 4], 5)]␤»

# 반면에 "단일 인수 규칙" slurpy는 "단일 인수 규칙"을 따릅니다.
# 이는 컨텍스트에 따라 slurpy 인수를 처리하는 방법을 지시하며,
# 단일 인수만 전달되고 해당 인수가 Iterable인 경우
# 해당 인수가 slurpy 매개변수 배열을 채우는 데 사용된다는 것을 대략적으로 명시합니다.
# 다른 모든 경우에 `+@`는 `**@`처럼 작동합니다.
sub c(+@arr) { @arr.perl.say };
c(['a', 'b', 'c']);             # OUTPUT: «["a", "b", "c"]␤»
c(1, $('d', 'e', 'f'), [2, 3]); # OUTPUT: «[1, ("d", "e", "f"), [2, 3]]␤»
c(1, [1, 2], ([3, 4], 5));      # OUTPUT: «[1, [1, 2], ([3, 4], 5)]␤»

# `|` (이 연산자의 유일한 역할은 아니지만)를 사용하여 배열로 함수를 호출할 수 있습니다.
sub concat3($a, $b, $c) {
    say "$a, $b, $c";
}
concat3(|@array); # OUTPUT: «a, b, c␤»
                  # `@array`가 인수 목록의 일부로 "평탄화"되었습니다.

####################################################
# 3. 컨테이너
####################################################

# Raku에서 값은 실제로 "컨테이너"에 저장됩니다. 할당 연산자는 왼쪽에 있는 컨테이너에게 오른쪽에 있는 값을 저장하도록 요청합니다.
# 전달될 때 컨테이너는 불변으로 표시됩니다. 즉, 함수에서 인수를 변경하려고 하면 오류가 발생합니다.
# 정말로 필요한 경우 `is rw` 특성을 사용하여 변경 가능한 컨테이너를 요청할 수 있습니다.
sub mutate( $n is rw ) {
    $n++; # 후위 ++ 연산자는 인수를 증가시키지만 이전 값을 반환합니다.
}
my $m = 42;
mutate $m; #=> 42, 값이 증가했지만 이전 값이 반환됩니다.
say $m;    # OUTPUT: «43␤»

# 컨테이너 $m을 `mutate` 서브루틴에 전달하고 있기 때문에 작동합니다.
# 변수가 아닌 숫자를 전달하려고 하면 컨테이너가 전달되지 않고 정수 자체는 불변이므로 작동하지 않습니다:

# mutate 42; # 매개변수 '$n'은 쓰기 가능한 컨테이너를 예상했지만 Int 값을 받았습니다.

# 마찬가지로, 바인딩된 변수가 서브루틴에 전달되면 유사한 오류가 발생합니다. Raku에서 값을 변수에 바인딩하려면 바인딩 연산자 `:=`를 사용합니다.
my $v := 50; # 변수 $v에 50 바인딩
# mutate $v;   # 매개변수 '$n'은 쓰기 가능한 컨테이너를 예상했지만 Int 값을 받았습니다.

# 대신 복사본을 원한다면 `is copy` 특성을 사용하여 인수를 복사하고 전달된 인수를 수정하지 않고 서브루틴 내에서 인수를 수정할 수 있습니다.

# 서브루틴 자체는 컨테이너를 반환합니다. 즉, `rw`로 표시할 수 있습니다.
# 또는 `return` 대신 `return-rw`를 사용하여 반환된 컨테이너를 명시적으로 변경 가능하게 표시할 수 있습니다.
my $x = 42;
my $y = 45;
sub x-store is rw { $x }
sub y-store       { return-rw $y }

# 이 경우 괄호는 필수입니다. 그렇지 않으면 Raku는 `x-store`와 `y-store`를 식별자로 간주합니다.
x-store() = 52;
y-store() *= 2;

say $x; # OUTPUT: «52␤»
say $y; # OUTPUT: «90␤»

####################################################
# 4. 제어 흐름 구조
####################################################

#
# 4.1 if/if-else/if-elsif-else/unless
#

# `if`에 대해 이야기하기 전에 어떤 값이 "참"(`True`를 나타냄)이고 어떤 값이 "거짓"(`False`를 나타냄)인지 알아야 합니다. 0, (), {}, "", Nil, 유형(예: `Str`, `Int` 등) 및 물론 `False` 자체만 거짓입니다. 다른 모든 값은 참입니다.
my $number = 5;
if $number < 5 {
    say "Number is less than 5"
}
elsif $number == 5 {
    say "Number is equal to 5"
}
else {
    say "Number is greater than 5"
}

unless False {
    say "It's not false!";
}

# `unless`는 조건문의 의미를 반전시키는 `if not (X)`와 유사합니다. 그러나 `else` 또는 `elsif`와 함께 사용할 수 없습니다.

# 문 수정자(postfix) 버전도 사용할 수 있습니다:
say "Quite truthy" if True;      # OUTPUT: «Quite truthy␤»
say "Quite falsey" unless False; # OUTPUT: «Quite falsey␤»

# 삼항 연산자(`??..!!`)는 `condition ?? expression1 !! expression2`와 같이 구성되며, 조건이 참이면 expression1을 반환합니다. 그렇지 않으면 expression2를 반환합니다.
my $age = 30;
say $age > 18 ?? "You are an adult" !! "You are under 18";
# OUTPUT: «You are an adult␤»

#
# 4.2 with/with-else/with-orwith-else/without
#

# `with` 문은 `if`와 유사하지만, 참 여부 대신 정의 여부를 테스트하며, 나중에 논의될 `given`과 마찬가지로 조건에 따라 주제를 정합니다.
my $s = "raku";
with   $s.index("r") { say "Found a at $_"      }
orwith $s.index("k") { say "Found c at $_"      }
else                 { say "Didn't find r or k" }

# 거짓 여부를 확인하는 `unless`와 유사하게, 정의되지 않은 여부를 확인하는 `without`을 사용할 수 있습니다.
my $input01;
without $input01 {
    say "No input given."
}
# OUTPUT: «No input given.␤»

# `with`와 `without` 모두에 대한 문 수정자 버전도 있습니다.
say $input02 with $input02;               # OUTPUT: «Hello␤»
say "No input given." without $input02;

#
# 4.3 given/when, 또는 Raku의 switch 구문
#

=begin comment
`given...when`은 다른 언어의 `switch`와 유사하지만, 스마트 매칭과 Raku의 "토픽 변수" `$_` 덕분에 훨씬 더 강력합니다.

토픽 변수 `$_`는 블록의 기본 인수, 루프의 현재 반복(명시적으로 명명되지 않은 경우) 등을 포함합니다.

`given`은 단순히 인수를 `$_`에 넣고(블록처럼), `when`은 "스마트 매칭"(`~~`) 연산자를 사용하여 비교합니다.

다른 Raku 구성도 이 변수를 사용하므로(`for`, 블록, `with` 문 등), 강력한 `when`은 `given`과 함께 사용할 수 있을 뿐만 아니라 `$_`가 존재하는 모든 곳에서 사용할 수 있습니다.

=end comment

given "foo bar" {
    say $_;            # OUTPUT: «foo bar␤»

    # 스마트 매칭에 대해 아직 걱정하지 마십시오. `when`이 스마트 매칭을 사용한다는 것만 아십시오. 이것은 `if $_ ~~ /foo/`와 동일합니다.
    when /foo/ {
        say "Yay !";
    }

    # `True`와 스마트 매칭하는 것은 항상 `True`입니다. 즉, (`$a ~~ True`)
    # 따라서 "일반적인" 조건문을 넣을 수도 있습니다. 예를 들어, 이 `when`은
    # 이 `if`와 동일합니다: `if $_ ~~ ($_.chars > 50) {...}`
    # 즉: `if $_.chars > 50 {...}`
    when $_.chars > 50 {
        say "Quite a long string !";
    }

    # `when *`와 동일합니다(Whatever Star 사용).
    default {
        say "Something else"
    }
}

#
# 4.4 루프 구성
#

# `loop` 구성은 인수를 전달하지 않으면 무한 루프이지만,
# C 스타일 `for` 루프일 수도 있습니다:
loop {
    say "This is an infinite loop !";
    last;
}
# 이전 예제에서 `last`는 다른 언어의 `break` 키워드와 매우 유사하게 루프를 빠져나옵니다.

# `next` 키워드는 다른 언어의 `continue`와 유사하게 다음 반복으로 건너뜁니다.
# 또한 postfix 조건문, 루프 등을 사용할 수 있습니다.
loop (my $i = 0; $i < 5; $i++) {
    next if $i == 3;
    say "This is a C-style for loop!";
}

# `for` 구성은 요소 목록을 반복합니다.
my @odd-array = 1, 3, 5, 7, 9;

# 토픽 변수 $_를 사용하여 배열의 요소에 액세스합니다.
for @odd-array {
    say "I've got $_ !";
}

# "pointy block"(`->`)을 사용하여 배열의 요소에 액세스합니다.
# 여기서 각 요소는 읽기 전용입니다.
for @odd-array -> $variable {
    say "I've got $variable !";
}

# "doubly pointy block"(`<->`)을 사용하여 배열의 요소에 액세스합니다.
# 여기서 각 요소는 읽기-쓰기이므로 `$variable`을 변경하면
# 배열의 해당 요소가 변경됩니다.
for @odd-array <-> $variable {
    say "I've got $variable !";
}

# `given`에서 본 것처럼, `for` 루프의 기본 "현재 반복" 변수는 `$_`입니다.
# 즉, `given`에서와 마찬가지로 `for` 루프에서도 `when`을 사용할 수 있습니다.
for @odd-array {
    say "I've got $_";

    # 이것도 허용됩니다. "topic"(수신자)이 없는 점 호출은 기본적으로 `$_`(토픽 변수)로 전송됩니다.
    .say;

    # 위 문과 동일합니다.
    $_.say;
}

for @odd-array {
    # 다음을 수행할 수 있습니다...
    next if $_ == 3; # 다음 반복으로 건너뛰기 (C-like 언어의 `continue`)
    redo if $_ == 4; # 동일한 토픽 변수(`$_`)를 유지하면서 반복 다시 실행
    last if $_ == 5; # 또는 루프에서 벗어나기 (C-like 언어의 `break`)
}

# "pointy block" 구문은 `for` 루프에만 국한되지 않습니다. Raku에서 블록을 표현하는 방법일 뿐입니다.
sub long-computation { "Finding factors of large primes" }
if long-computation() -> $result {
    say "The result is $result.";
}

####################################################
# 5. 연산자
####################################################

=begin comment
Perl 언어는 연산자 기반 언어이므로 Raku 연산자는 실제로
`infix:<+>`(덧셈) 또는 `prefix:<!>`(부울 부정)와 같은 구문 범주에서
재미있는 모양의 서브루틴일 뿐입니다.

범주는 다음과 같습니다:
    - "prefix": 앞에 (예: `!True`의 `!`).
    - "postfix": 뒤에 (예: `$a++`의 `++`).
    - "infix": 사이에 (예: `4 * 3`의 `*`).
    - "circumfix": 주위에 (예: `[1, 2]`의 `[`-`]`).
    - "post-circumfix": 다른 용어 뒤에 주위에 (예: `%hash{'key'}`의 `{`-`}`)

결합성 및 우선 순위 목록은 아래에 설명되어 있습니다.

자, 이제 시작할 준비가 되었습니다!

=end comment

#
# 5.1 같음 확인
#

# `==`는 숫자 비교입니다.
say 3 == 4; # OUTPUT: «False␤»
say 3 != 4; # OUTPUT: «True␤»

# `eq`는 문자열 비교입니다.
say 'a' eq 'b';  # OUTPUT: «False␤»
say 'a' ne 'b';  # OUTPUT: «True␤», 같지 않음
say 'a' !eq 'b'; # OUTPUT: «True␤», 위와 동일

# `eqv`는 정규 동등성(또는 "깊은 동등성")입니다.
say (1, 2) eqv (1, 3); # OUTPUT: «False␤»
say (1, 2) eqv (1, 2); # OUTPUT: «True␤»
say Int === Int;       # OUTPUT: «True␤»

# `~~`는 왼쪽을 `$_`에 별칭으로 지정한 다음 오른쪽을 평가하는 스마트 일치 연산자입니다.
# 여기서는 `Match` 객체를 반환하며, 정규식이 일치하면 `True`로 평가됩니다.
# 일치 결과는 `$/` 변수(암시적으로 어휘적으로 범위 지정됨)에서 사용할 수 있습니다. 또한 0부터 시작하는 캡처 변수 `$0`, `$1`, `$2` 등을 사용할 수 있습니다.

# `~~`는 시작/끝 검사를 수행하지 않습니다. 즉, 정규식은 문자열의 한 문자만으로 일치할 수 있습니다. 나중에 이 작업을 수행하는 방법을 설명합니다.

# Raku에서는 모든 영숫자를 리터럴로 사용할 수 있으며, 다른 모든 것은 백슬래시나 따옴표를 사용하여 이스케이프해야 합니다.
say so 'a|b' ~~ / a '|' b /; # OUTPUT: «True␤», `|`가 이스케이프되지 않으면 동일한 의미가 아닙니다.
say so 'a|b' ~~ / a \| b /;  # OUTPUT: «True␤», 이스케이프하는 또 다른 방법입니다.

# 정규식의 공백은 중요하지 않습니다. `:s`(`:sigspace`, 중요한 공백) 부사를 사용하지 않는 한.
say so 'a b c' ~~ / a  b  c /; #=> `False`, 여기서 공백은 중요하지 않습니다!
say so 'a b c' ~~ /:s a b c /; #=> `True`, 여기에 `:s` 수정자를 추가했습니다.

# 정규식에서 문자열 사이에 공백을 하나만 사용하면 공백이 정규식에서 중요하지 않다는 경고가 표시됩니다:
say so 'a b c' ~~ / a b c /;   # OUTPUT: «False␤»
say so 'a b c' ~~ / a  b  c /; # OUTPUT: «False»

# 참고: 따옴표나 `:s`(`:sigspace`) 수정자를 사용하십시오(또는 이 경고를 억제하려면 공백을 생략하거나 다른 방식으로 간격을 변경하십시오). 이 문제를 해결하고 공백을 덜 모호하게 만들려면 문자열 사이에 최소 두 개의 공백을 사용하거나 `:s` 부사를 사용하십시오.

# 이전에 본 것처럼, `:s`를 슬래시 구분 기호 안에 포함할 수 있지만, `m`을 '일치'로 지정하면 슬래시 구분 기호 외부에 배치할 수도 있습니다:
say so 'abc' ~~ m:s/a  b  c/; # OUTPUT: «True␤»

# `m/.../`는 `/.../`와 동일합니다:
say 'raku' ~~ m/raku/; # OUTPUT: «True␤»
say 'raku' ~~ /raku/;  # OUTPUT: «True␤»

# `:i` 부사를 사용하여 대소문자 구분을 지정합니다:
say so 'ABC' ~~ m:i{a  b  c};   # OUTPUT: «True␤»

# 그러나 공백은 수정자가 적용되는 방식에 중요합니다.
# (아래에서 설명)

#
# 5.1 수량자 - `?`, `+`, `*` 및 `**`.
#

# `?` - 0개 또는 1개 일치
say so 'ac' ~~ / a  b  c /;   # OUTPUT: «False␤»
say so 'ac' ~~ / a  b?  c /;  # OUTPUT: «True␤», "b"가 0번 일치했습니다.
say so 'abc' ~~ / a  b?  c /; # OUTPUT: «True␤», "b"가 1번 일치했습니다.

# ... 이전에 읽었듯이, 공백은 수정자가 적용되는 정규식의 대상을 결정하므로 중요합니다:
say so 'def' ~~ / a  b  c? /; # OUTPUT: «False␤», "c"만 선택 사항입니다.
say so 'def' ~~ / a  b?  c /; # OUTPUT: «False␤», 공백은 중요하지 않습니다.
say so 'def' ~~ / 'abc'? /;   # OUTPUT: «True␤», 전체 "abc" 그룹은 선택 사항입니다.

# 여기서(그리고 아래에서) 수량자는 "b"에만 적용됩니다.

# `+` - 1개 이상 일치
say so 'ac' ~~ / a  b+  c /;     # OUTPUT: «False␤», `+`는 최소한 하나의 'b'를 원합니다.
say so 'abc' ~~ / a  b+  c /;    # OUTPUT: «True␤», 하나면 충분합니다.
say so 'abbbbc' ~~ / a  b+  c /; # OUTPUT: «True␤», 4개의 "b"가 일치했습니다.

# `*` - 0개 이상 일치
say so 'ac' ~~ / a  b*  c /;     # OUTPUT: «True␤», 모두 선택 사항입니다.
say so 'abc' ~~ / a  b*  c /;    # OUTPUT: «True␤»
say so 'abbbbc' ~~ / a  b*  c /; # OUTPUT: «True␤»
say so 'aec' ~~ / a  b*  c /;    # OUTPUT: «False␤», "b"는 선택 사항이지만 대체할 수 없습니다.

# `**` - (무한) 수량자
# 열심히 노력하면 왜 거듭제곱이 수량에 사용되는지 이해할 수 있을 것입니다.
say so 'abc' ~~ / a  b**1  c /;         # OUTPUT: «True␤», 정확히 한 번
say so 'abc' ~~ / a  b**1..3  c /;      # OUTPUT: «True␤», 1에서 3번
say so 'abbbc' ~~ / a  b**1..3  c /;    # OUTPUT: «True␤»
say so 'abbbbbbc' ~~ / a  b**1..3  c /; # OUTPUT: «Fals␤», 너무 많음
say so 'abbbbbbc' ~~ / a  b**3..*  c /; # OUTPUT: «True␤», 무한 범위는 괜찮습니다.

#
# 18.2 `<[]>` - 문자 클래스
#

# 문자 클래스는 PCRE의 `[]` 클래스와 동일하지만, 더 Raku스러운 구문을 사용합니다:
say 'fooa' ~~ / f <[ o a ]>+ /;  # OUTPUT: «fooa␤»

# 범위(`..`)를 사용할 수 있습니다:
say 'aeiou' ~~ / a <[ e..w ]> /; # OUTPUT: «ae␤»

# 일반 정규식과 마찬가지로 특수 문자를 사용하려면 백슬래시나 따옴표를 사용하여 이스케이프해야 합니다(마지막은 공백을 이스케이프하는 것으로, ' '를 사용하는 것과 동일합니다):
say 'he-he !' ~~ / 'he-' <[ a..z \! \  ]> + /; # OUTPUT: «he-he !␤»

# 중복된 이름을 넣으면 경고가 표시됩니다(원시 인용을 잡는 좋은 효과가 있습니다):
'he he' ~~ / <[ h e ' ' ]> /;
# "문자 클래스에서 예기치 않게 반복된 문자(')가 발견되었습니다" 경고

# 문자 클래스를 부정할 수도 있습니다... (`<-[]>`는 PCRE의 `[^]`와 동일합니다)
say so 'foo' ~~ / <-[ f o ]> + /; # OUTPUT: «False␤»