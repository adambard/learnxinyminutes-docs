---
name: Perl
filename: learnperl.pl
contributors:
    - ["Korjavin Ivan", "http://github.com/korjavin"]
    - ["Dan Book", "http://github.com/Grinnz"]
---

Perl은 25년 이상 개발되어 온 매우 유능하고 기능이 풍부한 프로그래밍 언어입니다.

Perl은 휴대용 장치에서 메인프레임에 이르기까지 100개 이상의 플랫폼에서 실행되며 신속한 프로토타이핑과 대규모 개발 프로젝트 모두에 적합합니다.

```perl
# 한 줄 주석은 숫자 기호로 시작합니다.

#### Strict 및 warnings

use strict;
use warnings;

# 모든 perl 스크립트와 모듈은 이 줄들을 포함해야 합니다. Strict는
# 변수 이름 철자가 틀린 경우와 같은 경우에 컴파일을 실패하게 하고,
# warnings는 정의되지 않은 값에 연결하는 것과 같은 일반적인 함정의 경우에
# 경고 메시지를 출력합니다.

#### Perl 변수 유형

#  변수는 유형을 보여주는 기호인 시길로 시작합니다.
#  유효한 변수 이름은 문자나 밑줄로 시작하고,
#  그 뒤에 문자, 숫자, 밑줄이 여러 개 올 수 있습니다.

### Perl에는 세 가지 주요 변수 유형이 있습니다: $scalar, @array, %hash.

## 스칼라
#  스칼라는 단일 값을 나타냅니다:
my $animal = "camel";
my $answer = 42;
my $display = "You have $answer ${animal}s.\n";

# 스칼라 값은 문자열, 정수 또는 부동 소수점 수일 수 있으며,
# Perl은 필요에 따라 자동으로 변환합니다.

# 작은따옴표 안의 문자열은 리터럴 문자열입니다. 큰따옴표 안의 문자열은
# 변수와 개행을 위한 "\n"과 같은 이스케이프 코드를 보간합니다.

## 배열
#  배열은 값의 목록을 나타냅니다:
my @animals = ("camel", "llama", "owl");
my @numbers = (23, 42, 69);
my @mixed   = ("camel", 42, 1.23);

# 배열 요소는 대괄호를 사용하여 액세스하며, $를 사용하여
# 하나의 값이 반환됨을 나타냅니다.
my $second = $animals[1];

# 배열의 크기는 배열을 스칼라 컨텍스트에서 액세스하여 검색합니다.
# 예를 들어 스칼라 변수에 할당하거나 "scalar" 연산자를 사용합니다.

my $num_animals = @animals;
print "Number of numbers: ", scalar(@numbers), "\n";

# 배열은 큰따옴표로 묶인 문자열에 보간될 수도 있으며,
# 요소는 기본적으로 공백 문자로 구분됩니다.

print "We have these numbers: @numbers\n";

# 이메일 주소와 같은 기호가 포함된 문자열에 큰따옴표를 사용할 때는
# 변수로 해석되므로 주의하십시오.

my @example = ('secret', 'array');
my $oops_email = "foo@example.com"; # 'foosecret array.com'
my $ok_email = 'foo@example.com';

## 해시
#   해시는 키/값 쌍의 집합을 나타냅니다:

my %fruit_color = ("apple", "red", "banana", "yellow");

#  공백과 "=>" 연산자를 사용하여 더 멋지게
#  배치할 수 있습니다:

my %fruit_color = (
  apple  => "red",
  banana => "yellow",
);

# 해시 요소는 중괄호를 사용하여 액세스하며, 다시 $ 시길을 사용합니다.
my $color = $fruit_color{apple};

# 해시에 있는 모든 키 또는 값은 "keys" 및 "values" 함수를
# 사용하여 액세스할 수 있습니다.
my @fruits = keys %fruit_color;
my @colors = values %fruit_color;

# 스칼라, 배열 및 해시는 perldata에 더 자세히 문서화되어 있습니다.
# (perldoc perldata).

#### 참조

# 참조를 사용하여 더 복잡한 데이터 유형을 구성할 수 있으며,
# 이를 통해 배열 및 해시 내에 배열과 해시를 만들 수 있습니다.

my $array_ref = \@array;
my $hash_ref = \%hash;
my @array_of_arrays = (\@array1, \@array2, \@array3);

# 익명 배열 또는 해시를 만들어 참조를 반환할 수도 있습니다:

my $fruits = ["apple", "banana"];
my $colors = {apple => "red", banana => "yellow"};

# 참조는 적절한 시길을 접두사로 붙여 역참조할 수 있습니다.

my @fruits_array = @$fruits;
my %colors_hash = %$colors;

# 바로 가기로 화살표 연산자를 사용하여 단일 값을 역참조하고
# 액세스할 수 있습니다.

my $first = $array_ref->[0];
my $value = $hash_ref->{banana};

# 참조에 대한 자세한 내용은 perlreftut 및 perlref를 참조하십시오.

#### 조건 및 반복 구문

# Perl에는 대부분의 일반적인 조건 및 반복 구문이 있습니다.

if ($var) {
  ...
} elsif ($var eq 'bar') {
  ...
} else {
  ...
}

unless (condition) {
  ...
}
# 이것은 "if (!condition)"의 더 읽기 쉬운 버전으로 제공됩니다.

# Perl 방식의 후위 조건
print "Yow!" if $zippy;
print "We have no bananas" unless $bananas;

#  while
while (condition) {
  ...
}

my $max = 5;
# for 루프 및 반복
for my $i (0 .. $max) {
  print "index is $i";
}

for my $element (@elements) {
  print $element;
}

map {print} @elements;

# 암시적으로

for (@elements) {
  print;
}

# 해시 반복 (for와 foreach는 동일)

foreach my $key (keys %hash) {
  print $key, ': ', $hash{$key}, "\n";
}

# 다시 Perl 방식의 후위 조건
print for @elements;

# 참조된 해시의 키와 값을 반복
print $hash_ref->{$_} for keys %$hash_ref;

#### 정규 표현식

# Perl의 정규 표현식 지원은 광범위하고 심층적이며,
# perlrequick, perlretut 등에서 자세히 설명합니다.
# 그러나 간단히 말해서:

# 간단한 일치
if (/foo/)       { ... }  # $_에 "foo"가 포함되어 있으면 참
if ($x =~ /foo/) { ... }  # $x에 "foo"가 포함되어 있으면 참

# 간단한 대체

$x =~ s/foo/bar/;         # $x에서 foo를 bar로 대체
$x =~ s/foo/bar/g;        # $x에서 모든 foo 인스턴스를 bar로 대체


#### 파일 및 I/O

# "open()" 함수를 사용하여 입력 또는 출력용 파일을 열 수 있습니다.

# 읽기용:
open(my $in,  "<",  "input.txt")  or die "Can't open input.txt: $!";
# 쓰기용 (파일이 있으면 지움):
open(my $out, ">",  "output.txt") or die "Can't open output.txt: $!";
# 쓰기용 (파일 끝에 추가):
open(my $log, ">>", "my.log")     or die "Can't open my.log: $!";

# "<>" 연산자를 사용하여 열린 파일 핸들에서 읽을 수 있습니다.
# 스칼라 컨텍스트에서는 파일 핸들에서 한 줄을 읽고, 리스트
# 컨텍스트에서는 전체 파일을 읽어 각 줄을 리스트의
# 요소에 할당합니다:

my $line  = <$in>;
my @lines = <$in>;

# while 루프를 사용하여 파일의 줄을 한 번에 하나씩 반복할 수 있습니다:

while (my $line = <$in>) {
  print "Found apples\n" if $line =~ m/apples/;
}

# 표준 "print" 함수를 사용하여 열린 파일 핸들에 쓸 수 있습니다.

print $out @lines;
print $log $msg, "\n";

#### 서브루틴 작성

# 서브루틴 작성은 쉽습니다:

sub logger {
  my $logmessage = shift;

  open my $logfile, ">>", "my.log" or die "Could not open my.log: $!";

  print $logfile $logmessage;
}

# 이제 다른 내장 함수처럼 서브루틴을 사용할 수 있습니다:

logger("We have a logger subroutine!");

#### 모듈

# 모듈은 다른 Perl 코드에서 사용할 수 있는 Perl 코드,
# 보통 서브루틴의 집합입니다. Perl이 찾을 수 있도록
# 보통 .pm 확장자를 가진 파일에 저장됩니다.

package MyModule;
use strict;
use warnings;

sub trim {
  my $string = shift;
  $string =~ s/^\s+//;
  $string =~ s/\s+$//;
  return $string;
}

1;

# 다른 곳에서:

use MyModule;
MyModule::trim($string);

# Exporter 모듈은 서브루틴을 내보낼 수 있도록 도와주므로
# 다음과 같이 사용할 수 있습니다:

use MyModule 'trim';
trim($string);

# 많은 Perl 모듈은 CPAN(http://www.cpan.org/)에서 다운로드할 수 있으며
# 바퀴를 다시 발명하지 않도록 도와주는 다양한 기능을 제공합니다.
# Exporter와 같은 인기 있는 여러 모듈은 Perl 배포판 자체에
# 포함되어 있습니다. Perl의 모듈에 대한 자세한 내용은 perlmod를 참조하십시오.

#### 객체

# Perl의 객체는 어떤 클래스(패키지)에 속하는지 아는 참조일 뿐이므로,
# 호출된 메서드(서브루틴)를 거기에서 찾을 수 있습니다. bless 함수는
# 생성자(보통 new)에서 이를 설정하는 데 사용됩니다. 그러나 Moose나
# Moo와 같은 모듈을 사용하면 직접 호출할 필요가 없습니다.

package MyCounter;
use strict;
use warnings;

sub new {
  my $class = shift;
  my $self = {count => 0};
  return bless $self, $class;
}

sub count {
  my $self = shift;
  return $self->{count};
}

sub increment {
  my $self = shift;
  $self->{count}++;
}

1;

# 메서드는 화살표 연산자를 사용하여 클래스 또는 객체 인스턴스에서
# 호출할 수 있습니다.

use MyCounter;
my $counter = MyCounter->new;
print $counter->count, "\n"; # 0
$counter->increment;
print $counter->count, "\n"; # 1

# CPAN의 Moose 및 Moo 모듈은 객체 클래스를 설정하는 데
# 도움이 될 수 있습니다. 생성자와 속성을 선언하는 간단한 구문을
# 제공합니다. 이 클래스는 위 클래스와 동일하게 사용할 수 있습니다.

package MyCounter;
use Moo; # strict 및 warnings 가져오기

has 'count' => (is => 'rwp', default => 0, init_arg => undef);

sub increment {
  my $self = shift;
  $self->_set_count($self->count + 1);
}

1;

# 객체 지향 프로그래밍은 perlootut에서 더 자세히 다루고,
# Perl에서의 저수준 구현은 perlobj에서 다룹니다.
```

#### FAQ

perlfaq에는 많은 일반적인 작업과 관련된 질문과 답변이 포함되어 있으며, 종종 사용할 좋은 CPAN 모듈에 대한 제안을 제공합니다.

#### 더 읽을거리

 - [perl-tutorial](http://perl-tutorial.org/)
 - [www.perl.com에서 배우기](http://www.perl.org/learn.html)
 - [perldoc](http://perldoc.perl.org/)
 - 및 perl 내장: `perldoc perlintro`
