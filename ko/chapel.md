---
name: Chapel
filename: learnchapel.chpl
contributors:
    - ["Ian J. Bertolacci", "https://www.cs.arizona.edu/~ianbertolacci/"]
    - ["Ben Harshbarger", "https://github.com/benharsh/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

[Cray의 공식 Chapel 웹사이트](https://chapel-lang.org)에서 Chapel에 대한 모든 것을 읽을 수 있습니다.
간단히 말해, Chapel은 Cray Inc.에서 개발 중인 오픈 소스, 고생산성, 병렬 프로그래밍
언어이며, 다중 코어 PC뿐만 아니라 다중 킬로코어 슈퍼컴퓨터에서도 실행되도록 설계되었습니다.

더 많은 정보와 지원은 이 문서 하단에서 찾을 수 있습니다.

이 문서의 [최신 버전](https://chapel-lang.org/docs/master/primers/learnChapelInYMinutes.html)은 공식 사이트를 참조할 수 있습니다.

```chapel
/*
   Y분 만에 Chapel 배우기

   이 입문서는 Chapel의 기본 구문과 개념을 다룹니다.
   공식 페이지와 마지막 동기화: 2020년 3월 8일 일요일 08:05:53 +0000
*/

// 주석은 C 계열 스타일입니다.

// 한 줄 주석
/*
    여러 줄 주석
*/

/*
기본 인쇄
*/

write("Hello, ");
writeln("World!");

// ``write``와 ``writeln``은 인쇄할 항목 목록을 사용할 수 있습니다.
// 각 항목은 다른 항목 바로 옆에 인쇄되므로 간격을 포함하십시오!
writeln("There are ", 3, " commas (",") in this line of code");

// 다른 출력 채널:
use IO; // 다른 출력 채널에 액세스하려면 필요합니다.

stdout.writeln("This goes to standard output, just like plain writeln() does");
stderr.writeln("This goes to standard error");

/*
변수
*/

// 변수는 컴파일러가 보유할 유형을 파악할 수 있는 한
// 명시적으로 유형을 지정할 필요가 없습니다.
// 10은 ``int``이므로 ``myVar``는 암시적으로 ``int``입니다.
var myVar = 10;
myVar = -10;
var mySecondVar = myVar;
// ``var anError;``는 컴파일 타임 오류입니다.

// 명시적으로 유형을 지정할 수 있습니다(그리고 그래야 합니다).
var myThirdVar: real;
var myFourthVar: real = -1.234;
myThirdVar = myFourthVar;

/*
유형
*/

// 여러 가지 기본 유형이 있습니다.
var myInt: int = -1000; // 부호 있는 정수
var myUint: uint = 1234; // 부호 없는 정수
var myReal: real = 9.876; // 부동 소수점 숫자
var myImag: imag = 5.0i; // 허수
var myCplx: complex = 10 + 9i; // 복소수
myCplx = myInt + myImag; // 복소수를 형성하는 또 다른 방법
var myBool: bool = false; // 부울
var myStr: string = "Some string..."; // 문자열
var singleQuoteStr = 'Another string...'; // 작은따옴표가 있는 문자열 리터럴

// 일부 유형은 크기를 가질 수 있습니다.
var my8Int: int(8) = 10; // 8비트(1바이트) 크기 정수;
var my64Real: real(64) = 1.516; // 64비트(8바이트) 크기 실수

// 유형 변환.
var intFromReal = myReal : int;
var intFromReal2: int = myReal : int;

// 유형 별칭.
type chroma = int;        // 단일 색조의 유형
type RGBColor = 3*chroma; // 전체 색상을 나타내는 유형
var black: RGBColor = (0,0,0);
var white: RGBColor = (255, 255, 255);

/*
상수 및 매개변수
*/

// ``const``는 상수이며 런타임에 설정된 후에는 변경할 수 없습니다.
const almostPi: real = 22.0/7.0;

// ``param``은 컴파일 타임에 값을 알아야 하는 상수입니다.
param compileTimeConst: int = 16;

// ``config`` 수정자는 명령줄에서 값을 설정할 수 있도록 합니다.
// 런타임에 ``--varCmdLineArg=Value`` 또는 ``--varCmdLineArg Value``로 설정합니다.
config var varCmdLineArg: int = -123;
config const constCmdLineArg: int = 777;

// ``config param``은 컴파일 타임에 설정할 수 있습니다.
// 컴파일 타임에 ``--set paramCmdLineArg=value``로 설정합니다.
config param paramCmdLineArg: bool = false;
writeln(varCmdLineArg, ", ", constCmdLineArg, ", ", paramCmdLineArg);

/*
참조
*/

// ``ref``는 C++의 참조와 매우 유사하게 작동합니다. Chapel에서 ``ref``는 초기화된 변수 이외의 변수를 별칭으로 만들 수 없습니다.
// 여기서 ``refToActual``은 ``actual``을 참조합니다.
var actual = 10;
ref refToActual = actual;
writeln(actual, " == ", refToActual); // 동일한 값 인쇄
actual = -123; // actual 수정(refToActual이 참조하는)
writeln(actual, " == ", refToActual); // 동일한 값 인쇄
refToActual = 99999999; // refToActual이 참조하는 것 수정(actual임)
writeln(actual, " == ", refToActual); // 동일한 값 인쇄

/*
연산자
*/

// 수학 연산자:
var a: int, thisInt = 1234, thatInt = 5678;
a = thisInt + thatInt;  // 덧셈
a = thisInt * thatInt;  // 곱셈
a = thisInt - thatInt;  // 뺄셈
a = thisInt / thatInt;  // 나눗셈
a = thisInt ** thatInt; // 거듭제곱
a = thisInt % thatInt;  // 나머지 (모듈로)

// 논리 연산자:
var b: bool, thisBool = false, thatBool = true;
b = thisBool && thatBool; // 논리곱
b = thisBool || thatBool; // 논리합
b = !thisBool;            // 논리 부정

// 관계 연산자:
b = thisInt > thatInt;           // 보다 큼
b = thisInt >= thatInt;          // 보다 크거나 같음
b = thisInt < a && a <= thatInt; // 보다 작음, 그리고, 보다 작거나 같음
b = thisInt != thatInt;          // 같지 않음
b = thisInt == thatInt;          // 같음

// 비트 연산자:
a = thisInt << 10;     // 왼쪽으로 10비트 시프트;
a = thatInt >> 5;      // 오른쪽으로 5비트 시프트;
a = ~thisInt;          // 비트 부정
a = thisInt ^ thatInt; // 비트 배타적 논리합

// 복합 할당 연산자:
a += thisInt;          // 덧셈-등호 (a = a + thisInt;)
a *= thatInt;          // 곱셈-등호 (a = a * thatInt;)
b &&= thatBool;        // 논리곱-등호 (b = b && thatBool;)
a <<= 3;               // 왼쪽 비트 시프트-등호 (a = a << 10;)

// 다른 C 계열 언어와 달리 다음과 같은 전/후 증감 연산자는 없습니다:
//
// ``++j``, ``--j``, ``j++``, ``j--``

// 교환 연산자:
var old_this = thisInt;
var old_that = thatInt;
thisInt <=> thatInt; // thisInt와 thatInt의 값을 교환합니다.
writeln((old_this == thatInt) && (old_that == thisInt));

// 연산자 오버로드는 프로시저에서 볼 수 있듯이 정의할 수도 있습니다.

/*
튜플
*/

// 튜플은 동일한 유형이거나 다른 유형일 수 있습니다.
var sameTup: 2*int = (10, -1);
var sameTup2 = (11, -6);
var diffTup: (int,real,complex) = (5, 1.928, myCplx);
var diffTupe2 = (7, 5.64, 6.0+1.5i);

// 튜플은 대괄호나 괄호를 사용하여 액세스할 수 있으며 1-인덱싱됩니다.
writeln("(", sameTup[1], ",", sameTup(2), ")");
writeln(diffTup);

// 튜플에 쓸 수도 있습니다.
diffTup(1) = -1;

// 튜플 값은 자체 변수로 확장될 수 있습니다.
var (tupInt, tupReal, tupCplx) = diffTup;
writeln(diffTup == (tupInt, tupReal, tupCplx));

// 디버깅에 일반적인 변수 목록을 작성하는 데에도 유용합니다.
writeln((a,b,thisInt,thatInt,thisBool,thatBool));

/*
제어 흐름
*/

// ``if`` - ``then`` - ``else``는 다른 C 계열 언어와 마찬가지로 작동합니다.
if 10 < 100 then
  writeln("All is well");

if -1 < 1 then
  writeln("Continuing to believe reality");
else
  writeln("Send mathematician, something's wrong");

// 원한다면 괄호를 사용할 수 있습니다.
if (10 > 100) {
  writeln("Universe broken. Please reboot universe.");
}

if a % 2 == 0 {
  writeln(a, " is even.");
} else {
  writeln(a, " is odd.");
}

if a % 3 == 0 {
  writeln(a, " is even divisible by 3.");
} else if a % 3 == 1 {
  writeln(a, " is divided by 3 with a remainder of 1.");
} else {
  writeln(b, " is divided by 3 with a remainder of 2.");
}

// 삼항: 문에서 ``if`` - ``then`` - ``else``.
var maximum = if thisInt < thatInt then thatInt else thisInt;

// ``select`` 문은 다른 언어의 switch 문과 매우 유사합니다.
// 그러나 ``select`` 문은 C나 Java처럼 계단식으로 작동하지 않습니다.
var inputOption = "anOption";
select inputOption {
  when "anOption" do writeln("Chose 'anOption'");
  when "otherOption" {
    writeln("Chose 'otherOption'");
    writeln("Which has a body");
  }
  otherwise {
    writeln("Any other Input");
    writeln("the otherwise case doesn't need a do if the body is one line");
  }
}

// ``while`` 및 ``do``-``while`` 루프도 C 대응 항목처럼 동작합니다.
var j: int = 1;
var jSum: int = 0;
while (j <= 1000) {
  jSum += j;
  j += 1;
}
writeln(jSum);

do {
  jSum += j;
  j += 1;
} while (j <= 10000);
writeln(jSum);

// ``for`` 루프는 파이썬의 루프와 매우 유사하며 범위를 반복합니다. 아래의 ``1..10`` 표현식과 같은 범위는 Chapel에서 일급 객체이며 변수에 저장할 수 있습니다.
for i in 1..10 do write(i, ", ");
writeln();

var iSum: int = 0;
for i in 1..1000 {
  iSum += i;
}
writeln(iSum);

for x in 1..10 {
  for y in 1..10 {
    write((x,y), "\t");
  }
  writeln();
}

/*
범위 및 도메인
*/

// For-루프와 배열은 모두 반복할 수 있는 인덱스 집합을 정의하기 위해 범위와 도메인을 사용합니다. 범위는 단일 차원 정수 인덱스이고, 도메인은 다차원일 수 있으며 다른 유형의 인덱스를 나타낼 수 있습니다.

// 이들은 일급 시민 유형이며 변수에 할당할 수 있습니다.
var range1to10: range = 1..10;  // 1, 2, 3, ..., 10
var range2to11 = 2..11; // 2, 3, 4, ..., 11
var rangeThisToThat: range = thisInt..thatInt; // 변수 사용
var rangeEmpty: range = 100..-100; // 이것은 유효하지만 인덱스를 포함하지 않습니다.

// 범위는 무한할 수 있습니다.
var range1toInf: range(boundedType=BoundedRangeType.boundedLow) = 1.. ; // 1, 2, 3, 4, 5, ...
var rangeNegInfTo1 = ..1; // ..., -4, -3, -2, -1, 0, 1

// 범위는 ``by`` 연산자를 사용하여 보폭을 지정(및 반전)할 수 있습니다.
var range2to10by2: range(stridable=true) = 2..10 by 2; // 2, 4, 6, 8, 10
var reverse2to10by2 = 2..10 by -2; // 10, 8, 6, 4, 2

var trapRange = 10..1 by -1; // 속지 마십시오. 이것은 여전히 빈 범위입니다.
writeln("Size of range ", trapRange, " = ", trapRange.size);

// 참고: ``range(boundedType= ...)`` 및 ``range(stridable= ...)``는 변수를 명시적으로 입력하는 경우에만 필요합니다.

// 범위의 끝점은 카운트(``#``) 연산자를 사용하여 범위의 총 크기를 지정하여 계산할 수 있습니다.
var rangeCount: range = -5..#12; // -5에서 6까지의 범위

// 연산자를 혼합할 수 있습니다.
var rangeCountBy: range(stridable=true) = -5..#12 by 2; // -5, -3, -1, 1, 3, 5
writeln(rangeCountBy);

// 범위의 속성을 쿼리할 수 있습니다.
// 이 예에서는 첫 번째 인덱스, 마지막 인덱스, 인덱스 수, 보폭 및 2가 범위에 포함되어 있는지 여부를 인쇄합니다.
writeln((rangeCountBy.first, rangeCountBy.last, rangeCountBy.size,
           rangeCountBy.stride, rangeCountBy.contains(2)));

for i in rangeCountBy {
  write(i, if i == rangeCountBy.last then "\n" else ", ");
}

// 직사각형 도메인은 동일한 범위 구문을 사용하여 정의되지만 범위와 달리 경계가 있어야 합니다.
var domain1to10: domain(1) = {1..10};        // 1..10의 1D 도메인;
var twoDimensions: domain(2) = {-2..2,0..2}; // 범위의 곱에 대한 2D 도메인
var thirdDim: range = 1..16;
var threeDims: domain(3) = {thirdDim, 1..10, 5..10}; // 범위 변수 사용

// 도메인 크기도 조정할 수 있습니다.
var resizedDom = {1..10};
writeln("before, resizedDom = ", resizedDom);
resizedDom = {-10..#10};
writeln("after, resizedDom = ", resizedDom);

// 인덱스는 튜플로 반복할 수 있습니다.
for idx in twoDimensions do
  write(idx, ", ");
writeln();

// 이러한 튜플은 구조를 해제할 수도 있습니다.
for (x,y) in twoDimensions {
  write("(", x, ", ", y, ")", ", ");
}
writeln();

// 연관 도메인은 집합처럼 작동합니다.
var stringSet: domain(string); // 빈 문자열 집합
stringSet += "a";
stringSet += "b";
stringSet += "c";
stringSet += "a"; // 중복 추가 "a"
stringSet -= "c"; // "c" 제거
writeln(stringSet.sorted());

// 연관 도메인은 리터럴 구문을 가질 수도 있습니다.
var intSet = {1, 2, 4, 5, 100};

// 범위와 도메인은 모두 슬라이스하여 인덱스의 교집합이 있는 범위나 도메인을 생성할 수 있습니다.
var rangeA = 1.. ; // 1에서 무한대까지의 범위
var rangeB =  ..5; // 음의 무한대에서 5까지의 범위
var rangeC = rangeA[rangeB]; // 결과 범위는 1..5입니다.
writeln((rangeA, rangeB, rangeC));

var domainA = {1..10, 5..20};
var domainB = {-5..5, 1..10};
var domainC = domainA[domainB];
writeln((domainA, domainB, domainC));

/*
배열
*/

// 배열은 다른 언어의 배열과 유사합니다.
// 크기는 인덱스를 나타내는 도메인을 사용하여 정의됩니다.
var intArray: [1..10] int;
var intArray2: [{1..10}] int; // 동일

// 대괄호나 괄호를 사용하여 액세스할 수 있습니다.
for i in 1..10 do
  intArray[i] = -i;
writeln(intArray);

// ``intArray[0]``에 액세스할 수 없습니다. 왜냐하면 정의한 인덱스 집합 ``{1..10}`` 외부에 존재하기 때문입니다.
// ``intArray[11]``도 같은 이유로 불법입니다.
var realDomain: domain(2) = {1..5,1..7};
var realArray: [realDomain] real;
var realArray2: [1..5,1..7] real;   // 동일
var realArray3: [{1..5,1..7}] real; // 동일

for i in 1..5 {
  for j in realDomain.dim(2) {   // 도메인의 2차원만 사용
    realArray[i,j] = -1.61803 * i + 0.5 * j;  // 인덱스 목록을 사용하여 액세스
    var idx: 2*int = (i,j);                   // 참고: 'index'는 키워드입니다.
    realArray[idx] = - realArray[(i,j)];      // 튜플을 사용하여 인덱싱
  }
}

// 배열에는 멤버로 도메인이 있으며 일반적인 방식으로 반복할 수 있습니다.
for idx in realArray.domain {  // 다시 말하지만, idx는 2*int 튜플입니다.
  realArray[idx] = 1 / realArray[idx[1], idx[2]]; // 튜플 및 목록으로 액세스
}

writeln(realArray);

// 배열의 값은 직접 반복할 수도 있습니다.
var rSum: real = 0;
for value in realArray {
  rSum += value; // 값 읽기
  value = rSum;  // 값 쓰기
}
writeln(rSum, "\n", realArray);

// 연관 배열(사전)은 연관 도메인을 사용하여 만들 수 있습니다.
var dictDomain: domain(string) = { "one", "two", "three"};
var dict: [dictDomain] int = ["one" => 1, "two" => 2, "three" => 3];

for key in dictDomain.sorted() do
  writeln(dict[key]);

// 배열은 몇 가지 다른 방식으로 서로 할당할 수 있습니다.
// 이 배열은 예제에서 사용됩니다.
var thisArray : [0..5] int = [0,1,2,3,4,5];
var thatArray : [0..5] int;

// 첫째, 하나를 다른 하나에 간단히 할당합니다. 이것은 ``thisArray``를 ``thatArray``에 복사하는 것이지 참조를 만드는 것이 아닙니다. 따라서 ``thisArray``를 수정해도 ``thatArray``는 수정되지 않습니다.

thatArray = thisArray;
thatArray[1] = -1;
writeln((thisArray, thatArray));

// 한 배열의 슬라이스를 다른 배열의 슬라이스(동일한 크기)에 할당합니다.
thatArray[4..5] = thisArray[1..2];
writeln((thisArray, thatArray));

// 연산은 배열에 대해 승격될 수도 있습니다. 'thisPlusThat'도 배열입니다.
var thisPlusThat = thisArray + thatArray;
writeln(thisPlusThat);

// 계속해서, 배열과 루프는 표현식일 수도 있으며, 여기서 루프 본문의 표현식은 각 반복의 결과입니다.
var arrayFromLoop = for i in 1..10 do i;
Writeln(arrayFromLoop);

// 표현식은 if-표현식으로 필터링할 때와 같이 아무것도 반환하지 않을 수 있습니다.
var evensOrFives = for i in 1..10 do if (i % 2 == 0 || i % 5 == 0) then i;

Writeln(arrayFromLoop);

// 배열 표현식은 대괄호 표기법으로도 작성할 수 있습니다.
// 참고: 이 구문은 나중에 논의될 ``forall`` 병렬 개념을 사용합니다.
var evensOrFivesAgain = [i in 1..10] if (i % 2 == 0 || i % 5 == 0) then i;

// 배열의 값에 대해서도 작성할 수 있습니다.
arrayFromLoop = [value in arrayFromLoop] value + 1;


/*
프로시저
*/

// Chapel 프로시저는 다른 언어의 함수와 유사한 구문을 가집니다.
proc fibonacci(n : int) : int {
  if n <= 1 then return n;
  return fibonacci(n-1) + fibonacci(n-2);
}

// 입력 매개변수는 제네릭 프로시저를 만들기 위해 유형이 지정되지 않을 수 있습니다.
proc doublePrint(thing): void {
  write(thing, " ", thing, "\n");
}

// 반환 유형은 컴파일러가 파악할 수 있는 한 추론될 수 있습니다.
proc addThree(n) {
  return n + 3;
}

doublePrint(addThree(fibonacci(20)));

// 가변 개수의 매개변수를 사용하는 것도 가능합니다.
proc maxOf(x ...?k) {
  // x는 k개의 요소를 가진 한 가지 유형의 튜플을 참조합니다.
  var maximum = x[1];
  for i in 2..k do maximum = if maximum < x[i] then x[i] else maximum;
  return maximum;
}
writeln(maxOf(1, -10, 189, -9071982, 5, 17, 20001, 42));

// 프로시저는 기본 매개변수 값을 가질 수 있으며,
// 매개변수는 순서에 상관없이 호출에서 이름으로 지정할 수 있습니다.
proc defaultsProc(x: int, y: real = 1.2634): (int,real) {
  return (x,y);
}

writeln(defaultsProc(10));
writeln(defaultsProc(x=11));
writeln(defaultsProc(x=12, y=5.432));
writeln(defaultsProc(y=9.876, x=13));

// ``?`` 연산자는 쿼리 연산자라고 하며, 튜플이나 배열 크기 및 제네릭 유형과 같은 미정 값을 사용하는 데 사용됩니다.
// 예를 들어, 매개변수로 배열을 사용하는 경우입니다. 쿼리 연산자는 ``A``의 도메인을 결정하는 데 사용됩니다. 이것은 반환 유형을 정의하는 데 유용하지만 필수는 아닙니다.
proc invertArray(A: [?D] int): [D] int{
  for a in A do a = -a;
  return A;
}

writeln(invertArray(intArray));

// 제네릭 프로시저에 대한 인수의 유형을 쿼리할 수 있습니다.
// 여기서 우리는 동일한 유형의 두 인수를 사용하는 프로시저를 정의하지만 해당 유형이 무엇인지는 정의하지 않습니다.
proc genericProc(arg1 : ?valueType, arg2 : valueType): void {
  select(valueType) {
    when int do writeln(arg1, " and ", arg2, " are ints");
    when real do writeln(arg1, " and ", arg2, " are reals");
    otherwise writeln(arg1, " and ", arg2, " are somethings!");
  }
}

genericProc(1, 2);
genericProc(1.2, 2.3);
genericProc(1.0+2.0i, 3.0+4.0i);

// ``where`` 절을 사용하여 다형성의 한 형태를 적용할 수도 있습니다.
// 이렇게 하면 컴파일러가 사용할 함수를 결정할 수 있습니다.
// 참고: 즉, 모든 정보는 컴파일 타임에 알려져야 합니다.
// arg의 param 수정자는 이 제약 조건을 적용하는 데 사용됩니다.
proc whereProc(param N : int): void
 where (N > 0) {
  writeln("N is greater than 0");
}

proc whereProc(param N : int): void
 where (N < 0) {
  writeln("N is less than 0");
}

whereProc(10);
whereProc(-1);

// ``whereProc(0)``은 ``where`` 절의 조건을 만족하는 함수가 없기 때문에 컴파일러 오류가 발생합니다.
// ``where`` 절이 없는 ``whereProc``를 정의하여 다른 모든 경우(이 경우 하나만 있음)를 포괄하는 캐치올로 사용할 수 있습니다.

// ``where`` 절은 인수 유형에 따라 제약 조건을 지정하는 데에도 사용할 수 있습니다.
proc whereType(x: ?t) where t == int {
  writeln("Inside 'int' version of 'whereType': ", x);
}

proc whereType(x: ?t) {
  writeln("Inside general version of 'whereType': ", x);
}

whereType(42);
whereType("hello");

/*
의도
*/

/* 인수에 대한 의도 수정자는 해당 인수가 프로시저에 전달되는 방식을 전달합니다.

     * in: 인수를 복사하지만 출력하지 않음
     * out: 인수를 출력하지만 복사하지 않음
     * inout: 인수를 복사하고 출력함
     * ref: 참조로 인수 전달
*/
proc intentsProc(in inarg, out outarg, inout inoutarg, ref refarg) {
  writeln("Inside Before: ", (inarg, outarg, inoutarg, refarg));
  inarg = inarg + 100;
  outarg = outarg + 100;
  inoutarg = inoutarg + 100;
  refarg = refarg + 100;
  writeln("Inside After: ", (inarg, outarg, inoutarg, refarg));
}

var inVar: int = 1;
var outVar: int = 2;
var inoutVar: int = 3;
var refVar: int = 4;
writeln("Outside Before: ", (inVar, outVar, inoutVar, refVar));
intentsProc(inVar, outVar, inoutVar, refVar);
Writeln("Outside After: ", (inVar, outVar, inoutVar, refVar));

// 마찬가지로 반환 유형에 대한 의도를 정의할 수 있습니다.
// ``refElement``는 배열의 요소에 대한 참조를 반환합니다.
// 이것은 데이터 구조의 요소에 대한 참조가 메서드나 반복기를 통해 반환되는 클래스 메서드에 더 실용적입니다.
proc refElement(array : [?D] ?T, idx) ref : T {
  return array[idx];
}

var myChangingArray : [1..5] int = [1,2,3,4,5];
writeln(myChangingArray);
ref refToElem = refElement(myChangingArray, 5); // ref 변수에 요소에 대한 참조 저장
writeln(refToElem);
refToElem = -2; // 배열의 실제 값을 수정하는 참조 수정
writeln(refToElem);
Writeln(myChangingArray);

/*
연산자 정의
*/

// Chapel은 연산자를 오버로드할 수 있습니다.
// 단항 연산자를 정의할 수 있습니다:
// ``+ - ! ~``
// 이항 연산자:
// ``+ - * / % ** == <= >= < > << >> & | ˆ by``
// ``+= -= *= /= %= **= &= |= ˆ= <<= >>= <=>`

// 부울 배타적 논리합 연산자.
proc ^(left : bool, right : bool): bool {
  return (left || right) && !(left && right);
}

writeln(true  ^ true);
writeln(false ^ true);
writeln(true  ^ false);
writeln(false ^ false);

// 해당 유형의 튜플을 반환하는 모든 두 유형에 대해 ``*`` 연산자를 정의합니다.
proc *(left : ?ltype, right : ?rtype): (ltype, rtype) {
  writeln("	In our '*' overload!");
  return (left, right);
}

writeln(1 * "a"); // ``*`` 연산자를 사용합니다.
writeln(1 * 2);   // 기본 ``*`` 연산자를 사용합니다.

//  참고: 오버로드에 부주의하면 모든 것을 망칠 수 있습니다.
//  이것은 모든 것을 망칠 것입니다. 하지 마십시오.

/*

      proc +(left: int, right: int): int {
        return left - right;
      }
*/

/*
반복기
*/

// 반복기는 프로시저의 자매이며, 프로시저에 대한 거의 모든 것이 반복기에도 적용됩니다. 그러나 단일 값을 반환하는 대신 반복기는 루프에 여러 값을 생성할 수 있습니다.
//
// 이것은 반복을 정의하는 코드를 루프 본문과 분리할 수 있으므로 복잡한 반복 집합이나 순서가 필요할 때 유용합니다.
iter oddsThenEvens(N: int): int {
  for i in 1..N by 2 do
    yield i; // 반환하는 대신 값을 생성합니다.
  for i in 2..N by 2 do
    yield i;
}

for i in oddsThenEvens(10) do write(i, ", ");
Writeln();

// 반복기는 조건부로 생성할 수도 있으며, 그 결과는 아무것도 아닐 수 있습니다.
iter absolutelyNothing(N): int {
  for i in 1..N {
    if N < i { // 항상 거짓
      yield i;     // Yield 문은 절대 발생하지 않습니다.
    }
  }
}

for i in absolutelyNothing(10) {
  writeln("Woa there! absolutelyNothing yielded ", i);
}

// ``zip()``을 사용하여 두 개 이상의 반복기(반복 횟수가 동일한)를 함께 묶어 단일 압축 반복기를 만들 수 있습니다. 여기서 압축 반복기의 각 반복은 각 반복기에서 생성된 하나의 값의 튜플을 생성합니다.
for (positive, negative) in zip(1..5, -5..-1) do
  writeln((positive, negative));

// 지퍼 반복은 배열, 배열 슬라이스 및 배열/루프 표현식의 할당에 매우 중요합니다.
var fromThatArray : [1..#5] int = [1,2,3,4,5];
var toThisArray : [100..#5] int;

// 일부 지퍼 작업은 다른 작업을 구현합니다.
// 첫 번째 문과 루프는 동일합니다.
toThisArray = fromThatArray;
for (i,j) in zip(toThisArray.domain, fromThatArray.domain) {
  toThisArray[i] = fromThatArray[j];
}

// 이 두 덩어리도 동일합니다.
toThisArray = [j in -100..#5] j;
Writeln(toThisArray);

for (i, j) in zip(toThisArray.domain, -100..#5) {
  toThisArray[i] = j;
}
Writeln(toThisArray);

// 이 문이 런타임 오류를 나타내는 이유를 이해하는 데 매우 중요합니다.

/*
      var iterArray : [1..10] int = [i in 1..10] if (i % 2 == 1) then i;
*/

// 배열의 도메인과 루프 표현식의 크기가 같더라도 표현식의 본문은 반복기로 생각할 수 있습니다.
// 반복기는 아무것도 생성하지 않을 수 있으므로 해당 반복기는 배열이나 루프의 도메인과 다른 수의 항목을 생성하며 이는 허용되지 않습니다.

/*
클래스
*/
// 클래스는 C++ 및 Java의 클래스와 유사하며 힙에 할당됩니다.
class MyClass {

// 멤버 변수
  var memberInt : int;
  var memberBool : bool = true;

// 기본적으로 이니셜라이저를 정의하지 않는 모든 클래스는 컴파일러에서 생성된 이니셜라이저를 가져옵니다. 이 이니셜라이저는 필드당 하나의 인수를 가지며 필드의 초기 값을 인수의 기본값으로 사용합니다.
// 또는 사용자는 다음 주석 처리된 루틴에 표시된 대로 이니셜라이저를 수동으로 정의할 수 있습니다.
//
/*       // proc init(val : real) {
      //   this.memberInt = ceil(val): int;
      // }
*/

// 명시적으로 정의된 디이니셜라이저.
// 우리가 작성하지 않았다면 컴파일러에서 생성된 디이니셜라이저를 얻었을 것입니다. 이 디이니셜라이저는 빈 본문을 가집니다.
  proc deinit() {
    writeln("MyClass deinitializer called ", (this.memberInt, this.memberBool));
  }

// 클래스 메서드.
  proc setMemberInt(val: int) {
    this.memberInt = val;
  }

  proc setMemberBool(val: bool) {
    this.memberBool = val;
  }

  proc getMemberInt(): int{
    return this.memberInt;
  }

  proc getMemberBool(): bool {
    return this.memberBool;
  }
} // MyClass 끝

// 컴파일러에서 생성된 이니셜라이저를 호출하고 memberBool에 기본값을 사용합니다.
{
  var myObject = new owned MyClass(10);
      myObject = new owned MyClass(memberInt = 10); // 동일
  writeln(myObject.getMemberInt());

  // 동일하지만 memberBool 값을 명시적으로 제공합니다.
  var myDiffObject = new owned MyClass(-1, true);
      myDiffObject = new owned MyClass(memberInt = -1,
                                       memberBool = true); // 동일
  writeln(myDiffObject);

  // 유사하지만 memberInt의 기본값에 의존하고 memberBool을 전달합니다.
  var myThirdObject = new owned MyClass(memberBool = true);
  writeln(myThirdObject);

  // 위의 사용자 정의 이니셜라이저가 주석 처리되지 않았다면 다음 호출을 할 수 있습니다:
  //
  /*         // var myOtherObject = new MyClass(1.95);
        //     myOtherObject = new MyClass(val = 1.95);
        // writeln(myOtherObject.getMemberInt());
  */

  // 클래스에 대한 연산자를 정의할 수도 있지만
  // 정의는 클래스 정의 외부에 있어야 합니다.
  proc +(A : MyClass, B : MyClass) : owned MyClass {
    return
      new owned MyClass(memberInt = A.getMemberInt() + B.getMemberInt(),
                        memberBool = A.getMemberBool() || B.getMemberBool());
  }

  var plusObject = myObject + myDiffObject;
  writeln(plusObject);

  // 객체 소멸: deinit() 루틴을 호출하고 메모리를 해제합니다.
  // ``unmanaged`` 변수는 ``delete``를 호출해야 합니다.
  // ``owned`` 변수는 범위를 벗어날 때 소멸됩니다.
}

// 클래스는 하나 이상의 부모 클래스에서 상속할 수 있습니다.
class MyChildClass : MyClass {
  var memberComplex: complex;
}

// 제네릭 클래스의 예입니다.
class GenericClass {
  type classType;
  var classDomain: domain(1);
  var classArray: [classDomain] classType;

// 명시적 초기화 프로그램.
  proc init(type classType, elements : int) {
    this.classType = classType;
    this.classDomain = {1..elements};
    // 모든 제네릭 및 const 필드는 슈퍼클래스 초기화 프로그램 호출 전에 "1단계"에서 초기화해야 합니다.
  }

// 복사 스타일 초기화 프로그램.
// 참고: 첫 번째 인수의 유형인 기본값을 가진 유형 인수를 포함합니다.
// 이렇게 하면 초기화 프로그램이 다른 유형의 클래스를 복사하고 즉시 캐스팅할 수 있습니다.
  proc init(other : GenericClass(?),
            type classType = other.classType) {
    this.classType = classType;
    this.classDomain = other.classDomain;
    this.classArray = for o in other do o: classType;  // 복사 및 캐스팅
  }

// GenericClass에 대괄호 표기법을 정의합니다.
// 객체가 일반 배열처럼 동작할 수 있도록 합니다.
// 즉, ``objVar[i]`` 또는 ``objVar(i)``
  proc this(i : int) ref : classType {
    return this.classArray[i];
  }

// 클래스에 대한 암시적 반복기를 정의합니다.
// 배열에서 루프에 값을 생성합니다.
// 즉, ``for i in objVar do ...``
  iter these() ref : classType {
    for i in this.classDomain do
      yield this[i];
  }
} // GenericClass 끝

// 클래스의 소유 인스턴스 할당
var realList = new owned GenericClass(real, 10);

// 정의한 대괄호 표기법을 사용하여 객체의 멤버 배열에 할당할 수 있습니다.
for i in realList.classDomain do realList[i] = i + 1.0;

// 정의한 반복기를 사용하여 목록의 값을 반복할 수 있습니다.
for value in realList do write(value, ", ");
Writeln();

// 복사 초기화 프로그램을 사용하여 realList의 복사본을 만듭니다.
var copyList = new owned GenericClass(realList);
for value in copyList do write(value, ", ");
Writeln();

// realList의 복사본을 만들고 복사 초기화 프로그램을 사용하여 유형을 변경합니다.
var copyNewTypeList = new owned GenericClass(realList, int);
for value in copyNewTypeList do write(value, ", ");
Writeln();


/*
모듈
*/

// 모듈은 Chapel이 네임스페이스를 관리하는 방법입니다.
// 이러한 모듈을 포함하는 파일은 모듈 이름 뒤에 이름을 지정할 필요가 없습니다(Java에서와 같이). 그러나 파일은 암시적으로 모듈 이름을 지정합니다.
// 예를 들어, 이 파일은 암시적으로 ``learnChapelInYMinutes`` 모듈의 이름을 지정합니다.

module OurModule {

// 다른 모듈 내에서 모듈을 사용할 수 있습니다.
// Time은 표준 모듈 중 하나입니다.
  use Time;

// 병렬 처리 섹션에서 이 프로시저를 사용합니다.
  proc countdown(seconds: int) {
    for i in 1..seconds by -1 {
      writeln(i);
      sleep(1);
    }
  }

// 임의로 깊은 모듈 중첩을 만들 수 있습니다.
// 즉, OurModule의 하위 모듈
  module ChildModule {
    proc foo() {
      writeln("ChildModule.foo()")
    }
  }

  module SiblingModule {
    proc foo() {
      writeln("SiblingModule.foo()")
    }
  }
} // OurModule 끝

// ``OurModule``을 사용하면 사용하는 모든 모듈도 사용됩니다.
// ``OurModule``이 ``Time``을 사용하므로 우리도 ``Time``을 사용합니다.
use OurModule;

// 이 시점에서 ``ChildModule`` 또는 ``SiblingModule``을 사용하지 않았으므로 해당 기호(즉, ``foo``)를 사용할 수 없습니다. 그러나 모듈 이름은 사용할 수 있으며 이를 통해 ``foo()``를 명시적으로 호출할 수 있습니다.
SiblingModule.foo();
OurModule.ChildModule.foo();

// 이제 ``ChildModule``을 사용하여 정규화되지 않은 호출을 활성화합니다.
use ChildModule;
foo();

/*
병렬성
*/

// 다른 언어에서는 병렬 처리가 일반적으로 복잡한 라이브러리와 이상한 클래스 구조 계층으로 수행됩니다.
// Chapel은 언어에 내장되어 있습니다.

// 주 프로시저를 선언할 수 있지만 주 프로시저 위의 모든 코드는 여전히 실행됩니다.
proc main() {

// ``begin`` 문은 해당 문의 본문을 하나의 새 작업으로 분리합니다.
// ``sync`` 문은 주 작업의 진행이 자식이 다시 동기화될 때까지 진행되지 않도록 합니다.

  sync {
    begin { // 새 작업 본문의 시작
      var a = 0;
      for i in 1..1000 do a += 1;
      writeln("Done: ", a);
    } // 새 작업 본문의 끝
    writeln("spun off a task!");
  }
  writeln("Back together");

  proc printFibb(n: int) {
    writeln("fibonacci(",n,") = ", fibonacci(n));
  }

// ``cobegin`` 문은 본문의 각 문을 하나의 새 작업으로 분리합니다. 여기서 각 문의 인쇄가 어떤 순서로든 발생할 수 있음을 주목하십시오.
  cobegin {
    printFibb(20); // 새 작업
    printFibb(10); // 새 작업
    printFibb(5);  // 새 작업
    {
      // 이것은 중첩된 문 본문이므로 부모 문에 대한 단일 문이며 단일 작업으로 실행됩니다.
      writeln("this gets");
      writeln("executed as");
      writeln("a whole");
    }
  }

// ``coforall`` 루프는 각 반복에 대해 새 작업을 생성합니다.
// 다시 말하지만, 인쇄가 어떤 순서로든 발생한다는 것을 알 수 있습니다.
// 참고: ``coforall``은 작업을 생성하는 데만 사용해야 합니다!
// 구조를 반복하는 데 사용하는 것은 매우 나쁜 생각입니다!
  var num_tasks = 10; // 원하는 작업 수
  coforall taskID in 1..num_tasks {
    writeln("Hello from task# ", taskID);
  }

// ``forall`` 루프는 또 다른 병렬 루프이지만, 더 적은 수의 작업, 특히 ``--dataParTasksPerLocale=`` 수의 작업만 생성합니다.
  forall i in 1..100 {
    write(i, ", ");
  }
  writeln();

// 여기서 순서대로 된 섹션이 있고 그 뒤에 따르지 않는 섹션이 있음을 알 수 있습니다(예: 1, 2, 3, 7, 8, 9, 4, 5, 6,). 
// 이것은 각 작업이 1..10 범위의 청크(1..3, 4..6 또는 7..9)를 직렬로 수행하지만 각 작업은 병렬로 발생하기 때문입니다. 결과는 컴퓨터 및 구성에 따라 다를 수 있습니다.

// ``forall`` 및 ``coforall`` 루프 모두에 대해 부모 작업의 실행은 모든 자식이 동기화될 때까지 계속되지 않습니다.

// ``forall`` 루프는 배열에 대한 병렬 반복에 특히 유용합니다.
// (가지고 있는 코어 수에 따라) 병렬 루프가 직렬 루프보다 더 빨리 진행되었음을 알 수 있습니다.
// 이 문이 런타임 오류를 나타내는 이유를 이해하는 데 매우 중요합니다.

/*
      var iterArray : [1..10] int = [i in 1..10] if (i % 2 == 1) then i;
*/

// 배열의 도메인과 루프 표현식의 크기가 같더라도 표현식의 본문은 반복기로 생각할 수 있습니다.
// 반복기는 아무것도 생성하지 않을 수 있으므로 해당 반복기는 배열이나 루프의 도메인과 다른 수의 항목을 생성하며 이는 허용되지 않습니다.

/*
클래스
*/
// 클래스는 C++ 및 Java의 클래스와 유사하며 힙에 할당됩니다.
class MyClass {

// 멤버 변수
  var memberInt : int;
  var memberBool : bool = true;

// 기본적으로 이니셜라이저를 정의하지 않는 모든 클래스는 컴파일러에서 생성된 이니셜라이저를 가져옵니다. 이 이니셜라이저는 필드당 하나의 인수를 가지며 필드의 초기 값을 인수의 기본값으로 사용합니다.
// 또는 사용자는 다음 주석 처리된 루틴에 표시된 대로 이니셜라이저를 수동으로 정의할 수 있습니다.
//
/*       // proc init(val : real) {
      //   this.memberInt = ceil(val): int;
      // }
*/

// 명시적으로 정의된 디이니셜라이저.
// 우리가 작성하지 않았다면 컴파일러에서 생성된 디이니셜라이저를 얻었을 것입니다. 이 디이니셜라이저는 빈 본문을 가집니다.
  proc deinit() {
    writeln("MyClass deinitializer called ", (this.memberInt, this.memberBool));
  }

// 클래스 메서드.
  proc setMemberInt(val: int) {
    this.memberInt = val;
  }

  proc setMemberBool(val: bool) {
    this.memberBool = val;
  }

  proc getMemberInt(): int{
    return this.memberInt;
  }

  proc getMemberBool(): bool {
    return this.memberBool;
  }
} // MyClass 끝

// 컴파일러에서 생성된 이니셜라이저를 호출하고 memberBool에 기본값을 사용합니다.
{
  var myObject = new owned MyClass(10);
      myObject = new owned MyClass(memberInt = 10); // 동일
  writeln(myObject.getMemberInt());

  // 동일하지만 memberBool 값을 명시적으로 제공합니다.
  var myDiffObject = new owned MyClass(-1, true);
      myDiffObject = new owned MyClass(memberInt = -1,
                                       memberBool = true); // 동일
  writeln(myDiffObject);

  // 유사하지만 memberInt의 기본값에 의존하고 memberBool을 전달합니다.
  var myThirdObject = new owned MyClass(memberBool = true);
  writeln(myThirdObject);

  // 위의 사용자 정의 이니셜라이저가 주석 처리되지 않았다면 다음 호출을 할 수 있습니다:
  //
  /*         // var myOtherObject = new MyClass(1.95);
        //     myOtherObject = new MyClass(val = 1.95);
        // writeln(myOtherObject.getMemberInt());
  */

  // 클래스에 대한 연산자를 정의할 수도 있지만
  // 정의는 클래스 정의 외부에 있어야 합니다.
  proc +(A : MyClass, B : MyClass) : owned MyClass {
    return
      new owned MyClass(memberInt = A.getMemberInt() + B.getMemberInt(),
                        memberBool = A.getMemberBool() || B.getMemberBool());
  }

  var plusObject = myObject + myDiffObject;
  writeln(plusObject);

  // 객체 소멸: deinit() 루틴을 호출하고 메모리를 해제합니다.
  // ``unmanaged`` 변수는 ``delete``를 호출해야 합니다.
  // ``owned`` 변수는 범위를 벗어날 때 소멸됩니다.
}

// 클래스는 하나 이상의 부모 클래스에서 상속할 수 있습니다.
class MyChildClass : MyClass {
  var memberComplex: complex;
}

// 제네릭 클래스의 예입니다.
class GenericClass {
  type classType;
  var classDomain: domain(1);
  var classArray: [classDomain] classType;

// 명시적 초기화 프로그램.
  proc init(type classType, elements : int) {
    this.classType = classType;
    this.classDomain = {1..elements};
    // 모든 제네릭 및 const 필드는 슈퍼클래스 초기화 프로그램 호출 전에 "1단계"에서 초기화해야 합니다.
  }

// 복사 스타일 초기화 프로그램.
// 참고: 첫 번째 인수의 유형인 기본값을 가진 유형 인수를 포함합니다.
// 이렇게 하면 초기화 프로그램이 다른 유형의 클래스를 복사하고 즉시 캐스팅할 수 있습니다.
  proc init(other : GenericClass(?),
            type classType = other.classType) {
    this.classType = classType;
    this.classDomain = other.classDomain;
    this.classArray = for o in other do o: classType;  // 복사 및 캐스팅
  }

// GenericClass에 대괄호 표기법을 정의합니다.
// 객체가 일반 배열처럼 동작할 수 있도록 합니다.
// 즉, ``objVar[i]`` 또는 ``objVar(i)``
  proc this(i : int) ref : classType {
    return this.classArray[i];
  }

// 클래스에 대한 암시적 반복기를 정의합니다.
// 배열에서 루프에 값을 생성합니다.
// 즉, ``for i in objVar do ...``
  iter these() ref : classType {
    for i in this.classDomain do
      yield this[i];
  }
} // GenericClass 끝

// 클래스의 소유 인스턴스 할당
var realList = new owned GenericClass(real, 10);

// 정의한 대괄호 표기법을 사용하여 객체의 멤버 배열에 할당할 수 있습니다.
for i in realList.classDomain do realList[i] = i + 1.0;

// 정의한 반복기를 사용하여 목록의 값을 반복할 수 있습니다.
for value in realList do write(value, ", ");
Writeln();

// 복사 초기화 프로그램을 사용하여 realList의 복사본을 만듭니다.
var copyList = new owned GenericClass(realList);
for value in copyList do write(value, ", ");
Writeln();

// realList의 복사본을 만들고 복사 초기화 프로그램을 사용하여 유형을 변경합니다.
var copyNewTypeList = new owned GenericClass(realList, int);
for value in copyNewTypeList do write(value, ", ");
Writeln();


/*
모듈
*/

// 모듈은 Chapel이 네임스페이스를 관리하는 방법입니다.
// 이러한 모듈을 포함하는 파일은 모듈 이름 뒤에 이름을 지정할 필요가 없습니다(Java에서와 같이). 그러나 파일은 암시적으로 모듈 이름을 지정합니다.
// 예를 들어, 이 파일은 암시적으로 ``learnChapelInYMinutes`` 모듈의 이름을 지정합니다.

module OurModule {

// 다른 모듈 내에서 모듈을 사용할 수 있습니다.
// Time은 표준 모듈 중 하나입니다.
  use Time;

// 병렬 처리 섹션에서 이 프로시저를 사용합니다.
  proc countdown(seconds: int) {
    for i in 1..seconds by -1 {
      writeln(i);
      sleep(1);
    }
  }

// 임의로 깊은 모듈 중첩을 만들 수 있습니다.
// 즉, OurModule의 하위 모듈
  module ChildModule {
    proc foo() {
      writeln("ChildModule.foo()")
    }
  }

  module SiblingModule {
    proc foo() {
      writeln("SiblingModule.foo()")
    }
  }
} // OurModule 끝

// ``OurModule``을 사용하면 사용하는 모든 모듈도 사용됩니다.
// ``OurModule``이 ``Time``을 사용하므로 우리도 ``Time``을 사용합니다.
use OurModule;

// 이 시점에서 ``ChildModule`` 또는 ``SiblingModule``을 사용하지 않았으므로 해당 기호(즉, ``foo``)를 사용할 수 없습니다. 그러나 모듈 이름은 사용할 수 있으며 이를 통해 ``foo()``를 명시적으로 호출할 수 있습니다.
SiblingModule.foo();
OurModule.ChildModule.foo();

// 이제 ``ChildModule``을 사용하여 정규화되지 않은 호출을 활성화합니다.
use ChildModule;
foo();

/*
병렬성
*/

// 다른 언어에서는 병렬 처리가 일반적으로 복잡한 라이브러리와 이상한 클래스 구조 계층으로 수행됩니다.
// Chapel은 언어에 내장되어 있습니다.

// 주 프로시저를 선언할 수 있지만 주 프로시저 위의 모든 코드는 여전히 실행됩니다.
proc main() {

// ``begin`` 문은 해당 문의 본문을 하나의 새 작업으로 분리합니다.
// ``sync`` 문은 주 작업의 진행이 자식이 다시 동기화될 때까지 진행되지 않도록 합니다.

  sync {
    begin { // 새 작업 본문의 시작
      var a = 0;
      for i in 1..1000 do a += 1;
      writeln("Done: ", a);
    } // 새 작업 본문의 끝
    writeln("spun off a task!");
  }
  writeln("Back together");

  proc printFibb(n: int) {
    writeln("fibonacci(",n,") = ", fibonacci(n));
  }

// ``cobegin`` 문은 본문의 각 문을 하나의 새 작업으로 분리합니다. 여기서 각 문의 인쇄가 어떤 순서로든 발생할 수 있음을 주목하십시오.
  cobegin {
    printFibb(20); // 새 작업
    printFibb(10); // 새 작업
    printFibb(5);  // 새 작업
    {
      // 이것은 중첩된 문 본문이므로 부모 문에 대한 단일 문이며 단일 작업으로 실행됩니다.
      writeln("this gets");
      writeln("executed as");
      writeln("a whole");
    }
  }

// ``coforall`` 루프는 각 반복에 대해 새 작업을 생성합니다.
// 다시 말하지만, 인쇄가 어떤 순서로든 발생한다는 것을 알 수 있습니다.
// 참고: ``coforall``은 작업을 생성하는 데만 사용해야 합니다!
// 구조를 반복하는 데 사용하는 것은 매우 나쁜 생각입니다!
  var num_tasks = 10; // 원하는 작업 수
  coforall taskID in 1..num_tasks {
    writeln("Hello from task# ", taskID);
  }

// ``forall`` 루프는 또 다른 병렬 루프이지만, 더 적은 수의 작업, 특히 ``--dataParTasksPerLocale=`` 수의 작업만 생성합니다.
  forall i in 1..100 {
    write(i, ", ");
  }
  writeln();

// 여기서 순서대로 된 섹션이 있고 그 뒤에 따르지 않는 섹션이 있음을 알 수 있습니다(예: 1, 2, 3, 7, 8, 9, 4, 5, 6,). 
// 이것은 각 작업이 1..10 범위의 청크(1..3, 4..6 또는 7..9)를 직렬로 수행하지만 각 작업은 병렬로 발생하기 때문입니다. 결과는 컴퓨터 및 구성에 따라 다를 수 있습니다.

// ``forall`` 및 ``coforall`` 루프 모두에 대해 부모 작업의 실행은 모든 자식이 동기화될 때까지 계속되지 않습니다.

// ``forall`` 루프는 배열에 대한 병렬 반복에 특히 유용합니다.
// 병렬 루프가 직렬 루프보다 얼마나 빠른지 확인하기 위해 실험을 실행해 보겠습니다.
  use Time; // Timer 객체를 사용하려면 Time 모듈을 가져옵니다.
  var timer: Timer;
  var myBigArray: [{1..4000,1..4000}] real; // 쓸 큰 배열

// 직렬 실험:
  timer.start(); // 타이머 시작
  for (x,y) in myBigArray.domain { // 직렬 반복
    myBigArray[x,y] = (x:real) / (y:real);
  }
  timer.stop(); // 타이머 중지
  writeln("Serial: ", timer.elapsed()); // 경과 시간 인쇄
  timer.clear(); // 병렬 루프를 위해 타이머 지우기

// 병렬 실험:
  timer.start(); // 타이머 시작
  forall (x,y) in myBigArray.domain { // 병렬 반복
    myBigArray[x,y] = (x:real) / (y:real);
  }
  timer.stop(); // 타이머 중지
  writeln("Parallel: ", timer.elapsed()); // 경과 시간 인쇄
  timer.clear();

// (가지고 있는 코어 수에 따라) 병렬 루프가 직렬 루프보다 더 빨리 진행되었음을 알 수 있습니다.

// 이전에 설명한 대괄호 스타일 루프 표현식은 암시적으로 ``forall`` 루프를 사용합니다.
  [val in myBigArray] val = 1 / val; // 병렬 연산

// 많은 언어에서 일반적인 원자 변수는 작업이 중단되지 않는 변수입니다. 따라서 여러 스레드가 원자 변수를 수정할 수 있으며 해당 값이 안전하다는 것을 알 수 있습니다.
// Chapel 원자 변수는 ``bool``, ``int``, ``uint`` 및 ``real`` 유형일 수 있습니다.
  var uranium: atomic int;
  uranium.write(238);      // 변수를 원자적으로 씁니다.
  writeln(uranium.read()); // 변수를 원자적으로 읽습니다.

// 원자 연산은 함수로 설명되므로 자신만의 함수를 정의할 수 있습니다.
  uranium.sub(3); // 변수를 원자적으로 뺍니다.
  writeln(uranium.read());

  var replaceWith = 239;
  var was = uranium.exchange(replaceWith);
  writeln("uranium was ", was, " but is now ", replaceWith);

  var isEqualTo = 235;
  if uranium.compareAndSwap(isEqualTo, replaceWith) {
    writeln("uranium was equal to ", isEqualTo,
             " so replaced value with ", replaceWith);
  } else {
    writeln("uranium was not equal to ", isEqualTo,
             " so value stays the same...  whatever it was");
  }

  sync {
    begin { // 리더 작업
      writeln("Reader: waiting for uranium to be ", isEqualTo);
      uranium.waitFor(isEqualTo);
      writeln("Reader: uranium was set (by someone) to ", isEqualTo);
    }

    begin { // 라이터 작업
      writeln("Writer: will set uranium to the value ", isEqualTo, " in...");
      countdown(3);
      uranium.write(isEqualTo);
    }
  }

// ``sync`` 변수에는 두 가지 상태가 있습니다: 비어 있음과 가득 참.
// 비어 있는 변수를 읽거나 가득 찬 변수를 쓰면 변수가 다시 가득 차거나 비워질 때까지 기다립니다.
  var someSyncVar$: sync int; // varName$는 법이 아닌 관례입니다.
  sync {
    begin { // 리더 작업
      writeln("Reader: waiting to read.");
      var read_sync = someSyncVar$;
      writeln("Reader: value is ", read_sync);
    }

    begin { // 라이터 작업
      writeln("Writer: will write in...");
      countdown(3);
      someSyncVar$ = 123;
    }
  }

// ``single`` 변수는 한 번만 쓸 수 있습니다. 쓰지 않은 ``single``을 읽으면 대기하지만 변수에 값이 있으면 무기한 읽을 수 있습니다.
  var someSingleVar$: single int; // varName$는 법이 아닌 관례입니다.
  sync {
    begin { // 리더 작업
      writeln("Reader: waiting to read.");
      for i in 1..5 {
        var read_single = someSingleVar$;
        writeln("Reader: iteration ", i,", and the value is ", read_single);
      }
    }

    begin { // 라이터 작업
      writeln("Writer: will write in...");
      countdown(3);
      someSingleVar$ = 5; // 처음이자 마지막 쓰기.
    }
  }

// 다음은 원자 및 ``sync`` 변수를 사용하여 카운트다운 뮤텍스(멀티플렉서라고도 함)를 만드는 예입니다.
  var count: atomic int; // 카운터
  var lock$: sync bool;   // 뮤텍스 잠금

  count.write(2);       // 한 번에 두 개의 작업만 허용합니다.
  lock$.writeXF(true);  // lock$을 가득 참(잠금 해제)으로 설정합니다.
  // 참고: 값은 실제로 중요하지 않고 상태만 중요합니다.
  // (가득 참:잠금 해제 / 비어 있음:잠김)
  // 또한 writeXF()는 상태(X)에 관계없이 동기화 변수를 채웁니다(F).

  coforall task in 1..5 { // 작업 생성
    // 장벽 생성
    do {
      lock$;                 // lock$ 읽기(대기)
    } while (count.read() < 1); // 자리가 생길 때까지 계속 대기

    count.sub(1);          // 카운터 감소
    lock$.writeXF(true); // lock$을 가득 참(신호)으로 설정

    // 실제 '작업'
    writeln("Task #", task, " doing work.");
    sleep(2);

    count.add(1);        // 카운터 증가
    lock$.writeXF(true); // lock$을 가득 참(신호)으로 설정
  }

// 스캔 및 축소를 사용하여 전체 배열에 대해 ``+ * & | ^ && || min max minloc maxloc`` 연산을 정의할 수 있습니다.
// 축소는 전체 배열에 대해 연산을 적용하고 스칼라 값을 반환합니다.
  var listOfValues: [1..10] int = [15,57,354,36,45,15,456,8,678,2];
  var sumOfValues = + reduce listOfValues;
  var maxValue = max reduce listOfValues; // 'max'는 최대값만 제공합니다.

// ``maxloc``은 최대값과 최대값의 인덱스를 제공합니다.
// 참고: zip 반복기를 사용하여 배열과 도메인을 함께 압축해야 합니다.
  var (theMaxValue, idxOfMax) = maxloc reduce zip(listOfValues,
                                                  listOfValues.domain);

  writeln((sumOfValues, maxValue, idxOfMax, listOfValues[idxOfMax]));

// 스캔은 연산을 점진적으로 적용하고 해당 인덱스에서 연산의 값이 있는 배열을 반환합니다. 이 연산은 배열을 ``array.domain.low``에서 ``array.domain.high``로 진행하면서 수행됩니다.
  var runningSumOfValues = + scan listOfValues;
  var maxScan = max scan listOfValues;
  writeln(runningSumOfValues);
  writeln(maxScan);
} // main() 끝

## 이 튜토리얼은 누구를 위한 것입니까?

이 튜토리얼은 밧줄이 어떤 섬유 혼합물로 만들어졌는지, 어떻게 땋았는지, 또는 땋기 구성이 서로 어떻게 다른지에 대해 듣지 않고 채플의 요령을 배우고 싶은 사람들을 위한 것입니다. 놀랍도록 성능이 좋은 코드를 개발하는 방법을 가르쳐주지는 않으며, 완전하지도 않습니다. 자세한 내용은 [언어 사양](https://chapel-lang.org/docs/latest/language/spec.html) 및 [모듈 문서](https://chapel-lang.org/docs/latest/)를 참조하십시오.

때때로 여기와 [Chapel 사이트](https://chapel-lang.org)를 다시 확인하여 더 많은 주제가 추가되었는지 또는 더 많은 튜토리얼이 만들어졌는지 확인하십시오.

### 이 튜토리얼에 부족한 점:

* [표준 모듈](https://chapel-lang.org/docs/latest/modules/standard.html) 설명
* 다중 로케일(분산 메모리 시스템)
* 레코드
* 병렬 반복기

## 귀하의 의견, 질문 및 발견은 개발자에게 중요합니다!

Chapel 언어는 아직 활발하게 개발 중이므로 성능 및 언어 기능에 가끔 문제가 발생합니다. Chapel 개발팀에 발생하는 문제나 보고 싶은 기능에 대한 정보를 많이 제공할수록 언어가 더 좋아집니다.
개발자와 상호 작용하는 방법에는 여러 가지가 있습니다:

* [Gitter 채팅](https://gitter.im/chapel-lang/chapel)
* [sourceforge 이메일 목록](https://sourceforge.net/p/chapel/mailman)

컴파일러 개발에 정말 관심이 있거나 프로젝트에 기여하고 싶다면 [마스터 GitHub 리포지토리](https://github.com/chapel-lang/chapel)를 확인하십시오.
[Apache 2.0 라이선스](http://www.apache.org/licenses/LICENSE-2.0)에 따라 제공됩니다.

## 컴파일러 설치

[공식 Chapel 문서에는 Chapel 컴파일러를 다운로드하고 컴파일하는 방법이 자세히 설명되어 있습니다.](https://chapel-lang.org/docs/usingchapel/QUICKSTART.html)

Chapel은 일반적인 'nix 머신(및 cygwin)에 빌드하고 설치할 수 있습니다.
[최신 릴리스 버전 다운로드](https://github.com/chapel-lang/chapel/releases/)하고 다음과 같이 간단합니다.

1. `tar -xvf chapel-<VERSION>.tar.gz`
2. `cd chapel-<VERSION>`
3. `source util/setchplenv.bash # 또는 .sh 또는 .csh 또는 .fish`
4. `make`
5. `make check # 선택 사항`

터미널이 시작될 때마다 Chapel 디렉토리(`$CHPL_HOME`) 내에서 `source util/setchplenv.EXT`를 실행해야 하므로 시작 시 실행될 스크립트(.bashrc 등)에 해당 명령을 넣는 것이 좋습니다.

Chapel은 Homebrew를 사용하여 macOS에 쉽게 설치할 수 있습니다.

1. `brew update`
2. `brew install chapel`

## 코드 컴파일

다른 컴파일러와 같이 빌드합니다:

`chpl myFile.chpl -o myExe`

주목할 만한 인수:

* `--fast`: 여러 최적화를 활성화하고 배열 경계 검사를 비활성화합니다. 애플리케이션이 안정적일 때만 활성화해야 합니다.
* `--set <Symbol Name>=<Value>`: 컴파일 타임에 config param `<Symbol Name>`을 `<Value>`로 설정합니다.
* `--main-module <Module Name>`: 모듈 `<Module Name>`에 있는 main() 프로시저를 실행 파일의 main으로 사용합니다.
* `--module-dir <Directory>`: 모듈 검색 경로에 `<Directory>`를 포함합니다.

```