---
name: Pascal
filename: learnpascal.pas
contributors:
    - ["Ganesha Danu", "http://github.com/blinfoldking"]
    - ["Keith Miyake", "https://github.com/kaymmm"]
---


>파스칼은 1968-69년에 니클라우스 비르트가 설계하고 1970년에 발표한 명령형 및 절차적 프로그래밍 언어로, 구조적 프로그래밍과 데이터 구조화를 사용하여 좋은 프로그래밍 습관을 장려하기 위한 작고 효율적인 언어입니다. 프랑스의 수학자, 철학자, 물리학자인 블레즈 파스칼을 기리기 위해 명명되었습니다.
출처 : [위키백과](https://en.wikipedia.org/wiki/Pascal_(programming_language))



파스칼 프로그램을 컴파일하고 실행하려면 무료 파스칼 컴파일러를 사용할 수 있습니다. [여기서 다운로드](https://www.freepascal.org/)

```pascal
//파스칼 프로그램의 구조
//이것은 주석입니다
{
    이것은
    여러 줄 주석입니다
}

//프로그램 이름
program learn_pascal; //<-- 세미콜론을 잊지 마세요

const
    {
        상수 값을 선언해야 하는 곳입니다
    }
type
    {
        사용자 정의 데이터 타입을 선언해야 하는 곳입니다
    }
var
    {
        변수를 선언해야 하는 곳입니다
    }

//주 프로그램 영역
begin
    {
        명령을 선언하는 영역
    }
end. // 주 프로그램 영역의 끝에는 "." 기호가 필요합니다
```

```pascal
//변수를 선언할 때
//이렇게 할 수 있습니다
var a:integer;
var b:integer;
//또는 이렇게
var
    a : integer;
    b : integer;
//또는 이렇게
var a,b : integer;
```

```pascal
program Learn_More;
//데이터 타입과 그 연산에 대해 알아봅시다

const
    PI = 3.141592654;
    GNU = 'GNU''s Not Unix';
        // 상수는 관례적으로 대문자를 사용하여 명명합니다
        // 그 값은 고정되어 있으며 런타임 중에 변경할 수 없습니다
        // 모든 표준 데이터 타입(정수, 실수, 불리언, 문자, 문자열)을 가질 수 있습니다

type
    ch_array : array [0..255] of char;
        // 배열은 길이와 데이터 타입을 지정하는 새로운 '타입'입니다
        // 이것은 255개의 문자를 포함하는 새로운 데이터 타입을 정의합니다
        // (이것은 기능적으로 string[256] 변수와 동일합니다)
    md_array : array of array of integer;
        // 중첩 배열은 다차원 배열과 동일합니다
        // 동적으로 크기가 조정되는 길이가 0인 배열을 정의할 수 있습니다
        // 이것은 2차원 정수 배열입니다

//변수 선언
var
    int, c, d  : integer;
           // 정수 숫자를 포함하는 세 개의 변수
           // 정수는 16비트이며 [-32,768..32,767] 범위로 제한됩니다
    r    : real;
           // 실수 데이터 타입을 포함하는 변수
           // 실수는 [3.4E-38..3.4E38] 범위일 수 있습니다
    bool : boolean;
           // 불리언(True/False) 값을 포함하는 변수
    ch   : char;
           // 문자 값을 포함하는 변수
           // 문자 변수는 8비트 데이터 타입으로 저장되므로 UTF가 없습니다
    str  : string;
           // 문자열 값을 포함하는 비표준 변수
           // 문자열은 대부분의 파스칼 컴파일러에 포함된 확장 기능입니다
           // 기본 길이가 255인 문자 배열로 저장됩니다.
    s    : string[50];
           // 최대 길이가 50자인 문자열.
           // 메모리 사용량을 최소화하기 위해 문자열 길이를 지정할 수 있습니다
    my_str: ch_array;
           // 사용자 정의 타입의 변수를 선언할 수 있습니다
    my_2d : md_array;
           // 동적으로 크기가 조정되는 배열은 사용하기 전에 크기를 지정해야 합니다.

    // 추가 정수 데이터 타입
    b    : byte;     // 범위 [0..255]
    shi  : shortint; // 범위 [-128..127]
    smi  : smallint; // 범위 [-32,768..32,767] (표준 정수)
    w    : word;     // 범위 [0..65,535]
    li   : longint;  // 범위 [-2,147,483,648..2,147,483,647]
    lw   : longword; // 범위 [0..4,294,967,295]
    c    : cardinal; // longword
    i64  : int64;    // 범위 [-9223372036854775808..9223372036854775807]
    qw   : qword;    // 범위 [0..18,446,744,073,709,551,615]

    // 추가 실수 타입
    rr   : real;     // 범위는 플랫폼에 따라 다름 (예: 8비트, 16비트 등)
    rs   : single;   // 범위 [1.5E-45..3.4E38]
    rd   : double;   // 범위 [5.0E-324 .. 1.7E308]
    re   : extended; // 범위 [1.9E-4932..1.1E4932]
    rc   : comp;     // 범위 [-2E64+1 .. 2E63-1]

Begin
    int := 1;// 변수에 값을 할당하는 방법
    r   := 3.14;
    ch  := 'a';
    str := 'apple';
    bool := true;
    //파스칼은 대소문자를 구분하지 않는 언어입니다
    //산술 연산
    int := 1 + 1; // int = 2, 이전 할당을 덮어씁니다
    int := int + 1; // int = 2 + 1 = 3;
    int := 4 div 2; //int = 2, 결과가 버림되는 나눗셈 연산
    int := 3 div 2; //int = 1
    int := 1 div 2; //int = 0

    bool := true or false; // bool = true
    bool := false and true; // bool = false
    bool := true xor true; // bool = false

    r := 3 / 2; // 실수를 위한 나눗셈 연산자
    r := int; // 정수를 실수 변수에 할당할 수 있지만 그 반대는 안 됩니다

    c := str[1]; // str의 첫 글자를 c에 할당
    str := 'hello' + 'world'; //문자열 결합

    my_str[0] := 'a'; // 배열 할당에는 인덱스가 필요합니다

    setlength(my_2d,10,10); // 동적으로 크기가 조정되는 배열 초기화: 10×10 배열
    for c := 0 to 9 do // 배열은 0에서 시작하여 길이-1에서 끝납니다
        for d := 0 to 9 do // for 루프 카운터는 선언된 변수여야 합니다
        my_2d[c,d] := c * d;
          // 단일 대괄호 세트로 다차원 배열 주소 지정

End.
```

```pascal
program Functional_Programming;

Var
    i, dummy : integer;

function factorial_recursion(const a: integer) : integer;
{ 정수 매개변수 a의 계승을 재귀적으로 계산합니다 }

// 함수 내에서 지역 변수 선언
// 예:
// Var
//    local_a : integer;

Begin
    If a >= 1 Then
    // 함수 이름에 값을 할당하여 함수에서 값을 반환합니다
        factorial_recursion := a * factorial_recursion(a-1)
    Else
        factorial_recursion := 1;
End; // End 문 뒤에 세미콜론을 사용하여 함수를 종료합니다.

procedure get_integer(var i : integer; dummy : integer);
{ 사용자 입력을 받아 정수 매개변수 i에 저장합니다.
  'var'가 앞에 붙은 매개변수는 가변적이므로 매개변수 외부에서
  값이 변경될 수 있습니다. 'dummy'와 같은 값 매개변수('var' 없음)는
  정적이며 함수/프로시저 범위 내에서 변경해도
  매개변수로 전달된 변수에 영향을 미치지 않습니다 }

Begin
    write('Enter an integer: ');
    readln(i);
    dummy := 4; // dummy는 프로시저 외부에서 값이 변경되지 않습니다
End;

Begin // 주 프로그램 블록
    dummy := 3;
    get_integer(i, dummy);
    writeln(i, '! = ', factorial_recursion(i));
    // i! 출력
    writeln('dummy = ', dummy); // dummy는 변경되지 않았으므로 항상 '3'을 출력합니다.
End.
```
