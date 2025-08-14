---
name: Sing
filename: learnsing.sing
contributors:
    - ["Maurizio De Girolami", "https://github.com/mdegirolami"]
---

sing의 목적은 고성능 애플리케이션을 위한 c++의 좋은 대체재가 될 수 있는 간단하고 안전하며 빠른 언어를 제공하는 것입니다.

Sing은 사람이 읽을 수 있는 고품질의 c++로 컴파일되기 때문에 쉬운 선택입니다.

그 때문에 Sing으로 잠시 작업하다가 어느 시점에 Sing이 더 이상 마음에 들지 않는다는 것을 발견하더라도, 멋지고 깨끗한 c++ 코드가 남기 때문에 작업한 것을 잃지 않습니다.

어떤 면에서는 Sing을 몇 가지 모범 사례를 강제하는 방식으로 c++를 작성하는 도구로 생각할 수도 있습니다.

```go
/* 여러 줄 주석.
    /* 중첩될 수 있습니다 */
    코드의 일부를 주석 처리하는 데 사용하십시오.
    중간 c++ 코드에 흔적을 남기지 않습니다.
    (sing은 멋진 사람이 읽을 수 있는 c++로 번역됩니다)
*/

// 한 줄 주석은 문이나 선언 앞에만 올 수 있습니다...
// ...또는 문이나 선언의 첫 줄 오른쪽에.
// 한 줄 주석은 c++로 유지됩니다.
//
// 여기서는 다른 파일의 공개 선언을 사용해야 하는지 선언합니다.
// (이 경우 'sio', 'sys' 파일에서)
requires "sio";
requires "sys";

//
// sing 함수 선언.
// 모든 선언은 'public' 키워드로 공개할 수 있습니다.
// 모든 선언은 선언 유형을 지정하는 키워드로 시작합니다
// (이 경우 함수는 fn) 그런 다음 이름, 인수 및
// 반환 유형이 따릅니다.
//
// 각 인수는 방향 한정자(in, out, io)로 시작하여
// 인수가 입력인지, 출력인지 또는 둘 다인지 알려줍니다...
// ...그런 다음 인수 이름과 유형이 따릅니다.
public fn singmain(in argv [*]string) i32
{
    // print는 sio 파일에서 가져오고 콘솔에 문자열을 보냅니다.
    sio.print("Hello World\n");

    // 유형 변환은 <newtype>(expression) 형식으로 허용됩니다.
    sio.print(string(sum(5, 10)) + "\n");

    // 명확성을 위해 인수 뒤에 ':'로 구분된 이름을 지정할 수 있습니다.
    var result i32;
    recursive_power(10:base, 3:exponent, result);

    // '사용되지 않음' 오류를 피하기 위해 여기에서 참조됩니다.
    learnTypes();

    // 함수는 일부 기본 유형의 단일 값만 반환할 수 있습니다.
    return(0);
}

// 쉼표로 구분하여 원하는 만큼 인수를 가질 수 있습니다.
// 'in' 방향 한정자를 생략할 수도 있습니다(기본값임).
fn sum(arg1 i32, arg2 i32) i32
{
    // 'fn'이 함수를 선언하는 것처럼 'let'은 상수를 선언합니다.
    // 상수의 경우 이니셜라이저를 배치하면 유형을 생략할 수 있습니다.
    let the_sum = arg1 + arg2;

    return(the_sum);
}

// 인수는 참조로 전달됩니다. 즉, 함수 본문에서
// 인수 이름을 사용하여 전달된 변수를 참조합니다.
// 예: 재귀 스택의 모든 함수는 singmain 함수에서 제공하는
// 동일한 'result' 변수에 액세스합니다.
fn recursive_power(base i32, exponent i32, out result i32) void
{
    if (exponent == 0) {
        result = 1;
    } else {
        recursive_power(base, exponent - 1, result);
        result *= base;
    }
}

//**********************************************************
//
// 유형
//
//**********************************************************
fn learnTypes() void
{
    // var 키워드는 변경 가능한 변수를 선언합니다.
    // 이 경우 UTF-8로 인코딩된 문자열
    var my_name string;

    // 8..64비트 크기의 정수
    var int0 i8;
    var int1 i16;
    var int2 i32;
    var int3 i64;

    // 부호 없는 정수
    var uint0 u8;
    var uint1 u16;
    var uint2 u32;
    var uint3 u64;

    // 부동 소수점
    var float0 f32;
    var float1 f64;

    // 복소수
    var cmplx0 c64;
    var cmplx1 c128;

    cmplx0 = 0;
    cmplx1 = 0;

    // 그리고 물론...
    var bool0 bool;

    // 유형 추론: 기본적으로 상수는 i32, f32, c64입니다.
    let an_int32 = 15;
    let a_float32 = 15.0;
    let a_complex = 15.0 + 3i;
    let a_string = "Hello !";
    let a_bool = false;

    // 다른 유형의 상수를 만들려면 변환과 유사한 구문을 사용하십시오:
    // 참고: 이것은 변환이 아닙니다. 단지 유형 지정일 뿐입니다.
    let a_float64 = f64(5.6);

    // 유형 정의에서 []는 "의 배열"로 읽습니다.
    // 예에서 []i32 => i32의 배열.
    var intarray []i32 = {1, 2, 3};

    // 길이를 지정할 수 있습니다. 그렇지 않으면 길이는 이니셜라이저에 의해 주어집니다.
    // 마지막 이니셜라이저는 추가 항목에 복제됩니다.
    var sizedarray [10]i32 = {1, 2, 3};

    // 동적 배열을 얻으려면 크기로 *를 지정하십시오(길이를 변경할 수 있음).
    var dyna_array [*]i32;

    // 메서드와 유사한 함수를 호출하여 벡터에 항목을 추가할 수 있습니다.
    dyna_array.push_back(an_int32);

    // 배열의 크기를 가져옵니다. sys.validate()는 c의 assert와 같습니다.
    sys.validate(dyna_array.size() == 1);

    // 숫자를 문자열에 연결하는 맵.
    // "map(x)..."는 "키 유형이 x이고 값이 유형인 맵..."으로 읽습니다.
    var a_map map(string)i32;

    a_map.insert("one", 1);
    a_map.insert("two", 2);
    a_map.insert("three", 3);
    let key = "two";

    // 참고: get_safe의 두 번째 인수는
    // 키를 찾을 수 없을 때 반환될 값입니다.
    sio.print("\nAnd the value is...: " + string(a_map.get_safe(key, -1)));

    // 문자열 연결
    my_name = "a" + "b";
}

// 열거형 유형은 불연속 집합의 값만 가질 수 있습니다.
// int와 상호 변환할 수 없습니다!
enum Stages {first, second, last}

// 열거형 값을 참조할 수 있습니다(할당/비교하기 위해).
// 유형 이름과 태그 이름을 '.' 연산자로 구분하여 지정합니다.
var current_stage = Stages.first;


//**********************************************************
//
// 포인터
//
//**********************************************************

// 이것은 동적 벡터의 팩토리입니다.
// 유형 선언에서 '*'는 '에 대한 포인터..'로 읽습니다.
// 따라서 반환 유형은 'i32 벡터에 대한 포인터'입니다.
fn vectorFactory(first i32, last i32) *[*]i32
{
    var buffer [*]i32;

    // 채우기
    for (value in first : last) {
        buffer.push_back(value);
    }

    // & 연산자는 버퍼의 주소를 반환합니다.
    // 지역 변수에만 &를 사용할 수 있습니다.
    // 변수에 &를 사용하면 해당 변수는 HEAP에 할당됩니다.
    return(&buffer);
}

fn usePointers() void
{
    var bufferptr = vectorFactory(0, 100);

    // 포인터를 사용하기 위해 팩토리 패턴을 사용할 필요는 없습니다.
    var another_buffer [*]i32;
    var another_bufferptr = &another_buffer;

    // * 연산자로 포인터를 역참조할 수 있습니다.
    // sys.validate는 어설션입니다(인수가 false이면 신호 발생).
    sys.validate((*bufferptr)[0] == 0);

    /*
    // 변수에 대한 모든 포인터가 범위를 벗어나면 변수는
    // 더 이상 액세스할 수 없으며 삭제(해제)됩니다.
    */
}

//**********************************************************
//
// 클래스
//
//**********************************************************

// 이것은 클래스입니다. 멤버 변수는 여기에서 직접 초기화할 수 있습니다.
class AClass {
public:
    var public_var = 100;       // 다른 변수 선언과 동일
    fn is_ready() bool;         // 다른 함수 선언과 동일
    fn mut finalize() void;     // 소멸자(객체 삭제 시 호출)
private:
    var private_var string;

    // 멤버 변수를 변경하고 'mut'(변경 가능)로 표시해야 합니다.
    fn mut private_fun(errmsg string) void;
}

// 멤버 함수를 선언하는 방법
fn AClass.is_ready() bool
{
    // 멤버 함수 내에서 멤버는
    // 'this' 키워드와 필드 선택기 '.'를 통해 액세스할 수 있습니다.
    return(this.public_var > 10);
}

fn AClass.private_fun(errmsg string) void
{
    this.private_var = errmsg;
}

// 클래스 사용
fn useAClass() void
{
    // 이 방법으로 AClass 유형의 변수를 만듭니다.
    var instance AClass;

    // 그런 다음 '.' 연산자를 통해 해당 멤버에 액세스할 수 있습니다.
    if (instance.is_ready()) {
        instance.public_var = 0;
    }
}

//**********************************************************
//
// 인터페이스
//
//**********************************************************

// sing에서 인터페이스를 정의하여 다형성을 사용할 수 있습니다...
interface ExampleInterface {
    fn mut eraseAll() void;
    fn identify_myself() void;
}

// 그런 다음 인터페이스를 구현하는 클래스를 만듭니다.
// 참고: 인터페이스 함수를 다시 선언할 필요가 없습니다(그리고 할 수 없습니다).
class Implementer1 : ExampleInterface {
private:
    var to_be_erased i32 = 3;
public:
    var only_on_impl1 = 0;
}

class Implementer2 : ExampleInterface {
private:
    var to_be_erased f32 = 3;
}

fn Implementer1.eraseAll() void
{
    this.to_be_erased = 0;
}

fn Implementer1.identify_myself() void
{
    sio.print("\nI'm the terrible int eraser !!\n");
}

fn Implementer2.eraseAll() void
{
    this.to_be_erased = 0;
}

fn Implementer2.identify_myself() void
{
    sio.print("\nI'm the terrible float eraser !!\n");
}

fn interface_casting() i32
{
    // 업캐스팅은 자동입니다(예: *Implementer1에서 *ExampleInterface로).
    var concrete Implementer1;
    var if_ptr *ExampleInterface = &concrete;

    // (추측하셨겠지만) '.'으로 인터페이스 멤버에 액세스할 수 있습니다.
    if_ptr.identify_myself();

    // 다운캐스팅에는 특수 구문이 필요합니다.
    // (아래 조건 구조도 참조하십시오).
    typeswitch(ref = if_ptr) {
        case *Implementer1: return(ref.only_on_impl1);
        case *Implementer2: {}
        default: return(0);
    }

    return(1);
}

// 모든 루프 유형
fn loops() void
{
    // while: 조건은 엄격하게 부울 유형이어야 합니다.
    var idx = 0;
    while (idx < 10) {
        ++idx;
    }

    // 정수 범위의 for. 마지막 값은 제외됩니다.
    // 'it'은 루프에 로컬이며 이전에 선언되어서는 안 됩니다.
    for (it in 0 : 10) {
    }

    // 역방향
    for (it in 10 : 0) {
    }

    // 구성 가능한 단계. 최종 값보다 크거나 같으면 루프가 중지됩니다.
    for (it in 0 : 100 step 3) {
    }

    // 보조 카운터 사용.
    // 카운터는 항상 0에서 시작하여 각 반복에서 1씩 증가합니다.
    for (counter, it in 3450 : 100 step -22) {
    }

    // value는 차례로 배열의 모든 값을 가정합니다.
    var array [*]i32 = {0, 10, 100, 1000};
    for (value in array) {
    }

    // 보조 카운터와 함께 이전과 동일
    for (counter, value in array) {
    }
}

// 모든 조건 구조
interface intface {}
class c0_test : intface {public: fn c0stuff() void;}
class delegating : intface {}

fn conditionals(in object intface, in objptr *intface) void
{
    let condition1 = true;
    let condition2 = true;
    let condition3 = true;
    var value = 30;

    // condition1은 부울이어야 합니다.
    if (condition1) {
        ++value;    // 조건부 문
    }

    // else if로 조건을 연결할 수 있습니다.
    if (condition1) {
        ++value;
    } else if (condition2) {
        --value;
    }

    // 다른 조건이 false이면 최종 else가 실행됩니다.
    if (condition1) {
        ++value;
    } else if (condition2) {
        --value;
    } else {
        value = 0;
    }

    // switch 값을 기준으로 case 문을 선택합니다.
    switch (value) {
        case 0: sio.print("value is zero"); // 단일 문!
        case 1: {}                          // 아무것도 안 함
        case 2:                             // 통과
        case 3: sio.print("value is more than one");
        case 4: {                           // 블록은 단일 문입니다!
            value = 0;
            sio.print("how big !!");
        }
        default: return;                    // 다른 것이 일치하지 않으면
    }

    // switch와 유사하지만 인수 유형을 기준으로 case를 선택합니다.
    // - object는 인터페이스 유형의 함수 인수여야 합니다.
    // - case 유형은 object 인터페이스를 구현하는 클래스여야 합니다.
    // - 각 case 문에서 ref는 해당 case의 클래스 유형을 가정합니다.
    typeswitch(ref = object) {
        case c0_test: ref.c0stuff();
        case delegating: {}
        default: return;
    }

    // - object는 인터페이스 포인터여야 합니다.
    // - case 유형은 objptr 인터페이스를 구현하는 클래스에 대한 포인터여야 합니다.
    // - 각 case 문에서 ref는 해당 case의 클래스 포인터 유형을 가정합니다.
    typeswitch(ref = objptr) {
        case *c0_test: {
            ref.c0stuff();
            return;
        }
        case *delegating: {}
        default: sio.print("unknown pointer type !!");
    }
}
```

## 더 읽을거리

[공식 Sing 웹사이트](https://mdegirolami.wixsite.com/singlang).

sing을 가지고 놀고 싶다면 vscode 플러그인을 다운로드하는 것이 좋습니다.
[시작하기](https://mdegirolami.wixsite.com/singlang/copy-of-interfacing-sing-and-c-2)의 지침을 따르십시오.
