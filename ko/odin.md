---
name: Odin
contributors:
    - ["Collin MacDonald", "https://github.com/CollinEMac"]
filename: learnodin.odin
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Odin은 Bill "gingerBill" Hall에 의해 만들어졌습니다. 가비지 컬렉션 없이 단순성, 가독성 및 성능을 강조하는 범용 시스템 프로그래밍 언어입니다. Odin은 자신을 "프로그래밍의 즐거움을 위한 C 대안"이라고 소개합니다.

```
// 한 줄 주석은 두 개의 슬래시로 시작합니다.

/*
   여러 줄 주석은 슬래시-별표로 시작하고,
   별표-슬래시로 끝납니다. 중첩될 수 있습니다!
   /*
       이처럼!
   */
*/

// 이것은 Odin의 고전적인 "hello world" 프로그램입니다.
package main

import "core:fmt"

main :: proc() {
    fmt.println("Hellope!")
}

////////////////////////////////////////////////////
## 1. 기본 데이터 타입 및 연산자
////////////////////////////////////////////////////

// 정수 - Odin은 명시적인 크기의 정수 타입을 가집니다
x: i32 = 42          // 32비트 부호 있는 정수
y: u64 = 100         // 64비트 부호 없는 정수
z: int = 123         // 플랫폼 종속 정수 (보통 i64)

// 가독성을 위해 숫자에 밑줄을 사용할 수 있습니다
big_number := 1_000_000

// 부동 소수점 숫자
pi: f32 = 3.14159    // 32비트 부동 소수점
e: f64 = 2.71828     // 64비트 부동 소수점 (부동 소수점 리터럴의 기본값)

// 불리언
is_true: bool = true
is_false: bool = false

// 룬 (유니코드 문자)
letter: rune = 'A'
emoji: rune = '🚀'

// 문자열
name: string = "Odin Programming"
raw_string := `C:\Windows\System32`  // 백틱을 사용한 원시 문자열

// 문자열 길이 (문자가 아닌 바이트 단위!)
length := len(name)

// 산술 연산자는 예상대로 작동합니다
result := 10 + 5    // 15
diff := 10 - 5      // 5
product := 10 * 5   // 50
quotient := 10 / 5  // 2
remainder := 10 % 3 // 1

// 비교 연산자
is_equal := 10 == 10        // true
not_equal := 10 != 5        // true
greater := 10 > 5           // true
less_equal := 5 <= 10       // true

// 논리 연산자
and_result := true && false  // false
or_result := true || false   // true
not_result := !true          // false

// 비트 연산자
bit_and := 0b1010 & 0b1100   // 0b1000
bit_or := 0b1010 | 0b1100    // 0b1110
bit_xor := 0b1010 ~ 0b1100   // 0b0110 (참고: Odin에서 ~는 XOR입니다)
bit_not := ~0b1010           // 비트 NOT

////////////////////////////////////////////////////
## 2. 변수와 상수
////////////////////////////////////////////////////

// 타입 추론을 사용한 변수 선언
some_number := 42            // int로 타입 추론됨
some_text := "Hello"         // string으로 타입 추론됨

// 명시적 타입 선언
explicit_int: int = 42
explicit_float: f64 = 3.14

// 초기화되지 않은 변수는 0으로 초기화됩니다
uninitialized_int: int       // 기본값 0
uninitialized_bool: bool     // 기본값 false
uninitialized_string: string // 기본값 ""

// 상수는 ::로 정의됩니다
PI :: 3.14159
MESSAGE :: "This is a constant"

// 타입이 있는 상수
TYPED_CONSTANT : f32 : 2.71828

// 다중 할당
a, b := 10, 20
a, b = b, a  // 값 교환

////////////////////////////////////////////////////
## 3. 배열과 슬라이스
////////////////////////////////////////////////////

// 고정 크기 배열
numbers: [5]int = {1, 2, 3, 4, 5}
chars: [3]rune = {'A', 'B', 'C'}

// 크기가 추론된 배열
inferred := [..]int{10, 20, 30, 40}

// 0으로 초기화된 배열
zeros: [10]int  // 모든 요소는 0입니다

// 배열 요소 접근
first := numbers[0]     // 1
last := numbers[4]      // 5
array_length := len(numbers)  // 5

// 슬라이스 - 배열에 대한 동적 뷰
slice: []int = {1, 2, 3, 4, 5}  // 슬라이스 리터럴
array_slice := numbers[1:4]     // 인덱스 1에서 3까지의 배열 슬라이스
full_slice := numbers[:]        // 전체 배열의 슬라이스

// 동적 배열 - 커지거나 작아질 수 있음
dynamic_array: [dynamic]int
append(&dynamic_array, 1)
append(&dynamic_array, 2, 3, 4)  // 여러 요소 추가

// 동적 배열을 정리하는 것을 잊지 마십시오
defer delete(dynamic_array)

////////////////////////////////////////////////////
## 4. 제어 흐름
////////////////////////////////////////////////////

// If 문
age := 25
if age >= 18 {
    fmt.println("Adult")
} else if age >= 13 {
    fmt.println("Teenager")
} else {
    fmt.println("Child")
}

// For 루프 - Odin의 유일한 루프 구조
// C 스타일 for 루프
for i := 0; i < 10; i += 1 {
    fmt.println(i)
}

// While 스타일 루프
counter := 0
for counter < 5 {
    fmt.println(counter)
    counter += 1
}

// 무한 루프
for {
    // 이것은 영원히 실행됩니다 (break까지)
    break  // 루프 종료
}

// 인덱스로 배열/슬라이스 반복
numbers_array := [3]int{10, 20, 30}
for value, index in numbers_array {
    fmt.printf("Index %d: Value %d\n", index, value)
}

// 값만 반복
for value in numbers_array {
    fmt.println(value)
}

// Switch 문
day := "Monday"
switch day {
case "Monday", "Tuesday", "Wednesday", "Thursday", "Friday":
    fmt.println("Weekday")
case "Saturday", "Sunday":
    fmt.println("Weekend")
case:  // 기본 케이스
    fmt.println("Unknown day")
}

// 조건 없는 Switch (if-else 체인과 같음)
switch {
case age < 13:
    fmt.println("Child")
case age < 20:
    fmt.println("Teenager")
case:
    fmt.println("Adult")
}

////////////////////////////////////////////////////
## 5. 프로시저 (함수)
////////////////////////////////////////////////////

// 기본 프로시저 정의
add :: proc(a: int, b: int) -> int {
    return a + b
}

// 다중 반환 값이 있는 프로시저
divide :: proc(a: int, b: int) -> (int, bool) {
    if b == 0 {
        return 0, false  // 0으로 나누기
    }
    return a / b, true
}

// 프로시저 사용
sum := add(5, 3)                    // 8
quotient, ok := divide(10, 2)       // 5, true
quotient_bad, ok_bad := divide(10, 0) // 0, false

// 기본 매개변수가 있는 프로시저 (오버로딩 사용)
greet :: proc(name: string) {
    fmt.printf("Hello, %s!\n", name)
}

greet :: proc() {
    greet("World")
}

// 가변 프로시저 (가변 개수의 인수)
sum_all :: proc(numbers: ..int) -> int {
    total := 0
    for number in numbers {
        total += number
    }
    return total
}

result_sum := sum_all(1, 2, 3, 4, 5)  // 15

////////////////////////////////////////////////////
## 6. 구조체
////////////////////////////////////////////////////

// 구조체 정의
Person :: struct {
    name: string,
    age:  int,
    height: f32,
}

// 구조체 인스턴스 생성
person1 := Person{
    name = "Alice",
    age = 30,
    height = 5.6,
}

// 부분 초기화 (나머지 필드는 0으로 초기화됨)
person2 := Person{
    name = "Bob",
    // age와 height는 기본적으로 0입니다
}

// 구조체 필드 접근
fmt.printf("%s is %d years old\n", person1.name, person1.age)

// 구조체 필드 수정
person1.age = 31

// 구조체와 함께 작동하는 프로시저
celebrate_birthday :: proc(person: ^Person) {  // ^는 포인터를 의미합니다
    person.age += 1
    fmt.printf("Happy birthday! %s is now %d\n", person.name, person.age)
}

celebrate_birthday(&person1)  // &로 주소 전달

////////////////////////////////////////////////////
## 7. 열거형과 유니온
////////////////////////////////////////////////////

// 열거형
Color :: enum {
    RED,
    GREEN,
    BLUE,
    YELLOW,
}

my_color := Color.RED

// 명시적 값을 가진 열거형
Status :: enum u8 {
    OK = 0,
    ERROR = 1,
    WARNING = 2,
}

// 유니온 (태그된 유니온)
Shape :: union {
    Circle: struct { radius: f32 },
    Rectangle: struct { width, height: f32 },
    Triangle: struct { base, height: f32 },
}

my_shape := Shape(Circle{{radius = 5.0}})

// 유니온을 사용한 패턴 매칭
switch shape in my_shape {
case Circle:
    fmt.printf("Circle with radius %.2f\n", shape.radius)
case Rectangle:
    fmt.printf("Rectangle %.2f x %.2f\n", shape.width, shape.height)
case Triangle:
    fmt.printf("Triangle base %.2f, height %.2f\n", shape.base,
               shape.height)
}

////////////////////////////////////////////////////
## 8. 맵
////////////////////////////////////////////////////

// 맵 선언
scores: map[string]int

// 맵 초기화
scores = make(map[string]int)
defer delete(scores)  // 완료 시 정리

// 키-값 쌍 추가
scores["Alice"] = 95
scores["Bob"] = 87
scores["Charlie"] = 92

// 값 접근
alice_score := scores["Alice"]  // 95

// 키 존재 여부 확인
bob_score, exists := scores["Bob"]
if exists {
    fmt.printf("Bob's score: %d\n", bob_score)
}

// 맵 반복
for name, score in scores {
    fmt.printf("%s: %d\n", name, score)
}

// 맵 리터럴
ages := map[string]int{
    "Alice" = 30,
    "Bob" = 25,
    "Charlie" = 35,
}
defer delete(ages)

////////////////////////////////////////////////////
## 9. 포인터와 메모리 관리
////////////////////////////////////////////////////

// 포인터
number := 42
number_ptr := &number        // 숫자의 주소 가져오기
value := number_ptr^         // 포인터 역참조 (값 가져오기)

fmt.printf("Value: %d, Address: %p\n", value, number_ptr)

// 동적 메모리 할당
// new()는 포인터를 할당하고 반환합니다
int_ptr := new(int)
int_ptr^ = 100
defer free(int_ptr)  // 메모리 정리

// 복잡한 타입을 위한 make()
my_slice := make([]int, 5)    // 길이가 5인 슬라이스
defer delete(my_slice)

////////////////////////////////////////////////////
## 10. 오류 처리
////////////////////////////////////////////////////

// Odin은 오류 처리를 위해 다중 반환 값을 사용합니다
read_file :: proc(filename: string) -> (string, bool) {
    // 파일 읽기 시뮬레이션
    if filename == "" {
        return "", false  // 오류 케이스
    }
    return "file contents", true  // 성공 케이스
}

// 오류 반환 프로시저 사용
content, success := read_file("myfile.txt")
if !success {
    fmt.println("Failed to read file")
} else {
    fmt.printf("File content: %s\n", content)
}

// or_return을 사용한 일반적인 패턴
parse_number :: proc(s: string) -> (int, bool) {
    // 이것은 단순화된 예입니다
    if s == "42" {
        return 42, true
    }
    return 0, false
}

example_with_error_handling :: proc() -> bool {
    // or_return은 두 번째 값이 false이면 자동으로 false를 반환합니다
    num := parse_number("42") or_return
    fmt.printf("Parsed number: %d\n", num)
    return true
}

////////////////////////////////////////////////////
## 11. 패키지와 임포트
////////////////////////////////////////////////////

// 모든 .odin 파일은 패키지 선언으로 시작합니다
// package main  // (이미 맨 위에 선언됨)

// 코어 라이브러리에서 임포트
import "core:fmt"
import "core:strings"
import "core:os"

// 별칭으로 임포트
import str "core:strings"

// 임포트된 프로시저 사용
text := "Hello, World!"
upper_text := strings.to_upper(text)
fmt.println(upper_text)

// 벤더 패키지에서 임포트 (외부 라이브러리)
// import "vendor:raylib"

////////////////////////////////////////////////////
## 12. 컴파일 타임 기능
////////////////////////////////////////////////////

// 컴파일 타임 조건문
when ODIN_OS == .Windows {
    // Windows 특정 코드
    fmt.println("Running on Windows")
} else when ODIN_OS == .Linux {
    // Linux 특정 코드
    fmt.println("Running on Linux")
} else {
    // 다른 플랫폼
    fmt.println("Running on other platform")
}

// 컴파일 타임 상수
ODIN_DEBUG :: #config(DEBUG, false)

when ODIN_DEBUG {
    fmt.println("Debug mode enabled")
}

// 제네릭 (매개변수 다형성)
Generic_Array :: struct($T: typeid) {
    data: []T,
}

max :: proc(a: $T, b: T) -> T {
    return a if a > b else b
}

max_int := max(10, 20)      // T는 int가 됩니다
max_float := max(3.14, 2.71) // T는 f64가 됩니다

////////////////////////////////////////////////////
## 13. 내장 데이터 구조
////////////////////////////////////////////////////

// 플래그를 위한 비트 세트
File_Mode :: enum {
    READ,
    WRITE,
    EXECUTE,
}

permissions: bit_set[File_Mode]
permissions |= {.READ, .WRITE}        // 여러 플래그 설정
permissions &~= {.WRITE}              // 플래그 제거
has_read := .READ in permissions      // 플래그 확인
is_readonly := permissions == {.READ} // 세트 비교

// 복소수
z1 := complex64(3 + 4i)
z2 := complex64(1 - 2i)
sum := z1 + z2                        // (4 + 2i)
magnitude := abs(z1)                  // 5.0

// 선형 대수를 위한 행렬
transform := matrix[3, 3]f32{
    1, 0, 5,  // X축으로 5만큼 이동
    0, 1, 3,  // Y축으로 3만큼 이동
    0, 0, 1,  // 동차 좌표
}

point := [3]f32{10, 20, 1}
transformed := transform * point      // 행렬 곱셈

// 3D 회전을 위한 쿼터니언
identity_rot := quaternion128{0, 0, 0, 1}  // 회전 없음
rotation_90_z := quaternion128{0, 0, 0.707, 0.707}  // Z축 주위로 90°

////////////////////////////////////////////////////
## 14. 컨텍스트 시스템과 Defer
////////////////////////////////////////////////////

// Odin은 스레딩 할당자, 로거 및 기타 유틸리티를 프로그램을 통해 전달하기 위한 암시적 컨텍스트 시스템을 가지고 있습니다

example_with_context :: proc() {
    // 현재 컨텍스트 저장
    old_allocator := context.allocator

    // 일시적으로 다른 할당자 사용
    temp_allocator := context.temp_allocator
    context.allocator = temp_allocator

    // 이 범위의 모든 할당은 temp_allocator를 사용합니다
    temp_data := make([]int, 100)
    // temp_data를 삭제할 필요가 없습니다 - 자동으로 정리됩니다

    // 원래 할당자 복원
    context.allocator = old_allocator
}

// defer는 범위가 종료될 때 정리가 수행되도록 보장합니다
resource_management_example :: proc() {
    file_handle := os.open("example.txt", os.O_RDONLY, 0) or_return
    defer os.close(file_handle)  // 함수가 종료될 때 항상 닫힘

    buffer := make([]u8, 1024)
    defer delete(buffer)  // 함수가 종료될 때 항상 해제됨

    // file_handle과 버퍼 사용...
    // 일찍 반환하더라도 자동으로 정리됩니다
}
```

## 더 읽을거리

[Odin 프로그래밍 언어 웹사이트](https://odin-lang.org/)는 훌륭한 문서와 예제를 제공합니다.

[개요](https://odin-lang.org/docs/overview/)는 언어의 많은 부분을 자세히 다룹니다.

[GitHub 예제 저장소](https://github.com/odin-lang/examples)에는 관용적인 Odin 코드 예제가 포함되어 있습니다.

학습 자료:
- [Karl Zylinski의 "Odin 프로그래밍 언어 이해하기"](https://odinbook.com)
- 커뮤니티 지원을 위한 [Odin Discord](https://discord.gg/sVBPHEv)
- 일반적인 질문에 대한 [FAQ](https://odin-lang.org/docs/faq/)
