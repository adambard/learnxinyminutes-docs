---
name: Zig
filename: learnzig.zig
contributors:
    - ["Philippe Pittoli", "https://karchnu.fr/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

[Zig][ziglang]는 C 프로그래밍 언어를 대체하는 것을 목표로 합니다.

**경고**: 이 문서는 컴퓨터 과학의 몇 가지 기본 개념(예: 포인터, 스택 및 힙 메모리 등)을 이해하고 있다고 가정합니다. C 언어에 대한 사전 지식이 권장됩니다.

## 빠른 개요: C와 Zig 비교

- 구문은 대부분 동일하며, 일부 개선 사항이 있습니다 (모호함 감소).
- Zig는 네임스페이스를 도입했습니다.
- `try`와 `catch` 메커니미즘은 편리하고 효율적이며 선택 사항입니다.
- C의 대부분의 정의되지 않은 동작(UB)이 수정되었습니다.
- C에 비해 원시 포인터(raw pointer)는 사용하기에 더 안전하며 필요성이 적습니다.
  - 타입 시스템은 단일 값에 대한 포인터, 여러 값에 대한 포인터 등을 구분합니다.
  - 슬라이스(slice)가 선호되는데, 이는 포인터와 런타임에 알려진 크기를 가진 구조체로, 애초에 포인터의 대부분 사용 사례를 특징짓습니다.
- 일부 임의적인 언어 제한이 제거되었습니다. 예를 들어, 열거형, 구조체, 공용체는 함수를 가질 수 있습니다.
- SIMD 연산(벡터에 대한 기본 수학)에 간단하게 접근할 수 있습니다.
- Zig는 C의 저수준 기능과 컴파일러 확장을 통해 제공되는 기능을 모두 제공합니다.
  예: 압축 구조체(packed structure).
- 데이터 구조와 알고리즘을 포함한 광범위한 표준 라이브러리를 제공합니다.
- 어떠한 의존성 없이 기본적으로 크로스 컴파일 기능을 제공합니다.
  프로세스를 용이하게 하기 위해 다양한 libc가 제공됩니다.
  크로스 컴파일은 모든 운영 체제와 아키텍처 간에 작동합니다.

## Zig 언어

```zig
//! 최상위 문서화 주석.

/// 문서화 주석.

// 간단한 주석.
```

### Hello world.

```zig
// "std" 상수를 통해 접근할 수 있는 표준 라이브러리를 가져옵니다.
const std = @import("std");

// 이제 "info"는 "std.log.info" 함수를 참조합니다.
const info = std.log.info;

// 일반적인 hello world.
// 구문: [pub] fn <함수-이름>(<인자들>) <반환-타입> { <본문> }
pub fn main() void {
    // C 함수와 달리, Zig 함수는 고정된 수의 인자를 가집니다.
    // C에서: "printf"는 임의의 수의 인자를 받습니다.
    // Zig에서: std.log.info는 포맷과 출력할 요소 목록을 받습니다.
    info("hello world", .{});  // .{}=는 빈 익명 튜플입니다.
}
```

### 불리언, 정수, 부동소수점.

```zig
// 불리언.
// 불리언 연산에는 연산자보다 키워드가 선호됩니다.
print("{}
{}
{}
", .{
    true and false,
    true or false,
    !true,
});

// 정수.
const one_plus_one: i32 = 1 + 1;
print("1 + 1 = {}
", .{one_plus_one}); // 2

// 부동소수점.
const seven_div_three: f32 = 7.0 / 3.0;
print("7.0 / 3.0 = {}
", .{seven_div_three}); // 2.33333325e+00

// 정수는 임의의 값 길이를 가질 수 있습니다.
var myvar: u10 = 5; // 10비트 부호 없는 정수
// 예를 들어 네트워크 패킷이나 복잡한 바이너리 형식을 읽는 데 유용합니다.

// 숫자 표현은 C에 비해 크게 향상되었습니다.
const one_billion = 1_000_000_000;         // 10진수.
const binary_mask = 0b1_1111_1111;         // 2진수. 예: 네트워크 마스크.
const permissions = 0o7_5_5;               // 8진수.  예: 유닉스 권한.
const big_address = 0xFF80_0000_0000_0000; // 16진수. 예: IPv6 주소.


// 오버플로 연산자: 컴파일러에게 오버플로가 괜찮을 때를 알려줍니다.
var i: u8 = 0;  // "i"는 부호 없는 8비트 정수입니다.
i  -= 1;        // 런타임 오버플로 오류 (부호 없는 값은 항상 양수입니다)
i -%= 1;        // 괜찮음 (wrapping 연산자), i == 255

// 포화 연산자: 값은 하한과 상한에 고정됩니다.
var i: u8 = 200;   // "i"는 부호 없는 8비트 정수입니다 (값: 0에서 255까지)
i  +| 100 == 255   // u8: 255보다 커지지 않습니다
i  -| 300 == 0     // 부호 없음, 0보다 작아지지 않습니다
i  *| 2   == 255   // u8: 255보다 커지지 않습니다
i <<| 8   == 255   // u8: 255보다 커지지 않습니다
```

### 배열.

```zig
// 배열은 길이 속성(len)을 가진 잘 정의된 구조체입니다.

// 정의되지 않은 내용(스택 쓰레기 값)을 가진 5바이트 배열.
var array1: [5]u8 = undefined;

// 정의된 내용을 가진 5바이트 배열.
var array2 = [_]u8{ 1, 2, 3, 4, 5 };
// [_]는 컴파일러가 컴파일 타임에 길이를 안다는 것을 의미합니다.

// 정의된 내용(0)을 가진 1000바이트 배열.
var array3 = [_]u8{0} ** 1000;

// 정의된 내용을 가진 또 다른 1000바이트 배열.
// 내용은 컴파일 타임에 호출되는 "foo" 함수에 의해 제공되며,
// 복잡한 초기화를 허용합니다.
var array4 = [_]u8{foo()} ** 1000;

// 어떤 경우든, array.len은 배열의 길이를 제공합니다.
// array1.len과 array2.len은 5를, array3.len과 array4.len은 1000을 생성합니다.


// 배열 내용 수정 및 접근.

// 10개의 32비트 정의되지 않은 정수 배열.
var some_integers: [10]i32 = undefined;

some_integers[0] = 30; // 배열의 첫 번째 요소는 이제 30입니다.

var x = some_integers[0]; // "x"는 이제 30과 같고, 타입이 추론됩니다.
var y = some_integers[1]; // 배열의 두 번째 요소는 정의되지 않았습니다.
                          // "y"는 스택 쓰레기 값을 가집니다 (런타임 오류 없음).

// 10개의 32비트 정의되지 않은 정수 배열.
var some_integers: [10]i32 = undefined;

var z = some_integers[20]; // 인덱스 > 배열 크기, 컴파일 오류.

// 런타임에, 인덱스를 사용하여 "some_integers"의 요소를 반복합니다.
// 인덱스 i = 20일 때, 다음을 시도합니다:
try some_integers[i]; // 런타임 오류 'index out of bounds'.
                      // "try" 키워드는 인덱스로 배열에 접근할 때 필요합니다.
                      // 잠재적인 런타임 오류가 있기 때문입니다.
                      // 나중에 더 자세히 설명합니다.
```

### 다차원 배열.

```zig
const mat4x4 = [4][4]f32{
    .{ 1, 0, 0, 0 },
    .{ 0, 1, 0, 1 },
    .{ 0, 0, 1, 0 },
    .{ 0, 0, 0, 1 },
};

// 인덱스를 통해 2D 배열에 접근한 다음 내부 배열에 접근합니다.
try expect(mat4x4[1][1] == 1.0);

// 여기서는 for 루프로 반복합니다.
for (mat4x4) |row, row_index| {
    for (row) |cell, column_index| {
        // ...
    }
}
```

### 문자열.

```zig
// 간단한 문자열 상수.
const greetings = "hello";
// ... 이는 다음과 동일합니다:
const greetings: *const [5:0]u8 = "hello";
// 말로 풀면: "greetings"는 상수 값이며, 5개 요소(8비트 부호 없는 정수)로
// 이루어진 상수 배열에 대한 포인터이고, 끝에 추가적인 '0'이 있습니다.
// 추가적인 "0"은 "센티널 값(sentinel value)"이라고 불립니다.

print("string: {s}\n", .{greetings});

// 이것은 C 문자열을 상당히 충실하게 나타냅니다. 하지만 Zig 문자열은
// 구조체이므로, 크기를 계산하기 위해 "strlen"이 필요 없습니다.
// greetings.len == 5
```

### 슬라이스.

```zig
// 슬라이스는 포인터와 크기이며, 컴파일 타임에 크기를 알 수 없는 배열입니다.
// 슬라이스는 런타임에 경계 밖 검증을 합니다.

const array = [_]u8{1,2,3,4,5};     // [_] = 컴파일 타임에 크기를 아는 배열.
const slice = array[0..array.len];  // "slice"는 전체 배열을 나타냅니다.
                                    // slice[10]은 런타임 오류를 발생시킵니다.
```

### 포인터.

```zig
// 값에 대한 포인터는 "&"로 생성할 수 있습니다.
const x: i32 = 1;
const pointer: *i32 = &x;  // "pointer"는 i32 변수 "x"에 대한 포인터입니다.
print("1 = {}, {}\n", .{x, pointer});

// 포인터 값은 ".*"로 접근하고 수정합니다.
if (pointer.* == 1) {
    print("x value == {}
", .{pointer.*});
}

// ".?"는 "orelse unreachable"의 단축 표현입니다.
const foo = pointer.?; // 가리키는 값을 가져오고, 그렇지 않으면 충돌합니다.
```

### 옵셔널 값 (?<type>).

```zig
// 옵셔널은 어떤 타입의 값이거나 null일 수 있는 값입니다.

// 예: "optional_value"는 "null"이거나 부호 없는 32비트 정수일 수 있습니다.
var optional_value: ?u32 = null; // optional_value == null
optional_value = 42;             // optional_value != null

// "some_function"은 ?u32를 반환합니다.
var x = some_function();
if (x) |value| {
    // "some_function"이 값을 반환한 경우.
    // 'value'로 무언가를 합니다.
}
```

### 오류.

```zig
// Zig는 오류를 표현하는 통일된 방법을 제공합니다.

// 오류는 오류 열거형에 정의됩니다. 예:
const Error = error {
    WatchingAnyNetflixTVShow,
    BeOnTwitter,
};

// 일반 열거형은 "enum" 키워드를 사용하여 동일한 방식으로 표현됩니다.
const SuccessStory = enum {
    DoingSport,
    ReadABook,
};


// 오류 공용체 (!).
// "mylife" 값은 오류이거나 일반 값입니다.
var mylife: Error!SuccessStory = Error.BeOnTwitter;
// mylife는 오류입니다. 슬프네요.

mylife = SuccessStory.ReadABook;
// 이제 mylife는 열거형입니다.


// Zig는 많은 미리 정의된 오류와 함께 제공됩니다. 예:
const value: anyerror!u32 = error.Broken;


// 오류 처리.

// 몇 가지 오류 예제.
const Error = error {
    UnExpected,
    Authentication,
};

// "some_function"은 "Error" 또는 정수를 반환할 수 있습니다.
fn some_function() Error!u8 {
    return Error.UnExpected; // 오류를 반환합니다.
}

// 오류는 중간 변수 없이 "catch"될 수 있습니다.
var value = some_function() catch |err| switch(err) {
    Error.UnExpected     => return err,   // 오류를 반환합니다.
    Error.Authentication => unreachable,  // 예상되지 않음. 프로그램을 충돌시킵니다.
    else                 => unreachable,
};

// 오류는 이름을 지정하지 않고 "catch"될 수 있습니다.
const unwrapped = some_function() catch 1234; // "unwrapped" = 1234

// "try"는 "catch |err| return err"에 대한 매우 편리한 단축 표현입니다.
var value = try some_function();
// "some_function"이 실패하면, 현재 함수는 중지하고 오류를 반환합니다.
// "value"는 유효한 값만 가질 수 있으며, 오류는 이미 "try"로 처리되었습니다.
```

### 제어 흐름.

```zig
// 조건 분기.

if (condition) {
    ...
}
else {
    ...
}

// 삼항 연산자.
var value = if (condition) x else y;

// "if (x) x else 0"의 단축 표현
var value = x orelse 0;

// "a"가 값을 포함할 수 있는 옵셔널인 경우.
if (a) |value| {
    print("value: {}
", .{value});
}
else {
    print("'a' is null\n", .{});
}

// 값에 대한 포인터를 가져옵니다 (존재하는 경우).
if (a) |*value| { value.* += 1; }


// 루프.

// 구문 예제:
//   while (condition) statement
//   while (condition) : (end-of-iteration-statement) statement
//
//   for (iterable) statement
//   for (iterable) |capture| statement
//   for (iterable) statement else statement

// 참고: 루프는 배열이나 슬라이스에 대해 동일하게 작동합니다.

// 간단한 "while" 루프.
while (i < 10) { i += 1; }

// "continue expression"이 있는 while 루프
// (루프의 마지막 표현식으로 실행되는 표현식).
while (i < 10) : (i += 1) { ... }
// 더 복잡한 continue expression(코드 블록)을 사용한 동일한 예.
while (i * j < 2000) : ({ i *= 2; j *= 3; }) { ... }

// 슬라이스의 일부를 반복하려면, 다시 슬라이스합니다.
for (items[0..1]) |value| { sum += value; }

// 배열(또는 슬라이스)의 모든 항목을 반복합니다.
for (items) |value| { sum += value; }

// 복사본 대신 값에 대한 포인터를 가져와 반복합니다.
for (items) |*value| { value.* += 1; }

// 인덱스와 함께 반복합니다.
for (items) |value, i| { print("val[{}] = {}
", .{i, value}); }

// 포인터와 인덱스와 함께 반복합니다.
for (items) |*value, i| { print("val[{}] = {}
", .{i, value}); value.* += 1; }


// break와 continue가 지원됩니다.
for (items) |value| {
    if (value == 0)  { continue; }
    if (value >= 10) { break;    }
    // ...
}

// for 루프는 표현식으로도 사용될 수 있습니다.
// while 루프와 유사하게, for 루프에서 break하면,
// else 분기는 평가되지 않습니다.
var sum: i32 = 0;
// "for" 루프는 값을 제공해야 하며, 이는 "else" 값이 됩니다.
const result = for (items) |value| {
    if (value != null) {
        sum += value.?; // "result"는 마지막 "sum" 값이 됩니다.
    }
} else 0;                  // 마지막 값.
```

### 레이블.

```zig
// 레이블은 명령어, 코드 내 위치에 이름을 지정하는 방법입니다.
// 레이블은 중첩 루프에서 "continue" 또는 "break"하는 데 사용될 수 있습니다.
outer: for ([_]i32{ 1, 2, 3, 4, 5, 6, 7, 8 }) |_| {
    for ([_]i32{ 1, 2, 3, 4, 5 }) |_| {
        count += 1;
        continue :outer; // 첫 번째 루프에 대해 "continue"합니다.
    }
} // count = 8
outer: for ([_]i32{ 1, 2, 3, 4, 5, 6, 7, 8 }) |_| {
    for ([_]i32{ 1, 2, 3, 4, 5 }) |_| {
        count += 1;
        break :outer; // 첫 번째 루프에 대해 "break"합니다.
    }
} // count = 1


// 레이블은 블록에서 값을 반환하는 데에도 사용될 수 있습니다.
var y: i32 = 5;
const x = blk: {
    y += 1;
    break :blk y; // 이제 "x"는 6과 같습니다.
};
// "for else" 표현식과 같은 경우에 관련이 있습니다 (다음에 설명).

// for 루프는 표현식으로 사용될 수 있습니다.
// for 루프에서 break하면, else 분기는 평가되지 않습니다.
// 경고: 직관에 반합니다.
//      "for" 루프가 실행된 다음 "else" 블록이 실행됩니다.
//      "else" 키워드 다음에는 "result"에 줄 값이 와야 합니다.
//      다른 형태는 나중에 참조하십시오.
var sum: u8 = 0;
const result = for (items) |value| {
    sum += value;
} else 8; // result = 8

// 이 경우에도 "else" 키워드 다음에는 값이 옵니다.
// 그러나 구문이 다릅니다: 레이블이 지정되어 있습니다.
// 값 대신, 레이블과 코드 블록이 있으며, 이는
// 값을 반환하기 전에 작업을 수행할 수 있게 합니다 ("break" 호출 참조).
const result = for (items) |value| { // 첫째: 루프.
    sum += value;
} else blk: {                        // 둘째: "else" 블록.
    std.log.info("executed AFTER the loop!", .{});
    break :blk sum; // "sum" 값이 레이블 "blk"를 대체합니다.
};
```

### Switch.

```zig
// C의 switch와 같지만, 약간 더 발전했습니다.
// 구문:
//   switch (value) {
//       pattern => expression,
//       pattern => expression,
//       else    => expression
//   };

// 간단한 값만 확인하는 switch.
var x = switch(value) {
    Error.UnExpected     => return err,
    Error.Authentication => unreachable,
    else                 => unreachable,
};

// 값의 범위를 허용하는 약간 더 발전된 switch:
const foo: i32 = 0;
const bar = switch (foo) {
    0                        => "zero",
    1...std.math.maxInt(i32) => "positive",
    else                     => "negative",
};
```

### 구조체.

```zig
// 단일 값을 포함하는 구조체.
const Full = struct {
    number: u16,
};

// 메모리 내 레이아웃이 보장되는 압축 구조체.
const Divided = packed struct {
    half1: u8,
    quarter3: u4,
    quarter4: u4,
};

// Point는 두 개의 u32, "x"와 "y"를 포함하는 구조체를 나타내는 상수입니다.
// "x"는 기본값을 가지며, 이는 C에서는 불가능했습니다.
const Point = struct {
    x: u32 = 1, // 기본값
    y: u32,
};

// 변수 "p"는 x = 1 (기본값)이고 y = 2인 새로운 Point입니다.
var p = Point{ .y = 2 };

// 필드는 점 표기법으로 평소와 같이 접근합니다: variable.field.
print("p.x: {}
", .{p.x}); // 1
print("p.y: {}
", .{p.y}); // 2


// 구조체는 공개 상수와 함수를 포함할 수도 있습니다.
const Point = struct {
    pub const some_constant = 30;

    x: u32,
    y: u32,

    // 이 "init" 함수는 Point를 생성하고 반환합니다.
    pub fn init() Point {
        return Point{ .x = 0, .y = 0 };
    }
};


// 구조체 공개 상수에 접근하는 방법.
// 값은 구조체의 "인스턴스"에서 접근하는 것이 아니라,
// 구조체 정의를 나타내는 상수(Point)에서 접근합니다.
print("constant: {}
", .{Point.some_constant});

// "init" 함수를 갖는 것은 표준 라이브러리에서 상당히 관용적입니다.
// 나중에 더 자세히 설명합니다.
var p = Point.init();
print("p.x: {}
", .{p.x}); // p.x = 0
print("p.y: {}
", .{p.y}); // p.y = 0


// 구조체는 종종 객체 지향 프로그래밍과 유사하게
// 상태를 수정하는 함수를 가집니다.
const Point = struct {
    const Self = @This(); // 자신의 타입(나중에 "Point"라고 불림)을 참조합니다.

    x: u32,
    y: u32,

    // 시그니처를 보세요. 첫 번째 인자는 *Self 타입입니다: "self"는
    // 구조체의 인스턴스에 대한 포인터입니다.
    // 이것은 OOP에서와 같은 "점" 표기법을 허용합니다, 예: "instance.set(x,y)".
    // 다음 예제를 참조하십시오.
    pub fn set(self: *Self, x: u32, y: u32) void {
        self.x = x;
        self.y = y;
    }

    // 다시, 시그니처를 보세요. 첫 번째 인자는 Self 타입입니다 (*Self가 아님),
    // 이것은 포인터가 아닙니다. 이 경우, "self"는 구조체의 인스턴스를
    // 참조하지만, 수정될 수 없습니다.
    pub fn getx(self: Self) u32 {
        return self.x;
    }

    // PS: 이전 두 함수는 다소 쓸모없을 수 있습니다.
    //     속성은 직접 변경할 수 있으므로 접근자 함수가 필요 없습니다.
    //     이것은 단지 예시였습니다.
};

// 이전 구조체를 사용해 봅시다.
var p = Point{ .x = 0, .y = 0 }; // "p" 변수는 Point입니다.

p.set(10, 30); // "p"의 x와 y 속성은 "set" 함수를 통해 수정됩니다.
print("p.x: {}
", .{p.x}); // 10
print("p.y: {}
", .{p.y}); // 30

// C에서:
//   1. 우리는 point_set(p, 10, 30)과 같이 작성했을 것입니다.
//   2. 모든 함수가 동일한 네임스페이스에 있기 때문에, 다른 구조체에 대해
//      다른 이름의 함수를 만드는 것이 매우 번거로웠을 것입니다.
//      많은 긴 이름, 읽기 고통스러움.
//
// Zig에서, 구조체는 자신의 함수에 대한 네임스페이스를 제공합니다.
// 다른 구조체는 함수에 대해 동일한 이름을 가질 수 있으며,
// 이는 명확성을 가져옵니다.
```

### 튜플.

```zig
// 튜플은 잠재적으로 다른 타입의 요소 목록입니다.

const foo = .{ "hello", true, 42 };
// foo.len == 3
```

### 열거형.

```zig
const Type = enum { ok, not_ok };

const CardinalDirections = enum { North, South, East, West };
const direction: CardinalDirections = .North;
const x = switch (direction) {
    // CardinalDirections.North의 단축 표현
    .North => true,
    else => false
};

// Switch 문은 완전성을 요구합니다.
// 경고: 컴파일되지 않습니다. East와 West가 빠졌습니다.
const x = switch (direction) {
    .North => true,
    .South => true,
};

// 이것은 모든 가능한 값을 철저히 나열하므로 오류 없이 컴파일됩니다.
const x = switch (direction) {
    .North => true,
    .South => true,
    .East,          // 그 값은 다음 패턴과 동일합니다: false.
    .West => false,
};


// 열거형은 구조체와 같습니다: 함수를 가질 수 있습니다.
```

### 공용체.

```zig
const Bar = union {
    boolean: bool,
    int: i16,
    float: f32,
};

// 두 구문은 동일합니다.
const foo = Bar{ .int = 42 };
const foo: Bar = .{ .int = 42 };

// 공용체는 열거형 및 구조체와 마찬가지로 함수를 가질 수 있습니다.
```

### 태그된 공용체.

```zig
// 공용체는 enum 태그 타입으로 선언될 수 있으며, switch 표현식에서
// 사용될 수 있습니다.

const MaybeEnum = enum {
    success,
    failure,
};

const Maybe = union(MaybeEnum) {
    success: u8,
    failure: []const u8,
};

// 첫 번째 값: 성공!
const yay = Maybe{ .success = 42 };
switch (yay) {
    .success => |value|     std.log.info("success: {}", .{value}),
    .failure => |err_msg|   std.log.info("failure: {}", .{err_msg}),
}

// 두 번째 값: 실패! :(
const nay = Maybe{ .failure = "I was too lazy" };
switch (nay) {
    .success => |value|     std.log.info("success: {}", .{value}),
    .failure => |err_msg|   std.log.info("failure: {}", .{err_msg}),
}
```

### Defer와 errdefer.

```zig
// 어떤 동작(단일 명령어 또는 코드 블록)이 스코프(함수, 코드 블록)의
// 끝 이전에 실행되도록 보장합니다.
// 오류 발생 시에도 해당 동작은 실행됩니다.
// 메모리 할당 및 일반적인 리소스 관리에 유용합니다.

pub fn main() void {
    // 함수 끝에서 실행되어야 합니다.
    defer print("third!\n", .{});

    {
        // 스코프의 마지막 요소: 즉시 실행됩니다.
        defer print("first!\n", .{});
    }

    print("second!\n", .{});
}

fn hello_world() void {
    defer print("end of function\n", .{}); // "hello world!" 이후

    print("hello world!\n", .{});
}

// errdefer는 오류 발생 시에만 명령어(또는 코드 블록)를 실행합니다.
fn second_hello_world() !void {
    errdefer print("2. something went wrong!\n", .{}); // "foo"가 실패하면.
    defer    print("1. second hello world\n", .{});    // "foo" 이후에 실행됨

    try foo();
}
// Defer 문은 스택처럼 쌓이는 것으로 볼 수 있습니다: 첫 번째 것이 마지막에 실행됩니다.
```

### 메모리 할당자.
메모리는 표준 라이브러리에서 직접 관리되지 않고, 메모리에 대한 작업이 필요할 때마다 "할당자"에게 요청됩니다.
따라서 표준 라이브러리는 개발자가 모든 메모리 작업을 처리하는 "할당자"라는 구조체를 통해 필요에 따라 메모리를 처리하도록 합니다.

**참고**: 할당자의 선택은 이 문서의 범위를 벗어납니다.
그것에 대해 책 한 권을 쓸 수도 있습니다.
그러나 여기에 몇 가지 예가 있습니다. 어떤 것을 기대할 수 있는지에 대한 아이디어를 얻기 위해:

- `page_allocator`.
  메모리를 요청할 때마다 메모리 페이지 전체를 할당합니다.
  매우 간단하고, 매우 멍청하며, 매우 낭비적입니다.
- `GeneralPurposeAllocator`.
  먼저 일부 메모리를 얻고 메모리 버킷을 관리하여
  할당 횟수를 줄입니다.
  조금 복잡합니다. 다른 할당자와 결합할 수 있습니다.
  누수를 감지하고 그것들을 찾는 데 유용한 정보를 제공할 수 있습니다.
- `FixedBufferAllocator`.
  메모리를 얻기 위해 고정 버퍼를 사용하고, 커널에 메모리를 요청하지 않습니다.
  매우 간단하고, 제한적이며, 낭비적이지만(할당 해제 불가), 매우 빠릅니다.
- `ArenaAllocator`.
  할당된 모든 메모리를 한 번에 해제할 수 있습니다.
  다른 할당자와 조합하여 사용합니다.
  누수를 피하는 매우 간단한 방법입니다.

첫 번째 예제.

```zig
// "!void"는 함수가 오류를 제외하고는 어떤 값도 반환하지 않음을 의미합니다.
// 이 경우 메모리 할당을 시도하며, 실패할 수 있습니다.
fn foo() !void {
    // 이 예제에서는 페이지 할당자를 사용합니다.
    var allocator = std.heap.page_allocator;

    // "list"는 8비트 부호 없는 정수의 ArrayList입니다.
    // ArrayList는 메모리에서 연속적이고 확장 가능한 요소 목록입니다.
    var list = try ArrayList(u8).initAllocated(allocator);
    defer list.deinit(); // 스코프 끝에서 메모리를 해제합니다. 누수될 수 없습니다.
    // "defer"는 함수의 복잡성(루프, 조건 등)에 관계없이
    // 할당 직후에 메모리 해제를 표현할 수 있게 합니다.

    list.add(5); // 제공된 할당자로 여기서 일부 메모리가 할당됩니다.

    for (list.items) |item| {
        std.debug.print("item: {}
", .{item});
    }
}
```

### 메모리 할당과 오류 관리 및 defer의 결합.

```zig
fn some_memory_allocation_example() !void {
    // 메모리 할당은 실패할 수 있으므로, 메모리 할당을 "try"하고
    // 오류가 발생하면 현재 함수가 그것을 반환합니다.
    var buf = try page_allocator.alloc(u8, 10);
    // 할당 직후에 메모리 해제를 defer합니다.
    // 오류가 발생하더라도 일어날 것입니다.
    defer page_allocator.free(buf);

    // 두 번째 할당.
    // 실패 시, 첫 번째 할당은 올바르게 해제됩니다.
    var buf2 = try page_allocator.alloc(u8, 10);
    defer page_allocator.free(buf2);

    // 실패 시, 이전 두 할당 모두 올바르게 할당 해제됩니다.
    try foo();
    try bar();

    // ...
}
```

### 메모리 할당자: 표준 라이브러리 맛보기.

```zig
// 할당자: 알아야 할 4가지 주요 함수
//   single_value = create (type)
//   destroy (single_value)
//   slice = alloc (type, size)
//   free (slice)

// 페이지 할당자
fn page_allocator_fn() !void {
    var slice = try std.heap.page_allocator.alloc(u8, 3);
    defer std.heap.page_allocator.free(slice);

    // playing_with_a_slice(slice);
}

// GeneralPurposeAllocator
fn general_purpose_allocator_fn() !void {
    // GeneralPurposeAllocator는 구성되어야 합니다.
    // 이 경우, 메모리 누수를 추적하고 싶습니다.
    const config = .{.safety = true};
    var gpa = std.heap.GeneralPurposeAllocator(config){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    var slice = try allocator.alloc(u8, 3);
    defer allocator.free(slice);

    // playing_with_a_slice(slice);
}

// FixedBufferAllocator
fn fixed_buffer_allocator_fn() !void {
    var buffer = [_]u8{0} ** 1000; // 1000개의 u8 배열, 모두 0으로 초기화됨.
    var fba  = std.heap.FixedBufferAllocator.init(buffer[0..]);
    // 참고: buffer[0..]는 배열에서 슬라이스를 만드는 방법입니다.
    //       함수가 배열이 아닌 슬라이스를 받기 때문에, 이것은
    //       타입 시스템을 만족시킵니다.

    var allocator = fba.allocator();

    var slice = try allocator.alloc(u8, 3);
    // "free"가 필요 없습니다, 고정 버퍼 할당자로는 메모리를 해제할 수 없습니다.
    // defer allocator.free(slice);

    // playing_with_a_slice(slice);
}

// ArenaAllocator
fn arena_allocator_fn() !void {
    // 알림: 아레나는 메모리를 할당하지 않고, 내부 할당자를 사용합니다.
    // 이 경우, 아레나 할당자를 페이지 할당자와 결합합니다.
    var arena = std.heap.arena_allocator.init(std.heap.page_allocator);
    defer arena.deinit(); // 함수 끝 = 모든 할당이 해제됩니다.

    var allocator = arena.allocator();

    const slice = try allocator.alloc(u8, 3);
    // "free"가 필요 없습니다, 메모리는 어쨌든 해제될 것입니다.

    // playing_with_a_slice(slice);
}


// 범용 및 아레나 할당자 결합. 둘 다 매우 유용하며,
// 그들의 조합은 모든 사람의 즐겨찾는 요리책에 있어야 합니다.
fn gpa_arena_allocator_fn() !void {
    const config = .{.safety = true};
    var gpa = std.heap.GeneralPurposeAllocator(config){};
    defer _ = gpa.deinit();

    const gpa_allocator = gpa.allocator();

    var arena = arena_allocator.init(gpa_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var slice = try allocator.alloc(u8, 3);
    defer allocator.free(slice);

    // playing_with_a_slice(slice);
}
```

### Comptime.

```zig
// Comptime은 전처리기를 피하는 방법입니다.
// 아이디어는 간단합니다: 컴파일 시에 코드를 실행합니다.

inline fn max(comptime T: type, a: T, b: T) T {
    return if (a > b) a else b;
}

var res = max(u64, 1, 2);
var res = max(f32, 10.50, 32.19);


// Comptime: 제네릭 구조체 생성.

fn List(comptime T: type) type {
    return struct {
        items: []T,

        fn init()   ... { ... }
        fn deinit() ... { ... }
        fn do()     ... { ... }
    };
}

const MyList = List(u8);


// 사용
var list = MyList{
    .items = ... // 메모리 할당
};

list.items[0] = 10;
```

### 조건부 컴파일.

```zig
const available_os = enum { OpenBSD, Linux };
const myos = available_os.OpenBSD;


// 다음 switch는 상수 값을 기반으로 합니다.
// 이것은 유일하게 가능한 결과가 컴파일 타임에 알려져 있음을 의미합니다.
// 따라서 나머지 가능성을 빌드할 필요가 없습니다.
// C의 "#ifdef"와 유사하지만, 전처리기가 필요 없습니다.
const string = switch (myos) {
   .OpenBSD => "OpenBSD is awesome!",
   .Linux => "Linux rocks!",
};

// 이 경우에도 작동합니다.
const myprint = switch(myos) {
    .OpenBSD => std.debug.print,
    .Linux => std.log.info,
}
```

### 함수 테스트하기.

```zig
const std = @import("std");
const expect = std.testing.expect;

// 테스트할 함수.
pub fn some_function() bool {
    return true;
}

// 이 "test" 블록은 "zig test"로 실행할 수 있습니다.
// 컴파일 타임에 함수를 테스트합니다.
test "returns true" {
    expect(false == some_function());
}
```

### 컴파일러 내장 기능.

컴파일러에는 "@"로 시작하는 "내장 기능(built-ins)"이라는 특수 함수가 있습니다.
백 개가 넘는 내장 기능이 있으며, 매우 저수준의 작업을 허용합니다:

- 컴파일 타임 오류, 로깅, 검증
- 안전하지 않은 방식까지 포함한 타입 강제 변환 및 변환
- 정렬 관리
- 메모리 트릭 (예: 구조체에서 필드의 바이트 오프셋 가져오기)
- 컴파일 타임에 함수 호출
- 실행 파일에 파일 포함 (@embedFile)
- 프레임 조작 (예: 비동기 함수용)
- 등등.

예: 열거형은 정수가 아니므로, 내장 기능으로 변환해야 합니다.

```zig
const Value = enum { zero, stuff, blah };
if (@enumToInt(Value.zero)  == 0) { ... }
if (@enumToInt(Value.stuff) == 1) { ... }
if (@enumToInt(Value.blah)  == 2) { ... }
```

### Zig 언어의 몇 가지 "제 발등 찍지 않기" 조치.

- 네임스페이스: 이름 충돌을 쉽게 피할 수 있습니다.
  실제로, 이는 다른 구조체(데이터 타입) 간의 통일된 API를 의미합니다.
- 열거형은 정수가 아닙니다. 열거형을 정수와 비교하려면 변환이 필요합니다.
- 명시적 캐스트, 강제 변환은 존재하지만 제한적입니다.
  타입은 C보다 약간 더 강제됩니다, 맛보기:
    포인터는 정수가 아니며, 명시적 변환이 필요합니다.
    실수로 정밀도를 잃지 않으며, 암시적 강제 변환은 정밀도를 잃을 수 없는 경우에만 허용됩니다.
    공용체는 재해석될 수 없습니다 (정수와 부동소수점이 있는 공용체에서, 실수로 다른 값으로 취할 수 없음).
    등등.
- C의 대부분의 정의되지 않은 동작(UB)을 제거하고, 컴파일러가 하나를 만나면 중지합니다.
- 포인터보다 슬라이스와 배열 구조체가 선호됩니다.
  컴파일러에 의해 강제되는 타입은 포인터 조작보다 오류가 발생하기 쉽습니다.
- 숫자 오버플로는 wrapping 연산자를 사용하여 명시적으로 허용되지 않는 한 오류를 생성합니다.
- `try`와 `catch` 메커니즘.
  편리하고, 간단하게 구현되며(간단한 오류 열거형), 공간이나 계산 시간을 거의 차지하지 않습니다.
- 사용되지 않는 변수는 컴파일러에 의해 오류로 간주됩니다.
- 가리키는 것을 나타내기 위해 많은 포인터 타입이 존재합니다.
  예: 이것이 단일 값인가 배열인가, 길이가 알려져 있는가 등.
- 구조체는 속성에 대한 값이 필요하며, 정의되지 않은 값(스택 쓰레기)을 주는 것은 여전히 가능하지만, 적어도 명시적으로 정의되지 않았습니다.

## 추가 자료

시작으로, 몇 가지 개념이 [zig.guide][zigguide]에 소개되어 있습니다.

[공식 웹사이트][zigdoc]는 언어의 참조 문서를 제공합니다. 표준 라이브러리는 [자체 문서][zigstd]를 가지고 있습니다.

[ziglang]: https://ziglang.org
[zigguide]: https://zig.guide/
[zigdoc]: https://ziglang.org/documentation/
[zigstd]: https://ziglang.org/documentation/master/std/