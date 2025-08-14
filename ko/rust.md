---
name: Rust
contributors:
    - ["P1start", "http://p1start.github.io/"]
filename: learnrust.rs
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Rust는 Mozilla Research에서 개발한 프로그래밍 언어입니다. Rust는 성능에 대한 저수준 제어와 고수준의 편의성 및 메모리의 안전을 보장합니다.

가비지 수집기나 런타임 없이 이러한 목표를 달성하므로 Rust 라이브러리를 C의 "드롭인 대체품"으로 사용할 수 있습니다.

Rust의 첫 번째 릴리스인 0.1은 2012년 1월에 있었고, 3년 동안 개발이 너무 빠르게 진행되어 최근까지 안정적인 릴리스 사용이 권장되지 않았으며 대신 야간 빌드를 사용하라는 일반적인 조언이 있었습니다.

2015년 5월 15일, Rust 1.0이 완전한 하위 호환성 보장과 함께 릴리스되었습니다. 컴파일 시간 개선 및 컴파일러의 다른 측면에 대한
개선 사항은 현재 야간 빌드에서 사용할 수 있습니다. Rust는 6주마다 정기적으로 릴리스되는 기차 기반 릴리스 모델을 채택했습니다. Rust 1.1 베타는 Rust 1.0 릴리스와 동시에 제공되었습니다.

Rust는 비교적 저수준 언어이지만, 일반적으로 고수준 언어에서 발견되는 일부 함수형 개념을 가지고 있습니다. 이로 인해 Rust는 빠를 뿐만 아니라 코딩하기 쉽고 효율적입니다.

```rust
// 이것은 주석입니다. 한 줄 주석은 이렇게 생겼습니다...
// 그리고 이렇게 여러 줄로 확장됩니다.

/* 블록 주석
  /* 중첩될 수 있습니다. */ */

/// 문서 주석은 이렇게 생겼으며 마크다운 표기법을 지원합니다.
/// # 예제
///
/// ```
/// let five = 5
/// ```

///////////////
// 1. 기본 //
///////////////

#[allow(dead_code)]
// 함수
// `i32`는 32비트 부호 있는 정수 타입입니다.
fn add2(x: i32, y: i32) -> i32 {
    // 암시적 반환 (세미콜론 없음)
    x + y
}

#[allow(unused_variables)]
#[allow(unused_assignments)]
#[allow(dead_code)]
// 메인 함수
fn main() {
    // 숫자 //

    // 불변 바인딩
    let x: i32 = 1;

    // 정수/부동 소수점 접미사
    let y: i32 = 13i32;
    let f: f64 = 1.3f64;

    // 타입 추론
    // 대부분의 경우 Rust 컴파일러는 변수의 타입을 추론할 수 있으므로
    // 명시적인 타입 주석을 작성할 필요가 없습니다.
    // 이 튜토리얼 전체에서 타입은 많은 곳에서 명시적으로 주석 처리되지만,
    // 이는 시연 목적으로만 사용됩니다. 타입 추론은 대부분의 경우
    // 이를 처리할 수 있습니다.
    let implicit_x = 1;
    let implicit_f = 1.3;

    // 산술
    let sum = x + y + 13;

    // 가변 변수
    let mut mutable = 1;
    mutable = 4;
    mutable += 2;

    // 문자열 //

    // 문자열 리터럴
    let x: &str = "hello world!";

    // 출력
    println!("{} {}", f, x); // 1.3 hello world!

    // `String` – 힙에 할당된 문자열
    // `Vec<u8>`로 저장되며 항상 유효한 UTF-8 시퀀스를 포함하며,
    // null로 종료되지 않습니다.
    let s: String = "hello world".to_string();

    // 문자열 슬라이스 – 다른 문자열에 대한 불변 뷰
    // 이것은 기본적으로 문자열의 불변 포인터와 길이입니다.
    // 실제로 문자열의 내용을 포함하지 않고, 문자열 버퍼의 시작에 대한
    // 포인터와 길이만 포함합니다.
    // 정적으로 할당되거나 다른 객체에 포함됩니다(이 경우 `s`).
    // 문자열 슬라이스는 `Vec<T>`에 대한 `&[u8]` 뷰와 같습니다.
    let s_slice: &str = &s;

    println!("{} {}", s, s_slice); // hello world hello world

    // 벡터/배열 //

    // 고정 크기 배열
    let four_ints: [i32; 4] = [1, 2, 3, 4];

    // 동적 배열 (벡터)
    let mut vector: Vec<i32> = vec![1, 2, 3, 4];
    vector.push(5);

    // 슬라이스 – 벡터 또는 배열에 대한 불변 뷰
    // 이것은 문자열 슬라이스와 매우 유사하지만 벡터용입니다.
    let slice: &[i32] = &vector;

    // 디버그 스타일로 무언가를 출력하려면 `{:?}`를 사용합니다.
    println!("{:?} {:?}", vector, slice); // [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]

    // 튜플 //

    // 튜플은 잠재적으로 다른 타입의 값의 고정 크기 집합입니다.
    let x: (i32, &str, f64) = (1, "hello", 3.4);

    // `let` 구조 분해
    let (a, b, c) = x;
    println!("{} {} {}", a, b, c); // 1 hello 3.4

    // 인덱싱
    println!("{}", x.1); // hello

    //////////////
    // 2. 타입 //
    //////////////

    // 구조체
    struct Point {
        x: i32,
        y: i32,
    }

    let origin: Point = Point { x: 0, y: 0 };

    // 이름 없는 필드가 있는 구조체, '튜플 구조체'라고 함
    struct Point2(i32, i32);

    let origin2 = Point2(0, 0);

    // 기본 C와 유사한 열거형
    enum Direction {
        Left,
        Right,
        Up,
        Down,
    }

    let up = Direction::Up;

    // 필드가 있는 열거형
    // 무언가를 선택적으로 만들고 싶다면 표준 라이브러리에
    // `Option`이 있습니다.
    enum OptionalI32 {
        AnI32(i32),
        Nothing,
    }

    let two: OptionalI32 = OptionalI32::AnI32(2);
    let nothing = OptionalI32::Nothing;

    // 제네릭 //

    struct Foo<T> { bar: T }

    // 이것은 표준 라이브러리에서 `Option`으로 정의됩니다.
    // `Option`은 일반적으로 null 포인터가 사용되는
    // 곳에 사용됩니다.
    enum Optional<T> {
        SomeVal(T),
        NoVal,
    }

    // 메서드 //

    impl<T> Foo<T> {
        // 메서드는 명시적인 `self` 매개변수를 받습니다.
        fn bar(&self) -> &T { // self는 빌려옵니다.
            &self.bar
        }
        fn bar_mut(&mut self) -> &mut T { // self는 가변적으로 빌려옵니다.
            &mut self.bar
        }
        fn into_bar(self) -> T { // 여기서 self는 소비됩니다.
            self.bar
        }
    }

    let a_foo = Foo { bar: 1 };
    println!("{}", a_foo.bar()); // 1

    // 트레이트 (다른 언어에서는 인터페이스 또는 타입 클래스로 알려짐) //

    trait Frobnicate<T> {
        fn frobnicate(self) -> Option<T>;
    }

    impl<T> Frobnicate<T> for Foo<T> {
        fn frobnicate(self) -> Option<T> {
            Some(self.bar)
        }
    }

    let another_foo = Foo { bar: 1 };
    println!("{:?}", another_foo.frobnicate()); // Some(1)

    // 함수 포인터 타입 //

    fn fibonacci(n: u32) -> u32 {
        match n {
            0 => 1,
            1 => 1,
            _ => fibonacci(n - 1) + fibonacci(n - 2),
        }
    }

    type FunctionPointer = fn(u32) -> u32;

    let fib : FunctionPointer = fibonacci;
    println!("Fib: {}", fib(4)); // 5

    /////////////////////////
    // 3. 패턴 매칭 //
    /////////////////////////

    let foo = OptionalI32::AnI32(1);
    match foo {
        OptionalI32::AnI32(n) => println!("it’s an i32: {}", n),
        OptionalI32::Nothing  => println!("it’s nothing!"),
    }

    // 고급 패턴 매칭
    struct FooBar { x: i32, y: OptionalI32 }
    let bar = FooBar { x: 15, y: OptionalI32::AnI32(32) };

    match bar {
        FooBar { x: 0, y: OptionalI32::AnI32(0) } =>
            println!("The numbers are zero!"),
        FooBar { x: n, y: OptionalI32::AnI32(m) } if n == m =>
            println!("The numbers are the same"),
        FooBar { x: n, y: OptionalI32::AnI32(m) } =>
            println!("Different numbers: {} {}", n, m),
        FooBar { x: _, y: OptionalI32::Nothing } =>
            println!("The second number is Nothing!"),
    }

    /////////////////////
    // 4. 제어 흐름 //
    /////////////////////

    // `for` 루프/반복
    let array = [1, 2, 3];
    for i in array {
        println!("{}", i);
    }

    // 범위
    for i in 0u32..10 {
        print!("{} ", i);
    }
    println!("");
    // `0 1 2 3 4 5 6 7 8 9 ` 출력

    // `if`
    if 1 == 1 {
        println!("Maths is working!");
    } else {
        println!("Oh no...");
    }

    // 표현식으로서의 `if`
    let value = if true {
        "good"
    } else {
        "bad"
    };

    // `while` 루프
    while 1 == 1 {
        println!("The universe is operating normally.");
        // break 문은 while 루프를 빠져나갑니다.
        //  쓸모없는 반복을 피합니다.
        break
    }

    // 무한 루프
    loop {
        println!("Hello!");
        // break 문은 루프를 빠져나갑니다.
        break
    }

    /////////////////////////////////
    // 5. 메모리 안전성 및 포인터 //
    /////////////////////////////////

    // 소유된 포인터 – 한 번에 한 가지만 이 포인터를 '소유'할 수 있습니다.
    // 이것은 `Box`가 범위를 벗어날 때 자동으로 안전하게
    // 할당 해제됨을 의미합니다.
    let mut mine: Box<i32> = Box::new(3);
    *mine = 5; // 역참조
    // 여기서 `now_its_mine`은 `mine`의 소유권을 가져옵니다. 즉, `mine`은 이동됩니다.
    let mut now_its_mine = mine;
    *now_its_mine += 2;

    println!("{}", now_its_mine); // 7
    // println!("{}", mine); // `now_its_mine`이 이제 포인터를 소유하므로 컴파일되지 않습니다.

    // 참조 – 다른 데이터를 참조하는 불변 포인터
    // 값에 대한 참조가 취해지면 해당 값은 '빌려왔다'고 말합니다.
    // 값이 불변으로 빌려온 동안에는 변경하거나 이동할 수 없습니다.
    // 빌림은 빌리는 변수의 마지막 사용까지 활성화됩니다.
    let mut var = 4;
    var = 3;
    let ref_var: &i32 = &var;

    println!("{}", var); // `mine`과 달리 `var`는 여전히 사용할 수 있습니다.
    println!("{}", *ref_var);
    // var = 5; // `var`가 빌려왔기 때문에 컴파일되지 않습니다.
    // *ref_var = 6; // `ref_var`가 불변 참조이므로 이것도 컴파일되지 않습니다.
    ref_var; // 아무 작업도 하지 않지만 사용으로 간주되어 빌림을 활성 상태로 유지합니다.
    var = 2; // ref_var는 위 줄 이후 더 이상 사용되지 않으므로 빌림이 종료되었습니다.

    // 가변 참조
    // 값이 가변적으로 빌려온 동안에는 전혀 액세스할 수 없습니다.
    let mut var2 = 4;
    let ref_var2: &mut i32 = &mut var2;
    *ref_var2 += 2;         // '*'는 가변적으로 빌려온 var2를 가리키는 데 사용됩니다.

    println!("{}", *ref_var2); // 6 , // var2는 컴파일되지 않습니다.
    // ref_var2는 &mut i32 타입이므로 값이 아닌 i32에 대한 참조를 저장합니다.
    // var2 = 2; // `var2`가 빌려왔기 때문에 컴파일되지 않습니다.
    ref_var2; // 아무 작업도 하지 않지만 사용으로 간주되어 여기까지 빌림을 활성 상태로 유지합니다.
}
```

## 더 읽을거리

Rust와 그 기호/키워드에 대한 더 깊지만 여전히 빠른 설명을 보려면 Fasterthanlime의 [Rust를 배우는 데 30분](https://fasterthanli.me/articles/a-half-hour-to-learn-rust) 기사가 명확하고 간결한 방식으로 (거의) 모든 것을 설명합니다!

Rust에는 더 많은 것이 있습니다. 이것은 Rust의 기본 사항일 뿐이므로 가장 중요한 것을 이해할 수 있습니다. Rust에 대해 더 자세히 알아보려면 [The Rust Programming Language](http://doc.rust-lang.org/book/index.html)를 읽고 [/r/rust](http://reddit.com/r/rust) 서브레딧을 확인하십시오. irc.mozilla.org의 #rust 채널 사람들도 항상 신규 사용자를 돕고 싶어합니다.

공식 [Rust Playground](https://play.rust-lang.org) 또는 주요 [Rust 웹사이트](http://rust-lang.org)에서 온라인 컴파일러로 Rust의 기능을 시험해 볼 수도 있습니다.
