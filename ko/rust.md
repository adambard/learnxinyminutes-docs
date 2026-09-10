---
name: Rust
contributors:
    - ["P1start", "http://p1start.github.io/"]
translators:
    - ["Hleej", "https://github.com/RuneShell/"]
filename: learnrust.rs
---

Rust는 Mozilla Rust 팀이 개발하고 발표한 프로그래밍 언어입니다.
Rust는 성능을 위한 저수준 제어와 고수준의 편의성, 안전성을 모두 갖췄습니다.

가비지 컬렉터나 무거운 런타임 없이도 위의 장점들을 제공하기 때문에,
Rust 라이브러리를 C언어 대신 "바꿔 넣어" 사용하는 것이 가능합니다.

Rust의 첫 공개 버전인 0.1은 2012년 1월에 발표되었고,
이후 3년동안 매우 빠르게 개발이 이루어져 안정 버전보다도 나이틀리(nightly) 버전을 사용하는 것이 권장되었습니다.

2015년 5월 15일, 완전한 하위 호환성을 갖춘 Rust 1.0 버전이 출시되었습니다.
컴파일 시간 단축과 컴파일러 개선사항들은 현재 나이틀리 빌드에서 지속적으로 제공되고 있습니다.
Rust는 6개월마다 정기적으로 배포되는 Release train 모델을 적용하고 있습니다.
Rust 1.1 베타 버전은 Rust 1.1 출시와 동시에 공개되었습니다.

Rust는 상대적으로 저수준 언어임에도 불구하고, 고수준 언어에서 쓰이는 함수형 개념들을 일부 가지고 있습니다.
덕분에 Rust는 빠를 뿐만 아니라 코딩하기 쉽고 효율적입니다.

```rust
// 이것은 주석입니다. 이렇게 한 줄 주석을 쓸 수 있습니다.
// 그리고 이렇게 여러 줄을 추가할 수 있습니다.

/* 블록 주석은
  /* 중첩될 수 있습니다. */ */

/// docstring은 이렇게 생겼고 마크다운 문법을 제공합니다.
/// # 예시
///
/// ```
/// let five = 5
/// ```

///////////////
// 1. 기본    //
///////////////

#[allow(dead_code)]
// 함수
// `i32`는 32비트의 부호 있는 정수입니다.
fn add2(x: i32, y: i32) -> i32 {
    // 암시적 반환 (세미콜론(;) 생략)
    x + y
}

#[allow(unused_variables)]
#[allow(unused_assignments)]
#[allow(dead_code)]
// Main함수
fn main() {
    // 숫자 //

    // 불변 변수
    let x: i32 = 1;

    // 정수/실수 리터럴 지정   
    let y: i32 = 13i32;
    let f: f64 = 1.3f64;

    // 타입 추론
    // Rust컴파일러는 대부분의 상황에서 변수의 타입을 추론할 수 있기 때문에,
    // 명시적으로 타입 주석을 쓸 필요는 없습니다.
    // 하지만 이 문서에서는 예시를 보이기 위해 타입 주석을 사용합니다.
    // 일반적으로 아래와 같이 암시적으로 정의된 타입은 자동으로 추론됩니다.
    let implicit_x = 1;
    let implicit_f = 1.3;

    // 산술 연산
    let sum = x + y + 13;

    // 가변 변수
    let mut mutable = 1;
    mutable = 4;
    mutable += 2;

    // 문자열 //

    // 문자열 리터럴
    let x: &str = "hello world!";

    // 문자열 출력
    println!("{} {}", f, x); // 1.3 hello world!

    // `String` – 힙에 저장되는 문자열
    // `Vec<u8>`형태이며 항상 유효한 UTF-8 배열이지만, 
    // null 문자('\0')로 끝나지는 않습니다.
    let s: String = "hello world".to_string();

    // 문자열 슬라이스(slice) – 다른 문자열에 대한 불변 뷰
    // 실제로 문자열의 내용을 포함하지 않고, 
    // 문자열 시작지점의 포인터와 문자열 버퍼의 길이만 포함합니다. 
    // 이 버퍼는 정적으로 할당되었거나 다른 객체에 포함되어 있습니다(여기서는 `s`).
    // 문자열 슬라이스는 `Vec<T>`에 대한 `&[u8]` 뷰와 같습니다.
    let s_slice: &str = &s;

    println!("{} {}", s, s_slice); // hello world hello world

    // 벡터 / 배열 //

    // 고정 크기 배열 
    let four_ints: [i32; 4] = [1, 2, 3, 4];

    // 벡터 (동적 배열)
    let mut vector: Vec<i32> = vec![1, 2, 3, 4];
    vector.push(5);

    // 슬라이스 – 벡터와 배열에 대한 값을 변경할 수 없는 뷰
    // 문자열 슬라이스와 매우 유사하지만, 벡터에서 쓰입니다. 
    let slice: &[i32] = &vector;

    // `{:?}` 를 사용하여 debug스타일로 출력할 수 있습니다.
    println!("{:?} {:?}", vector, slice); // [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]

    // 튜플 //

    // 튜플은 서로 다른 타입의 값들을 저장할 수 있는 고정 크기의 배열입니다.
    let x: (i32, &str, f64) = (1, "hello", 3.4);

    // `let`을 이용한 분해
    let (a, b, c) = x;
    println!("{} {} {}", a, b, c); // 1 hello 3.4

    // 인덱싱
    println!("{}", x.1); // hello

    //////////////
    // 2. 타입   //
    //////////////

    // 구조체
    struct Point {
        x: i32,
        y: i32,
    }

    let origin: Point = Point { x: 0, y: 0 };

    // 이름 없는 필드를 가진 구조체, ‘튜플 구조체’라고 부릅니다.
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
    // 뭔가를 선택적으로 만들고 싶다면, 
    // 표준 라이브러리에 `Option`이 있습니다.
    enum OptionalI32 {
        AnI32(i32),
        Nothing,
    }

    let two: OptionalI32 = OptionalI32::AnI32(2);
    let nothing = OptionalI32::Nothing;

    // 제네릭(Generics) //

    struct Foo<T> { bar: T }

    // 이것은 표준 라이브러리에 정의된 `Option`입니다. 
    // `Option`은 일반적으로 null포인터 대신에 쓰입니다.
    enum Optional<T> {
        SomeVal(T),
        NoVal,
    }

    // 메소드(Methods) //

    impl<T> Foo<T> {
        // 메소드는 명시적으로 `self` 파라미터를 갖습니다.
        fn bar(&self) -> &T { // self는 빌려온 값입니다. 
            &self.bar
        }
        fn bar_mut(&mut self) -> &mut T { // self는 수정할 수 있게 빌려온 값입니다.
            &mut self.bar
        }
        fn into_bar(self) -> T { // self는 소유권을 반환함과 동시에 소멸합니다.
            self.bar
        }
    }

    let a_foo = Foo { bar: 1 };
    println!("{}", a_foo.bar()); // 1

    // 트레이트(Traits) (다른 언어에서는 인터페이스나 타입 클래스라고 불리는 것들) //

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
    // 3. 패턴 매칭         //
    /////////////////////////

    let foo = OptionalI32::AnI32(1);
    match foo {
        OptionalI32::AnI32(n) => println!("이건 i32에요: {}", n),
        OptionalI32::Nothing  => println!("이건 값을 가지지 않아요!"),
    }

    // 고급 패턴 매칭
    struct FooBar { x: i32, y: OptionalI32 }
    let bar = FooBar { x: 15, y: OptionalI32::AnI32(32) };

    match bar {
        FooBar { x: 0, y: OptionalI32::AnI32(0) } =>
            println!("숫자들이 모두 0이에요!"),
        FooBar { x: n, y: OptionalI32::AnI32(m) } if n == m =>
            println!("숫자들이 서로 같아요"),
        FooBar { x: n, y: OptionalI32::AnI32(m) } =>
            println!("숫자가 달라요: {} {}", n, m),
        FooBar { x: _, y: OptionalI32::Nothing } =>
            println!("두 번째 숫자가 값을 가지지 않아요!"),
    }

    /////////////////////
    // 4. 제어 흐름     //
    /////////////////////

    // `for`반복문 / 반복자
    let array = [1, 2, 3];
    for i in array {
        println!("{}", i);
    }

    // 범위
    for i in 0u32..10 {
        print!("{} ", i);
    }
    println!("");
    // prints `0 1 2 3 4 5 6 7 8 9 `

    // `if`문
    if 1 == 1 {
        println!("수학이 정상입니다!");
    } else {
        println!("맙소사...");
    }

    // 표현식으로 사용된 `if`
    let value = if true {
        "좋아요"
    } else {
        "별로네요"
    };

    // `while`반복문
    while 1 == 1 {
        println!("우주가 정상적으로 작동중이에요.");
        // break문은 while문을 빠져나옵니다.
        // 불필요한 반복을 막습니다.
        break
    }

    // 무한 반복문
    loop {
        println!("Hello!");
        // break문으로 반복문을 빠져나옵니다.
        break
    }

    /////////////////////////////////
    // 5. 메모리 안전성 & 포인터      //
    /////////////////////////////////

    // 소유된(Owned) 포인터 - 
    // 한번에 오직 하나의 객체만이 이 포인터를 '소유(Own)'할 수 있습니다.
    // 즉 `Box`가 선언된 범위(scope)를 벗어나면, 안전하게 자동으로 할당 해제됩니다.
    let mut mine: Box<i32> = Box::new(3);
    *mine = 5; // 역참조
    // 여기서 `now_its_mine`이 `mine`의 소유권을 가져옵니다. 
    // 즉, `mine`은 더 이상 유효하지 않습니다.
    let mut now_its_mine = mine;
    *now_its_mine += 2;

    println!("{}", now_its_mine); // 7
    // 아래는 `now_its_mine`이 포인터를 소유하고 있기 때문에, 컴파일 에러가 발생합니다.
    // println!("{}", mine); 
    // 참조(Reference) – 다른 데이터를 참조(refer)하는 불변 데이터.
    // 어떤 값을 참조하면, 그 값은 '빌려왔다(borrowed)'고 할 수 있습니다.
    // 값을 불변으로 빌려왔다면, 그 값을 수정하거나 이동할 수 없습니다.
    // 빌린 값(참조)은 해당 참조의 마지막 사용 시점까지 유효합니다.
    let mut var = 4;
    var = 3;
    let ref_var: &i32 = &var;

    println!("{}", var); // `mine`이랑 다르게, 빌려온 `var`은 여전히 유효합니다.
    println!("{}", *ref_var);
    // var = 5; // `var`은 빌려줬기 때문에, 컴파일 에러가 발생합니다.
    // *ref_var = 6; // 불변으로 빌려온 값이기 때문에, 컴파일 에러가 발생합니다.
    ref_var; // 아무것도 하지 않지만, 사용으로 간주되어 빌림 상태를 유지합니다.
    var = 2; // `ref_var`는 위 줄 이후로 사용되지 않으므로 빌림이 해제됩니다.

    // 가변 참조
    // 값을 가변으로 빌렸다면, 빌림이 유지되는 동안 원본 변수를 사용할 수 없습니다.
    let mut var2 = 4;
    let ref_var2: &mut i32 = &mut var2;
    *ref_var2 += 2; // '*' 는 가변적으로 빌려온 `var2`를 가리킵니다.

    println!("{}", *ref_var2); // 6 , // var2는 컴파일할 수 없습니다.
    // `ref_var2`는 &mut i32 타입이므로, i32의 값 대신에 참조를 저장합니다.
    // var2 = 2; // `var2`는 빌려준 상태이므로 컴파일 에러가 발생합니다.
    ref_var2; // 아무것도 하지 않지만, 사용된 것으로 간주되어 빌림 상태를 유지합니다.
}
```

## 기타 참고자료

Rust와 그 심볼/키워드에 대해 더 자세한(그래도 빠른) 설명을 보고 싶다면, Fasterthanlime의 [30분만에 Rust 배우기](https://fasterthanli.me/articles/a-half-hour-to-learn-rust)에서 명확하고 간결하게 (거의) 모든 것을 설명해줍니다!

Rust에는 더 많은 것이 있습니다. - 이것은 Rust의 기본일 뿐이므로 가장 중요한 것을 배울 수 있습니다. Rust의 더 많은 것들을 배우고 싶다면, [The Rust Programming
Language](http://doc.rust-lang.org/book/index.html) 를 읽고
[/r/rust](http://reddit.com/r/rust) 서브레딧을 확인해 보세요. irc.mozilla.org의 #rust 채널 사람들도 항상 Rust 초보자들을 돕고 싶어합니다.

공식 [Rust Playground](https://play.rust-lang.org)나 [Rust 웹사이트](http://rust-lang.org)에서 온라인 컴파일러로 Rust를 사용해볼 수도 있습니다.
