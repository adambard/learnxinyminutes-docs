---
language: Rust
contributors:
    - ["P1start", "http://p1start.github.io/"]
filename: learnrust-jp.rs
translators:
    - ["Takashi Takeda", "https://github.com/Takashicc"]
lang: ja-jp
---

RustはMozilla Researchによって開発されたプログラミング言語です。
Rustは低レベルの性能制御と高レベルの利便性と、安全性の保証を兼ね備えています。

ガベージコレクションやランタイムを必要としないことから上記の目標が達成出来ます。
それによってRustライブラリをC言語の「完全互換品」として使用することが可能です。

Rustの最初のリリースである0.1は、2012年1月にリリースされ、
その後3年間の開発は非常に速く進み、最近までは安定板リリースの利用は推奨されていませんでした。
代わりにナイトリービルドを使用することが一般的なアドバイスでした。

2015年5月15日、後方互換性を完全に保障したRust 1.0がリリースされました。
コンパイル時間などの改善は、現在ナイトリービルドで提供されています。
Rustはトレインリリースモデルを採用しており、6週間ごとにリリースしています。
Rust 1.1 ベータ版は、Rust 1.0がリリースされたのと同時にリリースされました。

Rustは比較的低レベルの言語ですが、一般的に高水準言語に見られるようないくつかの概念を持っています。
これによりRustはただ速いだけではなく、コーディングが簡単で効率的に書くことが出来ます。

```rust
// これはコメントです。 行コメントはこのように書けます...
// そしてこのように複数行に分けて書くこともできます。

/// ドキュメントコメントはこのように書けて、マークダウン記法をサポートしています。
/// # Examples
///
/// ```
/// let five = 5
/// ```

////////////
// 1. 基本 //
////////////

#[allow(dead_code)]
// 関数
// `i32` は32ビットの符号付き整数の型です
fn add2(x: i32, y: i32) -> i32 {
    // 暗黙の戻り値 (セミコロンなし)
    x + y
}

#[allow(unused_variables)]
#[allow(unused_assignments)]
#[allow(dead_code)]
// Main関数
fn main() {
    // 数値 //

    // 不変な変数
    let x: i32 = 1;

    // 整数/浮動小数点の型を数値の末尾に
    let y: i32 = 13i32;
    let f: f64 = 1.3f64;

    // 型推論
    // ほとんどの場合、Rustコンパイラは変数の型を推測することができるため、
    // 明示的に型アノテーションを書く必要はありません。
    // このチュートリアルでは、多くの場所で明示的に型がアノテーションされていますが、
    // あくまで説明目的です。型推論はほとんどの場合処理することができます。
    let implicit_x = 1;
    let implicit_f = 1.3;

    // 算術
    let sum = x + y + 13;

    // 可変な変数
    let mut mutable = 1;
    mutable = 4;
    mutable += 2;

    // 文字列 //

    // 文字列リテラル
    let x: &str = "hello world!";

    // プリント
    println!("{} {}", f, x); // 1.3 hello world

    // `String`はヒープに割り当てられた文字列です。
    // `Vec<u8>`としてヒープに格納され、
    // 常に有効なUTF-8シーケンス(ヌル文字で終了していない)を保持します。
    let s: String = "hello world".to_string();

    // 文字列スライスは不変なビューを別の文字列に変換します。
    // これは基本的に文字列への不変のポインタのペアを保持しており、
    // 文字そのものは保持していません。
    // 文字列バッファの先頭と末尾へのポイントだけを保持しています。
    // 静的割り当てられるか、別のオブジェクトに含まれます。(この場合は`s`)
    // 文字列スライスは`&[u8]`を`Vec<T>`に変換するようなものです。
    let s_slice: &str = &s;

    println!("{} {}", s, s_slice); // hello world hello world

    // ベクター/配列 //

    // 固定サイズの配列
    let four_ints: [i32; 4] = [1, 2, 3, 4];

    // 動的配列(ベクター)
    let mut vector: Vec<i32> = vec![1, 2, 3, 4];
    vector.push(5);

    // スライスは不変なビューをベクターまたは配列に変換します。
    // これは文字列スライスによく似ていますが、ベクターに対してのものです。
    let slice: &[i32] = &vector;

    // デバッグ形式で何かを表示する際は、`{:?}`を使用できます。
    println!("{:?} {:?}", vector, slice); // [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]

    // タプル //

    // タプルは固定サイズの値の集合でできており、値それぞれが異なる型でもよい
    let x: (i32, &str, f64) = (1, "hello", 3.4);

    // `let`をデストラクト
    let (a, b, c) = x;
    println!("{} {} {}", a, b, c); // 1 hello 3.4

    // インデックス
    println!("{}", x.1); // hello

    ///////////
    // 2. 型 //
    ///////////

    // 構造体
    struct Point {
        x: i32,
        y: i32,
    }

    let origin: Point = Point { x: 0, y: 0 };

    // 名前のないフィールドで構成された構造体は、タプル構造体と言われる。
    struct Point2(i32, i32);

    let origin2 = Point2(0, 0);

    // C言語風列挙型
    enum Direction {
        Left,
        Right,
        Up,
        Down,
    }

    let up = Direction::Up;

    // フィールドがある列挙型
    enum OptionalI32 {
        AnI32(i32),
        Nothing,
    }

    let two: OptionalI32 = OptionalI32::AnI32(2);
    let nothing = OptionalI32::Nothing;

    // ジェネリクス //

    struct Foo<T> { bar: T }

    // 以下は標準ライブラリで定義されている`Option`です。
    enum Optional<T> {
        SomeVal(T),
        NoVal,
    }

    // メソッド //

    impl<T> Foo<T> {
        // メソッドは明示的に`self`パラメータを受け取ります。
        fn bar(&self) -> &T { // `self`は借用されています。
            &self.bar
        }
        fn bar_mut(&mut self) -> &mut T { // `self`は可変に借用されています。
            &mut self.bar
        }
        fn into_bar(self) -> T { // `self`は消費されています。
            self.bar
        }
    }

    let a_foo = Foo { bar: 1 };
    println!("{}", a_foo.bar()); // 1

    // トレイト (他の言語ではインターフェースや型クラスとして知られています) //

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

    // 関数ポインタの種類 // 

    fn fibonacci(n: u32) -> u32 {
        match n {
            0 => 1,
            1 => 1,
            _ => fibonacci(n - 1) + fibonacci(n - 2),
        }
    }

    type FunctionPointer = fn(u32) -> u32;

    let fib : FunctionPointer = fibonacci;
    println!("Fib: {}", fib(4));

    /////////////////////////
    // 3. パターンマッチング //
    /////////////////////////

    let foo = OptionalI32::AnI32(1);
    match foo {
        OptionalI32::AnI32(n) => println!("it’s an i32: {}", n),
        OptionalI32::Nothing  => println!("it’s nothing!"),
    }

    // 応用的なパターンマッチング
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

    //////////////////
    // 4. 制御フロー //
    //////////////////

    // `for`ループ/イテレーション
    let array = [1, 2, 3];
    for i in array {
        println!("{}", i);
    }

    // 範囲
    for i in 0u32..10 {
        print!("{} ", i);
    }
    println!("");
    // 右記にある文をプリントします `0 1 2 3 4 5 6 7 8 9 `

    // `if`
    if 1 == 1 {
        println!("Maths is working!");
    } else {
        println!("Oh no...");
    }

    // 式として使う`if`
    let value = if true {
        "good"
    } else {
        "bad"
    };

    // `while` loop
    while 1 == 1 {
        println!("The universe is operating normally.");
        // break文でwhileループから抜け出せます。
        // 無駄な繰り返しを避けることができます。
        break
    }

    // 無限ループ
    loop {
        println!("Hello!");
        // break文でループから抜け出せます。
        break
    }

    ///////////////////////////
    // 5. メモリ安全とポインタ //
    ///////////////////////////

    // 所有ポインタはポインタを「所有」できるのは、一度に1つだけです。
    // つまり、`Box`がそのスコープから離れると、自動的に安全に解放されます。
    let mut mine: Box<i32> = Box::new(3);
    *mine = 5; // デリファレンス
    // ここで`now_its_mine`が`mine`の所有権を取得します。言い換えると`mine`がムーブしました。
    let mut now_its_mine = mine;
    *now_its_mine += 2;

    println!("{}", now_its_mine); // 7
    // println!("{}", mine); // これは`now_its_mine`がポインタを所有しているため、コンパイルができません。

    // 参照は他のデータを参照する不変なポインタです。
    // 値を参照する場合、値を「借用」したと言います。
    // 値が不変に借用されている間は、値を変更したり、ムーブすることはできない。
    // 借用は借用されている変数が使用されている最後まで有効です。
    let mut var = 4;
    var = 3;
    let ref_var: &i32 = &var;

    println!("{}", var); // `mine`と違って、`var`はまだ使用できます。
    println!("{}", *ref_var);
    // var = 5; // `var`が借用されているため、コンパイルができません。
    // *ref_var = 6; // `ref_var`が不変な参照であるため、コンパイルできません。
    ref_var; // 操作は行っていませんが、使用されているとみなされ、借用が有効になります。
    var = 2; // `ref_var`上記行以降使用されていないため、借用は終了しています。

    // 可変な参照
    // 値が可変な借用である間は、一切アクセスできません。
    let mut var2 = 4;
    let ref_var2: &mut i32 = &mut var2;
    *ref_var2 += 2;         // '*'は可変な参照をされた`var2`を指すために使用されます。

    println!("{}", *ref_var2); // 6 , // `var2`はコンパイルされません。
    // `ref_var2`は型が&mut i32であるため、i32への可変な参照が格納されています。値は入っていません。
    // var2 = 2; // `var2`が借用されているため、コンパイルできません。
    ref_var2; // 操作は行っていませんが、使用されているとみなされ、借用が有効になります。
}
```

## 補足資料

Rustにはまだまだ多くの魅力がありますが、ここではRustの基本的な知識をお伝えします。
Rustについてもっと知りたい場合は、[The Rust Programming
Language](http://doc.rust-lang.org/book/index.html)
を読んで、[/r/rust](http://reddit.com/r/rust)をチェックしてみてください。
irc.mozilla.orgの#rustチャンネルにいる人たちも、新参者にも熱心に助けてくれます。

また、Rustの機能を公式のオンラインコンパイラで試すこともできます。
[Rust Playground](https://play.rust-lang.org) またはメインの
[Rust website](http://rust-lang.org).
