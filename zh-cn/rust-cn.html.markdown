---
language: rust
contributors:
    - ["P1start", "http://p1start.github.io/"]
translators:
    - ["Guangming Mao", "http://maogm.com"]
filename: learnrust-cn.rs
lang: zh-cn
---

Rust 是由 Mozilla 研究院开发的编程语言。Rust 将底层的性能控制与高级语言的便利性和安全保障结合在了一起。

而 Rust 并不需要一个垃圾回收器或者运行时即可实现这个目的，这使得 Rust 库可以成为一种 C 语言的替代品。

Rust 第一版（0.1 版）发布于 2012 年 1 月，3 年以来一直在紧锣密鼓地迭代。
因为更新太频繁，一般建议使用每夜构建版而不是稳定版，直到最近 1.0 版本的发布。

2015 年 3 月 15 日，Rust 1.0 发布，完美向后兼容，最新的每夜构建版提供了缩短编译时间等新特性。
Rust 采用了持续迭代模型，每 6 周一个发布版。Rust 1.1 beta 版在 1.0 发布时同时发布。

尽管 Rust 相对来说是一门底层语言，它提供了一些常见于高级语言的函数式编程的特性。这让 Rust 不仅高效，并且易用。

```rust
// 这是注释，单行注释...
/* ...这是多行注释 */

///////////////
// 1. 基础   //
///////////////

// 函数 (Functions)
// `i32` 是有符号 32 位整数类型(32-bit signed integers)
fn add2(x: i32, y: i32) -> i32 {
    // 隐式返回 (不要分号)
    x + y
}

// 主函数(Main function)
fn main() {
    // 数字 (Numbers) //

    // 不可变绑定
    let x: i32 = 1;

    // 整形/浮点型数 后缀
    let y: i32 = 13i32;
    let f: f64 = 1.3f64;

    // 类型推导
    // 大部分时间，Rust 编译器会推导变量类型，所以不必把类型显式写出来。
    // 这个教程里面很多地方都显式写了类型，但是只是为了示范。
    // 绝大部分时间可以交给类型推导。
    let implicit_x = 1;
    let implicit_f = 1.3;

    // 算术运算
    let sum = x + y + 13;

    // 可变变量
    let mut mutable = 1;
    mutable = 4;
    mutable += 2;

    // 字符串 (Strings) //

    // 字符串字面量
    let x: &str = "hello world!";

    // 输出
    println!("{} {}", f, x); // 1.3 hello world

    // 一个 `String` – 在堆上分配空间的字符串
    let s: String = "hello world".to_string();

    // 字符串分片(slice) - 另一个字符串的不可变视图
    // 基本上就是指向一个字符串的不可变指针，它不包含字符串里任何内容，只是一个指向某个东西的指针
    // 比如这里就是 `s`
    let s_slice: &str = &s;

    println!("{} {}", s, s_slice); // hello world hello world

    // 数组 (Vectors/arrays) //

    // 长度固定的数组 (array)
    let four_ints: [i32; 4] = [1, 2, 3, 4];

    // 变长数组 (vector)
    let mut vector: Vec<i32> = vec![1, 2, 3, 4];
    vector.push(5);

    // 分片 - 某个数组(vector/array)的不可变视图
    // 和字符串分片基本一样，只不过是针对数组的
    let slice: &[i32] = &vector;

    // 使用 `{:?}` 按调试样式输出
    println!("{:?} {:?}", vector, slice); // [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]

    // 元组 (Tuples) //

    // 元组是固定大小的一组值，可以是不同类型
    let x: (i32, &str, f64) = (1, "hello", 3.4);

    // 解构 `let`
    let (a, b, c) = x;
    println!("{} {} {}", a, b, c); // 1 hello 3.4

    // 索引
    println!("{}", x.1); // hello

    //////////////
    // 2. 类型 (Type)  //
    //////////////

    // 结构体（Sturct)
    struct Point {
        x: i32,
        y: i32,
    }

    let origin: Point = Point { x: 0, y: 0 };

    // 匿名成员结构体，又叫“元组结构体”（‘tuple struct’）
    struct Point2(i32, i32);

    let origin2 = Point2(0, 0);

    // 基础的 C 风格枚举类型（enum）
    enum Direction {
        Left,
        Right,
        Up,
        Down,
    }

    let up = Direction::Up;

    // 有成员的枚举类型
    enum OptionalI32 {
        AnI32(i32),
        Nothing,
    }

    let two: OptionalI32 = OptionalI32::AnI32(2);
    let nothing = OptionalI32::Nothing;

    // 泛型 (Generics) //

    struct Foo<T> { bar: T }

    // 这个在标准库里面有实现，叫 `Option`
    enum Optional<T> {
        SomeVal(T),
        NoVal,
    }

    // 方法 (Methods) //

    impl<T> Foo<T> {
        // 方法需要一个显式的 `self` 参数
        fn get_bar(self) -> T {
            self.bar
        }
    }

    let a_foo = Foo { bar: 1 };
    println!("{}", a_foo.get_bar()); // 1

    // 接口（Traits） （其他语言里叫 interfaces 或 typeclasses） //

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

    ///////////////////////////////////
    // 3. 模式匹配 (Pattern matching) //
    ///////////////////////////////////

    let foo = OptionalI32::AnI32(1);
    match foo {
        OptionalI32::AnI32(n) => println!("it’s an i32: {}", n),
        OptionalI32::Nothing  => println!("it’s nothing!"),
    }

    // 高级模式匹配
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

    ///////////////////////////////
    // 4. 流程控制 (Control flow) //
    ///////////////////////////////

    // `for` 循环
    let array = [1, 2, 3];
    for i in array.iter() {
        println!("{}", i);
    }

    // 区间 (Ranges)
    for i in 0u32..10 {
        print!("{} ", i);
    }
    println!("");
    // 输出 `0 1 2 3 4 5 6 7 8 9 `

    // `if`
    if 1 == 1 {
        println!("Maths is working!");
    } else {
        println!("Oh no...");
    }

    // `if` 可以当表达式
    let value = if true {
        "good"
    } else {
        "bad"
    };

    // `while` 循环
    while 1 == 1 {
        println!("The universe is operating normally.");
    }

    // 无限循环
    loop {
        println!("Hello!");
    }

    ////////////////////////////////////////////////
    // 5. 内存安全和指针 (Memory safety & pointers) //
    ////////////////////////////////////////////////

    // 独占指针 (Owned pointer) - 同一时刻只能有一个对象能“拥有”这个指针
    // 意味着 `Box` 离开他的作用域后，会被安全地释放
    let mut mine: Box<i32> = Box::new(3);
    *mine = 5; // 解引用
    // `now_its_mine` 获取了 `mine` 的所有权。换句话说，`mine` 移动 (move) 了
    let mut now_its_mine = mine;
    *now_its_mine += 2;

    println!("{}", now_its_mine); // 7
    // println!("{}", mine); // 编译报错，因为现在 `now_its_mine` 独占那个指针

    // 引用 (Reference) – 引用其他数据的不可变指针
    // 当引用指向某个值，我们称为“借用”这个值，因为是被不可变的借用，所以不能被修改，也不能移动
    // 借用一直持续到生命周期结束，即离开作用域
    let mut var = 4;
    var = 3;
    let ref_var: &i32 = &var;

    println!("{}", var); //不像 `mine`, `var` 还可以继续使用
    println!("{}", *ref_var);
    // var = 5; // 编译报错，因为 `var` 被借用了
    // *ref_var = 6; // 编译报错，因为 `ref_var` 是不可变引用

    // 可变引用 (Mutable reference)
    // 当一个变量被可变地借用时，也不可使用
    let mut var2 = 4;
    let ref_var2: &mut i32 = &mut var2;
    *ref_var2 += 2;

    println!("{}", *ref_var2); // 6
    // var2 = 2; // 编译报错，因为 `var2` 被借用了
}
```

## 更深入的资料

Rust 还有很多很多其他内容 - 这只是 Rust 最基本的功能，帮助你了解 Rust 里面最重要的东西。
如果想深入学习 Rust，可以去读
[The Rust Programming Language](http://doc.rust-lang.org/book/index.html)
或者上 reddit [/r/rust](http://reddit.com/r/rust) 订阅。
同时 irc.mozilla.org 的 #rust 频道上的小伙伴们也非常欢迎新来的朋友。

你可以在这个在线编译器 [Rust playpen](http://play.rust-lang.org) 上尝试 Rust 的一些特性
或者上[官方网站](http://rust-lang.org).
