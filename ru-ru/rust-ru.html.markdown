---
language: rust

filename: learnrust-ru.rs
contributors:
    - ["P1start", "http://p1start.github.io/"]
translators:
    - ["Anatolii Kosorukov", "https://github.com/java1cprog"]
    - ["Vasily Starostin", "https://github.com/Basil22"]
lang: ru-ru

---

Язык Rust разработан в Mozilla Research. Он сочетает низкоуровневую производительность с удобством языка высокого уровня и одновременно гарантирует безопасность памяти.

Он достигает этих целей без сборщика мусора или сложной среды выполнения, что позволяет использовать библиотеки Rust как прямую замену
C-библиотек. И наоборот, Rust умеет использовать готовые С-библиотеки как есть, без накладных расходов.

Первый выпуск Rust, 0.1, произошел в январе 2012 года. В течение 3 лет развитие продвигалось настолько быстро, что язык серьезно менялся без сохранения совместимости. Это дало возможность обкатать и отполировать синтаксис и возможности языка.

15 мая 2015 года был выпущен Rust 1.0 с полной гарантией обратной совместимости. Сборка поставляется в трех вариантах: стабильная версия, бета-версия, ночная версия. Все нововведения языка сперва обкатываются на ночной и бета-версиях, и только потом попадают в стабильную. Выход очередной версии происходит раз в 6 недель. В 2018 году вышло второе большое обновление языка, добавившее ему новых возможностей.

Хотя Rust является языком относительно низкого уровня, он имеет все возможности высокоуровневых языков: процедурное, объектное, функциональное, шаблонное и другие виды программирования. На данный момент Rust является одним из самых мощных (а может быть и самым) по возможностям среди статически типизированных языков. Это делает Rust не только быстрым, но и простым и эффективным для разработки сложного кода.


```rust
// Это однострочный комментарий
// 

/// Так выглядит комментарий для документации
/// # Examples
///
/// ```
/// let seven  = 7
/// ```

///////////////
// 1. Основы //
///////////////

// Функции
// `i32` это целочисленный знаковый тип 32-bit
#[allow(dead_code)]
fn add2(x: i32, y: i32) -> i32 {
    // метод возвращает сумму x и y
    x + y
}

// Главная функция программы
#[allow(unused_variables)]
#[allow(unused_assignments)]
#[allow(dead_code)]
fn main() {
    // Числа //

    // неизменяемая переменная
    let x: i32 = 1;

    // Суффиксы целое/дробное
    let y: i32 = 13i32;
    let f: f64 = 1.3f64;

    // Автоматическое выведение типа данных
    // В большинстве случаев компилятор Rust может вычислить 
    // тип переменной, поэтому вам не нужно явно указывать тип.

    let implicit_x = 1;
    let implicit_f = 1.3;

    // Арифметика
    let sum = x + y + 13;

    // Изменяемая переменная
    let mut mutable = 1;
    mutable = 4;
    mutable += 2;

    // Строки //

    // Строковые литералы
    let x: &str = "hello world!";

    // Печать на консоль
    println!("{} {}", f, x); // 1.3 hello world

    // `String` – изменяемая строка
    let s: String = "hello world".to_string();

    // Строковый срез - неизменяемое представление части строки
    // Представляет собой пару из указателя на начало фрагмента и его длины

    let s_slice: &str = &s;

    println!("{} {}", s, s_slice); // hello world hello world

    // Vectors/arrays //

    // фиксированный массив
    let four_ints: [i32; 4] = [1, 2, 3, 4];

    // динамический массив
    let mut vector: Vec<i32> = vec![1, 2, 3, 4];
    vector.push(5);

    // Срез - неизменяемое представление значений вектора
    let slice: &[i32] = &vector;

    // Используйте шаблон `{:?}`для печати отладочной информации структур с данными
    println!("{:?} {:?}", vector, slice); // [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]

    // Кортежи //

    // Кортеж - это фиксированный набор. 
    // В нём могут находиться значения разных типов данных.
    let x: (i32, &str, f64) = (1, "hello", 3.4);

    // Инициализация группы переменных `let`
    let (a, b, c) = x;
    println!("{} {} {}", a, b, c); // 1 hello 3.4

    // Доступ по индексу
    println!("{}", x.1); // hello

    //////////////
    // 2. Типы //
    //////////////

    // Структура
    struct Point {
        x: i32,
        y: i32,
    }

    let origin: Point = Point { x: 0, y: 0 };

    // Структуры могут быть с безымянными полями ‘tuple struct’
    struct Point2(i32, i32);

    let origin2 = Point2(0, 0);

    // Перечисление
    enum Direction {
        Left,
        Right,
        Up,
        Down,
    }

    let up = Direction::Up;

    // Перечисление с полями
    // В отличие от C и C++ компилятор автоматически следит за тем,
    // какой именно тип хранится в перечислении.
    enum OptionalI32 {
        AnI32(i32),
        Nothing,
    }

    let two: OptionalI32 = OptionalI32::AnI32(2);
    let nothing = OptionalI32::Nothing;

    // Обобщенные типы данных //

    struct Foo<T> { bar: T }

    // Частоиспользуемое перечисление стандартной библиотеки `Option`
    enum Optional<T> {
        SomeVal(T),
        NoVal,
    }

    // Методы //

    impl<T> Foo<T> {
        fn get_bar(self) -> T {
            self.bar
        }
    }

    let a_foo = Foo { bar: 1 };
    println!("{}", a_foo.get_bar()); // 1

    // Типаж

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

    /////////////////////////////////
    // 3. Сопоставление по шаблону //
    /////////////////////////////////

    let foo = OptionalI32::AnI32(1);
    match foo {
        OptionalI32::AnI32(n) => println!("it’s an i32: {}", n),
        OptionalI32::Nothing  => println!("it’s nothing!"),
    }

    // Более сложный пример
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

    //////////////////////////////////////////////
    // 4. Управление ходом выполнения программы //
    //////////////////////////////////////////////

    // `for` loops/iteration
    let array = [1, 2, 3];
    for i in array.iter() {
        println!("{}", i);
    }

    // Диапазоны
    for i in 0u32..10 {
        print!("{} ", i);
    }
    println!("");
    // prints `0 1 2 3 4 5 6 7 8 9 `

    // `if`
    if 1 == 1 {
        println!("Maths is working!");
    } else {
        println!("Oh no...");
    }

    // `if` as expression
    let value = if true {
        "good"
    } else {
        "bad"
    };

    // `while` loop
    while 1 == 1 {
        println!("The universe is operating normally.");
        break;
    }

    // Infinite loop
    loop {
        println!("Hello!");
        break;
    }

    //////////////////////////////////
    // 5. Защита памяти и указатели //
    //////////////////////////////////

    // Владеющий указатель – такой указатель может быть только один
    // Это значит, что при выходе из блока переменная автоматически становится недействительной.
    let mut mine: Box<i32> = Box::new(3);
    *mine = 5; // dereference
    // Здесь, `now_its_mine` получает во владение `mine`. Т.е. `mine` была перемещена.
    let mut now_its_mine = mine;
    *now_its_mine += 2;

    println!("{}", now_its_mine); // 7
    // println!("{}", mine);  

    //  Ссылки - это неизменяемые указатели
    //  Если ссылка получает значения, то говорят, что она заимствует это значение.
    //  Такое значение не может быть изменено или перемещено.
    let mut var = 4;
    var = 3;
    let ref_var: &i32 = &var;

    println!("{}", var);
    println!("{}", *ref_var);
    // var = 5; // не скомпилируется
    // *ref_var = 6; // и это

    // Изменяемые ссылки
    //
    let mut var2 = 4;
    let ref_var2: &mut i32 = &mut var2;
    *ref_var2 += 2;         // '*' используется для изменения значения

    println!("{}", *ref_var2); // 6 , // var2 would not compile.
    // ref_var2 имеет тип &mut i32, т.е. он содержит ссылку на i32, а не значение.
    // var2 = 2; // не скомпилируется, т.к. эта переменная уже была заимствована ранее
} 

```

## Более подробная информация о языке

Уже есть хорошие книги для изучающих Rust. Основным источником остаётся 
[The Rust Programming Language](http://doc.rust-lang.org/book/index.html) 

Для компиляции программ при изучении языка весьма удобно использовать 
[Rust playpen](http://play.rust-lang.org). 
Множество ресурсов на разных языках можно найти в [этом проекте](https://github.com/ctjhoa/rust-learning).
