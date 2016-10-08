---
language: rust
filename: rust-pt.rs
contributors:
    - ["Paulo Henrique Rodrigues Pinheiro", "https://about.me/paulohrpinheiro"]
filename: learnrust.rs
lang: pt-br
---

Rust é uma linguagem de programação desenvolvida pelo Mozilla Research. Rust
combina controle de baixo nível sobre o desempenho com facilidades de alto
nível e garantias de segurança.

Ele atinge esse objetico sem necessitar de um coletor de lixo ou um processo
*runtime*, permitindo que se use bibliotecas Rust em substituição a bibliotecas 
em C.

A primeira versão de Rust, 0.1, apareceu em janeiro de 2012, e por três anos o
desenvolvimento correu tão rapidamente que que até recentemente o uso de
versões estáveis foi desencorajado e em vez disso a recomendação era usar as
versões empacotadas toda noite.

Em 15 de maio de 2015, a versão 1.0 de Rust foi liberada com a garantia total
de compatibilidade reversa. Melhorias no tempo de compilação e em outros
aspectos do compilador estão disponíveis atualmente nas versões empacotadas à
noite. Rust adotou um modelo de versões *train-based* com novas versões
regularmente liberadas a cada seis semanas. A versão 1.1 beta de Rust foi
disponibilizada ao mesmo tempo que a versão 1.0.

Apesar de Rust ser uma linguagem mais e baixo nível, Rust tem alguns conceitos
funcionais geralmente encontradas em linguagens de alto nível. Isso faz Rust
não apenas rápido, mas também fácil e eficiente para programar.

```rust
// Isso é um comentário. Linhas de comentários são assim...
// e múltiplas linhas se parecem assim.

/// Comentários para documentação são assim e permitem notação em markdown.
/// # Exemplos
///
/// ```
/// let five = 5
/// ```

///////////////
// 1. Básico //
///////////////

// Funções
// `i32` é o tipo para inteiros com sinal de 32 bits
fn add2(x: i32, y: i32) -> i32 {
    // Implicit return (no semicolon)
    x + y
}

// Função main
fn main() {
    // Números //

    // Immutable bindings
    let x: i32 = 1;

    // Inteiros/Sufixos para ponto flutuante
    let y: i32 = 13i32;
    let f: f64 = 1.3f64;

    // Inferência de tipos
    // Em geral, o compilador Rust consegue inferir qual o tipo de uma 
    // variável, então você não tem que escrever uma anotação explícita de tipo.
    // Ao longo desse tutorial, os tipos serão explicitamente anotados em 
    // muitos lugares, mas apenas com propóstico demonstrativo. A inferência de 
    // tipos pode gerenciar isso na maioria das vezes.
    let implicit_x = 1;
    let implicit_f = 1.3;

    // Aritmética
    let sum = x + y + 13;

    // Variáveis mutáveis
    let mut mutable = 1;
    mutable = 4;
    mutable += 2;

    // Strings //

    // String literais
    let x: &str = "hello world!";

    // Imprimindo
    println!("{} {}", f, x); // 1.3 hello world

    // Uma `String` – uma String alocada no heap
    let s: String = "hello world".to_string();

    // Uma String slice - uma visão imutável em outra string.
    // Basicamente, isso é um par imutável de ponteiros para uma string - ele
    // não contém o conteúdo de uma strinf, apenas um ponteiro para o começo e 
    // um ponteiro para o fim da área de memória para a string, estaticamente
    // alocada ou contida em outro objeto (nesse caso, `s`)
    let s_slice: &str = &s;

    println!("{} {}", s, s_slice); // hello world hello world

    // Vetores/arrays //

    // Um array de tamanho fixo
    let four_ints: [i32; 4] = [1, 2, 3, 4];

    // Um array dinâmico (vetor)
    let mut vector: Vec<i32> = vec![1, 2, 3, 4];
    vector.push(5);

    // Uma fatia – uma visão imutável em um vetor ou array
    // Isso é como um string slice, mas para vetores
    let slice: &[i32] = &vector;

    // Use `{:?}` para imprimir alguma coisa no estilo de depuração
    println!("{:?} {:?}", vector, slice); // [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]

    // Tuplas //

    // Uma tupla é um conjunto de tamanho fixo de valores de tipos
    // possivelmente diferentes
    let x: (i32, &str, f64) = (1, "hello", 3.4);

    // Desestruturando `let`
    let (a, b, c) = x;
    println!("{} {} {}", a, b, c); // 1 hello 3.4

    // Indexando
    println!("{}", x.1); // hello

    //////////////
    // 2. Tipos //
    //////////////

    // Struct
    struct Point {
        x: i32,
        y: i32,
    }

    let origin: Point = Point { x: 0, y: 0 };

    // Uma estrutura com campos sem nome, chamada 'estrutura em tupla'
    struct Point2(i32, i32);

    let origin2 = Point2(0, 0);

    // enum básico com na linguagem C
    enum Direction {
        Left,
        Right,
        Up,
        Down,
    }

    let up = Direction::Up;

    // Enum com campos
    enum OptionalI32 {
        AnI32(i32),
        Nothing,
    }

    let two: OptionalI32 = OptionalI32::AnI32(2);
    let nothing = OptionalI32::Nothing;

    // Generics //

    struct Foo<T> { bar: T }

    // Isso é definido na biblioteca padrão como um `Option`
    enum Optional<T> {
        SomeVal(T),
        NoVal,
    }

    // Methods //

    impl<T> Foo<T> {
        // Métodos recebem um parâmetro `self` explícito
        fn get_bar(self) -> T {
            self.bar
        }
    }

    let a_foo = Foo { bar: 1 };
    println!("{}", a_foo.get_bar()); // 1

    // Traits (conhecidos como interfaces ou typeclasses em outras linguagens)//

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

    //////////////////////////////////
    // 3. Reconhecimento de padrões //
    //////////////////////////////////

    let foo = OptionalI32::AnI32(1);
    match foo {
        OptionalI32::AnI32(n) => println!("it’s an i32: {}", n),
        OptionalI32::Nothing  => println!("it’s nothing!"),
    }

    // Reconhecimento avançado de padrões
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

    //////////////////////////
    // 4. Controle de fluxo //
    //////////////////////////

    // `for` laços de repetição/iteração
    let array = [1, 2, 3];
    for i in array.iter() {
        println!("{}", i);
    }

    // Ranges
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

    // `if` como expressão
    let value = if true {
        "good"
    } else {
        "bad"
    };

    // laço `while` de repetição
    while 1 == 1 {
        println!("The universe is operating normally.");
    }

    // Repetição infinita
    loop {
        println!("Hello!");
    }

    ////////////////////////////////////////
    // 5. Proteção de memória & ponteiros //
    ////////////////////////////////////////

    // Ponteiro com dono - somente uma coisa pode 'possuir' esse ponteiro por
    // vez.
    // Isso significa que quando `Box` perde seu escopo, ele pode ser
    // automaticamente desalocado com segurança.
    let mut mine: Box<i32> = Box::new(3);
    *mine = 5; // dereference
    // Aqui, `now_its_mine` possui o controle exclusivo de `mine`. Em outras 
    // palavras, `mine` tem o controle transferido.
    let mut now_its_mine = mine;
    *now_its_mine += 2;

    println!("{}", now_its_mine); // 7
    // println!("{}", mine); // não compila porque `now_its_mine` é o dono

    // Referência - um ponteiro imutável que referencia outro dado
    // Quando uma referência é dada a um valor, nós dizemos que o valor foi
    // emprestado 'borrowed'.
    // Quando um valor é emprestado sem ser mutável, ele não pode ser alterado
    // ou ter a sua propriedade transferida.
    // Um empréstimo finaliza quando o escopo em que ele foi criado termina.

    let mut var = 4;
    var = 3;
    let ref_var: &i32 = &var;

    println!("{}", var); // AO contrário de `mine`, `var` ainda pode ser usado
    println!("{}", *ref_var);
    // var = 5; // não compila porque `var` é emprestado
    // *ref_var = 6; // não compila, porque `ref_var` é uma referência imutável

    // Referência mutável
    // Quando um valor mutável é emprestado, ele não pode ser acessado.
    let mut var2 = 4;
    let ref_var2: &mut i32 = &mut var2;
    *ref_var2 += 2;         // '*' aponta para var2, que é mutável e emprestada

    println!("{}", *ref_var2); // 6 , // var2 não compila.
    // ref_var2 é do tipo &mut i32, que guarda uma referência i32, não o valor.
    // var2 = 2; // não compila porque `var2` é empretada.
}
```

## Outras leituras

Existe muita coisa sobre Rust - isto aqui é apenas o básico para que você possa 
entender as coisas mais importantes. Para aprender mais sobre Rust, leia [The 
Rust Programming Language](http://doc.rust-lang.org/book/index.html) e 
acompanhe [/r/rust](http://reddit.com/r/rust). A galera no canal #rust do
irc.mozilla.org também estão sempre dispostos a ajudar os novatos.

Você pode brincar com outras característica de Rust com um compilador online
no portal oficial do projeto [Rust playpen](http://play.rust-lang.org), or ler 
mais na página oficial [Rust website](http://rust-lang.org).

No Brasil acompanhe os encontros do [Meetup Rust São Paulo]
(http://www.meetup.com/pt-BR/Rust-Sao-Paulo-Meetup/).

