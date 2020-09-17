---
language: F#
filename: learnfsharp-pt.fs
contributors:
    - ["Scott Wlaschin", "http://fsharpforfunandprofit.com"]
    - ["Adelar da Silva Queiróz", "https://adelarsq.github.io"]
lang: pt-br
---

F# é uma linguagem de propósito geral funcional e orientada a objetos. É livre, de código aberto e executa em Linux, Mac, Windows e outros.

Possui um sistema de tipagem poderoso que evita muitos erros em tempo de compilação. Para isto utilizando inferência de tipos, o que a faz se comportar como uma linguagem dinâmica.

A sintaxe é diferente das linguagens do estilo C (C, C#, Java, etc):

* Chaves não são usadas para delimitar blocos de código. Ao invés disso é utilizada indentação (semelhante ao Python).
* Espaços em branco são usados para separar parâmetros, ao invés de vírgulas.

Se você deseja executar o código abaixo, copie e cole em [https://try.fsharp.org](https://try.fsharp.org), que é um REPL online.

```fsharp

// comentários de linhas únicas usam barras duplas
(* comentários de linhas múltiplas usam o par (* . . . *)

-fim do comentário de linhas múltiplas- *)

// ================================================
// Sintaxe básica
// ================================================

// ------ "Variáveis" (mas não exatamente) ------
// A palavra reservada "let" define um valor imutável
let myInt = 5
let myFloat = 3.14
let myString = "hello"           // note que nenhum tipo é necessário

// ------ Listas ------
let twoToFive = [2; 3; 4; 5]     // Colchetes criam uma lista com
                                 // ponto e vírgula como delimitadores
let oneToFive = 1 :: twoToFive   // :: cria uma lista com um novo primeiro elemento
// O resultado é [1; 2; 3; 4; 5]
let zeroToFive = [0; 1] @ twoToFive   // @ concatena duas listas

// IMPORTANTE: vírgulas nunca são usadas como delimitadores, somente ponto e vírgula!

// ------ Funções ------
// A palavra chave "let" também define nomes para funções.
let square x = x * x          // Note que não são usados parêntesis
square 3                      // Agora executando a função. Também sem parêntesis

let add x y = x + y           // Não use add (x,y)! Isto significa algo
                              // completamente diferente.
add 2 3                       // Agora execute a função.

// para definir uma função de múltiplas linhas apenas use indentação. Nenhum ponto e vírgula é necessário
let evens list =
   let isEven x = x % 2 = 0   // Define "isEven" as a sub function. Note
                              // that equality operator is single char "=".
   List.filter isEven list    // List.filter is a library function
                              // with two parameters: a boolean function
                              // and a list to work on

evens oneToFive               // Now run the function

// You can use parens to clarify precedence. In this example,
// do "map" first, with two args, then do "sum" on the result.
// Without the parens, "List.map" would be passed as an arg to List.sum
let sumOfSquaresTo100 =
   List.sum ( List.map square [1..100] )

// You can pipe the output of one operation to the next using "|>"
// Piping data around is very common in F#, similar to UNIX pipes.

// Here is the same sumOfSquares function written using pipes
let sumOfSquaresTo100piped =
   [1..100] |> List.map square |> List.sum  // "square" was defined earlier

// you can define lambdas (anonymous functions) using the "fun" keyword
let sumOfSquaresTo100withFun =
   [1..100] |> List.map (fun x -> x * x) |> List.sum

// In F# there is no "return" keyword. A function always
// returns the value of the last expression used.

// ------ Pattern Matching ------
// Match..with.. is a supercharged case/switch statement.
let simplePatternMatch =
   let x = "a"
   match x with
    | "a" -> printfn "x is a"
    | "b" -> printfn "x is b"
    | _ -> printfn "x is something else"   // underscore matches anything

// F# doesn't allow nulls by default -- you must use an Option type
// and then pattern match.
// Some(..) and None are roughly analogous to Nullable wrappers
let validValue = Some(99)
let invalidValue = None

// In this example, match..with matches the "Some" and the "None",
// and also unpacks the value in the "Some" at the same time.
let optionPatternMatch input =
   match input with
    | Some i -> printfn "input is an int=%d" i
    | None -> printfn "input is missing"

optionPatternMatch validValue
optionPatternMatch invalidValue

// ------ Printing ------
// The printf/printfn functions are similar to the
// Console.Write/WriteLine functions in C#.
printfn "Printing an int %i, a float %f, a bool %b" 1 2.0 true
printfn "A string %s, and something generic %A" "hello" [1; 2; 3; 4]

// There are also sprintf/sprintfn functions for formatting data
// into a string, similar to String.Format in C#.

// ================================================
// More on functions
// ================================================

// F# is a true functional language -- functions are first
// class entities and can be combined easily to make powerful
// constructs

// Modules are used to group functions together
// Indentation is needed for each nested module.
module FunctionExamples =

    // define a simple adding function
    let add x y = x + y

    // basic usage of a function
    let a = add 1 2
    printfn "1 + 2 = %i" a

    // partial application to "bake in" parameters
    let add42 = add 42
    let b = add42 1
    printfn "42 + 1 = %i" b

    // composition to combine functions
    let add1 = add 1
    let add2 = add 2
    let add3 = add1 >> add2
    let c = add3 7
    printfn "3 + 7 = %i" c

    // higher order functions
    [1..10] |> List.map add3 |> printfn "new list is %A"

    // lists of functions, and more
    let add6 = [add1; add2; add3] |> List.reduce (>>)
    let d = add6 7
    printfn "1 + 2 + 3 + 7 = %i" d

// ================================================
// Listas e coleções
// ================================================

// There are three types of ordered collection:
// * Lists are most basic immutable collection.
// * Arrays are mutable and more efficient when needed.
// * Sequences are lazy and infinite (e.g. an enumerator).
//
// Other collections include immutable maps and sets
// plus all the standard .NET collections

module ListExamples =

    // lists use square brackets
    let list1 = ["a"; "b"]
    let list2 = "c" :: list1    // :: is prepending
    let list3 = list1 @ list2   // @ is concat

    // list comprehensions (aka generators)
    let squares = [for i in 1..10 do yield i * i]

    // A prime number generator
    // - this is using a short notation for the pattern matching syntax
    // - (p::xs) is 'first :: tail' of the list, could also be written as p :: xs
    //   this means this matches 'p' (the first item in the list), and xs is the rest of the list
    //   this is called the 'cons pattern'
    // - uses 'rec' keyword, which is necessary when using recursion
    let rec sieve = function
        | (p::xs) -> p :: sieve [ for x in xs do if x % p > 0 then yield x ]
        | []      -> []
    let primes = sieve [2..50]
    printfn "%A" primes

    // pattern matching for lists
    let listMatcher aList =
        match aList with
        | [] -> printfn "the list is empty"
        | [first] -> printfn "the list has one element %A " first
        | [first; second] -> printfn "list is %A and %A" first second
        | first :: _ -> printfn "the list has more than two elements, first element %A" first

    listMatcher [1; 2; 3; 4]
    listMatcher [1; 2]
    listMatcher [1]
    listMatcher []

    // recursion using lists
    let rec sum aList =
        match aList with
        | [] -> 0
        | x::xs -> x + sum xs
    sum [1..10]

    // -----------------------------------------
    // Standard library functions
    // -----------------------------------------

    // mapas
    let add3 x = x + 3
    [1..10] |> List.map add3

    // filtros
    let even x = x % 2 = 0
    [1..10] |> List.filter even

    // muito mais -- veja a documentação

module ArrayExamples =

    // arrays usam colchetes com barra vertical
    let array1 = [| "a"; "b" |]
    let first = array1.[0]        // acesso por índice usando ponto

    // casamento de padrões (pattern matching) para arrays é feito da mesma forma que de listas
    let arrayMatcher aList =
        match aList with
        | [| |] -> printfn "the array is empty"
        | [| first |] -> printfn "the array has one element %A " first
        | [| first; second |] -> printfn "array is %A and %A" first second
        | _ -> printfn "the array has more than two elements"

    arrayMatcher [| 1; 2; 3; 4 |]

    // As funções da biblioteca padrão são as mesmas que para List

    [| 1..10 |]
    |> Array.map (fun i -> i + 3)
    |> Array.filter (fun i -> i % 2 = 0)
    |> Array.iter (printfn "value is %i. ")


module SequenceExamples =

    // sequências usam chaves
    let seq1 = seq { yield "a"; yield "b" }

    // sequências podem usar yield e
    // podem conter subsequencias
    let strange = seq {
        // "yield" adiciona um elemento
        yield 1; yield 2;

        // "yield!" adiciona uma subsequencia
        yield! [5..10]
        yield! seq {
            for i in 1..10 do
              if i % 2 = 0 then yield i }}
    // teste
    strange |> Seq.toList

    // Sequências podem ser criadas usando "unfold"
    // Este é um exemplo da série de Fibonacci
    let fib = Seq.unfold (fun (fst,snd) ->
        Some(fst + snd, (snd, fst + snd))) (0,1)

    // teste
    let fib10 = fib |> Seq.take 10 |> Seq.toList
    printf "first 10 fibs are %A" fib10


// ================================================
// Tipos de dados
// ================================================

module DataTypeExamples =

    // Todos os dados são imutáveis por padrão

    // Tuplas são uma forma rápida de reprentar n elementos de tipos anônimos
    // -- Use a vírgula para criar uma tupla
    let twoTuple = 1, 2
    let threeTuple = "a", 2, true

    // Casamento de padrões (pattern match) para desconstruir
    let x, y = twoTuple  // atribui x = 1, y = 2

    // ------------------------------------
    // O tipo registro possui nomes nos campos
    // ------------------------------------

    // Use "type" com chaves para definir um registro
    type Person = {First:string; Last:string}

    // Use "let" com chaves para criar um registro
    let person1 = {First="John"; Last="Doe"}

    // Casamento de padrões para desconstruir
    let {First = first} = person1    // atribui first="John"

    // ------------------------------------
    // Tipos union (variantes) possuem um conjunto de escolhas
    // Somente um caso pode ser válido por vez.
    // ------------------------------------

    // Use "type" com barra/pipe para definir um union
    type Temp =
        | DegreesC of float
        | DegreesF of float

    // Use qualquer dos tipos para criar um
    let temp1 = DegreesF 98.6
    let temp2 = DegreesC 37.0

    // Casamento de padrões deve cobrir todos os tipos de definidos para desconstruir
    let printTemp = function
       | DegreesC t -> printfn "%f degC" t
       | DegreesF t -> printfn "%f degF" t

    printTemp temp1
    printTemp temp2

    // ------------------------------------
    // Tipos recursivos
    // ------------------------------------

    // Tipos podem ser combinados recursivamente de formas complexas
    // sem ter que criar subclasses
    type Employee =
      | Worker of Person
      | Manager of Employee list

    let jdoe = {First="John"; Last="Doe"}
    let worker = Worker jdoe

    // ------------------------------------
    // Modelando com tipos
    // ------------------------------------

    // Tipos union são muito bons para modelagem de estados sem usar flags
    type EmailAddress =
        | ValidEmailAddress of string
        | InvalidEmailAddress of string

    let trySendEmail email =
        match email with // casamento de padrões
        | ValidEmailAddress address -> ()   // envia
        | InvalidEmailAddress address -> () // não envia

    // A combinação de tipos union e registros juntos
    // provê uma grande fundação para DDD (Domain Driven Design).
    // Você pode criar centenas de pequenos tipos que refletem
    // exatamente o seu domínio.

    type CartItem = { ProductCode: string; Qty: int }
    type Payment = Payment of float
    type ActiveCartData = { UnpaidItems: CartItem list }
    type PaidCartData = { PaidItems: CartItem list; Payment: Payment}

    type ShoppingCart =
        | EmptyCart  // nenhum dado
        | ActiveCart of ActiveCartData
        | PaidCart of PaidCartData

    // ------------------------------------
    // Comportamento padrão para tipos
    // ------------------------------------

    // Tipos padrões possuem um padrão já definido, não precisando de codificação nenhuma.
    // * Imutáveis
    // * Impressão formatada para depuração
    // * Igualdade e comparação
    // * Serialização

    // Impressão formatada usando %A
    printfn "twoTuple=%A,\nPerson=%A,\nTemp=%A,\nEmployee=%A"
             twoTuple person1 temp1 worker

    // Igualdade e comparação padrão.
    // Um exemplo com cartas:
    type Suit = Club | Diamond | Spade | Heart
    type Rank = Two | Three | Four | Five | Six | Seven | Eight
                | Nine | Ten | Jack | Queen | King | Ace

    let hand = [ Club, Ace; Heart, Three; Heart, Ace;
                 Spade, Jack; Diamond, Two; Diamond, Ace ]

    // ordenando
    List.sort hand |> printfn "sorted hand is (low to high) %A"
    List.max hand |> printfn "high card is %A"
    List.min hand |> printfn "low card is %A"


// ================================================
// Padrões ativos (Active patterns)
// ================================================

module ActivePatternExamples =

    // F# possui um tipo especial de casamento de padrões chamado "padrões ativos" ("active patterns")
    // onde o padrão pode ser interpretado ou detectado dinamicamente.

    // parêntesis e barra são a sintaxe para "padrões ativos"

    // Você pode usar "elif" ao invés de "else if" em expressões condicionais.
    // Elas são equivalentes em F#

    // por exemplo, defina um "padrão ativo" para tratar tipos de caracteres...
    let (|Digit|Letter|Whitespace|Other|) ch =
       if System.Char.IsDigit(ch) then Digit
       elif System.Char.IsLetter(ch) then Letter
       elif System.Char.IsWhiteSpace(ch) then Whitespace
       else Other

    // ... e então use ele para interpretar de forma bem mais simples
    let printChar ch =
      match ch with
      | Digit -> printfn "%c is a Digit" ch
      | Letter -> printfn "%c is a Letter" ch
      | Whitespace -> printfn "%c is a Whitespace" ch
      | _ -> printfn "%c is something else" ch

    // imprima a lista
    ['a'; 'b'; '1'; ' '; '-'; 'c'] |> List.iter printChar

    // ------------------------------------------------
    // FizzBuzz usando padrões ativos (active patterns)
    // ------------------------------------------------

    // É possível criar casamento de padrões parcial também
    // Apenas use sublinhado para a definição, e retorne Some se casado.
    let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
    let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None

    // a função principal
    let fizzBuzz i =
      match i with
      | MultOf3 & MultOf5 -> printf "FizzBuzz, "
      | MultOf3 -> printf "Fizz, "
      | MultOf5 -> printf "Buzz, "
      | _ -> printf "%i, " i

    // teste
    [1..20] |> List.iter fizzBuzz

// ================================================
// Expressividade
// ================================================

module AlgorithmExamples =

    // F# possui uma alta razão sinais/ruídos, assim o código
    // é lido praticamento como se descreve o algoritmo

    // ------ Exemplo: defina uma função que faça soma dos quadrados ------
    let sumOfSquares n =
       [1..n]              // 1) pega todos os números de 1 a n
       |> List.map square  // 2) eleva ao quadrado cada um
       |> List.sum         // 3) soma os resultados

    // teste
    sumOfSquares 100 |> printfn "Sum of squares = %A"

    // ------ Examplo: defina uma função de ordenação ------
    let rec sort list =
       match list with
       // Se a lista está vazia
       | [] ->
            []                            // retorna a lista vazia
       // Se a lista não está vazia
       | firstElem::otherElements ->      // pega o primeiro elemento
            let smallerElements =         // extrai os elementos menores
                otherElements             // dos restantes
                |> List.filter (fun e -> e < firstElem)
                |> sort                   // e ordena eles
            let largerElements =          // extrai os elementos maiores
                otherElements             // dos restantes
                |> List.filter (fun e -> e >= firstElem)
                |> sort                   // e ordena eles
            // Combine as 3 partes em uma nova lista e retorne ela
            List.concat [smallerElements; [firstElem]; largerElements]

    // teste
    sort [1; 5; 23; 18; 9; 1; 3] |> printfn "Sorted = %A"

// ================================================
// Código assíncrono
// ================================================

module AsyncExample =

    // F# possui suporte a funcionalidades para ajudar a escrever código assíncrono
    // sem tornar o código difícil de manter ("pyramid of doom")
    //
    // O seguinte exemplo efetua download de um conjunto de páginas em paralelo.

    open System.Net
    open System
    open System.IO
    open Microsoft.FSharp.Control.CommonExtensions

    // Obtém o conteúdo de cara página de forma assíncrona
    let fetchUrlAsync url =
        async {   // a palavra chave "async" e chaves
                  // criam um objeto assíncrono
            let req = WebRequest.Create(Uri(url))
            use! resp = req.AsyncGetResponse()
                // use! é uma atribuição assíncrona
            use stream = resp.GetResponseStream()
                // "use" dispara automaticamente close()
                // no recurso no fim do escopo
            use reader = new IO.StreamReader(stream)
            let html = reader.ReadToEnd()
            printfn "finished downloading %s" url
            }

    // uma lista de sites para fazer download
    let sites = ["http://www.bing.com";
                 "http://www.google.com";
                 "http://www.microsoft.com";
                 "http://www.amazon.com";
                 "http://www.yahoo.com"]

    // efetue
    sites
    |> List.map fetchUrlAsync  // cria uma lista de tarefas assíncronas
    |> Async.Parallel          // coloca as tarefas para executarem em paralelo
    |> Async.RunSynchronously  // inicia cada uma

// ================================================
// Compatibilidade com .NET
// ================================================

module NetCompatibilityExamples =

    // F# pode pode fazer praticamente tudo que C# pode fazer, e integra
    // de forma simples com bibliotecas .NET e Mono

    // ------- usando uma função de uma biblioteca existente -------

    let (i1success, i1) = System.Int32.TryParse("123");
    if i1success then printfn "parsed as %i" i1 else printfn "parse failed"

    // ------- Implementando interfaces de forma simples! -------

    // cria um novo objeto que implementa IDisposable
    let makeResource name =
       { new System.IDisposable
         with member this.Dispose() = printfn "%s disposed" name }

    let useAndDisposeResources =
        use r1 = makeResource "first resource"
        printfn "using first resource"
        for i in [1..3] do
            let resourceName = sprintf "\tinner resource %d" i
            use temp = makeResource resourceName
            printfn "\tdo something with %s" resourceName
        use r2 = makeResource "second resource"
        printfn "using second resource"
        printfn "done."

    // ------- Código orientado a objetos -------

    // F# também possui suporte a orientação a objetos.
    // Possui suporte a classes, herança, métodos virtuais, etc.

    // interface com tipo genérico
    type IEnumerator<'a> =
        abstract member Current : 'a
        abstract MoveNext : unit -> bool

    // classe base abstrata com métodos virtuais
    [<AbstractClass>]
    type Shape() =
        // propriedades somente leitura
        abstract member Width : int with get
        abstract member Height : int with get
        // método não virtual
        member this.BoundingArea = this.Height * this.Width
        // método virtual com implementação base
        abstract member Print : unit -> unit
        default this.Print () = printfn "I'm a shape"

    // classe concreta que herda da classe base e sobrescreve
    type Rectangle(x:int, y:int) =
        inherit Shape()
        override this.Width = x
        override this.Height = y
        override this.Print ()  = printfn "I'm a Rectangle"

    // testes
    let r = Rectangle(2, 3)
    printfn "The width is %i" r.Width
    printfn "The area is %i" r.BoundingArea
    r.Print()

    // ------- métodos de extensão -------

    // Assim como em C#, F# pode extender classes já existentes com métodos de extensão.
    type System.String with
       member this.StartsWithA = this.StartsWith "A"

    // testes
    let s = "Alice"
    printfn "'%s' starts with an 'A' = %A" s s.StartsWithA

    // ------- eventos  -------

    type MyButton() =
        let clickEvent = new Event<_>()

        [<CLIEvent>]
        member this.OnClick = clickEvent.Publish

        member this.TestEvent(arg) =
            clickEvent.Trigger(this, arg)

    // teste
    let myButton = new MyButton()
    myButton.OnClick.Add(fun (sender, arg) ->
            printfn "Click event with arg=%O" arg)

    myButton.TestEvent("Hello World!")

```

## Mais Informações

Para mais demonstrações de F# acesse [why use F#](http://fsharpforfunandprofit.com/why-use-fsharp/).

Leia mais sobre F# em [fsharp.org](http://fsharp.org/) e [dotnet's F# page](https://dotnet.microsoft.com/languages/fsharp).
