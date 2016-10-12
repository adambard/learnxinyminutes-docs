---
language: F#
contributors:
    - ["Scott Wlaschin", "http://fsharpforfunandprofit.com/"]
translators:
    - ["Alois de Gouvello", "https://github.com/aloisdg"]
filename: learnfsharp-fr.fs
---

F# est un langage de programmation fonctionnel et orienté objet. Il est gratuit et son code source est ouvert. Il tourne sur Linux, Mac, Windows et plus. 

Il a un puissant système de type qui piège de nombreuses erreurs à la compilation, mais il utilise l'inférence de type donc il se lit plus comme un langage dynamique.

La syntaxe de F# est différente des langages héritant de C.

* Les accolades ne sont pas utilisées pour délimiter les blocs de code. À la place, l'indentation est utilisée (à la manière de Python).
* Les espaces sont utilisés pour séparer les paramètres à la place des virgules.

Si vous voulez essayer le code ci-dessous, vous pouvez vous rendre sur [tryfsharp.org](http://www.tryfsharp.org/Create) et le coller dans le [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

```csharp

// Les commentaires d'une seule ligne commencent par un double slash
(* Les commentaires multilignes utilise les paires (* . . . *)

-fin du commentaire mulitlignes- *)

// ================================================
// Syntaxe de base
// ================================================

// ------ "Variables" (mais pas réellement) ------
// Le mot clé "let" définit une valeur (immutable)
let myInt = 5
let myFloat = 3.14
let myString = "hello"           // Notons qu'aucun type n'est nécessaire

// ------ Listes ------
let twoToFive = [2;3;4;5]        // Les crochets créent(?) une liste avec
                                 // des point-virgules pour délimiteurs.
let oneToFive = 1 :: twoToFive   // :: créé(?) une liste avec un nouvel élément
// Le résultat est [1;2;3;4;5]
let zeroToFive = [0;1] @ twoToFive   // @ concatène deux listes

// IMPORTANT: les virgules ne sont jamais utilisées pour délimiter,
// seulement les point-virgules !

// ------ Fonctions ------
// Le mot clé "let" définit aussi le nom d'une fonction.
let square x = x * x          // Notons qu'aucune parenthèse n'est utilisée.
square 3                      // Maitenant, exécutons la fonction.
                              // Encore une fois, aucune paranthèse.

let add x y = x + y           // N'utilisez pas add (x,y) ! Cela signifie
                              // quelque chose de complètement différent.
add 2 3                       // À présent, exécutons la fonction.

// Pour définir une fonction sur plusieurs lignes, utilisons l'indentation.
// Les point-virgules ne sont pas nécessaires.
let evens list =
   let isEven x = x%2 = 0     // Définit "isEven" comme une fonction imbriquée
   List.filter isEven list    // List.filter est une fonction de la librairie
                              // à deux paramètres: un fonction retournant un
                              // booléen et une list sur laquelle travailler

evens oneToFive               // À présent, exécutons la fonction.

// Vous pouvez utilisez les parenthèses pour clarifier.
// Dans cet exemple, "map" est exécutée en première, avec deux arguments,
// ensuite "sum" est exécutée sur le résultat.
// Sans les parenthèses, "List.map" serait passé en argument à List.sum.
let sumOfSquaresTo100 =
   List.sum ( List.map square [1..100] )

// Vous pouvez rediriger la sortie d'une fonction vers une autre avec "|>"
// Rediriger des données est très commun en F#, comme avec les pipes UNIX.

// Voici la même fonction sumOfSquares écrite en utilisant des pipes
let sumOfSquaresTo100piped =
   [1..100] |> List.map square |> List.sum  // "square" est déclaré avant

// Vous pouvez définir des lambdas (fonctions anonymes) grâce au mot clé "fun"
let sumOfSquaresTo100withFun =
   [1..100] |> List.map (fun x -> x*x) |> List.sum

// En F#, il n'y a pas de mot clé "return". Une fonction retourne toujours
// la valeur de la dernière expression utilisée.

// ------ Pattern Matching ------
// Match..with.. est une surcharge de la condition case/switch.
let simplePatternMatch =
   let x = "a"
   match x with
    | "a" -> printfn "x is a"
    | "b" -> printfn "x is b"
    | _ -> printfn "x is something else"   // underscore correspond à tout le reste

// F# n'autorise pas la valeur null par défaut -- vous devez utiliser le type Option
// et ensuite faire correspondre le pattern.  
// Some(..) et None sont approximativement analogue à des wrappers de Nullable
let validValue = Some(99)
let invalidValue = None

// Dans cet exemple, match..with trouve une correspondance à "Some" et à "None",
// et affiche la valeur du "Some" en même temps.
let optionPatternMatch input =
   match input with
    | Some i -> printfn "input is an int=%d" i
    | None -> printfn "input is missing"

optionPatternMatch validValue
optionPatternMatch invalidValue

// ------ Affichage ------
// Les fonctions printf/printfn sont similaires aux fonctions
// Console.Write/WriteLine de C#.
printfn "Printing an int %i, a float %f, a bool %b" 1 2.0 true
printfn "A string %s, and something generic %A" "hello" [1;2;3;4]

// Il y a aussi les fonctions printf/sprintfn pour formater des données
// en string. C'est similaire à String.Format de C#.

// ================================================
// Plus sur les fonctions 
// ================================================

// F# est un véritable langage fonctionel -- les fonctions sont des
// entités de premier ordre et peuvent êtres combinées facilement
// pour créer des constructions puissantes

// Les modules sont utilisés pour grouper des fonctions ensemble.
// L'indentation est nécessaire pour chaque module imbriqué.
module FunctionExamples = 

    // définit un simple fonction d'addition
    let add x y = x + y
    
    // usage basique d'une fonction
    let a = add 1 2
    printfn "1+2 = %i" a
    
    // partial application to "bake in" parameters (?)
    let add42 = add 42
    let b = add42 1
    printfn "42+1 = %i" b
    
    // composition pour combiner des fonctions
    let add1 = add 1
    let add2 = add 2
    let add3 = add1 >> add2
    let c = add3 7
    printfn "3+7 = %i" c
    
    // Fonctions de premier ordre
    [1..10] |> List.map add3 |> printfn "new list is %A"
    
    // Listes de fonctions et plus
    let add6 = [add1; add2; add3] |> List.reduce (>>)
    let d = add6 7
    printfn "1+2+3+7 = %i" d

// ================================================
// Listes et collections
// ================================================

// Il y a trois types de collection ordonnée :
// * Les listes sont les collections immutables les plus basiques
// * Les tableaux sont mmutables et plus efficients
// * Les séquences sont lazy et infinies (e.g. un enumerator)
//
// Des autres collections incluent des maps immutables et des sets
// plus toutes les collections de .NET

module ListExamples = 

    // les listes utilisent des crochets 
    let list1 = ["a";"b"]
    let list2 = "c" :: list1    // :: pour un ajout au début
    let list3 = list1 @ list2   // @ pour la concatenation
    
    // Compréhensions des listes (aka générateurs)
    let squares = [for i in 1..10 do yield i*i] 

    //  Générateur de nombre premier
    let rec sieve = function
        | (p::xs) -> p :: sieve [ for x in xs do if x % p > 0 then yield x ]
        | []      -> []
    let primes = sieve [2..50]
    printfn "%A" primes 
    
    // pattern matching pour les listes
    let listMatcher aList = 
        match aList with
        | [] -> printfn "the list is empty" 
        | [first] -> printfn "the list has one element %A " first 
        | [first; second] -> printfn "list is %A and %A" first second 
        | _ -> printfn "the list has more than two elements"    

    listMatcher [1;2;3;4]
    listMatcher [1;2]
    listMatcher [1]
    listMatcher []        

    // Récursion en utilisant les listes
    let rec sum aList = 
        match aList with
        | [] -> 0
        | x::xs -> x + sum xs
    sum [1..10]

    // -----------------------------------------    
    // Fonctions de la librairie standard 
    // -----------------------------------------

    // map
    let add3 x = x + 3
    [1..10] |> List.map add3

    // filtre
    let even x = x % 2 = 0
    [1..10] |> List.filter even
    
    // beaucoup plus -- se référer à la documentation
    
module ArrayExamples = 

    // les tableaux utilisent les crochets avec des barres
    let array1 = [| "a";"b" |]
    let first = array1.[0]        // accès à l'index en utilisant un point
   
    // pattern matching pour les tableaux est le même que celui des listes
    let arrayMatcher aList = 
        match aList with
        | [| |] -> printfn "the array is empty" 
        | [| first |] -> printfn "the array has one element %A " first 
        | [| first; second |] -> printfn "array is %A and %A" first second 
        | _ -> printfn "the array has more than two elements"    

    arrayMatcher [| 1;2;3;4 |]

    // Fonctions de la librairie standard comme celles des listes
    [| 1..10 |] 
    |> Array.map (fun i -> i+3)
    |> Array.filter (fun i -> i%2 = 0)
    |> Array.iter (printfn "value is %i. ")

module SequenceExamples = 

    // Les séquences utilisent des accolades
    let seq1 = seq { yield "a"; yield "b" }

    // Les séquences peuvent utiliser yield et 
    // peuvent contenir des sous-sequences
    let strange = seq {
        // "yield" ajoute un élément
        yield 1; yield 2;

        // "yield!" ajoute une complète subsequence
        yield! [5..10]
        yield! seq {
            for i in 1..10 do 
              if i%2 = 0 then yield i }}
    // test
    strange |> Seq.toList

    // Les séquences peuvent être créent(?) en utilisant "unfold"
    // Voici la suite de fibonacci
    let fib = Seq.unfold (fun (fst,snd) ->
        Some(fst + snd, (snd, fst + snd))) (0,1)

    // test
    let fib10 = fib |> Seq.take 10 |> Seq.toList
    printf "first 10 fibs are %A" fib10


// ================================================
// Types de données
// ================================================

module DataTypeExamples = 

    // Toutes les données sont immutables par défaut

    // Les tuples sont de simple et rapide types anonymes
    // -- Utilisons une virgule pour créer un tuple
    let twoTuple = 1,2
    let threeTuple = "a",2,true
    
    // Pattern match to unpack(?)
    let x,y = twoTuple  // assigne x=1 y=2

    // ------------------------------------ 
    // Record types have named fields 
    // ------------------------------------ 

    // On utilise "type" avec des accolades pour définir un record type(?)
    type Person = {First:string; Last:string}
    
    // On utilise "let" avec des accolades pour créer un record(?) 
    let person1 = {First="John"; Last="Doe"}

    // Pattern match to unpack(?)
    let {First=first} = person1    // assigne first="john"

    // ------------------------------------ 
    // Union types (aka variants) ont un set(?) de choix
    // Un seul cas peut être valide à la fois.
    // ------------------------------------ 

    // On utilise "type" avec bar/pipe pour definir un union type(?)
    type Temp = 
        | DegreesC of float
        | DegreesF of float
        
    // On utilise un de ces choix pour en créér un
    let temp1 = DegreesF 98.6
    let temp2 = DegreesC 37.0

    // Pattern match on all cases to unpack(?)
    let printTemp = function
       | DegreesC t -> printfn "%f degC" t
       | DegreesF t -> printfn "%f degF" t
    
    printTemp temp1 
    printTemp temp2

    // ------------------------------------ 
    // Recursive types
    // ------------------------------------ 

    // Types can be combined recursively in complex ways 
    // without having to create subclasses
    type Employee = 
      | Worker of Person
      | Manager of Employee list

    let jdoe = {First="John";Last="Doe"}
    let worker = Worker jdoe
    
    // ------------------------------------ 
    // Modelling with types
    // ------------------------------------ 
    
    // Union types are great for modelling state without using flags
    type EmailAddress = 
        | ValidEmailAddress of string
        | InvalidEmailAddress of string

    let trySendEmail email =
        match email with // use pattern matching
        | ValidEmailAddress address -> ()   // send
        | InvalidEmailAddress address -> () // dont send

    // The combination of union types and record types together
    // provide a great foundation for domain driven design.
    // You can create hundreds of little types that accurately 
    // reflect the domain.

    type CartItem = { ProductCode: string; Qty: int }
    type Payment = Payment of float
    type ActiveCartData = { UnpaidItems: CartItem list }
    type PaidCartData = { PaidItems: CartItem list; Payment: Payment}
        
    type ShoppingCart = 
        | EmptyCart  // no data
        | ActiveCart of ActiveCartData
        | PaidCart of PaidCartData    

    // ------------------------------------ 
    // Built in behavior for types
    // ------------------------------------ 

    // Core types have useful "out-of-the-box" behavior, no coding needed.
    // * Immutability
    // * Pretty printing when debugging
    // * Equality and comparison
    // * Serialization
        
    // Pretty printing using %A
    printfn "twoTuple=%A,\nPerson=%A,\nTemp=%A,\nEmployee=%A" 
             twoTuple person1 temp1 worker

    // Equality and comparison built in.
    // Here's an example with cards.
    type Suit = Club | Diamond | Spade | Heart
    type Rank = Two | Three | Four | Five | Six | Seven | Eight 
                | Nine | Ten | Jack | Queen | King | Ace    

    let hand = [ Club,Ace; Heart,Three; Heart,Ace; 
                 Spade,Jack; Diamond,Two; Diamond,Ace ]

    // sorting
    List.sort hand |> printfn "sorted hand is (low to high) %A"
    List.max hand |> printfn "high card is %A"
    List.min hand |> printfn "low card is %A"

             
// ================================================
// Active patterns
// ================================================

module ActivePatternExamples = 

    // F# has a special type of pattern matching called "active patterns" 
    // where the pattern can be parsed or detected dynamically. 

    // "banana clips" are the syntax for active patterns
    
    // for example, define an "active" pattern to match character types...
    let (|Digit|Letter|Whitespace|Other|) ch = 
       if System.Char.IsDigit(ch) then Digit
       else if System.Char.IsLetter(ch) then Letter
       else if System.Char.IsWhiteSpace(ch) then Whitespace
       else Other         

    // ... and then use it to make parsing logic much clearer
    let printChar ch = 
      match ch with
      | Digit -> printfn "%c is a Digit" ch
      | Letter -> printfn "%c is a Letter" ch
      | Whitespace -> printfn "%c is a Whitespace" ch
      | _ -> printfn "%c is something else" ch

    // print a list
    ['a';'b';'1';' ';'-';'c'] |> List.iter printChar

    // -----------------------------------
    // FizzBuzz using active patterns
    // -----------------------------------
    
    // You can create partial matching patterns as well
    // Just use undercore in the defintion, and return Some if matched.
    let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
    let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None

    // the main function
    let fizzBuzz i = 
      match i with
      | MultOf3 & MultOf5 -> printf "FizzBuzz, " 
      | MultOf3 -> printf "Fizz, " 
      | MultOf5 -> printf "Buzz, " 
      | _ -> printf "%i, " i
      
    // test
    [1..20] |> List.iter fizzBuzz 
    
// ================================================
// Conciseness 
// ================================================

module AlgorithmExamples = 

    // F# has a high signal/noise ratio, so code reads 
    // almost like the actual algorithm

    // ------ Example: define sumOfSquares function ------
    let sumOfSquares n = 
       [1..n]              // 1) take all the numbers from 1 to n
       |> List.map square  // 2) square each one
       |> List.sum         // 3) sum the results

    // test   
    sumOfSquares 100 |> printfn "Sum of squares = %A" 
       
    // ------ Example: define a sort function ------  
    let rec sort list =
       match list with
       // If the list is empty   
       | [] ->                            
            []                            // return an empty list
       // If the list is not empty  
       | firstElem::otherElements ->      // take the first element    
            let smallerElements =         // extract the smaller elements    
                otherElements             // from the remaining ones
                |> List.filter (fun e -> e < firstElem) 
                |> sort                   // and sort them
            let largerElements =          // extract the larger ones
                otherElements             // from the remaining ones
                |> List.filter (fun e -> e >= firstElem)
                |> sort                   // and sort them
            // Combine the 3 parts into a new list and return it
            List.concat [smallerElements; [firstElem]; largerElements]

    // test
    sort [1;5;23;18;9;1;3] |> printfn "Sorted = %A" 

// ================================================
// Asynchronous Code
// ================================================

module AsyncExample = 

    // F# has built-in features to help with async code
    // without encountering the "pyramid of doom"
    //
    // The following example downloads a set of web pages in parallel.

    open System.Net
    open System
    open System.IO
    open Microsoft.FSharp.Control.CommonExtensions   

    // Fetch the contents of a URL asynchronously
    let fetchUrlAsync url =        
        async {   // "async" keyword and curly braces 
                  // creates an "async" object
            let req = WebRequest.Create(Uri(url)) 
            use! resp = req.AsyncGetResponse()    
                // use! is async assignment
            use stream = resp.GetResponseStream() 
                // "use" triggers automatic close()
                // on resource at end of scope
            use reader = new IO.StreamReader(stream) 
            let html = reader.ReadToEnd() 
            printfn "finished downloading %s" url 
            }
            
    // a list of sites to fetch
    let sites = ["http://www.bing.com";
                 "http://www.google.com";
                 "http://www.microsoft.com";
                 "http://www.amazon.com";
                 "http://www.yahoo.com"]

    // do it
    sites 
    |> List.map fetchUrlAsync  // make a list of async tasks
    |> Async.Parallel          // set up the tasks to run in parallel
    |> Async.RunSynchronously  // start them off

// ================================================
// .NET compatability
// ================================================

module NetCompatibilityExamples = 

    // F# can do almost everything C# can do, and it integrates
    // seamlessly with .NET or Mono libraries.

    // ------- work with existing library functions  -------
    
    let (i1success,i1) = System.Int32.TryParse("123");
    if i1success then printfn "parsed as %i" i1 else printfn "parse failed"

    // ------- Implement interfaces on the fly! -------
    
    // create a new object that implements IDisposable
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

    // ------- Object oriented code -------
    
    // F# is also a fully fledged OO language.
    // It supports classes, inheritance, virtual methods, etc.

    // interface with generic type
    type IEnumerator<'a> = 
        abstract member Current : 'a
        abstract MoveNext : unit -> bool 

    // abstract base class with virtual methods
    [<AbstractClass>]
    type Shape() = 
        //readonly properties
        abstract member Width : int with get
        abstract member Height : int with get
        //non-virtual method
        member this.BoundingArea = this.Height * this.Width
        //virtual method with base implementation
        abstract member Print : unit -> unit 
        default this.Print () = printfn "I'm a shape"

    // concrete class that inherits from base class and overrides 
    type Rectangle(x:int, y:int) = 
        inherit Shape()
        override this.Width = x
        override this.Height = y
        override this.Print ()  = printfn "I'm a Rectangle"

    //test
    let r = Rectangle(2,3)
    printfn "The width is %i" r.Width
    printfn "The area is %i" r.BoundingArea
    r.Print()        

    // ------- extension methods  -------
        
    //Just as in C#, F# can extend existing classes with extension methods.
    type System.String with
       member this.StartsWithA = this.StartsWith "A"

    //test
    let s = "Alice"
    printfn "'%s' starts with an 'A' = %A" s s.StartsWithA    
    
    // ------- events  -------
   
    type MyButton() =
        let clickEvent = new Event<_>()

        [<CLIEvent>]
        member this.OnClick = clickEvent.Publish

        member this.TestEvent(arg) =
            clickEvent.Trigger(this, arg)

    // test
    let myButton = new MyButton()
    myButton.OnClick.Add(fun (sender, arg) -> 
            printfn "Click event with arg=%O" arg)

    myButton.TestEvent("Hello World!")
        
```

## More Information

For more demonstrations of F#, go to the [Try F#](http://www.tryfsharp.org/Learn) site, or my [why use F#](http://fsharpforfunandprofit.com/why-use-fsharp/) series.

Read more about F# at [fsharp.org](http://fsharp.org/).
