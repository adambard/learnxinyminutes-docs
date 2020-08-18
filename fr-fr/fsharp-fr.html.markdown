---
language: F#
lang: fr-fr
contributors:
    - ["Scott Wlaschin", "http://fsharpforfunandprofit.com/"]
translators:
    - ["Alois de Gouvello", "https://github.com/aloisdg"]
filename: learnfsharp-fr.fs
---

F# est un langage de programmation fonctionnel et orienté objet. Il est gratuit et son code source est ouvert. Il tourne sur Linux, Mac, Windows et plus.

Il possède un puissant système de type qui piège de nombreuses erreurs à la compilation, mais il utilise l'inférence de type ce qui lui permet d'être lu comme un langage dynamique.

La syntaxe de F# est différente des langages héritant de C.

* Les accolades ne sont pas utilisées pour délimiter les blocs de code. À la place, l'indentation est utilisée (à la manière de Python).
* Les espaces sont utilisés pour séparer les paramètres à la place des virgules.

Si vous voulez essayer le code ci-dessous, vous pouvez vous rendre sur [tryfsharp.org](http://www.tryfsharp.org/Create) et le coller dans le [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

```fsharp

// Les commentaires d'une seule ligne commencent par un double slash
(* Les commentaires multilignes utilise les paires (* . . . *)

-fin du commentaire multilignes- *)

// ================================================
// Syntaxe de base
// ================================================

// ------ "Variables" (mais pas vraiment) ------
// Le mot clé "let" définit une valeur (immutable)
let myInt = 5
let myFloat = 3.14
let myString = "hello"           // Notons qu'aucun type n'est nécessaire

// ------ Listes ------
let twoToFive = [2;3;4;5]        // Les crochets créent une liste avec
                                 // des point-virgules pour délimiteurs.
let oneToFive = 1 :: twoToFive   // :: crée une liste avec un nouvel élément
// Le résultat est [1;2;3;4;5]
let zeroToFive = [0;1] @ twoToFive   // @ concatène deux listes

// IMPORTANT: les virgules ne sont jamais utilisées pour délimiter,
// seulement les point-virgules !

// ------ Fonctions ------
// Le mot clé "let" définit aussi le nom d'une fonction.
let square x = x * x          // Notons qu'aucune parenthèse n'est utilisée.
square 3                      // Maitenant, exécutons la fonction.
                              // Encore une fois, aucune parenthèse.

let add x y = x + y           // N'utilisez pas add (x,y) ! Cela signifie
                              // quelque chose de complètement différent.
add 2 3                       // À présent, exécutons la fonction.

// Pour définir une fonction sur plusieurs lignes, utilisons l'indentation.
// Les point-virgules ne sont pas nécessaires.
let evens list =
   let isEven x = x%2 = 0     // Définit "isEven" comme une fonction imbriquée
   List.filter isEven list    // List.filter est une fonction de la librairie
                              // à deux paramètres: un fonction retournant un
                              // booléen et une liste sur laquelle travailler

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
// en string. C'est similaire au String.Format de C#.

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

    // application partielle des paramètres (curryfication ou "currying" en anglais)
    // add42 est une nouvelle fonction qui ne prend plus qu'un paramètre
    let add42 = add 42
    let b = add42 1
    printfn "42+1 = %i" b

    // composition pour combiner des fonctions
    let add1 = add 1
    let add2 = add 2
    let add3 = add1 >> add2
    let c = add3 7
    printfn "3+7 = %i" c

    // fonctions de premier ordre
    [1..10] |> List.map add3 |> printfn "new list is %A"

    // listes de fonction et plus
    let add6 = [add1; add2; add3] |> List.reduce (>>)
    let d = add6 7
    printfn "1+2+3+7 = %i" d

// ================================================
// Listes et collections
// ================================================

// Il y a trois types de collection ordonnée :
// * Les listes sont les collections immutables les plus basiques
// * Les tableaux sont mutables et plus efficients
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

    // le pattern matching pour les listes
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

    // le pattern matching des tableaux est le même que celui des listes
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

        // "yield!" ajoute une sous-sequence complète
        yield! [5..10]
        yield! seq {
            for i in 1..10 do
              if i%2 = 0 then yield i }}
    // test
    strange |> Seq.toList

    // Les séquences peuvent être créées en utilisant "unfold"
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

    // Pattern match pour déballer
    let x,y = twoTuple  // assigne x=1 y=2

    // ------------------------------------
    // Record types ont des champs nommés
    // ------------------------------------

    // On utilise "type" avec des accolades pour définir un type record
    type Person = {First:string; Last:string}

    // On utilise "let" avec des accolades pour créer un record (enregistrement)
    let person1 = {First="John"; Last="Doe"}

    // Pattern match pour déballer
    let {First=first} = person1    // assigne first="john"

    // ------------------------------------
    // Union types (ou variants) ont un set (ensemble) de choix
    // Un seul cas peut être valide à la fois.
    // ------------------------------------

    // On utilise "type" avec bar/pipe pour definir un union type
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
    // Types récursif
    // ------------------------------------

    // Les types peuvent être combinés récursivement de façon complexe
    // sans avoir à créer des sous-classes
    type Employee =
      | Worker of Person
      | Manager of Employee list

    let jdoe = {First="John";Last="Doe"}
    let worker = Worker jdoe

    // ------------------------------------
    // Modelling with types(?)
    // ------------------------------------

    // Les types union sont excellents pour modelling state without using flags(?)
    type EmailAddress =
        | ValidEmailAddress of string
        | InvalidEmailAddress of string

    let trySendEmail email =
        match email with // utilisations du pattern matching
        | ValidEmailAddress address -> ()   // envoyer
        | InvalidEmailAddress address -> () // ne pas envoyer

    // Combiner ensemble, les types union et les types record
    // offrent une excellente fondation pour le domain driven design.
    // Vous pouvez créer des centaines de petit types qui reflèteront fidèlement
    // le domain.

    type CartItem = { ProductCode: string; Qty: int }
    type Payment = Payment of float
    type ActiveCartData = { UnpaidItems: CartItem list }
    type PaidCartData = { PaidItems: CartItem list; Payment: Payment}

    type ShoppingCart =
        | EmptyCart  // aucune donnée
        | ActiveCart of ActiveCartData
        | PaidCart of PaidCartData

    // ------------------------------------
    // Comportement natif des types
    // ------------------------------------

    // Les types natifs ont un comportement "prêt-à-l'emploi" des plus utiles, sans code à ajouter.
    // * Immutabilité
    // * Pretty printing au debug
    // * Egalité et comparaison
    // * Sérialisation

    // Le Pretty printing s'utilise avec %A
    printfn "twoTuple=%A,\nPerson=%A,\nTemp=%A,\nEmployee=%A"
             twoTuple person1 temp1 worker

    // L'égalité et la comparaison sont innés
    // Voici un exemple avec des cartes.
    type Suit = Club | Diamond | Spade | Heart
    type Rank = Two | Three | Four | Five | Six | Seven | Eight
                | Nine | Ten | Jack | Queen | King | Ace

    let hand = [ Club,Ace; Heart,Three; Heart,Ace;
                 Spade,Jack; Diamond,Two; Diamond,Ace ]

    // tri
    List.sort hand |> printfn "sorted hand is (low to high) %A"
    List.max hand |> printfn "high card is %A"
    List.min hand |> printfn "low card is %A"

// ================================================
// Les Active patterns
// ================================================

module ActivePatternExamples =

    // F# a un type particulier de pattern matching nommé "active patterns"
    // où le pattern peut être parsé ou détecté dynamiquement.

    // "banana clips" est la syntaxe pour l'active patterns

    // par exemple, on définit un "active" pattern pour correspondre à des types "character"...
    let (|Digit|Letter|Whitespace|Other|) ch =
       if System.Char.IsDigit(ch) then Digit
       else if System.Char.IsLetter(ch) then Letter
       else if System.Char.IsWhiteSpace(ch) then Whitespace
       else Other

    // ... et ensuite on l'utilise pour rendre la logique de parsing plus claire
    let printChar ch =
      match ch with
      | Digit -> printfn "%c is a Digit" ch
      | Letter -> printfn "%c is a Letter" ch
      | Whitespace -> printfn "%c is a Whitespace" ch
      | _ -> printfn "%c is something else" ch

    // afficher une liste
    ['a';'b';'1';' ';'-';'c'] |> List.iter printChar

    // -----------------------------------------
    // FizzBuzz en utilisant les active patterns
    // -----------------------------------------

    // Vous pouvez créer un partial matching patterns également
    // On utilise just un underscore dans la définition, et on retourne Some si ça correspond.
    let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
    let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None

    // la fonction principale
    let fizzBuzz i =
      match i with
      | MultOf3 & MultOf5 -> printf "FizzBuzz, "
      | MultOf3 -> printf "Fizz, "
      | MultOf5 -> printf "Buzz, "
      | _ -> printf "%i, " i

    // test
    [1..20] |> List.iter fizzBuzz

// ================================================
// Concision
// ================================================

module AlgorithmExamples =

    // F# a un haut ratio signal/bruit, permettant au code de se lire
    // presque comme un véritable algorithme

    // ------ Exemple: definir une fonction sumOfSquares ------
    let sumOfSquares n =
       [1..n]              // 1) Prendre tous les nombres de 1 à n
       |> List.map square  // 2) Elever chacun d'entre eux au carré
       |> List.sum         // 3) Effectuer leur somme

    // test
    sumOfSquares 100 |> printfn "Sum of squares = %A"

    // ------ Exemple: definir un fonction de tri ------
    let rec sort list =
       match list with
       // Si la liste est vide
       | [] ->
            []                            // on retourne une liste vide
       // si la list n'est pas vide
       | firstElem::otherElements ->      // on prend le premier élément
            let smallerElements =         // on extrait les éléments plus petits
                otherElements             // on prend les restants
                |> List.filter (fun e -> e < firstElem)
                |> sort                   // et on les trie
            let largerElements =          // on extrait les plus grands
                otherElements             // de ceux qui restent
                |> List.filter (fun e -> e >= firstElem)
                |> sort                   // et on les trie
            // On combine les 3 morceaux dans une nouvelle liste que l'on retourne
            List.concat [smallerElements; [firstElem]; largerElements]

    // test
    sort [1;5;23;18;9;1;3] |> printfn "Sorted = %A"

// ================================================
// Code Asynchrone
// ================================================

module AsyncExample =

    // F# inclus des fonctionnalités pour aider avec le code asynchrone
    // sans rencontrer la "pyramid of doom"
    //
    // L'exemple suivant télécharge une séquence de page web en parallèle.

    open System.Net
    open System
    open System.IO
    open Microsoft.FSharp.Control.CommonExtensions

    // Récupérer le contenu d'une URL de manière asynchrone
    let fetchUrlAsync url =
        async {   // Le mot clé "async" et les accolades
                  // créent un objet "asynchrone"
            let req = WebRequest.Create(Uri(url))
            use! resp = req.AsyncGetResponse()
                // use! est un assignement asynchrone
            use stream = resp.GetResponseStream()
                // "use" déclenche automatiquement close()
                // sur les ressources à la fin du scope
            use reader = new IO.StreamReader(stream)
            let html = reader.ReadToEnd()
            printfn "finished downloading %s" url
            }

    // une liste des sites à rapporter
    let sites = ["http://www.bing.com";
                 "http://www.google.com";
                 "http://www.microsoft.com";
                 "http://www.amazon.com";
                 "http://www.yahoo.com"]

    // C'est parti!
    sites
    |> List.map fetchUrlAsync  // créez une liste de tâche asynchrone
    |> Async.Parallel          // dites aux tâches de tourner en parallèle
    |> Async.RunSynchronously  // démarrez les!

// ================================================
// .NET compatabilité
// ================================================

module NetCompatibilityExamples =

    // F# peut réaliser presque tout ce que C# peut faire, et il s'intègre
    // parfaitement avec les librairies .NET ou Mono.

    // ------- Travaillez avec les fonctions des librairies existantes  -------

    let (i1success,i1) = System.Int32.TryParse("123");
    if i1success then printfn "parsed as %i" i1 else printfn "parse failed"

    // ------- Implémentez des interfaces à la volée! -------

    // Créer un nouvel objet qui implémente IDisposable
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

    // ------- Code orienté objet -------

    // F# est aussi un véritable language OO.
    // Il supporte les classes, l'héritage, les méthodes virtuelles, etc.

    // interface avec type générique
    type IEnumerator<'a> =
        abstract member Current : 'a
        abstract MoveNext : unit -> bool

    // Classe de base abstraite avec méthodes virtuelles
    [<AbstractClass>]
    type Shape() =
        // propriétés en lecture seule
        abstract member Width : int with get
        abstract member Height : int with get
        // méthode non-virtuelle
        member this.BoundingArea = this.Height * this.Width
        // méthode virtuelle avec implémentation de la classe de base
        abstract member Print : unit -> unit
        default this.Print () = printfn "I'm a shape"

    // classe concrète qui hérite de sa classe de base et surcharge
    type Rectangle(x:int, y:int) =
        inherit Shape()
        override this.Width = x
        override this.Height = y
        override this.Print ()  = printfn "I'm a Rectangle"

    // test
    let r = Rectangle(2,3)
    printfn "The width is %i" r.Width
    printfn "The area is %i" r.BoundingArea
    r.Print()

    // ------- extension de méthode  -------

    // Juste comme en C#, F# peut étendre des classes existantes avec des extensions de méthode.
    type System.String with
       member this.StartsWithA = this.StartsWith "A"

    // test
    let s = "Alice"
    printfn "'%s' starts with an 'A' = %A" s s.StartsWithA

    // ------- événements -------

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

## Plus d'information

Pour plus de démonstration de F#, rendez-vous sur le site [Try F#](http://www.tryfsharp.org/Learn), ou suivez la série [why use F#](http://fsharpforfunandprofit.com/why-use-fsharp/).

Apprenez en davantage à propose de F# sur [fsharp.org](http://fsharp.org/).
