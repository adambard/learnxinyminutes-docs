---
language: F#
lang: es-es
contributors:
  - ['Scott Wlaschin', 'http://fsharpforfunandprofit.com/']
translators:
  - ['Angel Arciniega', 'https://github.com/AngelsProjects']
filename: learnfsharp-es.fs
---

F# es un lenguaje de programación funcional y orientado a objetos. Es gratis y su código fuente está abierto. Se ejecuta en Linux, Mac, Windows y más.

Tiene un poderoso sistema de tipado que atrapa muchos errores de tiempo de compilación, pero usa inferencias de tipados que le permiten ser leídos como un lenguaje dinámico.

La sintaxis de F# es diferente de los lenguajes que heredan de C.

- Las llaves no se usan para delimitar bloques de código. En cambio, se usa sangría (como en Python).
- Los espacios se usan para separar parámetros en lugar de comas.

Si quiere probar el siguiente código, puede ir a [tryfsharp.org](http://www.tryfsharp.org/Create) y pegarlo en [REPL](https://es.wikipedia.org/wiki/REPL).

```fsharp
// Los comentarios de una línea se escibren con una doble diagonal
(* Los comentarios multilínea usan parentesis (* . . . *)

-final del comentario multilínea- *)

// ================================================
// Syntaxis básica
// ================================================

// ------ "Variables" (pero no realmente) ------
// La palabra reservada "let" define un valor (inmutable)
let miEntero = 5
let miFlotante = 3.14
let miCadena = "hola"           // Tenga en cuenta que no es necesario ningún tipado

// ------ Listas ------
let dosACinco = [2;3;4;5]        // Los corchetes crean una lista con
                                 // punto y coma para delimitadores.
let unoACinco = 1 :: dosACinco   // :: Crea una lista con un nuevo elemento
// El resultado es [1;2;3;4;5]
let ceroACinco = [0;1] @ dosACinco   // @ Concatena dos listas

// IMPORTANTE: las comas no se usan para delimitar,
// solo punto y coma !

// ------ Funciones ------
// La palabra reservada "let" también define el nombre de una función.
let cuadrado x = x * x          // Tenga en cuenta que no se usa paréntesis.
cuadrado 3                      // Ahora, ejecutemos la función.
                              // De nuevo, sin paréntesis.

let agregar x y = x + y           // ¡No use add (x, y)! Eso significa
                              // algo completamente diferente.
agregar 2 3                       // Ahora, ejecutemos la función.

// Para definir una función en varias líneas, usemos la sangría.
// Los puntos y coma no son necesarios.
let pares lista =
   let esPar x = x%2 = 0     // Establece "esPar" como una función anidada
   List.filter esPar lista    // List.filter es una función de la biblioteca
                              // dos parámetros: una función que devuelve un
                              // booleano y una lista en la que trabajar

pares unoACinco               // Ahora, ejecutemos la función.

// Puedes usar paréntesis para aclarar.
// En este ejemplo, "map" se ejecuta primero, con dos argumentos,
// entonces "sum" se ejecuta en el resultado.
// Sin los paréntesis, "List.map" se pasará como argumento a List.sum.
let sumaDeCuadradosHasta100 =
   List.sum ( List.map cuadrado [1..100] )

// Puedes redirigir la salida de una función a otra con "|>"
// Redirigir datos es muy común en F#, como con los pipes de UNIX.

// Aquí está la misma función sumOfSquares escrita usando pipes
let sumaDeCuadradosHasta100piped =
   [1..100] |> List.map cuadrado |> List.sum  // "cuadrado" se declara antes

// Puede definir lambdas (funciones anónimas) gracias a la palabra clave "fun"
let sumaDeCuadradosHasta100ConFuncion =
   [1..100] |> List.map (fun x -> x*x) |> List.sum

// En F#, no hay palabra clave "return". Una función siempre regresa
// el valor de la última expresión utilizada.

// ------ Coincidencia de patrones ------
// Match..with .. es una sobrecarga de la condición de case/ switch.
let coincidenciaDePatronSimple =
   let x = "a"
   match x with
    | "a" -> printfn "x es a"
    | "b" -> printfn "x es b"
    | _ -> printfn "x es algo mas"   // guion bajo corresponde con todos los demás

// F# no permite valores nulos por defecto - debe usar el tipado de Option
// y luego coincide con el patrón.
// Some(..) y None son aproximadamente análogos a los envoltorios Nullable
let valorValido = Some(99)
let valorInvalido = None

// En este ejemplo, match..with encuentra una coincidencia con "Some" y "None",
// y muestra el valor de "Some" al mismo tiempo.
let coincidenciaDePatronDeOpciones entrada =
   match entrada with
    | Some i -> printfn "la entrada es un int=%d" i
    | None -> printfn "entrada faltante"

coincidenciaDePatronDeOpciones validValue
coincidenciaDePatronDeOpciones invalidValue

// ------ Viendo ------
// Las funciones printf/printfn son similares a las funciones
// Console.Write/WriteLine de C#.
printfn "Imprimiendo un int %i, a float %f, a bool %b" 1 2.0 true
printfn "Un string %s, y algo generico %A" "hola" [1;2;3;4]

// También hay funciones printf/sprintfn para formatear datos
// en cadena. Es similar al String.Format de C#.

// ================================================
// Mas sobre funciones
// ================================================

// F# es un verdadero lenguaje funcional - las funciones son
// entidades de primer nivel y se pueden combinar fácilmente
// para crear construcciones poderosas

// Los módulos se utilizan para agrupar funciones juntas.
// Se requiere sangría para cada módulo anidado.
module EjemploDeFuncion =

    // define una función de suma simple
    let agregar x y = x + y

    // uso básico de una función
    let a = agregar 1 2
    printfn "1+2 = %i" a

    // aplicación parcial para "hornear en" los parámetros (?)
    let agregar42 = agregar 42
    let b = agregar42 1
    printfn "42+1 = %i" b

    // composición para combinar funciones
    let agregar1 = agregar 1
    let agregar2 = agregar 2
    let agregar3 = agregar1 >> agregar2
    let c = agregar3 7
    printfn "3+7 = %i" c

    // funciones de primer nivel
    [1..10] |> List.map agregar3 |> printfn "la nueva lista es %A"

    // listas de funciones y más
    let agregar6 = [agregar1; agregar2; agregar3] |> List.reduce (>>)
    let d = agregar6 7
    printfn "1+2+3+7 = %i" d

// ================================================
// Lista de colecciones
// ================================================

// Il y a trois types de collection ordonnée :
// * Les listes sont les collections immutables les plus basiques
// * Les tableaux sont mutables et plus efficients
// * Les séquences sont lazy et infinies (e.g. un enumerator)
//
// Des autres collections incluent des maps immutables et des sets
// plus toutes les collections de .NET

module EjemplosDeLista =

    // las listas utilizan corchetes
    let lista1 = ["a";"b"]
    let lista2 = "c" :: lista1    // :: para una adición al principio
    let lista3 = lista1 @ lista2   // @ para la concatenación

    // Lista de comprensión (alias generadores)
    let cuadrados = [for i in 1..10 do yield i*i]

    //  Generador de números primos
    let rec tamiz = function
        | (p::xs) -> p :: tamiz [ for x in xs do if x % p > 0 then yield x ]
        | []      -> []
    let primos = tamiz [2..50]
    printfn "%A" primos

    // coincidencia de patrones para listas
    let listaDeCoincidencias unaLista =
        match unaLista with
        | [] -> printfn "la lista esta vacia"
        | [primero] -> printfn "la lista tiene un elemento %A " primero
        | [primero; segundo] -> printfn "la lista es %A y %A" primero segundo
        | _ -> printfn "la lista tiene mas de dos elementos"

    listaDeCoincidencias [1;2;3;4]
    listaDeCoincidencias [1;2]
    listaDeCoincidencias [1]
    listaDeCoincidencias []

    // Récursion en utilisant les listes
    let rec suma unaLista =
        match unaLista with
        | [] -> 0
        | x::xs -> x + suma xs
    suma [1..10]

    // -----------------------------------------
    // Funciones de la biblioteca estándar
    // -----------------------------------------

    // mapeo
    let agregar3 x = x + 3
    [1..10] |> List.map agregar3

    // filtrado
    let par x = x % 2 = 0
    [1..10] |> List.filter par

    // mucho más - consulte la documentación

module EjemploDeArreglo =

    // los arreglos usan corchetes con barras.
    let arreglo1 = [| "a";"b" |]
    let primero = arreglo1.[0]        // se accede al índice usando un punto

    // la coincidencia de patrones de los arreglos es la misma que la de las listas
    let coincidenciaDeArreglos una Lista =
        match unaLista with
        | [| |] -> printfn "la matriz esta vacia"
        | [| primero |] -> printfn "el arreglo tiene un elemento %A " primero
        | [| primero; second |] -> printfn "el arreglo es %A y %A" primero segundo
        | _ -> printfn "el arreglo tiene mas de dos elementos"

    coincidenciaDeArreglos [| 1;2;3;4 |]

    // La biblioteca estándar funciona como listas
    [| 1..10 |]
    |> Array.map (fun i -> i+3)
    |> Array.filter (fun i -> i%2 = 0)
    |> Array.iter (printfn "el valor es %i. ")

module EjemploDeSecuencia =

    // Las secuencias usan llaves
    let secuencia1 = seq { yield "a"; yield "b" }

    // Las secuencias pueden usar yield y
    // puede contener subsecuencias
    let extranio = seq {
        // "yield" agrega un elemento
        yield 1; yield 2;

        // "yield!" agrega una subsecuencia completa
        yield! [5..10]
        yield! seq {
            for i in 1..10 do
              if i%2 = 0 then yield i }}
    // prueba
    extranio |> Seq.toList

    // Las secuencias se pueden crear usando "unfold"
    // Esta es la secuencia de fibonacci
    let fib = Seq.unfold (fun (fst,snd) ->
        Some(fst + snd, (snd, fst + snd))) (0,1)

    // prueba
    let fib10 = fib |> Seq.take 10 |> Seq.toList
    printf "Los primeros 10 fib son %A" fib10

// ================================================
// Tipos de datos
// ================================================

module EejemploDeTipoDeDatos =

    // Todos los datos son inmutables por defecto

     // las tuplas son tipos anónimos simples y rápidos
     // - Usamos una coma para crear una tupla
    let dosTuplas = 1,2
    let tresTuplas = "a",2,true

    // Combinación de patrones para desempaquetar
    let x,y = dosTuplas  // asignado x=1 y=2

    // ------------------------------------
    // Los tipos de registro tienen campos con nombre
    // ------------------------------------

    // Usamos "type" con llaves para definir un tipo de registro
    type Persona = {Nombre:string; Apellido:string}

    // Usamos "let" con llaves para crear un registro
    let persona1 = {Nombre="John"; Apellido="Doe"}

    // Combinación de patrones para desempaquetar
    let {Nombre=nombre} = persona1    // asignado nombre="john"

    // ------------------------------------
    // Los tipos de unión (o variantes) tienen un conjunto de elección
     // Solo un caso puede ser válido a la vez.
    // ------------------------------------

    // Usamos "type" con barra/pipe para definir una unión estándar
    type Temp =
        | GradosC of float
        | GradosF of float

    // Una de estas opciones se usa para crear una
    let temp1 = GradosF 98.6
    let temp2 = GradosC 37.0

    // Coincidencia de patrón en todos los casos para desempaquetar (?)
    let imprimirTemp = function
       | GradosC t -> printfn "%f gradC" t
       | GradosF t -> printfn "%f gradF" t

    imprimirTemp temp1
    imprimirTemp temp2

    // ------------------------------------
    // Tipos recursivos
    // ------------------------------------

    // Los tipos se pueden combinar recursivamente de formas complejas
    // sin tener que crear subclases
    type Empleado =
      | Trabajador of Persona
      | Gerente of Empleado lista

    let jdoe = {Nombre="John";Apellido="Doe"}
    let trabajador = Trabajador jdoe

    // ------------------------------------
    // Modelado con tipados (?)
    // ------------------------------------

    // Los tipos de unión son excelentes para modelar el estado sin usar banderas (?)
    type DireccionDeCorreo =
        | DireccionDeCorreoValido of string
        | DireccionDeCorreoInvalido of string

    let intentarEnviarCorreo correoElectronico =
        match correoElectronico with // uso de patrones de coincidencia
        | DireccionDeCorreoValido direccion -> ()   // enviar
        | DireccionDeCorreoInvalido direccion -> () // no enviar

    // Combinar juntos, los tipos de unión y tipos de registro
     // ofrece una base excelente para el diseño impulsado por el dominio.
     // Puedes crear cientos de pequeños tipos que reflejarán fielmente
     // el dominio.

    type ArticuloDelCarrito = { CodigoDelProducto: string; Cantidad: int }
    type Pago = Pago of float
    type DatosActivosDelCarrito = { ArticulosSinPagar: ArticuloDelCarrito lista }
    type DatosPagadosDelCarrito = { ArticulosPagados: ArticuloDelCarrito lista; Pago: Pago}

    type CarritoDeCompras =
        | CarritoVacio  // sin datos
        | CarritoActivo of DatosActivosDelCarrito
        | CarritoPagado of DatosPagadosDelCarrito

    // ------------------------------------
    // Comportamiento nativo de los tipos
    // ------------------------------------

    // Los tipos nativos tienen el comportamiento más útil "listo para usar", sin ningún código para agregar.
     // * Inmutabilidad
     // * Bonita depuración de impresión
     // * Igualdad y comparación
     // * Serialización

     // La impresión bonita se usa con %A
    printfn "dosTuplas=%A,\nPersona=%A,\nTemp=%A,\nEmpleado=%A"
             dosTuplas persona1 temp1 trabajador

    // La igualdad y la comparación son innatas
     // Aquí hay un ejemplo con tarjetas.
    type JuegoDeCartas = Trebol | Diamante | Espada | Corazon
    type Rango = Dos | Tres | Cuatro | Cinco | Seis | Siete | Ocho
                | Nueve | Diez | Jack | Reina | Rey | As

    let mano = [ Trebol,As; Corazon,Tres; Corazon,As;
                 Espada,Jack; Diamante,Dos; Diamante,As ]

    // orden
    List.sort mano |> printfn "la mano ordenada es (de menos a mayor) %A"
    List.max mano |> printfn "la carta más alta es%A"
    List.min mano |> printfn "la carta más baja es %A"

// ================================================
// Patrones activos
// ================================================

module EjemplosDePatronesActivos =

    // F# tiene un tipo particular de coincidencia de patrón llamado "patrones activos"
    // donde el patrón puede ser analizado o detectado dinámicamente.

    // "clips de banana" es la sintaxis de los patrones activos

    // por ejemplo, definimos un patrón "activo" para que coincida con los tipos de "caracteres" ...
    let (|Digito|Latra|EspacioEnBlanco|Otros|) ch =
       if System.Char.IsDigit(ch) then Digito
       else if System.Char.IsLetter(ch) then Letra
       else if System.Char.IsWhiteSpace(ch) then EspacioEnBlanco
       else Otros

    // ... y luego lo usamos para hacer que la lógica de análisis sea más clara
    let ImprimirCaracter ch =
      match ch with
      | Digito -> printfn "%c es un Digito" ch
      | Letra -> printfn "%c es una Letra" ch
      | Whitespace -> printfn "%c es un Espacio en blanco" ch
      | _ -> printfn "%c es algo mas" ch

    // ver una lista
    ['a';'b';'1';' ';'-';'c'] |> List.iter ImprimirCaracter

    // -----------------------------------------
    // FizzBuzz usando patrones activos
    // -----------------------------------------

    // Puede crear un patrón de coincidencia parcial también
    // Solo usamos un guión bajo en la definición y devolvemos Some si coincide.
    let (|MultDe3|_|) i = if i % 3 = 0 then Some MultDe3 else None
    let (|MultDe5|_|) i = if i % 5 = 0 then Some MultDe5 else None

    // la función principal
    let fizzBuzz i =
      match i with
      | MultDe3 & MultDe5 -> printf "FizzBuzz, "
      | MultDe3 -> printf "Fizz, "
      | MultDe5 -> printf "Buzz, "
      | _ -> printf "%i, " i

    // prueba
    [1..20] |> List.iter fizzBuzz

// ================================================
// concisión
// ================================================

module EjemploDeAlgoritmo =

    // F# tiene una alta relación señal / ruido, lo que permite leer el código
    // casi como un algoritmo real

    // ------ Ejemplo: definir una función sumaDeCuadrados ------
    let sumaDeCuadrados n =
       [1..n]                   // 1) Tome todos los números del 1 al n
       |> List.map cuadrado     // 2) Elevar cada uno de ellos al cuadrado
       |> List.sum              // 3) Realiza su suma

    // prueba
    sumaDeCuadrados 100 |> printfn "Suma de cuadrados = %A"

    // ------ Ejemplo: definir una función de ordenación ------
    let rec ordenar lista =
       match lista with
       // Si la lista está vacía
       | [] ->
            []                              // devolvemos una lista vacía
       // si la lista no está vacía
       | primerElemento::otrosElementos ->  // tomamos el primer elemento
            let elementosMasPequenios =     // extraemos los elementos más pequeños
                otrosElementos              // tomamos el resto
                |> List.filter (fun e -> e < primerElemento)
                |> ordenar                  // y los ordenamos
            let elementosMasGrandes =       // extraemos el mas grande
                otrosElementos              // de los que permanecen
                |> List.filter (fun e -> e >= primerElemento)
                |> ordenar                  // y los ordenamos
            // Combinamos las 3 piezas en una nueva lista que devolvemos
            List.concat [elementosMasPequenios; [primerElemento]; elementosMasGrandes]

    // prueba
    ordenar [1;5;23;18;9;1;3] |> printfn "Ordenado = %A"

// ================================================
// Código asíncrono
// ================================================

module AsyncExample =

    // F# incluye características para ayudar con el código asíncrono
    // sin conocer la "pirámide del destino"
    //
    // El siguiente ejemplo descarga una secuencia de página web en paralelo.

    open System.Net
    open System
    open System.IO
    open Microsoft.FSharp.Control.CommonExtensions

    // Recuperar el contenido de una URL de forma asincrónica
    let extraerUrlAsync url =
        async {   // La palabra clave "async" y llaves
                  // crear un objeto "asincrónico"
            let solicitud = WebRequest.Create(Uri(url))
            use! respuesta = solicitud.AsyncGetResponse()
                // use! es una tarea asincrónica
            use flujoDeDatos = resp.GetResponseStream()
                // "use" dispara automáticamente la funcion close()
                // en los recursos al final de las llaves
            use lector = new IO.StreamReader(flujoDeDatos)
            let html = lector.ReadToEnd()
            printfn "terminó la descarga %s" url
            }

    // una lista de sitios para informar
    let sitios = ["http://www.bing.com";
                 "http://www.google.com";
                 "http://www.microsoft.com";
                 "http://www.amazon.com";
                 "http://www.yahoo.com"]

    // ¡Aqui vamos!
    sitios
    |> List.map extraerUrlAsync // crear una lista de tareas asíncrona
    |> Async.Parallel           // decirle a las tareas que se desarrollan en paralelo
    |> Async.RunSynchronously   // ¡Empieza!

// ================================================
// Compatibilidad .NET
// ================================================

module EjemploCompatibilidadNet =

    // F# puede hacer casi cualquier cosa que C# pueda hacer, y se ajusta
    // perfectamente con bibliotecas .NET o Mono.

    // ------- Trabaja con las funciones de las bibliotecas existentes  -------

    let (i1success,i1) = System.Int32.TryParse("123");
    if i1success then printfn "convertido como %i" i1 else printfn "conversion fallida"

    // ------- Implementar interfaces sobre la marcha! -------

    // Crea un nuevo objeto que implemente IDisposable
    let crearRecurso name =
       { new System.IDisposable
         with member this.Dispose() = printfn "%s creado" name }

    let utilizarYDisponerDeRecursos =
        use r1 = crearRecurso "primer recurso"
        printfn "usando primer recurso"
        for i in [1..3] do
            let nombreDelRecurso = sprintf "\tinner resource %d" i
            use temp = crearRecurso nombreDelRecurso
            printfn "\thacer algo con %s" nombreDelRecurso
        use r2 = crearRecurso "segundo recurso"
        printfn "usando segundo recurso"
        printfn "hecho."

    // ------- Código orientado a objetos -------

    // F# es también un verdadero lenguaje OO.
    // Admite clases, herencia, métodos virtuales, etc.

    // interfaz de tipo genérico
    type IEnumerator<'a> =
        abstract member Actual : 'a
        abstract MoverSiguiente : unit -> bool

    // Clase base abstracta con métodos virtuales
    [<AbstractClass>]
    type Figura() =
        // propiedades de solo lectura
        abstract member Ancho : int with get
        abstract member Alto : int with get
        // método no virtual
        member this.AreaDelimitadora = this.Alto * this.Ancho
        // método virtual con implementación de la clase base
        abstract member Imprimir : unit -> unit
        default this.Imprimir () = printfn "Soy una Figura"

    // clase concreta que hereda de su clase base y sobrecarga
    type Rectangulo(x:int, y:int) =
        inherit Figura()
        override this.Ancho = x
        override this.Alto = y
        override this.Imprimir ()  = printfn "Soy un Rectangulo"

    // prueba
    let r = Rectangulo(2,3)
    printfn "La anchura es %i" r.Ancho
    printfn "El area es %i" r.AreaDelimitadora
    r.Imprimir()

    // ------- extensión de método  -------

    // Al igual que en C#, F# puede extender las clases existentes con extensiones de método.
    type System.String with
       member this.EmpiezaConA = this.EmpiezaCon "A"

    // prueba
    let s = "Alice"
    printfn "'%s' empieza con una 'A' = %A" s s.EmpiezaConA

    // ------- eventos -------

    type MiBoton() =
        let eventoClick = new Event<_>()

        [<CLIEvent>]
        member this.AlHacerClick = eventoClick.Publish

        member this.PruebaEvento(arg) =
            eventoClick.Trigger(this, arg)

    // prueba
    let miBoton = new MiBoton()
    miBoton.AlHacerClick.Add(fun (sender, arg) ->
            printfn "Haga clic en el evento con arg=%O" arg)

    miBoton.PruebaEvento("Hola Mundo!")
```

## Más información

Para más demostraciones de F#, visite el sitio [Try F#](http://www.tryfsharp.org/Learn), o sigue la serie [why use F#](http://fsharpforfunandprofit.com/why-use-fsharp/).

Aprenda más sobre F# en [fsharp.org](http://fsharp.org/).
