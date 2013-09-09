---
name: Go
category: language
language: Go
filename: learngo.go
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]

translators:
	- ["Adrian Espinosa", "http://www.adrianespinosa.com"]

---

Go fue creado por la necesidad de hacer el trabajo rápidamente.  No es la última
tendencia en informática, pero es la forma nueva y más rápida de resolver probemas reales.

Tiene conceptos familiares de lenguajes imperativos con tipado estático.
Es rápido compilando y rápido al ejecutar, añade una concurrencia fácil de entender para las CPUs de varios núcleos de hoy en día, y tiene características que ayudan con la programación a gran escala.
Go viene con una librería estándar muy buena y una comunidad entusiasta.

```go
// Comentario de una sola línea
/* Comentario 
   multi línea */

// La cláusula package aparece al comienzo de cada archivo fuente.
// Main es un nombre especial que declara un ejecutable en vez de una librería.
package main

// La declaración Import declara los paquetes de librerías referenciados en este archivo.
import (
    "fmt"      // Un paquete en la librería estándar de Go
    "net/http" // Sí, un servidor web!
    "strconv"  // Conversiones de cadenas
)

// Definición de una función. Main es especial. Es el punto de entrada para el ejecutable.
// Te guste o no, Go utiliza llaves.
func main() {
    // Println imprime una línea a stdout.
    // Cualificalo con el nombre del paquete, fmt.
    fmt.Println("Hello world!")

    // Llama a otra función de este paquete.
    beyondHello()
}

// Las funciones llevan parámetros entre paréntesis.
// Si no hay parámetros, los paréntesis siguen siendo obligatorios.
func beyondHello() {
    var x int // Declaración de una variable. Las variables se deben declarar antes de 
    // utilizarlas.
    x = 3     // Asignación de variables.
    // Declaración "corta" con := para inferir el tipo, declarar y asignar.
    y := 4
    sum, prod := learnMultiple(x, y)        // función devuelve dos valores
    fmt.Println("sum:", sum, "prod:", prod) // simple salida
    learnTypes()                            // < y minutes, learn more!
}

// Las funciones pueden tener parámetros y (múltiples!) valores de retorno.
func learnMultiple(x, y int) (sum, prod int) {
    return x + y, x * y // devolver dos valores
}

// Algunos tipos incorporados y literales.
func learnTypes() {
    // La declaración corta suele darte lo que quieres.
    s := "Learn Go!" // tipo cadena

    s2 := ` Un tipo cadena "puro" puede incluir
saltos de línea.` // mismo tipo cadena

    // Literal no ASCII. Los fuentes de Go son UTF-8.
    g := 'Σ' // tipo rune, un alias de uint32, alberga un punto unicode.
    f := 3.14195 // float64, el estándar IEEE-754 de coma flotante 64-bit
    c := 3 + 4i  // complex128, representado internamente por dos float64
    // Sintaxis Var con inicializadores.
    var u uint = 7 // sin signo, pero la implementación depende del tamaño como en int
    var pi float32 = 22. / 7

    // Sintáxis de conversión con una declaración corta.
    n := byte('\n') // byte es un alias de uint8

    // Los Arrays tienen un tamaño fijo a la hora de compilar.
    var a4 [4]int           // un array de 4 ints, inicializados a 0
    a3 := [...]int{3, 1, 5} // un array de 3 ints, inicializados como se indica

    // Los Slices tienen tamaño dinámico. Los arrays y slices tienen sus ventajas
    // y desventajas pero los casos de uso para los slices son más comunes.
    s3 := []int{4, 5, 9}    // Comparar con a3. No hay puntos suspensivos
    s4 := make([]int, 4)    // Asigna slices de 4 ints, inicializados a 0
    var d2 [][]float64      // solo declaración, sin asignación
    bs := []byte("a slice") // sintaxis de conversión de tipo

    p, q := learnMemory() // declara p, q para ser un tipo puntero a int.
    fmt.Println(*p, *q)   // * sigue un puntero. Esto imprime dos ints.

    // Los Maps son arrays asociativos dinámicos, como los hash o diccionarios
    // de otros lenguajes
    m := map[string]int{"three": 3, "four": 4}
    m["one"] = 1

    // Las variables no utilizadas en Go producen error.
    // El guión bajo permite "utilizar" una variable, pero descartar su valor.
    _, _, _, _, _, _, _, _, _ = s2, g, f, u, pi, n, a3, s4, bs
    // Esto cuenta como utilización de variables.
    fmt.Println(s, c, a4, s3, d2, m)

    learnFlowControl() // vuelta al flujo
}

// Go posee recolector de basura. Tiene puntero pero no aritmética de punteros.
// Puedes cometer un errores con un puntero nil, pero no incrementando un puntero.
func learnMemory() (p, q *int) {
    // q y p tienen un tipo puntero a int.
    p = new(int) // función incorporada que asigna memoria.
    // La asignación de int se inicializa a 0, p ya no es nil.
    s := make([]int, 20) // asigna 20 ints a un solo bloque de memoria.
    s[3] = 7             // asignar uno de ellos
    r := -2              // declarar otra variable local
    return &s[3], &r     // & toma la dirección de un objeto.
}

func expensiveComputation() int {
    return 1e6
}

func learnFlowControl() {
    // La declaración If requiere llaves, pero no paréntesis.
    if true {
        fmt.Println("told ya")
    }
    // El formato está estandarizado por el comando "go fmt."
    if false {
        // pout
    } else {
        // gloat
    }
    // Utiliza switch preferiblemente para if encadenados.
    x := 1
    switch x {
    case 0:
    case 1:
        // los cases no se mezclan, no requieren de "break"
    case 2:
        // no llega
    }
    // Como if, for no utiliza paréntesis tampoco.
    for x := 0; x < 3; x++ { // ++ es una sentencia
        fmt.Println("iteration", x)
    }
    // x == 1 aqui.

    // For es la única sentencia de bucle en Go, pero tiene formas alternativas.
    for { // bucle infinito
        break    // solo bromeaba!
        continue // no llega
    }
    // Como en for, := en una sentencia if significa declarar y asignar primero,
    // luego comprobar y > x.
    if y := expensiveComputation(); y > x {
        x = y
    }
    // Los literales de funciones son "closures".
    xBig := func() bool {
        return x > 100 // referencia a x declarada encima de la sentencia switch.
    }
    fmt.Println("xBig:", xBig()) // verdadero (la última vez asignamos 1e6 a x)
    x /= 1e5                     // esto lo hace  == 10
    fmt.Println("xBig:", xBig()) // ahora es falso

    // Cuando lo necesites, te encantará.
    goto love
love:

    learnInterfaces() // Buen material dentro de poco!
}

// Define Stringer como un tipo interfaz con un método, String.
type Stringer interface {
    String() string
}

// Define pair como un struct con dos campos int, x e y.
type pair struct {
    x, y int
}

// Define un método del tipo pair. Pair ahora implementa Stringer.
func (p pair) String() string { // p se llama "recibidor"
    // Sprintf es otra función pública del paquete fmt.
    // Dot syntax references fields of p.
    return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
    // Brace syntax is a "struct literal."  It evaluates to an initialized
    // struct.  The := syntax declares and initializes p to this struct.
    p := pair{3, 4}
    fmt.Println(p.String()) // call String method of p, of type pair.
    var i Stringer          // declare i of interface type Stringer.
    i = p                   // valid because pair implements Stringer
    // Call String method of i, of type Stringer.  Output same as above.
    fmt.Println(i.String())

    // Functions in the fmt package call the String method to ask an object
    // for a printable representation of itself.
    fmt.Println(p) // output same as above. Println calls String method.
    fmt.Println(i) // output same as above

    learnErrorHandling()
}

func learnErrorHandling() {
    // ", ok" idiom used to tell if something worked or not.
    m := map[int]string{3: "three", 4: "four"}
    if x, ok := m[1]; !ok { // ok will be false because 1 is not in the map.
        fmt.Println("no one there")
    } else {
        fmt.Print(x) // x would be the value, if it were in the map.
    }
    // An error value communicates not just "ok" but more about the problem.
    if _, err := strconv.Atoi("non-int"); err != nil { // _ discards value
        // prints "strconv.ParseInt: parsing "non-int": invalid syntax"
        fmt.Println(err)
    }
    // We'll revisit interfaces a little later.  Meanwhile,
    learnConcurrency()
}

// c is a channel, a concurrency-safe communication object.
func inc(i int, c chan int) {
    c <- i + 1 // <- is the "send" operator when a channel appears on the left.
}

// We'll use inc to increment some numbers concurrently.
func learnConcurrency() {
    // Same make function used earlier to make a slice.  Make allocates and
    // initializes slices, maps, and channels.
    c := make(chan int)
    // Start three concurrent goroutines.  Numbers will be incremented
    // concurrently, perhaps in parallel if the machine is capable and
    // properly configured.  All three send to the same channel.
    go inc(0, c) // go is a statement that starts a new goroutine.
    go inc(10, c)
    go inc(-805, c)
    // Read three results from the channel and print them out.
    // There is no telling in what order the results will arrive!
    fmt.Println(<-c, <-c, <-c) // channel on right, <- is "receive" operator.

    cs := make(chan string)       // another channel, this one handles strings.
    cc := make(chan chan string)  // a channel of string channels.
    go func() { c <- 84 }()       // start a new goroutine just to send a value
    go func() { cs <- "wordy" }() // again, for cs this time
    // Select has syntax like a switch statement but each case involves
    // a channel operation.  It selects a case at random out of the cases
    // that are ready to communicate.
    select {
    case i := <-c: // the value received can be assigned to a variable
        fmt.Printf("it's a %T", i)
    case <-cs: // or the value received can be discarded
        fmt.Println("it's a string")
    case <-cc: // empty channel, not ready for communication.
        fmt.Println("didn't happen.")
    }
    // At this point a value was taken from either c or cs.  One of the two
    // goroutines started above has completed, the other will remain blocked.

    learnWebProgramming() // Go does it.  You want to do it too.
}

// A single function from package http starts a web server.
func learnWebProgramming() {
    // ListenAndServe first parameter is TCP address to listen at.
    // Second parameter is an interface, specifically http.Handler.
    err := http.ListenAndServe(":8080", pair{})
    fmt.Println(err) // don't ignore errors
}

// Make pair an http.Handler by implementing its only method, ServeHTTP.
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
    // Serve data with a method of http.ResponseWriter
    w.Write([]byte("You learned Go in Y minutes!"))
}
```

## Further Reading

The root of all things Go is the [official Go web site](http://golang.org/).
There you can follow the tutorial, play interactively, and read lots.

The language definition itself is highly recommended.  It's easy to read
and amazingly short (as language definitions go these days.)

On the reading list for students of Go is the source code to the standard
library.  Comprehensively documented, it demonstrates the best of readable
and understandable Go, Go style, and Go idioms.  Click on a function name
in the documentation and the source code comes up!

