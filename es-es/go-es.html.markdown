---
name: Go
category: language
language: Go
filename: learngo.go
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]
translators:
    - ["Adrian Espinosa", "http://www.adrianespinosa.com"]
lang: es-es


---

Go fue creado por la necesidad de hacer el trabajo rápidamente.  No es la última
tendencia en informática, pero es la forma nueva y más rápida de resolver problemas reales.

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
    // La sintaxis con punto referencia campos de p.
    return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
    // La sintaxis de llaves es un "literal struct". Evalúa a un struct
    // inicializado.  La sintaxis := declara e inicializa p a este struct.
    p := pair{3, 4}
    fmt.Println(p.String()) // llamar al método String de p, de tipo pair.
    var i Stringer          // declarar i como interfaz tipo Stringer.
    i = p                   // válido porque pair implementa Stringer
    // Llamar al metodo String de i, de tipo Stringer. Misma salida que arriba
    fmt.Println(i.String())

    // Las funciones en el paquete fmt llaman al método String para preguntar a un objeto
    // por una versión imprimible de si mismo
    fmt.Println(p) // salida igual que arriba. Println llama al método String.
    fmt.Println(i) // salida igual que arriba.

    learnErrorHandling()
}

func learnErrorHandling() {
    // ", ok" forma utilizada para saber si algo funcionó o no.
    m := map[int]string{3: "three", 4: "four"}
    if x, ok := m[1]; !ok { // ok será falso porque 1 no está en el map.
        fmt.Println("no one there")
    } else {
        fmt.Print(x) // x sería el valor, si estuviera en el map.
    }
    // Un valor de error comunica más información sobre el problema aparte de "ok".
    if _, err := strconv.Atoi("non-int"); err != nil { // _ descarta el valor
        // imprime "strconv.ParseInt: parsing "non-int": invalid syntax"
        fmt.Println(err)
    }
    // Revisarmeos las interfaces más tarde. Mientras tanto,
    learnConcurrency()
}

// c es un canal,  un objeto de comunicación de concurrencia segura.
func inc(i int, c chan int) {
    c <- i + 1 // <- es el operador "enviar" cuando un canal aparece a la izquierda.
}

// Utilizaremos inc para incrementar algunos números concurrentemente.
func learnConcurrency() {
    // Misma función make utilizada antes para crear un slice. Make asigna e
    // inicializa slices, maps, y channels.
    c := make(chan int)
    // Iniciar tres goroutines concurrentes. Los números serán incrementados
    // concurrentemente, quizás en paralelo si la máquina es capaz y
    // está correctamente configurada. Las tres envían al mismo channel.
    go inc(0, c) // go es una sentencia que inicia una nueva goroutine.
    go inc(10, c)
    go inc(-805, c)
    // Leer los tres resultados del channel e imprimirlos.
    // No se puede saber en que orden llegarán los resultados!
    fmt.Println(<-c, <-c, <-c) // channel a la derecha, <- es el operador "recibir".

    cs := make(chan string)       // otro channel, este gestiona cadenas.
    cc := make(chan chan string)  // un channel de cadenas de channels.
    go func() { c <- 84 }()       // iniciar una nueva goroutine solo para enviar un valor.
    go func() { cs <- "wordy" }() // otra vez, para cs en esta ocasión
    // Select tiene una sintáxis parecida a la sentencia switch pero cada caso involucra
    // una operacion de channels. Selecciona un caso de forma aleatoria de los casos
    // que están listos para comunicarse.
    select {
    case i := <-c: // el valor recibido puede ser asignado a una variable
        fmt.Printf("it's a %T", i)
    case <-cs: // o el valor puede ser descartado
        fmt.Println("it's a string")
    case <-cc: // channel vacío, no está listo para la comunicación.
        fmt.Println("didn't happen.")
    }
    // En este punto un valor fue devuelvto de c o cs. Uno de las dos
    // goroutines que se iniciaron se ha completado, la otrá permancerá bloqueada.

    learnWebProgramming() // Go lo hace. Tu también quieres hacerlo.
}

// Una simple función del paquete http inicia un servidor web.
func learnWebProgramming() {
    // El primer parámetro de la direccinón TCP a la que escuchar.
    // El segundo parámetro es una interfaz, concretamente http.Handler.
    err := http.ListenAndServe(":8080", pair{})
    fmt.Println(err) // no ignorar errores
}

// Haz pair un http.Handler implementando su único método, ServeHTTP.
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
    // Servir datos con un método de http.ResponseWriter
    w.Write([]byte("You learned Go in Y minutes!"))
}
```

## Para leer más 

La raíz de todas las cosas de Go es la [web oficial de Go](http://golang.org/).
Ahí puedes seguir el tutorial, jugar interactivamente y leer mucho.

La propia definición del lenguaje también está altamente recomendada. Es fácil de leer
e increíblemente corta (como otras definiciones de lenguajes hoy en día)

En la lista de lectura de estudiantes de Go está el código fuente de la
librería estándar. Muy bien documentada, demuestra lo mejor de Go leíble, comprendible,
estilo Go y formas Go. Pincha en el nombre de una función en la documentación
y te aparecerá el código fuente!

