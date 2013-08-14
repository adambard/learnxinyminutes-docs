---
name: Go
category: language
language: Go
filename: learngo.go
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]
---

Go was created out of the need to get work done.  It's not the latest trend
in computer science, but it is the newest fastest way to solve real-world
problems.

It has familiar concepts of imperative languages with static typing.
It's fast to compile and fast to execute, it adds easy-to-understand
concurrency to leverage today's multi-core CPUs, and has features to
help with large-scale programming.

Go comes with a great standard library and an enthusiastic community.

```go
// Single line comment
/* Multi-
   line comment */

// A package clause starts every source file.
// Main is a special name declaring an executable rather than a library.
package main

// Import declaration declares library packages referenced in this file.
import (
    "fmt"      // A package in the Go standard library
    "net/http" // Yes, a web server!
    "strconv"  // String conversions
)

// A function definition.  Main is special.  It is the entry point for the
// executable program.  Love it or hate it, Go uses brace brackets.
func main() {
    // Println outputs a line to stdout.
    // Qualify it with the package name, fmt.
    fmt.Println("Hello world!")

    // Call another function within this package.
    beyondHello()
}

// Functions have parameters in parentheses.
// If there are no parameters, empty parens are still required.
func beyondHello() {
    var x int // Variable declaration.  Variables must be declared before use.
    x = 3     // Variable assignment.
    // "Short" declarations use := to infer the type, declare, and assign.
    y := 4
    sum, prod := learnMultiple(x, y)        // function returns two values
    fmt.Println("sum:", sum, "prod:", prod) // simple output
    learnTypes()                            // < y minutes, learn more!
}

// Functions can have parameters and (multiple!) return values.
func learnMultiple(x, y int) (sum, prod int) {
    return x + y, x * y // return two values
}

// Some built-in types and literals.
func learnTypes() {
    // Short declaration usually gives you what you want.
    s := "Learn Go!" // string type

    s2 := `A "raw" string literal
can include line breaks.` // same string type

    // non-ASCII literal.  Go source is UTF-8.
    g := 'Σ' // rune type, an alias for uint32, holds a UTF-8 code point

    f := 3.14195 // float64, an IEEE-754 64-bit floating point number
    c := 3 + 4i  // complex128, represented internally with two float64s

    // Var syntax with an initializers.
    var u uint = 7 // unsigned, but implementation dependent size as with int
    var pi float32 = 22. / 7

    // Conversion syntax with a short declaration.
    n := byte('\n') // byte is an alias for uint8

    // Arrays have size fixed at compile time.
    var a4 [4]int           // an array of 4 ints, initialized to all 0
    a3 := [...]int{3, 1, 5} // an array of 3 ints, initialized as shown

    // Slices have dynamic size.  Arrays and slices each have advantages
    // but use cases for slices are much more common.
    s3 := []int{4, 5, 9}    // compare to a3.  no ellipsis here
    s4 := make([]int, 4)    // allocates slice of 4 ints, initialized to all 0
    var d2 [][]float64      // declaration only, nothing allocated here
    bs := []byte("a slice") // type conversion syntax

    p, q := learnMemory() // declares p, q to be type pointer to int.
    fmt.Println(*p, *q)   // * follows a pointer.  This prints two ints.

    // Maps are a dynamically growable associative array type, like the
    // hash or dictionary types of some other languages.
    m := map[string]int{"three": 3, "four": 4}
    m["one"] = 1

    // Unused variables are an error in Go.
    // The underbar lets you "use" a variable but discard its value.
    _, _, _, _, _, _, _, _, _ = s2, g, f, u, pi, n, a3, s4, bs
    // Output of course counts as using a variable.
    fmt.Println(s, c, a4, s3, d2, m)

    learnFlowControl() // back in the flow
}

// Go is fully garbage collected.  It has pointers but no pointer arithmetic.
// You can make a mistake with a nil pointer, but not by incrementing a pointer.
func learnMemory() (p, q *int) {
    // Named return values p and q have type pointer to int.
    p = new(int) // built-in function new allocates memory.
    // The allocated int is initialized to 0, p is no longer nil.
    s := make([]int, 20) // allocate 20 ints as a single block of memory
    s[3] = 7             // assign one of them
    r := -2              // declare another local variable
    return &s[3], &r     // & takes the address of an object.
}

func expensiveComputation() int {
    return 1e6
}

func learnFlowControl() {
    // If statements require brace brackets, and do not require parens.
    if true {
        fmt.Println("told ya")
    }
    // Formatting is standardized by the command line command "go fmt."
    if false {
        // pout
    } else {
        // gloat
    }
    // Use switch in preference to chained if statements.
    x := 1
    switch x {
    case 0:
    case 1:
        // cases don't "fall through"
    case 2:
        // unreached
    }
    // Like if, for doesn't use parens either.
    for x := 0; x < 3; x++ { // ++ is a statement
        fmt.Println("iteration", x)
    }
    // x == 1 here.

    // For is the only loop statement in Go, but it has alternate forms.
    for { // infinite loop
        break    // just kidding
        continue // unreached
    }
    // As with for, := in an if statement means to declare and assign y first,
    // then test y > x.
    if y := expensiveComputation(); y > x {
        x = y
    }
    // Function literals are closures.
    xBig := func() bool {
        return x > 100 // references x declared above switch statement.
    }
    fmt.Println("xBig:", xBig()) // true (we last assigned 1e6 to x)
    x /= 1e5                     // this makes it == 10
    fmt.Println("xBig:", xBig()) // false now

    // When you need it, you'll love it.
    goto love
love:

    learnInterfaces() // Good stuff coming up!
}

// Define Stringer as an interface type with one method, String.
type Stringer interface {
    String() string
}

// Define pair as a struct with two fields, ints named x and y.
type pair struct {
    x, y int
}

// Define a method on type pair.  Pair now implements Stringer.
func (p pair) String() string { // p is called the "receiver"
    // Sprintf is another public function in package fmt.
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
    cc := make(chan chan string)  // a channel of channels.
    go func() { c <- 84 }()       // start a new goroutine just to send a value
    go func() { cs <- "wordy" }() // again, for cs this time
    // Select has syntax like a switch statement but each case involves
    // a channel operation.  It selects a case at random out of the cases
    // that are ready to communicate.
    select {
    case i := <-c: // the value received can be assigned to a variable
        fmt.Println("it's a", i)
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

