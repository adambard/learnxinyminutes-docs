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

```Go
// Single line comment
/* Multi-
   line comment */

// A package clause starts every source file.
// Main is a special name declaring an executable rather than a library.
package main

// An import declaration comes next.  It declares library packages referenced
// in this file.  The list must be exactly correct!  Missing or unused packages
// are errors, not warnings.
import (
	"fmt"      // A package in the Go standard library
	"net/http" // Yes, a web server!
	"strconv"  // String conversions
)

// A function definition.  Main is special.  It is the entry point for the
// executable program.  Love it or hate it, Go uses brace brackets.
func main() {
	// Println is a function that outputs a line to stdout.  It can be
	// called here because fmt has been imported and the function name
	// "Println" is upper case.  Symbols starting with an upper case letter
	// are publicly visible.  No other special syntax is needed to export
	// something from a package.
	// To call Println, qualify it with the package name, fmt.
	fmt.Println("Hello world!")

	// Call another function within this package.
	beyondHello()
}

// Idiomatic Go uses camel case.  Functions have parameters in parentheses.
// If there are no parameters, empty parens are still required.
func beyondHello() {
	var x int // Variable declaration.  Variables must be declared before use.
	x = 3     // Variable assignment.
	// "Short" declarations use := syntax to declare and assign, infering the
	// type from the right hand side as much as possible and using some
	// defaults where the rhs could be interpreted different ways.
	// Idiomatic Go uses short declarations in preference to var keyword.
	y := 4
	sum, prod := learnMultiple(x, y)        // function returns two values
	fmt.Println("sum:", sum, "prod:", prod) // simple output
	learnTypes()                            // < y minutes, learn more!
}

// Functions can have parameters and (multiple!) return values.
// In declarations, the symbol precedes the type, and the type does not have
// to be repeated if it is the same for multiple symbols in a row.
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

	// You can use var syntax with an initializer if you want
	// something other than the default that a short declaration gives you.
	var u uint = 7 // unsigned, but implementation dependent size as with int
	var pi float32 = 22. / 7

	// Or more idiomatically, use conversion syntax with a short declaration.
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

	p, q := learnMemory() // A little side bar.
	// Did you read it?  This short declaration declares p and q to be of
	// type pointer to int.  P is now pointing into a block of of 20 ints, but
	// the only one accessible is the one that p is pointing at.  There is
	// no p++ to get at the next one.
	fmt.Println(*p, *q) // * follows a pointer.  This prints two ints.

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
	// Named return values p and q have type pointer to int.  They are
	// initialized to nil at this point.  Evaluating *p or *q here would cause
	// a panic--a run time error.
	p = new(int) // built-in function new allocates memory.
	// The allocated int is initialized to 0, p is no longer nil.
	s := make([]int, 20) // allocate 20 ints as a single block of memory
	s[3] = 7             // assign one of them
	r := -2              // declare another local variable
	return &s[3], &r     // Oh my.
	// The line above returns two values, yes, and both of the expressions
	// are valid.  & takes the address of an object.  Elements of a slice are
	// addressable, and so are local variables.  Built-in functions new and
	// make explicitly allocate memory, but local objects can be allocated
	// as needed.  Here memory for r will be still be referenced after the
	// function returns so it will be allocated as well.  The int allocated
	// with new on the other hand will no longer be referenced and can be
	// garbage collected as needed by the Go runtime.  The memory allocated
	// with make will still be referenced at that one element, and so it
	// cannot be garbage collected.  All 20 ints remain in memory because
	// one of them is still referenced.
}

func expensiveComputation() int {
	return 1e6
}

func learnFlowControl() {
	// If statements require brace brackets, and do not require parens.
	if true {
		fmt.Println("told ya")
	}
	// This is how we format the brace brackets.  Formatting is standardized
	// by the command line command "go fmt."  Everybody does it.  You will
	// suffer endless disparaging remarks until you conform as well.
	if false {
		// pout
	} else {
		// gloat
	}
	// If statements can be chained of course, but it's idiomatic to use
	// the handy switch statement instead.
	x := 1
	switch x {
	case 0:
	case 1:
		// cases don't "fall through"
	case 2:
		// unreached
	}
	// Like if, for doesn't use parens either.  The scope of a variable
	// declared in the first clause of the for statement is the statement
	// and block.  This x shadows the x declared above, but goes out of
	// scope after the for block.
	for x := 0; x < 3; x++ { // ++ is a statement
		fmt.Println("iteration", x)
	}
	// x == 1 here.

	// For is the only loop statement in Go, but it has alternate forms.
	for { // infinite loop
		break    // just kidding
		continue // unreached
	}
	// The initial assignment of the for statement is handy enough that Go
	// if statements can have one as well.  Just like in the for statement,
	// the := here means to declare and assign y first, then test y > x.
	// The scope of y is limited to the if statement and block.
	if y := expensiveComputation(); y > x {
		x = y
	}
	// Functions are first class objects and function literals are handy.
	// Function literals are closures.
	xBig := func() bool {
		return x > 100 // references x declared above switch statement.
	}
	fmt.Println("xBig:", xBig()) // true (we last assigned 1e6 to x)
	x /= 1e5                     // this makes it == 10
	fmt.Println("xBig:", xBig()) // false now

	// When you need it, you'll love it.  Actually Go's goto has been reformed
	// a bit to avoid indeterminate states.  You can't jump around variable
	// declarations and you can't jump into blocks.
	goto love
love:

	learnInterfaces() // Good stuff coming up!
}

// An interface is a list of functionality that a type supports.  Notably
// missing from an interface definition is any declaration of which types
// implement the interface.  Types simply implement an interface or they don't.
//
// An interface can have any number of methods, but it's actually common
// for an interface to have only single method.  It is idiomatic in this
// case for the single method to be named with some action, and for the
// interface name to end in "er."
//
// An interface definition is one kind of a type definition.  Interface is
// a built in type.  Stringer is defined here as an interface type with one
// method, String.
type Stringer interface {
	String() string
}

// Struct is another built in type.  A struct aggregates "fields."
// Pair here has two fields, ints named x and y.
type pair struct {
	x, y int
}

// User defined types can have "methods." These are functions that operate
// in the context of an instance of the user defined type.  The instance
// is called the "receiver" and is identified with a declaration just in front
// of the method name.  The receiver here is "p." In most ways the receiver
// works just like a function parameter.
//
// This String method has the same name and return value as the String method
// of the Stringer interface.  Further, String is the only method of Stringer.
// The pair type thus implements all methods of the Stringer interface and
// we say simply that pair implements Stringer.  No other syntax is needed.
func (p pair) String() string {
	// Sprintf is another public function in package fmt.
	// Dot syntax references fields of p.
	return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
	// Brace syntax is a "struct literal."  It evaluates to an initialized
	// struct.  The := syntax declares and initializes p to this struct.
	p := pair{3, 4}
	fmt.Println(p.String()) // call String method of p, of type pair.
	var i Stringer          // declare i of type Stringer.
	i = p                   // valid because pair implements Stringer
	// Call String method of i, of type Stringer.  Output same as above.
	fmt.Println(i.String())
	// It gets more interesting now.  We defined Stringer in this file,
	// but the same interface happens to be defined in package fmt.
	// Pair thus implements fmt.Stringer as well, and does so with no
	// declaration of the fact.  The definition of pair doesn't mention
	// any interfaces at all, and of course the authors of fmt.Stringer
	// had no idea that we were going to define pair.
	//
	// Functions in the fmt package know how to print some standard built in
	// types, and beyond that, they see if a type implements fmt.Stringer.
	// If so, they simply call the String method to ask an object for a
	// printable representation of itself.
	fmt.Println(p) // output same as above. Println calls String method.
	fmt.Println(i) // output same as above

	learnErrorHandling()
}

func learnErrorHandling() {
	// Sometimes you just need to know if something worked or not.  Go has
	// a ", ok" idiom for that.  Something, a map expression here, but commonly
	// a function, can return a boolean value of ok or not ok as a second
	// return value.
	m := map[int]string{3: "three", 4: "four"}
	if x, ok := m[1]; !ok { // , ok is optional but see how useful it is.
		fmt.Println("no one there")
	} else {
		fmt.Print(x)
	}
	// An error value communicates not just "ok" but more about the problem.
	if _, err := strconv.Atoi("non-int"); err != nil { // _ discards value
		// prints "strconv.ParseInt: parsing "non-int": invalid syntax"
		fmt.Println(err)
	}
	// error is a built in type.  It is an interface with a single method,
	// defined internally as,
	//
	// type error interface {
	//     Error() string
	// }
	//
	// The string returned by the Error method is conventionally a printable
	// error message.  You can define your own error types by simply adding
	// an Error method.  Your type then automatically implements the error
	// interface.  We've seen two interfaces now, fmt.Stringer and error.

	// We'll revisit interfaces a little later.  Meanwhile,
	learnConcurrency()
}

// Go has concurrency support in the language definition.  The element of
// concurrent execution is called a "goroutine" and is similar to a thread
// but "lighter."  Goroutines are multiplexed to operating system threads
// and a running Go program can have far more goroutines than available OS
// threads.  If a machine has multiple CPU cores, goroutines can run in
// parallel.
//
// Go "Channels" allow communication between goroutines in a way that is
// both powerful and easy to understand.  Channel is a type in Go and objects
// of type channel are first class objects--they can be assigned to variables,
// passed around to functions, and so on.  A channel works conceptually much
// like a Unix pipe.  You put data in at one end and it comes out the other.
// Channel "send" and "receive" operations are goroutine-safe.  No locks
// or additional synchronization is needed.

// Inc increments a number, and sends the result on a channel.  The channel
// operation makes this function useful to run concurrently with other
// goroutines.  There is no special declaration though that says this function
// is concurrent.  It is an ordinary function that happens to have a
// parameter of channel type.
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
	// Select has syntax like a switch statement but is doing something
	// pretty different.  Each case involves a channel operation.  In rough
	// terms, a case is selected at random out of the cases that are ready to
	// communicate.  If none are ready, select waits for one to become ready.
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

// A simple web server can be created with a single function from the standard
// library.  ListenAndServe, in package net/http, listens at the specified
// TCP address and uses an object that knows how to serve data.  "Knows how"
// means "satisfies an interface."  The second parameter is of type interface,
// specifically http.Handler.  http.Handler has a single method, ServeHTTP.
func learnWebProgramming() {
	err := http.ListenAndServe(":8080", pair{})
	// Error returns are ubiquitous in Go.  Always check error returns and
	// do something with them.  Often it's enough to print it out as an
	// indication of what failed.  Of course there are better things to do
	// in production code: log it, try something else, shut everything down,
	// and so on.
	fmt.Println(err)
}

// You can make any type into an http.Hander by implementing ServeHTTP.
// Lets use the pair type we defined earlier, just because we have it
// sitting around.  ServeHTTP has two parameters.  The request parameter
// is a struct that we'll ignore here.  http.ResponseWriter is yet another
// interface!  Here it is an object supplied to us with the guarantee that
// it implements its interface, which includes a method Write.
// We call this Write method to serve data.
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.Write([]byte("You learned Go in Y minutes!"))
}

// And that's it for a proof-of-concept web server!  If you run this program
// it will print out all the lines from the earlier parts of the lesson, then
// start this web server.  To hit the web server, just point a browser at
// localhost:8080 and you'll see the message.  (Then you can probably press
// ctrl-C to kill it.)
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

