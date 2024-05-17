---
language: Umka
filename: learnumka.um
contributors:
    - ["Marek Maškarinec", "https://mrms.cz"]
---

Umka is a statically typed scripting language meant to be embedded into
programs, useful in cases like mod support in games, or user scripting.

It draws inspiration from Pascal and Go.

```
// Single line comment
/* Multi line comments
 they can't be nested */

// The import statement can import other files
import (
	"std.um" // the standard library - included in the interpreter
	"map.um" // hashmaps
	"utf8.um" // utf8 encoding and decoding
)

// Functions are declared with the fn keyword. They have to be declared before
// usage. To combat this, umka allows function prototypes.
// The main function serves as an entry point when running the file.
// Included files can contain main too.
fn main() {
	// printf works the same as in c. It is builtin, so you don't need to
	// import any file to use it.
	printf("Hello world!\n")

	// Variables have to be declared before use. There are multiple ways to do so.
	var x: int = 3 // full declaration
	var y: uint32 // you can ignore the value
	var z = 5 // you can also ignore the type
	a := 6 // umka also allows short declarations like in go

	y = 4 // variable assignment

	// there aren't doubles or floats in umka. They are called real and
	// real32 instead.
	var pi: real = 3.14

	// if statements
	if pi == 3.14 {
		printf("Close enough.\n")
	} else if pi == 3 {
		printf("You should learn about real values.\n")
	} else {
		printf("You live in a strange world.\n")
	}

	// Umka has only one key word for loops.
	// Traditional while loop:
	for z > 0 {
		printf("%d\n", z)
		z--
	}

	// for loop:
	for i:=0; i < y; i++ {
		printf("%d\n", i)
	}

	// the string type is called str
	var name: str = "John"
	// string concatenation
	name += " Doe"

	// Umka has two types of arrays - fixed and dynamic. Fixed have fixed size,
	// are stored on the stack and passed by value. Dynamic are the opposite.
	// You can append and slice them, they are stored on the heap and passed by
	// reference.

	// A fixed array
	names := [4]str{"Jonathan", "Joseph", "Jotaro", "Josuke"}

	// Making a dynamic array is done by just removing the number
	dynNames := []str{"Jonathan", "Joseph", "Jotaro", "Josuke"}
	// It is also possible to create a zero initialized dynamic array of a given
	// length. The length doesn't have to be a constant.
	arr := make([]int, 23)

	// Dynamic arrays can be appended. Be aware, that the array is always newly
	// allocated, and the old elements are copied. This makes the process of
	// appending very slow.
	dynNames = append(dynNames, "Giorno")

	// You can also delete an element from an array.
	dynNames = delete(dynNames, 3) // => { "Jonathan", "Joseph", "Jotaro", "Giorno" }

	// You can also slice them. Start inclusive, end exclusive.
	part1 := slice(dynNames, 0, 2)
	// You can ommit the ending index and it will be replaced by the length
	// of the array.
	part2 := slice(dynNames, 2)

	// You can iterate over a string, array and a dynamic array
	// using the "for in" loop.
	for index, value in dynNames {
		printf("%d: %s\n", index, value)
	}

	// Umka supports pointers. You can make a pointer type by writing a ^ before
	// the base type.
	var xPointer: ^int

	// Values are referenced using an &
	xPointer = &x

	// You can dereference them by writing a ^ after the value
	printf("%s: %d\n", repr(xPointer), xPointer^) // => 0xaddress : 3
}

// Functions are declared using the fn keyword. Parameters are in parenthesis,
// the return type follows after a :. Functions don't have to take parameters,
// or return any values.
fn learnFunctions(a, b: int, c: real): real {
	return a + b * c
}

// Functions can return multiple values.
fn learnMultiple(): (int, int) {
	x, y := getPosition()

	x *= 2
	y *= 2

	return x, y
}

// The type keywords allows type aliases
type Number int

// You can use it to define your own structure types.
type Vector2 = struct {
	x, y: real
}

fn newVector2(x, y: read): Vector2 {
	// Structure literals
	return Vector2{ x, y }
}

// You can define methods on any type from the current module.
fn (v: ^Vector2) multiply(x: real) {
	v.x *= x
	v.y *= x
}

// An interface is a form of duck typing in Umka. It defines a set of methods,
// that must be implemented by a type to implement said interface.
type Representable = interface {
	toStr(): str
}

// Vector2 now implements Representable
fn (v: ^Vector2) toStr(): str {
	return "[ " + repr(v.x) + repr(v.y) + "]"
}

// This function takes a variadic number of Representable values
fn println(args ...Representable) {
	// variadic arguments act like an array inside a function
	for arg in args {
		printf("%s", repr(arg))
	}

	printf("\n")
}
```

## Further reading

You can learn more details in the [documentation](https://github.com/vtereshkov/umka-lang/tree/master/doc).
If you want to read real Umka code, read some of the [examples](https://github.com/vtereshkov/umka-lang/tree/master/examples).
