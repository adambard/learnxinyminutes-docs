---
language: v
filename: vlang.v
contributors:
    - ["Maou Shimazu", "https://github.com/Maou-Shimazu"]
---

V is a statically typed compiled programming language 
designed for building maintainable software.

It's similar to Go and its design has also been influenced by 
Oberon, Rust, Swift, Kotlin, and Python.

The language promotes writing 
simple and clear code with minimal abstraction.

Despite being simple, V gives the developer a lot of power. 
Anything you can do in other languages, you can do in V.

```v
// Single Line Comment.
/*
    Multi Line Comment
*/

struct User { // Cannot be defined in main, explained later.
	age  int
	name string
	pos int = -1 // custom default value
}
// struct method
fn (u User) can_register() bool {
	return u.age > 16
}

struct Parser {
	token Token
}

// c like enums
enum Token {
	plus
	minus
	div
	mult
}

// 1. functions
// language does not use semi colons
fn add(x int, y int) int {
	return x + y 
}
// can return multiple values
fn foo() (int, int) {
	return 2, 3
}

// function visibility 
pub fn public_function() { // pub can only be used from a named module.
}

fn private_function() {
}



// Main function
fn main() {
	// Anonymous functions can be declared inside other functions:
	double_fn := fn (n int) int {
		return n + n
	}
	// 2. Variables: they are immutable by default
	// implicitly typed
	x := 1
	// x = 2 // error
	mut y := 2
	y = 4
	name := "John"
	large_number := i64(9999999999999)
    println("$x, $y, $name, $large_number") // 1, 4, John, 9999999999999

	// unpacking values from functions.
	a, b := foo()
	println("$a, $b") // 2, 3
	c, _ := foo() // ignore values using `_`
	println("$c") // 2

	// Numbers
	u := u16(12)
	v := 13 + u    // v is of type `u16`
	r := f32(45.6)
	q := r + 3.14  // x is of type `f32`
	s := 75        // a is of type `int` 
	l := 14.7      // b is of type `f64` 
	e := u + s     // c is of type `int`
	d := l + r     // d is of type `f64`

	// Strings
	mut bob := 'Bob'
	assert bob[0] == u8(66) // indexing gives a byte, u8(66) == `B`
	assert bob[1..3] == 'ob'  // slicing gives a string 'ob'
	bobby := bob + 'by' // + is used to concatenate strings
	println(bobby) // "Bobby"
	bob += "by2" // += is used to append to strings
	println(bob) // "Bobby2"

	//String values are immutable. You cannot mutate elements:
	//mut s := 'hello 🌎'
	//s[0] = `H` // not allowed

	//For raw strings, prepend r. Escape handling is not done for raw strings:
	rstring := r'hello\nworld' // the `\n` will be preserved as two characters
	println(rstring) // "hello\nworld"

	// string interpolation
	println('Hello, $bob!') // Hello, Bob!
	println('Bob length + 10: ${bob.len + 10}!') // Bob length + 10: 13!

	// 3. Arrays
	mut numbers := [1, 2, 3]
	println(numbers) // `[1, 2, 3]`
	numbers << 4 // append elements with <<
	println(numbers[3]) // `4`
	numbers[1] = 5
	println(numbers) // `[1, 5, 3]`
	// numbers << "John" // error: `numbers` is an array of numbers
	numbers = [] // array is now empty
	arr := []int{len: 5, init: -1}
	// `arr == [-1, -1, -1, -1, -1]`, arr.cap == 5

	number_slices := [0, 10, 20, 30, 40]
	println(number_slices[1..4]) // [10, 20, 30]
	println(number_slices[..4]) // [0, 10, 20, 30]
	println(number_slices[1..]) // [10, 20, 30, 40]

	// 4. structs and enums
	// struct User {
	// 	age  int
	// 	name string
	//  pos int = -1 // custom default value
	// }
	mut users := User{21, 'Bob', 0}
	println(users.age) // 21
	
	// enum Token {
	// 	plus
	// 	minus
	// 	div
	// 	mult
	// }

	// struct Parser {
	// 	token Token
	// }
	parser := Parser{}
	if parser.token == .plus || parser.token == .minus 
	|| parser.token == .div || parser.token == .mult {
		// ...
	}


	// 5. Maps
	number_map := {
		'one': 1
		'two': 2
	}
	println(number_map) // {'one': 1, 'two': 2}
	println(number_map["one"]) // 1
	mut m := map[string]int{} // a map with `string` keys and `int` values
	m['one'] = 1
	m['two'] = 2
	println(m['one']) // "1"
	println(m['bad_key']) // "0"
	m.delete('two')

	// 6. Conditionals
	a_number := 10
	b_number := 20
	if a_number < b {
		println('$a_number < $b_number')
	} else if a_number > b {
		println('$a_number > $b_number')
	} else {
		println('$a_number == $b_number')
	}
	num := 777
	even_odd := if num % 2 == 0 { 'even' } else { 'odd' }
	println(even_odd)

	match even_odd {
		'even' { println('even') }
		'odd' { println('odd') }
		else { println('unknown') }
	} 

	// 7. Loops
	loops := [1, 2, 3, 4, 5]
	for lp in loops {
		println(lp)
	}
	loop_names := ['Sam', 'Peter']
	for i, lname in loop_names {
		println('$i) $lname')
		// Output: 0) Sam
		//         1) Peter
	}
	// You can also use break and continue followed by a 
	// label name to refer to an outer for loop:
	outer: for i := 4; true; i++ {
		println(i)
		for {
			if i < 7 {
				continue outer
			} else {
				break outer
			}
		}
	}
}
```

## Further reading

There are more complex concepts to be learnt in V which are available at the
official [V documentation](https://github.com/vlang/v/blob/master/doc/docs.md).

You can also find more information about the V language at the [official website](https://vlang.io/)
or check it out at the [v playground](https://v-wasm.vercel.app/).