---
language: D 
filename: learnd.d 
contributors:
    - ["Nick Papanastasiou", "www.nickpapanastasiou.github.io"]
lang: en
---

If you're like me and spend way to much time on the internet, odds are you've heard 
about [D](http://dlang.org/). The D programming language is a modern, general-purpose,
multi-paradigm language with fantastic support for OOP, functional programming, metaprogramming,
and easy concurrency and parallelism, and runs the gamut from low-level features such as
memory management, inline assembly, and pointer arithmetic, to high-level constructs 
such as higher-order functions and generic structures and functions via templates, all with
a pleasant syntax, and blazing fast performance! 

D is actively developed by Walter Bright and Andrei Alexandrescu, two super smart, really cool
dudes. With all that out of the way, let's look at some examples!

```d
// You know what's coming...
module hello;

import std.stdio;

// args is optional
void main(string[] args) {
    writeln("Hello, World!");
}

// Conditionals and loops work as expected.
import std.stdio;

void main() {
    for(int i = 0; i < 5; i++) {
        writeln(i);
    }

    auto n = 1; // use auto for type inferred variables

    while(n < 10_000) {
        n += n;
    }

    do {
        n -= (n / 2);
    } while(n > 0);

    // For and while are nice, but in D-land we prefer foreach
    foreach(i; 1..int.max) { // The .. creates a continuous range 
        if(n % 2 == 0)
            writeln(i);
    }

    foreach_reverse(i; 1..short.max) {
        if(n % 2 == 1)
            writeln(i);
        else
            writeln("No!");
    }
}
```

We can define new types and functions with `struct`, `class`, `union`, and `enum`. Structs and unions
are passed to functions by value (i.e. copied) and classes are passed by reference. Futhermore,
we can use templates to parameterize all of these on both types and values!

```d
// Here, T is a type parameter. Think <T> from C++/C#/Java
struct LinkedList(T) {
    T data = null;
    LinkedList!(T)* next; // The ! is used to instaniate a parameterized type. Again, think <T> 
}

class BinTree(T) {
    T data = null;

    BinTree!T left;
    BinTree!T right;
}

enum Day {
    Sunday,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
}

// Use alias to create abbreviations for types

alias IntList = LinkedList!int;
alias NumTree = BinTree!double;

// We can create function templates as well!

T max(T)(T a, T b) {
    if(a < b) 
        return b;

    return a;
}

// Use the ref keyword to pass by referece
void swap(T)(ref T a, ref T b) {
    auto temp = a;

    a = b;
    b = a; 
}

// With templates, we can also parameterize on values, not just types
class Matrix(T = int, uint m, uint n) {
    T[m] rows;
    T[n] columns;
}
```

Speaking of classes, let's talk about properties for a second. A property
is roughly a function that may act like an lvalue, so we can
have the syntax of POD structures (`structure.x = 7`) with the semantics of
getter and setter methods (`object.setX(7)`)!
