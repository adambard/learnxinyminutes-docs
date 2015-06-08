---
language: d 
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

'''d
// You know what's coming...
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

    foreach_reverse(i; short.max) {
        if(n % 2 == 1)
            writeln(i);
        else
            writeln("No!");
    }
}

'''
