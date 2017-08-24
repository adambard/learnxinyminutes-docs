---
language: vala
contributors:
    - ["Milo Gilad", "https://github.com/bigbash"]
filename: LearnVala.vala
---

In GNOME's own words, "Vala is a programming language that aims to bring modern programming language features to GNOME developers without imposing any additional runtime requirements and without using a different ABI compared to applications and libraries written in C."

Vala has aspects of Java and C#, so it'll be familiar to those who know either or.

[Read more here.](https://wiki.gnome.org/Projects/Vala)

```vala
// Single line comment

/* Multiline
Comment */

/**
* Documentation comment
*/

/*
Data Types
*/

// Vala supports the data types supported by most other programming languages.

char character = 'a'
unichar unicode_character = 'u' // 32-bit unicode character

int i; // ints can also have guaranteed sizes (e.g. int64, uint64)
uint j;

long k;

short l;
ushort m;



/*
Basic Syntax
*/

// Like in C#, scope is defined using braces.
// An object or reference is only valid between braces.


```
