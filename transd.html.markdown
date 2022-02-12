---
category: language
language: Transd
filename: learntransd.td
contributors:
    - ["Albert Berger", "http://github.com/al-berger"]
---

```
// One-line comment

/* Multiline
   comment */
   
/*
  Transd is a strongly typed data oriented programming language. It includes elements of
  the object oriented paradigm, functional programming, and has the strong emphasis on the
  processing of user data in various formats. It has classes, built-in data query sublanguage,
  means for simplified conversion of text data into active program objects, etc.
*/

// The first line of program code in each source file of a Transd program, should be the language marker:

#lang transd

// All code must reside within the top program objects called modules

module MainModule : {

// Variables are declared as follows

var1: Int(),
var2: 5,

// The statement separator in Transd is comma

var3: "Hello, World!",

// The language has generic containers

vec1: Vector<String>(),
vec2: Vector<Int>( [1,2,3,4,5] ),
vec3: [1.34, 2.36, 7.89, 10.11],

// Associative arrays are called indexes

idx1: Index<Int String>( {1:"one", 2:"two", 3:"three"} ),




}

```
