---
name: Motoko
filename: learnmotoko.mo
contributors:
  - ["Antonio Rodriguez", "https://github.com/antoniorodr"]
---

### Learn Motoko in Y Minutes

Motoko is a statically typed programming language designed for building secure, scalable software on the Internet Computer blockchain. It supports actor-based concurrency, functional and imperative styles, and has deep integration with WebAssembly and canister smart contracts.

This tutorial assumes familiarity with other programming languages like JavaScript, Rust, or OCaml.

```motoko
// Single-line comment
/* Multi-line
   comment */

// ---- Variables ----

// Immutable binding
let msg : Text = "Hello, Motoko!";

// Mutable binding
var count : Nat = 0;

// ---- Functions ----

func add(x : Int, y : Int) : Int {
  return x + y;
};

let result = add(2, 3); // 5

// Anonymous function
let double = func(x : Int) : Int { x * 2 };

// ---- Control Flow ----

if (result > 4) {
  Debug.print("Greater than 4");
} else {
  Debug.print("Less than or equal to 4");
};

// Expression form
let description = if (count == 0) { "first" } else { "not first" };

// while loop
var i : Nat = 0;
while (i < 3) {
  Debug.print(Nat.toText(i));
  i += 1;
};

// for-in loop
let arr = [1, 2, 3];
for (n in arr.vals()) {
  Debug.print(Nat.toText(n));
};

// ---- Option Types ----

let maybeName : ?Text = ?("Motoko");

switch (maybeName) {
  case (?name) Debug.print("Name is " # name);
  case null Debug.print("No name provided");
};

// ---- Records ----

let person = {
  name = "Alice";
  age = 30;
};

Debug.print(person.name);

// ---- Tuples ----

let coords : (Int, Int) = (10, 20);
let (x, y) = coords;

// ---- Arrays ----

import Array "mo:base/Array";

let squares = Array.map<Int, Int>(arr, func(n) { n * n });

// ---- Modules ----

module Math {
  public func square(n : Int) : Int {
    n * n
  };
};

let nine = Math.square(3);

// ---- Actors ----

actor Hello {
  var count : Nat = 0;

  public func greet() : async Text {
    count += 1;
    Debug.print("Called " # Nat.toText(count) # " times");
    return "Hello from Motoko!";
  };
};

// ---- Async/Await ----

actor Waiter {
  public func waitAndReturn(x : Nat) : async Nat {
    Debug.print("Waiting...");
    return x;
  };
};

// ---- Cross-Canister Calls ----

actor class Caller(target : Principal) = this {
  let other = actor(target) : actor {
    greet : () -> async Text;
  };

  public func callGreet() : async Text {
    await other.greet();
  };
};

// ---- Type Aliases ----

type Age = Nat;
let a : Age = 34;

// ---- Importing Base Library ----

import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import Text "mo:base/Text";

// ---- Resources ----

// Official docs: https://internetcomputer.org/docs/current/motoko/main/
// Base library: https://github.com/dfinity/motoko-base
// Playground: https://m7sm4-2iaaa-aaaab-qabra-cai.raw.icp0.io/

```
