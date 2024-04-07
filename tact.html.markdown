---
language: Tact
filename: tact.tc
contributors:
  - ["Tal Kol", "https://www.orbs.com/"]
  - ["Kirill Malev", "https://fslabs.io"]
  - ["Yash Garg", "https://github.com/yash0501"]
---

Tact language is used to program smart contracts on the
[The Open Network](https://ton.org) blockchain. Contract logic is executed in
TVM, the stack-based TON Virtual Machine.

Tact is a statically typed, but language was designed to be friendly for
developers with JS and Python background.

This page is based on [Tact-by-Example](https://tact-by-example.org/).
You can use this resource to play around with contracts and check out
the interactive features.

# Basic syntax, function definition

```c
// Single line comment

 // This is a multi-line comment
  // this is a comment in the comment

    get fun greeting(): String {
    // This is a function that returns "hello world" message
    // Return type is specified after a colon :
        return "hello world";
    }
```

# A Simple Counter contract

This is a simple counter contract that allows users to increment its value.

This contract has a state variable `val` that persists between contract calls

- the counter value. When persisted, this variable is encoded as `uint32` -
  a 32-bit unsigned integer. Contracts pay rent in proportion to the amount
  of persistent space they consume, so compact representations are encouraged.

State variables should be initialized in `init()` that runs on deployment of
the contract.

## Messages

The actor model is a model of concurrent computation and is at the heart of TON
smart contracts. Each smart contract can process one message at a time, change
its own state, or send one or several messages. Processing of the message
occurs in one transaction, that is, it cannot be interrupted. Messages to one
contract are processed consequently one by one. As a result, the execution of
each transaction is local and can be parallelized at the blockchain level,
which allows for on-demand throughput horizontal scaling and hosting an
unlimited number of users and transactions.

## Receiving messages

This contract can receive messages from users. Unlike getters that are just
read-only, messages can do write operations and change the contract's
persistent state. Incoming messages are processed in receive() methods as
transactions and cost gas for the sender.

After deploying the contract, send the increment message by pressing the Send
increment button in order to increase the counter value by one. Afterwards,
call the getter value() to see that the value indeed changed.

```c
contract Counter {
// Tact allows to create a contract
    // persistent state variable of type Int to hold the counter value
    val: Int as uint32;

    // initialize the state variable when contract is deployed
    init() {
        self.val = 0;
    }

    // handler for incoming increment messages that change the state
    receive("increment") {
        self.val = self.val + 1;
    }

    // read-only getter for querying the counter value
    get fun value(): Int {
        return self.val;
    }
}
```

# The Deployable Trait

Tact doesn't support classical class inheritance, but contracts can implement
traits. One of the commonly used traits is `Deployable`. It implements a simple
receiver for the Deploy message which helps deploy contracts in a standard way.

All contracts are deployed by sending them a message. This can be any message,
but best practice is to designate the special `Deploy`
message for this purpose.

This message has a single field, `queryId`, which is provided by the deployer
(normally zero). If the deploy succeeds, the contract will reply with the
message `DeployOk` and echo the same `queryId` in the response.

If you're using Tact's [auto-generated](https://docs.tact-lang.org/tools/typescript#tact-contract-in-typescript) TypeScript
classes to deploy, sending the deploy message should look like:

```c
const msg = { $$type: "Deploy", queryId: 0n };
 await contract.send(sender, { value: toNano(1) }, msg);
```

You can see the implementation of the trait [here](https://github.com/tact-lang/tact/blob/main/stdlib/libs/deploy.tact).
Notice that the file deploy.tact needs to be imported from the standard
library using the import keyword.

```c
// this trait has to be imported
import "@stdlib/deploy";

// the Deployable trait adds a default receiver for the "Deploy" message
contract Counter with Deployable {

    val: Int as uint32;

    init() {
        self.val = 0;
    }

    receive("increment") {
        self.val = self.val + 1;
    }

    get fun value(): Int {
        return self.val;
    }
}
```

# Integers

Tact supports a number of primitive data types that are tailored for
smart contract use.

`Int` is the primary number type. Math in smart contracts is always done
with integers and never with floating points since floats are [unpredictable](https://learn.microsoft.com/en-us/cpp/build/why-floating-point-numbers-may-lose-precision).

The runtime type `Int` is always 257-bit signed, so all runtime calculations
are done at 257-bit. This should be large enough for pretty much anything you
need as it's large enough to hold the number of atoms in the universe.

Persistent state variables can be initialized inline or inside `init()`.
If you forget to initialize a state variable, the code will not compile.

## State costs

When encoding `Int` to persistent state, we will usually use smaller
representations than 257-bit to reduce storage cost.
The persistent state size is specified in every declaration of
a state variable after the `as` keyword.

Storing 1000 257-bit integers in state [costs](https://ton.org/docs/develop/smart-contracts/fees#how-to-calculate-fees) about
0.184 TON per year. Storing 1000 32-bit integers only costs
0.023 TON per year by comparison.

```c
import "@stdlib/deploy";

contract Integers with Deployable {

    // contract persistent state variables
    // integers can be persisted in state in various sizes
    // range -2^256 to 2^256 - 1 (takes 257 bit = 32 bytes + 1 bit)
    i1: Int as int257 = 3001;
    i2: Int as uint256;         // range 0 to 2^256 - 1 (takes 256 bit = 32 bytes)
    // range -2^255 to 2^255 - 1 (takes 256 bit = 32 bytes)
    i3: Int as int256 = 17;
    i4: Int as uint128;         // range 0 to 2^128 - 1 (takes 128 bit = 16 bytes)
    // range -2^127 to 2^127 - 1 (takes 128 bit = 16 bytes)
    i5: Int as int128;
    i6: Int as coins;           // range 0 to 2^120 - 1 (takes 120 bit = 15 bytes)
    // range 0 to 18,446,744,073,709,551,615 (takes 64 bit = 8 bytes)
    i7: Int as uint64 = 0x1c4a;
    // range -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807
    // (takes 64 bit = 8 bytes)
    i8: Int as int64 = -203;
    i9: Int as uint32 = 0;      // range 0 to 4,294,967,295 (takes 32 bit = 4 bytes)
    // range -2,147,483,648 to 2,147,483,647 (takes 32 bit = 4 bytes)
    i10: Int as int32 = 0;
    i11: Int as uint16 = 0;     // range 0 to 65,535 (takes 16 bit = 2 bytes)
    i12: Int as int16 = 0;      // range -32,768 to 32,767 (takes 16 bit = 2 bytes)
    i13: Int as uint8 = 0;      // range 0 to 255 (takes 8 bit = 1 byte)
    i14: Int as int8 = 0;       // range -128 to 127 (takes 8 bit = 1 byte)

    init() {
        // we can define numbers in hex (base 16)
        self.i2 = 0x83dfd552e6372;
        self.i4 = 1507998500293440234999; // we can define numbers in decimal
        self.i5 = pow(10, 9);   // this is 10^9 = 1,000,000,000
        self.i6 = ton("1.23");  // easy to read coin balances
        //  (coins type is nano-tons, like cents, just with 9 decimals)
    }

    receive("show all") {
        dump(self.i1);
        dump(self.i2);
        dump(self.i3);
        dump(self.i4);
        dump(self.i5);
        dump(self.i6);
        dump(self.i7);
        dump(self.i8);
    }

    get fun result(): Int {
        return self.i1;
    }
}
```

## Bools, Addresses, Strings, Operators and Constants

### Bool

Bool can be used for boolean variables

```js
b1: Bool = true;
b2: Bool = false;
```

### Address

Address is another primitive data type. It represents standard addresses on
the TON blockchain.
TON is divided into multiple chains called workchains. One of the internal
fields of the address is the workchain id:
0 - The standard workchain, for regular users. Your contracts will be here.
-1 - The masterchain, usually for validators.

```js
// bouncable (same foundation wallet)
a1: Address = address("EQCD39VS5jcptHL8vMjEXrzGaRcCVYto7HUn4bpAOg8xqB2N");
// non-bounceable (same foundation wallet)
a2: Address = address("UQCD39VS5jcptHL8vMjEXrzGaRcCVYto7HUn4bpAOg8xqEBI");
```

### String

Tact has basic support for strings. Strings support unicode and don't
have any special escape characters like \n.
Strings are immutable. Once a sequence of characters is created, this
sequence cannot be modified.
If you need to concatenate strings in run-time, you can use a StringBuilder.
This object handles gas efficiently and supports append() of various types to
the string.

```js
s1: String = "hello world";
sb: StringBuilder = beginString();
sb.append(self.s1);
```

### Integer Operations

Addition, subtraction, multiplication, division, modulo,
shift left and right, minimum and maximum numbers, absolute value

```js
i: Int = -12; // temporary variable, runtime Int type is always int257
i = i1 * 3 + (i2 - i); // basic math expressions
i = i1 % 10; // modulo (remainder after division), 3001 % 10 = 1
i = i1 / 1000; // integer division (truncation toward zero), 3001 / 1000 = 3
i = i1 >> 3; // shift right (multiply by 2^n)
i = i1 << 2; // shift left (divide by 2^n)
i = min(i2, 11); // minimum between two numbers
i = max(i2, 66); // maximum between two numbers
i = abs(-1 * i2); // absolute value
```

### Constants

Unlike variables, constants cannot change. Their values are
calculated in compile-time and cannot change during execution.

```js
const StateUnpaid: Int = 0;
```

## Getters, Receivers and Messages

### Getters

Getters are special contract functions that allow users to query
information from the contract.
Contract methods starting with the prefix get fun are all getters.
Calling getters is free and does not cost gas.
Getters are read-only, they cannot change the contract persistent state.
A contract cannot execute a getter of another contract. Getters are only
executable by end-users off-chain.

```js
count: Int as uint32 = 17;

get fun counter(): Int {
    return self.count;
}
```

### Receivers

Contract methods named receive() are the handlers that process
each incoming message type.
Tact will automatically route every incoming message to the correct receiver
listening for it according to its type. A message is only handled by one receiver.

Handler for "increment" textual message - this is a textual string message,
these cannot carry input arguments

```js
receive("increment") {
    self.val = self.val + 1;
}
```

### Messages

Messages are defined using the message keyword. They can carry input
arguments. For integers, you must define the encoding size, just like in
state variables.

Handler for the "Add" message - this is a binary message that has an input
argument (amount)

```js
receive(msg: Add) {
    self.val = self.val + msg.amount;
}
```

## Structs

Structs allow you to combine multiple primitives together in a more semantic way.
Structs can define complex data types that contain multiple fields of
different types. They can also be nested.

```js
// Normal struct
struct Point {
    x: Int as int64;
    y: Int as int64;
}

// Nested struct
struct Params {
    name: String = "Satoshi";   // default value
    age: Int? = null;           // optional field
    point: Point;               // nested structs
}
```

## Message Sender and Throwing Errors

### Message Sender

Every incoming message is sent from some contract that has
an address. You can query the address of the message sender by calling sender()

```js
deployer: Address = sender();
```

### Errors

When an error is thrown, the transaction reverts. By writing a
require() on a condition that isn't met

```js
require(self.val < 5, "Counter is too high");
```

## Messages Between Contracts, Sending and Receiving TON Coins

### Messages Between Contracts

Different contracts can only communicate with
each other by sending each other messages.

This example sends a message to the to address with value of 1 TON and body
of a comment with a string "Hello, World!".
SendIgnoreErrors means that even when error occurs during message sending
next messages would be sent anyway.

```js
let to: Address = ...;
let value: Int = ton("1");
send(SendParameters{
    to: to,                             // address of receiver
    value: value,                       //  amount of TON you want to send
    mode: SendIgnoreErrors,             // 8-bit flag configuring how to send message
    bounce: true,                       // if set to true (default) then message
                                        // will be bounced back to sender
    body: "Hello, World!".asComment()   // message body as Cell
});
```

### Receiving TONs

You can query the contract balance with myBalance() - note
that the value is in nano-tons (like cents, just with 9 decimals). The balance
already contains the incoming message value.
You can also get the incoming TON balance with context().value

```js
val: Int as int64 = myBalance()
// or
// print how much TON coin were sent with this message
dump(context().value);
```

### Sending TONs

We can send any amount of TON to any address just like we created
a send call between different contracts

Send mode SendRemainingValue will add to the outgoing value any excess left
from the incoming message after all gas costs are deducted from it.

```js
amount: Int as coins = ton("1");
send(SendParameters{
    to: sender(),
    bounce: true,
    value: amount,
    mode: SendRemainingValue + SendIgnoreErrors
});
```

## If/Else statements and Loops

### If

Tact supports if statements in a similar syntax to most programming
languages. Curly braces are required.
We can have the else and else if similar to other programming languages.

```js
if (val > 1000) {
  dump("larger than 1000");
} else if (val > 500) {
  dump("between 500 and 1000");
} else {
  dump("smaller than 500");
}
```

### Loops

Tact does not support traditional 'for' loops, 'break' and 'continue'
statements in loops.
The repeat loop statement input number must fit within an int32.

```js
// repeat exactly 10 times

repeat (10) {
    i = i + 1;
    sum = sum + i;
}

// While loop

let x: Int = 10;
while(x > 0) {
  x = x - 1;
}

// do-until loop

let x: Int = 10;
do {
  x = x - 1;
} until (x <= 0);
```

## Functions

Functions in Tact start with the fun keyword. Functions can receive multiple
input arguments and can optionally return a single output value. You can
return a struct if you want to return multiple values.

```js
fun average(a: Int, b: Int): Int {
    return (a + b) / 2;
}
```

## Maps and Arrays

### Maps

Maps are a dictionary type that can hold an arbitrary number of items,
each under a different key.
The keys in maps can either be an Int type or an Address type.
You can check if a key is found in the map by calling the get() method.
Replace the value under a key by calling the set() method.

```js
mi1: map<Int, TokenInfo>;           // maps with Int as key
ma1: map<Address, TokenInfo>;       // maps with Address as key
```

### Arrays

To create an array, define a map with 'Int' type as key as well as value.

```js
arr: map<Int, Int>; // this is our array implemented with a map
```

## Ownable Standard Library

The Ownable trait allows the contract to set an owner role, which can have
higher priviliges from everybody else.
For this you would need to import the "@stdlib/ownable" library and inherit
it in your contract

- Use the self.requireOwner() call to verify that the person making that
  function call is the owner of contract
- 'ChangeOwner{newOwner: Address}' message which allows the owner to
  transfer ownership.
- Define state variables named 'owner: Address' and 'stopped: Bool' and
  call 'self.requireNotStopped()' on actions that should be stopped.
- Define state variables named 'owner: Address' and "stopped: Bool' and
  call 'self.requireNotStopped()' on actions that should be stopped.

```js
import "@stdlib/ownable";
import "@stdlib/deploy";

contract Counter with Deployable, Ownable {
    owner: Address;

    init() { // initialize a contract with default values like 'constructor'
        self.owner = sender(); // we can initialize owner to any value we want, the deployer in this case
        self.val = 0;
    }

    // this message in only available to the owner
    receive("double") {
        self.requireOwner();
        self.val = self.val * 2;
    }

    // this message will only work until the contract was stopped
    receive("increment") {
        self.requireNotStopped();
        self.val = self.val + 1;
    }

    // this message will only work as long as the contract is not stopped
    receive("increment2") {
        self.requireNotStopped();
        self.val = self.val + 1;
    }
}
```

## Additional resources

- [TON Documentation](https://ton.org/docs/#/)
- [Tact Docs](https://docs.tact-lang.org/)
- [Tact by Example](https://tact-by-example.org/)
- [Community portal](https://society.ton.org)
- [Blockchain portal](https://ton.org)
- [Stackoverflow](https://stackoverflow.com/questions/tagged/ton)

## Social

- [Tact community](https://t.me/tactlang)
- [Developer community](https://t.me/tondev_eng)
- [TON Learn](https://t.me/ton_learn)
- [Tondev News](https://t.me/tondevnews)
- [TON Technical Updates](https://t.me/thetontech)

## Useful blogposts

- [Setting up a TON Development Environment](https://society.ton.org/setting-up-a-ton-development-environment)
- [Hello World on TON](https://society.ton.org/ton-hello-world-step-by-step-guide-for-writing-your-first-smart-contract-in-func)

## Future To Dos

- Add smart contracts examples
- Add more links to documentations

This file is based on [Tact By Example](https://tact-by-example.org).

P.S. If by any chance you're familiar with [Forth](https://learnxinyminutes.com/docs/forth/),
you can also take a look at [Fift](https://ton-blockchain.github.io/docs/fiftbase.pdf).
