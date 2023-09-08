---
language: Tact
filename: tact.tc
contributors:
  - ["Tal Kol", "https://www.orbs.com/"]
  - ["Kirill Malev", "https://fslabs.io"]
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

## Additional resources

- [TON Documentation](https://ton.org/docs/#/)
- [Tact Docs](https://docs.tact-lang.org/)
- [Community portal](https://society.ton.org)
- [Blockchain portal](https://ton.org)
- [Stackoverflow](https://stackoverflow.com/questions/tagged/ton)

## Social
- [Tact community](https://t.me/tactlang)
- [Developer community](https://t.me/tondev_eng)
- [TON Learn](https://t.me/ton_learn)
- [Tondev News](https://t.me/tondevnews)

## Useful blogposts

- [Setting up a TON Development Environment](https://society.ton.org/setting-up-a-ton-development-environment)
- [Hello World on TON](https://society.ton.org/ton-hello-world-step-by-step-guide-for-writing-your-first-smart-contract-in-func)

## Future To Dos

- Add smart contracts examples
- Add more links to documentations

This file is based on [Tact By Example](https://tact-by-example.org/04-decimal-point).

P.S. If by any chance you're familiar with [Forth](https://learnxinyminutes.com/docs/forth/),
you can also take a look at [Fift](https://ton-blockchain.github.io/docs/fiftbase.pdf).
