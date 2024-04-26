---
language: Gno
filename: learnGno.gno
contributors:
    - ["Lav Leon Hudak", "https://github.com/leohhhn"]
    - ["Miloš Zivković", "https://github.com/zivkovicmilos"]
---

# Gno
Gno is an interpretation of the widely-used Go (Golang) programming language
created by Cosmos co-founder Jae Kwon to mark a new era in smart
contracting. Syntax-wise, Gno is ~99% identical to Go, allowing Go programmers 
can start coding blockchain apps right away, with a minimal learning curve.

Gno can be run inside the GnoVM. It is possible to run the GnoVM in two modes:
- as a standalone binary, 
- contained in a Gno.land blockchain node. 

Visit the gno.land [homepage](https://gno.land), as well as the [Official 
Gno Documentation](https://docs.gno.land) to learn more.

Let's get started with Gno development.

## Installing necessary tools & binaries
Before installing any Gno-specific tooling, make sure the following 
prerequisites are present on your machine:

- **Git**
- **`make` (for running Makefiles)**
- **Go 1.21+**
- **Go Environment Setup**:
  - Make sure `$GOPATH` is well-defined, and `$GOPATH/bin` is added to your 
`$PATH` variable.

After all prerequisites are present, you can continue with the installation. 
All tools needed for Gno development can be found in the
[Gno monorepo](https://github.com/gnolang/gno).

First, clone the monorepo and `cd` into it:

```bash
git clone git@github.com:gnolang/gno.git
cd gno
```

Next, install all the necessary binaries with the following command:

```bash
make install
```

This will install three binaries required to continue:
- `gnodev` - an all-in-one tool for simple Gno development,
- `gno` - the GnoVM binary for running & testing the Gno code,
- `gnokey` - binary used for keypair management.

## Creating a new Gno project
Gno mimics Go in most areas - including setting up a new project. Let's create
a simple counter application. In an empty folder, run 
`gno mod init gno.land/r/<your_username>/counter`. This will initiate a `gno.mod`
file with an appropriate path for your app. Read more about Gno package paths
[here](https://docs.gno.land/concepts/namespaces).

Following this, you can create a `counter.gno` file which will be the place for your
Gno code. If you already know Go, Gno code will come to you very naturally.

```shell
touch counter.gno
```

In the newly created file, start by declaring your package name:

```go
package counter
```

After this, we can declare a global variable `counter` of type `int`.

```go
var counter int
```

This is where Gno starts to differ from Go - variables declared in the global
scope get auto-persisted. What does this mean?

**Gno is a transaction-based language** - this means that each call to a Gno app
happens within a transactional context, ensuring atomicity, & consistency. Because 
of this, developers can focus on the business logic of their apps instead of 
having to deal with the intricacies of application state management and persistence.  

Moving forward, we can declare an `Increment()` function that will increment our
counter variable by `1`.

```go
func Increment() {
	counter++
}
```

Finally, we can define a `Render()` function - a Gno-specific concept allowing callers
to view the aforementioned persisted state of the app.

```go
func Render(_ string) string {
	return "Current counter value: " + strconv.Itoa(counter)
}
```

The `Render()` function is meant to return a correctly-formatted Markdown string.
We need to import `strconv`, which is part of the Gno standard library, to convert
the counter value to a string. Furthermore, a valid `Render()` function must 
take in a string parameter, which in our case is not used. 

### Testing in Gno
We can test our counter app exactly the same way we would in Go. First, create a
`counter_test.gno` file:
```shell
touch counter_test.gno
```

Declare the counter package name, and write a function testing the `Increment()`
functionality in the Counter package:

```go
package counter

import (
	"testing"
)

func TestIncrement(t*testing.T) {
	Increment() // counter should be 1 after call
	
	// counter is a package-level variable which is why it is available in this context
	if counter != 1 {
		t.Fatalf("expected counter value to be 1, got %d", counter)
	}   
}
```

To run the tests, use the `gno test` command in the package folder: 

```shell
gno test . -v
```

If all went well, you will get the following output:

```shell
=== RUN   TestIncrement
--- PASS: TestIncrement (0.00s)
ok      .       0.71s
```

Congratulations! You've just written & tested your first Gno app. Next step - 
deploying the app.

## Deploying a Gno app
Apart from running them locally, Gno apps can be deployed and ran on blockchain 
networks. This is another fundamental difference between Go and Gno.

Developers can choose to deploy to a local or remote blockchain network
depending on the development stage of their app. Let's discuss how to set up a
local development environment for Gno. 

### Running a local dev environment
The fastest and most convenient way to run a local development environment for 
Gno is by using `gnodev`. It is a binary containing multiple tools that make up
everything you need to write Gno apps:
- a local gno.land node running in the background,
- a `gnoweb` server allowing you to immediately see the state of your app using
the aforementioned `Render()` function.
  
Read more about `gnodev` [here](https://docs.gno.land/gno-tooling/cli/gno-tooling-gnodev).

To start `gnodev`, simply give it the path to where your `.gno` and `gno.mod` files
are:

```bash
gnodev <your_path>
```

Starting `gnodev` will leave you with the following terminal output:

```go
❯ gnodev .
Node        ┃ I pkgs loaded path=[<your_working_dir> <your_gnodev_installation_path>]
Node        ┃ I node started lisn=tcp://127.0.0.1:36657 addr=g1jg8mtutu9khhfwc4nxmuhcpftf0pajdhfvsqf5 chainID=dev
GnoWeb      ┃ I gnoweb started lisn=http://127.0.0.1:8888
--- READY   ┃ I for commands and help, press `h`
```

By giving `gnodev` the path to your counter application, you have automatically
loaded the code into the built-in node. `gnodev` also watches your development
directory, so that it can do an automatic reload of the code when needed.

You can view the output of the `Render` function in your app by visiting the 
`gnoweb` address listed above, and adding to it the path of your app.
In case of the local node contained within `gnodev`, `gno.land/` is replaced by
`http://127.0.0.1:8888`, so the `Render` of your app will be displayed on
`http://127.0.0.1:8888/r/<your_username>/counter`.

### Setting up a Gno keypair
To interact with any `gno.land` network, including the local node contained within
`gnodev`, users must have a public-private keypair. Keypairs allow users to sign
transactions and broadcast them to the node. Keypairs can also be assigned a
balance of units specific to the network they are interacting with. These units
are used to pay for computational power on the network. This serves many purposes,
among which is to stop DDoS attacks on blockchain networks.

By running a mathematical derivation algorithm on a 12 or 24 word phrase, a
public-private keypair is generated. This phrase serves as the "master password"
for the keypair. For more info on keypairs, look into `BIP39` and `HD Wallets`.

To generate a keypair, we can use the `gnokey` tool. `gnokey` provides users with
a randomly generated phrase (and thus keypair) to ensure privacy &
security. 

To start, run the following command:

```shell
gnokey add MyKeypair
```

`gnokey` will ask for a password to encrypt the keypair on your local disk. After 
this, it will show you the phrase that your keypair will be derived from.
Make sure you write this phrase down, as it is the only way you can recover your
keys in case of losing them.

To check if you've successfully added the keypair, run `gnokey list`:

```shell
❯ gnokey list           
0. MyKeypair (local) - addr: g1jg8mtutu9khhfwc4nxmuhcpftf0pajdhfvsqf5 pub: gpub1pgfj7ard9eg82cjtv4u4xetrwqer2dntxyfzxz3pq0skzdkmzu0r9h6gny6eg8c9dc303xrrudee6z4he4y7cs5rnjwmyf40yaj, path: <nil>
```

> The mnemonic and keypair stated above are publicly known. Do not use it 
> for anything other than testing. Instead, generate your own keypair and keep it
> private.

### Interacting with your Gno app
Apart from testing locally, we can manually call our app's `Render()` and
`Increment()` functions using the `gnokey maketx call` command to check if
it's working correctly:

```shell
# Calling Render to check current value of counter
gnokey maketx call \
--pkgpath "gno.land/r/demo/counter" \
--func "Render" \
--args ""
--gas-fee 10000000ugnot \
--gas-wanted 800000 \
--broadcast  \
--remote localhost:26657 \
MyKeypair

# Output:
# ("Current counter value: 0" string)
# OK!
# GAS WANTED: 800000
# GAS USED:   87405
```

Let's break down all the options from the `gnokey` command:
1. `maketx` - create a transaction and sign it
2. `call` - type of message to send to the chain, in this case
[Call](https://docs.gno.land/gno-tooling/cli/gno-tooling-gnokey/#call)
3. `--pkgpath` - path to where the app is found on-chain, defined in the `gno.mod`
file
4. `--func` - name of the function to be called
5. `--gas-wanted` - the upper limit of gas for the execution of the transaction
6. `--gas-fee` - the amount of network currency, in this case `ugnot`s, the caller 
is willing to pay
7. `--broadcast` - broadcast the transaction on-chain
8. `--chain-id` - id of the chain to connect to, in our case the local node, `dev`
9. `--remote` - specify node endpoint, in our case it's our local node
10. `MyKeypair` - the keypair to use for the transaction

Calling `Increment()`:
```shell
gnokey maketx call \
--pkgpath "gno.land/r/demo/counter" \
--func "Increment" \
--gas-fee 10000000ugnot \
--gas-wanted 800000 \
--broadcast  \
--remote localhost:26657 \
test1
```

Finally, calling `Render()` again, we see the updated value:

```shell
gnokey maketx call \
--pkgpath "gno.land/r/demo/counter" \
--func "Render" \   
--args "" \              
--gas-fee 10000000ugnot \
--gas-wanted 800000 \
--broadcast  \            
--remote localhost:26657 \
MyKeypair

# Output
# ("Current counter value: 1" string)
# OK!
# GAS WANTED: 800000
# GAS USED:   87411
```
Congratulations! You have successfully written and tested your first Gno app.

## Gno code organization
Gno code is meant to be reusable and transparent. This is why a recommended way
to organize Gno code exists. Code is meant to be organized in two main categories:
- `Packages` - stateless libraries meant to be reused
- `Realms` - instances of Gno apps, such as the Counter app detailed above

Packages can be deployed under `gno.land/p/demo/`, while realms can be deployed
under `gno.land/r/`. You can view *and import* packages with this path. Try
browsing for this link: [`gno.land/p/demo/blog`](https://gno.land/p/demo/blog). 

Below are some commonly used packages.

### Package `avl`
Deployed under `gno.land/p/demo/avl`, the AVL package provides a tree structure
for storing data. Currently, the AVL package is used to replace the functionality
of the native `map` in Gno, as maps are not fully deterministic and thus do not
work as expected in the language. Here is how using the AVL package from your
realm might look like:

```go
package myrealm

import "gno.land/p/demo/avl"

// This AVL tree will be persisted after transaction calls
var tree *avl.Tree

// Save simply exposes the avl.Set function to the user
func Save(key string, value int) {
	// tree.Set takes in a string key, and a value that can be of any type
	tree.Set(key, value)
}

// Get gets back a value at a specific key, if it exists
func Get(key string) int {
  // tree.Get returns the value at given key in its raw form, 
  // and a bool to signify the existence of the key-value pair
  rawValue, exists := tree.Get(key)
  if !exists {
	  panic("value at given key does not exist")
  }
  // rawValue needs to be converted into the proper type before returning it
  return rawValue.(int)
}
```

### Package `seqid`
Deployed under `gno.land/p/demo/seqid`, the `seqid` package provides a simple
way to have sequential IDs in Gno. Its encoding scheme is based on the `cford32`
package. From [`seqid.gno`](https://gno.land/p/demo/seqid/seqid.gno):

```go
// Package seqid provides a simple way to have sequential IDs which will be
// ordered correctly when inserted in an AVL tree.
//
// Sample usage:
//
//	var id seqid.ID
//	var users avl.Tree
//
//	func NewUser() {
//		users.Set(id.Next().String(), &User{ ... })
//	}
package seqid
```

You can view more package (and realm) examples
[here](https://github.com/gnolang/gno/tree/master/examples).

## Blockchain-specific functionality
Gno is designed to follow the syntax of Go. There are very few exceptions in how
Gno differs from Go syntactically, which is why this section will outline the 
major differences and added features. For a full list of Go-Gno compatability,
check out the [compatability page](https://docs.gno.land/reference/go-gno-compatibility) 
in the official documentation.

This section also concerns the blockchain aspect of Gno. It will introduce 
blockchain concepts that are commonly used in Gno apps. 

When Gno code runs on a blockchain network, it has access to the environment variables of
that network, such as the block height, block timestamp, current caller, amount 
of native currency sent along the call, etc. This context is fundamental for
Gno app development and can be accessed through functions found in the special
`std` package. 

Below you can find a "Learn X in Y"-styled report. Since Gno is designed to 
be as close as possible to Go syntax, we will not cover all bits and pieces
of its syntax, but only the most prominent ones, and the ones that are specific
to the language. 

```go
// Single line comment
/* Multi-
line comment */

// A package clause starts every Gno source file.
// Name of the package needs to match the name found in the `gno.mod` file.
package example

// Import declaration declares library packages referenced in this file.
import (
  // Gno, like Go, has built-in importable standard libraries.
  // View the full compatibility list at https://docs.gno.land/reference/go-gno-compatibility
  "encoding/binary"
  // std is a special Gno standard library that contains
  // blockchain-specific functionality.
  "std"

  // Apart from built-in libraries, you can import code found on the chain.
  "gno.land/p/demo/avl"
  // Gno currently does not support reflection.
  // This is why fmt is only partially supported, in the micro-fmt package:
  "gno.land/p/demo/ufmt"
)

// In Gno, global variables are auto-persisted after each call (transaction)
var (
  admin std.Address // std.Address represents the address of all things on-chain
)

// The init function is special. It is like a constructor - it is run only once
// in the lifetime of a Gno package/realm, upon its deployment.
// It is usually used to set initial values, such as admin addresses
func init() {
  // PrevRealm() retrieves the previous entity in the call stack.
  // When it is used within the init() function, it represents
  // the deployer of the code.
  admin = std.PrevRealm().Addr()
}

// Render returns a correctly-formatted Markdown string, allowing developers
// to have a default way to render the state of their apps.
// Render functions take in a string path which allows for flexibility.
func Render(path string) string {
  if path == "" {
    return "Hello 世界!"
  }

  return ufmt.Sprintf("Hello %s!", path)
}

// Main, contrary to Go, is not special. It does not have any special use cases,
// and its name signifies nothing in the Gno runtime.
func main() {
  // Get block number at time of call
  var height int64 = std.GetHeight()
  // Get current chain ID
  var chainId string = std.GetChainID()
  // Get the previous caller - represented by a Gno public key
  var caller std.Address = std.PrevRealm().Addr()
  // Get the address of entry point of the call
  var entryPoint std.Address = std.GetOrigPkgAddr()
  // Get amount of currency sent with call
  var currencySent int64 = std.GetOrigSend()
  // Get the current realm (smart contract) instance
  var currentRealm std.Realm = std.CurrentRealm()
  // Get the current realm path
  // Shorthand for std.CurrentRealm().PkgPath()
  var currentRealmPath string = std.CurrentRealmPath()

  // Banker - allows for manipulation of native currency
  // Readonly banker
  var readonlyBanker std.Banker = std.GetBanker(std.BankerTypeReadonly)
  // Banker that can manipulate only currency sent with the calling tx
  var origSendBanker std.Banker = std.GetBanker(std.BankerTypeOrigSend)
  // Banker with full access to the app's (realm's) currency
  var realmSendBanker std.Banker = std.GetBanker(std.BankerTypeRealmSend)
  // Banker that can issue new coins
  var realmIssueBanker std.Banker = std.GetBanker(std.BankerTypeRealmIssue)
  // View full banker API on
  // https://docs.gno.land/reference/standard-library/std/banker

  // Coin & Coins - native Gno struct defining currency
  // Make a new coin instance, of 100 coins of denomination lxy
  var lxyCoin std.Coin = std.Coin{"100", "lxy"}
  // Make a different coin instance, of 100 coins of denomination lyx
  var lyxCoin std.Coin = std.Coin{"100", "lyx"}
  // Make a set of coins
  var coinSet std.Coins = std.Coins{lxyCoin}
  // Check amount of specific coin in set
  coinSet.AmountOf("lxy") // 100
  // AmountOf will return 0 if coin does not exist in set
  coinSet.AmountOf("lyx") // 0
  // Add a coin to a set
  coinSet.Add(lyx)
  // View full coins API on
  // https://docs.gno.land/reference/standard-library/std/coin &
  // https://docs.gno.land/reference/standard-library/std/coins

  // Send coins using Banker
  // Ex: send coins from caller to different address
  from := caller
  to := "g1l9aypkr8xfvs82zeux486ddzec88ty69lue9de"
  // Make sure to use the appropriate banker for your intended purpose
  banker.SendCoins(from, to, coinSet)

  // Get coins owned by specific address
  ownedCoins := banker.GetCoins(caller)
}
```

## Additional resources
For more information, view the following resources:
- [Gno.land home page](https://gno.land)
- [Official Documentation](https://docs.gno.land) 
- [gno-by-example](https://gno-by-example.com/)
- [Gno monorepo](https://github.com/gnolang/gno)

If you're using VS Code for development, check out
the VS Code [Gno plugin](https://marketplace.visualstudio.com/items?itemName=harry-hov.gno).

If you have further questions, feel free to join [gno.land Discord
server](https://discord.com/invite/YFtMjWwUN7) and get support from the Gno team directly.