---
language: Cairo
filename: learnCairo.sol
contributors:
    - ["Darlington Nnam", "https://github.com/Darlington02"]
---

# Cairo

Cairo is a Turing-complete language that allows you write provable programs
(where one party can prove to another that a certain computation was executed
correctly) on StarkNet.

## StarkNet

StarkNet is a decentralized ZK-rollup that operates as an Ethereum layer 2
chain.

In this document, we are going to be going in-depth into understanding Cairo's
syntax and how you could create and deploy a Cairo smart contract on StarkNet.

**NB: As at the time of this writing, StarkNet is still at v0.10.3, with Cairo
1.0 coming soon. The ecosystem is young and evolving very fast, so you might
want to check the [official docs](https://www.cairo-lang.org/docs) to confirm
this document is still up-to-date. Pull requests are welcome!**

## Setting Up A Development Environment

Before we get started writing codes, we will need to setup a Cairo development
environment, for writing, compiling and deploying our contracts to StarkNet.
For the purpose of this tutorial we are going to be using the
[Protostar Framework](https://github.com/software-mansion/protostar).
Installation steps can be found in the docs
[here](https://docs.swmansion.com/protostar/docs/tutorials/installation).
Note that Protostar supports just Mac and Linux OS, Windows users might need to
use WSL, or go for other alternatives such as the Official
[StarkNet CLI](https://www.cairo-lang.org/docs/quickstart.html) or
[Nile from Openzeppelin](https://github.com/OpenZeppelin/nile)

Once you're done with the installations, run the command `protostar -v` to
confirm your installation was successful. If successful, you should see your
Protostar version displayed on the screen.

## Initializing a new project

Protostar similar to Truffle for solidity development can be installed once and
used for multiple projects. To initialize a new Protostar project, run the
following command:

```
protostar init
```

It would then request the project's name and the library's directory name,
you'd need to fill in this, and a new project will be initialized successfully.

## Compiling, Declaring, Deploying and Interacting with StarkNet Contracts

Within the `src` folder you'll find a boilerplate contract that comes with
initializing a new Protostar project, `main.cairo`. We are going to be
compiling, declaring and deploying this contract.

### Compiling Contracts

To compile a Cairo contract using Protostar, ensure a path to the contract is
specified in the `[contracts]` section of the `protostar.toml` file. Once
you've done that, open your terminal and run the command:

```
protostar build
```

And you should get an output similar to what you see below, with a `main.json`
and `main_abi.json` files created in the `build` folder.
<img src="./images/cairo/build.png" alt="building your contract">

### Declaring Contracts

With the recent StarkNet update to 0.10.3, the DEPLOY transaction was
deprecated and no longer works. To deploy a transaction, you must first declare
a Contract to obtain the class hash, then deploy the declared contract using the
[Universal Deployer Contract](https://community.starknet.io/t/universal-deployer-contract-proposal/1864).

Before declaring or deploying your contract using Protostar, you should set the
private key associated with the specified account address in a file, or in the
terminal. To set your private key in the terminal, run the command:

```
export PROTOSTAR_ACCOUNT_PRIVATE_KEY=[YOUR PRIVATE KEY HERE]
```

Then to declare our contract using Protostar run the following command (for
visual clarity, the backslash sign symbolizes the continuing line):

```
protostar declare ./build/main.json \
  --network testnet \
  --account 0x0691622bBFD29e835bA4004e7425A4e9630840EbD11c5269DE51C16774585b16 \
  --max-fee auto
```

where `network` specifies the network we are deploying to, `account` specifies
account whose private key we are using, `max-fee` specifies the maximum fee to
be paid for the transaction. You should get the class hash outputted as seen
below:
<img src="./images/cairo/declare.png" alt="declaring your contract">

### Deploying Contracts

After obtaining our class hash from declaring, we can now deploy using the
command below:

```
protostar \
  deploy 0x02a5de1b145e18dfeb31c7cd7ff403714ededf5f3fdf75f8b0ac96f2017541bc \
  --network testnet \
  --account 0x0691622bBFD29e835bA4004e7425A4e9630840EbD11c5269DE51C16774585b16 \
  --max-fee auto
```

where `0x02a5de1b145e18dfeb31c7cd7ff403714ededf5f3fdf75f8b0ac96f2017541bc` is
the class hash of our contract.
<img src="./images/cairo/deploy.png" alt="deploying your contract">

### Interacting with Contracts

To interact with your deployed contract, we will be using `Argent X`
(alternative: `Braavos`), and `Starkscan` (alternative: `Voyager`). To install
and setup `Argent X`, see this
[guide](https://www.argent.xyz/learn/how-to-create-an-argent-x-wallet/).

Copy your contract address, displayed on screen from the previous step, and
head over to [Starkscan](https://testnet.starkscan.co/) to search for the
contract. Once found, you can make write calls to the contract in the following
sequence:

+ click on the "connect wallet" button,
  <img src="./images/cairo/connect.png" alt="connect wallet">
+ select `Argent X` and approve the connection
  <img src="./images/cairo/connect2.png" alt="connect to argentX">
+ you can now make read and write calls easily.

## Let's learn Cairo

First let's look at a default contract that comes with Protostar which allows
you to set balance on deployment, increase, and get the balance.

```cairo
// Language directive - instructs compiler its a StarkNet contract
%lang starknet

// Library imports from the Cairo-lang library
from starkware.cairo.common.math import assert_nn
from starkware.cairo.common.cairo_builtins import HashBuiltin

// @dev Storage variable that stores the balance of a user.
// @storage_var is a decorator that instructs the compiler the function
//   below it is a storage variable.
@storage_var
func balance() -> (res: felt) {}

// @dev Constructor writes the balance variable to 0 on deployment
// Constructors sets storage variables on deployment. Can accept arguments too.
@constructor
func constructor{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}() {
  balance.write(0); 
  return();
}

// @dev increase_balance updates the balance variable
// @param amount the amount you want to add to balance
// @external is a decorator that specifies the func below it is an external
//   function.
@external
func increase_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}(amount: felt){
  with_attr error_message("Amount must be positive. Got: {amount}.") {
    assert_nn(amount);
  }

  let (res) = balance.read();
  balance.write(res + amount);
  return ();
}

// @dev returns the balance variable
// @view is a decorator that specifies the func below it is a view function.
@view
func get_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}() -> (res: felt) {
  let (res) = balance.read();
  return (res,);
}
```

Before proceeding to the main lessons, try to build, deploy and interact with
this contract.
NB: You should be at `main.cairo` if you are using Protostar.

### 1. The Felt data type

Unlike solidity, where you have access to various data types, Cairo comes with
just a single data type `..felts`. Felts stands for Field elements, and are a
252 bit integer in the range `0<=x<=P` where `P` is a prime number. You can
create a `Uint256` in Cairo by utlizing a struct of two 128 bits felts.

```cairo
struct Uint256 {
  low: felt, // The low 128 bits of the value.
  high: felt, // The high 128 bits of the value.
}
```

To avoid running into issues with divisions, it's safer to work with the
`unsigned_div_rem` method from Cairo-lang's library.

### 2. Lang Directive and Imports

To get started with writing a StarkNet contract, you must specify the directive:

```cairo
%lang starknet
```

This directive informs the compiler you are writing a contract and not a
program. The difference between both is contracts have access to StarkNet's
storage, programs don't and as such are stateless.

There are important functions you might need to import from the official
Cairo-lang library or Openzeppelin's, e.g.

```cairo
from starkware.cairo.common.cairo_builtins import HashBuiltin
from cairo_contracts.src.openzeppelin.token.erc20.library import ERC20
from starkware.cairo.common.uint256 import Uint256
from starkware.cairo.common.bool import TRUE
```

### 3. Data Structures

+ Storage variables: Cairo's storage is a map with `2^251` slots, where each
  slot is a felt which is initialized to `0`. You create one using the
  `@storage_var` decorator.

  ```cairo
  @storage_var
  func names() -> (name: felt) {}
  ```

+ Storage mappings: Unlike Solidity where mappings have a separate keyword, in
  Cairo you create mappings using storage variables.

  ```cairo
  @storage_var
  func names(address: felt) -> (name: felt) {}
  ```

+ Structs: are a means to create custom data types in Cairo. A `struct` has a
  size, which is the sum of the sizes of its members. The size can be
  retrieved using `MyStruct.SIZE`. You create a struct in Cairo using the
  `struct` keyword.

  ```cairo
  struct Person {
    name: felt,
    age: felt,
    address: felt,
  }
  ```

+ Constants: Constants are fixed and as such can't be altered after being set.
  They evaluate to an integer (field element) at compile time. To create a
  constant in Cairo, you use the `const` keyword. It's proper practice to
  capitalize constant names.

  ```cairo
  const USER = 0x01C6cfC1DB2ae90dACEA243F0a8C2F4e32560F7cDD398e4dA2Cc56B733774E9b
  ```

+ Arrays: Arrays can be defined as a `pointer(felt*)` to the first element of
  the array. As an array is populated, its elements take up contigous memory
  cells. The `alloc` keyword can be used to dynamically allocate a new memory
  segment, which can be used to store an array:

  ```cairo
  let (myArray: felt*) = alloc ();
  assert myArray[0] = 1;
  assert myArray[1] = 2;
  assert myArray[3] = 3;
  ```

  You can also use the `new` operator to create fixed-size arrays using
  tuples. The new operator is useful as it enables you allocate memory and
  initialize the object in one instruction

  ```cairo
  func foo() {
    tempvar arr: felt* = new (1, 1, 2, 3, 5);
    assert arr[4] = 5;
    return ();
  }
  ```

+ Tuples: A tuple is a finite, ordered, unchangeable list of elements. It is
  represented as a comma-separated list of elements enclosed by parentheses.
  Their elements may be of any combination of valid types.

  ```cairo
  local tuple0: (felt, felt, felt) = (7, 9, 13);
  ```

+ Events: Events allows a contract emit information during the course of its
  execution, that can be used outside of StarkNet. An event can be created,
  subsequently emitted:

  ```cairo
  @event
  func name_stored(address, name) {}

  name_stored.emit(address, name);
  ```

### 4. Constructors, External and View functions

+ Constructors: Constructors are a way to intialize state variables on
  contract deployment. You create a constructor using the `@constructor`
  decorator.

  ```cairo
  @constructor
  func constructor{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
  range_check_ptr}(_name: felt) {
    let (caller) = get_caller_address();
    names.write(caller, _name);
    return ();
  }
  ```

+ External functions: External functions are functions that modifies the state
  of the network. You create an external function using the `@external`
  decorator:

  ```cairo
  @external
  func store_name{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
  range_check_ptr}(_name: felt){
    let (caller) = get_caller_address();
    names.write(caller, _name);
    stored_name.emit(caller, _name);
    return ();
  }
  ```

+ View functions: View functions do not modify the state of the blockchain.
  You can create a view function using the `@view` decorator.

  ```cairo
  @view
  func get_name{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
  range_check_ptr}(_address: felt) -> (name: felt){
    let (name) = names.read(_address);
    return (name,);
  }
  ```

    NB: Unlike Solidity, Cairo supports just External and View function types.
    You can alternatively also create an internal function by not adding any
    decorator to the function.

### 5. Decorators

All functions in Cairo are specified by the `func` keyword, which can be
confusing. Decorators are used by the compiler to distinguish between these
functions.

Here are the most common decorators you'll encounter in Cairo:

+ `@storage_var` — used for specifying state variables.
+ `@constructor` — used for specifying constructors.
+ `@external` — used for specifying functions that write to a state variable.
+ `@event` — used for specifying events
+ `@view` — used to specify functions reading from a state variable
+ `@contract_interface` — used for specifying function interfaces.
+ `@l1_handler` — used for specifying functions that processes message sent from
  an L1 contract in a messaging bridge.

### 6. BUILTINS, HINTS & IMPLICIT Arguments

+ `BUILTINS` are predefined optimized low-level execution units, which are
  added to Cairo’s CPU board. They help perform predefined computations like
  pedersen hashing, bitwise operations etc, which are expensive to perform in
  Vanilla Cairo. Each builtin in Cairo is assigned a separate memory location,
  accessible through regular Cairo memory calls using implicit parameters. You
  specify them using the `%builtins` directive

  Here is a list of available builtins in Cairo:

    + `output` — the output builtin is used for writing program outputs
    + `pedersen` — the pedersen builtin is used for pedersen hashing
      computations
    + `range_check` — This builtin is mostly used for integer comparisons,
      and facilitates check to confirm that a field element is within a range
      `[0, 2^128)`
    + `ecdsa` — the ecdsa builtin is used for verifying ECDSA signatures
    + `bitwise` — the bitwise builtin is used for carrying out bitwise
      operations on felts

+ `HINTS` are pieces of Python codes, which contains instructions that only
  the prover sees and executes. From the point of view of the verifier these
  hints do not exist. To specify a hint in Cairo, you need to encapsulate it
  within `%{` and `%}`. It is good practice to avoid using hints as much as
  you can in your contracts, as hints are not added to the bytecode, and thus
  do not count in the total number of execution steps.

  ```cairo
  %{
    # Python hint goes here
  %}
  ```

+ `IMPLICIT ARGUMENTS` are not restricted to the function body, but can be
  inherited by other functions calls that require them. Implicit arguments are
  passed in between curly bracelets, like you can see below:

  ```cairo
  func store_name{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
  range_check_ptr}(_name: felt){
    let (caller) = get_caller_address();
    names.write(caller, _name);
    stored_name.emit(caller, _name);
    return ();
  }
  ```

### 7. Error Messages and Access Controls

You can create custom errors in Cairo which is outputted to the user upon failed
execution. This can be very useful for implementing checks and proper access
control mechanisms. An example is preventing a user to call a function except
user is `admin`.

```cairo
// imports
from starkware.starknet.common.syscalls import get_caller_address

// create an admin constant
const ADMIN = 0x01C6cfC1DB2ae90dACEA243F0a8C2F4e32560F7cDD398e4dA2Cc56B733774E9b

// implement access control
with_attr error_message("You do not have access to make this action!"){
  let (caller) = get_caller_address();
  assert ADMIN = caller;
}

// using an assert statement throws if condition is not true, thus
// returning the specified error.
```

### 8. Contract Interfaces

Contract interfaces provide a means for one contract to invoke or call the
external function of another contract. To create a contract interface, you use
the `@contract_interface` keyword:

```cairo
@contract_interface
  namespace IENS {
    func store_name(_name: felt) {
    }

    func get_name(_address: felt) -> (name: felt) {
    }
  }
```

Once a contract interface is specified, any contract can make calls to that
contract passing in the contract address as the first parameter like this:

```cairo
IENS.store_name(contract_address, _name);
```

Note that Interfaces exclude the function body/logic and the implicit
arguments.

### 9. Recursions

Due to the unavailability of loops, Recursion is the go-to for similar
operations. In simple terms, a recursive function is one which calls itself
repeatedly.

A good example to demonstrate this is writing a function for getting the nth
fibonacci number:

```cairo
@external
func fibonacci{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}(n : felt) -> (result : felt){
  alloc_locals;
  if (n == 0){
    return (0);
  }
  if (n == 1){
    return (1);
  }
  let (local x) = fibonacci(n - 1);
  let (local y) = fibonacci(n - 2);
  return (result=(x + y));
}
```

The nth fibonacci term is the sum of the `nth - 1` and the `nth - 2` numbers,
that's why we get these two as `(x,y)` using recursion.

NB: when implementing recursive functions, always remember to implement a base
case (`n==0`, `n==1` in our case), to prevent stack overflows.

### 10. Registers

Registers holds values that may change over time. There are 3 major types of
registers:

+ `ap` (allocation pointer) points to a yet unused memory. Temporary variables
   created using `let`, `tempvar` are held here, and thus susceptible to being
   revoked.
+ `fp` (frame pointer) points to the frame of the current function. The address
  of all the function arguments and local variables are relative to this
  register and as such can never be revoked.
+ `pc` (program counter) points to the current instruction.

### 11. Revoked References

Revoked references occur when there is a call instruction to another function,
between the definition of a reference variable that depends on `ap` (temp
variables) and its usage. This occurs as the compiler may not be able to compute
the change of `ap` (as one may jump to the label from another place in the
program, or call a function that might change ap in an unknown way).

Here is an example to demonstrate what I mean:

```cairo
@external
func get_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}() -> (res: felt) {
  return (res=100);
}

@external
func double_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}() -> (res: felt) {
  let multiplier = 2;
  let (balance) = get_balance();
  let new_balance = balance * multiplier;
  return (res=new_balance);
}
```

If you run that code, you'll run into the revoked reference error as we are
trying to access the `multiplier` variable after calling the `get_balance`
function.

In simple cases you can resolve revoked references by adding the keyword
`alloc_locals` within function scopes. In more complex cases you might need to
create a local variable to resolve it.

```cairo
// resolving the `double_balance` function:
@external
func double_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}() -> (res: felt) {
  alloc_locals;
  let multiplier = 2;
  let (balance) = get_balance();
  let new_balance = balance * multiplier;
  return (res=new_balance);
}
```

### 12. Understanding Cairo's Punctuations

+ `;` (semicolon). Used at the end of each instruction
+ `()` (parentheses). Used in a function declaration, if statements, and in a
  tuple declaration
+ `{}` (curly braces). Used in a declaration of implicit arguments and to define
  code blocks.
+ `[]` (square brackets). Standalone brackets represent the value at a
  particular address location (such as the allocation pointer, `[ap]`). Brackets
  following a pointer or a tuple act as a subscript operator, where `x[2]`
  represents the element with index `2` in `x`.
+ `*` (single asterisk). Refers to the pointer of an expression.
+ `%` (percent sign). Appears at the start of a directive, such as `%builtins`
  or `%lang`.
+ `%{` and `%}` represent Python hints.
+ `_` (underscore). A placeholder to handle values that are not used, such as an
  unused function return value.

## Full Contract Example

Below is a simple automated market maker contract example that implements most
of what we just learnt! Re-write, deploy, have fun!

```cairo
%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.hash import hash2
from starkware.cairo.common.alloc import alloc
from starkware.cairo.common.math import (assert_le, assert_nn_le,
  unsigned_div_rem)
from starkware.starknet.common.syscalls import (get_caller_address,
  storage_read, storage_write)


// CONSTANTS
//
// @dev the maximum amount of each token that belongs to the AMM
const BALANCE_UPPER_BOUND = 2 ** 64;

const TOKEN_TYPE_A = 1;
const TOKEN_TYPE_B = 2;

// @dev Ensure the user's balances are much smaller than the pool's balance
const POOL_UPPER_BOUND = 2 ** 30;
const ACCOUNT_BALANCE_BOUND = 1073741; // (2 ** 30 / 1000)


// STORAGE VARIABLES
//
// @dev A map from account and token type to corresponding balance
@storage_var
func account_balance(account_id: felt, token_type: felt) -> (balance: felt) {}

// @dev a map from token type to corresponding pool balance
@storage_var
func pool_balance(token_type: felt) -> (balance: felt) {}


// GETTERS
//
// @dev returns account balance for a given token
// @param account_id Account to be queried
// @param token_type Token to be queried
@view
func get_account_token_balance{syscall_ptr: felt*, pedersen_ptr:
HashBuiltin*, range_check_ptr}(
  account_id: felt, token_type: felt
  ) -> (balance: felt) {
  return account_balance.read(account_id, token_type);
}

// @dev return the pool's balance
// @param token_type Token type to get pool balance
@view
func get_pool_token_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}(
  token_type: felt
  ) -> (balance: felt) {
  return pool_balance.read(token_type);
}


// EXTERNALS
//
// @dev set pool balance for a given token
// @param token_type Token whose balance is to be set
// @param balance Amount to be set as balance
@external
func set_pool_token_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}(
  token_type: felt, balance: felt
  ) {
  with_attr error_message("exceeds maximum allowed tokens!"){
    assert_nn_le(balance, BALANCE_UPPER_BOUND - 1);
  }

  pool_balance.write(token_type, balance);
  return ();
}

// @dev add demo token to the given account
// @param token_a_amount amount of token a to be added
// @param token_b_amount amount of token b to be added
@external
func add_demo_token{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}(
    token_a_amount: felt, token_b_amount: felt
  ) {
  alloc_locals;
  let (account_id) = get_caller_address();

  modify_account_balance(account_id=account_id, token_type=TOKEN_TYPE_A,
    amount=token_a_amount);
  modify_account_balance(account_id=account_id, token_type=TOKEN_TYPE_B,
    amount=token_b_amount);

  return ();
}

// @dev intialize AMM
// @param token_a amount of token a to be set in pool
// @param token_b amount of token b to be set in pool
@external
func init_pool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}(
  token_a: felt, token_b: felt
  ) {
  with_attr error_message("exceeds maximum allowed tokens!"){
    assert_nn_le(token_a, POOL_UPPER_BOUND - 1);
    assert_nn_le(token_b, POOL_UPPER_BOUND - 1);
  }

  set_pool_token_balance(token_type=TOKEN_TYPE_A, balance=token_a);
  set_pool_token_balance(token_type=TOKEN_TYPE_B, balance=token_b);

  return ();
}


// @dev swaps token between the given account and the pool
// @param token_from token to be swapped
// @param amount_from amount of token to be swapped
// @return amount_to the token swapped to
@external
func swap{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
  token_from: felt, amount_from: felt
  ) -> (amount_to: felt) {
  alloc_locals;
  let (account_id) = get_caller_address();

  // verify token_from is TOKEN_TYPE_A or TOKEN_TYPE_B
  with_attr error_message("token not allowed in pool!"){
    assert (token_from - TOKEN_TYPE_A) * (token_from - TOKEN_TYPE_B) = 0;
    }

  // check requested amount_from is valid
  with_attr error_message("exceeds maximum allowed tokens!"){
    assert_nn_le(amount_from, BALANCE_UPPER_BOUND - 1);
    }

  // check user has enough funds
  let (account_from_balance) =
    get_account_token_balance(account_id=account_id, token_type=token_from);
  with_attr error_message("insufficient balance!"){
    assert_le(amount_from, account_from_balance);
    }

  let (token_to) = get_opposite_token(token_type=token_from);
  let (amount_to) = do_swap(account_id=account_id, token_from=token_from,
    token_to=token_to, amount_from=amount_from);

  return (amount_to=amount_to);
}


// INTERNALS
//
// @dev internal function that updates account balance for a given token
// @param account_id Account whose balance is to be modified
// @param token_type Token type to be modified
// @param amount Amount Amount to be added
func modify_account_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}(
  account_id: felt, token_type: felt, amount: felt
  ) {
  let (current_balance) = account_balance.read(account_id, token_type);
  tempvar new_balance = current_balance + amount;

  with_attr error_message("exceeds maximum allowed tokens!"){
    assert_nn_le(new_balance, BALANCE_UPPER_BOUND - 1);
    }

  account_balance.write(account_id=account_id, token_type=token_type,
    value=new_balance);
  return ();
}

// @dev internal function that swaps tokens between the given account and
// the pool
// @param account_id Account whose tokens are to be swapped
// @param token_from Token type to be swapped from
// @param token_to Token type to be swapped to
// @param amount_from Amount to be swapped
func do_swap{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*,
range_check_ptr}(
  account_id: felt, token_from: felt, token_to: felt, amount_from: felt
  ) -> (amount_to: felt) {
  alloc_locals;

  // get pool balance
  let (local amm_from_balance) = get_pool_token_balance(token_type =
    token_from);
  let (local amm_to_balance) = get_pool_token_balance(token_type=token_to);

  // calculate swap amount
  let (local amount_to, _) = unsigned_div_rem((amm_to_balance *
    amount_from), (amm_from_balance + amount_from));

  // update token_from balances
  modify_account_balance(account_id=account_id, token_type=token_from,
    amount=-amount_from);
  set_pool_token_balance(token_type=token_from, balance=(amm_from_balance
    + amount_from));

  // update token_to balances
  modify_account_balance(account_id=account_id, token_type=token_to,
    amount=amount_to);
  set_pool_token_balance(token_type=token_to, balance=(amm_to_balance -
    amount_to));

  return (amount_to=amount_to);
}


// @dev internal function to get the opposite token type
// @param token_type Token whose opposite pair needs to be gotten
func get_opposite_token(token_type: felt) -> (t: felt) {
  if(token_type == TOKEN_TYPE_A) {
    return (t=TOKEN_TYPE_B);
  } else {
    return (t=TOKEN_TYPE_A);
  }
}
```

## Additional Resources

+ [Official documentation](https://www.cairo-lang.org/docs/)
+ [Starknet EDU](https://medium.com/starknet-edu)
+ [Journey through Cairo](https://medium.com/@darlingtonnnam/journey-through-cairo-i-setting-up-protostar-and-argentx-for-local-development-ba40ae6c5524)
+ [Demystifying Cairo whitepaper](https://medium.com/@pban/demystifying-cairo-white-paper-part-i-b71976ad0108)
+ [Learn about StarkNet with Argent](https://www.argent.xyz/learn/tag/starknet/)

## Development Frameworks

+ [Protostar](https://docs.swmansion.com/protostar/docs/tutorials/installation)
+ [Nile](https://github.com/OpenZeppelin/nile)
+ [StarkNet CLI](https://www.cairo-lang.org/docs/quickstart.html)

## Helpful Libraries

+ [Cairo-lang](https://github.com/starkware-libs/cairo-lang)
+ [Openzeppelin](https://github.com/OpenZeppelin/cairo-contracts)

## Educational Repos

+ [StarkNet Cairo 101](https://github.com/starknet-edu/starknet-cairo-101)
+ [StarkNet ERC721](https://github.com/starknet-edu/starknet-erc721)
+ [StarkNet ERC20](https://github.com/starknet-edu/starknet-erc20)
+ [L1 -> L2 Messaging](https://github.com/starknet-edu/starknet-messaging-bridge)
+ [StarkNet Debug](https://github.com/starknet-edu/starknet-debug)
+ [StarkNet Accounts](https://github.com/starknet-edu/starknet-accounts)
+ [Min-Starknet](https://github.com/Darlington02/min-starknet)

## Security

+ [Amarna static analysis for Cairo programs](https://blog.trailofbits.com/2022/04/20/amarna-static-analysis-for-cairo-programs/)
+ [Cairo and StarkNet security by Ctrl03](https://ctrlc03.github.io/)
+ [How to hack almost any Cairo smart contract](https://medium.com/ginger-security/how-to-hack-almost-any-starknet-cairo-smart-contract-67b4681ac0f6)
+ [Analyzing Cairo code using Armana](https://dic0de.substack.com/p/analyzing-cairo-code-using-amarna?sd=pf)

## Future TO-DOs

Update tutorial to fit Cairo 1.0
