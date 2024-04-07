---
language: Vyper
filename: learnVyper.vy
contributors:
  - ["Kenny Peluso", "kennyp.herokuapp.com"]
---

> The content of this document is largely inspired by ["Learn Solidity in Y Minutes"](https:#github.com/adambard/learnxinyminutes-docs/blob/master/solidity.html.markdown)

Vyper lets you program on [Ethereum](https:#www.ethereum.org/), a
blockchain-based virtual machine that allows the creation and
execution of smart contracts, without requiring centralized or trusted parties. It was
designed to improve upon Solidity, another smart contract language for Ethereum, by
limiting unsafe practices and enhancing readability; Vyper seeks to optimize the
security and auditability of smart contracts.

Vyper is an experimental, statically typed, contract programming language meant to
resemble Python. Like objects in OOP, each contract contains state variables, functions,
and common data types. Contract-specific features include event notifiers for listeners,
and custom global variables, global constants.

Some Ethereum contract examples include crowdfunding, voting, and blind auctions.

---

## Table of Contents

- Intro
- Example
1. Data types and associated methods
2. Data structures
3. Simple operators
4. Global variables of note
5. Functions and more
    a. functions
    b. events
6. Branching and loops
7. Objects/contracts
    a. calling external contracts
    b. ERC20 built-in
    c. following an interface
8. Other keywords
    a. selfdestruct
9. Contract design notes
    a. obfuscation
    b. storage optimization
    c. data access in blockchain
    d. cron job
    e. observer pattern
10. Security
11. Style notes
12. Natspec comments
- Other documents

---

## Intro

From [the docs](https://media.readthedocs.org/pdf/vyper/latest/vyper.pdf)
the foundational tenants of Vyper are:

1. *Security*
2. *Language and compiler simplicity*
3. *Auditability*

This allows for the following features:

1. *Bounds and overflow checking*
   - On the arithmetic and array level
   - There are no dynamic arrays in Vyper
2. *Support for signed integers and decimal fixed point numbers*
3. *Decidability* - You can always compute precise upper bound on gas cost
4. *Strong typing* - for built-in and custom types
5. *Small and understandable compiler code*
6. *Limited support for pure functions*
    - Anything marked `@constant` is not allowed to change the state

Following the principles and goals, Vyper does not provide the following features:

1. *Modifiers* (defining parts of functions elsewhere)
2. *Class inheritance*
3. *Inline assembly*
4. *Function overloading*
5. *Operator overloading*
6. *Recursive calling*
7. *Infinite-length loops*
8. *Binary fixed point* (decimal fixed point is used for its exactness)

WITH THE RAPID CHANGES IN ETHEREUM, THIS DOCUMENT IS UNLIKELY TO STAY UP TO
DATE, SO YOU SHOULD FOLLOW THE LATEST VYPER DOCS AND ETHEREUM BLOG FOR THE LATEST.
ALL CODE HERE IS PROVIDED AS IS, WITH SUBSTANTIAL RISK OF ERRORS OR DEPRECATED CODE
PATTERNS.

This document primarily discusses syntax, and so excludes many
popular design patterns.

As Vyper and Ethereum are under active development, experimental or beta
features are typically marked, and subject to change. Pull requests welcome.

This document describes Vyper version `0.1.0-beta.8`.

*All of the following code exists for educational purposes only!*
*None of the following code should be used in production as-is!*

## Example

```python
# First, a simple todo list contract
# Implements CRUD operations for tasks

# todo.vy (note .vy extension)
### **** START EXAMPLE **** ###

# Start with Natspec comment
# used for documentation

# @title SimpleBank v1
# @author kennyp
# @notice This is a simple bank.

# Vyper contracts must obey a particular order:
#   struct -> interface -> events -> globals and constants -> functions
# Additionally, like Python, Vyper functions must be defined in the file
#    before they're called.

# Structs

struct Task:
    done: bool
    deleted: bool
    task: string[100]
    metadata: bytes32

# Interfaces

contract AnotherContract():
    def fetch() -> bytes32: constant
    def inform(_taskId: uint256, _status: uint256) -> bool: modifying

# Events

# Events - publicize actions to external listeners
# `indexed` means that it's easier to search/filter on this field
TaskStatus: event({_taskId: indexed(uint256), _status: uint256})

# Global Variables

# State variables are values which are permanently stored in contract storage
# State vars consist of any value persisting beyond any function's scope
#   and are permanently stored in contract storage

# You can define your own, custom, unmutable constants
CREATED: constant(uint256) = 0
COMPLETED: constant(uint256) = 1
DELETED: constant(uint256) = 2

# The `public` built-in allows for this address to be read externally
#   without defining a `get()` constant function
owner: public(address)
other: public(address)

# uint256 means "unsigned positive integer between 0 and 2^256 - 1"
# Overflow protection is built-in to Vyper
taskCount: uint256
tasks: map(uint256, Task) # dictionary: key=uint256, value: Task struct

# Private Functions

# Start each function with Pythonic decorators
# These decorators resemble Natspec but are actually enforced by Vyper's compiler
# These decorators are:
# @public XOR @private (either one or the other)
#   @public (if any contract/user can call it)
#   @private (if only internal functions can call it)
# @payable (if the function is payable i.e. accepting ETH)
# @constant (if the function is not modifying anything on-chain)
@private
def _changeTaskStatus( \
        _sender: address, \
        _taskId: uint256, \
        _status: uint256, \
    ):
    # backslashes (\) allow for multi-line code
    # Natspec comments are particularly helpful for documentation and readability
    # Natspec can be included using familiar Pythonic docstring syntax
    """
    @notice
    @dev `_sender` MUST be `self.owner`
    @param _sender Who is triggering this function
    @param _task The description of the task (only useful when task added)
    """
    # NOTE: Private functions do not have access to `msg.sender`
    # SIDE NOTE: `msg.sender` refers to whoever immediately called the function of
    #   immediate scope. In other words, if I call a function that calls another
    #   in-contract, public function, then `msg.sender` turns from my address to
    #   the address of the current contract.
    assert _sender == self.owner # failed assertions cause calls/transactions to fail
    # Note that unlike Solidity, `self.` is required to query the contract's state
    # Control flow is Pythonic, as is much of Vyper:
    _task: string[100] # initialized to default value
    _data: bytes32 = sha3(convert(_sender, bytes32)) # owner is obfuscated (but still visible in logs)
    if _status == CREATED: # control flow mimics python
        # How a new struct is instantiated:
        self.tasks[_taskId] = Task({ \
            done: False, deleted: False, task: _task, metadata: _data \
        })
    elif _status == COMPLETED:
        # Modifying an existing struct:
        self.tasks[_taskId].done = True
    elif _status == DELETED:
        self.tasks[_taskId].deleted = True
    AnotherContract(self.other).inform(_taskId, _status) # modifying external call
    log.TaskStatus(_taskId, _status) # emit an event

# Public Functions

# Pythonic constructor - can receive none or many arguments
@public
def __init__(_owner: address, _other_contract: address):
    """
    @dev Called once and only upon contract depoyment
    """
    self.owner = _owner
    self.other = _other_contract

# NOTE: Pythonic whitespace rules are mandated in Vyper

@public
def addTask(_task: string[100]) -> uint256:
    """
    @notice Adds a task to contract
    @param _task Description of task
    @return Id of newly minted task
    """
    # msg.sender gives the address of who/what contract is calling this function
    self._changeTaskStatus(msg.sender, self.taskCount, CREATED)
    self.tasks[self.taskCount].task = _task
    self.taskCount += 1
    return self.taskCount - 1

@public
def addSpecialTask(_task: string[100]) -> uint256:
    """
    @notice Adds a task with metadata pulled from elsewhere
    @param _task Description of task
    @return Id of newly minted task
    """
    self._changeTaskStatus(msg.sender, self.taskCount, CREATED)
    self.tasks[self.taskCount].task = _task
    self.tasks[self.taskCount].metadata = AnotherContract(self.other).fetch()
    self.taskCount += 1
    return self.taskCount - 1

@public
def completeTask(_taskId: uint256):
    """
    @notice Marks a task as "completed"
    @param _taskId Id of task to complete
    """
    self._changeTaskStatus(msg.sender, _taskId, COMPLETED)

@public
def deleteTask(_taskId: uint256):
    """
    @notice Adds a task to contract
    @param _taskId Id of task to delete
    """
    self._changeTaskStatus(msg.sender, _taskId, DELETED)

@public
@constant # allows function to run locally/off blockchain
def getTask(_taskId: uint256) -> string[100]:
    """
    @notice Getter for a task's description
    @param _taskId Id of task with desired description
    @return Description of task
    """
    return self.tasks[_taskId].task

### **** END EXAMPLE **** ###


# Now, the basics of Vyper


# ---


# 1. DATA TYPES AND ASSOCIATED METHODS
# uint256 used for currency amount and for dates (in unix time)
x: uint256

# int of 128 bits, cannot be changed after contract deployment
# with 'constant', compiler replaces each occurrence with actual value
a: constant(int128) = 5

# All state variables (those outside a function)
#   are by default 'internal' and accessible inside contract
# Need to explicitly set to 'public' to allow external contracts to access
#   A getter is automatically created, but NOT a setter
# Can only be called in the contract's scope (not within functions)
# Add 'public' field to indicate publicly/externally accessible
a: public(int128)

# No random functions built in, use other contracts for randomness

# Type casting is limited but exists
b: int128 = 5
x: uint256 = convert(b, uint256)

# Types of accounts:
# Contract Account: f(creator_addr, num_transactions)=address set on contract creation
# External Account: (person/external entity): f(public_key)=address

# Addresses - An address type can hold an Ethereum address which
#   equates to 20 bytes or 160 bits. It returns in hexadecimal notation
#   with a leading 0x. No arithmetic allowed
owner: public(address)

# Members can be invoked on all addresses:
owner.balance # returns balance of address as `wei_value`
owner.codesize # returns code size of address as `int128`
owner.is_contract # `True` if Contract Account

# All addresses can be sent ether via `send()` built-in
@public
@payable
def sendWei(any_addr: address):
    send(any_addr, msg.value)

# Bytes available
a: bytes[2]
b: bytes[32]
c: bytes32
# `b` and `c` are 2 different types

# Bytes are preferable to strings since Vyper currently offers better
#   support for bytes i.e. more built-ins to deal with `bytes32`, `bytes32`
#   can be returned from functions and strings[] can't be, UTF8 (string encoding)
#   uses more storage, etc.

# There are no dynamically sized bytes, similar to how there are no
#   dynamic arrays

# Fixed-size byte arrays (Strings)
a: string[100]
b: string[8]
c: string[108] = concat(a, b) # check the latest docs for more built-ins

# Time
t1: timedelta
t2: timestamp
# Both types are built-in "custom type" variants of `uint256`
# `timedelta` values can be added but not `timestamp` values

# Money
m: wei_value
# Also has the base type `uint256` like `timestamp` and `timedelta`
# 1 unit of WEI (a small amount of ETH i.e. ether)

# Custom types
# specify units used in the contract:
units: {
    cm: "centimeter",
    km: "kilometer"
}
# usage:
a: int128(cm)
b: uint256(km)

# BY DEFAULT: all values are set to 0 on instantiation

# `clear()` can be called on most types
#   Does NOT destroy value, but sets value to 0, the initial value


# ---


# 2. DATA STRUCTURES
# Arrays
bytes32[5] nicknames; # static array
bytes32[] names; # dynamic array
uint newLength = names.push("John"); # adding returns new length of the array
# Length
names.length; # get length
names.length = 1; # lengths can be set (for dynamic arrays in storage only)

# Multidimensional Arrays
# At initialization, array dimensions must be hard-coded or constants
# Initialize a 10-column by 3-row, multidimensional fixed array
ls: (uint256[10])[3] # parentheses are optional
@public
def setToThree():
    # Multidimensional Array Access and Write
    # access indices are reversed
    # set element in row 2 (3rd row) column 5 (6th column) to 3
    self.ls[2][5] = 3

# Dictionaries (any simple type to any other type including structs)
theMap: map(uint256, bytes32)
theMap[5] = sha3("charles")
# theMap[255] result is 0, all non-set key values return zeroes
# To make read public, make a getter that accesses the mapping
@public
def getMap(_idx: uint256) -> bytes32:
    """
    @notice Get the value of `theMap` at `_idx`
    """
    return self.theMap[_idx]

self.getMap(5) # returns sha3("charles") in bytes32

# Nested mappings
aMap: map(address, map(address, uint256))
# NOTE: Mappings are only allowed as state variables
# NOTE: Mappings are not iterable; can only be accessed

# To delete (reset the mapping's value to default at a key)
clear(balances["John"])
clear(balances); # sets all elements to 0

# Unlike other languages, CANNOT iterate through all elements in
#   mapping, without knowing source keys - can build data structure
#   on top to do this

# Structs
struct Struct:
    owner: address
    _balance: uint256 # balance is a reserved keyword, is a member for addresses

exampleStruct: Struct

@public
def foo() -> uint256:
    self.exampleStruct = Struct({owner: msg.sender, _balance: 5})
    self.exampleStruct._balance = 10
    self.exampleStruct._balance = 5 # set to new value
    clear(self.exampleStruct._balance)
    clear(self.exampleStruct)
    return self.exampleStruct._balance


# Data locations: Memory vs. storage vs. calldata - all complex types (arrays,
# structs) have a data location
# 'memory' does not persist, 'storage' does
# Default is 'storage' for local and state variables; 'memory' for func params
# stack holds small local variables

# for most types, can explicitly set which data location to use


# ---


# 3. SIMPLE OPERATORS
# Comparisons, bit operators and arithmetic operators are provided
# exponentiation: **
# modulo: %
# maximum: max(x, y)
# AND: bitwise_and(x, y)
# bitwise shift: shift(x, _shift)
#   where x,y are uint256
#         _shift is int128

# 4. GLOBAL VARIABLES OF NOTE
# ** self **
self # address of contract
# often used at end of contract life to transfer remaining balance to party:
self.balance # balance of current contract
self.someFunction() # calls func externally via call, not via internal jump

# ** msg - Current message received by the contract **
# Ethereum programmers take NOTE: this `msg` object is smaller than elsewhere
msg.sender # address of sender
msg.value # amount of ether provided to this contract in wei, the function should be marked `@payable`
msg.gas # remaining gas

# ** tx - This transaction **
# Ethereum programmers take NOTE: this `tx` object is smaller than elsewhere
tx.origin # address of sender of the transaction

# ** block - Information about current block **
block.timestamp # time at current block (uses Unix time)
# Note that `block.timestamp` can be manipulated by miners, so be careful
block.number # current block number
block.difficulty # current block difficulty

# ** storage - Persistent storage hash **
storage['abc'] = 'def'; # maps 256 bit words to 256 bit words


# ---


# 5. FUNCTIONS AND MORE

# A. FUNCTIONS
# Simple function
function increment(uint x) returns (uint) {
    x += 1;
    return x;
}

# Functions can return many arguments
@public
@constant
def increment(x: uint256, y: uint256) -> (uint256, uint256):
    x += 1
    y += 1
    return  (x, y)

# Call previous function
@public
@constant
def willCall() -> (uint256, uint256):
    return  self.increment(1,1)

# One should never have to call a function / hold any logic outside
#   outside the scope of a function in Vyper

# '@constant'
# indicates that function does not/cannot change persistent vars
# Constant function execute locally, not on blockchain
y: uint256
@public
@constant
def increment(x: uint256) -> uint256:
    x += 1
    y += 1 # this line would fail
    # y is a state variable => can't be changed in a constant function


# 'Function Decorators'
# Used like python decorators but are REQUIRED by Vyper
# @public - visible externally and internally (default for function)
# @private - only visible in the current contract
# @constant - doesn't change state
# @payable - receive ether/ETH
# @nonrentant(<unique_key>) - Function can only be called once, both externally
#                             and internally. Used to prevent reentrancy attacks

# Functions hare not hoisted
# Functions cannot be assigned to a variable
# Functions cannot be recursive

# All functions that receive ether must be marked 'payable'
@public
@payable
def depositEther():
    self.balances[msg.sender] += msg.value


# B. EVENTS
# Events are notify external parties; easy to search and
#   access events from outside blockchain (with lightweight clients)
#   typically declare after contract parameters

# Declare
LogSent: event({_from: indexed(address), address: indexed(_to), _amount: uint256})
# Call
log.LogSent(from, to, amount)

/**
For an external party (a contract or external entity), to watch using
the Web3 Javascript library:

# The following is Javascript code, not Vyper code
Coin.LogSent().watch({}, '', function(error, result) {
    if (!error) {
        console.log("Coin transfer: " + result.args.amount +
            " coins were sent from " + result.args.from +
            " to " + result.args.to + ".");
        console.log("Balances now:\n" +
            "Sender: " + Coin.balances.call(result.args.from) +
            "Receiver: " + Coin.balances.call(result.args.to));
    }
}
**/

# Common paradigm for one contract to depend on another (e.g., a
#   contract that depends on current exchange rate provided by another)


# ---


# 6. BRANCHING AND LOOPS

# All basic logic blocks from Python work - including if/elif/else, for,
#   while, break, continue, return - but no switch

# Syntax same as Python, but no type conversion from non-boolean
# to boolean (comparison operators must be used to get the boolean val)

# REMEMBER: Vyper does not allow resursive calls or infinite loops


# ---


# 7. OBJECTS/CONTRACTS
# REMEMBER: Vyper does not allow for inheritance or imports

# A. CALLING EXTERNAL CONTRACTS
# You must define an interface to an external contract in the current contract

contract InfoFeed():
    def getInfo() -> uint256: constant

info: uint256

@public
def __init__(_source: address):
    self.info = InfoFeed(_source).getInfo()


# B. ERC20 BUILT-IN
# Using the `ERC20` keyword implies that the contract at the address
#   follows the ERC20 token standard, allowing you to safely call
#   functions like `transfer()`, etc.

tokenAddress: address(ERC20)

@public
def transferIt(_to: address, _amt: uint256(wei)):
    self.tokenAddress.transfer(_to, _amt)


# C. FOLLOWING AN INTERFACE
# Vyper is experimenting with using the following syntax at the top of
#   a `.vy` file to specify what interfaces are followed by the contract
# This allows interfaces to be better organized, registered, and recognized

import interfaces.some_interface as SomeInterface
implements: SomeInterface
# <rest of contract>


# ---


# 8. OTHER KEYWORDS

# A. selfdestruct()
# selfdestruct current contract, sending funds to address (often creator)
selfdestruct(SOME_ADDRESS);

# removes storage/code from current/future blocks
# helps thin clients, but previous data persists in blockchain

# Common pattern, lets owner end the contract and receive remaining funds
@public
def endItAll() {
    assert msg.sender == self.creator # Only let the contract creator do this
    selfdestruct(self.creator) # Makes contract inactive, returns funds

# May want to deactivate contract manually, rather than selfdestruct
#   (ether sent to selfdestructed contract is lost)


# B. sha3()
# Encrypts strings and other data
# Very important on the blockchain
# Takes 1 argument, `concat()` can be called beforehand
# All strings passed are concatenated before hash action
sha3(concat("ab", "cd")) # returns bytes32


# ---


# 9. CONTRACT DESIGN NOTES

# A. Obfuscation
# All variables are publicly viewable on blockchain, so anything
#   that is private needs to be obfuscated (e.g., hashed w/secret)
# Oftentimes, a "commit-reveal" scheme is employed

# Step 1. Commit
# Place a commitment by sending output of `sha3()`
sha3("a secret"); # bytes32 commit
sha3(concat("secret", "other secret", "salt")); # commit multiple things
# The `sha3()` calculation should occur off-chain, only the bytes32
#   output should be inputted into some `commit()` function
commits: map(address, bytes32)
@public
def commit(commitment: bytes32):
    self.commits[msg.sender] = commitment

# Step 2. Reveal
# Send your previously committed data so the contract can check
#   if your commitment was honest
@public
def reveal(_secret: string[100], _salt: string[100]) -> bool:
    return sha3(concat(_secret, _salt)) == self.commits[msg.sender]


# B. Storage optimization
# Writing to blockchain can be expensive, as data stored forever; encourages
#   smart ways to use memory (eventually, compilation will be better, but for now
#   benefits to planning data structures - and storing min amount in blockchain)

# Cost can often be high for items like multidimensional arrays
#   (cost is for storing data - not declaring unfilled variables)


# C. Data access in blockchain
# Cannot restrict human or computer from reading contents of
#   transaction or transaction's state

# While 'private' prevents other *contracts* from reading data
#   directly - any other party can still read data in blockchain

# All data to start of time is stored in blockchain, so
#   anyone can observe all previous data and changes


# D. Cron Job
# Contracts must be manually called to handle time-based scheduling;
#   can create external code to regularly ping or provide incentives
#   (ether) for others to ping


# E. Observer Pattern
# An Observer Pattern lets you register as a subscriber and
#   register a function which is called by the oracle (note, the oracle
#   pays for this action to be run)
# Some similarities to subscription in Pub/sub

# This is an abstract contract, both client and server classes import,
#   the client should implement

### **** START EXAMPLE **** ###

contract SomeOracleCallback():
    def oracleCallback(_value: uint256, _time: timestamp, _info: bytes32): modifying

MAX_SUBS: constant(uint256) = 100
numSubs: public(uint256) # number of subscribers
subs: map(uint256, address) # enumerates subscribers

@public
def addSub(_sub: address) -> uint256:
    """
    @notice Add subscriber
    @param _sub Address to add
    @return Id of newly added subscriber
    """
    self.subs[self.numSubs] = _sub
    self.numSubs += 1
    return self.numSubs - 1

@private
def notify(_value: uint256, _time: timestamp, _info: bytes32) -> bool:
    """
    @notice Notify all subscribers
    @dev Check `numSubs` first; Watch out for gas costs!
    @param _value whatever
    @param _time what have you
    @param _info what else
    @return True upon successful completion
    """
    j: uint256
    for i in range(MAX_SUBS):
        j = convert(i, uint256) # `i` is int128 by default
        if j == self.numSubs:
            return True
        SomeOracleCallback(self.subs[j]).oracleCallback(_value, _time, _info)

@public
def doSomething():
    """
    @notice Do something and notify subscribers
    """
    # ...something...
    whatever: uint256 = 6
    what_have_you: timestamp
    what_else: bytes32 = sha3("6")
    self.notify(whatever, what_have_you, what_else)

# Now, your client contract can addSubscriber by importing SomeOracleCallback
# and registering with Some Oracle

### **** END EXAMPLE **** ###


# ---


# 10. SECURITY
# Bugs can be disastrous in Ethereum contracts - and even popular patterns in
#   Vyper may be found to be antipatterns

# See security links at the end of this doc


# ---


# 11. STYLE NOTES
# Based on Python's PEP8 style guide
# Full Style guide: http:#solidity.readthedocs.io/en/develop/style-guide.html

# Quick summary:
# 4 spaces for indentation
# Two lines separate contract declarations (and other top level declarations)
# Avoid extraneous spaces in parentheses
# Can omit curly braces for one line statement (if, for, etc)
# else should be placed on own line

# Specific to Vyper:
# arguments: snake_case
# events, interfaces, structs: PascalCase
# public functions: camelCase
# private functions: _prefaceWithUnderscore


# ---


# 12. NATSPEC COMMENTS
# used for documentation, commenting, and external UIs

# Contract natspec - always above contract definition
# @title Contract title
# @author Author name

# Function natspec
# Should include in docstring of functions in typical Pythonic fashion
# @notice Information about what function does; shown when function to execute
# @dev Function documentation for developer

# Function parameter/return value natspec
# @param someParam Some description of what the param does
# @return Description of the return value
```

## Additional resources
- [Installation](https://vyper.readthedocs.io/en/latest/installing-vyper.html)
- [Vyper Docs](https://media.readthedocs.org/pdf/vyper/latest/vyper.pdf)
- [Vyper GitHub (under active dev)](https://github.com/ethereum/vyper)
- [Tools and Resources](https://github.com/ethereum/vyper/wiki/Vyper-tools-and-resources)
- [Online Compiler](https://vyper.online/)

## Sample contracts
- [Uniswap](https://github.com/Uniswap/contracts-vyper)
- [Generalized Governance](https://github.com/kpeluso/gdg)
- [Dynamic Arrays](https://github.com/kpeluso/vyper-dynamic-array)

## Security
Vyper is secure by design, but it may be helpful to understand what Vyper is
protecting you from.
- [Thinking About Smart Contract Security](https:#blog.ethereum.org/2016/06/19/thinking-smart-contract-security/)
- [Smart Contract Security](https:#blog.ethereum.org/2016/06/10/smart-contract-security/)
- [Hacking Distributed Blog](http:#hackingdistributed.com/)

## Style
- [Vyper Style Guide WIP](https://github.com/ethereum/vyper/issues/905)
  - Heavily derived from [Solidity's style guide](http:#solidity.readthedocs.io/en/latest/style-guide.html) ...
  - ... which, in turn, is heavily derived from Python's [PEP 8](https:#www.python.org/dev/peps/pep-0008/) style guide.

## Editors
- [Vyper for VS Code (alpha)](https://github.com/p-/vscode-vyper)

## Future To Dos
- Update to current Vyper release
- List of common design patterns

*Feel free to send a pull request with any edits - or email* `pelusoken -/at-/ gmail`

