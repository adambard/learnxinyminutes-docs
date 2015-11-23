---
language: Solidity
filename: learnSolidity.sol
contributors:
    - ["Nemil Dalal", "https://www.nemil.com"]
---

Solidity is a statically typed, contract programming language for [Ethereum](https://www.ethereum.org/) that has similarities to Javascript and C. Like an object in object-oriented languages, each contract contains state variables, functions, and common data types. Contract-specific features include modifier (guard) clauses, event notifiers, and custom variables.

Solidity lets you program on Ethereum, a blockchain-based virtual machine that allows the creation and computation of smart contracts, without needing centralized or trusted parties.

As Solidity and Ethereum are under active development, experimental or beta features are explicitly marked, and subject to change. Pull requests welcome.

```javascript
// Let's start with a simple Bank contract, before diving into to the key components of the language

// Start with a Natspec comment (the three slashes) that can be used
// for documentation - and as descriptive data for UI elements
/// @title A simple deposit/withdrawal bank built on Bitcoin

// All contracts are declared and named (in CamelCase)
contract AcmeBank {
    // Declare state variables outside a function, 
    // these are persistent throughout the life of the contract

    // a dictionary that maps addresses to balances
    mapping (address -> uint) balances; 

    // the 'public' makes 'owner' externally readable by users or contracts 
    // (but not writeable), the 'constant' means this value to be 
    // changed after initialization
    address public constant owner; 
    
    // Constructor, can receive one or many variables here
    function AcmeBank() {
      // msg is a default variable that provides both the 
      // contract messager's address and amount
      owner = msg.address; 
      // the owner has no additional rights, we're setting it for 
      // illustrative purposes
    }
  
    function deposit(uint balance) {
        balances[msg.sender] += msg.value; // no need for "this." or "self." in front of the state variable

        return balances[msg.sender];
    }

    function withdraw(uint withdrawAmount) returns (uint remainingBalance) {
      if(balances[msg.sender] >= withdrawAmount) {
        balances[msg.sender] -= withdrawAmount;
        balances[msg.sender].send(withdrawAmount);

        return balances[msg.sender];
      }
    }

    // The 'constant' prevents the function from editing state variables
    function balance() constant {
      return balances[msg.sender];
    }

    // Fallback function
    // This function is called if invalid data is sent or ether without data; 
    // Added so that ether sent to this contract is reverted if the contract fails
    // otherwise, the sender loses their money; you should add this in most contracts
    function () { throw; }
}
// End example

// Now let's go through the basics of Solidity

// 1. DATA TYPES
// uint is the data type typically used for currency (there are no doubles
//  or floats) and for dates
uint x; 
int const a = 8; // int of 256 bits, cannot be changed after instantiation
uint8 b;
int64 c;
// int256 is same as int
// For both int and uint, you can explicitly set space in steps of 8, 
// e.g., int8, int16
uint248 e;

// Type casting
int x = int(b)

bool b = true; // or do 'var b = true;' for inferred typing

// Addresses - holds 20 byte/160 bit Ethereum addresses to another contract
// ('Contract Account)') or person/external entity ('External Account')
address public owner; // Add 'public' field to indicate publicly/externally accessible, a getter is automatically created, but NOT a setter

// All addresses can be sent ether in the following way:
owner.send(SOME_BALANCE); // returns false on failure
owner.balance; // the balance of the owner

// Bytes are provided from 1 to 32
byte a; // byte is same as bytes1
bytes32 b;

// Dynamically sized
bytes m; // A special array, same as byte[] (but packed tightly)
// same as bytes, but does not allow length or index access (for now)
string n = 'hello';

// Type inference
// var does inferred typing based on first assignment, 
// can't be used in functions parameters
var a = true;
// there are edge cases where inference leads to a value being set (e.g., an uint 8) 
// that is different from what the user wanted (uint16), so use carefully

// by default, all values are set to 0 on instantiation

// Delete can be called on most types, and will set the values to 0
uint x = 5;
delete(x); // x is now 0

// 2. DATA STRUCTURES
// Arrays
bytes32[] names;
uint newLength = names.push("John"); // adding returns new length of the array
// Length
names.length; // get length
names.length = 1; // lengths can also be set, unlike many other languages

// Dictionaries (any type to any other type)
mapping (string -> uint) public balances;
balances["john"] = 1;
console.log(balances[jill]); // is 0, all non-set key values return zeroes
// The 'public' lets you do the following from another contract
contractName.balances("john"); // returns 1
// The 'public' keyword here created a getter (but not setter) that behaves like the following:
function balances(address _account) returns (uint balance) {
  return balances[_account];
}

// To delete
delete(balances["John"]);
delete(balances); // deletes all elements

// Unlike languages like Javascript, you cannot iterate through all elements in
// a map, without knowing the source keys

// Structs and enums 
  struct Bank { // note the capital
      address owner;
      uint balance;
  }
Bank b = Bank({
  owner: msg.sender, 
  balance: 5
});
delete(b); // set all values to 0, except any mappings

// Enums
enum State { Created, Locked, Inactive };
State public state; // Declare variable from enum
state = State.Created;

// 3. Variables of note
// storage - A persistent storage hash (does not need to be declared)
storage['abc'] = 'def'; // maps 256 bit words to 256 bit words

// tx - This transaction
tx.origin // address, sender of the transaction
tx.gasprice // uint, gas price of the transaction

// msg - The current message received by the contract
msg.sender; // address, The address of the sender
msg.value; // uint, The amount of gas provided to this contract in wei
msg.data // bytes, complete call data

// balance of the current contract (both contract and external accounts 
// have balances) - often used at the end of a contracts life to send the 
// remaining balance to a party
this.balance 
// block
now // uint, current time, alias for block.timestamp
block.number // uint, current block number
block.difficulty // uint, current block difficulty
block.blockhash(1) // returns bytes32, only provides for most recent 256 block

// 4. FUNCTIONS AND MORE
// A. Functions
// Simple function
function increment(uint x) returns (uint) {
  x += 1;
  return x;
}

// Functions can return many arguments, and by specifying the returned arguments
//  you don't need to explicity return
function increment(uint x, uint y) returns (uint x, uint y) {
  x += 1;
  y += 1;
}
// This function would have been called like this, and assigned to a tuple
uint (a,b) = increment(1,1);

// The 'constant' indicates and ensures that a function does not/cannot change the persistent variables
uint y;

function increment(uint x) constant returns (uint x) {
  x += 1;
  y += 1; // this line would fail
  // as y is a state variable, and can't be changed in a constant function
}

// There are a few 'function visibility specifiers' that can be placed where 'constant'
//  is, which include:
// internal (can only be called by an internal function, not one external to the contract)
// public - visibile externally and internally
// private - only visible in the current contract

// Functions are hoisted (so you can call a function, even if it is declared later) - and you can assign a function to a variable
function a() {
  var z = b;
  b();
}

function b() {

}

// B. Events
// Events are an easy way to notify external listeners that something changed
// You typically decalre them after your contract parameters
event Sent(address from, address to, uint amount);

// You then call it in a function, when you want to trigger it
sent(from, to, amount);

// For an external party (a contract or external entity), to watch 
// for an event, you write the following:
Coin.Sent().watch({}, '', function(error, result) {
    if (!error) {
        console.log("Coin transfer: " + result.args.amount +
            " coins were sent from " + result.args.from +
            " to " + result.args.to + ".");
        console.log("Balances now:\n" +
            "Sender: " + Coin.balances.call(result.args.from) +
            "Receiver: " + Coin.balances.call(result.args.to));
    }
}
// This is a common paradigm for one contract to depend on another (e.g., a 
// contract that depends on the current exchange rate provided by another
// contract)

// C. Modifiers
// Modifiers let you validate inputs to functions
// The '_' (underscore) must be included, and is an indicator that the 
// function being called should be placed there
modifier onlyBefore(uint _time) { if (now >= _time) throw; _ }

// You can then append it right after the function declaration
function test()
  onlyBefore()
{

}

// 5. BRANCHING AND LOOPS

// All basic logic blocks work - including if/else, for, while, break, continue, return
// switch is not provided
// Syntax is the same as javascript, but there is no type conversion from 
// non-boolean to boolean

// 6. CALLING AN EXTERNAL CONTRACT

contract infoFeed {
  function info() returns (uint ret) { return 42; }
}

contract Consumer {
  InfoFeed feed; // create a variable that will point to a contract on the blockchain
  function setFeed(address addr) { 
    // Link to the contract by creating on the address
    feed = InfoFeed(addr); 
  }
  function callFeed() { 
    // T final parentheses call the contract, optionally adding 
    // custom value or gas numbers
    feed.info.value(10).gas(800)();
  }
}

// 7. CONTRACT DESIGN PATTERNS

// A. Obfuscation
// Remember that all variables are publicly viewable on the blockchain, so 
// anything that needs some privacy needs to be obfuscated (e.g., hashed)

// B. Throwing
// Throwing
throw; // throwing is easily done, and reverts unused money to the sender
// You can't currently catch

// A common design pattern is:
if (!addr.send(123)) {
  throw;
}

// C. Suicide
// Suicide
suicide(SOME_ADDRESS); // suicide the current contract, sending funds to the address (often the creator)

// D. Storage optimization
// Reading and writing can be expensive, as data needs to be stored in the 
// blockchain forever - this encourages smart ways to use memory (eventually, 
// compilation may better handle this, but for now there are benefits to 
// planning your data structures)

// *** EXAMPLE: Let's do a more complex example ***
// [TODO: Decide what a more complex example looks like, needs a few // characteristics:
//   - has a 'constant' state variable
//   - has a state machine (uses modifier)
//   - sends money to an address
//   - gets information from another contract (we'll show code for both contracts)
//   - Shows inheritance
//   - show variables being passed in on instantiation (and guard code to throw if variables not provided)
//   Ideas:
//     - crowdfunding?
//     - Peer to peer insurance
// ]

// *** END EXAMPLE ***

// Some final points
// 7. NATIVE FUNCTIONS

// Currency units
// By default, currency is defined using wei, the smallest unit of Ether
uint minAmount = 1 wei;
uint a = 1 finney; // 1 ether = 1000 finney
// There are a number of other units, see: http://ether.fund/tool/converter

// Time units
1 == 1 second
1 minutes == 60 seconds


// You typically multiply a variable times the unit, as these units are not 
// directly stored in a variable
uint x = 5;
(x * 1 days); // 5 days

// Be careful about leap seconds and leap years when using equality statements for time (instead, prefer greater than/less than)

// Cryptography
// All strings passed are concatenated before the hash is run
sha3("ab", "cd");
ripemd160("abc");
sha256("def");

// These are natspec comments, when a user is asked to confirm a transaction
/// 
/** */

// 8. COMMON MISTAKES
// A few common mistakes
// You cannot restrict a human or computer from reading the content of 
// your transaction or a transaction's state

// All data to the start of time is stored in the blockchain, so you and 
// anyone can observe all previous data states

// 9. STYLE NOTES
// Use 4 spaces for indentation 
// (Python's PEP8 is used as the baseline style guide, including its general philosophy)
```

## Additional resources
- [Solidity Docs](https://ethereum.github.io/solidity/docs/home/)
- [Solidity Style Guide](https://ethereum.github.io/solidity//docs/style-guide/): Ethereum's style guide is heavily derived from Python's [pep8](https://www.python.org/dev/peps/pep-0008/) style guide.
- [Browser-based Solidity Editor](http://chriseth.github.io/browser-solidity/)
- [Gitter Chat room](https://gitter.im/ethereum/go-ethereum)

Feel free to send a pull request with any edits - or email nemild -/at-/ gmail