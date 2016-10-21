---
language: Solidity
filename: learnSolidity.sol
contributors:
  - ["Nemil Dalal", "https://www.nemil.com"]
  - ["Joseph Chow", ""]
---

Solidity lets you program on [Ethereum](https://www.ethereum.org/), a
blockchain-based virtual machine that allows the creation and
execution of smart contracts, without requiring centralized or trusted parties.

Solidity is a statically typed, contract programming language that has
similarities to Javascript and C. Like objects in OOP, each contract contains
state variables, functions, and common data types. Contract-specific features
include modifier (guard) clauses, event notifiers for listeners, and custom
global variables.

Some Ethereum contract examples include crowdfunding, voting, and blind auctions.

There is a high risk and high cost of errors in Solidity code, so you must be very careful to test
and slowly rollout. WITH THE RAPID CHANGES IN ETHEREUM, THIS DOCUMENT IS UNLIKELY TO STAY UP TO
DATE, SO YOU SHOULD FOLLOW THE SOLIDITY CHAT ROOM AND ETHEREUM BLOG FOR THE LATEST. ALL CODE HERE IS
PROVIDED AS IS, WITH SUBSTANTIAL RISK OF ERRORS OR DEPRECATED CODE PATTERNS.

Unlike other code, you may also need to add in design patterns like pausing, deprecation, and
throttling usage to reduce risk. This document primarily discusses syntax, and so excludes many
popular design patterns.

As Solidity and Ethereum are under active development, experimental or beta
features are typically marked, and subject to change. Pull requests welcome.

```javascript
// First, a simple Bank contract
// Allows deposits, withdrawals, and balance checks

// simple_bank.sol (note .sol extension)
/* **** START EXAMPLE **** */

// Start with Natspec comment (the three slashes)
// used for documentation - and as descriptive data for UI elements/actions

/// @title SimpleBank
/// @author nemild

/* 'contract' has similarities to 'class' in other languages (class variables,
inheritance, etc.) */
contract SimpleBank { // CamelCase
    // Declare state variables outside function, persist through life of contract

    // dictionary that maps addresses to balances
    // always be careful about overflow attacks with numbers
    mapping (address => uint) private balances;

    // "private" means that other contracts can't directly query balances
    // but data is still viewable to other parties on blockchain

    address public owner;
    // 'public' makes externally readable (not writeable) by users or contracts

    // Events - publicize actions to external listeners
    event LogDepositMade(address accountAddress, uint amount);

    // Constructor, can receive one or many variables here; only one allowed
    function AcmeBank() {
        // msg provides details about the message that's sent to the contract
        // msg.sender is contract caller (address of contract creator)
        owner = msg.sender;
    }

    /// @notice Deposit ether into bank
    /// @return The balance of the user after the deposit is made
    function deposit() public returns (uint) {
        balances[msg.sender] += msg.value;
        // no "this." or "self." required with state variable
        // all values set to data type's initial value by default

        LogDepositMade(msg.sender, msg.value); // fire event

        return balances[msg.sender];
    }

    /// @notice Withdraw ether from bank
    /// @dev This does not return any excess ether sent to it
    /// @param withdrawAmount amount you want to withdraw
    /// @return The balance remaining for the user
    function withdraw(uint withdrawAmount) public returns (uint remainingBal) {
        if(balances[msg.sender] >= withdrawAmount) {
            // Note the way we deduct the balance right away, before sending - due to
            // the risk of a recursive call that allows the caller to request an amount greater
            // than their balance
            balances[msg.sender] -= withdrawAmount;

            if (!msg.sender.send(withdrawAmount)) {
                // increment back only on fail, as may be sending to contract that
                // has overridden 'send' on the receipt end
                balances[msg.sender] += withdrawAmount;
            }
        }

        return balances[msg.sender];
    }

    /// @notice Get balance
    /// @return The balance of the user
    // 'constant' prevents function from editing state variables;
    // allows function to run locally/off blockchain
    function balance() constant returns (uint) {
        return balances[msg.sender];
    }

    // Fallback function - Called if other functions don't match call or
    // sent ether without data
    // Typically, called when invalid data is sent
    // Added so ether sent to this contract is reverted if the contract fails
    // otherwise, the sender's money is transferred to contract
    function () {
        throw; // throw reverts state to before call
    }
}
// ** END EXAMPLE **


// Now, the basics of Solidity

// 1. DATA TYPES AND ASSOCIATED METHODS
// uint used for currency amount (there are no doubles
//  or floats) and for dates (in unix time)
uint x;

// int of 256 bits, cannot be changed after instantiation
int constant a = 8;
int256 constant a = 8; // same effect as line above, here the 256 is explicit
uint constant VERSION_ID = 0x123A1; // A hex constant
// with 'constant', compiler replaces each occurrence with actual value


// For int and uint, can explicitly set space in steps of 8 up to 256
// e.g., int8, int16, int24
uint8 b;
int64 c;
uint248 e;

// Be careful that you don't overflow, and protect against attacks that do

// No random functions built in, use other contracts for randomness

// Type casting
int x = int(b);

bool b = true; // or do 'var b = true;' for inferred typing

// Addresses - holds 20 byte/160 bit Ethereum addresses
// No arithmetic allowed
address public owner;

// Types of accounts:
// Contract account: address set on create (func of creator address, num transactions sent)
// External Account: (person/external entity): address created from public key

// Add 'public' field to indicate publicly/externally accessible
// a getter is automatically created, but NOT a setter

// All addresses can be sent ether
owner.send(SOME_BALANCE); // returns false on failure
if (owner.send) {} // REMEMBER: wrap in 'if', as contract addresses have
// functions executed on send and these can fail
// Also, make sure to deduct balances BEFORE attempting a send, as there is a risk of a recursive
// call that can drain the contract

// can override send by defining your own

// Can check balance
owner.balance; // the balance of the owner (user or contract)


// Bytes available from 1 to 32
byte a; // byte is same as bytes1
bytes2 b;
bytes32 c;

// Dynamically sized bytes
bytes m; // A special array, same as byte[] array (but packed tightly)
// More expensive than byte1-byte32, so use those when possible

// same as bytes, but does not allow length or index access (for now)
string n = "hello"; // stored in UTF8, note double quotes, not single
// string utility functions to be added in future
// prefer bytes32/bytes, as UTF8 uses more storage

// Type inferrence
// var does inferred typing based on first assignment,
// can't be used in functions parameters
var a = true;
// use carefully, inference may provide wrong type
// e.g., an int8, when a counter needs to be int16

// var can be used to assign function to variable
function a(uint x) returns (uint) {
    return x * 2;
}
var f = a;
f(22); // call

// by default, all values are set to 0 on instantiation

// Delete can be called on most types
// (does NOT destroy value, but sets value to 0, the initial value)
uint x = 5;


// Destructuring/Tuples
(x, y) = (2, 7); // assign/swap multiple value


// 2. DATA STRUCTURES
// Arrays
bytes32[5] nicknames; // static array
bytes32[] names; // dynamic array
uint newLength = names.push("John"); // adding returns new length of the array
// Length
names.length; // get length
names.length = 1; // lengths can be set (for dynamic arrays in storage only)

// multidimensional array
uint x[][5]; // arr with 5 dynamic array elements (opp order of most languages)

// Dictionaries (any type to any other type)
mapping (string => uint) public balances;
balances["charles"] = 1;
console.log(balances["ada"]); // is 0, all non-set key values return zeroes
// 'public' allows following from another contract
contractName.balances("charles"); // returns 1
// 'public' created a getter (but not setter) like the following:
function balances(string _account) returns (uint balance) {
    return balances[_account];
}

// Nested mappings
mapping (address => mapping (address => uint)) public custodians;

// To delete
delete balances["John"];
delete balances; // sets all elements to 0

// Unlike other languages, CANNOT iterate through all elements in
// mapping, without knowing source keys - can build data structure
// on top to do this

// Structs and enums
struct Bank {
    address owner;
    uint balance;
}
Bank b = Bank({
    owner: msg.sender,
    balance: 5
});
// or
Bank c = Bank(msg.sender, 5);

c.amount = 5; // set to new value
delete b;
// sets to initial value, set all variables in struct to 0, except mappings

// Enums
enum State { Created, Locked, Inactive }; // often used for state machine
State public state; // Declare variable from enum
state = State.Created;
// enums can be explicitly converted to ints
uint createdState = uint(State.Created); //  0

// Data locations: Memory vs. storage vs. stack - all complex types (arrays,
// structs) have a data location
// 'memory' does not persist, 'storage' does
// Default is 'storage' for local and state variables; 'memory' for func params
// stack holds small local variables

// for most types, can explicitly set which data location to use


// 3. Simple operators
// Comparisons, bit operators and arithmetic operators are provided
// exponentiation: **
// exclusive or: ^
// bitwise negation: ~


// 4. Global Variables of note
// ** this **
this; // address of contract
// often used at end of contract life to send remaining balance to party
this.balance;
this.someFunction(); // calls func externally via call, not via internal jump

// ** msg - Current message received by the contract ** **
msg.sender; // address of sender
msg.value; // amount of ether provided to this contract in wei
msg.data; // bytes, complete call data
msg.gas; // remaining gas

// ** tx - This transaction **
tx.origin; // address of sender of the transaction
tx.gasprice; // gas price of the transaction

// ** block - Information about current block **
now; // current time (approximately), alias for block.timestamp (uses Unix time)
block.number; // current block number
block.difficulty; // current block difficulty
block.blockhash(1); // returns bytes32, only works for most recent 256 blocks
block.gasLimit();

// ** storage - Persistent storage hash **
storage['abc'] = 'def'; // maps 256 bit words to 256 bit words


// 4. FUNCTIONS AND MORE
// A. Functions
// Simple function
function increment(uint x) returns (uint) {
    x += 1;
    return x;
}

// Functions can return many arguments, and by specifying returned arguments
// name don't need to explicitly return
function increment(uint x, uint y) returns (uint x, uint y) {
    x += 1;
    y += 1;
}
// Call previous functon
uint (a,b) = increment(1,1);

// 'constant' indicates that function does not/cannot change persistent vars
// Constant function execute locally, not on blockchain
uint y;

function increment(uint x) constant returns (uint x) {
    x += 1;
    y += 1; // this line would fail
    // y is a state variable, and can't be changed in a constant function
}

// 'Function Visibility specifiers'
// These can be placed where 'constant' is, including:
// public - visible externally and internally (default)
// external
// private - only visible in the current contract
// internal - only visible in current contract, and those deriving from it

// Functions hoisted - and can assign a function to a variable
function a() {
    var z = b;
    b();
}

function b() {

}


// Prefer loops to recursion (max call stack depth is 1024)

// B. Events
// Events are notify external parties; easy to search and
// access events from outside blockchain (with lightweight clients)
// typically declare after contract parameters

// Typically, capitalized - and add Log in front to be explicit and prevent confusion
// with a function call

// Declare
event LogSent(address indexed from, address indexed to, uint amount); // note capital first letter

// Call
Sent(from, to, amount);

// For an external party (a contract or external entity), to watch:
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
// Common paradigm for one contract to depend on another (e.g., a
// contract that depends on current exchange rate provided by another)

// C. Modifiers
// Modifiers validate inputs to functions such as minimal balance or user auth;
// similar to guard clause in other languages

// '_' (underscore) often included as last line in body, and indicates
// function being called should be placed there
modifier onlyAfter(uint _time) { if (now <= _time) throw; _ }
modifier onlyOwner { if (msg.sender == owner) _ }
// commonly used with state machines
modifier onlyIfState (State currState) { if (currState != State.A) _ }

// Append right after function declaration
function changeOwner(newOwner)
onlyAfter(someTime)
onlyOwner()
onlyIfState(State.A)
{
    owner = newOwner;
}

// underscore can be included before end of body,
// but explicitly returning will skip, so use carefully
modifier checkValue(uint amount) {
    _
    if (msg.value > amount) {
        uint amountToRefund = amount - msg.value;
        if (!msg.sender.send(amountToRefund)) {
            throw;
        }
    }
}


// 6. BRANCHING AND LOOPS

// All basic logic blocks work - including if/else, for, while, break, continue
// return - but no switch

// Syntax same as javascript, but no type conversion from non-boolean
// to boolean (comparison operators must be used to get the boolean val)

// For loops that are determined by user behavior, be careful - as contracts have a maximal
// amount of gas for a block of code - and will fail if that is exceeded
// For example:
for(uint x = 0; x < refundAddressList.length; x++) {
    if (!refundAddressList[x].send(SOME_AMOUNT)) {
       throw;
    }
}

// Two errors above:
// 1. A failure on send stops the loop from completing, tying up money
// 2. This loop could be arbitrarily long (based on the amount of users who need refunds), and
// therefore may always fail as it exceeds the max gas for a block
// Instead, you should let people withdraw individually from their subaccount, and mark withdrawn


// 7. OBJECTS/CONTRACTS

// A. Calling external contract
contract infoFeed {
    function info() returns (uint ret) { return 42; }
}

contract Consumer {
    InfoFeed feed; // points to contract on blockchain

    // Set feed to existing contract instance
    function setFeed(address addr) {
        // automatically cast, be careful; constructor is not called
        feed = InfoFeed(addr);
    }

    // Set feed to new instance of contract
    function createNewFeed() {
        feed = new InfoFeed(); // new instance created; constructor called
    }

    function callFeed() {
        // final parentheses call contract, can optionally add
        // custom ether value or gas
        feed.info.value(10).gas(800)();
    }
}

// B. Inheritance

// Order matters, last inherited contract (i.e., 'def') can override parts of
// previously inherited contracts
contract MyContract is abc, def("a custom argument to def") {

// Override function
    function z() {
        if (msg.sender == owner) {
            def.z(); // call overridden function from def
            super.z(); // call immediate parent overriden function
        }
    }
}

// abstract function
function someAbstractFunction(uint x);
// cannot be compiled, so used in base/abstract contracts
// that are then implemented

// C. Import

import "filename";
import "github.com/ethereum/dapp-bin/library/iterable_mapping.sol";

// Importing under active development
// Cannot currently be done at command line


// 8. OTHER KEYWORDS

// A. Throwing
// Throwing
throw; // reverts unused money to sender, state is reverted
// Can't currently catch

// Common design pattern is:
if (!addr.send(123)) {
    throw;
}

// B. Selfdestruct
// selfdestruct current contract, sending funds to address (often creator)
selfdestruct(SOME_ADDRESS);

// removes storage/code from current/future blocks
// helps thin clients, but previous data persists in blockchain

// Common pattern, lets owner end the contract and receive remaining funds
function remove() {
    if(msg.sender == creator) { // Only let the contract creator do this
        selfdestruct(creator); // Makes contract inactive, returns funds
    }
}

// May want to deactivate contract manually, rather than selfdestruct
// (ether sent to selfdestructed contract is lost)


// 9. CONTRACT DESIGN NOTES

// A. Obfuscation
// All variables are publicly viewable on blockchain, so anything
// that is private needs to be obfuscated (e.g., hashed w/secret)

// Steps: 1. Commit to something, 2. Reveal commitment
sha3("some_bid_amount", "some secret"); // commit

// call contract's reveal function in the future
// showing bid plus secret that hashes to SHA3
reveal(100, "mySecret");

// B. Storage optimization
// Writing to blockchain can be expensive, as data stored forever; encourages
// smart ways to use memory (eventually, compilation will be better, but for now
// benefits to planning data structures - and storing min amount in blockchain)

// Cost can often be high for items like multidimensional arrays
// (cost is for storing data - not declaring unfilled variables)

// C. Data access in blockchain
// Cannot restrict human or computer from reading contents of
// transaction or transaction's state

// While 'private' prevents other *contracts* from reading data
// directly - any other party can still read data in blockchain

// All data to start of time is stored in blockchain, so
// anyone can observe all previous data and changes

// D. Cron Job
// Contracts must be manually called to handle time-based scheduling; can create external
// code to regularly ping, or provide incentives (ether) for others to

// E. Observer Pattern
// An Observer Pattern lets you register as a subscriber and
// register a function which is called by the oracle (note, the oracle pays
// for this action to be run)
// Some similarities to subscription in Pub/sub

// This is an abstract contract, both client and server classes import
// the client should implement
contract SomeOracleCallback {
    function oracleCallback(int _value, uint _time, bytes32 info) external;
}

contract SomeOracle {
    SomeOracleCallback[] callbacks; // array of all subscribers

    // Register subscriber
    function addSubscriber(SomeOracleCallback a) {
        callbacks.push(a);
    }

    function notify(value, time, info) private {
        for(uint i = 0;i < callbacks.length; i++) {
            // all called subscribers must implement the oracleCallback
            callbacks[i].oracleCallback(value, time, info);
        }
    }

    function doSomething() public {
        // Code to do something

        // Notify all subscribers
        notify(_value, _time, _info);
    }
}

// Now, your client contract can addSubscriber by importing SomeOracleCallback
// and registering with Some Oracle

// F. State machines
// see example below for State enum and inState modifier


// *** EXAMPLE: A crowdfunding example (broadly similar to Kickstarter) ***
// ** START EXAMPLE **

// CrowdFunder.sol

/// @title CrowdFunder
/// @author nemild
contract CrowdFunder {
    // Variables set on create by creator
    address public creator;
    address public fundRecipient; // creator may be different than recipient
    uint public minimumToRaise; // required to tip, else everyone gets refund
    string campaignUrl;
    byte constant version = 1;

    // Data structures
    enum State {
        Fundraising,
        ExpiredRefund,
        Successful
    }
    struct Contribution {
        uint amount;
        address contributor;
    }

    // State variables
    State public state = State.Fundraising; // initialize on create
    uint public totalRaised;
    uint public raiseBy;
    uint public completeAt;
    Contribution[] contributions;

    event LogFundingReceived(address addr, uint amount, uint currentTotal);
    event LogWinnerPaid(address winnerAddress);

    modifier inState(State _state) {
        if (state != _state) throw;
        _
    }

    modifier isCreator() {
        if (msg.sender != creator) throw;
        _
    }

    // Wait 6 months after final contract state before allowing contract destruction
    modifier atEndOfLifecycle() {
    if(!((state == State.ExpiredRefund || state == State.Successful) &&
        completeAt + 6 months < now)) {
            throw;
        }
        _
    }

    function CrowdFunder(
        uint timeInHoursForFundraising,
        string _campaignUrl,
        address _fundRecipient,
        uint _minimumToRaise)
    {
        creator = msg.sender;
        fundRecipient = _fundRecipient;
        campaignUrl = _campaignUrl;
        minimumToRaise = _minimumToRaise;
        raiseBy = now + (timeInHoursForFundraising * 1 hours);
    }

    function contribute()
    public
    inState(State.Fundraising)
    {
        contributions.push(
            Contribution({
                amount: msg.value,
                contributor: msg.sender
            }) // use array, so can iterate
        );
        totalRaised += msg.value;

        LogFundingReceived(msg.sender, msg.value, totalRaised);

        checkIfFundingCompleteOrExpired();
        return contributions.length - 1; // return id
    }

    function checkIfFundingCompleteOrExpired() {
        if (totalRaised > minimumToRaise) {
            state = State.Successful;
            payOut();

            // could incentivize sender who initiated state change here
        } else if ( now > raiseBy )  {
            state = State.ExpiredRefund; // backers can now collect refunds by calling getRefund(id)
        }
        completeAt = now;
    }

    function payOut()
    public
    inState(State.Successful)
    {
        if(!fundRecipient.send(this.balance)) {
            throw;
        }


        LogWinnerPaid(fundRecipient);
    }

    function getRefund(id)
    public
    inState(State.ExpiredRefund)
    {
        if (contributions.length <= id || id < 0 || contributions[id].amount == 0 ) {
            throw;
        }

        uint amountToRefund = contributions[id].amount;
        contributions[id].amount = 0;

        if(!contributions[id].contributor.send(amountToSend)) {
            contributions[id].amount = amountToSend;
            return false;
        }

      return true;
    }

    function removeContract()
    public
    isCreator()
    atEndOfLifecycle()
    {
        selfdestruct(msg.sender);
        // creator gets all money that hasn't be claimed
    }

    function () { throw; }
}
// ** END EXAMPLE **

// 10. OTHER NATIVE FUNCTIONS

// Currency units
// Currency is defined using wei, smallest unit of Ether
uint minAmount = 1 wei;
uint a = 1 finney; // 1 ether == 1000 finney
// Other units, see: http://ether.fund/tool/converter

// Time units
1 == 1 second
1 minutes == 60 seconds

// Can multiply a variable times unit, as units are not stored in a variable
uint x = 5;
(x * 1 days); // 5 days

// Careful about leap seconds/years with equality statements for time
// (instead, prefer greater than/less than)

// Cryptography
// All strings passed are concatenated before hash action
sha3("ab", "cd");
ripemd160("abc");
sha256("def");

// 11. SECURITY

// Bugs can be disastrous in Ethereum contracts - and even popular patterns in Solidity,
// may be found to be antipatterns

// See security links at the end of this doc

// 12. LOW LEVEL FUNCTIONS
// call - low level, not often used, does not provide type safety
successBoolean = someContractAddress.call('function_name', 'arg1', 'arg2');

// callcode - Code at target address executed in *context* of calling contract
// provides library functionality
someContractAddress.callcode('function_name');


// 13. STYLE NOTES
// Based on Python's PEP8 style guide

// Quick summary:
// 4 spaces for indentation
// Two lines separate contract declarations (and other top level declarations)
// Avoid extraneous spaces in parentheses
// Can omit curly braces for one line statement (if, for, etc)
// else should be placed on own line


// 14. NATSPEC COMENTS
// used for documentation, commenting, and external UIs

// Contract natspec - always above contract definition
/// @title Contract title
/// @author Author name

// Function natspec
/// @notice information about what function does; shown when function to execute
/// @dev Function documentation for developer

// Function parameter/return value natspec
/// @param someParam Some description of what the param does
/// @return Description of the return value
```

## Additional resources
- [Solidity Docs](https://solidity.readthedocs.org/en/latest/)
- [Solidity Style Guide](https://ethereum.github.io/solidity//docs/style-guide/): Ethereum's style guide is heavily derived from Python's [pep8](https://www.python.org/dev/peps/pep-0008/) style guide.
- [Browser-based Solidity Editor](http://chriseth.github.io/browser-solidity/)
- [Gitter Solidity Chat room](https://gitter.im/ethereum/solidity)
- [Modular design strategies for Ethereum Contracts](https://docs.erisindustries.com/tutorials/solidity/)

## Sample contracts
- [Dapp Bin](https://github.com/ethereum/dapp-bin)
- [Solidity Baby Step Contracts](https://github.com/fivedogit/solidity-baby-steps/tree/master/contracts)
- [ConsenSys Contracts](https://github.com/ConsenSys/dapp-store-contracts)
- [State of Dapps](http://dapps.ethercasts.com/)

## Security
- [Thinking About Smart Contract Security](https://blog.ethereum.org/2016/06/19/thinking-smart-contract-security/)
- [Smart Contract Security](https://blog.ethereum.org/2016/06/10/smart-contract-security/)
- [Hacking Distributed Blog](http://hackingdistributed.com/)

## Information purposefully excluded
- Libraries

## Style
- Python's [PEP8](https://www.python.org/dev/peps/pep-0008/) is used as the baseline style guide, including its general philosophy

## Editors
- [Vim Solidity](https://github.com/tomlion/vim-solidity)
- Editor Snippets ([Ultisnips format](https://gist.github.com/nemild/98343ce6b16b747788bc))

## Future To Dos
- New keywords: protected, inheritable
- List of common design patterns (throttling, RNG, version upgrade)
- Common security anti patterns

Feel free to send a pull request with any edits - or email nemild -/at-/ gmail
