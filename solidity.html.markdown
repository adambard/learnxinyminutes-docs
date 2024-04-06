---
language: Solidity
filename: learnSolidity.sol
contributors:
  - ["Nemil Dalal", "https://www.nemil.com"]
  - ["Joseph Chow", ""]
  - ["Bhoomtawath Plinsut", "https://github.com/varshard"]
  - ["Shooter", "https://github.com/liushooter"]
  - ["Patrick Collins", "https://gist.github.com/PatrickAlphaC"]
---

Solidity lets you program on [Ethereum](https://www.ethereum.org/), a
blockchain-based virtual machine that allows the creation and
execution of smart contracts, without requiring centralized or trusted parties.

Solidity is a statically typed, contract programming language that has
similarities to Javascript and C. Like objects in OOP, each contract contains
state variables, functions, and common data types. Contract-specific features
include modifier (guard) clauses, event notifiers for listeners, and custom
global variables.

Some Ethereum contract examples include crowdfunding, voting, [decentralized finance](https://defipulse.com/), and blind auctions.

There is a high risk and high cost of errors in Solidity code, so you must be very careful to test
and slowly rollout. WITH THE RAPID CHANGES IN ETHEREUM, THIS DOCUMENT IS UNLIKELY TO STAY UP TO
DATE, SO YOU SHOULD FOLLOW THE SOLIDITY CHAT ROOM AND ETHEREUM BLOG FOR THE LATEST. ALL CODE HERE IS
PROVIDED AS IS, WITH SUBSTANTIAL RISK OF ERRORS OR DEPRECATED CODE PATTERNS.

Unlike other code, you may also need to add in design patterns like pausing, deprecation, and
throttling usage to reduce risk. This document primarily discusses syntax, and so excludes many
popular design patterns.

As Solidity and Ethereum are under active development, experimental or beta
features are typically marked, and subject to change. Pull requests welcome.

# Working with Remix and Metamask

One of the easiest ways to build, deploy, and test solidity code is by using the:

1. [Remix Web IDE](https://remix.ethereum.org/) 
2. [Metamask wallet](https://metamask.io/).

To get started, [download the Metamask Browser Extension](https://metamask.io/). 

Once installed, we will be working with Remix. The below code will be pre-loaded, but before we head over there, let's look at a few tips to get started with remix. Load it all by [hitting this link](https://remix.ethereum.org/#version=soljson-v0.6.6+commit.6c089d02.js&optimize=false&evmVersion=null&gist=f490c0d51141dd0515244db40bbd0c17&runs=200).

1. Choose the Solidity compiler

![Solidity-in-remix](images/solidity/remix-solidity.png)

2. Open the file loaded by that link

![Solidity-choose-file](images/solidity/remix-choose-file.png)

3. Compile the file

![Solidity-compile](images/solidity/remix-compile.png)

4. Deploy 

![Solidity-deploy](images/solidity/remix-deploy.png)

5. Play with contracts

![Solidity-deploy](images/solidity/remix-interact.png)

You've deployed your first contract! Congrats!

You can test out and play with the functions defined. Check out the comments to learn about what each does. 


## Working on a testnet

Deploying and testing on a testnet is the most accurate way to test your smart contracts in solidity. 
To do this let's first get some testnet ETH from the Kovan testnet. 

[Pop into this Gitter Channel](https://gitter.im/kovan-testnet/faucet) and drop your metamask address in.

In your metamask, you'll want to change to the `Kovan` testnet. 

![Solidity-in-remix](images/solidity/metamask-kovan.png)

You'll be given some free test Ethereum. Ethereum is needed to deploy smart contracts when working with a testnet. 

In the previous example, we didn't use a testnet, we deployed to a fake virtual environment. 
When working with a testnet, we can actually see and interact with our contracts in a persistent manner. 

To deploy to a testnet, on the `#4 Deploy` step, change your `environment` to `injected web3`.
This will use whatever network is currently selected in your metamask as the network to deploy to. 

![Solidity-in-remix](images/solidity/remix-testnet.png)

For now, please continue to use the `Javascript VM` unless instructed otherwise. When you deploy to a testnet, metamask will pop up to ask you to "confirm" the transaction. Hit yes, and after a delay, you'll get the same contract interface at the bottom of your screen. 


```javascript
// First, a simple Bank contract
// Allows deposits, withdrawals, and balance checks

// simple_bank.sol (note .sol extension)
/* **** START EXAMPLE **** */

// Declare the source file compiler version
pragma solidity ^0.6.6;

// Start with Natspec comment (the three slashes)
// used for documentation - and as descriptive data for UI elements/actions

/// @title SimpleBank
/// @author nemild

/* 'contract' has similarities to 'class' in other languages (class variables,
inheritance, etc.) */
contract SimpleBank { // CapWords
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
    constructor() public {
        // msg provides details about the message that's sent to the contract
        // msg.sender is contract caller (address of contract creator)
        owner = msg.sender;
    }

    /// @notice Deposit ether into bank
    /// @return The balance of the user after the deposit is made
    function deposit() public payable returns (uint) {
        // Use 'require' to test user inputs, 'assert' for internal invariants
        // Here we are making sure that there isn't an overflow issue
        require((balances[msg.sender] + msg.value) >= balances[msg.sender]);

        balances[msg.sender] += msg.value;
        // no "this." or "self." required with state variable
        // all values set to data type's initial value by default

        emit LogDepositMade(msg.sender, msg.value); // fire event

        return balances[msg.sender];
    }

    /// @notice Withdraw ether from bank
    /// @dev This does not return any excess ether sent to it
    /// @param withdrawAmount amount you want to withdraw
    /// @return remainingBal
    function withdraw(uint withdrawAmount) public returns (uint remainingBal) {
        require(withdrawAmount <= balances[msg.sender]);

        // Note the way we deduct the balance right away, before sending
        // Every .transfer/.send from this contract can call an external function
        // This may allow the caller to request an amount greater
        // than their balance using a recursive call
        // Aim to commit state before calling external functions, including .transfer/.send
        balances[msg.sender] -= withdrawAmount;

        // this automatically throws on a failure, which means the updated balance is reverted
        msg.sender.transfer(withdrawAmount);

        return balances[msg.sender];
    }

    /// @notice Get balance
    /// @return The balance of the user
    // 'view' (ex: constant) prevents function from editing state variables;
    // allows function to run locally/off blockchain
    function balance() view public returns (uint) {
        return balances[msg.sender];
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

// All state variables (those outside a function)
// are by default 'internal' and accessible inside contract
// and in all contracts that inherit ONLY
// Need to explicitly set to 'public' to allow external contracts to access
int256 public a = 8;

// For int and uint, can explicitly set space in steps of 8 up to 256
// e.g., int8, int16, int24
uint8 b;
int64 c;
uint248 e;

// Be careful that you don't overflow, and protect against attacks that do
// For example, for an addition, you'd do:
uint256 c = a + b;
assert(c >= a); // assert tests for internal invariants; require is used for user inputs
// For more examples of common arithmetic issues, see Zeppelin's SafeMath library
// https://github.com/OpenZeppelin/zeppelin-solidity/blob/master/contracts/math/SafeMath.sol


// No random functions built in, you can get a pseduo-random number by hashing the current blockhash, or get a truly random number using something like Chainlink VRF. 
// https://docs.chain.link/docs/get-a-random-number

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
owner.transfer(SOME_BALANCE); // fails and reverts on failure

// Can also do a lower level .send call, which returns a false if it failed
if (owner.send) {} // REMEMBER: wrap send in 'if', as contract addresses have
// functions executed on send and these can fail
// Also, make sure to deduct balances BEFORE attempting a send, as there is a risk of a recursive
// call that can drain the contract

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

// Type inference
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
delete x;


// Destructuring/Tuples
(x, y) = (2, 7); // assign/swap multiple values


// 2. DATA STRUCTURES
// Arrays
bytes32[5] nicknames; // static array
bytes32[] names; // dynamic array
uint newLength = names.push("John"); // adding returns new length of the array
// Length
names.length; // get length
names.length = 1; // lengths can be set (for dynamic arrays in storage only)

// multidimensional array
uint[][5] x; // arr with 5 dynamic array elements (opp order of most languages)

// Dictionaries (any type to any other type)
mapping (string => uint) public balances;
balances["charles"] = 1;
// balances["ada"] result is 0, all non-set key values return zeroes
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

// Structs
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

c.balance = 5; // set to new value
delete b;
// sets to initial value, set all variables in struct to 0, except mappings

// Enums
enum State { Created, Locked, Inactive }; // often used for state machine
State public state; // Declare variable from enum
state = State.Created;
// enums can be explicitly converted to ints
uint createdState = uint(State.Created); //  0

// Data locations: Memory vs. storage vs. calldata - all complex types (arrays,
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
// often used at end of contract life to transfer remaining balance to party
this.balance;
this.someFunction(); // calls func externally via call, not via internal jump

// ** msg - Current message received by the contract ** **
msg.sender; // address of sender
msg.value; // amount of ether provided to this contract in wei, the function should be marked "payable"
msg.data; // bytes, complete call data
msg.gas; // remaining gas

// ** tx - This transaction **
tx.origin; // address of sender of the transaction
tx.gasprice; // gas price of the transaction

// ** block - Information about current block **
now; // current time (approximately), alias for block.timestamp (uses Unix time)
// Note that this can be manipulated by miners, so use carefully

block.number; // current block number
block.difficulty; // current block difficulty
block.blockhash(1); // returns bytes32, only works for most recent 256 blocks
block.gasLimit();

// ** storage - Persistent storage hash **
storage['abc'] = 'def'; // maps 256 bit words to 256 bit words


// 5. FUNCTIONS AND MORE
// A. Functions
// Simple function
function increment(uint x) returns (uint) {
    x += 1;
    return x;
}

// Functions can return many arguments,
// and by specifying returned arguments name explicit return is not needed
function increment(uint x, uint y) returns (uint x, uint y) {
    x += 1;
    y += 1;
}
// Call previous function
uint (a,b) = increment(1,1);

// 'view' (alias for 'constant')
// indicates that function does not/cannot change persistent vars
// View function execute locally, not on blockchain
// Noted: constant keyword will soon be deprecated.
uint y = 1;

function increment(uint x) view returns (uint x) {
    x += 1;
    y += 1; // this line would fail
    // y is a state variable, and can't be changed in a view function
}

// 'pure' is more strict than 'view' or 'constant', and does not
// even allow reading of state vars
// The exact rules are more complicated, so see more about
// view/pure:
// http://solidity.readthedocs.io/en/develop/contracts.html#view-functions

// 'Function Visibility specifiers'
// These can be placed where 'view' is, including:
// public - visible externally and internally (default for function)
// external - only visible externally (including a call made with this.)
// private - only visible in the current contract
// internal - only visible in current contract, and those deriving from it

// Generally, a good idea to mark each function explicitly

// Functions hoisted - and can assign a function to a variable
function a() {
    var z = b;
    z();
}

function b() {

}

// All functions that receive ether must be marked 'payable'
function depositEther() public payable {
    balances[msg.sender] += msg.value;
}


// Prefer loops to recursion (max call stack depth is 1024)
// Also, don't setup loops that you haven't bounded,
// as this can hit the gas limit

// B. Events
// Events are notify external parties; easy to search and
// access events from outside blockchain (with lightweight clients)
// typically declare after contract parameters

// Typically, capitalized - and add Log in front to be explicit and prevent confusion
// with a function call

// Declare
event LogSent(address indexed from, address indexed to, uint amount); // note capital first letter

// Call
LogSent(from, to, amount);

/**

For an external party (a contract or external entity), to watch using
the Web3 Javascript library:

// The following is Javascript code, not Solidity code
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

// Common paradigm for one contract to depend on another (e.g., a
// contract that depends on current exchange rate provided by another)

// C. Modifiers
// Modifiers validate inputs to functions such as minimal balance or user auth;
// similar to guard clause in other languages

// '_' (underscore) often included as last line in body, and indicates
// function being called should be placed there
modifier onlyAfter(uint _time) { require (now >= _time); _; }
modifier onlyOwner { require(msg.sender == owner); _; }
// commonly used with state machines
modifier onlyIfStateA (State currState) { require(currState == State.A); _; }

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
    _;
    if (msg.value > amount) {
        uint amountToRefund = amount - msg.value;
        msg.sender.transfer(amountToRefund);
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
    refundAddressList[x].transfer(SOME_AMOUNT);
}

// Two errors above:
// 1. A failure on transfer stops the loop from completing, tying up money
// 2. This loop could be arbitrarily long (based on the amount of users who need refunds), and
// therefore may always fail as it exceeds the max gas for a block
// Instead, you should let people withdraw individually from their subaccount, and mark withdrawn
// e.g., favor pull payments over push payments


// 7. OBJECTS/CONTRACTS

// A. Calling external contract
contract InfoFeed {
    function info() payable returns (uint ret)  { return 42; }
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
            super.z(); // call immediate parent overridden function
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


// 8. OTHER KEYWORDS

// A. Selfdestruct
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
keccak256("some_bid_amount", "some secret"); // commit

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

// D. Oracles and External Data
// Oracles are ways to interact with your smart contracts outside the blockchain. 
// They are used to get data from the real world, send post requests, to the real world
// or vise versa.

// Time-based implementations of contracts are also done through oracles, as 
// contracts need to be directly called and can not "subscribe" to a time. 
// Due to smart contracts being decentralized, you also want to get your data
// in a decentralized manner, otherwise you run into the centralized risk that 
// smart contract design matter prevents. 

// To easiest way get and use pre-boxed decentralized data is with Chainlink Data Feeds
// https://docs.chain.link/docs/get-the-latest-price
// We can reference on-chain reference points that have already been aggregated by 
// multiple sources and delivered on-chain, and we can use it as a "data bank" 
// of sources. 

// You can see other examples making API calls here:
// https://docs.chain.link/docs/make-a-http-get-request

// And you can of course build your own oracle network, just be sure to know 
// how centralized vs decentralized your application is. 

// Setting up oracle networks yourself

// E. Cron Job
// Contracts must be manually called to handle time-based scheduling; can create external
// code to regularly ping, or provide incentives (ether) for others to
//

// F. Observer Pattern
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

// G. State machines
// see example below for State enum and inState modifier
```

Work with the full example below using the [`Javascript VM` in remix here.](https://remix.ethereum.org/#version=soljson-v0.6.6+commit.6c089d02.js&optimize=false&evmVersion=null&gist=3d12cd503dcedfcdd715ef61f786be0b&runs=200)

```javascript
// *** EXAMPLE: A crowdfunding example (broadly similar to Kickstarter) ***
// ** START EXAMPLE **

// CrowdFunder.sol
pragma solidity ^0.6.6;

/// @title CrowdFunder
/// @author nemild
contract CrowdFunder {
    // Variables set on create by creator
    address public creator;
    address payable public fundRecipient; // creator may be different than recipient, and must be payable
    uint public minimumToRaise; // required to tip, else everyone gets refund
    string campaignUrl;
    byte version = "1";

    // Data structures
    enum State {
        Fundraising,
        ExpiredRefund,
        Successful
    }
    struct Contribution {
        uint amount;
        address payable contributor;
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
        require(state == _state);
        _;
    }

    modifier isCreator() {
        require(msg.sender == creator);
        _;
    }

    // Wait 24 weeks after final contract state before allowing contract destruction
    modifier atEndOfLifecycle() {
    require(((state == State.ExpiredRefund || state == State.Successful) &&
        completeAt + 24 weeks < now));
        _;
    }

    function crowdFund(
        uint timeInHoursForFundraising,
        string memory _campaignUrl,
        address payable _fundRecipient,
        uint _minimumToRaise)
        public
    {
        creator = msg.sender;
        fundRecipient = _fundRecipient;
        campaignUrl = _campaignUrl;
        minimumToRaise = _minimumToRaise;
        raiseBy = now + (timeInHoursForFundraising * 1 hours);
    }

    function contribute()
    public
    payable
    inState(State.Fundraising)
    returns(uint256 id)
    {
        contributions.push(
            Contribution({
                amount: msg.value,
                contributor: msg.sender
            }) // use array, so can iterate
        );
        totalRaised += msg.value;

        emit LogFundingReceived(msg.sender, msg.value, totalRaised);

        checkIfFundingCompleteOrExpired();
        return contributions.length - 1; // return id
    }

    function checkIfFundingCompleteOrExpired()
    public
    {
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
        fundRecipient.transfer(address(this).balance);
        LogWinnerPaid(fundRecipient);
    }

    function getRefund(uint256 id)
    inState(State.ExpiredRefund)
    public
    returns(bool)
    {
        require(contributions.length > id && id >= 0 && contributions[id].amount != 0 );

        uint256 amountToRefund = contributions[id].amount;
        contributions[id].amount = 0;

        contributions[id].contributor.transfer(amountToRefund);

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
}
// ** END EXAMPLE **
```

Some more functions. 

```javascript
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
// Full Style guide: http://solidity.readthedocs.io/en/develop/style-guide.html

// Quick summary:
// 4 spaces for indentation
// Two lines separate contract declarations (and other top level declarations)
// Avoid extraneous spaces in parentheses
// Can omit curly braces for one line statement (if, for, etc)
// else should be placed on own line


// 14. NATSPEC COMMENTS
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
- [Chainlink Beginner Tutorials](https://docs.chain.link/docs/beginners-tutorial)
- [Smart Contract Best Practices](https://github.com/ConsenSys/smart-contract-best-practices)
- [Superblocks Lab - Browser based IDE for Solidity](https://lab.superblocks.com/)
- [EthFiddle - The JsFiddle for Solidity](https://ethfiddle.com/)
- [Browser-based Solidity Editor](https://remix.ethereum.org/)
- [Gitter Solidity Chat room](https://gitter.im/ethereum/solidity)
- [Modular design strategies for Ethereum Contracts](https://docs.erisindustries.com/tutorials/solidity/)
- [Chainlink Documentation](https://docs.chain.link/docs/getting-started)

## Smart Contract Development Frameworks
- [Hardhat](https://hardhat.org/)
- [Brownie](https://github.com/eth-brownie/brownie)
- [Truffle](https://www.trufflesuite.com/)

## Important libraries
- [Zeppelin](https://github.com/OpenZeppelin/openzeppelin-contracts): Libraries that provide common contract patterns (crowdfuding, safemath, etc)
- [Chainlink](https://github.com/smartcontractkit/chainlink): Code that allows you to interact with external data

## Sample contracts
- [Dapp Bin](https://github.com/ethereum/dapp-bin)
- [Defi Example](https://github.com/PatrickAlphaC/chainlink_defi)
- [Solidity Baby Step Contracts](https://github.com/fivedogit/solidity-baby-steps/tree/master/contracts)
- [ConsenSys Contracts](https://github.com/ConsenSys/dapp-store-contracts)
- [State of Dapps](http://dapps.ethercasts.com/)

## Security
- [Thinking About Smart Contract Security](https://blog.ethereum.org/2016/06/19/thinking-smart-contract-security/)
- [Smart Contract Security](https://blog.ethereum.org/2016/06/10/smart-contract-security/)
- [Hacking Distributed Blog](http://hackingdistributed.com/)

## Style
- [Solidity Style Guide](http://solidity.readthedocs.io/en/latest/style-guide.html): Ethereum's style guide is heavily derived from Python's [PEP 8](https://www.python.org/dev/peps/pep-0008/) style guide.

## Editors
- [Remix](https://remix.ethereum.org/)
- [Emacs Solidity Mode](https://github.com/ethereum/emacs-solidity)
- [Vim Solidity](https://github.com/tomlion/vim-solidity)
- Editor Snippets ([Ultisnips format](https://gist.github.com/nemild/98343ce6b16b747788bc))

## Future To Dos
- New keywords: protected, inheritable
- List of common design patterns (throttling, RNG, version upgrade)
- Common security anti patterns

Feel free to send a pull request with any edits - or email nemild -/at-/ gmail
