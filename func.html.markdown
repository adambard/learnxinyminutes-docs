---
language: FunC
filename: learnFunC.fc
contributors:
  - ["Ivan Romanovich", "https://t.me/ton_learn"]
  - ["Kirill Malev", "https://fslabs.io"]
---

The FunC language is used to program smart contracts on the [The Open Network](https://ton.org) blockchain. Contract logic is executed in TVM, the stack-based TON Virtual Machine.

FunC is a statically typed, which is similar to C.

# Basic syntax, the first Smart Contract — Data Types, Storage, Functions

```c
;; Single line comment

	{- This is a multi-line comment
		{- this is a comment in the comment -}
	-}
	
	(int) sum(int a, int b) { 
    ;; This is a function that gets two integer parameters 
    ;; and return integer result
	  return a + b;  
    ;; All integers are signed and are 257 bit long. Overflow throws exception
    ;; expressions must end with a semicolon
	}
	
	() f(int i, cell c, slice s, builder b, cont c, tuple t) {
	  ;; FunC has 7 atomic types: 
	  ;; int - 257 bit signed integers,
	  ;; cell - basic for TON opaque data structure,
      ;; which contains up to 1,023 bits and up to 4 references to other cells,
	  ;; slice and builder - special objects to read from and write to cells,
	  ;; continuation - another flavor of cell that contains 
      ;; ready-to-execute TVM byte-code.
	  ;; tuple is an ordered collection of up to 255 components,
      ;; having arbitrary value types, possibly distinct.
	  ;; Finally tensor type (A,B, ...) is an ordered collection ready for
      ;; mass assigning like: (int, int) a = (3, 5);
	  ;; Special case of tensor type is the unit type ().
	  ;; It represents that a function doesn’t return any value,
      ;; or has no arguments.
	}
	
	;; During execution, the contract has read access to local context:
    ;; its storage, balance, time, network config, etc.
	;; Contract may change its storage and code,
    ;; and also may send messages to other contracts

	;; Let’s write a counter smart contract that gets a number
    ;; from an incoming message,
	;; adds to already stored numbers and stores result in “storage”

	;; For handling special events, smart contracts have reserved methods:
	;; recv_internal() handles internal messages from other smart contracts
	;; recv_external() handles external messages from the outside world —
    ;; e.g., from a user.

	() recv_internal(slice in_msg_body) {
	  ;; Cells play the role of memory in the stack-based TVM.
      ;; A cell can be transformed into a slice,
	  ;; and then the data bits and references to
      ;; other cells from the cell can be obtained
	  ;; by loading them from the slice.
      ;; Data bits and references to other cells can be stored
	  ;; into a builder, and then the builder can be finalized into a new cell.
	  ;; recv_internal gets the slice
      ;; with incoming message data as an argument.

	  ;; As everything else on TON, permanent storage data is stored as a cell.
	  ;; It can be retrieved via the get_data() method
	  ;; begin_parse - converts a cell with data into a readable slice

	  slice ds = get_data().begin_parse(); 
      ;; `.` is a syntax sugar: a.b() is equivalent to b(a)

	  ;; load_uint is a function from the FunC standard library;
      ;; it loads an unsigned n-bit integer from a slice
	  int total = ds~load_uint(64); ;; `~` is a "modifying" method:
	  ;; essentially, it is a syntax sugar: `r = a~b(x)` 
      ;; is equivalent to (a,r) = b(a,x)
	  
	  ;; Now let’s read the incoming value from the message body slice
	  int n = in_msg_body~load_uint(32);

	  total += n;
      ;; integers support usual +-*/ operations as well as (+-*/)= syntax sugar

	  ;; In order to keep a store integer value, we need to do four things:
	  ;; create a Builder for the future cell - begin_cell()
	  ;; write a value to total - store_uint(value, bit_size)
	  ;; create a Cell from the Builder - end_cell()
	  ;; write the resulting cell into permanent storage - set_data()

	  set_data(begin_cell().store_uint(total, 64).end_cell());
	}



	;; The FunC program is essentially a list of
    function declarations/definitions and global variable declarations.

	;; Any function in FunC matches the following pattern:
	;; [<forall declarator>] <return_type> <function_name>(<comma_separated_function_args>) <specifiers>


	;; Specifiers:
	;; The impure specifier indicates that
    ;; function calls should not be optimized
    ;; (whether its result is used or not)
	;; it is important for methods that change the smart contract data
    ;; or send messages

	;; The method_id specifier allows you to call a GET function by name
	
	;; For instance, we can create a get method for the contract above
    ;; to allow outside viewers to read counter

	int get_total() method_id {
	  slice ds = get_data().begin_parse();
	  int total = ds~load_uint(64);

	  ;; Note that (int) and int is the same,
      ;; thus brackets in the function declaration
      ;; and in the return statement are omitted.
	  return total;
	}
	;; Now any observer can read get_total value via lite-client or explorer
```

# Messages

The actor model is a model of concurrent computation and is at the heart of TON smart contracts. Each smart contract can process one message at a time, change its own state, or send one or several messages. Processing of the message occurs in one transaction, that is, it cannot be interrupted. Messages to one contract are processed consequently one by one. As a result, the execution of each transaction is local and can be parallelized at the blockchain level, which allows for on-demand throughput horizontal scaling and hosting an unlimited number of users and transactions.

```c
;; For normal internal message-triggered transactions,
;; before passing control to recv_internal TVM puts the following
;; elements on stack.
;;;; Smart contract balance (in nanoTons)
;;;; Incoming message balance (in nanoTons)
;;;; Cell with an incoming message
;;;; Incoming message body, slice type
;; In turn, recv_internal may use only
;; the required number of fields (like 1 in the example above or 4 below)

;; Let’s dive into message sending

() recv_internal (
    int balance, int msg_value, cell in_msg_full, slice in_msg_body) {
    ;; 
    ;; Every message has a strict layout, thus by parsing it,
    ;; we can get the sender’s address
    ;; first, we need to read some tech flags and
    ;; then take the address using load_msg_addr
    ;; function from FunC standard library - ()
    var cs = in_msg_full.begin_parse();
    var flags = cs~load_uint(4);
    slice sender_address = cs~load_msg_addr();

    ;; if we want to send a message, we first need to construct it
    ;; message serialization in most cases may be reduced to
    var msg = begin_cell()
    .store_uint(0x18, 6) ;; tech flags
    .store_slice(addr)   ;; destination address
    .store_coins(amount) ;; attached value
    .store_uint(0, 107) ;; more tech flags :)
    .store_slice(in_msg_body) ;; just put some payload here
    .end_cell();

    ;; to send messages, use send_raw_message from the standard library.
    ;; it accepts two arguments message and mode
    send_raw_message(msg, 64);

    ;; mode parameter specifies how to process the funds passed into
    ;; the smart contract with the message and the smart contract funds
    ;; 64 means send everything from the incoming message — 
    ;; what’s left after the commission is deducted

    ;; Exceptions can be thrown by conditional primitives throw_if and
    ;; throw_unless and by unconditional throw
    ;; by default, it will automatically cause a bounce message with 64 mode

    var some  = 7;
    throw_if(102, some == 10);
    ;; Throw exception with code 102 conditionally
    throw_unless(103, some != 10);
    ;; Throw exception with code 103 conditionally
    throw(101);    ;; Throw exception with code 101 unconditionally
}
```

# Flow control: Conditional Statements and Loops; Dictionaries

```c
;; FunC, of course, supports if statements

;;;; usual if-else
if (flag) {
    ;;do_something();
}
else {
    ;;do_alternative();
}

;; If statements are often used as an operation identifier
;; for a smart contract, for example:

() recv_internal (
    int balance, int msg_value, cell in_msg_full, slice in_msg_body) {
    int op = in_msg_body~load_int(32);
    if (op == 1) {
    ;; smth here
    } else {
    if (op == 2) {
        ;; smth here
    } else {
        ;; smth here
    }
    }
}

;; Loops
;; FunC supports repeat, while and do { ... } until loops.
;; for loop is not supported.

;; repeat
int x = 1;
repeat(10) {
    x *= 2;
}
;; x = 1024

;; while
int x = 2;
while (x < 100) {
    x = x * x;
}
;; x = 256

;; until loops
int x = 0;
do {
    x += 3;
} until (x % 17 == 0);
;; x = 51

;; In practice, loops in TON smart contracts are often used to work with
;;  dictionaries, or as they are also called in TON hashmaps

;; A hashmap is a data structure represented by a tree.
;; Hashmap maps keys to values ​​of arbitrary type so that
;; quick lookup and modification are possible. 

;; udict_get_next? from FunC standard library in combination with
;; the loop will help, go through the dictionary

int key = -1;
do {
    (key, slice cs, int f) = dic.udict_get_next?(256, key);

} until (~ f);

;; udict_get_next? - Calculates the minimum key k in the dictionary dict
;; that is greater than some given value and returns k,
;; the associated value, and a flag indicating success.
;; If the dictionary is empty, returns (null, null, 0).
```

# Functions

```c
;; Most useful functions are slice reader and builder writer primitives,
;; storage handlers and sending messages

;; slice begin_parse(cell c) - Converts a cell into a slice
;; (slice, int) load_int(slice s, int len) - 
;; Loads a signed len-bit integer from a slice.
;; (slice, int) load_uint(slice s, int len) - 
;; Loads a unsigned len-bit integer from a slice.
;; (slice, slice) load_bits(slice s, int len) - 
;; Loads the first 0 ≤ len ≤ 1023 bits from slice into a separate slice.
;; (slice, cell) load_ref(slice s) - Loads the reference cell from the slice.

;; builder begin_cell() - Creates a new empty builder.
;; cell end_cell(builder b) - Converts a builder into an ordinary cell.
;; builder store_int(builder b, int x, int len) -
;; Stores a signed len-bit integer x into b for 0 ≤ len ≤ 257.
;; builder store_uint(builder b, int x, int len) -
;; Stores an unsigned len-bit integer x into b for 0 ≤ len ≤ 256.
;; builder store_slice(builder b, slice s) - Stores slice s into builder b.
;; builder store_ref(builder b, cell c) -
;; Stores a reference to cell c into builder b.

;; cell get_data() - Returns the persistent contract storage cell. 
;; () set_data(cell c) - Sets cell c as persistent contract data.

;; () send_raw_message(cell msg, int mode) -
;; put message msg into sending queue with mode.
;; Note, that message will be sent after a successful execution
;; of the whole transaction

;; Detailed descriptions of all standard functions can be found
;; in docs https://ton.org/docs/#/func/stdlib
;; 
```

## Additional resources
- [FunC Lessons](https://github.com/romanovichim/TonFunClessons_Eng)
- [TON Development Onboarding](https://www.tonspace.co)
- [TON Documentation](https://ton.org/docs/#/)
- [FunC Documentation](https://ton.org/docs/#/func/overview)
- [TON Smart Contracts examples](https://github.com/ton-blockchain/ton/tree/master/crypto/smartcont)
- [Community portal](https://society.ton.org)
- [Blockchain portal](https://ton.org)
- [Stackoverflow](https://stackoverflow.com/questions/tagged/ton)

## Social
- [Developer community](https://t.me/tondev_eng)
- [TON Learn](https://t.me/ton_learn)
- [FunC Lessons Channel](https://github.com/romanovichim/TonFunClessons_Eng)
- [FunC onboarding](https://t.me/func_guide)
- [Tondev News](https://t.me/tondevnews)

## Useful blogposts
- [Setting up a TON Development Environment](https://society.ton.org/setting-up-a-ton-development-environment)
- [Hello World on TON](https://society.ton.org/ton-hello-world-step-by-step-guide-for-writing-your-first-smart-contract-in-func)


## Future To Dos
- Add smart contracts examples
- Add more posts


This file is mostly copied from [TonFunClessons 15 minutes intro](https://github.com/romanovichim/TonFunClessons_Eng/blob/main/13lesson/15min.md).

P.S. If by any chance you're familiar with [Forth](https://learnxinyminutes.com/docs/forth/), you can also take a look at [Fift](https://ton-blockchain.github.io/docs/fiftbase.pdf).