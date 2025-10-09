---
name: Move
filename: learnmove.move
contributors:
    - ["Melonges", "https://github.com/melonges"]
---

Move is an open-source programming language for developing smart contracts and custom transactions on the blockchain. It was originally developed by Facebook for the Diem blockchain, but is now used by other blockchains like Aptos and Sui.

```move
module 0x1::Example;

    fun main() {
        let a: u8 = 10; // 8-bit unsigned integer
        let b: u64 = 1000; // 64-bit unsigned integer
        let c: u128 = 1000000000000000000000000000000000; // 128-bit unsigned integer

        const MAX_SUPPLY: u64 = 1_000_000; // Constant for maximum supply
        const ERROR_INVALID_AMOUNT: u64 = 100; // Constant for invalid amount error code


        // Arithmetic operations
        let sum = a + 5; // sum is 15
        let product = b * 2; // product is 2000

        // Attempting an overflow will cause a runtime error
        // let overflow = 250u8 + 10u8; // This would abort

        let is_active: bool = true; // Boolean value set to true
        let has_funds: bool = false; // Boolean value set to false

        let owner_address: address = @0x1; // Address literal
        let contract_address: address = @0xCAFE; // Address literal

        // Addresses can be compared for equality
        let are_same = owner_address == contract_address; // false

        let x = 10;
        let ref_x = &x; // Immutable reference
        let mut_x = &mut x; // Mutable reference

        // Dereferencing a mutable reference to change the value
        *mut_x = 20;

        // The original x is now 20


        let my_tuple = (10u64, true, @0x1); // Creating a tuple

        let (a, b, c) = my_tuple; // Destructuring a tuple

        let x = 10; // x is in scope here
        {
            let y = 20; // y is in scope here
            // x is also in scope here
        }
        // y is no longer in scope here


        let a = 10u64;
        let b = 10u64;
        let c = 20u64;

        let eq1 = a == b; // true
        let eq2 = a == c; // false
        let neq1 = a != c; // true


        let value = 50u64;

        // Abort if value is less than 100
        if (value < 100) {
            abort 1001; // Abort with error code 1001
        };

        // Assert that value is greater than 0
        assert(value > 0, 1002); // Abort with error code 1002 if false


        let x = 10;

        if (x > 5) {
            // x is greater than 5
        } else {
            // x is not greater than 5
        };

        let i = 0;
        while (i < 5) {
            // Do something
            i = i + 1;
        };

        loop {
            // Do something indefinitely
            if (i >= 10) {
                break;
            };
            i = i + 1;
        };
    }

    public fun add(a: u64, b: u64): u64 {
        a + b // Returns the sum of a and b
    }

```
## Further Reading
*   [The Move Book](https://move-language.github.io/move/)
*   [Sui Move](https://sui.io/move)
*   [Aptos Move](https://aptos.dev/build/smart-contracts)

