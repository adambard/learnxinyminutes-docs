---
language: "MIPS Assembly"
filename: MIPS.asm
contributors:
  - ["Stanley Lim", "https://github.com/Spiderpig86"]
---

The MIPS (Microprocessor without Interlocked Pipeline Stages) Assembly language
is designed to work with the MIPS microprocessor paradigm designed by J. L. 
Hennessy in 1981. These RISC processors are used in embedded systems such as 
gateways and routers.

[Read More](https://en.wikipedia.org/wiki/MIPS_architecture)

```asm
# Comments are denoted with a '#'

# Everything that occurs after a '#' will be ignored by the assembler's lexer.

# Programs typically contain a .data and .text sections

.data # Section where data is stored in memory (allocated in RAM), similar to
      # variables in higher-level languages

  # Declarations follow a ( label: .type value(s) ) form of declaration
  hello_world: .asciiz "Hello World\n"      # Declare a null terminated string
  num1: .word 42                            # Integers are referred to as words
                                            # (32-bit value)

  arr1: .word 1, 2, 3, 4, 5                 # Array of words
  arr2: .byte 'a', 'b'                      # Array of chars (1 byte each)
  buffer: .space 60                         # Allocates space in the RAM 
                                            # (not cleared to 0)

  # Datatype sizes
  _byte: .byte 'a'                          # 1 byte
  _halfword: .half 53                       # 2 bytes
  _word: .word 3                            # 4 bytes
  _float: .float 3.14                       # 4 bytes
  _double: .double 7.0                      # 8 bytes

  .align 2                                  # Memory alignment of data, where
                                            # number indicates byte alignment
                                            # in powers of 2. (.align 2
                                            # represents word alignment since
                                            # 2^2 = 4 bytes)

.text                                       # Section that contains 
                                            # instructions and program logic
.globl _main                                # Declares an instruction label as
                                            # global, making it accessible to
                                            # other files

  _main:                                    # MIPS programs execute 
                                            # instructions sequentially, where 
                                            # the code under this label will be
                                            # executed first

    # Let's print "hello world"
    la $a0, hello_world                     # Load address of string stored
                                            # in memory
    li $v0, 4                               # Load the syscall value (number
                                            # indicating which syscall to make)
    syscall                                 # Perform the specified syscall
                                            # with the given argument ($a0)

    # Registers (used to hold data during program execution)
    # $t0 - $t9                             # Temporary registers used for 
                                            # intermediate calculations inside 
                                            # subroutines (not saved across 
                                            # function calls)

    # $s0 - $s7                             # Saved registers where values are 
                                            # saved across subroutine calls. 
                                            # Typically saved in stack

    # $a0 - $a3                             # Argument registers for passing in 
                                            # arguments for subroutines
    # $v0 - $v1                             # Return registers for returning 
                                            # values to caller function

    # Types of load/store instructions
    la $t0, label                           # Copy the address of a value in
                                            # memory specified by the label
                                            # into register $t0
    lw $t0, label                           # Copy a word value from memory
    lw $t1, 4($s0)                          # Copy a word value from an address
                                            # stored in a register with an
                                            # offset of 4 bytes (addr + 4)
    lb $t2, label                           # Copy a byte value to the 
                                            # lower order portion of 
                                            # the register $t2
    lb $t2, 0($s0)                          # Copy a byte value from the source
                                            # address in $s0 with offset 0
    # Same idea with 'lh' for halfwords

    sw $t0, label                           # Store word value into
                                            # memory address mapped by label
    sw $t0, 8($s0)                          # Store word value into address 
                                            # specified in $s0 and offset of
                                            # 8 bytes
    # Same idea using 'sb' and 'sh' for bytes and halfwords. 'sa' does not exist

### Math ###
  _math:
    # Remember to load your values into a register
    lw $t0, num                             # From the data section
    li $t0, 5                               # Or from an immediate (constant)
    li $t1, 6
    add $t2, $t0, $t1                       # $t2 = $t0 + $t1
    sub $t2, $t0, $t1                       # $t2 = $t0 - $t1
    mul $t2, $t0, $t1                       # $t2 = $t0 * $t1
    div $t2, $t0, $t1                       # $t2 = $t0 / $t1 (Might not be 
                                            # supported in some versions of MARS)
    div $t0, $t1                            # Performs $t0 / $t1. Get the 
                                            # quotient using 'mflo' and 
                                            # remainder using 'mfhi'

    # Bitwise Shifting
    sll $t0, $t0, 2                         # Bitwise shift to the left with 
                                            # immediate (constant value) of 2
    sllv $t0, $t1, $t2                      # Shift left by a variable amount
                                            # in register
    srl $t0, $t0, 5                         # Bitwise shift to the right (does 
                                            # not sign preserve, sign-extends 
                                            # with 0)
    srlv $t0, $t1, $t2                      # Shift right by a variable amount 
                                            # in a register
    sra $t0, $t0, 7                         # Bitwise arithmetic shift to  
                                            # the right (preserves sign)
    srav $t0, $t1, $t2                      # Shift right by a variable amount 
                                            # in a register

    # Bitwise operators
    and $t0, $t1, $t2                       # Bitwise AND
    andi $t0, $t1, 0xFFF                    # Bitwise AND with immediate
    or $t0, $t1, $t2                        # Bitwise OR
    ori $t0, $t1, 0xFFF                     # Bitwise OR with immediate
    xor $t0, $t1, $t2                       # Bitwise XOR
    xori $t0, $t1, 0xFFF                    # Bitwise XOR with immediate
    nor $t0, $t1, $t2                       # Bitwise NOR

## BRANCHING ##
  _branching:
    # The basic format of these branching instructions typically follow <instr>
    # <reg1> <reg2> <label> where label is the label we want to jump to if the
    # given conditional evaluates to true
    # Sometimes it is easier to write the conditional logic backward, as seen
    # in the simple if statement example below

    beq $t0, $t1, reg_eq                    # Will branch to reg_eq if
                                            # $t0 == $t1, otherwise
                                            # execute the next line
    bne $t0, $t1, reg_neq                   # Branches when $t0 != $t1
    b branch_target                         # Unconditional branch, will 
                                            # always execute
    beqz $t0, req_eq_zero                   # Branches when $t0 == 0
    bnez $t0, req_neq_zero                  # Branches when $t0 != 0
    bgt $t0, $t1, t0_gt_t1                  # Branches when $t0 > $t1
    bge $t0, $t1, t0_gte_t1                 # Branches when $t0 >= $t1
    bgtz $t0, t0_gt0                        # Branches when $t0 > 0
    blt $t0, $t1, t0_gt_t1                  # Branches when $t0 < $t1
    ble $t0, $t1, t0_gte_t1                 # Branches when $t0 <= $t1
    bltz $t0, t0_lt0                        # Branches when $t0 < 0
    slt $s0, $t0, $t1                       # "Set on Less Than"
                                            # when $t0 < $t1 with result in $s0 
                                            # (1 for true)

    # Simple if statement
    # if (i == j)
    #     f = g + h;
    #  f = f - i;

    # Let $s0 = f, $s1 = g, $s2 = h, $s3 = i, $s4 = j
    bne $s3, $s4, L1 # if (i !=j)
    add $s0, $s1, $s2 # f = g + h

    L1:
      sub $s0, $s0, $s3 # f = f - i
    
    # Below is an example of finding the max of 3 numbers
    # A direct translation in Java from MIPS logic:
    # if (a > b)
    #   if (a > c)
    #     max = a;
    #   else
    #     max = c;
    # else
    #   if (b > c)
    #     max = b;
    #   else
    #     max = c;

    # Let $s0 = a, $s1 = b, $s2 = c, $v0 = return register
    ble $s0, $s1, a_LTE_b                   # if(a <= b) branch(a_LTE_b)
    ble $s0, $s2, max_C                     # if(a > b && a <=c) branch(max_C)
    move $v0, $s0                           # else [a > b && a > c] max = a
    j done                                  # Jump to the end of the program

    a_LTE_b:                                # Label for when a <= b
      ble $s1, $s2, max_C                   # if(a <= b && b <= c) branch(max_C)
      move $v0, $s1                         # if(a <= b && b > c) max = b
      j done                                # Jump to done

    max_C:
      move $v0, $s2                         # max = c

    done:                                   # End of program

## LOOPS ##
  _loops:
    # The basic structure of loops is having an exit condition and a jump 
    # instruction to continue its execution
    li $t0, 0
    while:
      bgt $t0, 9, end_while                 # While $t0 is less than 10, 
                                            # keep iterating
      #actual loop content would go here
      addi $t0, $t0, 1                      # Increment the value
      j while                               # Jump back to the beginning of 
                                            # the loop
    end_while:

    # 2D Matrix Traversal
    # Assume that $a0 stores the address of an integer matrix which is 3 x 3
    li $t0, 0                               # Counter for i
    li $t1, 0                               # Counter for j
    matrix_row:
      bgt $t0, 3, matrix_row_end

      matrix_col:
        bgt $t1, 3, matrix_col_end

        # Do stuff

        addi $t1, $t1, 1                  # Increment the col counter
      matrix_col_end:

      # Do stuff

      addi $t0, $t0, 1
    matrix_row_end:

## FUNCTIONS ##
  _functions:
    # Functions are callable procedures that can accept arguments and return 
    # values all denoted with labels, like above

    main:                                 # Programs begin with main func
      jal return_1                        # jal will store the current PC in $ra
                                          # and then jump to return_1

      # What if we want to pass in args?
      # First we must pass in our parameters to the argument registers
      li $a0, 1
      li $a1, 2
      jal sum                             # Now we can call the function

      # How about recursion?
      # This is a bit more work since we need to make sure we save and restore
      # the previous PC in $ra since jal will automatically overwrite 
      # on each call
      li $a0, 3
      jal fact

      li $v0, 10
      syscall
    
    # This function returns 1
    return_1:
      li $v0, 1                           # Load val in return register $v0
      jr $ra                              # Jump back to old PC to continue exec


    # Function with 2 args
    sum:
      add $v0, $a0, $a1
      jr $ra                              # Return

    # Recursive function to find factorial
    fact:
      addi $sp, $sp, -8                   # Allocate space in stack
      sw $s0, ($sp)                       # Store reg that holds current num
      sw $ra, 4($sp)                      # Store previous PC

      li $v0, 1                           # Init return value
      beq $a0, 0, fact_done               # Finish if param is 0

      # Otherwise, continue recursion
      move $s0, $a0                       # Copy $a0 to $s0
      sub $a0, $a0, 1
      jal fact

      mul $v0, $s0, $v0                   # Multiplication is done

      fact_done:
        lw $s0, ($sp)
        lw $ra, 4($sp)                     # Restore the PC
        addi $sp, $sp, 8

        jr $ra

## MACROS ##
  _macros:
    # Macros are extremely useful for substituting repeated code blocks with a
    # single label for better readability
    # These are in no means substitutes for functions
    # These must be declared before it is used

    # Macro for printing newlines (since these can be very repetitive)
    .macro println()
      la $a0, newline                     # New line string stored here
      li $v0, 4
      syscall
    .end_macro

    println()                             # Assembler will copy that block of
                                          # code here before running

    # Parameters can be passed in through macros.
    # These are denoted by a '%' sign with any name you choose
    .macro print_int(%num)
      li $v0, 1
      lw $a0, %num
      syscall
    .end_macro
    
    li $t0, 1
    print_int($t0)
    
    # We can also pass in immediates for macros
    .macro immediates(%a, %b)
      add $t0, %a, %b
    .end_macro

    immediates(3, 5)

    # Along with passing in labels
    .macro print(%string)
      la $a0, %string
      li $v0, 4
      syscall
    .end_macro

    print(hello_world)

## ARRAYS ##
.data
  list: .word 3, 0, 1, 2, 6                 # This is an array of words
  char_arr: .asciiz "hello"                 # This is a char array
  buffer: .space 128                        # Allocates a block in memory, does
                                            # not automatically clear
                                            # These blocks of memory are aligned
                                            # next to each other

.text
  la $s0, list                              # Load address of list
  li $t0, 0                                 # Counter
  li $t1, 5                                 # Length of the list

  loop:
    bge $t0, $t1, end_loop

    lw $a0, ($s0)
    li $v0, 1
    syscall                                 # Print the number

    addi $s0, $s0, 4                        # Size of a word is 4 bytes
    addi $t0, $t0, 1                        # Increment
    j loop
  end_loop:

## INCLUDE ##
# You do this to import external files into your program (behind the scenes, 
# it really just takes whatever code that is in that file and places it where
# the include statement is)
.include "somefile.asm"
```
