---
language: "MIPS"
filename: MIPS.mips
contributors:
  - ["Stanley Lim", "https://github.com/Spiderpig86"]
---

The MIPS (Microprocessor without Interlocked Pipeline Stages) Assembly language is designed to work with the MIPS microprocessor paradigm designed by J. L. Hennessy in 1981. These RISC processors are used in embedded systems such as gateways and routers.

[Read More](https://en.wikipedia.org/wiki/MIPS_architecture)

```assembly
# Comments are denoted with a '#'

# Everything that occurs after a '#' will be ignored by the assembler's lexer.

# Programs typically contain a .data and .text sections

.data # Section where data is stored in memory (allocated in RAM), similar to variables in higher level languages

  # Declarations follow a ( label: .type value(s) ) form of declaration
hello_world .asciiz "Hello World\n"         # Declare a null terminated string
  num1: .word 42                            # Integers are referred to as words (32 bit value)
  arr1: .word 1, 2, 3, 4, 5                 # Array of words
  arr2: .byte 'a', 'b'                      # Array of chars (1 byte each)
  buffer: .space 60                         # Allocates space in the RAM (not cleared to 0)

  # Datatype sizes
  _byte: .byte 'a'                          # 1 byte
  _halfword: .half 53                       # 2 bytes
  _word: .word 3                            # 4 bytes
  _float: .float 3.14                       # 4 bytes
  _double: .double 7.0                      # 8 bytes

  .align 2                                  # Memory alignment of data, where number indicates byte alignment in powers of 2. (.align 2 represents word alignment since 2^2 = 4 bytes)

.text                                       # Section that contains instructions and program logic
.globl _main                                # Declares an instruction label as global, making it accessible to other files

  _main:                                    # MIPS programs execute instructions sequentially, where the code under this label will be executed firsts

    # Let's print "hello world"
    la $a0, hello_world                     # Load address of string stored in memory
    li $v0, 4                               # Load the syscall value (indicating type of functionality)
    syscall                                 # Perform the specified syscall with the given argument ($a0)

    # Registers (used to hold data during program execution)
    # $t0 - $t9                             # Temporary registers used for intermediate calculations inside subroutines (not saved across function calls)
    # $s0 - $s7                             # Saved registers where values are saved across subroutine calls. Typically saved in stack
    # $a0 - $a3                             # Argument registers for passing in arguments for subroutines
    # $v0 - $v1                             # Return registers for returning values to caller function

    # Types of load/store instructions
    la $t0, label                           # Copy the address of a value in memory specified by the label into register $t0
    lw $t0, label                           # Copy a word value from memory
    lw $t1, 4($s0)                          # Copy a word value from an address stored in a register with an offset of 4 bytes (addr + 4)
    lb $t2, label                           # Copy a byte value to the lower order portion of the register $t2
    lb $t2, 0($s0)                          # Copy a byte value from the source address in $s0 with offset 0
    # Same idea with 'lh' for halfwords

    sw $t0, label                           # Store word value into memory address mapped by label
    sw $t0, 8($s0)                          # Store word value into address specified in $s0 and offset of 8 bytes
    # Same idea using 'sb' and 'sh' for bytes and halfwords. 'sa' does not exist

### Math ###
  _math:
    # Remember to load your values into a register
    lw $t0, num # From the data section
    li $t0, 5 # Or from an immediate (constant)
    li $t1, 6
    add $t2, $t0, $t1 # $t2 = $t0 + $t1

```