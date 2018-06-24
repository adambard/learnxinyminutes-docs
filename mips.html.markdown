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
  hello_world .asciiz           "Hello World\n" # Declare a null terminated string
  num1: .word 42                # Integers are referred to as words (32 bit value)
  arr1: .word 1, 2, 3, 4, 5     # Array of words
  arr2: .byte 'a', 'b'          # Array of chars (1 byte each)
  buffer: .space 60             # Allocates space in the RAM (not cleared to 0)

  # Datatype sizes
  _byte: .byte 'a'              # 1 byte
  _halfword: .half 53           # 2 bytes
  _word: .word 3                # 4 bytes
  _float: .float 3.14           # 4 bytes
  _double: .double 7.0          # 8 bytes


```