---
language: "ARM Assembly"
filename: "ARMasm.s"
contributors:
  - ["Karan Panjabi", "https://github.com/karanpanjabi"]
---

<!-- Info about ARM and ARM assembly. -->
ARM (Acorn RISC Machine) is an architecture based on the Reduced Instruction Set (RISC) paradigm. Being based on RISC, it has lesser hardware complexity compared to CISC based contemporaries. However, this leaves the burden of complexity on compilers and assemblers.

With over 100 billion ARM processors produced as of 2017, ARM is the most widely used instruction set architecture and the instruction set architecture produced in the largest quantity.

[Read More](https://en.wikipedia.org/wiki/ARM_architecture)

<!-- TODO: Info about the three main types of instructions -->

<!-- TODO: Add some sample programs with those instructions -->

<!-- TODO: Add info on how to get the programs up and running -->

```asm
; Comments start with semicolon
; There are 3 basic types of instructions you could come across
; while programming in ARM:
; Data Processing   (register - register operations)
; Data Transfer     (memory - register operations)
; Control Flow

; Before moving on to actual code lets clear up some specifics

; For a 32 bit ARM system, there are sixteen 32-bit registers available
; for the user, wherein only R0-R12 are usually used for storing data 
; while programming, while R13 is the stack pointer, R14 is the link register 
; and stores the return address when you call a function/subroutine, 
; and R15 is the program counter (PC) and 
; holds the address of next instruction that has to be executed.

; There is also a special register known as CPSR 
; (Current Program Status Register) or commonly just called the status register. 
; Main thing to note about it is that it has four important bits, 
; N (negative), Z (zero), C (carry) and V (overflow). 
; It's use will become obvious while going through the code below.

;; Data Processing Instructions
;;   Moving values into registers
MOV R0, #5        ; moves immediate decimal 5 into R0
MOV R0, #0x05     ; moves immediate hexadecimal 5 into R0
MOV R0, R1        ; moves value of R1 to R0

;;   Arithmetic and Logical Instructions:

ADD R0, R1, R2    ; R0 = R1 + R2
SUB R0, R1, R2    ; R0 = R1 - R2
RSB R0, R1, R2    ; R0 = R2 - R1  (reverse subtraction)

AND R0, R1, R2    ; R0 = R1 & R2
ORR R0, R1, R2    ; R0 = R1 | R2
EOR R0, R1, R2    ; R0 = R1 ^ R2  (exclusive or)
BIC R0, R1, R2    ; R0 = R1 & (~ R2) (bit clear)

;;    Comparing values
CMP R0, #5  
CMP R0, R1      ; Compares the values of the two operands and stores the
; results in CPSR
; It subtracts R0 and R1 (R0 - R1) and sets the 
; appropriate flags in the status register (N, Z, C, V)

; For each of the above you can also modify the last operand
; if its a register

MOV R0, R1, LSL #5        ; logical shift left by 5 places
AND R0, R1, R2, LSR R3    ; logical shift right by value stored in R3
CMP R0, R1, ASR #21       ; arithmetic shift right by 31


; The best part about ARM is, every instruction is conditionally executable!
; So, instead of complex branching logic, you can keep your code pretty simple.




```