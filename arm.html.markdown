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

;;    Arithmetic and Logical Instructions:

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


;; TODO: Add MUL instructions here



;; Data Transfer Instructions
; These instructions are used to tranfer data to-from memory and registers

;;    Single Word/Byte/Half word transfers
; Loading a word from memory
LDR R0, [R1]        ; Loads a word from address stored in R1

; How do we get R1 to store an address to some word in memory?
; This is where the concept of data segment and text segment comes into picture
; The data segment in memory is supposed to store all the global variables 
; that you'd like to declare in your program, while the text segment
; holds the instructions in your program.

; So, normally a program would look something like the following:

.DATA
varidentifier: .WORD 0x12345678   ; This is a 32 bit word with varidentifier
; as the label

.TEXT
LDR R1, =varidentifier  ; Using =label, the corresponding register stores
; the address of the word above
LDR R0, [R1]  ; Coming back to loading a word from memory
SWI 0x011     ; software interrupt; used to stop the program
; we'll do software interrupts later

; Now after making some changes we can store back the word:
STR R0, [R1]  ; Stores the word in R0, to the memory location pointed by R1

; Addressing modes:
; These addressing modes are very useful for accessing consecutive blocks
; of memory
; Pre-index (without writeback)
LDR R0, [R1, #4]  ; R0 = memory[R1 + #4]
LDR R0, [R1, R2, LSL #2]  ; R0 = memory[R1 + R2 * 2^2]

; Auto-indexing (Pre-index with writeback)
LDR R0, [R1, #4]!  ; R0 = memory[R1 + #4] and R1 = R1 + #4

; Post-indexing
LDR R0, [R1], #4  ; R0 = memory[R1] and R1 = R1 + #4

; We also have load-byte, store-byte, load-half word, store-half word
LDRH R0, [R1]
STRH R0, [R1]
LDRB R0, [R1] ; mostly used for accessing characters
STRB R0, [R1]

;; Multiple word transfers
; There are 4 main suffixes here that you can use
; IA: increment after accessing
; IB: increment before accessing
; DA: decrement after accessing
; DB: decrement before accessing
LDMIA R0, {R1-R5, R7, R8}   ; The order in which registers are specified
; does not matter, lower one gets the data in lower memory address
; R0 is the source register which holds the base memory address
; So, R1 gets mem[R0], R2 = mem[R0 + 4] and so on

; So if you wanted to copy an entire array you could just write 2 lines
; (if array isn't large enough)

.DATA
srcarr: .WORD #10, #30, #50, #23
dstarr: .WORD #0

.TEXT
LDR R0, =srcarr
LDR R1, =dstarr
LDMIA R0, {R2-R5}
STMIA R1, {R2-R5}
SWI 0x011

;; TODO: show how one could operate with stack



;; Control Flow Instructions
; The two main control flow instructions are
; B (branch) and BL (branch with link)

; Take the following high level code which adds 1st 10 numbers
; c = 0;
; sum = 0;
; while(c != 10)
; {
;   sum = sum + (c+1);
;   c = c + 1;
; }

; Equivalent arm code using branch

MOV R0, #0  ; c
MOV R1, #0  ; sum

LOOP:
CMP R0, #10
BEQ END   ; Branch if equal, to END label

ADD R2, R0, #1  ; c+1
ADD R1, R1, R2

B LOOP    ; Branch to LOOP label

END:
SWI 0x011

; The best part about ARM is, every instruction is conditionally executable!
; So, instead of complex branching logic, you can keep your code pretty simple.

; All conditional codes refer to the content in CPSR before executing
; The 'EQ' used above (BEQ) was a conditional code.
; More conditional codes available at ARM Infocenter
; Link below

; For example, take the following code:
; if(R0 != 8) 
; {
;   R1 = R1 + R0 - R2
; }

; We could write the above in 2 ways

CMP R0, #8
BEQ endif
ADD R1, R1, R0
SUB R1, R1, R2

endif:
...

; or a simpler one

CMP R0, #8
ADDNE R1, R1, R0
SUBNE R1, R1, R2


```

ARM Infocenter Conditional Codes: [Link](http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0204j/Chdhcfbc.html)