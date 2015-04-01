---
language: assembly
filename: learn.asm
contributors:
    - ["", ""]
---

Assembly language is a low-level programming language for a computer

```asm
; Comments

section .data
; it contains data, constants

; (variable-name) define-directive initial-value
byte_ db 0 ; 1 byte
word_ dw 0 ; 2 bytes
doubleword_ dw 0 ; 4 bytes
; string zero terminated
msg db "hello, world", 0
; array of short int
array dw 0, 1, 2, 3, 4

section .text
; it contains all the instructions to be executed

; where the program execution begins
global _start
; label the adress
_start:

; data registers
; 32b eax = 16b + 16b ax = 16b + 8b ah + 8b al
; eax accumulator
; ebx base
; ecx counter
; edx data
; pointer registers
; esp points to the top of the program stack
; ebp points to the base of the stack
; eip points to the next instruction to be executed
; index registers
; esi source index of strig operations
; edi destination index of string operations

; intel syntax
; mnemonic dest, src

; DATA TRANSFERT

nop ; do nothing

mov al, bl ; al = bl
mov cx, 0x1B
mov dx, word[word_] ; pointer-type[] get the pointed value
mov byte[doubleword_], 10 ; move 10 into the byte at adress doubleword_
mov ebx, word_
mov word[eax*4+ebx], cx ; you can add one value and multiply by 2,4,8 an adress

mov bl, -2 ; bl = 0xF1 = -2
movsx eax, bl ; eax = 0xFFFFFFF1 = -2
movsx eax, cx ; eax = 0x0000001B = 27
movzx eax, cx ; (movsx eax, cx)

mov esi, msg
mov edi, byte_
movsb ; [edi] = [esi] = "H"

bswap eax ; big endian to little endian
xchg eax, ebx ; exchange value between src and dest
lea eax, [ebx+4] ; eax = ebx+4

; ARITHMETIC

mov eax, 10
add eax, 15 ; eax += 15 = 25
stc ; CF = 1
adc eax, 20 ; eax += 20 + CF = 46

sub eax, eax ; eax -= eax = 0
sbb eax, 5 ; eax -= 5 + CF = -5 = 4294967291

mov edx, 7410
mul edx ; edx:eax = eax * edx = 4294967291 * 7410 = 0x0000740F:0xFFFDBBB0
imul edx ; edx:eax = -5 * 7410 = 0xFFFFFFFF:0XFFFDBBB0
imul edx, [word_] ; [word_]:edx = [word_] * edx
imul edx, [word_], 4 ; [word_]:edx = [word_] * 4

mov eax, -7 ; eax = -7 = 4294967289
xor edx, edx
mov ebx, 2
div ebx ; eax = eax / ebx = 4294967289 / 2 = 2147483644 ; edx = eax % ebx
xor edx, edx
idiv ebx

inc byte[byte_] ; [byte_]++
dec eax ; eax--
neg eax ; eax = -eax

; LOGICAL

and eax, ebx ; eax &= ebx
or eax, [doubleword_] ; eax |= [doubleword_]
xor [doubleword_], eax ; [doubleword_] ^= eax
not eax ; eax = !eax

mov al, 0b11001100
shr al, 1 ; 01100110(0) 
shl al, 2 ; (01)10011000
sar al, 3 ; 11110010(000)
sal al, 2 ; (11)11001000
ror al, 1 ; 10010001
rol al, 2 ; 01100100
stc ; CF = 1
rcr al, 3 ; 00(CF=1)01100 CF = 1
rcl al, 2 ; 101100(CF=1)0 CF = 0

; CONTROL FLOW

; flags register
; 16b = _ _ _ _ OF DF IF TF SF ZF _ AF _ PF _ CF

; overflow flag (OF): signed overflow
; jo jump if OF / jno
mov al, 0b01111111 ; 127
inc al ; 10000000 -128 OF = 1
sub al, 4 ; 01111100 124 OF = 1
; direction flag (DF): move left or right in string operations
std ; DF = 1
movsb ; esi++, edi++
cld ; DF = 0
movsb ; esi--, edi--
; sign flag (SF): the sign of the result of an arithmetic operation is negative
; js / jns
add al, 4 ; -128 SF = 1
; zero flag (ZF): the result of an arithmetic or comparison operation is zero
; jz / jnz
cmp al, 123 ; -128 + 123 = -5 != 0, ZF = 0
; carry flag (CF): overflow
; jc / jnc
mov al, 0b11111111 ; 255
inc al ; 00000000 256 CF = 1
sub al, 4 ; 11111100 252 CF = 1

; jmp loads EIP with the specified address
; jg >, jge >=, jl <, jle <=, je ==, jne !=
; unsigned ja >, jae >=, jb <, jbe <=

cmp al, 10 ; (al - 10) modify ZF, SF, CF, OF
test al, 10 ; (al & 10) modify flags
bt eax, 2 ; copy third bit to CF
; bts sets the bit, btr resets it, bt flips it

mov ecx, 5
_loop:
loop _loop ; ecx--, if ecx != 0 jump
loope _loop ; ecx--, if ecx != 0 && SF = 1 jump
; loopne _loop

sub esp, 8 ; allocate 8 bits on the stack
add esp, 8 ; free
push 10 ; (sub esp, 4, mov [esp], 10)
pop ebx ; (mov ebx, [esp], add esp, 4)
pushf ; push flags
popf
pusha ; push all registers
popa

xor ecx, ecx
rep movsb ; repeat string operation while ecx != 0, ecx--
; repe repz / repne repnz
movsb ; movsb, movsw, movsd, [edi] = [esi]
lodsb ; eax = [esi]
scasb ; cmp eax, [edi]
cmpsb ; cmp [esi], [edi]
stosb ; [edi] = eax

; CDECL calling convention

; _fct (2, 3)
push 3 ; pass arguments right to left
push 2
call _fct ; (push eip, jmp _fct) call a subroutine
add esp, 8 ; clean stack
jmp linux_system_calls

_fct:
push ebp ; save ebp
mov ebp, esp
mov eax, [ebp + 8] ; get arguments
mov edx, [ebp + 12]
add eax, edx ; return value in eax
pop ebp
ret ; (pop edx, jmp edx) return calling function

linux_system_calls:

; eax system call number
; 1 exit(int error_code)
; 3 read(uint fd, char *buf, int count)
; 4 write(uint fd, char *buf, int count)
; 5 open(char *filename, int flags, int mode)
; 6 close(uint fd)
; 8 creat(char *pathname, int mode)
; 13 time(int *tloc)
; 45 sys_brk(int brk) allocate memory

mov eax, 4 ; write
mov ebx, 1 ; stdout
mov ecx, msg
mov edx, 13
int 80h

mov eax, 1 ; exit
xor ebx, ebx
int 80h
```
