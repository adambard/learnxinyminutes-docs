---
name: WDS
filename: learnwds.wds
contributors:
    - ["Damarion Abendanon", "https://damarion.me/"]
---

WinDbg Script (WDS) is an integrated scripting language built into Windows'
native debugger. It is commonly used by low-level developers to inspect and
monitor machine state at the binary level. While the engine supports other
scripting methods, such as [NatVis], this cheatsheet primarily focuses on
native WDS, which has the best compatibility with older systems.

[NatVis]: https://learn.microsoft.com/en-us/windows-hardware/drivers/debugger/native-debugger-objects-in-natvis
```wds
$$$
>>> FUNDAMENTALS
$$$

$$ single-line comment
$$$
    multi-line
    comment...
$$$

$$ script execution
$$><C:\path\to\script.wds          $$ execute without arguments
$$>a<C:\path\to\script.wds <arg1>  $$ execute with arguments
$$><C:\a.wds; $$><C:\b.wds         $$ chain execution with semicolons

$$$
>>> ARITHMETIC
$$$

$$ primitives
0t17 $$ octal
0n10 $$ decimal
0x1A $$ hexadecimal (default)
8    $$ ^

$$ assignment
r @eax = 0xFF $$ <null>
r @eax        $$ 0xFF

$$ evaluation (WDS-style)
? 3 + 3 $$ 6
? 5 - 8 $$ -3 (ffffffff`fffffffd)
? 9 * 2 $$ 18

$$ evaluation (C++-style)
?? int(5.5)           $$ 5
?? (float)(8 % 2) + 1 $$ 1
?? (char)65           $$ 'A'

$$ logical (WDS-style)
? 5 and 5 $$ 1
? 0 or 0  $$ 0
? not 0   $$ 1

$$ logical (C++-style)
? 5 && 5 $$ true
? 0 || 0 $$ false
? !0     $$ true

$$ comparative
? 5 == 4  $$ 0 (eq)
? 4 > 2   $$ 1 (gt)
? 1 != 4  $$ 1 (neq)
? 0 <= -1 $$ 0 (lte)

$$ bitwise
? 5 & 5 $$ 5 (and)
? 0 | 0 $$ 0 (or)
? 0 ^ 0 $$ 0 (xor)
? ~0    $$ ffffffff`ffffffff (not)

$$$
>>> VARIABLES
$$$

r $t0 = 5           $$ <null>
r $t1 = 4           $$ <null>
r $t8 = @$t0 + @$t1 $$ <null> (use '@' when reading)
r $t8               $$ 9

$$ NOTE: maximum is "$t19"

$$$
>>> CONTROL-FLOW
$$$

$$ conditionals
.if (@eax == 0) {
    $$ eax is zero
}

.elsif (@eax > 0 and (@eax < 2)) {
    $$ eax is one
}

.else {
    $$ eax is anything else
}

$$ loops
.while (@$t0 < 5) {
    r $t0 = @$t0 + 1
}

.do {
    r $t0 = @$t0 + 1
} .while (@$t0 < 4)

.for (r $t0 = 0; @$t0 < 100; r $t0 = @$t0 + 1) {
    .if (@$t0 % 2 == 0) {
        .continue
    }
    .if (@$t0 == 50) {
        .break
    }
}

$$ iterate over each line of lm output, print the token
.foreach (mod { lm1m }) {
    .echo ${mod}
}

$$$
>>> ALIASES
$$$

$$ fixed
as MYALIAS1 .echo hello from alias
MYALIAS1 $$ invocation

$$ typed
aS SCRATCH 0x1234
dd ${SCRATCH}  $$ dd 0x1234

$$ manage
al          $$ list all aliases
ad MYALIAS1 $$ delete alias by name
ad *        $$ delete all aliases

$$$
>>> OUTPUT
$$$

$$ standard
.echo hello world

$$ formatted
.printf "eax = %p\n", @eax
.printf "%ma\n", 0x12345678 $$ deref as ascii string
.printf "%mu\n", 0x12345678 $$ deref as unicode string

$$ redirect output to file
.logopen C:\log.txt
.echo logging...
.logclose


$$$
>>> MEMORY
$$$

$$ display (read) — show memory contents at address
db 0x1234              $$ byte
db 0x1234 L4           $$ bytes (amount: 4)
da 0x1234              $$ ascii
dw 0x1234              $$ word  (short)
dd 0x1234              $$ dword (int)
dq 0x1234              $$ qword (long)
dt nt!_EPROCESS 0x1234 $$ struct

$$ pointers
poi(addr)  $$ ptr   read  (pointer-sized: 4 bytes on x86, 8 bytes on x64)
by(addr)   $$ byte  read
wo(addr)   $$ word  read  (2 bytes)
dwo(addr)  $$ dword read  (4 bytes)
qwo(addr)  $$ qword read  (8 bytes)

$$ searching
s -b 0x1234 L?0x5000 0A 0B  $$ search bytes 0A 0B (range 0x1234-0x6234)
s -a 0 L?0xFFFF "hello"     $$ search string "hello" (range 0x0-0xFFFF)
s -[1]b 0 L?0xFFFF 0x90     $$ search bytes, return addresses only (not dumps)

$$ edit (write) — modify memory at address
eb 0x1234 41               $$ byte
ea 0x1234 "A"              $$ ascii
ew 0x1234 4141             $$ word  (short)
ed 0x1234 41414141         $$ dword (int)
eq 0x1234 4141414141414141 $$ qword (long)

$$$
>>> BREAKPOINTS
$$$

$$ software
bp nt!NtCreateFile $$ by symbol
bp 01234           $$ by address
bu nt!NtCreateFile $$ unresolved (survives module reload)
bm nt!Nt*          $$ pattern match (sets multiple bp)
bm /a nt!Nt*       $$ allow match even if too many symbols resolve

$$ hardware
ba e1 0x1234 $$ execute,    1 byte
ba r4 0x1234 $$ read/write, 4 bytes
ba w8 0x1234 $$ write only, 8 bytes

$$ with commands
bp nt!NtCreateFile "k; g"                   $$ print stack, proceed
bp nt!NtCreateFile ".if (@rcx == 0) { g }"  $$ skip if rcx == 0

$$ management
bl    $$ list breakpoints
bd 0  $$ disable breakpoint
be 0  $$ enable breakpoint
bc 0  $$ clear breakpoint
bc *  $$ clear all breakpoints

$$$
>>> REGISTERS
$$$

$$ general-purpose
@rax; @eax; @ax; @ah; @al
@rbx; @ebx; @bx; @bh; @bl
@rcx; @ecx; @cx; @ch; @cl
@rdx; @edx; @dx; @dh; @dl
@rsi; @esi; @si; @sil
@rdi; @edi; @di; @dil
@rbp; @ebp; @bp; @bpl
@rsp; @esp; @sp; @spl

@r8;  @r8d;  @r8w;  @r8b
@r9;  @r9d;  @r9w;  @r9b
@r10; @r10d; @r10w; @r10b
@r11; @r11d; @r11w; @r11b
@r12; @r12d; @r12w; @r12b
@r13; @r13d; @r13w; @r13b
@r14; @r14d; @r14w; @r14b
@r15; @r15d; @r15w; @r15b

@xmm0;  @xmm1;  @xmm2;  @xmm3
@xmm4;  @xmm5;  @xmm6;  @xmm7
@xmm8;  @xmm9;  @xmm10; @xmm11
@xmm12; @xmm13; @xmm14; @xmm15

@ymm0;  @ymm1;  @ymm2;  @ymm3
@ymm4;  @ymm5;  @ymm6;  @ymm7
@ymm8;  @ymm9;  @ymm10; @ymm11
@ymm12; @ymm13; @ymm14; @ymm15

$$ control-flow
@rip; @eip
@rflags; @eflags

$$ segment & debug
@cs; @ds; @es; @fs; @gs; @ss
@dr0; @dr1; @dr2; @dr3; @dr6; @dr7

$$$
>>> PSEUDO-REGISTERS
$$$

$$ execution
@$ip          $$ instruction pointer (eip/rip)
@$eventip     $$ ip at last event
@$previp      $$ ip before current event
@$relip       $$ ip relative to module base
@$eventipl    $$ low 32 bits of $eventip
@$eventipf    $$ flat address of $eventip
@$scopeip     $$ ip of the current scope (local context in .frame / .catch)

$$ stack
@$csp         $$ stack pointer (esp/rsp)
@$ra          $$ return address
@$retreg      $$ return value register (eax/rax)
@$retreg64    $$ 64-bit return value (x86: edx:eax pair; x64: rax)
@$callret     $$ return value after .call
@$frame       $$ current stack frame index

$$ processes and threads
@$proc         $$ _EPROCESS address
@$thread       $$ _ETHREAD address
@$teb          $$ teb address
@$peb          $$ peb address
@$tpid         $$ current pid
@$tid          $$ current tid
@$exentry      $$ process entry point
@$numprocesses $$ number of processes
@$numthreads   $$ number of threads

$$ memory
@$ea          $$ effective address of memory operand
@$ea2         $$ effective address of second memory operand
@$eventea     $$ effective address at last event
@$p           $$ current poi() value
@$ptrsize     $$ pointer size (4 or 8)
@$pagesize    $$ page size (usually 0x1000)

$$ breakpoints
@$bpaddr      $$ last breakpoint address
@$bp0-@$bp9   $$ breakpoint addresses 0-9
```

## Further Reading

This cheatsheet covers WDS' most common usage patterns (typically in
user-mode), emphasizing logical constructs over exhaustive function
coverage. For a full reference of the engine's built-in functions, see
[the official documentation][wds-docs].

For a deeper understanding, the [Windows Internals][windows-internals] book
remains the definitive resource on WinDbg scripting and OS-level
interactions.

[wds-docs]: https://learn.microsoft.com/en-us/windows-hardware/drivers/debuggercmds/meta-commands
[windows-internals]: https://learn.microsoft.com/en-us/sysinternals/resources/windows-internals