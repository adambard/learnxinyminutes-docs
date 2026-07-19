---
name: C-Next
filename: learncnext.cnx
contributors:
  - ["Joshua Austill", "https://github.com/jlaustill"]
---

C-Next is a safer C for embedded systems. It transpiles to clean, readable C.

```c
// Single-line comments work like C99
/* Multi-line comments too */

// =============================================================================
// 1. BASICS
// =============================================================================

// Fixed-width types (no platform surprises)
u8  byte;       // uint8_t  (0 to 255)
u16 word;       // uint16_t (0 to 65535)
u32 dword;      // uint32_t
u64 qword;      // uint64_t
i8  sbyte;      // int8_t   (-128 to 127)
i16 sword;      // int16_t
i32 sdword;     // int32_t
i64 sqword;     // int64_t
f32 single;     // float
f64 dbl;        // double
bool flag;      // bool

// Assignment uses <- (not =)
u32 x <- 42;

// Comparison uses = (not ==)
if (x = 42) { }     // "x equals 42"
if (x != 0) { }     // "x not equal to 0"

// Variables are zero-initialized by default
u32 counter;        // counter = 0, not garbage!

// =============================================================================
// 2. OPERATORS
// =============================================================================

// Arithmetic (same as C)
x <- a + b;
x <- a - b;
x <- a * b;
x <- a / b;
x <- a % b;

// Compound assignment uses arrow
x +<- 1;            // x = x + 1
x -<- 1;            // x = x - 1
x *<- 2;            // x = x * 2
x &<- mask;         // x = x & mask
x |<- flags;        // x = x | flags
x <<<- 1;           // x = x << 1

// Bitwise (same as C)
x <- a & b;         // AND
x <- a | b;         // OR
x <- a ^ b;         // XOR
x <- ~a;            // NOT
x <- a << 2;        // Left shift
x <- a >> 2;        // Right shift

// Logical (same as C)
if (a && b) { }     // AND
if (a || b) { }     // OR
if (!a) { }         // NOT

// Ternary - parentheses required, must be boolean comparison
u32 max <- (a > b) ? a : b;     // OK
// u32 y <- x ? 1 : 0;          // ERROR: x is not boolean
// u32 z <- (x) ? 1 : 0;        // ERROR: still not boolean
// Nested ternary is forbidden - use if/else instead

// =============================================================================
// 3. FUNCTIONS
// =============================================================================

// Basic function
void doNothing() {
}

// Parameters and return
u32 add(u32 a, u32 b) {
    return a + b;
}

// Structs passed by reference automatically (no pointers needed)
struct Point { i32 x; i32 y; }

void movePoint(Point p, i32 dx, i32 dy) {
    p.x +<- dx;     // Modifies original!
    p.y +<- dy;
}

// Define-before-use is enforced (no forward declarations)
void helper() { }

void main() {
    helper();       // OK - helper defined above
    // other();     // ERROR: 'other' not defined yet
}

void other() { }

// =============================================================================
// 4. CONTROL FLOW
// =============================================================================

// If/else (braces required)
if (x > 0) {
    doSomething();
} else if (x < 0) {
    doOther();
} else {
    doDefault();
}

// While loop
while (running) {
    process();
}

// For loop
for (u32 i <- 0; i < 10; i +<- 1) {
    buffer[i] <- 0;
}

// Do-while (condition must be boolean comparison)
u8 byte;
do {
    byte <- readByte();
} while (byte != END_MARKER);   // OK: comparison
// } while (byte);              // ERROR: must be boolean

// Switch - braces replace break, no fallthrough, no colons!
switch (state) {
    case State.IDLE {
        startMotor();
    }
    case State.RUNNING {
        checkSensors();
    }
    default {
        handleError();
    }
}

// Multiple cases with || syntax
switch (cmd) {
    case Command.READ || Command.PEEK {
        readData();
    }
    case Command.WRITE {
        writeData();
    }
}

// No break/continue - use structured conditions
// Instead of: while (true) { if (done) break; }
while (!done) {
    process();
}

// =============================================================================
// 5. ARRAYS
// =============================================================================

// Fixed-size arrays
u8[256] buffer;
buffer[0] <- 0xFF;

// .element_count property (ADR-058)
usize len <- buffer.element_count;  // 256

// Array initialization uses [] not {}
u8[5] data <- [1, 2, 3, 4, 5];   // Size 5
u8[100] zeros <- [0*];          // All 100 elements = 0
u8[50] ones <- [1*];            // All 50 elements = 1

// Partial init forbidden (MISRA 9.3)
// u8[5] bad <- [1, 2, 3];      // ERROR: 3 elements for size-5

// Multi-dimensional arrays
u8[4][8] matrix;
matrix[0][0] <- 1;
u32 rows <- matrix.element_count;   // 4
u32 cols <- matrix[0].element_count; // 8

// =============================================================================
// 6. STRINGS
// =============================================================================

// Bounded strings with capacity
string<64> name <- "Hello";     // 64 chars max, transpiles to char[65]
string<128> buffer;             // Empty string

// Properties
u32 len <- name.char_count;     // Runtime: strlen(name) = 5
u32 cap <- name.capacity;       // Compile-time: 64

// Comparison uses =
string<32> a <- "Hello";
string<64> b <- "Hello";
if (a = b) {                    // strcmp(a, b) == 0
    // Equal content
}

// Concatenation with capacity validation
string<64> result <- a + " World";  // OK: 64 >= 32 + 6

// Substring extraction
string<5> hello <- name[0, 5];  // "Hello"

// =============================================================================
// 7. TYPE SAFETY
// =============================================================================

// Widening conversions (implicit, always safe)
u8 small <- 42;
u32 large <- small;             // u8 -> u32: OK

// Narrowing conversions (FORBIDDEN - use bit indexing)
u32 bigValue <- 0xDEADBEEF;
// u8 byte <- bigValue;         // ERROR: narrowing forbidden
u8 lowByte <- bigValue[0, 8];   // OK: explicit bit extraction

// Sign conversions (FORBIDDEN - use bit indexing)
i32 signedVal <- -100;
// u32 unsigned <- signedVal;   // ERROR: sign change forbidden
u32 asBits <- signedVal[0, 32]; // OK: explicit reinterpret

// =============================================================================
// 8. OVERFLOW BEHAVIOR
// =============================================================================

// clamp - saturating arithmetic (safe default)
clamp u8 brightness <- 200;
brightness +<- 100;             // Clamps to 255, not 44!

// wrap - two's complement wrapping (opt-in)
wrap u32 counter <- 0;
counter +<- 1;                  // Wraps at UINT32_MAX

// No modifier = clamp (safe default)
u16 temperature <- 0;
temperature -<- 100;            // Clamps to 0, not 65436!

// =============================================================================
// 9. BIT MANIPULATION
// =============================================================================

// Type-aware bit indexing
u8 flags <- 0;
flags[0] <- true;               // Set bit 0
flags[3] <- true;               // Set bit 3
bool isSet <- flags[0];         // Read bit 0

// Multi-bit fields
flags[4, 3] <- 5;               // Set 3 bits starting at bit 4
u8 field <- flags[4, 3];        // Read 3-bit field

// .bit_length on integers gives bit width
u8 w8 <- flags.bit_length;          // 8
u32 w32 <- counter.bit_length;      // 32

// =============================================================================
// 10. STRUCTS
// =============================================================================

// Struct declaration
struct Point {
    i32 x;
    i32 y;
}

// Zero-initialized by default
Point origin;                   // x=0, y=0

// Named field initializer (type inferred)
Point p <- { x: 10, y: 20 };

// Member access
p.x <- 100;
i32 y <- p.y;

// Nested structs (always named, no anonymous)
struct Rectangle {
    Point topLeft;
    Point bottomRight;
}

Rectangle bounds <- {
    topLeft: { x: 10, y: 20 },
    bottomRight: { x: 110, y: 120 }
};

// =============================================================================
// 11. ENUMS
// =============================================================================

enum State {
    IDLE,               // 0
    RUNNING,            // 1
    ERROR <- 255        // Explicit value
}

State current <- State.IDLE;

if (current = State.RUNNING) {
    // ...
}

// =============================================================================
// 12. BITMAPS
// =============================================================================

// Portable bit-packed types (guaranteed LSB-first ordering)
bitmap8 MotorFlags {
    Running,            // bit 0 (1 bit)
    Direction,          // bit 1 (1 bit)
    Fault,              // bit 2 (1 bit)
    Mode[3],            // bits 3-5 (3 bits)
    Reserved[2]         // bits 6-7 (2 bits)
}
// Total: 1+1+1+3+2 = 8 bits (must match bitmap8)

MotorFlags flags <- 0;
flags.Running <- true;          // Set single bit
flags.Mode <- 5;                // Set multi-bit field (0-7)
bool isRunning <- flags.Running;
u8 mode <- flags.Mode;

// =============================================================================
// 13. SCOPES
// =============================================================================

// Organize code with automatic name prefixing
// scopes are purely optional in c-next
scope LED {
    const u32 BIT <- 3;

    public void on() {
        GPIO.DR_SET[BIT] <- true;
    }

    public void off() {
        GPIO.DR_CLEAR[BIT] <- true;
    }
}

// Usage: dot syntax
LED.on();
LED.off();

// Generates: LED_on(), LED_off()

// =============================================================================
// 14. REGISTER BINDINGS
// =============================================================================

// Type-safe hardware access
register GPIO7 @ 0x42004000 {
    DR:         u32 rw @ 0x00,  // Read-Write
    GDIR:       u32 rw @ 0x04,  // Direction
    PSR:        u32 ro @ 0x08,  // Read-Only
    DR_SET:     u32 wo @ 0x84,  // Write-Only atomic set
    DR_CLEAR:   u32 wo @ 0x88,  // Write-Only atomic clear
}

u32 data <- GPIO7.DR;           // Read
GPIO7.DR <- 0xFF;               // Write
GPIO7.DR_SET[3] <- true;        // Set bit 3 (atomic)

// =============================================================================
// 15. ATOMIC & CRITICAL SECTIONS
// =============================================================================

// Atomic variables - ISR-safe
atomic u32 counter <- 0;

void increment() {
    counter +<- 1;              // Lock-free on Cortex-M3+
}

// Critical sections for multi-variable operations
u8[64] buffer;
u32 writeIdx <- 0;

void enqueue(u8 data) {
    critical {
        buffer[writeIdx] <- data;
        writeIdx +<- 1;
    }
}

// =============================================================================
// 16. CONSTANTS & PREPROCESSOR
// =============================================================================

// Use const instead of #define for values
const u32 BUFFER_SIZE <- 256;
const f32 PI <- 3.14159;

// #define only for flags (conditional compilation)
#define ARDUINO
#define DEBUG

#ifdef ARDUINO
// Arduino-specific code
#endif

// #define with values is FORBIDDEN
// #define MAX_SIZE 100       // ERROR: use const

// =============================================================================
// 17. COMPLETE EXAMPLE
// =============================================================================

#include <Arduino.h>

register GPIO7 @ 0x42004000 {
    DR_SET:     u32 wo @ 0x84,
    DR_CLEAR:   u32 wo @ 0x88,
    DR_TOGGLE:  u32 wo @ 0x8C,
}

const u32 LED_PIN <- 13;
const u32 LED_BIT <- 3;

scope LED {
    public void toggle() {
        GPIO7.DR_TOGGLE[LED_BIT] <- true;
    }
}

void setup() {
    pinMode(LED_PIN, OUTPUT);
}

void loop() {
    LED.toggle();
    delay(1000);
}
```

## Key Differences from C

| C                | C-Next         | Why                    |
| ---------------- | -------------- | ---------------------- |
| `x = 5`          | `x <- 5`       | Assignment is explicit |
| `x == 5`         | `x = 5`        | Equality uses math =   |
| `int`            | `i32`          | Fixed widths           |
| `(u8)big`        | `big[0, 8]`    | Explicit bit extract   |
| `case X: break;` | `case X { }`   | No fallthrough         |
| Silent overflow  | `clamp`/`wrap` | Explicit behavior      |
| `{1, 2, 3}`      | `[1, 2, 3]`    | Arrays use []          |

## Further Reading

- [GitHub Repository](https://github.com/jlaustill/c-next)
- [Architecture Decision Records](https://github.com/jlaustill/c-next/tree/main/docs/decisions)
