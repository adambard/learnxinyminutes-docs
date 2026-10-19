---
name: PL/I
filename: learnpli.pli
contributors:
    - ["Cyril Hansen"]
---

PL/I (Programming Language One, pronounced "PL one") grew out of an
ambitious idea at IBM in the 1960s: scientific programmers had FORTRAN,
business programmers had COBOL, and systems programmers had other needs.
Why not try to serve them all with one language? PL/I brought structured
programming, decimal arithmetic, strings, and condition handling together.

This tour uses **Digital Research PL/I-80** (1980), a CP/M compiler based
on the proposed ANSI PL/I General-Purpose Subset (Subset G). Much of the
mainframe language survived the move to an 8-bit computer, though full
IBM PL/I offers more features and sometimes different I/O conventions.

```text
/* Comments start with slash-star and end with star-slash.
   PL/I is case-insensitive, free-format, and uses semicolons. */

LEARN: PROCEDURE OPTIONS(MAIN);
    /* PL/I-80 adds a simple compile-time substitution facility. */
    %REPLACE LIMIT BY 4;

    /* DCL is short for DECLARE. Data must be declared explicitly. */
    DECLARE I FIXED BINARY(15);
    /* PL/I-80 requires STATIC for declarations using INITIAL. */
    DECLARE TOTAL FIXED BINARY(15) STATIC INITIAL(0);
    DECLARE AMOUNT FIXED DECIMAL(7,2) STATIC INITIAL(12.50);
    DECLARE HALF FLOAT BINARY(24) STATIC INITIAL(0.5);
    DECLARE READY BIT(1) STATIC INITIAL('1'B);
    DECLARE LABEL_TEXT CHARACTER(12) STATIC INITIAL('hello');
    DECLARE MESSAGE CHARACTER(40) VARYING;
    DECLARE NUMBERS(LIMIT) FIXED BINARY(15);
    DECLARE GRID(0:2,1:3) FLOAT BINARY(24);
    DECLARE HEAP_PTR POINTER;
    DECLARE HEAP_VALUE FIXED BINARY(15) BASED(HEAP_PTR);
    DECLARE OUTPUT FILE;

    /* A structure uses numeric nesting levels, like a record. */
    DECLARE 1 PERSON,
              2 PERSON_NAME CHARACTER(20),
              2 VISITS FIXED BINARY(15);

    /* PUT outputs to the console (SYSPRINT) by default. */
    PUT SKIP LIST('Hello, world!');

    /* = assigns a value. ** is exponentiation. */
    I = 2 ** 3;                  /* 8 */
    TOTAL = I + 2;               /* 10 */
    AMOUNT = AMOUNT + 1.25;      /* 13.75 */

    PUT SKIP LIST('Total:', TOTAL);
    PUT SKIP LIST('Half:', HALF);

    /* EDIT offers formatted output: F(width, decimal places). */
    PUT SKIP EDIT(AMOUNT) (F(7,2));

    /* Fixed-length strings are padded with spaces. */
    LABEL_TEXT = 'hi';

    /* VARYING strings keep their current length. */
    MESSAGE = 'PL/I' || ' on CP/M';  /* || concatenates */
    PUT SKIP LIST(MESSAGE, LENGTH(MESSAGE));
    PUT SKIP LIST(SUBSTR(MESSAGE, 1, 4));
    SUBSTR(MESSAGE, 1, 4) = 'PL/1';

    /* Comparisons yield BIT(1); & means AND, | means OR. */
    IF (TOTAL >= 10) & READY THEN DO;
        PUT SKIP LIST('Ready');
    END;
    ELSE DO;
        PUT SKIP LIST('Not ready');
    END;

    /* Arrays default to one-based indexing. */
    DO I = 1 TO LIMIT;
        NUMBERS(I) = I * I;
    END;

    DO I = 1 TO LIMIT;
        PUT SKIP LIST(I, NUMBERS(I));
    END;

    /* Alternative loop forms: BY and WHILE. */
    DO I = 2 TO 6 BY 2;
        PUT SKIP LIST('Even:', I);
    END;

    I = 3;
    DO WHILE (I > 0);
        I = I - 1;
    END;

    /* FORTRAN-like numerical arrays and mathematical functions. */
    GRID(0,1) = SQRT(9.0);
    PUT SKIP LIST('Square root:', GRID(0,1));

    /* Pointer-based storage can outlive the creating block. */
    ALLOCATE HEAP_VALUE SET(HEAP_PTR);
    HEAP_VALUE = 42;
    PUT SKIP LIST('Heap:', HEAP_PTR -> HEAP_VALUE);
    FREE HEAP_VALUE;
    HEAP_PTR = NULL();

    PERSON.PERSON_NAME = 'Ada';
    PERSON.VISITS = 2;
    PUT SKIP LIST(PERSON.PERSON_NAME, PERSON.VISITS);

    /* CALL invokes a procedure; a function returns a value. */
    CALL SHOW_NUMBER(TOTAL);
    PUT SKIP LIST('5 factorial:', FACT(5));

    /* PL/I-80 requires a GO TO to resume after ZERODIVIDE. */
    ON ZERODIVIDE BEGIN;
        PUT SKIP LIST('Division by zero signalled');
        GO TO AFTER_ZERODIVIDE;
    END;

    SIGNAL ZERODIVIDE;

    AFTER_ZERODIVIDE:
    REVERT ZERODIVIDE;

    /* Input from the console would be: GET LIST(I); */

    /* Stream file I/O on CP/M (uncomment to write OUTPUT.DAT):
       OPEN FILE(OUTPUT) STREAM OUTPUT;
       PUT FILE(OUTPUT) SKIP LIST('A line in a disk file');
       CLOSE FILE(OUTPUT);
    */

    STOP; /* Do not fall through into the procedure bodies. */

    SHOW_NUMBER: PROCEDURE(VALUE);
        DECLARE VALUE FIXED BINARY(15);
        PUT SKIP LIST('From procedure:', VALUE);
    END SHOW_NUMBER;

    /* Recursive procedures must explicitly say RECURSIVE. */
    FACT: PROCEDURE(N) RETURNS(FIXED BINARY(15)) RECURSIVE;
        DECLARE N FIXED BINARY(15);
        IF N <= 1 THEN RETURN(1);
        RETURN(N * FACT(N - 1));
    END FACT;
END LEARN;
```

To build on an original PL/I-80 CP/M system, save the source as
`LEARN.PLI`, then run `PLI LEARN`, `LINK LEARN`, and `LEARN`.
The compiler produces a `.REL` object; LINK-80 produces a `.COM`
program. These commands are specific to the Digital Research toolchain.

## What makes PL/I distinctive

### Pointers and explicit storage management

PL/I gives programmers unusual freedom with memory. A `POINTER` does
not carry the type of the data it points to. Instead, a `BASED`
declaration describes how to interpret storage at an address; it does
not allocate that storage. In the example above, `ALLOCATE ... SET(pointer)`
obtains space and `FREE` returns it. Use `pointer -> object` to name
the pointer explicitly, or `BASED(pointer)` to supply it in the
declaration. `ADDR(variable)` can also provide a view of existing data.

The freedom comes with a bill: there is no garbage collector. A `BASED`
allocation stays alive until the program frees it. PL/I-80 also
implements ordinary `AUTOMATIC` variables statically, except in
recursive procedures. It pays to know which storage you own.

### Decimal arithmetic and automatic conversions

PL/I is at home with decimal numbers as well as binary integers and
floating point. `FIXED DECIMAL(7,2)`, for example, is a natural fit for
money. `AMOUNT = AMOUNT + 1.25;` works in decimal arithmetic with a
defined scale, not an approximation in binary floating point.

The compiler is helpful about conversions between numeric types and
strings. Do not mistake that convenience for a promise of exactness.
**PL/I-80 converts `FIXED DECIMAL` to `FIXED BINARY` when the two
types are mixed in arithmetic**, and a fractional part may be lost.
Keep decimal operands compatible, or use `DECIMAL(...)`,
`BINARY(...)`, and `DIVIDE(...)` explicitly when precision matters.

### FORTRAN heritage

FORTRAN's influence is easy to spot: `FLOAT BINARY`, `**` for powers,
counted `DO` loops, and functions such as `SQRT`, `SIN`, and `LOG`.
Multidimensional arrays have explicit bounds: `GRID(0:2,1:3)` has
three rows and three columns. PL/I does away with FORTRAN's old
fixed-column source format, but keeps much of its numerical vocabulary.

### COBOL heritage and long versus abbreviated spelling

COBOL's influence is just as visible, though in different places:
packed-decimal arithmetic, numbered members of hierarchical structures,
formatted `PUT EDIT` output, and stream or record-oriented file I/O.

PL/I also offers a choice between compact and descriptive spelling.
`DCL RATE FIXED DEC(7,2);` means exactly the same as
`DECLARE RATE FIXED DECIMAL(7,2);`. Similarly,
`DCL TITLE CHAR(20) VAR;` is equivalent to
`DECLARE TITLE CHARACTER(20) VARYING;`.
You will also encounter `PROC`/`PROCEDURE`, `PTR`/`POINTER`, and
`INIT`/`INITIAL`. They are not separate dialects. The short forms
save typing; the full forms make old code easier to read.

### Subset G and the IBM mainframe legacy

PL/I was part of IBM's System/360 vision: one family of computers,
and a language intended to bridge scientific and commercial work.
The proposed General-Purpose Subset G kept a portable core.
Digital Research's PL/I-80 followed that proposal in 1980, before
ANSI X3.74-1981 appeared; the standard was revised in 1987.

How much of that mainframe language could fit on an 8-bit CP/M machine?
Quite a lot: `ON` condition handlers, nested procedures, `ENTRY`
and `EXTERNAL` linkage, pointers, decimal arithmetic, and both stream
and record I/O. Even keywords are context-sensitive: many can also
serve as identifiers.

There are limits, of course. PL/I-80 omits `DEFINED`, `LIKE`,
`FLOAT DECIMAL`, dynamic arrays, and the `PICTURE` **data attribute**.
Version 1.3 added a separate `P(...)` picture-style format for
`PUT EDIT`, but not the missing data attribute. `CONTROLLED`
storage and multitasking also remain outside this subset.
It is a practical subset of PL/I, not a miniature of every IBM feature.

## Further reading

- [Digital Research PL/I-80 Language Manual (December 1980)][language].
- [Digital Research PL/I-80 Applications Guide (December 1980)][apps].
- [Iron Spring PL/I compiler for Linux](https://www.iron-spring.com/)
- [IBM Research: The Early History and Characteristics of PL/I][history].

[history]: https://research.ibm.com/publications/the-early-history-and-characteristics-of-pli
[language]: https://ftpmirror.your.org/pub/misc/bitsavers/pdf/digitalResearch/pl1/PL1-80_Language_Manual_Dec80.pdf
[apps]: https://ftpmirror.your.org/pub/misc/bitsavers/pdf/digitalResearch/pl1/PL1-80_Applications_Guide_Dec80.pdf
