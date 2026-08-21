---
name: Active Oberon
filename: LearnActiveOberon.Mod
contributors:
    - ["Andrii Puhachenko", "https://github.com/andrqxa"]
---

**Active Oberon** is the last member of the Pascal / Modula-2 / Oberon family
designed at ETH Zürich (1996-1998, by the group around Niklaus Wirth and
Jürg Gutknecht). It keeps everything Oberon is known for — a grammar that fits
on one page, modules with explicit exports, strong static typing and a garbage
collector — and adds the feature it is named after: **active objects**.

An object may carry its own thread (`BEGIN {ACTIVE}`), any block can be declared
a critical section (`BEGIN {EXCLUSIVE}`), and `AWAIT(condition)` replaces
condition variables. Concurrency is part of the language, not a library. On top
of that the language grew operator overloading, enumerations, math (tensor)
arrays with element-wise operators, and cells for hardware description.

The whole [A2 operating
system](https://en.wikipedia.org/wiki/A2_%28operating_system%29) — kernel,
drivers, compiler, network stack and GUI — is written in Active Oberon, and A2
is the natural place to try the language.

The code below is accepted by the current Fox compiler of A2. Older
A2 / Bluebottle sources use the historic type names (`LONGINT`, `LONGREAL`,
`SHORTINT`) where modern code writes `SIGNED32`, `FLOAT64`, `SIGNED8`.

```componentpascal
(*  Comments look like this, and (* they nest *).  *)
(** A comment with two stars is documentation and is picked up by the
  documentation tools. *)

(* A compilation unit is always a MODULE. Keywords are written in upper
   case, identifiers are case sensitive, and the module name is repeated
   after the final END. The file is normally named after the module. *)
MODULE LearnActiveOberon;

(* Case convention: the scanner reads keywords case insensitively, and the
   extension of the file says which style is used. A file named X.Mod is
   written with UPPER CASE keywords, a file named x.mod with lower case
   ones, predeclared types and built-ins included:

     module HelloLower;
     import KernelLog;
     procedure Do*;
     var i: signed32;
     begin
       KernelLog.String("hi"); KernelLog.Ln
     end Do;
     end HelloLower.
*)

IMPORT
  KernelLog,            (* writes to the system log *)
  Streams,              (* Reader / Writer abstraction *)
  Commands,             (* command context: input, output, arguments *)
  Log := KernelLog;     (* an import may be renamed *)

CONST
  Max* = 100;           (* "*" exports the identifier *)
  Greeting = "Hello";   (* no mark: visible inside this module only *)
  EvenBits = {0, 2, 4, 6};                     (* a SET constant *)

TYPE
  Name* = ARRAY 32 OF CHAR;                    (* 0X terminated string *)
  Handler* = PROCEDURE {DELEGATE} (n: SIGNED32);

  Point* = RECORD
    x*, y*: SIGNED32;
  END;

  Point3d* = RECORD (Point)                    (* record extension *)
    z*: SIGNED32;
  END;

  Color* = ENUM Red*, Green*, Blue* END;       (* enumeration type *)

  Vec2* = RECORD x*, y*: FLOAT64 END;

VAR
  counter-: SIGNED32;   (* "-" exports read-only: clients cannot assign *)
  origin: Point;

(* ------------------------------------------------------------------ *)
(* Types, literals and operators                                       *)
(* ------------------------------------------------------------------ *)

PROCEDURE Basics;
VAR
  b: BOOLEAN;
  c: CHAR;
  i8: SIGNED8; i16: SIGNED16; i32: SIGNED32; i64: SIGNED64;
  u32: UNSIGNED32;
  f32: FLOAT32; f64: FLOAT64;
  z: COMPLEX64;
  s: SET;                        (* small bit set *)
  adr: ADDRESS; size: SIZE;      (* pointer sized integers *)
  any: ANY;                      (* reference to any object *)
BEGIN
  (* INTEGER, REAL and SET are the default width variants; A2 code
     usually spells the width out: SIGNED32, FLOAT64, SET32, ... *)
  i32 := 42;  i32 := 2AH;  i32 := 0x2A;  i32 := 0b101010;
  i32 := 1'000'000;              (* digit separators are allowed *)
  i8 := -1; i16 := 1000; i64 := 1; u32 := 0FFFFFFFFH;
  c := "A";  c := 41X;           (* character, hexadecimal character *)
  f32 := 1.5;  f64 := 1.5E-3;
  z := 1 + 2*IMAG;               (* IMAG is the imaginary unit *)
  f64 := RE(z);  f64 := IM(z);

  (* Arithmetic: + - * / DIV MOD, integer division is DIV *)
  i32 := 7 DIV 2;  i32 := 7 MOD 2;  f64 := 7 / 2;
  i32 := ABS(-3);  INC(i32);  DEC(i32, 2);

  (* Relations: = # < <= > >= IN IS. Booleans: & OR ~ (not) *)
  b := (i32 = 5) OR (i32 # 6) & ~(i32 < 0);

  (* Sets *)
  s := {0, 3, 5..7};  s := s + {1};  s := s * EvenBits;  s := s - {3};
  b := 3 IN s;  INCL(s, 9);  EXCL(s, 0);

  (* Conversions are explicit *)
  i32 := ORD("A");  c := CHR(65);  f64 := i32;  i32 := ENTIER(f64);
  adr := ADDRESSOF(i32);  size := SIZEOF(Point);

  any := NIL;
  ASSERT(i32 >= 0);              (* traps if the condition is FALSE *)
END Basics;

(* ------------------------------------------------------------------ *)
(* Control flow                                                        *)
(* ------------------------------------------------------------------ *)

PROCEDURE ControlFlow(n: SIGNED32): SIGNED32;
VAR i, sum: SIGNED32; c: CHAR; color: Color;
BEGIN
  IF n < 0 THEN
    n := -n
  ELSIF n = 0 THEN
    n := 1
  ELSE
    (* nothing *)
  END;

  c := "x";
  CASE c OF
      "a".."m": sum := 1
    | "n".."z": sum := 2
  ELSE
    sum := 0
  END;

  color := Color.Green;          (* enumerators are qualified *)
  IF color = Color.Green THEN sum := sum + 1 END;

  WHILE n > 1 DO n := n DIV 2 END;

  REPEAT INC(n) UNTIL n >= 4;

  FOR i := 0 TO 10 BY 2 DO sum := sum + i END;

  LOOP
    INC(i);
    IF i > 100 THEN EXIT END
  END;

  VAR local := sum + 1;          (* inline VAR declaration, type inferred *)
  RETURN local
END ControlFlow;

(* ------------------------------------------------------------------ *)
(* Arrays, strings, records and pointers                               *)
(* ------------------------------------------------------------------ *)

TYPE
  IntArray* = POINTER TO ARRAY OF SIGNED32;    (* dynamic array *)
  PointPtr* = POINTER TO Point;

PROCEDURE Sum(CONST a: ARRAY OF SIGNED32): SIGNED32;   (* open array *)
VAR i: SIZE; s: SIGNED32;                (* LEN returns a SIZE *)
BEGIN
  FOR i := 0 TO LEN(a) - 1 DO s := s + a[i] END;
  RETURN s
END Sum;

PROCEDURE Swap(VAR a, b: SIGNED32);      (* VAR = by reference *)
VAR t: SIGNED32;
BEGIN t := a; a := b; b := t
END Swap;

PROCEDURE Aggregates;
VAR
  fixed: ARRAY 4 OF SIGNED32;
  matrix: ARRAY 3, 3 OF FLOAT64;       (* multidimensional *)
  dyn: IntArray;
  name: Name;
  p: Point3d;
  pp: PointPtr;
BEGIN
  fixed[0] := 1; fixed[1] := 2;
  matrix[1, 2] := 0.5;
  NEW(dyn, 10);                        (* length known at run time *)
  dyn[0] := Sum(fixed);
  Swap(fixed[0], fixed[1]);

  COPY(Greeting, name);                (* safe string copy *)
  Strings0(name);

  p.x := 1; p.y := 2; p.z := 3;        (* Point3d inherits x and y *)
  origin.x := p.x; origin.y := p.y;

  NEW(pp);                             (* garbage collected, no free *)
  pp.x := 10;
  pp := NIL;
END Aggregates;

PROCEDURE Strings0(VAR name: Name);
VAR i: SIGNED32;
BEGIN
  i := 0;
  WHILE name[i] # 0X DO INC(i) END;    (* strings end with 0X *)
  Log.String(name); Log.Int(i, 1); Log.Ln
END Strings0;

(* ------------------------------------------------------------------ *)
(* Procedures, delegates and exception handling                        *)
(* ------------------------------------------------------------------ *)

PROCEDURE Outer(n: SIGNED32): SIGNED32;

  PROCEDURE Inner(k: SIGNED32): SIGNED32;   (* nested, sees n *)
  BEGIN RETURN k * n
  END Inner;

BEGIN
  RETURN Inner(2)
END Outer;

PROCEDURE Print(n: SIGNED32);
BEGIN Log.Int(n, 1); Log.Ln
END Print;

PROCEDURE UseDelegate;
VAR h: Handler;
BEGIN
  h := Print;                          (* plain procedure *)
  h(42);
  (* A DELEGATE may also hold a method together with its object. *)
END UseDelegate;

PROCEDURE MayTrap(a, b: SIGNED32): SIGNED32;
VAR result: SIGNED32;
BEGIN
  result := a DIV b;                   (* traps if b = 0 *)
  RETURN result
FINALLY                                  (* runs on a trap as well *)
  RETURN 0
END MayTrap;

(* ------------------------------------------------------------------ *)
(* Operator overloading                                                *)
(* ------------------------------------------------------------------ *)

OPERATOR "+"* (CONST a, b: Vec2): Vec2;
VAR r: Vec2;
BEGIN
  r.x := a.x + b.x; r.y := a.y + b.y;
  RETURN r
END "+";

(* ------------------------------------------------------------------ *)
(* Objects: methods, constructor, inheritance                          *)
(* ------------------------------------------------------------------ *)

TYPE
  Shape* = OBJECT
  VAR
    name-: Name;

    PROCEDURE &Init*(CONST n: ARRAY OF CHAR);   (* constructor *)
    BEGIN
      COPY(n, name)
    END Init;

    PROCEDURE Area*(): FLOAT64;                 (* overridable *)
    BEGIN
      RETURN 0
    END Area;

    PROCEDURE Describe*;
    BEGIN
      Log.String(name); Log.String(" area=");
      Log.Int(ENTIER(SELF.Area()), 1); Log.Ln   (* SELF = this *)
    END Describe;
  END Shape;

  Circle* = OBJECT (Shape)                        (* single inheritance *)
  VAR radius: FLOAT64;

    PROCEDURE &InitCircle*(r: FLOAT64);
    BEGIN
      Init("circle");                         (* call inherited method *)
      radius := r
    END InitCircle;

    PROCEDURE Area*(): FLOAT64;                 (* override *)
    BEGIN
      RETURN 3.14159 * radius * radius
    END Area;
  END Circle;

PROCEDURE UseObjects;
VAR s: Shape; c: Circle;
BEGIN
  NEW(c, 2.0);                (* NEW passes the constructor arguments *)
  c.Describe;
  s := c;                     (* a Circle is a Shape *)
  s.Describe;                 (* dynamic dispatch: prints the circle area *)

  IF s IS Circle THEN         (* run time type test *)
    Log.String(s(Circle).name); Log.Ln    (* type guard *)
  END;

  WITH s: Circle DO           (* regional type guard *)
    Log.Int(ENTIER(s.Area()), 1); Log.Ln
  END
END UseObjects;

(* ------------------------------------------------------------------ *)
(* Active objects: the reason the language is called *Active* Oberon   *)
(* ------------------------------------------------------------------ *)

TYPE
  (* An object with a BEGIN {ACTIVE} body owns a thread that starts as
     soon as the object is created. {EXCLUSIVE} turns an object into a
     monitor: only one activity at a time may run inside such a block,
     and AWAIT blocks until the condition becomes true. *)
  Buffer* = OBJECT
  VAR
    data: ARRAY 16 OF SIGNED32;
    head, tail, count: SIGNED32;
    alive: BOOLEAN;

    PROCEDURE &Init*;
    BEGIN
      head := 0; tail := 0; count := 0; alive := TRUE
    END Init;

    PROCEDURE Put*(x: SIGNED32);
    BEGIN {EXCLUSIVE}
      AWAIT(count < LEN(data));         (* wait until there is room *)
      data[tail] := x;
      tail := (tail + 1) MOD LEN(data);
      INC(count)
    END Put;

    PROCEDURE Get*(): SIGNED32;
    VAR x: SIGNED32;
    BEGIN {EXCLUSIVE}
      AWAIT(count > 0);                 (* wait for an element *)
      x := data[head];
      head := (head + 1) MOD LEN(data);
      DEC(count);
      RETURN x
    END Get;

    PROCEDURE Close*;
    BEGIN {EXCLUSIVE}
      alive := FALSE
    END Close;

  BEGIN {ACTIVE}                            (* the body is a thread *)
    WHILE alive DO
      Log.Int(SELF.Get(), 1); Log.Ln
    END
  END Buffer;

PROCEDURE Producer*;
VAR b: Buffer; i: SIGNED32;
BEGIN
  NEW(b);                                   (* the thread starts here *)
  FOR i := 1 TO 5 DO b.Put(i) END;
  b.Close
END Producer;

(* A whole procedure body can be a critical section, and a plain
   BEGIN {EXCLUSIVE} ... END block may also appear inside a procedure. *)

PROCEDURE Guarded(o: Buffer);
BEGIN
  BEGIN {EXCLUSIVE}
    INC(counter)
  END
END Guarded;

(* ------------------------------------------------------------------ *)
(* Math arrays: array structured types with element wise operators     *)
(* ------------------------------------------------------------------ *)

PROCEDURE MathArrays;
VAR
  v: ARRAY [*] OF FLOAT64;         (* one dimensional, open *)
  m: ARRAY [*, *] OF FLOAT64;      (* two dimensional *)
  t: ARRAY [?] OF FLOAT64;         (* tensor, any rank *)
  w: ARRAY [3] OF FLOAT64;         (* fixed length *)
  f: FLOAT64;
BEGIN
  NEW(v, 3); NEW(m, 3, 3);
  v := [1.0, 2.0, 3.0];            (* array constructor *)
  v := v .* v;                     (* element wise product *)
  v := 2 * v;                      (* scalar broadcast *)
  m := m + m;
  v := m * v;                      (* matrix vector product *)
  f := SUM(v);
  f := v[1];  v[0..1] := v[1..2];  (* ranges and slices *)
  Log.Int(LEN(v, 0), 1); Log.Ln;   (* length of dimension 0 *)
  Log.Int(DIM(m), 1); Log.Ln       (* number of dimensions *)
END MathArrays;

(* ------------------------------------------------------------------ *)
(* Commands: procedures callable from the A2 shell                     *)
(* ------------------------------------------------------------------ *)

(* An exported parameterless procedure, or one taking a
   Commands.Context, can be started from the command line or from any
   piece of text in the system by clicking on it. *)

PROCEDURE Hello*(context: Commands.Context);
VAR name: Name;
BEGIN
  context.arg.SkipWhitespace;
  context.arg.String(name);                    (* read an argument *)
  IF name = "" THEN COPY("world", name) END;
  context.out.String(Greeting); context.out.String(", ");
  context.out.String(name); context.out.String("!");
  context.out.Ln;
  context.out.Update                           (* flush the writer *)
END Hello;

PROCEDURE WriteTo(w: Streams.Writer);
BEGIN
  w.String("Streams.Writer works with files, network and screen");
  w.Ln; w.Update
END WriteTo;

(* The module body runs once, when the module is loaded. *)
BEGIN
  counter := 0;
  KernelLog.String("LearnActiveOberon loaded"); KernelLog.Ln
END LearnActiveOberon.

(* Text below the final dot is ignored by the compiler, so A2 sources
   traditionally end with the commands that build and test the module.
   In A2 you execute them by middle clicking on them:

Compiler.Compile LearnActiveOberon.Mod ~
LearnActiveOberon.Hello Active Oberon ~
LearnActiveOberon.Producer ~
System.Free LearnActiveOberon ~
*)
```

## Modifiers in braces

Almost every declaration — procedure, type, variable, parameter, statement
block, cell — may carry a list of flags in braces. The parser accepts any
identifier there, and the compiler rejects the ones it does not know.

Concurrency (object bodies and statement blocks):

* `{ACTIVE}` — the body runs as its own activity (thread), started when the
  object is created.
* `{EXCLUSIVE}` — the block is a critical section: at most one activity at a
  time is inside any exclusive block of the object.
* `{PRIORITY(n)}` — priority of the active body.
* `{SAFE}` — the active body is restarted after a trap and resists
  termination.
* `{REALTIME}` — the body is a realtime activity and may only use operations
  that are realtime safe.
* `{UNCOOPERATIVE}` — the block does not take part in cooperative scheduling
  (kernel and low level code).

Object orientation:

* `{ABSTRACT}` — record / object type or method without an implementation.
* `{FINAL}` — the record cannot be extended, the method cannot be
  overridden.
* `{OVERRIDE}` — states explicitly that the method redefines an inherited
  one (the compiler infers it otherwise).
* `{DELEGATE}` — a procedure type whose values may also be a method bound to
  its object.
* `{DYNAMIC}` — operator dispatched at run time.

Memory and safety:

* `{UNTRACED}` — the pointer variable is not traced by the collector.
* `{UNTRACKED}` — the statement block's local references are not tracked.
* `{UNSAFE}` — `POINTER {UNSAFE} TO ...` is a raw pointer: compatible with
  `ADDRESS`, no type guards, no checks.
* `{UNCHECKED}` — the block is compiled without nil, index and stack checks.
* `{DISPOSABLE}` — the pointer / object is released with `DISPOSE` instead of
  by the collector.
* `{ALIGNED(n)}` — align the symbol to `n` bytes.
* `{OFFSET(n)}` — place the field or variable at a fixed offset.
* `{MOVABLE}` — an `ADDRESS` parameter that may refer to memory the
  collector moves.
* `{REGISTER}` — keep the variable or parameter in a register if possible.

Procedures and linking:

* `{WINAPI}`, `{C}`, `{PlatformCC}` — calling convention of the procedure
  (type).
* `{INTERRUPT}` — the procedure is an interrupt handler.
* `{NORETURN}` — the procedure never returns.
* `{PLAIN}` — no activation frame, hence no local variables or parameters.
* `{OPENING}` / `{CLOSING}` — link the procedure before all module bodies /
  after them; both imply `PLAIN`.
* `{ALIGNSTACK}` — align the stack when entering the procedure.
* `{PCOFFSET(n)}` — program counter offset of the procedure type (back end).
* `{Fingerprint=x}` — fix the symbol fingerprint instead of computing it.
* `{TEST}` — mark a test procedure for the compiler's `--test` option.

Active Cells (hardware description, FPGA back end) — properties of the
generated cell or channel: `{DataMemorySize(n)}`, `{CodeMemorySize(n)}`,
`{InstructionWidth(n)}`, `{ChannelWidth(n)}`, `{ChannelDepth(n)}`,
`{Channels}`, `{Vector}`, `{FloatingPoint}`, `{NoMul}`,
`{HasNonBlockingIO}`, `{FrequencyDivider(n)}`, `{Engine}`, `{TRM}`, `{TRMS}`,
`{BaseMem}`, `{BaseDiv}`, `{Backend(s)}`, `{Runtime(s)}`.

## Notable details

* Identifiers are case sensitive. Keywords are read case insensitively, and
  the file extension picks the style: `X.Mod` is written in UPPER CASE,
  `x.mod` in lower case.
* `*` after a declared name exports it, `-` exports it read-only.
* There is no `free`: the system is garbage collected.
* Every module can be loaded and unloaded at run time
  (`System.Free Module ~`), which is how A2 is developed while it runs.
* `SYSTEM` gives access to unsafe operations (`SYSTEM.GET`, `SYSTEM.PUT`,
  `SYSTEM.VAL`, `SYSTEM.MOVE`), and `CODE ... END` embeds assembler.
* Conditional compilation uses `#if ... #else ... #end` with symbols passed to
  the compiler (`--define=UNIX,AMD64`).

## Further reading

* [t.me/A2OperatingSystem](https://t.me/A2OperatingSystem) — the main
  community channel. It is mostly Russian speaking, but questions asked in
  English are answered too.
* [Active Oberon](https://en.wikipedia.org/wiki/Active_Oberon) and
  [A2](https://en.wikipedia.org/wiki/A2_%28operating_system%29) on Wikipedia
* [A2 project page at ETH Zürich](http://cas.inf.ethz.ch/projects/a2)
* [Official A2 sources (ETH GitLab)](https://gitlab.inf.ethz.ch/felixf/oberon)
* [a2oberon](https://gitlab.com/a25665725/a2oberon) — actively maintained
  fork, branch `dev-andrii`, with the complete history imported from SVN;
  `docs/` holds the Oberon Language Report, the quick start guide and the
  concurrency framework paper
* [minia2](https://github.com/active-oberon/minia2) — a Go style SDK:
  compiler, language server and package manager in one Docker image. It
  builds standalone Linux and Windows console programs, so you can write
  Active Oberon without installing A2 at all
* [a2-registry](https://active-oberon.github.io/a2-registry/) — descriptions
  of the A2 modules
* [oberon.org](https://oberon.org/en) — catalogue of Oberon resources
