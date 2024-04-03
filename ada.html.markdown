---
language: Ada
filename: learn.ada
contributors:
    - ["Luke A. Guest", "https://github.com/Lucretia"]
    - ["Fernando Oleo Blanco", "https://github.com/Irvise"]
    - ["Fabien Chouteau", "https://github.com/Fabien-Chouteau"]
    - ["Manuel", "https://github.com/mgrojo"]
---

Ada is a strong statically typed imperative, [object-oriented](https://ada-lang.io/docs/arm/AA-3/AA-3.9), [real-time](https://ada-lang.io/docs/arm/AA-D), [parallel](https://ada-lang.io/docs/arm/AA-9) and [distributed](https://ada-lang.io/docs/arm/AA-9) programming language from the Pascal/Algol family of languages, but nowadays, it only has a passing resemblance to Pascal, with the only remnants left being the ```begin/end``` keyword pair, the ```:=``` assignment symbol, records and ```if/case``` control statement structures.

Ada was originally designed to be an [object-based](https://ada-lang.io/docs/arm/AA-3/AA-3.3) language and to replace 100's of languages in use by the US government. This means that all entities are objects, not in the object-oriented sense. The language became [Object-Oriented](https://ada-lang.io/docs/arm/AA-3/AA-3.9) in 1995, and added [interfaces](https://ada-lang.io/docs/arm/AA-3/AA-3.9#Subclause_3.9.4) derived from Java in 2005. [Contract based](https://ada-lang.io/docs/arm/AA-13/AA-13.1#Subclause_13.1.1) programming was introduced with Ada 2012.

Ada was designed to be easy to read and learn, even for non-programmers, e.g. management within an organisation, therefore programs written in the language tend to be a bit more verbose.

Ada is a modern programming language, and now has a package manager like other modern languages, Alire, see below.

```ada
--  Comments are written with a double hyphen and exist until the end of
--  the line.

--  You do not need to call the entry point "Main" or "main," you should
--  name it based on what the program does.
procedure Empty is
   --  This is a declarative part.
begin
   --  Statements go here.
   null;  --  Do nothing here.
end Empty;

--  Ada compilers accept compilation units which can be library packages,
--  tasks, sub-programs, generics, etc.

--  This is where "context clauses" go, these can be pragmas or "with"
--  statements. "with" is equivalent to "include" or "import" in other
--  languages.
with Ada.Text_IO;  --  Get access to a library package.

procedure Hello is
begin
   Ada.Text_IO.Put_Line ("Hello, world");

   Ada.Text_IO.Put ("Hello again, world");
   Ada.Text_IO.New_Line;
end Hello;


--  Ada has a real module system. Modules are called packages and are split into
--  two component parts, the specification and a body.
--  It is important to introduce packages early, as you will be using them from
--  the start.
package Stuff is
   --  We could add the following line in order to tell the compiler that this
   --  package does not have to run any code before the "main" procedure starts.
   --  pragma Preelaborate;

   --  Packages can be nested within the same file or externally.
   --  Nested packages are accessed via dot notation, e.g. Stuff.Things.My.
   package Things is
      My : constant Integer := 100;
   end Things;

   --  If there are sub-programs declared within the specification, the body
   --  of the sub-program must be declared within the package body.
   procedure Do_Something;  --  If a subprogram takes no parameters, empty
                            --  parentheses are not required, unlike other
                            --  languages.

   --  We can also make generic sub-programs.
   generic
      type Element is (<>);  --  The "(<>)" notation specifies that only
                             --  discrete types can be passed into the generic.
   procedure Swap (Left, Right : in out Element);

   --  Sometimes we want to hide how a type is defined from the outside world
   --  so that nobody can mess with it directly. The full type must be defined
   --  within the private section below.
   type Blobs is private;

   --  We can also make types "limited" by putting this keyword after the "is"
   --  keyword, this means that the user cannot copy objects of that type
   --  around, like they normally could.
private
   type Blobs is new Integer range -25 .. 25;
end Stuff;


package body Stuff is
   --  Sub-program body.
   procedure Do_Something is
      --  We can nest sub-programs too.
      --  Parameters are defined with the direction of travel, in, in out, out.
      --  If the direction of travel is not specified, they are in by default.
      function Times_4 (Value : in Integer) return Integer is
      begin
         return Value * 4;
      end Times_4;

      I : Integer := 4;
   begin
      I := Times_4 (I);
   end Do_Something;


   --  Generic procedure body.
   procedure Swap (Left, Right : in out Element) is
      Temp : Element := Left;
   begin
      Left  := Right;
      Right := Temp;
   end Swap;
begin
   --  If we need to initialise something within the package, we can do it
   --  here.
   Do_Something;
end Stuff;


with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Stuff;

procedure LearnAdaInY is
   --  Indentation is 3 spaces.

   --  The most important feature in Ada is the type. Objects have types and an
   --  object of one type cannot be assigned to an object of another type.

   --  You can, and should, define your own types for the domain you are
   --  modelling. But you can use the standard types to start with and then
   --  replace them later with your own types, this could be called a form of
   --  gradual typing.

   --  The standard types would only really be a good starting point for binding
   --  to other languages, like C. Ada is the only language with a standardised
   --  way to bind with C, Fortran and COBOL! See the links in the References
   --  section with more information on binding to these languages.

   type Degrees is range 0 .. 360;  --  This is a type. Its underlying
                                    --  representation is an Integer.

   type Hues is (Red, Green, Blue, Purple, Yellow);  --  So is this. Here, we
                                                     --  are declaring an
                                                     --  Enumeration.

   --  This is a modular type. They behave like Integers that automatically
   --  wrap around. In this specific case, the range would be 0 .. 359.
   --  If we added 1 to a variable containing the value 359, we would receive
   --  back 0. They are very useful for arrays.
   type Degrees_Wrap is mod 360;

   --  You can restrict a type's range using a subtype, this makes them
   --  compatible with each other, i.e. the subtype can be assigned to an
   --  object of the type, as can be seen below.
   subtype Primaries is Hues range Red .. Blue;  --  This is a range.

   --  You can define variables or constants like this:
   --  Var_Name : Type := Value;

   --  10 is a universal integer. These universal numerics can be used with
   --  any type which matches the base type.
   Angle : Degrees := 10;
   Value : Integer := 20;
   --  New_Angle : Degrees := Value;  --  Incompatible types won't compile.
   --  New_Value : Integer := Angle;

   Blue_Hue   :          Primaries := Blue;  --  A variable.
   Red_Hue    : constant Primaries := Red;   --  A constant.
   Yellow_Hue : constant Hues      := Yellow;
   Colour_1   : constant Hues      := Red_Hue;
   --  Colour_2   : constant Primaries := Yellow_Hue;  --  uncomment to compile.

   --  You can force conversions, but then you are warned by the name of the
   --  package that you may be doing something unsafe.
   function Degrees_To_Int is new Ada.Unchecked_Conversion
     (Source => Degrees,   --  Line continuations are indented by 2 spaces.
      Target => Integer);

   New_Value_2 : Integer := Degrees_To_Int (Angle);  --  Note, space before (.

   --  GNAT is the GNU Ada Translator (compiler).
   --  Ada has a style guide and GNAT will warn you to adhere to it, and has
   --  option to check your style so that you can correct it so that all Ada
   --  source looks consistent. However, the style can be customized.

   --  Yes, you can even define your own floating and fixed point types, this
   --  is a very rare and unique ability. "digits" refers to the minimum
   --  digit precision that the type should support. "delta" is for fixed
   --  point types and refers to the smallest change that the type will support.
   type Real_Angles is digits 3 range 0.0 .. 360.0;
   type Fixed_Angles is delta 0.01 digits 5 range 0.0 .. 360.0;

   RA : constant Real_Angles  := 36.45;
   FA : constant Fixed_Angles := 360.0;  --  ".0" in order to make it a Float.

   --  You can have normal Latin 1 based strings by default.
   Str  : constant String    := "This is a constant string";
   --  When initialising from a string literal, the compiler knows the bounds,
   --  so we don't have to define them.

   --  Strings are arrays. Note how parentheses are used to access elements of
   --  an array? This is mathematical notation and was used because square
   --  brackets were not available on all keyboards at the time Ada was
   --  created. Also, because an array can be seen as a function from a
   --  mathematical perspective, so it made converting between arrays and
   --  functions easier.
   Char : constant Character := Str (Str'First);  --  "'First" is a type
                                                  --  attribute.

   --  Ada 2022 includes the use of [] for array initialisation when using
   --  containers, which were added in Ada 2012.

   --  Arrays are usually always defined as a type.
   --  They can be any dimension.
   type My_Array_1 is array (1 .. 4, 3 .. 7, -20 .. 20) of Integer;

   --  Yes, unlike other languages, you can index arrays with other discrete
   --  types such as enumerations and modular types or arbitrary ranges.
   type Axes is (X, Y, Z);

   --  You can define the array's range using the 'Range attribute from
   --  another type.
   type Vector is array (Axes'Range) of Float;

   V1 : constant Vector := (0.0, 0.0, 1.0);

   --  A record is the same as a structure in C, C++.
   type Entities is record
      Name     : String (1 .. 10);  --  Always starts at a positive value,
                                    --  inclusive range.
      Position : Vector;
   end record;

   --  In Ada, array bounds are immutable. You therefore have to provide a
   --  string literal with a value for every character.
   E1 : constant Entities := ("Blob      ", (0.0, 0.0, 0.0));

   --  An alternative is to use an array aggregate and assign a default value
   --  to every element that wasn't previously assigned in this aggregate.
   --  "others" is used to indicate anything else that has not been
   --  explicitly initialized.
   E2 : constant Entities := (('B', 'l', 'o', 'b', others => ' '),
                              (0.0, 0.0, 0.0));

   --  There are dynamic length strings (see references section) available in
   --  the standard library.

   --  We can make an object be initialised to its default values with the box
   --  notation, "<>". "others" is used to indicate anything else that has not
   --  been explicitly initialized.
   Null_Entity : constant Entities := (others => <>);

   --  Object-orientation is accomplished via an extension of record syntax,
   --  tagged records, see link above in first paragraph.

   --  We can rename objects (aliases) to make readability a bit better.
   package IO renames Ada.Text_IO;
begin
   --  We can output enumerations as names.
   IO.Put_Line ("Blue_Hue   = " &  --  & is the string concatenation operator.
                Blue'Image);       --  ' accesses attributes on objects.
                  --  The Image attribute converts a value to a string.
                  --  Ada 2022 has extended Image to custom types too.
                  --  Access this with -gnat2022 compiler flag.
   IO.Put_Line ("Yellow_Hue = " &
                --  We can use the type's attribute.
                Primaries'Image (Yellow_Hue));

   --  We can define local variables within a declare block, this can be made
   --  more readable by giving it a label.
   Enum_IO : declare
      package Hue_IO is new IO.Enumeration_IO (Hues);

      --  Using a package makes everything inside that package visible within
      --  this block, it is good practice to only do this locally and not on
      --  a whole package within the context clause.
      use Hue_IO;
   begin
      --  We can print out the enumeration values too.
      Put (Purple); --  Note we don't have to prefix the Put procedure with
                    --  Hue_IO.
      IO.New_Line;  --  We still need to prefix with IO here.
      Put (Red_Hue);
      IO.New_Line;
   end Enum_IO;

   --  Loops have a consistent form. "<form> loop ... end loop".
   --  Where "form" can be "while" or "for" or missing as below, if
   --  you place the "loop ... end loop;" construct on their own lines,
   --  you can comment out or experiment with different loop constructs more
   --  easily.
   declare
      Counter : Positive := Positive'First;  --  This is 1.
   begin
      --  We can label loops so we can exit from them more easily if we need to.
      Infinite :
      loop
         IO.Put_Line ("[Infinite loop] Counter = " & Counter'Image);

         Counter := Counter + 1;

         --  This next line implements a repeat ... until or do ... while loop construct.
         --  Comment it out for an infinite loop.
         exit Infinite when Counter = 5;  --  Equality tests use a single "=".
      end loop Infinite;  --  Useful when implementing state machines.
   end;

   declare  --  We don't have to have a label.
      Counter : Positive := Positive'First;  --  This is 1.
   begin
      while Counter < 10
      loop
         IO.Put_Line ("Counter = " & Counter'Image);

         Counter := Counter + 1;  --  There is no explicit inc/decrement.

         --  Ada 2022 introduced @ for LHS, so the above would be written as
         --  Counter := @ + 1;  --  Try it, -gnat2022.
      end loop;
   end;

   declare
      package Hue_IO is new IO.Enumeration_IO (Hues);

      --  We can have multiple packages on one line, but I tend to use one
      --  package per line for readability.
      use IO, Hue_IO;
   begin
      Put ("Hues : ");  --  Note, no prefix.

      --  Because we are using the 'Range attribute, the compiler knows it is
      --  safe and can omit run-time checks here.
      for Hue in Hues'Range
      loop
         Put (Hue);

         --  Types and objects know about their bounds, their First .. Last
         --  values. These can be specified as range types.
         if Hue /= Hues'Last then  --  The /= means "not equal to" like the
                                   --  maths symbol ≠.
            Put (", ");
         end if;
      end loop;

      IO.New_Line;
   end;

   --  All objects know their bounds, including strings.
   declare
      C : Character := Str (50);  --  Warning caused and exception raised at
                                  --  runtime.
      --  The exception raised above can only be handled by an outer scope,
      --  see wikibook link below.
   begin
      null;  --  We will never get to this point because of the above.
   end;
exception
   when Constraint_Error =>
      IO.Put_Line ("Caught the exception");
end LearnAdaInY;
```

Now, that's a lot of information for a basic intro to Ada, and I've only touched the surface, there's much more to look at in the references section below. I haven't even touched on dynamic memory allocation which includes [pools](https://ada-lang.io/docs/arm/AA-13/AA-13.11), this is because for the most part, Ada programs don't need it, you can do a lot without it.

As I stated above, Ada barely looks like Pascal and if you look at the original [Green specification](https://apps.dtic.mil/sti/trecms/pdf/ADB950587.pdf) (Warning: Huge 4575 page scanned PDF - starting on page 460), it looks nothing like it at all (page 505 of that PDF).

The above source code will compile, but also will give warnings showing the power of the strong static type system.

## Download this source

If you already have the GNAT toolchain installed, you can cut and paste the above into a new file, e.g. ```learn-ada-in-y.ada``` and then run the following:

```bash
$ gnatchop learn-ada-in-y.ada # This breaks the program into its specification ".ads" and body ".adb".
$ gnatmake empty.adb # gnatmake takes care of compilation of all units and linking.
$ gnatmake hello.adb
$ gnatmake learnadainy.adb
```

Or, download [Alire](https://alire.ada.dev), copy it to somewhere in your PATH and then do the following:

**N.B.** Alire will automatically install the toolchain for you if you don't have one installed and will ask you to select which you want to use.

```bash
$ alr search learnadainy
$ alr get learnadainy
$ cd learnadainy
$ alr run empty
$ alr run hello
$ alr run learnadainy
```

## Further Reading

* [Ada Programming Language](https://ada-lang.io)
* [Ada 2022 Reference Manual](https://ada-lang.io/docs/arm)
* [Ada Style Guide](https://ada-lang.io/docs/style-guide/Ada_Style_Guide)
* [Learn more Ada/Spark at AdaCore's site](https://learn.adacore.com)

## References from the source above

1. [wikibook](https://en.wikibooks.org/wiki/Ada_Programming/Exceptions#Exception_handlers)
2. [C](https://ada-lang.io/docs/arm/AA-B/AA-B.3)
3. [Fortran](https://ada-lang.io/docs/arm/AA-B/AA-B.5/)
4. [COBOL](https://ada-lang.io/docs/arm/AA-B/AA-B.4/)
5. [dynamic length strings](https://ada-lang.io/docs/arm/AA-A/AA-A.4#Subclause_A.4.5)

### Multi-line comments

Multi-line comments are not allowed as they are error prone.

> Such comments would require a closing comment delimiter and this would again raise the dangers associated with the (unintentional) omission of the closing delimiter: entire sections of a program could be ignored by the compiler without the programmer realizing it
>
> [Ada 83 Rationale](http://archive.adaic.com/standards/83rat/html/ratl-02-01.html#2.1)

