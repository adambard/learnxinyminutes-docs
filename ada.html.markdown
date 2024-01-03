---
---
language: Ada
filename: learnc.ada
contributors:
    ["Luke A. Guest", "https://github.com/Lucretia"]
---

Ada is a strong statically typed imperative, object-oriented and parallel programming language from the Pascal/Algol family of languages, but only has a passing resemblence to Pascal.

Ada was originally designed to be an [object-based](https://ada-lang.io/docs/arm/AA-3/AA-3.3) language and to replace 1000's of languages in use by the US government. This means that all entities are objects, not in the object-oriented sense. The language became Object-Oriented in 1995, and added interfaces derived from Java in 2005.

Ada was designed to be easy to read, even for non-programmers, like management within an organisation, so programs written in the language tend to be a bit more verbose.

Ada is a modern programming language, and now has a package manager like other modern languages, alire, see below.

```ada
--  Comments are written with a double hyphen and exist until the end of
--  the line.

--  You do not need to call the entry point "Main" or "main," you should name
--  name it based on what the program does.
procedure Empty is
   --  This is a declarative part.
begin
   --  Statements go here.
   null;  -- Do nothing here.
end Empty;

--  Ada compiler's accept compilation units which can be library packages,
--  tasks, sub-programs, generics, etc.
with Ada.Text_IO;  --  Get access to a library package.


procedure Hello is
begin
   Ada.Text_IO.Put_Line ("Hello, world");

   Ada.Text_IO.Put ("Hello again, world");
   Ada.Text_IO.New_Line;
end Hello;


with Ada.Unchecked_Conversion;

procedure LearnAdaInY is
   -- Indentation is 3 spaces.

   --  The most important feature in Ada is the type, objects have types and an
   --  object of one type cannot be assigned to an object of another type.

   type Degrees is range 0 .. 360;   --  This is a type.

   type Hues is (Red, Green, Blue, Purple, Yellow);  --  So, is this.

   --  You can restrict a type's range using a subtype, this makes them
   --  compatible with each other, i.e. the subtype can be assigned to an object
   --  of the type, as can be seen below.
   subtype Primaries is Hues range Red .. Blue;

   --  You can define variables or constants like this:

   --  10 is the universal integer. These universal numerics can be used with
   --  any type which matches the base type.
   Angle : Degrees := 10;
   Value : Integer := 20;
   --  New_Angle : Degrees := Value;   -- Incompatible types.
   --  New_Value : Integer := Angle;

   Blue_Hue   :          Primaries := Blue;  --  A variable.
   Red_Hue    : constant Primaries := Red;   --  A constant.
   Yellow_Hue : constant Hues      := Yellow;
   Colour_1   : constant Hues      := Red_Hue;
   --  Colour_2   : constant Primaries := Yellow_Hue;   --  Comment to compile.

   --  You can force conversions, but the you are warned by the name of the
   --  package that you are doing something unsafe.
   function Degrees_To_Int is new Ada.Unchecked_Conversion
     (Source => Degrees,   --  Line continuations are indented by 2 spaces.
      Target => Integer);

   New_Value_2 : Integer := Degrees_To_Int (Angle);   --  Note, space before (.
begin
   null;
end LearnAdaInY;
```

## Download this source

You can cut and paste the above into a new file, e.g. ```learn-ada-in-y.ada```
and then run the following:

```bash
$ gnatchop learn-ada-in-y.ada
$ gnatmake empty.adb
$ gnatmake hello.adb
$ gnatmake learnadainy.adb
```

Or, download [Alire](https://alire.ada.dev), copy it to somewhere in your PATH and then do this:

```bash
$ alr toolchain --select    # Select the GCC and GPR tools to install
$ alr search learnadainy
$ alr get learnadainy
$ cd learnadainy
$ alr run learnadainy
```

## Further Reading

* [Ada Language](https://ada-lang.io)
* [Ada 2022 Reference Manual](https://ada-lang.io/docs/arm)

### Multi-line comments

Multi-line comments are not allowed as they are error prone.

> Such comments would require a closing comment delimiter and this would again raise the dangers associated with the (unintentional) omission of the closing delimiter: entire sections of a program could be ignored by the compiler without the programmer realizing it
>
> [Ada 83 Rationale](http://archive.adaic.com/standards/83rat/html/ratl-02-01.html#2.1)

