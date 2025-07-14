---
name: Liberty Eiffel
contributors:
    - ["Jakub Pavlík", "https://github.com/igneus"]
filename: learn_liberty.e
---

Eiffel is a general-purpose, class-based, purely object-oriented,
garbage-collected,
ECMA- and ISO-standardized programming language.

[Liberty Eiffel](https://www.liberty-eiffel.org/)
is a community-maintained free open source implementation
of the language, based off the codebase of the discontinued
SmartEiffel project.
Both its Eiffel language dialect and standard library differ
significantly from EiffelStudio and compatible implementations
("standard Eiffel"),
due partly to conserving Eiffel features common prior to the ECMA
specification,
partly to independent design decisions.

```eiffel
class LEARN_LIBERTY

-- 'inherit' section (not used in this example) specifies parent
-- classes. Structure of its contents is exactly the same as for
-- 'insert' section below.

-- 'insert' section specifies parent classes
-- for non-conforming inheritance:
-- the child class inherits features from the parent classes,
-- but is not considered their polymorphic subtype
-- (a LEARN_LIBERTY instance cannot be assigned to a variable of type ARGUMENTS)
insert
   ARGUMENTS -- provides access to command line arguments
      rename
         -- method renaming is just one example of Eiffel's granular
         -- control over inheritance. It's worth reading more on the topic.
         command_arguments as argv
      end

   EXCEPTIONS -- provides features related to exception throwing and handling

-- 'create' section lists methods which serve as constructors.
-- Visibility to all subclasses of {ANY} is the widest public
-- visibility, as all classes are subclasses of ANY.
create {ANY}
   make

feature {ANY}
   -- public features

   -- An attribute (instance variable)
   counter: INTEGER

   -- A constant
   Max_count: INTEGER is 1000

   make
         -- Constructor (see the 'create' block above).
         -- If this class is compiled on its own and without
         -- explicitly specifying the main method, it will serve
         -- also as a default entry point (root method).
      do
         counter := 2

         -- These are procedure calls.
         -- No compulsory semicolons or other statement delimiters,
         -- no parentheses required when calling methods without arguments.
         basic_io
         locals
         arguments (1, 2, "label")
         conditional
         cycle

         -- the result of a function call must always be consumed
         -- (assigned to a variable or passed as an argument)
         io.put_integer (factorial (7))

         io.put_integer (assertions (7))

         manifest_notation
         expanded_and_reference_classes
         agents
         exceptions
      end

feature {}
   -- private features ({} is an empty client list) are visible only
   -- to the defining class and its children

   basic_io
         -- Showcase basic terminal output and input
      do
         print ("Hello, world!%N") -- "%N" is an escape sequence for newline

         -- 'io' provides a convenient interface for reading and writing
         -- standard input and output
         io.put_integer (5)
         io.put_string (" apples")
         io.put_new_line

         io.put_string ("What's your name? > ")
         io.read_line
         io.put_string ("Hello, " + io.last_string + "!%N")

         -- printing to individual output streams
         std_output.put_string ("another way to print%N")
         std_error.put_string ("we have stderr, too%N")
      end

   locals
         -- Local variables are declared in a dedicated code block
      local
         i: INTEGER
      do
         i := 3
         io.put_integer (i * i - (i // 2)) -- the "//" operator on INTEGER
         -- is integer division
         io.put_new_line
      end

   arguments (a, b: INTEGER; label: STRING)
         -- This is how routine arguments are declared
      do
         io.put_string (label + ": ")
         io.put_integer (a + b)
         io.put_new_line
      end

   conditional
      local
         str: STRING
      do
         if 1 >= 2 then
            print ("Mathematicians panic%N")
         elseif 1 - 1 > 0 then
            print ("Mathematicians still panic%N")
         else
            -- do nothing, the world is all right
         end

         -- Conditional can be used as an expression returning a value:
         str :=
            if 1 = (2 - 1) then -- note single "=" as equality operator
               "do not "
            else
               "do "
            end
         print ("Mathematicians " + str + "panic%N")

         -- Evaluation of logical expressions is not short-circuited
         -- by default, but logical operators have short-circuiting variants
         -- 'and then' and 'or else'
         if io.last_string /= Void and then io.last_string.count > 10 then
            print ("The last string read from stdin was quite long!%N")
         end
      end

   cycle
         -- There is only a single language-level looping construct.
         -- By design there's no 'break' or 'continue' keyword
         -- as known from C-like languages.
      local
         i: INTEGER
      do
         from
            i := 0
         until
            i = 10
         loop
            io.put_integer (i)
            io.put_new_line

            i := i + 1
         end
      end

   factorial (n: INTEGER): INTEGER
         -- Computes factorial of n.
      do
         -- Result of a function is specified by assigning to the
         -- special variable 'Result'. It behaves like any other
         -- local variable, can be both assigned repeatedly
         -- and referenced.
         -- There is no return statement terminating execution
         -- of the routine prematurely.
         Result := n
         if n > 1 then
            Result := Result * factorial (n - 1)
         end
      end

   assertions (n: INTEGER): INTEGER
         -- Contract of a class is specified in terms of feature
         -- identifiers, visibility, types and assertions.
         -- Assertions are boolean expressions which in certain
         -- moments must evaluate true.
         -- Assertions are by default checked at runtime,
         -- an unfulfilled assertion results in an exception.
         -- Assertion checks can be disabled by compiler options in
         -- order to improve performance at the cost of security.
         --
         -- In addition to assertions showcased here there is class
         -- invariant - see at the end of the class body.
      require
         -- Must be fulfilled at the beginning of the method's execution.
         n >= 0
      local
         i: INTEGER
      do
         Result := 1

         from
            i := n
         invariant
            i >= 1 -- loop invariant must be true for every iteration
         variant
            i -- loop variant must decrease with every iteration
         until
            i = 1
         loop

            -- Unlike the other assertions, 'check' block can be placed
            -- anywhere in the method body, even repeatedly.
            check
               i > 1
               Result >= 1
            end

            Result := Result * i
            i := i - 1
         end

      ensure
         -- Must be fulfilled at the end of the method's execution.
         Result >= 1
      end

   manifest_notation
         -- For some general programming concepts the Eiffel
         -- community uses less common terms.
         -- "Manifest notation" is one such term, translating to
         -- "literals" in the usual terminology.
      local
         i: INTEGER
         r: REAL
         b: BOOLEAN
         s: STRING
         c: CHARACTER
         a: ARRAY[INTEGER_8] -- ARRAY is a generic type
         m: DICTIONARY[INTEGER, STRING]
      do
         i := 1

         r := 3.14159

         b := True
         b := False

         s := "This is a string"

         c := 'c'

         -- manifest array
         a := << 11, 22, 33, 44, 55 >>
         check
            a.item (1) = 11 -- array indices are 1-based
            -- (by default; can be changed per instance)
         end

         -- manifest dictionary
         m := {HASHED_DICTIONARY[INTEGER, STRING] << 1, "one"; 2, "two" >>}
         check
            m.at ("one") = 1
         end
      end

   expanded_and_reference_classes
      local
         s: STRING
         i: INTEGER
         b: BOOLEAN
      do
         -- Eiffel classes are by default reference types.
         -- Uninitialized variables of reference types have the
         -- value Void (empty reference).
         -- In routine calls and assignments instances of reference
         -- types are passed by reference.
         check
            s = Void
         end

         create s.with_capacity (16) -- initialize by creating a new instance
         s := "hello" -- or by assignment

         -- Variables of types defined as expanded (e.g. most numeric classes)
         -- are automatically initialized to a default value.
         -- In routine calls and assignments instances of expanded
         -- types are passed by value.
         check
            i = 0
            b = False
         end
      end

   agents
         -- Agent is an object encapsulating an operation,
         -- akin to anonymous functions in other programming
         -- languages.
         -- Usual use case is with higher-order functions.
      local
         numbers: COLLECTION[INTEGER_64]
      do
         numbers := {ARRAY[INTEGER_64] 1, << 1, 2, 3 >>}

         -- Agent created by referencing a method of an object
         numbers.for_each (agent io.put_integer (?))

         -- inline agent definition
         numbers.for_each (agent (i: INTEGER_64)
                              do
                                 io.put_string ("* ")
                                 io.put_integer (i)
                                 io.put_new_line
                              end)
      end

   exceptions
      local
         exception_encountered: BOOLEAN
      do
         if exception_encountered then
            std_error.put_string ("Operation failed, there was an exception%N")
         else
            check
               False -- failing check, throws an exception
            end
         end
      rescue
         -- Note: 'exception' and 'Check_instruction' (as well as
         -- other exception-related features) are features inherited
         -- from EXCEPTIONS (see the beginning of this file)
         if exception = Check_instruction then
            std_error.put_string ("check instruction failed%N")
         else
            std_error.put_string ("Unexpected exception type ")
            std_error.put_integer (exception)
            std_error.put_new_line
         end

         -- Exception handling in Eiffel is conceptually different from
         -- mainstream programming languages.
         -- An exception signals a failed contract.
         -- The only available way to "rescue" such a failed contract is to
         -- somehow adapt so as to be able to fulfill the contract
         exception_encountered := True
         -- and then _retry_ the routine where the failed contract
         -- was detected
         retry
      end

invariant
   -- Class invariant Must be true at any point of
   -- each instance's existence,
   -- beginning when execution of the constructor is finished.
   -- (In a root class invariant isn't of much use,
   -- because for a root class termination of the constructor
   -- is also termination of the program's execution.)
   counter >= 2
   counter <= Max_count

end
```

A simple program like in the example above can be built
simply by specifying the source file (and possibly some compiler options)
on the command line: `se compile learn_liberty.e`.
For programs even slightly more complex than that
it's preferable to provide persistent build configuration
in an [ACE file](https://wiki.liberty-eiffel.org/index.php/ACE)
and compile by `se compile the_ace_file.ace`.

ACE stands for "Assembly of Classes in Eiffel", the file format
vaguely resembles Eiffel source code.
Once common throughout the Eiffel ecosystem, today it remains in use
only in Liberty Eiffel.

```
system
   "learn_liberty" -- name of the resulting executable

root
   LEARN_LIBERTY: make -- root class and method / program's main entry point

default
   assertion (require) -- by default only execute 'require' assertions at runtime

cluster
   -- paths to search for (clusters of) classes +
   -- cluster-specific compiler settings

   "./src"
      default
         assertion (all) -- in this cluster execute all assertions
      end

   "${path_liberty_core}/loadpath.se"
   "${path_liberty_extra}/loadpath.se"

generate
   clean (yes) -- delete intermediate files when compilation is finished
```

## Further Reading

* [Eiffel for beginners](https://www.maths.tcd.ie/~odunlain/eiffel/eiffel_course/eforb.htm):
  a concise introduction to a slightly older version of the language,
  but the Liberty Eiffel compiler is very friendly in recognizinig obsolete
  language constructs and suggesting updates
* [SmartEiffel: a short course](https://www.maths.tcd.ie/~odunlain/eiffel/eiffel_course/shortcrs/html/notes.html):
  one more similar introduction
* [Liberty Eiffel Wiki](https://wiki.liberty-eiffel.org/index.php/Main_Page) -
  the most comprehensive and most detailed Liberty-specific human-readable
  documentation available
* [Liberty Eiffel documentation](https://doc.liberty-eiffel.org/) -
  API documentation for library classes
* [Liberty Eiffel tutorial](https://github.com/LibertyEiffel/Liberty/tree/master/tutorial)
  through small self-contained programs showcasing individual features
* for topics not covered by the tutorial it's often useful to look for usage
  examples in [tests](https://github.com/LibertyEiffel/Liberty/tree/master/test),
  in the [source code](https://github.com/LibertyEiffel/Liberty/tree/master/src)
  of library classes, the compiler and core tools,
  as well as in personal projects of major contributors, like
  [cadrian/pwd](https://github.com/cadrian/pwd)
