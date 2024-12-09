---
language: Phix
contributors:
    - ["pxtom", "https://gitgub.com/pxtom"]
filename: learnphix.exw
---

```
            -- single line comment

            // single line comment

            /* multi-
                     line comment */

// Start programming immediately

    -- write using UTF8; save as: hello.ex
    -- use ? for output

        ? "😍 hello , 😎 world!"
        ? sqrt(2+2)

// Interpret your program
                            /*
       p hello               */

// Compile your program
                            /*
       p -c hello            */

// Coding mistakes receive gentle help messages
                                                          /*
    string line
    line = 5
           ^ type error (storing atom in string)           */

// Every literal value, constant, and variable is an ''object''

    -- a literal object
        ? "hello"
        ? PI
        ? { "hello", PI }

    -- a named variable object
        object X
        X = "hello"
        X = PI
        X = { "hello", PI }

    -- a named constant object
        constant myPI = 22/7

//  Everything is an ''object'', just two fundemental kinds
                                                                /*
            ┌────────────────────▄
          ┌─┤   object           █─┐
          │ └─▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█ │
          │                        │
       "atom"                   "container"                     */

       number  x = 3.14159      sequence  s   = { "hello", PI }
       integer y = 3            string    txt = "hello"

            -- simplify,
            -- and use only two primitives
            number x1=3.14156, y1=3
            sequence s1={"hello",PI}, txt1="hello"

                -- simplify even more,
                -- and use just one primitive
                object x2=3.14156, y2=3, s2={"hello",PI}, txt2="hello"

// Elegant data-type design

    -- invent your own "type"
    -- organize with "struct" or "class"
                                                                /*
    ╔═══════════════════════════════╗
    ║   ┌─────────────────────────┐ ║
    ║   │      ┌───────────▄      │ ║
    ║   │    ┌─┤ object    █─┐    │ ║
    ║   │    │ └─▄▄▄▄▄▄▄▄▄▄█ │    │ ║
    ║   │    │               │    │ ║
    ║   │  number        sequence │ ║
    ║   │    │               │    │ ║
    ║   │  integer        string  │ ║
    ║   └──────── type ───────────┘ ║
    ║                               ║
    ╚════════ struct                ║
              class ════════════════╝
                                                                */

// Syntax is consistant: "keyword...end keyword"

    -- no invisible syntax or extra rules needed.

    // loop
    -- while ... end while

        integer index = 1
        while index <= 5 do
            ? index
            index += 1
        end while

    // loop
    -- for ... end for

        for i=5 to 1 by -1 do
            ? i
        end for

    //  conditional
    --  if ... end if

        number p = 4
        if p < 1 then
            ? "p is a small number"
        elsif p > 10 then
            ? "p is a large number"
        else
            ? "p is inbetween"
        end if

    // conditional
    -- switch ... end switch

        object ch = prompt_string("enter one character: " )
        switch ch
            case "a": ? "ch is a"
            case "b": ? "ch is b"
            case "c": ? "ch is c"
            default:  ? "ch is something else"
        end switch

// Operators are always consistant; never overloaded.

    -- the + operator ''always adds''
        ? 2+7      --> 9
        ? 'A' + 32 --> 97

    -- the & operator ''always concatenates''
        ? 2 & 7                --> {2,7}
        ? "cat" & " " & "dog"  --> "cat dog"
        ? {1,2,3} & "fish"     --> {1,2,3} & "fish"
        pp( {1,2,3} & "fish" ) --> {1,2,3,102'f',105'i',115's',104'h'}

// Use ''sq_'' functions to span entire containers.

        ? sq_add( {1,2,3}, 10 )     --> {11,12,13}
        ? sq_sqrt( {4,9,16} )       --> {2,3,4}

// Functions must return a value

        function add2( number x, number y )
            number sum = x + y
            return sum
        end function
        ? add2( 4, 9 )

// Procedures do not return a value

        procedure sum_all( sequence lst )
            number sum = 0
            for i=1 to length(lst) do
                sum += lst[i]
            end for
            ? sum
        end procedure
        sum_all( {1,3,9,11} )

// Recursion and mutal recursion are permitted

        function factorial(number n)
            if n == 0 then
                return 1
            end if
            if n<0 then
                return "error, no negative numbers for factorials"
            end if
            return n * factorial(n - 1)
        end function
        ? factorial(5)

// User defined data-types

        -- defined like a function: type ... end type
        -- they are fully programmable; add your own features

        type positive( number x )
            if not integer(x) then
                ? "use integers for factorials"
                return False
            end if
            if x < 0 then
                ? "error, no negative numbers for factorials"
                return False
            end if
            return True
        end type

     -- use them to declare variables and parameters

        function factorial2( positive n )
            if n == 0 then return 1 end if
            return n * factorial2(n-1)
        end function
        ? factorial(5)

    -- to catch errors, and recover, use: try ... end try

       try
          ? factorial2( -5 )
       catch e
          ? "that was a mistake"
       end try

// Sequences are versatile

    -- multiple assignment
            number a, b, c
            {a,b,c} = { -100, -200/-2, -300*3 }
            ? a      --> -100
            ? b      -->  100
            ? c      --> -900

    -- swapping values
                ? a       --> -100
                ? c       --> -900
            {a,c} = {c,a}
                ? a       --> -900
                ? c       --> -100


// Symmetrical one-based indexing does it all

    -- both sequence and string are mutable and work alike

    --              1   2   3   4   5    -- index head to tail
             s = { 10, 20, 30, 40, 50 }
    --             -5  -4  -3  -2  -1    -- index tail to head

        // one item
            ? s[ 2]
            ? s[-4]
                        -- output for both is:
            ----->  20

        // slice with one item
            ? s[ 2.. 2]
            ? s[-4..-4]
                        -- output for both is:
            -----> {20}

        // inclusive slice
            ? s[ 2.. 4]
            ? s[-4..-2]
                        -- output for both is:
            -----> {20,30,40}

        // empty sequence
            ? s[3 .. 2]
            ? s[-3..-4]
                        -- output for both is:
            -----> {}

        // insert
            s[3..2] = {99}
            ? s
            -----> {10,20,99,30,40,50}

        // prepend and append
            s = { 10,20,30,40,50 }

            s[  1..0] = {0}             -- prepend
            s[$+1..$] = {6}             -- append

            ? s
            -----> {0,10,20,99,30,40,50,6}

            s[0..-1] = {9999}           -- append

            ? s
            -----> {0,10,20,99,30,40,50,6,9999}

        // delete
            s = { 10,20,30,40,50 }

            s[2..2] = {}        -- item deleted
            ? s
            -----> {10,30,40,50}

            s[2..3] = {}        -- slice deleted
            ? s
            -----> {10,50}

// Learn and reuse; you keep what you learn.

            s = { 1,3,5,7 }
            txt = "jello"

        -- "find" locates one item in either a sequence or a string
        ? find( 3, s ) --> 2
        ? find( 'e', txt ) --> 2

        -- "match" locates a slice in either a sequence or a string
        ? match( {5,7}, s ) -- > 3
        ? match( "ll", txt ) --> 3

// Look back at the examples, Phix is generic!

// Batteries are installed

        ? sort( {2, 54,6,4, 0} )
        ? upper( "cat" )
        ? log( 10.4 )
        ? trunc(1.4)         --  1
        ? floor(1.4)         --  1
        ? trunc(-1.4)        -- -1
        ? floor(-1.4)        -- -2

// Batteries are included

        include builtins/regex.e

        string str = "say hello and smile"
        str = gsub( `s...e`, str, "😍" )
        ? str   --> "say hello and 😍"

// Yes, sequences are "powerful"

        function odd(integer a) return remainder(a,2)=1 end function
        function even(integer a) return remainder(a,2)=0 end function

        ? tagset(10)                 --> {1,2,3,4,5,6,7,8,9,10}
        ? filter(tagset(10),odd)     --> {1,3,5,7,9}
        ? filter(tagset(10),even)    --> {2,4,6,8,10}

// A ''struct'' provides named fields, type-checking, and dot notation

        struct point
            number x = 0
            number y = 0
        end struct

        procedure show( point q )
            printf(1, "(%g,%g)", { q.x, q.y } )
        end procedure

        point  p1 = new()
        show(p1)
            --> (0,0)

        p1.x = 3
        p1.y = 5
        show( p1 )
            --> (3,5)

// A ''class'' adds methods and scope control

        class pair
            public number x = 0
            public number y = 0

            procedure show( )
                printf(1, "(%g,%g)", { this.x, this.y } )
            end procedure
        end class

        pair  p2 = new()
        p2.show()
            --> (0,0)

        p2.x = 3
        p2.y = 5
        p2.show()
            --> (3,5)

// Inherit and compose

        class Pair -- any 2 objects
            public sequence xy
            public integer x,y
                 function get_x()
                  return xy[1]
                 end function

                 function get_y()
                  return xy[2]
                 end function
        end class

        type pos_seq(sequence x)
            return min(x) >= 0
        end type

        class Point extends Pair
            public pos_seq loc -- any two numbers >= 0

                procedure set_loc(object x)
                    this.xy = {x[1],x[2]}
                end procedure
        end class

        class Rectangle extends Point
            public Point tlc,brc --top_left, bottom_right corners;
            public sequence size

                  function get_size()
                    this.size = {brc.x-tlc.x , brc.y-tlc.y}
                    return this.size
                  end function
        end class

        Point p1a = new() p1a.loc = {50,10}
        Point p2a = new() p2a.loc = {300,200}

        Rectangle r = new()
                  r.tlc = p1a
                  r.brc = p2a
        ? r          -- {"struct","Rectangle",4,1}
        ? r.tlc      -- {"struct","Point",3,3}

        ? r.size       --> {250,190}
        ? r.get_size() --> {250,190}
```

Phix does not (although most can be emulated) directly support
operator|builtin|function overloading, lambda expressions, closures,
currying, eval, partial function application, function composition,
function prototyping, monads, generators, anonymous recursion,
the Y combinator, aspect oriented programming, interfaces, delegates,
first class environments, implicit type conversion
(of the destructive kind), interactive programming, inverted syntax,
list comprehensions, metaprogramming, pointers
(other than to raw allocated memory), topic variables,
enforced singletons, safe mode, s-expressions,
or formal proof construction.

The author wryly comments:

''That should both scare off and attract the right people''.


## References

* [http://phix.x10.mx](http://phix.x10.mx)
* [Source code](https://github.com/petelomax/Phix)
* [Forum](https://openeuphoria.org/forum/index.wc)
* [Rosetta Code](https://rosettacode.org/wiki/Category:Phix)
