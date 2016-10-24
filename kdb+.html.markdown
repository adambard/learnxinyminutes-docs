---
language: kdb+
contributors:
    - ["Matt Doherty", "https://github.com/picodoc"]
    - ["Jonny Press", "jonny.press@aquaq.co.uk"]
filename: learnkdb.q
---

The q langauge and its database component kdb+ were developed by Arthur
Whitney and released by Kx systems in 2003. q is a descendant of
[APL](https://en.wikipedia.org/wiki/APL_(programming_language)) and as such is
very terse and a little strange looking for anyone from a "C heritage" language
background. Its expressiveness and vector oriented nature make it well suited
to performing complex calculations on large amounts of data (while also
encouraging some amount of [code
golf](https://en.wikipedia.org/wiki/Code_golf)). The fundamental structure in
the language is not the object but instead the list, and tables are built as
collections of lists. This means - unlike most traditional RDBMS systems -
tables are column oriented.  The language has both an in-memory and on-disk
database built in, giving a large amount of flexibility. kdb+ is most widely
used in the world of finance to store, analyze, process and retrieve large
time-series data sets.

The terms *q* and *kdb+* are usually used interchangeably, as the two are not
separable so this distinction is not really useful.

All Feedback welcome!  You can reach me at matt.doherty@aquaq.co.uk, or Jonny
at jonny.press@aquaq.co.uk

```
/ Single line comments start with a forward-slash
/ These can also be used in-line, so long as at least one whitespace character
/ separates it from text to the left
/
  A forward-slash on a line by itself starts a multiline comment
  and a backward-slash on a line by itself terminates it
\

/ Run this file in an empty directory


////////////////////////////////////
// Basic Operators and Datatypes  //
////////////////////////////////////

/ We have integers, which are 8 byte by default
3 / => 3

/ And floats, also 8 byte as standard.  Trailing f distinguishes from int
3.0 / => 3f

/ 4 byte numerical types can also be specified with trailing chars
3i / => 3i
3.0e / => 3e

/ Math is mostly what you would expect
1+1 / => 2
8-1 / => 7
10*2 / => 20
/ Except division, which uses percent (%) instead of forward-slash (/)
35%5 / => 7f  (the result of division is always a float)

/ For integer division we have the keyword div
4 div 3 / => 1

/ Modulo also uses a keyword, since percent (%) is taken
4 mod 3 / => 1

/ And exponentiation...
2 xexp 4 / => 16

/ ...and rounding...
floor 3.14159 / => 3

/ ...getting the absolute value...
abs -3.14159 / => 3.14159
/ ...and many other things
/ see http://code.kx.com/wiki/Reference for more

/ q has no operator precedence, everything is evaluated right to left
/ so results like this might take some getting used to
2*1+1 / => 4 / (no operator precedence tables to remember!)

/ Precedence can be modified with parentheses (restoring the 'normal' result)
(2*1)+1 / => 3

/ Assignment uses colon (:) instead of equals (=)
/ No need to declare variables before assignment
a:3
a / => 3

/ Variables can also be assigned in-line
/ this does not affect the value passed on
c:3+b:2+a:1 / (data "flows" from right to left)
a / => 1
b / => 3
c / => 6

/ In-place operations are also as you might expect
a+:2
a / => 3

/ There are no "true" or "false" keywords in q
/ boolean values are indicated by the bit value followed by b
1b / => true value
0b / => false value

/ Equality comparisons use equals (=) (since we don't need it for assignment)
1=1 / => 1b
2=1 / => 0b

/ Inequality uses <>
1<>1 / => 0b
2<>1 / => 1b

/ The other comparisons are as you might expect
1<2 / => 1b
1>2 / => 0b
2<=2 / => 1b
2>=2 / => 1b

/ Comparison is not strict with regard to types...
42=42.0 / => 1b

/ ...unless we use the match operator (~)
/ which only returns true if entities are identical
42~42.0 / => 0b

/ The not operator returns true if the underlying value is zero
not 0b / => 1b
not 1b / => 0b
not 42 / => 0b
not 0.0 / => 1b

/ The max operator (|) reduces to logical "or" for bools
42|2.0 / => 42f
1b|0b / => 1b

/ The min operator (&) reduces to logical "and" for bools
42&2.0 / => 2f
1b&0b / => 0b

/ q provides two ways to store character data
/ Chars in q are stored in a single byte and use double-quotes (")
ch:"a"
/ Strings are simply lists of char (more on lists later)
str:"This is a string"
/ Escape characters work as normal
str:"This is a string with \"quotes\""

/ Char data can also be stored as symbols using backtick (`)
symbol:`sym
/ Symbols are NOT LISTS, they are an enumeration
/ the q process stores internally a vector of strings
/ symbols are enumerated against this vector
/ this can be more space and speed efficient as these are constant width

/ The string function converts to strings
string `symbol / => "symbol"
string 1.2345 / => "1.2345"

/ q has a time type...
t:01:00:00.000
/ date type...
d:2015.12.25
/ and a datetime type (among other time types)
dt:2015.12.25D12:00:00.000000000

/ These support some arithmetic for easy manipulation
dt + t / => 2015.12.25D13:00:00.000000000
t - 00:10:00.000 / => 00:50:00.000
/ and can be decomposed using dot notation
d.year / => 2015i
d.mm / => 12i
d.dd / => 25i
/ see http://code.kx.com/wiki/JB:QforMortals2/atoms#Temporal_Data for more

/ q also has an infinity value so div by zero will not throw an error
1%0 / => 0w
-1%0 / => -0w

/ And null types for representing missing values
0N / => null int
0n / => null float
/ see http://code.kx.com/wiki/JB:QforMortals2/atoms#Null_Values for more

/ q has standard control structures
/ if is as you might expect (; separates the condition and instructions)
if[1=1;a:"hi"]
a / => "hi"
/ if-else uses $ (and unlike if, returns a value)
$[1=0;a:"hi";a:"bye"] / => "bye"
a / => "bye"
/ if-else can be extended to multiple clauses by adding args separated by ;
$[1=0;a:"hi";0=1;a:"bye";a:"hello again"]
a / => "hello again"


////////////////////////////////////
////      Data Structures       ////
////////////////////////////////////

/ q is not an object oriented language
/ instead complexity is built through ordered lists
/ and mapping them into higher order structures: dictionaries and tables

/ Lists (or arrays if you prefer) are simple ordered collections
/ they are defined using parentheses () and semi-colons (;)
(1;2;3) / => 1 2 3
(-10.0;3.14159e;1b;`abc;"c")
/ => -10f
/ => 3.14159e
/ => 1b
/ => `abc
/ => "c"  (mixed type lists are displayed on multiple lines)
((1;2;3);(4;5;6);(7;8;9))
/ => 1 2 3
/ => 4 5 6
/ => 7 8 9

/ Lists of uniform type can also be defined more concisely
1 2 3 / => 1 2 3
`list`of`syms / => `list`of`syms
`list`of`syms ~ (`list;`of;`syms) / => 1b

/ List length
count (1;2;3) / => 3
count "I am a string" / => 13 (string are lists of char)

/ Empty lists are defined with parentheses
l:()
count l / => 0

/ Simple variables and single item lists are not equivalent
/ parentheses syntax cannot create a single item list (they indicate precedence)
(1)~1 / => 1b
/ single item lists can be created using enlist
singleton:enlist 1
/ or appending to an empty list
singleton:(),1
1~(),1 / => 0b

/ Speaking of appending, comma (,) is used for this, not plus (+)
1 2 3,4 5 6 / => 1 2 3 4 5 6
"hello ","there" / => "hello there"

/ Indexing uses square brackets []
l:1 2 3 4
l[0] / => 1
l[1] / => 2
/ indexing out of bounds returns a null value rather than an error
l[5] / => 0N
/ and indexed assignment
l[0]:5
l / => 5 2 3 4

/ Lists can also be used for indexing and indexed assignment
l[1 3] / => 2 4
l[1 3]: 1 3
l / => 1 1 3 3

/ Lists can be untyped/mixed type
l:(1;2;`hi)
/ but once they are uniformly typed, q will enforce this
l[2]:3
l / => 1 2 3
l[2]:`hi / throws a type error
/ this makes sense in the context of lists as table columns (more later)

/ For a nested list we can index at depth
l:((1;2;3);(4;5;6);(7;8;9))
l[1;1] / => 5

/ We can elide the indexes to return entire rows or columns
l[;1] / => 2 5 8
l[1;] / => 4 5 6

/ All the functions mentioned in the previous section work on lists natively
1+(1;2;3) / => 2 3 4 (single variable and list)
(1;2;3) - (3;2;1) / => -2 0 2 (list and list)

/ And there are many more that are designed specifically for lists
avg 1 2 3 / => 2f
sum 1 2 3 / => 6
sums 1 2 3 / => 1 3 6 (running sum)
last 1 2 3 / => 3
1 rotate 1 2 3 / => 2 3 1
/ etc.
/ Using and combining these functions to manipulate lists is where much of the
/ power and expressiveness of the language comes from

/ Take (#), drop (_) and find (?) are also useful working with lists
l:1 2 3 4 5 6 7 8 9
/ take the first 5 elements
5#l / => 1 2 3 4 5
/ drop the first 5
5_l / => 6 7 8 9
/ take the last 5
-5#l / => 5 6 7 8 9
/ drop the last 5
-5_l / => 1 2 3 4 5
/ find the first occurance of 4
l?4 / => 3
l[3] / => 4

/ Dictionaries in q are a generalization of lists
/ they map a list to another list (of equal length)
/ the bang (!) symbol is used for defining a dictionary
d:(`a;`b;`c)!(1;2;3)
/ or more simply with concise list syntax
d:`a`b`c!1 2 3
/ the keyword key returns the first list
key d / => `a`b`c
/ and value the second
value / => 1 2 3

/ Indexing is indentical to lists
/ with the first list as a key instead of the position
d[`a] / => 1
d[`b] / => 2

/ As is assignment
d[`c]:4
d
/ => a| 1
/ => b| 2
/ => c| 4

/ Arithmetic and comparison work natively, just like lists
e:(`a;`b;`c)!(2;3;4)
d+e
/ => a| 3
/ => b| 5
/ => c| 8
d-2
/ => a| -1
/ => b| 0
/ => c| 2
d > (1;1;1)
/ => a| 0
/ => b| 1
/ => c| 1

/ And the take, drop and find operators are remarkably similar too
`a`b#d
/ => a| 1
/ => b| 2
`a`b _ d
/ => c| 4
d?2
/ => `b

/ Tables in q are basically a subset of dictionaries
/ a table is a dictionary where all values must be lists of the same length
/ as such tables in q are column oriented (unlike most RDBMS)
/ the flip keyword is used to convert a dictionary to a table
/ i.e. flip the indices
flip `c1`c2`c3!(1 2 3;4 5 6;7 8 9)
/ => c1 c2 c3
/ => --------
/ => 1  4  7
/ => 2  5  8
/ => 3  6  9
/ we can also define tables using this syntax
t:([]c1:1 2 3;c2:4 5 6;c3:7 8 9)
t
/ => c1 c2 c3
/ => --------
/ => 1  4  7
/ => 2  5  8
/ => 3  6  9

/ Tables can be indexed and manipulated in a similar way to dicts and lists
t[`c1]
/ => 1 2 3
/ table rows are returned as dictionaries
t[1]
/ => c1| 2
/ => c2| 5
/ => c3| 8

/ meta returns table type information
meta t
/ => c | t f a
/ => --| -----
/ => c1| j
/ => c2| j
/ => c3| j
/ now we see why type is enforced in lists (to protect column types)
t[1;`c1]:3
t[1;`c1]:3.0 / throws a type error

/ Most traditional databases have primary key columns
/ in q we have keyed tables, where one table containing key columns
/ is mapped to another table using bang (!)
k:([]id:1 2 3)
k!t
/ => id| c1 c2 c3
/ => --| --------
/ => 1 | 1  4  7
/ => 2 | 2  5  8
/ => 3 | 3  6  9

/ We can also use this shortcut for defining keyed tables
kt:([id:1 2 3]c1:1 2 3;c2:4 5 6;c3:7 8 9)

/ Records can then be retreived based on this key
kt[1]
/ => c1| 1
/ => c2| 4
/ => c3| 7
kt[`id!1]
/ => c1| 1
/ => c2| 4
/ => c3| 7


////////////////////////////////////
////////     Functions      ////////
////////////////////////////////////

/ In q the function is similar to a mathematical map, mapping inputs to outputs
/ curly braces {} are used for function definition
/ and square brackets [] for calling functions (just like list indexing)
/ a very minimal function
f:{x+x}
f[2] / => 4

/ Functions can be annonymous and called at point of definition
{x+x}[2] / => 4

/ By default the last expression is returned
/ colon (:) can be used to specify return
{x+x}[2] / => 4
{:x+x}[2] / => 4
/ semi-colon (;) separates expressions
{r:x+x;:r}[2] / => 4

/ Function arguments can be specified explicitly (separated by ;)
{[arg1;arg2] arg1+arg2}[1;2] / => 3
/ or if ommited will default to x, y and z
{x+y+z}[1;2;3] / => 6

/ Built in functions are no different, and can be called the same way (with [])
+[1;2] / => 3
<[1;2] / => 1b

/ Functions are first class in q, so can be returned, stored in lists etc.
{:{x+y}}[] / => {x+y}
(1;"hi";{x+y})
/ => 1
/ => "hi"
/ => {x+y}

/ There is no overloading and no keyword arguments for custom q functions
/ however using a dictionary as a single argument can overcome this
/ allows for optional arguments or differing functionality
d:`arg1`arg2`arg3!(1.0;2;"my function argument")
{x[`arg1]+x[`arg2]}[d] / => 3.0

/ Functions in q see the global scope
a:1
{:a}[] / => 1

/ However local scope obscures this
a:1
{a:2;:a}[] / => 2

/ Functions cannot see nested scopes (only local and global)
{local:1;{:local}[]}[] / throws error as local is not defined in inner function

/ A function can have one or more of it's arguments fixed (projection)
f:+[4]
f[4] / => 8
f[5] / => 9
f[6] / => 10


////////////////////////////////////
//////////     q-sql      //////////
////////////////////////////////////

/ q has it's own syntax for manipulating tables, similar to standard SQL
/ This contains the usual suspects of select, insert, update etc.
/ and some new functionality not typically available
/ q-sql has two significant differences (other than syntax) to normal SQL:
/ - q tables have well defined record orders
/ - tables are stored as a collection of columns
/   (so vectorized column operations are fast)
/ a full description of q-sql is a little beyond the scope of this intro
/ so we will just cover enough of the basics to get you going

/ First define ourselves a table
t:([]name:`Arthur`Thomas`Polly;age:35 32 52;height:180 175 160;sex:`m`m`f)

/ equivalent of SELECT * FROM t
select from t / (must be lower case, and the wildcard is not necessary)
/ => name   age height
/ => -----------------
/ => Arthur 35  180
/ => Thomas 32  175
/ => Polly  52  160

/ Select specific columns
select name,age from t
/ => name   age
/ => ----------
/ => Arthur 35
/ => Thomas 32
/ => Polly  52

/ And name them (equivalent of using AS in standard SQL)
select charactername:name, currentage:age from t
/ => charactername currentage
/ => ------------------------
/ => Arthur        35
/ => Thomas        32
/ => Polly         52

/ This SQL syntax is integrated with the q language
/ so q can be used seamlessly in SQL statements
select name, feet:floor height*0.032, inches:12*(height*0.032) mod 1 from t
/ => name   feet inches
/ => ------------------
/ => Arthur 5    9.12
/ => Thomas 5    7.2

/ Including custom functions
select name, growth:{[h;a]h%a}[height;age] from t
/ => name   growth
/ => ---------------
/ => Arthur 5.142857
/ => Thomas 5.46875
/ => Polly  3.076923

/ The where clause can contain multiple statements separated by commas
select from t where age>33,height>175
/ => name   age height
/ => -----------------
/ => Arthur 35  180

/ The where statements are executed sequentially (not the same as logical AND)
select from t where age<40,height=min height
/ => name   age height
/ => -----------------
/ => Thomas 32  175
select from t where (age<40)&(height=min height)
/ => name age height
/ => ---------------

/ The by clause falls between select and from
/ and is equivalent to SQL's GROUP BY
select avg height by sex from t
/ => sex| height
/ => ---| ------
/ => f  | 160
/ => m  | 177.5

/ If no aggreation function is specified, last is assumed
select by sex from t
/ => sex| name   age height
/ => ---| -----------------
/ => f  | Polly  52  160
/ => m  | Thomas 32  175

/ Update has the same basic form as select
update sex:`male from t where sex=`m
/ => name   age height sex
/ => ----------------------
/ => Arthur 35  180    male
/ => Thomas 32  175    male
/ => Polly  52  160    f

/ As does delete
delete from t where sex=`m
/ => name  age height sex
/ => --------------------
/ => Polly 52  160    f

/ None of these sql operations are carried out in place
t
/ => name   age height sex
/ => ---------------------
/ => Arthur 35  180    m
/ => Thomas 32  175    m
/ => Polly  52  160    f

/ Insert however is in place, it takes a table name, and new data
`t insert (`John;25;178;`m)
t
/ => name   age height sex
/ => ---------------------
/ => Arthur 35  180    m
/ => Thomas 32  175    m
/ => Polly  52  160    f
/ => John   25  178    m

/ Upsert is similar (but doesn't have to be in-place)
t upsert (`Chester;58;179;`m)
/ => name    age height sex
/ => ----------------------
/ => Arthur  35  180    m
/ => Thomas  32  175    m
/ => Polly   52  160    f
/ => John    25  178    m
/ => Chester 58  179    m

/ it will also upsert dicts or tables
t upsert `name`age`height`sex!(`Chester;58;179;`m)
t upsert (`Chester;58;179;`m)
/ => name    age height sex
/ => ----------------------
/ => Arthur  35  180    m
/ => Thomas  32  175    m
/ => Polly   52  160    f
/ => John    25  178    m
/ => Chester 58  179    m

/ And if our table is keyed
kt:`name xkey t
/ upsert will replace records where required
kt upsert ([]name:`Thomas`Chester;age:33 58;height:175 179;sex:`f`m)
/ => name   | age height sex
/ => -------| --------------
/ => Arthur | 35  180    m
/ => Thomas | 33  175    f
/ => Polly  | 52  160    f
/ => John   | 25  178    m
/ => Chester| 58  179    m

/ There is no ORDER BY clause in q-sql, instead use xasc/xdesc
`name xasc t
/ => name   age height sex
/ => ---------------------
/ => Arthur 35  180    m
/ => John   25  178    m
/ => Polly  52  160    f
/ => Thomas 32  175    m

/ Most of the standard SQL joins are present in q-sql, plus a few new friends
/ see http://code.kx.com/wiki/JB:QforMortals2/queries_q_sql#Joins
/ the two most important (commonly used) are lj and aj

/ lj is basically the same as SQL LEFT JOIN
/ where the join is carried out on the key columns of the left table
le:([sex:`m`f]lifeexpectancy:78 85)
t lj le
/ => name   age height sex lifeexpectancy
/ => ------------------------------------
/ => Arthur 35  180    m   78
/ => Thomas 32  175    m   78
/ => Polly  52  160    f   85
/ => John   25  178    m   78

/ aj is an asof join.  This is not a standard SQL join, and can be very powerful
/ The canonical example of this is joining financial trades and quotes tables
trades:([]time:10:01:01 10:01:03 10:01:04;sym:`msft`ibm`ge;qty:100 200 150)
quotes:([]time:10:01:00 10:01:01 10:01:01 10:01:03;
          sym:`ibm`msft`msft`ibm; px:100 99 101 98)
aj[`time`sym;trades;quotes]
/ => time     sym  qty px
/ => ---------------------
/ => 10:01:01 msft 100 101
/ => 10:01:03 ibm  200 98
/ => 10:01:04 ge   150
/ for each row in the trade table, the last (prevailing) quote (px) for that sym
/ is joined on.
/ see http://code.kx.com/wiki/JB:QforMortals2/queries_q_sql#Asof_Join

////////////////////////////////////
/////     Extra/Advanced      //////
////////////////////////////////////

////// Adverbs //////
/ You may have noticed the total lack of loops to this point
/ This is not a mistake!
/ q is a vector language so explicit loops (for, while etc.) are not encouraged
/ where possible functionality should be vectorized (i.e. operations on lists)
/ adverbs supplement this, modifying the behaviour of functions
/ and providing loop type functionality when required
/ (in q functions are sometimes refered to as verbs, hence adverbs)
/ the "each" adverb modifies a function to treat a list as individual variables
first each (1 2 3;4 5 6;7 8 9)
/ => 1 4 7

/ each-left (\:) and each-right (/:) modify a two-argument function
/ to treat one of the arguments and individual variables instead of a list
1 2 3 +\: 1 2 3
/ => 2 3 4
/ => 3 4 5
/ => 4 5 6
1 2 3 +/: 1 2 3
/ => 2 3 4
/ => 3 4 5
/ => 4 5 6

/ The true alternatives to loops in q are the adverbs scan (\) and over (/)
/ their behaviour differs based on the number of arguments the function they
/ are modifying receives.  Here I'll summarise some of the most useful cases
/ a single argument function modified by scan given 2 args behaves like "do"
{x * 2}\[5;1] / => 1 2 4 8 16 3 (i.e. multiply by 2, 5 times)
{x * 2}/[5;1] / => 32 (using over only the final result is shown)

/ If the first argument is a function, we have the equivalent of "while"
{x * 2}\[{x<100};1] / => 1 2 4 8 16 32 64 128 (iterates until returns 0b)
{x * 2}/[{x<100};1] / => 128 (again returns only the final result)

/ If the function takes two arguments, and we pass a list, we have "for"
/ where the result of the previous execution is passed back into the next loop
/ along with the next member of the list
{x + y}\[1 2 3 4 5] / => 1 3 6 10 15 (i.e. the running sum)
{x + y}/[1 2 3 4 5] / => 15 (only the final result)

/ There are other adverbs and uses, this is only intended as quick overview
/ http://code.kx.com/wiki/JB:QforMortals2/functions#Adverbs

////// Scripts //////
/ q scripts can be loaded from a q session using the "\l" command
/ for example "\l learnkdb.q" will load this script
/ or from the command prompt passing the script as an argument
/ for example "q learnkdb.q"

////// On-disk data //////
/ Tables can be persisted to disk in several formats
/ the two most fundamental are serialized and splayed
t:([]a:1 2 3;b:1 2 3f)
`:serialized set t / saves the table as a single serialized file
`:splayed/ set t / saves the table splayed into a directory

/ the dir structure will now look something like:
/ db/
/ ├── serialized
/ └── splayed
/     ├── a
/     └── b

/ Loading this directory (as if it was as script, see above)
/ loads these tables into the q session
\l .
/ the serialized table will be loaded into memory
/ however the splayed table will only be mapped, not loaded
/ both tables can be queried using q-sql
select from serialized
/ => a b
/ => ---
/ => 1 1
/ => 2 2
/ => 3 3
select from splayed / (the columns are read from disk on request)
/ => a b
/ => ---
/ => 1 1
/ => 2 2
/ => 3 3
/ see http://code.kx.com/wiki/JB:KdbplusForMortals/contents for more

////// Frameworks //////
/ kdb+ is typically used for data capture and analysis.
/ This involves using an architecture with multiple processes
/ working together.  kdb+ frameworks are available to streamline the setup
/ and configuration of this architecuture and add additional functionality
/ such as disaster recovery, logging, access, load balancing etc.
/ https://github.com/AquaQAnalytics/TorQ
```

## Want to know more?

* [*q for mortals* q language tutorial](http://code.kx.com/wiki/JB:QforMortals2/contents)
* [*kdb for mortals* on disk data tutorial](http://code.kx.com/wiki/JB:KdbplusForMortals/contents)
* [q language reference](http://code.kx.com/wiki/Reference)
* [Online training courses](http://training.aquaq.co.uk/)
* [TorQ production framework](https://github.com/AquaQAnalytics/TorQ)
