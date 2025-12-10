---
name: Vim9 script
filename: learnvim9script.vim
contributors:
  - ["Alejandro Sanchez", "http://hiphish.github.io/"]
  - ["LacyGoill", "https://github.com/lacygoill"]
  - ["Peter Kenny", "https://github.com/kennypete"]
  - ["Ubaldo Tiberi", "https://github.com/ubaldot"]
  - ["Yegappan Lakshmanan", "https://github.com/yegappan"]
---

## About Vim9 script

Vim9 script is a modernized, TypeScript-like language to script the Vim
text editor.  It improves significantly on its predecessor,
legacy [Vim script](https://learnxinyminutes.com/vimscript/) (_aka_ “VimL”).

In Vim9 script, Ex commands (such as `:echo`, `:write`, `:substitute`,
etc.) can be used inside functions and, vice-versa, you can call a function
with an Ex command, using `:vim9cmd` on the command line.
However, Vim9 script removes many of legacy Vim script’s unusual
and esoteric features (see
[vim9-differences](https://vimhelp.org/vim9.txt.html#vim9-differences)).
Vim9 script also enforces stricter syntax, markedly improves performance,
and supports modern programming features such as strong typing, classes,
and generic functions.

### Sourcing the Code Blocks

The following sections include short, complete, and sourceable scripts.
For how to source them within Vim, see
[source-range](https://vimhelp.org/repeat.txt.html#%3Asource-range).
If using the link at "Get the code:
[learnvim9script.vim](https://learnxinyminutes.com/files/learnvim9script.vim)",
near the top of this page, each `vim9script` block delineates a distinct,
sourceable script.

## 1. Types, Operators, Expressions, and Regular Expressions

### Primitive and Collection Types

```
" The `vim9script` command is required to distinguish Vim9 script from
" legacy Vim script in a `.vim` file.  Before the command, legacy Vim script
" comments (starting with the " character) can be used, like these lines do.
"
" After the `vim9script` command, the # character is used for comments

vim9script
# PRIMITIVE AND COLLECTION TYPES

# Vim’s builtin function `typename()` may be used to reveal the type

# Primitive data types
echo typename(1)           # number
echo typename(1.1)         # float
echo typename(true)        # bool

# Collection data types
echo typename("Hi")        # string
echo typename(0zFE0F)      # blob (a binary object)
echo typename([1])         # list<number>
echo typename((1, ))       # tuple<number>
echo typename({1: 'one'})  # dict<string>

# Type casting may be used to fail early when there is a type mismatch
echo <number>3             # 3
echo <number>'3'           # E1012: Type mismatch; expected number but got str

# ----------------------------------------------------------------------------
```

### Operators and Expressions

```
vim9script
# OPERATORS AND EXPRESSIONS

# Arithmetic with the number (integer) type
echo 1 + 1                 # 2
echo 2 - 1                 # 1
echo 3 * 2                 # 6
echo 8 / 2                 # 4

# If the result is not an integer, the remainder is not returned
echo 9 / 2                 # 4
# But modulo returns the remainder
echo 9 % 2                 # 1

# The float type returns with the applicable number of decimal places
echo 1.25 * 6.1            # 7.625
echo 3.1405 + 0.0005       # 3.141
# An integer and float expression returns a float
echo 9 / 2.0               # 4.5

# ----------------------------------------------------------------------------
```

### Logical and Comparison Operators

```
vim9script
# LOGICAL AND COMPARISON OPERATORS

# Logical OR (||), AND (&&), and NOT (!)
echo true || false         # true
echo true && false         # false
echo !true                 # false

# Equality (==) and inequality (!=) work on all three primitive types and
# comparisons (>, >=, <=, and <) on numbers and floats
echo [1, 2] == [1, 2]      # true
echo 'apples' != 'pears'   # true
echo 9 > 8                 # true
echo 8 >= 8                # true
echo 8 <= 9                # true
echo 8 < 9                 # true

# Ternary operator
echo 9 > 8 ? true : false  # true

# Falsy operator ("null coalescing operator")
echo 9 > 8 ?? 1 + 1        # true
echo 8 > 9 ?? 1 + 1        # 2

# Bitwise operators (>> and <<)
echo 1 << 2                # 4
echo 9 >> 1                # 5

# ----------------------------------------------------------------------------
```

### Numbers

```
vim9script
# NUMBERS

# Numbers may be expressed as decimal, hexadecimal (prefixed with 0x),
# octal (prefixed with 0o or 0O), or binary (prefixed with 0b)

# These are all decimal 15
echo 15                       # 15
echo 0b1111                   # 15
echo 0xF                      # 15
echo 0o17                     # 15

# The maximum number is either a 32 or 64 bit signed number, which may
# be checked using Vim’s builtin variable `v:numbersize`
echo v:numbersize             # either 32 or 64

# The implication is that any arithmetic where the number exceeds the
# permitted value (for 64-bit 9,223,372,036,854,775,807) will fail
echo 9223372036854775807 + 1  # -9223372036854775808

# Not-a-real-number values may be tested with `isnan()` and `isinf()` builtin
# functions - the second and third lines also illustrate method calling
echo isnan(0.0 / 0.0)         # nan
echo 1.0 / 0.0 ->isinf()      # inf
echo -1.0 / 0.0 ->isinf()     # -inf

# ----------------------------------------------------------------------------
```

### Strings

```
vim9script
# STRINGS

# String concatenation with `..`
echo "Hello" .. " " .. 'world'    # Hello world

# String interpolation with `$`
echo $"Adam is {1 + 41}"          # Adam is 42

# String indexing is by character (like Java/Python 3) not byte (like C/Rust)
echo 'fenêtre'[-4 : -1]           # être

# Vim has dozens of builtin string functions, which can be called
# procedurally, as methods, and may be chained
echo toupper("summer")            # SUMMER
echo 'metre'->strcharlen()        # 5
echo "Ho"->reverse()->tolower()   # oh

# Pattern matching on strings
echo 'foobar' =~ 'foo'            # true (matches pattern)
echo 'foobar' !~ 'baz'            # true (does not match pattern)

# ----------------------------------------------------------------------------
```

### Regular Expressions

```
vim9script
# REGULAR EXPRESSIONS

# Vim uses a distinct flavor of regular expressions, though the basics
# are similar to many other languages.

# Using Vim's builtin function matchstr() to show regex matches:
echo matchstr("Hello", '.....')                 # '.' is any character
echo matchstr("Hello", 'Hel*o')                 # ('*' is zero+ times)
echo matchstr("Hello", 'H.\+')                  # ('\+' is one+ times)
echo matchstr("Hello", '\cheLlo')               # ('\c' is case-insensitive)
echo matchstr("Hello", '^Hello$')               # ('^' is start, '$' is end)
echo matchstr("Hello", 'H\(e\|x\)llo')          # ('\|' is or)

# Shorthand character classes and POSIX bracket expressions may be used
# \u is uppercase, \a is alpha, \s is space or tab, \d is decimal digit
echo "Dec 2025" =~ '\u\a\a\s\d\{4\}'                        # true
# [[:digit:]] is a decimal digit as are [0-9] and \d
echo "2025-12-10" =~ '[[:digit:]]\{4\}-[0-9]\{2\}-\d\{2\}'  # true

# Some things like "very magic" and "very nomagic" are unique to Vim:
#   \v  very magic means most chars are special, closer to extended regexes
#   \V  very nomagic, all but \ and the terminating character are literal
echo matchstr("Hello", '\v^(.*)$')                          # Hello
echo matchstr("Hello", '\V\^\(\.\*\)\$')                    # Hello

# \zs sets the start of a match and \ze sets the end of the match
var email = 'Addy user@example.com is fictitious'
echo matchstr(email, '@\zs\a\+[.]com\ze[^\w]')              # example.com

# PCRE-style assertions may also be used:
#   \@<= is positive lookbehind, \@= is positive lookahead
echo matchstr(email, '@\@<=\a\+[.]com\@=[^\w]')             # example.com

#   \@<! is negative lookbehind, \@= is negative lookahead
echo matchstr(email, '[^@]\@<!\a\+[.]com\([\w]\)\@!')       # example.com

# Combined with very magic they are easier to read
echo matchstr(email, '\v[@]@<=\a+[.]com@=[^\w]')            # example.com
echo matchstr(email, '\v[^@]@<!\a+[.]com([\w])@!')          # example.com

# ----------------------------------------------------------------------------
```

## 2. Variables

### Declarations

```
vim9script
# DECLARATIONS

# `var` is used to declare a variable, which may have a type specified
var count = 10               # type inferred (number)
var name: string = 'Vim'     # type declared

# When the type is a list or dict, the list’s type(s) must be declared too,
# though `<any>` may be used to allow mixed types
var a_list: list<list<number>> = [[1, 2], [3, 4]]
var a_dict: dict<any> = {a: 1, b: 'two'}
echo $"a_list is type {typename(a_list)} and a_dict is type {typename(a_dict)}"

# Constants
# `const` may be used to make both the variable and values constant
const PI: dict<float> = {2: 3.14, 4: 3.1415}  # Cannot add items to this dict
echo PI[4]                   # 3.1415
# `final` may be used to make the variable a constant and the values mutable.
final pi: dict<float> = {2: 3.14, 4: 3.1415}
# Adding a key-value pair to `pi`
pi[3] = 3.142
echo pi                      # {2: 3.14, 3: 3.142, 4: 3.1415}

# Dictionary key-value pairs may also be accessed using `{dict}.key`.
echo pi.4                    # 3.1415

# ----------------------------------------------------------------------------
```

### Manipulating Container Variables

```
vim9script
# MANIPULATING CONTAINER VARIABLES

# There are many builtin functions for variable manipulation. These are a few
# relevant to lists.
var MyList = ['a', 'b', 'c']
echo MyList[0]               # a
MyList->add('d')             # Adds 'd' to the end of the list
MyList->remove(2)            # Removes item at index 2 (starting at 0)
echo join(MyList, ', ')      # a, b, d

# String interpolation with $.  (More idiomatic than `printf()`)
echo $"The first and last items in MyList are '{MyList[0]}' and '{MyList[-1]}'"

# ----------------------------------------------------------------------------
```

### Variables’ Scopes

```
vim9script
# VARIABLES’ SCOPES

# Variables exist within scopes.  When unprefixed their scope is script-local.
# Other scopes are global (prefixed with `g:`), window-local (`w:`), 
# buffer-local (`b:`), and tab-local (`t:`).  Vim also has many Vim-defined
# (`v:`) global variables.
var my_var: string = 'Script-local variables have no prefix in Vim9 script'
g:my_var = 'Global variables with "g:" prefixed are available everywhere'
b:my_var = 'Buffer variables ("b:") are only visible within a buffer'
echo $"{my_var}\n{g:my_var}\n{b:my_var}\n"
echo v:version  # Vim variable for a Vim instance's major version (901 == 9.1)

# ----------------------------------------------------------------------------
```

### Registers

```
vim9script
# REGISTERS

# Registers are a form of variable used to store string values of many
# pre-defined and user-defined items.  When used in a Vim9 script, they are
# prefixed with `@`.

echo 'Current register values:'
echo "- Last command (:)\t" .. @:
echo "- Last search (/) \t" .. @/
echo "- Current file (%)\t" .. @%
@a = 'There are 26 named registers "a to "z and "A to "Z.'
echo $"- Register a equals\t{@a}"

# ----------------------------------------------------------------------------
```

### Builtin Variables and Settings

```
vim9script
# BUILTIN VARIABLES AND SETTINGS

echo $MYVIMRC       # (location of your .vimrc or, using Windows, _vimrc file)
echo $VIMRUNTIME    # (the Vim directory of the current Vim executable)

# Vim has many settings variables.  Some are global in scope, some are local,
# and others are both
echo &fileencoding  # (buffer-local: the character encoding, e.g., utf-8)
echo &equalalways   # (global: do/don't make newly split windows the same size)

# ----------------------------------------------------------------------------
```

## 3. Control Flow

### Conditionals and Loops

```
vim9script
# CONDITIONALS AND LOOPS

const INT: number = 5

# If / Else
if INT > 5
  echo 'big'
elseif INT == 5
  echo 'medium'
else
  echo 'small'
endif

# For loop (using builtin function range() for INT times, 0 indexed)
for j in INT->range()
  echo j
endfor

# While loop (using builtin function add() to add values to the list l)
var k: number
var l: list<number>
while k < INT
  l->add(k)
  k += 1
endwhile
echo l

# Loop control using a tuple literal
for x in (1, 2, 3)
  if x == 2
    continue
  elseif x == 3
    echo $'Stopping at {x}'
    break
  endif
  echo x
endfor

# ----------------------------------------------------------------------------
```

### Exceptions

```
vim9script
# EXCEPTIONS

try
  DoesNotExist()  # This fails
catch
  echo 'Function DoesNotExist() does not exist!'
endtry

try
  var lines = readfile('nofile.txt')
catch
  echo v:exception
finally
  echo 'Done'
endtry

try
  if !filereadable('file.txt')
    throw 'MyError'
  else
    # Read
    var lines = readfile('file.txt')
    # Append
    writefile(['line3'], 'file.txt', 'a')
    echo lines
  endif
catch /MyError/
  echo 'File not found'
endtry

# ----------------------------------------------------------------------------
```

## 4. Functions, Lambdas, and Closures

### Function Basics

```
vim9script
# FUNCTION BASICS

# :def functions have 0+ arguments and a return type, which may be `void`
def Add(x: number, y: number): void
  echo x + y
enddef

Add(3, 4)                # Echos 7

# Arguments may be defaulted
def Power(base: number, exp: number = 2): number
  return float2nr(pow(base, exp))
enddef

echo Power(8)            # Echos 64 because `exp` defaults to 2

# There may be a variable number of arguments
def MinMax(...args: list<number>): tuple<number, number>
  return (args->min(), args->max())
enddef

echo MinMax(8, 9, 2, 4)  # Echoes (2, 9)

# ----------------------------------------------------------------------------
```

### Generic Functions

```
vim9script
# GENERIC FUNCTIONS

# Return the maximum value of a list or, if it's empty, a default value
def MaxOrDefault<T, U>(lst: list<T>, default: U): any
  return lst->len() > 0 ? lst->max() : default
enddef
echo MaxOrDefault<number, number>([1, 2], 0)    # Echos 2
echo MaxOrDefault<number, number>([], 0)        # Echos 0
echo MaxOrDefault<number, string>([], 'empty')  # Echos empty

# ----------------------------------------------------------------------------
```

### Lambdas

```
vim9script
# LAMBDAS

# Syntax is `(args) => expr`
var Divide = (val: number, by: number): number => val / by
echo Divide(420, 10)  # Echos 42

# Sample list used in the following lambda examples
var nums = [1, 2, 3, 4, 5, 6]

# Using builtin function filter() to keep only the even numbers in nums list.
# This example uses the `_` argument, which ignores an argument.  It is useful
# in callbacks where the argument is not needed in the expression, but an
# argument must be provided to match the called function.  (Using the builtin
# function, `copy()`, avoids mutating the original list.)
var evens: list<number> = filter(copy(nums), (_, v) => v % 2 == 0)
echo evens            # Echos [2, 4, 6]

# Using builtin function map() to square each number in the nums list.
# This example also uses method chaining and the argument "_".
var squares: list<number> = nums->copy()->map((_, v) => v * v)
echo squares          # Echos [1, 4, 9, 16, 25, 36]

# Using builtin function `reduce()` to sum all the numbers in the nums
# list - it is called with two arguments:
#   1) the result so far (acc), and
#   2) the current item (v).
var sum: number = nums->copy()->reduce((acc, v) => acc + v, 0)
echo sum              # Echos 21

# Descending sort using a lambda
var sorted: list<number> = sort(copy(nums), (a, b) => b - a)
echo sorted           # Echos [6, 5, 4, 3, 2, 1]

# ----------------------------------------------------------------------------
```

### Closures

```
vim9script
# CLOSURES

def Outer(): func
  var x: number = 2      # Initial value captured by closure
  def Inner(): number
    x = x * 2            # Modifies the captured x
    return x             # Returns the new value
  enddef
  return Inner           # Returns the closure (function + captured x)
enddef

var Double = Outer()     # Creates a closure (with its own x = 2)

# Calling the closure
echo Double()            # Echos 4
echo Double()            # Echos 8
echo Double()            # Echos 16
echo Double()            # Echos 32

# ----------------------------------------------------------------------------
```

## 5. Classes and Enums

### A Class Example

```
vim9script
# A CLASS EXAMPLE

# Class
class Point
  var x: number
  var y: number

  def new(x: number, y: number)
    this.x = x
    this.y = y
  enddef

  def Move(dx: number, dy: number)
    this.x += dx
    this.y += dy
  enddef

  def ToString(): string
    return $"({this.x}, {this.y})"
  enddef
endclass

var p: Point = Point.new(1, 2)
p.Move(3, 4)
echo p.ToString()  # => (4, 6)

# ----------------------------------------------------------------------------
```

### An Enum Example

```
vim9script
# AN ENUM EXAMPLE

enum Quad
  Square('four', true),
  Rhombus('opposite', false)
  var es: string
  var ra: bool
  def QuadAbout(): string
    return $"A {tolower(this.name)} has {this.es} sides of equal length; it" ..
      (this.ra ? ' has only right angles' : ' has no right angles')
  enddef
endenum
var q: Quad = Quad.Square
echo q.QuadAbout()
q = Quad.Rhombus
echo q.QuadAbout()

# ----------------------------------------------------------------------------
```

## 6. Sourcing Scripts, Modules, and Importing

### Sourcing Another Script

```
vim9script
# SOURCING ANOTHER SCRIPT

# Source the optional helptoc plugin, distributed with Vim
source $VIMRUNTIME/pack/dist/opt/helptoc/plugin/helptoc.vim

# It’s better to use `packadd` for this, though
packadd helptoc

# Using `runtime` is another way (`*` or `**` wildcards can be used)
runtime pack/**/helptoc.vim

# ----------------------------------------------------------------------------
```

### Exporting and Importing

Vim9 script provides a modular, efficient import/export system.
Explicitly exported functions, constants, and variables declared
with `export` are accessible from other scripts which `import` them.
An example...

In file `MyModule.vim`:

```
vim9script
# EXPORT-IMPORT EXAMPLE: `MyModule.vim`

# Without `export`, this constant is visible only within MyModule.vim
const LOCAL: string = 'Not exported'

# This exported constant will be available from scripts importing MyModule.vim
export const GREETING: string = 'Hello from MyModule'

# ----------------------------------------------------------------------------
```

In another script file (presuming it is in the same directory):

```
vim9script
# EXPORT-IMPORT EXAMPLE (THE SCRIPT DOING THE IMPORT)

import "./MyModule.vim"

# Using the constant from MyModule.vim
echo MyModule.GREETING

# Trying to use MyModule.LOCAL fails because it's not exported
try
  echo MyModule.LOCAL
catch
  echo v:exception
endtry

# ----------------------------------------------------------------------------
```

## 7. Vim Idioms

### Plugin Development

```
vim9script
# PLUGIN DEVELOPMENT

# These are some of the patterns used when creating well-behaved plugins
# (which enable others to easily use your Vim9 scripts).

# Source guard (plugin pattern), which prevents duplicate loading of plugins
if exists('g:loaded_myplugin')
  finish
endif
# This would be put in the myplugin script (NB: intentionally commented here)
# g:loaded_myplugin = true

# Getting a global variable or, where it does not exist, a default value
var greeting: string = get(g:, 'myplugin_greeting', 'Hello')

# Toggling boolean settings is a common user interface pattern
def ToggleFeature()
  g:myplugin_enabled = !get(g:, 'myplugin_enabled', false)
  echo g:myplugin_enabled ? 'Enabled' : 'Disabled'
enddef
command! ToggleMyPlugin ToggleFeature()

# (After sourcing this script, try :ToggleMyPlugin a few times)

# ----------------------------------------------------------------------------
```

### Command and Mapping Definitions

```
vim9script
# COMMAND AND MAPPING DEFINITIONS

# Commands and mappings provide the primary user interface for plugins -
# see https://vimhelp.org/usr_05.txt.html#plugin - and custom functionality.

# Basic command definition
command! Hello echo 'Hello Vim9'
execute ':Hello'

# Script-local function mapping via <Plug> - recommended plugin mapping pattern
def DoSomething()
  echo 'Action triggered'
enddef

# Map function to unique <Plug> identifier, then to user keys
nnoremap <silent> <Plug>(MyPluginAction) <ScriptCmd>DoSomething()<CR>
nmap <silent> <Leader>a <Plug>(MyPluginAction)

# Command with arguments and completion
command! -nargs=1 -complete=file MyCmd edit <args>

# ----------------------------------------------------------------------------
```

### Event Handling

```
vim9script
# EVENT HANDLING

# Autocommand groups provide systematic event management and prevent
# duplicates on re-sourcing
augroup AutoReload
  autocmd!
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
  autocmd BufReadPost *.txt echo 'Hello text file'
augroup END

# ----------------------------------------------------------------------------
```

### Executing Normal Mode Commands

```
vim9script
# EXECUTING NORMAL MODE COMMANDS

# Yank eight lines into register a
normal! "a8Y

# Echo register a
echo @a

# (Sourcing this script as a visual selection echos the script itself)

# ----------------------------------------------------------------------------
```

### Feature Detection and Utility Functions

```
vim9script
# FEATURE DETECTION AND UTILITY FUNCTIONS

# Feature and existence checking
echo has('clipboard')           # Check for a feature - 1 means it is present
echo exists('myVariable')       # Check whether a script local variable exists

# Path and expression expansion
echo expand('%:p')                          # Full filepath of the current file

# Type checking with Vim `v:t_*` constants or more granularly with type names
echo type(123) == v:t_number                # true
echo typename([1, 2, 3]) == 'list<number>'  # true

# ----------------------------------------------------------------------------
```

## 8. External Commands

### Direct Shell Command Execution

```
vim9script
# DIRECT SHELL COMMAND EXECUTION

# This example should work in Linux, PowerShell, and Windows cmd.exe:

# output user info: (a) to non-interactive shell.
:!whoami

# Examples (b) and (c) would modify this buffer, so they're shown in
# a function, preventing execution.
def MoreExamples(): void
  # (b) to current line in the current buffer, replacing its contents
  :.!whoami
  # (c) to the same buffer, appending at cursor position
  :.read! whoami
enddef

# ----------------------------------------------------------------------------
```

### Capturing shell command output with `system()`

```
vim9script
# CAPTURING SHELL COMMAND OUTPUT WITH `system()`

# This script creates a 4-second popup of the date, removing any line feed
# characters (U+000A), which would otherwise appear as "^@" in the popup.

silent var result = system('date')
popup_notification(result->substitute('[\xA]', '', 'g'), {time: 4000})

# ----------------------------------------------------------------------------
```

### Processing Command Output as Lines with `systemlist()`

```
vim9script
# PROCESSING COMMAND OUTPUT AS LINES WITH `systemlist()`

silent var lines = systemlist('ls *.vim')

for line in lines
  echo line
endfor

# ----------------------------------------------------------------------------
```

### Asynchronous Command Execution with Jobs

```
vim9script
# ASYNCHRONOUS COMMAND EXECUTION WITH JOBS

# Jobs - see https://vimhelp.org/channel.txt.html#job%5Fstart%28%29 - execute
# commands asynchronously (unlike `system()` or `systemlist()`)

var mydate: list<string>

def Date(channel: channel, msg: string): void
    mydate->add(msg)
enddef

var myjob = job_start([&shell, &shellcmdflag, 'date'], {out_cb: Date})

echo [myjob, myjob->job_status()]
sleep 1
echo $"The date and time is {mydate->join('')}"
echo [myjob, myjob->job_status()]

# Check exit status of the most recent shell command
echo v:shell_error

# Clear the variable; release the job's resources
myjob = null_job

# ----------------------------------------------------------------------------
```

### A More Advanced Example: Git Integration

```
vim9script
# A MORE ADVANCED EXAMPLE: GIT INTEGRATION

# The following script populates Vim’s quickfix list with files modified in
# the current Git repository when `:GitDiffQF` is executed after sourcing
# the script

def GitDiffQuickfix()
  var diff_lines: list<string> = systemlist('git diff --name-only')
  if v:shell_error != 0
    echo 'Git not available or not a repo'
    return
  endif

  var qf_list: list<any>
  for file in diff_lines
    add(qf_list, {'filename': file, 'lnum': 1, 'text': 'Modified file'})
  endfor

  setqflist(qf_list, 'r')
  copen
enddef

command! GitDiffQF GitDiffQuickfix()

# ----------------------------------------------------------------------------
```

## 9. Testing, Debugging, and Inspecting Bytecode

### Assertions

```
vim9script
# ASSERTIONS

# The predefined Vim variable, `v:errors`, is populated when errors occur
# in builtin `assert_*` functions, like the three in this example

v:errors = []

assert_equal(4, 2 + 1)             # Expected 4 but got 3
assert_false(1 < 2)                # Expected False but got true
assert_notmatch('\d\+', '123')     # Pattern '\\d\\+' does match '123'

if !empty(v:errors)
  echo "Test failures:\n" .. $'{v:errors->join("\n")}'
endif

# ----------------------------------------------------------------------------
```

### Breakpoints

```
vim9script
# BREAKPOINTS

# Sourcing the following script runs the function in debug mode:
# - https://vimhelp.org/repeat.txt.html#%3Adebug
# While debugging, use commands like these (at the `>` command prompt):
# - "step" to execute the command and come back to debug mode for the
#    next command
# - "echo" to inspect variables
# - "cont" to continue execution until the next breakpoint is hit
# - "finish" to finish the current user function (or script)

breakdel *                  # clears all breakpoints

def MyFunc()
  var x = 10
  var y = x * 2
  echo y
enddef

breakadd func 3 MyFunc      # sets a breakpoint at line 3 of MyFunc

debug MyFunc()              # call MyFunc() with debugging on

# ----------------------------------------------------------------------------
```

### Listing Instructions / Bytecode of a Function

```
vim9script
# LISTING INSTRUCTIONS / BYTECODE OF A FUNCTION

# This can help understand how Vim optimizes a Vim9 script function,
# showing the instructions / bytecode generated by the function

def MyMultiply(a: number, b: number): number
  return a * b
enddef

# To compile and check for syntax/type errors:
# - Using `func MyMultiply` reveals the function definition
func MyMultiply

# - Using `disassemble` shows the instructions generated for MyMultiply
disassemble MyMultiply

# Example output:
# <SNR>757_MyMultiply
#   return a * b
#    0 LOAD arg[-2]
#    1 LOAD arg[-1]
#    2 OPNR *
#    3 RETURN

# ----------------------------------------------------------------------------
```

## Additional Resources

- [Vim9 script help](https://vimhelp.org/vim9.txt.html): An online
  version of Vim’s help pages (`:h vim9` within Vim itself), kept
  up-to-date automatically from the Vim source repository
- [Yegappan's Vim9 for Python Developers](https://github.com/yegappan/Vim9ScriptForPythonDevelopers):
  A guide focusing only on programming constructs that are common to both
  Python and Vim9 (and not constructs unique to Vim)
- [Lacygoill's Vim9 Notes](https://github.com/jessepav/lacygoill-wiki-backup/blob/master/vim/vim9.md)
- [vim9-conversion-aid](https://github.com/ubaldot/vim9-conversion-aid):
  a starting point to convert legacy Vim script to Vim9 script
- [awesome-vim9 plugins](https://github.com/saccarosium/awesome-vim9)
