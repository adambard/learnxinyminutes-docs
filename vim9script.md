---
name: Vim9Script
filename: learnvim9script.vim
contributors:
  - ["Alejandro Sanchez", "http://hiphish.github.io/"]
  - ["Yegappan Lakshmanan", "https://github.com/yegappan"]
  - ["LacyGoill", "https://github.com/lacygoill"]
---

Vim9Script is a modern scripting language introduced in Vim 9.0.
It improves performance, readability, and structure over legacy Vimscript. 
Try [vim9-conversion-aid](https://github.com/ubaldot/vim9-conversion-aid) as a starting point to convert legacy Vimscript to Vim9Script.

```vim
vim9script
# The vim9script namespace, above, is required to distinguish a Vim9 script
# *.vim file from a legacy vimscript file.  In Vim's command-line mode,
# or in a legacy script, using the command `:vim9cmd` (or just `:vim9`) before
# a command also evaluates and executes code as Vim9 script.
#
# There is no distinction between single and multi-line comments.
# Use # inside a line at any position to comment out all following characters.

##################################################################
## 1. Primitive types, collection types, operators, and regex
##################################################################

# The builtin function typename() may be used to reveal the type.
# Primitive data types are number (integer), float, and bool(ean)
echo typename(1)  # number
echo typename(1.1)  # float
echo typename(true)  # bool

# Collection data types are string, blob, list, tuple, and dict(ionary)
echo typename("Hello world")  # string
# Blob is a binary object
echo typename(0zFE0F)  # blob
echo typename([1, 2])  # list<number>
# echo typename((1, 2))  # tuple (Yet commented as it's a recent addition)
echo typename({1: 'one', 2: 'two'})  # dict<string>

# Arithmetic with the number (integer) type.
echo 1 + 1  # 2
echo 2 - 1  # 1
echo 3 * 2  # 6
echo 8 / 2  # 4
# If the result is not an integer, the remainder is not returned.
echo 9 / 2  # 4
# But modulo returns the remainder.
echo 9 % 2  # 1

# Similarly, for the float type.
echo 3.14 + 0.0015  # 3.1415
echo 3.0 * 2.0  # 6.0
# An integer and float will return a float.
echo 9 / 2.0  # 4.5

# Logical OR (||), AND (&&), and NOT (!).
echo true || false  # true
echo true && false  # false
echo !true  # false

# Equality (==) and inequality (!=) work on all three primitive types and
# comparisons (>, >=, <=, and <) on numbers and floats.
echo [1, 2] == [1, 2]  # true
echo 'apples' != 'pears'  # true
echo 9 > 8  # true
echo 8 >= 8  # true
echo 8 <= 9  # true
echo 8 < 9  # true

# Ternary operator.
echo 9 > 8 ? true : false  # true

# Falsy ("null coalescing operator").
echo 9 > 8 ?? 1 + 1  # true
echo 8 > 9 ?? 1 + 1  # 2

# Bitwise operators (>> and <<).
echo 1 << 2  # 4
echo 9 >> 1  # 5

# String concatenation.
echo "Hello" .. " " .. 'world'  # Hello world
# String indexing is at the character level (not byte level like vimscript)
echo 'fenêtre'[-4 : -1]  # être
# There are dozens of builtin string functions.  Examples.
echo reverse("moor")  # room
echo toupper("summer")  # SUMMER
echo str2list('yes')  # [121, 101, 115]
echo strcharlen('length')  # 6

# Type casting may be used to give an error for a type mismatch.
# (This example could use `try`, but that's covered later.)
echo <number>3  # 3
# echo <number>'3'  # This errors, but could be caught with try/catch

# Not-a-real-number values may be tested with isinf() and isnan() builtin
# functions.  These examples also illustrate method chaining.
echo 1.0 / 0.0 ->isinf()  # inf
echo -1.0 / 0.0 ->isinf()  # -inf
echo 0.0 / 0.0 ->isnan()  # nan

# The maximum number is either a 32 or 64 bit signed number, which may
# be checked using:
echo v:numbersize  # echos either 32 or 64
# The implication is that any arithmatic where the number exceeds the
# permitted value, for 64-bit 9,223,372,036,854,775,807, will fail:
echo 9223372036854775807 + 1  # -9223372036854775808

# Numbers may be expressed as decimal, hexadecimal (prefixed with 0x), 
# octal (0o or 0O), or binary (0b).  These are all decimal 15:
echo 15 + 0b1111 + 0xF + 0o17  # 60

# Pattern matching on strings.
echo 'foobar' =~ 'foo'  # true (matches pattern)
echo 'foobar' !~ 'baz'  # true (does not match pattern)

# Vim uses distinct regular expressions.  The basics are the same as many
# other languages.  The basics:
# `.` any character, `*` zero+ times, `\+` one+ times
# `\c` case-insensitive, `\C` case-sensitive
# `\d` digit, `\w` word char, `\s` whitespace, `^` start, `$` end, `\|` OR
# Character classes may also be used.  Examples.
# [:blank:] is the same as \s, [:digit:] is the same as \d
# Some things like the "very nomagic" and "very magic" are unique.
# `\v` very magic, most chars are special, closer to extended regexes
# `\V`: very nomagic, all but \ are literal
echo 'Foobar' =~ '\c^foo'  # true
echo "The year is 2025" =~ '[[:digit:]]\+$'  # true
echo 'abc123' =~ '\v\d+'  # true
echo 'a|b' =~ '\Va|b'  # true

# `\zs`: Sets the start of the match.
# `\ze`: Sets the end of the match.
# Match only the domain from an email:
var email = 'user@example.com'
echo matchstr(email, '@\zs[^ ]\+\ze')  # Output: 'example.com'


####################################################
## 2. Variables
####################################################

# `var` is used to declare a variable, which may have a type
var name: string = 'Vim'  # type declared
var count = 10  # type inferred (number)
# When the type is declared as a list or dict, its type(s) must be also, but
# may be "<any>".
var Alist: list<list<number>> = [[1, 2], [3, 4]]
var Adict: dict<any> = {a: 1, b: 'two'}

# Constants
# `const` may be used to make both the variable and values constant.
const PI: dict<float> = {2: 3.14, 4: 3.1415}  # Cannot add items to this dict
echo PI[4]  # 3.1415
# `final` may be used to make the variable a constant and the values mutable.
final pi: dict<float> = {2: 3.14, 4: 3.1415}
# Adding a key-value pair to pi.
pi[3] = 3.142
echo pi  # {2: 3.14, 3: 3.142, 4: 3.1415}
# Dictionary key-value pairs may also be accessed using dict.key
echo pi.4  # 3.1415

# There are many builtin functions for variable manipulation.  Some have been
# illustrated, above.  A selection of some more related to lists.
var MyList: list<number> = [0, 1, 2, 3]
echo MyList[0]  # 0
MyList->add(4)
MyList->remove(0)
echo join(MyList, ', ')  # 1, 2, 3, 4
# String interpolation with $.
echo $"The first and last items in MyList are {MyList[0]} and {MyList[-1]}"

# Variables exist within scopes.  When unprefixed, as the examples are, above,
# the scope is script-local (which differs from vimscript, which uses `s:`).
# Other scopes are global (prefixed with `g:`), window-local (`w:`),
# buffer-local (`b:`), and tab-local (`t:`).
# Vim help on scopes: https://vimhelp.org/eval.txt.html#variable-scope
# Use `g:` for global variables, `b:` for buffer-local, `w:` for window-local,
# and so on.  Vim also has many Vim-defined `v:` prefixed global variables.
g:global_var = 'I am global'
echo g:global_var  # I am global
echo v:version  # 901 (or whatever version you are running; 901 means 9.1)
echo b:current_syntax  # vim (if the buffer's filetype is vim with 'syntax' on)

# Registers are a form of variable used to store string values of many
# pre-defined or user-defined items.  When used in a Vim9 script, they are
# prefixed with `@`.
echo @:  # (echos the last command entered by the user)
echo @/  # (echos the last search performed by the user)
echo @1  # (echos the penultimate yanked or deleted text)
echo @%  # (echos the current file path)
@z = 'There are 26 named registers, a to z'
echo @z

# Other builtin variables.
echo $MYVIMRC  # (location of your .vimrc, or _vimrc using Windows, file)
echo $VIMRUNTIME  # (the Vim directory of the current Vim executable)
echo $PATH  # (references the environment variable, PATH)

# Vim has many settings, which are also variables.  They may be local or
# global scoped.
echo &textwidth # (echos the buffer‐local value of the textwidth option)
echo &wildmenu # (echos the boolean value of command-line wildcard expansion)


####################################################
## 3. Control Flow
####################################################

# If / Else
if count > 5
  echo 'big'
elseif count == 5
  echo 'medium'
else
  echo 'small'
endif

# For loop
for j in range(3)
  echo j
endfor

# While loop
var k = 0
while k < 3
  echo k
  k += 1
endwhile

# Loop control
for x in [1, 2, 3]
  if x == 2
    continue
  endif
  if x == 3
    break
  endif
  echo x
endfor

# Exceptions
try
  DoesNotExist()  # This fails
catch
  echo 'Function DoesNotExist() does not exist!'
endtry

try
  var lines = readfile('nofile.txt')
catch /E484:/
  echo 'File not found'
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


####################################################
## 4. Functions and Lambdas
####################################################

# Basic function
def Add(x: number, y: number): number
  return x + y
enddef

echo Add(3, 4)

# Default arguments
def Power(base: number, exp: number = 2): number
  return float2nr(pow(base, exp))
enddef

# Variable arguments
def Sum(...args: list<number>): number
  return reduce(args, (x, y) => x + y)
enddef

# Define a function that returns a closure capturing a local variable
def MakeAdder(x: number): func
  # Return a reference to the inner function
  return (y: number) => x + y
enddef

# Create a closure that adds 5
var Add5 = MakeAdder(5)

# Call the closure with 3; result is 8
echo Add5(3)

# Lambdas in Vim9Script use `(args) => expr` syntax:
var Divide = (val: number, by: number): number => val / by
echo Divide(420, 10)  # 42

# Sample list
var nums = [1, 2, 3, 4, 5, 6]

# map(): Square each number
var squares = map(copy(nums), (i, v) => v * v)
echo squares  # [1, 4, 9, 16, 25, 36]

# filter(): Keep even numbers
var evens = filter(copy(nums), (i, v) => v % 2 == 0)
echo evens  # [2, 4, 6]

# reduce(): Sum all numbers
var sum = reduce(copy(nums), (acc, v) => acc + v, 0)
echo sum  # 21

# sort(): Sort descending using lambda
# Use `copy()` to avoid mutating the original list.
var sorted = sort(copy(nums), (a, b) => b - a)
echo sorted  # [6, 5, 4, 3, 2, 1]

####################################################
## 5. Classes and enums
####################################################

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

var p = Point.new(1, 2)
p.Move(3, 4)
echo p.ToString()  # => (4, 6)

# Enum
enum Quad
  Square('four', true),
  Rectangle('opposite', true),
  Rhombus('opposite', false)
  var es: string
  var ra: bool
  def QuadProps(): list<any>
    return [this.es, this.ra]
  enddef
endenum
var q: Quad = Quad.Square
# result using string interpolation and showing multiple lines without `\`,
# which is required in vimscript but not in Vim9 script.
var result = $"A {tolower(q.name)} has {q.QuadProps()[0]} sides of equal " ..
  "length and " .. 
  (q.QuadProps()[1] ? 'has only right angles' : 'has no right angle')
echo result


####################################################
## 6. Modules and Imports
####################################################

# Sourcing another script file.
# In this case, sourcing the optional matchit plugin, distributed with Vim.
source $VIMRUNTIME/pack/dist/opt/matchit/plugin/matchit.vim
# That's not an ideal way though.  It's better to use `packadd`.
packadd helptoc
# Using `runtime` is another way (which can be used with wildcards)
runtime pack/**/**/**/comment.vim

# Importing functions from other files is achieved with `export` in the script
# file exporting the function/constant/variable and `import` in the
# script using the exported function/constant/variable.
# If this is in file, `MyFile.vim` in the same directory as the script file.
export const THEEND: string = 'The end.'
# The script importing THEEND would include (try used here to prevent error)
try
  import "MyFile.vim"
catch
  # Do something if the import fails
endtry
# And using the constant would be (try again used to prevent error)
try
  echo MyFile.THEEND
catch
  echo "THE END"
endtry

####################################################
## 7. Vim Idioms
####################################################

# Source guard (plugin pattern)
if exists('g:loaded_myplugin') | finish | endif
# Set to true to avoid sourcing the following code multiple times.
# g:loaded_myplugin = true

# Default value
var greeting = get(g:, 'myplugin_greeting', 'Hello')

# Command definition
command! Hello echo 'Hello Vim9'
# You can specify attributes like `-nargs`, `-range`, `-complete`;
# see https://vimhelp.org/usr_40.txt.html#40.2
command! -nargs=1 -complete=file MyCmd edit <args>
# Toggle a boolean setting
def ToggleFeature()
  g:myplugin_enabled = !get(g:, 'myplugin_enabled', false)
  echo g:myplugin_enabled ? 'Enabled' : 'Disabled'
enddef
command! ToggleMyPlugin ToggleFeature()

# Define a script-local function and map it via <Plug>
def DoSomething()
  echo 'Action triggered'
enddef

nnoremap <silent> <Plug>(MyPluginAction) <ScriptCmd>DoSomething()<CR>
nmap <silent> <Leader>a <Plug>(MyPluginAction)

# Group autocommands to manage them systematically
# to prevent duplicate autocommands on re-sourcing.
augroup AutoReload
  autocmd!
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
  autocmd BufReadPost *.txt echo 'Hello text file'
augroup END

# You can run normal commands from Vim9Script:
# This executes like pressing `ggddGp` in normal mode.
normal! ggddGp

# `exists({name})` checks if a variable, function, or command is defined.
echo exists(':myVariable') == 2
# `has({feature})` checks if Vim has a specific feature (e.g. `has('unix')`).
echo has('unix')
if has('nvim')
  echo 'Running in Neovim'
endif
# `expand({expr})` expands filenames, `<cword>`, etc.
echo expand('%:p')
# `printf({fmt}, ...)` formats strings.
echo printf('Hello, %s!', 'world')
# `type()`, along with `v:t_*` constants, indicates object types.
echo type(123) == v:t_number


####################################################
## 8. External Commands
####################################################

# Run a shell command and capture output
silent result = system('ls')
echo result

# Run and split into lines
silent var lines = systemlist('ls')
for line in lines
  echo line
endfor

# output files in folder of current file
# ... to non-interactive shell
!ls %:h:S
# ... to current buffer, replacing its contents
:%!ls %:h:S
# ... to the same buffer, appending at cursor position
:.read! ls %:h:S

# Using job_start() callback
var shell_job: job
def GotOutput(channel: channel, msg: string)
  echo msg
enddef
# Start a shell in the background.
shell_job = job_start(["/bin/sh", "-c", "ls"], {
				out_cb: GotOutput,
				err_cb: GotOutput
				})
# Check exit status
echo v:shell_error

# Create a quickfix list from Git diff
def GitDiffQuickfix()
  var diff_lines = systemlist('git diff --name-only')
  if v:shell_error != 0
    echo 'Git not available or not a repo'
    return
  endif

  var qf_list = []
  for file in diff_lines
    add(qf_list, {'filename': file, 'lnum': 1, 'text': 'Modified file'})
  endfor

  call setqflist(qf_list, 'r')
  copen
enddef
command! GitDiffQF call GitDiffQuickfix()

####################################################
## 9. Debugging, Compiling and Inspecting Bytecode
####################################################

def MyFunc()
  var x = 10
  var y = x * 2
  echo y
enddef

# To debug this function:
# Add a breakpoint at line 3 of the function (line numbers are relative to the function)
# :breakadd func MyFunc 3

# Then run the function in debug mode
# :debug call MyFunc()

# While debugging, use commands like:
# - :step to step into
# - :next to step over
# - :finish to run to end
# - :cont to continue
# - :echo to inspect variables

## Listing Bytecode of a Function,
## Useful to understand how Vim optimizes Vim9Script functions

def MyMultiply(a: number, b: number): number
  return a * b
enddef

# To compile and check for syntax/type errors:
# Use :func MyMultiply to view the function definition

# To inspect the compiled bytecode:
disassemble MyMultiply

# Example output:
# <SNR>757_MyMultiply
#   return a * b
#    0 LOAD arg[-2]
#    1 LOAD arg[-1]
#    2 OPNR *
#    3 RETURN
```

### Additional Resources

- [Vim9 Script Reference](https://vimhelp.org/vim9.txt.html)
- [Yegappan's Vim9 for Python Developers](https://github.com/yegappan/Vim9ScriptForPythonDevelopers)
- [Lacygoill's Vim9 Notes](https://github.com/jessepav/lacygoill-wiki-backup/blob/master/vim/vim9.md)
