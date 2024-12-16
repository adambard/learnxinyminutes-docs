---
name: Vim9script
filename: learn9vimscript.vim
contributors:
    - ["HiPhish", "http://hiphish.github.io/"]
---

```vim
# ##############
#  Introduction
# ##############

# Vim9 script is a modernized version of Vim's scripting language, offering features like variables, functions, and loops.
# Accoding to `:help vim9-differences`, the principal differences from legacy Vim script are as follows:
# 
# - Comments start with #, not ": >
# 	echo "hello"   # comment
# - Using a backslash for line continuation is hardly ever needed: >
# 	echo "hello "
# 	     .. yourName
# 	     .. ", how are you?"
# - White space is required in many places to improve readability.
# - Assign values without `:let` *E1126* , declare variables with `:var`: >
# 	var count = 0
# 	count += 3
# - Constants can be declared with `:final` and `:const`: >
# 	final matches = []		  # add to the list later
# 	const names = ['Betty', 'Peter']  # cannot be changed
# - `:final` cannot be used as an abbreviation of `:finally`.
# - Variables and functions are script-local by default.
# - Functions are declared with argument types and return type: >
# 	def CallMe(count: number, message: string): bool
# - Call functions without `:call`: >
# 	writefile(['done'], 'file.txt')
# - You cannot use old Ex commands:
# 	`:Print`
# 	`:append`
# 	`:change`
# 	`:d`  directly followed by 'd' or 'p'.
# 	`:insert`
# 	`:k`
# 	`:mode`
# 	`:open`
# 	`:s`  with only flags
# 	`:t`
# 	`:xit`
# - Some commands, especially those used for flow control, cannot be shortened.
#   E.g., `:throw` cannot be written as `:th`.  *vim9-no-shorten*
# - You cannot use curly-braces names.
# - A range before a command must be prefixed with a colon: >
# 	:%s/this/that
# - Executing a register with "@r" does not work, you can prepend a colon or use
#   `:exe`: >
# 	:exe @a
# - Unless mentioned specifically, the highest |scriptversion| is used.
# - When defining an expression mapping, the expression will be evaluated in the
#   context of the script where it was defined.
# - When indexing a string the index is counted in characters, not bytes:
#   |vim9-string-index|
# - Some possibly unexpected differences: |vim9-gotchas|.
#
# You can run Vim9 script commands in command-line mode or write them to a file and source it in Vim.
# This guide assumes familiarity with ex-commands and focuses on scripting.

# Comments start with #
# The vertical line '|' separates commands
echo 'Hello' | echo 'world!'

# Putting a comment after a command usually works
pwd  # Displays the current working directory

# Line continuation is rarely needed
echo "Hello "
     .. "world"

echo [1, 2]

echo {
    'a': 1,
    'b': 2
}

# #######
#  Types
# #######

# Numbers
echo 123         # Decimal
echo 0b1111011   # Binary
echo 0173        # Octal
echo 0x7B        # Hexadecimal
echo 123.0       # Floating-point
echo 1.23e2      # Floating-point (scientific notation)

# Booleans
echo true      # Evaluates to true
echo false     # Evaluates to false

# Boolean values from comparison
echo x == y      # Equality by value
echo x != y      # Inequality
echo x > y       # Greater than
echo x >= y      # Greater than or equal
echo x < y       # Smaller than
echo x <= y      # Smaller than or equal
echo x is y      # Instance identity
echo x isnot y   # Instance non-identity

# Strings
echo 'a' < 'B'   # True or false depending on 'ignorecase'
echo 'a' <? 'B'  # True
echo 'a' <# 'B'  # False

# Regular expression matching
echo "hi" =~ "hello"    # Regular expression match
echo "hi" =~# "hello"   # Case sensitive
echo "hi" =~? "hello"   # Case insensitive
echo "hi" !~ "hello"    # Regular expression unmatch
echo "hi" !~# "hello"   # Case sensitive
echo "hi" !~? "hello"   # Case insensitive

# Boolean operations
echo true && false  # Logical AND
echo true || false  # Logical OR
echo !true            # Logical NOT
echo true ? 'yes' : 'no'  # Ternary operator

# Strings
echo "Hello world\n"   # Newline
echo 'Hello world\n'   # Literal
echo 'Let''s go!'      # Two single quotes become one

# String concatenation
echo 'Hello ' .. 'world'  # String concatenation

# String indexing
echo 'Hello'[0]           # First character
echo 'Hello'[1]           # Second character
echo 'Hellö'[4]           # Returns a character

# Substrings
echo 'Hello'[:]           # Copy of entire string
echo 'Hello'[1:3]         # Substring
echo 'Hello'[1:-2]        # Substring until second to last character
echo 'Hello'[1:]          # Substring with starting index
echo 'Hello'[:2]          # Substring with ending index
echo 'Hello'[-2:]         # Substring relative to end

# Lists
echo []                   # Empty list
echo [1, 2, 'Hello']      # List with elements
echo [1, 2, 'Hello', ]    # Trailing comma permitted
echo [[1, 2], 'Hello']    # Nested lists

# List concatenation
echo [1, 2] + [3, 4]      # Creates a new list

# List indexing
echo [1, 2, 3, 4][2]      # Third element
echo [1, 2, 3, 4][-1]     # Last element

# List slicing
echo [1, 2, 3, 4][:]      # Shallow copy
echo [1, 2, 3, 4][:2]     # Sublist until third item
echo [1, 2, 3, 4][2:]     # Sublist from third item
echo [1, 2, 3, 4][:-2]    # Sublist until second-to-last item

# Dictionaries
echo {}                       # Empty dictionary
echo {'a': 1, 'b': 2}         # Dictionary literal
echo {'a': 1, 'b': 2, }       # Trailing comma permitted
echo {'x': {'a': 1, 'b': 2}}  # Nested dictionary

# Indexing a dictionary
echo {'a': 1, 'b': 2}['a']    # Literal index
echo {'a': 1, 'b': 2}.a       # Syntactic sugar

# Funcref
echo function('type')         # Reference to function type()
echo {x -> x * x}             # Anonymous function

# Regular expression
substitute/hello/Hello/

# ###########################
#  Implicit type conversions
# ###########################

echo "1" + 1         # Number
echo "1" .. 1        # String
echo "0xA" + 1       # Number

# ###########
#  Variables
# ###########

var b_my_var = 1        # Local to current buffer
var w_my_var = 1        # Local to current window
var t_my_var = 1        # Local to current tab page
var g_my_var = 1        # Global variable
var l_my_var = 1        # Local to current function
var s_my_var = 1        # Local to current script file
var a_my_arg = 1        # Function argument

# The Vim scope is read-only
echo true            # Special built-in Vim variables

# Access special Vim memory like variables
var @a = 'Hello'        # Register
var $PATH=''            # Environment variable
var &textwidth = 79     # Option
var &l_textwidth = 79   # Local option
var &g_textwidth = 79   # Global option

# Access scopes as dictionaries
echo b:                # All buffer variables
echo w:                # All window variables
echo t:                # All tab page variables
echo g:                # All global variables
echo l:                # All local variables
echo s:                # All script variables
echo a:                # All function arguments
echo v:                # All Vim variables

# Constant variables
const x = 10            # Constant

# Function reference variables
var IsString = {x -> type(x) == type('')}    # Global
var s_isNumber = {x -> type(x) == type(0)}   # Local

# Multiple value binding
var [x, y] = [1, 2]

# Assign the remainder to a rest variable
var [mother, father; children] = ['Alice', 'Bob', 'Carol', 'Dennis', 'Emily']

# ##############
#  Flow control
# ##############

# Conditional
var condition = true

if condition
    echo 'First condition'
elseif another_condition
    echo 'Second condition'
else
    echo 'Fail'
endif

# Loops

# For-loop
for person in ['Alice', 'Bob', 'Carol', 'Dennis', 'Emily']
    echo 'Hello ' .. person
endfor

# Iterate over a nested list
for [x, y] in [[1, 0], [0, 1], [-1, 0], [0, -1]]
    echo 'Position: x =' .. x .. ', y = ' .. y
endfor

# Iterate over a range of numbers
for i in range(10, 0, -1)
    echo 'T minus' .. i
endfor

# Iterate over the keys of a dictionary
for symbol in keys({'π': 3.14, 'e': 2.71})
    echo 'The constant ' .. symbol .. ' is a transcendent number'
endfor

# Iterate over the values of a dictionary
for value in values({'π': 3.14, 'e': 2.71})
    echo 'The value ' .. value .. ' approximates a transcendent number'
endfor

# Iterate over the keys and values of a dictionary
for [symbol, value] in items({'π': 3.14, 'e': 2.71})
    echo 'The number ' .. symbol .. ' is approximately ' .. value
endfor

# While-loops
var there_yet = true
while !there_yet
    echo 'Are we there yet?'
endwhile

# Exception handling
try
    source path/to/file
catch /Cannot open/
    echo 'Looks like that file does not exist'
catch /.*/
    echo 'Something went wrong, but I do not know what'
finally
    echo 'I am done trying'
endtry

# ##########
#  Functions
# ##########

# Defining functions
def AddNumbersLoudly(x: number, y: number): number
    echo 'Adding' .. x .. 'and' .. y
    return x + y
enddef

def s:addNumbersLoudly(x: number, y: number): number
    echo 'Adding' .. x .. 'and' .. y
    return x + y
enddef

# Range functions
def FirstAndLastLine() range
    echo [a:firstline, a:lastline]
enddef

# Aborting functions
def SourceMyFile() abort
    source my-file.vim
    echo 'This will never be printed'
enddef

# Closures
def MakeAdder(x: number)
    def Adder(n: number) closure
        return n + x
    enddef
    return funcref('Adder')
enddef
var AddFive = MakeAdder(5)
echo AddFive(3)  # Prints 8

# Dictionary functions
def Mylen() dict
    return len(self.data)
enddef
var mydict = {'data': [0, 1, 2, 3], 'len': function("Mylen")}
echo mydict.len()

# Alternatively, more concise
var mydict = {'data': [0, 1, 2, 3]}
def mydict.len()
    return len(self.data)
enddef

# Calling functions
var animals = keys({'cow': 'moo', 'dog': 'woof', 'cat': 'meow'})

# Call a function for its side effects only
call sign_undefine()

# The call() function
echo call(function('get'), [{'a': 1, 'b': 2}, 'c', 3])  # Prints 3

# Function namespaces
def foo#bar#log(value: string)
    echomsg value
enddef

call foo#bar#log('Hello')

# #############################
#  Frequently used ex-commands
# #############################

# Sourcing runtime files
runtime plugin/my-plugin.vim

# Defining new ex-commands
command! SwapAdjacentLines normal! ddp

command! -nargs=1 Error echoerr <args>

# Defining auto-commands
autocmd BufWritePost $MYVIMRC source $MYVIMRC

# Auto groups
augroup auto-source
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END

# Executing
var line = 3
execute line .. 'delete'

# Executing normal-mode commands
normal! ggddGp

# Window commands
wincmd L

# ###########################
#  Frequently used functions
# ###########################

# Feature check
echo has('nvim')
echo has('python3')
echo has('unix')
echo has('win32')

# Test if something exists
echo exists('&mouse')
echo exists('+mouse')
echo exists('$HOSTNAME')
echo exists('*strftime')
echo exists('**s:MyFunc')
echo exists('bufcount')
echo exists('my_dict["foo"]')
echo exists(':Make')
echo exists("#CursorHold")
echo exists("#BufReadPre#*.gz")
echo exists("#filetypeindent")
echo exists("##ColorScheme")

# Various dynamic values
echo expand('%')
echo expand('<cword>')
echo expand('%:p')

# Type tests
echo type(my_var) == v:t_number
echo type(my_var) == v:t_string
echo type(my_var) == v:t_func
echo type(my_var) == v:t_list
echo type(my_var) == v:t_dict
echo type(my_var) == v:t_float
echo type(my_var) == v:t_bool
echo my_var is v:null

# Format strings
echo printf('%d in hexadecimal is %X', 123, 123)

# #####################
#  Tricks of the trade
# #####################

# Source guard
if exists('g:loaded_my_plugin')
    finish
endif
var g_loaded_my_plugin = true

# Default values
var s_greeting = get(g:, 'my_plugin_greeting', 'Hello')
```
