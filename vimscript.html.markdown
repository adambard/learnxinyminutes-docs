---
language: Vimscript
filename: learnvimscript.vim
contributors:
    - ["HiPhish", "http://hiphish.github.io/"]
---

```vim
" ##############
"  Introduction
" ##############
"
" Vim script (also called VimL) is the subset of Vim's ex-commands which
" supplies a number of features one would expect from a scripting language,
" such as values, variables, functions or loops. Always keep in the back of
" your mind that a Vim script file is just a sequence of ex-commands. It is
" very common for a script to mix programming-language features and raw
" ex-commands.
"
" You can run Vim script directly by entering the commands in command-line mode
" (press `:` to enter command-line mode), or you can write them to a file
" (without the leading `:`) and source it in a running Vim instance (`:source
" path/to/file`). Some files are sourced automatically as part of your
" configuration (see |startup|). This guide assumes that you are familiar
" with ex-commands and will only cover the scripting. Help topics to the
" relevant manual sections are included.
"
" See |usr_41.txt| for the official introduction to Vim script. A comment is
" anything following an unmatched `"` until the end of the line, and `|`
" separates instructions (what `;` does in most other languages). References to
" the manual as surrounded with `|`, such as |help.txt|.

" This is a comment

" The vertical line '|' (pipe) separates commands
echo 'Hello' | echo 'world!'

" Putting a comment after a command usually works
pwd                   " Displays the current working directory

" Except for some commands it does not; use the command delimiter before the
" comment (echo assumes that the quotation mark begins a string)
echo 'Hello world!'  | " Displays a message

" Line breaks can be escaped by placing a backslash as the first non-whitespace
" character on the *following* line. Only works in script files, not on the
" command line
echo " Hello
    \ world "

echo [1, 
    \ 2]

echo {
    \ 'a': 1,
    \ 'b': 2
\}


" #######
"  Types
" #######
"
" For an overview of types see |E712|. For an overview of operators see
" |expression-syntax|

" Numbers (|expr-number|)
" #######

echo  123         | " Decimal
echo  0b1111011   | " Binary
echo  0173        | " Octal
echo  0x7B        | " Hexadecimal
echo  123.0       | " Floating-point
echo  1.23e2      | " Floating-point (scientific notation)

" Note that an *integer* number with a leading `0` is in octal notation. The
" usual arithmetic operations are supported.

echo  1 + 2       | " Addition
echo  1 - 2       | " Subtraction
echo  - 1         | " Negation (unary minus)
echo  + 1         | " Unary plus (does nothing really, but still legal)
echo  1 * 2       | " Multiplication
echo  1 / 2       | " Division
echo  1 % 2       | " Modulo (remainder)

" Booleans (|Boolean|)
" ########
"
" The number 0 is false, every other number is true. Strings are implicitly
" converted to numbers (see below). There are two pre-defined semantic
" constants.

echo  v:true      | " Evaluates to 1 or the string 'v:true'
echo  v:false     | " Evaluates to 0 or the string 'v:false'

" Boolean values can result from comparison of two objects.

echo  x == y             | " Equality by value
echo  x != y             | " Inequality
echo  x >  y             | " Greater than
echo  x >= y             | " Greater than or equal
echo  x <  y             | " Smaller than
echo  x <= y             | " Smaller than or equal
echo  x is y             | " Instance identity (lists and dictionaries)
echo  x isnot y          | " Instance non-identity (lists and dictionaries)

" Strings are compared based on their alphanumerical ordering
" echo 'a' < 'b'. Case sensitivity depends on the setting of 'ignorecase'
"
" Explicit case-sensitivity is specified by appending '#' (match case) or '?'
" (ignore case) to the operator. Prefer explicitly case sensitivity when writing
" portable scripts.

echo  'a' <  'B'         | " True or false depending on 'ignorecase'
echo  'a' <? 'B'         | " True
echo  'a' <# 'B'         | " False

" Regular expression matching
echo  "hi" =~  "hello"    | " Regular expression match, uses 'ignorecase'
echo  "hi" =~# "hello"    | " Regular expression match, case sensitive
echo  "hi" =~? "hello"    | " Regular expression match, case insensitive
echo  "hi" !~  "hello"    | " Regular expression unmatch, use 'ignorecase'
echo  "hi" !~# "hello"    | " Regular expression unmatch, case sensitive
echo  "hi" !~? "hello"    | " Regular expression unmatch, case insensitive

" Boolean operations are possible.

echo  v:true && v:false       | " Logical AND
echo  v:true || v:false       | " Logical OR
echo  ! v:true                | " Logical NOT
echo  v:true ? 'yes' : 'no'   | " Ternary operator


" Strings (|String|)
" #######
"
" An ordered zero-indexed sequence of bytes. The encoding of text into bytes
" depends on the option |'encoding'|.

" Literal constructors
echo  "Hello world\n"   | " The last two characters stand for newline
echo  'Hello world\n'   | " The last two characters are literal
echo  'Let''s go!'      | " Two single quotes become one quote character

" Single-quote strings take all characters are literal, except two single
" quotes, which are taken to be a single quote in the string itself. See 
" |expr-quote| for all possible escape sequences.

" String concatenation
" The .. operator is preferred, but only supported in since Vim 8.1.1114
echo  'Hello ' .  'world'  | " String concatenation
echo  'Hello ' .. 'world'  | " String concatenation (new variant)

" String indexing
echo  'Hello'[0]           | " First byte
echo  'Hello'[1]           | " Second byte
echo  'Hellö'[4]           | " Returns a byte, not the character 'ö'

" Substrings (second index is inclusive)
echo  'Hello'[:]           | " Copy of entire string
echo  'Hello'[1:3]         | " Substring, second to fourth byte
echo  'Hello'[1:-2]        | " Substring until second to last byte
echo  'Hello'[1:]          | " Substring with starting index
echo  'Hello'[:2]          | " Substring with ending index
echo  'Hello'[-2:]         | " Substring relative to end of string

" A negative index is relative to the end of the string. See
" |string-functions| for all string-related functions.

" Lists (|List|)
" #####
"
" An ordered zero-indexed heterogeneous sequence of arbitrary Vim script
" objects.

" Literal constructor
echo  []                   | " Empty list
echo  [1, 2, 'Hello']      | " List with elements
echo  [1, 2, 'Hello', ]    | " Trailing comma permitted
echo  [[1, 2], 'Hello']    | " Lists can be nested arbitrarily

" List concatenation
echo  [1, 2] + [3, 4]      | " Creates a new list

" List indexing, negative is relative to end of list (|list-index|)
echo  [1, 2, 3, 4][2]      | " Third element
echo  [1, 2, 3, 4][-1]     | " Last element

" List slicing (|sublist|)
echo  [1, 2, 3, 4][:]      | " Shallow copy of entire list
echo  [1, 2, 3, 4][:2]     | " Sublist until third item (inclusive)
echo  [1, 2, 3, 4][2:]     | " Sublist from third item (inclusive)
echo  [1, 2, 3, 4][:-2]    | " Sublist until second-to-last item (inclusive)

" All slicing operations create new lists. To modify a list in-place use list
" functions (|list-functions|) or assign directly to an item (see below about
" variables).


" Dictionaries (|Dictionary|)
" ############
"
" An unordered sequence of key-value pairs, keys are always strings (numbers
" are implicitly converted to strings).

" Dictionary literal
echo  {}                       | " Empty dictionary
echo  {'a': 1, 'b': 2}         | " Dictionary literal
echo  {'a': 1, 'b': 2, }       | " Trailing comma permitted
echo  {'x': {'a': 1, 'b': 2}}  | " Nested dictionary

" Indexing a dictionary
echo  {'a': 1, 'b': 2}['a']    | " Literal index
echo  {'a': 1, 'b': 2}.a       | " Syntactic sugar for simple keys

" See |dict-functions| for dictionary manipulation functions.


" Funcref (|Funcref|)
" #######
"
" Reference to a function, uses the function name as a string for construction.
" When stored in a variable the name of the variable has the same restrictions
" as a function name (see below).

echo  function('type')                   | " Reference to function type()
" Note that `funcref('type')` will throw an error because the argument must be
" a user-defined function; see further below for defining your own functions.
echo  funcref('type')                    | " Reference by identity, not name
" A lambda (|lambda|) is an anonymous function; it can only contain one
" expression in its body, which is also its implicit return value.
echo  {x -> x * x}                       | " Anonymous function
echo  function('substitute', ['hello'])  | " Partial function


" Regular expression (|regular-expression|)
" ##################
"
" A regular expression pattern is generally a string, but in some cases you can
" also use a regular expression between a pair of delimiters (usually `/`, but
" you can choose anything).

" Substitute 'hello' for 'Hello'
substitute/hello/Hello/


" ###########################
"  Implicit type conversions
" ###########################
"
" Strings are converted to numbers, and numbers to strings when necessary. A
" number becomes its decimal notation as a string. A string becomes its
" numerical value if it can be parsed to a number, otherwise it becomes zero.

echo  "1" + 1         | " Number
echo  "1" .. 1        | " String
echo  "0xA" + 1       | " Number

" Strings are treated like numbers when used as booleans
echo "true" ? 1 : 0   | " This string is parsed to 0, which is false

" ###########
"  Variables
" ###########
" 
" Variables are bound within a scope; if no scope is provided a default is
" chosen by Vim. Use `:let` and `:const` to bind a value and `:unlet` to unbind
" it.

let b:my_var = 1        | " Local to current buffer
let w:my_var = 1        | " Local to current window
let t:my_var = 1        | " Local to current tab page
let g:my_var = 1        | " Global variable
let l:my_var = 1        | " Local to current function (see functions below)
let s:my_var = 1        | " Local to current script file
let a:my_arg = 1        | " Function argument (see functions below)

" The Vim scope is read-only
echo  v:true            | " Special built-in Vim variables (|v:var|)

" Access special Vim memory like variables
let @a = 'Hello'        | " Register
let $PATH=''            | " Environment variable
let &textwidth = 79     | " Option
let &l:textwidth = 79   | " Local option
let &g:textwidth = 79   | " Global option

" Access scopes as dictionaries (can be modified like all dictionaries)
" See the |dict-functions|, especially |get()|, for access and manipulation
echo  b:                | " All buffer variables
echo  w:                | " All window variables
echo  t:                | " All tab page variables
echo  g:                | " All global variables
echo  l:                | " All local variables
echo  s:                | " All script variables
echo  a:                | " All function arguments
echo  v:                | " All Vim variables

" Constant variables
const x = 10            | " See |:const|, |:lockvar|

" Function reference variables have the same restrictions as function names
let IsString = {x -> type(x) == type('')}    | " Global: capital letter
let s:isNumber = {x -> type(x) == type(0)}   | " Local: any name allowed

" When omitted the scope `g:` is implied, except in functions, there `l:` is
" implied.


" Multiple value binding (list unpacking)
" #######################################
"
" Assign values of list to multiple variables (number of items must match)
let [x, y] = [1, 2]

" Assign the remainder to a rest variable (note the semicolon)
let [mother, father; children] = ['Alice', 'Bob', 'Carol', 'Dennis', 'Emily']


" ##############
"  Flow control
" ##############

" Conditional (|:if|, |:elseif|, |:else|, |:endif|)
" ###########
" 
" Conditions are set between `if` and `endif`. They can be nested.

let condition = v:true

if condition
    echo 'First condition'
elseif another_condition
    echo 'Second condition'
else
    echo 'Fail'
endif

" Loops (|:for|, |:endfor|, |:while|, |:endwhile|, |:break|, |:continue|)
" #####
"
" Two types of loops: `:for` and `:while`. Use `:continue` to skip to the next
" iteration, `:break` to break out of the loop.

" For-loop (|:for|, |:endfor|)
" ========
"
" For-loops iterate over lists and nothing else. If you want to iterate over
" another sequence you need to use a function which will create a list.

" Iterate over a list
for person in ['Alice', 'Bob', 'Carol', 'Dennis', 'Emily']
    echo 'Hello ' .. person
endfor

" Iterate over a nested list by unpacking it
for [x, y] in [[1, 0], [0, 1], [-1, 0], [0, -1]]
    echo 'Position: x ='  .. x .. ', y = ' .. y
endfor

" Iterate over a range of numbers
for i in range(10, 0, -1)  " Count down from 10
    echo 'T minus'  .. i
endfor

" Iterate over the keys of a dictionary
for symbol in keys({'π': 3.14, 'e': 2.71})
    echo 'The constant ' .. symbol .. ' is a transcendent number'
endfor

" Iterate over the values of a dictionary
for value in values({'π': 3.14, 'e': 2.71})
    echo 'The value ' .. value .. ' approximates a transcendent number'
endfor

" Iterate over the keys and values of a dictionary
for [symbol, value] in items({'π': 3.14, 'e': 2.71})
    echo 'The number ' .. symbol .. ' is approximately ' .. value
endfor

" While-loops (|:while|, |:endwhile|)

let there_yet = v:true
while !there_yet
    echo 'Are we there yet?'
endwhile


" Exception handling (|exception-handling|)
" ##################
"
" Throw new exceptions as strings, catch them by pattern-matching a regular
" expression against the string

" Throw new exception
throw "Wrong arguments"

" Guard against an exception (the second catch matches any exception)
try
    source path/to/file
catch /Cannot open/
    echo 'Looks like that file does not exist'
catch /.*/
    echo 'Something went wrong, but I do not know what'
finally
    echo 'I am done trying'
endtry


" ##########
"  Functions
" ##########

" Defining functions (|:function|, |:endfunction|)
" ##################

" Unscoped function names have to start with a capital letter
function! AddNumbersLoudly(x, y)
    " Use a: scope to access arguments
    echo 'Adding'  .. a:x ..  'and'  .. a:y   | " A side effect
    return a:x + a:y                          | " A return value
endfunction

" Scoped function names may start with a lower-case letter
function! s:addNumbersLoudly(x, y)
    echo 'Adding'  .. a:x ..  'and'  .. a:y
    return a:x + a:y
endfunction

" Without the exclamation mark it would be an error to re-define a function,
" with the exclamation mark the new definition can replace the old one. Since
" Vim script files can be reloaded several times over the course of a session
" it is best to use the exclamation mark unless you really know what you are
" doing.

" Function definitions can have special qualifiers following the argument list.

" Range functions define two implicit arguments, which will be set to the range
" of the ex-command
function! FirstAndLastLine() range
    echo [a:firstline, a:lastline]
endfunction

" Prints the first and last line that match a pattern (|cmdline-ranges|)
/^#!/,/!#$/call FirstAndLastLine()

" Aborting functions, abort once error occurs (|:func-abort|)
function! SourceMyFile() abort
    source my-file.vim        | " Try sourcing non-existing file
    echo 'This will never be printed'
endfunction

" Closures, functions carrying values from outer scope (|:func-closure|)
function! MakeAdder(x)
    function! Adder(n) closure
        return a:n + a:x
    endfunction
    return funcref('Adder')
endfunction
let AddFive = MakeAdder(5)
echo AddFive(3)               | " Prints 8

" Dictionary functions, poor man's OOP methods (|Dictionary-function|)
function! Mylen() dict
    return len(self.data)     | " Implicit variable self
endfunction
let mydict = {'data': [0, 1, 2, 3], 'len': function("Mylen")}
echo mydict.len()

" Alternatively, more concise
let mydict = {'data': [0, 1, 2, 3]}
function! mydict.len()
    return len(self.data)
endfunction

" Calling functions (|:call|)
" #################

" Call a function for its return value, and possibly for its side effects
let animals = keys({'cow': 'moo', 'dog': 'woof', 'cat': 'meow'})

" Call a function for its side effects only, ignore potential return value
call sign_undefine()

" The call() function calls a function reference and passes parameters as a
" list, and returns the function's result.
echo  call(function('get'), [{'a': 1, 'b': 2}, 'c', 3])   | " Prints 3

" Recall that Vim script is embedded within the ex-commands, that is why we
" cannot just call a function directly, we have to use the `:call` ex-command.

" Function namespaces (|write-library-script|, |autoload|)
" ###################

" Must be defined in autoload/foo/bar.vim
" Namspaced function names do not have to start with a capital letter
function! foo#bar#log(value)
    echomsg value
endfunction

call foo#bar#log('Hello')


" #############################
"  Frequently used ex-commands
" #############################


" Sourcing runtime files (|'runtimepath'|)
" ######################

" Source first match among runtime paths
runtime plugin/my-plugin.vim


" Defining new ex-commands (|40.2|, |:command|)
" ########################

" First argument here is the name of the command, rest is the command body
command! SwapAdjacentLines normal! ddp

" The exclamation mark works the same as with `:function`. User-defined
" commands must start with a capital letter. The `:command` command can take a
" number of attributes (some of which have their own parameters with `=`), such
" as `-nargs`, all of them start with a dash to set them apart from the command
" name.

command! -nargs=1 Error echoerr <args>


" Defining auto-commands (|40.3|, |autocmd|, |autocommand-events|)
" ######################

" The arguments are "events", "patterns", rest is "commands"
autocmd BufWritePost $MYVIMRC source $MYVIMRC

" Events and patterns are separated by commas with no space between. See
" |autocmd-events| for standard events, |User| for custom events. Everything
" else are the ex-commands which will be executed.

" Auto groups
" ===========
"
" When a file is sourced multiple times the auto-commands are defined anew,
" without deleting the old ones, causing auto-commands to pile up over time.
" Use auto-groups and the following ritual to guard against this.

augroup auto-source   | " The name of the group is arbitrary
    autocmd!          | " Deletes all auto-commands in the current group
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END           | " Switch back to default auto-group

" It is also possible to assign a group directly. This is useful if the
" definition of the group is in one script and the definition of the
" auto-command is in another script.

" In one file
augroup auto-source
    autocmd!
augroup END

" In another file
autocmd auto-source BufWritePost $MYVIMRC source $MYVIMRC

" Executing (run-time macros of sorts)
" ####################################

" Sometimes we need to construct an ex-command where part of the command is not
" known until runtime.

let line = 3                | " Line number determined at runtime
execute line .. 'delete'    | " Delete a line

" Executing normal-mode commands
" ##############################
"
" Use `:normal` to play back a sequence of normal mode commands from the
" command-line. Add an exclamation mark to ignore user mappings.

normal! ggddGp             | " Transplant first line to end of buffer

" Window commands can be used with :normal, or with :wincmd if :normal would
" not work
wincmd L                   | " Move current window all the way to the right


" ###########################
"  Frequently used functions
" ###########################

" Feature check
echo  has('nvim')                  | " Running Neovim
echo  has('python3')               | " Support for Python 3 plugins
echo  has('unix')                  | " Running on a Unix system
echo  has('win32')                 | " Running on a Windows system


" Test if something exists
echo  exists('&mouse')             | " Option (exists only)
echo  exists('+mouse')             | " Option (exists and works)
echo  exists('$HOSTNAME')          | " Environment variable
echo  exists('*strftime')          | " Built-in function
echo  exists('**s:MyFunc')         | " User-defined function
echo  exists('bufcount')           | " Variable (scope optional)
echo  exists('my_dict["foo"]')     | " Variable (dictionary entry)
echo  exists('my_dict["foo"]')     | " Variable (dictionary entry)
echo  exists(':Make')              | " Command
echo  exists("#CursorHold")        | " Auto-command defined for event
echo  exists("#BufReadPre#*.gz")   | " Event and pattern
echo  exists("#filetypeindent")    | " Auto-command group
echo  exists("##ColorScheme")      | " Auto-command supported for event

" Various dynamic values (see |expand()|)
echo  expand('%')                  | " Current file name
echo  expand('<cword>')            | " Current word under cursor
echo  expand('%:p')                | " Modifier are possible

" Type tests
" There are unique constants defined for the following types. Older versions
" of Vim lack the type variables, see the reference " documentation for a
" workaround
echo  type(my_var) == v:t_number      | " Number
echo  type(my_var) == v:t_string      | " String
echo  type(my_var) == v:t_func        | " Funcref
echo  type(my_var) == v:t_list        | " List
echo  type(my_var) == v:t_dict        | " Dictionary
echo  type(my_var) == v:t_float       | " Float
echo  type(my_var) == v:t_bool        | " Explicit Boolean
" For the null object should compare it against itself
echo  my_var is v:null

" Format strings
echo  printf('%d in hexadecimal is %X', 123, 123)


" #####################
"  Tricks of the trade
" #####################

" Source guard
" ############

" Prevent a file from being sourced multiple times; users can set the variable
" in their configuration to prevent the plugin from loading at all.
if exists('g:loaded_my_plugin')
    finish
endif
let g:loaded_my_plugin = v:true

" Default values
" ##############

" Get a default value: if the user defines a variable use it, otherwise use a
" hard-coded default. Uses the fact that a scope is also a dictionary.
let s:greeting = get(g:, 'my_plugin_greeting', 'Hello')
```
