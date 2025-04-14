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
# Above line is necessary to distinguish code in a *.vim file from legacy vimscript.

####################################################
## 1. Primitive Datatypes and Operators
####################################################

# Numbers
var i: number = 42
var f: float = 3.14

# Booleans
var done: bool = true

# Strings
var s: string = 'hello'

# Arithmetic
var sum = 1 + 2
var div = 10 / 3
var mod = 10 % 3
var pow = float2nr(pow(2, 3))

# Numeric comparisons
echo 1 == 1      # true
echo 2 != 3      # true
echo 2 > 1       # true
echo 2 >= 2      # true

# String equality
echo 'foo' == 'foo'     # true
echo 'foo' != 'bar'     # true

# Pattern matching
echo 'foobar' =~ 'foo'  # true (matches pattern)
echo 'foobar' !~ 'baz'  # true (does not match pattern)

# Regex Basics:
# - `.` any char, `*` zero+ times, `\+` one+ times  
# - `\d` digit, `\w` word char, `^` start, `$` end, `\|` OR
# Case Sensitivity & Magic Modes
# - `\c` / `\C`: case-insensitive / case-sensitive  
# - `\v`: very magic, most chars are special, closer to extended regexes
# - `\V`: very nomagic, all but \ are literal
echo 'Foobar' =~ '\cfoo'   # true
echo 'abc123' =~ '\v\d+'   # true
echo 'a|b' =~ '\Va|b'      # true

# Logical
echo true && false  # false
echo true || false  # true
echo !true          # false

# Ternary
echo true ? 'yes' : 'no'

####################################################
## 2. Variables and Collections
####################################################

# Variable declaration
var name: string = 'Vim'
var count = 10  # type inferred

# Constants
const pi = 3.1415

# Lists
var l: list<number> = [1, 2, 3]
echo l[0]
l->add(4)
l->remove(1)

# Tuples
var t: tuple<number, string, number> = (1, 'a', 3)
echo t[1]

# Vim help on scopes: https://vimhelp.org/eval.txt.html#variable-scope
# Use `g:` for global variables, `b:` for buffer-local, `w:` for window-local, and so on.  
# - @a accesses the contents of register "a"
# - $PATH references the environment variable PATH
# - &l:textwidth gets the buffer‐local value of the textwidth option
g:global_var = 'I am global'

# Dictionaries
var d: dict<number> = {a: 1, b: 2}
echo d.a
d.c = 3

# Sets (via dict keys)
var set = {1: true, 2: true}

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

####################################################
## 4. Functions
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

####################################################
## 5. Classes
####################################################

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

####################################################
## 6. Modules and Imports
####################################################

# Source another script file
source mylib.vim

# Runtime path
runtime plugin/myplugin.vim

# Vim loads `.vimrc`, then plugin files in `plugin/` directories.  
# Vim9Script can coexist with older scripts.  
# Place Vim9 files in separate `.vim` files with `vim9script` at the top.  
# Use `autoload/` or `ftplugin/` for specialized features.

# Define script-local functions
def Helper()
  echo 'internal'
enddef

####################################################
## 7. File I/O
####################################################

# Write
writefile(['line1', 'line2'], 'file.txt')

# Append
writefile(['line3'], 'file.txt', 'a')

# Read
var lines = readfile('file.txt')
echo lines

####################################################
## 8. Exceptions
####################################################

try
  var lines = readfile('nofile.txt')
catch /E484:/
  echo 'File not found'
finally
  echo 'Done'
endtry

# Throw
throw 'MyError'

####################################################
## 9. Advanced Features
####################################################

# Lambda
var Square = (x) => x * x
echo Square(4)

# Partial
def Log(level: string, msg: string)
  echo $"[{level}] {msg}"
enddef

var Warn = function('Log', ['WARN'])
Warn('Disk low')

# Decorator-like
def LogWrap(F: func): func
  def wrapper(...args: list<any>): any
    echo 'Calling'
    var result = call(F, args)
    echo 'Done'
    return result
  enddef
  return funcref('wrapper')
enddef

####################################################
## 10. Testing
####################################################

v:errors = []

assert_equal(4, 2 + 2)
assert_notequal(1, 2)
assert_true(1 < 2)
assert_false(2 < 1)
assert_match('\d\+', 'abc123')
assert_notmatch('\d\+', 'abc')

if len(v:errors) == 0
  echo 'All tests passed'
else
  echo 'Test failures:'
  echo v:errors
endif

####################################################
## 11. External Commands
####################################################

# Run a shell command and capture output
var result = system('ls')
echo result

# Run and split into lines
silent var lines = systemlist('ls')
for line in lines
  echo line
endfor

# Using :!ls
:!ls

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

####################################################
## 12. JSON and Regex
####################################################

# JSON encode/decode
var data = {'name': 'Vim', 'version': 9}
var json = json_encode(data)
echo json

var parsed = json_decode(json)
echo parsed.name

# Regex match
var s = 'abc123'
if s =~ '\d\+'
  echo 'Contains digits'
endif

# Replace
var new = substitute('foo bar', 'bar', 'baz', '')
echo new

####################################################
## 13. Vim Idioms
####################################################

# Source guard (plugin pattern)
if exists('g:loaded_myplugin')
  finish
endif
var g:loaded_myplugin = true

# Default value
var greeting = get(g:, 'myplugin_greeting', 'Hello')

# Command definition
command! Hello echo 'Hello Vim9'
# You can specify attributes like `-nargs`, `-range`, `-complete`;
# see https://vimhelp.org/usr_40.txt.html#40.2
command! -nargs=1 -complete=file MyCmd edit <args>

# Group autocommands to manage them systematically
# to prevent duplicate autocommands on re-sourcing.
augroup AutoReload
  autocmd!
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
  autocmd BufReadPost *.txt echo 'Hello text file'
augroup END

# Define a script-local function and map it via <Plug>
def DoSomething()
  echo 'Action triggered'
enddef

nnoremap <silent> <Plug>(MyPluginAction) <ScriptCmd>DoSomething()<CR>
nmap <silent> <Leader>a <Plug>(MyPluginAction)

# You can run normal commands from Vim9Script:
# This executes like pressing `ggddGp` in normal mode.
normal! ggddGp

# `exist({name})` checks if a variable, function, or command is defined.
echo exist(':myVariable') == 2
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

if v:version >= 900
  echo 'Vim 9+ detected'
endif

# Toggle a boolean setting
def ToggleFeature()
  g:myplugin_enabled = !get(g:, 'myplugin_enabled', false)
  echo g:myplugin_enabled ? 'Enabled' : 'Disabled'
enddef
command! ToggleMyPlugin ToggleFeature()

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
```

### Additional Resources

- [Vim9 Script Reference](https://vimhelp.org/vim9.txt.html)
- [Yegappan's Vim9 for Python Developers](https://github.com/yegappan/Vim9ScriptForPythonDevelopers)
- [Lacygoill's Vim9 Notes](https://github.com/jessepav/lacygoill-wiki-backup/blob/master/vim/vim9.md)
