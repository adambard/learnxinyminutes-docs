---
category: tool
tool: tcsh
filename: LearnTCSH.csh
contributors:
       - ["Nicholas Christopoulos", "https://github.com/nereusx"]

---
tcsh ("tee-see-shell") is a Unix shell based on and compatible with the C shell (csh).
It is essentially the C shell with programmable command-line completion, command-line editing,
and a few other features.
It is the native root shell for BSD-based systems such as FreeBSD.

Almost all Linux distros and BSD today use tcsh instead of the original csh. In
most cases csh is a symbolic link that points to tcsh.
This is because tcsh is backward compatible with csh, and the last
is not maintained anymore.

- [TCSH Home](http://www.tcsh.org/)
- [TCSH Wikipedia](https://en.wikipedia.org/wiki/Tcsh)
- [TCSH manual page](http://www.tcsh.org/tcsh.html/top.html)
- [“An Introduction to the C shell”, William Joy](https://docs.freebsd.org/44doc/usd/04.csh/paper.html)
- [TCSH Bug reports and/or features requests](https://bugs.gw.com/)

Some more files:
[tcsh help command (for 132x35 terminal size)](https://github.com/nereusx/dotfiles/blob/master/csh-help),
[my ~/.tcshrc](https://github.com/nereusx/dotfiles/blob/master/.tcshrc)

```tcsh
#!/bin/tcsh
# The first line of the script is a shebang which tells the system how to execute
# the script: http://en.wikipedia.org/wiki/Shebang_(Unix)
# TCSH emulates the shebang on systems that don't understand it.

# In most cases you'll use `#!/bin/tcsh -f`, because `-f` option does not load
# any resource or start-up files, or perform any command hashing, and thus
# starts faster.

# --- the echo command --------------------------------------------------------
# The `echo` writes each word to the shell's standard output, separated by
# spaces and terminated with a newline. The echo_style shell variable may be
# set to emulate (or not) the flags and escape sequences.

# Display the value of echo_style
echo $echo_style

# Enable `echo` to support backslashed characters and `-n` option (no new line)
# This is the default for tcsh, but your distro may change it. Slackware has
# done so.
set echo_style = both

# Prints "Hello world"
echo Hello world
echo "Hello world"
echo 'Hello world'
echo `echo Hello world`

# This prints "twonlines" in one line
echo two\nlines

# Prints the two lines
echo "two\nlines"
echo 'two\nlines'

# --- Basic Syntax ------------------------------------------------------------

# A special character (including a blank or tab) may be prevented from having
# its special meaning by preceding it with a backslash `\`.
# This will display the last history commands
echo !!
# This will not
echo \!\!

# Single quotes prevent expanding special characters too, but some
# characters like `!` and backslash have higher priority
# `$` (variable value) will not expand
echo '$1 tip'
# `!` (history) will expand
echo '!!'

# Strings enclosed by back-quotes will be executed and replaced by the result.
echo `ls`

# Semi-colon separate commands
echo 'first line'; echo 'second line'

# There is also conditional execution
echo "Always executed" || echo "Only executed if the first command fails"
echo "Always executed" && echo "Only executed if the first command does NOT fail"

# Parenthesised commands are always executed in a subshell,

# example: creates a project and then informs you that it finished while
# it does the installation.
make && ( espeak "BOSS, compilation finished"; make install )

# prints the home directory but leaves you where you were
(cd; pwd); pwd

# Read tcsh man-page documentation
man tcsh

# --- Variables ---------------------------------------------------------------
# The shell maintains a list of variables, each of which has as value a list of
# zero or more words. The values of shell variables can be displayed and
# changed with the `set` and `unset` commands.
# The system maintains its own list of "environment" variables.
# These can be displayed and changed with `printenv`, `setenv`, and `unsetenv`.
# The syntax of `setenv` is similar to POSIX sh.

# Assign a value or nothing will create a variable
# Assign nothing
set var
# Assign a numeric value
# the '@' denotes the expression is arithmetic; it works similar to 'set' but
# the right value can be a numeric expression.
@ var = 1 + 2
# Assign a string value
set var = "Hello, I am the contents of 'var' variable"
# Assign the output of a program
set var = `ls`

# Remove a variable
unset var
# Prints 1 (true) if the variable `var` exists otherwise prints 0 (false)
echo $?var
# Print all variables and their values
set

# Prints the contents of 'var'
echo $var;
echo "$var";
# Prints the string `$var`
echo \$var
echo '$var'
# Braces can be used to separate variables from the rest when it is needed
set num = 12; echo "There ${num}th element"

# Prints the number of characters of the value: 6
set var = '123456'; echo $%var

### LISTs
# Assign a list of values
set var = ( one two three four five )
# Print all the elements: one two three four five
echo $var
echo $var[*]
# Print the count of elements: 5
echo $#var
# Print the indexed element; This prints the second element: two
echo $var[2]
# Print range of elements; prints 2nd up to 3rd: two, three
echo $var[2-3]
# Prints all elements starting from the 3rd: three four five
echo $var[3-]
# Prints print all up to 3rd element: one two three
echo $var[-3]

### Special Variables
# $argv         list of command-line arguments
# $argv[0]      this file-name (the file of the script file)
# $# $0, $n, $* are the same as $#argv, $argv[0], $argv[n], $argv[*]
# $status, $?   the exit code of the last command that executed
# $_            the previous command line
# $!            the PID of the last background process started by this shell
# $$            script's PID

# $path, $PATH  the list of directories that will search for an executable to run
# $home, $HOME  user's home directory, also the `~` can be used instead
# $uid          user's login ID
# $user         user's login name
# $gid          the user's group ID
# $group        the user's group-name
# $cwd, $PWD    the Current/Print Working Directory
# $owd          the previous working directory
# $tcsh         tcsh version
# $tty          the current tty; ttyN for Linux console, pts/N for terminal
#               emulators under X
# $term         the terminal type
# $verbose      if set, causes the words of each command to be printed.
#               can be set by the `-v` command line option too.
# $loginsh      if set, it is a login shell

# TIP: $?0 is always false in interactive shells
# TIP: $?prompt is always false in non-interactive shells
# TIP: if `$?tcsh` is unset; you run the original `csh` or something else;
#      try `echo $shell`
# TIP: `$verbose` is useful for debugging scripts
# NOTE: `$PWD` and `$PATH` are synchronised with `$cwd` and `$pwd` automatically.

# --- Variable modifiers ------------------------------------------------------
# Syntax: ${var}:m[:mN]
# Where <m> is:
# h : the directory  t : the filename  r : remove extension   e : the extension
# u : uppercase the first lowercase letter
# l : lowercase the first uppercase letter
# p : print but do not execute it (hist)
# q : quote the substituted words, preventing further substitutions
# x : like q, but break into words at white spaces
# g : apply the following modifier once to each word
# a  : apply the following modifier as many times as possible to single word
# s/l/r/ : search for `l` and replace with `r`, not regex; the `&` in the `r` is
# replaced by `l`
# & : Repeat the previous substitution

# start with this file
set f = ~/Documents/Alpha/beta.txt
# prints ~/Documents/Alpha/beta
echo $f:r
# prints ~/Documents/Alpha
echo $f:h
# prints beta.txt
echo $f:t
# prints txt
echo $f:e
# prints beta
echo $f:t:r
# prints Beta
echo $f:t:r:u
# prints Biota
echo $f:t:r:u:s/eta/iota/

# --- Redirection -------------------------------------------------------------

# Create file.txt and write the standard output to it
echo 'this string' > file.txt
# Create file.txt and write the standard output and standard error to it
echo 'this string' >& file.txt
# Append the standard output to file.txt
echo 'this string' >> file.txt
# Append the standard output and standard error to file.txt
echo 'this string' >>& file.txt
# Redirect the standard input from file.txt
cat < file.txt
# Input from keyboard; this stores the input line to variable `x`
set x = $<
# Document here;
cat << LABEL
...text here...
LABEL

# TIP: this is how to get standard error separated:
(grep 'AGP' /usr/src/linux/Documentation/* > output-file.txt) >& error-file.txt

# example: read a name from standard input and display a greetings message
echo -n "Enter your name: "
set name = $<
echo "Greetings $name"

# --- Expressions ------------------------------------------------------------

# Operators:
# ==  equal         !=  not equal    !  not
#  >  greater than   <  less than   >=  greater or equal  <= less or equal
# &&  logical AND   ||  logical OR

if ( $name != $user ) then
    echo "Your name isn't your username"
else
    echo "Your name is your username"
endif

# single-line form
if ( $name != $user ) echo "Your name isn't your username"

# NOTE: if $name is empty, tcsh sees the above condition as:
# if ( != $user ) ...
# which is invalid syntax
# The "safe" way to use potentially empty variables in tcsh is:
# if ( "$name" != $user ) ...
# which, when $name is empty, is seen by tcsh as:
# if ( "" != $user ) ...
# which works as expected

# There is also conditional execution
echo "Always executed" || echo "Only executed if the first command fails"
echo "Always executed" && echo "Only executed if the first command does NOT fail"

# To use && and || with if statements, you don't need multiple pairs of
# square brackets:
if ( "$name" == "Steve" && "$age" == 15 ) then
    echo "This will run if $name is Steve AND $age is 15."
endif

if ( "$name" == "Daniya" || "$name" == "Zach" ) then
    echo "This will run if $name is Daniya OR Zach."
endif

# String matching operators ( `=~` and `!~` )
# The ‘==’ ‘!=’ ‘=~’ and ‘!~’ operators compare their arguments as strings;
# all others operate on numbers. The operators ‘=~’ and ‘!~’ are like ‘!=’
# and ‘==’ except that the right hand side is a glob-pattern against which
# the left-hand operand is matched.

if ( $user =~ ni[ck]* ) echo "Greetings Mr. Nicholas."
if ( $user !~ ni[ck]* ) echo "Hey, get out of Nicholas' PC."

# Arithmetic expressions are denoted with the following format:
@ result = 10 + 5
echo $result

# Arithmetic Operators
# +, -, *, /, %
#
# Arithmetic Operators which must be parenthesized
# !, ~, |, &, ^, ~, <<, >>,
# Compare and logical operators
#
# All operators are the same as in C.

# It is non so well documented that numeric expressions require spaces
# in-between; Also, `@` has its own parser, it seems that it works well when
# the expression is parenthesized, otherwise the primary parser seems to be
# active. Parentheses require spaces around, this is documented.

# wrong
@ x = $y+1
@ x = 0644 & 022;      echo $x
@ x = (0644 & 022) +1; echo $x
@ x = (0644 & 022)+ 1; echo $x
@ x = ( ~077 );        echo $x

# correct
@ x = $y + 1
@ x = ( 0644 & 022 ) + 1; echo $x
@ x = ( ~ 077 );          echo $x
@ x = ( ~ 077 | 022 );    echo $x
@ x = ( ! 0 );            echo $x

# C's operators ++ and -- are supported if there is not assignment
@ result ++

# No shell was created to do mathematics;
# Except for the basic operations, use an external command with backslashes.
#
# I suggest the calc as the best option.
# (http://www.isthe.com/chongo/tech/comp/calc/)
#
# The standard Unix's bc as the second option
# (https://www.gnu.org/software/bc/manual/html_mono/bc.html)
#
# The standard Unix's AWK as the third option
# (https://www.gnu.org/software/gawk/manual/gawk.html)

# You can also use `Perl`, `PHP`, `python`, or even several BASICs, but prefer
# the above utilities for faster load-and-run results.

# real example: (that I answer in StackExchange)
# REQ: x := 1001b OR 0110b

# in `tcsh` expression (by using octal)
@ x = ( 011 | 06 ); echo $x

# the same by using `calc` (and using binary as the original req)
set x = `calc '0b1001 | 0b110'`; echo $x

# --- File Inquiry Operators --------------------------------------------------
# NOTE: The built-in `filetest` command does the same thing.

#### Boolean operators
# -r  read access    -w  write access    -x  execute access    -e  existence
# -f  plain file     -d  directory       -l  symbolic link     -p  named pipe
# -S  socket file
# -o  ownership      -z  zero size       -s  non-zero size
# -u  SUID is set    -g  SGID is set     -k  sticky is set
# -b  block device   -c  char device
# -t  file (digit) is an open file descriptor for a terminal device

# If the file `README` exists, display a message
if ( -e README ) echo "I have already README file"

# If the `less` program is installed, use it instead of `more`
if ( -e `where less` ) then
    alias more 'less'
endif

#### Non-boolean operators
# -Z  returns the file size in bytes
# -M  returns the modification time (mtime)    -M: returns mtime string
# -A  returns the last access time (atime)     -A: returns atime string
# -U  returns the owner's user ID              -U: returns the owner's user name
# -G  returns the owner's group ID             -G: returns the owner's group name
# -P  returns the permissions as octal number  -Pmode returns perm. AND mode

# this will display the date as a Unix-time integer: 1498511486
filetest -M README.md

# This will display "Tue Jun 27 00:11:26 2017"
filetest -M: README.md

# --- Basic Commands ----------------------------------------------------------

# Navigate through the filesystem with `chdir` (cd)
cd path # change working directory
cd      # change to the home directory
cd -    # change to the previous directory
cd ..   # go up one directory

# Examples:
cd ~/Downloads # go to my `Downloads` directory

# Use `mkdir` to create new directories.
mkdir newdir
# The `-p` flag causes new intermediate directories to be created as necessary.
mkdir -p ~/.backup/saves

# which & where
# find if csh points to tcsh
ls -lha `which csh`
# find if csh is installed on more than one directory
where csh

# --- Pipe-lines --------------------------------------------------------------
# A pipeline is a sequence of processes chained together by their standard
# streams, so that the output of each process (stdout) feeds directly as input
# (stdin) to the next one. These `pipes` are created with the `|` special
# character and it is one of the most powerful characteristics of Unix.

# example:
ls -l | grep key | less
# "ls -l" produces a process, the output (stdout) of which is piped to the
# input (stdin) of the process for "grep key"; and likewise for the process
# for "less".

# the `ls`, the `grep`, and the `less` are Unix programs and they have their
# own man-page. The `pipe` mechanism is part of the kernel but the syntax
# and the control is the shell's job, the tcsh in our case.

# NOTE: Windows has the `pipe` mechanism too, but it is buggy and I signed it
# for all versions until Windows XP SP3 API32 which was the last one that I
# worked on. Microsoft denied it, but it is a well-known bug since it is a
# common method for inter-process communication. For small I/O it will work well.
# tcsh, along with grep, GCC, and Perl is one of the first Unix programs that
# ported to DOS (with EMX DOS extender) and later to Windows (1998).

# example: this will convert tcsh to PostScript and will show it with Okular
zcat /usr/man/man1/tcsh.1.gz | groff -Tps -man | okular -

# a better version
zcat `locate -b -n 1 '\tcsh.1.gz'` | groff -Tps -man | okular -

# even better
set page = tcsh; set loc = (locate -b -n 1 "\\\\"${page}".1.gz");
 zcat `eval $loc` | groff -Tps -man | okular -

# the same, modified to create man page pdf
set page = tcsh; set loc = (locate -b -n 1 "\\\\"${page}".1.gz");
 zcat `eval $loc` | groff -Tps -man | ps2pdf - ${page}.pdf

# the same, but now shows the ${page}.pdf too
set page = tcsh; set loc = (locate -b -n 1 "\\\\"${page}".1.gz");
 zcat `eval $loc` | groff -Tps -man | ps2pdf - ${page}.pdf && okular tcsh.pdf

# NOTE: `okular` is the default application of the KDE environment and it shows
# postcript and pdf files. You can replace it with your lovely PDF viewer.
# `zcat`, `locate`, `groff`, are common programs in all Unixes. The `ps2pdf`
# program is part of the `ghostscript` package that is widely used.

# --- Control Flow ------------------------------------------------------------

#### IF-THEN-ELSE-ENDIF
# Syntax:
# if ( expr ) then
#    ...
# [else if ( expr2 ) then
#    ...]
# [else
#    ...]
# endif
#
# If the specified `expr` is true then the commands to the first else are
# executed; otherwise if `expr2` is true then the commands to the second else
# are executed, etc.
# Any number of else-if pairs are possible; only one endif is needed.
#
# Single-line form:
#
# if ( expr ) command
#
# If `expr` evaluates to true, then the command is executed.
# `command` must be a simple command, not an alias, a pipeline, a command list
#, or a parenthesized command list. With a few words, avoid using it.
#
# BUG: Input/output redirection occurs even if expr is false and the command
# is thus not executed.
#

# check if we are in a non-interactive shell and quit if true
if ( $?USER == 0 || $?prompt == 0 ) exit

# check if we are a login shell
if ( $?loginsh ) then
    # check if you are on linux console (not X's terminal)
    if ( $tty =~ tty* ) then
        # enable keypad application keys (man console_codes)
        echo '\033='
    endif
endif

#### SWITCH-ENDSW
# Syntax:
# switch ( expr )
# case pattern:
#     ...
#     [breaksw]
# [default:
#     ...]
# endsw
#
# tcsh uses a case statement that works similarly to switch in C.
# Each case label is successively matched, against the specified string which
# is first command and filename expanded. The file metacharacters `*`, `?`
# and `[...]` may be used in the case labels. If none of the labels match the
# execution begins after the default label if it's defined.
# The command `breaksw` causes execution to continue after the endsw. Otherwise,
# control may fall through case labels and default labels as in C.

switch ( $var )
case *.[1-9]:
case *.[1-9].gz:
    echo "$var is a man-page."
    breaksw
case *gz:
    echo "$var is gzipped"
    breaksw
default:
    file $var
endsw

#### FOREACH-END
# Syntax:
# foreach name ( wordlist )
#    ...
#   [break | continue]
# end
#
# Successively sets the variable `name` to each member of `wordlist` and
# executes the sequence of commands between this command and the matching
# `end` keyword. The `continue` keyword jumps to the next element back to
# top, and the `break` keyword terminates the loop.
#
# BUG: `foreach` doesn't ignore here documents when looking for its end.

# example: counting 1 to 10
foreach i ( `seq 1 10` )
    echo $i
end

# example: type all files in the list
foreach f ( a.txt b.txt c.txt )
    cat $f
end

# example: convert wma to ogg
foreach f ( *.wma )
    ffmpeg -i "$f" "$f:r".ogg
end

#### WHILE-END
# while ( expr )
#     ...
#     [break | continue]
# end
#
# Executes the commands between the `while` and the matching `end` while `expr`
# evaluates non-zero. `break` and `continue` may be used to terminate or
# continue the loop prematurely.

# count from 1 to 10
set num = 1
while ( $num <= 10 )
    echo $num
    @ num ++
end

# print all directories of CWD
set lst = ( * )
while ( $#lst )
    if ( -d $lst[1] ) echo $lst[1] is directory
    shift lst
end

# separate command-line arguments to options or parameters
set options
set params
set lst = ( $* )
while ( $#lst )
    if ( "$lst[1]" =~ '-*' ) then
        set options = ( $options $lst[1] )
    else
        set params = ( $params $lst[1] )
    endif
    shift lst
end
echo 'options =' $options
echo 'parameters =' $params

#### REPEAT
# Syntax: repeat count command
#
# The specified command, which is subject to the same restrictions as the
# command in the one line `if` statement above, is executed count times.
# I/O redirections occur exactly once, even if `count` is 0.
#
# TIP: in most cases prefer `while`

repeat 3 echo "ding dong"

# --- Functions ---------------------------------------------------------------
# tcsh has no functions but its expression syntax is advanced enough to use
# `alias` as functions. Another method is recursion

# Alias argument selectors; the ability to define an alias to take arguments
# supplied to it and apply them to the commands that it refers to.
# Tcsh is the only shell that provides this feature.
#
# \!#   argument selector for all arguments, including the alias/command
#       itself; arguments need not be supplied.
# \!*   argument selector for all arguments, excluding the alias/command;
#       arguments need not be supplied.
# \!$   argument selector for the last argument; argument need not be supplied,
#       but if none is supplied, the alias name is considered to be the
#       last argument.
# \!^   argument selector for first argument; argument MUST be supplied.
# \!:n  argument selector for the nth argument; argument MUST be supplied;
#       n=0 refers to the alias/command name.
# \!:m-n   argument selector for the arguments from the mth to the nth;
#       arguments MUST be supplied.
# \!:n-$   argument selector for the arguments from the nth to the last;
#       at least argument n MUST be supplied.

# Alias the cd command so that when you change directories, the contents
# are immediately displayed.
alias cd 'cd \!* && ls'

# --- Recursion method --- begin ---
#!/bin/tcsh -f
set todo = option1
if ( $#argv > 0 ) then
    set todo = $argv[1]
endif

switch ( $todo )
case option1:
#    ...
    $0 results
    breaksw
case option2:
#    ...
    $0 results
    breaksw
case results:
    echo "print the results here"
#    ...
    breaksw
default:
    echo "Unknown option: $todo"
#    exit 0
endsw
# --- Recursion method --- end ---

# --- examples ----------------------------------------------------------------

# this script prints available power-states if no argument is set;
# otherwise it sets the state of the $argv[1]
# --- power-state script --- begin --------------------------------------------
#!/bin/tcsh -f
# get parameter ("help" for none)
set todo = help
if ( $#argv > 0 ) then
    set todo = $argv[1]
endif
# available options
set opts = `cat /sys/power/state`
# is known?
foreach o ( $opts )
    if ( $todo == $o ) then
        # found; execute it
        echo -n $todo > /sys/power/state
        break
    endif
end
# print help and exit
echo "usage: $0 [option]"
echo "available options on kernel: $opts"
# --- power-state script --- end ----------------------------------------------

# Guess the secret number game
# --- secretnum.csh --- begin -------------------------------------------------
#!/bin/tcsh -f
set secret=`shuf -i1-100 -n1`
echo "I have a secret number from 1 up to 100"
while ( 1 )
    echo -n "Guess: "
    set guess = $<
    if ( $secret == $guess ) then
        echo "You found it"
        exit 1
    else
        if ( $secret > $guess ) then
            echo "its greater"
        else if ( $secret < $guess ) then
                echo "its lesser"
            endif
        endif
    endif
end
# --- secretnum.csh --- end ---------------------------------------------------

# -----------------------------------------------------------------------------
# Appendices

#### About [T]CSH:
# * CSH is notorious for its bugs;
# * It is also famous for its advanced interactive mode.
# * TCSH is famous for having the most advanced completion subsystem.
# * TCSH is famous for having the most advanced aliases subsystem; aliases
#   can take parameters and often be used as functions!
# * TCSH is well known and preferred by people (me too) because of better
#   syntax. All shells are using Thomson's syntax with the exception of
#   [t]csh, fish, and plan9's shells (rc, ex).
# * It is smaller and consumes far less memory than bash, zsh, and even mksh!
#   (memusage reports)
# * TCSH still has bugs; fewer, but it does; if you write readable clean code
#   you'll find none; well almost none... This has to do with the implementation
#   of csh; that doesn't mean the other shells have a good implementation.
# * no well-known shell is capable of regular programming; if your script
#   is getting big, use a programming language, like Python, PHP, or Perl (good
#   scripting languages).
#
# Advice:
# 1. Do not use redirection in single-line IFs (it is well documented bug)
#    In most cases avoid using single-line IFs.
# 2. Do not mess up with other shells' code, c-shell is not compatible with
#    other shells and has different abilities and priorities.
# 3. Use spaces as you'll use them to write readable code in any language.
#    A bug of csh was `set x=1` and `set x = 1` worked, but `set x =1` did not!
# 4. It is well documented that numeric expressions require spaces in between;
#    also parenthesize all bit-wise and unary operators.
# 5. Do not write a huge weird expression with several quotes, backslashes, etc
#    It is bad practice for generic programming, it is dangerous in any shell.
# 6. Help tcsh, report the bug here <https://bugs.gw.com/>
# 7. Read the man page, `tcsh` has a huge number of options and variables.
#
#    I suggest the following options enabled by default
#    --------------------------------------------------
# Even in non-interactive shells
#    set echo_style=both
#    set backslash_quote
#    set parseoctal
#    unset noclobber
#
# Whatever...
#    set inputmode=insert
#    set autolist
#    set listjobs
#    set padhour
#    set color
#    set colorcat
#    set nobeep
#    set cdtohome
#
#    set histdup
#    set histlit
#    set nohistclop
#
#    unset compat_expr
#    unset noglob
#    unset autologout
#    unset time
#    unset tperiod
#
# NOTE: If the `backslash_quote` is set, it may create compatibility issues
# with other tcsh scripts that were written without it.
#
# NOTE: The same for `parseoctal`, but it is better to fix the problematic
# scripts.
#
# NOTE: **for beginners only**
# This enables automatic rescanning of `path` directories if needed. (like bash)
#    set autorehash

#### common aliases
#    alias hist  'history 20'
#    alias ll    'ls --color -lha'
#    alias today "date '+%d%h%y'
#    alias ff    'find . -name '

#### a nice prompt
#    set prompt = "%B%{\033[35m%}%t %{\033[32m%}%n@%m%b %C4 %# "
```
