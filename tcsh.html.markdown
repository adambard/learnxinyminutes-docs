---
language: tcsh
filename: LearnTCSH.csh
contributors:
    - ["Nicholas Christopoulos", "https://github.com/nereusx"]
lang: en
---
# The Tenex C Shell (TCSH)

## Abstract
**tcsh** (tee-see-shell) is a **Unix** shell based on and compatible with the **C shell** (csh).
It is essentially the C shell with programmable command-line *completion*, command-line editing, and a lot more features.
It is the native root shell for **BSD**-based systems such as the *FreeBSD* and the *MacOS X*.

Almost all **Linux** distros and **BSD** today use **tcsh** instead of the original **csh**.
In most cases **csh** is a *symbolic link* that points to **tcsh**.
This is because **tcsh** is backward compatible with **csh**, and the last is not maintained anymore.

## About [T]CSH:
* CSH is **notorious** about its bugs;
* It was also **famous** about its advanced interactive mode.
* TCSH is famous that have the most advanced completion subsystem.
* TCSH is famous that have the most advanced aliases subsystem; aliases
   can take parameters and often used as functions!
* TCSH is well known that preferred by people (me too) because of better
   syntax. All shells are using Thomson's syntax with exception of [t]csh,
   fish and plan9's shells (rc, ex).
* It is smaller and consume far less memory than bash, zsh even mksh!
   (memusage reports)
* TCSH still has bugs; less but have; if you write readable clean code you'll
   find none; well almost none... This has to do with the implementation of
   csh; that no means the other shells has good implementation.
* No shell is capable of regular programming. If your script is large, use
   a programming language or at least PHP or Perl (good scripting languages)

---

```tcsh
#!/bin/tcsh
# The lines which starting with '#' are comments; comments are ignored.
#
# The first line of the script is the "shebang" which tells the system how to
# execute the script (see http://en.wikipedia.org/wiki/Shebang_(Unix))
#
# In most cases you'll use `#!/bin/tcsh -f', because `-f' option does not load
# any resource or start-up files, or perform any command hashing, and thus
# starts faster.

# --- the echo command --------------------------------------------------------
# The `echo' writes each word to the shell's standard output, separated by
# spaces and terminated with a newline. The shell's standard output it is the
# terminal screen but can be also a file or a pipe-line.
#
# The echo_style shell variable may be set to emulate (or not) the flags and
# escape sequences.

# Print the value of echo_style
echo $echo_style

# Enable `echo' to support back-slashed characters and the `-n' option (no new
# line). This is the default for tcsh, but your distro may change it. Slackware
# has done so. If your script run it as `csh' instead of `tcsh' the echo_style
# also will be altered for compatibility with Berkley's C-Shell.
set echo_style = both

# Several ways to print "Hello world"
echo Hello world
echo "Hello world"
echo 'Hello world'
echo `echo Hello world`

# For those who are unfamiliar with backslash: 
# This character has special meaning in C language and so in shells and in the
# whole Unix.
# \n   = a new line,
# \t   = a [TAB] character, 
# \033 = the ESC-ape character (ASCII 27) used often by terminals to select
# colours or communicate with its hardware.
#
# The quote characters in shells have the special meaning to defines a string.
# So if you want to include one of those characters inside the string, type:
# \"   = for double quotes
# \'   = for single quotes
#
# Using the \ as the last character of a line, it means that the line continues
# to the next line; This is very useful on long pipe-lines and other cases.
#
# To display the backslash you have to type it two times, like this: \\

# This prints "line1nline2" in one line, because the \n interpreted
# before executed by the `echo'. This is the correct, not a bug.
echo line1\nline2

# To print the two lines you have to type
echo "line1\nline2"
# or
echo 'line1\nline2'

# --- Basic Syntax ------------------------------------------------------------

# A special character (including a blank or tab) may be prevented from having
# its special meaning by preceding it with a backslash `\'.
# this will display the last history commands
echo !!
# this will not
echo \!\!

# Single quotes prevents expanding special characters too, but some
# characters like `!' and backslash have higher priority
# `$' (variable value) will not expands
echo '$1 tip'
# `!' (history) will expands
echo '!!'

# Strings enclosed by back-quotes will be executed and replaced by the result.
echo `ls`

# Execute multiple cmds on the same line (;)
echo 'first line'; echo 'second line'

# There is also conditional execution (||) (&&)
echo "Always executed" || echo "Only executed if first command fails"
echo "Always executed" && echo "Only executed if first command does NOT fail"

# Parenthesised commands are always executed in a subshell,

# Example: create a project and then informs you that it finished while
# it does the installation.
make && ( espeak "BOSS, compilation finished"; make install )

# Prints the home directory but leaving you where you were
(cd; pwd); pwd

# Execute command in background (&)
(sleep 5; echo 'DING! dinner is ready') &

# Read tcsh manual page
man tcsh

# --- Executing a script ------------------------------------------------------
# Read and execute commands - in the current process - from a file.
# Because we execute it in the current process any change that made will affect
# the our current shell's environment. This is useful when we changes our
# ~/.tcshrc; by giving `source ~/.tcshrc' it reloads the configuration file.

echo "echo Hello World; set new_variable = 1" > script.csh
source script.csh
echo $new_variable

# Another method to run a script but in separated process, is by using
# the shebang 

echo "#\!/bin/tcsh -f\necho Hello World" > script.csh
chmod +x script.csh
./script.csh

# Another method to run a script is by using it as parameter of tcsh

echo "echo Hello World" > script.csh
tcsh -f script.csh

# or without creating the file, just passing the commands as parameter

tcsh -f -c "echo Hello World"

# Of course we can use back-quotes as already explained
`echo "echo Hello World"`

# or using the braces but only inside '@', 'if' or 'while' expressions
@ x = { echo "Hello, World" }

# --- Variables ---------------------------------------------------------------
# The shell maintains a list of variables, each of which has as value a list of
# zero or more words. The values of shell variables can be displayed and
# changed with the `set' and `unset' commands.
# The system maintains its own list of ``environment'' variables.
# These can be displayed and changed with `printenv', `setenv' and `unsetenv'.
# The syntax of `setenv' is similar to POSIX shell.

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
# Prints 1 (true) if the variable `var' exists otherwise prints 0 (false)
echo $?var
# Print all variables and their values
set

# Prints the contents of 'var'
echo $var;
echo "$var";
# Prints the string `$var'
echo \$var
echo '$var'
# Braces can be used to separate variable from the rest when its needed
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
# Print indexed element; prints the second element: two
echo $var[2]
# Print range of elements; prints 2nd up to 3rd: two, three
echo $var[2-3]
# Prints all elements starting from the 3rd: three four five
echo $var[3-]
# Prints print all up to 3rd element: one two three
echo $var[-3]
# insert a value at the beginning of the list
set -f path = ( $path /bin )
# append a value at the end of the list
set -l path = ( $path /usr/local/bin )

### Special Variables
# $argv         list of command-line arguments
# $argv[0]      this file-name (the file of the script file)
# $# $0, $n, $* are the same as $#argv, $argv[0], $argv[n], $argv[*]
# $status, $?   the exit code of the last command that executed
# $_            the previous command line
# $!            the PID of the last background process started by this shell
# $$            script's PID

# $path, $PATH  the list of directories that will search for executable to run
# $home, $HOME  user's home directory, also the `~' can be used instead
# $uid          user's login ID
# $user         user's login name
# $gid          the user's group ID
# $group        the user's group-name
# $cwd, $PWD    the Current/Print Working Directory
# $owd          the previous working directory
# $tcsh         tcsh version
# $tty          the current tty; ttyN for linux console, pts/N for terminal
#               emulators under X
# $term         the terminal type
# $verbose      if set, causes the words of each command to be printed.
#               can be set by the `-v' command line option too.
# $loginsh      if set, it is a login shell

# TIP: $?0 is always false in interactive shells
# TIP: $?prompt is always false in non-interactive shells
# TIP: if `$?tcsh' is unset; you run the original `csh' or something else;
#      try `echo $shell'
# TIP: $verbose this is useful to debugging scripts
# NOTE: $PWD and $PATH are synchronised with $cwd and $pwd automatically.

# --- Variable Modifiers ------------------------------------------------------
# Syntax: ${var}:m[:mN]
# Where <m> is:
# h : the directory  t : the filenane  r : remove extension   e : the extension
# u : uppercase the first lowercase letter
# l : lowercase the first uppercase letter
# p : print but do not execute it (hist)
# q : quote the substituted words, preventing further substitutions
# x : like q, but break into words at white spaces
# g : apply the following modifier once to each word
# a  : apply the following modifier as many times as possible to single word
# s/l/r/ : search for `l' and replace with `r', not regex; the `&' in the r is
# replaced by l
# & : Repeat the previous substitution

# Start with this file
set f = ~/Documents/Alpha/beta.txt
# Prints ~/Documents/Alpha/beta
echo $f:r
# Prints ~/Documents/Alpha
echo $f:h
# Prints beta.txt
echo $f:t
# Prints txt
echo $f:e
# Prints beta
echo $f:t:r
# Prints Beta
echo $f:t:r:u
# Prints Biota
echo $f:t:r:u:s/eta/iota/

# Playing with modifiers in lists
set lst = ( bench-expr/* )

# This prints the whole list:
# bench-expr/awk.csh bench-expr/bas.csh bench-expr/bc.csh
echo $lst

# This prints the base-names with the directories:
# bench-expr/awk bench-expr/bas bench-expr/bc
echo $lst:gr

# This prints the extensions:
# csh csh csh
echo $lst:ge

# This prints the directories:
# bench-expr bench-expr bench-expr
echo $lst:gh

# This prints the file-names:
# awk.csh bas.csh bc.csh
echo $lst:gt

# The `shift' built-in command removes the first element of a list.
# If no variable name is given, then uses the $argv
set lst = ( a b c )
shift lst
# Prints: b c
echo $lst

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
# Input from keyboard; this stores the input line to variable `x'
set x = $<
# The so called "Here Document" method
cat << LABEL
...text here...
LABEL

# TIP: this is how to get standard error separated:
(grep 'AGP' /usr/src/linux/Documentation/* > output-file.txt) >& error-file.txt

# example: read a name from standard input and display a greetings message
echo -n "Enter your name: "
set name = $<
echo "Greetings $name"

# "Here Document" examples
# This prints the whole text until EOT label found
cat << EOT
Welcome $user to XYZ server.

We are provide the following services:
1. help . .. Help-desk menu
2. hosts ... List of associated hosts
...
EOT

# here's an example of creating a temporary file:
set tempdata = /tmp/tempdata.$$
cat > $tempdata << ENDOFTMP
53.3 94.3 67.1
48.3 01.3 99.9
42.1 48.6 92.8
ENDOFTMP

# --- Expressions -------------------------------------------------------------

# Operators:
# ==  equal         !=  not equal    !  not
#  >  greater than   <  less than   >=  greater or equal  <= less or equal
# &&  logical AND   ||  logical OR

if ( $user == "mchris" ) then
	echo "Welcome Maria"
else
	echo "Welcome $user"
endif

# Single-line form
if ( $user == "nicholas" ) echo "Greetings Mr. Nicholas"

# Conditional execution
echo "Always executed" || echo "Only executed if first command fails"
echo "Always executed" && echo "Only executed if first command does NOT fail"

# You don't need multiple pairs of parenthesis as in other shells, no special
# operators, nor quotes; generally speaking the expressions are more improved
# from other shells

if ( $name != "root" && $age > 18 ) then
	echo "This will run for any normal adult user."
endif

if ( $name == "Daniya" || $name == "Zach" ) then
	echo "This will run if $name is Daniya OR Zach."
endif

# String matching operators ( `=~' and `!~' )
# The ‘==’ ‘!=’ ‘=~’ and ‘!~’ operators compare their arguments as strings;
# all others operate on numbers. The operators ‘=~’ and ‘!~’ are like ‘!=’
# and ‘==’ except that the right hand side is a glob-pattern against which
# the left hand operand is matched.

if ( $user =~ ni[ck]* ) echo "Greetings Mr. Nicholas."
if ( $user !~ ni[ck]* ) echo "Hey, get out of Nicholas PC."

# TIP: If something goes wrong with your expressions, try to enclose it
#      in quotes and single-quotes, depending of what is most logical.
#
# My advice is, always use quotes, this will save you many times.

if ( "$user" =~ 'ni[ck]*' ) echo "Greetings Mr. Nicholas."

# Special operators { cmd }
# Executes the `cmd' and returns 0 if the command failed.
# It works only inside `@', `if' and `while' expressions

# Check for a user in password database
if ( { grep -s "$1" /etc/passwd } ) then
	echo "User name $1 already exists"
endif

# Arithmetic expressions are denoted with the following format:
@ result = 10 + 5
echo $result

# Arithmetic Operators
# +, -, *, /, %
#
# Reassign operators
# += -= *= /= %= &= ^= |=
#
# Arithmetic Operators which must be parenthesised
# !, ~, |, &, ^, ~, <<, >>,
# Compare and logical operators
#
# All operators are same as in C with the same priority.

# It is non so well documented that numeric expressions require spaces
# in-between; Also, `@' has its own parser, it seems that work well when the
# expression is parenthesised otherwise the primary parser seems it is active.
# Parenthesis require spaces around, this is documented.

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

# No shell is designed to do math .-
# Except for the integer expressions, use an external command with back-quotes.
#
# I suggest the `calc' as the best option; it is powerful and fast.
# (http://www.isthe.com/chongo/tech/comp/calc/)
#
# The standard UNIX's bc as second option
# (https://www.gnu.org/software/bc/manual/html_mono/bc.html)
#
# You can also use `perl', or several BASICs, but prefer the above
# utilities for faster load-and-run results.

# real example: (that I answered in StackExchange)
# REQ: x := 1001b OR 0110b

# In `tcsh' expression (by using octal)
@ x = ( 011 | 06 ); echo $x

# The same by using `calc' (and using binary as the original REQ)
set x = `calc '0b1001 | 0b110'`; echo $x

# --- File Inquiry Operators --------------------------------------------------
# NOTE: The builtin `filetest' command do the same thing.

#### Boolean operators
# -r  read access    -w  write access    -x  execute access    -e  existence
# -f  plain file     -d  directory       -l  symbolic link     -p  named pipe
# -S  socket file
# -o  ownership      -z  zero size       -s  non-zero size
# -u  SUID is set    -g  SGID is set     -k  sticky is set
# -b  block device   -c  char device
# -t  file (digit) is an open file descriptor for a terminal device

# If the file `README' exists, displays a message
if ( -e README ) echo "I have already README file"

# If the `less' program is installed, use this instead of `more'
if ( -e `where less` ) then
	alias more 'less'
endif

#### Non-boolean operators
# -Z  returns the file size in bytes
# -M  returns the modification time (mtime)    -M: returns mtime string
# -A  returns the lass access time (atime)     -A: returns atime string
# -U  returns the owners user ID               -U: returns the owners user-name
# -G  returns the group ID                     -G: returns the group-name
# -P  returns the permissions as octal number  -Pmode returns perm. AND mode

# This will display the date as Unix-time integer: 1498511486
filetest -M README.md

# This will display "Tue Jun 27 00:11:26 2017"
filetest -M: README.md

# --- Basic Commands ----------------------------------------------------------

# Navigate though file system with `chdir' (cd)
cd path # change working directory
cd      # change to home directory
cd -    # change to previous directory
cd ..   # go up one directory

# Examples:
cd ~/Downloads # go to my `Downloads' directory

# Use `mkdir` to create new directories.
mkdir newdir
# The `-p` flag causes new intermediate directories to be created as necessary.
mkdir -p ~/.backup/saves

# Find if csh points to tcsh
ls -lha `which csh`

# Find if csh is installed on more than one directory
where csh

# --- Pipe-lines --------------------------------------------------------------
# A pipeline is a sequence of processes chained together by their standard
# streams, so that the output of each process (stdout) feeds directly as input
# (stdin) to the next one. This `pipes' are created with the `|' special
# character and it is one of the most powerful characteristics of Unix.

# example:
ls -l | grep key | less
# "ls -l" produces a process, the output (stdout) of which is piped to the
# input (stdin) of the process for "grep key"; and likewise for the process
# for "less".

# The `ls', the `grep' and the `less' are programs of Unix and they have their
# own man-page. The `pipe' mechanism is part of the kernel but the syntax
# and the control is job of the shell, the tcsh in our case.

# Example: this will convert tcsh to PDF and will show it with okular
zcat /usr/man/man1/tcsh.1.gz | groff -Tpdf -man | okular -

# a better version
zcat `locate -b -n 1 '\tcsh.1.gz'` | groff -Tpdf -man | okular -

# even better
set page = tcsh; set loc = (locate -b -n 1 "\\\\"${page}".1.gz"); \
	zcat `eval $loc` | groff -Tpdf -man | okular -

# even more better...
# This one-line code will ask you to type the page that are you
# looking for and then it will show its manual with okular
echo -n "Enter the command you are looking for or press ^C to cancel: "; \
	set page = $<; \
	set loc = (locate -b -n 1 "\\\\"${page}".1.gz"); \
	zcat `eval $loc` | groff -Tpdf -man | okular -

# NOTE: `okular' is the default application of KDE environment and it shows
# Postscript and PDF files. You can replace it with your lovely PDF viewer.
# zcat, locate, groff, are common programs in all Unixes.

# --- Control Flow ------------------------------------------------------------

#### IF-THEN-ELSE-ENDIF
# if ( expr ) then
#    ...
# [else if ( expr2 ) then
#    ...]
# [else
#    ...]
# endif
#
# If the specified expr is true then the commands to the first else are
# executed; otherwise if expr2 is true then the commands to the second else
# are executed, etc.
# Any number of else-if pairs are possible; only one endif is needed.

# Check if we are a login shell
if ( $?loginsh ) then
	# check if you are on linux console (not X's terminal)
	if ( $tty =~ tty* ) then
		# enable keypad application keys (man console_codes)
		echo '\033='
	endif
endif

#### SINGLE-LINE-IF
# if ( expr ) command
#
# If `expr' evaluates true, then command is executed.
# `command' must be a simple command, not an alias, a pipeline, a command list
# or a parenthesised command list. With few words, avoid to use it.
#
# BUG: Input/output redirection occurs even if expr is false and command is
# thus not executed.

# check if we are in non-interactive shell and quit if true
if ( $?USER == 0 || $?prompt == 0 ) exit

#### SWITCH-ENDSW
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
# is first command and filename  expanded. The file  metacharacters `*', `?'
# and `[...]' may be used in the case labels. If none of the labels match the
# execution begins after the default label if its defined.
# The command `breaksw' causes execution to continue after the endsw. Otherwise
# control may fall through case labels and default labels as in C.

switch ( $var )
case *.[1-8]:
case *.[1-8].gz:
	echo "$var is a man-page."
	breaksw
case *gz:
	echo "$var is gzipped"
	breaksw
default:
	file $var
endsw

#### FOREACH-END
# foreach name ( wordlist )
#	...
#   [break | continue]
# end
#
# Successively sets the variable `name' to each member of `wordlist' and
# executes the sequence of commands between this command and the matching
# `end' keyword. The `continue' keyword jump to the next element back to
# top; and the `break' keyword terminates the loop.
#
# BUG: `foreach' doesn't ignore here documents when looking for its end.

# Counting 1 to 10
foreach i ( `seq 1 10` )
	echo $i
end

# Type all files in the list
foreach f ( a.txt b.txt c.txt )
	cat $f
end

# Convert all .wma files (windows audio) to .ogg format
foreach f ( *.wma )
	ffmpeg -i "$f" "$f:r".ogg
end

#### WHILE-END
# while ( expr )
#     ...
#     [break | continue]
# end
#
# Executes the commands between the `while' and the matching `end' while `expr'
# evaluates non-zero. `break' and `continue' may be used to terminate or
# continue the loop prematurely.

# Count from 1 to 10
set num = 1
while ( $num <= 10 )
	echo $num
	@ num ++
end

# Print all directories of CWD
set lst = ( * )
while ( $#lst )
	if ( -d $lst[1] ) echo $lst[1] is directory
	shift lst
end

# Separate command-line arguments to options or parameters
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
# command in the one line if statement above, is executed count times.
# I/O redirections occur exactly once, even if count is 0.
#
# TIP: in most cases prefer `while'

repeat 3 echo "ding dong"

#### GOTO
# label:
#	...
# goto label
#
# The shell rewinds its input as much as possible, searches for a line of the
# form `label:', and continues execution after that line.
# The label must be alone in the line.

# A classic endless loop
set n = 1
start:
echo $n bottles of beer
@ n ++
goto start

# A Random Destination Program; results are random
# Also, this is an implementation of BASIC's ON-GOTO
start:
set n = `shuf -i1-3 -n1`
set ongoto = ( label_a label_b label_c )
goto $ongoto[$n]
label_a:
label_b:
echo "this port is closed"
goto start
label_c:
echo "at last, a light in the tunnel"

# --- Functions ---------------------------------------------------------------
# tcsh has no functions but its expression syntax is advanced enough to use
# `alias' as functions. Another method is recursion

# Aliases is almost the most useful command in TCSH, 'alias' command assigns
# a string to a name, 'set' do to variables; but aliases are not variables,
# they are commands.

# In this example the newly created 'reload' command will reloads the
# our configuration file every time we execute it.
alias reload ‘source ~/.tcshrc‘
reload

# The 'make' is an common UNIX utility that it is used when build a program
# from its source files. The 'make clean' removes the already built files
# and the 'make' rebuilt them again.
# It is needed to use clean parameter first if you want to make a clean build
# from the beginning.
alias remake 'make clean; make'
remake

# Prints the aliases list
alias

# Removes aliases with 'unalias'
unalias reload
unalias remake

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

# Sets the pager with selection order as parameters
alias selectpager 'setenv PAGER=`which \!:1` || setenv PAGER=`which \!:2`'
selectpager less most

# --- Recursion method example ---
# You can use this example as template for your scripts

# --- begin ---
#!/bin/tcsh -f
# set todo to default action
set todo = help
# if parameters have been passed, use the parameter
if ( $#argv > 0 ) set todo = $argv[1]
# execute
switch ( $todo )
case option1:
	echo "do stuff for option1"
	$0 results ...
	breaksw
case option2:
	echo "do stuff for option2"
	$0 results ...
	breaksw
case results:
	echo "print the results here"
	exit 0
help:
	echo "usage: $0 { help | option1 | option2 }"
	breaksw
default:
	echo "error, unknown parameter: $todo"
	exit 1
endsw
# --- end ---

# --- Completion --------------------------------------------------------------

# first get the latest package of the tcsh's community with a big list of
# command's completions, and put it in the  ~/.tcshrc
cd
wget https://raw.githubusercontent.com/tcsh-org/tcsh/master/complete.tcsh
install -m 0644 -o root -g root complete.tcsh /etc
echo 'source /etc/complete.tcsh' >> ~/.tcshrc
# reload the tcshrc
source ~/.tcshrc

# is it annoying for some commands? lets say for the cp, mv and rm...
# remove them...
uncomplete {cp,mv,rm}

# lets build a completion rule for tmux
set tmux_cmds = `tmux list-commands | awk '{print$1}'`
uncomplete tmux
complete tmux "p/1/(${tmux_cmds})/"

# ready, now type 'tmux ' and press the [TAB], it will display a big list
# with all the parameters of tmux.

# TCSH has the most advanced completion subsystem in the UNIX world.
# Use the man, it is your friend!
# Read the /etc/complete.tcsh it is your tutorial.

# --- Directory Stack ---------------------------------------------------------
# pushd, popd, and dirs are shell built-ins which allow you manipulate a 
# directory stack. This can be used to change directories but return to the
# directory from which you came.

# push directory in stack and changes to it
pushd /etc
# again
pushd /var/spool/news
# prints the directory stack:
# 0	/var/spool/news
# 1	/etc
dirs -v

# now we can move to one of those directories or using their names in other
# commands with the sequence character = and the number as it displayed by
# `dirs'

# this will copy the file /etc/termcap to the current directory
cp =1/termcap .
# this will move us back to /etc
cd =1
# and this will remove the top directory from the stack and move us to the next
# which is in our example, the `/etc/'
popd
# `dirs' now prints:
# 0 /etc
dirs -v

# this ability was so famous that copied to all other shells. I really had big
# problem to use this; because when I was needed a pre-previous directory,
# always I had been forgot to use the pushd.

# But the tcsh is so powerful, that I solved it easily with aliases

# 1st, I don't want the same directory more than one time in the stack
set dunique
# 2nd, do not bother me with stack's messages
set pushdsilent
# 3rd, when I use pushd without parameters I demand to go to my $HOME,
# exactly as cd does.
set pushdtohome

# Finally I want to use always pushd instead of cd
alias cd 'pushd'
# and dirs to display the directories vertical with their numbers
alias dirs 'dirs -v'
# the standard cd command can be accessed with its long name: `chdir'

# the popd can remove any directory in the stack without change to next.
pushd /a; pushd /b; pushd /c
# it will print:
# 0 /c
# 1 /b
# 2 /a
dirs -v
# this will remove the #2 directory:
popd +2

# In the man-page you ll find more tools to play with directory stack...

# --- Job Control -------------------------------------------------------------
TODO

# --- History -----------------------------------------------------------------
TODO

# --- Terminal manipulation ---------------------------------------------------
TODO

# --- other built-in commands -------------------------------------------------

# prints all built-in commands in alphabetical order
set bins = `builtins`
echo $bins > syntax-highlight-keywords.conf

# bindkey is used to configure our keyboard key.
# let's fix the key-code of [END] key on urxvt terminal emulator
if ( $TERM == "rxvt-unicode" ) then
	bindkey "^[[8~" end-of-line
endif

# `umask' defines the default permissions of newly created file
# 022 means octal mode 0755 rwxr-xr-x (0644 rw-r--r-- for simple files) 
umask 022

# --- examples ----------------------------------------------------------------

# this script prints available power-states if no argument is set;
# otherwise it set the state of the $argv[1]
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
		break
	else
		if ( $secret > $guess ) echo "its greater"
		if ( $secret < $guess ) echo "its lesser"
	endif
end
# --- secretnum.csh --- end ---------------------------------------------------

# Advises:
# 1. Do not use redirection in single-line if (it is well documented bug)
#    In most cases avoid to use single-line IFs.
# 2. Do not mess up with other shells code, c-shell is not compatible with
#    other shells and has different abilities and priorities.
# 3. Use spaces as you'll use them to write readable code in any language.
#    It is not so well documented (never was) that it requires spaces
#    (separators) between everything except those that constitute an element.
#    A bug of csh was `set x=1' worked, `set x = 1' worked, `set x =1' did not!
#    A bug of tcsh was `if ( ! $x )' worked but `if (! $x ) did not.
# 4. It is well documented that numeric expressions require spaces in-between;
#    also parenthesise all bit-wise and unary operators.
# 5. Do not write a huge weird expression with several quotes, backslashes etc
#    It is bad practice for generic programming, it is dangerous in any shell.
# 6  Use quotes, it will save you many times.
# 7. Help tcsh, report the bug here <https://bugs.gw.com/>
# 8. Read the man page, `tcsh' has a huge list of options, and variables.
#
#    I suggest the following options enabled by default
#    --------------------------------------------------
# Even in non-interactive shells
#    set echo_style=both
#    set backslash_quote
#    set parseoctal
#    unset noclobber
#
# These options are not necessary but I suggest to begin using tcsh with them
#
# enable:
#    set inputmode=insert
#    set autolist
#    set listjobs
#    set padhour
#    set color
#    set colorcat
#    set nobeep
#    set cdtohome
#    set ellipsis
#
#    set histdup
#    set histlit
#    set nohistclop
#
# disable:
#    unset compat_expr
#    unset noglob
#    unset autologout
#    unset time
#    unset tperiod
#
# NOTE: If the `backslash_quote' is set, it may create compatibility issues
# with other tcsh scripts which was written without it.
#
# NOTE: The same for `parseoctal', but it is better to fix the problematic
# scripts.
#
# NOTE: **for beginners only**
# This enable automatically rescan `path' directories if need to. (like bash)
#    set autorehash

#### common aliases
#    alias hist  'history 20'
#    alias ll    'ls --color -lha'
#    alias today "date '+%d%h%y'"
#    alias ff    'find . -name '

#### a nice prompt
#    set prompt = "%B%{\033[35m%}%t %{\033[32m%}%n@%m%b %C4 %# "

#### debugging a script
# TCSH options:
# -v Display commands before executing them; expand history substitutions, but
#    not other substitutions (e.g., filename, variable, and command).
# -V as -v, but also display .tcshrc
# -x Display commands before executing them; expand all substitutions.
# -X as -x, but also display .tcshrc
# -n parse commands, but do not execute them.

# example
tcsh -v script
```

### Numeric expression replacement benchmarks
- tcsh '@' native loop, integer only = 0.09 sec
- tcsh '@' external, integer only = 8.20 sec
- each test executed at least 2 times to ensure that the program is loaded in cache

#### the script for the tests
```tcsh
#!/bin/tcsh -f
set count = 1000
while ( $count )
	echo '1.0/2.0' | program > /dev/null
	@ count --
end
```
---

#### Results

| Application | Seconds |     ALT | Comments                   |
| ----------- | -------:| -------:| -------------------------- |
| calc        |    1.62 |    1.52 |                            |
| bc          |    1.80 |         |                            |
| sbasic      |    1.96 |         | SmallBASIC console-version |
| bas         |    3.18 |         | bas 2.4 (Michael Haardt)   |
| perl        |    4.98 |         |                            |
| awk         |         |    5.02 | GNU version                |
| python-2.7  |   18.05 |         |                            |

ALT means alternative method.
**calc** can be used with block-quotes and **awk** takes parameter the code.

### WARNING: Pipe-lines at Windows
Anonymous pipe mechanism has Windows too, but it is buggy and I sign it for all
versions until Windows XP SP3 API32 - and for all of their examples in MSDN -
which was the last one that I worked on. Microsoft still denied it but
is well known bug since it is a common method for inter-process communication.
For small I/O it will work well.
 
### Further Readings
- [TCSH Home](http://www.tcsh.org/)
- [TCSH Wikipedia](https://en.wikipedia.org/wiki/Tcsh)
- [TCSH manual page](http://www.tcsh.org/tcsh.html/top.html)
- [“An Introduction to the C shell”, William Joy](https://docs.freebsd.org/44doc/usd/04.csh/paper.html)
- [TCSH Bug reports and/or features requests](https://bugs.gw.com/)
- [Writing Aliases in csh and tcsh](http://home.adelphi.edu/sbloch/class/archive/271/fall2005/notes/aliases.html)

### Some more files:
- [tcsh help command (for 132x35 terminal size)](https://github.com/nereusx/dotfiles/blob/master/csh-help),
- [my ~/.tcshrc](https://github.com/nereusx/dotfiles/blob/master/.tcshrc)
- [tcsh-mode for JED text editor](https://github.com/nereusx/dotfiles/blob/master/.jed/tcsh-mode.sl?ts=4)
- [tcsh-mode for NANO text editor](https://github.com/nereusx/nanorc/blob/master/csh.nanorc?ts=4)
- [tcsh-mode for EMACS text editor](https://github.com/tcsh-org/tcsh/blob/master/csh-mode.el)
