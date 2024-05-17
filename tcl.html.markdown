---
language: Tcl
contributors:
    - ["Poor Yorick", "https://pooryorick.com/"]
filename: learntcl.tcl
---

Tcl was created by [John Ousterhout](https://wiki.tcl-lang.org/page/John+Ousterhout) as a
reusable scripting language for circuit design tools that he authored.  In 1997 he
was awarded the [ACM Software System
Award](https://en.wikipedia.org/wiki/ACM_Software_System_Award) for Tcl.   Tcl
can be used both as an embeddable scripting language and as a general
programming language.  It can also be used as a portable C library, even in
cases where no scripting capability is needed, as it provides data structures
such as dynamic strings, lists, and hash tables.  The C library also provides
portable functionality for loading dynamic libraries, string formatting and
code conversion, filesystem operations, network operations, and more.  Various
features of Tcl stand out:

* Convenient cross-platform networking API

* Fully virtualized filesystem

* Stackable I/O channels

* Asynchronous to the core

* Full coroutines

* A threading model recognized as robust and easy to use


Tcl has much in common with Lisp, but instead of lists, Tcl uses strings as the
currency of the language.  All values are strings.  A list is a string with a
defined format, and the body of a procedure (a script) is also a string rather
than a block.  To achieve performance, Tcl internally caches structured
representations of these values.  list routines, for example, operate on
the internal cached representation, and Tcl takes care of updating the string
representation if it is ever actually needed in the script.  The copy-on-write
design of Tcl allows script authors to pass around large data values without
actually incurring additional memory overhead.  Procedures are automatically
byte-compiled unless they use the more dynamic routines such as "uplevel",
"upvar", and "trace".

Tcl is a pleasure to program in.  It will appeal to hacker types who find Lisp,
Forth, or Smalltalk interesting, as well as to engineers and scientists who
just want to get down to business with a tool that bends to their will.  Its
discipline of exposing all programmatic functionality as routines, including
things like looping and mathematical operations that are usually baked into the
syntax of other languages, allows it to fade into the background of whatever
domain-specific functionality a project needs. Its syntax, which is even
lighter than that of Lisp, just gets out of the way.



```tcl
#! /bin/env tclsh

###############################################################################
## 1. Guidelines
###############################################################################

# Tcl is not Sh or C!  This needs to be said because standard shell quoting
# habits almost work in Tcl and it is common for people to pick up Tcl and try
# to get by with syntax they know from another language.  It works at first,
# but soon leads to frustration when scripts become more complex.

# Braces are a quoting mechanism, not syntax for the construction of code
# blocks or lists. Tcl doesn't have either of those things.  Braces are used to
# escape special characters, which makes them well-suited for quoting procedure
# bodies and strings that should be interpreted as lists.


###############################################################################
## 2. Syntax
###############################################################################

# A script is made up of commands delimited by newlines or semicolons.  Each
# command is a call to a routine.  The first word is the name of a routine to
# call, and subsequent words are arguments to the routine.  Words are delimited
# by whitespace.  Since each argument is a word in the command it is already a
# string, and may be unquoted:
set part1 Sal
set part2 ut; set part3 ations


# a dollar sign introduces variable substitution:
set greeting $part1$part2$part3


# When "set" is given only the name of a variable, it returns the
# value of that variable:
set part3 ;# Returns the value of the variable.


# Left and right brackets embed a script to be evaluated for a result to
# substitute into the word:
set greeting $part1$part2[set part3]


# An embedded script may be composed of multiple commands, the last of which provides
# the result for the substitution:
set greeting $greeting[
    incr i
    incr i
    incr i
]
puts $greeting ;# The output is "Salutations3"

# Every word in a command is a string, including the name of the routine, so
# substitutions can be used on it as well. Given this variable
# assignment,
set action pu

# , the following three commands are equivalent:
puts $greeting
${action}ts $greeting 
[set action]ts $greeting


# backslash suppresses the special meaning of characters:
set amount \$16.42


# backslash adds special meaning to certain characters:
puts lots\nof\n\n\n\n\n\nnewlines


# A word enclosed in braces is not subject to any special interpretation or
# substitutions, except that a backslash before a brace is not counted when
# looking for the closing brace:
set somevar {
    This is a literal $ sign, and this \} escaped
    brace remains uninterpreted
}


# In a word enclosed in double quotes, whitespace characters lose their special
# meaning:
set name Neo
set greeting "Hello, $name"


# A variable name can be any string:
set {first name} New


# The braced form of variable substitution handles more complex variable names:
set greeting "Hello, ${first name}"


# "set" can always be used instead of variable substitution, and can handle all
# variable names:
set greeting "Hello, [set {first name}]"


# To unpack a list into the command, use the expansion operator, "{*}".  These
# two commands are equivalent:
set name Neo
set {*}{name Neo}


# An array is a special variable that is a container for other variables.
set person(name) Neo
set person(destiny) {The One}
set greeting "Hello, $person(name)"


# "variable" can be used to declare or set variables. In contrast with "set",
# which uses both the global namespace and the current namespace to resolve a
# variable name, "variable" uses only the current namespace:
variable name New


# "namespace eval" creates a new namespace if it doesn't exist.  A namespace
# can contain both routines and variables:
namespace eval people {
    namespace eval person1 {
        variable name Neo
    }
}


# Use two or more colons to delimit namespace components in variable names:
namespace eval people {
    set greeting "Hello $person1::name"
}

# Two or more colons also delimit namespace components in routine names:
proc people::person1::speak {} {
    puts {I am The One.}
}

# Fully-qualified names begin with two colons:
set greeting "Hello $::people::person1::name"



###############################################################################
## 3. No More Syntax
###############################################################################

# All other functionality is implemented via routines.  From this point on,
# there is no new syntax.  Everything else there is to learn about
# Tcl is about the behaviour of individual routines and what meaning they
# assign to their arguments.



###############################################################################
## 4. Variables and Namespaces
###############################################################################

# Each variable and routine is associated with some namespace.

# To end up with an interpreter that can do nothing, delete the global
# namespace.  It's not very useful to do such a thing, but it illustrates the
# nature of Tcl.  The name of the global namespace is actually the empty
# string, but the only way to represent it is as a fully-qualified name. To
# try it out call this routine:
proc delete_global_namespace {} {
    namespace delete ::
}

# Because "set" always keeps its eye on both the global namespace and the
# current namespace, it's safer to use "variable" to declare a variable or
# assign a value to a variable.  If a variable called "name" already exists in
# the global namespace, using "set" here will assign a value to the global
# variable instead of to a variable in the current namespace, whereas
# "variable" operates only on the current namespace.
namespace eval people {
    namespace eval person1 {
        variable name Neo
    }
}

# Once a variable is declared in a namespace, [set] sees it instead of seeing
# an identically-named variable in the global namespace:
namespace eval people {
    namespace eval person1 {
        variable name
        set name Neo
    }
}

# But if "set" has to create a new variable, it always does it relative to the
# current namespace:
unset name
namespace eval people {
    namespace eval person1 {
        set name neo
    }

}
set people::person1::name


# An absolute name always begins with the name of the global namespace (the
# empty string), followed by two colons:
set ::people::person1::name Neo


# Within a procedure, the "variable" links a variable in the current namespace
# into the local scope:
namespace eval people::person1 {
    proc fly {} {
        variable name
        puts "$name is flying!"
    }
}




###############################################################################
## 5. Built-in Routines
###############################################################################

# Math can be done with the "expr":
set a 3
set b 4
set c [expr {$a + $b}]

# Since "expr" performs variable substitution on its own, brace the expression
# to prevent Tcl from performing variable substitution first.  See
# "https://wiki.tcl-lang.org/page/Brace+your+expr-essions" for details.


# "expr" understands variable and script substitution:
set c [expr {$a + [set b]}]


# "expr" provides a set of mathematical functions:
set c [expr {pow($a,$b)}]


# Mathematical operators are available as routines in the ::tcl::mathop
# namespace:
::tcl::mathop::+ 5 3

# Routines can be imported from other namespaces:
namespace import ::tcl::mathop::+
set result [+ 5 3]


# Non-numeric values must be quoted, and operators like "eq" can be used to
# constrain the operation to string comparison:
set name Neo
expr {{Bob} eq $name}

# The general operators fall back to string comparison if numeric
# operation isn't feasible:
expr {{Bob} == $name}


# "proc" creates new routines:
proc greet name {
    return "Hello, $name!"
}

#multiple parameters can be specified:
proc greet {greeting name} {
    return "$greeting, $name!"
}


# As noted earlier, braces do not construct a code block.  Every value, even
# the third argument to "proc", is a string.  The previous command
# can be rewritten using no braces:
proc greet greeting\ name return\ \"\$greeting,\ \$name!\"
# "



# When the last parameter is the literal value "args", all extra arguments
# passed to the routine are collected into a list and assigned to "args":
proc fold {cmd first args} {
    foreach arg $args {
        set first [$cmd $first $arg]
    }
    return $first
}
fold ::tcl::mathop::* 5 3 3 ;# ->  45


# Conditional execution is implemented as a routine:
if {3 > 4} {
    puts {This will never happen}
} elseif {4 > 4} {
    puts {This will also never happen}
} else {
    puts {This will always happen}
}


# Loops are implemented as routines.  The first and third arguments to 
# "for" are treated as scripts, while the second argument is treated as
# an expression:
set res 0
for {set i 0} {$i < 10} {incr i} {
    set res [expr {$res + $i}]
}
unset res


# The first argument to "while" is also treated as an expression:
set i 0
while {$i < 10} {
    incr i 2
}


# A list is a string, and items in the list are delimited by whitespace:
set amounts 10\ 33\ 18
set amount [lindex $amounts 1]

# Whitespace in a list item must be quoted:
set inventory {"item 1" item\ 2 {item 3}}


# It's generally a better idea to use list routines when modifying lists:
lappend inventory {item 1} {item 2} {item 3}


# Braces and backslash can be used to format more complex values in a list.  A
# list looks exactly like a script, except that the newline character and the
# semicolon character lose their special meanings, and there is no script or
# variable substitution.  This feature makes Tcl homoiconic.  There are three
# items in the following list:
set values {

    one\ two

    {three four}

    five\{six

}


# Since, like all values, a list is a string, string operations could be
# performed on it, at the risk of corrupting the formatting of the list:
set values {one two three four}
set values [string map {two \{} $values] ;# $values is no-longer a \
    properly-formatted list


# The sure-fire way to get a properly-formatted list is to use "list" routines:
set values [list one \{ three four]
lappend values { } ;# add a single space as an item in the list


# Use "eval" to evaluate a value as a script:
eval {
    set name Neo
    set greeting "Hello, $name"
}


# A list can always be passed to "eval" as a script composed of a single
# command:
eval {set name Neo}
eval [list set greeting "Hello, $name"]


# Therefore, when using "eval", use "list" to build up the desired command:
set command {set name}
lappend command {Archibald Sorbisol}
eval $command


# A common mistake is not to use list functions when building up a command:
set command {set name}
append command { Archibald Sorbisol}
try {
    eval $command ;# The error here is that there are too many arguments \
        to "set" in {set name Archibald Sorbisol}
} on error {result eoptions} {
    puts [list {received an error} $result]
}

# This mistake can easily occur with "subst":

set replacement {Archibald Sorbisol}
set command {set name $replacement}
set command [subst $command] 
try {
    eval $command ;# The same error as before:  too many arguments to "set" in \
        {set name Archibald Sorbisol}
} trap {TCL WRONGARGS} {result options} {
    puts [list {received another error} $result]
}


# "list" correctly formats a value for substitution:
set replacement [list {Archibald Sorbisol}]
set command {set name $replacement}
set command [subst $command]
eval $command


# "list" is commonly used to format values for substitution into scripts: There
# are several examples of this, below.


# "apply" evaluates a two-item list as a routine:
set cmd {{greeting name} {
    return "$greeting, $name!"
}}
apply $cmd Whaddup Neo

# A third item can be used to specify the namespace to apply the routine in:
set cmd [list {greeting name} {
    return "$greeting, $name!"
} [namespace current]]
apply $cmd Whaddup Neo


# "uplevel" evaluates a script at some higher level in the call stack:
proc greet {} {
    uplevel {puts "$greeting, $name"}
}

proc set_double {varname value} {
    if {[string is double $value]} {
        uplevel [list variable $varname $value]
    } else {
        error [list {not a double} $value]
    }
}


# "upvar" links a variable at the current level in the call stack to a variable
# at some higher level:
proc set_double {varname value} {
    if {[string is double $value]} {
        upvar 1 $varname var
        set var $value
    } else {
        error [list {not a double} $value]
    }
}


# Get rid of the built-in "while" routine, and use "proc" to define a new one:
rename ::while {}
# handling is left as an exercise:
proc while {condition script} {
    if {[uplevel 1 [list expr $condition]]} {
        uplevel 1 $script
        tailcall [namespace which while] $condition $script
    }
}


# "coroutine" creates a new call stack, a new routine to enter that call stack,
# and then calls that routine.  "yield" suspends evaluation in that stack and
# returns control to the calling stack:
proc countdown count {
    # send something back to the creator of the coroutine, effectively pausing
    # this call stack for the time being.
    yield [info coroutine]

    while {$count > 1} {
        yield [incr count -1]
    }
    return 0
}
coroutine countdown1 countdown 3
coroutine countdown2 countdown 5
puts [countdown1] ;# -> 2 
puts [countdown2] ;# -> 4 
puts [countdown1] ;# -> 1 
puts [countdown1] ;# -> 0 
catch {
    puts [coundown1] ;# -> invalid command name "countdown1"
} cres copts 
puts $cres
puts [countdown2] ;# -> 3 


# Coroutine stacks can yield control to each other:

proc pass {whom args} {
    return [yieldto $whom {*}$args]
}

coroutine a apply {{} {
        yield
        set result [pass b {please pass the salt}]
        puts [list got the $result]
        set result [pass b {please pass the pepper}]
        puts [list got the $result]
}}

coroutine b apply {{} {
    set request [yield]
    while 1 {
        set response [pass c $request]
        puts [list [info coroutine] is now yielding]
        set request [pass a $response]
    }
}}

coroutine c apply {{} {
    set request [yield]
    while 1 {
        if {[string match *salt* $request]} {
            set request [pass b salt]
        } else {
            set request [pass b huh?]
        }
    }
}}

# get things moving
a
```

## Reference

[Official Tcl Documentation](https://www.tcl-lang.org)

[Tcl Wiki](https://wiki.tcl-lang.org)

[Tcl Subreddit](http://www.reddit.com/r/Tcl)
