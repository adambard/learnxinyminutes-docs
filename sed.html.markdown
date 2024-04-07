---
category: tool
tool: sed
filename: learnsed.sed
contributors:
     - ["Diomidis Spinellis", "https://www.spinellis.gr"]

---

__Sed__ is a standard tool on every POSIX-compliant UNIX system.
It's like an editor, such as Vim, Visual Studio Code, Atom, or Sublime.
However, rather than typing the commands interactively, you
provide them on the command line or in a file.

_Sed_'s advantages over an interactive editor is that it can be easily
used to automate text processing tasks, and that it can process
efficiently huge (terabyte-sized) files.
It can perform more complex tasks than _grep_ and for many text
processing tasks its commands are much shorter than what you would
write in _awk_, _Perl_, or _Python_.

_Sed_ works by reading a line of text (by default from its standard
input, unless some files are specified as arguments), processing
it with the specified commands, and then outputting the result
on its standard output.
You can suppress the default output by specifying the `-n` command-line
argument.

```perl
#!/usr/bin/sed -f
# Files that begin with the above line and are given execute permission
# can be run as regular scripts.

# Comments are like this.

# Commands consist of a single letter and many can be preceded
# by a specification of the lines to which they apply.

# Delete the input's third line.
3d

# The same command specified the command line as an argument to sed:
# sed 3d

# For many commands the specification can consist of two addresses,
# which select an inclusive range.
# Addresses can be specified numerically ($ is the last line) or through
# regular expressions delimited by /.

# Delete lines 1-10
1,10d

# Lines can also be specified as regular expressions, delimited by /.

# Delete empty lines.
/^$/d

# Delete blocks starting with SPOILER-BEGIN and ending with SPOILER-END.
/SPOILER-BEGIN/,/SPOILER-END/d

# A command without an address is applied to all lines.

# List lines in in a visually unambiguous form (e.g. tab appears as \t).
l

# A command prefixed by ! will apply to non-matching lines.
# Keep only lines starting with a #.
/^#/!d

# Below are examples of the most often-used commands.

# Substitute the first occurence in a line of John with Mary.
s/John/Mary/

# Remove all underscore characters (global substitution).
s/_//g

# Remove all HTML tags.
s/<[^>]*>//g

# In the replacement string & is the regular expression matched.

# Put each line inside double quotes.
s/.*/"&"/

# In the matched regular expression \(pattern\) is used to store
# a pattern into a buffer.
# In the replacement string \1 refers to the first pattern, \2 to the second
# and so on. \u converts the following character to uppercase \l to lowercase.

# Convert snake_case_identifiers into camelCaseIdentifiers.
s/_\(.\)/\u\1/g


# The p (print) command is typically used together with the -n
# command-line option, which disables the print by default functionality.
# Output all lines between ``` and ```.
/```/,/```/p


# The y command maps characters from one set to another.
# Swap decimal and thousand separators (1,234,343.55 becomes 1.234.343,55).
y/.,/,./

# Quit after printing the line starting with END.
/^END/q

# You can stop reading here, and still get 80% of sed's benefits.
# Below are examples of how you can specify multiple sed commands.

# You can apply multiple commands by separating them with a newline or
# a semicolon.

# Delete the first and the last line.
1d
$d

# Delete the first and the last line.
1d;$d


# You can group commands in { } blocks.

# Convert first line to uppercase and print it.
1 {
  s/./\u&/g
  p
}

# Convert first line to uppercase and print it (less readable one-liner).
1{s/./\u&/g;p;}


# You can also stop reading here, if you're not interested in creating
# sed script files.

# Below are more advanced commands.  You typically put these in a file
# rather than specify them on a command line.  If you have to use
# many of these commands in a script, consider using a general purpose
# scripting language, such as Python or Perl.

# Append a line containing "profile();" after each line ending with ";".
/;$/a\
profile();

# Insert a line containing "profile();" before each line ending with ";".
/;$/i\
profile();

# Change each line text inside REDACTED blocks into [REDACTED].
/REDACTED-BEGIN/,/REDACTED-END/c\
[REDACTED]

# Replace the tag "<ourstyle>" by reading and outputting the file style.css.
/<ourstyle>/ {
  r style.css
  d
}

# Change each line inside REDACTED blocks into [REDACTED].
# Also write (append) a copy of the redacted text in the file redacted.txt.
/REDACTED-BEGIN/,/REDACTED-END/ {
  w redacted.txt
  c\
  [REDACTED]
}

# All operations described so far operate on a buffer called "pattern space".
# In addition, sed offers another buffer called "hold space".
# The following commands operate on the two, and can be used to keep
# state or combine multiple lines.

# Replace the contents of the pattern space with the contents of
# the hold space.
g

# Append a newline character followed by the contents of the hold
# space to the pattern space.
G

# Replace the contents of the hold space with the contents of the
# pattern space.
h

# Append a newline character followed by the contents of the
# pattern space to the hold space.
H

# Delete the initial segment of the pattern space through the first
# newline character and start the next cycle.
D

# Replace the contents of the pattern space with the contents of
# the hold space.
g

# Append a newline character followed by the contents of the hold
# space to the pattern space.
G

# Replace the contents of the hold space with the contents of the
# pattern space.
h

# Append a newline character followed by the contents of the
# pattern space to the hold space.
H

# Write the pattern space to the standard output if the default
# output has not been suppressed, and replace the pattern space
# with the next line of input.
n

# Append the next line of input to the pattern space, using an
# embedded newline character to separate the appended material from
# the original contents.  Note that the current line number
# changes.
N

# Write the pattern space, up to the first newline character to the
# standard output.
P

# Swap the contents of the pattern and hold spaces.
x

# Here is a complete example of some of the buffer commands.
# Move the file's first line to its end.
1 {
  h
  d
}

$ {
  p
  x
}


# Three sed commands influence a script's control flow

# Name this script position "my_label", to which the "b" and
# "t" commands may branch.
:my_label

# Continue executing commands from the position of my_label.
b my_label

# Branch to the end of the script.
b

# Branch to my_label if any substitutions have been made since the most
# recent reading of an input line or execution of a "t" (test) function.
t my_label

# Here is a complete example of branching
# Join lines that end with a backspace into a single space-separated one

# Name this position "loop"
: loop
# On lines ending with a backslash
/\\$/ {
  # Read the next line and append it to the pattern space
  N
  # Substitute backslash newline with a space
  s/\\\n/ /
  # Branch to the top for testing this line's ending
  b loop
}
```

Further Reading:

* [The Open Group: sed - stream editor](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/sed.html)
  The POSIX standard regarding sed.
  Follow this for maximum portability.
* [FreeBSD sed -- stream editor](https://www.freebsd.org/cgi/man.cgi?query=sed&sektion=&n=1)
  The BSD manual page.
  This version of sed runs on BSD systems and macOS.
* [Project GNU: sed, a stream editor](https://www.gnu.org/software/sed/manual/sed.html)
  The GNU manual page. GNU sed is found on most Linux systems.
* [Lee E. McMahon: SED -- A Non-interactive Text Editor](https://wolfram.schneider.org/bsd/7thEdManVol2/sed/sed.pdf)
  The original sed documentation
* [A collection of sed resources](http://sed.sourceforge.net/)
* [The sed FAQ](http://sed.sourceforge.net/sedfaq.html)
