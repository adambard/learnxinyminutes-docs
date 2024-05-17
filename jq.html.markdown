---
category: tool
tool: jq
contributors:
    - ["Jack Kuan", "https://github.com/kjkuan"]
filename: learnjq.sh
---

`jq` is a tool for transforming JSON inputs and generating JSON outputs. As a
programming language, jq supports boolean and arithmetic expressions, object
and array indexing; it has conditionals, functions, and even exception
handling... etc.  Knowing jq enables you to easily write small programs that
can perform complex queries on JSON documents to find answers, make reports, or
to produce another JSON document for further processing by other programs.

> **NOTE**: This guide demonstrates the use of jq from the command line,
> specifically, under an environment running the Bash shell.

```bash
# When running jq from the command line, jq program code can be specified as the
# first argument after any options to `jq`. We often quote such jq program with
# single quotes (`'`) to prevent any special interpretation from the command line
# shell.
#
jq -n '# Comments start with # until the end of line.
       # The -n option sets the input to the value, `null`, and prevents `jq`
       # from reading inputs from external sources.
'

# Output:
# null


# By default jq reads from *STDIN* a stream of JSON inputs (values). It
# processes each input with the jq program (filters) specified at the command
# line, and prints the outputs of processing each input with the program to
# *STDOUT*.
#
echo '
  "hello" 123 [
    "one",
    "two",
    "three"
  ]
  { "name": "jq" }
' |
 jq '.  # <-- the jq program here is the single dot (.), called the identity
        # operator, which stands for the current input.
'

# Output:
# "hello"
# 123
# [
#   "one",
#   "two",
#   "three"
# ]
# {
#   "name": "jq"
# }


# Notice that jq pretty-prints the outputs by default, therefore, piping
# to `jq` is a simple way to format a response from some REST API endpoint
# that returns JSON. E.g., `curl -s https://freegeoip.app/json/ | jq`


# Instead of processing each JSON input with a jq program, you can also
# ask jq to slurp them up as an array.
#
echo '1 "two" 3' | jq -s .

# Output:
# [
#   1,
#   "two",
#   3
# ]


# Or, treat each line as a string.
#
(echo line 1; echo line 2) | jq -R .

# Output:
# "line 1"
# "line 2"


# Or, combine -s and -R to slurp the input lines into a single string.
#
(echo line 1; echo line 2) | jq -sR .

# Output:
# "line 1\nline2\n"


# Inputs can also come from a JSON file specified at the command line:
#
echo '"hello"' > hello.json
jq . hello.json

# Output:
# "hello"


# Passing a value into a jq program can be done with the `--arg` option.
# Below, `val` is the variable name to bind the value, `123`, to.
# The variable is then referenced as `$val`.
#
jq -n --arg val 123 '$val'  # $val is the string "123" here

# Output:
# "123"


# If you need to pass a JSON value, use `--argjson`
#
jq -n --argjson val 123 '$val'  # $val is a number

# Output:
# 123


# Using `--arg` or `--argjson` is an useful way of building JSON output from
# existing input.
#
jq --arg text "$(date; echo "Have a nice day!")" -n '{ "today": $text }'

# Output:
# {
#   "today": "Sun Apr 10 09:53:07 PM EDT 2022\nHave a nice day!"
# }


# Instead of outputting values as JSON, you can use the `-r` option to print
# string values unquoted / unescaped. Non-string values are still printed as
# JSON.
#
echo '"hello" 2 [1, "two", null] {}' | jq -r .

# Output:
# hello
# 2
# [
#   1,
#   "two",
#   null
# ]
# {}


# Inside a string in jq, `\(expr)` can be used to substitute the output of
# `expr` into the surrounding string context.
#
jq -rn '"1 + 2 = \(1+2)"'

# Output:
# 1 + 2 = 3


# The `-r` option is most useful for generating text outputs to be processed
# down in a shell pipeline, especially when combined with an intepolated
# string that is prefixed the `@sh` prefix operator.
#
# The `@sh` operator escapes the outputs of `\(...)` inside a string with
# single quotes so that each resulting string of `\(...)` can be evaluated
# by the shell as a single word / token / argument without special
# interpretations.
#
env_vars=$(
    echo '{"var1": "value one", "var2": "value\ntwo"}' \
     |
    jq -r '
      "export " + @sh "var1=\(.var1) var2=\(.var2)"
      #                     ^^^^^^^^      ^^^^^^^^
      #                  "'value one'"  "'value\ntwo'"
      #
      # NOTE: The + (plus) operator here concatenates strings.
    '
)
echo "$env_vars"
eval "$env_vars"
declare -p var1 var2

# Output:
# export var1='value one' var2='value
# two'
# declare -- var1="value one"
# declare -- var2="value
# two"

# There are other string `@prefix` operators (e.g., @base64, @uri, @csv, ...)
# that might be useful to you. See `man jq` for details.


# The comma (`,`) operator in jq evaluates each operand and generates multiple
# outputs:
#
jq -n '"one", 2, ["three"], {"four": 4}'

# Output:
# "one"
# 2
# [
#   "three"
# ]
# {
#   "four": 4
# }


# Any JSON value is a valid jq expression that evaluates to the JSON value
# itself.
#
jq -n '1, "one", [1, 2], {"one": 1}, null, true, false'

# Output:
# 1
# "one"
# [
#   1,
#   2
# ]
# {
#   "one": 1
# }
# null
# true
# false


# Any jq expression can be used where a JSON value is expected, even as object
# keys. (though parenthesis might be required for object keys or values)
#
jq -n '[2*3, 8-1, 16/2], {("tw" + "o"): (1 + 1)}'

# Output:
# [
#   6,
#   7,
#   8
# ]
# {
#   "two": 2
# }


# As a shortcut, if a JSON object key looks like a valid identifier (matching
# the regex `^[a-zA-Z_][a-zA-Z_0-9]*$`), quotes can be omitted.
#
jq -n '{ key_1: "value1" }'

# If a JSON object's key's value is ommited, it is looked up in the current
# input using the key: (see next example for the meaning of `... | ...`)
#
jq -n '{c: 3} | {a: 1, "b", c}'

# Output:
# {
#   "a": 1,
#   "b": null,
#   "c": 3
# }


# jq programs are more commonly written as a series of expressions (filters)
# connected by the pipe (`|`) operator, which makes the output of its left
# filter the input to its right filter.
#
jq -n '1 | . + 2 | . + 3'  # first dot is 1; second dot is 3

# Output:
# 6

# If an expression evaluates to multiple outputs, then jq will iterate through
# them and propagate each output down the pipeline, and generate multiple
# outputs in the end.
#
jq -n '1, 2, 3 | ., 4 | .'

# Output:
# 1
# 4
# 2
# 4
# 3
# 4

# The flows of the data in the last example can be visualized like this:
# (number prefixed with `*` indicates the current output)
#
# *1,  2,  3 | *1,  4 | *1
#  1,  2,  3 |  1, *4 | *4
#  1, *2,  3 | *2,  4 | *2
#  1,  2,  3 |  2, *4 | *4
#  1,  2, *3 | *3,  4 | *3
#  1,  2,  3 |  3, *4 | *4
#
#
# To put it another way, the evaluation of the above example is very similar
# to the following pieces of code in other programming languages:
#
# In Python:
#
#   for first_dot in 1, 2, 3:
#       for second_dot in first_dot, 4:
#           print(second_dot)
#
# In Ruby:
#
#   [1, 2, 3].each do |dot|
#     [dot, 4].each { |dot| puts dot }
#   end
#
# In Javascript:
#
#   [1, 2, 3].forEach(dot => {
#       [dot, 4].forEach(dot => console.log(dot))
#   })
#


# Below are some examples of array index and object attribute lookups using
# the `[expr]` operator after an expression. If `expr` is a number then it's
# an array index lookup; otherwise, it should be a string, in which case it's
# an object attribute lookup:

# Array index lookup
#
jq -n '[2, {"four": 4}, 6][1 - 1]' # => 2
jq -n '[2, {"four": 4}, 6][0]'     # => 2
jq -n '[2, {"four": 4}, 6] | .[0]' # => 2

# You can chain the lookups since they are just expressions.
#
jq -n '[2, {"four": 4}, 6][1]["fo" + "ur"]' # => 4

# For object attributes, you can also use the `.key` shortcut.
#
jq -n '[2, {"four": 4}, 6][1].four'  # => 4

# Use `."key"` if the key is not a valid identifier.
#
jq -n '[2, {"f o u r": 4}, 6][1]."f o u r"' # => 4

# Array index lookup returns null if the index is not found.
#
jq -n '[2, {"four": 4}, 6][99]' # => null

# Object attribute lookup returns null if the key is not found.
#
jq -n '[2, {"four": 4}, 6][1].whatever' # => null

# The alternative operator `//` can be used to provide a default
# value when the result of the left operand is either `null` or `false`.
#
jq -n '.unknown_key // 7' # => 7

# If the thing before the lookup operator (`[expr]`) is neither an array
# or an object, then you will get an error:
#
jq -n '123 | .[0]'     # => jq: error (at <unknown>): Cannot index number with number
jq -n '"abc" | .name'  # => jq: error (at <unknown>): Cannot index string with string "name"
jq -n '{"a": 97} | .[0]'    # => jq: error (at <unknown>): Cannot index object with number
jq -n '[89, 64] | .["key"]' # => jq: error (at <unknown>): Cannot index array with string "key"

# You can, however, append a `?` to a lookup to make jq return `empty`
# instead when such error happens.
#
jq -n '123 | .[0]?'    # no output since it's empty.
jq -n '"abc" | .name?' # no output since it's empty.

# The alternative operator (`//`) also works with `empty`:
#
jq -n '123 | .[0]? // 99'           # => 99
jq -n '"abc" | .name? // "unknown"' # => "unknown"

# NOTE: `empty` is actually a built-in function in jq.
# With the nested loop explanation we illustrated earlier before,
# `empty` is like the `continue` or the `next` keyword that skips
# the current iteration of the loop in some programming languages.


# Strings and arrays can be sliced with the same syntax (`[i:j]`, but no
# steppings) and semantic as found in the Python programming language:
#
#                0   1    2    3    4   5 ... infinite
#        array = ["a", "b", "c", "d"]
# -infinite ... -4  -3   -2   -1
#
jq -n '["Peter", "Jerry"][1]'            # => "Jerry"
jq -n '["Peter", "Jerry"][-1]'           # => "Jerry"
jq -n '["Peter", "Jerry", "Tom"][1:]'    # => ["Jerry", "Tom"]
jq -n '["Peter", "Jerry", "Tom"][:1+1]'  # => ["Peter", "Jerry"]
jq -n '["Peter", "Jerry", "Tom"][1:99]'  # => ["Jerry", "Tom"]


# If the lookup index or key is ommited then jq iterates through
# the collection, generating one output value from each iteration.
#
# These examples produce the same outputs.
#
echo 1 2 3 | jq .
jq -n '1, 2, 3'
jq -n '[1, 2, 3][]'
jq -n '{a: 1, b: 2, c: 3}[]'

# Output:
# 1
# 2
# 3


# You can build an array out of multiple outputs.
#
jq -n '{values: [{a: 1, b: 2, c: 3}[] | . * 2]}'

# Output:
# {
#   "values": [
#     2,
#     4,
#     6
#   ]
# }


# If multiple outputs are not contained, then we'd get multiple outputs
# in the end.
#
jq -n '{values: ({a: 1, b: 2, c: 3}[] | . * 2)}'

# Output:
# {
#   "values": 2
# }
# {
#   "values": 4
# }
# {
#   "values": 6
# }


# Conditional `if ... then ... else ... end` in jq is an expression, so
# both the `then` part and the `else` part are required. In jq, only
# two values, `null` and `false`, are false; all other values are true.
#
jq -n 'if 1 > 2 | not and 1 <= 2 then "Makes sense" else "WAT?!" end'

# Output
# "Makes sense"

# Notice that `not` is a built-in function that takes zero arguments,
# that's why it's used as a filter to negate its input value.
# We'll talk about functions soon.

# Another example using a conditional:
#
jq -n '1, 2, 3, 4, 5 | if . % 2 != 0 then . else empty end'

# Output
# 1
# 3
# 5

# The `empty` above is a built-in function that takes 0 arguments and
# generates no outputs. Let's see more examples of built-in functions.

# The above conditional example can be written using the `select/1` built-in
# function (`/1` indicates the number of arguments expected by the function).
#
jq -n '1, 2, 3, 4, 5 | select(. % 2 != 0)'  # NOTE: % gives the remainder.

# Output
# 1
# 3
# 5


# Function arguments in jq are passed with call-by-name semantic, which
# means, an argument is not evaulated at call site, but instead, is
# treated as a lambda expression with the calling context of the call
# site as its scope for variable and function references used in the
# expression.
#
# In the above example, the expression `. % 2 != 0` is what's passed to
# `select/1` as the argument, not `true` or `false`, which is what would
# have been the case had the (boolean) expression was evaluated before it's
# passed to the function.


# The `range/1`, `range/2`, and `range/3` built-in functions generate
# integers within a given range.
#
jq -n '[range(3)]'         # => [0, 1, 2]
jq -n '[range(0; 4)]'      # => [0, 1, 2, 3]
jq -n '[range(2; 10; 2)]'  # => [2, 4, 6, 8]

# Notice that `;` (semicolon) is used to separate function arguments.


# The `map/1` function applies a given expression to each element of
# the current input (array) and outputs a new array.
#
jq -n '[range(1; 6) | select(. % 2 != 0)] | map(. * 2)'

# Output:
# [
#   2,
#   6,
#   10
# ]

# Without using `select/1` and `map/1`, we could have also written the
# above example like this:
#
jq -n '[range(1; 6) | if . % 2 != 0 then . else empty end | . * 2]'


# `keys/0` returns an array of keys of the current input. For an object,
# these are the object's attribute names; for an array, these are the
# array indices.
#
jq -n '[range(2; 10; 2)] | keys'   # => [0, 1, 2, 3]
jq -n '{a: 1, b: 2, c: 3} | keys'  # => ["a", "b", "c"]

# `values/0` returns an array of values of the current input. For an object,
# these are the object's attribute values; for an array, these are the
# elements of the array.
#
jq -n '[range(2; 10; 2)] | values'   # => [2, 4, 6, 8]
jq -n '{a: 1, b: 2, c: 3} | values'  # => [1, 2, 3]


# `to_entries/0` returns an array of key-value objects of the current input
# object.
#
jq -n '{a: 1, b: 2, c: 3} | to_entries'

# Output:
# [
#   {
#     "key": "a",
#     "value": 1
#   },
#   {
#     "key": "b",
#     "value": 2
#   },
#   {
#     "key": "c",
#     "value": 3
#   }
# ]


# Here's how you can turn an object's attribute into environment variables
# using what we have learned so far.
#
env_vars=$(
    jq -rn '{var1: "1 2  3   4", var2: "line1\nline2\n"}
            | to_entries[]
            | "export " + @sh "\(.key)=\(.value)"
           '
)
eval "$env_vars"
declare -p var1 var2

# Output:
# declare -x var1="1 2  3   4"
# declare -x var2="line1
# line2
# "


# `from_entries/0` is the opposite of `to_entries/0` in that it takes an
# an array of key-value objects and turn that into an object with keys
# and values from the `key` and `value` attributes of the objects.
#
# It's useful together with `to_entries/0` when you need to iterate and
# do something to each attribute of an object.
#
jq -n '{a: 1, b: 2, c: 3} | to_entries | map(.value *= 2) | from_entries'

# Output:
# {
#   "a": 2,
#   "b": 4,
#   "c": 6
# }


# The example above can be further shortened with the  `with_entries/1` built-in:
#
jq -n '{a: 1, b: 2, c: 3} | with_entries(.value *= 2)'


# The `group_by/1` generates an array of groups (arrays) from the current
# input (array). The classification is done by applying the expression argument
# to each member of the input array.
#
# Let's look at a contrived example (Note that `tostring`, `tonumber`,
# `length` and `max` are all built-in jq functions. Feel free to look
# them up in the jq manual):
#
# Generate some random numbers.
numbers=$(echo $RANDOM{,,,,,,,,,,,,,,,,,,,,})
#
# Feed the numbers to jq, classifying them into groups and calculating their
# averages, and finally generate a report.
#
echo $numbers | jq -rs '  # Slurp the numbers into an array.
[
  [ map(tostring)          # Turn it into an array of strings.
    | group_by(.[0:1])     # Group the numbers by their first digits.
    | .[]                  # Iterate through the array of arrays (groups).
    | map(tonumber)        # Turn each group back to an array of numbers.
  ] # Finally, contain all groups in an array.

  | sort_by([length, max]) # Sort the groups by their sizes.
    # If two groups have the same size then the one with the largest
    # number wins (is bigger).

  | to_entries[]           # Enumerate the array, generating key-value objects.
  |                        # For each object, generate two lines:
  "Group \(.key): \(.value | sort | join(" "))"   + "\n" +
  "Average: \(      .value | (add / length)  )"

] # Contain the group+average lines in an array.
  # Join the array elements by separator lines (dashes) to produce the report.
| join("\n" + "-"*78 + "\n")
'

# Output:
#
# Group 0: 3267
# Average: 3267
# ------------------------------------------------------------------------------
# Group 1: 7854
# Average: 7854
# ------------------------------------------------------------------------------
# Group 2: 4415 4447
# Average: 4431
# ------------------------------------------------------------------------------
# Group 3: 681 6426
# Average: 3553.5
# ------------------------------------------------------------------------------
# Group 4: 21263 21361 21801 21832 22947 23523 29174
# Average: 23128.714285714286
# ------------------------------------------------------------------------------
# Group 5: 10373 12698 13132 13924 17444 17963 18934 18979
# Average: 15430.875


# The `add/1` built-in "reduces" an array of values to a single value.
# You can think of it as sticking the `+` operator in between each value of
# the collection. Here are some examples:
#
jq -n '[1, 2, 3, 4, 5] | add'  # => 15
jq -n '["a", "b", "c"] | add'  # => "abc"

# `+` concatenates arrays
jq -n '[["a"], ["b"], ["c"]] | add'

# Output:
# [
#   "a",
#   "b",
#   "c"
# ]

# `+` merges objects non-recursively.
jq -n '[{a: 1, b: {c: 3}}, {b: 2, c: 4}] | add'

# Output:
# {
#   "a": 1,
#   "b": 2,
#   "c": 4
# }


# jq provides a special syntax for writing an expression that reduces
# the outputs generated by a given expresion to a single value.
# It has this form:
#
#   reduce outputs_expr as $var (initial_value; reduction_expr)
#
# Examples:
#
jq -n 'reduce range(1; 6) as $i (0; . + $i)'             # => 15
jq -n 'reduce (1, 2, 3, 4, 5) as $i (0; . + $i)'         # => 15
jq -n '[1, 2, 3, 4, 5] | reduce .[] as $i (0; . + $i)'   # => 15
jq -n '["a", "b", "c"] | reduce .[] as $i (""; . + $i)'  # => "abc"

# Notice the `.` in the `reduction_expr` is the `initial_value` at first,
# and then it becomes the result of applying the `reduction_expr` as
# we iterate through the values of `outputs_expr`. The expression:
#
#    reduce (1, 2, 3, 4, 5) as $i (0; . + $i)
#
# can be thought of as doing:
#
#    0 + 1 | . + 2 | . + 3 | . + 4 | . + 5
#


# The `*` operator when used on two objects, merges both recursively.
# Therefore, to merge JSON objects recursively, you can use `reduce`
# with the `*` operator. For example:
#
echo '
  {"a": 1,  "b": {"c": 3}}
  {         "b": {"d": 4}}
  {"a": 99, "e": 5       }
' | jq -s 'reduce .[] as $m ({}; . * $m)'

# Output:
# {
#   "a": 99,
#   "b": {
#     "c": 3,
#     "d": 4
#   },
#   "e": 5
# }


# jq has variable assignment in the form of `expr as $var`, which binds
# the value of `expr` to `$var`, and `$var` is immutable. Further more,
# `... as ...` doesn't change the input of the next filter; its introduction
# in a filter pipeline is only for establishing the binding of a value to a
# variable, and its scope extends to the filters following its definition.
# (i.e., to look up a variable's definition, scan to the left of the filter
# chain from the expression using it until you find the definition)
#
jq -rn '[1, 2, 3, 4, 5]
        | (.[0] + .[-1])      as $sum     # Always put ( ) around the binding `expr` to avoid surprises.
        | ($sum * length / 2) as $result  # The current input at this step is still the initial array.
        | "The result is: \($result)"     # Same.
'

# Output:
# The result is: 15


# With the `expr as $var` form, if multiple values are generated by `expr`
# then jq will iterate through them and bind each value to `$var` in turn
# for the rest of the pipeline.
#
jq -rn 'range(2; 4) as $i
        | range(1; 6) as $j
          | "\($i) * \($j) = \($i * $j)"
'

# Output:
# 2 * 1 = 2
# 2 * 2 = 4
# 2 * 3 = 6
# 2 * 4 = 8
# 2 * 5 = 10
# 3 * 1 = 3
# 3 * 2 = 6
# 3 * 3 = 9
# 3 * 4 = 12
# 3 * 5 = 15


# It's sometimes useful to bind the initial input to a variable at the
# start of a program, so that you can refer to it later down the pipeline.
#
jq -rn "$(cat <<'EOF'
    {lookup:  {a: 1, b: 2, c: 3},
     bonuses: {a: 5, b: 2, c: 9}
    }
    | . as $doc
    | .bonuses
    | to_entries[]
    | "\(.key)'s total is \($doc.lookup[.key] + .value)"
EOF
)"

# Output:
# a's total is 6
# b's total is 4
# c's total is 12


# jq supports destructing during varible binding. This lets you extract values
# from an array or an object and bind them to variables.
#
jq -n '[range(5)] | . as [$first, $second] | $second'

# Output:
# 1

jq -n '{ name: "Tom", numbers: [1, 2, 3], age: 32}
       | . as {
            name: $who,                  # bind .name to $who
            $name,                       # shorthand for `name: $name`
            numbers: [$first, $second],
         }
       | $name, $second, $first, $who
'

# Output:
# "Tom"
# 2
# 1
# "Tom"


# In jq, values can be assigned to an array index or object key via the
# assignment operator, `=`. The same current input is given to both sides
# of the assignment operator, and the assignment itself evaluates to the
# current input. In other words, the assignment expression is evaluated
# for its side effect, and doesn't generate a new output.
#
jq -n '.a = 1 | .b = .a + 1'  # => {"a": 1, "b": 2}

# Note that input is `null` due to `jq -n`, so `.` is `null` in the first
# filter, and assiging to a key under `null` turns it into an object with
# the key. The same input (now an object) then gets piped to the next filter,
# which then sets the `b` key to the value of the `a` key plus `1`, which is `2`.
#

# Another example:
#
jq -n '.a=1, .a.b=2'   # => {"a": 1} {"a": {"b": 2}}

# In the above example, two objects are generated because both assignments
# received `null` as their inputs, and each operand of the comma operator
# is evaluated independently. Notice also how you can easily generate
# nested objects.


# In addition to the assignment operator, jq also has operators like:
# `+=`, `-=`, `*=`, and '/=', ... etc. Basically, `a op= b` is a shorthand
# for `a = a op b`, and they are handy for updating an object attribute or
# an item in an array based on its current value. Examples:
#
jq -n '.a.b.c = 3 | .a.b.c = .a.b.c + 1' # => {"a": {"b": {"c": 4}}}
jq -n '.a.b.c = 3 | .a.b.c += 1'         # => {"a": {"b": {"c": 4}}}


# To delete a value, use `del/1`, which takes a path expression that specifies
# the locations of the things to be deleted. Example:
#
jq -n '{a: 1, b: {c: 2}, d: [3, 4, 5]} | del(.b.c, .d[1]) | .b.x = 6'

# Output:
# {
#   "a": 1,
#   "b": {
#     "x": 6
#   },
#   "d": [
#     3,
#     5
#   ]
# }


# Other than using jq's built-in functions, you can define your own.
# In fact, many built-in functions are defined using jq (see the link
# to jq's built-in functions at the end of the doc).
#
jq -n '
    def my_select(expr): if expr then . else empty end;
    def my_map(expr): [.[] | expr];
    def sum: reduce .[] as $x (0; . + $x);
    def my_range($from; $to):
        if $from >= $to then
            empty
        else
            $from, my_range($from + 1; $to)
        end
    ;
    [my_range(1; 6)] | my_map(my_select(. % 2 != 0)) | sum
'

# Output:
# 9

# Some notes about function definitons:
#
# - Functions are usually defined at the beginning, so that they are available
#   to the rest of the jq program.
#
# - Each function definion should end with a `;` (semicolon).
#
# - It's also possible to define a function within another, though it's not shown here.
#
# - Function parameters are separated by `;` (semicolor). This is consistent with
#   passing multiple arguments when calling a function.
#
# - A function can call itself; in fact, jq has TCO (Tail Call Optimization).
#
# - `def f($a; $b): ...;` is a shorthand for: `def f(a; b): a as $a | b as $b | ...`
```


## Further Reading
- https://stedolan.github.io/jq/manual/
- https://github.com/stedolan/jq/wiki/jq-Language-Description
- https://github.com/stedolan/jq/wiki/Cookbook
- https://github.com/stedolan/jq/blob/master/src/builtin.jq
