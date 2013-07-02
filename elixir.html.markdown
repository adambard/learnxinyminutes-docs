---
language: elixir
author: Joao Marques
author_url: http://github.com/mrshankly
filename: learnelixir.ex
---

```elixir
# Single line comments start with a hash.

## --------------------
## -- Basic types
## --------------------

# There are numbers
3    # integer
0x1F # integer
3.0  # float

# Atoms, that are literals, a constant with name. They start with `:`.
:hello # atom

# Tuples that are stored contigously in memory.
{1,2,3} # tuple

# We can access a tuple element with the `elem` function:
elem({1, 2, 3}, 0) # => 1

# Lists that are implemented as linked lists.
[1,2,3] # list

# We can access the head and tail of a list as follows:
[head | tail] = [1,2,3]
head # => 1
tail # => [2,3]

# In elixir, just like in erlang, the `=` denotes pattern matching and
# not an assignment.
#
# This means that the left-hand side (pattern) is matched against a
# right-hand side.
#
# This is how the above example of accessing the head and tail of a list works.

# A pattern match will error when the sides don't match, in this example
# the tuples have different sizes.
{a, b, c} = {1, 2} # => ** (MatchError) no match of right hand side value: {1,2}

# There's also binaries
<<1,2,3>> # binary

# Strings and char lists
"hello" # string
'hello' # char list

# Strings are all encoded in UTF-8:
"héllò" # => "héllò"

# Strings are really just binaries, and char lists are just lists.
<<?a, ?b, ?c>> # => "abc"
[?a, ?b, ?c]   # => 'abc'

# `?a` in elixir returns the ASCII integer for the letter `a`
?a # => 97

## TODO:
######################################################
## JOIN BINARIES AND LISTS
######################################################

## --------------------
## -- Operators
## --------------------

# Some math
1 + 1  # => 2
10 - 5 # => 5
5 * 2  # => 10
10 / 2 # => 5.0

# In elixir the operator `/` always returns a float.

# To do integer division use `div`
div(10, 2) # => 5

# To get the division remainder use `rem`
rem(10, 3) # => 1

# There's also boolean operators: `or`, `and` and `not`.
# These operators expect a boolean as their first argument.
true and true # => true
false or true # => true
1 and true    # => ** (ArgumentError) argument error

# Elixir also provides `||`, `&&` and `!` which accept arguments of any type.
# All values except `false` and `nil` will evaluate to true.
1 || true  # => 1
false && 1 # => false
nil && 20  # => nil

!true # => false

# For comparisons we have: `==`, `!=`, `===`, `!==`, `<=`, `>=`, `<` and `>`
1 == 1 # => true
1 != 1 # => false
1 < 2  # => true

# `===` and `!==` are more strict when comparing integers and floats:
1 == 1.0  # => true
1 === 1.0 # => false

# We can also compare two different data types:
1 < :hello # => true

# The overall sorting order is defined below:
number < atom < reference < functions < port < pid < tuple < list < bit string

# To quote Joe Armstrong on this: "The actual order is not important,
# but that a total ordering is well defined is important."

## --------------------
## -- Control Flow
## --------------------

# `if` expression
if false do
	"This will never be seen"
else
	"This will"
end

# There's also `unless`
unless true do
	"This will never be seen"
else
	"This will"
end

# Remember pattern matching? Many control-flow structures in elixir rely on it.

# `case` allows us to compare a value against many patterns:
case {:one, :two} do
	{:four, :five} ->
		"This won't match"
	{:one, x} ->
		"This will match and assign `x` to `:two`"
	_ ->
		"This will match any value"
end

# It's common practive to assign a value to `_` if we don't need it.
# For example, if only the head of a list matters to us:
[head | _] =  [1,2,3]
head # => 1

# For better readability we can do the following:
[head | _tail] = [:a, :b, :c]
head # => :a

# `cond` lets us check for many conditions at the same time.
# Use `cond` instead of nesting many `if` expressions.
cond do
	1 + 1 == 3 ->
		"I will never be seen"
	2 * 5 == 12 ->
		"Me neither"
	1 + 2 == 3 ->
		"But I will"
end

# It is common to see a last condition equal to `true`, which will always match.
cond do
	1 + 1 == 3 ->
		"I will never be seen"
	2 * 5 == 12 ->
		"Me neither"
	true ->
		"But I will (this is essentially an else)"
end

# `try/catch` is used to catch values that are thrown, it also supports an
# `after` clause that is invoked whether or not a value is catched.
try do
	throw(:hello)
catch
	message -> "Got #{message}."
after
	IO.puts("I'm the after clause.")
end
# => I'm the after clause
#	 "Got :hello"

## TODO:
######################################################
## GUARDS
######################################################

## ---------------------------
## -- Modules and Functions
## ---------------------------



```