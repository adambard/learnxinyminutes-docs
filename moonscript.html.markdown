---
language: moonscript
contributors:
  - ["RyanSquared", "https://ryansquared.github.io/"]
  - ["Job van der Zwan", "https://github.com/JobLeonard"]
filename: moonscript.moon
---

MoonScript is a dynamic scripting language that compiles into Lua. It gives
you the power of one of the fastest scripting languages combined with a
rich set of features.

See [the MoonScript website](https://moonscript.org/) to see official guides on installation for all platforms.

```moon
-- Two dashes start a comment. Comments can go until the end of the line.
-- MoonScript transpiled to Lua does not keep comments.

-- As a note, MoonScript does not use 'do', 'then', or 'end' like Lua would and
-- instead uses an indented syntax, much like Python.

--------------------------------------------------
-- 1. Assignment
--------------------------------------------------

hello = "world"
a, b, c = 1, 2, 3
hello = 123 -- Overwrites `hello` from above.

x = 0
x += 10 -- x = x + 10

s = "hello "
s ..= "world" -- s = s .. "world"

b = false
b and= true or false -- b = b and (true or false)

--------------------------------------------------
-- 2. Literals and Operators
--------------------------------------------------

-- Literals work almost exactly as they would in Lua. Strings can be broken in
-- the middle of a line without requiring a \.

some_string = "exa
mple" -- local some_string = "exa\nmple"

-- Strings can also have interpolated values, or values that are evaluated and
-- then placed inside of a string.

some_string = "This is an #{some_string}" -- Becomes 'This is an exa\nmple'

--------------------------------------------------
-- 2.1. Function Literals
--------------------------------------------------

-- Functions are written using arrows:

my_function = -> -- compiles to `function() end`
my_function() -- calls an empty function

-- Functions can be called without using parenthesis. Parentheses may still be
-- used to have priority over other functions.

func_a = -> print "Hello World!"
func_b = ->
	value = 100
	print "The value: #{value}"

-- If a function needs no parameters, it can be called with either `()` or `!`.

func_a!
func_b()

-- Functions can use arguments by preceding the arrow with a list of argument
-- names bound by parentheses.

sum = (x, y)-> x + y -- The last expression is returned from the function.
print sum(5, 10)

-- Lua has an idiom of sending the first argument to a function as the object,
-- like a 'self' object. Using a fat arrow (=>) instead of a skinny arrow (->)
-- automatically creates a `self` variable. `@x` is a shorthand for `self.x`.

func = (num)=> @value + num

-- Default arguments can also be used with function literals:

a_function = (name = "something", height=100)->
	print "Hello, I am #{name}.\nMy height is #{height}."

-- Because default arguments are calculated in the body of the function when
-- transpiled to Lua, you can reference previous arguments.

some_args = (x = 100, y = x + 1000)-> print(x + y)

--------------------------------------------------
-- Considerations
--------------------------------------------------

-- The minus sign plays two roles, a unary negation operator and a binary
-- subtraction operator. It is recommended to always use spaces between binary
-- operators to avoid the possible collision.

a = x - 10 --  a = x - 10
b = x-10 -- b = x - 10
c = x -y -- c = x(-y)
d = x- z -- d = x - z

-- When there is no space between a variable and string literal, the function
-- call takes priority over following expressions:

x = func"hello" + 100 -- func("hello") + 100
y = func "hello" + 100 -- func("hello" + 100)

-- Arguments to a function can span across multiple lines as long as the
-- arguments are indented. The indentation can be nested as well.

my_func 5, -- called as my_func(5, 8, another_func(6, 7, 9, 1, 2), 5, 4)
	8, another_func 6, 7, -- called as
		9, 1, 2,		  -- another_func(6, 7, 9, 1, 2)
	5, 4

-- If a function is used at the start of a block, the indentation can be
-- different than the level of indentation used in a block:

if func 1, 2, 3, -- called as func(1, 2, 3, "hello", "world")
		"hello",
		"world"
	print "hello"

--------------------------------------------------
-- 3. Tables
--------------------------------------------------

-- Tables are defined by curly braces, like Lua:

some_values = {1, 2, 3, 4}

-- Tables can use newlines instead of commas.

some_other_values = {
	5, 6
	7, 8
}

-- Assignment is done with `:` instead of `=`:

profile = {
	name: "Bill"
	age: 200
	"favorite food": "rice"
}

-- Curly braces can be left off for `key: value` tables.

y = type: "dog", legs: 4, tails: 1

profile =
	height: "4 feet",
	shoe_size: 13,
	favorite_foods: -- nested table
		foo: "ice cream", 
		bar: "donuts"

my_function dance: "Tango", partner: "none" -- :( forever alone

-- Tables constructed from variables can use the same name as the variables
-- by using `:` as a prefix operator.

hair = "golden"
height = 200
person = {:hair, :height}

-- Like in Lua, keys can be non-string or non-numeric values by using `[]`.

t =
	[1 + 2]: "hello"
	"hello world": true -- Can use string literals without `[]`.

--------------------------------------------------
-- 3.1. Table Comprehensions
--------------------------------------------------

-- List Comprehensions

-- Creates a copy of a list but with all items doubled. Using a star before a
-- variable name or table can be used to iterate through the table's values.

items = {1, 2, 3, 4}
doubled = [item * 2 for item in *items]
-- Uses `when` to determine if a value should be included.

slice = [item for item in *items when item > 1 and item < 3]

-- `for` clauses inside of list comprehensions can be chained.

x_coords = {4, 5, 6, 7}
y_coords = {9, 2, 3}

points = [{x,y} for x in *x_coords for y in *y_coords]

-- Numeric for loops can also be used in comprehensions:

evens = [i for i=1, 100 when i % 2 == 0]

-- Table Comprehensions are very similar but use `{` and `}` and take two
-- values for each iteration.

thing = color: "red", name: "thing", width: 123
thing_copy = {k, v for k, v in pairs thing}

-- Tables can be "flattened" from key-value pairs in an array by using `unpack`
-- to return both values, using the first as the key and the second as the
-- value.

tuples = {{"hello", "world"}, {"foo", "bar"}}
table = {unpack tuple for tuple in *tuples}

-- Slicing can be done to iterate over only a certain section of an array. It
-- uses the `*` notation for iterating but appends `[start, end, step]`.

-- The next example also shows that this syntax can be used in a `for` loop as
-- well as any comprehensions.

for item in *points[1, 10, 2]
	print unpack item

-- Any undesired values can be left off. The second comma is not required if
-- the step is not included.

words = {"these", "are", "some", "words"}
for word in *words[,3] 
	print word

--------------------------------------------------
-- 4. Control Structures
--------------------------------------------------

have_coins = false
if have_coins
	print "Got coins"
else
	print "No coins"

-- Use `then` for single-line `if`
if have_coins then "Got coins" else "No coins"

-- `unless` is the opposite of `if`
unless os.date("%A") == "Monday"
	print "It is not Monday!"

-- `if` and `unless` can be used as expressions
is_tall = (name)-> if name == "Rob" then true else false
message = "I am #{if is_tall "Rob" then "very tall" else "not so tall"}"
print message -- "I am very tall"

-- `if`, `elseif`, and `unless` can evaluate assignment as well as expressions.
if x = possibly_nil! -- sets `x` to `possibly_nil()` and evaluates `x`
	print x

-- Conditionals can be used after a statement as well as before. This is
-- called a "line decorator".

is_monday = os.date("%A") == "Monday"
print("It IS Monday!") if isMonday
print("It is not Monday..") unless isMonday
--print("It IS Monday!" if isMonday) -- Not a statement, does not work

--------------------------------------------------
-- 4.1 Loops
--------------------------------------------------

for i = 1, 10
	print i

for i = 10, 1, -1 do print i -- Use `do` for single-line loops.

i = 0
while i < 10
	continue if i % 2 == 0 -- Continue statement; skip the rest of the loop.
	print i

-- Loops can be used as a line decorator, just like conditionals
print "item: #{item}" for item in *items

-- Using loops as an expression generates an array table. The last statement
-- in the block is coerced into an expression and added to the table.
my_numbers = for i = 1, 6 do i -- {1, 2, 3, 4, 5, 6}

-- use `continue` to filter out values
odds = for i in *my_numbers
	continue if i % 2 == 0 -- acts opposite to `when` in comprehensions!
	i -- Only added to return table if odd

-- A `for` loop returns `nil` when it is the last statement of a function
-- Use an explicit `return` to generate a table.
print_squared = (t) -> for x in *t do x*x -- returns `nil`
squared = (t) -> return for x in *t do x*x -- returns new table of squares

-- The following does the same as `(t) -> [i for i in *t when i % 2 == 0]`
-- But list comprehension generates better code and is more readable!

filter_odds = (t) -> 
	return for x in *t
		if x % 2 == 0 then x else continue
evens = filter_odds(my_numbers) -- {2, 4, 6}

--------------------------------------------------
-- 4.2 Switch Statements
--------------------------------------------------

-- Switch statements are a shorthand way of writing multiple `if` statements
-- checking against the same value. The value is only evaluated once.

name = "Dan"

switch name
	when "Dave"
		print "You are Dave."
	when "Dan"
		print "You are not Dave, but Dan."
	else
		print "You are neither Dave nor Dan."

-- Switches can also be used as expressions, as well as compare multiple
-- values. The values can be on the same line as the `when` clause if they
-- are only one expression.

b = 4
next_even = switch b
	when 1 then 2
	when 2, 3 then 4
	when 4, 5 then 6
	else error "I can't count that high! D:"

--------------------------------------------------
-- 5. Object Oriented Programming
--------------------------------------------------

-- Classes are created using the `class` keyword followed by an identifier,
-- typically written using CamelCase. Values specific to a class can use @ as
-- the identifier instead of `self.value`.

class Inventory
	new: => @items = {}
	add_item: (name)=> -- note the use of fat arrow for classes!
		@items[name] = 0 unless @items[name]
		@items[name] += 1

-- The `new` function inside of a class is special because it is called when
-- an instance of the class is created.

-- Creating an instance of the class is as simple as calling the class as a
-- function. Calling functions inside of the class uses \ to separate the
-- instance from the function it is calling.

inv = Inventory!
inv\add_item "t-shirt"
inv\add_item "pants"

-- Values defined in the class - not the new() function - will be shared across
-- all instances of the class.

class Person
	clothes: {}
	give_item: (name)=>
		table.insert @clothes name

a = Person!
b = Person!

a\give_item "pants"
b\give_item "shirt"

-- prints out both "pants" and "shirt"

print item for item in *a.clothes

-- Class instances have a value `.__class` that are equal to the class object
-- that created the instance.

assert(b.__class == Person)

-- Variables declared in class body the using the `=` operator are locals,
-- so these "private" variables are only accessible within the current scope.

class SomeClass
	x = 0
	reveal: ->
		x += 1
		print x

a = SomeClass!
b = SomeClass!
print a.x -- nil
a.reveal! -- 1
b.reveal! -- 2

--------------------------------------------------
-- 5.1 Inheritance
--------------------------------------------------

-- The `extends` keyword can be used to inherit properties and methods from
-- another class.

class Backpack extends Inventory
	size: 10
	add_item: (name)=>
		error "backpack is full" if #@items > @size
		super name -- calls Inventory.add_item with `name`.

-- Because a `new` method was not added, the `new` method from `Inventory` will
-- be used instead. If we did want to use a constructor while still using the
-- constructor from `Inventory`, we could use the magical `super` function
-- during `new()`.

-- When a class extends another, it calls the method `__inherited` on the 
-- parent class (if it exists). It is always called with the parent and the
-- child object.

class ParentClass
	@__inherited: (child)=>
		print "#{@__name} was inherited by #{child.__name}"
	a_method: (a, b) => print a .. ' ' .. b

-- Will print 'ParentClass was inherited by MyClass'

class MyClass extends ParentClass
	a_method: =>
		super "hello world", "from MyClass!" 
		assert super == ParentClass

--------------------------------------------------
-- 6. Scope
--------------------------------------------------

-- All values are local by default. The `export` keyword can be used to
-- declare the variable as a global value.

export var_1, var_2
var_1, var_3 = "hello", "world" -- var_3 is local, var_1 is not.

export this_is_global_assignment = "Hi!"

-- Classes can also be prefixed with `export` to make them global classes.
-- Alternatively, all CamelCase variables can be exported automatically using
-- `export ^`, and all values can be exported using `export *`.

-- `do` lets you manually create a scope, for when you need local variables.

do
	x = 5
print x -- nil

-- Here we use `do` as an expression to create a closure.

counter = do 
	i = 0
	->
		i += 1
		return i

print counter!  -- 1
print counter!  -- 2

-- The `local` keyword can be used to define variables
-- before they are assigned.

local var_4
if something
	var_4 = 1
print var_4 -- works because `var_4` was set in this scope, not the `if` scope.

-- The `local` keyword can also be used to shadow an existing variable.

x = 10
if false
	local x
	x = 12
print x -- 10

-- Use `local *` to forward-declare all variables. 
-- Alternatively, use `local ^` to forward-declare all CamelCase values.

local *

first = ->
	second!

second = ->
	print data

data = {}

--------------------------------------------------
-- 6.1 Import
--------------------------------------------------

-- Values from a table can be brought to the current scope using the `import`
-- and `from` keyword. Names in the `import` list can be preceded by `\` if
-- they are a module function.

import insert from table -- local insert = table.insert
import \add from state: 100, add: (value)=> @state + value
print add 22

-- Like tables, commas can be excluded from `import` lists to allow for longer
-- lists of imported items.

import
	asdf, gh, jkl
	antidisestablishmentarianism
	from {}

--------------------------------------------------
-- 6.2 With
--------------------------------------------------

-- The `with` statement can be used to quickly call and assign values in an
-- instance of a class or object.

file = with File "lmsi15m.moon" -- `file` is the value of `set_encoding()`.
	\set_encoding "utf8"

create_person = (name, relatives)->
	with Person!
		.name = name
		\add_relative relative for relative in *relatives
me = create_person "Ryan", {"sister", "sister", "brother", "dad", "mother"}

with str = "Hello" -- assignment as expression! :D
	print "original: #{str}"
	print "upper: #{\upper!}"

--------------------------------------------------
-- 6.3 Destructuring
--------------------------------------------------

-- Destructuring can take arrays, tables, and nested tables and convert them
-- into local variables.

obj2 =
	numbers: {1, 2, 3, 4}
	properties:
		color: "green"
		height: 13.5

{numbers: {first, second}, properties: {:color}} = obj2

print first, second, color -- 1 2 green

-- `first` and `second` return [1] and [2] because they are as an array, but
-- `:color` is like `color: color` so it sets itself to the `color` value.

-- Destructuring can be used in place of `import`.

{:max, :min, random: rand} = math -- rename math.random to rand

-- Destructuring can be done anywhere assignment can be done.

for {left, right} in *{{"hello", "world"}, {"egg", "head"}}
	print left, right
```

## Additional Resources

- [Language Guide](https://moonscript.org/reference/)
- [Online Compiler](https://moonscript.org/compiler/)
