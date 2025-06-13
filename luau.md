---
name: Luau
contributors:
	- ["ratplier", "http://github.com/ratplier"]
filename: learnLuau.luau
---

**Luau** builds upon Lua 5.1, enhancing performance and adding features while maintaining broad compatibility.  
This example explores core concepts and highlights distinctions between Luau and standard Lua 5.1.

```lua
-- Single line comment
--[[
	Two sets of brackets create a multi-line comment
]]

--[=[
	Adding equal signs between the brackets allows for nested multi-line comments.
]=]

--[==[
	The above is matched in equal signs to the below, so it will not cause an error.
	This allows for commenting out large blocks of code that contain multi-line comments.
]==]


-- Variables:

local a = 0
local x, y, z = 0, 0, 0  -- Multiple assignments
local z, x, y = x, y, z -- Variables are evaluated before assignment

local number = 0       -- Integer
local decimal = 220.022  -- Floating-point number
local hexadecimal = 0xAA -- Hexadecimal number


-- Arithmetic Operations:

number = number + 1
number = number - 1
number = number * 2
number = number / 2
number = number ^ 2
number = number % 2

-- Compound Assignment Operators:
-- Used as syntax sugar
-- works with all math operations
-- special exceptions are listed below

number += 1
number -= 1
number *= 1
number /= 1
number //= 1 -- Floor division (rounded down)
number %= 1 -- Modulus (or remainder)
number ^= 1 -- Exponent

-- Strings:


local helloString = "Hello"
local worldString = "World!"
local multiLineString = [[
This is a multi-line string.
    It can span multiple lines.
]]
local interpolatedString = `var(number) -> {20}` -- var(number) -> 20

local concatenatedString = helloString .. worldString -- String concatenation
concatenatedString ..= multiLineString -- Concatenation compound operator

-- Lua (and Luau) use automatic garbage collection.  Setting a variable to nil makes it
-- eligible for GC sooner, but isn't strictly necessary.  It's mainly beneficial for
-- releasing large objects quickly or preventing accidental access to stale values.  Values
-- are also GC'd when a scope (e.g., do-end block, function) ends.

-- More details: https://www.lua.org/wshop18/Ierusalimschy.pdf

multiLineString = nil -- No longer needed; allows quicker GC.


-- Booleans and Logical Operators:

local booleanValue = true
booleanValue = not booleanValue -- Flip boolean to false

local andResult = true and false -- Logical AND
local orResult = true or false  -- Logical OR
local chainedResult = (true and false) or (true or not true)  -- Chained expressions



-- Tables (Arrays and Dictionaries):

local emptyTable = {}

local myArray = { "index 1", "index 2", "index 3", [4] = "index 4" } -- Array
local dictionary = { key1 = 20, key2 = 30, key3 = 40 }          -- Dictionary

local preAllocatedArray = table.create(20)        -- Efficiently pre-allocate an array
local preFilledArray = table.create(20, "hello") -- Create and fill an array

emptyTable["a"] = 20  -- Add to table (dictionary style)
emptyTable.b = 10       -- Alternative syntax
emptyTable[1] = 30    -- Add to table (array style)

print(emptyTable[1])     -- Access array element
print(emptyTable["a"])  -- Access dictionary element
print(dictionary.key1)   -- Access dictionary element


-- Table Library Functions:

local packedTable = table.pack(1, 2, 3)   -- Pack values into an array
local anotherTable = table.create(3)

local val1, val2, val3 = table.unpack(packedTable) -- Unpack array into variables

table.insert(myArray, "hello")           -- Insert at the end
table.insert(myArray, 1, "world")      -- Insert at index 1

table.remove(myArray, 1)                -- Remove element at index 1

local clonedTable = table.clone(myArray)      -- Shallow copy

local arrayLength = table.maxn(myArray)   -- Get array length (for numerically indexed tables)


table.sort(myArray, function(a, b)  -- Sort with custom comparator
	return a > b
end)

table.clear(emptyTable)              -- Remove all elements

table.move(myArray, 2, 3, 1, anotherTable) -- Copy a section of array to another table

local concatenatedValues = table.concat(myArray, " ") -- Join array elements into a string

local readOnlyTable = table.freeze(myArray) -- Create a read-only version of the table (Luau Specific)





-- Functions:

local function add(a, b)
	return a + b
end

local sum = add(2, 3)


local function average(...) -- Variadic function (accepts any number of arguments)
	local args = table.pack(...)
	local total = 0
	for _, n in ipairs(args) do
		total += n
	end
	return total / #args
end

local avg = average(10, 20, 30, 40)


-- Control Flow:

do
	local myLocal = 20     -- Local scope
	myGlobal = 10        -- Global scope
	local myNilValue   -- Declaring a nil variable (Luau allows this locally not globally)
	myNilValue = 20
end

local counter = 0
while counter < 50 do
	counter -= 1
	counter += 2
end


while false do  -- Never executes
	print("This will not print")
end


if counter > 40 then
	print(counter)
end


local loopCount = 0
repeat
	loopCount += 1
until loopCount > 20


repeat
	print("Hello (once)")
until true  -- Only runs once


local age = 20
if age >= 18 then
	print("Adult")
elseif age >= 13 then
	print("Teenager")
else
	print("Child")
end


-- For Loops:

for i = 1, 5 do
	print(i)
end


for k = 10, 1, -2 do
	print(k)
end


-- Generic For Loops (Iterators):

local dataTable = { a = 1, b = 2, c = 3 }

for key, value in pairs(dataTable) do  -- Key-value pairs
	print(key, value)
end


local numArray = { 10, 20, 30 }
for index, value in ipairs(numArray) do  -- Numerically indexed arrays
	print(index, value)
end


local mixedTable = { 10, 20, 30, a = 20 }
for index, value in next, mixedTable do -- For tables where the indexing type is unknown
	print(index, value)
end


-- Custom Iterator:
local function customIterator(t, i)
	i = i + 1
	local v = t[i]
	if v then
		return i, v
	end
end


local customTable = { 10, 20, 30 }
for index, value in customIterator, customTable, 0 do
	print(index, value)
end

-- Luau specific features:

-- Buffers

-- Buffers are used for storing and manipulating raw binary data.
-- They are often used when dealing with network communication, file I/O, or when precise control over memory layout is needed.
-- Unlike strings which are UTF-8 encoded, buffers store raw bytes without any interpretation.

-- Creating Buffers:

local initialSize = 30
local myBuffer = buffer.create(initialSize)  -- Create a buffer with an initial capacity of 20 bytes.
print("Initial buffer size:", buffer.len(myBuffer))  -- Output: 20

local myBuffer2 = buffer.create(5, 0xFF) -- create a buffer and prefill with the byte value 0xFF (255 in decimal)
print(
	"Initial buffer size:",
	buffer.len(myBuffer2),
	" first byte: ",
	buffer.readu8(myBuffer2, 0)
) -- Output: 5, 255

-- Writing Data to a Buffer:

buffer.writeu8(myBuffer, 0, 0x41)         -- Write the byte 0x41 ('A') at index 0 (as an unsigned 8-bit int)
buffer.writeu16(myBuffer, 1, 0x1234)       -- Write the 16-bit value 0x1234 at index 1 (as an unsigned 16-bit int, little-endian)
buffer.writeu32(myBuffer, 3, 0xABCDEF01)   -- Write the 32-bit value at index 3 (unsigned 32-bit int, little-endian)
buffer.writef32(myBuffer, 7, 3.14)  -- Write a 32-bit floating-point number (little-endian)
buffer.writef64(myBuffer, 11, 2.71828) -- Write a 64-bit floating-point number (little-endian)
buffer.writestring(myBuffer, 19, "Luau") -- Write a string at index 19

print("buffer after writing:", buffer.len(myBuffer)) -- output: 24


-- Reading Data from a Buffer:

local byte1 = buffer.readu8(myBuffer, 0)     -- Read the byte at index 0
local short1 = buffer.readu16(myBuffer, 1)  -- Read the 16-bit value at index 1
local int1 = buffer.readu32(myBuffer, 3)  -- Read the 32-bit value at index 3
local float1 = buffer.readf32(myBuffer, 7)     -- Read the 32-bit float
local double1 = buffer.readf64(myBuffer, 11)   -- Read the 64-bit double
local str = buffer.readstring(myBuffer, 19, 4) -- Read the string from beginning to char 4


print("Read bytes:", string.format("0x%X",byte1) )            -- Output: 0x41
print("Read short:", string.format("0x%X",short1) )           -- Output: 0x1234
print("Read int:", string.format("0x%X",int1) )            -- Output: 0xABCDEF01
print("Read float:", float1)                -- Output: 3.140000104904175
print("Read double:", double1)             -- Output: 2.7182800000000001
print("Read string:", str)  -- Output: Luau

-- Buffer Size and Capacity:
local currentSize = buffer.len(myBuffer)          -- Get the current size (used bytes)
print("Current buffer size:", currentSize) -- Output: 24

-- Other Operations:

buffer.fill(myBuffer, 0, 0x00)  -- Fill buffer with 0x00 starting from index 0
local newBuffer = buffer.create(currentSize) -- Create a buffer to copy to from another buffer

buffer.copy(myBuffer, 0, newBuffer, 5, 10) -- copy 10 bytes from myBuffer starting at 0 to newBuffer at index 5
print("Copied buffer:", buffer.readu8(newBuffer, 5)) -- read one of the bytes copied above


-- Error Handling:
local function tryRead(buf, index)
	local success, value = pcall(buffer.readu8, buf, index)
	if success then
		print("Read byte at", index, ":", string.format("0x%X",value))
	else
		print("Error at index", index, ":", value)
	end
end

tryRead(myBuffer, 31) -- this will error
tryRead(newBuffer, 6) -- this will be okay

-- Important Notes:

--   * Byte Order (Endianness): Luau buffers typically use little-endian byte order for multi-byte values (U16, U32, U64, F32, F64).
--   * Indexing: Buffer indices are 0-based. The first byte is at index 0.
--   * Error Handling: Attempting to read or write outside the bounds of a buffer can lead to errors, so handle these cases accordingly, consider `pcall` or manually handling it.
--   * Buffer usage is more memory efficient than string when dealing with large chunks of binary data.

-- Types

-- Types in Luau are used to define the kind of data a variable, function, or table can hold or return.
-- They improve code readability, prevent errors, and make development faster by enabling better tooling support.
-- Types can describe simple values like numbers or strings, as well as more complex structures like tables or functions.

-- Basic Types:
-- These are the most common types used in Luau.

local aNumber: number = 10          -- Represents numeric values (e.g., integers, floating-point numbers).
local aString: string = "Hello"     -- Represents text or character sequences.
local aBoolean: boolean = true      -- Represents true or false values.
local aNil: nil = nil               -- Represents the absence of a value (or "nothing").

-- Compound Types:
-- Used to represent collections of values.

local aTable: table = {1, 2, 3}     -- Can store arrays, dictionaries, or mixed data.
local aDictionary: { [string]: number } = { key1 = 10, key2 = 20 }
-- A table with string keys and number values.

-- Union and Optional Types:
-- Union types allow a variable to hold multiple possible types.
-- Optional types allow a variable to hold a specific type or nil.

local aUnion: string | number = 42  -- Can be a string or a number.
local anOptional: number? = nil     -- Can be a number or nil.

-- Defining Types:
-- You can predefine types to be used locally or in other modules

type vector3 = {
	x: number,
	y: number,
	z: number
}

local a: vector3 = {}
a.x = 20

-- Export a type for other modules to use
export type vector3 = vector3

-- Generic Types:
-- You can make types that take in types and return a new type

type array<T> = {T} -- basic array type
type dictionary<K, V> = {[K]: V}

type recursiveType<T> = {
	head: recursizeType<T>?,
	value: T,
}

-- Function Types:
-- Functions can also have types to define their inputs and outputs.

local function sub(x: number, y: number): number
	return x + y
end

-- You can also do it like this

local add: (number, number) -> number = function(x, y)
	return x + y
end

-- basic function type definition 
type callback<...I, ...O> = (I...) -> (O...)

local function addVec3(a: vector3, b, vector3): vector3
	return {
		a.x + b.x,
		a.y + b.y,
		a.z + b.z,
	}
end

local function myTypedFunction<A>(parameter: A): A
	print(A)
	return A
end



-- Metamethods and Metatables

-- Metatables allow you to customize the behavior of tables.  They provide a mechanism
-- to intercept operations like addition, indexing, function calls, etc.  Metamethods
-- are functions defined within the metatable that are triggered when these operations
-- are performed on a table associated with that metatable.


-- Example: Overloading the addition operator (+)

local mt = {}  -- Create a metatable
mt.__index = mt -- Make methods self-referencing

mt.__add = function(self, other)  -- Define the __add metamethod
    return self.value + other.value
end

local t1 = { value = 10 }
local t2 = { value = 20 }

setmetatable(t1, mt)  -- Assign the metatable
setmetatable(t2, mt)

local sum = t1 + t2  -- Calls the __add metamethod
print(sum) --> 30


-- Other common metamethods:
-- __index: For handling table lookups (when a key doesn't exist).
-- __newindex: For handling table assignments.
-- __call: For making tables callable like functions.
-- __len: For getting the length of a table.
-- __idiv: Integer division in Luau.
-- etc.  (See Lua/Luau documentation for a complete list)
-- at https://create.roblox.com/docs/luau/metatables




-- Native Code Generation (NCG)
-- Luau's NCG compiles Lua code directly into native machine code, leading to significant
-- performance improvements compared to interpreted Lua.  NCG leverages LLVM to
-- optimize the generated code further.  Not all platforms support NCG.




-- Code Optimization (Luau)

-- Luau provides various optimization techniques:
-- * Loop unrolling: Reduces loop overhead for small, fixed iterations.
-- * Constant folding: Evaluates constant expressions at compile time.
-- * Inlining: Replaces function calls with the function's body in some cases.
-- * Dead code elimination: Removes code that is never executed.
-- * String interning: Stores identical strings only once, saving memory.
-- * Efficient table implementation: Luau's table implementation is optimized for
--   common usage patterns.
-- * Type specialization (Luau):  When type information is available, Luau can generate
--   more efficient code tailored to specific types.

-- These optimizations, combined with NCG, contribute significantly to Luau's performance advantages over standard Lua 5.1.

-- End of Luau Showcase
```

## Lua Libraries in Luau and Differences:

Most Lua 5.1 libraries are present in Luau (e.g., `math`, `string`, `table`, `coroutine`).  However, there are some key differences:

* `io` (restricted): Limited or no access to file system functions due to sandboxing.
* `os` (mostly absent): Operating system specific functions are generally unavailable.
* `debug` (limited): Debugging functionality may be curtailed.
* `package` (modified): Luau's module system may have significant changes depending on the embedding platform (e.g., Roblox).

## Luau Specific Libraries

* `bit32`: Bitwise operations library similar to Lua 5.2's `bit32`
* `utf8`: If supported, provides UTF-8 string handling functions