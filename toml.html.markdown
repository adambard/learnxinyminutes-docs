---
language: toml
filename: learntoml.toml
contributors:
  - ["Alois de Gouvello", "https://github.com/aloisdg"]
---

TOML stands for Tom's Obvious, Minimal Language. It is a data serialisation language designed to be a minimal configuration file format that's easy to read due to obvious semantics.

It is an alternative to YAML and JSON. It aims to be more human friendly than JSON and simpler that YAML. TOML is designed to map unambiguously to a hash table. TOML should be easy to parse into data structures in a wide variety of languages.

This document follows [TOML v1.0.0](https://toml.io/en/v1.0.0). Future [changes](https://github.com/toml-lang/toml/blob/main/CHANGELOG.md) are expected to be minor and backwards-compatible.

```toml
# Comments in TOML look like this.

##################
# Key/Value Pair #
##################

# The primary building block of a TOML document is the key/value pair.
# The key, equals sign, and value must be on the same line
# (though some values can be broken over multiple lines).
key = "value"

# Valus types must be one of the followings.
string = "hello"
integer = 42
float = 3.14
boolean = true
offsetDateTime = 1979-05-27T07:32:00Z
localDateTime = 1979-05-27T07:32:00
localDate = 1979-05-27
localTime = 07:32:00
array = [ 1, 2, 3 ]
inlineTable = { first = "Tom", last = "Preston-Werner" }

# A key may be either bare, quoted, or dotted.

## bare
bare_key = "value" # bare keys may only contain ASCII letters/digits, underscore, and dashes
1234 = "value" # only-digits bare keys are allowed
= "no key name" # empty bare key is invalid
# defining duplicated key is invalid
name = "Tom"
name = "Pradyun" # invalid

## quoted
"key can be quoted" = true # Both " and ' are fine
# A bare key must be non-empty, but an empty quoted key is allowed
"" = "blank"     # VALID but discouraged
'' = 'blank'     # VALID but discouraged

# between bare and quoted key, best practice is to use bare keys except when absolutely necessary

## dotted
# dotted keys are a sequence of bare or quoted keys joined with a dot.
# This allows for grouping similar properties together:
name = "Orange"
physical.color = "orange"
physical.shape = "round"
site."google.com" = true
# In JSON land, that would give you the following structure:
# {
#   "name": "Orange",
#   "physical": {
#     "color": "orange",
#     "shape": "round"
#   },
#   "site": {
#     "google.com": true
#   }
# }
# Tips: online TOML/JSON converter like this helps to understand TOML document structure
# https://pseitz.github.io/toml-to-json-online-converter/

other_kêys = "are permitted by spec but most implementations don't actually permit them"

## When defining long dotted key, indirectly defined key can be wrote into.
# Below indirectly makes the key "fruit" and "apple" into a table (more on table below).
fruit.apple.smooth = true

# So then you can add to the "fruit" and "apple" like so:
fruit.orange = 2
fruit.apple.amount = 1
# In JSON land, that would give you the following structure:
# {
#   "fruit": {
#     "orange": 2,
#     "apple": {
#       "amount": 1,
#       "smooth": true
#     }
#   }
# }

# And obviously, you can not do this:
fruit.apple = "something" # because that would be a duplicated key for 'fruit.apple'


##########
# String #
##########

# All strings must contain only valid UTF-8 characters.
# We can escape characters and some of them have a compact escape sequence.
# For example, \t add a tabulation. Refers to the spec to get all of them.
basicString = "are surrounded by quotation marks. \"I'm quotable\". Name\tJos"

multiLineString = """
are surrounded by three quotation marks
on each side and allow newlines."""

literalString = 'are surrounded by single quotes. Escaping are not allowed.'

multiLineLiteralString = '''
are surrounded by three single quotes on each side
and allow newlines. Still no escaping.
The first newline is trimmed in raw strings.
   All other whitespace
   is preserved. #! are preserved?
'''

# Control characters other than tab are not permitted in a literal string.
# Thus, for binary data it is recommended that you use Base64, another ASCII or UTF8
# encoding. The handling of that encoding will be application specific.


###########
# Integer #
###########

# Integers can start with a +, a - or nothing.
# Leading zeros are not allowed.
# Hex, octal, and binary forms are allowed.
# Values that cannot be expressed as a series of digits are not allowed.
int1 = +42
int2 = 0
int3 = -21
int4 = 0xdeadbeef
int5 = 0o755
int6 = 0b11011100
integerRange = 64

# You can use underscores to enhance readability.
# Each underscore must be surrounded by at least one digit on each side.
int4 = 5_349_221
int5 = 1_2_3_4_5     # VALID but discouraged
int6 = _1_2_3        # INVALID


#########
# Float #
#########

# Floats are an integer followed by a fractional and/or an exponent part.
flt1 = 3.1415
flt2 = -5e6
flt3 = 6.626E-34


###########
# Boolean #
###########

bool1 = true
bool2 = false
boolMustBeLowercase = true


############
# Datetime #
############

date1 = 1979-05-27T07:32:00Z # UTC time, following RFC 3339/ISO 8601 spec
date2 = 1979-05-26T15:32:00+08:00 # with RFC 3339/ISO 8601 offset
date3 = 1979-05-27T07:32:00 # without offset
date4 = 1979-05-27 # without offset or time


####################
# COLLECTION TYPES #
####################

#########
# Array #
#########

array1 = [ 1, 2, 3 ]
array2 = [ "Commas", "are", "delimiters" ]
array3 = [ "Don't mix", "different", "types" ]
array4 = [ [ 1.2, 2.4 ], ["all", 'strings', """are the same""", '''type'''] ]
array5 = [
  "Whitespace", "is", "ignored"
]

#########
# Table #
#########

## Tables (also known as hash tables or dictionaries) are collections of key/value pairs.
# They are defined by headers, with square brackets on a line by themselves.
# Empty tables are allowed and simply have no key/value pairs within them.
[table]


## Under that, and until the next table or EOF are the key/values of that table.
# Key/value pairs within tables are not guaranteed to be in any specific order.
[table-1]
key1 = "some string"
key2 = 123

[table-2]
key1 = "another string"
key2 = 456


## Naming rules for tables are the same as for keys.
[dog."tater.man"]
type = "pug"
# In JSON land, that would give you the following structure:
# {
#   "dog": {
#     "tater.man": {
#       "type": "pug"
#     }
#   }
# }
 

## Whitespace around dot-separated parts is ignored, however, best practice is to
# not use any extraneous whitespace.
[a.b.c]            # this is best practice
[ d.e.f ]          # same as [d.e.f]
[ j . "ʞ" . 'l' ]  # same as [j."ʞ".'l']


## You don't need to specify all the super-tables if you don't want to. TOML knows
# how to do it for you.
# [x] you
# [x.y] don't
# [x.y.z] need these
[x.y.z.w] # for this to work


## Like keys, you cannot define a table more than once. Doing so is invalid.
# DO NOT DO THIS
[fruit]
apple = "red"

[fruit] # invalid: key duplication
orange = "orange"

# DO NOT DO THIS EITHER
[fruit]
apple = "red"

[fruit.apple] # fruit.apple is a string, not a table, thus can not add key/value pair
texture = "smooth"


## The whole TOML document is a top-level table, starts at the beginning of the 
# document and ends just before the first table header (or EOF). Unlike other
# tables, it is nameless and cannot be relocated.


## Dotted keys create and define a table for each key part before the last one,
# provided that such tables were not previously created. Examples:

# This line also...
fruit.apple.color = "red"
# defines a table named fruit
# defines a table named fruit.apple

# Similarly, this line also...
fruit.apple.taste.sweet = true
# defines a table named fruit.apple.taste
# fruit and fruit.apple were already created


## Since tables cannot be defined more than once, redefining such tables using
# a [table] header is not allowed. Likewise, using dotted keys to redefine tables
# already defined in [table] form is not allowed. 
[fruit]
apple.color = "red"
apple.taste.sweet = true
# table named fruit, fruit.apple, fruit.apple.taste defined

# so belows are invalid:
[fruit.apple]  # INVALID
[fruit.apple.taste]  # INVALID

# The [table] form can, however, be used to define sub-tables within tables defined via dotted keys.
[fruit]
apple.color = "red"
apple.taste.sweet = true
# same as above, fruit, fruit.apple, fruit.apple.taste defined

# below add sub-table named fruit.apple.texture
[fruit.apple.texture]  # you can add sub-tables
smooth = true


## All table names must be non-empty.
[]     # INVALID
[a.]   # INVALID
[a..b] # INVALID
[.b]   # INVALID
[.]    # INVALID


################
# Inline table #
################

# Inline tables provide a more compact syntax for expressing tables.
# They are intended to appear on a single line.
inlineTables = { areEnclosedWith = "{ and }", a = { b = { c = { d = 1 } } } }
point = { x = 1, y = 2 }
usingMultiple = {
  lines = "discouraged!",
  instead = "use normal TOML tables",
}

# this inline table:
name = { first = "Tom", last = "Preston-Werner" }
# is equivalent to this standard table:
[name]
first = "Tom"
last = "Preston-Werner"


###################
# Array of Tables #
###################

## An array of tables can be expressed by using a table name in double brackets.
# Each table with the same double bracketed name will be an item in the array.
# The tables are inserted in the order encountered.

[[products]] # define array and first table element
name = "array of table"
sku = 738594937
emptyTableAreAllowed = true

[[products]] # second element is an empty table

[[products]] # third table element
name = "Nail"
sku = 284758393
color = "gray"

# The equivalent in JSON would be:
# { 
#   "products": [
#     {
#       "name": "array of table",
#       "sku": 7385594937,
#       "emptyTableAreAllowed": true
#     },
#     {},
#     {
#       "name": "Nail",
#       "sku": 284758393,
#       "color": "gray"
#     }
#   ]
# }


## You can create nested arrays of tables as well. Each double-bracketed
# sub-table will belong to the nearest table element above it.
# 

[[fruit]]
  name = "apple" # I am a property in fruit table/map

  [fruit.geometry]
    shape = "round"
    note = "I am a property in geometry table/map"

  [[fruit.color]]
    name = "red"
    note = "I am an array item in apple fruit's table/map"

  [[fruit.color]]
    name = "green"
    note = "I am in the same array as red"

[[fruit]]
  name = "banana"

  [[fruit.color]]
    name = "yellow"
    note = "I am an array item in banana fruit's table/map"

# According to spec, indentation is treated as whitespace and ignored.
# Here is just for better demonstration.

# The equivalent in JSON would be:
# {
#   "fruit": [
#     {
#       "name": "apple",
#       "geometry": { "shape": "round", "note": "..."},
#       "color": [
#         { "name": "red", "note": "..." },
#         { "name": "green", "note": "..." }
#       ]
#     },
#     {
#       "name": "banana",
#       "color": [
#         { "name": "yellow", "note": "..." }
#       ]
#     }
#   ]
# }


## The following TOML is invalid
# this table by itself is subtable, but what unclear is its parent element type
[fruit.physical]  
color = "red"
shape = "round"

# and if this array of tables definition follows
# parser will complain the key fruit is already defined
[[fruit]] 
name = "apple"

# But otherwise this TOML would be valid
# array of tables comes first
[[fruit]]
name = "apple"

# the following is array's first element
[fruit.physical]
color = "red"
shape = "round"

# As spec explained:
# If the parent of a table or array of tables is an array element, that element
# must already have been defined before the child can be defined.
# Use TOML/JSON Online converter to get the hang of it.

```


### More Resources

+ [TOML official repository](https://github.com/toml-lang/toml)
