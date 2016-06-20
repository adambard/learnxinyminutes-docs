---
language: toml
filename: learntoml.toml
contributors:
  - ["Alois de Gouvello", "https://github.com/aloisdg"]
---

TOML stands for Tom's Obvious, Minimal Language. It is a data serialisation language designed to be a minimal configuration file format that's easy to read due to obvious semantics.

It is an alternative of YAML and JSON. It aims to be more human friendly than JSON and simpler that YAML. TOML is designed to map unambiguously to a hash table. TOML should be easy to parse into data structures in a wide variety of languages.

```toml
# Comments in TOML look like this.

################
# SCALAR TYPES #
################

# Our root object (which continues for the entire document) will be a map,
# which is equivalent to a dictionary, hash or object in other languages.

# The key, equals sign, and value must be on the same line
# (though some values can be broken over multiple lines).
key = "value"
string = "hello"
number = 42
float = 3.14
boolean = true
dateTime = 1979-05-27T07:32:00-08:00 #! Array, or Inline Table.
scientific notation = 1e+12
"key can be quoted" = true # Both " and ' are fine
"key may contains" = "letters, numbers, underscores, and dashes"

# A bare key must be non-empty, but an empty quoted key is allowed
"" = "blank"     # VALID but discouraged
'' = 'blank'     # VALID but discouraged

##########
# String #
##########

# All strings must contain only valid UTF-8 characters.
# We can escape and some characters have a compact escape sequence.
# For example, \t add a tabulation. Refers to spec to get all of them.
basicString = "are surrounded by quotation marks. \"I'm quotable\". Name\tJos"

multiLineString = """
are surrounded by three quotation marks
on each side and allow newlines."""

literalString = 'are surrounded by single quotes. Escaping are not allowed.'

MultiLineLiteralString = """
are surrounded by three single quotes on each side
and allow newlines. Still no escaping.
The first newline is trimmed in raw strings.
   All other whitespace
   is preserved. #! are preserved?
"""

# For binary data it is recommended that you use Base64, another ASCII or UTF8
# encoding. The handling of that encoding will be application specific.

###########
# Integer #
###########

## Integers can start with a +, a - or nothing.
## Leading zeros are not allowed. Hex, octal, and binary forms are not allowed.
## Values that cannot be expressed as a series of digits are not allowed.
int1 = +42
int2 = 0
int3 = -21
integerRange = 64

## You can use underscores to enhance readability. Each
## underscore must be surrounded by at least one digit.
int4 = 5_349_221
int5 = 1_2_3_4_5     # VALID but discouraged

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

date1 = 1979-05-27T07:32:00Z # follows the RFC 3339 spec
date2 = 1979-05-27T07:32:00 # without offset
date3 = 1979-05-27 # without offset nor time

####################
# COLLECTION TYPES #
####################

#! todo

#######################
# EXTRA YAML FEATURES #
#######################

#! todo

```

### More Resources

+ [TOML official repository](https://github.com/toml-lang/toml)