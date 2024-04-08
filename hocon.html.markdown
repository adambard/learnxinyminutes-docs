---
language: hocon
filename: learnhocon.conf
contributors:
- [TehBrian, 'https://tehbrian.xyz']
---

Human-Optimized Configuration Object Notation, or HOCON, is a configuration and
data serialization format designed to be easily editable by humans.

It's a superset of JSON, meaning that any valid JSON is valid HOCON, but it
differs in being much less pedantic and opinionated. With its flexible yet
easily determinable syntax, resulting configuration files are often much less
noisy than some other formats.

Additionally, its support for comments makes it much better suited for
user-facing configurations than JSON.

```
// Comments can either look like this,
# or they can look like this.
// Anything after // or # is a comment.

##################
### THE BASICS ###
##################

# Everything in HOCON is either a key, a value, or a separator.
# : and = are separators. They separate the key from the value.
key: value
another_key = another_value

# You can use either separator with or without whitespace on either side.
colon1:value
colon2: value
colon3 : value
equals1=value
equals2= value
equals3 = value
# As you'll see, HOCON is very nonrestrictive regarding its syntax style.

# HOCON also isn't opinionated on how keys look.
THIS_IS_A_VALID_KEY: value
this-is-also-a-valid-key: value
keys can have spaces too: value
or even numbers like 12345: value
"you can even quote keys if you'd like!": value

# A key, followed by any separator, and then finally a value, is called a field.
this_entire_line_is: a field

###################
### VALUE TYPES ###
###################

# The types that a value can be are string, number, object, array, boolean, and
# null. Every value type except for array and object are called simple values.

## SIMPLE VALUES ##

quoted_string: "I like quoting my strings."
unquoted_string: I don't like quoting my strings.
# Special characters that cannot be used in unquoted strings are:
# $ " { } [ ] : = , + # ` ^ ? ! @ * &
# Unquoted strings do not support any kind of escaping. If using one of those
# special characters is desired, use a quoted string.
multi-line_string: """
  This entire thing is a string!
  One giant, multi-line string.
  You can put 'single' and "double" quotes without it being invalid.
"""

number: 123
negative: -123
fraction: 3.1415926536
scientific_notation: 1.2e6 // same as 1.2 * (10^6)

boolean: true # or false
empty: null

## ARRAYS ##

# Arrays hold lists of values.
# Values in arrays can be separated with commas..
array: [ 1, 2, 3, 4, 5 ]
fibonacci: [1,1,2,3,5,8,13]
multiples_of_5: [5, 10, 15, 20,] # Notice the trailing comma. That's okay here.
# or newlines..
friends: [
  "Brian"
  "Sophie"
  "Maya"
  "Sabina"
]
# or both!
ingredients: [
  "Egg",
  "Sugar",
  "Oil",
  "Flour", # Notice the trailing comma. That's okay here too.
]
# Once again, HOCON has a very loose syntax. Use whichever style you prefer.
no newline before or after bracket: ["This"
  "is"
  "an"
  "array!"]

# Just like any other value, arrays can hold other arrays.
array in array: [ [1, 2, 3], ["a", "b", "c"] ]
array in array in array: [ [ [1, 2], [8, 9] ], [ ["a", "b" ], ["y", "z"] ] ]

## OBJECTS ##

# Objects hold fields.
# Just like arrays, fields in objects can be separated with commas..
object: { key: value, another_key: another_value }
server_connection: {ip: "127.0.0.1", port: 80}
first: {letter: a, number: 1,} # Notice the trailing comma.
# or newlines..
power_grid: {
  max_capacity: 15000
  current_power: 1200
}
# or both!
food_colors: {
  carrot: orange,
  pear: green,
  apple: red,
  plum: purple,
  banana: yellow, # Trailing commas are okay here too!
}

# Arrays can hold objects just like any other value type.
coworkers: [
  {
    name: Jeff
    age: 27
  },
  {
    name: Henry
    age: 35
  },
  {
    name: Timmy
    age: 12
  }
]

# The field separator may be omitted if the key is followed by {
no_separator {
  key: value
  speed_of_light: very fast
  ten: 10

  # Objects can go inside other objects just like any other value.
  another_object {
    twenty: 20
    speed_of_sound: also pretty fast
  }
}

# In fact, the entirety of any HOCON document is an actually just an object.
# That object is called the root object. The only difference between it and any
# other object is that the curly brackets at the top and bottom of the document
# may be omitted.

# This means that HOCON documents can be formatted in the same way that
# regular objects can be formatted, including separating fields with commas
# rather than with newlines.

# Additionally, while the entirety of a HOCON document can be and is usually an
# object, it can also be an array. If it is an array, the opening and closing
# brackets at the top and bottom of the document must be explicitly written.

######################
### DUPLICATE KEYS ###
######################

is_happy: false
# If there is a duplicate key, the new value overrides the previous value.
is_happy: true
online_users: [Jacob, Mike]
# Same with arrays.
online_users: [Jacob, Mike, Henry]

# For objects, it's a bit different.
my_car: {
  color: blue
  speed: 9001
  passengers: null

  engine: {
    running: true
    temperature: 137
  }
}
# If there is a duplicate key and both values are objects,
# then the objects are merged.
my_car: {
  // These fields are added to the old, previous object.
  nickname: "My Favorite Car"
  type: 2-door sedan

  // Since the value of this duplicate key is NOT an object,
  // it simply overrides the previous value.
  speed: 60
  // Same with arrays. They override, not merge.
  passengers: ["Nate", "Ty"]

  // This object is recursively merged with the other object.
  engine: {
    // These two fields are added to the previous object.
    type: gas
    oil_level: 10
    // This field overrides the previous value.
    temperature: 179
  }
}

# Object merging is done two at a time. That is to say, the first two objects
# merge into one, then that object merges with the next object, and so on.

# Because of this, if you set a field with an object value to a non-object value
# and then back to an object value, the new object will completely override any
# previous value.

// Null, a non-object value, completely overrides the object value.
my_car: null

// Then, this object completely overrides null.
my_car: {
  nickname: "My New Car"
  type: 4-door minivan
  color: gray
  speed: 90
  passengers: ["Ayden", "Liz"]
}

###########################
### VALUE CONCATENATION ###
###########################

## SIMPLE VALUE CONCATENATION ##

# Simple values (all value types except objects and arrays) separated by
# whitespace are concatenated into a single string. The whitespace between
# values is preserved.
number_concatenation: 1 2 3 12.5 -3 2e5 // same as: "1 2 3 12.5 -3 2e5"
boolean_concat: true false true // "true false true"
null_concat: null null null // "null null null"
mixed_concat: 1 true null // "1 true null"

# String value concatenation can appear anywhere that a quoted string can.
number_concat_in_array: [1 2, 3 4, 5 6] // same as: ["1 2", "3 4", "5 6"]

# In fact, unquoted strings are actually just string value concatenations.
unquoted_string_concat: his name is jeff // same as: "his name is jeff"

# Going further, even keys that are unquoted strings are actually just string
# value concatenations.
this is a key: value // the KEY is the same as: "this is a key"
# The following field is identical to the field above.
"this is a key": value

# Quoted strings can also be concatenated. This will be useful later,
# when we cover substitutions.
quoted_string_concat: "her"" name" "is ""jenna" // same as: "her name is jenna"
# Notice that the whitespace (or lack thereof) between values is preserved.

## ARRAY CONCATENATION ##

# Arrays separated by whitespace are merged into a single array.
array_concat: [1, 2, 3] [4, 5, 6] // same as: [1, 2, 3, 4, 5, 6]

# Arrays cannot be concatenated with a non-array value.
//array_concat: true [false] results in an error
//array_concat: 1 [2] results in an error

## OBJECT CONCATENATION ##

# Objects separated by whitespace are merged into a single object.
# The merge functionality is identical to that of duplicate key object merging.
lamp: {on: true} {color: tan} // same as: {on: true, color: tan}

# Similarly to arrays, objects cannot be concatenated with a non-object value.
//object_concat: true {on: false} results in an error
//object_concat: 1 {number: 2} results in an error

########################
### PATH EXPRESSIONS ###
########################

# Path expressions are used to write out a path through the object graph.
# Think of it as navigating through objects to a specific field.
# Each object to traverse through is called an element, and each element is
# separated with a period.

country: {
  city: {
    neighborhood: {
      house: {
        name: "My House"
        address: 123 Example Dr.
      }
    }
  }
}
# For example, the path to the address of the house could be written as:
# country.city.neighborhood.house.address
# Country, city, neighborhood, house, and address are all elements.

# Path expressions are used in two places: substitutions (which will be
# covered in a moment), and as keys.
# That's right: keys themselves can also be path expressions.
foo: {
  bar: {
    baz: {
      number: 12
    }
  }
}
# Rather than tediously specifying each object, a path expression can be used.
# The following field represents the same object found above.
foo.bar.baz.number: 12

# Fields and objects specified with path expressions are merged in the same way
# that any object is usually merged.
foo.bar.baz.bool: true
// the object foo's value is: foo { bar { baz { number: 12, bool: true } } }

#####################
### SUBSTITUTIONS ###
#####################

# Substitutions refer to a specific value from some path expression.
# They're only allowed in values, not keys or nested inside other substitutions.

me: {
  favorite_animal: parrots
  favorite_food: cookies
}
# The syntax for a substitution is either ${path_expression} or
# ${?path_expression}. The latter syntax will be discussed in a moment.
my_fav_animal: ${me.favorite_animal}
my_fav_food: ${me.favorite_food}

# Substitutions are not parsed inside quoted strings. To get around this,
# either use an unquoted string or value concatenation.
animal_announcement: My favorite animal is ${my_fav_animal}
// the value is: My favorite animal is parrots
food_announcement: "My favorite food is "${my_fav_food}"!"
// the value is: "My favorite food is cookies!"

# Substitutions are parsed last in the document. Because of this, you can
# reference a key that hasn't been defined yet.
color_announcement: "My favorite color is" ${my_fav_color}"!"
// the value is: "My favorite color is blue!"
my_fav_color: blue

# Another effect of substitutions being parsed last is that substitutions will
# always use the latest, as in last, value assigned in the entire document,
# which includes merged objects.
color: green
their_favorite_color: ${color} // the value is: orange
color: orange

random_object: {
  number: 12
}
the_number: ${random_object.number} // the value is: 15
random_object: {
  number: 15
}

###############################
### UNDEFINED SUBSTITUTIONS ###
###############################

# A substitution using the ${path_expression} syntax with an undefined path
# expression, meaning a path expression that does not point to a defined value,
# is invalid and will therefore generate an error.
//${does.not.exist} will throw an error

# However, an undefined substitution using the ${?path_expression} syntax
# has different behavior depending on what it is the value of.
request: {
  # If it is the value of a field, then the field will not be created.
  response: ${?does.not.exist} // this field won't be created and does not exist
  type: HTTP
}

request: {
  # Additionally, if it would have overridden a previous value, then the
  # previous value remains unchanged.
  type: ${?does.not.exist} // request.type is still HTTP
}

# If it is a value in an array, then it is simply not added.
values: [ 172, "Brian", ${?does.not.exist}, null, true, ]
// the value is: [ 172, "Brian", null, true ]

# If it is part of simple value concatenation, it acts as an empty string.
final_string: "String One"${?does.not.exist}"String Two"
// the value is: "String OneString Two"

# If it is part of array concatenation, it acts as an empty array.
final_array: [ 1, 2, 3 ] ${?does.not.exist} [ 7, 8, 9 ]
// the value is: [ 1, 2, 3, 7, 8, 9 ]

# If it is part of object concatenation, it acts as an empty object.
final_array: { a: 1 } ${?does.not.exist} { c: 3 }
// the value is: { a: 1, c: 3 }

######################################
### SELF-REFERENTIAL SUBSTITUTIONS ###
######################################

# Substitutions normally "look forward" and use the final value defined in the
# document. However, in cases when this would create a cycle, the substitution
# looks only backwards.

# A field which contains a substitution that points to itself or points to
# other fields that eventually point back to itself is called a
# self-referential field.
letters: "a b c" // the value is: "a b c"
letters: ${letters}" d" // "a b c d"
letters: ${letters}" e" // "a b c d e"

PATH: [/bin] // the value is: [/bin]
PATH: ${PATH} [/usr/bin] // [/bin, /usr/bin]
PATH: ${PATH} [/usr/local/bin] // [/bin, /usr/bin, /usr/local/bin]

x: "x" // the value is: "x"
y: ${x}"y" // "xy"
x: ${y}"z" // "xyz"

##########################
### += FIELD SEPARATOR ###
##########################

# In addition to : and =, there actually exists another separator: +=
# A field separated with += acts as a self-referential array concatenation.
# In short, it appends an element to a previously defined array.

a: [1]
b: [1]
# This field:
a += 2 // the value is: [1, 2]
# functions the same as:
b: ${?b} [2] // the value is: [1, 2]

USERS: [/usr/luke] // the value is: [/usr/luke]
USERS += /usr/devon // [/usr/luke, /usr/devon]
USERS += /usr/michael // [/usr/luke, /usr/devon, /usr/michael]

# Since += only appends elements to a previously existing array, if the previous
# value was not an array, an error will be generated.
OTHER_USERS: /usr/luke
//OTHER_USERS += /usr/devon results in an error

# Notice that the underlying substitution syntax used is ${?path}, not ${path}.
# Recall that, using the ${?} syntax, an undefined substitution in array
# concatenation acts as an empty array. Because of this, it is perfectly
# acceptable if the field that is being set is initially undefined.
//z: [] not necessary
z += 3 // the value is: [3]
z += 4 // the value is: [3, 4]

NEW_USERS += /usr/sandra // the value is: [/usr/sandra]
NEW_USERS += /usr/kennedy // [/usr/sandra, /usr/kennedy]
NEW_USERS += /usr/robin // [/usr/sandra, /usr/kennedy, /usr/robin]

################
### INCLUDES ###
################

# Includes allow you to "import" one HOCON document into another.

# An include statement consists of the unquoted string "include" followed by
# whitespace and then a resource name, which is one of the following:
# - a single quoted string which is heuristically interpreted as a URL,
#   filename, or a Java classpath resource.
# - url(), file(), or classpath(), with the parentheses surrounding a quoted
#   string which is either a URL, filename, or classpath resource respectively.
# - required(), with the parentheses surrounding one of the above.
include "https://example.com/config.conf"
include "/foo/bar/config.conf"
include "config.conf"

include url("https://example.com/config.conf")
include file("/foo/bar/config.conf")
include classpath("config.conf")

# If the included file does not exist, it will be silently ignored and act as if
# it were an empty object. However, if it is wrapped around required(), then
# parsing will explicitly error if the file cannot be resolved.
//include required("doesnt_exist.conf") will error
//include required(url("https://example.com/doesnt_exist.conf")) will error
//include required(file("doesnt_exist.conf")) will error
//include required(classpath("doesnt_exist.conf")) will error

# The file specified by the include statement is called the included file, and
# the file which contains the include statement is called the including file.

# Including a file functions as if you directly replaced the include statement,
# wherever it may be, with the contents of the included file's root object.

# An included file must have an object as its root value and not an array.
# If the included file has an array as its root value, then it is invalid and
# an error will be generated.

# Pretend that the following is in a file called user_config.conf:
username: RandomUser1337
auto_login: true
color_theme: dark
screensaver: {
  image: usr/images/screensaver.jpg
  turn_on_after: 1m
}

# And then we include that file.
include file("user_config.conf")

# We can now reference values from that file!
path_to_user_screensaver: ${screensaver.image} //
greeting: "Welcome, "${username}"!" // the value is: "Welcome, RandomUser1337!"

# Duplicate keys override as they normally do.
status: "Auto Login: "${auto_login} // the value is: "Auto Login: true"
auto_login: false
status: "Auto Login: "${auto_login} // the value is: "Auto Login: false"

# Object merging is also the same as usual.
screensaver: {
  // This gets added to the screensaver object.
  enable_during_day: false
  // This overrides the previous value.
  turn_on_after: 30s
}

# Include statements can appear in place of a field. Anywhere that a field
# could appear, an include statement could appear as well.

# Pretend that the following is in a file called server_settings.conf:
max_connections: 10
url: example.com
port: 80
admin_page: {
  username: admin
  password: pass12345
}

# And then we include that file nested inside another object.
websites: {
  my_epic_website: {
    include file("server_settings.conf")
  }
}

# Now, we can reference the contents of server_settings.conf as if they
# had been written directly into the object my_epic_website.
server_port: ${websites.my_epic_website.port}

the_password: "The password is: "${websites.my_epic_website.admin_page.password}
// the value is: The password is: pass12345

max_conn: "Max Connections: "${websites.my_epic_website.max_connections}
// the value is: Max Connections: 10
```

### More Resources

+ [Official HOCON Specification](https://github.com/lightbend/config/blob/master/HOCON.md)
+ [HOCON Playground](https://hocon-playground.herokuapp.com)
