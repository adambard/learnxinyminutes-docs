---
language: hocon
filename: learnhocon.conf
contributors:
- [TehBrian, 'https://tehbrian.xyz']
---

Human-Optimized Configuration Object Notation, or HOCON, is a configuration and
data serialization format designed to be easily editable by humans.

It's a superset of JSON, meaning that any valid JSON is valid HOCON, but it
differs in being less opinionated. With its flexible yet determinable syntax,
resulting configuration files are often less noisy than with other formats.

Additionally, its support for comments makes it better-suited for user-facing
configuration than JSON.

```
// Anything after // or # is a comment. This is a comment.
# This is also a comment.

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
# As you'll see, HOCON has a very nonrestrictive syntax.

# HOCON isn't opinionated on how keys look.
THIS_IS_A_VALID_KEY: value
this-is-also-a-valid-key: value
keys can have spaces: value
or even numbers like 12345: value
"you can even quote keys if you'd like!": value

# Keys are case sensitive.
unique: value 1
UnIqUe: value 3
UNIQUE: value 2

# A key, followed by any separator, followed by a value, is called a field.
this_entire_line_is: a field

###################
### VALUE TYPES ###
###################

# A value can be of type: string, number, object, array, boolean, null.
# Simple values are values of any type except array and object.

## SIMPLE VALUES ##

quoted_string: "I like quoting my strings."
unquoted_string: I don't like quoting my strings.
# Special characters that cannot be used in unquoted strings are:
# $ " { } [ ] : = , + # ` ^ ? ! @ * &
# Unquoted strings do not support any kind of escaping.
# To use one of those special characters in a string, use a quoted string.
multiline_string: """This entire thing is a string!
One giant, multiline string.
You can put 'single' and "double" quotes without it being invalid."""

number: 123
negative: -123
fraction: 3.1415926536
scientific_notation: 1.2e6 // 1.2 * 10^6

boolean: true # or false
empty: null

## ARRAYS ##

# Arrays hold lists of values.

# Values in arrays can be separated with commas..
array: [ 1, 2, 3, 4, 5 ]
fibonacci: [1,1,2,3,5,8,13]
multiples_of_5: [5, 10, 15, 20,] # Notice the trailing comma. That's allowed.
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
  "Flour", # Trailing comma. That's allowed here too.
]
# Once again, HOCON has a very liberal syntax. Use whichever style you prefer.

no newline before or after bracket: ["This"
  "is"
  "an"
  "array!"]

# Arrays can hold other arrays.
array in array: [ [1, 2, 3], ["a", "b", "c"] ]
array in array in array: [ [ [1, 2], [8, 9] ], [ ["a", "b" ], ["y", "z"] ] ]

## OBJECTS ##

# Objects hold fields.

# Just like arrays, fields in objects can be separated with commas..
object: { key: value, another_key: another_value }
server_connection: {ip: "127.0.0.1", port: 80}
first: {letter: a, number: 1,} # Trailing comma.
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
  banana: yellow, # Trailing comma. These pesky things show up everywhere!
}

# Arrays can hold objects.
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

  # Objects can hold other objects.
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

// Null, a non-object value, overrides the object.
my_car: null

// Then, this object overrides null.
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

# Simple values (all value types except array and object) separated by
# whitespace are concatenated into a single string. The whitespace between
# values is preserved.
number_concat: 1 2 3 12.5 -3 2e5 // "1 2 3 12.5 -3 2e5"
boolean_concat: true false true // "true false true"
null_concat: null null null // "null null null"
mixed_concat: 1 true null // "1 true null"

# String value concatenation can appear anywhere that a quoted string can.
number_concat_in_array: [1 2, 3 4, 5 6] // ["1 2", "3 4", "5 6"]

# In fact, unquoted strings are actually just string value concatenations.
unquoted_string_concat: his name is jeff // "his name is jeff"

# Going further, even keys that are unquoted strings are actually just string
# value concatenations.
this is a key: value // the KEY is: "this is a key"
# The following field is identical to the field above.
"this is a key": value

# Quoted strings can also be concatenated.
# This will be useful later, when we cover substitutions.
quoted_string_concat: "her"" name" "is ""jenna" // "her name is jenna"
# Notice that the whitespace (or lack thereof) between values is preserved.

## ARRAY CONCATENATION ##

# Arrays separated by whitespace are merged into a single array.
array_concat: [1, 2, 3] [4, 5, 6] // [1, 2, 3, 4, 5, 6]

# Arrays cannot be concatenated with a non-array value.
//array_concat: true [false] // error!
//array_concat: 1 [2] // error!

## OBJECT CONCATENATION ##

# Objects separated by whitespace are merged into a single object.
# The merge functionality is identical to that of duplicate key object merging.
lamp: {on: true} {color: tan} // {on: true, color: tan}

# Similarly to arrays, objects cannot be concatenated with a non-object value.
//object_concat: true {on: false} // error!
//object_concat: 1 {number: 2} // error!

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
# The path to the address could be written as:
# country.city.neighborhood.house.address
# Country, city, neighborhood, house, and address are all elements.

# Path expressions are used in two places: substitutions (which we'll get to
# in just a moment), and as keys. That's right: keys can be path expressions.
foo: {
  bar: {
    baz: {
      number: 12
    }
  }
}
# Rather than tediously specifying each object, a path expression could be used.
# The following field represents the same object.
foo.bar.baz.number: 12

# Fields and objects specified with path expressions are merged in the same way
# that any object is usually merged.
foo.bar.baz.bool: true
// the object foo's value is: foo { bar { baz { number: 12, bool: true } } }

#####################
### SUBSTITUTIONS ###
#####################

# Substitutions refer to a specific value from some path expression.
# They're only allowed in values, not in keys or nested in other substitutions.

me: {
  favorite_animal: parrots
  favorite_food: cookies
}
# There are two syntaxes for substitutions:
# ${path_expression} and ${?path_expression}.
# The latter syntax will be covered in a moment.
my_fav_animal: ${me.favorite_animal}
my_fav_food: ${me.favorite_food}

# Substitutions are not parsed inside quoted strings. To get around this,
# either use an unquoted string or value concatenation.
animal_announcement: My favorite animal is ${my_fav_animal}
// "My favorite animal is parrots"
food_announcement: "My favorite food is "${my_fav_food}"!"
// "My favorite food is cookies!"

# Substitutions are parsed last in the document. Because of this, you can
# reference a key that hasn't been defined yet.
color_announcement: "My favorite color is" ${my_fav_color}"!"
// "My favorite color is blue!"
my_fav_color: blue

# Another effect of substitutions being parsed last is that substitutions will
# always use the latest, as in last, value assigned in the entire document.
color: green
their_favorite_color: ${color} // orange
color: orange

# This includes merged objects.
random_object: {
  number: 12
}
the_number: ${random_object.number} // 15
random_object: {
  number: 15
}

###############################
### UNDEFINED SUBSTITUTIONS ###
###############################

# A substitution using the ${path_expression} syntax with an undefined path
# expression, meaning a path expression that does not point to a defined value,
# is invalid and will therefore generate an error.
//${does.not.exist} // error!

# However, an undefined substitution using the ${?path_expression} syntax
# has different behavior depending on what it is the value of.
request: {
  # If it is the value of a field, then the field won't be created.
  response: ${?does.not.exist} // this field does not exist
  type: HTTP
}

request: {
  # Additionally, if it would have overridden a previous value, then the
  # previous value remains unchanged.
  type: ${?does.not.exist} // request.type is still HTTP
}

# If it is a value in an array, then it is simply not added.
values: [ 172, "Brian", ${?does.not.exist}, null, true, ]
// [ 172, "Brian", null, true ]

# If it is part of simple value concatenation, it acts as an empty string.
final_string: "String One"${?does.not.exist}"String Two"
// "String OneString Two"

# If it is part of array concatenation, it acts as an empty array.
final_array: [ 1, 2, 3 ] ${?does.not.exist} [ 7, 8, 9 ]
// [ 1, 2, 3, 7, 8, 9 ]

# If it is part of object concatenation, it acts as an empty object.
final_object: { a: 1 } ${?does.not.exist} { c: 3 }
// { a: 1, c: 3 }

######################################
### SELF-REFERENTIAL SUBSTITUTIONS ###
######################################

# Substitutions normally "look forward" and use the final value defined in the
# document. However, in cases when this would create a cycle, the substitution
# looks only backwards.

# A field that contains a substitution that points to itself or points to
# other fields that eventually point back to itself is called a
# self-referential field.
letters: "a b c" // "a b c"
letters: ${letters}" d" // "a b c d"
letters: ${letters}" e" // "a b c d e"

PATH: [/bin] // [/bin]
PATH: ${PATH} [/usr/bin] // [/bin, /usr/bin]
PATH: ${PATH} [/usr/local/bin] // [/bin, /usr/bin, /usr/local/bin]

x: "x" // "x"
y: ${x}"y" // "xy"
x: ${y}"z" // "xyz"

##########################
### += FIELD SEPARATOR ###
##########################

# In addition to : and =, there actually exists another separator: +=
# A field separated with += implies self-referential array concatenation.
# Essentially, it appends an element to a previously defined array.

a: [1]
b: [1]
# These two fields are equivalent.
a += 2 // [1, 2]
b: ${?b} [2] // [1, 2]

USERS: [/usr/luke] // [/usr/luke]
USERS += /usr/devon // [/usr/luke, /usr/devon]
USERS += /usr/michael // [/usr/luke, /usr/devon, /usr/michael]

# Since += only appends elements to a previously existing array, if the previous
# value was not an array, an error will be generated.
OTHER_USERS: /usr/luke
//OTHER_USERS += /usr/devon // error!

# The underlying substitution syntax used is ${?path}, not ${path}.
# Recall that, using the ${?} syntax, an undefined substitution in array
# concatenation acts as an empty array. Because of this, it is perfectly
# acceptable if the field that is being set is initially undefined.
//z: [] // not necessary
z += 3 // [3]
z += 4 // [3, 4]

NEW_USERS += /usr/sandra // [/usr/sandra]
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
//include required("doesnt_exist.conf") // error!
//include required(url("https://example.com/doesnt_exist.conf")) // error!
//include required(file("doesnt_exist.conf")) // error!
//include required(classpath("doesnt_exist.conf")) // error!

# The file specified by the include statement is called the included file.
# The file containing the include statement is called the including file.

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

# Then, we include that file.
include file("user_config.conf")

# We can now reference values from that file!
path_to_user_screensaver: ${screensaver.image} // "usr/images/screensaver.jpg"
greeting: "Welcome, "${username}"!" // "Welcome, RandomUser1337!"

# Duplicate keys override as they normally do.
status: "Auto Login: "${auto_login} // "Auto Login: true"
auto_login: false
status: "Auto Login: "${auto_login} // "Auto Login: false"

# Object merging is the same as usual.
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

# Then, we include that file nested inside an object.
websites: {
  my_epic_website: {
    include file("server_settings.conf")
  }
}

# Now, we can reference the contents of server_settings.conf as if they
# had been written directly into the object my_epic_website.
server_port: ${websites.my_epic_website.port}

the_password: "The password is: "${websites.my_epic_website.admin_page.password}
// "The password is: pass12345"

max_conn: "Max Connections: "${websites.my_epic_website.max_connections}
// "Max Connections: 10"
```

### More Resources

+ [Official HOCON Specification](https://github.com/lightbend/config/blob/master/HOCON.md)
+ [HOCON Playground](https://hocon-playground.tehbrian.dev)
