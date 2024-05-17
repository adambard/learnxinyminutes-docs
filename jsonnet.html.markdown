---
language: jsonnet
filename: learnjsonnet.jsonnet
contributors:
  - ["Huan Wang", "https://github.com/fredwangwang"]
---

Jsonnet is a powerful templating language for JSON. Any valid JSON
document is a valid Jsonnet object. For an interactive demo/tutorial,
click [here](https://jsonnet.org/learning/tutorial.html)

```python
// single line comment

/*
    multiline comment
*/

# as well as python style comment

# define a variable.
# Variables have no effect in the generated JSON without being used.
local num1 = 1;
local num2 = 1 + 1;
local num3 = 5 - 2;
local num4 = 9 % 5;
local num5 = 10 / 2.0;
# jsonnet is a lazy language, if a variable is not used, it is not evaluated.
local num_runtime_error = 1 / 0;

# fields are valid identifiers without quotes
local obj1 = { a: 'letter a', B: 'letter B' };

local arr1 = ['a', 'b', 'c'];

# string literals use " or '.
local str1 = 'a' + 'B';
# multiline text literal in between |||
# Each line must start with a white space.
local str_multiline = |||
  this is a
  multiline string
|||;
# Python-compatible string formatting is available via %
# When combined with ||| this can be used for templating text files.
local str_templating = |||
  %(f1)0.3f
||| % { f1: 1.2345678 };
assert str_templating == '1.235\n';

# if b then e else e. The else branch is optional and defaults to null
local var1 = if 3 < 2 then "YES";
assert var1 == null;

local obj2 = {
  # variable defined inside the object ends with ','
  local var_in_obj = 0,

  local vowels = ['a', 'e', 'i', 'o', 'u'],
  local numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],

  # [num] to look up an array element
  first_vowel: vowels[0],
  # can also slice the array like in Python
  even_numbers: numbers[1::2],

  # python-style list and object comprehensions are also supported
  double_numbers: [x * 2 for x in numbers],
  even_numbers_map: {
      # [ ] syntax in field name is to compute the field name dynamically
      [x + '_is_even']: true for x in numbers if x % 2 == 0
  },

  nested: {
    nested_field1: 'some-value',
    # self refers to the current object
    # ["field-name"] or .field-name can be used to look up a field
    nested_field2: self.nested_field1,
    nested_field3: self.nested_field1,
    # $ refers to outer-most object
    nested_field4: $.first_vowel,

    assert self.nested_field1 == self.nested_field2,
    assert self.nested_field1 == self.nested_field3,
  },

  special_field: 'EVERYTHING FEELS BAD',
};

local obj3 = {
  local var_in_obj = 1.234,
  local var_in_obj2 = { a: { b: 'c' } },

  concat_array: [1, 2, 3] + [4],
  # strings can be concat with +,
  # which implicitly converts one operand to string if needed.
  concat_string: '123' + 4,

  # == tests deep equality
  equals: { a: { b: 'c', d: {} } } == var_in_obj2,

  special_field: 'this feels good',
};

# objects can be merged with + where the right-hand side wins field conflicts
local obj4 = obj2 + obj3;
assert obj4.special_field == 'this feels good';

# define a function
# functions have positional parameters, named parameters, and default arguments
local my_function(x, y, z=1) = x + y - z;
local num6 = my_function(7, 8, 9);
local num7 = my_function(8, z=10, y=9);
local num8 = my_function(4, 5);
# inline anonymous function
local num9 = (function(x) x * x)(3);

local obj5 = {
  # define a method
  # fields defined with :: are hidden, which does not apper in generated JSON
  # function cannot be serialized so need to be hidden
  # if the object is used in the generated JSON.
  is_odd(x):: x % 2 == 1,
};
assert obj5 == {};

# a jsonnet document has to evaluate to something
# be it an object, list, number or just string literal
"FIN"
```

## Further Reading
There are a few but important concepts that are not touched in this example, including:

- Passing variables from command line: [Parameterize Entire Config](https://jsonnet.org/learning/tutorial.html#parameterize-entire-config)
- Import other jsonnet libraries/files: [Imports](https://jsonnet.org/learning/tutorial.html#imports)
- In depth example of OOP aspect of Jsonnet: [Object-Orientation](https://jsonnet.org/learning/tutorial.html#Object-Orientation)
- Useful standard library: [Stdlib](https://jsonnet.org/ref/stdlib.html)
