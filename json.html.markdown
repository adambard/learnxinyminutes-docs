---
language: json
filename: learnjson.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["himanshu", "https://github.com/himanshu81494"]
  - ["Michael Neth", "https://github.com/infernocloud"]
---

As JSON is an extremely simple data-interchange format, this is most likely going to be the simplest Learn X in Y Minutes ever.

JSON in its purest form has no actual comments, but most parsers will accept C-style (`//`, `/* */`) comments. Some parsers also tolerate a trailing comma (i.e. a comma after the last element of an array or the after the last property of an object), but they should be avoided for better compatibility.

For the purposes of this, however, everything is going to be 100% valid JSON. Luckily, it kind of speaks for itself.

A JSON value must be a number, a string, an array, an object, or one of the following 3 literal names: true, false, null.

Supporting browsers are: Firefox 3.5+, Internet Explorer 8.0+, Chrome 1.0+, Opera 10.0+, and Safari 4.0+.

File extension for JSON files is ".json" and the MIME type for JSON text is "application/json".

Many programming languages have support for serializing (encoding) and unserializing (decoding) JSON data into native data structures. Javascript has implicit support for manipulating JSON text as data.

More information can be found at http://www.json.org/

JSON is built on two structures:
* A collection of name/value pairs. In various languages, this is realized as an object, record, struct, dictionary, hash table, keyed list, or associative array.
* An ordered list of values. In most languages, this is realized as an array, vector, list, or sequence.

An object with various name/value pairs.

```json
{
  "key": "value",

  "keys": "must always be enclosed in double quotes",
  "numbers": 0,
  "strings": "Hellø, wørld. All unicode is allowed, along with \"escaping\".",
  "has bools?": true,
  "nothingness": null,

  "big number": 1.2e+100,

  "objects": {
    "comment": "Most of your structure will come from objects.",

    "array": [0, 1, 2, 3, "Arrays can have anything in them.", 5],

    "another object": {
      "comment": "These things can be nested, very useful."
    }
  },

  "silliness": [
    {
      "sources of potassium": ["bananas"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "alternative style": {
    "comment": "check this out!"
  , "comma position": "doesn't matter - as long as it's before the next key, then it's valid"
  , "another comment": "how nice"
  }
}
```

A single array of values by itself is also valid JSON.

```json
[1, 2, 3, "text", true]
```

Objects can be a part of the array as well.

```json
[{"name": "Bob", "age": 25}, {"name": "Jane", "age": 29}, {"name": "Jack", "age": 31}]
```

Whitespace characters are ignored by JSON parsers which means that all spaces, tabs and newline characters can be stripped to minimize file size.
