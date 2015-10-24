---
language: json
filename: learnjson.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["himanshu", "https://github.com/himanshu81494"]
---

As JSON is an extremely simple data-interchange format, this is most likely going
to be the simplest Learn X in Y Minutes ever.

JSON in its purest form has no actual comments, but most parsers will accept
C-style (`//`, `/* */`) comments. Some parsers also tolerate a trailing comma
(i.e. a comma after the last element of an array or the after the last property of an object),
but they should be avoided for better compatibility.

For the purposes of this, however, everything is going to be 100% valid JSON. Luckily, it kind of speaks for itself.

Data types supported by JSON includes: numbers, string, boolean, array, object and null.
Supporting browsers are: Firefox(Mozilla) 3.5, Internet Explorer 8, Chrome, Opera 10, Safari 4.
JSON file type for JSON files is ".json". The MIME type for JSON text is "application/json"
Drawbacks of JSON include lack of type definition and some sort of DTD.

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
  , "comma position": "doesn't matter - as long as it's before the value, then it's valid"
  , "another comment": "how nice"
  },

  "that was short": "And, you're done. You now know everything JSON has to offer."
}
```
