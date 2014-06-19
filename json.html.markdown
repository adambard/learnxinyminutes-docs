---
language: json
filename: learnjson.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
---

As JSON is an extremely simple data-interchange format, this is most likely going
to be the simplest Learn X in Y Minutes ever.

JSON in its purest form has no actual comments, but most parsers will accept
C-style (//, /\* \*/) comments. For the purposes of this, however,  everything is
going to be 100% valid JSON. Luckily, it kind of speaks for itself.

```json
{
  "key": "value",
  
  "keys": "must always be enclosed in quotes (either double or single)",
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
  , "comma position": "doesn't matter - as long as its before the value, then its valid"
  , "another comment": "how nice"
  },

  "that was short": "And, you're done. You now know everything JSON has to offer."
}
```
