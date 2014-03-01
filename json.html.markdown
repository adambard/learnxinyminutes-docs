---
language: json
filename: learnjson.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
---

                                                                                0
As JSON is an extremely simple data-interchange format, this is most likely going
to be the simplest Learn X in Y Minutes ever.

JSON in its purest form has no actual comments, but most parsers will accept
C-style (//, /\* \*/) comments. For the purposes of this, everything is going to
be 100% valid JSON. Luckily, it kind of speaks for itself.

```json
{
  "numbers": 0,
  "strings": "Hellø, wørld. All unicode is allowed.",
  "has bools?": true,
  "nothingness": null,

  "big number": 1.2e+100,

  "objects": {
    "comment": "Most of your structure will come from objects.",

    "array": [0, 1, 2, 3, "banana", 5],

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

  "that was short": "And, you're done. You know know everything JSON has to offer."
}
```
