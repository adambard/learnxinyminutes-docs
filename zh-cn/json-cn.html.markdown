---
language: json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
translators:
  - ["Zach Zhang", "https://github.com/checkcheckzz"]
filename: learnjson-cn.json
lang: zh-cn
---

因为JSON是一个极其简单的数据交换形式，这个最有可能将会是曾经最简单
的Learn X in Y Minutes。

最纯正形式的JSON没有实际的注解，但是大多数解析器将会
接受C-风格(//, /\* \*/)的注解。为了这个目的，但是，
一切都将会是100%有效的JSON。幸亏，它是不言自明的。

```json
{
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

  "that was short": "And, you're done. You now know everything JSON has to offer."
}
```
