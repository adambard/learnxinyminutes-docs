---
language: json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
translators:
  - ["Zach Zhang", "https://github.com/checkcheckzz"]
filename: learnjson-cn.json
lang: zh-cn
---

因为JSON是一个极其简单的数据交换格式，本教程最有可能成为有史以来最简单的
Learn X in Y Minutes。

纯正的JSON实际上没有注释，但是大多数解析器都
接受C-风格(//, /\* \*/)的注释。为了兼容性，最好不要在其中写这样形式的注释。

因此，本教程的一切都会是100%有效的JSON。幸亏，它的表达能力很丰富。

支持的数据类型：

- 字符串: "hello", "\"A quote.\"", "\u0abe", "Newline.\n"
- 数字: 23, 0.11, 12e10, 3.141e-10, 1.23e+4
- 对象: { "key": "value" }
- 数组: ["Values"]
- 其他: true, false, null

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
