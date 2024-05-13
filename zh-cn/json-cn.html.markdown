---
language: json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
translators:
  - ["Zach Zhang", "https://github.com/checkcheckzz"]
filename: learnjson-cn.json
lang: zh-cn
---

JSON是一个极其简单的数据交换格式。按[json.org](https://json.org)说的，它对人类易读易写，对机器易解析易生成。

一段JSON可以是下文列出的类型的任意值，但实际一般按以下两种方式之一呈现：

* 一个键值对的集合(`{ }`)。按不同语言，这可能被理解为对象/记录/结构体/字典/哈希表/有键列表/关联数组
* 一个有序的值列表(`[ ]`)。按不同语言，这可能被理解为数组/向量/列表/序列

纯正的JSON实际上没有注释，但是大多数解析器都接受C-风格(//, /\* \*/)的注释。一些解析器还容许trailing comma，即最后一个数组元素或最后一个对象属性之后的逗号。不过为了兼容性最好避免。

因此，本教程的一切都会是100%有效的JSON。幸亏，它的表达能力很丰富。

支持的数据类型：

* 字符串：`"hello"`、`"\"A quote.\""`、`"\u0abe"`、`"Newline.\n"`
* 数字：`23`、`0.11`、`12e10`、`3.141e-10`、`1.23e+4`
* 对象：`{ "key": "value" }`
* 数组：`["Values"]`
* 其它：`true`、`false`、`null`

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
  , "comma position": "doesn't matter, if it's before the next key, it's valid"
  , "another comment": "how nice"
  },



  "whitespace": "Does not matter.",



  "that was short": "And done. You now know everything JSON has to offer."
}
```

## 进一步阅读

* [JSON.org](https://www.json.org/json-zh.html) 完美图解JSON的一切
* [JSON Tutorial](https://www.youtube.com/watch?v=wI1CWzNtE-M) 简要介绍
