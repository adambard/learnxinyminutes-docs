---
language: asciidoc
filename: asciidoc-cn.adoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
    - ["Abel Salgado Romero", "https://twitter.com/abelsromero"]
translators:
    - ["Liu Yihua", "https://github.com/yihuajack"]
lang: zh-cn
---

AsciiDoc 是一种类似于 Markdown 的标记语言，它可以用来写从书到博客的任何东西。它由 Stuart Rackham 在 2002 年发明，它的语言很简单，但同时也可以有丰富的定制。

文档标题

标题是可选的，且不可以包含空行。它必须离正文空出至少一个空行。

仅标题

```
= 文档标题

文档的第一句话。
```

标题和作者

```
= 文档标题
张三 <san.zhang@learnxinyminutes.com>

文档开始。
```

多名作者的情形

```
= 文档标题
李四 <si@go.com>; 王五 <wu@yo.com>; 赵六 <xzhao@pirate.com>

有多名作者的文档开始。
```

版本行（需要作者行）

```
= 文档标题（第一版）
土豆人 <chip@crunchy.com>
v1.0, 2016-01-13

这篇关于炸薯条的文章会很有趣。
```

段落

```
段落不需要什么特别操作。

在两段之间用一个空行隔开。

当你需要换行时，添加一个 +
你就会得到一个换行符！
```

文本格式化

```
_用下划线创建斜体_
*用星号加粗*
*_组合起来用更有趣_*
`用重音符显示等宽字体`
`*加粗等宽字体*`
```

节标题

```
= 第 0 级 （一般只用于文档标题）

== 第 1 级 <h2>

=== 第 2 级 <h3>

==== 第 3 级 <h4>

===== 第 4 级 <h5>
```

列表

用星号创建无序列表。

```
* 甲
* 乙
* 丙
```

用句点创建有序列表。

```
. 项目 1
. 项目 2
. 项目 3
```

你可以用额外的星号或句点来嵌套最多五次列表。

```
* 甲 1
** 甲 2
*** 甲 3
**** 甲 4
***** 甲 5

. 甲 1
.. 甲 2
... 甲 3
.... 甲 4
..... 甲 5
```

## 补充材料

处理 AsciiDoc 文档有两种工具：

1. [AsciiDoc](http://asciidoc.org/): 原版的 Python 实现，在主流 Linux 发行版中已附带，目前处于稳定版本维护模式。
2. [Asciidoctor](http://asciidoctor.org/): 使用 Ruby 的另一种实现，也可以从 Java 和 JavaScript 中使用。它处于积极的开发中，目标是用新特性和输出格式扩展 AsciiDoc 的语法。

以下是 `Asciidoctor` 实现的相关链接：

* [Markdown - AsciiDoc 语法比较](http://asciidoctor.org/docs/user-manual/#comparison-by-example)：并列比较一般 Markdown 和 AsciiDoc 的元素。
* [入门](http://asciidoctor.org/docs/#get-started-with-asciidoctor)：安装和快速启动指南，帮助构建简单的文档。
* [Asciidoctor 用户手册](http://asciidoctor.org/docs/user-manual/): 完整的单文档指南，包含语法参考、示例、渲染工具等。
