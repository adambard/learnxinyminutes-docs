---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
translators:
    - ["Fangzhou Chen"]
filename: markdown.md
lang: zh-cn
---

Markdown 由 John Gruber 于 2004年创立. 它旨在成为一门容易读写的语法结构，并可以便利地转换成HTML（以及其他很多）格式。

欢迎您多多反馈以及分支和请求合并。


```
<!-- Markdown 是HTML的父集，所以任何HTML文件都是有效的Markdown。
这意味着我们可以在Markdown里使用任何HTML元素，比如注释元素，
并不会被Markdown解析器所影响。不过如果你在Markdown文件内创建了HTML元素，
你将无法在HTML元素的内容中使用Markdown语法。-->

<!-- 在不同的解析器中，Markdown的实现方法有所不同。此教程会指出当某功能是否通用及是否只对某一解析器有效。 -->

<!-- 标头 -->
<!-- 通过在文本前加上不同数量的hash(#), 你可以创建相对应的 <h1> 到 <h6> HTML元素。 -->
# 这是一个 <h1>
## 这是一个 <h2>
### 这是一个 <h3>
#### 这是一个 <h4>
##### 这是一个 <h5>
###### 这是一个 <h6>

<!-- 对于 <h1> 和 <h2> 元素，Markdown 额外提供了两种添加方式。 -->
这是一个 h1
=============

这是一个 h2
-------------

<!-- 简易文本样式 -->
<!-- 文本的斜体，粗体，和删除线在Markdown中可以轻易的被实现。-->

*此文本为斜体。*
_此文本也是。_

**此文本为粗体。**
__此文本也是__

***此文本是斜体加粗体。***
**_或者这样。_**
*__这个也是！__*

<!-- 在Github采用的Markdown中 -->

~~此文本为删除线效果。~~

<!-- 单个段落由一句或多句邻近的句子组成，这些句子由一个或多个空格分隔。 -->

这是第一段落. 这句话在同一个段落里，好玩么？

现在我是第二段落。
这句话也在第二段落！

这句话在第三段落！

<!-- 如果你插入一个HTML中的<br />标签，你可以在段末加入两个以上的空格，
然后另起一段。-->

此段落结尾有两个空格（选中以显示）。      

上文有一个 <br /> ！

<!-- 段落引用可由 > 字符轻松实现。 -->

> 这是一个段落引用. 你可以
> 手动断开你的句子，然后在每句句子前面添加 “>” 字符。或者让你的句子变得很长，以至于他们自动得断开。
> 只要你的文字以“>” 字符开头，两种方式无异。

> 你也对文本进行
>> 多层引用
> 这多机智啊！

<!-- 序列 -->
<!-- 无序序列可由星号，加号或者减号来建立 -->

* 项目
* 项目
* 另一个项目

或者

+ 项目
+ 项目
+ 另一个项目

或者 

- 项目
- 项目
- 最后一个项目

<!-- 有序序列可由数字加点来实现 -->

1. 项目一
2. 项目二
3. 项目三

<!-- 即使你的标签数字有误，Markdown依旧会呈现出正确的序号，不过这并不是一个好主意-->

1. 项目一
1. 项目二
1. 项目三
<!-- (此段与前例一模一样) -->

<!-- 你也可以使用子序列 -->

1. 项目一
2. 项目二
3. 项目三
    * 子项目
    * 子项目
4. 项目四

<!-- 代码段落 -->
<!-- 代码段落（HTML中<code>标签）可以由缩进四格（spaces）或者一个标签页（tab）实现-->

    This is code
    So is this

<!-- 在你的代码中，你仍然使用tab可以进行缩进操作 -->

    my_array.each do |item|
        puts item
    end

<!-- 内联代码可由反引号 ` 实现 -->

John didn't even know what the `go_to()` function did!

<!-- In Github Flavored Markdown, you can use a special syntax for code -->

\`\`\`ruby <!-- except remove those backslashes when you do this, just ```ruby ! -->
def foobar
    puts "Hello world!"
end
\`\`\` <!-- here too, no backslashes, just ``` -->

<-- The above text doesn't require indenting, plus Github will use syntax
highlighting of the language you specify after the ``` -->

<!-- Horizontal rule (<hr />) -->
<!-- Horizontal rules are easily added with three or more asterisks or hyphens,
with or without spaces. -->

***
---
- - - 
****************

<!-- Links -->
<!-- One of the best things about markdown is how easy it is to make links. Put
the text to display in hard brackets [] followed by the url in parentheses () -->

[Click me!](http://test.com/)

<!-- You can also add a link title using quotes inside the parentheses -->

[Click me!](http://test.com/ "Link to Test.com")

<!-- Relative paths work too. -->

[Go to music](/music/).

<!-- Markdown also supports reference style links -->

[Click this link][link1] for more info about it!
[Also check out this link][foobar] if you want to.

[link1]: http://test.com/ "Cool!"
[foobar]: http://foobar.biz/ "Alright!"

<!-- The title can also be in single quotes or in parentheses, or omitted
entirely. The references can be anywhere in your document and the reference IDs
can be anything so long as they are unique. -->

<!-- There is also "implicit naming" which lets you use the link text as the id -->

[This][] is a link.

[this]: http://thisisalink.com/

<!-- But it's not that commonly used. -->

<!-- Images -->
<!-- Images are done the same way as links but with an exclamation point in front! -->

![This is hover-text (alt text) for my image](http://imgur.com/myimage.jpg "An optional title")

<!-- And reference style works as expected -->

![This is the hover-text.][myimage]

[myimage]: relative/urls/cool/image.jpg "if you need a title, it's here"

<!-- Miscellany -->
<!-- Auto-links -->

<http://testwebsite.com/> is equivalent to
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Auto-links for emails -->

<foo@bar.com>

<!-- Escaping characters -->

I want to type *this text surrounded by asterisks* but I don't want it to be
in italics, so I do this: \*this text surrounded by asterisks\*.

<!-- Tables -->
<!-- Tables are only available in Github Flavored Markdown and are slightly
cumbersome, but if you really want it: -->

| Col1         | Col2     | Col3          |
| :----------- | :------: | ------------: |
| Left-aligned | Centered | Right-aligned |
| blah         | blah     | blah          |

<!-- or, for the same results -->

Col 1 | Col2 | Col3
:-- | :-: | --:
Ugh this is so ugly | make it | stop

<!-- The end! -->

```

For more info, check out John Gruber's official post of syntax [here](http://daringfireball.net/projects/markdown/syntax) and Adam Pritchard's great cheatsheet [here](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
