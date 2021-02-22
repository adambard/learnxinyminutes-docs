---
language: html
filename: learnhtml-cn.html
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["zxyqwe", "https://github.com/zxyqwe"]
lang: zh-cn
---

HTML是超文本标记语言的缩写。
这门语言可以让我们为万维网创造页面。
这是一门标记语言，它允许我们用代码来指示网页上文字和数据应该如何显示。
实际上html文件是简单的文本文件。
什么是标记？标记是通过使用开始和结束标签包围数据的方法，来组织管理页面上的数据。
这些标记对它们环绕的文本有重要的意义。
和其它计算机语言意义，HTML有很多版本。这里我们将讨论HTML5。

**注意：**  你可以在类似[codepen](http://codepen.io/pen/)的网站上的教程中，尝试不同的标签和元素带来的效果，理解它们如何起效，并且逐渐熟悉这门语言。
本文主要关注HTML的语法和一些有用的小窍门。


```html
<!-- 注释要像本行一样被包围起来！ -->

<!-- #################### 标签 #################### -->
   
<!-- 下面是一个我们将要分析的HTML文件的例子。 -->

<!doctype html>
	<html>
		<head>
			<title>我的网站</title>
		</head>
		<body>
			<h1>Hello, world!</h1>
			<a href = "http://codepen.io/anon/pen/xwjLbZ">来看看这里有什么</a>
			<p>这是一个段落。</p>
			<p>这是另外一个段落。</p>
			<ul>
				<li>这是一个非计数列表的一项（项目符合列表）</li>
				<li>这是另一项</li>
				<li>这是列表中的最后一项</li>
			</ul>
		</body>
	</html>

<!-- 一个HTML文件通常开始于向浏览器表明本页面是HTML。 -->
<!doctype html>

<!-- 在这之后，由<html>开始标签作为起始。 -->
<html>

<!-- 在文件的最后会由</html>标签结束。 -->
</html>

<!-- 在最终的标签后面应该没有任何东西。 -->

<!-- 在其中（在开始标签<html>和结束标签</html>中间）我们可以看到： -->

<!-- 由标签<head>定义的头部 （头部必须被</head>标签关闭）。 -->
<!-- 头部包含一些不显示的描述和额外信息；这些是元数据。 -->

<head>
	<title>我的网站</title><!-- <title>标签告诉浏览器在浏览器窗口的标题区和标签栏应该显示什么标题。 -->
</head>

<!-- 在<head>区域之后，我们可以看到<body>标签 -->
<!-- 在这点之前的内容都不会显示在浏览器的窗口中。 -->
<!-- 我们必须在正文区填上需要显示的内容。 -->

<body>
	<h1>Hello, world!</h1> <!-- h1标签创建了一个标题 -->
	<!-- <h1>标签可以有一些副标题，从最重要的（h2）到最细微的（h6）。 -->
	<a href = "http://codepen.io/anon/pen/xwjLbZ">来看看这里有什么</a> <!-- 一个指向href=""属性中URL的超链接 -->
	<p>这是一个段落。</p> <!-- <p>标签让我们在html页面中显示文字 -->
	<p>这是另外一个段落。</p>
	<ul> <!-- <ul>标签创建了一个项目符合列表。 -->
	<!-- 如果需要一个编号列表，我们可以使用<ol>标签。这样会在在第一项前显示1.，第二项前显示2.，以此类推。 -->
		<li>这是一个非计数列表的一项（项目符合列表）</li>
		<li>这是另一项</li>
		<li>这是列表中的最后一项</li>
	</ul>
</body>

<!-- 好了，创建一个HTML文件就是这么简单。 -->

<!-- 当然我们还可以加入很多额外的HTML标签类型。 -->

<!-- 插入图片。 -->
<img src="http://i.imgur.com/XWG0O.gif"/> <!-- 图片源是由src=""属性指明的 -->
<!-- 图片源可以是一个URL或者你电脑上一个文件的路径。 -->

<!-- 创建表格也没问题。 -->

<table> <!-- 我们开始一个<table>元素 -->
	<tr> <!-- <tr>让我们创建一行 -->
		<th>第一个表头</th> <!-- <th>让我们给表格列一个标题 -->
		<th>第二个表头</th>
	</tr>
	<tr>
		<td>第一行第一列</td> <!-- <td>让我们创建一个单元格 -->
		<td>第一行第二列</td>
	</tr>
	<tr>
		<td>第二行第一列</td>
		<td>第二行第二列</td>
	</tr>
</table>

```

## 使用

HTML文件使用`.html`后缀。

## 扩展阅读

* [维基百科](https://en.wikipedia.org/wiki/HTML)
* [HTML tutorial](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
