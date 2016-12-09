---
language: less
filename: learnless-cn.less
contributors:
  - ["Saravanan Ganesh", "http://srrvnn.me"]
translators:
   - ["Jiang Haiyun", "http://www.atjiang.com"]
lang: zh-cn
---


Less是一种CSS预处理器，它增加了诸如变量、嵌套、mixin等功能。
Less(以及其它预处理器，如[Sass](http://sass-lang.com/))能帮助开发人员编写易维护，DRY (Don't Repeat Yourself) 的代码。

```css


//单行注释在编译成CSS后会被删除。

/* 多行注释将保留. */



/* 变量
==============================*/


/* 你可以将一个CSS值（如一个颜色值）保存到变量中。
   使用'@'符号来创建一个变量。*/

@primary-color: #a3a4ff;
@secondary-color: #51527f;
@body-font: 'Roboto', sans-serif;

/* 你可以在你的样式文件中使用这些变量。
   现在假如你想修改颜色，你只需修改一次即可。*/

body {
	background-color: @primary-color;
	color: @secondary-color;
	font-family: @body-font;
}

/* 以上将编译成： */

body {
	background-color: #a3a4ff;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}


/* 相比于在你的样式文件中逐个修改，这种方式维护性更好。 */



/* Mixins
==============================*/


/* 如果你要为多个元素编写同样的代码，
   你可能想实现轻松地重用。*/

.center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* 你只需简单地将选择子作为样式添加进来就能使用mixin了 */

div {
	.center;
	background-color: @primary-color;
}

/* 它将编译成: */

.center {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
}
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #a3a4ff;
}

/* 通过在选择子后添加括号，可以使这些mixin代码不被编译 */

.center() {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
}

div {
  .center;
  background-color: @primary-color;
}

/* 将编译成: */
div {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
  background-color: #a3a4ff;
}



/* 嵌套
==============================*/


/* Less允许你在选择子中嵌套选择子 */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #f00;
	}
}

/* '&'将被替换成父选择子。*/
/* 你也可以嵌套伪类。 */
/* 注意过度嵌套将会导致代码难以维护。
   最佳实践推荐在嵌套时不超过3层。
   例如： */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: red;

		&:hover {
		  background-color: blue;
		}

		a {
		  color: white;
		}
	}
}

/* 编译成： */

ul {
  list-style-type: none;
  margin-top: 2em;
}

ul li {
  background-color: red;
}

ul li:hover {
  background-color: blue;
}

ul li a {
  color: white;
}



/* 函数
==============================*/


/* Less提供的函数可以用来完成多种任务。
   考虑以下情况： */

/* 函数可以通过其名称及传入其所需的参数来调用。 */

body {
  width: round(10.25px);
}

.header {
	background-color: lighten(#000, 0.5);
}

.footer {
  background-color: fadeout(#000, 0.25)
}

/* 编译成: */

body {
  width: 10px;
}

.header {
  background-color: #010101;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}

/* 你也可以定义自己的函数。函数非常类似于mixin。
   当你在函数和mixin之间抉择时，
   记住mixin最适合用来创建CSS而函数更适合于
   处理那些可能在你的Less代码中使用的逻辑。
   '数学运算符'部分的例子是转成可重用函数的最佳选择。*/

/* 该函数计算两数的平均值： */

.average(@x, @y) {
  @average-result: ((@x + @y) / 2);
}

div {
  .average(16px, 50px); // "调用"mixin
  padding: @average-result;    // 使用它的"返回"值
}

/* 编译成: */

div {
  padding: 33px;
}



/* 扩展 (继承)
==============================*/


/* 扩展是在选择子间共享属性的一种方法。 */

.display {
  height: 50px;
}

.display-success {
  &:extend(.display);
	border-color: #22df56;
}

/* 编译成: */
.display,
.display-success {
  height: 50px;
}
.display-success {
  border-color: #22df56;
}

/* 扩展一条CSS语句优于创建一个mixin，
   这是由其组合所有共享相同基样式的类的方式决定的。
   如果使用mixin完成，其属性将会在调用了该mixin的每条语句中重复。
   虽然它不至会影响你的工作流，但它会在由Less编译器
   生成的的文件中添加不必要的代码。*/



/* 片段与导入
==============================*/


/* Less允许你创建片段文件。它有助于你的Less代码保持模块化。
   片段文件习惯上以'_'开头，例如 _reset.css，并被导入到
   一个将会被编译成CSS的主less文件中。*/

/* 考虑以下的CSS，我们将把它们放入一个叫_reset.css的文件中 */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

/* Less提供的@import能用来将片段导入到文件中。
   它与传统的CSS @import语句不同，无需通过
   HTTP请求获取导入文件。Less提取导入文件
   并将它们与编译后的代码结合起来。 */

@import 'reset';

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}

/* 编译成: */

html, body, ul, ol {
  margin: 0;
  padding: 0;
}

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}



/* 数学运算符
==============================*/


/* Less提供以下的运算符: +, -, *, /, 和 %。
   相比于使用你事先手工计算好了的数值，它们
   对于直接在你的Less文件中计算数值很有用。
   以下是设置一个两列设计的例子。*/

@content-area: 960px;
@main-content: 600px;
@sidebar-content: 300px;

@main-size: @main-content / @content-area * 100%;
@sidebar-size: @sidebar-content / @content-area * 100%;
@gutter: 100% - (@main-size + @sidebar-size);

body {
  width: 100%;
}

.main-content {
  width: @main-size;
}

.sidebar {
  width: @sidebar-size;
}

.gutter {
  width: @gutter;
}

/* 编译成： */

body {
  width: 100%;
}

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}

.gutter {
  width: 6.25%;
}


```

## 实践Less

如果你想在你的浏览器中尝试LESS，参阅：
* [Codepen](http://codepen.io/)
* [LESS2CSS](http://lesscss.org/less-preview/)

## 兼容性

Less可以用于任何项目中，只要你有程序能将它编译成CSS即可。你还需要验证你所使用的CSS是否与你的目标浏览器兼容。

[QuirksMode CSS](http://www.quirksmode.org/css/)和[CanIUse](http://caniuse.com) 对于检查兼容性来说都是不错的资源。

## 延伸阅读资料
* [Official Documentation](http://lesscss.org/features/)
* [Less CSS - Beginner's Guide](http://www.hongkiat.com/blog/less-basic/)
