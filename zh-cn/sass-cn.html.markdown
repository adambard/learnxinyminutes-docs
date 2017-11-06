---
language: sass
filename: learnsass-cn.scss
contributors:
  - ["Laura Kyle", "https://github.com/LauraNK"]
  - ["Sean Corrales", "https://github.com/droidenator"]
  - ["Kyle Mendes", "https://github.com/pink401k"]
  - ["Keith Miyake", "https://github.com/kaymmm"]
translators:
   - ["Jiang Haiyun", "http://www.atjiang.com"]
lang: zh-cn
---

Sass是一种CSS扩展语言，它增加了诸如变量、嵌套、mixin等功能。
Sass(以及其它预处理器，如[Less](http://lesscss.org/)等) 能帮助开发人员编写易维护和 DRY (Don't Repeat Yourself)的代码。

Sass有两种不同的语法可选用。SCSS的语法和CSS的相同，但增加了Sass的额外功能。或者Sass（原来的语法），它使用缩进而非大括号和分号。

本教程使用SCSS编写。

如果你已熟悉CSS3，你可能相对能较快地掌握Sass。它并没有提供任何新的类型属性，而只是提供了一些工具使你能更高效的编写CSS，并且使维护更加容易。

```scss


// 单行注释当Sass被编译成CSS后会被删除。

/* 多行注释将保留. */

/* 变量
============================== */



/* 你可以将一个CSS值（如一个颜色值）保存到变量中。
使用'$'符号来创建一个变量。*/

$primary-color: #A3A4FF;
$secondary-color: #51527F;
$body-font: 'Roboto', sans-serif;

/* 你可以在你的样式文件中使用变量。
   现在假如你想修改颜色，你只需修改一次即可。*/

body {
	background-color: $primary-color;
	color: $secondary-color;
	font-family: $body-font;
}

/* 以上将编译成： */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}

/* 相比于在你的样式文件中逐个进行修改，这种方式维护性更好。 */



/* 控制指令
============================== */

/* Sass允许你使用@if, @else, @for, @while, 和 @each 来控制
   你的代码如何编译成CSS */

/* @if/@else块的行为和你可能预想的会完全相同 */

$debug: true !default;

@mixin debugmode {
	@if $debug {
		@debug "Debug mode enabled";

		display: inline-block;
	}
	@else {
		display: none;
	}
}

.info {
	@include debugmode;
}

/* 如果$debug设置为了true, .info 将会显示; 如果设置为false那么
   .info 将不显示。

注意： @debug将在命令行中输出调试信息。
在调试你的SCSS时它对于检查变量很有用。*/

.info {
	display: inline-block;
}

/* @for是控制循环，它能遍历区间值。
它对于设置一组元素的类型特别有用。
有两种形式，"through"和"to"。前者包括最末那个值，
而后者止于最末那个值。 
*/

@for $c from 1 to 4 {
	div:nth-of-type(#{$c}) {
		left: ($c - 1) * 900 / 3;
	}
}

@for $c from 1 through 3 {
	.myclass-#{$c} {
		color: rgb($c * 255 / 3, $c * 255 / 3, $c * 255 / 3);
	}
}

/* 将编译成: */

div:nth-of-type(1) {
	left: 0;
}

div:nth-of-type(2) {
	left: 300;
}

div:nth-of-type(3) {
	left: 600;
}

.myclass-1 {
	color: #555555;
}

.myclass-2 {
	color: #aaaaaa;
}

.myclass-3 {
	color: white;
// SASS automatically converts #FFFFFF to white
}

/* @while也非常直白： */

$columns: 4;
$column-width: 80px;

@while $columns > 0 {
	.col-#{$columns} {
		width: $column-width;
		left: $column-width * ($columns - 1);
	}

	$columns: $columns - 1;
}

/* 将输出以下CSS: */

.col-4 {
	width: 80px;
	left: 240px;
}

.col-3 {
	width: 80px;
	left: 160px;
}

.col-2 {
	width: 80px;
	left: 80px;
}

.col-1 {
	width: 80px;
	left: 0px;
}

/* @each函数类似@for, 除了它使用一个列表而不是序列值
注意: 你指定列表的方式和指定其它变量一样，
用空格作为分隔符。 */

$social-links: facebook twitter linkedin reddit;

.social-links {
	@each $sm in $social-links {
		.icon-#{$sm} {
			background-image: url("images/#{$sm}.png");
		}
	}
}

/* 将输出: */

.social-links .icon-facebook {
	background-image: url("images/facebook.png");
}

.social-links .icon-twitter {
	background-image: url("images/twitter.png");
}

.social-links .icon-linkedin {
	background-image: url("images/linkedin.png");
}

.social-links .icon-reddit {
	background-image: url("images/reddit.png");
}


/* Mixins
==============================*/

/* 如果你发现你要为多个元素编写相同的代码，
你可能想将那些代码保存到一个mixin中。

使用'@mixin'指令，再为你的mixin加上一个名称。*/

@mixin center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* 你可以通过'@include'及mixin名来调用mixin。 */

div {
	@include center;
	background-color: $primary-color;
}

/* 将编译成: */
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #A3A4FF;
}

/* 你可以使用mixin来创建一个快捷属性。*/

@mixin size($width, $height) {
	width: $width;
	height: $height;
}

/* 你可以通过传入width和height参数来调用它。*/

.rectangle {
	@include size(100px, 60px);
}

.square {
	@include size(40px, 40px);
}

/* 编译成: */
.rectangle {
  width: 100px;
  height: 60px;
}

.square {
  width: 40px;
  height: 40px;
}



/* 函数
============================== */



/* Sass提供的函数可以用来完成各种各样的任务。
   考虑以下情况 */

/* 函数可以通过其名称及传入其所需的参数来调用 */
body {
  width: round(10.25px);
}

.footer {
  background-color: fade_out(#000000, 0.25);
}

/* 编译成: */

body {
  width: 10px;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}

/* 你也可以定义你自己的函数。函数非常类似于mixin。
   当你在函数和mixin之间抉择时，记住mixin最适合于创建CSS而函数更适合于
   处理那些可能在你的Sass代码中使用的逻辑。'数学运算符'部分的例子
   是转成可重用函数的最理想选择。 */

/* 该函数将接收一个目标尺寸大小和父结点尺寸大小，然后计算并
    返回百分数 */

@function calculate-percentage($target-size, $parent-size) {
  @return $target-size / $parent-size * 100%;
}

$main-content: calculate-percentage(600px, 960px);

.main-content {
  width: $main-content;
}

.sidebar {
  width: calculate-percentage(300px, 960px);
}

/* 编译成: */

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}



/* 扩展 (继承)
============================== */



/* 扩展是在选择子间共享属性的一种方法。 */

.display {
	@include size(5em, 5em);
	border: 5px solid $secondary-color;
}

.display-success {
	@extend .display;
	border-color: #22df56;
}

/* 编译成: */
.display, .display-success {
  width: 5em;
  height: 5em;
  border: 5px solid #51527F;
}

.display-success {
  border-color: #22df56;
}

/* 扩展一条CSS语句优于创建一个mixin，
   这是由Sass组合所有共享相同基样式的类的方式决定的。
   如果使用mixin完成，width, height, 和border将会在
   调用了该mixin的每条语句中重复。虽然它不至于会影响你的工作流，
   但它会在由Sass编译器生成的的文件中添加不必要的代码。*/


/* 嵌套
============================== */



/* Sass允许在选择子中嵌套选择子 */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #FF0000;
	}
}

/* '&'将被父选择子替换。*/
/* 你也可以嵌套伪类。 */
/* 注意过度嵌套将导致你的代码难以维护。
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



/* 片段与导入
============================== */



/* Sass允许你创建片段文件。它有助于你的Sass代码保持模块化。
   片段文件应该以 '_' 开头，例如 _reset.css。
   片段不会输出到CSS中。*/

/* 考虑以下的CSS，我们会将它们放入一个叫作_reset.css的文件中 */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

/* Sass提供的@import能用来将片段导入到文件中。
   它与传统的CSS @import语句不同，不需要通过
   另外的HTTP请求来获取导入的文件。
   Sass提取导入文件并将它与编译后的代码结合起来。 */

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



/* 占位符选择子
============================== */



/* 占位符在创建用于扩展的CSS语句时非常有用。
   如果你想创建一条只通过@extend使用的CSS语句，你可以利用占位符来实现。
   占位符以'%'而非'.'或'#'开头。占位符不会出现在编译后的CSS中 */

%content-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  @extend %content-window;
  background-color: #0000ff;
}

/* 编译成: */

.message-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  background-color: #0000ff;
}



/* 数学运算
============================== */



/* Sass提供以下的运算符: +, -, *, /, 和 %。它们
   相比于使用你事先手工计算好了的数值，它们
   对于直接在你的Sass文件中计算数值很有用。
   以下是设置一个简单的两列设计的例子。*/

$content-area: 960px;
$main-content: 600px;
$sidebar-content: 300px;

$main-size: $main-content / $content-area * 100%;
$sidebar-size: $sidebar-content / $content-area * 100%;
$gutter: 100% - ($main-size + $sidebar-size);

body {
  width: 100%;
}

.main-content {
  width: $main-size;
}

.sidebar {
  width: $sidebar-size;
}

.gutter {
  width: $gutter;
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

## SASS还是Sass?
该语言的名字，“Sass”，是一个词，不是一个缩写。
你有没想过Sass是否是一个缩写词？你可能没有，但我反正会告诉你。
该语言的名字是一个单词，不是一个缩写词。
由于人们老是将它写成"SASS"，语言的作者开玩笑地称它为"Syntactically Awesome StyleSheets"。


## 实践Sass
如果你想在你的浏览器中尝试Sass，参阅[SassMeister](http://sassmeister.com/)。
你可以选用任一种语法，只需进到设置页然后选择Sass或SCSS。


## 兼容性
Sass可以用于任何项目中，只要你有程序能将它编译成CSS即可。你还需要验证你所使用的CSS是否与你的目标浏览器兼容。

[QuirksMode CSS](http://www.quirksmode.org/css/)和[CanIUse](http://caniuse.com)对于检查兼容性来说都是不错的资源。


## 延伸阅读资料
* [Official Documentation](http://sass-lang.com/documentation/file.SASS_REFERENCE.html)
* [The Sass Way](http://thesassway.com/) 上提供了教程(初学者-高级)和文章。
