---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
    - ["Jakukyo Friel", "https://weakish.github.io"]
lang: zh-cn
filename: learncss-cn.css
---

早期的web没有样式，只是单纯的文本。通过CSS，可以实现网页样式和内容的分离。

简单来说，CSS可以指定HTML页面上的元素所使用的样式。

和其他语言一样，CSS有很多版本。最新的版本是CSS 3. CSS 2.0兼容性最好。

你可以使用[dabblet](http://dabblet.com/)来在线测试CSS的效果。

```css
/* 注释 */

/* ####################
   ## 选择器
   ####################*/

/* 一般而言，CSS的声明语句非常简单。 */
选择器 { 属性: 值; /* 更多属性...*/ }

/* 选择器用于指定页面上的元素。

针对页面上的所有元素。 */
* { color:red; }

/*
假定页面上有这样一个元素

<div class='some-class class2' id='someId' attr='value' />
*/

/* 你可以通过类名来指定它 */
.some-class { }

/* 给出所有类名 */
.some-class.class2 { }

/* 标签名 */
div { }

/* id */
#someId { }

/* 由于元素包含attr属性，因此也可以通过这个来指定 */
[attr] { font-size:smaller; }

/* 以及有特定值的属性 */
[attr='value'] { font-size:smaller; }

/* 通过属性的值的开头指定 */
[attr^='val'] { font-size:smaller; }

/* 通过属性的值的结尾来指定 */
[attr$='ue'] { font-size:smaller; }

/* 通过属性的值的部分来指定 */
[attr~='lu'] { font-size:smaller; }


/* 你可以把这些全部结合起来，注意不同部分间不应该有空格，否则会改变语义 */
div.some-class[attr$='ue'] { }

/* 你也可以通过父元素来指定。*/

/* 某个元素是另一个元素的直接子元素 */
div.some-parent > .class-name {}

/* 或者通过该元素的祖先元素 */
div.some-parent .class-name {}

/* 注意，去掉空格后语义就不同了。
你能说出哪里不同么？ */
div.some-parent.class-name {}

/* 你可以选择某元素前的相邻元素 */
.i-am-before + .this-element { }

/* 某元素之前的同级元素（相邻或不相邻） */
.i-am-any-before ~ .this-element {}

/* 伪类允许你基于页面的行为指定元素（而不是基于页面结构） */

/* 例如，当鼠标悬停在某个元素上时 */
:hover {}

/* 已访问过的链接*/
:visited {}

/* 未访问过的链接*/
:link {}

/* 当前焦点的input元素 */
:focus {}


/* ####################
   ## 属性
   ####################*/

选择器 {
    
    /* 单位 */
    width: 50%; /* 百分比 */
    font-size: 2em; /* 当前字体大小的两倍 */
    width: 200px; /* 像素 */
    font-size: 20pt; /* 点 */
    width: 5cm; /* 厘米 */
    width: 50mm; /* 毫米 */
    width: 5in; /* 英尺 */
    
    /* 颜色 */
    background-color: #F6E;  /* 短16位 */
    background-color: #F262E2; /* 长16位 */
    background-color: tomato; /* 颜色名称 */
    background-color: rgb(255, 255, 255); /* rgb */
    background-color: rgb(10%, 20%, 50%); /*  rgb 百分比 */
    background-color: rgba(255, 0, 0, 0.3); /*  rgb 加透明度 */
    
    /* 图片 */
    background-image: url(/path-to-image/image.jpg);
    
    /* 字体 */
    font-family: Arial;
    font-family: "Courier New"; /* 使用双引号包裹含空格的字体名称 */
    font-family: "Courier New", Trebuchet, Arial; /* 如果第一个
    						 字体没找到，浏览器会使用第二个字体，一次类推 */
}

```

## 使用

CSS文件使用 `.css` 后缀。

```xml
<!-- 你需要在文件的 <head> 引用CSS文件 -->
<link rel='stylesheet' type='text/css' href='filepath/filename.css' />

<!-- 你也可以在标记中内嵌CSS。不过强烈建议不要这么干。 -->
<style>
   选择器 { 属性:值; }
</style>

<!-- 也可以直接使用元素的style属性。
这是你最不该干的事情。 -->
<div style='property:value;'>
</div>

```

## 优先级

同一个元素可能被多个不同的选择器指定，因此可能会有冲突。

假定CSS是这样的：

```css
/*A*/
p.class1[attr='value']

/*B*/
p.class1 {}

/*C*/
p.class2 {}

/*D*/
p {}

/*E*/
p { property: value !important; }

```

然后标记语言为：

```xml
<p style='/*F*/ property:value;' class='class1 class2' attr='value'>
</p>
```

那么将会按照下面的顺序应用风格：


* `E` 优先级最高，因为它使用了 `!important`，除非很有必要，尽量避免使用这个。
* `F` 其次，因为它是嵌入的风格。
* `A` 其次，因为它比其他指令更具体。
* `C` 其次，虽然它的具体程度和`B`一样，但是它在`B`之后。
* 接下来是 `B`。
* 最后是 `D`。

## 兼容性

CSS2 的绝大部分特性兼容各种浏览器和设备。现在 CSS3 的兼容性也越来越好了。
但是兼容性问题仍然是需要留意的一个问题。

[QuirksMode CSS](http://www.quirksmode.org/css/)是关于这方面最好的资源。

## 扩展阅读

* [理解CSS的风格优先级: 特定性, 继承和层叠](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
