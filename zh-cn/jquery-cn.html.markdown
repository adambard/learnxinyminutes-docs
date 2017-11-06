---
category: tool
tool: jquery
contributors:
    - ["Sawyer Charles", "https://github.com/xssc"]
translators:
    - ["zxyqwe", "https://github.com/zxyqwe"]
lang: zh-cn
filename: jquery-cn.js
---

jQuery是JavaScript的一个函数库，它可以帮你“写更少，做更多”。它集成了很多常见的JavaScript任务并且很容易调用。jQuery被世界各地的很多的大公司和开发者使用。它包括了AJAX，事件处理，文档操作以及很多其它功能，并且更加简单和快速。

正因为jQuery是JavaScript的一个函数库，所以你需要[首先学习JavaScript](https://learnxinyminutes.com/docs/javascript/)

```js


///////////////////////////////////
// 1. 选择器

// jQuery中的选择器被用来选择一个元素
var page = $(window); // 选择整个视窗

// 选择器可以作为CSS选择器使用
var paragraph = $('p'); // 选择所有段落元素
var table1 = $('#table1'); // 选择id为table1的元素
var squares = $('.square'); // 选择所有类是square的元素
var square_p = $('p.square') // 选择具有square类的所有段落


///////////////////////////////////
// 2. 事件和效果
// jQuery非常善于处理当事件触发的时候应该做什么
// 一个非常常见的事件就是文档的就绪事件
// 你可以用ready方法，在所有元素完成加载的时候执行
$(document).ready(function(){
  // 只有文档加载完成以后代码才会执行
});
// 你也可以用定义了的函数
function onAction() {
  // 本函数在事件触发的时候被执行
}
$('#btn').click(onAction); // 当点击的时候调用onAction函数

// 其它常见的事件：
$('#btn').dblclick(onAction); // 双击
$('#btn').hover(onAction); // 划过
$('#btn').focus(onAction); // 聚焦
$('#btn').blur(onAction); // 失焦
$('#btn').submit(onAction); // 提交
$('#btn').select(onAction); // 当元素被选中
$('#btn').keydown(onAction); // 当一个按键被按下
$('#btn').keyup(onAction); // 当一个按键被抬起
$('#btn').keypress(onAction); // 当一个按键被按住
$('#btn').mousemove(onAction); // 当鼠标在移动
$('#btn').mouseenter(onAction); // 鼠标移入元素
$('#btn').mouseleave(onAction); // 鼠标离开元素


// 如果不提供任何参数的话，那么这些方法可以触发事件
// 而不是定义处理事件的方法
$('#btn').dblclick(); // 触发元素上的双击

// 你可以只用选择器一次而处理多个事件
$('#btn').on(
  {dblclick: myFunction1} // 双击的时候触发
  {blur: myFunction1} // 失焦的时候触发
);

// 你可以用一些效果函数来移动或隐藏元素
$('.table').hide(); // 隐藏元素

// 注意：在这些方法中调用函数会仍然隐藏元素
$('.table').hide(function(){
    // 元素先隐藏然后函数被执行
});

// 你可以在变量中储存选择器
var tables = $('.table');

// 一些基本的文档操作方法有：
tables.hide(); // 隐藏元素
tables.show(); // 显示元素
tables.toggle(); // 对被选元素进行隐藏和显示的切换
tables.fadeOut(); // 淡出
tables.fadeIn(); // 淡入
tables.fadeToggle(); // 对被选元素进行淡入和淡出显示的切换
tables.fadeTo(0.5); // 把被选元素逐渐改变至给定的不透明度（0和1之间）
tables.slideUp(); // 通过调整高度来滑动隐藏被选元素
tables.slideDown(); // 对被选元素进行滑动隐藏和滑动显示的切换
tables.slideToggle(); // 对被选元素进行滑动隐藏和滑动显示的切换

// 上面所有的方法接受速度参数（毫秒）和一个回调函数
tables.hide(1000, myFunction); // 持续一秒的隐藏动画然后执行函数

// fadeTo要求提供透明度参数作为第二个参数
tables.fadeTo(2000, 0.1, myFunction); // 通过2秒钟将透明度变为0.1然后执行函数

// 你可以用animate方法实现一些略微高级的效果
tables.animate({margin-top:"+=50", height: "100px"}, 500, myFunction);
// animate方法接受一个包含CSS和值的对象作为目标，
// 其次是可选的速度参数，
// 以及最后的回调函数

///////////////////////////////////
// 3. 操作

// 这些类似效果函数但是可以做更多
$('div').addClass('taming-slim-20'); // 给所有div添加类taming-slim-20

// 常见操作方法
$('p').append('Hello world'); // 添加到元素末尾
$('p').attr('class'); // 获取属性
$('p').attr('class', 'content'); // 设置属性
$('p').hasClass('taming-slim-20'); // 如果有类则为真
$('p').height(); // 获取和设置元素的高度


// 对于很多的操作函数来说，获取元素的信息
// 仅仅是第一个符合元素的
$('p').height(); // 仅仅获取第一个p标签的高度

// 你可以用each来迭代所有元素
var heights = [];
$('p').each(function() {
  heights.push($(this).height()); // 把所有p标签的高度加入数组
});


```
