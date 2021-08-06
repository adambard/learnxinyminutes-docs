---
language: html
filename: learnhtml-fa.txt
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
    - ["Ehsan Parsania", "https://github.com/EhsanParsania"]
translators:
    - ["Ehsan Parsania", "https://github.com/EhsanParsania"]
---
<div dir='rtl'> 
	
**HTML مخفف HyperText Markup Language است.**


 html زبانی است که به ما امکان می دهد صفحاتی را برای شبکه جهانی وب بنویسیم.
این یک زیان برنامه نویسی نیست بلکه یک زبان نشانه گذاری است که ما را قادر می سازد تا صفحات وب را برای نشان دادن تکست ها و داده ها بنویسیم و تعیین کنیم که 
چگونه متن و داده ها باید نمایش داده شوند. در واقع HTML یک متن ساده خام می باشد.

این نشانه گذاری چیست؟ این یک روش سازماندهی داده های صفحه توسط تگ ها است
	هر تکه کد باید درون تگ باز و بسته HTML .رار بگیرد
نوع تگ 
برای متنی که در آن قرار دارد اهمیت دارد. مانند سایر زبان هاHTML
نسخه های زیادی دارد در اینجا ما در مورد HTML5 صحبت خواهیم کرد.

 توجه: ** می توانید برچسب ها و عناصر مختلف را در حین پیشرفت آزمایش کنید**
	
	
به منظور آشنایی بیشتر میتوانید در سایتی مانند (http://codepen.io/pen/) کدهای HTML را امتحان و نتیجه آن را به سادگی ببینید ،تأثیرات آنها را بشناسید ، نحوه عملکرد آنها را بشناسید و با آنها آشنا شوید
</div>

```html
<!--تگ کامنت ها مانند این خط بسته نمی شود -->

<!--
	کامنت ها
	میتوانند     
	در خطوط 
	متوالی باشند
-->

<!-- #################### تگ ها ################ -->

<!-- این یک یک فایل HTML است که ما آن را بررسی میکنیم -->


<!doctype html>
	<html>
		<head>
			<title>My Site</title>
		</head>
		<body>
			<h1>Hello, world!</h1>
			<a href="http://codepen.io/anon/pen/xwjLbZ">
				Come look at what this shows
			</a>
			<p>This is a paragraph.</p>
			<p>This is another paragraph.</p>
			<ul>
				<li>This is an item in a non-enumerated list (bullet list)</li>
				<li>This is another item</li>
				<li>And this is the last item on the list</li>
			</ul>
		</body>
	</html>

<!--
در شروع یک فایل html همیشه یک معرف برای مرورگر وجود دارد
-->
<!doctype html>

	<!-- بعد از آن تگ	 
	html 
	قرار میگیرد 
	-->
<html>

<!-- 
 و در آخر فایل آن تگ باید بسته شود
-->
</html>

<!-- 
و هیج چیز دیگری بعد از بسته شدن این تگ نوشته نشود
-->

<!-- 
 درون تگ 
html
ما تگهای دیگری خواهیم داشت:
<head>
</head>
و 
<body>
</body>

-->

<!-- A header defined by <head> (it must be closed with </head>). -->
<!--
			درون تگ هد اطلاعات از جمله توصیفات و اطلاعات تکمیلی قرار میگیرد مانند تگ های :
	<title> : موضوع صفحه که در قسمت تب مرورگر قرار میگیرد
<
-->

<head>
	<title>My Site</title>
</head>

<!-- 
بعد از آن تگ 
body 
قرار میگیرد که تمام کد ما در این تگ نوشته میشود
-->

<body>
	<!-- 
 	تگ 
	<h1>
	برای نشان دادن موضوع میباشد 
	-->
	<h1>Hello, world!</h1>
	<!--
		و تگ های دیگری با اولویت های کمتر هم برای موضوع 
		وجود دارد مانند :
		h2, h3, h4 ,h5, h6
	-->

	<!-- 
 	 تگ لینک
	<a>
	با اتریبیوت href
	 برای دادن آدرس لینک می باشد
	-->
	<a href="http://codepen.io/anon/pen/xwjLbZ">
		Come look at what this shows
	</a>

	<!-- 
 	تگ <p>
	برای نشان دادن متن به صورت پارگراف میباشد
	-->
	<p>This is a paragraph.</p>
	<p>This is another paragraph.</p>

	<!-- 
	تگ <ul>
	برای ساختن لیست به صورت مرتب عددی و 
	 به صورت غیرعددی میباشد.
	-->
	
	<ul>
		<li>This is an item in a non-enumerated list (bullet list)</li>
		<li>This is another item</li>
		<li>And this is the last item on the list</li>
	</ul>
</body>

<!-- 
 به همین راحتی اینها مفاهیم اولیه بودند 
-->

<!-- 
اما صفت ها و جزییات بیشتری وجود داره که میتونید انها رو هم یاد بگیرید 
-->

<!-- 
تگ <img>
 برای نشان دادن عکس با آدرس مشخص است.
-->

<img src="http://i.imgur.com/XWG0O.gif"/>

<!--
همچنین میتوانید یک جدول هم به همین راحتی درست کنید. 
-->

<!-- 
 با استفاده از تگ 
<table>
-->

<table>

	<!-- <tr> 
 	و با تگ 
	<tr>\
	برای جدول ردیف درست کنید.
	-->
	
	<tr>

		<!-- 
 		و درون هر ردیف با تگ
		<th>
		سر ستون ها رو بسازید
		-->
		<th>First Header</th>
		<th>Second Header</th>
	</tr>

	<tr>

		<!-- 
 		و با تگ 
		<td>
		بقیه ستون ها رو درست کنید
		-->
		
		<td>first row, first column</td>
		<td>first row, second column</td>
	</tr>

	<tr>
		<td>second row, first column</td>
		<td>second row, second column</td>
	</tr>
</table>

```

## Usage
پسوند فایل HTML 
.htm
یا 
.html 
میباشد.
و مایم تایپ آن
'text/html'
است.

**HTML یک زبان برنامه نویسی نیست**

## برای یادگیری بیشتر : 

* [wikipedia](https://en.wikipedia.org/wiki/HTML)
* [HTML tutorial](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
