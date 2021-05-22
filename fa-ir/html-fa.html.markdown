---
language: html
filename: learnhtml.txt
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:

    - ["Hiva zarei" , "https://github.com/hivazarei"]
---

 <p dir="rtl">HTML مخفف كلمه ی Hyper Text Markup Language می باشد.</p>
.يك زبان نشانه گيري است كه با استفاده از كد نويسي ميتوانيم متن و داده هاي مختلف را با استفاده از آن نمايش دهيم

Markup چيست؟ روشي است يراي مرتب كردن داده هاي صفحه كه با تگ ها ي باز و بسته احاطه شده است.  همانند زبان ها ي برنامه نويسي ديگر نسخهاي متفاوتي دارد . در اينجا درباره ي html 5 توضيح داده شده است



```html
<!-- كامنت ها به شكل روبر باز و بسته ميشوند! -->

<!--
	كامنت ها
     ميتوانند
      چند خطي
       نيز باشند
-->

<!-- #################### تگ ها #################### -->

////<!-- توضيحات زير مثال يك فايل( اچ تي ام ال) است كه تجزيه و تحليل شده است. -->


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
	فايل html هميشه براي يادآوري به مرور گر با  نشانه شروع ميشود.
-->
<!doctype html>

<!-- عد از آن با باز كردن تگ html tag شروع ميشود. -->
<html>

<!-- که در انتهای فایل با بسته خواهد شد</html>. -->
</html>

<!-- هیچ چیز نباید بعد از این تگ نهایی ظاهر شود. -->

<!--
	سرصفحه شامل برخی توضیحات و اطلاعات اضافی است
-->

<head>
	<!--
		تگ title نمايانگر نام tab و نوار عنوان در مرور گر ميباشد
	-->
	<title>My Site</title>
</head>

<!-- بعد از بخش head , بخش body را ميتوانيد پيدا كنيد -->
<!-- . تا این مرحله، هیچ چیز توصیف شده در پنجره مرورگر نشان داده خواهد شد-->
<!-- بايد body را با محتوايي پر كنيم تا نمايان داده شود -->

<body>
	<!-- تگ h1 یک عنوان ایجاد می کند. -->
	<h1>Hello, world!</h1>
	

	<!-- يك لينك به آدرس url با مشخصات زير داده ميشود href="" -->
	<a href="http://codepen.io/anon/pen/xwjLbZ">
		Come look at what this shows
	</a>

	<!-- تگ p يه ما اين اجازه را ميدهد تا متني را در صفحه html قرار دهيم -->
	<p>This is a paragraph.</p>
	<p>This is another paragraph.</p>

	<!-- تگ ul يك ليست ايجاد ميكند. -->
	<!--
		To have a numbered list instead we would use <ol> giving 1. for the first
		element, 2. for the second, etc.
	-->
	<ul>
		<li>This is an item in a non-enumerated list (bullet list)</li>
		<li>This is another item</li>
		<li>And this is the last item on the list</li>
	</ul>
</body>

<!-- درست كردن فايل html كار ساده است اما اضافه كردن تگ هاي مختلف امكان پذير است. -->



<!-- The <img /> قرار دادن عکس میباشد برای   -->
<!--
	منبع عكس بايد ذكر شده باشد تا بتوانيم به آن دسترسي پيدا كنيم attribute src=""
	منبع ميتواند از فايل هاي كامپيوتر يا url باشد.
-->
<img src="http://i.imgur.com/XWG0O.gif"/>

<!-- همچنين شما ميتوانيد جدول هم ايجاد كنيد. -->

<!-- We open a <table> element. -->
<table>

	<!-- تگ tr باعث ايجاد رديف ميشود -->
	<tr>

		<!-- تگ th يك عنوان به ستون جدوا ميدهد -->
		<th>First Header</th>
		<th>Second Header</th>
	</tr>

	<tr>

		<!-- باعث ايجاد خانه هاي جدول ميشود TD-->
		<td>first row, first column</td>
		<td>first row, second column</td>
	</tr>

	<tr>
		<td>second row, first column</td>
		<td>second row, second column</td>
	</tr>
</table>

```

* [wikipedia](https://en.wikipedia.org/wiki/HTML)
* [HTML tutorial](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
