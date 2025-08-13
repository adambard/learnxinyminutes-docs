---
name: HTML
filename: learnhtml.txt
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["Robert Steed", "https://github.com/robochat"]
    - ["Dimitri Kokkonis", "https://github.com/kokkonisd"]
    - ["Taeyoon Kim", "https://github.com/partrita"]
---
HTML은 하이퍼텍스트 마크업 언어의 약자입니다.

월드 와이드 웹을 위한 페이지를 작성할 수 있는 언어입니다.
마크업 언어로, 텍스트와 데이터가 어떻게 표시되어야 하는지를 나타내는 코드를 사용하여 웹 페이지를 작성할 수 있습니다. 사실, HTML 파일은 간단한 텍스트 파일입니다.

이 마크업은 무엇입니까? 여는 태그와 닫는 태그로 둘러싸서 페이지의 데이터를 구성하는 방법입니다. 이 마크업은 둘러싸는 텍스트에 의미를 부여하는 역할을 합니다. 다른 컴퓨터 언어와 마찬가지로 HTML에는 여러 버전이 있습니다. 여기서는 HTML5에 대해 이야기하겠습니다.

**참고:** [codepen](http://codepen.io/pen/)과 같은 사이트에서 튜토리얼을 진행하면서 다양한 태그와 요소를 테스트하여 효과를 확인하고 언어에 익숙해질 수 있습니다. 이 문서는 주로 HTML 구문과 몇 가지 유용한 팁에 중점을 둡니다.


```html
<!-- 주석은 이 줄처럼 묶습니다! -->

<!--
	주석은
	여러 줄에
	걸쳐
	있을 수 있습니다!
-->

<!-- #################### 태그 #################### -->

<!-- 다음은 분석할 예제 HTML 파일입니다. -->


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
	HTML 파일은 항상 브라우저에 페이지가 HTML임을 나타내는 것으로 시작합니다.
-->
<!doctype html>

<!-- 그런 다음 <html> 태그를 여는 것으로 시작합니다. -->
<html>

<!-- 파일 끝에서 </html>로 닫힙니다. -->
</html>

<!-- 이 마지막 태그 뒤에는 아무것도 나타나서는 안 됩니다. -->

<!-- 내부(여는 태그와 닫는 태그 <html></html> 사이)에는 다음이 있습니다: -->

<!-- <head>로 정의된 헤더 (</head>로 닫아야 함). -->
<!--
	헤더에는 표시되지 않는 일부 설명 및 추가 정보가 포함되어 있습니다. 이것은 메타데이터입니다.
-->

<head>
	<!--
		<title> 태그는 브라우저 창의 제목 표시줄과 탭 이름에 표시할 제목을 브라우저에 나타냅니다.
	-->
	<title>My Site</title>
</head>

<!-- <head> 섹션 뒤에는 <body> 태그가 있습니다. -->
<!-- 이 지점까지 설명된 내용은 브라우저 창에 표시되지 않습니다. -->
<!-- 표시할 내용으로 본문을 채워야 합니다. -->

<body>
	<!-- h1 태그는 제목을 만듭니다. -->
	<h1>Hello, world!</h1>
	<!--
		가장 중요한(h2) 것부터 가장 정확한(h6) 것까지 <h1>에 대한 부제목도 있습니다.
	-->

	<!-- href="" 속성으로 지정된 URL에 대한 하이퍼링크 -->
	<a href="http://codepen.io/anon/pen/xwjLbZ">
		Come look at what this shows
	</a>

	<!-- <p> 태그를 사용하면 html 페이지에 텍스트를 포함할 수 있습니다. -->
	<p>This is a paragraph.</p>
	<p>This is another paragraph.</p>

	<!-- <ul> 태그는 글머리 기호 목록을 만듭니다. -->
	<!--
		대신 번호가 매겨진 목록을 사용하려면 <ol>을 사용하여 첫 번째 요소에 1., 두 번째 요소에 2. 등을 제공합니다.
	-->
	<ul>
		<li>This is an item in a non-enumerated list (bullet list)</li>
		<li>This is another item</li>
		<li>And this is the last item on the list</li>
	</ul>
</body>

<!-- 그리고 그게 다입니다. HTML 파일을 만드는 것은 간단할 수 있습니다. -->

<!-- 하지만 많은 추가 유형의 HTML 태그를 추가할 수 있습니다. -->

<!-- <img /> 태그는 이미지를 삽입하는 데 사용됩니다. -->
<!--
	이미지 소스는 src="" 속성을 사용하여 나타냅니다.
	소스는 URL이거나 컴퓨터의 파일 경로일 수 있습니다.
-->
<img src="http://i.imgur.com/XWG0O.gif"/>

<!-- 테이블을 만들 수도 있습니다. -->

<!-- <table> 요소를 엽니다. -->
<table>

	<!-- <tr>을 사용하면 행을 만들 수 있습니다. -->
	<tr>

		<!-- <th>를 사용하면 테이블 열에 제목을 지정할 수 있습니다. -->
		<th>First Header</th>
		<th>Second Header</th>
	</tr>

	<tr>

		<!-- <td>를 사용하면 테이블 셀을 만들 수 있습니다. -->
		<td>first row, first column</td>
		<td>first row, second column</td>
	</tr>

	<tr>
		<td>second row, first column</td>
		<td>second row, second column</td>
	</tr>
</table>
```

## 사용법

HTML은 `.html` 또는 `.htm` 확장자로 끝나는 파일에 작성됩니다. 마임 유형은 `text/html`입니다.
**HTML은 프로그래밍 언어가 아닙니다.**
## 더 배우기

* [위키백과](https://en.wikipedia.org/wiki/HTML)
* [HTML 튜토리얼](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3Schools](http://www.w3schools.com/html/html_intro.asp)