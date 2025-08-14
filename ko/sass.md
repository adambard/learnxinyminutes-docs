---
name: Sass
filename: learnsass.scss
contributors:
  - ["Laura Kyle", "https://github.com/LauraNK"]
  - ["Sean Corrales", "https://github.com/droidenator"]
  - ["Kyle Mendes", "https://github.com/pink401k"]
  - ["Keith Miyake", "https://github.com/kaymmm"]
---

Sass는 변수, 중첩, 믹스인 등과 같은 기능을 추가하는 CSS 확장 언어입니다.
Sass(및 [Less](http://lesscss.org/)와 같은 다른 전처리기)는 개발자가 유지 관리 가능하고 DRY(반복하지 않기) 코드를 작성하는 데 도움이 됩니다.

Sass에는 선택할 수 있는 두 가지 다른 구문 옵션이 있습니다. SCSS는 CSS와 동일한 구문을 가지고 있지만 Sass의 추가 기능이 있습니다. 또는 Sass(원래 구문)는 중괄호와 세미콜론 대신 들여쓰기를 사용합니다.
이 튜토리얼은 SCSS를 사용하여 작성되었습니다.

이미 CSS3에 익숙하다면 Sass를 비교적 빨리 배울 수 있습니다. 새로운 스타일링 속성을 제공하는 것이 아니라 CSS를 보다 효율적으로 작성하고 유지 관리를 훨씬 쉽게 만드는 도구를 제공합니다.

```scss
//한 줄 주석은 Sass가 CSS로 컴파일될 때 제거됩니다.

/* 여러 줄 주석은 유지됩니다. */



/* 변수
============================== */



/* CSS 값(예: 색상)을 변수에 저장할 수 있습니다.
'$' 기호를 사용하여 변수를 만듭니다. */

$primary-color: #A3A4FF;
$secondary-color: #51527F;
$body-font: 'Roboto', sans-serif;

/* 스타일시트 전체에서 변수를 사용할 수 있습니다.
이제 색상을 변경하려면 한 번만 변경하면 됩니다. */

body {
	background-color: $primary-color;
	color: $secondary-color;
	font-family: $body-font;
}

/* 이것은 다음과 같이 컴파일됩니다: */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}

/* 이것은 스타일시트 전체에서 색상이 나타날 때마다
변경해야 하는 것보다 훨씬 유지 관리가 용이합니다. */



/* 제어 지시문
============================== */

/* Sass는 @if, @else, @for, @while 및 @each를 사용하여
   코드를 CSS로 컴파일하는 것을 제어할 수 있습니다. */

/* @if/@else 블록은 예상대로 정확하게 작동합니다. */

$debug: true !default;

@mixin debugmode {
	@if $debug {
		@debug "디버그 모드 활성화됨";

		display: inline-block;
	}
	@else {
		display: none;
	}
}

.info {
	@include debugmode;
}

/* $debug가 true로 설정되면 .info가 표시되고, false로 설정되면
.info가 표시되지 않습니다.

참고: @debug는 명령줄에 디버깅 정보를 출력합니다.
SCSS를 디버깅하는 동안 변수를 확인하는 데 유용합니다. */

.info {
	display: inline-block;
}

/* @for는 값 범위를 반복하는 제어 루프입니다.
항목 모음에 스타일을 설정하는 데 특히 유용합니다.
"through"와 "to"의 두 가지 형식이 있습니다. 전자는 마지막 값을 포함하고,
후자는 마지막 값에서 멈춥니다. */

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

/* 다음과 같이 컴파일됩니다: */

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
// SASS는 #FFFFFF를 자동으로 white로 변환합니다.
}

/* @while은 매우 간단합니다: */

$columns: 4;
$column-width: 80px;

@while $columns > 0 {
	.col-#{$columns} {
		width: $column-width;
		left: $column-width * ($columns - 1);
	}

	$columns: $columns - 1;
}

/* 다음 CSS를 출력합니다: */

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

/* @each는 서수 값 대신 목록을 사용하는 것을 제외하고 @for와 같이 작동합니다.
참고: 다른 변수와 마찬가지로 목록을 지정하며 공백을
구분 기호로 사용합니다. */

$social-links: facebook twitter linkedin reddit;

.social-links {
	@each $sm in $social-links {
		.icon-#{$sm} {
			background-image: url("images/#{$sm}.png");
		}
	}
}

/* 다음과 같이 출력됩니다: */

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


/* 믹스인
==============================*/

/* 둘 이상의 요소에 대해 동일한 코드를 작성하는 경우
해당 코드를 믹스인에 저장할 수 있습니다.

'@mixin' 지시문과 믹스인 이름을 사용하십시오. */

@mixin center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* '@include'와 믹스인 이름을 사용하여 믹스인을 사용할 수 있습니다. */

div {
	@include center;
	background-color: $primary-color;
}

/* 다음과 같이 컴파일됩니다: */
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #A3A4FF;
}

/* 믹스인을 사용하여 약식 속성을 만들 수 있습니다. */

@mixin size($width, $height) {
	width: $width;
	height: $height;
}

/* 너비와 높이 인수를 전달하여 호출할 수 있습니다. */

.rectangle {
	@include size(100px, 60px);
}

.square {
	@include size(40px, 40px);
}

/* 다음과 같이 컴파일됩니다: */
.rectangle {
  width: 100px;
  height: 60px;
}

.square {
  width: 40px;
  height: 40px;
}



/* 함수
============================== */



/* Sass는 다양한 작업을 수행하는 데 사용할 수 있는 함수를 제공합니다.
   다음을 고려하십시오. */

/* 함수는 이름을 사용하고 필요한 인수를 전달하여 호출할 수 있습니다. */
body {
  width: round(10.25px);
}

.footer {
  background-color: fade_out(#000000, 0.25);
}

/* 다음과 같이 컴파일됩니다: */

body {
  width: 10px;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}

/* 자신만의 함수를 정의할 수도 있습니다. 함수는 믹스인과 매우 유사합니다.
   함수와 믹스인 중에서 선택할 때 믹스인은
   CSS를 생성하는 데 가장 적합하고 함수는 Sass 코드 전체에서
   사용될 수 있는 논리에 더 적합하다는 것을 기억하십시오.
   '수학 연산자' 섹션의 예는 재사용 가능한
   함수가 되기에 이상적인 후보입니다. */

/* 이 함수는 대상 크기와 부모 크기를 가져와
   백분율을 계산하고 반환합니다. */

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

/* 다음과 같이 컴파일됩니다: */

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}



/* 확장 (상속)
============================== */



/* 확장은 한 선택자의 속성을 다른 선택자와 공유하는 방법입니다. */

.display {
	@include size(5em, 5em);
	border: 5px solid $secondary-color;
}

.display-success {
	@extend .display;
	border-color: #22df56;
}

/* 다음과 같이 컴파일됩니다: */
.display, .display-success {
  width: 5em;
  height: 5em;
  border: 5px solid #51527F;
}

.display-success {
  border-color: #22df56;
}

/* CSS 문을 확장하는 것이 믹스인을 만드는 것보다 바람직합니다.
   Sass가 동일한 기본 스타일을 공유하는 모든 클래스를
   그룹화하는 방식 때문입니다. 믹스인으로 수행했다면
   너비, 높이 및 테두리가 믹스인을 호출한 각 문에 대해
   복제됩니다. 작업 흐름에 영향을 미치지는 않지만
   Sass 컴파일러가 만든 파일에 불필요한 팽창을 추가합니다. */



/* 중첩
============================== */



/* Sass는 선택자 내에 선택자를 중첩할 수 있습니다. */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #FF0000;
	}
}

/* '&'는 부모 선택자로 대체됩니다. */
/* 의사 클래스를 중첩할 수도 있습니다. */
/* 과도한 중첩은 코드를 덜 유지 관리하기 쉽게 만든다는 점을 명심하십시오.
모범 사례는 중첩할 때 3단계 이상 깊이 들어가지 않는 것을 권장합니다.
예를 들어: */

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

/* 다음과 같이 컴파일됩니다: */

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



/* 부분 파일 및 가져오기
============================== */



/* Sass는 부분 파일을 만들 수 있습니다. 이렇게 하면 Sass
   코드를 모듈화하는 데 도움이 될 수 있습니다. 부분 파일은 '_'로 시작해야 합니다(예: _reset.css).
   부분 파일은 CSS로 생성되지 않습니다. */

/* _reset.css라는 파일에 넣을 다음 CSS를 고려하십시오. */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

/* Sass는 부분 파일을 파일로 가져오는 데 사용할 수 있는 @import를 제공합니다.
   이것은 가져온 파일을 가져오기 위해 다른 HTTP 요청을 하는
   전통적인 CSS @import 문과 다릅니다. Sass는 가져온
   파일을 가져와 컴파일된 코드와 결합합니다. */

@import 'reset';

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}

/* 다음과 같이 컴파일됩니다: */

html, body, ul, ol {
  margin: 0;
  padding: 0;
}

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}



/* 자리 표시자 선택자
============================== */



/* 자리 표시자는 확장할 CSS 문을 만들 때 유용합니다.
   @extend와 함께 독점적으로 사용되는 CSS 문을 만들고 싶다면
   자리 표시자를 사용하여 그렇게 할 수 있습니다. 자리 표시자는 '.' 또는 '#' 대신
   '%'로 시작합니다. 자리 표시자는 컴파일된 CSS에 나타나지 않습니다. */

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

/* 다음과 같이 컴파일됩니다: */

.message-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  background-color: #0000ff;
}



/* 수학 연산
============================== */



/* Sass는 +, -, *, /, % 연산자를 제공합니다. 이것들은
   직접 계산한 값을 사용하는 대신 Sass 파일에서 직접
   값을 계산하는 데 유용할 수 있습니다. 아래는 간단한
   2열 디자인을 설정하는 예입니다. */

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

/* 다음과 같이 컴파일됩니다: */

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

## SASS인가, Sass인가?
Sass가 약어인지 궁금한 적이 있습니까? 아마 없을 것입니다. 하지만 어쨌든 알려 드리겠습니다. 언어의 이름은 "Sass"라는 단어이며 약어가 아닙니다.
사람들이 계속 "SASS"라고 썼기 때문에 언어 제작자는 농담으로 "구문적으로 멋진 스타일시트(Syntactically Awesome StyleSheets)"라고 불렀습니다.


## Sass 연습
브라우저에서 Sass를 가지고 놀고 싶다면 [SassMeister](http://sassmeister.com/)를 확인하십시오.
두 구문 중 하나를 사용할 수 있으며, 설정으로 이동하여 Sass 또는 SCSS를 선택하기만 하면 됩니다.


## 호환성
Sass는 CSS로 컴파일하는 프로그램이 있는 한 모든 프로젝트에서 사용할 수 있습니다. 사용 중인 CSS가 대상 브라우저와 호환되는지 확인해야 합니다.

[QuirksMode CSS](http://www.quirksmode.org/css/) 및 [CanIUse](http://caniuse.com)는 호환성을 확인하는 데 훌륭한 리소스입니다.


## 더 읽을거리
* [공식 문서](http://sass-lang.com/documentation/file.SASS_REFERENCE.html)
* [The Sass Way](http://thesassway.com/)는 튜토리얼(초급-고급)과 기사를 제공합니다.
