# less.md (번역)

---
name: Less
filename: learnless.less
contributors:
  - ["Saravanan Ganesh", "http://srrvnn.me"]
---

Less는 변수, 중첩, 믹스인 등과 같은 기능을 추가하는 CSS 전처리기입니다.
Less (및 [Sass](http://sass-lang.com/)와 같은 다른 전처리기)는 개발자가 유지 관리가 가능하고 DRY(Don't Repeat Yourself) 코드를 작성하는 데 도움이 됩니다.

```less
//한 줄 주석은 Less가 CSS로 컴파일될 때 제거됩니다.

/*여러 줄 주석은 유지됩니다. */



/* 변수
==============================*/


/* 변수에 CSS 값(예: 색상)을 저장할 수 있습니다.
   변수를 만들려면 '@' 기호를 사용하십시오. */

@primary-color: #a3a4ff;
@secondary-color: #51527f;
@body-font: 'Roboto', sans-serif;

/* 스타일시트 전체에서 변수를 사용할 수 있습니다.
   이제 색상을 변경하려면 한 번만 변경하면 됩니다.*/

body {
	background-color: @primary-color;
	color: @secondary-color;
	font-family: @body-font;
}

/* 이것은 다음과 같이 컴파일됩니다. */

body {
	background-color: #a3a4ff;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}


/* 스타일시트 전체에서 나타날 때마다 색상을 변경하는 것보다
   유지 관리가 훨씬 쉽습니다. */



/* 믹스인
==============================*/


/* 둘 이상의 요소에 대해 동일한 코드를 작성하고 있다는 것을
   발견하면 쉽게 재사용하고 싶을 수 있습니다.*/

.center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* 단순히 선택자를 스타일로 추가하여 믹스인을 사용할 수 있습니다. */

div {
	.center;
	background-color: @primary-color;
}

/* 다음과 같이 컴파일됩니다. */

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

/* 선택자 뒤에 괄호를 추가하여 믹스인 코드가 컴파일되지 않도록
   생략할 수 있습니다. */

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

/* 다음과 같이 컴파일됩니다. */
div {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
  background-color: #a3a4ff;
}



/* 중첩
==============================*/


/* Less를 사용하면 선택자 내에 선택자를 중첩할 수 있습니다. */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #f00;
	}
}

/* '&'는 부모 선택자로 대체됩니다. */
/* 의사 클래스를 중첩할 수도 있습니다. */
/* 과도하게 중첩하면 코드를 유지 관리하기 어려워진다는 점을 명심하십시오.
   모범 사례는 중첩 시 3단계 이상으로 들어가지 않는 것을 권장합니다.
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

/* 컴파일 결과: */

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



/* 함수
==============================*/


/* Less는 다양한 작업을 수행하는 데 사용할 수 있는 함수를 제공합니다.
   다음을 고려하십시오. */

/* 함수는 이름을 사용하고 필요한 인수를 전달하여 호출할 수 있습니다. */

body {
  width: round(10.25px);
}

.header {
	background-color: lighten(#000, 0.5);
}

.footer {
  background-color: fadeout(#000, 0.25)
}

/* 컴파일 결과: */

body {
  width: 10px;
}

.header {
  background-color: #010101;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}

/* 자신만의 함수를 정의할 수도 있습니다. 함수는 믹스인과 매우 유사합니다.
   함수와 믹스인 중에서 선택할 때, 믹스인은 CSS를 생성하는 데 가장 적합하고
   함수는 Less 코드 전체에서 사용할 수 있는 논리에 더 적합하다는 것을
   기억하십시오. '수학 연산자' 섹션의 예제는 재사용 가능한
   함수가 되기에 이상적인 후보입니다. */

/* 이 함수는 두 숫자의 평균을 계산합니다. */

.average(@x, @y) {
  @average-result: ((@x + @y) / 2);
}

div {
  .average(16px, 50px); // 믹스인 "호출"
  padding: @average-result;    // "반환" 값 사용
}

/* 컴파일 결과: */

div {
  padding: 33px;
}



/* 확장 (상속)
==============================*/


/* 확장은 한 선택자의 속성을 다른 선택자와 공유하는 방법입니다. */

.display {
  height: 50px;
}

.display-success {
  &:extend(.display);
	border-color: #22df56;
}

/* 컴파일 결과: */
.display,
.display-success {
  height: 50px;
}
.display-success {
  border-color: #22df56;
}

/* 믹스인을 만드는 것보다 CSS 문을 확장하는 것이 좋습니다.
   모두 동일한 기본 스타일을 공유하는 클래스를 그룹화하는 방식 때문입니다.
   이것이 믹스인으로 수행되었다면 속성은
   믹스인을 호출한 각 문에 대해 복제됩니다.
   워크플로에는 영향을 미치지 않지만 Less 컴파일러가 생성한
   파일에 불필요한 부풀림을 추가합니다. */



/* 부분 및 가져오기
==============================*/


/* Less를 사용하면 부분 파일을 만들 수 있습니다. 이렇게 하면 Less
   코드를 모듈화하는 데 도움이 될 수 있습니다. 부분 파일은 관례적으로
   '_'로 시작합니다(예: _reset.less). 그리고 CSS로 컴파일되는
   주요 less 파일로 가져옵니다. */

/* _reset.less라는 파일에 넣을 다음 CSS를 고려하십시오. */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

/* Less는 @import를 제공하여 부분 파일을 파일로 가져올 수 있습니다.
   이는 가져온 파일을 가져오기 위해 다른 HTTP 요청을 하는
   전통적인 CSS @import 문과 다릅니다. Less는
   가져온 파일을 가져와 컴파일된 코드와 결합합니다. */

@import 'reset';

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}

/* 컴파일 결과: */

html, body, ul, ol {
  margin: 0;
  padding: 0;
}

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}



/* 수학 연산
==============================*/


/* Less는 +, -, *, /, % 연산자를 제공합니다.
   이러한 연산자는 수동으로 계산한 값을 사용하는 대신
   Less 파일에서 직접 값을 계산하는 데 유용할 수 있습니다.
   아래는 간단한 2열 디자인을 설정하는 예입니다. */

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

/* 컴파일 결과: */

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

## Less 연습

브라우저에서 Less를 사용해보고 싶다면 다음을 확인하십시오.
* [Codepen](http://codepen.io/)
* [LESS2CSS](http://lesscss.org/less-preview/)

## 호환성

Less는 CSS로 컴파일하는 프로그램만 있으면 모든 프로젝트에서 사용할 수 있습니다. 사용 중인 CSS가 대상 브라우저와 호환되는지 확인해야 합니다.

[QuirksMode CSS](http://www.quirksmode.org/css/) 및 [CanIUse](http://caniuse.com)는 호환성을 확인하는 데 유용한 자료입니다.

## 더 읽을거리
* [공식 문서](http://lesscss.org/features/)
* [Less CSS - 초보자 가이드](http://www.hongkiat.com/blog/less-basic/)
