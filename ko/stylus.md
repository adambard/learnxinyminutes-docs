# stylus.md (번역)

---
name: Stylus
filename: learnStylus.styl
contributors:
  - ["Salomão Neto", "https://github.com/salomaosnff"]
  - ["Isaac Henrique", "https://github.com/Isaachi1"]
translators:
  - ["Divay Prakash", "https://github.com/divayprakash"]
---

Stylus는 CSS로 컴파일되는 동적 스타일시트 전처리기 언어입니다. 웹 브라우저 간의 호환성을 깨지 않으면서 CSS에 기능을 추가하는 것을 목표로 합니다.
변수, 중첩, 믹스인, 함수 등을 사용하여 이를 수행합니다.

Stylus 구문은 매우 유연합니다. 표준 CSS 구문을 사용하고 세미콜론(;), 콜론(:) 및 ({)과 (})를 선택적으로 생략하여 코드를 더욱 읽기 쉽게 만들 수 있습니다.

Stylus는 새로운 스타일 옵션을 제공하지 않지만 CSS를 훨씬 더 동적으로 만들 수 있는 기능을 제공합니다.

```scss
/* 코드 스타일
==============================*/

/* Stylus에서는 키, 세미콜론 및 콜론이 선택 사항입니다. */

body {
  background: #000;
}

body {
  background: #000
}

body {
  background #000
}

body
  background #000

body
  background: #000;

body
  background: #000

// 한 줄 주석은 Stylus가 CSS로 컴파일될 때 제거됩니다.

/* 여러 줄 주석은 유지됩니다. */


/* 선택자
==============================*/

/* 다른 요소 내의 요소 선택 */
body {
  background: #000000;
  h1 {
    color: #FF0000;
  }
}

/* 또는 원한다면... */
body
  background #000000
  h1
    color #FF0000


/* 부모 요소 참조 가져오기
==============================*/
a {
  color: #0088dd;
  &:hover {
    color: #DD8800;
  }
}


/* 변수
==============================*/


/*
  변수의 CSS 값(예: 색상)을 저장할 수 있습니다.
  선택 사항이지만 변수 이름 앞에 $를 추가하여
  변수를 다른 CSS 값과 구별하는 것이 좋습니다.
*/

$primary-color = #A3A4FF
$secondary-color = #51527F
$body-font = 'Roboto', sans-serif

/* 스타일시트 전체에서 변수를 사용할 수 있습니다.
이제 색상을 변경하려면 한 번만 변경하면 됩니다. */

body
  background-color $primary-color
  color $secondary-color
  font-family $body-font

/* 컴파일 후: */
body {
  background-color: #A3A4FF;
  color: #51527F;
  font-family: 'Roboto', sans-serif;
}

/ *
스타일시트 전체에서 색상이 나타날 때마다 색상을 변경하는 것보다
유지 관리가 훨씬 쉽습니다.
* /


/* 믹스인
==============================*/

/* 둘 이상의 요소에 대해 동일한 코드를 작성하고 있다는 것을
알게 되면 해당 코드를 믹스인에 저장하고 싶을 수 있습니다.

center()
  display block
  margin-left auto
  margin-right auto
  left 0
  right 0

/* 믹스인 사용 */
body {
  center()
  background-color: $primary-color
}

/* 컴파일 후: */
div {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
  background-color: #A3A4FF;
}

/* 믹스인을 사용하여 약식 속성을 만들 수 있습니다. */

size($width, $height)
  width $width
  height $height

.rectangle
  size(100px, 60px)

.square
  size(40px, 40px)

/* 믹스인을 CSS 속성으로 사용할 수 있습니다. */
circle($ratio)
  width $ratio * 2
  height $ratio * 2
  border-radius $ratio

.ball
  circle 25px


/* 보간
==============================*/

vendor(prop, args)
  -webkit-{prop} args
  -moz-{prop} args
  {prop} args

border-radius()
  vendor('border-radius', arguments)

box-shadow()
  vendor('box-shadow', arguments)

button
  border-radius 1px 2px / 3px 4px


/* 함수
==============================*/

/* Stylus의 함수를 사용하면 일부 데이터를 다시 호출하는 등 다양한 작업을 수행할 수 있습니다. */

body {
  background darken(#0088DD, 50%) // 색상 #0088DD를 50% 어둡게 합니다.
}

/* 자신만의 함수 만들기 */
add(a, b)
  a + b

body
  padding add(10px, 5)


/* 조건
==============================*/
compare(a, b)
  if a > b
    bigger
  else if a < b
    smaller
  else
    equal

compare(5, 2)   // => bigger
compare(1, 5)   // => smaller
compare(10, 10) // => equal


/* 반복
==============================*/

/*
반복 루프 구문:
for <val-name> [, <key-name>] in <expression>
*/

for $item in (1..2) /* 블록 12번 반복 */
  .col-{$item}
    width ($item / 12) * 100% /* 열 번호로 행 계산 */
```

이제 이 강력한 CSS 전처리기에 대해 조금 알았으니 더 동적인 스타일 시트를 만들 준비가 되었습니다. 더 자세히 알아보려면 공식 스타일러스 문서 [stylus-lang.com](https://stylus-lang.com)을 방문하십시오.
