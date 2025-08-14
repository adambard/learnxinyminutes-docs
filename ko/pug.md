---
name: Pug
contributors:
  - ["Michael Warner", "https://github.com/MichaelJGW"]
filename: index.pug
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Pug는 HTML로 컴파일되는 언어입니다. if 문과 루프와 같은 추가 기능이 있는 더 깔끔한 구문을 가지고 있습니다. Node.js와 같은 서버 언어의 서버 측 템플릿 언어로도 사용할 수 있습니다.

```pug
//- 한 줄 주석

//- 여러 줄
    주석

//- ---태그---
//- 기본
div
//- <div></div>
h1
//- <h1></h1>
my-customTag
//- <my-customTag></my-customTag>

//- 형제
div
div
//- <div></div>
    <div></div>

//- 자식
div
  div
//- <div>
      <div></div>
    </div>

//- 텍스트
h1 Hello there
//- <h1>Hello there</h1>

//- 여러 줄 텍스트
div.
  Hello
  There
//- <div>
      Hello
      There
    </div>

//- ---속성---
div(class="my-class" id="my-id" my-custom-attrs="data" enabled)
//- <div class="my-class" id="my-id" my-custom-attrs="data" enabled></div>

//- 단축
span.my-class
//- <span class="my-class"></span>
.my-class
//- <div class="my-class"></div>
div#my-id
//- <div id="my-id"></div>
div#my-id.my-class
//- <div class="my-class" id="my-id"></div>


//- ---JS---
- const lang = "pug";

//- 여러 줄 JS
-
  const lang = "pug";
  const awesome = true;

//- JS 클래스
- const myClass = ['class1', 'class2', 'class3']
div(class=myClass)
//- <div class="class1 class2 class3"></div>

//- JS 스타일
- const myStyles = {'color':'white', 'background-color':'blue'}
div(style=myStyles)
//- <div style="color:white;background-color:blue;"></div>

//- JS 속성
- const myAttributes = {"src": "photo.png", "alt": "My Photo"}
img&attributes(myAttributes)
//- <img src="photo.png" alt="My Photo">
- let disabled = false
input(type="text" disabled=disabled)
//- <input type="text">
- disabled = true
input(type="text" disabled=disabled)
//- <input type="text" disabled>

//- JS 템플릿
- const name = "Bob";
h1 Hi #{name}
h1= name
//- <h1>Hi Bob</h1>
//- <h1>Bob</h1>

//- ---루프---

//- 'each'와 'for'는 같은 작업을 수행하므로 'each'만 사용합니다.

each value, i in [1,2,3]
  p=value
//-
  <p>1</p>
  <p>2</p>
  <p>3</p>

each value, index in [1,2,3]
  p=value + '-' + index
//-
  <p>1-0</p>
  <p>2-1</p>
  <p>3-2</p>

each value in []
  p=value
//-

each value in []
  p=value
else
  p 여기에 값이 없습니다

//- <p>No Values are here</p>

//- ---조건문---

- const number = 5
if number < 5
  p 숫자가 5보다 작습니다
else if number > 5
  p 숫자가 5보다 큽니다
else
  p 숫자는 5입니다
//- <p>number is 5</p>

- const orderStatus = "Pending";
case orderStatus
  when "Pending"
    p.warn 주문이 보류 중입니다
  when "Completed"
    p.success 주문이 완료되었습니다.
  when -1
    p.error 오류가 발생했습니다
  default
    p 주문 기록을 찾을 수 없습니다
//- <p class="warn">Your order is pending</p>

//- --포함--
//- 파일 경로 -> "includes/nav.pug"
h1 회사 이름
nav
  a(href="index.html") 홈
  a(href="about.html") 회사 소개

//- 파일 경로 -> "index.pug"
html
  body
    include includes/nav.pug
//-
  <html>
    <body>
      <h1>Company Name</h1>
      <nav><a href="index.html">Home</a><a href="about.html">About Us</a></nav>
    </body>
  </html>

//- JS 및 CSS 가져오기
script
  include scripts/index.js
style
  include styles/theme.css

//- ---믹스인---
mixin basic
  div Hello
+basic
//- <div>Hello</div>

mixin comment(name, comment)
  div
    span.comment-name= name
    div.comment-text= comment
+comment("Bob", "This is Awesome")
//-
  <div>
    <span class="comment-name">Bob</span>
    <div class="comment-text">This is Awesome</div>
  </div>
```

### 추가 자료

- [사이트](https://pugjs.org/)
- [문서](https://pugjs.org/api/getting-started.html)
- [GitHub 저장소](https://github.com/pugjs/pug)
