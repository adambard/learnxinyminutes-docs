---
category: framework
name: jQuery
contributors:
    - ["Sawyer Charles", "https://github.com/xssc"]
    - ["Devansh Patil", "https://github.com/subtra3t"]
filename: jquery.js
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

jQuery는 "더 적게 쓰고, 더 많이 하라"는 데 도움이 되는 JavaScript 라이브러리입니다. 많은 일반적인 JavaScript 작업을 더 쉽게 작성할 수 있도록 합니다. jQuery는 많은 대기업과 개발자가 어디에서나 사용합니다. AJAX, 이벤트 처리, 문서 조작 등을 더 쉽고 빠르게 만듭니다.

jQuery는 JavaScript 라이브러리이므로 [먼저 JavaScript를 배워야 합니다](../javascript/)

**참고**: 최근 몇 년 동안 jQuery는 바닐라 DOM(문서 개체 모델) API로 동일한 작업을 수행할 수 있기 때문에 주목을 받지 못했습니다. 따라서 사용되는 유일한 것은 [jQuery 날짜 선택기](https://api.jqueryui.com/datepicker)(실제로 `<input type="date">` HTML 요소와 달리 표준이 있음)와 같은 몇 가지 편리한 기능과 코드 길이의 명백한 감소입니다.

```js
///////////////////////////////////
// 1. 선택자

// jQuery의 선택자는 요소를 선택하는 데 사용됩니다.
var page = $(window); // 전체 뷰포트 선택

// 선택자는 CSS 선택자일 수도 있습니다.
var paragraph = $('p'); // 모든 단락 요소 선택
var table1 = $('#table1'); // id가 'table1'인 요소 선택
var squares = $('.square'); // 클래스가 'square'인 모든 요소 선택
var square_p = $('p.square') // 'square' 클래스가 있는 단락 선택


///////////////////////////////////
// 2. 이벤트 및 효과
// jQuery는 이벤트가 트리거될 때 발생하는 일을 처리하는 데 매우 좋습니다.
// 매우 일반적인 이벤트는 문서의 준비 이벤트입니다.
// 'ready' 메서드를 사용하여 요소 로드가 완료될 때까지 기다릴 수 있습니다.
$(document).ready(function(){
  // 문서가 로드될 때까지 코드가 실행되지 않습니다.
});
// 정의된 함수를 사용할 수도 있습니다.
function onAction() {
  // 이벤트가 트리거될 때 실행됩니다.
}
$('#btn').click(onAction); // 클릭 시 onAction 호출

// 다른 일반적인 이벤트는 다음과 같습니다:
$('#btn').dblclick(onAction); // 더블 클릭
$('#btn').hover(onAction); // 위에 마우스를 올리면
$('#btn').focus(onAction); // 포커스 시
$('#btn').blur(onAction); // 포커스 잃음
$('#btn').submit(onAction); // 제출 시
$('#btn').select(onAction); // 요소가 선택될 때
$('#btn').keydown(onAction); // 키를 누를 때
$('#btn').keyup(onAction); // 키를 놓을 때
$('#btn').keypress(onAction); // 키를 누를 때
$('#btn').mousemove(onAction); // 마우스를 움직일 때
$('#btn').mouseenter(onAction); // 마우스가 요소에 들어갈 때
$('#btn').mouseleave(onAction); // 마우스가 요소를 떠날 때


// 이 모든 것은 매개변수를 제공하지 않고 이벤트를 처리하는 대신 트리거할 수도 있습니다.
$('#btn').dblclick(); // 요소에서 더블 클릭 발생

// 선택자를 한 번만 사용하여 여러 이벤트를 처리할 수 있습니다.
$('#btn').on(
  {dblclick: myFunction1} // 더블 클릭 시 트리거됨
  {blur: myFunction1} // 블러 시 트리거됨
);

// 일부 효과 메서드로 요소를 이동하고 숨길 수 있습니다.
$('.table').hide(); // 요소 숨기기

// 참고: 이러한 메서드에서 함수를 호출해도 요소가 숨겨집니다.
$('.table').hide(function(){
    // 요소가 숨겨진 다음 함수 실행
});

// 선택자를 변수에 저장할 수 있습니다.
var tables = $('.table');

// 일부 기본 문서 조작 메서드는 다음과 같습니다:
tables.hide(); // 요소 숨기기
tables.show(); // 요소 표시(숨기기 해제)
tables.toggle(); // 숨기기/표시 상태 변경
tables.fadeOut(); // 페이드 아웃
tables.fadeIn(); // 페이드 인
tables.fadeToggle(); // 페이드 인 또는 아웃
tables.fadeTo(0.5); // 불투명도로 페이드(0과 1 사이)
tables.slideUp(); // 위로 슬라이드
tables.slideDown(); // 아래로 슬라이드
tables.slideToggle(); // 위 또는 아래로 슬라이드

// 위의 모든 것은 속도(밀리초)와 콜백 함수를 사용합니다.
tables.hide(1000, myFunction); // 1초 숨기기 애니메이션 후 함수

// fadeTo에는 두 번째 매개변수로 필수 불투명도가 있습니다.
tables.fadeTo(2000, 0.1, myFunction); // 2초. 0.1 불투명도로 페이드 후 함수

// animate 메서드로 약간 더 고급화할 수 있습니다.
tables.animate({margin-top:"+=50", height: "100px"}, 500, myFunction);
// animate 메서드는 끝낼 css 및 값의 개체, 애니메이션을 조정하기 위한 선택적 옵션 매개변수 및 물론 콜백 함수를 사용합니다.

///////////////////////////////////
// 3. 조작

// 이것들은 효과와 비슷하지만 더 많은 것을 할 수 있습니다.
$('div').addClass('taming-slim-20'); // 모든 div에 클래스 taming-slim-20 추가

// 일반적인 조작 메서드
$('p').append('Hello world'); // 요소 끝에 추가
$('p').attr('class'); // 속성 가져오기
$('p').attr('class', 'content'); // 속성 설정
$('p').hasClass('taming-slim-20'); // 클래스가 있으면 true 반환
$('p').height(); // 요소 높이 가져오기 또는 높이 설정


// 많은 조작 메서드의 경우 요소에 대한 정보 가져오기는 첫 번째 일치하는 요소만 가져옵니다.
$('p').height(); // 첫 번째 'p' 태그의 높이만 가져옵니다.

// each를 사용하여 모든 요소를 반복할 수 있습니다.
var heights = [];
$('p').each(function() {
  heights.push($(this).height()); // 모든 'p' 태그 높이를 배열에 추가
});
```

## 추가 자료

* [Codecademy - jQuery](https://www.codecademy.com/learn/learn-jquery) "하면서 배우기" 형식의 jQuery에 대한 좋은 소개입니다.