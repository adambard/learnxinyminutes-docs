---
name: CSS
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
    - ["Geoffrey Liu", "https://github.com/g-liu"]
    - ["Connor Shea", "https://github.com/connorshea"]
    - ["Deepanshu Utkarsh", "https://github.com/duci9y"]
    - ["Brett Taylor", "https://github.com/glutnix"]
    - ["Tyler Mumford", "https://tylermumford.com"]
filename: learncss.css
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

웹 페이지는 HTML로 구축되며, HTML은 페이지의 내용을 지정합니다.
CSS(Cascading Style Sheets)는 페이지의 **모양**을 지정하는 별도의 언어입니다.

CSS 코드는 정적 *규칙*으로 구성됩니다. 각 규칙은 하나 이상의 *선택자*를 사용하고
여러 시각적 *속성*에 특정 *값*을 부여합니다. 이러한 속성은
선택자가 나타내는 페이지 요소에 적용됩니다.

이 가이드는 CSS2를 염두에 두고 작성되었으며, CSS3의 새로운 기능으로 확장되었습니다.

**참고:** CSS는 시각적 결과를 생성하므로, 배우려면 [dabblet](http://dabblet.com/)과 같은 CSS 플레이그라운드에서 모든 것을 시도해야 합니다.
이 문서의 주요 초점은 구문과 몇 가지 일반적인 팁에 있습니다.

## 구문

```css
/* 주석은 슬래시-별표 안에 나타납니다. 이 줄처럼요!
   "한 줄 주석"은 없습니다. 이것이 유일한 주석 스타일입니다. */

/* ####################
   ## 선택자
   #################### */

/* 선택자는 페이지의 요소를 대상으로 하는 데 사용됩니다. */
selector { property: value; /* 더 많은 속성... */ }

/*
다음은 예제 요소입니다:

<div class='class1 class2' id='anID' attr='value' otherAttr='en-us foo bar' />
*/

/* CSS 클래스 중 하나를 사용하여 대상을 지정할 수 있습니다. */
.class1 { }

/* 또는 두 클래스 모두! */
.class1.class2 { }

/* 또는 이름 */
div { }

/* 또는 ID */
#anID { }

/* 또는 속성이 있다는 사실을 사용하여! */
[attr] { font-size:smaller; }

/* 또는 속성이 특정 값을 가집니다. */
[attr='value'] { font-size:smaller; }

/* 값으로 시작합니다 (CSS 3) */
[attr^='val'] { font-size:smaller; }

/* 또는 값으로 끝납니다 (CSS 3) */
[attr$='ue'] { font-size:smaller; }

/* 또는 값을 포함합니다 (CSS 3) */
[attr*='foo'] { }

/* 또는 공백으로 구분된 목록에 값을 포함합니다. */
[otherAttr~='foo'] { }
[otherAttr~='bar'] { }

/* 또는 대시로 구분된 목록에 값을 포함합니다. 예: "-" (U+002D) */
[otherAttr|='en'] { font-size:smaller; }


/* 다른 선택자를 결합하여 더 집중된 선택자를 만들 수 있습니다. 그 사이에 공백을 두지 마십시오. */
div.some-class[attr$='ue'] { }

/* 다른 요소의 자식인 요소를 선택할 수 있습니다. */
div.some-parent > .class-name { }

/* 또는 다른 요소의 후손. 자식은 부모 요소의 직접적인 후손이며, 트리에서 한 레벨만 아래에 있습니다. 후손은 트리에서 어떤 레벨이든 될 수 있습니다. */
div.some-parent .class-name { }

/* 경고: 공백이 없는 동일한 선택자는 다른 의미를 가집니다.
   무엇인지 추측할 수 있습니까? */
div.some-parent.class-name { }

/* 인접 형제를 기반으로 요소를 선택할 수도 있습니다. */
.i-am-just-before + .this-element { }

/* 또는 그 앞에 오는 모든 형제 */
.i-am-any-element-before ~ .this-element { }

/* 특정 상태에 있을 때만 요소를 선택하는 데 사용할 수 있는 의사 클래스라는 선택자가 있습니다. */

/* 예를 들어, 링크가 방문되지 않았을 때 */
selected:link { }

/* 또는 링크가 방문되었을 때 */
selector:visited { }

/* 또는 요소가 포커스에 있을 때 */
selected:focus { }

/* 또는 커서가 요소 위에 있을 때 */
selector:hover { }

/* 또는 링크를 클릭했을 때 */
selector:active { }

/* 링크에 관한 이러한 의사 클래스는 항상 위 순서대로 작성해야 합니다. 그렇지 않으면 코드가 예상대로 작동하지 않을 수 있습니다. */

/* 부모의 첫 번째 자식인 모든 요소 */
selector:first-child {}

/* 부모의 마지막 자식인 모든 요소 */
selector:last-child {}

/* 선택자 부모의 n번째 자식 선택 (CSS 3) */
selector:nth-child(n) { }

/* 의사 클래스와 마찬가지로 의사 요소는 문서의 특정 부분을 스타일링할 수 있도록 합니다. */

/* 선택한 요소의 가상 첫 번째 자식과 일치 */
selector::before {}

/* 선택한 요소의 가상 마지막 자식과 일치 */
selector::after {}

/* 적절한 위치에서 별표는 모든 요소를 선택하는 와일드카드로 사용될 수 있습니다. */
* { } /* 모든 요소 */
.parent * { } /* 모든 후손 */
.parent > * { } /* 모든 자식 */

/* 그룹의 모든 선택자에 영향을 미치는 스타일을 정의하기 위해 여러 선택자를 그룹화합니다. */
selector1, selector2 { }

/* 특정 상태가 없는 요소를 선택합니다 (CSS 3) */
/* 여기서는 id 속성이 없는 div를 선택합니다. */
div:not([id]) {
   background-color: red;
}

/* ####################
   ## 속성
   #################### */

selector {

    /* 길이 단위는 절대 또는 상대일 수 있습니다. */

    /* 상대 단위 */
    width: 50%;       /* 부모 요소 너비의 백분율 */
    font-size: 2em;   /* 요소의 원래 글꼴 크기의 배수 */
    font-size: 2rem;  /* 또는 루트 요소의 글꼴 크기 */
    font-size: 2vw;   /* 뷰포트 너비의 1%의 배수 (CSS 3) */
    font-size: 2vh;   /* 또는 높이 */
    font-size: 2vmin; /* vh 또는 vw 중 더 작은 것 */
    font-size: 2vmax; /* 또는 더 큰 것 */

    /* 절대 단위 */
    width: 200px;     /* 픽셀 */
    font-size: 20pt;  /* 포인트 */
    width: 5cm;       /* 센티미터 */
    min-width: 50mm;  /* 밀리미터 */
    max-width: 5in;   /* 인치 */

    /* 색상 */
    color: #F6E;                    /* 짧은 16진수 형식 */
    color: #FF66EE;                 /* 긴 16진수 형식 */
    color: tomato;                  /* 명명된 색상 */
    color: rgb(255, 255, 255);      /* rgb 값으로 */
    color: rgb(10%, 20%, 50%);      /* rgb 백분율로 */
    color: rgba(255, 0, 0, 0.3);    /* rgba 값으로 (CSS 3) 참고: 0 <= a <= 1 */
    color: transparent;             /* 알파를 0으로 설정하는 것과 동일 */
    color: hsl(0, 100%, 50%);       /* hsl 백분율로 (CSS 3) */
    color: hsla(0, 100%, 50%, 0.3); /* 알파가 있는 hsl 백분율로 */

    /* 테두리 */
    border-width:5px;
    border-style:solid;
    border-color:red;      /* background-color 설정과 유사 */
    border: 5px solid red; /* 동일한 약어 접근 방식 */
    border-radius:20px;    /* CSS3 속성 */

    /* 요소의 배경 이미지 */
    background-image: url(/img-path/img.jpg); /* url() 내부의 따옴표는 선택 사항 */

    /* 글꼴 */
    font-family: Arial;
    /* 글꼴 패밀리 이름에 공백이 있으면 따옴표로 묶어야 합니다. */
    font-family: "Courier New";
    /* 첫 번째 글꼴을 찾을 수 없으면 브라우저는 다음 글꼴을 사용합니다. */
    font-family: "Courier New", Trebuchet, Arial, sans-serif;
}

/* 변수를 사용한 사용자 정의 CSS 속성 (CSS 3) */
:root {
   --main-bg-color: whitesmoke;
}
body {
   background-color: var(--main-bg-color)
}

/* 계산 수행 (CSS 3) */
body {
   width: calc(100vw - 100px)
}

/* 다른 스타일 규칙 내부에 중첩 (CSS 3) */
.main {
   .bgred { /* .main .bgred { }와 동일 */
      background: red;
   }
   & .bggreen { /* .main .bggreen { }와 동일 */
      background: green;
   }
   &.bgblue { /* (공백 없음) .main.bgblue { }와 동일 */
      background: blue;
   }
}

/* flexbox를 사용한 반응형 레이아웃 디자인 (CSS 3) */
.container {
   display: flex;
   flex-direction: row;      /* flex 항목을 쌓는 방향 */
   flex-wrap: wrap;          /* flex 항목이 줄 바꿈될지 여부 */
   justify-content: center;  /* flex 항목을 가로로 정렬하는 방법 */
   align-items: center;      /* flex 항목을 세로로 정렬하는 방법 */
}
```

## 사용법

CSS 스타일시트를 `.css` 확장자로 저장하십시오.

```html
<!-- CSS 파일을 페이지의 <head>에 포함해야 합니다. 이것이 권장되는 방법입니다. -->
<link rel='stylesheet' type='text/css' href='path/to/style.css'>

<!-- 일부 CSS를 마크업에 인라인으로 포함할 수도 있습니다. -->
<style>
   a { color: purple; }
</style>

<!-- 또는 요소에 직접 CSS 속성을 설정합니다. -->
<div style="border: 1px solid red;">
</div>
```

## 우선 순위 또는 캐스케이드

요소는 여러 선택자에 의해 대상으로 지정될 수 있으며, 한 번 이상 속성이 설정될 수 있습니다. 이러한 경우 규칙 중 하나가 다른 규칙보다 우선합니다. 더 구체적인 선택자를 가진 규칙이 덜 구체적인 선택자보다 우선하며, 스타일시트에 나중에 나타나는 규칙이 이전 규칙을 덮어씁니다(이는 두 개의 다른 연결된 스타일시트에 요소에 대한 규칙이 있고 규칙의 특이성이 동일한 경우 연결 순서가 우선하며 가장 나중에 연결된 시트가 스타일링을 제어한다는 의미이기도 합니다).

이 프로세스를 캐스케이딩이라고 하며, 캐스케이딩 스타일 시트라는 이름이 붙었습니다.

다음 CSS가 주어졌을 때:

```css
/* A */
p.class1[attr='value']

/* B */
p.class1 { }

/* C */
p.class2 { }

/* D */
p { }

/* E */
p { property: value !important; }
```

그리고 다음 마크업:

```html
<p style='/*F*/ property:value;' class='class1 class2' attr='value'>
```

스타일의 우선 순위는 다음과 같습니다. 각 **속성**에 대한 우선 순위이며 전체 블록에 대한 우선 순위가 아님을 기억하십시오.

* `E`는 `!important` 키워드 때문에 가장 높은 우선 순위를 가집니다. 사용을 피하는 것이 좋습니다.
* `F`는 인라인 스타일이기 때문에 다음입니다.
* `A`는 다른 어떤 것보다 더 "구체적"이기 때문에 다음입니다. 3개의 지정자를 가집니다: 요소 이름 `p`, 클래스 `class1`, 속성 `attr='value'`.
* `C`는 `B`와 동일한 특이성을 가짐에도 불구하고 다음입니다.
    `B` 다음에 나타나기 때문입니다.
* `B`는 다음입니다.
* `D`는 마지막입니다.

## 미디어 쿼리

CSS 미디어 쿼리는 CSS 3의 기능으로, 특정 CSS 규칙을 적용해야 하는 시기(예: 인쇄 시 또는 특정 크기나 픽셀 밀도를 가진 화면에서)를 지정할 수 있습니다. 선택자의 특이성을 추가하지 않습니다.

```css
/* 모든 장치에서 사용될 규칙 */
h1 {
  font-size: 2em;
  color: white;
  background-color: black;
}

/* 프린터에서 h1이 잉크를 덜 사용하도록 변경 */
@media print {
  h1 {
    color: black;
    background-color: white;
  }
}

/* 너비가 480px 이상인 화면에 표시될 때 글꼴을 더 크게 만듭니다. */
@media screen and (min-width: 480px) {
  h1 {
    font-size: 3em;
    font-weight: normal;
  }
}
```

미디어 쿼리에는 다음 기능이 포함될 수 있습니다:
`width`, `height`, `device-width`, `device-height`, `orientation`, `aspect-ratio`, `device-aspect-ratio`, `color`, `color-index`, `monochrome`, `resolution`, `scan`, `grid`. 이러한 기능 대부분은 `min-` 또는 `max-` 접두사를 가질 수 있습니다.

`resolution` 기능은 이전 장치에서 지원되지 않으며, 대신 `device-pixel-ratio`를 사용하십시오.

많은 스마트폰과 태블릿은 `viewport` 메타 태그를 제공하지 않으면 페이지를 데스크톱에서처럼 렌더링하려고 시도합니다.

```html
<head>
  <meta name="viewport" content="width=device-width; initial-scale=1.0">
</head>
```

## 호환성

CSS2의 대부분의 기능 및 CSS3의 많은 기능은은모든 브라우저 및 장치에서 사용할 수 있습니다. 그러나 항상 새 기능을 사용하기 전에 확인하는 것이 좋습니다.

## 자료

* [CanIUse](http://caniuse.com) (자세한 호환성 정보)
* [Dabblet](http://dabblet.com/) (CSS 플레이그라운드)
* [Mozilla 개발자 네트워크의 CSS 문서](https://developer.mozilla.org/en-US/docs/Web/CSS) (튜토리얼 및 참조)
* [Codrops의 CSS 참조](http://tympanus.net/codrops/css_reference/) (참조)
* [DevTips의 CSS 기본 사항](https://www.youtube.com/playlist?list=PLqGj3iMvMa4IOmy04kDxh_hqODMqoeeCy) (튜토리얼)

## 더 읽을거리

* [CSS의 스타일 우선 순위 이해: 특이성, 상속 및 캐스케이드](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [속성을 사용하여 요소 선택](https://css-tricks.com/almanac/selectors/a/attribute/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - 스택 컨텍스트](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
* [SASS](http://sass-lang.com/) 및 [LESS](http://lesscss.org/) CSS 전처리용
* [CSS-Tricks](https://css-tricks.com)