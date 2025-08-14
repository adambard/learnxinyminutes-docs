---
name: Wikitext
contributors:
    - ["Yuxi Liu", "https://github.com/yuxiliu1995/"]
filename: wikitext.md
---

위키는 온라인에서 공동으로 편집하는 하이퍼텍스트 출판물이며, 가장 유명한 것은 위키백과입니다. 위키텍스트는 위키에서 사용하는 마크업 언어입니다. 구문은 마크다운과 HTML의 혼합과 유사합니다.

## 구문

`<!--- 주석은 읽을 때는 숨겨지지만 편집할 때는 보입니다 --->`

| 위키텍스트 | 동등한 마크다운 | 효과 |
| ---- | ---- | ---- |
| `''기울임꼴''` | `*기울임꼴*` | *기울임꼴* |
| `'''굵게'''` | `**굵게**` | **굵게** |
| `'''''둘 다'''''` | `***둘 다***` | ***둘 다*** |
| `<u>밑줄</u>` | `<u>밑줄</u>` | <u>밑줄</u> |
| `<nowiki>렌더링하지 않음</nowiki>` | 해당 없음 | `렌더링하지 않음` |
| `<code>인라인 코드 조각</code>` | \`인라인 코드 조각\` | `인라인 코드 조각` |
| `----` | `----` | 수평선 |
| `<s>취소선</s>` | `~~취소선~~` | ~~취소선~~ |

섹션 제목은 `=`로 묶습니다. `= 한 개의 등호 =`에서 `====== 여섯 개의 등호 ======`까지 있습니다. 이것은 마크다운의 해시태그 제목, `# 한 개의 해시태그`에서 `###### 여섯 개의 해시태그`까지와 동일합니다. 왜 둘 다 여섯 개일까요? HTML에 `<h1>`에서 `<h6>`까지 여섯 단계의 제목이 있기 때문이라고 생각합니다.

`= 한 개의 등호 =` 제목은 실제로는 페이지의 제목에 해당하며, 따라서 페이지 내에서 실제로 사용할 수 없습니다. 결과적으로, 가장 적은 수의 등호는 `== 두 개의 등호 ==`입니다.

아래 첨자와 위 첨자는 `x<sub>1</sub>` 및 `x<sup>1</sup>`으로 쓸 수 있습니다. 또는 `<math>` 태그로 쓸 수 있습니다(아래 참조). `<small>작게</small>` 및 `<big>크게</big>` 텍스트는 거의 사용되지 않습니다.

```wikitext
콜론은 들여쓰기를 허용합니다.
   :각 콜론은 세 문자 너비의 들여쓰기를 만듭니다.
      ::그리고 중첩될 수 있습니다.
```

`*` 번호 없는 목록은 `*`로 시작하고, 번호 있는 목록은 `#`로 시작합니다. <br>
&emsp; `**` 목록은 중첩될 수 있습니다 <br>
&emsp; &emsp; `***` 임의의 많은 수준에 대해.

표의 구문은 [매우 복잡합니다](https://en.wikipedia.org/wiki/Help:Table). [간단한 표](https://en.wikipedia.org/wiki/Help:Basic_table_markup) 중 가장 간단한 것은 다음과 같습니다:

```wikitext
{| class="wikitable"
|+
! 열 제목 A
! 열 제목 B
|-
| 셀 A1
| 셀 B1
|-
| 셀 A2
| 셀 B2
|-
| ...
| ...
|}
```

다음과 같이 렌더링됩니다.

| **열 제목 A** | **열 제목 B** |
|---|---|
| 셀 A1 | 셀 B1 |
| 셀 A2 | 셀 B2 |

위키텍스트 표의 개행은 의미가 있다는 점에 유의하십시오. 위의 단일 개행을 삭제하면 렌더링된 표의 모양이 완전히 바뀝니다.

`[[File:Image.png|thumb|right|Image caption]]`으로 이미지, 오디오, 비디오 또는 기타 미디어 형식을 삽입할 수 있습니다. 모든 미디어 파일은 [Wikimedia Commons](https://commons.wikimedia.org/wiki/Main_Page)에 호스팅되어야 합니다.

HTML과 유사한 태그로 인용문을 삽입할 수 있습니다.

```wikitext
<blockquote>
<p>인용문 텍스트.</p>
<p>이름, 출처, 참조</p>
</blockquote>
```

또는 [템플릿](#템플릿)

```wikitext
{{Quote|text=인용문 텍스트.|title=제목|author=저자|source=출판물 내 위치}}
```

"[줄 바꿈 없는 공백](https://en.wikipedia.org/wiki/Non-breaking_space)"은 "400km/h"의 공백과 같이 줄 바꿈으로 분리되어서는 안 되는 공백입니다. 이것은 `400&amp;nbsp;km/h`로 작성됩니다.

추가 공백은 `pad` 태그로 지정할 수 있습니다. 예를 들어, `{{pad|4.0em}}`은 길이 4.0 [em-대시](https://en.wikipedia.org/wiki/Dash#Em_dash)의 공백입니다.

더 긴 코드 블록은 다음과 같이 할 수 있습니다.

```wikitext
<syntaxhighlight lang="cpp">
#include <iostream>
int m2 (int ax, char *p_ax) {
  std::cout <<"Hello World!";
  return 0;
}</syntaxhighlight>
```

다음과 같이 렌더링됩니다.

```cpp
#include <iostream>
int m2 (int ax, char *p_ax) {
  std::cout <<"Hello World!";
  return 0;
}
```

## 링크

기본 `[[링크]]`는 이중 대괄호로 수행됩니다.

`|` 기호는 `[[실제 페이지 제목|다른 텍스트]]`를 표시할 수 있습니다.

`#` 기호는 `[[Frog#Locomotion]]` 또는 `[[Frog#Locomotion|개구리의 이동]]`과 같이 텍스트 내 섹션에 연결할 수 있습니다.

단어가 링크로 중단되면 링크에 "혼합"됩니다. 예를 들어, `[[copy edit]]ors`는 [copy editors](https://en.wikipedia.org/wiki/copy_edit)로 렌더링됩니다.

이 동작을 억제하려면 `<nowiki>`를 사용하십시오. 예를 들어, `[[micro-]]<nowiki />second`는 [micro-](https://en.wikipedia.org/wiki/micro-)second로 렌더링됩니다.

외부 링크에는 세 가지 종류가 있습니다. 세 번째 종류가 선호됩니다:

| 위키텍스트 | 렌더링 결과 |
|----|----|
| `https://www.wikipedia.org` | [https://www.wikipedia.org](https://www.wikipedia.org) |
| `[https://www.wikipedia.org]` | [[1]](https://www.wikipedia.org) |
| `[https://www.wikipedia.org Wikipedia]` | [Wikipedia](https://www.wikipedia.org) |

## 템플릿

템플릿은 위키텍스트용 매크로이며, `{{템플릿 이름|속성=값|...}}`처럼 보입니다. 수천 개의 템플릿이 있지만 일반적으로 사용되는 것은 몇 가지뿐입니다.

가장 (악)명 높은 것은 \[출처 필요\]`{{cn}}` 템플릿입니다. `{{cn}}`은 `{{citation needed}}`와 동의어이며, 한 템플릿이 여러 이름을 가질 수 있다는 점에 유의하십시오.

`{{reflist}}`는 일반적으로 페이지 끝에 배치되어 페이지에서 사용된 참조 목록을 생성합니다.

`infobox` 템플릿은 이름에서 알 수 있듯이 정보를 포함하는 상자용 템플릿입니다. 일반적으로 각 페이지에는 위쪽에 하나, 아래쪽에 하나, 최대 두 개의 정보 상자가 포함됩니다. 특히 상세한 페이지의 경우 두 개 이상 있을 수 있습니다.

위쪽 정보 상자는 일반적으로 표 형식 정보를 간결하게 표시하는 데 사용됩니다. 전기, 지리적 위치 등에 일반적입니다. 예를 들어, [오일러](https://en.wikipedia.org/wiki/Leonhard_Euler)의 위쪽 정보 상자는 다음과 같습니다:

```wikitext
{{Infobox scientist
| name              = Leonhard Euler
| image             = Leonhard Euler.jpg
| caption           = Portrait by [[Jakob Emanuel Handmann]], 1753
| birth_date        = {{birth date|df=y|1707|4|15}}
| birth_place       = [[Basel]], [[Swiss&nbsp;Confederacy]]
| death_date        = {{nowrap|{{death date and age|df=y|1783|9|18|1707|4|15}}}} {{awrap|{{bracket|[[Adoption of the Gregorian calendar#Adoption in Eastern Europe|OS]]: 7 September 1783}}}}
...
}}
```

아래쪽 정보 상자는 일반적으로 관련 링크의 선별된 표를 표시하는 데 사용됩니다. 예를 들어, [오일러-라그랑주 방정식](https://en.wikipedia.org/wiki/Euler%E2%80%93Lagrange_equation)의 아래쪽 정보 상자는 `{{Leonhard Euler}}`뿐이며, 오일러의 이름을 딴 많은 것들에 대한 링크가 포함된 상자를 표시합니다.

`~~~~`는 토론 페이지에 서명하는 데 사용되며, `Username (talk) 10:50, 12 June 2023 (UTC)`와 같이 확장됩니다.

### 수학

`<math>` 태그는 `$`처럼 $\LaTeX$를 인라인으로 렌더링하고, `<math display=block>`은 `$$`처럼 별도의 줄에 렌더링합니다.

`<math>E = mc^2</math>`는 $E = mc^2$로 렌더링됩니다.

`<math display=block></math>`는 $$E = mc^2$$로 렌더링됩니다.

[HTML 렌더링](https://en.wikipedia.org/wiki/Wikipedia:Manual_of_Style/Mathematics#Using_HTML)을 사용하거나 [일반 유니코드](https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode)를 사용하여 수학을 포함할 수도 있습니다. 이것들은 덜 유연하지만 이전 브라우저와 더 호환됩니다. 또한 위키백과 구문의 일부는 섹션 제목이나 일부 템플릿에서와 같이 `<math>`와 호환되지 않으므로 이러한 경우 HTML 또는 유니코드를 사용해야 합니다.

정리와 증명은 상자로 묶고 이름을 지정할 수 있습니다:

```wikitext
{{Math theorem
|name=피타고라스 정리
|note=피타고라스, 기원전 500년대
|math_statement=직각삼각형의 세 변의 길이를 <math>a, b, c</math>라고 하면
<math display=block>a^2 + b^2 = c^2</math>
}}

{{Math proof
|title=닮은 삼각형에 의한 증명
|proof=점 C에서 변 AB에 수선을 내립니다. 이제 비례 관계로 논증합니다. <math>\blacksquare</math>
}}
```

## 참조

참조는 위키백과의 근간입니다 `{{출처 필요}}`. 일반적으로 인용에는 두 가지 방법이 있습니다.

| 유형 | 인라인 인용 | 확장 인용 |
| ---- | ---- | ---- |
| 목적 | 특정 주장을 뒷받침합니다. | 전체 페이지에 대한 일반 참조 작업을 제공합니다. |
| 위치 | 지원되는 주장 바로 뒤. | `== 참조 ==` 섹션. |
| 모양 | _f_의 해석적 연속.<sup>[\[6\]](#6)</sup> | Abramowitz, Milton; Stegun, Irene A., eds. (1972). ["Chapter 6"](http://www.math.sfu.ca/~cbm/aands/page_253.htm)... |
| 구문 | `<ref>{{cite book\|...}}</ref>` | `{{cite book\|...}}` |

확장 인용은 `<ref>` 태그가 없는 인라인 인용이므로 인라인 인용만 설명합니다.

가장 기본적인 형태는 `<ref>저자, 제목, 날짜, [url](https://example.com/) 등</ref>`과 같은 일반 텍스트 인용입니다.

일반적으로 `<ref>{{cite web|url=https://example.com/|title=예제|date=2001|access-date=2023}}</ref>`와 같은 템플릿 인용을 사용해야 합니다. 인용 템플릿에는 [`cite web`](https://en.wikipedia.org/wiki/Template:Cite_web), [`cite journal`](https://en.wikipedia.org/wiki/Template:Cite_journal), [`cite book`](https://en.wikipedia.org/wiki/Template:Cite_book)의 세 가지 형태가 있습니다.

인용은 `<ref name="X">...</ref>`로 이름을 지정할 수 있습니다. 그런 다음 `<ref name="X" />`로 호출할 수 있습니다. 인스턴스 `<ref name="X">...</ref>`는 `<ref name="X" />` 앞이나 뒤에 올 수 있습니다. 어떤 순서든 동일한 페이지로 렌더링됩니다.

## 일반적인 위키백과 페이지

```wikitext
{{짧은 설명|페이지의 한 문장 요약}}

{{위쪽 정보 상자
|infobox_data_1=...
|...
}}

[[File:X의 이미지.png|thumb|right|이미지 캡션]]

'''X''' 개념은 일반적으로 굵게 표시됩니다. 이제 X 개념을 정의합니다. 비전문가 페이지의 경우 이 섹션은 평이한 언어로 작성되어야 하며, 전문 용어는 인라인으로 정의됩니다. 일부 [[링크]]가 도움이 될 것입니다.


== 소개 ==

여기서는 일반적으로 표기법을 설정하고, 역사를 개괄하며, 기타 등등을 합니다. 자세한 내용은 다음 섹션에서 다룹니다.

각주는 인라인 참조와 별도로 번호가 매겨집니다.{{NoteTag|note=각주 텍스트.}}

== Y와의 관계 ==
{{Main|Y}}
{{See also|다른 페이지}}

X와 Y의 관계에 대한 무언가.

== 함께 보기 ==
* [[매우 관련 있는 링크]]
* [[덜 관련 있는 링크]]

== 외부 링크 ==
* [https://example.com/ 외부 링크 1]: 외부 링크에 있는 내용 요약.

== 각주 ==

<references group="note" />{{Notelist}}

== 참조 ==
<!-- 인라인 참조 태그에서 참조 목록 생성, 최소 너비 30em-대시의 열 포함. -->
{{Reflist|30em}}

<!-- 추가, 인라인되지 않은 참조 아래 -->
{{Refbegin|30em}}
* {{cite book|title=책 제목|date=2001|chapter=1장|...}}
* ...

== 더 읽을거리 ==
* ...
* ...

{{아래쪽 정보 상자}}

[[Category:기사가 속한 첫 번째 카테고리]]
[[Category:기사가 속한 첫 번째 카테고리]]
[[Category:허용되는 카테고리 수에는 제한이 없습니다]]
```

## 더 읽을거리

* [위키백과의 스타일 매뉴얼](https://en.wikipedia.org/wiki/Wikipedia:Manual_of_Style)
* [위키텍스트 치트 시트](https://en.wikipedia.org/wiki/Help:Cheatsheet)
* [위키텍스트, 전체 참조](https://en.wikipedia.org/wiki/Help:Wikitext).
* [표, 전체 참조](https://en.wikipedia.org/wiki/Help:Table#Simple_straightforward_tables)
