---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
    - ["Jacob Ward", "http://github.com/JacobCWard/"]
filename: markdown-kr.md
lang: ko-kr
---

마크다운은 2004년에 존 그루버가 창시했습니다. HTML으로 (그리고 이제는 다른 다양한 형식으로도) 쉽게 변환되는 읽고 쓰기 쉬운 문법입니다.

마크다운은 또한 파서마다 구현이 다양합니다. 본 문서는 어떤 기능이 보편적인지,
혹은 어떤 기능이 특정 파서에 종속되어 있는지 명확히 하고자 합니다.

- [HTML 요소](#html-elements)
- [제목](#headings)
- [간단한 텍스트 꾸미기](#simple-text-styles)
- [문단](#paragraphs)
- [목록](#lists)
- [코드](#code-blocks)
- [수평선](#horizontal-rule)
- [링크](#links)
- [이미지](#images)
- [기타](#miscellany)

## HTML 요소
HTML은 마크다운의 수퍼셋입니다. 모든 HTML 파일은 유효한 마크다운이라는 것입니다.
```markdown
<!--따라서 주석과 같은 HTML 요소들을 마크다운에 사용할 수 있으며, 마크다운 파서에 영향을
받지 않을 것입니다. 하지만 마크다운 파일에서 HTML 요소를 만든다면 그 요소의 안에서는
마크다운 문법을 사용할 수 없습니다.-->
```
## 제목

텍스트 앞에 붙이는 우물 정 기호(#)의 갯수에 따라 `<h1>`부터 `<h6>`까지의 HTML 요소를
손쉽게 작성할 수 있습니다.
```markdown
# <h1>입니다.
## <h2>입니다.
### <h3>입니다.
#### <h4>입니다.
##### <h5>입니다.
###### <h6>입니다.
```
또한 h1과 h2를 나타내는 다른 방법이 있습니다.
```markdown
h1입니다.
=============

h2입니다.
-------------
```
## 간단한 텍스트 꾸미기

마크다운으로 쉽게 텍스트를 기울이거나 굵게 할 수 있습니다.
```markdown
*기울인 텍스트입니다.*
_이 텍스트도 같습니다._

**굵은 텍스트입니다.**
__이 텍스트도 같습니다.__

***기울인 굵은 텍스트입니다.***
**_이 텍스트도 같습니다._**
*__이것도 같습니다.__*
```
깃헙 전용 마크다운에는 취소선도 있습니다.
```markdown
~~이 텍스트에는 취소선이 그려집니다.~~
```
## 문단

문단은 하나 이상의 빈 줄로 구분되는, 한 줄 이상의 인접한 텍스트입니다.

```markdown
문단입니다. 문단에 글을 쓰다니 재밌지 않나요?

이제 두 번째 문단입니다.
아직도 두 번째 문단입니다.

나는 세 번째 문단!
```
HTML `<br />` 태그를 삽입하고 싶으시다면, 두 개 이상의 띄어쓰기로 문단을 끝내고
새 문단을 시작할 수 있습니다.

```markdown
띄어쓰기 두 개로 끝나는 문단 (마우스로 긁어 보세요).  

이 위에는 `<br />` 태그가 있습니다.
```

인용문은 > 문자로 쉽게 쓸 수 있습니다.

```markdown
> 인용문입니다. 수동으로 개행하고서
> 줄마다 `>`를 칠 수도 있고 줄을 길게 쓴 다음에 저절로 개행되게 내버려 둘 수도 있습니다.
> `>`로 시작하기만 한다면 차이가 없습니다.

> 한 단계 이상의 들여쓰기를
>> 사용할 수도 있습니다.
> 깔끔하죠?
```

## 목록
순서가 없는 목록은 별표, 더하기, 하이픈을 이용해 만들 수 있습니다.
```markdown
* 이거
* 저거
* 그거
```

또는

```markdown
+ 이거
+ 저거
+ 그거
```

또는

```markdown
- 이거
- 저거
- 그거
```

순서가 있는 목록은 숫자와 마침표입니다.

```markdown
1. 하나
2. 둘
3. 셋
```

숫자를 정확히 붙이지 않더라도 제대로 된 순서로 보여주겠지만, 좋은 생각은 아닙니다.

```markdown
1. 하나
1. 둘
1. 셋
```
(위의 예시와 똑같이 나타납니다.)

목록 안에 목록이 올 수도 있습니다.

```markdown
1. 하나
2. 둘
3. 셋
    * 이거
    * 저거
4. 넷
```

심지어 할 일 목록도 있습니다. HTML 체크박스가 만들어집니다.

```markdown
x가 없는 박스들은 체크되지 않은 HTML 체크박스입니다.
- [ ] 첫 번째 할 일
- [ ] 두 번째 할 일
이 체크박스는 체크된 HTML 체크박스입니다.
- [x] 완료된 일
```

## 코드

띄어쓰기 네 개 혹은 탭 한 개로 줄을 들여씀으로서 (`<code> 요소를 사용하여`) 코드를
나타낼 수 있습니다.

```markdown
    puts "Hello, world!"
```

탭을 더 치거나 띄어쓰기를 네 번 더 함으로써 코드를 들여쓸 수 있습니다.

```markdown
    my_array.each do |item|
        puts item
    end
```

인라인 코드는 백틱 문자를 이용하여 나타냅니다. `

```markdown
철수는 `go_to()` 함수가 뭘 했는지도 몰랐어!
```

깃헙 전용 마크다운에서는 코드를 나타내기 위해 특별한 문법을 쓸 수 있습니다.

<pre>
<code class="highlight">&#x60;&#x60;&#x60;ruby
def foobar
    puts "Hello world!"
end
&#x60;&#x60;&#x60;</code></pre>

위의 경우에 들여쓰기가 필요없을 뿐 아니라 \`\`\` 뒤에 특정해 준 언어의 문법에 따라
색을 입혀줄 것입니다.

## 수평선

수평선(`<hr/>`)은 셋 이상의 별표나 하이픈을 이용해 쉽게 나타낼 수 있습니다.
띄어쓰기가 포함될 수 있습니다.
```markdown
***
---
- - -
****************
```
## 링크

마크다운의 장점 중 하나는 링크를 만들기 쉽다는 것입니다. 대괄호 안에 나타낼 텍스트를 쓰고
괄호 안에 URL을 쓰면 됩니다.

```markdown
[클릭](http://test.com/)
```

괄호 안에 따옴표를 이용해 링크에 제목을 달 수도 있습니다.

```markdown
[클릭](http://test.com/ "test.com으로 가기")
```

상대 경로도 유효합니다.

```markdown
[music으로 가기](/music/).
```

참조하는 식으로 링크를 걸 수도 있습니다.

<pre><code class="highlight">&#x5b;<span class="nv">이 </span>][<span class="ss">링크</span>]에서 더 알아보세요!
&#x5b;<span class="nv">원하신다면 </span>][<span class="ss">foobar</span>]도 참고하세요.

&#x5b;<span class="nv">링크</span>]: <span class="sx">http://test.com/</span> <span class="nn">"좋아!"</span>
&#x5b;<span class="nv">foobar</span>]: <span class="sx">http://foobar.biz/</span> <span class="nn">"됐다!"</span></code></pre>

제목은 작은 따옴표나 괄호에 들어갈 수도 있고, 완전히 생략할 수도 있습니다. 참조는 문서의
어느 곳에든 올 수 있고 참조 ID는 유일하다면 무엇이든 될 수 있습니다.

링크 텍스트를 ID로 사용하는 "묵시적 이름"도 있습니다.

<pre><code class="highlight">&#x5b;<span class="nv">이것</span>][]은 링크입니다.

&#x5b;<span class="nv">이것</span>]: <span class="sx">http://thisisalink.com/</span></code></pre>

하지만 보통 그렇게 추천하지는 않습니다.

## 이미지
이미지는 링크와 같지만 앞에 느낌표가 붙습니다.

```markdown
![이미지의 alt 속성](http://imgur.com/myimage.jpg "제목")
```

참조 방식도 가능합니다.

<pre><code class="highlight">!&#x5b;<span class="nv">alt 속성</span>][<span class="ss">이미지</span>]

&#x5b;<span class="nv">이미지</span>]: <span class="sx">relative/urls/cool/image.jpg</span> <span class="nn">"제목이 필요하다면 여기에"</span></code></pre>

## 기타
### 자동 링크

```markdown
<http://testwebsite.com/>와
[http://testwebsite.com/](http://testwebsite.com/)는 동일합니다.
```

### 이메일 자동 링크
```markdown
<foo@bar.com>
```
### 탈출 문자

```markdown
*별표 사이에 이 텍스트*를 치고 싶지만 기울이고 싶지는 않다면
이렇게 하시면 됩니다. \*별표 사이에 이 텍스트\*.
```

### 키보드 키

깃헙 전용 마크다운에서는 `<kbd>` 태그를 이용해 키보드 키를 나타낼 수 있습니다.

```markdown
컴퓨터가 멈췄다면 눌러보세요.
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>
```

### 표

표는 깃헙 전용 마크다운에서만 쓸 수 있고 다소 복잡하지만, 정말 쓰고 싶으시다면
```markdown
| 1열      | 2열        | 3열       |
| :--------| :-------: | --------: |
| 왼쪽 정렬 | 가운데 정렬 | 오른쪽 정렬 |
| 머시기    | 머시기     | 머시기     |
```
혹은
```markdown
1열 | 2열 | 3열
:-- | :-: | --:
으악 너무 못생겼어 | 그만 | 둬
```
---
추가 정보를 위해, 존 그루버의 공식 문법 [(영어) 문서](http://daringfireball.net/projects/markdown/syntax)와 애덤 프릿차드의 훌륭한 [(영어) 치트싯](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)을 확인하세요.
