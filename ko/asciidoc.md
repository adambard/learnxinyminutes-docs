---
name: AsciiDoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
    - ["Abel Salgado Romero", "https://twitter.com/abelsromero"]
filename: asciidoc.adoc
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

AsciiDoc은 마크다운과 유사한 마크업 언어로, 책에서 블로그에 이르기까지 모든 것에 사용할 수 있습니다. 2002년 Stuart Rackham이 만든 이 언어는 간단하지만 뛰어난 사용자 정의 기능을 제공합니다.

문서 헤더

헤더는 선택 사항이며 빈 줄을 포함할 수 없습니다. 콘텐츠와 최소 한 줄 이상의 빈 줄로 구분되어야 합니다.

제목만

```
= 문서 제목

문서의 첫 문장.
```

제목 및 저자

```
= 문서 제목
First Last <first.last@learnxinyminutes.com>

이 문서의 시작.
```

여러 저자

```
= 문서 제목
John Doe <john@go.com>; Jane Doe<jane@yo.com>; Black Beard <beardy@pirate.com>

여러 저자가 있는 문서의 시작.
```

개정판 줄 (저자 줄 필요)

```
= Doc Title V1
Potato Man <chip@crunchy.com>
v1.0, 2016-01-13

칩에 대한 이 기사는 재미있을 것입니다.
```

단락

```
단락에는 특별한 것이 필요하지 않습니다.

단락을 구분하려면 단락 사이에 빈 줄을 추가하십시오.

줄 바꿈을 만들려면 +
를 추가하면 줄 바꿈이 됩니다!
```

텍스트 서식 지정

```
_밑줄은 기울임꼴을 만듭니다_
*별표는 굵게*
*_재미를 더하기 위해 결합_*
`단일 인용 부호는 고정 폭을 나타냅니다`
`*굵은 고정 폭*`
```

섹션 제목

```
= 레벨 0 (문서 헤더에서만 사용할 수 있음)

== 레벨 1 <h2>

=== 레벨 2 <h3>

==== 레벨 3 <h4>

===== 레벨 4 <h5>
```

목록

글머리 기호 목록을 만들려면 별표를 사용하십시오.

```
* foo
* bar
* baz
```

번호 매기기 목록을 만들려면 마침표를 사용하십시오.

```
. 항목 1
. 항목 2
. 항목 3
```

최대 5번까지 추가 별표나 마침표를 추가하여 목록을 중첩할 수 있습니다.

```
* foo 1
** foo 2
*** foo 3
**** foo 4
***** foo 5

. foo 1
.. foo 2
... foo 3
.... foo 4
..... foo 5
```

## 더 읽을거리

AsciiDoc 문서를 처리하는 두 가지 도구가 있습니다:

1. [AsciiDoc](http://asciidoc.org/): 주요 Linux 배포판에서 사용할 수 있는 원래 Python 구현입니다. 안정적이며 현재 유지 관리 모드입니다.
2. [Asciidoctor](http://asciidoctor.org/): Java 및 JavaScript에서도 사용할 수 있는 대체 Ruby 구현입니다. 활발하게 개발 중이며 새로운 기능과 출력 형식으로 AsciiDoc 구문을 확장하는 것을 목표로 합니다.

다음 링크는 `Asciidoctor` 구현과 관련이 있습니다:

* [마크다운 - AsciiDoc 구문 비교](http://asciidoctor.org/docs/user-manual/#comparison-by-example): 일반적인 마크다운 및 AsciiDoc 요소의 나란히 비교.
* [시작하기](http://asciidoctor.org/docs/#get-started-with-asciidoctor): 간단한 문서를 렌더링하기 위한 설치 및 빠른 시작 가이드.
* [Asciidoctor 사용자 설명서](http://asciidoctor.org/docs/user-manual/): 구문 참조, 예제, 렌더링 도구 등이 포함된 완전한 단일 문서 설명서.