---
name: Texinfo
contributors:
    - ["Julien Lepiller", "https://github.com/roptat"]
filename: learntexinfo.texi
---

Texinfo는 동일한 소스에서 다양한 유형의 문서를 만드는 데 사용할 수 있는 문서 형식입니다. 주요 용도는 GNU 프로젝트의 문서 매뉴얼 및 정보 페이지를 만드는 것입니다.

Texinfo는 텍스트와 생성기가 수행해야 할 작업을 지정하는 *@-명령*을 포함하는 마크업 언어입니다.

## 초기 파일

간단한 매뉴얼의 간단한 예:

```
\input texinfo
@setfilename simple-document.info
@documentencoding UTF-8
@settitle simple-document
@c 이것은 주석입니다
@c 위의 simple-document를 (두 번) 실제 문서 제목으로 바꾸십시오.

@c Automake가 version.texi를 처리합니다.
@include version.texi

@copying
Copyright @copyright{} YEAR MY NAME

@c GFDL은 GNU 프로젝트에 일반적입니다.
@quotation
이 문서를 복사, 배포 및/또는 수정하는 것은
자유 소프트웨어 재단에서 발행한 GNU 자유 문서 사용 허가서 버전 1.3 또는
그 이후 버전의 조건에 따라 허용됩니다. 불변 섹션, 앞표지 텍스트 및
뒷표지 텍스트는 없습니다. 라이선스 사본은
"GNU 자유 문서 사용 허가서"라는 제목의 섹션에 포함되어 있습니다.
@end quotation
@end copying

@titlepage
@end titlepage

@c 이제 실제 내용이 시작됩니다.
@contents

@c 첫 번째 노드는 항상 Top이어야 합니다.
@node Top
@c 그리고 제목을 지정합니다.
@top simple-document

이 문서는 Texinfo 기능을 간략하게 설명합니다.

@c 이것은 목차입니다:
@menu
* Introduction::           이 장의 간략한 요약

@detailmenu
--- 자세한 노드 목록 ---

Introduction

* Formatting::             텍스트를 멋지게 서식 지정하는 방법
* Links::                  다른 리소스, 페이지 또는 매뉴얼에 연결

@end detailmenu
@end menu

@node Introduction
@chapter Introduction

각 노드는 목차에 정의된 메뉴 항목과 동일한 이름을 가져야 합니다.

@node Formatting
@section Formatting
@c 내용 색인에 무언가를 추가하여 사람들이 검색할 때
@c 여기에 올 수 있도록 합니다.
@cindex 굵은 텍스트
@cindex 제목

장과 마찬가지로 섹션은 동일한 이름을 가져야 하며 동일한 순서로 나타나야 합니다.

@subsection 이것은 하위 섹션 제목입니다
@subsubsection 이것은 하위 하위 섹션 제목입니다

각 텍스트 블록은 단락입니다. 단락에 여러 줄을 사용할 수 있으며,
빈 줄만 단락을 구분합니다.

일반적인 서식에는 @emph{강조}, @code{인라인 코드}가 포함됩니다. 특정 유형의
텍스트도 표시할 수 있습니다: @file{file.txt}, @option{--learn-fast},
@command{ls} 또는 @var{variable}. 다음과 같이 명령 문자를 이스케이프할 수 있습니다:
@@, 그리고 줄 끝에 단일 @@를 사용하여 줄 바꿈.

다양한 유형의 블록을 추가할 수 있습니다:

@example
다음은 예입니다.
@end example

@lisp
'(이것은 lisp 코드입니다)
@end lisp

@itemize
@item 정렬되지 않은 목록의 요소
@item 동일한 목록의 두 번째 요소
@end itemize

@enumerate
@item 이 목록은 유사합니다
@item 하지만 정렬됨
@end enumerate

@quotation
유명한 사람이 한 인용문 블록일 수 있습니다.
@end quotation

@table @asis
@item 요소 제목
요소 설명

@item 두 번째 요소 제목
두 번째 요소 설명. 설명 부분은 여러 단락에 걸쳐 있을 수 있으며,
다른 블록 등을 포함할 수 있습니다. 이것은 일반적으로 정의 목록으로 사용됩니다.

@code{@@asis}는 요소 제목을 감싸고 Texinfo에 그대로 사용하도록 지시합니다.
@end table

@table @code
@item do-x
이 항목 제목은 이제 @code{@@code{do-x}}와 같이 코드 블록으로 래핑됩니다.
@end table

@c 내용 색인은 문서의 어느 곳에나 나타날 수 있으며, 반드시
@c 제목 뒤에 올 필요는 없습니다.
@cindex 함수 정의
@deffn {함수의 종류} function_name @var{arg1} @var{arg2} @
  @var{arg3} @var{arg4} [@var{optional5}]
이 텍스트는 함수를 설명합니다. 단일 @@로 줄을 이스케이프하여
함수 개요에 여러 줄을 사용할 수 있는 방법을 확인하십시오.

이것은 다시 여러 단락이나 블록을 포함할 수 있습니다.
@end deffn

@node Links
@section Links

사용할 수 있는 다양한 유형의 링크가 있습니다. @uref{https://github.com}을 사용하여
URL에 대한 간단한 링크를 만들고 선택적으로 제목을 추가할 수 있습니다:
@uref{https://github.com, GitHub}. 이메일 주소 @email{me@@me.me}.
이 문서의 노드, @xref{Introduction}. 항상 해당 노드의 정확한 이름을
사용하십시오. @code{xref}는 링크 앞에 "see" 텍스트를 포함합니다.
다른 것을 삽입하려면 @pxref{Introduction}("See") 또는
@xref{Introduction}(아무것도 삽입되지 않음)을 사용하십시오. 추가 인수를 사용하여
링크 텍스트를 변경할 수 있습니다. @xref{Introduction, this introduction}.

@code{@@xref{Node name,,, manual-name, link text}}와 같이
더 많은 인수를 추가하여 이러한 명령으로 외부 매뉴얼에 연결할 수 있습니다.
Texinfo에 대한 전체 참조는 @xref{Overview,,, texinfo, Texinfo's manual}를 참조하십시오!

@bye
```

## 사용 방법

`automake`를 사용하면 `Makefile.am`에 매뉴얼 경로를
지정하기만 하면 됩니다:

```
info_TEXINFOS= doc/simple-manual.texi
```

그런 다음 `make doc/simple-manual.info`로 정보 매뉴얼을 얻거나 다른 형식으로,
예를 들어 `make doc/simple-manual.html`로 HTML을 얻을 수 있습니다.

## 읽을거리

- [공식 매뉴얼](https://www.gnu.org/software/texinfo/manual/texinfo/html_node/)
