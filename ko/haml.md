---
name: Haml
filename: learnhaml.haml
contributors:
  - ["Simon Neveu", "https://github.com/sneveu"]
  - ["Vasiliy Petrov", "https://github.com/Saugardas"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Haml은 주로 Ruby와 함께 사용되는 마크업 언어로, 인라인 코드 없이 모든 웹 문서의 HTML을 깔끔하고 간단하게 설명합니다. Rails 템플릿 언어(.erb)를 사용하는 인기 있는 대안이며, 마크업에 Ruby 코드를 포함할 수 있습니다.

코드의 들여쓰기 구조에 따라 태그를 닫아 마크업의 반복을 줄이는 것을 목표로 합니다. 그 결과 잘 구조화되고, DRY하며, 논리적이고, 읽기 쉬운 마크업이 됩니다.

Ruby와 독립적인 프로젝트에서 Haml을 사용할 수도 있습니다. 컴퓨터에 Haml gem을 설치하고 명령줄을 사용하여 html로 변환하면 됩니다.

```shell
$ haml input_file.haml output_file.html
```


```haml
/ -------------------------------------------
/ 들여쓰기
/ -------------------------------------------

/
  코드가 렌더링되는 방식에 들여쓰기가 중요하기 때문에, 문서 전체에서 들여쓰기는 일관되어야 합니다. 들여쓰기에 차이가 있으면 오류가 발생합니다. 두 칸 공백을 사용하는 것이 일반적이지만, 일관성만 있다면 정말로 여러분에게 달려 있습니다.


/ -------------------------------------------
/ 주석
/ -------------------------------------------

/ 이것이 Haml의 주석 모양입니다.

/
  여러 줄 주석을 작성하려면, 주석 처리된 코드를 슬래시로 감싸도록 들여쓰십시오.

-# 이것은 조용한 주석으로, 문서에 전혀 렌더링되지 않습니다.


/ -------------------------------------------
/ Html 요소
/ -------------------------------------------

/ 태그를 작성하려면 퍼센트 기호 뒤에 태그 이름을 사용하십시오.
%body
  %header
    %nav

/ 닫는 태그가 없습니다. 위 코드는 다음과 같이 출력됩니다.
  <body>
    <header>
      <nav></nav>
    </header>
  </body>

/
  div 태그는 기본 요소이므로 생략할 수 있습니다.
  . 또는 #을 사용하여 클래스/id만 정의할 수 있습니다.
  예를 들어

%div.my_class
  %div#my_id

/ 다음과 같이 작성할 수 있습니다.
.my_class
  #my_id

/ 태그에 콘텐츠를 추가하려면 선언 바로 뒤에 텍스트를 추가하십시오.
%h1 헤드라인 복사

/ 여러 줄 콘텐츠를 작성하려면 대신 중첩하십시오.
%p
  이것은 아마도 두 줄로 나눌 수 있는 많은 콘텐츠입니다.

/
  앰퍼샌드와 등호( &= )를 사용하여 html을 이스케이프할 수 있습니다. 이렇게 하면 html에 민감한 문자(&, /, :)가 html 인코딩된 해당 문자로 변환됩니다. 예를 들어

%p
  &= "Yes & yes"

/ 'Yes &amp; yes'를 출력합니다.

/ 느낌표와 등호( != )를 사용하여 html을 이스케이프 해제할 수 있습니다.
%p
  != "This is how you write a paragraph tag <p></p>"

/ 'This is how you write a paragraph tag <p></p>'를 출력합니다.

/ CSS 클래스는 .classnames를 태그에 연결하여 추가할 수 있습니다.
%div.foo.bar

/ 또는 Ruby 해시의 일부로
%div{:class => 'foo bar'}

/ 모든 태그에 대한 속성은 해시에 추가할 수 있습니다.
%a{:href => '#', :class => 'bar', :title => 'Bar'}

/ 부울 속성의 경우 값 'true'를 할당합니다.
%input{:selected => true}

/ 데이터 속성을 작성하려면 :data 키를 다른 해시인 값과 함께 사용하십시오.
%div{:data => {:attribute => 'foo'}}

/ Ruby 버전 1.9 이상에서는 Ruby의 새 해시 구문을 사용할 수 있습니다.
%div{ data: { attribute: 'foo' } }

/ 또한 HTML 스타일 속성 구문을 사용할 수 있습니다.
%a(href='#' title='bar')

/ 그리고 두 구문을 함께
%a(href='#'){ title: @my_class.title }


/ -------------------------------------------
/ Ruby 삽입
/ -------------------------------------------

/
  Ruby 값을 태그의 내용으로 출력하려면 등호 뒤에 Ruby 코드를 사용하십시오.

%h1= book.name

%p
  = book.author
  = book.publisher


/ html에 렌더링하지 않고 일부 Ruby 코드를 실행하려면 대신 하이픈을 사용하십시오.
- books = ['book 1', 'book 2', 'book 3']

/ Ruby 블록과 같이 모든 종류의 멋진 작업을 할 수 있습니다.
- books.shuffle.each_with_index do |book, index|
  %h1= book

  - if book do
    %p This is a book

/ 순서 있는/순서 없는 목록 추가
%ul
  %li
    =item1
    =item2

/
  다시 말하지만, Ruby에 대해서도 블록에 닫는 태그를 추가할 필요가 없습니다.
  들여쓰기가 처리합니다.

/ -------------------------------------------
/ 부트스트랩 클래스가 있는 테이블 삽입
/ -------------------------------------------

%table.table.table-hover
  %thead
    %tr
      %th 헤더 1
      %th 헤더 2

    %tr
      %td 값1
      %td 값2

  %tfoot
    %tr
      %td
        바닥글 값


/ -------------------------------------------
/ 인라인 Ruby / Ruby 보간
/ -------------------------------------------

/ 일반 텍스트 줄에 Ruby 변수를 포함하려면 #{ }를 사용하십시오.
%p Your highest scoring game is #{best_game}


/ -------------------------------------------
/ 필터
/ -------------------------------------------

/
  필터는 블록을 다른 필터링 프로그램에 전달하고 Haml에서 결과를 반환합니다.
  필터를 사용하려면 콜론과 필터 이름을 입력하십시오.

/ Markdown 필터
:markdown
  # 헤더

  **블록** *안의* 텍스트

/ 위 코드는 다음과 같이 컴파일됩니다.
<h1>헤더</h1>

<p><strong>블록</strong> <em>안의</em> 텍스트</p>

/ JavaScript 필터
:javascript
  console.log('This is inline <script>');

/ 다음과 같이 컴파일됩니다.
<script>
  console.log('This is inline <script>');
</script>

/
  다양한 유형의 필터가 있습니다(:markdown, :javascript, :coffee, :css, :ruby 등).
  또한 Haml::Filters를 사용하여 자신만의 필터를 정의할 수 있습니다.
```

## 추가 자료

- [HAML이란 무엇입니까?](http://haml.info/) - HAML 사용의 이점을 훨씬 더 잘 설명하는 좋은 소개입니다.
- [공식 문서](http://haml.info/docs/yardoc/file.REFERENCE.html) - 좀 더 깊이 들어가고 싶다면.