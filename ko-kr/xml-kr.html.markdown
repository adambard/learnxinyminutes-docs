---
language: xml
filename: learnxml-kr.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
  - ["Rachel Stiyer", "https://github.com/rstiyer"]
  - ["Deepanshu Utkarsh", "https://github.com/duci9y"]
translators:
  - ["Wooseop Kim", "https://github.com/linterpreteur"]
lang: ko-kr
---

XML은 데이터를 저장하고 전송하기 위해 설계된 마크업 언어입니다. 인간과 기계 모두가 읽을 수 있도록 만들어졌습니다.

XML은 HTML과는 달리 데이터를 보여주는 방법이나 그 형식을 특정하지 않습니다. 단지 데이터를 담을 뿐입니다.

차이는 **내용**과 **마크업**에 있습니다. 내용은 무엇이든 될 수 있지만, 마크업은 정의되어 있습니다.

## 기초 정의 및 도입

XML 문서는 기본적으로 자신을 설명하는 *속성*을 가질 수 있으며 자식으로서 텍스트 혹은 다른 요소를 가질 수 있는 *요소*들로 이루어집니다. 모든 XML 문서는 반드시 루트 요소를 가져야 합니다. 루트 요소는 문서에 있는 모든 다른 요소들의 조상입니다.

XML 파서는 매우 엄격하게 설계되어 있으므로 문서의 형식이 틀렸다면 파싱을 멈출 것입니다. 그러므로 모든 XML 문서는 [(영어) XML 문법 규칙](http://www.w3schools.com/xml/xml_syntax.asp)을 따른다고 보장할 수 있습니다.

```xml
<!-- 주석에는 두 개의 연속된 하이픈(-)이 들어갈 수 없습니다. -->
<!-- 주석은 여러 줄로
  이어질 수 있습니다. -->

<!-- 요소 -->
<!-- 요소는 XML의 기본적 구성품입니다. 요소에는 두 개의 유형이 있습니다. -->
<element1 attribute="value" /> <!-- 빈 요소는 내용을 담지 않습니다. -->
<!-- 그리고 비지 않은 요소가 있습니다. -->
<element2 attribute="value">내용</element2>
<!-- 요소 이름에는 알파벳과 숫자만이 허용됩니다. -->

<empty /> <!-- 요소는 어떠한 내용도 없이 순수한 마크업인 -->
<!-- 빈 요소 태그로 구성될 수 있습니다. -->

<notempty> <!-- 혹은 여는 태그와 -->
  <!-- 내용, -->
</notempty> <!-- 그리고 닫는 태그로 구성될 수도 잇습니다. -->

<!-- 요소 이름은 대소문자를 구별합니다. -->
<element />
<eLEMENT />
<!-- 둘은 같지 않습니다. -->

<!-- 속성 -->
<!-- 속성은 요소 안에 존재하는 키와 값의 쌍입니다. -->
<element attribute="value" another="anotherValue" many="space-separated list" />
<!-- 속성은 원소에서 단 한 번만 나타날 수 있습니다. 속성은 단 하나의 값만 갖습니다.
  이에 대한 흔한 해결책은 공백으로 구분된 리스트를 포함하는 것입니다. -->

<!-- 중첩 요소 -->
<!-- 한 요소의 내용은 다른 요소들을 포함할 수 있습니다. -->
<parent>
  <child>Text</child>
  <emptysibling />
</parent>
<!-- 표준적인 트리 명칭이 사용됩니다. 각각의 요소는 노드라고 부릅니다.
  한 단계 위의 조상은 부모이며, 한 단계 아래의 후손은 자식입니다.
  같은 부모 요소를 가진 요소들은 자매입니다. -->

<!-- XML은 공백을 보존합니다. -->
<child>
  Text
</child>
<child>Text</child>
<!-- 둘은 같지 않습니다. -->
```

## XML 문서

XML이 유용한 것은 인간도 읽을 수 있다는 것입니다. 다음의 문서는 에릭 레이의 XML 배우기를 포함해 세 권의 책을 파는 서점을 정의한다는 것을 알 수 있습니다. XML 파서 없이도 이렇게 쉽습니다.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- XML 프롤로그라는 것입니다. 필수는 아니지만, 권장됩니다. -->
<bookstore>
  <book category="COOKING">
    <title lang="ko">매일 이탈리아 요리</title>
    <author>지아다 데 라우렌티스</author>
    <year>2005</year>
    <price>30.00</price>
  </book>
  <book category="CHILDREN">
    <title lang="ko">해리 포터</title>
    <author>J K 롤링</author>
    <year>2005</year>
    <price>29.99</price>
  </book>
  <book category="WEB">
    <title lang="ko">XML 배우기</title>
    <author>에릭 레이</author>
    <year>2003</year>
    <price>39.95</price>
  </book>
</bookstore>
```

## 적격성과 유효성

XML 문서는 문법적으로 정확할 경우 *적격*합니다. 하지만 문서 유형 정의(DTD)를 이용하여 문서에 제약을 더 추가할 수 있습니다. 한 문서의 요소와 속성이 DTD 안에 정의되어 있고 그 파일에 특정된 문법을 따른다면 *적격*할 뿐만 아니라 그 DTD에 대하여 *유효*하다고 말합니다.

```xml
<!-- DTD를 외부에 선언: -->
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE bookstore SYSTEM "Bookstore.dtd">
<!-- bookstore가 루트 요소이며 'Bookstore.dtd'가 DTD 파일의
  경로임을 선언합니다. -->
<bookstore>
  <book category="COOKING">
    <title lang="en">Everyday Italian</title>
    <author>Giada De Laurentiis</author>
    <year>2005</year>
    <price>30.00</price>
  </book>
</bookstore>

<!-- DTD 파일 -->
<!ELEMENT bookstore (book+)>
<!-- bookstore 요소는 하나 이상의 book 요소를 자식으로 가질 수 있습니다. -->
<!ELEMENT book (title, price)>
<!-- 각각의 book은 title과 price를 자식으로 반드시 갖습니다. -->
<!ATTLIST book category CDATA "Literature">
<!-- book은 category 속성을 가져야 합니다. 그렇지 않다면 그 기본값은 'Literature'입니다. -->
<!ELEMENT title (#PCDATA)>
<!-- title 요소는 반드시 PCDATA만 포함해야 합니다. 즉,
  파서가 읽을 텍스트만을 포함해야 하며 자식을 포함할 수 없습니다.
  CDATA와 비교해 보세요. -->
<!ELEMENT price (#PCDATA)>
]>

<!-- DTD는 XML 파일 안에 선언될 수도 있습니다. -->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE bookstore [
<!ELEMENT bookstore (book+)>
<!ELEMENT book (title, price)>
<!ATTLIST book category CDATA "Literature">
<!ELEMENT title (#PCDATA)>
<!ELEMENT price (#PCDATA)>
]>

<bookstore>
  <book category="COOKING">
    <title>Everyday Italian</title>
    <price>30.00</price>
  </book>
</bookstore>
```

## DTD 호환성과 XML 스키마 정의

DTD는 오래되었기 때문에 지원이 광범위합니다. 불행히도 네임스페이스와 같은 현대적 XML 기능은 DTD에서 지원하지 않습니다. XML 스키마 정의(XSD)가 XML 문서의 문법을 정의하기 위한 DTD의 대체재입니다.

## Resources

* [(영어) Validate your XML](http://www.xmlvalidation.com)

## Further Reading

* [(영어) XML 스키마 정의 튜토리얼](http://www.w3schools.com/xml/xml_schema.asp)
* [(영어) DTD 튜토리얼](http://www.w3schools.com/xml/xml_dtd_intro.asp)
* [(영어) XML 튜토리얼](http://www.w3schools.com/xml/default.asp)
* [(영어) XPath 쿼리로 XML 파싱하기](http://www.w3schools.com/xml/xml_xpath.asp)
