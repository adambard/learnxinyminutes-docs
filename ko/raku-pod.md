---
name: Pod
contributors:
    - ["Luis F. Uceta", "https://uzluisf.gitlab.io/"]
filename: learnpod.pod6
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Pod는 표현하기 쉽고 순수하게 기술적인 마크업 언어로, 표현 구성 요소가 없습니다. Raku 프로그램 및 모듈을 문서화하는 데 사용될 뿐만 아니라 언어 문서, 블로그 및 기타 유형의 문서 작성을 위해서도 활용될 수 있습니다.

Pod 문서는 해당 `Pod::To` 모듈(예: HTML 변환을 위한 `Pod::To::HTML`)의 해당 변형을 사용하여 HTML 및 기타 여러 형식(예: Markdown, Latex, 일반 텍스트 등)으로 쉽게 변환할 수 있습니다.

- [일반 정보](#general-info)
- [Pod 기본 사항](#pod-basics)
	- [기본 텍스트 서식](#basic-text-formatting)
	- [제목](#headings)
	- [일반 단락](#ordinary-paragraphs)
	- [목록](#lists)
	- [코드 블록](#code-blocks)
	- [주석](#comments)
	- [링크](#links)
	- [표](#tables)
- [블록 구조](#block-structures)
	- [약어 블록](#abbreviated-blocks)
	- [구분된 블록](#delimited-blocks)
	- [단락 블록](#paragraph-blocks)
- [구성 데이터](#configuration-data)
	- [표준 구성 옵션](#standard-configuration-options)
	- [블록 사전 구성](#block-pre-configuration)
- [의미 블록](#semantic-blocks)
- [기타](#miscellaneous)
	- [참고](#notes)
	- [키보드 입력](#keyboard-input)
	- [터미널 출력](#terminal-output)
	- [유니코드](#unicode)
- [Pod 렌더링](#rendering-pod)
- [Pod 액세스](#accessing-pod)

## 일반 정보

모든 Pod 문서는 `=begin pod`로 시작하고 `=end pod`로 끝나야 합니다.
이 두 구분 기호 사이에 있는 모든 것은 처리되어
문서 생성에 사용됩니다.

```
=begin pod

매우 간단한 Raku Pod 문서. 다른 모든 지시문은 여기에 있습니다!

=end pod
```

Pod 문서는 일반적으로 Raku 코드와 공존합니다. 단독으로 사용되는 경우
Pod 파일은 종종 `.pod6` 접미사를 가집니다. 앞으로는
논의되는 구성이 `=begin pod ... =end pod`
지시문으로 둘러싸여 있다고 가정합니다.

## Pod 기본 사항

### 기본 텍스트 서식

텍스트는 `B<>`, `I<>`, `U<>` 및 `C<>`와 같은 서식 코드를 사용하여 굵게, 기울임꼴, 밑줄 또는 그대로(코드 서식용) 쉽게 스타일을 지정할 수 있습니다.

```
B<이 텍스트는 굵게 표시됩니다.>

I<이 텍스트는 기울임꼴로 표시됩니다.>

U<이 텍스트는 밑줄이 그어져 있습니다.>

함수 C<sub sum { $^x + $^y}>는 그대로 처리됩니다.
```

더 많은 서식 코드(예: `L<>`, `T<>` 등)가 있지만, 문서 전체에서 나중에 논의될 것입니다. 단일 대문자 뒤에 즉시 단일 또는 이중 꺾쇠 괄호가 오는 것을 보면 알 수 있습니다. 꺾쇠 괄호의 유니코드 변형(«»)도 사용할 수 있습니다.

### 제목

제목은 `N`이 제목 수준인 `=headN` 지시문을 사용하여 생성됩니다.

```
=head1 레벨 1입니다.
=head2 레벨 2입니다.
=head3 레벨 3입니다.
=head4 레벨 4입니다.
=head5 레벨 5입니다.
=head6 레벨 6입니다.
```

### 일반 단락

일반 단락은 하나 이상의 인접한 텍스트 줄로 구성되며, 각 줄은 공백이 아닌 문자로 시작합니다. 모든 단락은 첫 번째 빈 줄 또는 블록 지시문으로 종료됩니다.

```
=head1 첫 번째 수준 제목 블록

=head2 단락 1

이것은 일반 단락입니다. 텍스트는 압축되고
짧은 줄은 채워집니다. 첫 번째 빈 줄에서 종료됩니다.

=head2 단락 2

이것은 더 짧지만 또 다른 일반 단락입니다.
```

또는 `=para` 지시문을 사용하여 인접한 텍스트 줄을 단락으로 명시적으로 표시할 수 있습니다.

```
=head1 첫 번째 수준 제목 블록

=head2 단락 1

=para
이것은 일반 단락입니다. 텍스트는 압축되고
짧은 줄은 채워집니다. 첫 번째 빈 줄에서 종료됩니다.

=head2 단락 2

=para
이것은 더 짧지만 또 다른 일반 단락입니다.
```

### 목록

순서 없는 목록은 `=item` 지시문을 사용하여 만들 수 있습니다.

```
=item 항목
=item 항목
=item 다른 항목
```

하위 목록은 `=item1`, `=item2`, `=item3`, `...`, `=itemN` 등과 같은 지시문을 사용하여 각 수준의 항목으로 달성됩니다. `=item` 지시문은 기본적으로 `=item1`입니다.

```
=item1 항목 1
=item1 항목 2
=item1 항목 3
    =item2 하위 항목
    =item2 하위 항목
=item1 항목 4
```

용어 또는 명령을 정의하는 정의 목록은 `=defn` 지시문을 사용합니다.
이는 HTML의 `<dl>` 요소와 동일합니다.

```
=defn 보드민의 야수
보드민 무어에 서식하는 큰 고양이과 동물.

=defn 모르가우르
바다뱀.

=defn 올맨
거대한 올빼미와 같은 생물.
```

### 코드 블록

코드 블록(HTML `<code>` 요소 사용)은 각 줄을 하나 이상의 공백 문자로 시작하여 생성됩니다.

```
    #`( 이것은 주석입니다 )
    my $sum = -> $x, $y { $x + $y }
    say $sum(12, 5);
```

[기본 텍스트 서식](#basic-text-formatting) 섹션에서 보여주듯이,
인라인 코드는 `C<>` 코드를 사용하여 만들 수 있습니다.

```
Raku에는 텍스트를 출력하는 여러 함수/메서드가 있습니다. 그 중 일부는 C<print>, C<put> 및 C<say>입니다.
```

### 주석

Pod 블록은 Rakudo Raku 컴파일러에 의해 무시되지만, Pod 블록으로 식별된 모든 것은 Pod 렌더러에 의해 읽히고 해석됩니다. Pod 블록이 렌더러에 의해 렌더링되는 것을 방지하려면 `=comment` 지시문을 사용하십시오.

```
=comment 알고리즘에 대해 여기에 더 추가하십시오.

=comment Pod 주석은 문서의 문서를 작성하는 데 좋습니다.
```

인라인 주석을 만들려면 `Z<>` 코드를 사용하십시오.

```
Pod는 훌륭합니다 Z<물론이죠!>. 그리고 Raku도요!
```

Raku 인터프리터는 내장된 Pod 블록을 실행하지 않으므로,
주석 블록은 중첩 가능한 블록 주석의 대체 형태로도 사용할 수 있습니다.

### 링크

Pod에서 링크를 만드는 것은 매우 쉽고 `L<>` 코드에 링크를 묶어서 수행됩니다. 일반적인 형식은 `L<레이블|URL>`이며 `레이블`은 선택 사항입니다.

```
Raku 홈페이지는 L<https://raku.org>입니다.
L<클릭하세요!|http://link.org/>.
```

상대 경로도 작동합니다.

```
L<음악으로 이동|/music/>.
```

동일한 문서의 섹션에 연결하는 것도 작동합니다.

```
L<제목으로 연결|#Headings>
```

### 표

Pod 사양은 아직 완전히 제대로 처리되지 않았으며
여기에는 테이블 처리도 포함됩니다. 간단함을 위해
테이블을 구성하는 한 가지 방법만 여기에 표시됩니다. 모범 사례를 배우고
좋은 테이블과 나쁜 테이블의 예시를 보려면
<https://docs.raku.org/language/tables>를 방문하십시오.

```
=begin table
옵션      | 설명
============|================
data        | 데이터 파일 경로.
engine      | 템플릿 처리에 사용될 엔진.
ext         | 대상 파일에 사용될 확장자.
=end table
```

## 블록 구조

앞서 언급했듯이, Pod 문서는 지시문을 사용하여 지정되며, 이는 텍스트 콘텐츠 블록을 구분하고 선택적 [구성 정보](#configuration-data)를 선언하는 데 사용됩니다. 모든 지시문은 첫 번째 열에 등호(`=`)로 시작합니다. 문서의 내용은 하나 이상의 블록 내에 지정됩니다. 모든 Pod 블록은 세 가지 동등한 형식 중 하나로 선언될 수 있습니다: 구분된 스타일, 단락 스타일 또는 약어 스타일.

지금까지는 블록 유형에 대해 약어 스타일만 사용했습니다(예: `=head1`, `=para`, `=comment`, `=item` 등).

### 약어 블록

약어 블록은 첫 번째 열에 `=` 기호로 시작하며, 그 뒤에 블록의 `typename`이 오고 그 뒤에 내용이 옵니다. 줄의 나머지 부분은 블록 데이터로 처리되며 구성으로 처리되지 않습니다. 내용은 다음 Pod 지시문 또는 첫 번째 빈 줄(블록 데이터의 일부가 아님)에서 종료됩니다. 일반적인 구문은 다음과 같습니다.

```
=BLOCK_TYPE  BLOCK_DATA
```

예를 들어:

```
=head1 최상위 제목
```

### 구분된 블록

구분된 블록은 `=begin` 및 `=end` 마커로 경계가 지정되며, 둘 다 유효한 Pod 식별자(블록의 `typename`)가 뒤따릅니다. 일반적인 구문은 다음과 같습니다.

```
=begin BLOCK_TYPE
BLOCK_DATA
=end BLOCK_TYPE
```

예를 들어:

```
=begin head1
최상위 제목
=end head1
```

이러한 유형의 블록은 여러 단락이 있는 제목, 목록 항목, 코드 블록 등을 만드는 데 유용합니다. 예를 들어,

* 목록의 여러 줄 항목

```
=begin item
이것은 목록 항목의 단락입니다.

이것은 동일한 목록 항목의 또 다른 단락입니다.
=end item
```

* 코드 블록

```
=begin code
#`( 
비효율적인 거듭제곱 함수 재귀 구현 (다중 서브 사용).
)

multi pow( Real $base, 0 ) { 1 }

multi pow( Real $base, Int $exp where * ≥ 0) {
	$base * pow($base, $exp - 1)
}

multi pow( Real $base ) {
     pow($base, 2)
}

say pow(3, 0);   #=> 1
say pow(4.2, 2); #=> 17.64
say pow(6);      #=> 36
=end code
```

### 단락 블록

단락 블록은 `=for` 마커로 시작하며 다음 Pod 지시문 또는 첫 번째 빈 줄(블록 내용의 일부로 간주되지 않음)에서 종료됩니다. `=for` 마커 뒤에는 블록의 `typename`이 옵니다. 일반적인 구문은 다음과 같습니다.

```
=for BLOCK_TYPE
BLOCK DATA
```

예를 들어:

```
=for head1
최상위 제목
```

## 구성 데이터

약어 블록을 제외하고, 구분된 블록과 단락 블록 모두 블록의 `typename` 바로 뒤에 내용에 대한 구성 정보를 제공할 수 있습니다. 따라서 다음은 이러한 블록에 대한 더 일반적인 구문입니다.

* 구분된 블록

```
=begin BLOCK_TYPE OPTIONAL_CONFIG_INFO
=                 ADDITIONAL_CONFIG_INFO
BLOCK_DATA
=end BLOCK_TYPE
```

* 단락 블록

```
=for BLOCK_TYPE OPTIONAL_CONFIG_INFO
=               ADDITIONAL_CONFIG_INFO
BLOCK DATA
```

구성 정보는 Raku의 ["콜론 쌍"](https://docs.raku.org/language/glossary#index-entry-Colon_Pair) 구문과 유사한 형식으로 제공됩니다. 다음 표는 구성 정보를 제공할 수 있는 다양한 방법의 간략화된 버전입니다. 주제에 대한 더 자세한 내용은 <https://docs.raku.org/language/pod#Configuration_information>을 참조하십시오.

| 값     | 지정 방법...             | 예시                        |
| :-------- | :------                     | :------                        |
| 목록      | :key($elem1, $elem2, ...)   | :tags('Pod', 'Raku')          |
| 해시      | :key{$key1 => $value1, ...} | :feeds{url => 'raku.org'}     |
| 부울   | :key/:key(True)             | :skip-test(True)               |
| 부울   | :!key/:key(False)           | :!skip-test                    |
| 문자열    | :key('string')              | :nonexec-reason('SyntaxError') |
| 정수       | :key(2)                     | :post-number(6)                |


### 표준 구성 옵션

Pod는 특정 블록 유형에 균일하게 적용할 수 있는 소수의 표준 구성 옵션을 제공합니다. 그 중 일부는 다음과 같습니다.

* `:numbered`

이 옵션은 블록에 번호를 매길 것을 지정합니다. 이 옵션의 가장 일반적인 용도는 번호가 매겨진 제목과 순서가 있는 목록을 만드는 것이지만, 어떤 블록에도 적용할 수 있습니다.

예를 들어:

```
=for head1 :numbered
문제
=for head1 :numbered
해결책
=for head2 :numbered
분석
=for head3 :numbered
개요
```

* `:allow`

`:allow` 옵션의 값은 하나 이상의 서식 코드(단일 문자 이름) 목록이어야 합니다. 그러면 해당 코드는 코드 블록 내에서 활성 상태로 유지됩니다. 이 옵션은 일반적으로 `=code` 블록에서 사용되며, 해당 블록 내에서 마크업을 허용하지만, 다른 모든 블록에서도 사용할 수 있습니다.

다음 스니펫이 주어졌을 때:

```
=begin code :allow('B', 'I')
B<sub> greet( $name ) {
    B<say> "Hello, $nameI<!>";
}
=end code
```

다음과 같은 출력을 얻습니다:

<pre><strong>sub</strong> greet( $name ) {
    <strong>say</strong> &quot;Hello, $name<em>!</em>&quot;;
}
</pre>

이는 출력 형식에 따라 크게 달라집니다. 예를 들어, Pod가 HTML로 변환될 때는 작동하지만, Markdown으로 변환될 때는 보존되지 않을 수 있습니다.

### 블록 사전 구성

`=config` 지시문은 특정 유형의 모든 블록에 적용되는 표준 구성 정보를 미리 지정할 수 있도록 합니다.
구성 지시문의 일반적인 구문은 다음과 같습니다.

```
=config BLOCK_TYPE  CONFIG OPTIONS
=                  ADDITIONAL_CONFIG_INFO
```

예를 들어, 모든 헤더 레벨 1에 번호가 매겨지고 굵게 및 밑줄이 그어지도록 지정하려면 `=head1`을 다음과 같이 미리 구성합니다.

```
=config head1 :formatted('B', 'U') :numbered
```

## 의미 블록

모든 대문자 블록 유형 이름은 표준 문서, 게시, 소스 구성 요소 또는 메타 정보를 지정하는 데 예약되어 있습니다.
그 중 일부는 다음과 같습니다.

```
=NAME
=AUTHOR
=VERSION
=CREATED
=SYNOPSIS
=DESCRIPTION
=USAGE
```

이러한 블록 대부분은 일반적으로 전체 구분된 형식으로 사용됩니다. 예를 들어,

```
=NAME B<Doc::Magic>

=begin DESCRIPTION
이 모듈은 문서를 자동으로 생성하는 데 도움이 됩니다.
소스 코드가 필요 없습니다! 대부분은 블랙홀에서 가져왔습니다.
=end DESCRIPTION

=begin SYNOPSIS
=begin code
	use Doc::Magic;

	my Doc::Magic $doc .= new();

    my $result = $doc.create-documentation($fh);
=end code
=end SYNOPSIS

=AUTHOR Authorius Docus
=VERSION 42
```

## 기타

### 참고

참고는 각주로 렌더링되며 `N<>` 코드에 참고를 묶어서 생성됩니다.

```
또한, 이 언어는 다중 패러다임입니다 N<위키백과에 따르면,
이것은 절차적, 객체 지향적, 함수형 프로그래밍을 지원한다는 의미입니다.>
```

### 키보드 입력

텍스트를 키보드 입력으로 표시하려면 `K<>` 코드로 묶으십시오.

```
이름을 입력하십시오 K<John Doe>
```

### 터미널 출력

터미널 출력으로 텍스트를 표시하려면 `T<>` 코드로 묶으십시오.

```
안녕하세요, T<John Doe>
```

### 유니코드

Pod 문서에 유니코드 코드 포인트 또는 HTML5 문자 참조를 포함하려면 `E<>` 코드로 묶으십시오.

예를 들어:

```
Raku는 E<171> 및 E<187> 문자를 상당히 사용합니다.
Raku는 E<laquo> 및 E<raquo> 문자를 상당히 사용합니다.
```

다음과 같이 렌더링됩니다:

Raku는 « 및 » 문자를 상당히 사용합니다.
Raku는 « 및 » 문자를 상당히 사용합니다.

## Pod 렌더링

출력(예: Markdown, HTML, Text 등)을 생성하려면 Rakudo Raku 컴파일러가 설치되어 있어야 합니다. 또한 Pod에서 원하는 출력을 생성하는 모듈(예: `Pod::To::Markdown`, `Pod::To::HTML`, `Pod::To::Text` 등)을 설치해야 합니다.

raku 프로그램을 실행하기 위해 Rakudo를 설치하는 방법에 대한 지침은 [여기](https://raku.org/downloads/)를 참조하십시오.

특정 출력을 생성하려면 다음 명령을 실행하십시오.

```
raku --doc=TARGET input.pod6 > output.html
```

여기서 `TARGET`은 `Markdown`, `HTML`, `Text` 등입니다. 따라서 Pod에서 Markdown을 생성하려면 다음을 실행하십시오.

```
raku --doc=Markdown input.pod6 > output.html
```

## Pod 액세스

Raku 프로그램 내에서 Pod 문서에 액세스하려면 특수 `=` 트위길(예: `$=pod`, `$=SYNOPSIS` 등)을 사용해야 합니다.

`$=` 구문은 Pod 구조에 대한 인트로스펙션을 제공하여 `Pod::Block` 트리 루트를 생성하며, 이를 통해 Pod 문서의 전체 구조에 액세스할 수 있습니다.

다음 Raku 코드와 [의미 블록](#semantic-blocks) 섹션의 Pod 문서를 동일한 파일에 배치하면:

```
my %used-directives;
for $=pod -> $pod-item {
    for $pod-item.contents -> $pod-block {
        next unless $pod-block ~~ Pod::Block::Named;
        %used-directives{$pod-block.name} = True;
    }
}

say %used-directives.keys.join("\n");
```

다음과 같은 출력을 얻습니다:

```
SYNOPSIS
NAME
VERSION
AUTHOR
DESCRIPTION
```

## 추가 정보

* <https://docs.raku.org/language/pod> Pod 문서.
* <https://docs.raku.org/language/tables> Pod 테이블에 대한 조언.
* <https://design.raku.org/S26.html> Pod 사양.

```