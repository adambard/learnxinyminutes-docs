---
name: RDF
filename: learnrdf.ttl
contributors:
- ["Bob DuCharme", "http://bobdc.com/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

RDF(Resource Description Framework)는 [W3C 표준](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/) 데이터 모델입니다. W3C는 여러 RDF 구문을 표준화했습니다. 아래 예제는 가장 인기 있는 [Turtle](https://www.w3.org/TR/turtle/)을 사용합니다.

Turtle 파일의 한 가지 장점은 두 개의 구문적으로 유효한 Turtle 파일을 연결하면 또 다른 구문적으로 유효한 Turtle 파일이 생성된다는 것입니다. 이는 데이터 통합을 용이하게 하는 RDF의 여러 장점 중 하나입니다.

RDF 데이터셋에 대한 W3C 표준 쿼리 언어는 [SPARQL](https://www.w3.org/TR/sparql11-query/)입니다.

RDF는 모든 사실을 트리플로 알려진 세 부분으로 구성된 {주어, 술어, 목적어} 문으로 표현합니다. 동일한 엔티티가 일부 트리플의 주어이고 다른 트리플의 목적어일 수 있으므로, 트리플 집합은 그래프 데이터 구조를 나타낼 수 있습니다. 트리플에 대한 대규모 저장 시스템을 트리플스토어라고 하며, NoSQL 데이터베이스의 그래프 데이터베이스 범주에 속합니다.

RDF 주어와 술어는 URI(Uniform Resource Identifiers)여야 합니다. URI는 일반적으로 URL처럼 보이지만, 로케이터가 아닌 식별자로 기능합니다. URI를 사용하면 리소스 식별자에 대한 컨텍스트를 제공하여 모호성을 없앨 수 있습니다. 예를 들어, 책 제목과 직책을 구별할 수 있습니다.

```
# 해시 기호는 주석 구분 기호입니다.

# Turtle 트리플 문은 자연어 문장처럼 마침표로 끝납니다.

# 다음 두 트리플은 신화적인 Example Company의 직원 134가 2022-11-12에 고용되었고 성이 Smith임을 알려줍니다:

<http://example.com/emp134> <http://example.com/hireDate> "2022-11-12" .
<http://example.com/emp134> <http://example.com/familyName> "Smith" .

# 네임스페이스를 대신하는 접두사를 선언하면 장황함이 줄어듭니다. 이러한 선언은 일반적으로 파일 시작 부분에 오지만, 접두사를 선언하기 전에 처음 사용해야 한다는 요구 사항만 있습니다.

@prefix ex: <http://example.com/> .
ex:emp134 ex:hireDate "2022-11-12" .
ex:emp134 ex:familyName "Smith" .

# 세미콜론은 다음 트리플이 이전 트리플과 동일한 주어를 사용함을 의미합니다.
# 이는 단일 리소스에 대한 데이터를 나열하는 데 편리합니다. 다음 예제는 이전 예제와 동일한 의미를 가집니다.

@prefix ex: <http://example.com/> .
ex:emp134 ex:hireDate "2022-11-12" ;
          ex:familyName "Smith" .

# 쉼표는 다음 트리플이 이전 트리플과 동일한 주어와 술어를 가짐을 의미합니다.

ex:emp134 ex:nickname "Smithy", "Skipper", "Big J".

# 값의 시작과 끝에 세 개의 단일 또는 이중 따옴표를 사용하면 여러 줄 문자열 값을 정의할 수 있습니다.

ex:emp134 ex:description """
Skipper는 11월에 회사에 입사했습니다.

그는 항상 모두에게 농담을 합니다.""" .

# 기존 표준 어휘 네임스페이스에서 URI를 사용하면 데이터 통합 및 이미 존재하는 많은 RDF와의 상호 운용성이 용이해집니다. 표준 및 로컬 사용자 정의 네임스페이스를 혼합하여 사용하는 것이 일반적입니다.

@prefix vcard: <http://www.w3.org/2006/vcard/ns#> .
ex:emp134 ex:hireDate "2022-11-12" ;
          vcard:family-name "Smith" .

# RDF 스키마 표준의 rdfs:label 술어는 엔티티의 사람이 읽을 수 있는 이름을 나타내는 일반적인 방법입니다.

@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
ex:hireDate rdfs:label "hire date" .

# 문자열 객체 값에는 언어 코드가 포함될 수 있어,
# 데이터를 읽는 애플리케이션(예: 사용자 인터페이스를 생성할 때)에서 엔티티의 다국어 표현을 더 쉽게 만듭니다.

ex:hireDate rdfs:label "hire date"@en, "date d'embauche"@fr  .

# 트리플의 객체를 URI(또는 접두사 이름)로 나타내는 것은 필수는 아니지만,
# 트리플을 그래프로 연결할 수 있습니다.

ex:emp134 vcard:family-name "Smith" .
ex:emp113 vcard:family-name "Jones" ;
          ex:reportsTo ex:emp134 .

# 객체는 XML 스키마 파트 2 표준의 데이터 유형이거나 사용자 정의 데이터 유형일 수 있습니다.

@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
ex:emp134 vcard:family-name "Smith"^^xsd:string ;  # 기본 데이터 유형
          ex:hireDate "2022-11-12"^^xsd:date ;
          ex:rating "3.5"^^ex:someCustomType .

# RDF에서 스키마 사용은 선택 사항입니다. 스키마는 데이터셋의 전체 또는 일부를 설명할 수 있습니다. 일반적으로 rdfs 접두사를 사용하여 W3C RDF 스키마(RDFS) 표준에서 설명하는 어휘를 사용합니다.

# 이러한 스키마는 새 데이터셋을 수용하기 쉽게 하기 위한 설명적인 것이며,
# 새 데이터를 생성하는 방법에 대한 규범적인 규칙이 아닙니다. 다음은 클래스를 선언합니다.
# (RDFS 자체도 트리플로 표현됩니다.)

@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
ex:Person rdf:type rdfs:Class .

# 다음 트리플은 이전 트리플과 동일한 의미를 가지지만,
# 간결성과 가독성을 위해 Turtle 단축키를 사용합니다.

ex:Person a rdfs:Class .

# 마지막 트리플은 ex:Person이 클래스의 인스턴스임을 선언하고,
# 다음은 직원 113이 클래스 Employee의 인스턴스임을 선언합니다.

ex:emp113 a ex:Employee .

# 다음 네 개의 트리플을 읽고 RDFS를 이해하는 RDF 파서는
# ex:emp113이 ex:Person의 인스턴스임을 추론합니다. 왜냐하면
# ex:Employee의 인스턴스이고, ex:Person의 하위 클래스이기 때문입니다.

ex:Employee a rdfs:Class .
ex:Employee rdfs:subClassOf ex:Person .

# RDFS를 사용하면 속성을 선언하고 클래스와 연결할 수 있습니다.
# 속성은 일급 리소스이며 객체 지향적 의미에서 클래스에 "속하지" 않습니다.
# rdfs:domain은 "다음 객체 클래스가 이 트리플의 주어가 지정하는 속성을 사용한다"는 의미입니다.
# rdfs:range는 "이 트리플의 주어가 지정하는 속성은 다음 클래스 또는 유형의 값을 가질 것이다"는 의미입니다.

ex:birthday rdf:type rdf:Property ;
            rdfs:domain ex:Person ;
            rdfs:range xsd:date .
```

## 더 읽을거리

* [RDF Primer — Turtle 버전](https://www.w3.org/2007/02/turtle/primer/) W3C에서
* [RDF란 무엇인가?](https://www.bobdc.com/blog/whatisrdf/) bobdc.com에서
* [RDFS란 무엇인가?](https://www.bobdc.com/blog/whatisrdfs/) bobdc.com에서
* [RDF 및 SPARQL 소개](https://data.europa.eu/sites/default/files/d2.1.2_training_module_1.3_introduction_to_rdf_sparql_en_edp.pdf) data.europa.eu에서