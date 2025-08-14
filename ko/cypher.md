---
name: Cypher
filename: LearnCypher.cql
contributors:
    - ["Théo Gauchoux", "https://github.com/TheoGauchoux"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Cypher는 그래프를 쉽게 조작하기 위한 Neo4j의 쿼리 언어입니다.
SQL의 구문을 재사용하고 그래프를 나타내기 위해 ASCII 아트와 같은 것을 혼합합니다.
이 튜토리얼은 노드 및 관계와 같은 그래프 개념을 이미 알고 있다고 가정합니다.

## 노드는 그래프의 레코드를 나타냅니다.

`()`는 *노드*를 나타내는 빈 *노드*이지만, 쿼리와는 관련이 없습니다.

`(n)`은 변수 `n`으로 참조되는 *노드*이며, 쿼리에서 재사용할 수 있습니다. 소문자로 시작하고 camelCase를 사용합니다.

`(p:Person)` - 노드에 *레이블*을 추가할 수 있습니다. 여기서는 `Person`입니다. 유형/클래스/범주와 같습니다. 대문자로 시작하고 camelCase를 사용합니다.

`(p:Person:Manager)` - 노드는 여러 *레이블*을 가질 수 있습니다.

`(p:Person {name : 'Théo Gauchoux', age : 22})` - 노드는 일부 *속성*을 가질 수 있습니다. 여기서는 `name`과 `age`입니다. 소문자로 시작하고 camelCase를 사용합니다.

속성에서 허용되는 유형:

- 숫자
- 부울
- 문자열
- 이전 기본 유형 목록

*경고: Cypher에는 날짜/시간 속성이 없습니다! 특정 패턴의 문자열 또는 특정 날짜의 숫자를 사용할 수 있습니다.*

`p.name` - 점 스타일로 속성에 액세스할 수 있습니다.

## 관계(또는 에지)는 두 노드를 연결합니다.

`[:KNOWS]`는 *레이블* `KNOWS`가 있는 *관계*입니다. 노드의 레이블과 같은 *레이블*입니다. UPPER_SNAKE_CASE를 사용합니다.

`[k:KNOWS]` - 변수 `k`로 참조되는 동일한 *관계*이지만, 쿼리에서 재사용할 필요는 없습니다.

`[k:KNOWS {since:2017}]` - *속성*(`노드`와 유사)이 있는 동일한 *관계*입니다. 여기서는 `since`입니다.

`[k:KNOWS*..4]`는 *경로*(나중에 설명)에서 사용할 구조 정보입니다. 여기서 `*..4`는 "`k` 관계가 1에서 4번 반복될 수 있는 패턴과 일치"를 의미합니다.

## 경로 - 노드와 관계를 혼합하는 방법.

`(a:Person)-[:KNOWS]-(b:Person)` - `a`와 `b`가 서로 아는 관계를 설명하는 경로입니다.

`(a:Person)-[:MANAGES]->(b:Person)` - 경로는 방향을 가질 수 있습니다. 이 경로는 `a`가 `b`의 관리자임을 설명합니다.

`(a:Person)-[:KNOWS]-(b:Person)-[:KNOWS]-(c:Person)` - 여러 관계를 연결할 수 있습니다. 이 경로는 친구의 친구를 설명합니다.

`(a:Person)-[:MANAGES]->(b:Person)-[:MANAGES]->(c:Person)` - 체인도 방향을 가질 수 있습니다. 이 경로는 `a`가 `b`의 상사이고 `c`의 대리인임을 설명합니다.

일반적으로 사용되는 패턴(Neo4j 문서에서):

```cypher
// 친구의 친구
(user)-[:KNOWS]-(friend)-[:KNOWS]-(foaf)

// 최단 경로
path = shortestPath( (user)-[:KNOWS*..5]-(other) )

// 협업 필터링
(user)-[:PURCHASED]->(product)<-[:PURCHASED]-()-[:PURCHASED]->(otherProduct)

// 트리 탐색
(root)<-[:PARENT*]-(leaf:Category)-[:ITEM]->(data:Product)
```

## 쿼리 생성

새 노드 생성

```cypher
CREATE (a:Person {name:"Théo Gauchoux"})
RETURN a
```

*`RETURN`은 쿼리 후 결과를 얻을 수 있도록 합니다. `RETURN a, b`와 같이 여러 개일 수 있습니다.*

새 관계 생성 (2개의 새 노드 포함)

```cypher
CREATE (a:Person)-[k:KNOWS]-(b:Person)
RETURN a,k,b
```

## 쿼리 일치

모든 노드 일치

```cypher
MATCH (n)
RETURN n
```

레이블별 노드 일치

```cypher
MATCH (a:Person)
RETURN a
```

레이블 및 속성별 노드 일치

```cypher
MATCH (a:Person {name:"Théo Gauchoux"})
RETURN a
```

관계에 따라 노드 일치 (방향 없음)

```cypher
MATCH (a)-[:KNOWS]-(b)
RETURN a,b
```

관계에 따라 노드 일치 (방향 있음)

```cypher
MATCH (a)-[:MANAGES]->(b)
RETURN a,b
```

`WHERE` 절이 있는 노드 일치

```cypher
MATCH (p:Person {name:"Théo Gauchoux"})-[s:LIVES_IN]->(city:City)
WHERE s.since = 2015
RETURN p,state
```

`MATCH WHERE` 절을 `CREATE` 절과 함께 사용할 수 있습니다.

```cypher
MATCH (a), (b)
WHERE a.name = "Jacquie" AND b.name = "Michel"
CREATE (a)-[:KNOWS]-(b)
```

## 쿼리 업데이트

노드의 특정 속성 업데이트

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p.age = 23
```

노드의 모든 속성 교체

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p = {name: "Michel", age: 23}
```

노드에 새 속성 추가

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p += {studies: "IT Engineering"}
```

노드에 레이블 추가

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p:Internship
```

## 쿼리 삭제

특정 노드 삭제 (연결된 관계는 먼저 삭제해야 함)

```cypher
MATCH (p:Person)-[relationship]-()
WHERE p.name = "Théo Gauchoux"
DELETE relationship, p
```

특정 노드에서 속성 제거

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
REMOVE p.age
```

*`REMOVE` 키워드에 주의하십시오. `DELETE`가 아닙니다!*

특정 노드에서 레이블 제거

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
DELETE p:Person
```

전체 데이터베이스 삭제

```cypher
MATCH (n)
OPTIONAL MATCH (n)-[r]-()
DELETE n, r
```

*정말, Cypher의 `rm -rf /`입니다!*

## 기타 유용한 절

`PROFILE` - 쿼리 전에 실행 계획을 표시합니다.

`COUNT(e)` - `e`와 일치하는 엔티티(노드 또는 관계) 수를 계산합니다.

`LIMIT x` - 결과를 처음 `x`개로 제한합니다.

## 특별 힌트

- Cypher는 이중 슬래시를 사용하는 한 줄 주석만 있습니다: `// comment`
- .cql 파일에 저장된 Cypher 스크립트를 Neo4j에서 직접 실행할 수 있습니다(가져오기입니다). 그러나 이 파일에는 여러 문(세미콜론으로 구분)을 가질 수 없습니다.
- Cypher를 작성하려면 Neo4j 셸을 사용하십시오. 정말 훌륭합니다.
- Cypher는 모든 그래프 데이터베이스의 표준 쿼리 언어가 될 것입니다([openCypher](https://opencypher.org/)로 알려짐).

[여기](https://neo4j.com/developer/cypher-query-language/)에서 더 읽어보십시오.