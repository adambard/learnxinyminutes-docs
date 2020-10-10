---
language: cypher
filename: LearnCypher-br.cql
contributors:
    - ["Théo Gauchoux", "https://github.com/TheoGauchoux"]

lang: pt-br
---

O Cypher é a linguagem de consulta do Neo4j para manipular gráficos facilmente. Ela reutiliza a sintaxe do SQL e a mistura com o tipo de ascii-art para representar gráficos. Este tutorial pressupõe que você já conheça conceitos de gráficos como nós e relacionamentos.

[Leia mais aqui.](https://neo4j.com/developer/cypher-query-language/)


Nós
---

**Representa um registro em um gráfico.**

`()`
É um *nó* vazio, para indicar que existe um *nó*, mas não é relevante para a consulta.

`(n)`
É um *nó* referido pela variável **n**, reutilizável na consulta. Começa com minúsculas e usa o camelCase.

`(p:Person)`
Você pode adicionar um *label* ao seu nó, aqui **Person**. É como um tipo / uma classe / uma categoria. Começa com maiúsculas e usa o camelCase.

`(p:Person:Manager)`
Um nó pode ter muitos *labels*.

`(p:Person {name : 'Théo Gauchoux', age : 22})`
Um nó pode ter algumas *propriedades*, aqui **name** e **age**. Começa com minúsculas e usa o camelCase.

Os tipos permitidos nas propriedades:

 - Numeric
 - Boolean
 - String
 - Lista de tipos primitivos anteriores

*Aviso: não há propriedade datetime no Cypher! Você pode usar String com um padrão específico ou um Numeric a partir de uma data específica.*

`p.name`
Você pode acessar uma propriedade com o estilo de ponto.


Relacionamentos (ou Arestas)
---

**Conecta dois nós**

`[:KNOWS]`
É um *relacionamento* com o *label* **KNOWS**. É um *label* como um rótulo do nó. Começa com maiúsculas e usa UPPER_SNAKE_CASE.

`[k:KNOWS]`
O mesmo *relacionamento*, referido pela variável **k**, reutilizável na consulta, mas não é necessário.

`[k:KNOWS {since:2017}]`
O mesmo *relacionamento*, com *propriedades* (como *nó*), aqui **since**.

`[k:KNOWS*..4]`
É uma informação estrutural para usar em um *path* (visto posteriormente). Aqui, **\*..4** diz, “Corresponda o padrão, com a relação **k** que é repetida de 1 a 4 vezes.


Paths
---

**A maneira de misturar nós e relacionamentos.**

`(a:Person)-[:KNOWS]-(b:Person)`
Um path descrevendo que **a** e **b** se conhecem.

`(a:Person)-[:MANAGES]->(b:Person)`
Um path pode ser direcionado. Este path descreve que **a** é o gerente de **b**.

`(a:Person)-[:KNOWS]-(b:Person)-[:KNOWS]-(c:Person)`
Você pode encadear vários relacionamentos. Este path descreve o amigo de um amigo.

`(a:Person)-[:MANAGES]->(b:Person)-[:MANAGES]->(c:Person)`
Uma encadeamento também pode ser direcionada. Este path descreve que **a** é o chefe de **b** e o grande chefe de **c**.

Padrões frequentemente usados ​​(do Neo4j doc) :

```
// Amigo de um amigo
(user)-[:KNOWS]-(friend)-[:KNOWS]-(foaf)

// Path mais curto
path = shortestPath( (user)-[:KNOWS*..5]-(other) )

// Filtragem colaborativa
(user)-[:PURCHASED]->(product)<-[:PURCHASED]-()-[:PURCHASED]->(otherProduct)

// Navegação de árvore
(root)<-[:PARENT*]-(leaf:Category)-[:ITEM]->(data:Product)

```


Crie consultas
---

Crie um novo nó
```
CREATE (a:Person {name:"Théo Gauchoux"})
RETURN a
```
*`RETURN` permite ter um resultado após a consulta. Pode ser múltiplo, como `RETURN a, b`.*

Crie um novo relacionamento (com 2 novos nós)
```
CREATE (a:Person)-[k:KNOWS]-(b:Person)
RETURN a,k,b
```

Consultas que casam
---

Casam todos os nós
```
MATCH (n)
RETURN n
```

Casam nós por label
```
MATCH (a:Person)
RETURN a
```

Casam nós por label e propriedade
```
MATCH (a:Person {name:"Théo Gauchoux"})
RETURN a
```

Casam nós de acordo com os relacionamentos (não direcionados)
```
MATCH (a)-[:KNOWS]-(b)
RETURN a,b
```

Casam nós de acordo com os relacionamentos (direcionados)
```
MATCH (a)-[:MANAGES]->(b)
RETURN a,b
```

Casam nós com um cláusula `WHERE`
```
MATCH (p:Person {name:"Théo Gauchoux"})-[s:LIVES_IN]->(city:City)
WHERE s.since = 2015
RETURN p,state
```

Você pode usa a cláusula `MATCH WHERE` com a cláusula `CREATE`
```
MATCH (a), (b)
WHERE a.name = "Jacquie" AND b.name = "Michel"
CREATE (a)-[:KNOWS]-(b)
```


Atualizar consultas
---

Atualizar uma propriedade específica de um nó
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p.age = 23
```

Substituir todas as propriedades de um nó
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p = {name: "Michel", age: 23}
```

Adicionar nova propriedade a um nó
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p + = {studies: "IT Engineering"}
```

Adicione um label a um nó
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p:Internship
```


Excluir consultas
---

Excluir um nó específico (os relacionamentos vinculados devem ser excluídos antes)
```
MATCH (p:Person)-[relationship]-()
WHERE p.name = "Théo Gauchoux"
DELETE relationship, p
```

Remover uma propriedade em um nó específico
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
REMOVE p.age
```
*Prestar atenção à palavra chave `REMOVE`, não é `DELETE` !*

Remover um label de um nó específico
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
DELETE p:Person
```

Excluir o banco de dados inteiro
```
MATCH (n)
OPTIONAL MATCH (n)-[r]-()
DELETE n, r
```
*Sério, é o `rm -rf /` do Cypher !*


Outras cláusulas úteis
---

`PROFILE`
Antes de uma consulta, mostre o plano de execução dela.

`COUNT(e)`
Contar entidades (nós ou relacionamentos) que casam com **e**.

`LIMIT x`
Limite o resultado aos primeiros x resultados.


Dicas Especiais
---

- Há apenas comentários de uma linha no Cypher, com barras duplas : // Comentários
- Você pode executar um script Cypher armazenado em um arquivo **.cql** diretamente no Neo4j (é uma importação). No entanto, você não pode ter várias instruções neste arquivo (separadas por **;**).
- Use o shell Neo4j para escrever Cypher, é realmente incrível.
- O Cypher será a linguagem de consulta padrão para todos os bancos de dados de gráficos (conhecidos como **OpenCypher**).
