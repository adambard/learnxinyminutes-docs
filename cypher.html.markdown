---
language: cypher
filename: LearnCypher.cql
contributors:
    - ["Théo Gauchoux", "https://github.com/TheoGauchoux"]
---

Cypher is Neo4j's query language for easily manipulating graphs.
It reuses syntax from SQL and mixes it with kind of an ASCII-art to represent graphs.
This tutorial assumes that you already know graph concepts like nodes and relationships.

## Nodes represent a record in a graph

`()` is an empty *node*, to indicate that there is a *node*, but it's not relevant for the query.

`(n)` is a *node* referred by the variable `n`, reusable in the query. It begins with lowercase and uses camelCase.

`(p:Person)` - you can add a *label* to your node, here `Person`. It's like a type/class/category. It begins with uppercase and uses camelCase.

`(p:Person:Manager)` - a node can have many *labels*.

`(p:Person {name : 'Théo Gauchoux', age : 22})` - a node can have some *properties*, here `name` and `age`. It begins with lowercase and uses camelCase.

The types allowed in properties:

- Numeric
- Boolean
- String
- List of previous primitive types

*Warning: there's no datetime properties in Cypher! You can use a String with a specific pattern or a Numeric from a specific date.*

`p.name` - you can access a property with the dot style.

## Relationships (or Edges) connect two nodes

`[:KNOWS]` is a *relationship* with the *label* `KNOWS`. It's a *label* as the node's label. It uses UPPER\_SNAKE\_CASE.

`[k:KNOWS]` - the same *relationship*, referred by the variable `k`, reusable in the query, but it's not necessary.

`[k:KNOWS {since:2017}]` - the same *relationship*, with *properties* (like *node*), here `since`.

`[k:KNOWS*..4]` is structural information to use in a *path* (seen later). Here, `\*..4` says "Match the pattern, with the relationship `k` which can be repeated between 1 and 4 times.

## Paths - the way to mix nodes and relationships.

`(a:Person)-[:KNOWS]-(b:Person)` - a path describing that `a` and `b` know each other.

`(a:Person)-[:MANAGES]->(b:Person)` - a path can be directed. This path describes that `a` is the manager of `b`.

`(a:Person)-[:KNOWS]-(b:Person)-[:KNOWS]-(c:Person)` - you can chain multiple relationships. This path describes the friend of a friend.

`(a:Person)-[:MANAGES]->(b:Person)-[:MANAGES]->(c:Person)` - a chain can also be directed. This path describes that `a` is the boss of `b` and the big boss of `c`.

Commonly used patterns (from Neo4j documentation):

```cypher
// Friend-of-a-friend
(user)-[:KNOWS]-(friend)-[:KNOWS]-(foaf)

// Shortest path
path = shortestPath( (user)-[:KNOWS*..5]-(other) )

// Collaborative filtering
(user)-[:PURCHASED]->(product)<-[:PURCHASED]-()-[:PURCHASED]->(otherProduct)

// Tree navigation
(root)<-[:PARENT*]-(leaf:Category)-[:ITEM]->(data:Product)
```

## Create queries

Create a new node

```cypher
CREATE (a:Person {name:"Théo Gauchoux"})
RETURN a
```

*`RETURN` allows to have a result after the query. It can be multiple, as `RETURN a, b`.*

Create a new relationship (with 2 new nodes)

```cypher
CREATE (a:Person)-[k:KNOWS]-(b:Person)
RETURN a,k,b
```

## Match queries

Match all nodes

```cypher
MATCH (n)
RETURN n
```

Match nodes by label

```cypher
MATCH (a:Person)
RETURN a
```

Match nodes by label and property

```cypher
MATCH (a:Person {name:"Théo Gauchoux"})
RETURN a
```

Match nodes according to relationships (undirected)

```cypher
MATCH (a)-[:KNOWS]-(b)
RETURN a,b
```

Match nodes according to relationships (directed)

```cypher
MATCH (a)-[:MANAGES]->(b)
RETURN a,b
```

Match nodes with a `WHERE` clause

```cypher
MATCH (p:Person {name:"Théo Gauchoux"})-[s:LIVES_IN]->(city:City)
WHERE s.since = 2015
RETURN p,state
```

You can use `MATCH WHERE` clause with `CREATE` clause

```cypher
MATCH (a), (b)
WHERE a.name = "Jacquie" AND b.name = "Michel"
CREATE (a)-[:KNOWS]-(b)
```

## Update queries

Update a specific property of a node

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p.age = 23
```

Replace all properties of a node

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p = {name: "Michel", age: 23}
```

Add new property to a node

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p += {studies: "IT Engineering"}
```

Add a label to a node

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p:Internship
```

## Delete queries

Delete a specific node (linked relationships must be deleted before)

```cypher
MATCH (p:Person)-[relationship]-()
WHERE p.name = "Théo Gauchoux"
DELETE relationship, p
```

Remove a property in a specific node

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
REMOVE p.age
```

*Pay attention to the `REMOVE` keyword, it's not `DELETE`!*

Remove a label from a specific node

```cypher
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
DELETE p:Person
```

Delete entire database

```cypher
MATCH (n)
OPTIONAL MATCH (n)-[r]-()
DELETE n, r
```

*Seriously, it's the `rm -rf /` of Cypher!*

## Other useful clauses

`PROFILE` - before a query, show its execution plan.

`COUNT(e)` - count entities (nodes or relationships) matching `e`.

`LIMIT x` - limit the result to the first `x` results.

## Special hints

- Cypher only has single-line comments, using double-slashes: `// comment`
- You can execute a Cypher script stored in a .cql file directly in Neo4j (it's an import). However, you can't have multiple statements in this file (separated by `;`).
- Use the Neo4j shell to write Cypher, it's really awesome.
- Cypher will be the standard query language for all graph databases (known as [openCypher](https://opencypher.org/)).

Read more [here](https://neo4j.com/developer/cypher-query-language/).

