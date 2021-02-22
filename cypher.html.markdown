---
language: cypher
filename: LearnCypher.cql
contributors:
    - ["Théo Gauchoux", "https://github.com/TheoGauchoux"]
---

Cypher is the Neo4j’s query language to manipulate graphs easily. It reuses syntax from SQL and mixes it with kind of ascii-art to represent graphs.
This tutorial assumes that you already know graph concepts like nodes and relationships.

[Read more here.](https://neo4j.com/developer/cypher-query-language/)


Nodes
---

**Represents a record in a graph.**

`()`
It's an empty *node*, to indicate that there is a *node*, but it's not relevant for the query.

`(n)`
It's a *node* referred by the variable **n**, reusable in the query. It begins with lowercase and uses camelCase.

`(p:Person)`
You can add a *label* to your node, here **Person**. It's like a type / a class / a category. It begins with uppercase and uses camelCase.

`(p:Person:Manager)`
A node can have many *labels*.

`(p:Person {name : 'Théo Gauchoux', age : 22})`
A node can have some *properties*, here **name** and **age**. It begins with lowercase and uses camelCase.

The types allowed in properties :

 - Numeric
 - Boolean
 - String
 - List of previous primitive types

*Warning : there isn't datetime property in Cypher ! You can use String with a specific pattern or a Numeric from a specific date.*

`p.name`
You can access to a property with the dot style.


Relationships (or Edges)
---

**Connects two nodes**

`[:KNOWS]`
It's a *relationship* with the *label* **KNOWS**. It's a *label* as the node's label. It begins with uppercase and use UPPER\_SNAKE\_CASE.

`[k:KNOWS]`
The same *relationship*, referred by the variable **k**, reusable in the query, but it's not necessary.

`[k:KNOWS {since:2017}]`
The same *relationship*, with *properties* (like *node*), here **since**.

`[k:KNOWS*..4]`
It's a structural information to use in a *path* (seen later). Here, **\*..4** says "Match the pattern, with the relationship **k** which be repeated between 1 and 4 times.


Paths
---

**The way to mix nodes and relationships.**

`(a:Person)-[:KNOWS]-(b:Person)`
A path describing that **a** and **b** know each other.

`(a:Person)-[:MANAGES]->(b:Person)`
A path can be directed. This path describes that **a** is the manager of **b**.

`(a:Person)-[:KNOWS]-(b:Person)-[:KNOWS]-(c:Person)`
You can chain multiple relationships. This path describes the friend of a friend.

`(a:Person)-[:MANAGES]->(b:Person)-[:MANAGES]->(c:Person)`
A chain can also be directed. This path describes that **a** is the boss of **b** and the big boss of **c**.

Patterns often used (from Neo4j doc) :

```
// Friend-of-a-friend 
(user)-[:KNOWS]-(friend)-[:KNOWS]-(foaf)

// Shortest path
path = shortestPath( (user)-[:KNOWS*..5]-(other) )

// Collaborative filtering
(user)-[:PURCHASED]->(product)<-[:PURCHASED]-()-[:PURCHASED]->(otherProduct)

// Tree navigation 
(root)<-[:PARENT*]-(leaf:Category)-[:ITEM]->(data:Product)

```


Create queries
---

Create a new node
```
CREATE (a:Person {name:"Théo Gauchoux"})
RETURN a
```
*`RETURN` allows to have a result after the query. It can be multiple, as `RETURN a, b`.*

Create a new relationship (with 2 new nodes)
```
CREATE (a:Person)-[k:KNOWS]-(b:Person)
RETURN a,k,b
```

Match queries
---

Match all nodes
```
MATCH (n)
RETURN n
```

Match nodes by label
```
MATCH (a:Person)
RETURN a
```

Match nodes by label and property
```
MATCH (a:Person {name:"Théo Gauchoux"})
RETURN a
```

Match nodes according to relationships (undirected)
```
MATCH (a)-[:KNOWS]-(b)
RETURN a,b
```

Match nodes according to relationships (directed)
```
MATCH (a)-[:MANAGES]->(b)
RETURN a,b
```

Match nodes with a `WHERE` clause
```
MATCH (p:Person {name:"Théo Gauchoux"})-[s:LIVES_IN]->(city:City)
WHERE s.since = 2015
RETURN p,state
```

You can use `MATCH WHERE` clause with `CREATE` clause
```
MATCH (a), (b)
WHERE a.name = "Jacquie" AND b.name = "Michel"
CREATE (a)-[:KNOWS]-(b)
```


Update queries
---

Update a specific property of a node
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p.age = 23
```

Replace all properties of a node
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p = {name: "Michel", age: 23}
```

Add new property to a node
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p + = {studies: "IT Engineering"}
```

Add a label to a node
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
SET p:Internship
```


Delete queries
---

Delete a specific node (linked relationships must be deleted before)
```
MATCH (p:Person)-[relationship]-()
WHERE p.name = "Théo Gauchoux"
DELETE relationship, p
```

Remove a property in a specific node
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
REMOVE p.age
```
*Pay attention to the `REMOVE`keyword, it's not `DELETE` !*

Remove a label from a specific node
```
MATCH (p:Person)
WHERE p.name = "Théo Gauchoux"
DELETE p:Person
```

Delete entire database
```
MATCH (n)
OPTIONAL MATCH (n)-[r]-()
DELETE n, r
```
*Seriously, it's the `rm -rf /` of Cypher !*


Other useful clauses
---

`PROFILE`
Before a query, show the execution plan of it.

`COUNT(e)`
Count entities (nodes or relationships) matching **e**.

`LIMIT x`
Limit the result to the x first results.


Special hints
---

- There is just single-line comments in Cypher, with double-slash : // Comments
- You can execute a Cypher script stored in a **.cql** file directly in Neo4j (it's an import). However, you can't have multiple statements in this file (separated by **;**).
- Use the Neo4j shell to write Cypher, it's really awesome.
- The Cypher will be the standard query language for all graph databases (known as **OpenCypher**).
