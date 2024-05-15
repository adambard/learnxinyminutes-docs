---
language: RDF
filename: learnrdf.ttl
contributors:
- ["Bob DuCharme", "http://bobdc.com/"]
---

RDF (Resource Description Framework) is a [W3C
standard](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/) data
model. The W3C has standardized several RDF syntaxes; examples below use the
most popular one, [Turtle](https://www.w3.org/TR/turtle/).

One nice advantage of Turtle files is that if you concatenate any two
syntactically valid Turtle files, you will have another syntactically valid
Turtle file. This is one of many things about RDF that ease data integration.

The W3C standard query language for RDF datasets is
[SPARQL](https://www.w3.org/TR/sparql11-query/).

RDF expresses all facts as three-part {subject, predicate, object} statements
known as triples. Because the same entity can be the subject of some triples
and the object of others, a set of triples can represent a graph data
structure. A large-scale storage system for triples is called a triplestore,
and falls into the graph database category of NoSQL databases.

RDF subjects and predicates must be URIs (Uniform Resource Identifiers), which
usually look like URLs but function as identifiers, not locators. The use of
URIs provides context for resource identifiers to make them unambiguous—for
example, to tell a book title from a job title.

```
# The hash symbol is the comment delimiter. 

# Turtle triple statements end with periods like natural language sentences.

# These two triples tell us that the mythical Example Company's
# employee 134 has a hire date of 2022-11-12 and a family name of Smith:

<http://example.com/emp134> <http://example.com/hireDate> "2022-11-12" .
<http://example.com/emp134> <http://example.com/familyName> "Smith" .

# Declaring prefixes to stand in for namespaces reduces verbosity. These
# declarations typically go at the beginning of the file, but the only
# requirement is that they come before the first use of the prefix they declare.

@prefix ex: <http://example.com/> .
ex:emp134 ex:hireDate "2022-11-12" .
ex:emp134 ex:familyName "Smith" .

# A semicolon means that the next triple uses the same subject as the last
# one. This is handy for listing data about a single resource. The following
# example means the same thing as the previous one.

@prefix ex: <http://example.com/> .
ex:emp134 ex:hireDate "2022-11-12" ;
          ex:familyName "Smith" .

# A comma means that the next triple has the same subject and predicate as
# the previous one.

ex:emp134 ex:nickname "Smithy", "Skipper", "Big J". 

# Three single or double quote marks at the beginning and end of a value let
# you define a  multi-line string value.

ex:emp134 ex:description """
Skipper joined the company in November. 

He always has a joke for everyone.""" . 

# Using URIs from existing standard vocabulary namespaces eases both data
# integration and interoperability with the large amount of RDF that already
# exists. Mixing and matching of standard and local custom namespaces is
# common.

@prefix vcard: <http://www.w3.org/2006/vcard/ns#> .
ex:emp134 ex:hireDate "2022-11-12" ;
          vcard:family-name "Smith" .

# Related RDF standards provide vocabularies that are popular for basic
# facts. The rdfs:label predicate from the RDF Schema standard is a common 
# way to indicate a human-readable name.

@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
ex:hireDate rdfs:label "hire date" . 

# String object values can include language codes, making
# multi-lingual representation of entities easier for applications
# reading the data (for example, when generating a user interface).

ex:hireDate rdfs:label "hire date"@en, "date d'embauche"@fr  . 

# Representing a triple's object with a URI (or prefixed name) is not required
# but lets you connect up triples into a graph.

ex:emp134 vcard:family-name "Smith" .
ex:emp113 vcard:family-name "Jones" ;
          ex:reportsTo ex:emp134 . 

# Objects can be datatypes from the XML Schema part 2 standard or your own
# custom datatypes.

@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
ex:emp134 vcard:family-name "Smith"^^xsd:string ;  # default data type
          ex:hireDate "2022-11-12"^^xsd:date ;
          ex:rating "3.5"^^ex:someCustomType . 

# The use of schemas with RDF is optional. Schemas may describe all or a
# subset of a dataset. They use a vocabulary described by the W3C RDF Schema
# (RDFS) standard, usually with a prefix of rdfs.

# These schemas are descriptive, to ease the accommodation of new
# datasets, not proscriptive rules about how new data should be 
# created. The following declares a class. (Note that RDFS is itself 
# expressed in triples.)

@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> . 
ex:Person rdf:type rdfs:Class .

# The following triple means the same as the preceding one but 
# uses a Turtle shortcut for terseness and more readability.

ex:Person a rdfs:Class .

# That last triple declares that ex:Person is an instance of a class, and the
# following declares that employee 113 is an instance of the class Employee.

ex:emp113 a ex:Employee . 

# The first triple below is actually unnecessary because a typical
# RDFS processor will infer from the second one that ex:Employee is a
# class. (Only a subset of RDF parsers perform RDFS inferencing.)

ex:Employee a rdfs:Class .
ex:Employee rdfs:subClassOf ex:Person .

# An RDF parser that reads the last four triples shown and understands
# RDFS will infer that ex:emp113 is an instance of ex:Person, because
# it's an instance of ex:Employee, a subclass of ex:Person.

# RDFS lets you declare properties and associate them with classes. 
# Properties are first class resources and don't "belong" to classes 
# in the object-oriented sense. rdfs:domain means "the following object 
# class uses the property named by this triple's subject". rdfs:range 
# means "the property named by this triple's subject will have a value of 
# the following class or type". 

ex:birthday rdf:type rdf:Property ; 
            rdfs:domain ex:Person ;
            rdfs:range xsd:date .
```

## Further Reading

* [RDF Primer — Turtle version](https://www.w3.org/2007/02/turtle/primer/) from the W3C
* [What is RDF?](https://www.bobdc.com/blog/whatisrdf/) on bobdc.com
* [What is RDFS?](https://www.bobdc.com/blog/whatisrdfs/) on bobdc.com 
* [Introduction to RDF and SPARQL](https://data.europa.eu/sites/default/files/d2.1.2_training_module_1.3_introduction_to_rdf_sparql_en_edp.pdf) at data.europa.eu

