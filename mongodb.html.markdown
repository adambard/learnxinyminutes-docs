---
language: MongoDB
filename: mongo.js
contributors:
  - ['Raj Piskala', 'https://www.rajpiskala.ml/']
---

MongoDB is a NoSQL document database for high volume data storage.

MongoDB uses collections and documents for its storage. Each document consists
of key-value pairs using JSON-like syntax, similar to a dictionary or JavaScript
object.

Likewise, as MongoDB is a NoSQL database, it uses its own query language, Mongo
Query Language (MQL) which uses JSON for querying.

## Getting Started

### Installation

MongoDB can either be installed locally following the instructions
[here](https://docs.mongodb.com/manual/installation/) or you can create a
remotely-hosted free 512 MB cluster
[here](https://www.mongodb.com/cloud/atlas/register). Links to videos with
instructions on setup are at the bottom.

This tutorial assumes that you have the MongoDB Shell from
[here](https://www.mongodb.com/try/download/shell). You can also download the
graphical tool, MongoDB Compass, down below from the same link.

### Components

After installing MongoDB, you will notice there are multiple command line tools.
The three most important of which are:

- `mongod` - The database server which is responsible for managing data and
  handling queries
- `mongos` - The sharding router, which is needed if data will be distributed
  across multiple machines
- `mongo` - The database shell (using JavaScript) through which we can configure
  our database

Usually we start the `mongod` process and then use a separate terminal with
`mongo` to access and modify our collections.

### JSON & BSON

While queries in MongoDB are made using a JSON-like* format, MongoDB stores its documents internally in the Binary JSON (BSON format). BSON is not human readable like JSON as it's a binary encoding. However, this allows for end users to have access to more types than regular JSON, such as an integer or float type. Many other types, such as regular expressions, dates, or raw binary are supported too. 

[Here](https://docs.mongodb.com/manual/reference/bson-types/) is the full list
of all types that are supported.

* We refer JSON-like to mean JSON but with these extended types. For example, you can make queries directly with a regular expression or timestamp in MongoDB and you can receive data that has those types too.

