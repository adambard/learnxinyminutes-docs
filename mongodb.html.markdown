---
language: MongoDB 
filename: mongo.js 
contributors:
  - ["Raj Piskala", "https://www.rajpiskala.ml/"]
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

While queries in MongoDB are made using a JSON-like\* format, MongoDB stores its
documents internally in the Binary JSON (BSON format). BSON is not human
readable like JSON as it's a binary encoding. However, this allows for end users
to have access to more types than regular JSON, such as an integer or float
type. Many other types, such as regular expressions, dates, or raw binary are
supported too.

[Here](https://docs.mongodb.com/manual/reference/bson-types/) is the full list
of all types that are supported.

- We refer JSON-like to mean JSON but with these extended types. For example,
  you can make queries directly with a regular expression or timestamp in
  MongoDB and you can receive data that has those types too.

```js
/////////////////////////////////////////////////////////
/////////////////// Getting Started /////////////////////
/////////////////////////////////////////////////////////

// Start up the mongo database server
// NOTE - You will need to do this in a separate terminal as the process will 
// take over the terminal. You may want to use the --fork option
mongod // --fork

// Connecting to a remote Mongo server
// mongo "mongodb+srv://host.ip.address/admin" --username your-username

// Mongoshell has a proper JavaScript interpreter built in
3 + 2 // 5

// Show available databases
// MongoDB comes with the following databases built-in: admin, config, local
show dbs

// Switch to a new database (pre-existing or about to exist)
// NOTE: There is no "create" command for a database in MongoDB. 
// The database is created upon data being inserted into a collection
use employees

// Create a new collection
// NOTE: Inserting a document will implicitly create a collection anyways,
// so this is not required
db.createCollection('engineers')
db.createCollection('doctors')

// See what collections exist under employees
show collections

/////////////////////////////////////////////////////////
// Basic Create/Read/Update/Delete (CRUD) Operations: ///
/////////////////////////////////////////////////////////

/////////////// Insert (Create) /////////////////////////

// Insert one employee into the database
// Each insertion returns acknowledged true or false
// Every document has a unique _id value assigned to it automatically
db.engineers.insertOne({ name: "Jane Doe", age: 21, gender: 'Female' })

// Insert a list of employees into the `engineers` collection
// Can insert as an array of objects
db.engineers.insert([
  { name: "Foo Bar", age: 25, gender: 'Male' },
  { name: "Baz Qux", age: 27, gender: 'Other' },
])

// MongoDB does not enforce a schema or structure for objects
// Insert an empty object into the `engineers` collection
db.engineers.insertOne({})

// Fields are optional and do not have to match rest of documents
db.engineers.insertOne({ name: "Your Name", gender: "Male" })

// Types can vary and are preserved on insertion
// This can require additional validation in some languages to prevent problems
db.engineers.insert({ name: ['Foo', 'Bar'], age: 3.14, gender: true })

// Objects or arrays can be nested inside a document
db.engineers.insertOne({
  name: "Your Name",
  gender: "Female",
  skilledIn: [
    "MongoDB",
    "NoSQL",
  ],
  "date-of-birth": {
    "date": 1993-07-20T09:44:18.674Z,
    "age": 26
  },
})

// We can override the _id field
// Works fine
db.engineers.insertOne({
  _id: 1,
  name: "An Engineer",
  age: 25,
  gender: "Female",
})

// Be careful, as _id must ALWAYS be unique for the collection otherwise 
// the insertion will fail
// Fails with a WriteError indicating _id is a duplicate value
db.engineers.insertOne({
  _id: 1,
  name: "Another Engineer",
  age: 25,
  gender: "Male",
})

// Works fine as this is a different collection
db.doctors.insertOne({
  _id: 1,
  name: "Some Doctor",
  age: 26,
  gender: "Other",
})

/////////////////// Find (Read) ////////////////////////
// Queries are in the form of db.collectionName.find(<filter>)
// Where <filter> is an object

// Show everything in our database so far, limited to a 
// maximum of 20 documents at a time
// Press i to iterate this cursor to the next 20 documents
db.engineers.find({})

// We can pretty print the result of any find() query
db.engineers.find({}).pretty()

// MongoDB queries take in a JS object and search for documents with matching 
// key-value pairs
// Returns the first document matching query
// NOTE: Order of insertion is not preserved in the database, output can vary
db.engineers.findOne({ name: 'Foo Bar' })

// Returns all documents with the matching key-value properties as a cursor 
// (which can be converted to an array)
db.engineers.find({ age: 25 })

// Type matters when it comes to queries
// Returns nothing as all ages above are integer type
db.engineers.find({ age: '25' })

// find() supports nested objects and arrays just like create()
db.engineers.find({
  name: "Your Name",
  gender: "Female",
  skilledIn: [
    "MongoDB",
    "NoSQL",
  ],
  "date-of-birth": {
    "date": 1993-07-20T09:44:18.674Z,
    "age": 26
  },
})

///////////////////////// Update ////////////////////////
// Queries are in the form of db.collectionName.update(<filter>, <update>)
// NOTE: <update> will always use the $set operator.
// Several operators are covered later on in the tutorial.

// We can update a single object
db.engineers.updateOne({ name: 'Foo Bar' }, { $set: { name: 'John Doe', age: 100 }})

// Or update many objects at the same time
db.engineers.update({ age: 25 }, { $set: { age: 26 }})

// We can use { upsert: true } if we would like it to insert if the document doesn't already exist,
// or to update if it does
// Returns matched, upserted, modified count
db.engineers.update({ name: 'Foo Baz' },
  { $set:
    {
      age: 26,
      gender: 'Other'
    }
  },
  { upsert: true }
)

/////////////////////// Delete /////////////////////////
// Queries are in the form of db.collectionName.delete(<filter>)

// Delete first document matching query, always returns deletedCount
db.engineers.deleteOne({ name: 'Foo Baz' })

// Delete many documents at once
db.engineers.deleteMany({ gender: 'Male' })

// NOTE: There are two methods db.collection.removeOne(<filter>) and 
// db.collection.removeMany(<filter>) that also delete objects but have a
// slightly different return value.
// They are not included here as they have been deprecated in the NodeJS driver.

/////////////////////////////////////////////////////////
//////////////////// Operators //////////////////////////
/////////////////////////////////////////////////////////

// Operators in MongoDB have a $ prefix. For this tutorial, we are only looking 
// at comparison and logical operators, but there are many other types of
// operators

//////////////// Comparison Operators ///////////////////

// Find all greater than or greater than equal to some condition
db.engineers.find({ age: { $gt: 25 }})
db.engineers.find({ age: { $gte: 25 }})

// Find all less than or less than equal to some condition
db.engineers.find({ age: { $lt: 25 }})
db.engineers.find({ age: { $lte: 25 }})

// Find all equal or not equal to
// Note: the $eq operator is added implicitly in most queries
db.engineers.find({ age: { $eq: 25 }})
db.engineers.find({ age: { $ne: 25 }})

// Find all that match any element in the array, or not in the array
db.engineers.find({ age: { $in: [ 20, 23, 24, 25 ]}})
db.engineers.find({ age: { $nin: [ 20, 23, 24, 25 ]}})

//////////////// Logical Operators ///////////////////

// Join two query clauses together
// NOTE: MongoDB does this implicitly for most queries
db.engineers.find({ $and: [
  gender: 'Female',
  age: {
    $gte: 18
  }
]})

// Match either query condition
db.engineers.find({ $or: [
  gender: 'Female',
  age: {
    $gte: 18
  }
]})

// Negates the query
db.engineers.find({ $not: {
  gender: 'Female'
}})

// Must match none of the query conditions
db.engineers.find({ $nor [
  gender: 'Female',
  age: {
    $gte: 18
  }
]})

/////////////////////////////////////////////////////////
//////////////// Database Operations: ///////////////////
/////////////////////////////////////////////////////////

// Delete (drop) the employees database
// THIS WILL DELETE ALL DOCUMENTS IN THE DATABASE!
db.dropDatabase()

// Create a new database with some data
use example
db.test.insertOne({ name: "Testing data, please ignore!", type: "Test" })

// Quit Mongo shell
exit

// Import/export database as BSON:

// Mongodump to export data as BSON for all databases
// Exported data is found in under "MongoDB Database Tools/bin/dump"
// NOTE: If the command is not found, navigate to "MongoDB Database Tools/bin" 
// and use the executable from there mongodump

// Mongorestore to restore data from BSON
mongorestore dump

// Import/export database as JSON:
// Mongoexport to export data as JSON for all databases
mongoexport --collection=example

// Mongoimport to export data as JSON for all databases
mongoimport  --collection=example
```

## Further Reading

### Setup Videos

- [Install MongoDB - Windows 10](https://www.youtube.com/watch?v=85A6m1soKww)
- [Install MongoDB - Mac](https://www.youtube.com/watch?v=DX15WbKidXY)
- [Install MongoDB - Linux
  (Ubuntu)](https://www.youtube.com/watch?v=wD_2pojFWoE)

### Input Validation

From the examples above, if input validation or structure is a concern, I would
take a look at the following ORMs:

- [Mongoose (Node.js)](https://mongoosejs.com/docs/) - Input validation through
  schemas that support types, required values, minimum and maximum values.
- [MongoEngine (Python)](http://mongoengine.org/) - Similar to Mongoose, but I
  found it somewhat limited in my experience
- [MongoKit (Python)](https://github.com/namlook/mongokit) - Another great
  alternative to MongoEngine that I find easier to use than MongoEngine

For statically strongly typed languages (e.g. Java, C++, Rust), input validation
usually doesn't require a library as they define types and structure at compile
time.

### Resources

If you have the time to spare, I would strongly recommend the courses on
[MongoDB University](https://university.mongodb.com/). They're by MongoDB
themselves and go into much more detail while still being concise. They're a mix
of videos and quiz questions and this was how I gained my knowledge of MongoDB.

I would recommend the following video series for learning MongoDB:

- [MongoDB Crash Course - Traversy
  Media](https://www.youtube.com/watch?v=-56x56UppqQ)
- [MongoDB Tutorial for Beginners -
  Amigoscode](https://www.youtube.com/watch?v=Www6cTUymCY)

Language-specific ones that I used before:

- [Build A REST API With Node.js, Express, & MongoDB - Web Dev
  Simplified](https://www.youtube.com/watch?v=fgTGADljAeg)
- [MongoDB with Python Crash Course - Tutorial for Beginners -
  FreeCodeCamp](https://www.youtube.com/watch?v=E-1xI85Zog8)
- [How to Use MongoDB with Java - Random
  Coder](https://www.youtube.com/watch?v=reYPUvu2Giw)
- [An Introduction to Using MongoDB with Rust -
  MongoDB](https://www.youtube.com/watch?v=qFlftfLGwPM)

Most of the information above was cross-referenced with the [MongoDB
docs](https://www.mongodb.com/). Here are the docs for each section:

- [MongoDB Types](https://docs.mongodb.com/manual/reference/bson-types/) - List
  of all types that MongoDB supports natively
- [MongoDB Operators](https://docs.mongodb.com/manual/reference/operator/) -
  List of operators MongoDB supports natively
- [MongoDB CRUD](https://docs.mongodb.com/manual/reference/command/nav-crud/) -
  Commands for create, read, update, delete

If you've been enjoying MongoDB so far and want to explore intermediate
features, I would look at
[aggregation](https://docs.mongodb.com/manual/reference/command/nav-aggregation/),
[indexing](https://docs.mongodb.com/manual/indexes/), and
[sharding](https://docs.mongodb.com/manual/sharding/).

- Aggregation - useful for creating advanced queries to be executed by the
  database
- Indexing allows for caching, which allows for much faster execution of queries
- Sharding allows for horizontal data scaling and distribution between multiple
  machines.
