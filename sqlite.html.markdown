---
language: SQLite with Python
filename: learnsqlite.py
contributors:
  - ["Arihant Tripathy", "https://github.com/Arihant25"]
---

SQLite is a relational database management system (RDBMS). It is a software library that implements a self-contained, serverless, zero-configuration, transactional SQL database engine. SQLite is the most widely used open-source database engine.

Here we are going to learn how to use SQLite to create a simple database and insert data into it, using Python.


```python
import sqlite3

# Create a database file with the name books-collection.db

db = sqlite3.connect('books-collection.db')

# Creates a cursor which is used to execute SQL commands

cursor = db.cursor()

# This executes standard SQL syntax. Here we are creating a new table called books.

cursor.execute('''
CREATE TABLE books (
id INTEGER PRIMARY KEY,
title varchar(250) NOT NULL UNIQUE,
author varchar(250) NOT NULL,
rating FLOAT NOT NULL)
''')

# This inserts a new row into the table with the given values.

cursor.execute('''
INSERT INTO books VALUES(1, "Harry Potter", "J.K. Rowling", 9.3)
''')

# Commit the changes to the database so that they are saved.

db.commit()
```

## Further Reading

* [Codecademy - SQL](https://www.codecademy.com/learn/learn-sql) A good introduction to basic SQL syntax in a "learn by doing it" format.
