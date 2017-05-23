---
language: sql
contributors:
    - ["Abraham Toriz", "https://github.com/categulario"]
filename: learnsql.sql
---

SQL stands for Structured Query Language. It is the stantdard query language
or relational databases.

```sql
-- This is a single line comment

-- The first thing you need is a database, so create one
CREATE SCHEMA company;

-- Next you need to organize the data into tables whose structure you define
-- when it is created.
--
-- Each table may have a primary key that identifies each row uniquely and allows
-- optimized queries.
--
-- When defining the fields for a table you should specify its name and type.
-- It is also possible to specify wether or not the values for that column
-- are allowed to be null.
--
-- Some of the data types available are:
-- VARCHAR(N)      Variable length string (up to N chars)
-- CHAR(N)         Fixed length string (up to N chars)
-- BIT(N)          Fixed length bit string (up to N bits)
-- INT             A 4-byte integer value
-- SMALLINT        A 2-byte integer value
-- FLOAT           A floating-point number
-- DECIMAL(N, M)   A formatted decimal value with N integer positions and M decimal
-- DATE            A year-month-day date
-- TIMESTAMP       A field containing both date and time data
CREATE TABLE department (
    id INT NOT NULL,
    name VARCHAR(50),

    PRIMARY KEY (id)
);

-- Here is a more extended CREATE TABLE statement that adds an unique key
-- that tells the database engine that every value for that column must be
-- unique. It is also possible to specify these keys later via ALTER TABLE
-- statements.
--
-- The last part tells that this table references the previously created
-- deparment table via the department_id column.
CREATE TABLE employee (
    id INT NOT NULL,
    name VARCHAR(100) NOT NULL,
    age INT,
    bird_date DATE,

    email VARCHAR(100),
    department_id INT,

    salary DECIMAL(10, 2) DEFAULT 50000,
    created_at TIMESTAMP,

    PRIMARY KEY (id),
    UNIQUE (email),
    FOREIGN KEY (department_id) REFERENCES department(id)
);

-- Query for all rows and columns in a table
SELECT * FROM employee;

-- Query some fields with condition
SELECT id, name, age FROM employee WHERE age < 25;

-- Count number of rows in a table
SELECT COUNT(*) FROM employee;

-- Count distinct rows, set alias to column and order
SELECT DISTINCT(age), COUNT(*) AS count FROM table GROUP BY age ORDER BY count DESC;

-- Insert data
INSERT INTO employee (id, name) VALUES (1, 'Fernando');

-- Insert from local file
LOAD DATA LOCAL INFILE 'data.csv' INTO TABLE employee FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n' (col_1, col_2);

-- Update row
UPDATE employee SET salary=100000, department_id=3 WHERE id=1;
```

## Ready For More?

### Free Online

* [Official MySQL docs](https://dev.mysql.com/doc/refman/5.7/en/)
