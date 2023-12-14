---
language: SQL
filename: learnsql.sql
contributors:
  - ["Bob DuCharme", "http://bobdc.com/"]
---

Structured Query Language (SQL) is an [ISO/IEC 9075](https://www.iso.org/standard/63555.html) standard language for creating and working with databases stored in a set of tables. Implementations usually add their own extensions to the language; [Comparison of different SQL implementations](http://troels.arvin.dk/db/rdbms/) is a good reference on product differences.

Implementations typically provide a command line prompt where you can enter the commands shown here interactively, and they also offer a way to execute a series of these commands stored in a script file.  (Showing that you’re done with the interactive prompt is a good example of something that isn’t standardized--most SQL implementations support the keywords QUIT, EXIT, or both.)

Several of these sample commands assume that the [MySQL employee sample database](https://dev.mysql.com/doc/employee/en/) available on [github](https://github.com/datacharmer/test_db) has already been loaded. The github files are scripts of commands, similar to the relevant commands below, that create and populate tables of data about a fictional company’s employees. The syntax for running these scripts will depend on the SQL implementation you are using. A utility that you run from the operating system prompt is typical.


```sql
-- Comments start with two hyphens. End each command with a semicolon.

-- SQL is not case-sensitive about keywords. The sample commands here
-- follow the convention of spelling them in upper-case because it makes
-- it easier to distinguish them from database, table, and column names.

-- Create and delete a database. Database and table names are case-sensitive.
CREATE DATABASE someDatabase;
DROP DATABASE someDatabase;

-- List available databases.
SHOW DATABASES;

-- Use a particular existing database.
USE employees;

-- Select all rows and columns from the current database's departments table.
-- Default activity is for the interpreter to scroll the results on your screen.
SELECT * FROM departments;

-- Retrieve all rows from the departments table,
-- but only the dept_no and dept_name columns.
-- Splitting up commands across lines is OK.
SELECT dept_no,
       dept_name FROM departments;

-- Retrieve all departments columns, but just 5 rows.
SELECT * FROM departments LIMIT 5;

-- Retrieve dept_name column values from the departments
-- table where the dept_name value has the substring 'en'.
SELECT dept_name FROM departments WHERE dept_name LIKE '%en%';

-- Retrieve all columns from the departments table where the dept_name
-- column starts with an 'S' and has exactly 4 characters after it.
SELECT * FROM departments WHERE dept_name LIKE 'S____';

-- Select title values from the titles table but don't show duplicates.
SELECT DISTINCT title FROM titles;

-- Same as above, but sorted (case-sensitive) by the title values.
SELECT DISTINCT title FROM titles ORDER BY title;

-- Show the number of rows in the departments table.
SELECT COUNT(*) FROM departments;

-- Show the number of rows in the departments table that
-- have 'en' as a substring of the dept_name value.
SELECT COUNT(*) FROM departments WHERE dept_name LIKE '%en%';

-- A JOIN of information from multiple tables: the titles table shows
-- who had what job titles, by their employee numbers, from what
-- date to what date. Retrieve this information, but instead of the
-- employee number, use the employee number as a cross-reference to
-- the employees table to get each employee's first and last name
-- instead. (And only get 10 rows.)

SELECT employees.first_name, employees.last_name,
       titles.title, titles.from_date, titles.to_date
FROM titles INNER JOIN employees ON
       employees.emp_no = titles.emp_no LIMIT 10;

-- List all the tables in all the databases. Implementations typically provide
-- their own shortcut command to do this with the database currently in use.
SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE='BASE TABLE';

-- Create a table called tablename1, with the two columns shown, for
-- the database currently in use. Lots of other options are available
-- for how you specify the columns, such as their datatypes.
CREATE TABLE tablename1 (fname VARCHAR(20), lname VARCHAR(20));

-- Insert a row of data into the table tablename1. This assumes that the
-- table has been defined to accept these values as appropriate for it.
INSERT INTO tablename1 VALUES('Richard','Mutt');

-- In tablename1, change the fname value to 'John'
-- for all rows that have an lname value of 'Mutt'.
UPDATE tablename1 SET fname='John' WHERE lname='Mutt';

-- Delete rows from the tablename1 table
-- where the lname value begins with 'M'.
DELETE FROM tablename1 WHERE lname LIKE 'M%';

-- Delete all rows from the tablename1 table, leaving the empty table.
DELETE FROM tablename1;

-- Remove the entire tablename1 table.
DROP TABLE tablename1;
```

## Further Reading

* [Codecademy - SQL](https://www.codecademy.com/learn/learn-sql) A good introduction to SQL in a "learn by doing it" format.
* [Database System Concepts](https://www.db-book.com) book's Chapter 3 - Introduction to SQL has an in depth explanation of SQL concepts.
