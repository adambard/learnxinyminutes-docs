---
language: SQL
filename: learnsql.sql
contributors:
  - ["Bob DuCharme", "http://bobdc.com/"]
  - ["Jonathan Bowman", "https://bowmanjd.com/"]
---

Structured Query Language (SQL) is an [ISO/IEC 9075](https://www.iso.org/standard/63555.html) standard language for creating and working with databases stored in a set of tables. Implementations usually add their own extensions to the language; [Comparison of different SQL implementations](http://troels.arvin.dk/db/rdbms/) is a good reference on product differences.

Implementations typically provide a command line prompt where you can enter the commands shown here interactively, and they also offer a way to execute a series of these commands stored in a script file.  (Showing that you’re done with the interactive prompt is a good example of something that isn’t standardized--most SQL implementations support the keywords QUIT, EXIT, or both.)

```sql
-- Simple comments start with two hyphens.

/*
Bracketed comments start with a forward slash and asterisk. End each
complete SQL command with a semicolon. This is good practice even when
the database engine does not require it in all cases. SQL is not
case-sensitive about keywords. The sample commands here follow the
convention of spelling them in upper-case because it makes it easier
to distinguish them from database, table, and column names. Bracketed
comments end with an asterisk followed by a forward slash.
*/

-- Create a database. Database and table names are often case-sensitive.
CREATE DATABASE employee;

-- If using Oracle, the database creation process is significantly more involved
-- than just CREATE DATABASE. You might consider using the graphical DBCA tool or
-- creating the database when installing Oracle.

-- Use a particular existing database.
USE employee;

-- Some database engines do not support the USE command. If using PostgreSQL
-- and the psql client, you can switch to the database with "\c employee". Or you
-- can disconnect and reconnect, specifying the employee database. If using IBM
-- DB2, you can connect with "connect to employee"

-- If you are using the SQLite database engine, it does not support the
-- CREATE DATABASE or USE commands. Instead, each file is itself a
-- database. If the filename does not already exist when you launch
-- sqlite3, it will be created. For instance, the command:
-- sqlite3 employee.sqlite3
-- will create (or attach to an existing) SQLite database.

-- Create a table called employees for the database currently in use.
-- Some of the columns are marked required by specifying NOT NULL.
-- The emp_no column is declared as the primary key.
-- If your database client has difficulty with multiline commands, you
-- may need to join the lines together into one line first.
CREATE TABLE employees (
    emp_no INT NOT NULL,
    birth_date DATE,
    first_name VARCHAR(14) NOT NULL,
    last_name VARCHAR(16) NOT NULL,
    gender CHAR(1), 
    hire_date DATE NOT NULL,
    PRIMARY KEY (emp_no)
);

-- Create a table called title for the database currently in use.
-- Three of the columns: emp_no, title, and from_date are marked
-- NOT NULL, and are also declared as a compound primary key.
-- (All primary key columns are required to uniquely identify a row.)
-- A foreign key constraint defines a connection between emp_no in
-- this table, and the emp_no in the employees table.
CREATE TABLE title (
    emp_no INT NOT NULL,
    title VARCHAR(50) NOT NULL,
    from_date DATE NOT NULL,
    to_date DATE,
    PRIMARY KEY (emp_no, title, from_date),
    FOREIGN KEY (emp_no) REFERENCES employees (emp_no)
);

-- Insert a row of data into the table employees. This assumes that the
-- table has been defined to accept these values as appropriate for it.
-- If using Oracle, you may need to adjust the expected date format with
-- ALTER SESSION SET NLS_DATE_FORMAT = 'YYYY-MM-DD'
INSERT INTO employees (emp_no, birth_date, first_name, last_name, gender, hire_date)
VALUES (10001,'1953-09-02','Georgi','Facello','M','1986-06-26');

-- Insert additional multiple rows of data into the table employees,
-- leaving birth_date and gender NULL. This will not affect the first row
-- that we already inserted.
-- If using Oracle, the multiple insert syntax here does not work. Instead,
-- each row will need to be inserted individually as above.
INSERT INTO employees (emp_no, first_name, last_name, hire_date)
VALUES (10002,'Bezalel','Simmel','1985-11-21'),
       (10003,'Parto','Bamford','1986-08-28'),
       (10004,'Chirstian','Koblick','1986-12-01'),
       (10005,'Kyoichi','Maliniak','1989-09-12'),
       (10006,'Anneke','Preusig','1989-06-02');

-- Insert multiple rows of data into the table title.
-- Notice that each emp_no corresponds to one that we inserted above,
-- supporting the foreign key constraint define above in on the title table.
INSERT INTO title (emp_no, title, from_date, to_date)
VALUES (10001,'Senior Engineer','1986-06-26','9999-01-01'),
       (10002,'Staff','1996-08-03','9999-01-01'),
       (10003,'Senior Engineer','1995-12-03','9999-01-01'),
       (10004,'Engineer','1986-12-01','1995-12-01'),
       (10004,'Senior Engineer','1995-12-01','9999-01-01'),
       (10005,'Senior Staff','1996-09-12','9999-01-01'),
       (10005,'Staff','1989-09-12','1996-09-12'),
       (10006,'Senior Engineer','1990-08-05','9999-01-01');

-- Select all rows and columns from the current database's employees table.
SELECT * FROM employees;

-- Retrieve all rows from the employees table,
-- but only the first_name and last_name columns.
SELECT first_name,
       last_name FROM employees;

-- Same as above, but sorted (case-sensitive) by the last_name values.
SELECT first_name,
       last_name FROM employees ORDER BY last_name;

-- Retrieve all employees columns, but just 4 rows, sorted by hire_date.
SELECT * FROM employees ORDER BY hire_date OFFSET 0 ROWS FETCH NEXT 4 ROWS ONLY;

-- While ORDER BY... OFFSET... FETCH... is ANSI compliant, and supported
-- by database engines such as MariaDB, PostgreSQL, and Microsoft SQL Server,
-- engines such as MySQL and SQLite do not support it. However, such engines
-- often support a LIMIT clause:
SELECT * FROM employees ORDER BY hire_date LIMIT 4;

-- Alternatively, Microsoft SQL Server and Sybase support a TOP keyword.
SELECT TOP 4 * FROM employees ORDER BY hire_date;

-- Retrieve last_name column values from the employees
-- table where the last_name value has the substring 'li'.
SELECT last_name FROM employees WHERE last_name LIKE '%li%';

-- Retrieve all columns from the employees table where the last_name
-- column starts with an 'S' and has exactly 5 characters after it.
SELECT last_name FROM employees WHERE last_name LIKE 'S_____';

-- Select title values from the title table but don't show duplicates.
SELECT DISTINCT title FROM title;

-- Show the number of rows in the employees table.
SELECT COUNT(*) FROM employees;

-- Show the number of rows in the employees table that
-- have 'li' as a substring of the last_name value.
SELECT COUNT(*) FROM employees WHERE last_name LIKE '%li%';

-- A JOIN of information from multiple tables: the titles table shows
-- who had what job titles, by their employee numbers, from what
-- date to what date. Retrieve this information, but instead of the
-- employee number, use the employee number as a cross-reference to
-- the employee table to get each employee's first and last name
-- instead.
SELECT employees.first_name, employees.last_name,
       title.title, title.from_date, title.to_date
FROM title INNER JOIN employees ON
       employees.emp_no = title.emp_no;

-- List all the tables in all the databases. Implementations typically provide
-- their own shortcut command to do this with the database currently in use.
-- (For instance, the below doesn't work in SQLite, Oracle, or DB2, but
-- SQLite supports the ".tables" command, Oracle supports "SELECT * FROM all_tables"
-- and DB2 supports "list tables for all")
SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE='BASE TABLE';

-- In employees, change the first_name value to 'John'
-- for all rows that have a last_name value of 'Facello'.
UPDATE employees SET first_name='John' WHERE last_name='Facello';

-- Delete rows from the title table where the title value begins with
-- 'Senior'.
DELETE FROM title WHERE title like 'Senior%';

-- Delete all rows from the title table, leaving the empty table.
DELETE FROM title;

-- Remove the entire title table.
DROP TABLE title;

-- Remove the entire employee database. Some engines may require you to disconnect
-- or switch to a different database before dropping the current one.
DROP DATABASE employee;
```

## Further Reading

* [Codecademy - SQL](https://www.codecademy.com/learn/learn-sql) A good introduction to SQL in a "learn by doing it" format.
* [Database System Concepts](https://www.db-book.com) book's Chapter 3 - Introduction to SQL has an in depth explanation of SQL concepts.
