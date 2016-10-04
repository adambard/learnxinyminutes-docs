---
language: sql
contributors:
    - ["Abraham Toriz", "https://github.com/categulario"]
filename: learnsql.sql
---

```sql
-- Create a schema/database
CREATE SCHEMA company;

-- Create a table with a primary key
CREATE TABLE department (
-- a column that cannot be null
    id INT NOT NULL,
    name VARCHAR(50),

    PRIMARY KEY (id)
);

-- Create a table with an unique key
-- and a foreign key to the previously created table
CREATE TABLE employee (
    id INT NOT NULL,
    name VARCHAR(100) NOT NULL,
    age INT,
    bird_date DATE,

-- variable-lenght strings
    email VARCHAR(100),
    department_id INT,

-- specify a default value
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

### Books

* [Fundamentals of Database Systems](https://www.amazon.com/Fundamentals-Database-Systems-Ramez-Elmasri/dp/0136086209?SubscriptionId=AKIAILSHYYTFIVPWUY6Q&tag=duckduckgo-ffab-20&linkCode=xm2&camp=2025&creative=165953&creativeASIN=0136086209)
