---
language: SQL
filename: learnsql.sql
contributors:
  - ["Bob DuCharme", "http://bobdc.com/"]
translators:
  - ["Shuxin Shu", "https://github.com/NamelessAshone"]
lang: zh-cn
---

结构化查询语言(SQL)是一个ISO标准语言，用于创建和管理数据库，
这种数据库存储一系列表。不同的实现通常会添加特有的语言扩展;
[不同SQL实现的比较(Comparison of different SQL implementat-
ions)](http://troels.arvin.dk/db/rdbms/)是一份很好的产品差
异参考文档。

不同的实现通常会提供一个命令行用于交互式键入命令和显示输出，
同时这些实现也会提供一种执行脚本文件的方法。(如何退出命令行
就是就是SQL中尚未被标准化部分的一个典型例子，绝大多数SQL实
现支持关键字QUIT、EXIT或者两者。)

本文的实例命令假设你已经加载了[github](https://github.com/datacharmer/test_db)上的[MySQL示例员工数据库](https://dev.mysql.com/doc/employee/en/)。
运行脚本的语法取决于你使用的SQL实现。通常是一个命令行工具。

```sql

-- 注释以两个连字符开始。命令以分号结束。

-- SQL关键字大小写不敏感。在下文的示例命令中关键字大写，
-- 因为大写更容易区分数据库、表和列名。

-- 创建和删除一个数据库。数据库名和表名是大小写敏感的。
CREATE DATABASE someDatabase;
DROP DATABASE someDatabase;

-- 列出可用的数据库。
SHOW DATABASES;

-- 使用某个已经存在的数据库
USE employees;

-- 从当前的departments表，选择所有的行和列
-- 解释器的默认行为是将结果打印在屏幕上。
SELECT * FROM departments;

-- 检索departments表中所有的行，但只取dept_no和dept_name列。
-- 一条命令可以跨越多行
SELECT dept_no,
       dept_name FROM departments;

-- 检索departments表中所有的行，但是只输出5行。
SELECT * FROM departments LIMIT 5;

-- 检索departments表中dept_name列包含子串'en'的行。
SELECT dept_name FROM departments WHERE dept_name LIKE '%en%';

-- 检索departmnets表中所有dept_name列值为'S'开头并且'S'后接4个字符的行。
SELECT * FROM departments WHERE dept_name LIKE 'S____';

-- 检索title表中所有行，不显示重复的行。
SELECT DISTINCT title FROM titles;

-- 和上面的查询相同，但是以title的值排序(大小写敏感)。
SELECT DISTINCT title FROM titles ORDER BY title;

-- 计算departments表的总行数。
SELECT COUNT(*) FROM departments;

-- 计算departments表中dept_name列以'en'字段开头的行的数量。
SELECT COUNT(*) FROM departments WHERE dept_name LIKE '%en%';

-- 不同表中信息的JOIN: titles表显示谁有什么工作，员工编号，
-- 入职离职时间。检索这些信息，但是使用员工编号作为employees表
-- 的交叉引用，而不是直接使用员工编号，来获得每个员工的名和姓。
-- (同时只取10行)

SELECT employees.first_name, employees.last_name,
       titles.title, titles.from_date, titles.to_date
FROM titles INNER JOIN employees ON
       employees.emp_no = titles.emp_no LIMIT 10;

-- 列出所有数据库中所有的表。不同实现通常提供各自的快捷命令
-- 来列出当前使用数据库的所有表。
SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE='BASE TABLE';

-- 在当前使用的数据库中，创建一个名为tablename1的表，包含下
-- 述两列。许多其它选项可用于定制列，比如列的数据类型。
CREATE TABLE tablename1 (fname VARCHAR(20), lname VARCHAR(20));

-- 向tablename1表插入一行数据。假设该表已经定义并且接受这些值。
INSERT INTO tablename1 VALUES('Richard','Mutt');

-- 更新tablename1表中lname为'Mutt'的行fname的值改为'John'。
UPDATE tablename1 SET fname='John' WHERE lname='Mutt';

-- 删除tablename1表lname列以'M'开头的行。
DELETE FROM tablename1 WHERE lname like 'M%';

-- 删除tablename1表的所有行，留下空表。
DELETE FROM tablename1;

-- 删除整个tablename1表。
DROP TABLE tablename1;
```
