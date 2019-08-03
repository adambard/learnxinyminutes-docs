---
language: SQL
filename: learnsql.sql
contributors:
  - ["Bob DuCharme", "http://bobdc.com/"]
translators:
  - ["Maoyin Sun", "http://github.com/simonmysun"]
lang: zh-cn
---

结构化查询语言(SQL)是一个用来创建和操作数据库和其中的数据表的 ISO 标准语言.
该语言的诸多实现常常为它添加其特有的扩展.
不同的实现间的区别可以于[SQL 实现的比较(英语)][]查阅.

[SQL 实现的比较(英语)]: http://troels.arvin.dk/db/rdbms/

SQL 的实现通常包含一个命令提示符, 你可以交互地输入后文中的指令,
它们也会提供一个可以执行文件中一系列这些指令的文件的方法.
(结束命令提示符是一个很好的例子来说明部分语法没有被标准化
-- 大部分 SQL 实现支持关键字`QUIT`, `EXIT`, 或者二者都支持. )

部分示例指令假设 [GitHub 上][test_db]的 [MySQL 员工示例数据库][]已经被加载.
GitHub 仓库中的文件有类似后文中的指令的脚本,
可以创建并填入一个虚构的公司职员的数据.
运行它们所需要的 SQL 语法取决于你正在使用的 SQL 实现.
典型的使用方法是从操作系统的命令提示符中运行实用程序.

[test_db]: https://github.com/datacharmer/test_db
[MySQL 员工示例数据库]: https://dev.mysql.com/doc/employee/en/

```sql
-- 注释由两个连字符开始. 语句由分号结束.

-- SQL 的关键字不区分大小写, 这里的示例指令遵循使用大写字母的传统, 原因是这样可
-- 以使它们更容易与数据库, 数据表和列名区分.

-- 创建和删除一个数据库. 数据库名和数据表名区分大小写.
CREATE DATABASE someDatabase;
DROP DATABASE someDatabase;

-- 列出可用数据库.
SHOW DATABASES;

-- 指定一个现存的数据库.
USE employees;

-- 从当前数据库的 `departments` 表中检索所有行和列. 解释器的默认行为是在你的屏幕
-- 上滚动显示出结果.
SELECT * FROM departments;

-- 检索 `departments` 表中的所有行, 但只检索 `dept_no` 列和 `dept_name` 列.
-- 将语句分割为多行是允许的.
SELECT dept_no,
       dept_name FROM departments;

-- 检索所有 `departments` 中的列, 但之只检索 5 行.
SELECT * FROM departments LIMIT 5;

-- 从 `departments` 表中检索 `dept_name` 列中值包含子串 "en" 的行.
SELECT dept_name FROM departments WHERE dept_name LIKE '%en%';

-- 检索 `departements` 表中, 所有的 `dept_name` 的值以 "S" 开始, 之后有且只有 4
-- 个字符的行.
SELECT * FROM departments WHERE dept_name LIKE 'S____';

-- 从 `titles` 表中检索 `title` 列, 但不显示重复的值.
SELECT DISTINCT title FROM titles;

-- 同上, 但以 `title` 列的值 (区分大小写) 排序.
SELECT DISTINCT title FROM titles ORDER BY title;

-- 检索 `departments` 表的总行数.
SELECT COUNT(*) FROM departments;

-- 检索 `departments` 表中 `dept_name` 列的值包含子串 "en" 的总行数.
SELECT COUNT(*) FROM departments WHERE dept_name LIKE '%en%';

-- 多个数据表的信息的连接: `titles` 表以员工编号为索引储存每个人的职称, 入职日期
-- 和离职日期. 我们获取这些信息, 但将员工编号替换为通过以员工编号为参照从
-- `employees` 表中检索得到的员工姓名 (并且限制只检索 10 行).
SELECT employees.first_name, employees.last_name,
       titles.title, titles.from_date, titles.to_date
FROM titles INNER JOIN employees ON
       employees.emp_no = titles.emp_no LIMIT 10;

-- 列出所有数据库中的所有数据表. SQL 的实现通常包含它们自己的简写语句来对当前数
-- 据库进行此操作.
SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE='BASE TABLE';

-- 以 `tablename1` 为名称创建一个包含两个分别名为 `fname` 和 `lname` 的列的表.
-- 声明数据表的列的名称和类型的方法有很多种选择.
CREATE TABLE tablename1 (fname VARCHAR(20), lname VARCHAR(20));

-- 向 `tablename1` 中插入一行数据. 这假设了数据表已被定义为可以接受这些值.
INSERT INTO tablename1 VALUES('Richard','Mutt');

-- 更新数据表 `tablename1` 中所有 `lname` 列的值为 "Mutt" 的行, 将的 `fname` 列
-- 的值更新为 "John".
UPDATE tablename1 SET fname='John' WHERE lname='Mutt';

-- 删除数据表 `tablename1` 中所有 `lname` 列的值以 M 开始的行.
DELETE FROM tablename1 WHERE lname like 'M%';

-- 删除数据表 `tablename1` 中的所有行, 这样做会留下一个空表.
DELETE FROM tablename1;

-- 删除整个 `tablename1` 数据表.
DROP TABLE tablename1;
```
