---
language: SQL
filename: learnsql.sql
contributors:
  - ["Bob DuCharme", "http://bobdc.com/"]
translators:
    - ["Ahmed Omar Eissa", "https://twitter.com/AhmedOmarEissa"]
lang: ar-ar
---
<div dir="rtl">

لغة الاستعلام الهيكلية 
(SQL) 
هي لغة قياسية
[ISO/IEC 9075](https://www.iso.org/standard/63555.html)
لإنشاء قواعد البيانات المخزنة في مجموعة من الجداول التعامل معها. عادةً ما تضيف التطبيقات 
امتدادات خاصة بها إلى اللغة ؛ تعد 
[مقارنة نسخ SQL المختلفة](http://troels.arvin.dk/db/rdbms/) 
مرجعًا جيدًا لاختلافات النسخ.

توفر النسخ عادةً موجه سطر أوامر
command line prompt
حيث يمكنك إدخال الأوامر المعروضة هنا بشكل تفاعلي، كما أنها توفر طريقة لتنفيذ سلسلة من هذه الأوامر المخزنة في ملف نصي. إظهار رسالة الانتهاء من العمل مع الموجه التفاعلي مثال جيد على امكانية اضافة أوامر غير قياسية، معظم النسخ تدعم أحد أوامر
QUIT , EXIT
أو كليهما

العديد من هده الأوامر تعتمد أن قاعدة بيانات الموظفين
[MySQL employee sample database](https://dev.mysql.com/doc/employee/en/) 
الموجودة على
[github](https://github.com/datacharmer/test_db)
قد تم تحليمها، 

الملفات على
github 
هي مجموعة من الاوامر تشبه الموجودة بالاسفل و تقوم الأوامر بإنشاء الجدوال وإدخال بيانات مجموعة من الموظفين المتخيلين في شركة. تعتمد الأوامر المستخدمة في هذا البرنامج على نسخة 
SQL 
التي تستخدمها، 
</div>




```sql
-- تبدأ التعليقات بشرطتين. قم بإنهاء كل أمر بفاصلة منقوطة

--  لا تعتبر اللغة حساسة لحالة الاحرف والاوامر الموجودة هنا
-- فقط ليسهل تمييزها عن أسماه الأعمدة والجداول وقواعد البيانات UPPER-CASE تستخدم الحالة العليا للاحرف 

-- Create and delete a database. Database and table names are case-sensitive.
-- إنشاء ومسح قاعدة بيانات، أسماء قواعد البيانات والجداول حساسة لحالة الأحرف

CREATE DATABASE someDatabase;
DROP DATABASE someDatabase;

-- List available databases.
-- عرض قواعد البيانات الموجودة

SHOW DATABASES;

-- Use a particular existing database.
--استخدام قاعدة بيانات محددة

USE employees;

-- Select all rows and columns from the current database's departments table.
-- في قاعدة البيانات المستخدمة departments ارجاع كل السطور والاعمدة في جدول 
-- Default activity is for the interpreter to scroll the results on your screen.
-- ستظهر النتائج على الشاشة بشكل تلقائي لتتصفحها.


SELECT * FROM departments;

-- Retrieve all rows from the departments table,
-- but only the dept_no and dept_name columns.
-- Splitting up commands across lines is OK.

-- فقط dept_name و dept_no لكن سنسترجع عمودي  departments استرجاع كل أسطر من جدول 
-- لا مانع من تقسيم الاوامر بين السطور


SELECT dept_no,
       dept_name FROM departments;

-- Retrieve all departments columns, but just 5 rows.
-- لكن هذه المرة سنسترجع ٥ أسطر فقط departments استرجاع كل الاعمدة من جدول


SELECT * FROM departments LIMIT 5;

-- Retrieve dept_name column values from the departments
-- table where the dept_name value has the substring 'en'.
--en يحتوي علي dept_name في حالة أن عمود  departments من جدول  dept_name استرجاع عمود 

SELECT dept_name FROM departments WHERE dept_name LIKE '%en%';

-- Retrieve all columns from the departments table where the dept_name
-- column starts with an 'S' and has exactly 4 characters after it.
-- S استرجاع كل أعمدة جدول الاقسام في حالة أن اسم القسم يبدأ بحرف  
-- متبوعا بأربعة حروف 


SELECT * FROM departments WHERE dept_name LIKE 'S____';

-- Select title values from the titles table but don't show duplicates.
-- استرجاع قيم العناوين من جدول العناوين بدون تكرار 

SELECT DISTINCT title FROM titles;

-- Same as above, but sorted (case-sensitive) by the title values.
-- نفس المثال السابق مع ترتيب العناوين أبجديا 

SELECT DISTINCT title FROM titles ORDER BY title;

-- Show the number of rows in the departments table.
-- اظهار عدد السطور في جدول الأقسام 

SELECT COUNT(*) FROM departments;

-- Show the number of rows in the departments table that
-- have 'en' as a substring of the dept_name value.

-- en اظهار عدد السطور في جدول الأقسام التي تحتوي في عمود اسم القسم علي 

SELECT COUNT(*) FROM departments WHERE dept_name LIKE '%en%';

-- A JOIN of information from multiple tables: the titles table shows
-- who had what job titles, by their employee numbers, from what
-- date to what date. Retrieve this information, but instead of the
-- employee number, use the employee number as a cross-reference to
-- the employees table to get each employee's first and last name
-- instead. (And only get 10 rows.)

-- ربط المعلومات بين الجداول، جدول العناوين يظهر رقم كل موظف ومسماه الوظيفي 
-- ومتي حصل على هذا المسمى وإلي متى ولكن بدلا من اظهار رقم الموظف سنستخدم هذا الرقم
-- للحصول على اسم الموظف الاول والأخير من جدول الموظفين مع إظهار ١٠ سطور فقط

SELECT employees.first_name, employees.last_name,
       titles.title, titles.from_date, titles.to_date
FROM titles INNER JOIN employees ON
       employees.emp_no = titles.emp_no LIMIT 10;

-- List all the tables in all the databases. Implementations typically provide
-- their own shortcut command to do this with the database currently in use.
-- إظهار كل الجدوال في كل قواعد البيانات 
-- النسخ المختلفة تقدم اختصارات لمثل هذا الأمر لقاعدة البيانات المستخدمة

SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE='BASE TABLE';

-- Create a table called tablename1, with the two columns shown, for
-- the database currently in use. Lots of other options are available
-- for how you specify the columns, such as their datatypes.
-- يحتوى على عمودان في قاعدة البيانات المستخدمة tablename1 أنشاء جدول يسمى  
-- يوجد العديد من الطرق لتعريف الاعمدة وأنواع البيانات في العمود

CREATE TABLE tablename1 (fname VARCHAR(20), lname VARCHAR(20));

-- Insert a row of data into the table tablename1. This assumes that the
-- table has been defined to accept these values as appropriate for it.


INSERT INTO tablename1 VALUES('Richard','Mutt');

-- In tablename1, change the fname value to 'John'
-- for all rows that have an lname value of 'Mutt'.

--John إلى fname سنغير قيمة عمود tablename1  في 
-- Mutt هي lname  في حالة أن قيمة العمود

UPDATE tablename1 SET fname='John' WHERE lname='Mutt';

-- Delete rows from the tablename1 table
-- where the lname value begins with 'M'.
-- 'M' تبدأ ب  lname في حالة أن قيمة عمود  tablename1 مسح السطور من جدول 

DELETE FROM tablename1 WHERE lname like 'M%';

-- Delete all rows from the tablename1 table, leaving the empty table.
-- مع ترك الجدول فارغ tablename1 مسح جميع السطور من جدول 

DELETE FROM tablename1;

-- Remove the entire tablename1 table.
-- تماما tablename1 إزالة جدول 

DROP TABLE tablename1;
```
<div dir="rtl">

## اقرأ أكثر

* [Codecademy - SQL](https://www.codecademy.com/learn/learn-sql)مقدمة جيدة للتعلم عن طريق التطبيق.
* [Database System Concepts](https://www.db-book.com) الفصل رقم ٣ من الكتاب مقدمة في (SQL) تحتوى علي شرح مفصل لمفاهيم (SQL)

</div>
