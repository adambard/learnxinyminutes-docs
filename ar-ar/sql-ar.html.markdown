---
language: SQL
filename: learnsql-ar.sql
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
.أو كليهما

في الامثلة بالأسفل تعتمدالعديد من الأوامرأن قاعدة بيانات الموظفين
[MySQL employee sample database](https://dev.mysql.com/doc/employee/en/) 
الموجودة على
[GitHub](https://github.com/datacharmer/test_db)
قد تم تحميلها مسبقا. الملفات على
GitHub 
هي مجموعة من الاوامر تشبه الموجودة بالاسفل و تقوم الأوامر بإنشاء الجدوال وإدخال بيانات مجموعة من الموظفين المتخيلين في شركة. تعتمد الأوامر المستخدمة في هذا البرنامج على نسخة 
SQL 
التي تستخدمها، 
</div>




```sql
-- تبدأ التعليقات بشرطتين. قم بإنهاء كل أمر بفاصلة منقوطة

-- حساسة لحالة الاحرف SQL لا تعتبر
-- فقط ليسهل تمييزها عن أسماه الأعمدة والجداول وقواعد البيانات UPPER-CASE  الاوامر الموجودة هنا تستخدم الحالة العليا للاحرف 

-- إنشاء ومسح قاعدة بيانات، أسماء قواعد البيانات والجداول حساسة لحالة الأحرف
CREATE DATABASE someDatabase;
DROP DATABASE someDatabase;

-- عرض قواعد البيانات الموجودة
SHOW DATABASES;

--استخدام قاعدة بيانات محددة
USE employees;

-- في قاعدة البيانات المستخدمة departments ارجاع كل السطور والاعمدة في جدول 
-- ستظهر النتائج على الشاشة بشكل تلقائي لتتصفحها.
SELECT * FROM departments;

-- فقط dept_name و dept_no لكن سنسترجع عمودي departments استرجاع كل أسطر من جدول 
-- لا مانع من تقسيم الاوامر بين السطور

SELECT dept_no,
       dept_name FROM departments;

-- لكن هذه المرة سنسترجع ٥ أسطر فقط departments استرجاع كل الاعمدة من جدول
SELECT * FROM departments LIMIT 5;

--en يحتوي علي dept_name في حالة أن عمود  departments من جدول  dept_name استرجاع عمود 

SELECT dept_name FROM departments WHERE dept_name LIKE '%en%';

-- S استرجاع كل أعمدة جدول الاقسام في حالة أن اسم القسم يبدأ بحرف  
-- متبوعا بأربعة حروف 

SELECT * FROM departments WHERE dept_name LIKE 'S____';

-- استرجاع قيم العناوين من جدول العناوين بدون تكرار 
SELECT DISTINCT title FROM titles;

-- نفس المثال السابق مع ترتيب العناوين أبجديا 
SELECT DISTINCT title FROM titles ORDER BY title;

-- اظهار عدد السطور في جدول الأقسام 
SELECT COUNT(*) FROM departments;


-- en اظهار عدد السطور في جدول الأقسام التي تحتوي في عمود اسم القسم علي 
SELECT COUNT(*) FROM departments WHERE dept_name LIKE '%en%';


-- ربط المعلومات بين الجداول، جدول العناوين يظهر رقم كل موظف ومسماه الوظيفي 
-- ومتي حصل على هذا المسمى وإلي متى ولكن بدلا من اظهار رقم الموظف سنستخدم هذا الرقم
-- للحصول على اسم الموظف الاول والأخير من جدول الموظفين مع إظهار ١٠ سطور فقط
SELECT employees.first_name, employees.last_name,
       titles.title, titles.from_date, titles.to_date
FROM titles INNER JOIN employees ON
       employees.emp_no = titles.emp_no LIMIT 10;

-- إظهار كل الجدوال في كل قواعد البيانات 
-- النسخ المختلفة تقدم اختصارات لمثل هذا الأمر لقاعدة البيانات المستخدمة

SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE='BASE TABLE';

-- يحتوى على عمودان في قاعدة البيانات المستخدمة tablename1 أنشاء جدول يسمى  
-- يوجد العديد من الطرق لتعريف الاعمدة وأنواع البيانات في العمود
CREATE TABLE tablename1 (fname VARCHAR(20), lname VARCHAR(20));

--  هذا بافتراض ان الجدول يمكن اضافة الاسطر له .tablename1 اضافة سطر في جدول 
INSERT INTO tablename1 VALUES('Richard','Mutt');

--John إلى fname سنغير قيمة عمود tablename1  في 
-- Mutt هي lname  في حالة أن قيمة العمود
UPDATE tablename1 SET fname='John' WHERE lname='Mutt';


-- 'M' تبدأ ب  lname في حالة أن قيمة عمود  tablename1 مسح السطور من جدول 
DELETE FROM tablename1 WHERE lname like 'M%';

-- مع ترك الجدول فارغ tablename1 مسح جميع السطور من جدول 
DELETE FROM tablename1;

-- تماما tablename1 إزالة جدول 
DROP TABLE tablename1;
```

<div dir="rtl">

## اقرأ أكثر

* [Codecademy - SQL](https://www.codecademy.com/learn/learn-sql)مقدمة جيدة للتعلم عن طريق التطبيق.
* [Database System Concepts](https://www.db-book.com) الفصل رقم ٣ من الكتاب مقدمة في (SQL) تحتوى علي شرح مفصل لمفاهيم (SQL)

</div>
