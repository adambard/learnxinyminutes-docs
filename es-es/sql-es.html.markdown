---
language: SQL
filename: learnsql-es.sql
contributors:
  - ["Bob DuCharme", "http://bobdc.com/"]
translators:
  - ["FedeHC", "https://github.com/FedeHC"]
lang: es-es
---

El lenguaje de consulta estructurada (SQL en inglés) es un lenguaje estándar ISO para crear y trabajar con bases de datos almacenados en un conjunto de tablas. Las implementaciones generalmente añaden sus propias extensiones al lenguaje; [Comparación entre diferentes implementaciones de SQL](http://troels.arvin.dk/db/rdbms/) es una buena referencia sobre las diferencias entre distintos productos.

Las implementaciones típicamente proveen de una línea de comandos donde uno puede introducir los comandos que se muestran aquí en forma interactiva, y también ofrecen una forma de ejecutar una serie de estos comandos almacenados en un archivo de script (mostrar que uno ha terminado con el prompt interactivo es un buen ejemplo de algo que no está estandarizado - la mayoría de las implementaciones de SQL soportan las palabras clave QUIT, EXIT, o ambas).

Varios de estos comandos que sirven de ejemplo asumen que la [base de datos de empleados de muestra de MySQL](https://dev.mysql.com/doc/employee/en/) disponible en [github](https://github.com/datacharmer/test_db) ya ha sido cargada. Los archivos github son scripts de comandos, similares a los comandos que aparecen a continuación, que crean y cargan tablas de datos sobre los empleados de una empresa ficticia. La sintaxis para ejecutar estos scripts dependerá de la implementación de SQL que esté utilizando. Una aplicación que se ejecuta desde el prompt del sistema operativo suele ser lo habitual.


```sql
-- Los comentarios empiezan con dos guiones. Se termina cada comando con punto
-- y coma.

-- SQL no distingue entre mayúsculas y minúsculas en palabras clave. Los
-- comandos de ejemplo que aquí se muestran siguen la convención de ser escritos
-- en mayúsculas porque hace más fácil distinguirlos de los nombres de las bases
-- de datos, de las tablas y de las columnas.

-- A cont. se crea y se elimina una base de datos. Los nombres de la base de
-- datos y de la tabla son sensibles a mayúsculas y minúsculas.
CREATE DATABASE someDatabase;
DROP DATABASE someDatabase;

-- Lista todas las bases de datos disponibles.
SHOW DATABASES;

-- Usa una base de datos existente en particular.
USE employees;

-- Selecciona todas las filas y las columnas de la tabla departments en la base
-- de datos actual. La actividad predeterminada es que el intérprete desplace
-- los resultados por la pantalla.
SELECT * FROM departments;

-- Recupera todas las filas de la tabla departments, pero sólo las columnas
-- dept_no y dept_name.
-- Separar los comandos en varias líneas está permitido.
SELECT dept_no,
       dept_name FROM departments;

-- Obtiene todas las columnas de departments, pero se limita a 5 filas.
SELECT * FROM departments LIMIT 5;

-- Obtiene los valores de la columna dept_name desde la tabla departments cuando
-- dept_name tiene como valor la subcadena 'en'.
SELECT dept_name FROM departments WHERE dept_name LIKE '%en%';

-- Recuperar todas las columnas de la tabla departments donde la columna
-- dept_name comienza con una 'S' y tiene exactamente 4 caracteres después
-- de ella.
SELECT * FROM departments WHERE dept_name LIKE 'S____';

-- Selecciona los valores de los títulos de la tabla titles, pero no muestra
-- duplicados.
SELECT DISTINCT title FROM titles;

-- Igual que el anterior, pero ordenado por los valores de title (se distingue
-- entre mayúsculas y minúsculas).
SELECT DISTINCT title FROM titles ORDER BY title;

-- Muestra el número de filas de la tabla departments.
SELECT COUNT(*) FROM departments;

-- Muestra el número de filas en la tabla departments que contiene 'en' como
-- subcadena en la columna dept_name.
SELECT COUNT(*) FROM departments WHERE dept_name LIKE '%en%';

-- Una unión (JOIN) de información desde varias tablas: la tabla titles muestra
-- quién tiene qué títulos de trabajo, según sus números de empleados, y desde
-- qué fecha hasta qué fecha. Se obtiene esta información, pero en lugar del
-- número de empleado se utiliza el mismo como una referencia cruzada a la
-- tabla employee para obtener el nombre y apellido de cada empleado (y se
-- limita los resultados a 10 filas).
SELECT employees.first_name, employees.last_name,
       titles.title, titles.from_date, titles.to_date
FROM titles INNER JOIN employees ON
       employees.emp_no = titles.emp_no LIMIT 10;

-- Se enumera todas las tablas de todas las bases de datos. Las implementaciones
-- típicamente proveen sus propios comandos para hacer esto con la base de datos
-- actualmente en uso.
SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE='BASE TABLE';

-- Crear una tabla llamada tablename1, con las dos columnas mostradas, a partir
-- de la base de datos en uso. Hay muchas otras opciones disponibles para la
-- forma en que se especifican las columnas, como por ej. sus tipos de datos.
CREATE TABLE tablename1 (fname VARCHAR(20), lname VARCHAR(20));

-- Insertar una fila de datos en la tabla tablename1. Se asume que la tabla ha
-- sido definida para aceptar estos valores como aptos.
INSERT INTO tablename1 VALUES('Richard','Mutt');

-- En tablename1, se cambia el valor de fname a 'John' para todas las filas que
-- tengan un valor en lname igual a 'Mutt'.
UPDATE tablename1 SET fname='John' WHERE lname='Mutt';

-- Se borra las filas de la tabla tablename1 donde el valor de lname comience
-- con 'M'.
DELETE FROM tablename1 WHERE lname like 'M%';

-- Se borra todas las filas de la tabla tablename1, dejando la tabla vacía.
DELETE FROM tablename1;

-- Se elimina toda la tabla tablename1 por completo.
DROP TABLE tablename1;
```
