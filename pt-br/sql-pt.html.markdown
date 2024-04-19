---
language: SQL
filename: learnsql-pt.sql
contributors:
  - ["Bob DuCharme", "http://bobdc.com/"]
translators:
  - ["jovinoguerrero", "https://github.com/jovinoguerrero"]
lang: pt-br
---

A linguagem de consulta estruturada (SQL em inglês) é uma linguagem padrão ISO para criar e trabalhar com bancos de dados armazenados num conjunto de tabelas. As implementações geralmente adicionam suas próprias extensões à linguagem; [Comparação entre diferentes implementações de SQL](http://troels.arvin.dk/db/rdbms/) é uma boa referência sobre as diferenças entre os diferentes produtos.

As implementações normalmente fornecem uma linha de comando onde se pode digitar os comandos mostrados aqui de forma interativa, e também oferecem uma maneira de executar uma série desses comandos armazenados em um arquivo de script (mostrar que é feito com o prompt interativo é um bom exemplo de algo que não é padronizado - a maioria das implementações SQL suporta as palavras-chave QUIT, EXIT ou ambas).

Vários desses comandos de exemplo assumem que o [banco de dados de funcionários de exemplo MySQL](https://dev.mysql.com/doc/employee/en/) disponível em [GitHub](https://github.com/datacharmer/test_db) já foi carregado. Os arquivos do GitHub são scripts de comandos, semelhantes aos comandos abaixo, que criam e carregam tabelas de dados sobre os funcionários de uma empresa fictícia. A sintaxe para executar esses scripts dependerá da implementação SQL que você está usando. Um aplicativo executado a partir do prompt do sistema operacional geralmente é o normal.


```sql
-- Os comentários começam com dois hífens. Cada comando é encerrado com um
-- ponto e vírgula.

-- SQL não diferencia maiúsculas de minúsculas em palavras-chave. Os
-- comandos de exemplo mostrados aqui seguem a convenção de serem escritos
-- em maiúsculas porque torna mais fácil distingui-los dos nomes dos bancos
-- de dados, tabelas e colunas.

-- Em seguida, um banco de dados é criado e excluído. Os nomes do banco de
-- dados e da tabela são sensíveis a maiúsculas de minúsculas.
CREATE DATABASE someDatabase;
DROP DATABASE someDatabase;

-- Mostra numa lista todos os bancos de dados disponíveis.
SHOW DATABASES;

-- Usa um determinado banco de dados existente.
USE employees;

-- Seleciona todas as filas e colunas da tabela de departamentos no banco
-- de dados atual. A atividade padrão é o intérprete rolar os resultados
-- na tela.
SELECT * FROM departments;

-- Recupera todas as filas da tabela de departamentos, mas apenas as colunas
-- dept_no e dept_name.
-- A separação de comandos em várias linhas é permitida.
SELECT dept_no,
       dept_name FROM departments;

-- Obtém todas as colunas de departments, mas é limitado a 5 filas.
SELECT * FROM departments LIMIT 5;

-- Obtém os valores da coluna dept_name da tabela de departments quando
-- dept_name tem como valor a substring 'en'.
SELECT dept_name FROM departments WHERE dept_name LIKE '%en%';

-- Recupera todas as colunas da tabela de departments onde a coluna
-- dept_name começa com um 'S' e tem exatamente 4 caracteres depois dele.
SELECT * FROM departments WHERE dept_name LIKE 'S____';

-- Seleciona os valores dos títulos da tabela de titles, mas não mostra
-- duplicatas.
SELECT DISTINCT title FROM titles;

-- Igual ao anterior, mas ordenado por valores de da coluna title (diferencia
-- maiúsculas de minúsculas).
SELECT DISTINCT title FROM titles ORDER BY title;

-- Mostra o número de filas da tabela departments.
SELECT COUNT(*) FROM departments;

-- Mostra o número de filas da tabela departments que contém 'en' como
-- substring na coluna dept_name.
SELECT COUNT(*) FROM departments WHERE dept_name LIKE '%en%';

-- Uma união (JOIN) de informações de várias tabelas: a tabela titles mostra
-- quem tem quais cargos laborais, de acordo com seu número de funcionários,
-- e desde que data até que data. Esta informação é obtida, mas em vez do
-- número do funcionário é usada como referência cruzada a tabela employee
-- para obter o nome e sobrenome de cada funcionário (e limita os resultados
--  a 10 filas).
SELECT employees.first_name, employees.last_name,
       titles.title, titles.from_date, titles.to_date
FROM titles INNER JOIN employees ON
       employees.emp_no = titles.emp_no LIMIT 10;

-- São listadas todas as tabelas de todas os bancos de dados. As implementações
-- normalmente fornecem seus próprios comandos para fazer isso com o banco de
-- dados atualmente em uso.
SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE='BASE TABLE';

-- Cria uma tabela chamada tablename1, com as duas colunas mostradas, a partir
-- do banco de dados em uso. Há muitas outras opções dísponiveis para a forma
-- em que se especificam as colunas, por ex. seus tipos de dados.
CREATE TABLE tablename1 (fname VARCHAR(20), lname VARCHAR(20));

-- Insere uma fila de dados na tabela tablename1. É assumido que a tabela foi
-- definida para aceitar esses valores como adequados.
INSERT INTO tablename1 VALUES('Richard','Mutt');

-- Em tablename1, se altera o valor de fname para 'John' em todas as filas que
-- contenham um valor em lname igual a 'Mutt'.
UPDATE tablename1 SET fname='John' WHERE lname='Mutt';

-- Se excluem as filas da tabela tablename1 em que o valor de lname começa
-- com 'M'.
DELETE FROM tablename1 WHERE lname like 'M%';

-- Se excluem as filas da tabela tablename1, deixando a tabla vazia.
DELETE FROM tablename1;

-- A tabela tablename1, é excluída completamente.
DROP TABLE tablename1;
```

# Leitura adicional

* [Codecademy - SQL](https://www.codecademy.com/learn/learn-sql) A good introduction to SQL in a “learn by doing it” format.
* [Database System Concepts](https://www.db-book.com/) book’s Chapter 3 - Introduction to SQL has an in depth explanation of SQL concepts.
