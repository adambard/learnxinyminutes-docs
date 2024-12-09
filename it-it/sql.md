---
language: SQL
filename: learnsql-it.sql
contributors:
  - ["Bob DuCharme", "http://bobdc.com/"]
translators:
  - ["Christian Grasso", "https://grasso.io"]
lang: it-it
---

Structured Query Language (SQL) è un linguaggio standard ISO per la creazione e la gestione
di database organizzati in un insieme di tabelle. Le diverse implementazioni aggiungono
spesso le proprie estensioni al linguaggio base ([confronto tra le diverse implementazioni](http://troels.arvin.dk/db/rdbms/))

Le diverse implementazioni forniscono inoltre un prompt per inserire in modo interattivo i comandi
o eseguire il contenuto di uno script.

I comandi di seguito lavorano sul [database di esempio MySQL](https://dev.mysql.com/doc/employee/en/)
disponibile su [GitHub](https://github.com/datacharmer/test_db). I file .sql contengono liste di comandi
simili a quelli mostrati di seguito, che creano e riempiono delle tabelle con dati di un'azienda fittizia.
Il comando per eseguire questi script può variare in base all'implementazione in uso.


```sql
-- I commenti iniziano con due trattini. Ogni comando va terminato con il punto e virgola

-- SQL è case-insensitive per quanto riguarda i comandi; in genere si
-- preferisce scriverli in maiuscolo per distinguerli dai nomi di 
-- database, tabelle e colonne

-- Crea ed elimina un database. I nomi di database e tabelle sono case-sensitive
CREATE DATABASE someDatabase;
DROP DATABASE someDatabase;

-- Lista dei database disponibili
SHOW DATABASES;

-- Attiva uno specifico database
USE employees;

-- Seleziona tutte le righe e le colonne dalla tabella departments
SELECT * FROM departments;

-- Seleziona tutte le righe della tabella departments, 
-- ma solo le colonne dept_no e dept_name. 
-- È possibile suddividere i comandi su più righe.
SELECT dept_no,
       dept_name FROM departments;

-- Seleziona solo le prime 5 righe della tabella departments. 
SELECT * FROM departments LIMIT 5;

-- Ottiene la colonna dept_name della tabella departments
-- solo per le righe il cui valore di dept_name contiene 'en'. 
SELECT dept_name FROM departments WHERE dept_name LIKE '%en%';

-- Ottiene tutte le colonne della tabella departments
-- solo per le righe che hanno un dept_name formato da una 'S'
-- seguita esattamente da altri 4 caratteri
SELECT * FROM departments WHERE dept_name LIKE 'S____';

-- Seleziona i valori di title dalla tabella titles eliminando i duplicati
SELECT DISTINCT title FROM titles;

-- Come sopra, ma i valori sono ordinati alfabeticamente
SELECT DISTINCT title FROM titles ORDER BY title;

-- Mostra il numero di righe della tabella departments
SELECT COUNT(*) FROM departments;

-- Mostra il numero di righe della tabella departments
-- il cui valore di dept_name contiene 'en'.
SELECT COUNT(*) FROM departments WHERE dept_name LIKE '%en%';

-- Un JOIN tra più tabelle: la tabella titles contiene gli 
-- incarichi lavorativi associati ad un certo numero di impiegato.
-- Con il JOIN utilizziamo il numero di impiegato per ottenere
-- le informazioni ad esso associate nella tabella employees.
-- (Inoltre selezioniamo solo le prime 10 righe)

SELECT employees.first_name, employees.last_name,
       titles.title, titles.from_date, titles.to_date
FROM titles INNER JOIN employees ON
       employees.emp_no = titles.emp_no LIMIT 10;

-- Mostra tutte le tabelle di tutti i database.
-- Spesso le implementazioni forniscono degli shortcut per questo comando
SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE='BASE TABLE';

-- Crea una tabella tablename1, con due colonne, per il database in uso.
-- Per le colonne specifichiamo il tipo di dato (stringa di max 20 caratteri)
CREATE TABLE tablename1 (fname VARCHAR(20), lname VARCHAR(20));

-- Inserisce una riga nella tabella tablename1. I valori devono essere
-- appropriati per la definizione della tabella
INSERT INTO tablename1 VALUES('Richard','Mutt');

-- In tablename1, modifica il valore di fname a 'John'
-- in tutte le righe che hanno come lname 'Mutt'. 
UPDATE tablename1 SET fname='John' WHERE lname='Mutt';

-- Elimina tutte le righe di tablename1
-- il cui lname inizia per 'M'.
DELETE FROM tablename1 WHERE lname like 'M%';

-- Elimina tutte le righe della tabella tablename1
DELETE FROM tablename1;

-- Elimina la tabella tablename1
DROP TABLE tablename1;
```
