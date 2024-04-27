---
language: SQL
filename: learnsql-fr.sql
contributors:
  - ["Bob DuCharme", "http://bobdc.com/"]
translators:
  - ["Th3G33k", "https://github.com/Th3G33k"]
lang: fr-fr
---

Le Structured Query Language (SQL) est un langage standard [ISO/IEC 9075](https://www.iso.org/standard/63555.html) permettant de créer et d'utiliser des bases de données. Les implémentations (MySQL, PostgreSQL, Oracle etc.) ajoutent généralement leurs propres extensions au langage ([Comparaison des différentes implémentations SQL](http://troels.arvin.dk/db/rdbms/)).

Les implémentations fournissent généralement une interface en ligne de commande, où vous pouvez saisir les commandes présentées ici de manière interactive. Elles offrent également un moyen d'exécuter une série de ces commandes stockées dans un fichier script. (Exemple de commande non standardisée, pour quitter l'invite de commande, la plupart des implémentations SQL prennent en charge les mots-clés QUIT, EXIT, ou les deux).

Les exemples de commandes présentés ici utilisent la [base de données d'exemple des employés MySQL](https://dev.mysql.com/doc/employee/en/), disponible sur [GitHub](https://github.com/datacharmer/test_db). Des fichiers scripts sont présents, et permettent de créer et de remplir des tables de données sur les employés d'une entreprise fictive. La syntaxe d'exécution de ces scripts dépendra de l'implémentation SQL que vous utilisez, et du système d'exploitation.

```sql
-- Un commentaire commence par deux traits d'union. Chaque commande se termine par un point-virgule.

/*
Commentaires sur plusieurs lignes.
*/

-- Le langage SQL n'est pas sensible à la casse, pour les mots-clés.
-- Les exemples de commandes présentés ici respectent la convention de nommage
-- (mots-clé en majuscule, nom en minuscule) afin de permettre une meilleure lecture.

-- Crée et supprime une base de données. Les noms des bases de données et des tables sont sensibles à la casse.
CREATE DATABASE someDatabase;
DROP DATABASE someDatabase;

-- Liste les bases de données disponibles.
SHOW DATABASES;

-- Utilise une base de données existante spécifique.
USE employees;

-- Sélectionne toutes les colonnes et lignes de la table departments, de la base de données actuelle.
-- Par défaut, l'interprète SQL fera défiler les résultats sur votre écran.
SELECT * FROM departments;

-- Récupère les colonnes dept_no et dept_name, de toutes les lignes, de la table departments.
-- Une commande peut être sur plusieurs lignes.
SELECT dept_no,
       dept_name FROM departments;

-- Récupère toutes les colonnes, et 5 lignes de la table departments.
SELECT * FROM departments LIMIT 5;

-- Récupère les valeurs de la colonne dept_name de la table departments
-- dont la valeur dept_name contient la chaîne de caractères 'en'.
SELECT dept_name FROM departments WHERE dept_name LIKE '%en%';

-- Récupère toutes les colonnes de la table departments
-- dont la colonne dept_name commence par un 'S' et est suivie d'exactement 4 caractères.
SELECT * FROM departments WHERE dept_name LIKE 'S____';

-- Sélectionne les valeurs uniques title, sans doublons, dans la table titles
SELECT DISTINCT title FROM titles;

-- Identique qu'au-dessus, mais trie (sensible à la casse) selon les valeurs title.
-- L'ordre peut être défini en ajoutant ASC (croissant) ou DESC (décroissant).
-- Si non défini, le tri sera croissant par défaut.
SELECT DISTINCT title FROM titles ORDER BY title;

-- Utilise les opérateurs de comparaison (=, >, <, >=, <=, <>) et
-- les mots-clés conditionnels (AND, OR) pour affiner votre requête.
SELECT * FROM departments WHERE dept_no = 'd001' OR dept_no = 'd002';

-- Identique qu'au-dessus.
SELECT * FROM departments WHERE dept_no IN ('d001', 'd002');

-- Le contraire d'au-dessus.
SELECT * FROM departments WHERE dept_no NOT IN ('d001', 'd002');

-- Sélectionne dans un intervalle donné.
SELECT * from departments WHERE dept_no BETWEEN 'd001' AND 'd002';

-- Indique le nombre de lignes dans la table departments.
SELECT COUNT(*) FROM departments;

-- Indique le nombre de lignes de la table departments
-- qui ont 'en' comme sous-chaîne de la valeur dept_name.
SELECT COUNT(*) FROM departments WHERE dept_name LIKE '%en%';

-- Les fonctions d'agrégation peuvent être utilisées, avec GROUP BY, pour calculer une valeur
-- à partir d'un ensemble de valeur. Les fonctions les plus courantes sont :
-- MIN(), MAX(), COUNT(), SUM(), AVG().
-- Utilise HAVING pour filtrer les lignes en fonction des valeurs agrégées.

-- Récupère le nombre total d'employés, par numéro de département (dept_no),
-- à condition d'avoir plus de 100 employés.
SELECT dept_no, COUNT(dept_no) FROM dept_emp GROUP BY dept_no
HAVING COUNT(dept_no) > 100;

-- Des alias peuvent être utilisés, en utilisant le mot-clé optionnel AS,
-- pour les noms des colonnes et tables.
SELECT COUNT(A.*) AS total_employees, COUNT(B.*) total_departments
FROM employees AS A, departments B;

-- En général, le format de la date est "aaaa-mm-jj".
-- Cependant, cela peut varier en fonction de l'implémentation, du système d'exploitation,
-- et des paramètres locaux de session.
SELECT * FROM dept_manager WHERE from_date >= '1990-01-01';

-- Une jointure (JOIN) permet de réunir des informations de plusieurs tables.
-- La table titles contient l'intitulé de poste (title), le numéro d'employé (emp_no),
-- de quelle date (from_date) à quelle date (to_date).
-- Plutôt que d'afficher le numéro d'employé, nous allons l'utiliser comme référence croisée
-- pour récupérer le nom et le prénom de chaque employé, depuis la table employees.
-- (Affiche seulement 10 lignes)

SELECT employees.first_name, employees.last_name,
       titles.title, titles.from_date, titles.to_date
FROM titles INNER JOIN employees ON
       employees.emp_no = titles.emp_no
LIMIT 10;

-- Combine le résultat de plusieurs SELECT.
-- UNION sélectionne des lignes distinctes, UNION ALL sélectionne toutes les lignes.
SELECT * FROM departments WHERE dept_no = 'd001'
UNION
SELECT * FROM departments WHERE dept_no = 'd002';

-- L'ordre de la syntaxe SQL, avec plusieurs mots-clés appris jusqu'à présent, est :
-- SELECT _ FROM _ JOIN _ ON _ WHERE _ GROUP BY _ HAVING _ ORDER BY _ UNION

-- Liste toutes les tables, de toutes les bases de données. 
-- Les implémentations SQL fournissent généralement leur propre commande de raccourci
-- pour effectuer cette opération avec la base de données actuellement utilisée.
SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE='BASE TABLE';

-- Crée une table appelée tablename1, avec les deux colonnes indiquées, dans la base de données actuellement utilisée.
-- De nombreuses autres options sont disponibles pour spécifier les colonnes, telles que le type de donnée (VARCHAR etc.)
CREATE TABLE tablename1 (fname VARCHAR(20), lname VARCHAR(20));

-- Insère une ligne de données dans la table tablename1.
-- Cela suppose que la table a été défini de manière à accepter les valeurs appropriées.
INSERT INTO tablename1 VALUES('Richard','Mutt');

-- Dans tablename1, remplace la valeur fname par "John"
-- pour toutes les lignes dont la valeur lname est "Mutt".
UPDATE tablename1 SET fname='John' WHERE lname='Mutt';

-- Supprime les lignes de la table tablename1 
-- dont la valeur lname commence par 'M'.
DELETE FROM tablename1 WHERE lname LIKE 'M%';

-- Supprime toutes les lignes de la table tablename1, en laissant la table vide.
DELETE FROM tablename1;

-- Supprime entièrement la table tablename1
DROP TABLE tablename1;
```

## Lectures complémentaires

* [Codecademy - SQL](https://www.codecademy.com/learn/learn-sql) : Une bonne introduction au SQL, avec des exercices pratiques.
* Chapitre 3 du livre [Database System Concepts](https://www.db-book.com) : contient une explication approfondie des concepts SQL.
