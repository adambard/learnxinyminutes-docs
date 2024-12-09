---
language: SQL
filename: learnsql-de.sql
contributors:
  - ["Bob DuCharme", "http://bobdc.com/"]
translators:
    - ["denniskeller", "https://github.com/denniskeller"]
lang: de-de
---

Die Structured Query Language (SQL) ist eine ISO Standardsprache zum Erstellen und Arbeiten mit Datenbanken, die in einem Set von Tabellen gespeichert sind. Implementierungen fügen in der Regel eigene Erweiterungen zur Sprache hinzu; [Der Vergleich von verschiedenen SQL Implementierungen](http://troels.arvin.dk/db/rdbms/) ist eine gute Referenz für Produktunterschiede.

Implementierungen bieten typischerweise eine Eingabeaufforderung, in den du die hier gezeigten Befehle interaktiv eingeben kannst. Sie bieten auch einen Weg, um Serien von Befehlen in einer Skript auszuführen. (Die Anzeige, dass du fertig mit der interaktiven Eingabeaufforderung bist, ist ein gutes Beispiel für etwas, was nicht standardisiert ist. Die meisten SQL Implementierungen unterstützen die Schlüsselwörter QUIT, EXIT oder beides.
Einige dieser Beispielbefehle gehen davon aus, dass sie die [MySQL employee sample database](https://dev.mysql.com/doc/employee/en/), verfügbar auf [GitHub](https://github.com/datacharmer/test_db), schon geladen wurde. Die GitHub Dateien sind Skripte von Befehlen, ähnlich wie die entsprechenden Befehle unten, die Tabellen mit Daten über die Mitarbeiter einer fiktiven Firma erstellen und füllen. Die Syntax für die Ausführung dieser Skripte hängt von der verwendeten SQL-Implementierung ab. Ein Dienstprogramm, das man über die Betriebssystemeingabeaufforderung ausführen kann, ist typisch.


```sql
-- Kommentare starten mit zwei Bindestrichen. Jeder Befehl endet mit einem Semikolon.

-- SQL unterscheidet nicht zwischen Groß- und Kleinschreibung bei
-- Schlüsselwörtern. Die Beispielbefehle folgen der Konvention der
-- Schreibweise in Großbuchstaben, damit sie leichter von Datenbank-,
-- Tabellen- und Spaltennamen zu unterscheiden sind.

-- Erstellen und Löschen einer Datenbank. Bei Datenbank- und Tabellennamen
-- wird zwischen Groß- und Kleinschreibung unterschieden.
CREATE DATABASE someDatabase;
DROP DATABASE someDatabase;

-- Liste verfügbare Datenbanken.
SHOW DATABASES;

-- Verwende eine bestimmte Datenbank.
USE employees;

-- Wähle alle Zeilen und Spalten aus der Tabelle departmens aus der aktuellen
-- Datenbank aus.
-- Das Standardverhalten für den Interpreter ist die Ergebnisse auf
-- dem Bildschirm zu scrollen.
SELECT * FROM departments;

-- Hole dir alle Zeilen aus der departments Tabelle,
-- aber nur die dept_no und die dept_name Spalten.
-- Das Aufteilen von Befehlen auf mehrere Zeilen ist in Ordnung.
SELECT dept_no,
       dept_name FROM departments;

-- Hole dir alle departments Spalten, aber nur 5 Zeilen.
SELECT * FROM departments LIMIT 5;

-- Hole dir die dept_name Spaltenwerte aus der departments Tabelle,
-- in der der Wert dept_name die Teilzeichenfolge 'en' hat.
SELECT dept_name FROM departments WHERE dept_name LIKE '%en%';

-- Hole dir alle Spalten von der departments Tabelle, in der die dept_name
-- Spalte mit einem 'S' beginnt und exakt 4 Zeichen danach besitzt.
SELECT * FROM departments WHERE dept_name LIKE 'S____';

-- Wähle die Titelwerte aus der Titeltabelle, aber zeige keine Duplikate an.
SELECT DISTINCT title FROM titles;

-- Das Gleiche wie oben, aber sortiert nach den Titelwerten, mit Beachtung
-- der Groß und Kleinschreibung.
SELECT DISTINCT title FROM titles ORDER BY title;

-- Zeige die Anzahl der Zeilen in der departments Tabelle an.
SELECT COUNT(*) FROM departments;

-- Zeige die Anzahl der Zeilen in der departments Tabelle an, die 'en' als
-- Teilezeichenkette des Wertes dept_name haben.
SELECT COUNT(*) FROM departments WHERE dept_name LIKE '%en%';

-- Eine Vereinigung von Informationen von mehreren Tabellen:
-- Die titles Tabelle zeigt, wer welche Jobtitel hatte, wer welche Mitarbeiter-
-- nummer hat, von welchen Startdatum und zu welchen Enddatum
-- Wir rufen diese Information ab, aber anstelle der Mitarbeiternummer,
-- verwenden wir die Mitarbeiternummer als Querverweis auf die employees Tabelle
-- um die die Vor- und Nachnamen jedes Mitarbeiters zu erhalten.
-- (und nur 10 Reihen)
SELECT employees.first_name, employees.last_name,
       titles.title, titles.from_date, titles.to_date
FROM titles INNER JOIN employees ON
       employees.emp_no = titles.emp_no LIMIT 10;

-- Liste alle Tabellen in allen Datenbanken auf. Verschiedene Implementierungen
-- stellen typischerweise einen eigenen Abkürzungsbefehl zur Verfügung für
-- die aktuell verwendete Datenbank.
SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE='BASE TABLE';

-- Erstelle eine Tabelle in der aktuell verwendeten Datenbank
-- mit dem Namen tablename1, in der die beiden Spalten angezeigt werden
-- Es gibt viele weiteren Optionen, wie man die Spalten spezifizieren kann,
-- wie z.B. deren Datentyp.
CREATE TABLE tablename1 (fname VARCHAR(20), lname VARCHAR(20));

-- Füge eine Zeile mit Daten in die Tabelle tablename1. Dies setzt voraus,
-- das die Tabelle so definiert worden ist, dass sie die geeigneten
-- Werte akzeptiert.
INSERT INTO tablename1 VALUES('Richard','Mutt');

-- Verändere den fname Wert zu 'John' für alle Zeilen,
-- die einen lname Wert von 'Mutt' haben.
UPDATE tablename1 SET fname='John' WHERE lname='Mutt';

-- Lösche Zeilen aus der tablename1 Tabelle,
-- deren lname Wert mit dem Wert 'M' beginnen.
DELETE FROM tablename1 WHERE lname like 'M%';

-- Lösche alle Zeilen von der tablename1 Tabelle, hinterlasse nur eine leere
-- Tabelle.
DELETE FROM tablename1;

-- Lösche die gesamte tablename1 Tabelle.
DROP TABLE tablename1;
```
