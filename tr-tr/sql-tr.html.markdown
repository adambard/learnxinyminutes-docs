---
language: SQL
contributors:
  - ["Metin Yalçınkaya", "https://github.com/mtnylnky"]
lang: tr-tr
filename: learnsql-tr.sql
---


```sql
-- Yorumlar iki tire ile başlar

-- KISITLAR
Not null -- Bir kolon asla boş olamaz
default -- Boş olan yerlere varsayılan bir değer atar
unique -- Bir kolondaki tüm değerlerin farklı olması kısıtlaması
primary key -- Bir tablodaki her veri için kimlik bilgisi niteliğindedir
check -- Bir kolondaki değerlerin belli bir kısıtlamayı sağlamasını sağlar

-- Tablo oluşturulur
CREATE TABLE tablo1 ();

-- Tabloyu içerisinde kolonlar ile oluşturma
CREATE TABLE tablo1(id INTEGER PRIMARY KEY NOT NULL UNIQUE, ad TEXT, soyad TEXT, yas INTEGER);

-- TABLO varlığını kontrol eder
.table

-- Veri tabanında olan bütün tabloları görüntüler.
.schema

-- Satır ekle
INSERT INTO tablo1 ( ad, soyad) VALUES ("Deger1","Deger2");

-- Veritabanında tablo üzerindeki verileri görüntüle 
-- Sadece 'ad' gibi sınırlı bir veri için
SELECT ad FROM tablo1;
-- Bütün veriler için
SELECT * FROM tablo1;

-- Veri güncelleme
UPDATE tablo1 SET ad = "deger1-2"; WHERE name = "Deger1";

-- Satır sil
DELETE FROM tablo1 WHERE id = 1;
DELETE FROM tablo1 WHERE ad = "Deger1" OR ad = "Deger2";

-- Tabloya sonradan kolon ekleme
ALTER TABLE tablo1 ADD COLUMN email TEXT;

-- Tablodaki kolon adı değiştirme
EXEC sp_rename ' tablo1.[ad]', Ad, 'COLUMN';

-- Tablo adı değiştirme
ALTER TABLE table1 RENAME TO Table1;

-- Tabloyu silme
DROP TABLE Table1;

-- BİR TABLOYU BAŞKA TABLO KULLANARAK DOLDURMAK
INSERT INTO Tablo2 SELECT id,ad, soyad, email from Tablo1;

-- LIKE KOMUTU
-- Belirli bir kritere göre arama yaparken kullanılır
-- Adı 'A' ile başlayan veriler
SELECT * FROM tablo1 WHERE adi LIKE "A%";
-- İçinde 'A' olan veriler
SELECT * FROM tablo1 WHERE adi LIKE "%A%";

-- LIMIT KOMUTU
-- Gösterilen satır sayısını sınırlamak için
SELECT * FROM Tablo1 LIMIT 6;
-- Gösterilen satırları belirli bir noktadan başlamak üzere sınırlamak için
SELECT * FROM Tablo1 LIMIT 6 OFFSET 3;

-- ORDER BY KOMUTU
-- Herhangi bir kolona göre gösterilen değerleri azalan veya artan şekilde sıralamak için
SELECT kolon FROM tablo1 WHERE yas ORDER BY column1, column2, .. columnN] [ASC | DESC];
SELECT * FROM Tablo1 ORDER BY yas ASC
SELECT * FROM Tablo1 ORDER BY yas DESC

-- DISTINCT ANAHTAR SÖZCÜĞÜ
-- Bu anahtar sözcükle sadece farklı değerler gösterilir.
SELECT DISTINCT yas FROM tablo1;

-- JOIN KOMUTU
-- CROSS JOIN
-- Cross join bir tablodaki her satırı ikinci tablodaki bir satır ile eşleştirmek için kulanılır.
-- Eğer birinci tabloda x satır ikinci tabloda y satır varsa sonuçta x*y satır olur.
SELECT ... FROM table1 CROSS JOIN table2 …
SELECT ad, yas FROM Tablo1 CROSS JOIN Tablo2;

-- INNER JOIN
-- Inner join iki tablodaki ortak kolon değerlerini kullanarak bir sonuç üretir.
SELECT ... FROM table1 [INNER] JOIN table2 ON conditional_expression …
SELECT ad, yas FROM Tablo1 INNER JOIN Tablo2 ON Tablo1.ad = Tablo2.soyad;

-- OUTER JOIN
-- Outer join iki tablodaki ortak kolon değerlerinin dışında kalanları kullanarak bir sonuç üretir.
SELECT isci_num, isim, dept FROM Tablo1 LEFT OUTER JOIN Tablo2 ON Tablo1.id = Tablo2.isci_num;

-- ÇEKİRDEK FONKSİYONLAR
COUNT -- Sayma
AVG -- Ortalama
ABS -- Mutlak değer
SUM -- Toplam
RANDOM -- Rastgele
ROUND -- Yuvarlama
MAX -- Maksimim
MIN -- Minimum
UPPER -- Büyük Harf
LOWER -- Küçük Harf
LENGTH -- Uzunluk
CURRENT_TIMESTAMP -- Zaman

SELECT max(yas) FROM Table1;
SELECT min(yas) FROM Table1;
SELECT avg(yas) FROM Table1;
SELECT * From Table1 WHERE yas ==18;
SELECT sum(yas) FROM Table1;
SELECT random() AS Random;
SELECT upper(ad) FROM Table1;
SELECT lower(ad) FROM Table1;
SELECT ad, length(ad) FROM Table1;
```