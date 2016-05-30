---
language: xml
filename: learnxml-ms.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
    - ["hack1m", "https://github.com/hack1m"]
lang: ms-my   
---

XML adalah bahasa markup direka untuk menyimpan dan mengangkutan data.

Tidak seperti HTML, XML tidak menyatakan bagaimana paparan atau mengformat data, hanya membawanya.

* Sintaks XML

```xml
<!-- Komen di XML seperti ini -->

<?xml version="1.0" encoding="UTF-8"?>
<bookstore>
  <book category="COOKING">
    <title lang="en">Everyday Italian</title>
    <author>Giada De Laurentiis</author>
    <year>2005</year>
    <price>30.00</price>
  </book>
  <book category="CHILDREN">
    <title lang="en">Harry Potter</title>
    <author>J K. Rowling</author>
    <year>2005</year>
    <price>29.99</price>
  </book>
  <book category="WEB">
    <title lang="en">Learning XML</title>
    <author>Erik T. Ray</author>
    <year>2003</year>
    <price>39.95</price>
  </book>
</bookstore>

<!-- Di atas adalah fail XML biasa.
  Ia bermula dengan perisytiharan, memaklumkan beberapa metadata (pilihan).

  XML menggunakan struktur pokok, Di atas, nod akar ialah ‘bookstore’, yang mana mempunyai tiga nod anak, semua ‘books’. Nod itu mempunyai lebih nod anak (atau anak-anak), dan seterusnya…

  Nod dibuat menggunakan tag pembuka/penutup, dan anak-anak hanya nod antara
  pembuka dan penutup tag.-->


  <!-- XML membawa dua jenis data:
   1 - Atribut -> Iaitu metadata mengenai nod.
       Biasanya, penghurai XML menggunakan informasi untuk menyimpan data dengan betul.
       Ia mempunyai ciri-ciri yang dipaparkan bersama format name=“value” dalam tag
       pembuka.

   2 - Elemen -> Iaitu data tulen.
       Iaitu apa penghurai akan menerima daripada fail XML.
       Elemen memaparkan diantara pembuka dan penutup tag. —>


<!-- Di bawah, elemen dengan dua atribut -->
<file type="gif" id="4293">computer.gif</file>


```

* Dokumen Format sempurna x Pengesahan

Satu dokumen XML adalah format sempurna jika ia adalah sintaksis yang betul.
Walau bagaimanapun, ia mungkin menyuntik lebih banyak kekangan dalam dokumen itu,
menggunakan definasi dokumen, seperti DTD dan Skema XML.

Satu dokumen XML yang mana mengikut definasi dokumen dipanggil sah,
mengenai dokumen itu.

Dengan alat ini, anda boleh menyemak data XML di luar logik aplikasi.

```xml

<!-- Dibawah, anda boleh melihat versi ringkas daripada dokumen bookstore,
  dengan tambahan definisi DTD. -->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "Bookstore.dtd">
<bookstore>
  <book category="COOKING">
    <title >Everyday Italian</title>
    <price>30.00</price>
  </book>
</bookstore>

<!-- DTD boleh menjadi sesuatu seperti ini: -->

<!DOCTYPE note
[
<!ELEMENT bookstore (book+)>
<!ELEMENT book (title,price)>
<!ATTLIST book category CDATA "Literature">
<!ELEMENT title (#PCDATA)>
<!ELEMENT price (#PCDATA)>
]>


<!-- DTD bermula dengan pengisytiharan.
  Berikut, nod akar diisytihar, memerlukan 1 atau lebih nod anak ‘book’.
  Setiap ‘book’ harus mengandungi betul-betul satu ‘title’ dan ‘price’ dan atribut
  dipanggil ‘category’, bersama “Literature" sebagai nilai lalai ia.
  Nod ‘title’ dan ‘price’  mengandungi aksara data terhurai.-—>

<!-- DTD boleh diisytiharkan di dalam fail XML itu sendiri. -->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE note
[
<!ELEMENT bookstore (book+)>
<!ELEMENT book (title,price)>
<!ATTLIST book category CDATA "Literature">
<!ELEMENT title (#PCDATA)>
<!ELEMENT price (#PCDATA)>
]>

<bookstore>
  <book category="COOKING">
    <title >Everyday Italian</title>
    <price>30.00</price>
  </book>
</bookstore>
```
