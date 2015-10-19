---
language: xml
filename: learnxml.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
  - ["Rizky Luthfianto", "https://github.com/rilut"]
lang: id-id
---

XML adalah bahasa markup yang dirancang untuk menyimpan dan mengirim data.

Tidak seperti HTML, XML tidak menentukan bagaimana menampilkan atau format data, hanya membawanya.

* Sintaks XML

```xml
<!-- Komentar di XML seperti ini -->

<?xml version="1.0" encoding="UTF-8"?>
<tokobuku>
  <buku category="MEMASAK">
    <judul lang="en">Everyday Italian</judul>
    <pengarang>Giada De Laurentiis</pengarang>
    <tahun>2005</tahun>
    <harga>30.00</harga>
  </buku>
  <buku category="ANAK">
    <judul lang="en">Harry Potter</judul>
    <pengarang>J K. Rowling</pengarang>
    <tahun>2005</tahun>
    <harga>29.99</harga>
  </buku>
  <buku category="WEB">
    <judul lang="en">Learning XML</judul>
    <pengarang>Erik T. Ray</pengarang>
    <tahun>2003</tahun>
    <harga>39.95</harga>
  </buku>
</tokobuku>

<!-- Di atas adalah contoh file XML biasa.
   Dimulai dengan deklarasi, menginformasikan beberapa metadata (opsional).
  
   XML menggunakan struktur pohon. Di atas, simpul akar adalah 'tokobuku',
   yang memiliki tiga node anak, para 'buku'. Node-node tersebut dapat memiliki
   node-node anak, dan seterusnya ...
  
   Node dibuat menggunakan tag buka/tutup, dan node-node anak hanya
   berada di antara tag buka dan tutup .-->


<!-- XML membawa dua jenis data:
   1 - Atribut -> Itu metadata tentang sebuah node.
       Biasanya, parser XML menggunakan informasi ini untuk menyimpan data dengan
       benar. Hal ini ditandai dengan muncul dengan format nama = "nilai" dalam pembukaan tag.
   2 - Elemen -> Itu data yang murni.
       Itulah yang parser akan mengambil dari file XML.
       Elemen muncul antara tag membuka dan menutup.-->
      
  
<!-- Di bawah ini, unsur dengan dua atribut-->
<file type="gif" id="4293">komputer.gif</file>


```

* Dokumen yang well-formated & Validasi

Sebuah dokumen XML disebut well-formated jika sintaksisnya benar.
Namun, juga mungkin untuk mendefinisikan lebih banyak batasan dalam dokumen,
menggunakan definisi dokumen, seperti DTD dan XML Schema.

Sebuah dokumen XML yang mengikuti definisi dokumen disebut valid,
jika sesuai dokumen itu.

Dengan alat ini, Anda dapat memeriksa data XML di luar logika aplikasi.

```xml

<!-- Di bawah, Anda dapat melihat versi sederhana dari dokumen tokobuku,
  dengan penambahan definisi DTD .-->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE catatan SYSTEM "tokobuku.dtd">
<tokobuku>
  <buku category="MEMASAK">
    <judul >Everyday Italian</judul>
    <harga>30.00</harga>
  </buku>
</tokobuku>

<!-- This DTD could be something like:-->

<!DOCTYPE catatan
[
<!ELEMENT tokobuku (buku+)>
<!ELEMENT buku (judul,harga)>
<!ATTLIST buku category CDATA "Sastra">
<!ELEMENT judul (#PCDATA)>
<!ELEMENT harga (#PCDATA)>
]>


<!-- DTD dimulai dengan deklarasi.
  Berikut, node akar dinyatakan, membutuhkan 1 atau lebih anak node 'buku'.
  Setiap 'buku' harus berisi tepat satu 'judul' dan 'harga' dan atribut
  disebut 'kategori', dengan "Sastra" sebagai nilai default.
  Node yang 'judul' dan 'harga' mengandung karakter data diurai .-->

<!-- DTD dapat dideklarasikan di dalam file XML itu sendiri .-->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE catatan
[
<!ELEMENT tokobuku (buku+)>
<!ELEMENT buku (judul,harga)>
<!ATTLIST buku category CDATA "Sastra">
<!ELEMENT judul (#PCDATA)>
<!ELEMENT harga (#PCDATA)>
]>

<tokobuku>
  <buku category="MEMASAK">
    <judul >Everyday Italian</judul>
    <harga>30.00</harga>
  </buku>
</tokobuku>
```
