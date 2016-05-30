---
language: xml
filename: learnxml-id.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
  - ["Rizky Luthfianto", "https://github.com/rilut"]
  - ["Ahmad Zafrullah", "https://github.com/23Pstars"]
lang: id-id
---

XML adalah bahasa markup yang dirancang untuk menyimpan dan mengirim data. XML mudah dibaca oleh manusia dan mesin.

Tidak seperti HTML, XML tidak menentukan bagaimana menampilkan atau format data, hanya membawanya.

Terdapat perbedaan antara **konten** dan **markup**. Singkatnya, konten dapat berupa apapun dan markup adalah sebagai penentu.
 
## Definisi dan Pendahuluan
 
Dokumen XML pada dasarnya disusun oleh *elemen* yang dapat memiliki *atribut* untuk menjelaskan elemen tersebut dan dapat memiliki beberapa konten tekstual atau beberapa elemen sebagai anak-nya. Setiap dokumen XML hendaknya memiliki satu elemen akar, yang menjadi induk dari semua elemen dalam dokumen XML.
 
Pengurai XML dirancang menjadi sangat ketat, dan akan berhenti melakukan penguraian terhadap dokumen yang cacat. Oleh karena itu semua dokumen XML harus mengikuti [Aturan Sintaks XML](http://www.w3schools.com/xml/xml_syntax.asp).

```xml
<!-- Ini adalah komentar. Komentar harus memiliki dua tanda penghubung secara berurutan (-). -->
<!-- Komentar dapat renggang
  menjadi banyak baris -->

<!-- Elemen -->
<!-- Elemen merupakan komponen dasar dari XML. Ada dua tipe dari elemen, kosong: -->
<elemen1 atribut="nilai" /> <!-- Elemen kosong tidak memiliki konten apapun -->
<!-- dan tidak-kosong: -->
<elemen2 atribut="nilai">Konten</elemen2>
<!-- Nama elemen hanya dapat berupa huruf dan angka saja. -->

<kosong /> <!-- Elemen yang terdiri dari tag elemen kosong… -->
<!-- …tidak memiliki content apapun dan murni markup. -->

<tidakkosong> <!-- Atau, elemen ini memiliki tag pembuka… -->
  <!-- …suatu konten… -->
</tidakkosong> <!-- dan sebuah tag penutup. -->

<!-- Nama elemen merupakan *case sensitive*. -->
<elemen />
<!-- …tidak sama dengan elemen sebelumnya -->
<eLEMEN />

<!-- Atribut -->
<!-- Sebuah atribut merupakan hubungan kunci-nilai yang terdapat pada elemen. -->
<elemen atribut="nilai" lainnya="nilaiLainnya" banyakNilai="daftar nilai ber-spasi" />
<!-- Sebuah atribut digunakan hanya sekali dalam sebuah elemen. Dan hanya memiliki satu nilai.
  Salah satu solusi untuk mengatasi permasalahan tersebut adalah dengan menggunakan daftar nilai ber-spasi. -->

<!-- Elemen bersarang -->
<!-- Konten dari sebuah elemen dapat berupa elemen lainnya:: -->
<ayah>
  <anak>Teks</anak>
  <oranglain />
</ayah>
<!-- Mengikuti standar tatanan pohon. Setiap elemen disebut *node*.
  Induk yang berada satu tingkat diatasnya disebut *parent*, keturunan yang berada satu tingkat dibawahnya disebut *children*.
  Elemen yang berada pada *parent* yang sama disebut Saudara (*siblings*). -->

<!-- XML mempertahankan spasi. -->
<anak>
  Teks
</anak>
<!-- …tidak sama dengan -->
<anak>Teks</anak>
```
 

## Dokumen XML

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- XML prolog, boleh tidak digunakan namun direkomendasikan untuk digunakan. -->
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

## Dokumen yang well-formated & Validasi

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
## Kompatibilitas DTD dan Definisi Skema XML

Dukungan untuk DTD dapat ditemukan dimana-mana karena sudah sangat lama. Namun sayangnya, fitur XML terkini seperti *namespaces* tidak didukung oleh DTD. XML Xchema Definitions (XSDs) bertujuan untuk mengganti DTD dalam mendefinisikan tatabahasa dokumen XML.

## Sumber

* [Validasi dokumen XML](http://www.xmlvalidation.com)

## Bacaan lainnya

* [XML Schema Definitions Tutorial](http://www.w3schools.com/schema/)
* [DTD Tutorial](http://www.w3schools.com/xml/xml_dtd_intro.asp)
* [XML Tutorial](http://www.w3schools.com/xml/default.asp)
* [Using XPath queries to parse XML](http://www.w3schools.com/xml/xml_xpath.asp)
