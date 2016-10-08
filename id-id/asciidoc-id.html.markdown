---
language: asciidoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
translators:
    - ["Rizky Luthfianto", "http://github.com/rilut"]
filename: asciidoc-id.md
lang: id-id
---

AsciiDoc adalah bahasa markup yang mirip dengan Markdown dan dapat digunakan untuk apa saja, untuk menulis buku maupun blog. Dibuat pada tahun 2002 oleh Stuart Rackham, bahasa ini sederhana tetapi memungkinkan sejumlah besar kustomisasi.

Kepala Dokumen

Kepala Dokumen adalah opsional dan tidak dapat berisi baris kosong. Harus diimbangi konten, setidaknya satu baris kosong.

Hanya Judul 

```
= Judul Dokumen

Kalimat pertama dokumen.
```

Judul dan Penulis

```
= Judul Dokumen
Pertama terakhir <first.last@learnxinyminutes.com>

Awal dokumen ini.
```

Banyak Penulis

```
= Judul Dokumen
John Doe <john@go.com>; Jane Doe <jane@yo.com>; Black Beard <beardy@pirate.com>

Memulai dokumen dengan banyak penulis.
```

Garis Revisi (membutuhkan garis penulis)

```
= Judul Dokumen V1
Manusia Kentang <keripik@renyah.com>
v1.0, 2016/01/13

Artikel tentang keripik ini akan menjadi menyenangkan.
```

Paragraf

```
Anda tidak perlu sesuatu yang istimewa untuk paragraf.

Tambahkan baris kosong antara paragraf untuk memisahkan mereka.

Untuk membuat baris kosong, tambahkan: +
dan Anda akan mendapat satu baris kosong!
```

Memformat Teks

```
_underscore menciptakan miring_
*Tanda bintang untuk tebal*
*_Gabungkan biar makin asyik_*
`Penggunaan tanda petik untuk menandakan monospace`
`*Monospace tebal*`
```

Judul bagian

```
= Level 0 (hanya dapat digunakan dalam header dokumen)

== Level 1 <h2>

=== Level 2 <h3>

==== Level 3 <h4>

===== Level 4 <h5>

====== Level 5 <h6>

======= Level 6 <h7>

```

Daftar

Untuk membuat daftar bullet, gunakan tanda bintang.

```
* foo
* bar
* baz
```

Untuk membuat daftar bernomor, gunakan titik.

```
. Item 1
. item 2
. Item 3
```

Anda bisa membuat daftar bersarang dengan menambahkan tanda bintang atau titik tambahan hingga lima kali.

```
* Foo 1
** Foo 2
*** Foo 3
**** Foo 4
***** Foo 5

. foo 1
.. Foo 2
... Foo 3
.... Foo 4
..... Foo 5
```
