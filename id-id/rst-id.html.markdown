---
language: restructured text (RST)
filename: rst-id.html
contributors:
    - ["DamienVGN", "https://github.com/martin-damien"]
    - ["Andre Polykanine", "https://github.com/Oire"]
translators:
    - ["Haydar Ali Ismail", "https://github.com/haydarai"]
lang: id-id
---

RST adalah sebual format file yang dibuat oleh komunitas Python untuk menulis
dokumentasi (dan menjadi bagian dari Docutils).

File-file RST adalah sebuah file-file teks simpel dengan sintaks yang ringan
(dibandingkan dengan HTML).


## Pemasangan

Untuk menggunakan RST, anda harus memasang [Python](http://www.python.org) dan
paket `docutils` terlebih dahulu.

`docutils` bisa dipasang menggunakan command berikut:

```bash
$ easy_install docutils
```

Jika sistem anda sudah mempunyai `pip`, anda bisa menggunakannya juga:

```bash
$ pip install docutils
```


## Sintaks file

Sebuah contoh sederhana dari sintaks file:

```
.. Baris yang dimulai dengan dua titik adalah perintah spesial. Tetapi jika
perintah tidak ditemukan, maka baris tersebut akan dianggap sebagai komentar

===============================================================================
Judul utama ditulis menggunakan rentetan tanda sama dengan di atas dan bawahnya
===============================================================================

Ingat bahwa jumlah tanda sama dengan harus sama panjangnya dengan total
karakter judul

Judul juga digarisbawahi dengan tanda sama dengan juga
======================================================

Sub-judul dengan menggunakan dash
---------------------------------

Dan sub-sub-judul dengan menggunakan tilde
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Anda bisa menulis teks dalam *italik* atau *tebal*, anda bisa "menandai" teks
sebagai kode dengan menggunakan backquote ganda ``: ``print()``.

Membuat daftar semudah seperti dalam Markdown:

- Barang pertama
- Barang kedua
    - Sub barang

atau

* Barang pertama
* Barang kedua
    * Sub barang

Tabel sangat mudah untuk ditulis:

=========== ========
Negara      Ibu Kota
=========== ========
Prancis     Paris
Jepang      Tokyo
=========== ========

Tabel yang lebih kompleks bisa dibuat dengan mudah (kolom tergabung atau/dan
baris) tetapi saya menyarankan anda untuk membaca dokumentasi lengkap tentang
ini :)

Ada berbagai macam cara untuk membuat tautan:

- Dengan menambahkan garis bawah setelah sebuah huruf : Github_ dan dengan
menambahkan URL target setelah teks (cara ini mempunyai kelebihan dengan tidak
memasukkan URL yang tidak penting ke dalam teks yang bisa dibaca).
- Dengan mengetik URL lengkap yang dapat dipahami : https://github.com (akan
otomatis diubah menjadi sebuah link)
- Dengan membuat link seperti di Markdown: `Github <https://github.com/>`_ .

.. _Github https://github.com/

```


## Bagaimana Cara Menggunakannya

RST hadir dengan docutils di mana anda mempunyai `rst2html`, sebagai contoh:

```bash
$ rst2html fileku.rst hasil.html
```

*Catatan : Di beberapa sistem, perintah tersebut bisa menjadi rst2html.py*

Tetapi ada beberapa aplikasi kompleks yang menggunakan format RST:

- [Pelican](http://blog.getpelican.com/), Generator web statik
- [Sphinx](http://sphinx-doc.org/), Generator dokumnetasi
- dan masih banyak lainnya


## Bacaan

- [Referensi singkat resmi](http://docutils.sourceforge.net/docs/user/rst/quickref.html)
