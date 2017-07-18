---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
translators:
    - ["Tasya Aditya Rukmana", "http://github.com/tadityar"]
lang: id-id
filename: markdown-id.md
---

Markdown dibuat oleh John Gruber pada tahun 2004. Tujuannya untuk menjadi syntax yang mudah dibaca dan ditulis yang dapat berubah menjadi HTML (dan sekarang berbagai format lainnya) dengan mudah.

Beri masukan sebanyak-banyaknya! / Jangan sungkan untuk melakukan fork dan pull request!


```markdown
<!-- Markdown adalah superset dari HTML, jadi setiap berkas HTML adalah markdown yang
valid, ini berarti kita dapat menggunakan elemen HTML dalam markdown, seperti elemen
komentar, dan ia tidak akan terpengaruh parser markdown. Namun, jika Anda membuat
elemen HTML di berkas markdown Anda, Anda tidak dapat menggunakan syntax markdown
di dalam konten elemen tersebut. -->

<!-- Markdown juga bervariasi dalam implementasinya dari berbagai parser. Panduan ini
akan mencoba untuk mengklarifikasikan kapan suatu fitur universal atau spesifik
terhadap parser tertentu -->

<!-- Header -->
<!-- Anda dapat membuat elemen HTML <h1> sampai <h6> dengan mudah dengan mendahului
teks yang diinginkan untuk elemen tersebut oleh sejumlah tanda pagar (#) -->
# Ini adalah <h1>
## Ini adalah <h2>
### Ini adalah <h3>
#### Ini adalah <h4>
##### Ini adalah <h5>
###### Ini adalah <h6>

<!-- Markdown juga menyediakan dua cara alternatif untuk menandai h1 and h2 -->
Ini adalah h1
=============

Ini adalah h2
-------------

<!-- Ragam teks simpel -->
<!-- Teks dapat diubah menjadi miring atau tebal dengan mudah menggunakan markdown -->

*Ini adalah teks miring.*
_Dan juga teks ini._

**Ini adalah teks tebal.**
__Dan juga teks ini.__

***Ini adalah teks dengan keduanya.***
**_Dan juga ini!_**
*__Dan ini!__*

<!-- Di markdown ala Github, yang digunakan untuk me-render berkas markdown pada
Github, kita juga punya coretan: -->

~~Teks ini dirender dengan coretan.~~

<!-- Paragraf adalah satu atau beberapa baris teks yang dipisahkan oleh satu atau
beberapa baris kosong. -->

Ini adalah paragraf. Saya mengetik dalam paragraf, bukankah ini menyenangkan?

Sekarang saya ada di paragraf 2.
Saya juga masih ada dalam paragraf 2!


Saya ada di paragraf 3!

<!-- Jika Anda ingin memasukkan tag HTML <br />, Anda dapat mengakhiri sebuah
paragraf dengan dua atau lebih spasi lalu memulai paragraf yang baru. -->

Aku diakhiri dua spasi (soroti aku untuk melihatnya).

Ada sebuah <br /> diatasku!

<!-- Kutipan mudah dibuat dengan karakter >. -->

> Ini adalah kutipan. Anda dapat
> membungkusnya secara manual dan meletakkan `>` sebelum tiap baris atau Anda dapat membuat baris yang sangat panjang dan membuatnya membungkus secara otomatis.
> Tidak ada masalah selama ia diawali dengan `>`.

> Anda juga dapat menggunakan lebih dari satu level
>> indentasi!
> Sangat rapi bukan?

<!-- Daftar -->
<!-- Daftar tak beraturan dapat dibuat dengan bintang, plus, atau strip -->

* Item
* Item
* Item lainnya

atau

+ Item
+ Item
+ Satu lagi item

or

- Item
- Item
- Item terakhir

<!-- List beraturan dibuat dengan angka diikuti titik -->

1. Item satu
2. Item dua
3. Item tiga

<!-- Anda tidak diharuskan melabeli item dengan benar dan markdown akan tetap
me-render angka sesuai urutan, namun mungkin hal ini kurang baik -->

1. Item satu
1. Item dua
1. Item tida
<!-- (Ini dirender sama seperti contoh di atas) -->

<!-- Anda juga dapat menggunakan sublist -->

1. Item satu
2. Item dua
3. Item tiga
    * Sub-item
    * Sub-item
4. Item empat

<!-- Bahkan ada daftar tugas. Ini membuat kotak centang HTML. -->

Kotak di bawah tanpa 'x' adalah kotak centang HTML yang belum diisi.
- [ ] Tugas pertama selesai.
- [ ] Tugas kedua yang harus diselesaikan
Kotak centang HTML berikut telah diisi.
- [x] Tugas ini telah diselesaikan

<!-- Blok kode -->
<!-- Anda dapat menandai blok kode (yang menggunakan elemen <code>) dengan mengindentasi
sebuah garis dengan empat spasi atau tab -->

    Ini adalah kode
    Dan ini juga

<!-- Anda juga dapat me-re-tab (atau menambahkan empat spasi tambahan) untuk indentasi
di dalam kode Anda -->

    array_ku.each do |item|
        puts item
    end

<!-- Sebaris kode dapat dibuat dengan karakter backtick ` -->

John bahkan tidak tahu apa fungsi dari `go_to()` !

<!-- Di Markdown ala Github, Anda dapat menggunakan syntax spesial untuk kode -->

\`\`\`ruby <!-- kecuali hapus backlash tersebut ketika melakukannya, hanya ```ruby ! -->
def foobar
    puts "Halo Dunia!"
end
\`\`\` <!-- Disini juga, tidak ada backslashes, hanya ``` -->

<!-- Teks di atas tidak membutuhkan indentasi, plus Github akan menggunakan syntax
highlighting dari bahasa yang digunakan setelah ``` -->

<!-- Horizontal rule (<hr />) -->
<!-- Horizontal rules ditambahkan dengan mudah oleh beberapa bintang atau strip,
dengan atau tanpa spasi. -->

***
---
- - -
****************

<!-- Tautan -->
<!-- Salah satu hal terbaik dari markdown adalah mudahnya membuat tautan. Letakkan
teks yang akan di tampilkan di dalam kurung siku [] diikuti oleh url-nya dalam kurung () -->

[Klik aku!](http://test.com/)

<!-- Anda juga dapat menambahkan judul link dengan tanda kutip di dalam kurung -->

[Klik aku!](http://test.com/ "Link to Test.com")

<!-- Path relatif juga bisa. -->

[Pergi ke musik](/music/).

<!-- Markdown juga mendukung tautan gara referal -->

[Klik link ini][link1] untuk info lebih banyak!
[Juga cek link ini][foobar] jika Anda mau.

[link1]: http://test.com/ "Keren!"
[foobar]: http://foobar.biz/ "OK!"

<!-- Judulnya juga bisa dalam kutip satu atau kurung, atau dihilangkan sepenuhnya.
Referensinya juga bisa di mana saja di dokumen anda dan IF referensinya bisa jadi
apa saja selama ia unik. -->

<!-- Ada juga "penamaan implisit" yang membuat Anda dapat menggunakan teks tautan sebagai id -->

[Ini][] adalah tautan.

[ini]: http://thisisalink.com/

<!-- Tapi ia tidak lazim digunakan. -->

<!-- Gambar -->
<!-- Gambar digunakan sama seperti tautan namun dengan tanda seru di depannya! -->

![Ini adalah atribut alt dari gambar saya](http://imgur.com/myimage.jpg "Judul opsional")

<!-- Dan gaya referensi juga bekerja seperti yang diharapkan -->

![Ini adalah atribut alt.][myimage]

[myimage]: relative/urls/cool/image.jpg "jika Anda membutuhkan judul, disini"

<!-- Lain-lain -->
<!-- Tautan otomatis -->

<http://testwebsite.com/> sama dengan
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Tautan otomatis untuk email -->

<foo@bar.com>

<!-- Melewati karakter -->

Saya ingin mengetik *teks ini dikelilingi tanda bintang* tapi saya tidak mau teksnya menjadi
miring, jadi saya melakukan: \*teks ini dikelilingi tanda bintang\*.

<!-- Tombol keyboard -->
<!-- Pada Markdown ala Github, Anda dapat menggunakan tag <kbd> untuk merepresentasikan tombol
keyboard -->

Komputer Anda hang? Coba kirim sebuah
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>

<!-- Tabel -->
<!-- Tabel hanya tersedia pada Markdown ala Github dan sedikit merepotkan, namun jika Anda
sangat menginginkannya: -->

| Kol1         | Kol2     | Kol3          |
| :----------- | :------: | ------------: |
| Rata-kiri    | Tengah   | Rata-Kanan    |
| blah         | blah     | blah          |

<!-- atau, untuk hasil yang sama -->

Kol 1 | Kol2 | Kol3
:-- | :-: | --:
Ugh ini sangat jelek | buat ia | berhenti

<!-- Selesai! -->

```

Untuk info lebih lanjut, cek post syntax resmi John Gruber [di sini](http://daringfireball.net/projects/markdown/syntax) dan contekan hebat Adam Pritchard's [di sini](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
