---
language: bf
filename: brainfuck-id.bf
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Muhammad Rifqi Fatchurrahman", "http://muhrifqii.github.io/"]
lang: id-id
---

Brainfuck (tidak dalam huruf kapital kecuali pada awal kalimat) adalah sebuah
bahasa pemrograman Turing-complete yang sangat minim yang hanya memiliki 8 perintah.

Anda bisa mencoba brainfuck pada browser dengan menggunakan [brainfuck-visualizer](http://fatiherikli.github.io/brainfuck-visualizer/).

```bf
Karakter apapun selain "><+-.,[]" (tanda kutip tidak termasuk) diabaikan.

Brainfuck direpresentasikan dengan sebuah array yang memiliki 30,000 cell yang
diinisialisasi dengan nol dan pointer data yang menunjuk ke current cell.

Terdapat delapan perintah:
+ : Menaikkan nilai pada current cell sebesar satu.
- : Menurunkan nilai pada current cell sebesar satu.
> : Menggeser pointer data ke cell selanjutnya (cell sebelah kanan).
< : Menggeser pointer data ke cell sebelumnya (cell sebelah kiri).
. : Mencetak nilai ASCII pada current cell (misal 65 = 'A').
, : Membaca sebuah karakter masukan tunggal ke dalam current cell.
[ : Jika nilai pada current cell bernilai nol, lewati hingga mencapai ] yang sesuai.
    Jika tidak, pindah ke instruksi berikutnya.
] : Jika nilai pada current cell bernilai nol, pindah ke instruksi berikutnya.
    Jika tidak, mundur pada instruksi hingga mencapai [ yang sesuai.

[ dan ] membentuk sebuah rekursi while. Tentu saja mereka harus seimbang.

Mari kita lihat beberapa program brainfuck dasar.

++++++ [ > ++++++++++ < - ] > +++++ .

Program ini mencetak huruf 'A'. Mula-mula, cell #1 dinaikkan ke 6.
Cell #1 akan digunakan untuk rekursi. Lalu, masuk ke rekursi ([) dan pindah
ke cell #2. Cell #2 dinaikkan 10 kali, mundur ke cell #1, dan menurunkan
cell #1. Rekursi ini berlangsung 6 kali (melakukan 6 penurunan nilai untuk 
cell #1 hingga mencapai 0, di titik mana dia melewati hingga mencapai ] dan
terus berlanjut).

Pada titik ini, kita berada pada cell #1, yang memiliki nilai 0, sedangkan cell #2
memiliki sebuah nilai 60. Kita berpindah ke cell #2, menaikkan nilai 5 kali, memunculkan 
nilai 65, lalu cetak nilai pada cell #2. 65 adalah 'A' pada ASCII, jadi 'A' 
dicetak ke terminal.

, [ > + < - ] > .

Program ini membaca sebuah karakter dari masukan user dan menyalin karakternya ke
cell #1. Setelah itu rekursi dimulai. Geser ke cell #2, menaikkan nilai pada cell #2,
mundur ke cell #1, dan menurunkan nilai pada cell #1. Hal ini berlanjut sampai cell #1
bernilai 0, dan cell #2 menyimpan nilai lama dari cell #1. Karena kita berada di cell #1
saat ujung rekursi, geser ke cell #2, lalu cetak nilai dalam bentuk ASCII.

Perlu diingat bahwa spasi itu murni untuk memudahkan membaca. Anda bisa 
menuliskannya dengan mudah seperti:

,[>+<-]>.

Coba dan cari tahu apa yang program ini lakukan:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Program ini menerima dua buah angka sebagai input, lalu mengalikannya.

Intinya adalah membaca dua masukan. Lalu mulai pada rekursi terluar yang
kondisinya pada cell #1. Lalu pindah ke cell #2, dan mulai rekursi terdalam
yang kondisinya ada pada cell #2, menaikkan nilai pada cell #3. Namun,
ada suatu masalah: Pada akhir dari rekursi terdalam, cell #2 bernilai nol.
Pada kasus tersebut, rekursi terdalam tidak dapat bekerja lagi mulai setelah ini.
Untuk menyelesaikan masalah tersebut, kita juga menaikkan cell #4, dan menyalin
ulang cell #4 ke cell #2. Maka cell #3 adalah hasilnya.
```

Dan itulah brainfuck. Tidak terlalu sulit kan? Hanya untuk iseng-iseng, anda 
bisa menuliskan porgram brainfuck anda sendiri, atau anda bisa menuliskan interpreter 
brainfuck pada bahasa lain. Interpreternya tidak begitu sulit untuk diimplementasikan, 
tapi jika anda seorang masokis, cobalah menulis sebuah interpreter brainfuck... dalam 
brainfuck.

