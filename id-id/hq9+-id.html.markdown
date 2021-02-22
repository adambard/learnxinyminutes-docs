---
language: HQ9+
filename: hq9+-id.html
contributors:
    - ["Alexey Nazaroff", "https://github.com/rogaven"]
translators:
  - ["Haydar Ali Ismail", "http://github.com/haydarai"]
lang: id-id
---

HQ9+ adalah bahasa pemrograman gurauan yang dibuat oleh Cliff Biffle. Bahasa
ini hanya memiliki empat perintah dan tidak memenuhi Turing-complete.

```
Hanya ada 4 perintah, masing-masing direpresentasikan oleh karakter berikut
H: mencetak "Hello, world!"
Q: mencetak kode sumber dari program ini (Quine)
9: mencetak lirik dari lagu "99 Bottles of Beer"
+: menambah nilai satu ke akumulator (nilai dari akumulator tidak dapat
	diakses)
Karakter lain akan dihiraukan.

Ok. Mari kita menulis beberapa program:
  HQ9

Hasil:
  Hello world!
  HQ9

HQ9+ sangat sederhana, tetapi membuat anda bisa melakukan hal yang sangat sulit
dilakukan di bahasa lain. Sebagai contoh, berikut sebuah program yang
menciptakan tiga salinan dirinya sendiri ke layar:
  QQQ

Ini menghasilakn:
  QQQ
  QQQ
  QQQ
```

Dan itu semuanya. Ada banyak interpreters untuk HQ9+. Kamu bisa menemukannya di
bawah

+ [Salah satu interpreter online](https://almnet.de/esolang/hq9plus.php)
+ [Website resmi HQ9+](http://cliffle.com/esoterica/hq9plus.html)
