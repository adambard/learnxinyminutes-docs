---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
    - ["Geoffrey Liu", "https://github.com/g-liu"]
    - ["Connor Shea", "https://github.com/connorshea"]
    - ["Deepanshu Utkarsh", "https://github.com/duci9y"]
    - ["Tyler Mumford", "https://tylermumford.com"]
translators:
    - ["Abdul Muhaimin", "http://github.com/infacq"]
lang: my-my
filename: learncss-my.css
---

Pada mulanya, halaman web tidak mempunyai elemen visual, hanya teks sahaja.
Tetapi sejajar perkembangan pembangunan pelayar, laman web disertai elemen visual menjadi suatu kemestian.
CSS adalah salah satu bahasa yang digunakan untuk menjaga keterasingan antara
kandungan (HTML) serta tampilan-dan-kesan laman web.

Secara kasar, fungsi CSS menyajikan sintaks yang memampukan kita
untuk memilih elemen tertentu dalam sesebuah halaman HTML
dan menerapkan berbagai kesan khas visual untuk elemen tersebut.

Seperti bahasa yang lain-lain, CSS juga mengalami beberapa jilid.
Di artikel ini, kita akan fokus hanya pada CSS2.0 - yang meskipun bukan versi terkini
namun paling serasi dan didukung meluas sekali oleh kebanyakan pelayar.

**CATATAN:** Lantaran keluaran dari CSS berwujud kesan khas visual,
maka untuk mempelajarinya, kita perlu mencuba berbagai hal dalam dunia olah CSS
seperti [dabblet](http://dabblet.com/).
Fokus utama artikel ini adalah pada sintaks dan sejumlah tips umumnya.


```css
/* catatan komen terletak diantara sepasang tanda garis condong dan bintang,
persis seperti baris ini! */

/* ####################
   ## SELEKTOR
   ####################*/

/* Umumnya, statemen utama dalam CSS sangat sederhana */
selektor { properti: nilai; /* properti lainnya */ }

/* selektor berfungsi untuk memilih suatu elemen didalam sesebuah halaman.

Kita juga boleh memilih kesemua elemen dalam sebuah halaman! */
* { color:red; }

/*
Dengan menentukan sebuah elemen seperti ini pada sebuah laman:

<div class='suatu-class class2' id='suatuId' attr='nilai' />
*/

/* kita boleh memilih elemen berdasarkan nama class-nya */
.suatu-class { }

/*atau dengan dua class sekaligus! */
.suatu-class.class2 { }

/* atau dengan tag-nya */
div { }

/* atau id-nya */
#suatuId { }

/* atau - jika ada - dengan attribute-nya! */
[attr] { font-size:smaller; }

/* atau jika attribute tersebut memiliki nilai spesifik */
[attr='nilai'] { font-size:smaller; }

/* dibuka dengan sebuah nilai*/
[attr^='nil'] { font-size:smaller; }

/* atau ditutup dengan nilai */
[attr$='ai'] { font-size:smaller; }

/* atau bahkan diiring sebarang nilai */
[attr~='la'] { font-size:smaller; }


/* dan yang lebih penting lagi, kita boleh menggabungkannya sekaligus
dengan syarat tidak ada ruang kosong diantara selektor-selektor. sebab adanya ruang kosong
akan membuat selektor itu memiliki makna yang berbeza.*/
div.suatu-class[attr$='ai'] { }

/* kita juga boleh memilih sebuah elemen berdasarkan posisi elemen induknya.*/

/*sebuah elemen yang merupakan anak langsung dari elemen induk (diseleksi dengan
cara yang sama) */
div.suatu-induk > .-suatu-class {}

/* atau salah satu induk elemennya dalam hirarki elemen */
/* berikut ini dimaksudkan pada elemen manapun dengan class "class-entah" dan
merupakan anak elemen dari suatu div dengan class "induk-entah" PADA LEVEL
HIRARKI MANAPUN */
div.suatu-induk .suatu-class {}

/* AWAS: selektor yang sama jika tanpa ada ruang kosong akan membawa makna yang lain.
misalnya? */
div.suatu-induk.suatu-class {}

/* kita juga boleh memilih sebuah elemen berdasarkan saudara elemen yang muncul
tepat sebelumnya */
.aku-muncul-tepat-sebelum + .elemen-ini { }

/*atau saudara elemen manapun yang pernah muncul selang beberapa elemen
sebelumnya */
.aku-pernah-muncul-sebelum ~ .elemen-ini {}

/* Ada beberapa pseudo-class yang memampukan kita memilih suatu elemen
berdasarkan perilaku lamannya (bukan struktur lamannya) */

/* contoh seperti ketika sebuah elemen ditimpa hover (pointer mouse) */
:hover {}

/* atau link yang sudah pernah diklik*/
:visited {}

/* atau link yang belum pernah diklik*/
:link {}

/* atau elemen input yang menjadi fokus */
:focus {}


/* ####################
   ## PROPERTI
   ####################*/

selektor {

    /* Unit */
    width: 50%; /* dalam peratusan */
    font-size: 2em; /* angka kali jumlah font-size saat ini */
    width: 200px; /* piksel */
    font-size: 20pt; /* point */
    width: 5cm; /* sentimeter */
    width: 50mm; /* milimeter */
    width: 5in; /* inci */

    /* Warna */
    background-color: #F6E;  /* kod hex  yang pendek */
    background-color: #F262E2; /* bentuk hex yang panjang */
    background-color: tomato; /* warna yang sudah punya konvensi nama */
    background-color: rgb(255, 255, 255); /* dalam rgb */
    background-color: rgb(10%, 20%, 50%); /* dalam peratusan rgb */
    background-color: rgba(255, 0, 0, 0.3); /* dalam rgb separuh-tampak-tembus*/

    /* Gambar */
    background-image: url(/folder-gambar/image.jpg);

    /* Font */
    font-family: Arial;
    font-family: "Courier New"; /* jika nama font memiliki ruang antaranya,
    							ia mesti dirangkum dalam tanda petik ganda */
    font-family: "Courier New", Trebuchet, Arial; /* jika font pertama tidak
    							dijumpai, ia akan menggunakan font berikutnya,
    							demikian secara berturut-turut */
}

```

## Penggunaan

Simpan semua CSS yang hendak kita pakai dengan sambungan `.css`.

```xml
<!-- menetapkan fail css itu ke laman di bagian <head>: -->
<link rel='stylesheet' type='text/css' href='folder/namafile.css' />

<!-- kita juga boleh meletak sintaks CSS secara inline di dalam markup.
Namun, hindari kaedah ini sebaik mungkin. -->
<style>
   selektor { properti:nilai; }
</style>

<!-- atau langsung memasukkan properti CSS pada sebuah elemen.
Kaedah ini harus dihindari seboleh mungkin. -->
<div style='properti:nilai;'>
</div>

```

## Keutamaan

Kita sedia maklum sesebuah elemen boleh dipilih menggunakan lebih dari satu selektor,
serta boleh diberi lebih dari satu properti.
Merujuk kes dibawah ini, hanya salah satu properti saja yang akan diterapkan
pada elemen dengan keutamaan tertentu.

Dengan susunan CSS:

```css

/*A*/
p.class1[attr='nilai']

/*B*/
p.class1 {}

/*C*/
p.class2 {}

/*D*/
p {}

/*E*/
p { properti: nilai !important; }

```

dan susunan markup:

```xml
<p style='/*F*/ properti:nilai;' class='class1 class2' attr='nilai'></p>
```

Maka keutamaan penerapan style-nya ialah sbb.:  
Ingat, penerapan ini untuk masing-masing **properti**,
bukan keseluruhan baris.

* `E` keutamaan pertama sebab ada kata `!important`.  
	Dianjurkan untuk menghindari kata ini jika tidak betul-betul diperlukan.
* `F` keutamaan kedua sebab ia digunakan secara inline.
* `A` keutamaan ketiga sebab selektor ini lebih spesifik berbanding yang lain.  
	lebih khusus = lebih banyak unsur selektor. contoh ini punya 3 unsur:
	1 tagname `p` + 1 nama class `class1` + 1 attribute `attr='nilai'`
* `C` keutamaan berikutnya sebab meski sama spesifik dengan `B` namun
	ia muncul lebih akhir.
* Lalu `B`
* dan terakhir baru `D`.

## Keserasian

Sebagian besar ciri-ciri dalam CSS2 (dan lambat laun juga CSS3) serasi dengan
semua peramban dan perangkat. Namun adalah penting untuk memastikan keserasian
unsur dan nilai yang kita gunakan dalam CSS dengan peramban yang disasarkan.

[QuirksMode CSS](http://www.quirksmode.org/css/) ialah salah satu sumber terbaik untuk memeriksa kesesuaian CSS dan peramban.

## Maklumat Lanjut

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
