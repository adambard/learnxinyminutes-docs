---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
translators:
    - ["Eka Y Saputra", "http://github.com/ekajogja"]
lang: id-id
filename: learncss-id.css
---

Pada mulanya, web tidak memiliki elemen visual, murni teks saja.
Tapi seiring perkembangan peramban, laman web dengan elemen visual menjadi umum.
CSS adalah bahasa standar yang ada untuk menjaga keterpisahan antara
konten (HTML) serta tampilan-dan-kesan laman web.

Singkatnya, fungsi CSS ialah menyajikan sintaks yang memampukan kita
untuk memilih elemen tertentu dalam sebuah laman HTML
dan menerapkan berbagai properti visual bagi elemen tersebut.

Seperti bahasa lainnya, CSS memiliki banyak versi.
Di artikel ini, kita fokus pada CSS2.0 - yang meskipun bukan versi termutakhir
namun paling kompatibel dan didukung secara luas.

**CATATAN:** Lantaran keluaran dari CSS berwujud efek-efek visual,
maka untuk mempelajarinya, kita perlu mencoba berbagai hal dalam dunia olah CSS
semisal [dabblet](http://dabblet.com/).
Fokus utama artikel ini ialah pada sintaks dan sejumlah tips umum.


```css
/* komentar terletak diantara sepasang tanda garis miring dan bintang,
persis seperti larik ini! */

/* ####################
   ## SELEKTOR
   ####################*/

/* Secara garis besar, statemen utama dalam CSS sangat sederhana */
selektor { properti: nilai; /* properti lainnya */ }

/* selektor berfungsi untuk memilih suatu elemen dalam sebuah laman.

Kita juga bisa memilih semua elemen di sebuah halaman! */
* { color:red; }

/*
Dengan menentukan sebuah elemen seperti ini pada sebuah laman:

<div class='suatu-class class2' id='suatuId' attr='nilai' />
*/

/* kita bisa memilih elemen berdasarkan nama class-nya */
.suatu-class { }

/*atau dengan dua class sekaligus! */
.suatu-class.class2 { }

/* atau dengan nama tag-nya */
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

/* atau bahkan disisipi nilai */
[attr~='la'] { font-size:smaller; }


/* dan yang lebih penting lagi, kita bisa mengombinasikannya sekaligus
dengan syarat tidak ada spasi diantara selektor-selektor. sebab adanya spasi
akan membuat selektor itu memiliki makna yang berbeda.*/
div.suatu-class[attr$='ai'] { }

/* kita juga bisa memilih sebuah elemen berdasarkan posisi elemen induknya.*/

/*sebuah elemen yang merupakan anak langsung dari elemen induk (diseleksi dng
cara yang sama) */
div.suatu-induk > .-suatu-class {}

/* atau salah satu induk elemennya dalam hirarki elemen */
/* berikut ini dimaksudkan pada elemen manapun dengan class "class-entah" dan
merupakan anak elemen dari suatu div dengan class "induk-entah" PADA LEVEL
HIRARKI MANAPUN */
div.suatu-induk .suatu-class {}

/* peringatan: selektor yang sama jika tanpa ada spasi akan bermakna lain.
misalnya? */
div.suatu-induk.suatu-class {}

/* kita juga bisa memilih sebuah elemen berdasarkan saudara elemen yang muncul
tepat sebelumnya */
.aku-muncul-tepat-sebelum + .elemen-ini { }

/*atau saudara elemen manapun yang pernah muncul selang beberapa elemen
sebelumnya */
.aku-pernah-muncul-sebelum ~ .elemen-ini {}

/* Ada beberapa pseudo-class yang memampukan kita memilih suatu elemen
berdasarkan perilaku lamannya (bukan struktur lamannya) */

/* semisal ketika sebuah elemen ditimpa hover (pointer mouse) */
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
    width: 50%; /* dalam persen */
    font-size: 2em; /* angka kali jumlah font-size saat ini */
    width: 200px; /* dalam pixel */
    font-size: 20pt; /* dalam point */
    width: 5cm; /* dalam centimeter */
    width: 50mm; /* dalam milimeter */
    width: 5in; /* dalam inci */
    
    /* Warna */
    background-color: #F6E;  /* dalam short hex */
    background-color: #F262E2; /* dalam format long hex */
    background-color: tomato; /* warna yang sudah punya konvensi nama */
    background-color: rgb(255, 255, 255); /* dalam rgb */
    background-color: rgb(10%, 20%, 50%); /* dalam persen rgb */
    background-color: rgba(255, 0, 0, 0.3); /* dalam rgb semi-transparan*/
    
    /* Gambar */
    background-image: url(/folder-gambar/image.jpg);
    
    /* Font */
    font-family: Arial;
    font-family: "Courier New"; /* jika nama font memiliki spasi,
    							ia diketik dalam tanda petik ganda */
    font-family: "Courier New", Trebuchet, Arial; /* jika font pertama tidak
    							ditemukan, peramban menggunakan font berikutnya,
    							demikian secara berturut-turut */
}

```

## Penggunaan

Simpan semua CSS yang hendak kita pakai dengan ekstensi `.css`.

```xml
<!-- kita harus menautkan file css itu ke laman di bagian <head>: -->
<link rel='stylesheet' type='text/css' href='folder/namafile.css' />

<!-- kita juga bisa mengetik CSS secara inline di dalam markup.
Namun, sebisa mungkin metode ini dihindari. -->
<style>
   selektor { properti:nilai; }
</style>

<!-- atau langsung mengetik properti CSS pada sebuah elemen. 
Metode ini harus dihindari sebisa mungkin. -->
<div style='properti:nilai;'>
</div>

```

## Prioritas

Kita tahu bahwa sebuah elemen bisa dipilih dengan lebih dari satu selektor, 
serta bisa diberi lebih dari satu properti.
Dalam kasus seperti ini, hanya salah satu properti saja yang akan diterapkan
pada elemen dengan prioritas tertentu.

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
<p style='/*F*/ properti:nilai;' class='class1 class2' attr='nilai'>
</p>
```

Maka prioritas penerapan style-nya ialah sbb.:  
Ingat, penerapan ini untuk masing-masing **properti**,
bukan keseluruhan larik.

* `E` prioritas pertama sebab ada kata `!important`.  
	Dianjurkan untuk menghindari kata ini jika tidak benar-benar perlu.
* `F` prioritas kedua sebab ia diketik secara inline.
* `A` prioritas ketiga sebab selektor ini lebih spesifik dibanding yang lain.  
	lebih spesifik = lebih banyak unsur selektor. contoh ini punya 3 unsur:
	1 tagname `p` + 1 nama class `class1` + 1 attribute `attr='nilai'`
* `C` prioritas berikutnya sebab meski sama spesifik dengan `B` namun
	ia muncul lebih akhir.
* Lalu `B`
* dan terakhir baru `D`.

## Kompatibilitas

Sebagian besar fitur dalam CSS2 (dan lambat laun juga CSS3) kompatibel dengan
semua peramban dan perangkat. Namun selalu vital untuk memastikan kompatibilitas
unsur dan nilai yang kita ketikkan dalam CSS dengan peramban yang ditargetkan.

[QuirksMode CSS](http://www.quirksmode.org/css/) ialah salah satu sumber terbaik untuk memeriksa kompatibilitas CSS dan peramban.

## Referensi Lanjut

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)

