---
language: sass
filename: learnsass-ms.scss
contributors:
  - ["Laura Kyle", "https://github.com/LauraNK"]
translators:
  - ["hack1m", "https://github.com/hack1m"]
lang: ms-my
---

Sass ialah bahasa sambungan CSS yang menambah ciri-ciri seperti pembolehubah, bersarang, mixins dan banyak lagi.
Sass (dan prapemproses lain, seperti [Less](http://lesscss.org/)) membantu pembangun untuk menulis kod mampu diselenggara dan DRY (Don't Repeat Yourself).

Sass mempunyai dua perbezaan pilihan sintaks untuk dipilih. SCSS, yang mana mempunyai sintaks yang sama seperti CSS tetapi dengan ditambah ciri-ciri Sass. Atau Sass (sintaks asal), yang menggunakan indentasi bukannya tanda kurung dakap dan semikolon.
Tutorial ini ditulis menggunakan SCSS.

```scss

//Komen baris tunggal dikeluarkan apabila Sass dikompil ke CSS.

/*Komen multi dikekalkan. */



/*Pembolehubah
==============================*/



/* Anda boleh menyimpan nilai CSS (seperti warna) dalam pembolehubah.
Guna simbol '$' untuk membuat pembolehubah. */

$primary-color: #A3A4FF;
$secondary-color: #51527F;
$body-font: 'Roboto', sans-serif;

/* Anda boleh mengguna pembolehubah diseluruh lembaran gaya anda.
Kini jika anda ingin mengubah warna, anda hanya perlu membuat perubahan sekali.*/

body {
	background-color: $primary-color;
	color: $secondary-color;
	font-family: $body-font;
}

/* Ia akan dikompil kepada: */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}


/* Ini jauh lebih mampu diselenggara daripada perlu menukar warna
setiap yang ada diseluruh lembaran gaya anda. */



/*Mixins
==============================*/



/* Jika anda jumpa yang anda menulis kod yang sama pada lebih dari satu
elemen, anda mungkin ingin menyimpan kod itu di dalam mixin.

Guna arahan '@mixin', tambah dengan nama untuk mixin anda.*/

@mixin center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* Anda boleh guna mixin bersama '@include' dan nama mixin. */

div {
	@include center;
	background-color: $primary-color;
}

/*Ia akan dikompil kepada: */
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #A3A4FF;
}


/* Anda boleh guna mixins untuk membuat singkatan property. */

@mixin size($width, $height) {
	width: $width;
	height: $height;
}

/*Yang mana anda boleh seru dengan memberi argumen lebar dan tinggi. */

.rectangle {
	@include size(100px, 60px);
}

.square {
	@include size(40px, 40px);
}

/* Ia dikompil kepada: */
.rectangle {
  width: 100px;
  height: 60px;
}

.square {
  width: 40px;
  height: 40px;
}




/*Extend (Inheritance)
==============================*/



/*Extend ialah jalan untuk berkongsi sifat dengan satu pemilih dengan yang lain. */

.display {
	@include size(5em, 5em);
	border: 5px solid $secondary-color;
}

.display-success {
	@extend .display;
	border-color: #22df56;
}

/* Dikompil kepada: */
.display, .display-success {
  width: 5em;
  height: 5em;
  border: 5px solid #51527F;
}

.display-success {
  border-color: #22df56;
}




/*Bersarang
==============================*/



/*Sass membenarkan anda untuk sarangkan pemilih dengan pemilih */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #FF0000;		
	}
}

/* '&' akan digantikan dengan pemilih induk. */
/* Anda juga boleh sarangkan kelas-pseudo. */
/* Perlu diingat terlebih bersarang akan membuat kod anda kurang mampu diselenggara.
Sebagai contoh: */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: red;

		&:hover {
		  background-color: blue;
		}

		a {
		  color: white;
		}
	}
}

/* Dikompil kepada: */

ul {
  list-style-type: none;
  margin-top: 2em;
}

ul li {
  background-color: red;
}

ul li:hover {
  background-color: blue;
}

ul li a {
  color: white;
}




```



## SASS atau Sass?
Adakah anda tertanya-tanya sama ada Sass adalah akronim atau tidak? Anda mungkin tidak perlu, tetapi saya akan memberitahu. Nama bahasa ini adalah perkataan, "Sass", dan tidak akronim.
Kerana orang sentiasa menulis ia sebagai "Sass", pencipta bahasa bergurau memanggilnya "Syntactically Awesome StyleSheets".

## Berlatih Sass
Jika anda ingin bermain dengan Sass di pelayar anda, lihat [SassMeister](http://sassmeister.com/).
Anda boleh guna salah satu sintaks, hanya pergi ke tetapan dan pilih sama ada Sass atau SCSS.


## Bacaan lanjut
* [Dokumentasi Rasmi](http://sass-lang.com/documentation/file.SASS_REFERENCE.html)
* [The Sass Way](http://thesassway.com/) menyediakan tutorial (asas-lanjutan) dan artikel.
