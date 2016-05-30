---
language: json
filename: learnjson-ms.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["himanshu", "https://github.com/himanshu81494"]
  - ["Michael Neth", "https://github.com/infernocloud"]
translators:
    - ["abdalim", "https://github.com/abdalim"]
lang: ms-my
---

Disebabkan JSON adalah format pertukaran-data yang sangat ringkas, panduan ini
kemungkinan besar adalah Learn X in Y Minutes yang paling mudah.

JSON dalam bentuk paling aslinya sebenarnya tidak mempunyai sebarang komentar,
tetapi kebanyakan pembaca menerima komen dalam bentuk C (`\\`,`/* */`). Beberapa
pembaca juga bertoleransi terhadap koma terakhir (iaitu koma selepas elemen
terakhir di dalam array atau selepas ciri terakhir sesuatu objek), tetapi semua
ini harus dielakkan dan dijauhkan untuk keserasian yang lebih baik.

Untuk tujuan ini bagaimanapun, semua di dalam panduan ini adalah 100% JSON yang
sah. Luckily, it kind of speaks for itself.

Sebuah nilai JSON harus terdiri dari salah satu, iaitu, nombor, string, array,
objek atau salah satu dari nama literal berikut: true, false, null.

Pelayar web yang menyokong adalah: Firefox 3.5+, Internet Explorer 8.0+, Chrome
1.0+, Opera 10.0+, dan Safari 4.0+.

Sambungan fail untuk fail - fail JSON adalah ".json" dan jenis MIME untuk teks
JSON adalah "application/json".

Banyak bahasa aturcara mempunyai fungsi untuk menyirikan (mengekod) dan
menyah-sirikan (men-dekod) data JSON kepada struktur data asal. Javascript
mempunyai sokongon tersirat untuk memanipulasi teks JSON sebagai data.

Maklumat lebih lanjut boleh dijumpai di http://www.json.org/

JSON dibina pada dua struktur:
* Sebuah koleksi pasangan nama/nilai. Di dalam pelbagai bahasa aturcara, ini
direalisasikan sebagai objek, rekod, "struct", "dictionary", "hash table",
senarai berkunci, atau "associative array".
* Sebuah senarai nilai yang tersusun. Dalam kebanyakan bahasa aturcara, ini
direalisasikan sebagai array, vektor, senarai atau urutan.

Sebuah objek dengan pelbagai pasangan nama/nilai.

```json
{
  "kunci": "nilai",

  "kekunci": "harus sentiasa dibalut dengan 'double quotes'",
  "nombor": 0,
  "strings": "Hellø, wørld. Semua unicode dibenarkan, bersama \"escaping\".",
  "ada bools?": true,
  "tiada apa - apa": null,

  "nombor besar": 1.2e+100,

  "objek": {
    "komen": "Sebahagian besar struktur akan terdiri daripada objek.",

    "array": [0, 1, 2, 3, "Array boleh mempunyai sebarang jenis data di dalamnya.", 5],

    "objek lain": {
      "komen": "Objek boleh dibina dengan pelbagai lapisan, sangat berguna."
    }
  },

  "kebendulan": [
    {
      "punca potassium": ["pisang"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "stail alternatif": {
    "komen": "cuba lihat ini!"
  , "posisi koma": "tidak mengapa - selagi ia adalah sebelum nama atau kunci seterusnya, maka ia sah"
  , "komen lain": "sungguh bagus"
  }
}
```

Sebuah array sahaja yang mengandungi nilai - nilai juga adalah JSON yang sah.

```json
[1, 2, 3, "text", true]
```

Objek - objek boleh menjadi sebahagian dari array juga.

```json
[{"nama": "Abe", "umur": 25}, {"nama": "Jemah", "umur": 29}, {"name": "Yob", "umur": 31}]
```
