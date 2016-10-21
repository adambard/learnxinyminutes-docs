---
language: json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
filename: learnjson-id.json
translators:
  - ["Rizky Luthfianto", "https://github.com/rilut"]
  - ["Ahmad Zafrullah", "https://github.com/23Pstars"]
lang: id-id
---

JSON adalah format pertukaran data yang sangat sederhana. Sebagaimana dikutip dari [json.org](http://json.org), JSON mudah untuk dibaca atau ditulis oleh manusia, dan mudah diuraikan dan diproses oleh mesin.

Sebuah format JSON setidaknya memiliki:
* Sebuah pasangan nama atau nilai dinyatakan dengan karakter (`{ }`). Dibeberapa bahasa pemrograman, karakter ini sering digunakan sebagai object, record, struct, dictionary, hash table, keyed list, atau associative array.
* Daftar nilai dinyatakan dengan karakter (`[ ]`). Dibeberapa bahasa pemrograman, karakter ini sering digunakan sebagai array, vector, list, atau sequence.

Format JSON murni tidak memiliki komentar, namun beberapa pengurai (parser) dapat mengenali komentar seperti yang digunakan oleh bahasa C (`//`, `/**/`). Beberapa pengurai lainnya juga memiliki toleransi terhadap akhiran sisa koma (seperti koma yang terdapat pada akhir elemen dari larik atau properti terakhir dari objek), tapi koma tersebut memang seharusnya diabaikan untuk dukungan yang lebih baik.

Dalam tutorial ini, semuanya menggunakan format JSON murni.

Tipe data yang didukung oleh JSON:

* Teks: `"halo"`, `"\"tanda petik.\""`, `"\u0abe"`, `"baris baru.\n"`
* Angka: `23`, `0.11`, `12e10`, `3.141e-10`, `1.23e+4`
* Objek: `{ "kunci": "nilai" }`
* Larik: `["nilai"]`
* Lainnya: `true`, `false`, `null`

```json
{
  "kunci": "nilai",
  
  "kunci": "harus selalu diapit tanda kutip",
  "angka": 0,
  "strings": "Halø, dunia. Semua karaktor unicode diperbolehkan, terumasuk \"escaping\".",
  "punya tipe data boolean?": true,
  "nilai kosong": null,

  "angka besar": 1.2e+100,

  "obyek": {
    "komentar": "Most of your structure will come from objects.",

    "array": [0, 1, 2, 3, "Array bisa berisi apapun.", 5],

    "obyek lainnya": {
      "komentar": "Obyek-obyek JSON dapat dibuat bersarang, sangat berguna."
    }
  },

  "iseng-iseng": [
    {
      "sumber potassium": ["pisang"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],
  
  "gaya alternatif": {
    "komentar": "lihat ini!"
  , "posisi tanda koma": "tak masalah. selama sebelum nilai berikutnya, valid-valid saja"
  , "komentar lainnya": "betapa asyiknya"
  },

  "singkat": "Dan Anda selesai! Sekarang Anda tahu apa saja yang disediakan oleh JSON."
}
```

## Referensi lebih labjut

* [JSON.org](http://json.org/json-id.html) semua keindahan JSON dijelaskan dalam bentuk alur-grafis (bahasa indonesia).
