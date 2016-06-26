---
language: json
filename: learnjson-id.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
  - ["Rizky Luthfianto", "https://github.com/rilut"]
lang: id-id
---

JSON adalah format pertukaran data yang sangat simpel, kemungkinan besar,
ini adalah "Learn X in Y Minutes" yang paling singkat.

Murninya, JSON tidak mempunyai fitur komentar, tapi kebanyakan parser akan
menerima komentar bergaya bahasa C (`//`, `/* */`). Namun, pada halaman ini,
hanya dicontohkan JSON yang 100% valid.

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
