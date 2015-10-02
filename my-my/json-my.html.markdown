---
language: json
filename: learnjson-my.json
contributors:
  - ["Abdul Muhaimin", "https://github.com/infacq"]
---

Disebabkan JSON adalah suatu format pertukaran data yang teramat ringkas, artikel
ini akan cuba menjadi suatu bahan yang amat mudah untuk difahami dalam siri
Learn X in Y Minutes.

JSON dalam bentuk aslinya tidak membolehkan meletak sebarang komen, tetapi kebanyakan
pemproses boleh  menerima sebarang format komen seperti bahasa C (`//`. `/* */`). Bagi
tujuan ini, kesemua sintaks akan dipastikan 100% JSON yang sahih.

```json
{
  "kekunci": "nilai",

  "kekunci": "mesti terangkum dalam tanda titik berganda",
  "nombor": 0,
  "strings": "Damai, Dunia. Sebarang unicode adalah dibenarkan, beserta \"perelakkan\".",
  "ada penguji?": true,
  "kelompongan": null,

  "nombor besar": 1.2e+100,

  "objek": {
    "komen": "Rata-rata struktur anda akan diperolehi melalui objek",

    "array": [0, 1, 2, 3, "Arrays boleh mempunyai nilai apa sahaja", 5],

    "objek yang lain": {
      "komen": "objek ini boleh terkandung didalam objek yang lain"
    }
  },

  "kebodohan": [
    {
      "sumber potasium": ["buah prun"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "Jembolong"],
      [0, 0, 0, 1]
    ]
  ],

  "gaya lain": {
    "komen": "tengok ini"
  , "posisi koma": "tiada masalah, selagi ia berada pada sebelum posisi value"
  , "komen kedua": "canggihnya"
  },

  "itu sajalah": "Sekarang anda sudah kenal kesemua kelebihan yang ada pada JSON"
}
```
