---
language: json
filename: learnjson.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
  - ["Dragomir Ioan", "https://github.com/ianiD"]
---

Cum JSON este un format de schimb de date extrem de simplu, probabil va fi cel
mai scurt "Învață X în Y minute".

JSON nu are comentarii la nivel fundamental, deși majoritatea interpretoarelor
vor accepta comentarii ca în limbajul C ('//', '/* bla */'). Unele interpretoare
tolerează și virgule care nu sunt urmate de nimic, dar ar trebui să fie evitate
acestea pentru compatibilitate mai sigură.

Pentru motivele acestei 'lecții', totul va fi 100% JSON valabil. Din fericire, acesta
vorbește de la sine.

```json
{
  "cheie": "valoare",

  "chei": "întotdeauna trebuie înconjurate de ghilimele duble",
  "numere": 0,
  "string-uri": "Hellø, wørld. Unicode funcționează, la fel și \"escape sequence-urile\".",
  "are booleeni?": true,
  "nimicenie": null,

  "număr mare": 1.2e+100,

  "obiecte": {
    "comentariu": "O mare parte a structurii vine din obiecte.",

    "array": [0, 1, 2, 3, "Array-urile pot conține orice.", 5],

    "alt obiect": {
      "comentariu": "Acestea pot fi imbricate."
    }
  },

  "aiureli": [
    {
      "surse de potasiu": ["banane"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "stil alternativ": {
    "comentariu": "fii atent!"
  , "poziția virgulei": "nu contează. Cât timp e între termeni funcționează"
  , "alt comentariu": "ce tare!"
  },

  "asta a fost scurt": "Ai terminat. Acum știi tot ce JSON îți pune la dispoziție."
}
```
