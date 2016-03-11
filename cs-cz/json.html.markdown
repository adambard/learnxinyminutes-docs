---
language: json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
    - ["Vojta Svoboda", "https://github.com/vojtasvoboda/"]
filename: learnjson-cz.json
lang: cs-cz
---

JSON je exterémně jednoduchý datově nezávislý formát a bude asi jeden z 
nejjednodušších 'Learn X in Y Minutes' ze všech.

JSON nemá ve své nejzákladnější podobě žádné komentáře, ale většina parserů 
umí pracovat s komentáři ve stylu jazyka C (`//`, `/* */`). Pro tyto účely 
však budeme používat 100% validní JSON bez komentářů. Pojďme se podívat na 
syntaxi formátu JSON:

```json
{
  "klic": "value",
  
  "hodnoty": "Musí být vždy uvozený v dvojitých uvozovkách",
  "cisla": 0,
  "retezce": "Hellø, wørld. Všechny unicode znaky jsou povolené, společně s \"escapováním\".",
  "pravdivostni_hodnota": true,
  "prazdna_hodnota": null,

  "velke_cislo": 1.2e+100,

  "objekt": {
    "komentar": "Most of your structure will come from objects.",

    "pole": [0, 1, 2, 3, "Pole nemusí být pouze homogenní.", 5],

    "jiny_objekt": {
      "comment": "Je povolené jakkoli hluboké zanoření."
    }
  },

  "cokoli": [
    {
      "zdroje_drasliku": ["banány"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],
  
  "alternativni_styl_zapisu": {
    "komentar": "Mrkni se na toto!"
  , "pozice_carky": "Na pozici čárky nezáleží - pokud je před hodnotou, ať už je kdekoli, tak je validní."
  , "dalsi_komentar": "To je skvělé."
  },

  "to_bylo_rychle": "A tím jsme hotový. Nyní již víte vše, co může formát JSON nabídnout!"
}
```
