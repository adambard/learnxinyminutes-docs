---
language: json
filename: learnjson-sk.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
  - ["Juraj Kostolanský", "http://www.kostolansky.sk"]
lang: sk-sk
---

Nakoľko je JSON extrémne jednoduchý formát na výmenu dát, toto bude
pravdepodobne najjednoduchšie "Learn X in Y Minutes".

JSON v jeho základnej forme nemá komentáre, ale veľa parserov akceptuje
komentáre v štýle C (`//`, `/* */`). V tomto návode však bude všetko
100% valídny JSON.

```json
{
  "kľúč": "hodnota",

  "kľúč": "musí byť vždy uzavretý v dvojitých uvodzovkách",
  "čísla": 0,
  "reťazce": "Ahøj, svet. Unicode je povolený pri použití \"únikovej sekvencie (escaping)\".",
  "boolean?": true,
  "nič": null,

  "veľké číslo": 1.2e+100,

  "objekty": {
    "komentár": "Väčšina štruktúry bude pochádzať z objektov.",

    "pole": [0, 1, 2, 3, "Pole môže obsahovať čokoľvek.", 5],

    "iný objekt": {
      "komentár": "Môžu byť vhniezdené, čo môže byť užitočné."
    }
  },

  "nezmysly": [
    {
      "zdroje draslíka": ["banány"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "alternatívny štýl": {
    "komentár": "sleduj toto!"
  , "pozícia čiarky": "nezáleží na nej - pokiaľ je pred hodnotou, všetko je ok"
  , "iný komentár": "pekné, že?"
  },

  "to bolo rýchle": "A už sme aj na konci. Teraz ovládš všetko, čo ti JSON môže ponúknuť."
}
```
