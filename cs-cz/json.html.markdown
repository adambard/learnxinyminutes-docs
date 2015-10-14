---
language: json
filename: learnjson.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["himanshu", "https://github.com/himanshu81494"]
  - ["ScheRas", "https://github.com/scheras"]
translators:
  - ["ScheRas", "https://github.com/scheras"]
lang: cs-cz
---

Jelikož je JSON extrémně jednoduchý formát pro výměnu dat, pravděpodobně se bude
jednat o nejjednodušší Learn X in Y minutes všech dob.

JSON ve své nejčistší podobě neobsahuje komentáře, ovšem většina parserů zpracuje
komentáře ve stylu jazyka C ( //. /* */ ). Některé parsery také tolerují čárku za
posledním prvkem pole nebo za posledním atributem objektu, ovšem z důvodu kompatibility
by neměla být uváděna.

Tento dokument však obsahuje pouze 100% validní JSON. Vše by mělo být zcela samovysvětlující. 

Mezi datové typy podporované JSONem patří: čísla, řetězce, logické hodnoty, pole, objekty a null.
Podporované prohlížeče jsou: Firefox(Mozilla) 3.5, Internet Explorer 8, Chrome, Opera 10 a Safari 4.
Soubory obsahující JSON obvykle mají koncovku ".json". Mime type pro JSON je "application/json".
Nevýhodou JSONu je nemožnost definice vlastních typů a neexistence nějakého DTD.

```json
{
  "klíč": "hodnota",

  "klíče": "musí být vždy uzavřeny ve dvojitých uvozovkách",
  "čísla": 0,
  "řetězce": "Hellø, wørld. Všechny unicode znaky jsou povolené, jen některé je třeba \"escapovat\".",
  "má bool?": true,
  "nic": null,

  "velké číslo": 1.2e+100,

  "objekty": {
    "komentář": "Většina struktury bude tvořena objekty.",

    "pole": [0, 1, 2, 3, "Pole mohou obsahovat cokoliv.", 5],

    "další objekt": {
      "komentář": "Všechny věci je možné vnořovat, velice užitečné."
    }
  },

  "hloupost": [
    {
      "zdroj draslíku": ["banány"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "alternativní styl": {
    "komentář": "koukej na tohle!"
  , "pozice čárky": "nezáleží na ni - pokud předchází dalšímu klíči, je vše v pořádku"
  , "další komentář": "jak pěkné"
  },

  "bylo to krátké": "A je hotovo. Nyní víte o všem, co vám JSON může nabídnout."
}
