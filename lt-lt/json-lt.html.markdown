---
language: json
filename: learnjson-lt.json
lang: lt-lt
contributors:
  - ["Zygimantus", "https://github.com/zygimantus"]
---

JSON („džeisonas“) yra itin paprastas duomenų mainų formatas, todėl tai bus pati lengviausia „Learn X in Y Minutes“ pamoka.

JSON savo gryniausioje formoje neturi jokių komentarų, tačiau dauguma analizatorių priimtų C stiliaus komentarus (`//`, `/* */`). Kai kurie analizatoriai taip pat toleruoja gale esantį kablelį, pvz., kablelis po kiekvieno masyvo paskutinio elemento arba po paskutinio objekto lauko, tačiau jų reikėtų vengti dėl geresnio suderinamumo.

JSON reikšmė privalo būti skaičius, eilutė, masyvas, objektas arba viena reikšmė iš šių: true, false, null.

Palaikančios naršyklės yra: Firefox 3.5+, Internet Explorer 8.0+, Chrome 1.0+, Opera 10.0+, and Safari 4.0+.

Failo plėtinys JSON failams yra „.json“, o MIME tipas yra „application/json“.

Dauguma programavimo kalbų palaiko JSON duomenų serializaciją (kodavimą) ir deserializaciją (dekodavimą) į natyviasias duomenų struktūras. Javascript turi visišką JSON teksto kaip duomenų manipuliavimo palaikymą.

Daugiau informacijos galima rasti http://www.json.org/

JSON yra pastatytas iš dviejų struktūrų:
* Vardų/reikšmių porų rinkinys. Daugomoje kalbų, tai yra realizuojama kaip objektas, įrašas, struktūra, žodynas, hash lentelė, sąrašas su raktais arba asociatyvusis masyvas.
* Rūšiuotas reikšmių sąrašas. Daugumoje kalbų, toks sąrašas yra realizuojama kaip masyvas, vektorius, sąrašas arba seka.

Objektas su įvairiomis vardo/reikšmės poromis.

```json
{
  "raktas": "reikšmė",

  "raktai": "privalo visada būti uždaryti dvigubomis kabutėmis",
  "skaičiai": 0,
  "eilutės": "Labas, pasauli. Visas unikodas yra leidžiamas, kartu su  \"vengimu\".",
  "turi logiką?": true,
  "niekas": null,

  "didelis skaičius": 1.2e+100,

  "objektai": {
    "komentaras": "Dauguma tavo struktūrų ateis iš objektų.",

    "masyvas": [0, 1, 2, 3, "Masyvas gali turėti bet ką savyje.", 5],

    "kitas objektas": {
      "komentaras": "Šie dalykai gali būti įdedami naudingai."
    }
  },

  "kvailumas": [
    {
      "kalio šaltiniai": ["bananai"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "alternativus stilius": {
    "komentaras": "tik pažiūrėk!"
  , "kablelio padėti": "nesvarbi - kol jis prieš kitą raktą, tada teisingas"
  , "kitas komentaras": "kaip gražu"
  }
}
```

Paprastas reikšmių masyvas pats savaime yra galiojantis JSON.

```json
[1, 2, 3, "tekstas", true]
```

Objektai taip pat gali būti masyvų dalis.

```json
[{"vardas": "Jonas", "amžius": 25}, {"vardas": "Eglė", "amžius": 29}, {"vardas": "Petras", "amžius": 31}]
```
