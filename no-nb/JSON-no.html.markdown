--- 
language: json
filename: learnjson-no.json
lang: no-nb
contributors:
  - ["Ole Mathias Heggem", "https://github.com/msbone"]
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
---

JSON er en enkel tekstbasert standard for datautveksling.
Den er opprinnelig avledet fra JavaScript for å representere enkle datastrukturer.
Standarden er imidlertid uavhengig av JavaScript eller andre programmeringsspråk.

JSON i sin reneste form har ingen faktiske kommentarer, men de fleste parsere vil akseptere
C-stil (`//`, `/* */`) kommentarer.

```json
{
  "nøkkel": "verdi",
  
  "nøkler": "må alltid være i doble anførselstegn",
  "tall": 0,
  "strings": "Hellø, wørld. Alt unicode er godkjent, også \"escaping\".",
  "har bools?": true,
  "ingenting": null,

  "stort tall": 1.2e+100,

  "objekt": {
    "kommentar": "Meste av strukturen kommer ifra objekt.",

    "array": [0, 1, 2, 3, "Arrays kan inneholde alt.", 5],

    "nytt object": {
      "comment": "Ny kommentar"
    }
  },

  "tull": [
    {
      "Kilde til Kalium": ["bananer"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],
  
  "Alternativ": {
    "Kommentar": "Sjekk ut ditta!"
  , "plassering av komma": "Sålenge den er før verdien er det gyldig"
  , "Enda en kommentar": "TØFT!"
  },

  "Ferdig": "Da er den korte innledninga til JSON ferdig"
}
```
