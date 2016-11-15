---
language: json
filename: learnjson-de.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
  - ["Timm Albers", "https://github.com/nunull"]
lang: de-de
---

Da JSON ein äußerst einfaches Format für den Austausch von Daten ist, wird dieses
Dokument das vermutlich einfachste "Learn X in Y Minutes" werden.

In seiner grundlegenden Form hat JSON keine eigentlichen Kommentare. Dennoch
akzeptieren die meisten Parser Kommentare in C-Syntax (`//`, `/* */`). Dennoch
soll für dieses Dokument nur 100% gültiges JSON verwendet werden, weshalbt keine
Kommentare verwendet werden. Glücklicherweise ist das nachfolgende Dokument
selbsterklärend.

```json
{
  "schlüssel": "wert",
  
  "alle schlüssel": "müssen durch doppelte Anführungszeichen begrenzt werden",
  "zahlen": 0,
  "zeichenketten": "Alle Unicode-Zeichen (inklusive \"escaping\") sind erlaubt.",
  "boolesche werte": true,
  "nullwert": null,

  "große zahlen": 1.2e+100,

  "objekte": {
    "kommentar": "Die meisten Datenstrukturen in JSON kommen aus Objekten.",

    "array": [0, 1, 2, "Arrays können Werte jeglichen Datentyps aufnehmen.", 4],

    "weiteres objekt": {
      "kommentar": "Objekte können verschachtelt werden."
    }
  },

  "quatsch": [
    {
      "quellen von kalium": ["Bananen"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "Neo"],
      [0, 0, 0, 1]
    ]
  ],
  
  "alternative formatierung": {
    "kommentar": "..."
  , "die position": "des Kommas ist nicht relevant - so lange es vor dem Wert steht."
  , "weiterer kommentar": "wie schön"
  , "übrigens": "Auch die Einrückung ist nicht relevant." 
  , "jede": "beliebige Anzahl von Leerzeichen / Tabs ist erlaubt.", "wirklich?":true
  },

  "das war kurz": "Und, du bist fertig. Du weißt nun (fast) alles über JSON."
}
```
