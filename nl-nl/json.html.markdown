---
language: json
filename: learnjson-nl.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
  - ["Mathieu De Coster", "https://github.com/m-decoster"]
lang: nl-nl
---

Aangezien JSON een extreem eenvoudig datauitwisselingsformaat is, zal dit waarschijnlijk
de meest eenvoudige Learn X in Y Minutes ooit zijn.

Puur JSON heeft geen commentaar, maar de meeste parsers zullen commentaar in de stijl
van C (`//`, `/* */`) aanvaarden. In dit voorbeeld zal alles 100% correcte JSON zijn.
Gelukkig spreekt het meeste voor zichzelf.

```json
{
  "key": "value",

  "keys": "moeten altijd tussen dubbele aanhalingstekens staan",
  "getallen": 0,
  "strings": "Hellø, world. Alle Unicode-karakters zijn toegelaten, zo ook \"escaping\".",
  "heeft json booleans?": true,
  "niets": null,

  "groot getal": 1.2e+100,

  "objecten": {
    "commentaar": "De meeste structuur wordt gemaakt met objecten.",

    "array": [0, 1, 2, 3, "Arrays kunnen eender wat bevatten.", 5],

    "nog een object": {
      "commentaar": "Hoe handig, we kunnen objecten nesten."
    }
  },

  "dwaasheid": [
    {
      "bronnen van kalium": ["bananen"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "alternatieve stijl": {
    "commentaar": "kijk hier eens naar!"
  , "komma locatie": "maakt niet uit - zo lang het voor de value komt, is alles in orde"
  , "nog commentaar": "hoe leuk"
  },

  "dat was kort": "Je bent klaar. Je kent nu alles dat JSON kan aanbieden."
}
```
