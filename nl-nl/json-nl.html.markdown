---
language: json
filename: learnjson-nl.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["himanshu", "https://github.com/himanshu81494"]
  - ["Maarten Jacobs", "https://github.com/maartenJacobs"]
translators:
  - ["Niels van Velzen", "https://nielsvanvelzen.me"]
lang: nl-nl
---

Gezien JSON een zeer eenvouding formaat heeft zal dit één van de simpelste
Learn X in Y Minutes ooit zijn.

JSON heeft volgens de specificaties geen commentaar. Ondanks dat hebben de
meeste parsers support voor C-stijl (`//`, `/* */`) commentaar.
Sommige parsers staan zelfs trailing komma's toe.
(Een komma na het laatste element in een array of achter de laatste eigenschap van een object).
Het is wel beter om dit soort dingen te vermijden omdat het niet in elke parser zal werken.

In het voorbeeld zal alleen 100% geldige JSON gebruikt worden.

Data types gesupport door JSON zijn: nummers, strings, booleans, arrays, objecten en null.
Gesupporte browsers zijn: Firefox(Mozilla) 3.5, Internet Explorer 8, Chrome, Opera 10, Safari 4.
De extensie voor JSON bestanden is ".json". De MIME type is "application/json".
Enkele nadelen van JSON zijn het gebrek aan type definities en een manier van DTD.

```json
{
  "sleutel": "waarde",

  "sleutels": "zijn altijd in quotes geplaatst",
  "nummers": 0,
  "strings": "Hallø, wereld. Alle unicode karakters zijn toegestaan, samen met \"escaping\".",
  "boolean": true,
  "niks": null,

  "groot nummer": 1.2e+100,

  "objecten": {
    "commentaar": "In JSON gebruik je vooral objecten voor je strutuur",

    "array": [0, 1, 2, 3, "Arrays kunnen alles in zich hebben.", 5],

    "nog een object": {
      "commentaar": "Objecten kunnen genest worden, erg handig."
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
    "commentaar": "Kijk dit!"
  , "De komma positie": "maakt niet uit zolang het er maar is"
  , "nog meer commentaar": "wat leuk"
  },

  "dat was kort": "En nu ben je klaar, dit was alles wat je moet weten over JSON."
}
```
