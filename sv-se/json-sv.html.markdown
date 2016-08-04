---
language: json
filename: learnjson-sv.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["himanshu", "https://github.com/himanshu81494"]
  - ["Michael Neth", "https://github.com/infernocloud"]
translators:
  - ["Lari Kovanen", "https://github.com/larkov"]
  - ["Joakim Lahtinen", "https://github.com/VibyJocke"]
lang: sv-se
---

JSON är ett extremt enkelt datautbytesformat. Som [json.org](http://json.org) beskriver så är det lätt för människor att läsa och skriva, och för datorer att tolka och generera.

En bit av JSON måste representera antingen:
* En samling av namn/värde-par (`{ }`). I olika språk kan denna realiseras som ett objekt, struct, dictionary, hash-tabell, nyckellista eller en associativ array.
* En ordnad lista av värden (`[ ]`). I olika språk kan denna realiseras som en array, vektor, lista eller sekvens.

JSON i dess renaste form har inga kommentarer, men de flesta tolkarna accepterar C-stils (`//`, `/* */`) kommentarer. Vissa tolkar tolererar även komman efter sista elementet i en array, eller det sista attributet av ett objekt, men dessa bör undvikas för bättre kompabilitet.

Detta dokument kommer dock att tillämpa 100% giltigt JSON. Lyckligtvis så är resten av dokumentet självförklarande.

Följande datatyper stöds:
* Strängar: `"hello"`, `"\"A quote.\""`, `"\u0abe"`, `"Newline.\n"`
* Nummer: `23`, `0.11`, `12e10`, `3.141e-10`, `1.23e+4`
* Objekt: `{ "key": "value" }`
* Arrayer: `["Values"]`
* Övriga: `true`, `false`, `null`

```json
{
  "nyckel": "värde",

  "nycklar": "måste alltid omslutas med dubbla citationstecken",
  "nummer": 0,
  "strängar": "Alla unicode-tecken (inklusive \"escaping\") är tillåtna.",
  "boolska värden?": true,
  "nullvärden": null,

  "stora tal": 1.2e+100,

  "objekt": {
    "kommentar": "De flesta datastukturerna i JSON kommer i form av objekt.",

    "matris": [0, 1, 2, 3, "Matriser kan innehålla vad som helst.", 5],

    "ytterligare objekt": {
      "kommentar": "Objekten kan vara nästlade."
    }
  },

  "trams": [
    {
      "kaliumkällor": ["bananer"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "alternativ formatering": {
    "kommentar": "kolla på detta!"
  , "kommats position": "spelar ingen roll - så länge det kommer innan värdet"
  , "en kommentar till": "vad fint"
  },



  "blanksteg": "Spelar ingen roll.",



  "det var kort": "Nu är du klar och kan allt vad JSON har att erbjuda."
}
```

## Fortsatt läsning

* [JSON.org](http://json.org/json-sv.html) Allt du kan tänkas vilja veta om JSON, och lite därtill.
