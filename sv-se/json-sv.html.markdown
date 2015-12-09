---
language: json
filename: learnjson-sv.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
  - ["Lari Kovanen", "https://github.com/larkov"]
lang: sv-se
---

Eftersom JSON är ett extremt lätt data-utbytes format så kommer detta
förmodligen att vara den lättaste "Learn X in Y Minutes" någonsin.

JSON i dess renaste form har inga kommentarer, men de flesta tolkarna accepterar
C-stils (`//`, `/* */`) kommentarer. Detta dokument kommer dock att tillämpa
100% giltigt JSON. Lyckligtvis så är resten av dokumentet självförklarande.


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

  "det var kort": "Nu är du klar och kan allt vad JSON har att erbjuda."
}
```
