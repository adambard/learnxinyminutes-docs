---
language: json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["himanshu", "https://github.com/himanshu81494"]
translators:
    - ["Robert Margelli", "http://github.com/sinkswim/"]
    - ["Christian Grasso", "http://chris54721.net"]
lang: it-it
---

JSON è un formato per l'interscambio di dati estremamente semplice, per cui questo sarà
con molta probabilità il più semplice Learn X in Y Minutes.

Nella sua forma più pura JSON non ha commenti, ma molti parser accettano
commenti in stile C (//, /\* \*/). Per lo scopo prefissato, tuttavia, tutto sarà
100% JSON valido. Fortunatamente, si spiega da sè.

I tipi supportati da JSON comprendono: numeri, stringhe, boolean, array, oggetti e null.
I browser supportati sono: Firefox (Mozilla) 3.5+, Internet Explorer 8+, Google Chrome,
Opera 10+, Safari 4+.
I file JSON sono salvati nel formato ".json". Il MIME type per documenti JSON è
"application/json". Gli svantaggi del JSON includono l'assenza di una definizione dei tipi
e di una sorta di [DTD](https://it.wikipedia.org/wiki/Document_Type_Definition).

```json
{
  "chiave": "valore",
  
  "chiavi": "devono sempre essere racchiuse tra doppi apici",
  "numeri": 0,
  "stringhe": "Ciaø, møndø. Tutti i caratteri Unicode sono permessi, insieme all'\"escaping\".",
  "ha booleani?": true,
  "il nulla": null,

  "numero grande": 1.2e+100,

  "oggetti": {
    "commento": "La maggior parte della tua struttura viene dagli oggetti.",

    "array": [0, 1, 2, 3, "Gli array possono contenere qualsiasi cosa.", 5],

    "un altro oggetto": {
      "commento": "Queste cose possono essere annidate, molto utile."
    }
  },

  "sciocchezze": [
    {
      "sorgenti di potassio": ["banane"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],
  
  "stile alternativo": {
    "commento": "Guarda qua!"
  , "posizione della virgola": "non conta - se è prima della chiave successiva, allora è valida"
  , "un altro commento": "che bello"
  },

  "è stato molto breve": "Ed hai finito. Adesso sai tutto cio che JSON ha da offrire."
}
```
