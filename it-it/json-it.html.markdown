---

language: json
contributors:
  	- ["Anna Harren", "https://github.com/iirelu"]
  	- ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
    - ["Robert Margelli", "http://github.com/sinkswim/"]
lang: it-it

---

Dato che JSON è un formato per lo scambio di dati estremamente semplice, questo sarà con molta probabilità
il più semplice Learn X in Y Minutes.

Nella sua forma più pura JSON non ha commenti, ma molti parser accettano
commenti in stile C (//, /\* \*/). Per lo scopo prefissato, tuttavia, tutto sarà
100% JSON valido. Fortunatamente, si spiega da sè.

```json
{
  "chiave": "valore",
  
  "chiavi": "devono sempre essere racchiuse tra doppi apici",
  "numeri": 0,
  "stringhe": "Ciaø, møndø. Tutti gli unicode sono permessi, assieme con l \"escaping\".",
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
    "commento": "Guarda quà!"
  , "posizione della virgola": "non conta - fintantochè è prima del valore, allora è valida"
  , "un altro commento": "che bello"
  },

  "è stato molto breve": "Ed hai finito. Adesso sai tutto cio che JSON ha da offrire."
}
```
