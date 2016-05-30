---
language: json
filename: learnjson-ro.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
    - ["Serban Constantin", "https://github.com/fuzzmz"]
lang: ro-ro
---

Deoarece JSON este un fromat foarte simplu de schimb de date acesta va fi
probabil cel mai simplu Invata X in Y minute.

JSON in forma cea mai pura nu contine comentarii insa majoritatea parserelor
vor accepta comentarii in stil C (`//`, `/* */`). Pentru acest caz insa totul
va fi JSON 100% valid. Din fericire codul vorbeste de la sine.

```json
{
  "cheie": "valoare",
  
  "chei": "trebuie mereu inconjurate de ghilimele",
  "numere": 0,
  "stringuri": "Bunã. Tot setul unicode este permis, chiar si \"escaping\".",
  "are booleane?": true,
  "nimic": null,

  "numere mari": 1.2e+100,

  "obiecte": {
    "comentariu": "Majoritatea structurii va veni din obiecte.",

    "vectori": [0, 1, 2, 3, "Vectorii pot avea orice in ei.", 5],

    "alt obiect": {
      "comentariu": "Lucrurile pot fi subordonate. Foarte util."
    }
  },

  "glumite": [
    {
      "surse de potasiu": ["banane"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],
  
  "stil alternativ": {
    "comentariu": "ia uite la asta!"
  , "pozitia virgulei": "nu conteaza - daca e inaintea valorii atunci e valida"
  , "alt comentariu": "ce dragut"
  },

  "a fost scurt": "Am terminat. Acum stii tot ce are JSON de oferit."
}
```
