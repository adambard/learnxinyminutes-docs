---
language: json
filename: learnjson-es.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
  - ["Daniel Zendejas","https://github.com/DanielZendejas"]
lang: es-es
---

Siendo JSON un formato de intercambio de infomación tan sencillo, probablemente este será el Learn X in Y más sencillo jamás.

JSON en su forma más pura no tiene comentarios, pero la mayoría de los parseadores aceptarán comentarios de C (//, /\* \*/). De todas formas, para el propóstio de esto todo será JSON 100% válido. Por suerte, habla por sí mismo.

```json

{
  "llave": "valor",
  
  "llaves": "siempre debe estar entre comillas (ya sean dobles o simples)",
  "numeros": 0,
  "strings": "Høla, múndo. Todo el unicode está permitido, así como \"escapar\".",
  "¿soporta booleanos?": true,
  "vacíos": null,

  "numero grande": 1.2e+100,

  "objetos": {
    "comentario": "La mayoría de tu estructura vendrá de objetos.",

    "arreglo": [0, 1, 2, 3, "Los arreglos pueden contener cualquier cosa.", 5],

    "otro objeto": {
      "comentario": "Estas cosas pueden estar anidadas, muy útil."
    }
  },

  "tontería": [
    {
      "fuentes de potasio": ["bananas"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],
  
  "estilo alternativo": {
    "comentario": "Mira esto!"
  , "posición de la coma": "no importa - mientras este antes del valor, entonces sera válido"
  , "otro comentario": "qué lindo"
  },

  "eso fue rapido": "Y, estás listo. Ahora sabes todo lo que JSON tiene para ofrecer."
}
```
