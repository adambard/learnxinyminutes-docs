---
language: json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
translators:
  - ["Miguel Araújo", "https://github.com/miguelarauj1o"]
lang: pt-br
filename: learnjson-pt.json
---

Como JSON é um formato de intercâmbio de dados, este será, muito provavelmente, o
"Learn X in Y minutes" mais simples existente.

JSON na sua forma mais pura não tem comentários em reais, mas a maioria dos analisadores 
aceitarão comentários no estilo C (//, /\* \*/). Para os fins do presente, no entanto, 
tudo o que é vai ser 100% JSON válido. Felizmente, isso meio que fala por si.


```json
{
  "números": 0,
  "strings": "Olá, mundo. Todo o padrão UNICODE é permitido, junto com \"escapando\".",
  "possui booleano?": true,
  "nada": null,

  "número grande": 1.2e+100,

  "objetos": {
    "comentário": "A maior parte da sua estrutura virá de objetos.",

    "array": [0, 1, 2, 3, "Arrays podem ter qualquer coisa em si.", 5],

    "outro objeto": {
      "ccomentário": "Estas coisas podem ser aninhadas, muito úteis."
    }
  },

  "tolice": [
    {
      "fonte de potássio": ["bananas"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "que foi curto": "E, você está feito. Você já sabe tudo que JSON tem para oferecer.".
}
```
