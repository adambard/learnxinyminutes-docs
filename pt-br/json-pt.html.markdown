---
language: json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["Francisco Marques", "https://github.com/ToFran"]
translators:
  - ["Miguel Araújo", "https://github.com/miguelarauj1o"]
lang: pt-br
filename: learnjson-pt.json
---

Como JSON é um formato de intercâmbio de dados, este será, muito provavelmente, o
"Learn X in Y minutes" mais simples existente.

JSON na sua forma mais pura não tem comentários, mas a maioria dos analisadores 
aceitarão comentários no estilo C (//, /\* \*/). No entanto estes devem ser evitados para otimizar a compatibilidade.

Um valor JSON pode ser um numero, uma string, um array, um objeto, um booleano (true, false) ou null.

Os browsers suportados são: Firefox 3.5+, Internet Explorer 8.0+, Chrome 1.0+, Opera 10.0+, e Safari 4.0+.

A extensão dos ficheiros JSON é “.json” e o tipo de mídia de Internet (MIME) é “application/json”.

Mais informação em: http://www.json.org/

```json
{
  "chave": "valor",
  
  "chaves": "deve ser sempre entre aspas (junto ou separado)",
  "números": 0,
  "strings": "Olá, mundo. Todo o padrão UNICODE é permitido, junto com \"escapando\".",
  "possui booleano?": true,
  "nada": null,

  "número grande": 1.2e+100,

  "objetos": {
    "comentário": "A maior parte da sua estrutura virá de objetos.",

    "array": [0, 1, 2, 3, "Arrays podem ter qualquer coisa em si.", 5],

    "outro objeto": {
      "comentário": "Estas coisas podem ser aninhadas, muito úteis."
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

  "estilo alternativo": {
  "comentário": "verificar isso!"
  , "posição da vírgula": "não importa - enquanto é antes do valor, então é válido"
  , "outro comentário": "que bom"
  },

  "que foi curto": "E, você está feito. Você já sabe tudo que JSON tem para oferecer."
}
```
