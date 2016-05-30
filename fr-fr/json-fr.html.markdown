---
language: json
filename: learnjson-fr.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
  - ["Alois de Gouvello","https://github.com/aloisdg"]
lang: fr-fr
---

Comme JSON est un format d'échange de données extrêmement simple, ce Apprendre X en Y minutes
est susceptible d'être le plus simple jamais réalisé.

JSON dans son état le plus pur n'a aucun commentaire, mais la majorité des parseurs accepterons
les commentaires du langage C (`//`, `/* */`). Pour les besoins de ce document, cependant,
tout sera du JSON 100% valide. Heureusement, il s'explique par lui-même.


```json
{
  "Clé": "valeur",
  
  "Clés": "devront toujours être entourées par des guillemets",
  "nombres": 0,
  "chaînes de caractères": "Hellø, wørld. Tous les caractères Unicode sont autorisés, accompagné d'un \"caractère d'échappement\".",
  "a des booléens ?": true,
  "rien": null,

  "grand nombre": 1.2e+100,

  "objets": {
    "commentaire": "La majorité de votre strucutre sera des objets.",

    "tableau": [0, 1, 2, 3, "Les tableaux peuvent contenir n'importe quoi.", 5],

    "un autre objet": {
      "commentaire": "Ces choses peuvent être imbriquées. C'est très utile."
    }
  },

  "bêtises": [
    {
      "sources de potassium": ["bananes"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],
  
  "style alternatif": {
    "commentaire": "regarde ça !"
  , "position de la virgule": "n'a pas d'importance - aussi longtemps qu'elle est avant la valeur, alors elle est valide."
  , "un autre commentaire": "comme c'est gentil"
  },

  "C'était court": "Et, vous avez terminé. Maintenant, vous savez tout ce que JSON a à offrir."
}
```
