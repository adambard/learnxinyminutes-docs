---
language: json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["Geoffrey Liu", "https://github.com/g-liu"]
filename: learnjson.json
---

JSON est un format extrêmement simple pour l'échange de donnees. Donc, c'est possible que cela
sera le Learn X in Y Minutes le plus simple jamais.

Dans sa form la plus pure, JSON n'ai pas de commentaires réels, mais la plupart d'analyseurs
accepteront les commentaires C-style (// /\* \*/). Pour les besois de ce tutoriel, cependant,
tout va être en JSON 100% valide. Heureusement, JSON parle pour lui-même.

```json
{
  "clé": "valeur",
  
  "clés": "doit être toujours entre guillemets (simple ou double)",
  "numéros": 0,
  "strings": "Bønjøur, tøut le mønde. Tous les symboles d'unicode est permis, avec \"l'echappement\".",
  "a de bools?": true,
  "néant": null,

  "grand nombre": 1.2e+100,

  "objets": {
    "commentaire": "La plupart de votre structure viendra des objets.",

    "matrice": [0, 1, 2, 3, "Les tableaux peuvent avoir quelque chose en eux.", 5],

    "un autre objet": {
      "commentaire": "Ces choses peuvent être imbriquées, très utile."
    }
  },

  "niaiserie": [
    {
      "des sources de potassium": ["bananes"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "néo"],
      [0, 0, 0, 1]
    ]
  ],
  
  "style alternatif": {
    "commentaire": "regardez ça!"
  , "position de la virgule": "n'a pas d'importance - tant qu'il est avant de la valeur, il est valide"
  , "un autre commentaire": "comment génial"
  },

  "c'était court": "Et, vous avez terminé. Maintenant, vous savez tout que JSON offre."
}
```
