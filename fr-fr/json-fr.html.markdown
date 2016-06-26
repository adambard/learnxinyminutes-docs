---
language: json
filename: learnjson-fr.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
  - ["Alois de Gouvello","https://github.com/aloisdg"]
  - ["Thibault", "https://github.com/napnac"]
lang: fr-fr
---

Comme JSON est un format d'échange de données extrêmement simple, ce Apprendre X en Y minutes
est susceptible d'être le plus simple jamais réalisé.

JSON dans son état le plus pur n'a aucun commentaire, mais la majorité des parseurs accepterons
les commentaires du langage C (`//`, `/* */`). Pour les besoins de ce document, cependant,
tout sera du JSON 100% valide. Heureusement, il s'explique par lui-même.

Une valeur en JSON doit être un nombre, une chaîne de caractère, un tableau, un objet, ou une des trois valeurs suivantes : true, false, null.

JSON est supporté par Firefox 3.5+, Internet Explorer 8.0+, Chrome 1.0+, Opera 10.0+, et Safari 4.0+.

Les fichiers JSON ont pour format ".json".

De nombreux langages de programmation supportent l'encodement et de le décodement de structure de données en JSON. JavaScript par exemple possède un support implicite permettant de manipuler du text JSON en tant que données.

Plus d'informations sur le site officiel : <http://www.json.org/>

Le langage JSON se construit sur deux structures fondamentales :

* Plusieurs paires nom/valeur. Dans plusieurs langages, on peut implémenter cela grâce à un objet, une structure, un dictionnaire, une table de hachage, ou un tableau.
* Une liste ordonnée de valeurs. Cette liste se traduit dans plusieurs langages par un tableau, un vecteur, une liste ou une séquence.

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
  , "la position de la virgule": "n'a pas d'importance - aussi longtemps qu'elle est avant la valeur, elle est valide."
  , "un autre commentaire": "comme c'est gentil"
  },

  "C'était court": {
    "Et, vous avez terminé. Maintenant, vous savez tout ce que JSON a à offrir."
  }
}
```

Un tableau contenant des valeurs est en lui même valide en JSON.

```json
[1, 2, 3, "texte", true]
```

Des objets peuvent aussi faire partie du tableau.

```json
[{"nom": "Bob", "age": 25}, {"nom": "Jane", "age": 29}, {"nom": "Jack", :"age": 31}]
```
