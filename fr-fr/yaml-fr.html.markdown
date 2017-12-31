---
language: yaml
filename: learnyaml-fr.yaml
contributors:
  - ["Andrei Curelaru", "http://www.infinidad.fr"]
lang: fr-fr
---

Proposé à l'origine par Clark Evans en Mai 2001, YAML est un un format de 
représentation de données par sérialisation, conçu pour être aisément 
modifiable et lisible par nous-mêmes, les humains.

YAML est plus concis que le XML auquel il est parfois comparé par ceux qui le
découvre, plus lisible et clair que le CSV, et emprunte beaucoup au JSON dont
il est un parent naturel. Toutefois, YAML emprunte également des idées et
concepts de Python, et s'intègre bien avec bon nombre de langages.
Contrairement à ce dernier, YAML interdit l'utilisation des tabulations.


```yaml
# Les commentaires sont précédés d'un signe "#", comme cette ligne.

#############
# SCALAIRES #
#############

# Les scalaires sont l'ensemble des types YAML qui ne sont pas des collections
# (listes ou tableaux associatifs).

# Notre objet root (racine), sera une map (carte) et englobera
# l'intégralité du document. Cette map est l'équivalent d'un dictionnaire,
# hash ou objet dans d'autres langages.
clé: valeur
autre_clé: une autre valeur
valeur_numérique: 100
notation_scientifique: 1e+12
booléen: true
valeur_null: null
clé avec espaces: valeur
# Bien qu'il ne soit pas nécessaire de mettre les chaînes de caractères
# entre guillemets, cela reste possible, et parfois utile.
toutefois: "Une chaîne, peut être contenue entre guillemets."
"Une clé entre guillemets.": "Utile si l'on veut utiliser ':' dans la clé."

# Les chaînes couvrant plusieurs lignes, peuvent être écrites au choix,
# comme un "bloc littéral" (avec '|') ou bien un "bloc replié" (avec '>').
bloc_littéral: |
    Tout ce bloc de texte sera la valeur de la clé "bloc_littéral", 
    avec préservation des retours à la ligne.

    Le littéral continue jusqu'à ce que l'indentation soit annulée.

        Toutes lignes qui seraient "davantage indentées" conservent leur
        indentation, constituée de 4 espaces.
bloc_replié: >
    Tout ce bloc de texte sera la valeur de la clé "bloc_replié", mais 
    cette fois-ci, toutes les nouvelles lignes deviendront un simple espace.

    Les lignes vides, comme ci-dessus, seront converties en caractère de
    nouvelle ligne.

        Les lignes "plus-indentées" gardent leurs retours à la ligne -
        ce texte apparaîtra sur deux lignes.

###############
# COLLECTIONS #
###############

# L'imbrication est créée par indentation.
une_map_imbriquée:
    clé: valeur
    autre_clé: autre valeur
    autre_map_imbriquée:
        bonjour: bonjour

# Les clés des maps ne sont pas nécessairement des chaînes de caractères.
0.25: une clé de type flottant

# Les clés peuvent également être des objets s'étendant sur plusieurs lignes,
# en utilisant le signe "?" pour indiquer le début de la clé.
? |
    ceci est une clé
    sur de multiples lignes
: et ceci est sa valeur

# YAML autorise aussi l'usage des collections à l'intérieur des clés,
# mais certains langages de programmation ne le tolère pas si bien.

# Les séquences (équivalent des listes ou tableaux) ressemblent à cela :
une_séquence:
    - Objet 1
    - Objet 2
    - 0.5 # les séquences peuvent contenir des types variés.
    - Objet 4
    - clé: valeur
      autre_clé: autre_valeur
    -
        - Ceci est une séquence
        - dans une autre séquence

# YAML étant un proche parent de JSON, vous pouvez écrire directement
# des maps et séquences façon JSON
json_map: {"clé": "valeur"}
json_seq: [1, 2, 3, "soleil"]

################################
# AUTRES FONCTIONNALITÉES YAML #
################################

# YAML possède une fonctionnalité fort utile nommée "ancres". Celle-ci
# vous permet de dupliquer aisément du contenu au sein de votre document.

# Les deux clés suivantes auront la même valeur :
contenu_ancré: &nom_ancre Cette chaîne sera la valeur des deux clés.
autre_ancre: *nom_ancre

# Avec les tags YAML, vous pouvez explicitement déclarer des types de données.
chaine_explicite: !!str 0.5

# Certains analyseurs syntaxiques (parsers) implémentent des tags spécifiques à
# d'autres langages, comme par exemple celui des nombres complexes de Python.
python_complex_number: !!python/complex 1+2j

#####################
# AUTRES TYPES YAML #
#####################

# YAML interprète également les données formatées ISO de type date et datetime,
# pas seulement les chaînes et nombres.
datetime: 2001-12-15T02:59:43.1Z
datetime_avec_espaces: 2001-12-14 21:59:43.10 -5
date: 2002-12-14

# Le tag !!binary indique que la chaîne à suivre est la représentation binaire
# d'un blob encodé en base64. En clair ? Une image !
fichier_gif: !!binary |
    R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
    OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
    +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
    AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=

# YAML a de même un type "set", semblable à ceci :
set:
    ? item1
    ? item2
    ? item3

# Comme dans Python, les sets ne sont que des maps contenant des valeurs null ;
# le set précédent est l'équivalent du suivant :
set2:
    item1: null
    item2: null
    item3: null

```

Quelques références et outils :

- Documentation officielle [YAML 1.2](http://www.yaml.org/spec/1.2/spec.html) *anglais*,
- Une [Introduction à YAML](http://sweetohm.net/html/introduction-yaml.html) très bien construite et claire,
- Un outil pour tester [en ligne](http://yaml-online-parser.appspot.com/) la syntaxe YAML, avec des exemples.
