---
language: PCRE
filename: pcre-fr.txt
contributors:
    - ["Sachin Divekar", "http://github.com/ssd532"]
translators:
    - ["Th3G33k", "https://github.com/Th3G33k"]
lang: fr-fr

---

Une expression régulière (regex ou regexp en abrégé) est une chaîne de texte spéciale pour décrire un masque de recherche. Par exemple, pour extraire le protocole d'une URL, nous pouvons dire `/^[a-z]+:/` et cela correspondra à `http:` à partir de `http://github.com/`.

PCRE (Perl Compatible Regular Expressions) est une bibliothèque C implémentant les expressions régulières. Elle a été écrite en 1997, à l'époque où Perl était le choix de facto pour les tâches complexes de traitement de texte. La syntaxe utilisée dans PCRE ressemble beaucoup à Perl, et est utilisée dans de nombreux grands projets, notamment PHP, Apache et R, pour n'en citer que quelques-uns.

Il existe deux sortes de méta-caractères :

* Ceux reconnus n'importe où dans le masque, sauf entre crochets, sont :

```
  \      caractère d'échappement général ayant plusieurs usages
  ^      définit le début de la chaîne (ou de la ligne, en mode multiligne)
  $      définit la fin de la chaîne (ou de la ligne, en mode multiligne)
  .      correspond à n'importe quel caractère à l'exception de la nouvelle ligne (par défaut)
  [      début de définition d'une classe de caractères
  |      début d'une branche alternative
  (      début d'un sous-masque
  )      fin d'un sous-masque
  ?      étend le sens de (
         et aussi : quantificateur 0 ou 1
         et aussi : quantificateur de minimisation (fait correspondre un sous-masque avec le moins de caractères)
  *      quantificateur 0 ou plus
  +      quantificateur 1 ou plus
         et aussi : quantificateur possessive (fait correspondre un sous-masque avec le plus de caractères)
  {      début d'un quantificateur min/max
```

* Ceux reconnus entre crochets, en dehors des crochets, et appelées également classes de caractères, sont :

```
  \      caractère d'échappement général
  ^      négation de la classe, mais uniquement si placé au tout début de la classe
  -      indique une plage de caractères
  [      classe de caractères POSIX (uniquement si elle est suivie d'une syntaxe POSIX)
  ]      termine la classe de caractères
```

PCRE fournit des types de caractères génériques, également appelés classes de caractères.

```
  \d     tout chiffre décimal
  \D     tout sauf un chiffre décimal
  \h     tout caractère d'espace horizontal blanc
  \H     tout sauf un caractère d'espace horizontal blanc
  \s     tout caractère d'espace blanc
  \S     tout sauf un caractère d'espace blanc
  \v     tout caractère d'espace vertical blanc
  \V     tout sauf un caractère d'espace vertical blanc
  \w     tout caractère pouvant former un mot (lettre, chiffre, underscore)
  \W     tout sauf un caractère pouvant former un mot (lettre, chiffre, underscore)
``

## Exemples

Nous allons tester nos exemples sur la chaîne sujet suivante :

```
66.249.64.13 - - [18/Sep/2004:11:07:48 +1000] "GET /robots.txt HTTP/1.0" 200 468 "-" "Googlebot/2.1"
```

Il s'agit d'un log d'accès Apache standard.

| Regex | Résultat          | Commentaire |
| :---- | :-------------- | :------ |
| `GET`   | GET | `GET` correspond à `GET` littéralement (sensible à la casse) |
| `\d+.\d+.\d+.\d+` | 66.249.64.13 | `\d+` correspond à un chiffre [0-9] une ou plusieurs fois, définit par le quantificateur `+`, `\.` correspond à `.` littéralement |
| `(\d+\.){3}\d+` | 66.249.64.13 | `(\d+\.){3}` essaye de faire correspondre le sous-masque (`\d+\.`) exactement trois fois. |
| `\[.+\]` | [18/Sep/2004:11:07:48 +1000] | `.+` fait correspondre tout caractère (sauf la nouvelle ligne), plusieurs fois, `.` remplace n'importe quel caractère |
| `^\S+` | 66.249.64.13 | `^` correspond au début de la ligne, `\S+` correspond à n'importe quel nombre de caractère non-vide |
| `\+[0-9]+` | +1000 | `\+` correspond au caractère `+` littéralement. `[0-9]` classe de caractère, faisant correspondre un seul chiffre. La même chose peut être obtenu en utilisant `\+\d+` |

## Lectures complémentaires
[Regex101](https://regex101.com/) - Regular Expression tester et debugger
