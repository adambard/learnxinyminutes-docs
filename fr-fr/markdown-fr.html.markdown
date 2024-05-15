---
language: markdown
contributors:
- ["Andrei Curelaru", "http://www.infinidad.fr"]
filename: markdown-fr.md
lang: fr-fr
---

Markdown a été créé par John Gruber en 2004. Il se veut être d'une syntaxe
facile à lire et à écrire, aisément convertible en HTML (et dans beaucoup
d'autres formats aussi).

Les implémentations du Markdown varient d'un analyseur syntaxique à un autre.
Ce guide va essayer de clarifier quand une fonctionnalité est universelle ou
quand elle est specifique à un certain analyseur syntaxique.

## Balises HTML

Markdown est un sur-ensemble du HTML, donc tout fichier HTML est un ficher
Markdown valide.

```md
<!-- Ce qui veut dire que vous pouvez utiliser des balises HTML dans un fichier
Markdown, comme la balise commentaire dans laquelle nous sommes à présent, car
celle-ci ne sera pas affectée par l'analyseur syntaxique du Markdown.
Toutefois, si vous voulez créer une balise HTML dans un fichier Markdown,
vous ne pourrez pas utiliser du Markdown à l'intérieur de cette derniere. -->
```

## En-têtes

Vous pouvez facilement créer des balises HTML `<h1>` à `<h6>` en précédant le
texte de votre futur titre par un ou plusieurs dièses ( # ), de un à six, selon
le niveau de titre souhaité.

```md
# Ceci est un <h1>
## Ceci est un <h2>
### Ceci est un <h3>
#### Ceci est un <h4>
##### Ceci est un <h5>
###### Ceci est un <h6>
```

Markdown fournit également une façon alternative de marquer les `<h1>` et `<h2>`

```md
Ceci est un h1
=============

Ceci est un h2
-------------
```

## Styles de texte basiques

On peut facilement rendre un texte "gras" ou "italique" en Markdown.

```md
*Ce texte est en italique.*
_Celui-ci aussi._

**Ce texte est en gras.**
__Celui-là aussi.__

***Ce texte a les deux styles.***
**_Pareil ici_**
*__Et là!__*
```

Dans le "GitHub Flavored Markdown", utilisé pour interpréter le Markdown sur
GitHub, on a également le texte barré.

```md
~~Ce texte est barré.~~
```

## Paragraphes

Les paragraphes sont représentés par une ou plusieurs lignes de texte séparées
par une ou plusieurs lignes vides.

```md
Ceci est un paragraphe. Là, je suis dans un paragraphe, facile non?

Maintenant je suis dans le paragraphe 2.
Je suis toujours dans le paragraphe 2!


Puis là, eh oui, le paragraphe 3!
```

Si jamais vous souhaitez insérer une balise HTML `<br />`, vous pouvez ajouter
un ou plusieurs espaces à la fin de votre paragraphe, et en commencer un
nouveau.

```md
J'ai deux espaces vides à la fin (sélectionnez moi pour les voir).  

Bigre, il y a un <br /> au dessus de moi!
```

Les blocs de citations sont générés aisément, grâce au caractère `>`.

```md
> Ceci est une superbe citation. Vous pouvez même
> revenir à la ligne quand ça vous chante, et placer un `>`
> devant chaque bout de ligne faisant partie
> de la citation.
> La taille ne compte pas^^ tant que chaque ligne commence par un `>`.

> Vous pouvez aussi utiliser plus d'un niveau
>> d'imbrication!
> Classe et facile, pas vrai?
```

## Listes

Les listes non ordonnées sont marquées par des asterisques, signes plus ou
signes moins.

```md
* Item
* Item
* Un autre item
```

ou

```md
+ Item
+ Item
+ Encore un item
```

ou

```md
- Item
- Item
- Un dernier item
```

Les listes ordonnées sont générées via un nombre suivi d'un point.

```md
1. Item un
2. Item deux
3. Item trois
```

Vous pouvez même vous passer de tout numéroter, et Markdown générera les bons
chiffres. Ceci dit, cette variante perd en clarté.

```md
1. Item un
1. Item deux
1. Item trois
```

(Cette liste sera interprétée de la même façon que celle au dessus)

Vous pouvez également utiliser des sous-listes.

```md
1. Item un
2. Item deux
3. Item trois
   * Sub-item
   * Sub-item
4. Item quatre
```

Il y a même des listes de taches. Elles génèrent des champs HTML de type case à
cocher.

```md
Les [ ] ci-dessous, n'ayant pas de [ x ], deviendront des cases à cocher HTML
non-cochées.
- [ ] Première tache à réaliser.
- [ ] Une autre chose à faire.
La case suivante sera une case à cocher HTML cochée.
- [x] Ça ... c'est fait!
```

## Blocs de code

Pour marquer du texte comme étant du code (qui utilise la balise `<code>`), il
suffit d'indenter chaque ligne avec 4 espaces ou une tabulation.

```md
    echo "Ça, c'est du Code!";
    var Ça = "aussi !";
```

L'indentation par tabulation (ou série de quatre espaces) fonctionne aussi à
l'intérieur du bloc de code.

```md
    my_array.each do |item|
      puts item
    end
```

Des bouts de code en mode en ligne s'ajoutent en utilisant le caractères
`` ` ``.

```md
La fonction `run()` ne vous oblige pas à aller courir!
```

En Markdown GitHub, vous pouvez utiliser des syntaxes spécifiques.

````md
```ruby
def foobar
  puts "Hello world!"
end
```
````

Pas besoin d'indentation pour le code juste au-dessus, de plus, GitHub
va utiliser une coloration syntaxique pour le langage indiqué après les <code>```</code>.

## Ligne Horizontale

Pour insérer une ligne horizontale, utilisez trois ou plusieurs astérisques ou tirets, avec ou sans espaces entre.

```md
***
---
- - -
****************
```

## Liens hypertextes

Une des fonctionnalités sympathiques du Markdown est la facilité d'ajouter des
liens hypertextes. Le texte du lien entre crochet `` [] ``, l'url entre
parenthèses `` () ``, et voilà le travail.

```md
[Clic moi!](http://test.com/)
```

Pour ajouter un attribut `Title`, collez-le entre guillemets, avec le lien.

```md
[Clic moi!](http://test.com/ "Lien vers Test.com")
```

Markdown supporte aussi les liens relatifs.

```md
[En avant la musique](/music/).
```

Les liens de références sont eux aussi disponibles en Markdown.

```md
[Cliquez ici][link1] pour plus d'information!
[Regardez aussi par ici][foobar] si vous voulez.

[link1]: http://test.com/ "Cool!"
[foobar]: http://foobar.biz/ "Génial!"
```

Le titre peut aussi être entouré de guillemets simples, ou de parenthèses, ou
absent. Les références peuvent être placées où vous voulez dans le document et
les identifiants peuvent être n'importe quoi tant qu'ils sont uniques.

Il y a également le nommage implicite qui transforme le texte du lien en
identifiant.

```md
[Ceci][] est un lien.

[Ceci]:http://ceciestunlien.com/
```

Mais ce n'est pas beaucoup utilisé.

## Images

Pour les images, la syntaxe est identique à celle des liens, sauf que précédée
d'un point d'exclamation!

```md
![Attribut ALT de l'image](http://imgur.com/monimage.jpg "Titre optionnel")
```

Là aussi, on peut utiliser le mode "références".

```md
![Ceci est l'attribut ALT de l'image][monimage]

[monimage]: relative/urls/cool/image.jpg "si vous voulez un titre, c'est ici."
```

## Divers

### Liens Automatiques

```md
<http://testwebsite.com/> est équivalent à :
[http://testwebsite.com/](http://testwebsite.com/)
```

### Liens Automatiques pour emails

```md
<foo@bar.com>
```

### Caracteres d'echappement

Il suffit de précéder les caractères spécifiques à ignorer par des backslash `\`.

```md
Pour taper *ce texte* entouré d'astérisques mais pas en italique :
Tapez \*ce texte\*.
```

### Touches de clavier

Avec le "GitHub Flavored Markdown", vous pouvez utiliser la balise `<kdb>`
pour représenter une touche du clavier.

```md
Ton ordinateur a planté? Essayer de taper :
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>
```

### Tableaux

Les tableaux ne sont disponibles que dans le "GitHub Flavored Markdown" et
ne sont pas tres agréable d'utilisation. Mais si vous en avez besoin :

```md
| Col1              | Col2     | Col3              |
| :---------------- | :------: | ----------------: |
| Alignement Gauche | Centré   | Alignement Droite |
| bla               | bla      | bla               |
```

ou bien, pour un résultat équivalent :

```md
Col 1 | Col2 | Col3
:-- | :-: | --:
Ough que c'est moche | svp | arrêtez
```

Pour plus d'information, consultez le post officiel de Jhon Gruber à propos de
la syntaxe [ici](http://daringfireball.net/projects/markdown/syntax) et la
superbe fiche pense-bête de Adam Pritchard [là](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
