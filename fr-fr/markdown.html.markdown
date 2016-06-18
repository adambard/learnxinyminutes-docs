---
language: markdown
contributors:
- ["Andrei Curelaru", "http://www.infinidad.fr"]
filename: markdown-fr.md
lang: fr-fr
---

Markdown a été créé par John Gruber en 2004. Il se veut être d'une syntaxe
facile à lire et à écrire, aisément convertible en HTML
 (et beaucoup d'autres formats aussi à présent).

Faites moi autant de retours que vous voulez! Sentez vous libre de "forker" 
et envoyer des pull request!


```markdown
<!-- Markdown est une sorte de cousin du HTML, si bien que tout document HTML 
est un document Markdown valide. Autrement dit, vous pouvez utiliser des 
balises HTML dans un fichier Markdown, comme la balise commentaire dans 
laquelle nous sommes à présent, car celle-ci ne sera pas affectée par 
le parser( analyseur syntaxique ) Markdown. -->

<!-- Toutefois, si vous voulez créer un élément HTML dans un fichier Markdown,
 vous ne pourrez pas utiliser du Markdown à l'intérieur de ce dernier. -->

<!--  Le Markdown est implémenté de différentes manières, selon le parser. 
Ce guide va alors tenter de trier les fonctionnalités universelles de celles
spécifiques à un parser.  -->

<!-- Headers ( En-têtes ) -->
<!-- Vous pouvez facilement créer des éléments HTML <h1> à <h6> en précédant
 le texte de votre futur titre par un ou plusieurs dièses ( # ), de un à six,
  selon le niveau de titre souhaité. -->
# Ceci est un <h1>
## Ceci est un <h2>
### Ceci est un <h3>
#### Ceci est un <h4>
##### Ceci est un <h5>
###### Ceci est un <h6>

<!-- 
Markdown fournit également une façon alternative de marquer les h1 et h2 
-->

Ceci est un h1
=============

Ceci est un h2
-------------

<!-- Styles basiques pour du texte -->
<!-- On peut facilement rendre un texte "gras" ou "italique" en Markdown -->

*Ce texte est en italique.*
_Celui-ci aussi._

**Ce texte est en gras.**
__Celui-là aussi.__

***Ce texte a les deux styles.***
**_Pareil ici_**
*__Et là!__*

<!-- Dans le "GitHub Flavored Markdown", utilisé pour interpréter le Markdown 
sur GitHub, on a également le strikethrough ( texte barré ) : -->

~~Ce texte est barré avec strikethrough.~~

<!--  Les Paragraphes sont représentés par une ou plusieurs lignes de texte 
séparées par une ou plusieurs lignes vides. -->

Ceci est un paragraphe. Là, je suis dans un paragraphe, facile non?

Maintenant je suis dans le paragraphe 2.
Je suis toujours dans le paragraphe 2!


Puis là, eh oui, le paragraphe 3!

<!--  
Si jamais vous souhaitez insérer une balise HTML <br />, vous pouvez ajouter 
un ou plusieurs espaces à la fin de votre paragraphe, et en commencer 
un nouveau.
-->

J'ai deux espaces vides à la fin (sélectionnez moi pour les voir). 

Bigre, il y a un <br /> au dessus de moi!

<!-- Les 'Blocs de Citations' sont générés aisément, grâce au caractère > -->

> Ceci est une superbe citation. Vous pouvez même
> revenir à la ligne quand ça vous chante, et placer un `>` 
> devant chaque bout de ligne faisant partie
> de la citation.
> La taille ne compte pas^^ tant que chaque ligne commence par un `>`.

> Vous pouvez aussi utiliser plus d'un niveau
>> d'imbrication!
> Classe et facile, pas vrai?

<!-- les Listes -->
<!-- les Listes non ordonnées sont marquées par des asterisques, 
signes plus ou signes moins. -->

* Item
* Item
* Un autre item

ou

+ Item
+ Item
+ Encore un item

ou

- Item
- Item
- Un dernier item

<!-- les Listes Ordonnées sont générées via un nombre suivi d'un point -->

1. Item un
2. Item deux
3. Item trois

<!-- Vous pouvez même vous passer de tout numéroter, et Markdown générera 
les bons chiffres. Ceci dit, cette variante perd en clarté.-->

1. Item un
1. Item deux
1. Item trois
<!-- ( cette liste sera interprétée de la même façon que celle au dessus ) -->

<!-- Vous pouvez également utiliser des sous-listes -->

1. Item un
2. Item deux
3. Item trois
* Sub-item
* Sub-item
4. Item quatre

<!-- Il y a même des "listes de Taches". Elles génèrent des champs HTML 
de type checkbox. -->

Les [ ] ci dessous, n'ayant pas de [ x ], 
deviendront des cases à cocher HTML non-cochées.

- [ ] Première tache à réaliser.
- [ ] Une autre chose à faire.
La case suivante sera une case à cocher HTML cochée.
- [x] Ça ... c'est fait!

<!-- les Blocs de Code -->
<!-- Pour marquer du texte comme étant du code, il suffit de commencer 
chaque ligne en tapant 4 espaces (ou un Tab) -->

    echo "Ça, c'est du Code!";
    var Ça = "aussi !";

<!-- L'indentation par tab ou série de quatre espaces 
fonctionne aussi à l'intérieur du bloc de code -->

    my_array.each do |item|
       puts item
    end

<!-- Des bouts de code en mode 'inline' s'ajoutent en les entourant de ` -->

La fonction `run()` ne vous oblige pas à aller courir!

<!-- Via GitHub Flavored Markdown, vous pouvez utiliser 
des syntaxes spécifiques -->

\`\`\`ruby 
<!-- mais enlevez les backslashes quand vous faites ça, 
gardez juste ```ruby ( ou nom de la syntaxe correspondant à votre code )-->
def foobar
puts "Hello world!"
end
\`\`\` <!-- pareil, pas de backslashes, juste ``` en guise de fin -->

<-- Pas besoin d'indentation pour le code juste au dessus, de plus, GitHub 
va utiliser une coloration syntaxique pour le langage indiqué après les ``` -->

<!-- Ligne Horizontale (<hr />) -->
<!-- Pour en insérer une, utilisez trois ou plusieurs astérisques ou tirets, 
avec ou sans espaces entre chaque un. -->

***
---
- - -
****************

<!-- Liens -->
<!-- Une des fonctionnalités sympathiques du Markdown est la facilité 
d'ajouter des liens. Le texte du lien entre [ ], l'url entre ( ), 
et voilà l'travail.
-->

[Clic moi!](http://test.com/)

<!-- 
Pour ajouter un attribut Title, collez le entre guillemets, avec le lien. 
-->

[Clic moi!](http://test.com/ "Lien vers Test.com")

<!-- les Liens Relatifs marchent aussi -->

[En avant la musique](/music/).

<!-- Les liens façon "références" sont eux aussi disponibles en Markdown -->

[Cliquez ici][link1] pour plus d'information!
[Regardez aussi par ici][foobar] si vous voulez.

[link1]: http://test.com/ "Cool!"
[foobar]: http://foobar.biz/ "Alright!"

<!--  Le titre peut aussi être entouré de guillemets simples, 
entre parenthèses ou absent. Les références peuvent être placées 
un peu où vous voulez dans le document, et les identifiants 
(link1, foobar, ...) quoi que ce soit tant qu'ils sont uniques -->

<!-- Il y a également le "nommage implicite" qui transforme le texte du lien
 en identifiant -->

[Ceci][] est un lien.

[ceci]: http://ceciestunlien.com/

<!-- mais ce n'est pas beaucoup utilisé. -->

<!-- Images -->
<!-- Pour les images, la syntaxe est identique aux liens, sauf que précédée
 d'un point d'exclamation! -->

![Attribut ALT de l'image](http://imgur.com/monimage.jpg "Titre optionnel")

<!-- Là aussi, on peut utiliser le mode "références" -->

![Ceci est l'attribut ALT de l'image][monimage]

[monimage]: relative/urls/cool/image.jpg "si vous voulez un titre, c'est ici."

<!-- Divers -->
<!-- Liens Automatiques -->

<http://testwebsite.com/> est équivalent à :
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Liens Automatiques pour emails -->

<foo@bar.com>

<!-- Escaping -->
Il suffit de précéder les caractères spécifiques à ignorer par des backslash \

Pour taper *ce texte* entouré d'astérisques mais pas en italique : 
Tapez \*ce texte\*.

<!-- Tableaux -->
<!-- les Tableaux ne sont disponibles que dans le GitHub Flavored Markdown
 et c'est ce n'est pas super agréable d'utilisation. 
 Mais si vous en avez besoin :
 -->

| Col1 | Col2 | Col3 |
| :----------- | :------: | ------------: |
| Alignement Gauche | Centé | Alignement Droite |
| bla | bla | bla |

<!-- ou bien, pour un résultat équivalent : -->

Col 1 | Col2 | Col3
:-- | :-: | --:
Ough que c'est moche | svp | arrêtez

<!-- Fin! -->

```

Pour plus d'information :
 consultez [ici](http://daringfireball.net/projects/markdown/syntax) le post officiel de Jhon Gruber à propos de la syntaxe, 
 et [là](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) la superbe cheatsheet de Adam Pritchard.
