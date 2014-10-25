---
language: markdown
contributors:
- ["Andrei Curelaru", "http://www.infinidad.fr"]
filename: markdown.md
---

Markdown a été crée par Jhon Gruber en 2004. Ceci se veut être d'une syntaxe facile à lire et à écrire,
aisément convertible en HTML(et beaucoup d'autres formats aussi à présent).

Faites moi autant de retours que vous voulez! Sentez vous libre de "forker" et envoyer des pull request!


```markdown
<!-- Markdown est une sorte de cousin du HTML, si bien que tout document HTML est un document Markdown valide.
Autrement dit, nous pouvons utiliser des balises HTML dans un fichier Markdown, comme la balise commentaire dans laquelle
nous sommes à présent, car celle-ci ne sera pas affectée par le parser Markdown.
Toutefois, si l'on peut créer un élément HTML dans un fichier Markdown, on ne peut utiliser du Markdown à l'intérieur de ce dernier. -->
<!--  Le Markdown est implémenté de différentes manières, selon le parser(analyseur syntaxique). Ce guide va alors tenter de trier
les fonctionnalités universelles de celles spécifiques à un parser.  -->

<!-- Headers -->
<!-- Vous pouvez facilement créer des éléments HTML <h1> à <h6> en précédant le texte de votre futur titre par un ou plusieurs dièses (#),
de un à six, selon le niveau de titre souhaité. -->
# Ceci est un <h1>
## Ceci est un <h2>
### Ceci est un <h3>
#### Ceci est un <h4>
##### Ceci est un <h5>
###### Ceci est un <h6>

<!-- Markdown fournit également une façon alternative de marquer les h1 et h2 -->
Ceci est un h1
=============

Ceci est un h2
-------------

<!-- Styles basiques pour du texte -->
<!-- On peut facilement rendre un texte "gras" ou "italique" en Markdown -->

*Ce texte est en italique.*
_Celui-ci aussi._

**CE texte est en gras.**
__Celui-là aussi.__

***Ce texte a les deux styles.***
**_Pareil ici_**
*__Et là!__*

<!--Dans le  "Github Flavored Markdown", utilisé pour interpréter le Markdown sur Github
,on a également le strikethrough(texte barré) : -->

~~Ce texte est barré avec strikethrough.~~
<!--  Les Paragraphes sont représentés par une ou plusieurs lignes de texte
séparées par une ou plusieurs lignes vides.   -->

Ceci est un paragraphe. J'écris dans un paragraphe, marrant non?

Maintenant je suis dans le paragraphe 2.
Je suis toujours dans le paragraphe 2 ici aussi!


Puis là, eh oui, le paragraphe3!

<!-- Should you ever want to insert an HTML <br /> tag, you can end a paragraph
with two or more spaces and then begin a new paragraph. -->
<!--  
Si jamais vous souhaitez insérer une balise HTML <br />, vous pouvez ajouter un ou plusieurs espaces
à la fin de votre paragraphe, et en commencer un nouveau.
-->

J'ai deux espaces vides à la fin (sélectionnez moi pour les voir). 

Bigre, il y a un <br /> au dessus de moi!

<!-- Les 'Blocs de Citations' sont générés simplement aussi, grâce au caractère >. -->

> Ceci est une superbe citation. Vous pouvez même
> revenir à la ligne quand ça vous chante, et placer un  `>` devant chaque bout de ligne faisant partie
> de la citation.
> La taille ne compte pas^^ tant que chaque ligne commence par un `>`.

> Vous pouvez aussi utiliser plus d'un niveau
>> d'imbrication!
> Class et facile, pas vrai?

<!-- les Listes -->
<!-- Les listes non ordonnées sont marquées par des asterix (heuu astérisques), signes plus ou signes moins. -->

* Item
* Item
* Un autre item

or

+ Item
+ Item
+ Encore un item

or

- Item
- Item
- Un dernier item

<!-- les listes Ordonnées sont générées via un nombre suivi d'un point -->

1. Item un
2. Item deux
3. Item trois

<!-- Vous pouvez même vous passer de tout numéroter, et Markdown générera les bons chiffres.
Ceci dit, cette variante perds en clarté lors de la rédaction.-->

1. Item un
1. Item deux
1. Item trois
<!-- (Cette liste sera interprétée de la même façon que celle juste au dessus.) -->

<!-- Vous pouvez également utiliser des sous-listes -->

1. Item un
2. Item deux
3. Item trois
* Sub-item
* Sub-item
4. Item quatre

<!-- Il y a même des "listes de Taches". Elles génèrent des champs HTML de type checkbox. -->

Les [ ] ci dessous, n'ayant pas de [ x ], deviendront des cases à cocher HTML non-cochées.
- [ ] Première tache à réaliser.
- [ ] Une autre chose à faire.
La case suivante sera une case à cocher HTML cochée.
- [x] Ca ... c'est fait!

<!-- les Blocs de Code -->
<!-- Pour marquer du texte comme étant du code, il suffit de -->

This is code
So is this

<!-- You can also re-tab (or add an additional four spaces) for indentation
inside your code -->

my_array.each do |item|
puts item
end

<!-- Inline code can be created using the backtick character ` -->

John didn't even know what the `go_to()` function did!

<!-- In Github Flavored Markdown, you can use a special syntax for code -->

\`\`\`ruby <!-- except remove those backslashes when you do this, just ```ruby ! -->
def foobar
puts "Hello world!"
end
\`\`\` <!-- here too, no backslashes, just ``` -->

<-- The above text doesn't require indenting, plus Github will use syntax
highlighting of the language you specify after the ``` -->

<!-- Horizontal rule (<hr />) -->
<!-- Horizontal rules are easily added with three or more asterisks or hyphens,
with or without spaces. -->

***
---
- - -
****************

<!-- Links -->
<!-- One of the best things about markdown is how easy it is to make links. Put
the text to display in hard brackets [] followed by the url in parentheses () -->

[Click me!](http://test.com/)

<!-- You can also add a link title using quotes inside the parentheses -->

[Click me!](http://test.com/ "Link to Test.com")

<!-- Relative paths work too. -->

[Go to music](/music/).

<!-- Markdown also supports reference style links -->

[Click this link][link1] for more info about it!
[Also check out this link][foobar] if you want to.

[link1]: http://test.com/ "Cool!"
[foobar]: http://foobar.biz/ "Alright!"

<!-- The title can also be in single quotes or in parentheses, or omitted
entirely. The references can be anywhere in your document and the reference IDs
can be anything so long as they are unique. -->

<!-- There is also "implicit naming" which lets you use the link text as the id -->

[This][] is a link.

[this]: http://thisisalink.com/

<!-- But it's not that commonly used. -->

<!-- Images -->
<!-- Images are done the same way as links but with an exclamation point in front! -->

![This is the alt-attribute for my image](http://imgur.com/myimage.jpg "An optional title")

<!-- And reference style works as expected -->

![This is the alt-attribute.][myimage]

[myimage]: relative/urls/cool/image.jpg "if you need a title, it's here"

<!-- Miscellany -->
<!-- Auto-links -->

<http://testwebsite.com/> is equivalent to
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Auto-links for emails -->

<foo@bar.com>

<!-- Escaping characters -->

I want to type *this text surrounded by asterisks* but I don't want it to be
in italics, so I do this: \*this text surrounded by asterisks\*.

<!-- Tables -->
<!-- Tables are only available in Github Flavored Markdown and are slightly
cumbersome, but if you really want it: -->

| Col1 | Col2 | Col3 |
| :----------- | :------: | ------------: |
| Left-aligned | Centered | Right-aligned |
| blah | blah | blah |

<!-- or, for the same results -->

Col 1 | Col2 | Col3
:-- | :-: | --:
Ugh this is so ugly | make it | stop

<!-- Fin! -->

```

For more info, check out John Gruber's official post of syntax [here](http://daringfireball.net/projects/markdown/syntax) and Adam Pritchard's great cheatsheet [here](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).