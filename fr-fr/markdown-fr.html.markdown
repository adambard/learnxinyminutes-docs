---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
    - ["Geoffrey Liu", "http://github.com/g-liu"]
filename: markdown.md
---

John Gruber a créé la langue Markdown en 2004. Il est destiné à être un langue facile à lire et à écrire, et qui se convertit facilement à HTML (et actuellement de nombreux autres formats ainsi).

Donnez-moi autant de réactions que vous voulez! / N'hésitez pas à débourser et tirez-demander!


```markdown
<!--
	Markdown (MD) est un sur-ensemble de HTML, de sorte que tout fichier HTML est de MD valide, que 
	signifie que nous pouvons utiliser des éléments HTML dans MD, comme l'élément de commentaire, et 
	ils ne seront pas affectés par un analyseur de MD. Toutefois, si vous créez un élément HTML 
	dans votre MD, vous ne pouvez pas utiliser la syntaxe MD dans le contenu de l'élément.
-->

<!--
	Markdown varie également dans son implementation d'un analyseur à un autre. Ce 
	guide tenter de clarifier quand certains caractéristiques sont universelles
	ou quand ils sont spécifiques à un certain analyseur.
-->

<!-- Boutisses -->
<!-- Vous pouvez facilement créer des éléments HTML <h1> à <h6> en le faisant précéder du 
texte par un certain nombre de symboles hachages (#) -->
# C'est un <h1>
## C'est un <h2>
### C'est un <h3>
#### C'est un <h4>
##### C'est un <h5>
###### C'est un <h6>

<!-- Le MD nous fournit deux chemins alternatives d'indiquer h1 et h2 -->
C'est un h1
=============

C'est un h2
-------------

<!-- Styles de texte simples -->
<!-- Le texte peut être facilement stylé comme italique ou en gras avec MD -->

*Ce texte est en italique.*
_et si ce texte._

**Ce texte est en gras.**
__Et si ce texte.__

***Ce texte est dans les deux.***
**_Comme ça!_**
*__Et ça!__*

<!-- En «Github Flavored Markdown», où Github utilise pour rendre les fichiers MD,
on a aussi le barré: -->
~~Ce texte est rendu avec le barré.~~

<!-- Les paragraphes sont une ou plusieurs lignes de texte adjacentes séparées par un ou 
plusieurs lignes vides. -->

Ceci est un paragraphe. Je type dans un paragraphe, n'est-ce pas amusant? 

Maintenant, je suis dans le deuxieme paragraphe.
Je suis encore dans le 2ème paragraphe!


Je suis dans le 3ème paragraphe!

<!-- Si vous voulez insérer une balise HTML «<br />», vous pouvez finir un paragraphe 
avec deux ou plusieurs espaces et puis commencer un nouveau paragraphe. -->

Je finis avec deux espaces (soulignez-moi pour les voir).

Il y a un <br /> dessus de moi!

<!-- Citations de blocs sont faciles et rendu avec le caractère «>». -->

> Ceci est un citation de bloc. Vous pouvez
> utilise leur propre sauts de line et puis
> insérer un `>` avant de chaque ligne, ou vouz pouvez laisser vos lignes deviennent vraiment long et enrouler sur eux-mêmes.
> Il ne fait pas de différence tant qu'ils commencent par `>`.

> Vous pouvez également utiliser plus d'une etage
>> de l'échancrure
> Comment génial ce que c'est?

<!-- Listes -->
<!-- On rend des listes à puces avec des étoiles, points positifs, ou des tirets -->

* Article
* Article
* Un autre article

ou

+ Article
+ Article
+ Un article plus

ou

- Article
- Article
- Un dernier article

<!-- On rend des listes ordonnées avec un nombre suivi d'une péroide -->

1. Article un
2. Article deux
3. Article trois

<!-- vous n'avez même pas besoin de marquer correctement les articles et Markdown encore 
rendre les nombres dans le bon ordre, mais cela peut ne pas être une bonne idée -->

1. Article un
1. Article deux
1. Article trois
<!-- (Cela se rend la même chose que l'exemple ci-dessus) -->

<!-- Vous pouvez utiliser également les sous-listes -->

1. Article un
2. Article deux
3. Article trois
    * Sous-Article
    * Sous-Article
4. Article quatre

<!-- Les blocs de code -->
<!-- Vous pouvez indiquer un bloc de code (qui utilise l'élément <code>) avec une
échancrure d'une ligne avec quatre espaces ou une marque de tabulation -->

    Cela est de code
    Alors est-ce

<!-- Vous pouvez ajouter plus de niveaux d'échancrure
en ajoutant plus d'espaces ou des marques de tabulation -->

    my_array.each do |item|
        puts item
    end

<!-- On crée le code en ligne par le caractère d'accent grave «`» -->
Jon ne savait pas que fait le fonction `go_to()`!

<!-- Dans le «Github Flavored Markdown», vous pouvez utiliser une syntaxe spéciale pour le code -->

\`\`\`ruby <!-- supprimer ces barres obliques inverses quand vous faites cela, juste ```ruby ! -->
def foobar
    puts "Bonjour tout le monde!"
end
\`\`\` <!-- ici aussi, pas de «\»s -->

<!-- Le texte ci-dessus n'exige pas l'échancrure, et Github va utiliser de coloration
	syntaxique de la langue que vous spécifiez après le ```-->

<!-- Règle horizontale (<hr />) -->
<!-- Les règles horizontales sont facilement ajoutées avec trois ou plusieurs d'étoiles ou tirets,
	avec ou sans d'éspaces. -->

***
---
- - - 
****************

<!-- Liens -->
<!-- Un des meilleurs traits de Markdown est la facilité de créer des liens. Mettez
le texte à afficher entre parenthèses carrés [], suivi par l'URL entre parenthèses () -->

[Cliquez sur moi!](http://exemple.fr/)

<!-- Vous pouvez ajouter également un titre lien en utilisant les guillemets entre les parenthèses -->

[Cliquez sur moi!](http://exemple.fr/ "Lien à exemple.fr")

<!-- Les chemins relatifs se marchent aussi. -->
[Aller à /musique/](/musique/).

<!-- Le MD soutient également les liens de style référence -->

[Cliquez ce lien][lien1] pour plus d'informations!
[Consultez également ce lien][foobar] si vous voulez.

[lien1]: http://exemple.fr/ "Cool!"
[foobar]: http://foobar.biz/ "Alright!"

<!-- Le titre peut aussi être entre guillemets ou entre parenthèses, ou être omis 
entièrement. Les références peuvent être n'importe où dans votre document et les IDs de référence 
peut être n'importe quoi, tant qu'ils sont uniques. -->

<!-- Il y a aussi le "nommage implicite" qui vous permet d'utiliser le texte du lien que l'ID -->
[Ceci][] est un lien.

[ceci]: http://ceciestunlien.com.fr/

<!-- Mais on ne l'utilise pas communément -->

<!-- Images -->
<!-- Les images sont rendues da la même manière que les liens, mais avec un point d'exclamation en avant! -->

![C'est l'attribut alt pour mon image](http://imgur.com/myimage.jpg "Un titre optionel")

<!-- Et les liens de style référence se marchent comme prévu -->

![C'est l'attribut alt.][monimage]

[monimage]: relatif/urls/cool/image.jpg "si vous avez besoin d'un titre, il est ici"

<!-- Recueil -->
<!-- Auto-liens -->

<http://testwebsite.com/> est égal à
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Auto-liens pour les emails -->

<foo@bar.com.fr>

<!-- Echapper les caractères -->

Je veux typer *ce texte entre des étoiles*, mais je ne veux pas qu'il soit 
en italique, alors je le fais: \* ce texte entre des étoiles \*.

<!-- Tables -->
<!-- Tables ne sont disponibles que dans «Github Flavored Markdown» et sont un peu 
lourd, mais si vous en voulez vraiment: -->

| Col1         | Col2      | Col3          |
| :----------- | :------:  | ------------: |
| À gauche     | Centrée   | À droite      |
| blablabla    | blablabla | blablabla     |

<!-- ou, pour les mêmes résultats -->

Col1 | Col2 | Col3
:-- | :-: | --:
Pouah! Ceci est si moche | faites que ça | s'arrête

<!-- La fin! -->

```

Pour plus d'informations, consultez l'article officiel de John Gruber de la syntaxe [ici](http://daringfireball.net/projects/markdown/syntax) et le grand antisèche d'Adam Pritchard [ici](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
