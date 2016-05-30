---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
    - ["@prrrnd", "https://github.com/prrrnd"]
lang: fr-fr
---

Au début du web, il n'y avait pas d'élements visuels, simplement du texte pur. Mais avec le dévelopement des navigateurs,
des pages avec du contenu visuel sont arrivées.
CSS est le langage standard qui existe et permet de garder une séparation entre
le contenu (HTML) et le style d'une page web.

En résumé, CSS fournit une syntaxe qui vous permet de cibler des élements présents
sur une page HTML afin de leur donner des propriétés visuelles différentes.

Comme tous les autres langages, CSS a plusieurs versions. Ici, nous allons parler de CSS2.0
qui n'est pas le plus récent, mais qui reste le plus utilisé et le plus compatible avec les différents navigateurs.

**NOTE :** Vous pouvez tester les effets visuels que vous ajoutez au fur et à mesure du tutoriel sur des sites comme [dabblet](http://dabblet.com/) afin de voir les résultats, comprendre, et vous familiariser avec le langage.
Cet article porte principalement sur la syntaxe et quelques astuces.


```css
/* Les commentaires sont entourés par slash-étoile, comme cette ligne! */

/* ####################
   ## SÉLECTEURS
   ####################*/

/* Généralement, la première déclaration en CSS est très simple */
selecteur { propriete: valeur; /* autres proprietés...*/ }

/* Le sélecteur sert à cibler un élément du HTML

Vous pouvez cibler tous les éléments d'une page! */
* { color:red; }

/*
Voici un élément dans notre HTML :

<div class='une-classe classe2' id='unId' attr='valeur' />
*/

/* Vous pouvez le cibler par une classe */
.une-classe { }

/* ou par deux */
.une-classe.classe2 { }

/* ou par son type */
div { }

/* ou son id */
#unId { }

/* ou par le fait qu'il a un attribut */
[attr] { font-size:smaller; }

/* ou que l'attribut a une valeur spécifique */
[attr='valeur'] { font-size:smaller; }

/* commence avec une valeur */
[attr^='val'] { font-size:smaller; }

/* termine avec une valeur */
[attr$='eur'] { font-size:smaller; }

/* contient une valeur */
[attr~='leu'] { font-size:smaller; }


/* Ce qu'il faut bien comprendre, c'est que vous pouvez combiner ceci -- Il ne doit pas y avoir
d'espaces entre. */
div.une-classe[attr$='eu'] { }

/* Vous pouvez aussi cibler un élément par son parent. */

/* Un élément qui est en enfant direct */
div.un-parent > .enfant {}

/* Cela cible aussi les .enfants plus profonds dans la structure HTML */
div.un-parent .enfants {}

/* Attention : le même sélecteur sans espace a un autre sens. */
div.un-parent.classe {}

/* Vous pouvez cibler un élément basé sur un enfant de même parent */
.je-suis-avant + .cet-element { }

/* ou n'importe quel enfant de même parent avec celui ci */
.je-suis-tout-avant ~ .cet-element {}

/* Il y a des pseudo-classes qui permettent de cibler un élément
basé sur le comportement, en plus de la structure de la page */

/* élément avec le curseur au-dessus */
:hover {}

/* lien visité */
:visited {}

/* lien non visité */
:link {}

/* élément avec le focus */
:focus {}


/* ####################
   ## PROPRIÉTÉS
   ####################*/

selecteur {

    /* Units */
    width: 50%; /* pourcentage */
    font-size: 2em; /* taille de la police multipliée par X */
    width: 200px; /* pixels */
    font-size: 20pt; /* points */
    width: 5cm; /* centimetres */
    width: 50mm; /* millimetres */
    width: 5in; /* pouces */

    /* Couleurs */
    background-color: #F6E;  /* court hex */
    background-color: #F262E2; /* long hex */
    background-color: tomato; /* couleur nommée */
    background-color: rgb(255, 255, 255); /* rouge, vert, bleu */
    background-color: rgb(10%, 20%, 50%); /* rouge, vert, bleu en pourcent */
    background-color: rgba(255, 0, 0, 0.3); /* rouge, vert, bleu avec transparence */

    /* Images */
    background-image: url(/chemin-vers-image/image.jpg);

    /* Polices */
    font-family: Arial;
    font-family: "Courier New"; /* Si espace, entre guillemets */
    font-family: "Courier New", Trebuchet, Arial; /* Si la première n'est pas trouvée, la deuxième est utilisée, etc... */
}

```

## Utilisation

Le CSS s'écrit dans des fichiers `.css`.

```xml
<!-- Vous devez inclure le CSS dans la balise <head> : -->
<link rel='stylesheet' type='text/css' href='chemin/style.css' />

<!-- Vous pouvez inclure du CSS dans le HTML directement, mais ce n'est vraiment pas recommandé. -->
<style>
   selecteur { propriete:valeur; }
</style>

<!-- ou directement sur l'élément HTML.
PS : à ne pas faire. -->
<div style='propriete:valeur;'>
</div>

```

## Priorités

Comme on vient de le voir, un élément peut être ciblé par plus qu'un seul sélecteur
et une même propriété peut être définie plusieurs fois.
Dans ces cas, une des propriétés devient prioritaire.

Voici du code CSS :

```css
/*A*/
p.classe1[attr='valeur']

/*B*/
p.classe1 {}

/*C*/
p.classe2 {}

/*D*/
p {}

/*E*/
p { propriete: valeur !important; }

```

et le code HTML:

```xml
<p style='/*F*/ propriete:valeur;' class='classe1 classe2' attr='valeur'>
</p>
```

Les priorités de style sont :
Attention, les priorités s'appliquent aux **propriétés**, pas aux blocs entiers.

* `E` a la priorité grâce à `!important`.  
* `F` vient ensuite, car le code se trouve directement dans le HTML.
* `A` vient ensuite, car il est le plus spécifique.  
	plus spécifique veut dire, celui qui cible le plus l'élément
* `C` vient ensuite. Il est aussi spécifique que `B`, mais est écrit après.
* Puis `B`
* Et enfin `D`.

## Compatibilité

La plupart des fonctionnalités de CSS2 (et de plus en plus CSS3) sont compatibles
avec tous les navigateurs. Mais il est important de vérifier la compatibilité.

[QuirksMode CSS](http://www.quirksmode.org/css/) est une très bonne source pour cela.

## En savoir plus (en anglais)

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
