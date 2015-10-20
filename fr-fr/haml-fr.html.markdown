---
language: haml
filename: learnhaml.haml
contributors:
  - ["Simon Neveu", "https://github.com/sneveu"]
  - ["Thibault", "https://github.com/iTech-"]
lang: fr-fr
---

Haml est un langage de balisage utilisé majoritairement avec Ruby, qui décrit de manière simple et propre le HTML de n'importe quelle page web sans l'utilisation des traditionnelles lignes de code. Le langage est une alternative très populaire au langage de templates Rails (.erb) et permet d'intégrer du code en Ruby dans votre balisage.

Son but est de réduire le nombre de répétitions dans le balisage en fermant des balises pour vous en se basant sur l'indentation de votre code. Finalement, le balisage est bien structuré, ne contient pas de répétition, est logique et facile à lire.

Vous pouvez aussi utiliser Haml sur un projet indépendant de Ruby, en installant les gems de Haml et en le convertissant en html grâce aux commandes.

$ haml fichier_entree.haml fichier_sortie.html


```haml
/ -------------------------------------------
/ Indentation 
/ -------------------------------------------

/
  A cause de l'importance de l'indentation sur la manière dont votre code sera
  converti, l'indentation doit être constante à travers votre document. Un 
  simple changement d'indentation entrainera une erreur. En général, on utilise
  deux espaces, mais ce genre de décision sur l'indentation vous appartient, du
  moment que vous vous y tenez.

/ -------------------------------------------
/ Commentaires
/ -------------------------------------------

/ Ceci est un commentaire en Haml.

/
  Pour écrire un commentaire sur plusieurs lignes, indentez votre code
  commenté en le commençant par un slash

-# Ceci est un commentaire silencieux, qui n'apparaîtra pas dans le fichier


/ -------------------------------------------
/ Eléments HTML
/ -------------------------------------------

/ Pour écrire vos balises, utilisez un pourcentage suivi du nom de votre balise
%body
  %header
    %nav

/ Remarquez qu'il n'y a aucunes balises fermées. Le code produira alors ceci
  <body>
    <header>
      <nav></nav>
    </header>
  </body>

/ La balise div est l'élément par défaut, vous pouvez donc l'écrire comme ceci
.balise

/ Pour ajouter du contenu à votre balise, ajoutez le texte après sa déclaration
%h1 Titre contenu

/ Pour écrire du contenu sur plusieurs lignes, imbriquez le
%p
  Ce paragraphe contient beaucoup de contenu qui pourrait
  probablement tenir sur deux lignes séparées.

/
  Vous pouvez utiliser des caractères html spéciaux en utilisant &=. Cela va
  convertir les caractères comme &, /, : en leur équivalent HTML. Par exemple

%p
  &= "Oui & oui"

/ Produira 'Oui &amp; oui'

/ Vous pouvez écrire du contenu html sans qu'il soit converti en utilisant !=
%p
  != "Voici comment écrire une balise de paragraphe <p></p>"

/ Cela produira 'Voici comment écrire une balise de paragraphe <p></p>'

/ Une classe CSS peut être ajouté à votre balise en chainant le nom de la classe
%div.truc.machin

/ ou en utilisant un hash de Ruby
%div{:class => 'truc machin'}

/ Des attributs pour n'importe quelles balises peuvent être ajoutés au hash
%a{:href => '#', :class => 'machin', :title => 'Titre machin'}

/ Pour affecter une valeur à un booléen, utilisez 'true' 
%input{:selected => true}

/ Pour écrire des data-attributes, utilisez le :data avec la valeur d'un hash
%div{:data => {:attribute => 'machin'}}


/ -------------------------------------------
/ Insérer du Ruby
/ -------------------------------------------

/
  Pour transférer une valeur de Ruby comme contenu d'une balise, utilisez le
  signe égal suivi du code Ruby

%h1= livre.titre

%p
  = livre.auteur
  = livre.editeur


/ Pour lancer du code Ruby sans le convertir en HTML, utilisez un trait d'union
- livres = ['livre 1', 'livre 2', 'livre 3']

/ Ceci vous permet de faire des choses géniales comme des blocs Ruby
- livre.shuffle.each_with_index do |livre, index|
  %h1= livre 

  if livre do
    %p Ceci est un livre

/
  Encore une fois il n'est pas nécessaire d'ajouter une balise fermante, même
  pour Ruby.
  L'indentation le fera pour vous.


/ -------------------------------------------
/ Ruby en-ligne / Interpolation en Ruby
/ -------------------------------------------

/ Inclure une variable Ruby dans une ligne en utilisant #{}
%p Votre meilleur score est #{record}


/ -------------------------------------------
/ Filtres
/ -------------------------------------------

/
  Utilisez les deux points pour définir un filtre Haml, vous pouvez par exemple
  utiliser un filtre :javascript pour écrire du contenu en-ligne js

:javascript
  console.log('Ceci est la balise en-ligne <script>');

```

## Lectures complémentaires

- [Qu'est-ce que HAML ?](http://haml.info/) - Une bonne introduction qui explique très bien les avantages d'utiliser HAML.
- [Documentation officielle](http://haml.info/docs/yardoc/file.REFERENCE.html) - Si vous souhaitez en apprendre plus et aller plus loin.
