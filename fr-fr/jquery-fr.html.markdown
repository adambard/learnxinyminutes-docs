---
category: tool
tool: jquery
contributors:
    - ["Sawyer Charles", "https://github.com/xssc"]
translators:
    - ["Sylvain Vaure", "https://github.com/Gnomino"]
filename: jquery-fr.js
lang: fr-fr
---

jQuery est une bibliothèque JavaScript dont le but est de permettre de "faire plus en écrivant moins" (do more, write less). Elle facilite l'écriture de nombreuses fonctions, notamment au niveau d'AJAX, de la gestion d'événements, ou encore de la manipulation de documents. 
C'est pourquoi aujourd'hui, jQuery est utilisée par de nombreuses grandes entreprises et par des développeurs du monde entier.

Étant donné que jQuery est une bibliothèque JavaScript, vous devriez d'abord [apprendre le JavaScript](https://learnxinyminutes.com/docs/fr-fr/javascript-fr/)
```js


///////////////////////////////////
// 1. Les sélecteurs

// On utilise les sélecteurs de jQuery pour sélectionner des éléments
var page = $(window); // Sélectionne tout le viewport

// On peut aussi utiliser des sélecteurs CSS
var paragraph = $('p'); // Sélectionne tous les éléments paragraphes
var table1 = $('#table1'); // Sélectionne l'élément qui a l'id 'table1'
var squares = $('.square'); // Sélectionne tous les éléments avec la classe 'square'
var square_p = $('p.square') // Sélectionne tous les paragraphes avec la classe 'square'


///////////////////////////////////
// 2. Événements et effets
// jQuery gère très bien ce qui se passe lorsqu'un événement est déclenché
// L'événement 'ready' est très souvent utilisé sur le document
// On utilise la méthode 'ready' pour attendre que l'élément ait fini de se charger
$(document).ready(function(){
  // Ce code ne s'exécutera pas avant que le document soit chargé (prêt)
});
// On peut aussi utiliser des fonctions définies
function onAction() {
  // Ceci est exécuté quand l'événement est déclenché
}
$('#btn').click(onAction); // Appelle onAction à chaque clic

function onAction() {
  // Ceci est exécuté quand un évènement est déclenché
}

// D'autres évènements communs :
$('#btn').dblclick(onAction); // Double clic
$('#btn').hover(onAction); // Survol de la souris
$('#btn').focus(onAction); // Gain du focus
$('#btn').blur(onAction); // Perte du focus
$('#btn').submit(onAction); // Envoi (d'un formulaire)
$('#btn').select(onAction); // Quand un élement est sélectionné
$('#btn').keydown(onAction); // Quand une touche est enfoncée
$('#btn').keyup(onAction); // Quand une touche est relâchée
$('#btn').keypress(onAction); // Quand on appuie sur un touche
$('#btn').mousemove(onAction); // Quand la souris se déplace
$('#btn').mouseenter(onAction); // La souris entre dans l'élément
$('#btn').mouseleave(onAction); // La souris sort de l'élément

// On peut aussi utiliser des fonctions lambdas
$('#btn').hover(function(){
  // exécuté lors d'un survol de la souris
});

// Il est possible de déclencher l'événement sans le gérer
// simplement en ne passant aucun paramètre à la méthode
$('#btn').dblclick(); // Simule un double clic sur l'élément

// On peut gérer plusieurs événements en utilisant le sélecteur une seule fois
$('#btn').on(
  {dblclick: myFunction1} // Déclenché à chaque double clic
  {blur: myFunction1} // Déclenché quand l'élément perd le focus
);

// On peut déplacer et cacher des éléments grâce à des fonctions d'effets
$('.table').hide(); // Cache le(s) élément(s)

// Note: même avec un appel à une fonction dans ces méthodes 
// cache quand même l'élément
$('.table').hide(function(){
    // L'élément est caché, puis la fonction est exécutée
});

// On peut stocker des sélecteurs dans des variables
var tables = $('.table');

// Des méthodes basique de manipulation de document :
tables.hide(); // Cache un(des) élément(s)
tables.show(); // Montre (dé-cache) un(des) élément(s)
tables.toggle(); // Change le statut le statut caché/montré
tables.fadeOut(); // Fait disparaître l'élément
tables.fadeIn(); // Fait apparaître l'élément
tables.fadeToggle(); // Fait apparaître ou disparaître
tables.fadeTo(0.5); // Fondu jusqu'à une certaine opacité (entre 0 et 1)
tables.slideUp(); // Cache l'élément avec un effet de glissement vers le haut
tables.slideDown(); // Fait apparaître l'élément avec un glissement vers le bas
tables.slideToggle(); // Cache/Montre l'élément avec un effet de glissement

// Les méthodes ci-dessus prennent en arguments 
// une vitesse (millisecondes) et une function callback
tables.hide(1000, myFunction); // Animation d'une seconde, puis appel à la fonction

// fadeTo doit avoir une opacité entre 0 et 1 comme deuxième argument
tables.fadeTo(2000, 0.1, myFunction); // 2 sec. fade to 0.1 opacity then function

// La méthode animate permet des animations légèrement plus poussées
tables.animate({margin-top:"+=50", height: "100px"}, 500, myFunction);
// La méthode prend un objet de css et de valeurs finales,
// des paramètres d'options facultatifs pour régler l'animation,
// et bien sûr la fonction callback

///////////////////////////////////
// 3. Manipulation

// Ces méthodes sont similaires aux effets mais permettent d'aller plus loin
$('div').addClass('taming-slim-20'); // Ajoute la classe taming-slim-20 aux div 

// Méthodes ordinaires de manipulation
$('p').append('Hello world'); // Ajoute à la fin de l'élément
$('p').attr('class'); // Renvoie la valeur de l'attribut
$('p').attr('class', 'content'); // Change la valeur de l'attribut
$('p').hasClass('taming-slim-20'); // Renvoie vrai si l'élément est de la classe
$('p').height(); // Renvoie la hauteur de l'élément ou la change


// Pour beaucoup de méthodes de manipulation, récupérer des informations
// d'un élément renverra SEULEMENT ceelles du premier
$('p').height(); // Renvoie SEULEMENT la hauteur du premier élément 'p'

// On peut utiliser 'each' pour parcourir tous les éléments
var heights = [];
$('p').each(function() {
  heights.push($(this).height()); // Ajoute la hauteur de tous les éléments 'p' à la liste
});


``

