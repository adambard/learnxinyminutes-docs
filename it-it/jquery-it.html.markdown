---
category: tool
tool: jquery
contributors:
    - ["Sawyer Charles", "https://github.com/xssc"]
filename: jquery-it.js
translators:
    - ["Ale46", "https://github.com/ale46"]
lang: it-it
---

jQuery è una libreria JavaScript che ti aiuta a "fare di più, scrivendo meno". Rende molte attività comuni di JavaScript più facili da scrivere. jQuery è utilizzato da molte grandi aziende e sviluppatori in tutto il mondo. Rende AJAX, gestione degli eventi, manipolazione dei documenti e molto altro, più facile e veloce.

Visto che jQuery è una libreria JavaScript dovresti prima [imparare JavaScript](https://learnxinyminutes.com/docs/javascript/)

```js


///////////////////////////////////
// 1. Selettori

// I selettori in jQuery vengono utilizzati per selezionare un elemento
var page = $(window); // Seleziona l'intera finestra

// I selettori possono anche essere selettori CSS
var paragraph = $('p'); // Seleziona tutti gli elementi del paragrafo
var table1 = $('#table1'); // Seleziona elemento con id 'table1'
var squares = $('.square'); // Seleziona tutti gli elementi con la classe 'square'
var square_p = $('p.square') // Seleziona i paragrafi con la classe 'square'


///////////////////////////////////
// 2. Eventi ed effetti
// jQuery è molto bravo a gestire ciò che accade quando un evento viene attivato
// Un evento molto comune è l'evento "pronto" sul documento
// Puoi usare il metodo 'ready' per aspettare che l'elemento abbia finito di caricare
$(document).ready(function(){
  // Il codice non verrà eseguito fino a quando il documento non verrà caricato
});
// Puoi anche usare funzioni definite
function onAction() {
  // Questo viene eseguito quando l'evento viene attivato
}
$('#btn').click(onAction); // Invoca onAction al click

// Alcuni altri eventi comuni sono:
$('#btn').dblclick(onAction); // Doppio click
$('#btn').hover(onAction); // Al passaggio del mouse
$('#btn').focus(onAction); // Al focus
$('#btn').blur(onAction); // Focus perso
$('#btn').submit(onAction); // Al submit
$('#btn').select(onAction); // Quando un elemento è selezionato
$('#btn').keydown(onAction); // Quando un tasto è premuto (ma non rilasciato)
$('#btn').keyup(onAction); // Quando viene rilasciato un tasto
$('#btn').keypress(onAction); // Quando viene premuto un tasto
$('#btn').mousemove(onAction); // Quando il mouse viene spostato
$('#btn').mouseenter(onAction); // Il mouse entra nell'elemento
$('#btn').mouseleave(onAction); // Il mouse lascia l'elemento


// Questi possono anche innescare l'evento invece di gestirlo
// semplicemente non passando alcun parametro
$('#btn').dblclick(); // Innesca il doppio click sull'elemento

// Puoi gestire più eventi mentre usi il selettore solo una volta
$('#btn').on(
  {dblclick: myFunction1} // Attivato con doppio clic
  {blur: myFunction1} // Attivato al blur
);

// Puoi spostare e nascondere elementi con alcuni metodi di effetto
$('.table').hide(); // Nascondi gli elementi

// Nota: chiamare una funzione in questi metodi nasconderà comunque l'elemento
$('.table').hide(function(){
    // Elemento nascosto quindi funzione eseguita
});

// È possibile memorizzare selettori in variabili
var tables = $('.table');

// Alcuni metodi di manipolazione dei documenti di base sono:
tables.hide(); // Nascondi elementi
tables.show(); // Mostra elementi
tables.toggle(); // Cambia lo stato nascondi/mostra
tables.fadeOut(); // Fades out
tables.fadeIn(); // Fades in
tables.fadeToggle(); // Fades in o out
tables.fadeTo(0.5); // Dissolve in opacità (tra 0 e 1)
tables.slideUp(); // Scorre verso l'alto
tables.slideDown(); // Scorre verso il basso
tables.slideToggle(); // Scorre su o giù

// Tutti i precedenti prendono una velocità (millisecondi) e la funzione di callback
tables.hide(1000, myFunction); // nasconde l'animazione per 1 secondo quindi esegue la funzione

// fadeTo ha un'opacità richiesta come secondo parametro
tables.fadeTo(2000, 0.1, myFunction); // esegue in 2 sec. il fade sino ad una opacità di 0.1 opacity e poi la funzione

// Puoi ottenere un effetti più avanzati con il metodo animate
tables.animate({margin-top:"+=50", height: "100px"}, 500, myFunction);
// Il metodo animate accetta un oggetto di css e valori con cui terminare,
// parametri opzionali per affinare l'animazione,
// e naturalmente la funzione di callback

///////////////////////////////////
// 3. Manipolazione

// Questi sono simili agli effetti ma possono fare di più
$('div').addClass('taming-slim-20'); // Aggiunge la classe taming-slim-20 a tutti i div 

// Metodi di manipolazione comuni
$('p').append('Hello world'); // Aggiunge alla fine dell'elemento
$('p').attr('class'); // Ottiene l'attributo
$('p').attr('class', 'content'); // Imposta l'attributo
$('p').hasClass('taming-slim-20'); // Restituisce vero se ha la classe
$('p').height(); // Ottiene l'altezza dell'elemento o imposta l'altezza


// Per molti metodi di manipolazione, ottenere informazioni su un elemento
// restituirà SOLO il primo elemento corrispondente
$('p').height(); // Ottiene solo la prima altezza del tag 'p'

// È possibile utilizzare each per scorrere tutti gli elementi
var heights = [];
$('p').each(function() {
  heights.push($(this).height()); // Aggiunge tutte le altezze del tag 'p' all'array
});


```
