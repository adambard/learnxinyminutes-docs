---
category: tool
tool: jquery
contributors:
    - ["Sawyer Charles", "https://github.com/xssc"]
translators:
    - ["Ivan Alburquerque", "https://github.com/AlburIvan"]
lang: es-es
filename: jquery-es.js
---

jQuery es una librería de JavaScript que le ayuda a "hacer más y escribir menos". Esto hace que muchas de las tareas comunes de JavaScript sean más fáciles de escribir. jQuery es utilizado por muchas de las grandes empresas y desarrolladores de todo el mundo. Hace que AJAX, la gestión de eventos, la manipulación de documentos, y mucho más, sea más fácil y rápido.

Debido a que jQuery es una librería de JavaScript debes [aprender JavaScript primero](https://learnxinyminutes.com/docs/es-es/javascript-es/)

```js


///////////////////////////////////
// 1. Selectores

// Los selectores en jQuery son usados para seleccionar un elemento
var page = $(window); // Selecciona toda la ventana gráfica

// Los selectores también pueden ser selectores CSS
var paragraph = $('p'); // Selecciona todos los elementos de párrafo
var table1 = $('#table1'); // Selecciona el elemento con id 'tabla1'
var squares = $('.square'); // Selecciona todos los elementos con la clase "square"
var square_p = $('p.square') // Selecciona los párrafos con la clase "square"


///////////////////////////////////
// 2. Eventos y efectos

// Un evento muy común que se utiliza es el evento 'ready' en el documento
// Se puede utilizar el método de 'ready' para esperar hasta que el elemento haya terminado de cargar
$(document).ready(function(){
  // El código no se ejecutará hasta que el documento haya terminado de cargar
});

// jQuery es muy bueno activando eventos
// Y también en el manejo de lo que ocurre cuando se activa un evento
$('#button').click(); // Dispara un evento click en $ ('# botón')
$('#button').click(function(){
  // El código es ejecutado cuando se hace clic en el elemento de botón #
});

function onAction() {
 // Esto se ejecuta cuando se activa el evento
}

// Algunos otros eventos comunes son:
$('#btn').dblclick(onAction); //Doble clic
$('#btn').hover(onAction); // Pasar el cursor por encima
$('#btn').focus(onAction); // Enfocado
$('#btn').blur(onAction); // Pierde enfoque
$('#btn').submit(onAction); // Enviado
$('#btn').select(onAction); // Cuando se selecciona un elemento
$('#btn').keydown(onAction); // Cuando una tecla es empujada hacia abajo
$('#btn').keyup(onAction); // Cuando se suelta una tecla
$('#btn').keypress(onAction); // Cuando se pulsa una tecla
$('#btn').mousemove(onAction); // Cuando se mueve el mouse
$('#btn').mouseenter(onAction); // El mouse entra en el elemento
$('#btn').mouseleave(onAction); // El mouse sale en el elemento

// También se puede utilizar una función anónima
$('#btn').hover(function(){
  // Se ejecuta al pasar por encima
});

// Todos estos pueden también desencadenar el evento en lugar de manejarlo
// Simplemente no pasando ningún parámetro
$('#btn').dblclick(); // Dispara el evento de doble clic sobre el elemento

// Se puede manejar múltiples eventos, usando el selector una vez
$('#btn').on(
  {dblclick: myFunction1} // Activado con doble clic
  {blur: myFunction1} // Activo en la perdida de enfoque
);

// Puede mover y ocultar elementos con algunos métodos de efecto
$('.table').hide(); # Oculta el(los) elemento(s)

// Nota: llamar a una función en estos métodos aún oculta el elemento
$('.table').hide(function(){
    // El elemento se oculta entonces función ejecutada
});

// Puedes almacenar los selectores en las variables
var tables = $('.table');

// Algunos métodos básicos de manipulación de documento son:
tables.hide(); // Oculta elemento(s)
tables.show(); // Muestra elemento(s)
tables.toggle(); // Cambia el estado de ocultar / mostrar
tables.fadeOut(); // Desvanece
tables.fadeIn(); // Fundirse
tables.fadeToggle(); // Desvanece dentro o fuera
tables.fadeTo(0.5); // Desvanece a una opacidad (entre 0 y 1)
tables.slideUp(); // Desliza hacia arriba
tables.slideDown(); // Desliza hacia abajo
tables.slideToggle(); // Desliza hacia arriba o hacia abajo

// Todo lo anterior toma una velocidad (milisegundos) y la función de devolución de llamada
tables.hide(1000, myFunction); // Animación de ocultar elemento a 1 segundo y luego la funcion de devolución

// 'fadeTo' requiere de una opacidad como su segundo parámetro
tables.fadeTo(2000, 0.1, myFunction); // 2 segundos. decolorar a opacidad de 0.1 luego la función

// Puede conseguir un efecto un poco más avanzado con el método 'animate'
tables.animate({margin-top:"+=50", height: "100px"}, 500, myFunction);
// El método 'animate' toma un objeto de CSS y los valores finales,
// Parámetro opcional de opciones para afinar la animación,
// Y por supuesto la función de devolución de llamada

///////////////////////////////////
// 3. Manipulación

// Estos son similares a los efectos, pero pueden hacer más
$('div').addClass('div') // Añade la clase div a todos los divs

// Métodos comunes de manipulación 
$('p').append('Hola mundo'); // Añade al final del elemento
$('p').attr('class'); // Obtiene atributo
$('p').attr('class', 'content'); // Configura atributos
$('p').hasClass('div'); //Devuelve verdadero si tiene la clase
$('p').height(); // Obtiene la altura del elemento o define la altura


// Para muchos métodos de manipulación, obtener información sobre un elemento
// consigue solamente el primer elemento coincidente
$('p').height(); // Obtiene sólo la altura de la primera etiqueta 'p'

// Puedes utilizar 'each' para recorrer todos los elementos
var heights = [];
$('p').each(function() {
  heights.push($(this.height)); // Añade todas las alturas "p" de la etiqueta a la matriz
});


```