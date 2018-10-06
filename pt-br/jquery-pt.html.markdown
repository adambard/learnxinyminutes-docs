---
category: tool
tool: jquery
contributors:
    - ["Sawyer Charles", "https://github.com/xssc"]
translators:
    - ["Nikolas Silva", "https://github.com/nikolassilva"]
filename: jquery-pt.js
lang: pt-br
---

jQuery é uma biblioteca JavaScript que te ajuda a "fazer mais, escrevendo menos". Ela faz com que muitas tarefas comuns em JavaScript sejam mais simples de escrever. jQuery é usado por grandes empresas e desenvolvedores do mundo todo. Ela torna o AJAX, manipulação de eventos, manipulação do DOM, entre outros, mais fácil e rápido.

Pelo jQuery ser uma biblioteca JavaScript você deve [aprende-lo primeiro](https://learnxinyminutes.com/docs/pt-br/javascript-pt/)

```js


///////////////////////////////////
// 1. Seletores

// Seletores no jQuery são usados para selecionar um elemento
var page = $(window); // Seleciona toda a viewport

// Seletores também podem ser na forma do CSS
var paragraph = $('p'); // Seleciona todos elementos de parágrafo
var table1 = $('#table1'); // Seleciona o elemento com id 'table1'
var squares = $('.square'); // Seleciona todos elementos com classe 'square'
var square_p = $('p.square') // Seleciona todos elementos de parágrafo com a classe 'square'


///////////////////////////////////
// 2. Eventos e Efeitos
// jQuery é muito bom em manipular o que acontece quando um evento é disparado
// Um evento muito usado é o 'ready'  
// Você pode usar o método ready para esperar até que um elemento tenha terminado de carregar
$(document).ready(function(){
  // O código não será executado até que o documento carregue
});
// Você também pode usar funções declaradas
function onAction() {
  // Isso será executado quando um evento for disparado
}
$('#btn').click(onAction); // Chama 'onAction' quando o elemento receber um clique

// Outros eventos comuns são:
$('#btn').dblclick(onAction); // Clique duplo
$('#btn').hover(onAction); // Mouse sobre elemento
$('#btn').focus(onAction); // Elemento recebe foco
$('#btn').blur(onAction); // Elemento perde foco
$('#btn').submit(onAction); // Envio de formulário
$('#btn').select(onAction); // Quando o elemento é selecionado
$('#btn').keydown(onAction); // Quando uma tecla é segurada
$('#btn').keyup(onAction); // Quando uma tecla é solta
$('#btn').keypress(onAction); // Quando uma tecla é pressionada
$('#btn').mousemove(onAction); // Quando o mouse é movido
$('#btn').mouseenter(onAction); // Quando o mouse entra no elemento
$('#btn').mouseleave(onAction); // Quando o mouse sai do elemento


// Eles também podem disparar os eventos em vez de manipulá-los,
// simplesmente deixando de passar os parâmetros
$('#btn').dblclick(); // Dispara um clique duplo no elemento

// Você pode manipular múltiplos eventos usando o seletor apenas uma vez
$('#btn').on(
  {dblclick: myFunction1} // Disparado num clique duplo
  {blur: myFunction1} // Disparado quando perder o foco
);

// Você pode mover e esconder elementos com alguns métodos de efeito
$('.table').hide(); // Esconde o elemento

// Nota: chamar uma função nesse método ainda irá esconder o elemento
$('.table').hide(function(){
    // Elemento é escondido e a função é executada
});

// Você pode guardar seletores em variáveis
var tables = $('.table');

// Alguns métodos básicos de manipulação do DOM:
tables.hide(); // Esconde elemento(s)
tables.show(); // Exibe elemento(s)
tables.toggle(); // Alterna entre esconder/exibir
tables.fadeOut(); // Efeito fade out
tables.fadeIn(); // Efeito fade in
tables.fadeToggle(); // Alterna entre fade out/in
tables.fadeTo(0.5); // Efeito fade com opacidade específica (entre 0 e 1)
tables.slideUp(); // Efeito de deslize pra cima
tables.slideDown(); // Efeito de deslize pra baixo
tables.slideToggle(); // Alterna entre deslizar pra cima/baixo

// Todos os métodos acima levam velocidade (em milissegundos) e uma função callback
tables.hide(1000, myFunction); // Esconde o elemento em 1 segundo e chama a função

// No fadeTo é obrigatório definir a opacidade como segundo parâmetro
tables.fadeTo(2000, 0.1, myFunction); // 2 segundos de fade para 0.1 de opacidade e chama a função

// Você pode fazer animações mais avançadas com o método animate
tables.animate({'margin-top':"+=50", height: "100px"}, 500, myFunction);
// O método animate leva um objeto com valores CSS,
// um parâmetro de opções para melhorar a animação
// e uma função callback, como de costume

///////////////////////////////////
// 3. Manipulação

// São similares aos efeitos, mas podem fazer mais
$('div').addClass('taming-slim-20'); // Adiciona a classe taming-slim-20 em todas as divs

// Métodos comuns de manipulação
$('p').append('Hello world'); // Adiciona ao final do elemento
$('p').attr('class'); // Obtém o valor de um atributo
$('p').attr('class', 'content'); // Define o valor de um atributo
$('p').hasClass('taming-slim-20'); // Retorna true se tiver a classe
$('p').height(); // Obtém/define a altura do elemento


// Pra maioria dos métodos de manipulação, pegar o valor de um 
// elemento só afetará o primeiro deles
$('p').height(); // Obtém a altura da primeira tag 'p'

// Você pode usar o método each pra percorrer os elementos
var heights = [];
$('p').each(function() {
  heights.push($(this).height()); // Adiciona a altura das tags 'p' na array
});


```
