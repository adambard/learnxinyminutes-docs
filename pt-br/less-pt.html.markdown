---
language: less
filename: learnless-br.less
contributors:
  - ["Saravanan Ganesh", "http://srrvnn.me"]

lang: pt-br
---

Less é um pré-processador de CSS, que adiciona recursos como variáveis, aninhamento, mixins e muito mais.
Less (e outros pré-processadores, como o [Sass](http://sass-lang.com/)) ajudam os desenvolvedores a escreverem código que pode ser mantido e DRY (não se repita).

```css


//Comentários de linha única são removidos quando Less é compilado para CSS.

/*Comentários de várias linhas são preservados.*/



/* Variáveis
==============================*/


/* Você pode armazenar um valor de CSS (como uma cor) em uma variável.
   Use o símbolo '@' para criar uma variável. */

@primary-color: #a3a4ff;
@secondary-color: #51527f;
@body-font: 'Roboto', sans-serif;

/* Você pode usar as variáveis ​​em toda a sua folha de estilo.
   Agora, se você quiser alterar uma cor, só precisa fazer a alteração uma vez. */

body {
	background-color: @primary-color;
	color: @secondary-color;
	font-family: @body-font;
}

/* Isso compilará para: */

body {
	background-color: #a3a4ff;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}


/* Isso é muito mais sustentável do que ter que mudar a cor
   cada vez que aparece em toda a sua folha de estilo. */



/* Mixins
==============================*/


/* Se você achar que está escrevendo o mesmo código para mais de um
   elemento, você pode querer reutilizá-lo facilmente. */

.center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* Você pode usar o mixin simplesmente adicionando o seletor como um estilo. */

div {
	.center;
	background-color: @primary-color;
}

/* Que compilaria para: */

.center {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
}
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #a3a4ff;
}

/* Você pode omitir o código mixin de ser compilado adicionando parênteses
   depois do seletor. */

.center() {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
}

div {
  .center;
  background-color: @primary-color;
}

/* Que compilaria para: */
div {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
  background-color: #a3a4ff;
}



/* Aninhamento
==============================*/


/* Less permite aninhar seletores nos seletores. */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #f00;
	}
}

/* '&' será substituído pelo seletor pai. */
/* Você também pode aninhar pseudo-classes. */
/* Tenha em mente que o aninhamento excessivo tornará seu código menos sustentável.
   As melhores práticas recomendam não ultrapassar 3 níveis de profundidade ao aninhar.
   Por exemplo: */

ul {
  list-style-type: none;
  margin-top: 2em;

  li {
    background-color: red;

    &:hover {
      background-color: blue;
    }

    a {
      color: white;
    }
  }
}

/* Compila para: */

ul {
  list-style-type: none;
  margin-top: 2em;
}

ul li {
  background-color: red;
}

ul li:hover {
  background-color: blue;
}

ul li a {
  color: white;
}



/* Functions
==============================*/


/* Less fornece funções que podem ser usadas para realizar uma variedade de
   tarefas. Considere o seguinte: */

/* Funções podem ser invocadas usando seu nome e passando os
   argumentos requeridos. */

body {
  width: round(10.25px);
}

.header {
	background-color: lighten(#000, 0.5);
}

.footer {
  background-color: fadeout(#000, 0.25)
}

/* Compila para: */

body {
  width: 10px;
}

.header {
  background-color: #010101;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}

/* Você também pode definir suas próprias funções. Funções são muito semelhantes às
   mixins. Ao tentar escolher entre uma função ou a um mixin, lembre-se
   que mixins são melhores para gerar CSS, enquanto as funções são melhores para
   lógica que pode ser usada em todo o seu código Less. Os exemplos na
   seção 'Operadores Matemáticos' são candidatos ideais para se tornarem funções reutilizáveis. */

/* Esta função calcula a média de dois números: */

.average(@x, @y) {
  @average-result: ((@x + @y) / 2);
}

div {
  .average(16px, 50px); // "chama" o mixin
  padding: @average-result;    // use seu valor de "retorno"
}

/* Compila para: */

div {
  padding: 33px;
}



/* Estender (herança)
==============================*/


/* Estender é uma maneira de compartilhar as propriedades de um seletor com outro. */

.display {
  height: 50px;
}

.display-success {
  &:extend(.display);
	border-color: #22df56;
}

/* Compila para: */

.display,
.display-success {
  height: 50px;
}
.display-success {
  border-color: #22df56;
}

/* Estender uma instrução CSS é preferível para criar um mixin
   por causa da maneira como agrupa as classes que compartilham
   o mesmo estilo base. Se isso foi feito com um mixin, as propriedades
   seriam duplicadas para cada declaração que
   chamou o mixin. Embora isso não afete o seu fluxo de trabalho,
   adicione o inchaço desnecessário aos arquivos criados pelo compilador Less. */



/* Parciais e Importações
==============================*/


/* Less permite criar arquivos parciais. Isso pode ajudar a manter o seu
   código Less modularizado. Arquivos parciais convencionalmente começam com um '_',
   por exemplo. _reset.less. e são importados para um arquivo less principal que recebe
   o css compilado. */

/* Considere o seguinte CSS que vamos colocar em um arquivo chamado _reset.less */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

/* Less disponibiliza @import que podem ser usadas para importar parciais em um arquivo.
   Isso difere da declaração tradicional CSS @import que faz
   outra solicitação HTTP para buscar o arquivo importado. Less leva o
   arquivo importado e combina com o código compilado. */

@import 'reset';

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}

/* Compila para: */

html, body, ul, ol {
  margin: 0;
  padding: 0;
}

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}



/* Operações Matemáticas
==============================*/


/* Less fornece os seguintes operadores: +, -, *, / e %. Estes podem
   ser úteis para calcular valores diretamente nos seus arquivos Less
   para usar valores que você já calculou manualmente. Abaixo está um exemplo
   de como configurar um design simples de duas colunas. */

@content-area: 960px;
@main-content: 600px;
@sidebar-content: 300px;

@main-size: @main-content / @content-area * 100%;
@sidebar-size: @sidebar-content / @content-area * 100%;
@gutter: 100% - (@main-size + @sidebar-size);

body {
  width: 100%;
}

.main-content {
  width: @main-size;
}

.sidebar {
  width: @sidebar-size;
}

.gutter {
  width: @gutter;
}

/* Compila para: */

body {
  width: 100%;
}

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}

.gutter {
  width: 6.25%;
}


```

## Pratique Less

Se você quiser praticar com Less no seu navegador, confira: * [Codepen](http://codepen.io/) * [LESS2CSS](http://lesscss.org/less-preview/)

## Compatibilidade

Less pode ser usado em qualquer projeto, desde que você tenha um programa para compilá-lo em CSS. Você deseja verificar
se o CSS que você está usando é compatível com seus navegadores de destino.

[QuirksMode CSS](http://www.quirksmode.org/css/) e [CanIUse](http://caniuse.com) são ótimos recursos para verificar a compatibilidade.

## Leitura adicional
* [Documentação Oficial](http://lesscss.org/features/)
* [Less CSS - Guia do iniciante](http://www.hongkiat.com/blog/less-basic/)
