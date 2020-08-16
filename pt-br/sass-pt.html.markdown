---
language: sass
filename: learnsass-pt.scss
contributors:
  - ["Laura Kyle", "https://github.com/LauraNK"]
  - ["Sean Corrales", "https://github.com/droidenator"]
translators:
  - ["Gabriel Gomes", "https://github.com/gabrielgomesferraz"]
  - ["Cássio Böck", "https://github.com/cassiobsilva"]
lang: pt-br
---

Sass é uma linguagem de extensão CSS que adiciona recursos, como variáveis, aninhamento, mixins e muito mais.
Sass (e outros pré-processadores, como [Less](http://lesscss.org/)) ajudam os desenvolvedores a escrever código de fácil manutenção e DRY (Do not Repeat Yourself).

Sass tem duas opções de sintaxe diferentes para escolher. SCSS, que tem a mesma sintaxe de CSS, mas com os recursos adicionais de Sass. Ou Sass (a sintaxe original), que usa o recuo, em vez de chaves e ponto e vírgula.
Este tutorial é escrito usando SCSS.

Se você já está familiarizado com CSS3, você será capaz de pegar Sass de forma relativamente rápida. Ele não fornece quaisquer novas opções de estilo, mas sim as ferramentas para escrever sua CSS de forma mais eficiente e fazer a manutenção mais fácilmente.

```scss


// Comentários de linha única são removidos quando Sass é compilado para CSS.

/* Comentários multi-line são preservados. */



/*Variáveis
==============================*/



/* É possível armazenar um valor CSS (tais como a cor) de uma variável.
Use o símbolo "$" para criar uma variável. */

$primary-color: #A3A4FF;
$secondary-color: #51527F;
$body-font: 'Roboto', sans-serif;

/* Você pode usar as variáveis em toda a sua folha de estilo.
Agora, se você quer mudar a cor, você só tem que fazer a mudança uma vez. */

body {
	background-color: $primary-color;
	color: $secondary-color;
	font-family: $body-font;
}

/* Quando compilar ficaria assim: */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}


/* Este é muito mais fácil de manter do que ter de mudar a cor
cada vez que aparece em toda a sua folha de estilo. */




/* Mixins
==============================*/



/* Se você achar que está escrevendo o mesmo código para mais de um
elemento, você pode armazenar esse código em um mixin.

Use a diretiva '@mixin', além de um nome para o seu mixin. */

@mixin center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* Você pode usar o mixin com '@include' e o nome mixin. */

div {
	@include center;
	background-color: $primary-color;
}

/* Após compilar ficaria assim: */
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #A3A4FF;
}


/* Você pode usar mixins para criar uma propriedade estenográfica. */

@mixin size($width, $height) {
	width: $width;
	height: $height;
}

/* O que você pode invocar passando argumentos de largura e altura. */

.rectangle {
	@include size(100px, 60px);
}

.square {
	@include size(40px, 40px);
}

/* Isso compilado ficará assim: */
.rectangle {
  width: 100px;
  height: 60px;
}

.square {
  width: 40px;
  height: 40px;
}



/* Funções
==============================*/



/* Sass fornece funções que podem ser utilizados para realizar uma variedade de
    tarefas. Considere o seguinte */

/* Funções pode ser chamado usando seu nome e passando o
    argumentos necessários */
    
body {
  width: round(10.25px);
}

.footer {
  background-color: fade_out(#000000, 0.25)
}

/* Compiles to: */

body {
  width: 10px;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}

/* Você também pode definir suas próprias funções. As funções são muito semelhantes 
   aos mixins. Ao tentar escolher entre uma função ou um mixin, lembre
   que mixins são os melhores para gerar CSS enquanto as funções são melhores para
   lógica que pode ser usado em todo o seu código Sass. Os exemplos na 
   seção "Operações Math" são candidatos ideais para se tornar um função 
   reutilizável. */

/* Esta função terá um tamanho de destino e o tamanho do pai (parent), calcular
   e voltar a percentagem */

@function calculate-percentage($target-size, $parent-size) {
  @return $target-size / $parent-size * 100%;
}

$main-content: calculate-percentage(600px, 960px);

.main-content {
  width: $main-content;
}

.sidebar {
  width: calculate-percentage(300px, 960px);
}

/* Compila para: */

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}



/* Extend (Herança)
============================== */



/*Extend é uma maneira de compartilhar as propriedades de um seletor com outro. */

.display {
	@include size(5em, 5em);
	border: 5px solid $secondary-color;
}

.display-success {
	@extend .display;
	border-color: #22df56;
}

/* Compiles to: */
.display, .display-success {
  width: 5em;
  height: 5em;
  border: 5px solid #51527F;
}

.display-success {
  border-color: #22df56;
}

/* Ao ampliar uma declaração CSS é preferível a criação de um mixin,
   por causa da maneira em que agrupa as classes com todos que compartilham
   o mesmo estilo base. Se isso for feito com um mixin, a largura,
   altura, e a borda seria duplicado para cada instrução que
   o chamado mixin. Enquanto isso não irá afetar o seu fluxo de trabalho, será
   adicionado inchaço desnecessário para os arquivos criados pelo compilador Sass. */



/* Assentamento
==============================*/



/* Sass permite seletores ninhos dentro seletores */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #FF0000;
	}
}

/* '&' será substituído pelo selector pai (parent). */
/* Você também pode aninhar pseudo-classes. */
/* Tenha em mente que o excesso de nidificação vai fazer seu código menos sustentável.
Essas práticas também recomendam não vai mais de 3 níveis de profundidade quando nidificação.
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



/* Parciais e Importações
==============================*/


/* Sass permite criar arquivos parciais. Isso pode ajudar a manter seu Sass
   código modularizado. Arquivos parciais deve começar com um '_', por exemplo, _reset.css.
   Parciais não são geradas em CSS. */



/* Considere o seguinte CSS que nós vamos colocar em um arquivo chamado _reset.css */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

/* Sass oferece @import que pode ser usado para importar parciais em um arquivo.
   Isso difere da declaração CSS @import tradicional, que faz
   outra solicitação HTTP para buscar o arquivo importado. Sass converte os
   arquivo importados e combina com o código compilado. */

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



/* Placeholder Selectors
==============================*/  


/* Os Placeholders são úteis na criação de uma declaração CSS para ampliar. Se você
   deseja criar uma instrução CSS que foi usado exclusivamente com @extend,
   você pode fazer isso usando um Placeholder. Placeholder começar com um '%' em vez
   de '.' ou '#'. Placeholder não aparece no CSS compilado. */

%content-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  @extend %content-window;
  background-color: #0000ff;
}

/* Compilado para: */

.message-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  background-color: #0000ff;
}



/* Operações Math
============================== */


/* Sass fornece os seguintes operadores: +, -, *, /, e %. estes podem
   ser úteis para calcular os valores diretamente no seu arquivos Sass em vez
   de usar valores que você já calculados manualmente. O exemplo abaixo é
   de um projeto simples de duas colunas. */

$content-area: 960px;
$main-content: 600px;
$sidebar-content: 300px;

$main-size: $main-content / $content-area * 100%;
$sidebar-size: $sidebar-content / $content-area * 100%;
$gutter: 100% - ($main-size + $sidebar-size);

body {
  width: 100%;
}

.main-content {
  width: $main-size;
}

.sidebar {
  width: $sidebar-size;
}

.gutter {
  width: $gutter;
}

/* Compiles to: */

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



## SASS ou Sass?
Alguma vez você já se perguntou se Sass é um acrônimo ou não? Você provavelmente não tem, mas vou dizer-lhe de qualquer maneira. O nome do idioma é uma palavra, "Sass", e não uma sigla.
Porque as pessoas estavam constantemente a escrevê-lo como "SASS", o criador da linguagem de brincadeira chamou de "StyleSheets Sintaticamente Incríveis".


## Prática Sass
Se você quiser jogar com Sass em seu navegador, vá para [SassMeister](http://sassmeister.com/).
Você pode usar uma sintaxe, basta ir para as configurações e selecionar Sass ou SCSS.


## Compatibilidade

Sass pode ser usado em qualquer projeto, desde que você tenha um programa para compilá-lo
em CSS. Você vai querer verificar se o CSS que você está usando é compatível
com os seus navegadores de destino.

[QuirksMode CSS](http://www.quirksmode.org/css/) e [CanIUse](http://caniuse.com) são ótimos recursos para verificação de compatibilidade.


## Leitura
* [Official Documentation](http://sass-lang.com/documentation/file.SASS_REFERENCE.html)
* [The Sass Way](http://thesassway.com/) fornece tutoriais (iniciante avançados) e artigos.
