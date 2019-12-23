---
language: stylus
filename: learnStylus-pt.styl
contributors:
  - ["Salomão Neto", "https://github.com/salomaosnff"]
  - ["Isaac Henrique", "https://github.com/Isaachi1"]
lang: pt-br
---

Stylus tem como propósito, adicionar funcionalidades às suas folhas de estilos CSS que te ajudam no desenvolvimento, sem que haja a quebra de compartibilidade entre os navegadores Web.
Entre elas estão variáveis, aninhamento, mixins, funções e muito mais.

A sintaxe do Stylus é muito flexivel podendo utilizar a sintaxe padrão do CSS e deixando opcional o ponto e vírgula (;), dois pontos (:) e até mesmo as chaves ({ e }), tornando assim o seu código ainda mais legível.

Stylus não fornece novas opções de estilos, mas dá funcionalidades que permitem deixar seu CSS muito mais dinâmico.


```scss

/* Estilo de código
==============================*/

/* As chaves, ponto e vírgula, e dois pontos são opcionais no Stylus. */

body {
  background: #000;
}

body {
  background: #000
}

body {
  background #000
}

body
  background #000

body
  background: #000;

body
  background: #000


// Comentários de linha única são removidos quando Stylus é compilado para CSS.

/* Comentários multi-line são preservados. */


/* Seletores
==============================*/

/* Selecionando elementos dentro de outro elemento */
body {
  background: #000000;
  h1 {
    color: #FF0000;
  }
}

/* Ou se preferir... */
body
  background #000000
  h1
    color #FF0000


/* Obtendo a referência do elemento pai
==============================*/
a {
  color: #0088dd;
  &:hover {
    color: #DD8800;
  }
}


/*Variáveis
==============================*/


/* 
  É possível armazenar um valor CSS (tais como a cor) de uma variável.
  Embora seja opcional, é recomendado adicionar $ antes de um nome de variável 
  para que você possa distinguir uma variável de outro valor CSS.
*/

$primary-color = #A3A4FF
$secondary-color = #51527F
$body-font = 'Roboto', sans-serif

/* Você pode usar as variáveis em toda a sua folha de estilo.
Agora, se você quer mudar a cor, você só tem que fazer a mudança uma vez. */

body
	background-color $primary-color
	color $secondary-color
	font-family $body-font

/* Quando compilar ficaria assim: */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}

/ * 
Este é muito mais fácil de manter do que ter de mudar a cor
cada vez que aparece em toda a sua folha de estilo. 
* /



/*Mixins
==============================*/

/* Se você achar que você está escrevendo o mesmo código para mais de um
elemento, você pode querer armazenar esse código em um mixin.

center()
  display block
	margin-left auto
	margin-right auto
	left 0
	right 0

/* Utilizando um mixin */
body {
  center()
  background-color: $primary-color
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

size($width, $height)
  width $width
  height $height

.rectangle
  size(100px, 60px)

.square
	size(40px, 40px)

/* Você pode usar um mixin como uma propriedade CSS. */
circle($ratio)
  width $ratio * 2
  height $ratio * 2
  border-radius $ratio

.ball
  circle 25px


/* Interpolação
==============================*/

vendor(prop, args)
  -webkit-{prop} args
  -moz-{prop} args
  {prop} args

border-radius()
  vendor('border-radius', arguments)

box-shadow()
  vendor('box-shadow', arguments)

button
  border-radius 1px 2px / 3px 4px

/* Funções
==============================*/

/* Funções no Stylus permitem fazer uma variedade de tarefas, como por exemplo, manipular algum dado. */

body {
  background darken(#0088DD, 50%) // Escurece a cor #0088DD em 50%
}

/* Criando sua própria função */
somar(a, b)
  a + b

body
  padding somar(10px, 5)

/* Condições
==============================*/
comparar(a, b)
  if a > b
    maior
  else if a < b
    menor
  else
    igual

comparar(5, 2)   // => maior
comparar(1, 5)   // => menor
comparar(10, 10) // => igual

/* Iterações
==============================*/

/**
Sintaxe de laço de repetição for:
for <val-name> [, <key-name>] in <expression>
**/

for $item in (1..2) /* Repete o bloco 12 vezes */
  .col-{$item}
    width ($item / 12) * 100% /* Calcula a largura pelo número da coluna*

```

Agora que você conhece um pouco sobre esse poderoso pré-processador de CSS, você está pronto para criar folhas de estilos mais dinâmicas. Para aprofundar seus conhecimentos visite a documentação oficial do stylus em http://stylus-lang.com.
