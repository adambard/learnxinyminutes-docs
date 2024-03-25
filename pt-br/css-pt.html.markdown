---
language: css
filename: learncss-pt.css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
    - ["Geoffrey Liu", "https://github.com/g-liu"]
    - ["Connor Shea", "https://github.com/connorshea"]
    - ["Deepanshu Utkarsh", "https://github.com/duci9y"]
translators:
    - ["Adaías Magdiel", "https://adaiasmagdiel.com/"]
    - ["Gabriel Gomes", "https://github.com/gabrielgomesferraz"]
    - ["Gabriele Luz", "https://github.com/gabrieleluz"]
lang: pt-br
---

Páginas web são feitas utilizando HTML para demarcar o conteúdo.
O CSS (Cascading Style Sheets) é uma linguagem separada que é responsável
especificamente pela **aparência** da página.

Códigos CSS são construídos utilizando várias regras estáticas. Cada regra aceita um ou mais *seletores*
e aplica *valores* específicos de propriedades visuais. Essas propriedades são aplicadas
nos elementos da página indicados pelos seletores.

Este guia foi escrito com o CSS 2 em mente, complementado pelas novas
funcionalidades do CSS 3.

**NOTA:** Devido ao fato do CSS produzir resultados visuais, a fim de aprender, você precisa treinar em um playground CSS como [dabblet](http://dabblet.com/).
O foco principal deste artigo é a sintaxe e algumas dicas gerais.

```css
/* Comentários aparecem dentro de blocos com / e *, tal como esta linha!
   Não há "comentários de uma linha"; este é o único estilo de comentário. * /

/* ####################
   ## SELETORES
   #################### */

/* O seletor é usado para selecionar um elemento em uma página. */
seletor { propriedade: valor; /* Mais propriedades... */ }

/*
Abaixo um elemento de exemplo:

<div class='class1 class2' id='anID' attr='value' otherAttr='pt-br foo bar' />
*/

/* Você pode seleciona-lo usando uma das suas classes CSS */
.class1 { }

/* Ou ambas as classes! */
.class1.class2 { }

/* Ou o seu nome */
div { }

/* Ou o seu id */
#anID { }

/* Ou através de um dos seus atributos! */
[attr] { font-size: smaller; }

/* Ou utilizando um atributo com um valor específico */
[attr='value'] { font-size: smaller; }

/* Um atributo que começa com um valor (CSS 3) */
[attr^='val'] { font-size:smaller; }

/* Ou termina com um valor (CSS 3) */
[attr$='ue'] { font-size: smaller; }

/* Ou contém um valor em uma lista separada por espaços */
[otherAttr~='foo'] {}
[otherAttr~='bar'] {}

/* Ou contém um valor em uma lista separada por hífen, ou seja, "-" (U + 002D) */
[otherAttr|='pt'] {font-size: smaller; }


/* Você pode combinar diferentes seletores para criar um seletor mais específica. Lembre-se
   de não colocar espaço entre eles */
div.some-class[attr$='ue'] {}

/* Você pode selecionar um elemento que está diretamento dentro de outro elemento */
div.some-parent > .class-name {}

/* Ou um descendente desse elemento. Os filhos são os descendentes diretos de
  um elemento pai, apenas um nível abaixo. Da seguinte forma, você pode seleciona qualquer
  elemento que esteja dentro do elemento principal. */
div.some-parent .class-name {}

/* Atenção: o mesmo seletor sem espaço tem um outro significado.
   Dessa forma você estará selecionando um elemento que contenha as duas classes. */
div.some-parent.class-name {}

/* Você também pode selecionar um elemento com base em seu irmão mais próximo */
.i-am-just-before + .this-element {}

/* Ou qualquer irmão que o precede */
.i-am-any-element-before ~ .this-element { }

/* Existem alguns seletores, chamados pseudo classes, que podem ser usados para selecionar um
   elemento quando ele está em um determinado estado */

/* Por exemplo, quando o cursor passa sobre um elemento */
seletor:hover {}

/* Ou um link foi visitado */
seletor:visited {}

/* Ou não tenha sido visitado */
seletor:link {}

/* Ou um elemento em foco */
seletor:focus {}

/* Qualquer elemento que é o primeiro filho */
seletor:first-child {}

/* Qualquer elemento que é o último filho */
seletor:last-child {}

/* Assim como pseudo classes, pseudo elementos permitem que você estilize certas partes de um documento */

/* Corresponde a um primeiro filho virtual do elemento selecionado */
seletor::before {}

/* Corresponde a um último filho virtual do elemento selecionado */
seletor::after {}

/* Nos locais apropriados, um asterisco pode ser utilizado como um curinga para selecionar
   todos os elementos */

* {} /* Todos os elementos */
.parent * {} /* todos os descendentes */
.parent > * {} /* todos os filhos */

/* ####################
   ## PROPRIEDADES
   #################### */

seletor {
    /* Unidades de comprimento pode ser absoluta ou relativa. */

    /* Unidades relativas */
    width: 50%; /* Percentagem de largura do elemento pai */
    font-size: 2em; /* Múltiplos de font-size original de elemento */
    font-size: 2rem; /* Ou do elemento raiz font-size */
    font-size: 2vw; /* Múltiplos de 1% da largura da janela de exibição (CSS 3) */
    font-size: 2vh; /* Ou a sua altura */
    font-size: 2vmin; /* Qualquer um de VH ou um VW é menor */
    font-size: 2vmax; /* Ou superior */

    /* Unidades absolutas */
    width: 200px; /* Píxeis */
    font-size: 20pt; /* Pontos */
    width: 5cm; /* Centímetros */
    min-width: 50mm; /* Milímetros */
    max-width: 5 polegadas; /* Polegadas */

    /* Cores */
    color: #F6E; /* Formato hexadecimal curto */
    color: #FF66EE; /* Formato hexadecimal longo */
    color: tomato; /* Uma cor nomeada */
    color: rgb(255, 255, 255); /* Como valores rgb */
    color: RGB(10%, 20%, 50%); /* Como porcentagens rgb */
    color: rgba(255, 0, 0, 0,3); /* Como valores RGBA (CSS 3) NOTA: 0 <a <1 */
    color: transparent; /* Equivale a definir o alfa a 0 */
    color: HSL(0, 100%, 50%); /* Como porcentagens HSL (CSS 3) */
    color: HSLA(0, 100%, 50%, 0,3); /* Como porcentagens HSLA com alfa */

    /* Imagens como fundos de elementos */
    background-image: url(/img-path/img.jpg); /* O uso das aspas dentro de url() é opcional */

    /* Fontes */
    font-family: Arial;
    /* Se o nome da família de fonte tem um espaço, deve estar entre aspas */
    font-family: "Courier New";
    /* Se o primeiro não for encontrada, o navegador usa a próxima, e assim por diante */
    font-family: "Courier New", Trebuchet, Arial, sans-serif;
}
```

## Uso

Salve o arquivo de estilos CSS com a extensão `.css`.

```html
<!-- Você precisa incluir o arquivo css dentro da tag <head>. Esse é o
     método recomendado. Consulte http://stackoverflow.com/questions/8284365 -->
<link rel="stylesheet" type="text/css" href="path/to/style.css" />

<!-- Você também pode incluir alguns CSS inline no seu HTML. -->
<style>
    a { color: purple; }
</style>

<!-- Ou definir propriedades CSS diretamente no elemento. -->
<div style="border: 1px solid red;">
</div>
```

## Precedência ou Cascata

Um elemento pode ser alvo de vários seletores e pode ter um conjunto de propriedades que
são adicionados mais de uma vez. Nestes casos, uma das regras tem precedência sobre as
outras. Geralmente, uma regra em um seletor mais específico têm precedência sobre um
menos específico, e uma regra que ocorre mais tarde na folha de estilo substitui uma anterior.

Este processo é chamado de cascata, daí vem o nome: Cascading Style Sheets (Folhas de Estilo em Cascata).

Dado o seguinte CSS:

```css
/* A */
p.class1[attr="value"]

/* B */
p.class1 {}

/* C */
p.class2 {}

/* D */
p { }

/* E */
p { property: value !important; }
```

e a seguinte marcação:

```html
<p style='/*F*/ property:value;' class='class1 class2' attr='value' />
```

A precedência de estilo é a seguinte: Lembre-se, a precedência se aplica a
cada **propriedade**, não ao bloco como um todo.

* `E` tem a precedência mais alta por causa de uma palavra-chave`!important`. É recomendado evitar seu uso.
* `F` é o seguinte, porque é um estilo interno.
* `A` é o seguinte, porque é "mais específico" do que os outros. Tem 3 especificadores: O nome do elemento `p`, a sua classe `class1` e um atributo `attr="value"`.
* `C` é o seguinte, mesmo que ele tenha a mesma especificidade que `B`. Isso acontece porque ele aparece logo após o `B`.
* `B` é o seguinte.
* `D` é o último.

## Media Queries

Media queries são recursos do CSS3 que permitem especificar quando determinadas regras de CSS devem ser aplicadas; é possível aplicar regras diferentes quando a página é impressa, quando a tela possui determinadas dimensões ou densidade de pixels e quando é lida por um leitor de tela. Media queries não adicionam especificidade ao seletor.

```css
/* Uma regra que será aplicada a todos os dispositivos */
h1 {
  font-size: 2em;
  color: white;
  background-color: black;
}

/* Altera a cor do h1 para utilizar menos tinta durante a impressão */
@media print {
  h1 {
    color: black;
    background-color: white;
  }
}

/* Altera o tamanho da fonte quando exibida numa tela com pelo menos 480px de largura */
@media screen and (min-width: 480px) {
  h1 {
    font-size: 3em;
    font-weight: normal;
  }
}
```

Media queries podem incluir os seguintes atributos: `width`, `height`, `device-width`, `device-height`, `orientation`, `aspect-ratio`, `device-aspect-ratio`, `color`, `color-index`, `monochrome`, `resolution`, `scan` e `grid`. A maioria desses atributos pode ser prefixada com `min-` ou `max-`.

O atributo `resolution` não é suportado em dispositivos mais antigos. Em vez disso, use `device-pixel-ratio`.

Muitos smartphones e tablets tentarão renderizar a página como se estivesse num desktop a menos que você utilize a meta-tag `viewport`.

```html
<head>
  <meta name="viewport" content="width=device-width; initial-scale=1.0">
</head>
```

## Compatibilidade

A maioria dos recursos do CSS 2 (e muitos do CSS 3) está disponível em todos os navegadores
e dispositivos.

## Recursos Adicionais

* Para executar uma verificação de compatibilidade rápida, [CanIUse](http://caniuse.com).
* CSS Playground [Dabblet](http://dabblet.com/).
* [Documentação CSS Mozilla Developer](https://developer.mozilla.org/en-US/docs/Web/CSS)
* [Codrops Referência CSS](http://tympanus.net/codrops/css_reference/)
* [DevTips CSS Basics](https://www.youtube.com/playlist?list=PLqGj3iMvMa4IOmy04kDxh_hqODMqoeeCy) (Tutorials)

## Leitura adicional

* [Entendendo Estilo Precedência em CSS: Especificidade, Herança, e o Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [Selecionando elementos usando atributos](https://css-tricks.com/almanac/selectors/a/attribute/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - O contexto de empilhamento](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
* [SASS](http://sass-lang.com/) e [Less](http://lesscss.org/) para pré-processamento do CSS
* [CSS-Tricks](https://css-tricks.com)
