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
    - ["Gabriel Gomes", "https://github.com/gabrielgomesferraz"]
lang: pt-br
---

Nos primeiros dias da web não havia elementos visuais, apenas texto puro. Mas com maior desenvolvimento de navegadores da web, páginas web totalmente visuais também se tornou comum.

CSS ajuda a manter a separação entre o conteúdo (HTML) e o look-and-feel de uma página web.

CSS permite atingir diferentes elementos em uma página HTML e atribuir diferentes propriedades visuais para eles.

Este guia foi escrito para CSS2, embora CSS3 está rapidamente se tornando popular.

**NOTA:** Porque CSS produz resultados visuais, a fim de aprender, você precisa tentar de tudo em um playground CSS como [dabblet](http://dabblet.com/).
O foco principal deste artigo é sobre a sintaxe e algumas dicas gerais.

```css
/* Comentários aparecem dentro do slash-asterisk, tal como esta linha!
   não há "comentários de uma linha"; este é o único estilo de comentário * /

/* ####################
   ## SELETORES
   #################### */

/* O seletor é usado para direcionar um elemento em uma página.
   seletor { propriedade: valor; / * Mais propriedades ... * / }

/*
Abaixo um elemento de exemplo:

<div class='class1 class2' id='anID' attr='value' otherAttr='pt-br foo bar' />
*/

/* Você pode direciona-lo usando uma das suas classes CSS */
.class1 { }

/* ou ambas as classes! */
.class1.class2 { }

/* ou o seu nome */
div { }

/* ou o seu id */
#anID { }

/* ou utilizando o fator de que tem um atributo!*/
[attr] { font-size:smaller; }

/* ou que o atributo tem um valor específico */
[attr='value'] { font-size:smaller; }

/* começa com um valor (CSS 3) */
[attr^='val'] { font-size:smaller; }

/* ou terminando com um valor (CSS 3) */
[attr$='ue'] { font-size:smaller; }


/* Ou contém um valor em uma lista separada por espaços */
[otherAttr ~ = 'foo'] {}
[otherAttr ~ = 'bar'] {}

/* Ou contém um valor em uma lista separada por hífen, ou seja, "-" (U + 002D) */
[otherAttr | = 'en'] {font-size: smaller; }


/* Você pode concatenar diferentes seletores para criar um seletor mais estreito. Não
   colocar espaços entre eles. */
classe div.some [attr $ = 'ue'] {}

/* Você pode selecionar um elemento que é filho de outro elemento */
div.some-parent> .class-name {}

/* Ou um descendente de um outro elemento. As crianças são os descendentes diretos de
   seu elemento pai, apenas um nível abaixo da árvore. Pode ser qualquer descendentes
   nivelar por baixo da árvore. */
div.some-parent class-name {}

/* Atenção: o mesmo seletor sem espaço tem um outro significado.
   Você consegue adivinhar o que? */
div.some-parent.class-name {}

/* Você também pode selecionar um elemento com base em seu irmão adjacente */
.i am just-antes + .Este elemento {}

/* Ou qualquer irmão que o precede */
.i am-qualquer-elemento antes ~ .Este elemento {}

/* Existem alguns selectores chamado pseudo classes que podem ser usados para selecionar um
   elemento quando ele está em um determinado estado */

/* Por exemplo, quando o cursor passa sobre um elemento */
seletor:hover {}

/* Ou um link foi visitado */
seletor:visited {}

/* Ou não tenha sido visitado */
seletor:link {}

/* Ou um elemento em foco */
seletor:focus {}

/* Qualquer elemento que é o primeiro filho de seu pai */
seletor:first-child {}

/* Qualquer elemento que é o último filho de seu pai */
seletor:last-child {}

/* Assim como pseudo classes, pseudo elementos permitem que você estilo certas partes de um documento */

/* Corresponde a um primeiro filho virtual do elemento selecionado */
seletor::before {}

/* Corresponde a um último filho virtual do elemento selecionado */
seletor::after {}

/* Nos locais apropriados, um asterisco pode ser utilizado como um curinga para selecionar todos
   elemento */
* {} /* */ Todos os elementos
.parent * {} /* */ todos os descendentes
.parent> * {} /* */ todas as crianças

/* ####################
   ## PROPRIEDADES
   #################### */

seletor {

    /* Unidades de comprimento pode ser absoluta ou relativa. */

    /* Unidades relativas */
    width: 50%; /* Percentagem de largura elemento pai */
    font-size: 2em; /* Múltiplos de font-size original de elemento */
    font-size: 2rem; /* Ou do elemento raiz font-size */
    font-size: 2vw; /* Múltiplos de 1% da largura da janela de exibição (CSS 3) */
    font-size: 2vh; /* Ou a sua altura */
    font-size: 2vmin; /* Qualquer um de VH ou um VW é menor */
    font-size: 2vmax; /* Ou superior */

    /* Unidades absolutas */
    width: 200px; /* píxeis */
    font-size: 20pt; /* Pontos */
    width: 5cm; /* Centímetros */
    min-width: 50mm; /* Milímetros */
    max-width: 5 polegadas; /* Polegadas */

    /* Cores */
    color: # F6E; /* Formato hexadecimal curto */
    color: # FF66EE; /* Formato hexadecimal longo */
    color: tomato; /* Uma cor nomeada */
    color: rgb (255, 255, 255); /* Como valores rgb */
    color: RGB (10%, 20%, 50%); /* Como porcentagens rgb */
    color: rgba (255, 0, 0, 0,3); /* Como valores RGBA (CSS 3) NOTA: 0 <a <1 */
    color: transparent; /* Equivale a definir o alfa a 0 */
    color: HSL (0, 100%, 50%); /* Como porcentagens HSL (CSS 3) */
    color: HSLA (0, 100%, 50%, 0,3); /* Como porcentagens HSLA com alfa */

    /* Imagens como fundos de elementos */
    background-image: url (/img-path/img.jpg); /* Citações dentro url () opcional */

    /* Fontes */
    font-family: Arial;
    /* Se o nome da família de fonte tem um espaço, deve ser citado */
    font-family: "Courier New";
    /* Se o primeiro não for encontrada, o navegador usa o próximo, e assim por diante */
    font-family: "Courier New", Trebuchet, Arial, sans-serif;
}
```

## Uso

Guardar uma folha de estilo CSS com a extensão `.css`.

```xml
<!-- Você precisa incluir o arquivo css no da sua página <head>. Isto é o
     método recomendado. Consulte http://stackoverflow.com/questions/8284365 -->
<link rel='stylesheet' type='text/css' href='path/to/style.css' />

<!-- Você também pode incluir alguns CSS inline na sua marcação. -->
<style>
   a { color: purple; }
</style>

<!-- Ou diretamente definir propriedades CSS no elemento. -->
<div style="border: 1px solid red;">
</div>
```

## Precedência ou Cascata

Um elemento pode ser alvo de vários seletores e pode ter um conjunto de propriedades em que mais de uma vez. Nestes casos, uma das regras tem precedência sobre os outros. Geralmente, uma regra em um seletor mais específico têm precedência sobre um menos específico, e uma regra que ocorre mais tarde na folha de estilo substitui uma anterior.

Este processo é chamado de cascata, portanto, as Fichas de nome de estilo em cascata.

Dado o seguinte CSS:

```css
/* UMA */
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

```xml
<p style='/*F*/ property:value;' class='class1 class2' attr='value' />
```

A precedência de estilo é a seguinte. Lembre-se, a precedência é para cada  **propriedade**, não para todo o bloco.

* `E` tem a precedência mais alta por causa de uma palavra-chave`!important`. É recomendável que você evitar seu uso.
* `F` é a próxima, porque é um estilo interno.
* `A` é a próxima, porque é mais" específico "do que qualquer outra coisa. Tem 3 especificadores: O nome do elemento `p`, o seu `class1` classe, um atributo `attr='value'`.
* `C` está próximo, mesmo que ele tenha a mesma especificidade que `B`. Isso é porque ele aparece depois de `B`.
* `B` é o próximo.
* `D` é a última.

## Compatibilidade

A maior parte dos recursos do CSS 2 (e muitos em CSS 3) estão disponíveis em todos os navegadores e dispositivos. Mas é sempre boa prática para verificar antes de usar um novo recurso.

## Recursos

* Para executar uma verificação de compatibilidade rápida, [CanIUse](http://caniuse.com).
* CSS Playground [Dabblet](http://dabblet.com/).
* [Documentação CSS Mozilla Developer Rede](https://developer.mozilla.org/en-US/docs/Web/CSS)
* [Codrops 'Referência CSS](http://tympanus.net/codrops/css_reference/)

## Leitura adicional

* [Entendendo Estilo Precedência em CSS: Especificidade, Herança, eo Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [Selecionando elementos usando atributos](https://css-tricks.com/almanac/selectors/a/attribute/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - O empilhamento context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
* [SASS](http://sass-lang.com/) e [menos](http://lesscss.org/) para CSS pré-processamento
* [CSS-Tricks](https://css-tricks.com)
