---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
    - ["Geoffrey Liu", "https://github.com/g-liu"]
    - ["Connor Shea", "https://github.com/connorshea"]
    - ["Deepanshu Utkarsh", "https://github.com/duci9y"]
translators:
    - ["Lucas Pugliesi Ferreira", "https://github.com/fplucas"]
filename: learncss.css
lang: pt-br
---

Nos primórdios da web, não haviam elementos visuais, apenas puro texto. Mas com maior desenvolvimento dos navegadores web, páginas totalmente visuais vieram a ser comuns.

CSS ajuda a manter uma separação entre o conteúdo (HTML) e o 'look-and-feel', aparência, de uma página web.

CSS permite você selecionar diferentes elementos em uma página HTML e designar propriedades e visuais diferentes para eles.

Este guia foi escrito para a versão CSS 2, embora CSS 3 está rapidamente se tornando popular.

**NOTA:** Pelo resultado visual que o CSS produz, a fim de aprender, você precisa tentar de tudo em um 'parque de diversões' do CSS, como o [dabblet](http://dabblet.com/).
O foco principal deste artigo, é mostrar a sintaxe e algumas dicas gerais.

```css
/* comentários aparecerão dentro de uma barra seguida por um asterisco, assim como esta linha!
   Não há comentários de uma linha; esta é a única maneira de se comentar */

/* ####################
   ## SELETORES
   #################### */

/* o seletor é usado para atingir um alvo na página.
seletor { propriedade: valor; /* mais propriedades...*/ }

/*
Aqui está um elemento de exemplo:

<div class='classe1 classe2' id='algumID' attr='valor' otherAttr='pt-br foo bar' />
*/

/* Você pode atingir esse elemento utilizando uma das classes CSS */
.classe1 { }

/* as duas classes! */
.classe1.classe2 { }

/* pelo nome */
div { }

/* pelo id */
#algumID { }

/* usando o fato de que isso tem um atributo! */
[attr] { font-size:smaller; }

/* que um atributo tem um valor específico */
[attr='value'] { font-size:smaller; }

/* comece com um valor (CSS 3) */
[attr^='val'] { font-size:smaller; }

/* termine com um valor (CSS 3) */
[attr$='or'] { font-size:smaller; }

/* contenha um valor na lista de outros atributos */
[otherAttr~='foo'] { }
[otherAttr~='bar'] { }

/* ou contenha um valor separado por hífen nna lista, ou seja, "-" (U+002D) */
[otherAttr|='pt'] { font-size:smaller; }


/* Voê pode concatenar diferentes seletores para criar um seletor mais específico. Não coloque espaço entre eles. */
div.some-class[attr$='or'] { }

/* Você pode selecionar um elemento que é filho de outro elemento */
div.algum-pai > .nome-da-classe { }

/* ou um descendente de outro elemento. Filhos são diretamente descendentes de
   seu elemento pai, apenas um nível abaixo da árvore. Descendentes podem estar
   em qualquer nível abaixo da árvore. */
div.algum-parente .nome-da-classe { }

/* Atenção: o mesmo seletor sem espaço, tem um significado diferente.
   Você pode adivinhar qual? */
div.algum-parente.nome-da-classe { }

/* Você pode também selecionar um elemento baseado em seu irmão adjacente */
.eu-estou-apenas-antes + .este-elemento { }

/* ou qualquer irmão anterior a esse */
.eu-sou-qualquer-elemento-anterior ~ .este-elemento { }

/* Aqui estão alguns seletores chamados pseudo-classes que podem ser usados para
   selecionar um elemento onde está em um lugar em particular */

/* Por exemplo, quando o ponteiro do mouse passa em cima de um elemento */
seletor:hover { }

/* um link que já foi visitado */
seletor:visited { }

/* um link que ainda não foi visitado */
seletor:link { }

/* um elemento em foco */
seletor:focus { }

/* algum elemento que é o primeiro filho do pai */
seletor:first-child {}

/* algum elemento que é o último filho do pai */
seletor:last-child {}

/* Assim como pseudo-classes, pseudo-elementos permitem você estilizar certas
   partes de um documento */

/* corresponde ao primeiro pseudo-filho do elemento selecionado */
seletor::before {}

/* corresponde ao último pseudo-filho do elemento selecionado */
seletor::after {}

/* Em locais apropriados, um asterisco pode ser usado como um curinga de cada
   elemento selecionado */
* { } /* todos os elementos */
.pai * { } /* todos descendentes */
.pai > * { } /* todos os filhos */

/* ####################
   ## PROPRIEDADES
   #################### */

seletor {
    
    /* Unidades de comprimento podem ser absolutas ou relativas. */
    
    /* Unidades relativas */
    width: 50%;       /* Porcentagem a partir da largura do elemento pai */
    font-size: 2em;   /* múltiplo do elemento font-size original */
    font-size: 2rem;  /* ou do elemento raiz font-size */
    font-size: 2vw;   /* múltiplos de 1% da largura da viewport (CSS 3) */
    font-size: 2vh;   /* ou sua altura */
    font-size: 2vmin; /* sua vw ou vh menor */
    font-size: 2vmax; /* ou maior */
    
    /* Unidades absolutas */
    width: 200px;     /* pixels */
    font-size: 20pt;  /* pontos */
    width: 5cm;       /* centímetros */
    min-width: 50mm;  /* milímetros */
    max-width: 5in;   /* polegadas */
    
    /* Cores */
    color: #F6E;                 /* formato hexadecimal resumido */
    color: #FF66EE;              /* formato hexadecimal longo */
    color: tomato;               /* nome da cor */
    color: rgb(255, 255, 255);   /* valor rgb */
    color: rgb(10%, 20%, 50%);   /* porcentagens de rgb */
    color: rgba(255, 0, 0, 0.3); /* valores rgb com opacidade (CSS 3) Nota: 0 < a < 1 */
    color: transparent;          /* equivalente a definir o alpha (opacidade) como 0 */
    color: hsl(0, 100%, 50%);    /* porcentagens de hsl (CSS 3) */
    color: hsla(0, 100%, 50%, 0.3); /* porcentagens de hsla com alpha (opacidade) */
    
    /* Imagens de fundo dos elementos */
    background-image: url(/img-path/img.jpg); /* aspas dentro da url() são opcionais */
    
    /* Fontes */
    font-family: Arial;
    /* se o nome da fonte family tem um espaço, o nome deverá ser escrito entre aspas */
    font-family: "Courier New";
    /* se a primeira fonte não for encontrada, o navegador usará a próxima e
    assim por diante */
    font-family: "Courier New", Trebuchet, Arial, sans-serif;
}
```

## Uso

Salve uma folha de estilo CSS com a extensão `.css`.

```xml
<!-- Você ŕecosa incluir o arquivo css em sua página <head>. Esse é o método
     recomendado. Referência em http://stackoverflow.com/questions/8284365 -->
<link rel='stylesheet' type='text/css' href='path/to/style.css' />

<!-- Você pode também incluir algum CSS dentro da sua marcação. -->
<style>
   a { color: purple; }
</style>

<!-- Ou diretamente informar as propriedades CSS do elemento. -->
<div style="border: 1px solid red;">
</div>
```

## Precedência or Cascata

Um elemento pode ser selecionado por múltiplos seletores e pode ter uma propriedade informada mais de uma vez. Nesses casos, uma das regras toma precedência sobre outras. Basicamente, a regra mais específica do seletor toma procedência sobre a uma menos específica, então a última sobrepõe a anterior.

Esse processo é chamado de cascata, consequentemente o nome Cascading Style Sheets (Folha de estilo em cascata).

Dados os seguintes CSS:

```css
/* A */
p.classe1[attr='valor']

/* B */
p.classe1 { }

/* C */
p.classe2 { }

/* D */
p { }

/* E */
p { propriedade: valor !important; }
```

e a seguinte marcação:

```xml
<p style='/*F*/ propriedade:valor;' class='classe1 classe2' attr='valor' />
```

A precedência desse estilo é como segue. Lembrando, a precedência é para cada
**propriedade**, não para o bloco inteiro.

* `E` tem a maior precedência por causa da palavra-chave `!important`. É
recomendado evitar o seu uso.
* `F` é o próximo, porque é um estilo em linha.
* `A` é o próximo, porque é mais "específico" do que qualquer outro. Contém 3
especificações: O nome do elemento `p`, sua classe `classe1` e um atributo `attr='valor'`.
* `C` é o próximo, mesmo tendo a mesma especificidade de `B`. Isso é porque
aparece depois de `B`.
* `B` é o próximo.
* `D` é o último.

## Compatibilidade

Muitas das ferramentas no CSS 2 (e muitas no CSS 3) são disponíveis para todos os navegadores e dispositivos. Porém é sempre uma boa prática checar antes de utilizar uma nova ferramenta.

## Recursos

* Para checar rápidamente a compatibilidade, [CanIUse](http://caniuse.com).
* CSS Playground [Dabblet](http://dabblet.com/).
* [Mozilla Developer Network's CSS documentation](https://developer.mozilla.org/en-US/docs/Web/CSS)
* [Codrops' CSS Reference](http://tympanus.net/codrops/css_reference/)

## Mais leituras

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [Selecting elements using attributes](https://css-tricks.com/almanac/selectors/a/attribute/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
* [SASS](http://sass-lang.com/) and [LESS](http://lesscss.org/) for CSS pre-processing
* [CSS-Tricks](https://css-tricks.com)
