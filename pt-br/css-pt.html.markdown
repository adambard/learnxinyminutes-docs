---
language: css
filename: learncss-ptbr.css
contributors: 
- ["Mohammad Valipour", "https://github.com/mvalipour"]
translators:
- ["Carolina Knoll", "https://github.com/carolinaknoll"]
lang: pt-br
---

Tutorial de CSS em Português

Nos primeiros dias da web não havia elementos visuais, apenas texto puro. Mas com maior desenvolvimento de navegadores da web, páginas web totalmente visuais também se tornaram comuns.

CSS ajuda a manter a separação entre o conteúdo (HTML) e o estilo visual de uma página web.

CSS permite atingir diferentes elementos em uma página HTML e atribuir diferentes propriedades visuais para eles.

Este guia foi escrito para CSS 2, embora CSS 3 está rapidamente se tornando popular.

NOTA: Como o CSS produz resultados visuais, para aprender você precisa tentar de tudo em um playground CSS como [dabblet] (http://dabblet.com/). O foco principal deste artigo é sobre a sintaxe e algumas dicas gerais.

/* Comentários são criados dentro deste conjunto de símbolos, assim como esta linha!
  Em CSS, não há "comentários de uma linha"; este é o único estilo de comentário. */

/* ####################
   ## SELETORES
   #################### */

/* O seletor é usado para selecionar um elemento em uma página. */
seletor {propriedade: valor; /* Mais propriedades ... */}

/*
Aqui está um elemento de exemplo:

<div class='classe1 classe2' id='umaID' attr='valor' otherAttr='en-us foo bar' />
*/

/* Você pode selecioná-lo através de uma classe! */
.classe1 { }

/* ou através das duas classes, ao mesmo tempo! */
.classe1.classe2 { }

/* ou por seu nome */
div { }

/* ou por sua ID */
#umaID { }

/* ou utilizando o fato de que se tem um atributo! */
[attr] { font-size:smaller; }

/* ou que esse atributo tem um valor específico */
[attr='valor'] { font-size:smaller; }

/* que inicia com um valor x (CSS 3) */
[attr^='val'] { font-size:smaller; }

/* ou que termina com este valor (CSS 3) */
[attr$='or'] { font-size:smaller; }

/* ou contém um valor em uma lista separada por espaços */
[otherAttr~='foo'] { }
[otherAttr~='bar'] { }

/* ou contém um valor em uma lista separada por hífen, ou seja, "-" (U + 002D) */
[otherAttr|='en'] { font-size:smaller; }


/* Você pode concatenar diferentes seletores para criar um seletor mais específico. não
    coloque espaços entre eles. */
div.some-class[attr$='or'] { }

/* Você pode selecionar um elemento que é filho de outro elemento */
div.some-parent > .class-name { }

/* ou um descendente de um outro elemento. As crianças são os descendentes diretos de
  seu elemento pai, apenas um nível abaixo da árvore. Descendentes podem ter qualquer nível abaixo na árvore. */
div.some-parent .class-name { }

/* Atenção: o mesmo seletor sem espaço tem um outro significado.
    Você consegue adivinhar o que? */
div.some-parent.class-name { }

/* Você também pode selecionar um elemento com base em seu irmão adjacente */
.i-am-just-before + .this-element { }

/* ou qualquer irmão que o precede */
.i-am-any-element-before ~ .this-element { }

/* Existem alguns selectores chamado pseudo classes que podem ser usados para selecionar um
    elemento quando ele está em um estado particular */

/* por exemplo, quando o cursor passa sobre um elemento */
selector:hover { }

/* ou quando um link foi visitado */
selector:visited { }

/* ou que ainda não foi visitado */
selected:link { }

/* ou um elemento em foco */
selected:focus { }

/* qualquer elemento que é o primeiro filho de seu pai */
selector:first-child {}

/* qualquer elemento que é o último filho de seu pai */
selector:last-child {}

/* Assim como pseudo classes, pseudo elementos permitem que você estilize certas partes de um documento  */

/* corresponde a um primeiro filho virtual do elemento selecionado */
selector::before {}

/* corresponde a um último filho virtual do elemento selecionado */
selector::after {}

/* Em locais adequados, um asterisco pode ser utilizado como um curinga para selecionar todos
  os elementos */
* { } /* todos os elementos */
.parent * { } /* todos os descendentes */
.parent > * { } /* todos os filhos */

/* ####################
   ## PROPRIEDADES
   #################### */

selector {

    /* Unidades de comprimento podem ser absolutas ou relativas. */

    /* Unidades relativas */
    width: 50%;       /* porcentagem de largura do elemento pai */
    font-size: 2em;   /* múltiplos do tamanho inicial da fonte do elemento */
    font-size: 2rem;  /* ou o tamanho da fonte do elemento raíz */
    font-size: 2vw;   /* múltiplos de 1% da largura da janela de exibição (CSS 3) */
    font-size: 2vh;   /* ou a sua altura */
    font-size: 2vmin; /* qualquer que seja o menor valor entre sua altura ou largura */
    font-size: 2vmax; /* ou então maior */

    /* Unidades absolutas */
    width: 200px;     /* em pixels */
    font-size: 20pt;  /* em pontos */
    width: 5cm;       /* em centímetros */
    min-width: 50mm;  /* em milímetros */
    max-width: 5in;   /* em polegadas */

    /* Cores */
    color: #F6E;                 /* valor em hexadecimal curto */
    color: #FF66EE;              /* valor em hexadecimal longo */
    color: tomato;               /* com o nome de uma cor */
    color: rgb(255, 255, 255);   /* com seu valor em RGB */
    color: rgb(10%, 20%, 50%);   /* com porcentagens em RGB */
    color: rgba(255, 0, 0, 0.3); /* com valores em RGB (CSS 3) Nota: 0 < a < 1 */
    color: transparent;          /* equivalente a deixar o valor alfa em 0 */
    color: hsl(0, 100%, 50%);    /* com porcentagens hsl (CSS 3) */
    color: hsla(0, 100%, 50%, 0.3); /* como porcentagens sla com alfa */

    /* Imagens como fundos de elementos */
    background-image: url(/caminho-da-imagem/imagem.jpg); /* as aspas dentro da url() são opcionais */

    /* Fontes */
    font-family: Arial;
    /* se o nome da fonte possui espaços, deve ser colocado entre aspas */
    font-family: "Courier New";
    /* se a primeira fonte não for encontrada, o navegador usa a próxima, e assim por diante */
    font-family: "Courier New", Trebuchet, Arial, sans-serif;
}

Uso

Sempre salve uma folha de estilos CSS como .css.

<!-- Você precisará incluir o arquivo .css na parte do <head> da sua página. 
<link rel='stylesheet' type='text/css' href='caminho/do/estilo.css' />

<!-- Também é possível incluir alguns estilos css inline em seu arquivo html. -->
<style>
   a { color: purple; }
</style>

<!-- Ou definir diretamente as propriedades CSS em seu elemento. -->
<div style="border: 1px solid red;">
</div>

Procedência em Cascata

Um elemento pode ser alvo de vários seletores e pode ter um conjunto de propriedades em que mais de uma vez. Nestes casos, uma das regras tem precedência sobre os outros. Geralmente, uma regra em um seletor mais específico têm precedência sobre um menos específico, e uma regra que ocorre mais tarde na folha de estilo substitui uma anterior.

Este processo é chamado de Cascata, por isso o nome Folhas de Estilo em Cascata (Cascading Style Sheets).

Dado o seguinte CSS:

/* A */
p.classe1[attr='valor']

/* B */
p.classe1 { }

/* C */
p.classe2 { }

/* D */
p { }

/* E */
p { property: valor !important; }

e a seguinte marcação:

<p style='/*F*/ propriedade:valor;' class='classe1 classe2' attr='valor' />

A precedência de estilo é a seguinte. Lembre-se, a precedência é para cada propriedade, e não para todo o bloco.

    E tem a precedência mais alta por causa da palavra-chave !important. É recomendável que você evite seu uso.
    F é a próxima, porque é um estilo interno (inline).
    A é a próxima, porque é mais "específica" do que qualquer outra coisa. Possui 3 especificadores: O nome do elemento p, sua classe classe1, e um atributo attr = 'valor'.
    C é a próxima, embora possua a mesma especificidade como B. Isto acontece porque C aparece depois de B.
    B é a próxima.
    D é, por fim, a última.

Compatibilidade

A maior parte dos recursos do CSS 2 (e muitos em CSS 3) estão disponíveis em todos os navegadores e dispositivos. Mas é sempre boa prática para verificar antes de usar um novo recurso.

Referências

    [To run a quick compatibility check, CanIUse.] (http://caniuse.com/)
    [CSS Playground Dabblet.] (http://dabblet.com/)
    [Mozilla Developer Network’s CSS documentation] (https://developer.mozilla.org/en-US/docs/Web/CSS)
    [Codrops CSS Reference] (http://tympanus.net/codrops/css_reference/)

Leitura Adicional

    [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade] (http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
    [Selecting elements using attributes] (https://css-tricks.com/almanac/selectors/a/attribute/)
    [QuirksMode CSS] (http://www.quirksmode.org/css/)
    [Z-Index - The stacking context] (https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
    [[SASS] (http://sass-lang.com/)  and [LESS] (http://lesscss.org/) for CSS pre-processing]
    [CSS-Tricks] (https://css-tricks.com/)

Possui uma sugestão? Uma correção, talvez? Abra um issue no Repositório GitHub, ou então faça um pull request!
