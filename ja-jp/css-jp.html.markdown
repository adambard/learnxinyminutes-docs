---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
    - ["Geoffrey Liu", "https://github.com/g-liu"]
    - ["Connor Shea", "https://github.com/connorshea"]
    - ["Deepanshu Utkarsh", "https://github.com/duci9y"]
    - ["Brett Taylor", "https://github.com/glutnix"]
    - ["Tyler Mumford", "https://tylermumford.com"]
translators:
    - ["Kenryu Shibata", "https://github.com/kenryuS"]
filename: learncss-jp.css
lang: ja-jp
---

ウェブサイトはHTMLでページの構造を指定します。
CSS (カスケーディングスタイルシート) はページの**見た目**を指定する別の言語です。

CSSは単純な*ルール*で構成されています。それぞれが1つ以上の*セレクター*の視覚的*要素*を
指定した*値*にします。

この解説ページではCSS 2で書かれいていますが、CSS 3でより拡張性のある、
より新しい機能を使うことが出来ます。

**注釈:** CSSは視覚的な情報として出力するので、[dabblet](http://dabblet.com/)の様な、
CSSをすぐに試せる環境で学習することを勧めます。
この解説ページの主な目的は構文と小技の紹介です。

## 構文

```css
/* コメントはスラッシュ・アスタリスクの中に書きます。
   1行コメントはありません。*/

/* ####################
   ##   セレクター   ##
   #################### */

/* セレクターはページにある要素を指定します。 */
selector { property: value; /* その他のプロパティ...*/ }

/*
例となる要素:

<div class='class1 class2' id='anID' attr='value' otherAttr='en-us foo bar' />
*/

/* 一つのクラスでセレクトする */
.class1 { }

/* または、両方を使う */
.class1.class2 { }

/* または、要素名だけで */
div { }

/* または、ID名で */
#anID { }

/* または、属性名で */
[attr] { font-size:smaller; }

/* または、指定された値を持つ属性で */
[attr='value'] { font-size:smaller; }

/* 指定した値で始まる属性 (CSS 3) */
[attr^='val'] { font-size:smaller; }

/* または、値で終わる属性 (CSS 3) */
[attr$='ue'] { font-size:smaller; }

/* または、半角スペースで分けられた値の持つどれかの属性 */
[otherAttr~='foo'] { }
[otherAttr~='bar'] { }

/* または、ダッシュで区切られた値のどれかの属性 例： "-" (U+002D) */
[otherAttr|='en'] { font-size:smaller; }


/* 様々なセレクターを組み合わせることで限定されたセレクターを作ることができます。
   その場合は、間に半角スペースを入れないでください。*/
div.some-class[attr$='ue'] { }

/* 他の要素の子要素を指定することができます。*/
div.some-parent > .class-name { }

/* または、要素の子孫を指定できます。子要素は親要素の直接的な子孫です。
   （親要素からインデンテーション一回分）子孫は親要素からすべての
   インデンテーションされた階を含みます。*/
div.some-parent .class-name { }

/* 注意：半角スペースのない同じセレクターは別の意味になります。
   なんだと思いますか。*/
div.some-parent.class-name { }

/* 一個前の要素を選択することもできます。*/
.i-am-just-before + .this-element { }

/* または、その要素の前のすべての要素をすべて選択できます。*/
.i-am-any-element-before ~ .this-element { }

/* いくつかのセレクターには擬似クラスがあり、要素が特定の状態のときだけ
   適用するスタイルを指定できます。*/

/* 例えば、マウスカーソルが要素の上にホバーしている状態のスタイルを
   指定するときは、*/
selector:hover { }

/* リンク先にすでに訪れている状態を指定する場合は、*/
selector:visited { }

/* または訪れていない場合は、*/
selected:link { }

/* 要素が選択されている場合は、*/
selected:focus { }

/* 親要素内の最初の子要素を指定する場合は、*/
selector:first-child {}

/* 最後の要素を指定する場合は、*/
selector:last-child {}

/* 擬似クラスのように、疑似子要素を使えば、特定の部分のスタイルを
   指定できます。*/

/* 選択された要素の最初の疑似子要素を指定する。*/
selector::before {}

/* 選択された要素の最後の疑似子要素を指定する。*/
selector::after {}

/* アスタリスクを適切な場所に記述すれば、
   ワイルドカードとしてすべての要素を指定することができます。 */
* { } /* すべての要素 */
.parent * { } /* すべての子孫要素 */
.parent > * { } /* すべての子要素 */

/* セレクタを一度にグループで複数選択して同じスタイルを
   適用することができます。*/
selector1, selector2 { }

/* ####################
   ##  プロパティー  ##
   #################### */

selector {

    /* 長さの単位は画面の大きさに対して相対的か絶対的なものを選べます */

    /* 相対的単位 */
    width: 50%;       /* 親要素の幅に対する割合で指定する */
    font-size: 2em;   /* フォントサイズの何倍かで指定する */
    font-size: 2rem;  /* か、ルート要素のフォントサイズを元にする */
    font-size: 2vw;   /* ビューポートの幅1パーセントの何倍かで指定する (CSS 3) */
    font-size: 2vh;   /* ビューポートの高さでの指定 */
    font-size: 2vmin; /* vhかvwのどちらかの小さい方を選びます */
    font-size: 2vmax; /* または、大きい方 */

    /* 絶対的単位 */
    width: 200px;     /* ピクセル */
    font-size: 20pt;  /* ポイント */
    width: 5cm;       /* センチメートル */
    min-width: 50mm;  /* ミリメートル */
    max-width: 5in;   /* インチ */

    /* 色 */
    color: #F6E;                    /* 省略された六進法値での色 */
    color: #FF66EE;                 /* 六進法値での色 */
    color: tomato;                  /* 名前の付いた色 */
    color: rgb(255, 255, 255);      /* rgb値での色 */
    color: rgb(10%, 20%, 50%);      /* rgbの割合での色 */
    color: rgba(255, 0, 0, 0.3);    /* rgba値での色(CSS3) 値は0 <= a <= 1 */
    color: transparent;             /* アルファ値を0にするのと同じ効果 */
    color: hsl(0, 100%, 50%);       /* hsl割合(CSS 3)での色 */
    color: hsla(0, 100%, 50%, 0.3); /* アルファ値を含んだhsl割合での色 */

    /* ボーダー */
	border-width:5px;      /* 幅を指定*/
    border-style:solid;    /* 線のスタイルを指定 */
    border-color:red;      /* 背景色を指定するときと似たようなこと */
    border: 5px solid red; /* 上記の3つのプロパティーを一つのプロパティーで書く */
    border-radius:20px;    /* CSS3での新しいプロパティーです（ボーダー角半径） */

    /* 画像を要素の背景として使う */
    background-image: url(/img-path/img.jpg); /* url()内のクォーテーションは必須ではありません */

    /* フォント */
    font-family: Arial;

    /* フォント名に半角空白がある場合にはクォーテーションで囲む必要があります。 */
    font-family: "Courier New";

    /* もしフォントが存在しないなら、ブラウザが定義されたフォントを探します。 */
    font-family: "Courier New", Trebuchet, Arial, sans-serif;
}
```

## 使い方

CSSファイルを`.css`拡張子で保存する.

```html
<!-- CSSファイルを<head>タグ内に<link>タグで組み入れる必要があります。
	 https://stackoverflow.com/questions/8284365 を参考にしてください。-->
<link rel='stylesheet' type='text/css' href='path/to/style.css'>

<!-- HTML内にCSSを<style>タグを使って直接書くこともできます。-->
<style>
   a { color: purple; }
</style>

<!-- または、要素に直接CSSプロパティを記述することもできます。-->
<div style="border: 1px solid red;">
</div>
```

## 順序と数珠繋ぎ性質

ある要素が複数のセレクタの対象となり、複数のプロパティが設定されることがあります。
このような場合、いずれかのルールが他よりも優先されます。より具体的なセレクタを持つルールは
具体的でないセレクタより優先され、スタイルシートの後で使用されるルールは前のものを上書きします。
（これは、リンクされた二つの異なるスタイルシートがある要素に対するルールを含み、
ルールが同じ具体性を持つ場合、リンクの順序が優先され、一番最初にリンクしたシートが
スタイルを決定する、ということでもあります）。

この性質がキャスケーディング（数珠繋ぎ）と呼ばれ、CSSの名前もこの言葉からきています。

このようなCSSを考慮する：

```css
/* A */
p.class1[attr='value']

/* B */
p.class1 { }

/* C */
p.class2 { }

/* D */
p { }

/* E */
p { property: value !important; }
```

そしてこのようなHTML：

```html
<p style='/*F*/ property:value;' class='class1 class2' attr='value'>
```

スタイルの優先順位は次の通りです。 優先順位はブロック全体ではなく、
それぞれの**プロパティ**に対してであることを忘れないでください。

* `E`は、`!important`というキーワードがあるため、最も優先順位が高いです。
ただし、その使用は避けることが推奨されます。
* `F` はインラインスタイルなので、その次です。
* `A` は、他の何よりも「具体的」であるため、その次になります。
   これは3つの指定子を持っています： 要素名 `p`、クラス名 `class1`、
   属性名 `attr='value'` です。
* `C` is next, even though it has the same specificity as `B`.
    This is because it appears after `B`.
* `B`が次で、
* `D` が最後になります。

## メディアクエリ

CSSメディアクエリとは、CSS 3の機能で、印刷時や、特定の寸法やピクセル密度の画面上で、特定のCSSルールを適用する際の条件を指定できるものです。 セレクタの具体性を高めるものではありません。

```css
/* すべてのデバイスで使われるルール */
h1 {
  font-size: 2em;
  color: white;
  background-color: black;
}

/* 印刷するときにh1タグの印刷に使われるインクの量を減らす。 */
@media print {
  h1 {
    color: black;
    background-color: white;
  }
}

/* 画面の幅が480ピクセルより小さいときにフォントサイズを大きくする。 */
@media screen and (min-width: 480px) {
  h1 {
    font-size: 3em;
    font-weight: normal;
  }
}
```

メディアクエリには次の機能があります：
`width`, `height`, `device-width`, `device-height`, `orientation`, `aspect-ratio`, `device-aspect-ratio`, `color`, `color-index`, `monochrome`, `resolution`, `scan`, `grid`
`min-` や `max-`は多くの機能名に前置することができます。

`resolution`機能は古いデバイスでは対応しておらず、`device-pixel-ratio`を代わりに使います。

多くのスマホやタブレットはウェブページに`viewport`メタタグをつけない限り、デスクトップに表示されるサイズで表示します。

```html
<head>
  <meta name="viewport" content="width=device-width; initial-scale=1.0">
</head>
```

## 互換性

大抵のCSS2(または多くのCSS3)の機能はすべてのブラウザやデバイスで機能します。
しかしそれでも、1度ちゃんと新しい機能が動作するかを確認するのが無難です。

## その他の資料（英語）

* [CanIUse](http://caniuse.com) (互換性に関する細かい情報)
* [Dabblet](http://dabblet.com/) (CSS テスター)
* [Mozilla Developer Network's CSS documentation](https://developer.mozilla.org/en-US/docs/Web/CSS) (チュートリアルとレファレンス)
* [Codrops' CSS Reference](http://tympanus.net/codrops/css_reference/) (レファレンス)
* [DevTips' CSS Basics](https://www.youtube.com/playlist?list=PLqGj3iMvMa4IOmy04kDxh_hqODMqoeeCy) (チュートリアル)

## 関連記事（英語）

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [Selecting elements using attributes](https://css-tricks.com/almanac/selectors/a/attribute/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
* [SASS](http://sass-lang.com/) と [LESS](http://lesscss.org/) でCSSプリプロセッシング。
* [CSS-Tricks](https://css-tricks.com)
