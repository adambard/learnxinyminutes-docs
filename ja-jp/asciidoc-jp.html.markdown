---
language: asciidoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
    - ["Abel Salgado Romero", "https://twitter.com/abelsromero"]
translators:
    - ["Ryota Kayanuma", "https://github.com/PicoSushi"]
filename: asciidoc-ja.md
lang: ja-jp
---

AsciiDocはMarkdownに似たマークアップ言語で、書籍の執筆からブログを書くことまでなんでも使うことができます。2002年、Stuart RackhamによりAsciiDocは作成され、シンプルでありつつも沢山のカスタマイズを可能にしています。

文書のヘッダー

ヘッダーはオプションで、空行を含むことはできません。本文から1行以上の改行を開ける必要があります。

タイトルのみの例

```
= 文章タイトル

文書の最初の行
```

タイトルと著者

```
= 文書タイトル
文書 太郎 <first.last@learnxinyminutes.com>

文書の開始
```

複数の著者

```
= 文書タイトル
John Doe <john@go.com>; Jane Doe<jane@yo.com>; Black Beard <beardy@pirate.com>

複数の著者による文書の始まり。
```

版(著者の行を必要とします)

```
= 第一版のタイトル
芋男 <chip@crunchy.com>
v1.0, 2016-01-13

このポテトについての文書は面白いです。
```

段落

```
段落は特別なことは不要です。

空行を段落の間に入れることで、段落を分けることができます。

折り返しをしたい場合、+
を書くことで折り返せます!
```

文書の整形

```
_アンダースコアで斜体になります。_
*アスタリスクで太字になります。*
*_組み合わせると楽しい_*
`バッククォートで固定幅になります。`
`*太字の固定幅*`
```

節タイトル

```
= Level 0 (文書のヘッダーにのみ使用してください)

== Level 1 <h2>

=== Level 2 <h3>

==== Level 3 <h4>

===== Level 4 <h5>

```

リスト

箇条書きリストを作るには、アスタリスクを使用してください。

```
* foo
* bar
* baz
```

番号付きリストを作るには、ピリオドを使用してください。

```
. item 1
. item 2
. item 3
```

リストはアスタリスクやピリオドを追加することで5段階まで入れ子にできます。

```
* foo 1
** foo 2
*** foo 3
**** foo 4
***** foo 5

. foo 1
.. foo 2
... foo 3
.... foo 4
..... foo 5
```

## 補足資料

AsciiDocの文書を処理するツールは2種類あります。

1. [AsciiDoc](http://asciidoc.org/): オリジナルのPython実装で、Linuxで利用可能です。現在は開発されておらず、メンテナンスのみの状態です。
2. [Asciidoctor](http://asciidoctor.org/): Rubyによる別実装で、JavaやJavascriptでも利用可能です。AsciiDocに新しい機能や出力形式を追加するため、現在活発に開発されています。

以下のリンクは `AsciiDoctor` 実装関連のものです。

* [Markdown - AsciiDoc syntax comparison](http://asciidoctor.org/docs/user-manual/#comparison-by-example): Common MarkdownとAsciidocの要素を並べて比較しています。
* [Getting started](http://asciidoctor.org/docs/#get-started-with-asciidoctor): インストールから簡潔な文書を作るための簡単なガイドです。
* [Asciidoctor User Manual](http://asciidoctor.org/docs/user-manual/): 文法のリファレンス、例、描画ツール、その他を含む完全なドキュメントです。
