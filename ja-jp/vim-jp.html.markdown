---
category: tool
tool: vim
contributors:
    - ["RadhikaG", "https://github.com/RadhikaG"]
translators:
    - ["Kota Kato", "https://github.com/kato-k"]
filename: LearnVim-jp.txt
lang: ja-jp
---


[Vim](http://www.vim.org)
(Vi IMproved) は、Unix用の人気なエディタである vi のクローンです。
これは、速度と生産性を高めることを目的に設計されたエディタであり、
ほとんどのUnix互換のシステムに組込まれています。
ファイル内の特定の位置に移動したり、素早く編集したりするための多数のキーバインドを持ちます。

`vimtutor`はあなたに`Vim`の使い方を教える素晴しいアプリケーションです。
Vimパッケージのインストール時に一緒に付属しますので、
コマンドラインで「vimtutor」を実行するだけで、このチュートリアルを開けるはずです。
これは、`vim`の全ての主要機能を説明します。

訳注) 日本語で`vimtutor`を利用するには、「vimtutor ja」を実行しなければならない場合があります。

## 基本のVim操作

```
    vim <filename>   # <filename>をVimで開く
    :help <topic>    # <topic>についての組み込みドキュメントが存在する場合、
                     # それを開く
    :q               # Vimを終了する
    :w               # 編集中のファイルを保存する
    :wq              # ファイルを保存して、Vimを終了する
    ZZ               # ファイルを保存して、Vimを終了する。:xと同様
    :q!              # ファイルを保存せずにVimを終了する
                     # :q を ! *強制的に* 実行するため保存せずにVimが終了します
    ZQ               # ファイルを保存せずにVimを終了する
    :x               # 変更点がある時、ファイルを保存してVimを終了する

    u                # Undo
    CTRL+R           # Redo

    h                # 左に一文字移動
    j                # 一行下に移動
    k                # 一行上に移動
    l                # 右に一文字移動

    Ctrl+B           # ウィンドウを一画面上に移動
    Ctrl+F           # ウィンドウを一画面下に移動
    Ctrl+D           # ウィンドウを半画面上に移動
    Ctrl+U           # ウィンドウを半画面下に移動

    # 行内を移動する

    0                # 行頭に移動
    $                # 行末に移動
    ^                # 行の初めの非空白文字に移動

    # テキストの検索

    /word            # カーソル以降に出現する全ての一致をハイライト
    ?word            # カーソル以前に出現する全ての一致をハイライト
    n                # カーソルを次の一致に移動
    N                # カーソルを前の一致に移動

    :%s/foo/bar/g    # 全ての行について「foo」を「bar」に置換
    :s/foo/bar/g     # 現在の行について「foo」を「bar」に置換
    :%s/\n/\r/g      # 改行文字の置換

    # 文字への移動

    f<character>     # 前方の<character>に移動する
    t<character>     # 前方の<character>の一文字前に移動する

    # 例
    f<               # 前方の < に移動
    t<               # 前方の < の一文字前に移動

    # 単語ごとの移動

    w                # 一単語前に移動
    b                # 一単語後ろに移動
    e                # 現在の単語の後部に移動

    # 移動のためのキーバインド

    gg               # ファイルの先頭に移動
    G                # ファイルの最後に移動
    :NUM             # ファイルのNUM行に移動(NUMは任意の行数)
    H                # カーソルをウィンドウ上部に移動
    M                # カーソルをウィンドウ中央に移動
    L                # カーソルをウィンドウ下部に移動
```

## ヘルプドキュメント:

Vimには`:help <topic>`でアクセスできるヘルプドキュメントが組込まれています。
例えば、`:help navigation`はカーソルを移動する方法についてのドキュメントを開きます。

`:help`はオプション無しでも利用できます。
これにより、Vimにより親しみやすくすることを目的としたデフォルトのヘルプダイアログが開かれます。

## モード:

Vimは**モード**の概念に基づいています。

- Command Mode - Vimはこのモードで起動し、移動とコマンドの実行に使われます
- Insert Mode  - ファイルに変更を加えるのに使われます
- Visual Mode  - テキストをハイライトしてオペレータを適用するために使われます
- Ex Mode      - コマンドを入力するための「:」プロンプトで使われます

```
    i                # カーソル位置の前からInsert Modeに入る
    a                # カーソル位置の後ろからInsert Modeに入る
    v                # Visual Modeに入る
    :                # Ex Modeに入る
    <esc>            # 現在のモードからコマンドモードに「脱出」

    # テキストのコピーと貼り付け

    y                # 選択された対象をヤンクする
    yy               # 現在の行をヤンクする
    d                # 選択された対象を削除する
    dd               # 現在の行を削除する
    p                # ヤンクされたテキストをカーソルの後ろに貼り付ける
    P                # ヤンクされたテキストをのカーソルの前に貼り付ける
    x                # カーソル位置の文字を削除
```

## Vimの「文法」

Vimの操作は「動詞・修飾子・名詞」形式のコマンドとして考えることができます。

- 動詞   - 動作
- 修飾子 - 動作の実行方法
- 名詞   - 動作が作用するオブジェクト

「動詞・修飾子・名詞」関するいくつかの重要な例:

```
    # '動詞'

    d                # 削除
    c                # 変更
    y                # ヤンク (コピー)
    v                # ビジュアル選択

    # '修飾子'

    i                # 内部
    a                # 周り
    NUM              # 回数 (NUMは任意の番号)
    f                # 任意の一文字まで
    t                # 任意の一文字の手前まで
    /                # カーソル以降の任意の文字列まで
    ?                # カーソル以前の任意の文字列まで

    # '名詞'

    w                # 単語
    s                # 文
    p                # 段落
    b                # ブロック

    # 「文」の例

    d2w              # 削除   2        単語 (2単語を削除)
    cis              # 変更   内部     文   (文の内部を変更)
    yip              # ヤンク 内部     段落 (段落の内部をヤンク)
    ct<              # 変更   手前     <    (<の手前まで変更)
    d$               # 削除   行末まで      (行末まで削除)
```

## いくつかのショートカットと小技

        <!--TODO: Add more!-->
```
    >                # 選択部を1ブロックインデント
    <                # 選択部を1ブロックデインデント
    :earlier 15m     # ファイルを15分前の状態に戻す
    :later 15m       # 上記のコマンドの逆
    ddp              # 連続する行を入れ替え
    .                # 前回の動作を繰り返す
    :w !sudo tee %   # 編集中のファイルを管理者として保存
    :set syntax=c    # 「C言語」のシンタックスハイライトを利用する
    :sort            # 全ての行をソートする
    :sort!           # 全ての行を降順にソートする
    :sort u          # 全ての行をソートして重複を削除する
    ~                # 選択部分の大文字小文字を入れ替える
    u                # 選択部分を小文字にする
    U                # 選択部分を大文字にする
    J                # 現在の行と次の行を結合する

    # テキストの折り畳み
    zf               # 選択したテキストを折り畳む
    zo               # 折り畳みを開く
    zc               # 折り畳みを閉じる
    zR               # 全ての折り畳みを開く
    zM               # 全ての折り畳みを閉じる
```

## マクロ

マクロは基本的に記録可能なアクションです。
マクロの記録を開始すると、記録を停止するまで**全て**の操作とコマンドが記録されます。
マクロを呼びだすと、まったく同じ一連の操作とコマンドが文書に再度適用されます。

```
    qa               # 「a」という名前のマクロの記録を開始する
    q                # 記録を停止する
    @a               # 「a」マクロを再生する
```

### ~/.vimrc の設定

ファイル.vimrcは起動時にVimの設定として利用されます

次は~/.vimrcファイルのサンプルです

```
" Example ~/.vimrc
" 2015.10

" Required for vim to be iMproved
set nocompatible

" 自動インデントなどを利用するために、ファイル名からファイルタイプを決定する
filetype indent plugin on

" シンタックスハイライトを利用する
syntax on

" より良いコマンドライン補完
set wildmenu

" 大文字を利用しない場合、検索で大文字・小文字を区別しない
set ignorecase
set smartcase

" ファイル固有のインデントが有効でない場合、現在行のインデントを継続する
set autoindent

" 行番号の表示
set number

" インデントに関するオプション

" TAB文字の幅
set tabstop=4

" 編集中TABキーを押した際の挙動
set softtabstop=4

" << >> を利用した再インデント時のスペースの数
set shiftwidth=4

" TABキーをスペースに変換する
set expandtab

" 賢いTAB機能を有効にする
set smarttab
```

### 参考文献

[Vim | Home](http://www.vim.org/index.php)

`$ vimtutor`

[A vim Tutorial and Primer](https://danielmiessler.com/study/vim/)

[What are the dark corners of Vim your mom never told you about? (Stack Overflow thread)](http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about)

[Arch Linux Wiki](https://wiki.archlinux.org/index.php/Vim)
