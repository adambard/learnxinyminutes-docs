---
category: tool
tool: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
    - ["Denis Arh", "https://github.com/darh"]
translators:
    - ["akirahirose", "https://twitter.com/akirahirose"]
filename: LearnBash-jp.sh
lang: ja-jp
---

Bash はunixシェルの1つです。GNUオペレーションシステムのシェルとして配布されています。
LinuxやMac OS Xの、デフォルトシェルにもなっています。
以下にある例は、ほぼ全部シェルスクリプトの一部として使えます。また、一部はそのままシェルから実行できます。

[ちゃんとした説明は、こちらをどうぞ](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# 最初の行はShebang(シェバング、シバン)というもので、システムに対して何を使って実行するのかを教えるためのものです
# ちゃんとした説明、こちらをどうぞ: http://en.wikipedia.org/wiki/Shebang_(Unix)
# 既にお気づきのように、コメント行は#で始まります. Shebangもコメントです

# まずは Hello world です
echo Hello world!

# コマンド毎に改行を入れるか、セミコロンで区切ります
echo 'This is the first line'; echo 'This is the second line'

# 変数の宣言はこのようにやります
VARIABLE="Some string"

# が、以下のようにやってはダメです
VARIABLE = "Some string"
# このように（空白を入れて）書くと、Bash はVARIABLEを実行するべきコマンドとみなし、実行します。
# そして、VARIABLEというコマンドはない（はずな）ので、エラーになります


# 変数の使い方
echo $VARIABLE
echo "$VARIABLE"
echo '$VARIABLE'
# 変数の値（中身）を使わず、変数名だけを使うとき（代入するときや渡すときなど）は、$なしで変数名を書いてください
# 変数の値（中身）を使うときは、$をつけます
# 上記例の最後にある、' (シングルクォート) で囲んだ場合は、変数の値は表示されません!

# 変数値の文字列置換
echo ${VARIABLE/Some/A}
# 最初に出てくる "Some" を "A" で置換します

# 変数値の一部を取り出します
echo ${VARIABLE:0:7}
# 最初の7文字だけを取り出します

# デフォルトの変数値設定(訳注:シェル実行時に引数で変数値を設定できるが、設定しなかった場合の値を指定できる）
echo ${FOO:-"DefaultValueIfFOOIsMissingOrEmpty"}
# 上記は、FOO番目の引数がnullだったとき(FOO番目=)や、空文字が指定されたとき(FOO番目="")に、変数に0を入れます
# ( 当然ですが、引数に0を指定したとき(FOO番目=0) も、0は入ります）

# 組込み変数
# 以下のような便利な組込み変数があります
echo "Last program return value: $?"
echo "Script's PID: $$"
echo "Number of arguments: $#"
echo "Scripts arguments: $@"
echo "Scripts arguments separated in different variables: $1 $2..."

# 入力値の読み込み
echo "What's your name?"
read NAME # 新しく変数を宣言する必要はないことに注意
echo Hello, $NAME!

# 普通のif文も使えます
# 利用できる判定条件については、'man test' で参照してください
if [ $NAME -ne $USER ]
then
    echo "Your name is your username"
else
    echo "Your name isn't your username"
fi

# 他にも、条件判定ができます
echo "Always executed" || echo "Only executed if first command fails"
echo "Always executed" && echo "Only executed if first command does NOT fail"

# 数式は以下のように書きます
echo $(( 10 + 5 ))

# 他のプログラム言語とは違ってbashはシェルなので、現在いるディレクトリ位置が異なると、異なる結果になります
# lsコマンドで、現在いるディレクトリにあるファイルと、ディレクトリのリストが取得できます
ls

# これらのコマンドには、実行オプションがいろいろあります
ls -l # ファイルとディレクトリのリストを行に分けて表示します

# あるコマンド実行による返値を、次のコマンドの入力値としてつかえます
# 例えばですが、lsコマンドの返値を、grepコマンドによって指定したルールに基づいてフィルタできます。
# 以下は、現在いるディレクトリにある、.txtファイルのリストを表示する例です
ls -l | grep "\.txt"

# コマンドに対する入力元や出力先、またエラー出力先などを変更できます
python2 hello.py < "input.in"
python2 hello.py > "output.out"
python2 hello.py 2> "error.err"
# 出力先として指定したファイルが既に存在する場合は、上書きされます
# もしもファイルに追記したい場合は、代わりに">>" を使ってください

# コマンド文中で、$()内に別コマンドを入れると、その別コマンドの返値をコマンド文の一部として使う事ができます
# 次のコマンドは、現在いるディレクトリにあるファイルの数を表示します
echo "There are $(ls | wc -l) items here."

# バッククォート(backticks) `` でも同じことができますが、入れ子にはできません
# そのため、$()がお勧めです
echo "There are `ls | wc -l` items here."

# BashはJavaやC++のように、case文による分岐ができます
case "$VARIABLE" in
    #分岐条件として使いたいパターンを並べてください
    0) echo "There is a zero.";;
    1) echo "There is a one.";;
    *) echo "It is not null.";;
esac

# 指定した回数、処理を繰り返し
# 変数の値 $VARIABLE が3回表示されます
for VARIABLE in {1..3}
do
    echo "$VARIABLE"
done

# while ループです
while [true]
do
    echo "loop body here..."
    break
done

# 関数の定義もできます
function foo ()
{
    echo "Arguments work just like script arguments: $@"
    echo "And: $1 $2..."
    echo "This is a function"
    return 0
}

# 以下のように、もっと簡単な書き方もあります
bar ()
{
    echo "Another way to declare functions!"
    return 0
}

# 自作関数を呼びます
foo "My name is" $NAME

# 他にもいろいろと、知っておくと便利なコマンドがあります
# file.txtの最後10行を表示します
tail -n 10 file.txt
# file.txtの最初10行を表示します
head -n 10 file.txt
# file.txt's の行を並び替えます
sort file.txt
# 重複している行を表示するか、削除できます。-dオプションをつけると、表示します
uniq -d file.txt
# 1行ごとに、','が最初に出てくる前の部分を表示します
cut -d ',' -f 1 file.txt

```
