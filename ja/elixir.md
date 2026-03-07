---
contributors:
    - ["Joao Marques", "https://github.com/mrshankly"]
    - ["Dzianis Dashkevich", "https://github.com/dskecse"]
    - ["Ryan Plant", "https://github.com/ryanplant-au"]
    - ["Ev Bogdanov", "https://github.com/evbogdanov"]
translators:
    - ["itepechi", "https://github.com/itepechi"]
---

Elixirは、Erlang VM上で動作するモダンな関数型言語です。Erlangと完全な互換性があ
りますが、より一般的な構文を持っているほか、機能も大きく拡張されています。

```elixir
# 1行コメントはシャープで始まります。

# 複数行コメントはありませんが、
# コメントを連続して書くことはできます。

# Elixirシェルを利用するには`iex`コマンドを使用します。
# モジュールをコンパイルするには`elixirc`コマンドを使用します。

# Elixirが正しくインストールされている場合、両コマンドはPATHに入っています。

## ---------------------------
## -- 基本的な型
## ---------------------------

# これらは数字です
3    # 整数
0x1F # 整数
3.0  # 浮動小数点数

# アトムは自身の名前を値として持つ定数です。基本的に`:`で始まります。
:hello # アトム

# タプルはメモリーの中に連続して格納されます。
{1,2,3} # タプル

# `elem`関数を使うことで、タプルの要素にアクセスできます
elem({1, 2, 3}, 0) #=> 1

# リストは連結リストとして実装されています。
[1,2,3] # リスト

# リストのヘッドとテールは次の形でアクセスできます
[head | tail] = [1,2,3]
head #=> 1
tail #=> [2,3]

# ElixirではErlangと同様、`=`は代入ではなくパターンマッチを表します。
#
# 言い換えれば、右側の内容を左側のパターンを当てはめる指示を意味します。
#
# 上記の例でヘッドとテールに正しくアクセスできるカラクリはここにあります。

# パターンマッチはそれぞれの内容がマッチしない場合、失敗します。
# この例では、それぞれのタプルのサイズが一致していません。
# {a, b, c} = {1, 2} #=> ** (MatchError) no match of right hand side value: {1,2}

# 値にはバイナリーもあります
<<1,2,3>> # バイナリー

# 文字列と文字リスト
"hello" # 文字列
'hello' # 文字リスト

# 複数行文字列もあります
"""
私は複数行
文字列です。
"""
#=> "私は複数行\n文字列です。\n"

# 文字列はすべてUTF-8でエンコードされています
"héllò" #=> "héllò"

# 文字列の実態はバイナリーで、文字リストはただのリストです。
<<?a, ?b, ?c>> #=> "abc"
[?a, ?b, ?c]   #=> 'abc'

# Elixirでは、`?a`は`a`のASCIIコード整数を返します
?a #=> 97

# リストをつなげるには`++`を、バイナリーをつなげるには`<>`を使います
[1,2,3] ++ [4,5]     #=> [1,2,3,4,5]
'hello ' ++ 'world'  #=> 'hello world'

<<1,2,3>> <> <<4,5>> #=> <<1,2,3,4,5>>
"hello " <> "world"  #=> "hello world"

# レンジは`start..end`の形で表現されます（両端を含む）
1..10 #=> 1..10
lower..upper = 1..10 # レンジにもパターンマッチを適用できます
[lower, upper] #=> [1, 10]

# マップはそれぞれのキーと値を集めたものです
genders = %{"david" => "male", "gillian" => "female"}
genders["david"] #=> "male"

# 次の例ではアトムをキーとして使用しています
genders = %{david: "male", gillian: "female"}
genders.gillian #=> "female"

## ---------------------------
## -- 演算子
## ---------------------------

# 単純な計算
1 + 1  #=> 2
10 - 5 #=> 5
5 * 2  #=> 10
10 / 2 #=> 5.0

# Elixirでは、`/`演算子は必ず浮動小数点数を返します。

# 整数として除算する場合は`div`を使用します
div(10, 2) #=> 5

# 余りを計算するには`rem`を使用します
rem(10, 3) #=> 1

# `or`、`and`や`not`など、ブーリアン演算子も存在します。
# これらの演算子は第一引数（左側）としてブーリアンを期待します。
true and true #=> true
false or true #=> true
# 1 and true
#=> ** (BadBooleanError) expected a boolean on left-side of "and", got: 1

# さらに、Elixirにはあらゆる型に対応した`||`、`&&`、`!`演算子が存在します。
# `false`と`nil`を除いたあらゆる値は真値として扱われます。
1 || true  #=> 1
false && 1 #=> false
nil && 20  #=> nil
!true #=> false

# `==`、`!=`、`===`、`!==`、`<=`、`>=`、`<`、`>`を使うことで比較ができます
1 == 1 #=> true
1 != 1 #=> false
1 < 2  #=> true

# `===`と`!==`は、整数と浮動小数点数を厳格に区別して比較します
1 == 1.0  #=> true
1 === 1.0 #=> false

# Elixirの演算子は基本的に厳格ですが、異なる型を受け取る比較演算子は例外です
1 < :hello #=> true

# これにより異なる型の集まりも作成できるようになります
["string", 123, :atom]

# 異なる型同士にも順序は定義されています。ここでは詳しい順序を説明する代わりに
# 「どのような順序であるかは重要ではない。何らかの順序が定義されているという
# 事実自体が重要なのだ」というジョー・アームストロングの言葉を引用します。

## ---------------------------
## -- 制御フロー
## ---------------------------

# `if`式
if false do
  "ここには到達しません"
else
  "ここにはします"
end

# パターンマッチは覚えていますか？Elixirの制御フローでたくさん登場します。

# `case`では値を複数のパターンに対して比較できます
case {:one, :two} do
  {:four, :five} ->
    "これはマッチしません"
  {:one, x} ->
    "これはマッチし、`x`に`:two`をバインドします"
  _ ->
    "これはどんな値もマッチします"
end

# 内容に興味がない場合、`_`に値をバインドするのが一般的です。
# たとえば、リストの一番最初の要素のみを取り出したい時は
[head | _] = [1,2,3]
head #=> 1

# 可読性を高めたい場合はこのように書くこともできます
[head | _tail] = [:a, :b, :c]
head #=> :a

# `cond`では複数の条件を同時に比較できます。
# `if`式を重ねるよりも、`cond`を優先して使いましょう。
cond do
  1 + 1 == 3 ->
    "ここには到達しません"
  2 * 5 == 12 ->
    "ここにもしません"
  1 + 2 == 3 ->
    "ここにはします"
end

# 一般的には、常にマッチする条件を設定するため`true`を最後に置きます。
cond do
  1 + 1 == 3 ->
    "ここには到達しません"
  2 * 5 == 12 ->
    "ここにもしません"
  true ->
    "ここにはします（事実上のelse）"
end

# スローされた値をキャッチするには`try/catch`を使います。
# キャッチの有無を問わず実行される`after`句もサポートされています。
try do
  throw(:hello)
catch
  message -> "#{message}を得ました。"
after
  IO.puts("ここがafter句です。")
end
#=> ここがafter句です。
# ":helloを得ました。"

## ---------------------------
## -- モジュールと関数
## ---------------------------

# 無名関数（ドットに注目）
square = fn(x) -> x * x end
square.(5) #=> 25

# 句やガードは複数回指定できます。
# ガードは`when`キーワードで示され、パターンマッチの調整に利用されます
f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

f.(1, 3)  #=> 4
f.(-1, 3) #=> -3

# Elixirはさまざまなビルトイン関数を提供しています。
# 例として、これらの関数が現在のスコープで呼び出せます。
is_number(10)    #=> true
is_list("hello") #=> false
elem({1,2,3}, 0) #=> 1

# 関数はモジュールとしてまとめることもできます。
# モジュール内では`def`を使用して関数を定義します。
defmodule Math do
  def sum(a, b) do
    a + b
  end

  def square(x) do
    x * x
  end
end

Math.sum(1, 2)  #=> 3
Math.square(3) #=> 9

# 今回定義したMathモジュールをコンパイルするには`math.ex`として保存してから、
# ターミナルで`elixirc`を呼び出します。例：elixirc math.ex

# モジュール内では`def`を使い関数を定義できるのと同様に、`defp`を使うことで
# プライベート関数を定義できます。`def`で定義された関数は他のモジュールからも
# 呼び出せますが、プライベート関数は同一のモジュール内からのみ呼び出せます。
defmodule PrivateMath do
  def sum(a, b) do
    do_sum(a, b)
  end

  defp do_sum(a, b) do
    a + b
  end
end

PrivateMath.sum(1, 2)    #=> 3
# PrivateMath.do_sum(1, 2) #=> ** (UndefinedFunctionError)

# 関数は複数回定義できるほか、ガードもサポートされています。
# 複数回定義された関数を呼び出すと、条件を満たす最初の関数が実行されます。
# 例：area({:circle, 3})は上のarea関数ではなく、2番目の関数を呼び出します
defmodule Geometry do
  def area({:rectangle, w, h}) do
    w * h
  end

  def area({:circle, r}) when is_number(r) do
    3.14 * r * r
  end
end

Geometry.area({:rectangle, 2, 3}) #=> 6
Geometry.area({:circle, 3})       #=> 28.25999999999999801048
# Geometry.area({:circle, "not_a_number"})
#=> ** (FunctionClauseError) no function clause matching in Geometry.area/1

# Elixirは原則的に不変であるため、再起が多く活用されます
defmodule Recursion do
  def sum_list([head | tail], acc) do
    sum_list(tail, acc + head)
  end

  def sum_list([], acc) do
    acc
  end
end

Recursion.sum_list([1,2,3], 0) #=> 6

# Elixirのモジュールには属性を関連付けられます。ビルトインの属性に限らず、
# あなた独自のカスタム属性も指定できます。
defmodule MyMod do
  @moduledoc """
  これはサンプルのモジュールのビルトイン属性です。
  """

  @my_data 100 # これはカスタム属性です。
  IO.inspect(@my_data) #=> 100
end

# パイプ演算子 |> を使うと、式の結果を次の関数の第一引数に渡すことができます。

Range.new(1,10)
|> Enum.map(fn x -> x * x end)
|> Enum.filter(fn x -> rem(x, 2) == 0 end)
#=> [4, 16, 36, 64, 100]

## ---------------------------
## -- 構造体と例外
## ---------------------------

# 構造体はマップの拡張で、初期値、コンパイル時保証、ポリモーフィズムをElixir
# に導入します。
defmodule Person do
  defstruct name: nil, age: 0, height: 0
end

joe_info = %Person{ name: "Joe", age: 30, height: 180 }
#=> %Person{age: 30, height: 180, name: "Joe"}

# nameの値にアクセス
joe_info.name #=> "Joe"

# ageの値を更新
older_joe_info = %{ joe_info | age: 31 }
#=> %Person{age: 31, height: 180, name: "Joe"}

# 例外を扱うには`try`ブロックと`rescue`キーワードを組み合わせます
try do
  raise "何かしらのエラー"
rescue
  RuntimeError -> "ランタイムエラーをレスキューしました"
  _error -> "こちらは任意のエラーをレスキューします"
end
#=> "ランタイムエラーをレスキューしました"

# あらゆる例外はメッセージを持ちます
try do
  raise "何かしらのエラー"
rescue
  x in [RuntimeError] ->
    x.message
end
#=> "何かしらのエラー"

## ---------------------------
## -- 並行性
## ---------------------------

# Elixirは並行性を実現するために、アクターモデルを採用しています。Elixirで
# 並行処理を実現するために必要なものは、プロセスのスポーン、メッセージの送信、
# メッセージの受信の3つです。

# プロセスを開始するには、関数を引数として受け取る`spawn`関数を使用します。
f = fn -> 2 * 2 end #=> #Function<erl_eval.20.80484245>
spawn(f) #=> #PID<0.40.0>

# `spawn`が返すpid（プロセスID）を使うことで、プロセスにメッセージを送る
# ことができます。メッセージを送るには、`send`演算子を使います。しかし、
# メッセージは受け取らなければ意味がありません。`receive`を使って、これを
# 実現しましょう

# メッセージを受け取り、処理するためには`receive do`ブロックを使用します。
# ただし、`receive do`ブロックは基本的に単一のメッセージのみを処理します。
# 複数のメッセージを処理するには、`receive do`ブロックを持つ関数を再帰的に
# 呼び出すことで、`receive do`ブロックを繰り返し評価する必要があります。

defmodule Geometry do
  def area_loop do
    receive do
      {:rectangle, w, h} ->
        IO.puts("Area = #{w * h}")
        area_loop()
      {:circle, r} ->
        IO.puts("Area = #{3.14 * r * r}")
        area_loop()
    end
  end
end

# モジュールをコンパイルし、`area_loop`をシェル内で実行します
pid = spawn(fn -> Geometry.area_loop() end) #=> #PID<0.40.0>
# 他の書き方もあります
pid = spawn(Geometry, :area_loop, [])

# recieve文のパターンに合致するメッセージを`pid`に送信します
send pid, {:rectangle, 2, 3}
#=> Area = 6
#   {:rectangle,2,3}

send pid, {:circle, 2}
#=> Area = 12.56000000000000049738
#   {:circle,2}

# シェルそれ自身もプロセスで、`self`を使えばpidを取得できます
self() #=> #PID<0.27.0>

## ---------------------------
## -- エージェント
## ---------------------------

# エージェントは変化する値をトラッキングするプロセスです

# `Agent.start_link`でエージェントを作成し、関数を入力します
# この関数の戻り値がエージェントの初期状態になります
{:ok, my_agent} = Agent.start_link(fn -> ["red", "green"] end)

# `Agent.get`はエージェント名と現在の状態を受け取る`fn`を引数に取ります
# この`fn`によって返される値が最終的な戻り値になります
Agent.get(my_agent, fn colors -> colors end) #=> ["red", "green"]

# 同じ方法でエージェントの状態を更新できます
Agent.update(my_agent, fn colors -> ["blue" | colors] end)
```

## リファレンス

* [Getting started guide](https://elixir-lang.org/getting-started/introduction.html), [Elixir website](https://elixir-lang.org)より
* [Elixir Documentation](https://elixir-lang.org/docs.html)
* ["Programming Elixir"](https://pragprog.com/book/elixir/programming-elixir), Dave Thomas著
* [Elixir Cheat Sheet](https://media.pragprog.com/titles/elixir/ElixirCheat.pdf)
* ["Learn You Some Erlang for Great Good!"](https://learnyousomeerlang.com/), Fred Hebert著
* ["Programming Erlang: Software for a Concurrent World"](https://pragprog.com/book/jaerlang2/programming-erlang), Joe Armstrong著
* [Introduction to Elixir](https://learn-elixir.com/)
