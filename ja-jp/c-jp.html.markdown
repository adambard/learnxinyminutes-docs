---
language: C
filename: learnc.c
contributors:
  - ["Adam Bard", "http://adambard.com/"]
  - ["Árpád Goretity", "http://twitter.com/H2CO3_iOS"]
  - ["Jakub Trzebiatowski", "http://cbs.stgn.pl"]
  - ["Marco Scannadinari", "https://marcoms.github.io"]
  - ["Zachary Ferguson", "https://github.io/zfergus2"]
  - ["himanshu", "https://github.com/himanshu81494"]
  - ["Joshua Li", "https://github.com/JoshuaRLi"]
  - ["Dragos B. Chirila", "https://github.com/dchirila"]
  - ["Heitor P. de Bittencourt", "https://github.com/heitorPB/"]
translators:
  - ["Kenryu Shibata", "https://github.com/kenryuS"]
---

え、C？　あぁ、**未だに**モダンで高パフォーマンスを実現できるあの言語のことだな。

Cはほとんどのプログラマが最低水準言語として使われているが、その特徴は実行速度の速さだけ
ではないのだ。CはPythonなどの高水準言語とは異なり、メモリの自動管理機能がなく、
プログラマーの手で管理する必要があり、これが初学者を苦しめる要素となるが、うまく使えば、
ロボットなどで実行速度やメモリの使用率などを大幅に最適化できる。

> **コンパイラフラグについて**
>
> gccやclangなどのコンパイラではデフォルトでデバッグに有益なエラーや警告を表示しない
> 設定になっています。なので、それらのエラーを詳細に、厳しく表示させるフラグと共に
> 実行することをおすすめします。下記はそのフラグの例です：
>
> `-Wall -Wextra -Werror -O2 -std=c99 -pedantic`
>
> このようなフラグの詳細については、オンライン検索にかけるか、
> コンパイラのドキュメンテーションを読んでください。(Linuxなら`man 1 gcc`等)

```c
// 行コメントは//で始まる (C99より前のC標準では使えない)

// Cに限ったことではないが、ソースコードで日本語コメントを書くときにはファイルを
// UTF-8で保存することをおすすめします。なぜならgccなど特定のコンパイラでは
// 文字コード変換の影響で意図しないコメントアウトが引き起こされる可能性があります。

// 例：
// forループで似たコードの繰り返しを解消することが可能
// このコメントを消すと何故か動かない
for (int i = 0; i < 100; i++) {
    printf("%d\n", i);
}
// 解説：shift-jisで「能」は 94 5c で、標準ASCIIでは 5c は"\"でLinux gccでは
// 次の行もコメントアウトされる仕様で、この例ではforループの最初の定義が
// コメントアウトされエラーとなります。

/*
複数行コメント、C89標準でも使える。
*/

/*
複数行コメントはネストできないので/*注意*/ // コメントはここで終わり、
*/　// ここのコメント終了は扱われない。

// 定数・マクロ：#define <定数名(英数字のみ)>
// 定数はすべて大文字で定義することをおすすめします。
#define DAYS_IN_YEAR 365

// 列挙体も定数を定義する方法の一つです。
// すべてのコード行は半角英数字で書き、セミコロン「;」で終わる必要があります。
enum days {SUN, MON, TUE, WED, THU, FRI, SAT};
// SUNは0、MONは1、TUEは2、などと続く。

// 列挙体の値は別の値にできますが、数字が大きくなるようにする必要があります。
enum days {SUN = 1, MON, TUE, WED = 99, THU, FRI, SAT};
// MONは自動的に2、TUEは3、と続き、WEDで99、THUは100、FRIは101、などと続く。

// #include でヘッダーファイルをインポートできる。
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// <アングルブラケット>で囲まれたファイル名はヘッダーファイルをシステムライブラリから
// 探すことをコンパイラに伝え、自分で書いたヘッダーファイルを使うときには ”引用符” で
//そのファイルのパスを囲みます。
#include "my_header.h" 		// ローカルファイル
#include "../my_lib/my_lib_header.h" // 相対パス

// 予め関数を .h (ヘッダー)ファイルで宣言するか、
// .c (ソースコード)ファイルの上方に書いて宣言してください。
void function_1();
int function_2(void);

// 関数を使用する前に、最低でも、関数プロトタイプを宣言しなければなりません。
// プロトタイプは関数定義の前に書くのが一般的です。
int add_two_ints(int x1, int x2); // 関数プロトタイプ
// 上記の書き方でも問題ありませんが(引数の連番)、引数にはコード保守を
// 容易にするためになるべくちゃんとした名前をつけてあげましょう。

// 関数プロトタイプはその関数を使う前に定義を書いておけば必要ありません。
// しかし、関数プロトタイプをヘッダーファイルに記述し、ソースコードの上方に#includeを
// 使ってインポートすれば、コンパイラにまだ定義されていない関数を呼び出すことを防ぎ、
// ヘッダーファイルにどんな関数が定義されるのかが分かるのでプログラムの保守性も上がります。

// プログラムが最初に実行する関数はエントリーポイントといい、Cではmain()関数となります。
// 返り値はどんな型でも良いですが、Windowsなどの大抵のOSはエラーコードを検知・処理するために
// 関数はint型（整数型）を返すことが定められています。
int main(void) {
  // プログラムはここへ
}

// コマンドライン引数はプログラムの挙動やオプションを実行時に設定することができます。
// argcは引数の数を表し、プログラム名もカウントされるので常に1以上の値が入ります。
// argvは引数文字列の配列を表し、プログラム名含むすべての引数が入るます。
// argv[0]はプログラム名を、argv[1]は最初の引数などです。
int main (int argc, char** argv)
{
  // コンソールに文字などを表示するときにはprintf関数を使います。
  // printfは”print format”のことで、書式に沿って値を表示させます。
  // %dには整数が入り、\nは新しい行("n"ew line)へ移動します。
  printf("%d\n", 0); // => 0が表示される

  // scanf関数はコンソールからユーザの入力を受け付けます。
  // 変数の前の'&'記号はメモリ上の変数の住所(address)を求める一種の演算子です。
  // この例では整数型の値をinput変数の住所に値を代入します。
  int input;
  scanf("%d", &input);

  ///////////////////////////////////////
  // 型
  ///////////////////////////////////////

  // C99標準の互換性がないコンパイラでは、そのスコープで使用するすべての変数は
  // スコープの一番上に宣言する必要があります。C99標準互換のコンパイラは、使用する前なら
  // スコープ内のどこでも宣言可能です。このチュートリアルでは、C99標準に統一して書いていきます。

  // int型は大抵の場合整数を4バイトのメモリで格納しますが、古いCPUでは2バイトで格納します。
  // sizeof演算子を使えば、その型が何バイト使うか確認できます。
  int x_int = 0;

  // short型は2バイトで整数を格納。
  short x_short = 0;

  // char型は大抵のプロセッサーでは、最小のサイズで、
  // 1バイトのサイズで整数またはASCII文字一つを格納できます。
  // この型のサイズはプロセッサーによって異なり、2バイト以上の物もあります。
  // (例：TIからリリースされたTMS320は2バイトで格納される。)
  char x_char = 0;
  char y_char = 'y'; // ASCII文字リテラルは''で囲まれる。

  // long型は4~8バイトで整数を格納します。long long型は常に8バイトであることが保証されます。
  long x_long = 0;
  long long x_long_long = 0;

  // float型は32ビットの単精度浮遊少数を格納します。
  float x_float = 0.0f; // 'f'はその数字リテラルが単精度浮遊少数であることを示します。

  // double型は64ビットの倍精度浮遊少数を格納します。
  double x_double = 0.0; // 実数のあとに何もつかない数字リテラルは倍精度浮遊少数として扱います。

  // 整数型はunsignedをつけることで0以上の正の数のみを格納させることができます。
  unsigned short ux_short;
  unsigned int ux_int;
  unsigned long long ux_long_long;

  // char型に格納されている文字はASCIIなどの文字コードに対応する整数でもあります。
  '0'; // => ASCIIで48を表す。
  'A'; // => ASCIIで65を表す。

  // sizeof(T)でその型のサイズをバイトで返す(Tには型名が入る。)
  // sizeof(obj)はその値の型のサイズを返す。(objには定数、変数、生の値が入る。)
  printf("%zu\n", sizeof(int)); // => 4 (on most machines with 4-byte words)

  // もしsizeof演算子の引数が式だった場合、VLA(可変長配列、Variable Length Array)でない限り、
  // その式は評価されません。この場合の引数の値はコンパイル時定数である。
  int a = 1;
  // size_t型は変数などの型サイズを表す2バイト以上の非負の整数を格納します。
  size_t size = sizeof(a++); // a++ は評価されない。
  printf("sizeof(a++) = %zu where a = %d\n", size, a);
  // prints "sizeof(a++) = 4 where a = 1" (32ビット環境での場合)

  // 配列は定義時にサイズを決める必要があります。
  char my_char_array[20]; // この配列は 1 * 20 = 20 バイト使います
  int my_int_array[20]; // この配列は 4 * 20 = 80 バイト使います
  // (4バイト整数環境であると仮定した場合)

  // 次の定義では整数配列を20個の0で埋めた状態で初期値が与えられます。
  int my_array[20] = {0};
  // "{0}"は配列初期化子です。
  // 初期化子に含まれる要素以外の要素は、（もしあれば）すべて0に初期化されます：
  int my_array[5] = {1, 2};
  // 上記の定義ではmy_arrayは5つの要素があり、最初の2つ以外は0に初期化されています：
  // [1, 2, 0, 0, 0]
  // 配列定義のときに明示的に初期化を行えば、要素数を決める必要がなくなります：
  int my_array[] = {0};
  // サイズを指定しないで定義すると配列初期化子の要素数がそのまま自動的に決めまれます。
  // よって"{0}"で初期化した場合、配列のサイズは1となり、"[0]"が代入されます。
  // 実行時に配列の要素数を調べるには、配列のサイズを1つの要素のサイズで割れば良いのです。
  size_t my_array_size = sizeof(my_array) / sizeof(my_array[0]);
  // 注意；この操作は配列ごとに、かつ関数に渡す前に実行することを勧めします。なぜなら
  // 関数に配列を渡すと、ポインター（メモリ上の場所を表す単なる整数）に変換され、
  // 関数内で同じ操作を行うと、間違った結果につながる恐れがあるからです。

  // 要素へアクセスするときは他の言語と同じようにできます。
  // 正しく言えば、Cに似た言語です。
  my_array[0]; // => 0

  // 配列は変更可能です。
  my_array[1] = 2;
  printf("%d\n", my_array[1]); // => 2

  // C99標準以降（C11では任意選択）では、可変長配列(VLA)が使用可能で、コンパイル時に
  // 定数による要素数指定をしなくても、変数などによる指定ができるようになります。
  printf("Enter the array size: "); // ユーザーに要素数を入力してもらう。
  int array_size;
  fscanf(stdin, "%d", &array_size);
  int var_length_array[array_size]; // VLAを宣言する。
  printf("sizeof array = %zu\n", sizeof var_length_array);

  // 例:
  // > Enter the array size: 10
  // > sizeof array = 40

  // 文字列はヌル文字(0x00, '\0')で終わる配列でもあります。
  // 文字列リテラルを使用する場合はコンパイラが末尾に塗る文字を追加するので明示的に
  // 入れなくても良いです。
  char a_string[20] = "This is a string";
  printf("%s\n", a_string); // %s フォーマットで文字列を表示

  printf("%d\n", a_string[16]); // => 0
  // 例, 17番目のバイトは0 (18, 19, 20番目も同様)

  // シングルクォーテーションで囲まれた文字は文字リテラルです。これはchar型*ではなく*、
  // int型です。(これには歴史的背景があります。)
  int cha = 'a'; // OK
  char chb = 'a'; // これもOK (intからcharへの暗黙的型変換)

  // 多次元配列:
  int multi_array[2][5] = {
    {1, 2, 3, 4, 5},
    {6, 7, 8, 9, 0}
  };
  // 要素の取得:
  int array_int = multi_array[0][2]; // => 3

  ///////////////////////////////////////
  // 演算子
  ///////////////////////////////////////

  // 複数の同一型変数の略記法:
  int i1 = 1, i2 = 2;
  float f1 = 1.0, f2 = 2.0;

  int b, c;
  b = c = 0;

  // 四則演算は直感的にかけます:
  i1 + i2; // => 3
  i2 - i1; // => 1
  i2 * i1; // => 2
  i1 / i2; // => 0 (0.5だが、0に繰り下げられている)

  // 結果を少数にするにはどちらか一方の変数をfloat型へキャスティングする必要がある。
  (float)i1 / i2; // => 0.5f
  i1 / (double)i2; // => 0.5 // double型でも同様の操作ができる
  f1 / f2; // => 0.5, プラスマイナス計算機イプシロン(その型が表せる最小の少数)

  // 浮動小数点数はIEEE 754の仕様で定義されているので、コンピューターは正確な
  // 数をメモリ上で保存できない。よって意図しない数になることがある。例えば、0.1は
  // 0.099999999999、0.3は0.300000000001として保存されているかもしれません。
  (0.1 + 0.1 + 0.1) != 0.3; // => 1 (真)
  // なのでこれは上記の理由でこの真偽式は真になりません。
  1 + (1e123 - 1e123) != (1 + 1e123) - 1e123; // => 1 (真)
  // こちらは科学的表記法です : 1e123 = 1*10^123

  // ほとんどのシステムはIEEE 754に基づいて浮動小数点数を定義していることを
  // 知っておくことが重要になってきます。科学演算で多用されるPythonでも最終的に
  // IEEE 754を使うCを呼び出すことになります。この注意書きはCの浮動小数点数の
  // 仕様が悪く使うべきではないということをほのめかすのではなく、こういった誤差
  // (イプシロン)を考慮した上で比較するというのを頭に入れておくために書かれました。

  // 剰余演算もありますが、負の値を計算するときには注意してください：
  11 % 3;    // => 2 (11 = 2 + 3*x (x=3))
  (-11) % 3; // => -2 (-11 = -2 + 3*x (x=-3))
  11 % (-3); // => 直感に反し被除数と同じ符号になる、2 (11 = 2 + (-3)*x (x=-3)) 

  // 比較演算は親しみがあるかもしれませんが、Cには真偽型がなく、
  // 代わりに整数型が使われます。(C99以降は _Bool型がstdbool.hで
  // 提供されました。) 0は偽を表し、それ以外はすべて真として扱います。
  // 比較演算を使用する際は必ず0か1を返します。
  3 == 2; // => 0 (偽)　等しい
  3 != 2; // => 1 (真)　等しくない
  3 > 2;  // => 1       より大きい
  3 < 2;  // => 0       より小さい
  2 <= 2; // => 1       以下
  2 >= 2; // => 1       以上

  // CはPython出ないので、演算子の連鎖はできません。
  // 下記の例では問題なくコンパイルしますが、`0 < a < 2`は`(0 < a) < 2`になり、
  // `(0 < a)`の結果が真でも偽でも結局`0 < 2`または`1 < 2`となるので常に真となります。
  int between_0_and_2 = 0 < a < 2;
  // 代わりにこう書きます:
  int between_0_and_2 = 0 < a && a < 2;

  // 整数に対する論理演算子:
  !3; // => 0 (否定)
  !0; // => 1
  1 && 1; // => 1 (論理積)
  0 && 1; // => 0
  0 || 1; // => 1 (論理和)
  0 || 0; // => 0

  // 条件付き三元式 ( ? : )
  int e = 5;
  int f = 10;
  int z;
  z = (e > f) ? e : f; // => 10 "もし(e > f)が真ならばeを、偽ならばfを返す。"

  // 加算・減算演算子:
  int j = 0;
  int s = j++; // jを返してからjを1増やす (s = 0, j = 1)
  s = ++j; // jを1増やしてからjを返す (s = 2, j = 2)
  // 減算演算子 j-- と --j でも同様

  // ビット演算子
  // 整数などのデータは0と1の2進数で表されておりそれぞれをビットといいます。
  // これらの演算子は各ビットに論理演算を適用します。
  ~0x0F; // => 0xFFFFFFF0 (ビット単位NOT、補数、32ビット16進数整数での例)
  0x0F & 0xF0; // => 0x00 (ビット単位AND)
  0x0F | 0xF0; // => 0xFF (ビット単位OR)
  0x04 ^ 0x0F; // => 0x0B (ビット単位XOR)
  0x01 << 1; // => 0x02 (算術左シフト (1ビット幅))
  0x02 >> 1; // => 0x01 (算術右シフト (1ビット幅))

  // 正負のついた整数に対するビットシフトには注意してください - これらの操作は未定義です:
  // - 符号ビットへのビットシフト (int a = 1 << 31)
  // - 負の整数を左シフトする (int a = -1 << 2)
  // - 型のビットサイズ以上の幅でシフト:
  //   int a = 1 << 32; // 32ビット幅の整数の場合では未定義の動作

  ///////////////////////////////////////
  // 制御構造
  ///////////////////////////////////////

  // 条件文
  if (0) {
    printf("I am never run\n");
  } else if (0) {
    printf("I am also never run\n");
  } else {
    printf("I print\n");
  }

  // whileループ文
  int ii = 0;
  while (ii < 10) { // 10以下の整数がこの条件を満たす
    printf("%d, ", ii++); // ii++ が値を使用してから1加算される。
  } // => "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "が出力される

  printf("\n");

  // do-whileループ文
  int kk = 0;
  do {
    printf("%d, ", kk);
  } while (++kk < 10); // ++kk が値を使用する*前*に1加算される.
  // => "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "が出力される

  printf("\n");

  // forループ文
  int jj;
  for (jj=0; jj < 10; jj++) {
    printf("%d, ", jj);
  } // => "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "が出力される

  printf("\n");

  // *****注*****:
  // ループ文、関数には最低でも一つの命令・文が必要になります:
  int i;
  for (i = 0; i <= 5; i++) {
    ; // セミコロン単体で何もしないという命令を作れる(ヌル命令)
  }
  // 別の表記法:
  for (i = 0; i <= 5; i++);

  // switch文(if文より高速)
  switch (a) {
  case 0: // caseレーベルにはint型や列挙型やchar型等の整数で表せるものに限定されます。
    printf("Hey, 'a' equals 0!\n");
    break; // ブレイク文がなければ後続のcaseレーベルも実行されてしまいます。
  case 1:
    printf("Huh, 'a' equals 1!\n");
    break;
    // break文がそのcaseレーベルになければ、break文があるレーベルまですべて実行されます。
  case 3:
  case 4:
    printf("Look at that.. 'a' is either 3, or 4\n");
    break;
  default:
    // 上記の条件がすべて合致しなければdefaultレーベル下の命令が実行されます。
    fputs("Error!\n", stderr);
    exit(-1);
    break;
  }
  
  // goto文
  typedef enum { false, true } bool;
  // C99より前のC標準ではブール値が標準で定義されていません。
  bool disaster = false;
  int i, j;
  for(i=0; i<100; ++i)
  for(j=0; j<100; ++j)
  {
    if((i + j) >= 150)
        disaster = true;
    if(disaster)
        goto error;  // 両方のforループから抜ける
  }
  error: // goto error;"で"error"レーベルまで「ジャンプ」します。
  printf("Error occurred at i = %d & j = %d.\n", i, j);
  /*
    この例の出所: https://ideone.com/GuPhd6
    "Error occurred at i = 51 & j = 99."が出力されます。
  */
  /*
    ほとんどの場合、goto文を使うのは、そのコードが何をするかわかっていない限り、
    良くないとされています。詳細は
    https://en.wikipedia.org/wiki/Spaghetti_code#Meaning
    を読んでください。
  */

  ///////////////////////////////////////
  // 型キャスティング(型変換)
  ///////////////////////////////////////

  // Every value in C has a type, but you can cast one value into another type
  // if you want (with some constraints).
  // すべての値には型がありますが、これらは、互換性がある別の型にキャスティングすることができます。

  int x_hex = 0x01; // 16進数リテラルで変数を定義できます。
                    // 2進数リテラルにはコンパイラごとに差があります。
                    // (GCCではx_bin = 0b0010010110)

  // Casting between types will attempt to preserve their numeric values
  // 型キャスティングを行うとその値を保持しようとします。
  printf("%d\n", x_hex); // => 1
  printf("%d\n", (short) x_hex); // => 1
  printf("%d\n", (char) x_hex); // => 1

  // If you assign a value greater than a types max val, it will rollover
  // without warning.
  // キャスティング先の型のサイズより大きい値をキャストすると警告なしに値が丸められます。
  printf("%d\n", (unsigned char) 257); // => 1 (8ビット長のunsigned char型が保持できる最大値は255)

  // char, signed char, unsigned char型の最大値はそれぞれ、<limits.h>で提供される
  // CHAR_MAX, SCHAR_MAX, UCHAR_MAXマクロを使用できます。

  // Integral types can be cast to floating-point types, and vice-versa.
  // 整数型と浮動小数点数型は双方向にキャスティング可能です。
  printf("%f\n", (double) 100); // %f はdouble型と
  printf("%f\n", (float)  100); // float型をフォーマットします。
  printf("%d\n", (char)100.0);

  ///////////////////////////////////////
  // ポインター
  ///////////////////////////////////////

  // ポインターはメモリ上のアドレスを保持する整数の変数であり、型と共に宣言・定義されます。
  // 変数から直接アドレスを取得できることができます。

  int x = 0;
  printf("%p\n", (void *)&x); // &を用いて変数のアドレスを取得します。
  // (%p は void *型の値をフォーマットします。)
  // => 結果: 変数が保持されているメモリーアドレスが表示される

  // ポインターは型名の直後にまたは変数名の直前に * を書いて宣言・定義します。
  int *px, not_a_pointer; // px はint型の値を指すポインター
  px = &x; // pxにxのアドレスを代入する。
  printf("%p\n", (void *)px); // => &xと同様の結果が出力されるはずです。
  printf("%zu, %zu\n", sizeof(px), sizeof(not_a_pointer));
  // => 64ビット環境では"8, 4"が出力されます。

  // ポインターから指示しているメモリー領域の値を取得(ディレファレンス)するには
  // ポインター宣言と同じようにポインター名の前に * を書きます。
  printf("%d\n", *px); // => xの値である0を出力

  // この機能を用いて、ポインターが指示している値を変更することができます。
  // 加算演算子はディレファレンス演算子より優先順位が高いので数学同様ディレファレンス操作を
  // 丸括弧で括ります。
  (*px)++; // pxが指しているxの値を1加算する
  printf("%d\n", *px); // => 1
  printf("%d\n", x); // => 1

  // 配列は連続したメモリー領域を確保するのに有効です。
  int x_array[20]; // 長さ20の不可変長配列を宣言
  int xx;
  for (xx = 0; xx < 20; xx++) {
    x_array[xx] = 20 - xx;
  } // x_arrayの値を 20, 19, 18,... 2, 1 と一括初期化する。

  // Declare a pointer of type int and initialize it to point to x_array
  // int型の値を指し示すポインターを宣言し、x_arrayのアドレスで初期化する
  int* x_ptr = x_array;
  // x_ptr now points to the first element in the array (the integer 20).
  // This works because arrays often decay into pointers to their first element.
  // For example, when an array is passed to a function or is assigned to a pointer,
  // it decays into (implicitly converted to) a pointer.
  // Exceptions: when the array is the argument of the `&` (address-of) operator:
  // x_ptrは整数20個の配列の最初の要素を指しています。
  // この場合配列は代入時に最初の要素へのポインターへ変換されます。
  // 関数に配列を渡す際にも暗黙的にポインターに変換されます。
  // 例外：`&`を配列に適用した場合、その配列のアドレスが返り、要素の型ではなく、
  // 配列型のポインターが使用されます：
  int arr[10];
  int (*ptr_to_arr)[10] = &arr; // &arr は `int *`型ではない！
  // It's of type "pointer to array" (of ten `int`s).
  // or when the array is a string literal used for initializing a char array:
  // これは「（10個の整数の）配列へのポインター」型です。
  // もう一つの例外には文字列リテラルをchar型配列に代入する場合：
  char otherarr[] = "foobarbazquirk";
  // or when it's the argument of the `sizeof` or `alignof` operator:
  // または、`sizeof`, `alignof`演算子を使用した場合：
  int arraythethird[10];
  int *ptr = arraythethird; // equivalent with int *ptr = &arr[0];
  printf("%zu, %zu\n", sizeof(arraythethird), sizeof(ptr));
  // "40, 4" または "40, 8" が出力されます。

  // Pointers are incremented and decremented based on their type
  // (this is called pointer arithmetic)
  // ポインター型の値を加算・減算するとその方に応じて操作できます。
  // この操作のことをポインター演算といいます。
  printf("%d\n", *(x_ptr + 1)); // => 19
  printf("%d\n", x_array[1]); // => 19

  // You can also dynamically allocate contiguous blocks of memory with the
  // standard library function malloc, which takes one argument of type size_t
  // representing the number of bytes to allocate (usually from the heap, although this
  // may not be true on e.g. embedded systems - the C standard says nothing about it).
  // 標準ライブラリ関数の一つであるmallocを使えば連続したメモリ領域を動的に確保できます。
  // malloc関数は確保するバイト数を設定するsize_t型の引数が一つあります。
  // （確保するのは大抵の場合ヒープ領域に確保されますが、組み込みデバイスなどでは
  // 挙動が異なる場合があります。このことはC標準では説明されていません。）
  int *my_ptr = malloc(sizeof(*my_ptr) * 20);
  for (xx = 0; xx < 20; xx++) {
    *(my_ptr + xx) = 20 - xx; // my_ptr[xx] = 20-xx
  } // メモリー領域を整数型配列として初期化する [20, 19, 18 ... 1]

  // Be careful passing user-provided values to malloc! If you want
  // to be safe, you can use calloc instead (which, unlike malloc, also zeros out the memory)
  // mallocで確保されたメモリー領域へのデータの書き込みには注意してください。
  // 安全性を保証するには、確保すると同時にそのメモリー領域をすべて0で埋め尽くすcalloc関数を使用してください。
  int* my_other_ptr = calloc(20, sizeof(int));

  // Note that there is no standard way to get the length of a
  // dynamically allocated array in C. Because of this, if your arrays are
  // going to be passed around your program a lot, you need another variable
  // to keep track of the number of elements (size) of an array. See the
  // functions section for more info.
  // Cには動的配列のサイズをその場で求める方法はほとんどなく、関数などに渡すときに要素数を記録する別の変数が
  // 必要になることがよくあります。詳細は次の関数についてのセクションを読んでください。
  size_t size = 10;
  int *my_arr = calloc(size, sizeof(int));
  // 要素を追加する
  size++;
  my_arr = realloc(my_arr, sizeof(int) * size); // realloc関数で配列のサイズを更新する。
  if (my_arr == NULL) {
    //Remember to check for realloc failure!
    // mallocやreallocなどを使う際には領域確保に異常がない確認するために
    // ヌルチェックをすることをおすすめします。
    return
  }
  my_arr[10] = 5;

  // Dereferencing memory that you haven't allocated gives
  // "unpredictable results" - the program is said to invoke "undefined behavior"
  // 確保されていないメモリー領域へアクセスは予測不可能な結果を招く可能性があります。
  printf("%d\n", *(my_ptr + 21)); // => who-knows-what? が出力される**かも**、クラッシュするかもしれない。

  // When you're done with a malloc'd block of memory, you need to free it,
  // or else no one else can use it until your program terminates
  // (this is called a "memory leak"):
  // メモリー領域の使用を終えたら必ずfree関数を使ってその領域を解放しなければなりません。
  // 解放しなければ、プログラムが終了しても他のプログラムからそのメモリー領域を再利用できず、
  // システム全体で使用できる容量が減ってしまいます。このことをメモリーリークと呼びます。
  free(my_ptr); // my_ptrでポイントされてるメモリー領域を解放する。

  // Strings are arrays of char, but they are usually represented as a
  // pointer-to-char (which is a pointer to the first element of the array).
  // It's good practice to use `const char *' when referring to a string literal,
  // since string literals shall not be modified (i.e. "foo"[0] = 'a' is ILLEGAL.)
  // 文字列はchar型の配列で表せますが、よく使用されるのは文字列の最初の文字を指すcharポインターです。
  // もし、単に文字列リテラルを使用するだけならば"const char*"を使い、変更不能にしておくことが推奨されています。
  // なぜならば、本来文字列リテラルのデータは変更すべきではないからです。
  // なので、" foo[0] = 'a' "といった操作はできません。
  const char *my_str = "This is my very own string literal";
  printf("%c\n", *my_str); // => 'T'

  // This is not the case if the string is an array
  // (potentially initialized with a string literal)
  // that resides in writable memory, as in:
  // char型の配列で定義されている場合は別で、文字列リテラルで初期化できますが、
  // 各要素は変更可能です。例に：
  char foo[] = "foo";
  foo[0] = 'a'; // この操作は許されており、"aoo" に変更される。

  function_1();
} // main 関数の終わり

///////////////////////////////////////
// 関数
///////////////////////////////////////

// Function declaration syntax:
// <return type> <function name>(<args>)
// 関数定義の構文：
// <戻り値の型> <関数名>(<引数>)

int add_two_ints(int x1, int x2)
{
  return x1 + x2; // returnで値を返す。
}

/*
Functions are call by value. When a function is called, the arguments passed to
the function are copies of the original arguments (except arrays). Anything you
do to the arguments in the function do not change the value of the original
argument where the function was called.
関数は値によって呼び出されます。関数が呼ばれると、引数として渡した値はコピーされ、
関数内で値を変更したとしても、渡した引数の値は変わりません。（配列はこれに従わない。）

Use pointers if you need to edit the original argument values (arrays are always
passed in as pointers).
関数内で引数の値を変更したい場合はポインターとして渡す必要があり、配列も渡すときに自動的にポインターになります。

Example: in-place string reversal
例：即興文字列反転
*/

// void型関数は値を返さない
void str_reverse(char *str_in)
{
  char tmp;
  size_t ii = 0;
  size_t len = strlen(str_in); // `strlen()` はC標準ライブラリ関数です。
                               // NOTE: `strlen` で返される文字列の長さは終端文字の
                               //       ヌルバイト('\0')を含んでいない状態です。
  // in C99 and newer versions, you can directly declare loop control variables
  // in the loop's parentheses. e.g., `for (size_t ii = 0; ...`
  // C99標準以降では、ループ定義の中にループ制御変数が定義できます。
  // 例：｀for (size_t ii = 0; ...｀
  for (ii = 0; ii < len / 2; ii++) {
    tmp = str_in[ii];
    str_in[ii] = str_in[len - ii - 1]; // ii-th char from end
    str_in[len - ii - 1] = tmp;
  }
}
//NOTE: string.h のヘッダーファイルを#includeしないとstrlen()関数が使用できません。

/*
char c[] = "This is a test.";
str_reverse(c);
printf("%s\n", c); // => ".tset a si sihT"
*/
/*
as we can return only one variable
to change values of more than one variables we use call by reference
*/
void swapTwoNumbers(int *a, int *b)
{
    int temp = *a;
    *a = *b;
    *b = temp;
}
/*
int first = 10;
int second = 20;
printf("first: %d\nsecond: %d\n", first, second);
swapTwoNumbers(&first, &second);
printf("first: %d\nsecond: %d\n", first, second);
// values will be swapped
*/

// Return multiple values.
// C does not allow for returning multiple values with the return statement. If
// you would like to return multiple values, then the caller must pass in the
// variables where they would like the returned values to go. These variables must
// be passed in as pointers such that the function can modify them.
int return_multiple( int *array_of_3, int *ret1, int *ret2, int *ret3)
{
    if(array_of_3 == NULL)
        return 0; //return error code (false)

    //de-reference the pointer so we modify its value
   *ret1 = array_of_3[0];
   *ret2 = array_of_3[1];
   *ret3 = array_of_3[2];

   return 1; //return error code (true)
}

/*
With regards to arrays, they will always be passed to functions
as pointers. Even if you statically allocate an array like `arr[10]`,
it still gets passed as a pointer to the first element in any function calls.
Again, there is no standard way to get the size of a dynamically allocated
array in C.
*/
// Size must be passed!
// Otherwise, this function has no way of knowing how big the array is.
void printIntArray(int *arr, size_t size) {
    int i;
    for (i = 0; i < size; i++) {
        printf("arr[%d] is: %d\n", i, arr[i]);
    }
}
/*
int my_arr[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
int size = 10;
printIntArray(my_arr, size);
// will print "arr[0] is: 1" etc
*/

// if referring to external variables outside function, you should use the extern keyword.
int i = 0;
void testFunc() {
  extern int i; //i here is now using external variable i
}

// make external variables private to source file with static:
static int j = 0; //other files using testFunc2() cannot access variable j
void testFunc2() {
  extern int j;
}
// The static keyword makes a variable inaccessible to code outside the
// compilation unit. (On almost all systems, a "compilation unit" is a .c
// file.) static can apply both to global (to the compilation unit) variables,
// functions, and function-local variables. When using static with
// function-local variables, the variable is effectively global and retains its
// value across function calls, but is only accessible within the function it
// is declared in. Additionally, static variables are initialized to 0 if not
// declared with some other starting value.
//**You may also declare functions as static to make them private**

///////////////////////////////////////
// User-defined types and structs
///////////////////////////////////////

// Typedefs can be used to create type aliases
typedef int my_type;
my_type my_type_var = 0;

// Structs are just collections of data, the members are allocated sequentially,
// in the order they are written:
struct rectangle {
  int width;
  int height;
};

// It's not generally true that
// sizeof(struct rectangle) == sizeof(int) + sizeof(int)
// due to potential padding between the structure members (this is for alignment
// reasons). [1]

void function_1()
{
  struct rectangle my_rec = { 1, 2 }; // Fields can be initialized immediately

  // Access struct members with .
  my_rec.width = 10;
  my_rec.height = 20;

  // You can declare pointers to structs
  struct rectangle *my_rec_ptr = &my_rec;

  // Use dereferencing to set struct pointer members...
  (*my_rec_ptr).width = 30;

  // ... or even better: prefer the -> shorthand for the sake of readability
  my_rec_ptr->height = 10; // Same as (*my_rec_ptr).height = 10;
}

// You can apply a typedef to a struct for convenience
typedef struct rectangle rect;

int area(rect r)
{
  return r.width * r.height;
}

// Typedefs can also be defined right during struct definition
typedef struct {
  int width;
  int height;
} rect;
// Like before, doing this means one can type
rect r;
// instead of having to type
struct rectangle r;

// if you have large structs, you can pass them "by pointer" to avoid copying
// the whole struct:
int areaptr(const rect *r)
{
  return r->width * r->height;
}

///////////////////////////////////////
// Function pointers
///////////////////////////////////////
/*
At run time, functions are located at known memory addresses. Function pointers are
much like any other pointer (they just store a memory address), but can be used
to invoke functions directly, and to pass handlers (or callback functions) around.
However, definition syntax may be initially confusing.

Example: use str_reverse from a pointer
*/
void str_reverse_through_pointer(char *str_in) {
  // Define a function pointer variable, named f.
  void (*f)(char *); // Signature should exactly match the target function.
  f = &str_reverse; // Assign the address for the actual function (determined at run time)
  // f = str_reverse; would work as well - functions decay into pointers, similar to arrays
  (*f)(str_in); // Just calling the function through the pointer
  // f(str_in); // That's an alternative but equally valid syntax for calling it.
}

/*
As long as function signatures match, you can assign any function to the same pointer.
Function pointers are usually typedef'd for simplicity and readability, as follows:
*/

typedef void (*my_fnp_type)(char *);

// Then used when declaring the actual pointer variable:
// ...
// my_fnp_type f;


/////////////////////////////
// Printing characters with printf()
/////////////////////////////

//Special characters:
/*
'\a'; // alert (bell) character
'\n'; // newline character
'\t'; // tab character (left justifies text)
'\v'; // vertical tab
'\f'; // new page (form feed)
'\r'; // carriage return
'\b'; // backspace character
'\0'; // NULL character. Usually put at end of strings in C.
//   hello\n\0. \0 used by convention to mark end of string.
'\\'; // backslash
'\?'; // question mark
'\''; // single quote
'\"'; // double quote
'\xhh'; // hexadecimal number. Example: '\xb' = vertical tab character
'\0oo'; // octal number. Example: '\013' = vertical tab character

//print formatting:
"%d";    // integer
"%3d";   // integer with minimum of length 3 digits (right justifies text)
"%s";    // string
"%f";    // float
"%ld";   // long
"%3.2f"; // minimum 3 digits left and 2 digits right decimal float
"%7.4s"; // (can do with strings too)
"%c";    // char
"%p";    // pointer. NOTE: need to (void *)-cast the pointer, before passing
         //                it as an argument to `printf`.
"%x";    // hexadecimal
"%o";    // octal
"%%";    // prints %
*/

///////////////////////////////////////
// Order of Evaluation
///////////////////////////////////////

// From top to bottom, top has higher precedence
//---------------------------------------------------//
//        Operators                  | Associativity //
//---------------------------------------------------//
// () [] -> .                        | left to right //
// ! ~ ++ -- + = *(type) sizeof      | right to left //
// * / %                             | left to right //
// + -                               | left to right //
// << >>                             | left to right //
// < <= > >=                         | left to right //
// == !=                             | left to right //
// &                                 | left to right //
// ^                                 | left to right //
// |                                 | left to right //
// &&                                | left to right //
// ||                                | left to right //
// ?:                                | right to left //
// = += -= *= /= %= &= ^= |= <<= >>= | right to left //
// ,                                 | left to right //
//---------------------------------------------------//

/******************************* Header Files **********************************

Header files are an important part of C as they allow for the connection of C
source files and can simplify code and definitions by separating them into
separate files.

Header files are syntactically similar to C source files but reside in ".h"
files. They can be included in your C source file by using the precompiler
command #include "example.h", given that example.h exists in the same directory
as the C file.
*/

/* A safe guard to prevent the header from being defined too many times. This */
/* happens in the case of circle dependency, the contents of the header is    */
/* already defined.                                                           */
#ifndef EXAMPLE_H /* if EXAMPLE_H is not yet defined. */
#define EXAMPLE_H /* Define the macro EXAMPLE_H. */

/* Other headers can be included in headers and therefore transitively */
/* included into files that include this header.                       */
#include <string.h>

/* Like for c source files, macros can be defined in headers */
/* and used in files that include this header file.          */
#define EXAMPLE_NAME "Dennis Ritchie"

/* Function macros can also be defined.  */
#define ADD(a, b) ((a) + (b))

/* Notice the parenthesis surrounding the arguments -- this is important to   */
/* ensure that a and b don't get expanded in an unexpected way (e.g. consider */
/* MUL(x, y) (x * y); MUL(1 + 2, 3) would expand to (1 + 2 * 3), yielding an  */
/* incorrect result)                                                          */

/* Structs and typedefs can be used for consistency between files. */
typedef struct Node
{
    int val;
    struct Node *next;
} Node;

/* So can enumerations. */
enum traffic_light_state {GREEN, YELLOW, RED};

/* Function prototypes can also be defined here for use in multiple files,  */
/* but it is bad practice to define the function in the header. Definitions */
/* should instead be put in a C file.                                       */
Node createLinkedList(int *vals, int len);

/* Beyond the above elements, other definitions should be left to a C source */
/* file. Excessive includes or definitions should also not be contained in   */
/* a header file but instead put into separate headers or a C file.          */

#endif /* End of the if precompiler directive. */

```

## Further Reading

Best to find yourself a copy of [K&R, aka "The C Programming Language"](https://en.wikipedia.org/wiki/The_C_Programming_Language)
It is _the_ book about C, written by Dennis Ritchie, the creator of C, and Brian Kernighan. Be careful, though - it's ancient and it contains some
inaccuracies (well, ideas that are not considered good anymore) or now-changed practices.

Another good resource is [Learn C The Hard Way](http://learncodethehardway.org/c/) (not free).

If you have a question, read the [compl.lang.c Frequently Asked Questions](http://c-faq.com).

It's very important to use proper spacing, indentation and to be consistent with your coding style in general.
Readable code is better than clever code and fast code. For a good, sane coding style to adopt, see the
[Linux kernel coding style](https://www.kernel.org/doc/Documentation/process/coding-style.rst).

Other than that, Google is your friend.

[1] [Why isn't sizeof for a struct equal to the sum of sizeof of each member?](https://stackoverflow.com/questions/119123/why-isnt-sizeof-for-a-struct-equal-to-the-sum-of-sizeof-of-each-member)
