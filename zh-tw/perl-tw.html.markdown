---
name: perl
category: language
language: perl
filename: learnperl-tw.pl
contributors:
    - ["Korjavin Ivan", "http://github.com/korjavin"]
    - ["Dan Book", "http://github.com/Grinnz"]
translators:
    - ["Kang-min Liu", "https://gugod.org"]
    - ["Shih-Kai Chiu", "https://twitter.com/zard1989"]
lang: zh-tw
---

Perl 5 是一款強大且功能豐富的程式語言，已經持續發展超過 25 年。

從大型主機到行動裝置，Perl 5 能在上百種平台執行，適合快速打造產品原型，也適合大型專案開發。

```perl
# 註解列皆以井字號為開頭

#### 嚴謹度

use strict;
use warnings;

# 所有的 perl 程式檔案都應當包含此兩列程式碼。在如變數名稱有拼寫錯誤之時，
# strict 能使編譯過程失敗。而對於像是將未定義值接到字串中等等易犯之錯誤，
# warnings 則能提供適當的警告訊息。

#### Perl 變數與其型別

# 變數的開頭皆為一印記（sigil），是為一符號，用以標示其型別。
# 變數名稱唯有以字母或底線開頭，後接字母、數字、底線若干，方為有效。

### 在 Perl 語言中，主要的變數型別有三種：$純量、@陣列、%雜湊。

## 純量
#  一個純量變數，只能裝一個值：
my $animal = "camel";
my $answer = 42;
my $display = "You have $answer ${animal}s.\n";

# 純量值可為字串、整數、浮點數。Perl 會自動地在需要之時進行轉換。

# 以單引號括住的字串內容與其字面之值完全相同。而以雙引號括住的字串，
# 其中則能內插變數與像是這種表示換列字符 "\n" 的控制碼。

## 陣列
# 一個陣列，可以裝下很多值：
my @animals = ("camel", "llama", "owl");
my @numbers = (23, 42, 69);
my @mixed   = ("camel", 42, 1.23);

# 陣列元素的存取，需要角括號。前方的印記為 $ 符號，表示只取一個值。
my $second = $animals[1];

# 欲知陣列之大小，在純量語境之下使用陣列便可。例如，將陣列裝到一個純量變數中。
# 又或者是使用 "scalar" 算符。

my $num_animals = @animals;
print "Number of numbers: ", scalar(@numbers), "\n";

# 陣列也能夠被安插在雙引號字串之內。各內容元素間隔，預設是一個空白字符。

print "We have these numbers: @numbers\n";

# 雙引號字串中，若有像電子郵件地址的部分，會被視為是在內插某個陣列的內容物。
# 請稍加留意。

my @example = ('secret', 'array');
my $oops_email = "foo@example.com"; # 'foosecret array.com'
my $ok_email = 'foo@example.com';

## 雜湊
# 一個雜湊，能裝下許多對的鍵與值：

my %fruit_color = ("apple", "red", "banana", "yellow");

# 善用空白與 "=>" 算符，就能將其排得得好看一些：

my %fruit_color = (
  apple  => "red",
  banana => "yellow",
);

# 雜湊元素的存取，需要大括號。前方的印記仍為 $ 符號，表示只取一個值。
my $color = $fruit_color{apple};

# 以 "keys" 與 "values" 兩個函數，則可一次取得雜湊中的所有鍵、所有值。
my @fruits = keys %fruit_color;
my @colors = values %fruit_color;

# 關於純量、陣列、雜湊，在 perldata 文件之中，有更完整的描述。
# (perldoc perldata)

#### 參照

# 以參照能組出結構更為複雜的資料型別。像是在陣列中放入雜湊、在雜湊裡放入陣列的雜湊。

my $array_ref = \@array;
my $hash_ref = \%hash;
my @array_of_arrays = (\@array1, \@array2, \@array3);

# 匿名陣列與匿名雜湊也是參照

my $fruits = ["apple", "banana"];
my $colors = {apple => "red", banana => "yellow"};

# 在參照之前補上適當的印記，是為解參照。

my @fruits_array = @$fruits;
my %colors_hash = %$colors;

# 以箭頭算符，便可在解參照同時存取其中一值。

my $first = $array_ref->[0];
my $value = $hash_ref->{banana};

# 欲深入了解參照，詳見 perlreftut 與 perlref 兩份文件

#### 條件結構與迴圈結構

# Perl 語言中亦具備常見的條件結講與迴圈結構。

if ($var) {
  ...
} elsif ($var eq 'bar') {
  ...
} else {
  ...
}

unless (condition) {
  ...
}
# 這算是可讀性較好的 "if (!condition)"

# 倒裝句型算是某「很 Perl 的」寫法
print "Yow!" if $zippy;
print "We have no bananas" unless $bananas;

#  while
while (condition) {
  ...
}

my $max = 5;
# 以 for 迴圈，$i 為迭代變數
for my $i (0 .. $max) {
  print "index is $i";
}

for my $element (@elements) {
  print $element;
}

map {print} @elements;

# 迭代變數為 $_
for (@elements) {
  print;
}

# 對雜湊進行迭代（for 與 foreach 完全相同）

foreach my $key (keys %hash) {
  print $key, ': ', $hash{$key}, "\n";
}

# 又是「很 Perl 的」倒裝句法
print for @elements;

# 對一雜湊參照之中迭代，逐一走過其鍵與值
print $hash_ref->{$_} for keys %$hash_ref;

#### 正規表示式

# Perl 中，對正規表示式的支援既廣亦深，在 perlrequick、perlretut 等各處文件中
# 都有更加完整的文件。不過，簡而言之：

# 簡易比對
if (/foo/)       { ... }  # 若 $_ 內含 "foo" 則為真
if ($x =~ /foo/) { ... }  # 若 $x 內含 "foo" 則為真

# 簡易取代
$x =~ s/foo/bar/;         # 將 $x 中第一個出現的 foo 換為 bar
$x =~ s/foo/bar/g;        # 將 $x 中所有出現的 foo 換為 bar

#### 檔案與輸出入

# 以 "open" 函式開檔後，便可自檔案輸入或對其輸出

# 讀檔：
open(my $in,  "<",  "input.txt")  or die "Can't open input.txt: $!";

# 寫檔（若檔案已經存在，舊內容會被清空）：
open(my $out, ">",  "output.txt") or die "Can't open output.txt: $!";

# 寫檔（若檔案已經存在，會寫到檔尾去）：
open(my $log, ">>", "my.log")     or die "Can't open my.log: $!";

# 使用 "<>" 算符，能對檔案代號進行讀取。在純量語境下，會自檔案代號讀一列內容。
# 而在串列語境下，對讀入整個檔案。每一列都會成為串列中一項元素。

my $line  = <$in>;
my @lines = <$in>;

# 以 "print" 函式，則可對檔案代號進行輸出。

print $out @lines;
print $log $msg, "\n";

#### 函式之撰寫

# 撰寫函式很是容易：

sub logger {
  my $logmessage = shift;

  open my $logfile, ">>", "my.log" or die "Could not open my.log: $!";

  print $logfile $logmessage;
}

# 之後，使用起來就與內建函式無異：

logger("We have a logger subroutine!");

#### 模組

# A module is a set of Perl code, usually subroutines, which can be used
# in other Perl code. It is usually stored in a file with the extension
# .pm so that Perl can find it.

# 所謂模組，就是一組 Perl 程式碼，由一些函式組成，並可讓其他 Perl 程式碼來利用。
# 為了讓 perl 能找至，通常模組之副標名為 .pm 。

package MyModule;
use strict;
use warnings;

sub trim {
  my $string = shift;
  $string =~ s/^\s+//;
  $string =~ s/\s+$//;
  return $string;
}

1;

# 自他處利用：

use MyModule;
MyModule::trim($string);

# Exporter 模組能將函式出口，好讓它們能被這樣利用：

use MyModule 'trim';
trim($string);

# 有許多 Perl 模組能從 CPAN (https://www.cpan.org) 下載下來，各式各樣的機能讓你
# 能免於重新發明輪子。不少高人氣模組，如 Exporter，則是與 Perl 一同釋出、散佈。
# 更多關於 Perl 模組的細節，詳見 perlmod 文件。

#### 物件

# Perl 中的物件，只是個參照，但同時又知道自己屬於哪個類別（package），於是對自身
# 調用方法（函式）時方知去何處尋找函式本體。在建構子（通常是 "new"）中，都是以
# "bless" 函式來標記參照與其類別。只不過，若你使用像 Moose 或 Moo 模組的話，這些
# 都不必自己來（總之請繼續往下讀）。

package MyCounter;
use strict;
use warnings;

sub new {
  my $class = shift;
  my $self = {count => 0};
  return bless $self, $class;
}

sub count {
  my $self = shift;
  return $self->{count};
}

sub increment {
  my $self = shift;
  $self->{count}++;
}

1;

# 以箭頭運算符，便可對某類別或某物件呼叫某方法：
use MyCounter;
my $counter = MyCounter->new;
print $counter->count, "\n"; # 0
$counter->increment;
print $counter->count, "\n"; # 1

# CPAN 上的 Moose 與 Moo 模組能助你撰寫類別本體。它們提供了建構子，與簡單易懂的
# 語法能來宣告屬性。前述的類別改寫之後，如下：

package MyCounter;
use Moo; # 同時也啟用 strict 與 warnings

has 'count' => (is => 'rwp', default => 0, init_arg => undef);

sub increment {
  my $self = shift;
  $self->_set_count($self->count + 1);
}

1;

# 物件導向程式設計於 perlootut 文件中有詳盡的說明，同時，perlobj 文件中更函蓋了
# 底層實做之細節。
```

#### 常見問答集

perlfaq 是問與答，涵蓋許多常見問題和解法，常常對該用哪些 CPAN 模組有很好的建議。

#### 延伸閱讀

 - [perl-tutorial](http://perl-tutorial.org/)
 - [Learn Perl](https://www.perl.org/learn.html)
 - [perldoc](http://perldoc.perl.org/)
 - 內建函式 : `perldoc perlintro`
