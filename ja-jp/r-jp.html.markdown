---
language: R
contributors:
    - ["e99n09", "http://github.com/e99n09"]
    - ["isomorphismes", "http://twitter.com/isomorphisms"]
translators:
    - ["akirahirose", "https://twitter.com/akirahirose"]
filename: learnr-jp.r
lang: ja-jp
---


R は統計計算用の言語です。
データの取得やクリーニング、統計処理やグラフ作成をするために便利な、たくさんのライブラリがあります。また、LaTeX文書からRコマンドを呼び出すこともできます


```r
# コメント行は、#で開始します


# 複数行をまとめてコメントにすることはできないので、
# コメントを複数の行に分けたい場合、このように、単に毎行をコメントにしてください


# WindowsやMacでは、 COMMAND-ENTERで、コマンドを1行実行できます






#############################################################################
# プログラミングがわからなくとも使えるコマンド類
#############################################################################


# この節では、プログラミングがわからなくとも使える便利なRコマンドを紹介します
# 全てを理解できなくとも、まずはやってみましょう！


data()                # 既にロードされているデータを閲覧します
data(rivers)        # "北米にある大きな川の長さ"データを取得します
ls()                # "rivers" がワークスペースに表示されました
head(rivers)        # データの先頭部分です
# 735 320 325 392 524 450


length(rivers)        # 何本の川がデータにある?
# 141
summary(rivers) # 統計的に要約するとどうなる?
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  135.0   310.0   425.0   591.2   680.0  3710.0 


# 茎葉図（ヒストグラムに似た図）を描く
stem(rivers)


#  The decimal point is 2 digit(s) to the right of the |
#
#   0 | 4
#   2 | 011223334555566667778888899900001111223333344455555666688888999
#   4 | 111222333445566779001233344567
#   6 | 000112233578012234468
#   8 | 045790018
#  10 | 04507
#  12 | 1471
#  14 | 56
#  16 | 7
#  18 | 9
#  20 | 
#  22 | 25
#  24 | 3
#  26 | 
#  28 | 
#  30 | 
#  32 | 
#  34 | 
#  36 | 1


stem(log(rivers)) # このデータは、正規分布でも対数正規分布でもないので、注意！
# 特に正規分布原理主義のみなさん


#  The decimal point is 1 digit(s) to the left of the |
#
#  48 | 1
#  50 | 
#  52 | 15578
#  54 | 44571222466689
#  56 | 023334677000124455789
#  58 | 00122366666999933445777
#  60 | 122445567800133459
#  62 | 112666799035
#  64 | 00011334581257889
#  66 | 003683579
#  68 | 0019156
#  70 | 079357
#  72 | 89
#  74 | 84
#  76 | 56
#  78 | 4
#  80 | 
#  82 | 2


# ヒストグラム作成
hist(rivers, col="#333333", border="white", breaks=25) # これらのパラメータをつかいます
hist(log(rivers), col="#333333", border="white", breaks=25) # いろいろな使い方ができます


# 別のロード済データでやってみましょう。Rには、いろいろなデータがロードされています。
data(discoveries)
plot(discoveries, col="#333333", lwd=3, xlab="Year",
     main="Number of important discoveries per year")
plot(discoveries, col="#333333", lwd=3, type = "h", xlab="Year",
     main="Number of important discoveries per year")


# 年次のソートだけではなく、
# 標準的な並べ替えもできます
sort(discoveries)
#  [1]  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1  1  1  1  1  2  2  2  2
# [26]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  3  3  3
# [51]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  4  4  4  4  4  4  4  4
# [76]  4  4  4  4  5  5  5  5  5  5  5  6  6  6  6  6  6  7  7  7  7  8  9 10 12


stem(discoveries, scale=2)
# 
#  The decimal point is at the |
#
#   0 | 000000000
#   1 | 000000000000
#   2 | 00000000000000000000000000
#   3 | 00000000000000000000
#   4 | 000000000000
#   5 | 0000000
#   6 | 000000
#   7 | 0000
#   8 | 0
#   9 | 0
#  10 | 0
#  11 | 
#  12 | 0


max(discoveries)
# 12
summary(discoveries)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0.0     2.0     3.0     3.1     4.0    12.0 


# サイコロを振ります
round(runif(7, min=.5, max=6.5))
# 1 4 6 1 4 6 4
# 私と同じrandom.seed(31337)を使わない限りは、別の値になります


# ガウス分布を9回生成します
rnorm(9)
# [1]  0.07528471  1.03499859  1.34809556 -0.82356087  0.61638975 -1.88757271
# [7] -0.59975593  0.57629164  1.08455362






##################################################
# データ型と基本計算
##################################################


# ここからは、プログラミングをつかうチュートリアルです
# この節ではRで重要なデータ型（データクラス）の、整数型、数字型、文字型、論理型と因子（ファクター）型をつかいます
# 他にもいろいろありますが、これらの必要最小限なものから始めましょう


# 整数型
# 整数型はLで指定します
5L # 5
class(5L) # "integer"
# (?class を実行すると、class()関数について、さらなる情報が得られます)
# Rでは、この5Lのような1つの値は、長さ1のベクトルとして扱われます
length(5L) # 1
# 整数型のベクトルはこのようにつくります
c(4L, 5L, 8L, 3L) # 4 5 8 3
length(c(4L, 5L, 8L, 3L)) # 4
class(c(4L, 5L, 8L, 3L)) # "integer"


# 数字型
# 倍精度浮動小数点数です
5 # 5
class(5) # "numeric"
# しつこいですが、すべてはベクトルです
# 1つ以上の要素がある数字のベクトルも、作ることができます
c(3,3,3,2,2,1) # 3 3 3 2 2 1
# 指数表記もできます
5e4 # 50000
6.02e23 # アボガドロ数
1.6e-35 # プランク長
# 無限大、無限小もつかえます
class(Inf)        # "numeric"
class(-Inf)        # "numeric"
# 例のように、"Inf"を使ってください。integrate( dnorm(x), 3, Inf);
# Z-スコア表が必要なくなります


# 基本的な計算
# 数を計算できます
# 整数と整数以外の数字を両方使った計算をすると、結果は整数以外の数字になります
10L + 66L # 76      # 整数足す整数は整数
53.2 - 4  # 49.2    # 整数引く数字は数字
2.0 * 2L  # 4       # 数字かける整数は数字
3L / 4    # 0.75    # 整数割る数字は数字
3 %% 2          # 1       # 二つの数字を割った余りは数字
# 不正な計算は "not-a-number"になります
0 / 0 # NaN
class(NaN) # "numeric"
# 長さが1より大きなベクター同士の計算もできます
# どちらかが長い場合、短い方は何度も繰り返して使われます
c(1,2,3) + c(1,2,3) # 2 4 6

# 文字
# Rでは、文字列と文字に区別がありません
"Horatio" # "Horatio"
class("Horatio") # "character"
class('H') # "character"
# 上記は両方とも、長さ1のベクターです
# 以下は、より長い場合です
c('alef', 'bet', 'gimmel', 'dalet', 'he')
# =>
# "alef"   "bet"    "gimmel" "dalet"  "he"
length(c("Call","me","Ishmael")) # 3
# 正規表現処理を文字ベクターに適用できます
substr("Fortuna multis dat nimis, nulli satis.", 9, 15) # "multis "
gsub('u', 'ø', "Fortuna multis dat nimis, nulli satis.") # "Fortøna møltis dat nimis, nølli satis."
# Rはいくつかの文字ベクターを組み込みで持っています
letters
# =>
#  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z"
month.abb # "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"


# 論理
# Rでは、Booleanは論理（logical）型です
class(TRUE)        # "logical"
class(FALSE)        # "logical"
# 以下は比較演算子の例です
TRUE == TRUE        # TRUE
TRUE == FALSE        # FALSE
FALSE != FALSE        # FALSE
FALSE != TRUE        # TRUE
# 無いデータ (NA) も論理型です
class(NA)        # "logical"
# 以下のようにすると、複数の要素を持つ、論理型ベクターが返ります
c('Z', 'o', 'r', 'r', 'o') == "Zorro" # FALSE FALSE FALSE FALSE FALSE
c('Z', 'o', 'r', 'r', 'o') == "Z" # TRUE FALSE FALSE FALSE FALSE


# 因子（ファクター）
# 因子型は、カテゴリカルデータ用の型です
# 因子には、子供の学年のように順序がつけられるものか、性別のように順序がないものがあります
factor(c("female", "female", "male", "NA", "female"))
#  female female male   NA     female
# Levels: female male NA
# "levels" は、カテゴリカルデータがとりうる値を返します
levels(factor(c("male", "male", "female", "NA", "female"))) # "female" "male"   "NA" 
# 因子ベクターの長さが1ならば、そのlevelも1です
length(factor("male")) # 1
length(levels(factor("male"))) # 1
# 因子型は、この後で紹介するデータフレーム（というデータ型）内で、よくみられます
data(infert) # "Infertility after Spontaneous and Induced Abortion"
levels(infert$education) # "0-5yrs"  "6-11yrs" "12+ yrs"


# NULL
# "NULL" は特殊な型なのですが、ベクターを空にするときに使います
class(NULL)        # NULL
parakeet
# =>
# [1] "beak"     "feathers" "wings"    "eyes"    
parakeet <- NULL
parakeet
# =>
# NULL


# 型の強制
# 型の強制とは、ある値を、強制的に別の型として利用する事です
as.character(c(6, 8)) # "6" "8"
as.logical(c(1,0,1,1)) # TRUE FALSE  TRUE  TRUE
# さまざまな要素が入っているベクターに対して型の強制を行うと、おかしなことになります
c(TRUE, 4) # 1 4
c("dog", TRUE, 4) # "dog"  "TRUE" "4"
as.numeric("Bilbo")
# =>
# [1] NA
# Warning message:
# NAs introduced by coercion 


# 追記: ここで紹介したのは、基本的な型だけです
# 実際には、日付（dates）や時系列（time series）など、いろいろな型があります






##################################################
# 変数、ループ、もし/ほかに（if/else）
##################################################


# 変数は、ある値を後で使うために入れておく、箱のようなものです
# 箱に入れることを、変数に値を代入する、といいます
# 変数を使うと、ループや関数、if/else 分岐を利用できます


# 変数
# 代入する方法はいろいろあります
x = 5 # これはできます
y <- "1" # これがおすすめです
TRUE -> z # これも使えますが、ちょっとわかりにくいですね


# ループ
# forでループできます
for (i in 1:4) {
  print(i)
}
# whileでループできます
a <- 10
while (a > 4) {
        cat(a, "...", sep = "")
        a <- a - 1
}
# Rでは、forやwhileは遅いことを覚えておいてください
# ベクターを丸ごと処理する（つまり、行全体や、列全体を指定して処理する）か、
# 後述する、apply()系の関数を使うのが、速度的にはお勧めです


# IF/ELSE
# ごく普通のif文です
if (4 > 3) {
        print("4 is greater than 3")
} else {
        print("4 is not greater than 3")
}
# =>
# [1] "4 is greater than 3"


# 関数
# 以下のように定義します
jiggle <- function(x) {
        x = x + rnorm(1, sd=.1)        #すこしだけ（制御された）ノイズを入れます
        return(x)
}
# 他の関数と同じように、呼びます
jiggle(5)        # 5±ε.  set.seed(2716057)をすると、jiggle(5)==5.005043






###########################################################################
# データ構造: ベクター、行列、データフレーム、配列
###########################################################################


# 1次元


# まずは基本からです。ご存じベクターからです
vec <- c(8, 9, 10, 11)
vec        #  8  9 10 11
# 特定の要素を、[角括弧]による指定で取り出せます
# (Rでは、最初の要素は1番目と数えます)
vec[1]                # 8
letters[18]        # "r"
LETTERS[13]        # "M"
month.name[9]        # "September"
c(6, 8, 7, 5, 3, 0, 9)[3]        # 7
# 特定のルールに当てはまる要素を見つけることもできます
which(vec %% 2 == 0)        # 1 3
# 最初か最後の数個を取り出すこともできます
head(vec, 1)        # 8
tail(vec, 2)        # 10 11
# ある値がベクターにあるかどうかをみることができます
any(vec == 10) # TRUE
# ベクターの数より大きなインデックスを指定すると、NAが返ります
vec[6]        # NA
# ベクターの長さは、length()で取得できます
length(vec)        # 4
# ベクター全体、または1部に対して、操作ができます
vec * 4        # 16 20 24 28
vec[2:3] * 5        # 25 30
any(vec[2:3] == 8) # FALSE
# R には、ベクターにある値を要約するための様々な関数があります
mean(vec)        # 9.5
var(vec)        # 1.666667
sd(vec)                # 1.290994
max(vec)        # 11
min(vec)        # 8
sum(vec)        # 38
# 他にも、ベクター関連ではいろいろな関数があります。以下はベクターをつくるための方法です
5:15        # 5  6  7  8  9 10 11 12 13 14 15
seq(from=0, to=31337, by=1337)
# =>
#  [1]     0  1337  2674  4011  5348  6685  8022  9359 10696 12033 13370 14707
# [13] 16044 17381 18718 20055 21392 22729 24066 25403 26740 28077 29414 30751


# 2次元配列 (すべての値が同じ型の場合)


# 同じ型の値が含まれる2次元配列は、このように作れます
mat <- matrix(nrow = 3, ncol = 2, c(1,2,3,4,5,6))
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# ベクターとは違い、2次元配列の型名は"matrix"です。
class(mat) # => "matrix"
# 最初の行
mat[1,]        # 1 4
# 最初の列に対する操作
3 * mat[,1]        # 3 6 9
# 特定のセルを取り出し
mat[3,2]        # 6


# 2次元配列全体を転置します
t(mat)
# =>
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6


# 2次元配列の積
mat %*% t(mat)
# =>
#      [,1] [,2] [,3]
# [1,]   17   22   27
# [2,]   22   29   36
# [3,]   27   36   45


# cbind() は、複数のベクターを、別々の列に並べて2次元配列を作ります
mat2 <- cbind(1:4, c("dog", "cat", "bird", "dog"))
mat2
# =>
#      [,1] [,2]   
# [1,] "1"  "dog"  
# [2,] "2"  "cat"  
# [3,] "3"  "bird" 
# [4,] "4"  "dog"
class(mat2)        # matrix
# ここでいま1度、2次元配列内の型について注意してください!
# 2次元配列にある値は、すべて同じ型にする必要があります。そのため、すべて文字型に変換されています
c(class(mat2[,1]), class(mat2[,2]))


# rbind() は、複数のベクターを、別々の行に並べて2次元配列を作ります
mat3 <- rbind(c(1,2,4,5), c(6,7,0,4))
mat3
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    4    5
# [2,]    6    7    0    4
# 全ての値は同じ型になります。上記例は幸い、強制変換がされないものでした


# 2次元配列 (いろいろな型を含む場合)


# 異なる型の値を含む配列をつくりたい場合、データフレームを使ってください
# データフレームは、統計処理を行うプログラムをする際にとても便利です
# Pythonでも、 "pandas"というパッケージにて、似たものが利用可能です


students <- data.frame(c("Cedric","Fred","George","Cho","Draco","Ginny"),
                       c(3,2,2,1,0,-1),
                       c("H", "G", "G", "R", "S", "G"))
names(students) <- c("name", "year", "house") #カラム名
class(students)        # "data.frame"
students
# =>
#     name year house
# 1 Cedric    3     H
# 2   Fred    2     G
# 3 George    2     G
# 4    Cho    1     R
# 5  Draco    0     S
# 6  Ginny   -1     G
class(students$year)        # "numeric"
class(students[,3])        # "factor"
# 行と列の数をみます
nrow(students)        # 6
ncol(students)        # 3
dim(students)        # 6 3
# このdata.frame() 関数は、デフォルトでは文字列ベクターを因子ベクターに変換します
# stringsAsFactors = FALSE に設定してからデータフレームを作成すると、変換されません
?data.frame


# データフレームの1部を取り出すには、いろいろな（変な）、似たような方法があります
students$year        # 3  2  2  1  0 -1
students[,2]        # 3  2  2  1  0 -1
students[,"year"]        # 3  2  2  1  0 -1


# データフレームの拡張版が、データテーブルです。
# 大きなデータやパネルデータ、データセットの結合が必要な場合には、データテーブルを使うべきです。
# 以下に駆け足で説明します
install.packages("data.table") # CRANからパッケージをダウンロードします
require(data.table) # ロードします
students <- as.data.table(students)
students # 若干異なる出力がされることに注意
# =>
#      name year house
# 1: Cedric    3     H
# 2:   Fred    2     G
# 3: George    2     G
# 4:    Cho    1     R
# 5:  Draco    0     S
# 6:  Ginny   -1     G
students[name=="Ginny"] # name == "Ginny"の行を取り出します
# =>
#     name year house
# 1: Ginny   -1     G
students[year==2] # year == 2の行を取り出します
# =>
#      name year house
# 1:   Fred    2     G
# 2: George    2     G
# データテーブルは、二つのデータセットを結合するのにも便利です
# 結合用に、生徒データが入った別のデータテーブルをつくります
founders <- data.table(house=c("G","H","R","S"),
                       founder=c("Godric","Helga","Rowena","Salazar"))
founders
# =>
#    house founder
# 1:     G  Godric
# 2:     H   Helga
# 3:     R  Rowena
# 4:     S Salazar
setkey(students, house)
setkey(founders, house)
students <- founders[students] # 二つのデータテーブルを、"house"をキーとして結合します
setnames(students, c("house","houseFounderName","studentName","year"))
students[,order(c("name","year","house","houseFounderName")), with=F]
# =>
#    studentName year house houseFounderName
# 1:        Fred    2     G           Godric
# 2:      George    2     G           Godric
# 3:       Ginny   -1     G           Godric
# 4:      Cedric    3     H            Helga
# 5:         Cho    1     R           Rowena
# 6:       Draco    0     S          Salazar


# データテーブルは、要約を作るのも簡単です
students[,sum(year),by=house]
# =>
#    house V1
# 1:     G  3
# 2:     H  3
# 3:     R  1
# 4:     S  0


# データフレームやデータテーブルから列を消したい場合は、NULL値を代入します
students$houseFounderName <- NULL
students
# =>
#    studentName year house
# 1:        Fred    2     G
# 2:      George    2     G
# 3:       Ginny   -1     G
# 4:      Cedric    3     H
# 5:         Cho    1     R
# 6:       Draco    0     S


# データテーブルから行を消す場合は、以下のように除く行を指定すればできます
students[studentName != "Draco"]
# =>
#    house studentName year
# 1:     G        Fred    2
# 2:     G      George    2
# 3:     G       Ginny   -1
# 4:     H      Cedric    3
# 5:     R         Cho    1
# データフレームの場合も同様です
students <- as.data.frame(students)
students[students$house != "G",]
# =>
#   house houseFounderName studentName year
# 4     H            Helga      Cedric    3
# 5     R           Rowena         Cho    1
# 6     S          Salazar       Draco    0


# 多次元 (すべての値が同じ型の場合)


# 配列を並べて、N次元の表を作ります
# 配列なので、すべての値は同じ型にする必要があります
# ちなみに、以下のようにすれば2次元配列・2次元表も作成可能です
array(c(c(1,2,4,5),c(8,9,3,6)), dim=c(2,4))
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    4    8    3
# [2,]    2    5    9    6
# 2次元配列を並べて、3次元配列を作ることもできます
array(c(c(c(2,300,4),c(8,9,0)),c(c(5,60,0),c(66,7,847))), dim=c(3,2,2))
# =>
# , , 1
#
#      [,1] [,2]
# [1,]    2    8
# [2,]  300    9
# [3,]    4    0
#
# , , 2
#
#      [,1] [,2]
# [1,]    5   66
# [2,]   60    7
# [3,]    0  847


# リスト（多次元、不完全または複数の型が使われているもの)


# ついにRのリストです
list1 <- list(time = 1:40)
list1$price = c(rnorm(40,.5*list1$time,4)) # random
list1
# リストの要素は以下のようにして取得できます
list1$time # ある方法
list1[["time"]] # 別の方法
list1[[1]] # また別の方法
# =>
#  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33
# [34] 34 35 36 37 38 39 40
# 他のベクターと同じく、1部を取り出すことができます
list1$price[4]


# リストは、Rで1番効率的なデータ型ではありません
# 特別な理由がない限りは、リストの代わりにデータフレームを使うべきです
# リストは、線形回帰関数の返値として、しばしば使われています


##################################################
# apply() 系の関数
##################################################


# matは覚えていますよね？
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# apply(X, MARGIN, FUN) は、行列Xの行（MARGIN=1で指定)または列（MARGIN=2で指定)に対して、関数FUNを実行します
# Rで、このように指定してXの全行または全列に関数を実行するのは、forやwhileループを使うよりも、遥かに速いです
apply(mat, MAR = 2, jiggle)
# =>
#      [,1] [,2]
# [1,]    3   15
# [2,]    7   19
# [3,]   11   23
# 他にも便利な関数があります。?lapply, ?sapply で確認してみてください


# apply()系関数の使い方は、ちょっとややこしいです（みんなそう思ってます）。なので、あまり怖がりすぎないでください


# plyr パッケージは、*apply() 系の関数を置き換えて（さらに改善して）いこうとしています
install.packages("plyr")
require(plyr)
?plyr






#########################
# データロード
#########################


# "pets.csv"は、インターネット上に置いてあるファイルです
# (しかし、自分のPCにあるのと同じぐらい簡単に扱う事ができます)
pets <- read.csv("http://learnxinyminutes.com/docs/pets.csv")
pets
head(pets, 2) # 最初の2行
tail(pets, 1) # 最後の行


# データフレームか行列をcsvファイルとして保存します
write.csv(pets, "pets2.csv") # 新しくcsvファイルを作ります
# ワーキングディレクトリを、setwd()で設定します。　ワーキングディレクトリは getwd()で確認可能です


# ?read.csv や ?write.csv を入力すると、よりたくさんの情報を確認できます






#########################
# プロット
#########################


# Rに組込まれているプロット関数をつかいます
# 散布図!
plot(list1$time, list1$price, main = "fake data")
# 回帰図!
linearModel <- lm(price  ~ time, data = list1)
linearModel # outputs result of regression
# 回帰直線を既存の図上に引きます
abline(linearModel, col = "red")
# いろいろな散布図をつくって、確認できます
plot(linearModel)
# ヒストグラム！
hist(rpois(n = 10000, lambda = 5), col = "thistle")
# 棒グラフ！
barplot(c(1,4,5,1,2), names.arg = c("red","blue","purple","green","yellow"))


# GGPLOT2
# 上記の組込み関数を使うよりも、もっときれいな図を描くこともできます
# ggplot2 パッケージを使って、より多くのよい図を描いてみましょう
install.packages("ggplot2")
require(ggplot2)
?ggplot2
pp <- ggplot(students, aes(x=house))
pp + geom_histogram()
ll <- as.data.table(list1)
pp <- ggplot(ll, aes(x=time,price))
pp + geom_point()
# ggplot2 には、素晴らしい関連ドキュメントがそろっています (http://docs.ggplot2.org/current/)






```


## Rの入手方法


* RとR GUIはこちら [http://www.r-project.org/](http://www.r-project.org/)
* [RStudio](http://www.rstudio.com/ide/) 別のGUI
