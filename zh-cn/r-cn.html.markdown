---
language: R
contributors:
    - ["e99n09", "http://github.com/e99n09"]
    - ["isomorphismes", "http://twitter.com/isomorphisms"]
translators:
    - ["小柒", "http://weibo.com/u/2328126220"]
    - ["alswl", "https://github.com/alswl"]
filename: learnr-zh.r
lang: zh-cn
---

R 是一门统计语言。它有很多数据分析和挖掘程序包。可以用来统计、分析和制图。
你也可以在 LaTeX 文档中运行 `R` 命令。

```r
# 评论以 # 开始

# R 语言原生不支持 多行注释
# 但是你可以像这样来多行注释

# 在窗口里按回车键可以执行一条命令


###################################################################
# 不用懂编程就可以开始动手了
###################################################################

data()	# 浏览内建的数据集
data(rivers)	# 北美主要河流的长度（数据集）
ls()	# 在工作空间中查看「河流」是否出现
head(rivers)	# 撇一眼数据集
# 735 320 325 392 524 450
length(rivers)	# 我们测量了多少条河流？
# 141
summary(rivers)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  135.0   310.0   425.0   591.2   680.0  3710.0
stem(rivers)	# 茎叶图（一种类似于直方图的展现形式）
#
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


stem(log(rivers))	# 查看数据集的方式既不是标准形式，也不是取log后的结果! 看起来，是钟形曲线形式的基本数据集

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


hist(rivers, col="#333333", border="white", breaks=25)	# 试试用这些参数画画 （译者注：给 river 做统计频数直方图，包含了这些参数：数据源，颜色，边框，空格）
hist(log(rivers), col="#333333", border="white", breaks=25)	#你还可以做更多式样的绘图

# 还有其他一些简单的数据集可以被用来加载。R 语言包括了大量这种 data()
data(discoveries)
plot(discoveries, col="#333333", lwd=3, xlab="Year", main="Number of important discoveries per year")
# 译者注：参数为（数据源，颜色，线条宽度，X 轴名称，标题）
plot(discoveries, col="#333333", lwd=3, type = "h", xlab="Year", main="Number of important discoveries per year")


# 除了按照默认的年份排序，我们还可以排序来发现特征
sort(discoveries)
#  [1]  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1  1  1  1  1  2  2  2  2
# [26]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  3  3  3
# [51]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  4  4  4  4  4  4  4  4
# [76]  4  4  4  4  5  5  5  5  5  5  5  6  6  6  6  6  6  7  7  7  7  8  9 10 12

stem(discoveries, scale=2) # 译者注：茎叶图（数据，放大系数）
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




#基本的统计学操作也不需要任何编程知识

#随机生成数据
round(runif(7, min=.5, max=6.5))
# 译者注：runif 产生随机数，round 四舍五入
# 1 4 6 1 4 6 4

# 你输出的结果会和我们给出的不同，除非我们设置了相同的随机种子 random.seed(31337)


#从标准高斯函数中随机生成 9 次
rnorm(9)
# [1]  0.07528471  1.03499859  1.34809556 -0.82356087  0.61638975 -1.88757271
# [7] -0.59975593  0.57629164  1.08455362









#########################
# 基础编程
#########################

# 数值

#“数值”指的是双精度的浮点数
5	# 5
class(5)	# "numeric"
5e4	# 50000				# 用科学技术法方便的处理极大值、极小值或者可变的量级
6.02e23	# 阿伏伽德罗常数#
1.6e-35	# 布朗克长度

# 长整数并用 L 结尾
5L	# 5
#输出5L
class(5L)	# "integer"

# 可以自己试一试？用 class() 函数获取更多信息
# 事实上，你可以找一些文件查阅 `xyz` 以及xyz的差别
# `xyz` 用来查看源码实现，?xyz 用来看帮助

# 算法
10 + 66	# 76
53.2 - 4	# 49.2
2 * 2.0	# 4
3L / 4	# 0.75
3 %% 2	# 1

# 特殊数值类型
class(NaN)	# "numeric"
class(Inf)	# "numeric"
class(-Inf)	# "numeric"		# 在以下场景中会用到 integrate( dnorm(x), 3, Inf ) -- 消除 Z 轴数据

# 但要注意，NaN 并不是唯一的特殊数值类型……
class(NA)	# 看上面
class(NULL)	# NULL


# 简单列表
c(6, 8, 7, 5, 3, 0, 9)	# 6 8 7 5 3 0 9
c('alef', 'bet', 'gimmel', 'dalet', 'he')
c('Z', 'o', 'r', 'o') == "Zoro"	# FALSE FALSE FALSE FALSE

# 一些优雅的内置功能
5:15	# 5  6  7  8  9 10 11 12 13 14 15

seq(from=0, to=31337, by=1337)
#  [1]     0  1337  2674  4011  5348  6685  8022  9359 10696 12033 13370 14707
# [13] 16044 17381 18718 20055 21392 22729 24066 25403 26740 28077 29414 30751

letters
#  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z"

month.abb	# "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"


# Access the n'th element of a list with list.name[n] or sometimes list.name[[n]]
# 使用 list.name[n] 来访问第 n 个列表元素，有时候需要使用 list.name[[n]]
letters[18]	# "r"
LETTERS[13]	# "M"
month.name[9]	# "September"
c(6, 8, 7, 5, 3, 0, 9)[3]	# 7



# 字符串

# 字符串和字符在 R 语言中没有区别
"Horatio"	# "Horatio"
class("Horatio") # "character"
substr("Fortuna multis dat nimis, nulli satis.", 9, 15)	# "multis "
gsub('u', 'ø', "Fortuna multis dat nimis, nulli satis.")	# "Fortøna møltis dat nimis, nølli satis."



# 逻辑值

# 布尔值
class(TRUE)	# "logical"
class(FALSE)	# "logical"
# 和我们预想的一样
TRUE == TRUE	# TRUE
TRUE == FALSE	# FALSE
FALSE != FALSE	# FALSE
FALSE != TRUE	# TRUE
# 缺失数据（NA）也是逻辑值
class(NA)	# "logical"
#定义NA为逻辑型



# 因子
# 因子是为数据分类排序设计的（像是排序小朋友们的年级或性别）
levels(factor(c("female", "male", "male", "female", "NA", "female")))	# "female" "male"   "NA"

factor(c("female", "female", "male", "NA", "female"))
#  female female male   NA     female
# Levels: female male NA

data(infert)	# 自然以及引产导致的不育症
levels(infert$education)	# "0-5yrs"  "6-11yrs" "12+ yrs"



# 变量

# 有许多种方式用来赋值
x = 5 # 这样可以
y <- "1" # 更推荐这样
TRUE -> z # 这样可行，但是很怪

#我们还可以使用强制转型
as.numeric(y)	# 1
as.character(x)	# "5"

# 循环

# for 循环语句
for (i in 1:4) {
  print(i)
}

# while 循环
a <- 10
while (a > 4) {
	cat(a, "...", sep = "")
	a <- a - 1
}

# 记住，在 R 语言中 for / while 循环都很慢
# 建议使用 apply()（我们一会介绍）来操作一串数据（比如一列或者一行数据）

# IF/ELSE

# 再来看这些优雅的标准
if (4 > 3) {
	print("Huzzah! It worked!")
} else {
	print("Noooo! This is blatantly illogical!")
}

# =>
# [1] "Huzzah! It worked!"

# 函数

# 定义如下
jiggle <- function(x) {
	x = x + rnorm(1, sd=.1)	# 添加一点（正态）波动	
	return(x)
}

# 和其他 R 语言函数一样调用
jiggle(5)	# 5±ε. 使用 set.seed(2716057) 后， jiggle(5)==5.005043

#########################
# 数据容器：vectors, matrices, data frames, and arrays
#########################

# 单维度
# 你可以将目前我们学习到的任何类型矢量化，只要它们拥有相同的类型
vec <- c(8, 9, 10, 11)
vec	#  8  9 10 11
# 矢量的类型是这一组数据元素的类型
class(vec)	# "numeric"
# If you vectorize items of different classes, weird coercions happen
#如果你强制的将不同类型数值矢量化，会出现特殊值
c(TRUE, 4)	# 1 4
c("dog", TRUE, 4)	# "dog"  "TRUE" "4"

#我们这样来取内部数据，（R 的下标索引顺序 1 开始）
vec[1]	# 8
# 我们可以根据条件查找特定数据
which(vec %% 2 == 0)	# 1 3
# 抓取矢量中第一个和最后一个字符
head(vec, 1)	# 8
tail(vec, 1)	# 11
#如果下标溢出或不存会得到 NA
vec[6]	# NA
# 你可以使用 length() 获取矢量的长度
length(vec)	# 4

# 你可以直接操作矢量或者矢量的子集
vec * 4	# 16 20 24 28
vec[2:3] * 5	# 25 30
# 这里有许多内置的函数，来表现向量
mean(vec)	# 9.5
var(vec)	# 1.666667
sd(vec)	# 1.290994
max(vec)	# 11
min(vec)	# 8
sum(vec)	# 38

# 二维（相同元素类型）

#你可以为同样类型的变量建立矩阵
mat <- matrix(nrow = 3, ncol = 2, c(1,2,3,4,5,6))
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# 和 vector 不一样的是，一个矩阵的类型真的是 「matrix」，而不是内部元素的类型
class(mat) # => "matrix"
# 访问第一行的字符
mat[1,]	# 1 4
# 操作第一行数据
3 * mat[,1]	# 3 6 9
# 访问一个特定数据
mat[3,2]	# 6
# 转置整个矩阵（译者注：变成 2 行 3 列）
t(mat)
# =>
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6

# 使用 cbind() 函数把两个矩阵按列合并，形成新的矩阵
mat2 <- cbind(1:4, c("dog", "cat", "bird", "dog"))
mat2
# =>
#      [,1] [,2]
# [1,] "1"  "dog"
# [2,] "2"  "cat"
# [3,] "3"  "bird"
# [4,] "4"  "dog"
class(mat2)	# matrix
# Again, note what happened!
# 注意
# 因为矩阵内部元素必须包含同样的类型
# 所以现在每一个元素都转化成字符串
c(class(mat2[,1]), class(mat2[,2]))

# 按行合并两个向量，建立新的矩阵
mat3 <- rbind(c(1,2,4,5), c(6,7,0,4))
mat3
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    4    5
# [2,]    6    7    0    4
# 哈哈，数据类型都一样的，没有发生强制转换，生活真美好

# 二维(不同的元素类型)

# 利用 data frame 可以将不同类型数据放在一起
dat <- data.frame(c(5,2,1,4), c("dog", "cat", "bird", "dog"))
names(dat) <- c("number", "species") # 给数据列命名
class(dat)	# "data.frame"
dat
# =>
#   number species
# 1      5     dog
# 2      2     cat
# 3      1    bird
# 4      4     dog
class(dat$number)	# "numeric"
class(dat[,2])	# "factor"
# data.frame() 会将字符向量转换为 factor 向量

# 有很多精妙的方法来获取 data frame 的子数据集
dat$number	# 5 2 1 4
dat[,1]	# 5 2 1 4
dat[,"number"]	# 5 2 1 4

# 多维（相同元素类型）

# 使用 arry 创造一个 n 维的表格
# You can make a two-dimensional table (sort of like a matrix)
# 你可以建立一个 2 维表格（有点像矩阵）
array(c(c(1,2,4,5),c(8,9,3,6)), dim=c(2,4))
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    4    8    3
# [2,]    2    5    9    6
#你也可以利用数组建立一个三维的矩阵
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

#列表（多维的，不同类型的）

# R语言有列表的形式
list1 <- list(time = 1:40)
list1$price = c(rnorm(40,.5*list1$time,4)) # 随机
list1

# You can get items in the list like so
# 你可以这样获得列表的元素
list1$time
# You can subset list items like vectors
# 你也可以和矢量一样获取他们的子集
list1$price[4]

#########################
# apply()函数家族
#########################

# 还记得 mat 么？
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# Use apply(X, MARGIN, FUN) to apply function FUN to a matrix X
# 使用(X, MARGIN, FUN)将函数 FUN 应用到矩阵 X 的行 (MAR = 1) 或者 列 (MAR = 2)
# That is, R does FUN to each row (or column) of X, much faster than a
# R 在 X 的每一行/列使用 FUN，比循环要快很多
apply(mat, MAR = 2, myFunc)
# =>
#      [,1] [,2]
# [1,]    3   15
# [2,]    7   19
# [3,]   11   23
# 还有其他家族函数 ?lapply, ?sapply

# 不要被吓到，虽然许多人在此都被搞混
# plyr 程序包的作用是用来改进 apply() 函数家族

install.packages("plyr")
require(plyr)
?plyr

#########################
# 载入数据
#########################

# "pets.csv" 是网上的一个文本
pets <- read.csv("http://learnxinyminutes.com/docs/pets.csv")
pets
head(pets, 2) # 前两行
tail(pets, 1) # 最后一行

# 以 .csv 格式来保存数据集或者矩阵
write.csv(pets, "pets2.csv") # 保存到新的文件 pets2.csv
# set working directory with setwd(), look it up with getwd()
# 使用 setwd() 改变工作目录，使用 getwd() 查看当前工作目录

# 尝试使用 ?read.csv 和 ?write.csv 来查看更多信息

#########################
# 画图
#########################

# 散点图
plot(list1$time, list1$price, main = "fake data") # 译者注：横轴 list1$time，纵轴 wlist1$price，标题 fake data
# 回归图
linearModel <- lm(price  ~ time, data = list1) # 译者注：线性模型，数据集为list1，以价格对时间做相关分析模型
linearModel # 拟合结果
# 将拟合结果展示在图上，颜色设为红色
abline(linearModel, col = "red")
# 也可以获取各种各样漂亮的分析图
plot(linearModel)

# 直方图
hist(rpois(n = 10000, lambda = 5), col = "thistle") # 译者注：统计频数直方图

# 柱状图
barplot(c(1,4,5,1,2), names.arg = c("red","blue","purple","green","yellow"))

# 可以尝试着使用 ggplot2 程序包来美化图片
install.packages("ggplot2")
require(ggplot2)
?ggplot2

```

## 获得 R

* 从 [http://www.r-project.org/](http://www.r-project.org/) 获得安装包和图形化界面
* [RStudio](http://www.rstudio.com/ide/) 是另一个图形化界面
