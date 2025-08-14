---
name: R
contributors:
    - ["e99n09", "http://github.com/e99n09"]
    - ["isomorphismes", "http://twitter.com/isomorphisms"]
    - ["kalinn", "http://github.com/kalinn"]
    - ["mribeirodantas", "http://github.com/mribeirodantas"]
filename: learnr.r
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

R은 통계 컴퓨팅 언어입니다. 데이터 세트를 업로드하고 정리하고, 통계 절차를 실행하고, 그래프를 만드는 데 많은 라이브러리가 있습니다. LaTeX 문서 내에서 `R` 명령을 실행할 수도 있습니다.

```r
# 주석은 해시 기호, 즉 숫자 기호(#)로 시작합니다.

# 여러 줄 주석을 만들 수는 없지만,
# 다음과 같이 여러 주석을 쌓을 수 있습니다.

# Windows에서는 CTRL-ENTER를 사용하여 한 줄을 실행할 수 있습니다.
# Mac에서는 COMMAND-ENTER입니다.



#############################################################################
# 프로그래밍에 대해 아무것도 이해하지 않고도 할 수 있는 것들
#############################################################################

# 이 섹션에서는 프로그래밍에 대해 아무것도 이해하지 않고도
# R에서 할 수 있는 멋진 것들을 보여줍니다. 코드가
# 하는 모든 것을 이해하려고 걱정하지 마십시오. 그냥 즐기세요!

data()          # 미리 로드된 데이터 세트 찾아보기
data(rivers)    # 이것을 가져옵니다: "주요 북미 강의 길이"
ls()            # 이제 작업 공간에 "rivers"가 나타나는 것을 확인하십시오
head(rivers)    # 데이터 세트 살짝 보기
# 735 320 325 392 524 450

length(rivers)  # 몇 개의 강이 측정되었나요?
# 141
summary(rivers) # 몇 가지 요약 통계는 무엇인가요?
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  135.0   310.0   425.0   591.2   680.0  3710.0

# 줄기-잎 그림 만들기 (히스토그램과 유사한 데이터 시각화)
stem(rivers)

#  소수점은 |의 오른쪽으로 2자리입니다.
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

stem(log(rivers)) # 데이터가 정규 분포도 아니고 로그-정규 분포도 아님을 주목하십시오!
# 벨 곡선 근본주의자들아, 받아라.

#  소수점은 |의 왼쪽으로 1자리입니다.
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

# 히스토그램 만들기:
hist(rivers, col = "#333333", border = "white", breaks = 25)
hist(log(rivers), col = "#333333", border = "white", breaks = 25)
# 이 매개변수들을 가지고 놀아보세요. 나중에 더 많은 플로팅을 할 것입니다.

# 미리 로드된 또 다른 멋진 데이터 세트입니다. R에는 이런 것들이 많이 있습니다.
data(discoveries)
plot(discoveries, col = "#333333", lwd = 3, xlab = "Year",
     main="Number of important discoveries per year")
plot(discoveries, col = "#333333", lwd = 3, type = "h", xlab = "Year",
     main="Number of important discoveries per year")

# 기본 순서(연도별)를 그대로 두는 대신,
# 정렬하여 일반적인 것을 볼 수도 있습니다:
sort(discoveries)
#  [1]  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1  1  1  1  1  2  2  2  2
# [26]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  3  3  3
# [51]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  4  4  4  4  4  4  4  4
# [76]  4  4  4  4  5  5  5  5  5  5  5  6  6  6  6  6  6  7  7  7  7  8  9 10 12

stem(discoveries, scale = 2)
#
#  소수점은 |에 있습니다.
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

# 주사위를 몇 번 굴리기
round(runif(7, min = .5, max = 6.5))
# 1 4 6 1 4 6 4
# random.seed(31337)을 동일하게 설정하지 않으면 제 숫자와 다를 것입니다.

# 표준 가우시안에서 9번 추출
rnorm(9)
# [1]  0.07528471  1.03499859  1.34809556 -0.82356087  0.61638975 -1.88757271
# [7] -0.59975593  0.57629164  1.08455362



##################################################
# 데이터 유형 및 기본 산술
##################################################

# 이제 프로그래밍 지향적인 튜토리얼 부분입니다.
# 이 섹션에서는 R의 중요한 데이터 유형을 만날 것입니다:
# 정수, 숫자, 문자, 논리 및 요인.
# 다른 것들도 있지만, 이것들은 시작하는 데 필요한
# 최소한의 것입니다.

# 정수
# 긴 저장 정수는 L로 작성됩니다.
5L          # 5
class(5L)   # "integer"
# (class() 함수에 대한 자세한 내용은 ?class를 시도하십시오.)
# R에서는 5L과 같은 모든 단일 값이 길이 1의 벡터로 간주됩니다.
length(5L)  # 1
# 길이가 1보다 큰 정수 벡터도 가질 수 있습니다:
c(4L, 5L, 8L, 3L)          # 4 5 8 3
length(c(4L, 5L, 8L, 3L))  # 4
class(c(4L, 5L, 8L, 3L))   # "integer"

# 숫자
# "숫자"는 배정밀도 부동 소수점 숫자입니다.
5           # 5
class(5)    # "numeric"
# 다시 말하지만, R의 모든 것은 벡터입니다.
# 하나 이상의 요소가 있는 숫자 벡터를 만들 수 있습니다.
c(3, 3, 3, 2, 2, 1) # 3 3 3 2 2 1
# 과학적 표기법도 사용할 수 있습니다.
5e4         # 50000
6.02e23     # 아보가드로 수
1.6e-35     # 플랑크 길이
# 무한히 크거나 작은 숫자도 가질 수 있습니다.
class(Inf)  # "numeric"
class(-Inf) # "numeric"
# 예를 들어 integrate(dnorm, 3, Inf)에서 "Inf"를 사용할 수 있습니다.
# 이것은 Z-점수 표를 불필요하게 만듭니다.

# 기본 산술
# 숫자로 산술을 할 수 있습니다.
# 정수와 숫자를 혼합하여 산술을 하면 다른 숫자가 됩니다.
10L + 66L   # 76    # 정수 더하기 정수는 정수
53.2 - 4    # 49.2  # 숫자 빼기 숫자는 숫자
2.0 * 2L    # 4     # 숫자 곱하기 정수는 숫자
3L / 4      # 0.75  # 정수 나누기 숫자는 숫자
3 %% 2      # 1     # 두 숫자의 나머지는 다른 숫자
# 불법적인 산술은 "숫자가 아님"을 산출합니다:
0 / 0       # NaN
class(NaN)  # "numeric"
# 길이가 1보다 큰 두 벡터에 대해 산술을 할 수 있습니다.
# 더 큰 벡터의 길이가 더 작은 벡터의 정수배인 한
c(1, 2, 3) + c(1, 2, 3)     # 2 4 6
# 단일 숫자는 길이 1의 벡터이므로 스칼라는
# 벡터에 요소별로 적용됩니다.
(4 * c(1, 2, 3) - 2) / 2    # 1 3 5
# 스칼라를 제외하고 길이가 다른 벡터에 대해 산술을 수행할 때는
# 주의하십시오. 가능하지만,
c(1, 2, 3, 1, 2, 3) * c(1, 2)               # 1 4 3 2 2 6
# 길이를 맞추는 것이 대부분의 경우 더 나은 관행이며 읽기 쉽습니다.
c(1, 2, 3, 1, 2, 3) * c(1, 2, 1, 2, 1, 2)   # 1 4 3 2 2 6

# 문자
# R에서는 문자열과 문자 사이에 차이가 없습니다.
"Horatio"           # "Horatio"
class("Horatio")    # "character"
class("H")          # "character"
# 이것들은 모두 길이 1의 문자 벡터였습니다.
# 더 긴 것입니다:
c("alef", "bet", "gimmel", "dalet", "he")
# => "alef"   "bet"    "gimmel" "dalet"  "he"
length(c("Call","me","Ishmael")) # 3
# 문자 벡터에 대해 정규식 연산을 할 수 있습니다:
substr("Fortuna multis dat nimis, nulli satis.", 9, 15)  # "multis "
gsub('u', 'ø', "Fortuna multis dat nimis, nulli satis.") # "Fortøna møltis dat nimis, nølli satis."
# R에는 여러 내장 문자 벡터가 있습니다:
letters
# =>
#  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z"
month.abb # "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

# 논리
# R에서 "논리"는 부울입니다.

class(TRUE)     # "logical"
class(FALSE)    # "logical"
# 그들의 행동은 정상입니다.
TRUE == TRUE    # TRUE
TRUE == FALSE   # FALSE
FALSE != FALSE  # FALSE
FALSE != TRUE   # TRUE
# 누락된 데이터(NA)도 논리적입니다.
class(NA)       # "logical"
# 논리 연산에는 |와 &를 사용하십시오.
# OR
TRUE | FALSE    # TRUE
# AND
TRUE & FALSE    # FALSE
# 벡터에 |와 &를 적용하면 요소별 논리 연산이 반환됩니다.
c(TRUE, FALSE, FALSE) | c(FALSE, TRUE, FALSE)   # TRUE TRUE FALSE
c(TRUE, FALSE, TRUE) & c(FALSE, TRUE, TRUE)     # FALSE FALSE TRUE
# x가 TRUE인지 테스트할 수 있습니다.
isTRUE(TRUE)    # TRUE
# 여기서는 많은 요소가 있는 논리 벡터를 얻습니다:
c("Z", "o", "r", "r", "o") == "Zorro"   # FALSE FALSE FALSE FALSE FALSE
c("Z", "o", "r", "r", "o") == "Z"       # TRUE FALSE FALSE FALSE FALSE

# 요인
# 요인 클래스는 범주형 데이터용입니다.
# 요인은 순서가 있거나(학년과 같이) 순서가 없을 수 있습니다(색상과 같이).
factor(c("blue", "blue", "green", NA, "blue"))
#  blue blue green   <NA>   blue
# Levels: blue green
# "수준"은 범주형 데이터가 가질 수 있는 값입니다.
# 누락된 데이터는 수준에 들어가지 않습니다.
levels(factor(c("green", "green", "blue", NA, "blue"))) # "blue" "green"
# 요인 벡터의 길이가 1이면 수준의 길이도 1입니다.
length(factor("green"))         # 1
length(levels(factor("green"))) # 1
# 요인은 데이터 프레임에서 흔히 볼 수 있으며, 나중에 다룰 데이터 구조입니다.
data(infert)             # "자연 및 유도 유산 후 불임"
levels(infert$education) # "0-5yrs"  "6-11yrs" "12+ yrs"

# NULL
# "NULL"은 이상한 것입니다. 벡터를 "비우는" 데 사용하십시오.
class(NULL) # NULL
parakeet = c("beak", "feathers", "wings", "eyes")
parakeet # "beak"     "feathers" "wings"    "eyes"
parakeet <- NULL
parakeet # NULL

# 유형 강제 변환
# 유형 강제 변환은 값을 다른 유형으로 강제로 변환하는 것입니다.
as.character(c(6, 8))   # "6" "8"
as.logical(c(1,0,1,1))  # TRUE FALSE  TRUE  TRUE
# 다른 유형의 요소를 벡터에 넣으면 이상한 강제 변환이 발생합니다:
c(TRUE, 4)          # 1 4
c("dog", TRUE, 4)   # "dog"  "TRUE" "4"
as.numeric("Bilbo")
# =>
# [1] NA
# Warning message:
# NAs introduced by coercion

# 또한 참고: 이것들은 단지 기본 데이터 유형이었습니다.
# 날짜, 시계열 등과 같은 더 많은 데이터 유형이 있습니다.



##################################################
# 변수, 루프, if/else
##################################################

# 변수는 나중에 사용하기 위해 값을 저장하는 상자와 같습니다.
# 우리는 이것을 변수에 값을 "할당"한다고 합니다.
# 변수가 있으면 루프, 함수 및 if/else 문을 작성할 수 있습니다.

# 변수
# 할당하는 방법은 여러 가지가 있습니다:
x = 5       # 가능합니다
y <- "1"    # 전통적으로 선호됩니다
TRUE -> z   # 작동하지만 이상합니다
# 그들의 행동과 선호도에 대해서는 인터넷을 참조하십시오.

# 루프
# for 루프가 있습니다.
for (i in 1:4) {
	print(i)
}
# while 루프가 있습니다.
a <- 10
while (a > 4) {
	cat(a, "...", sep = "")
	a <- a - 1
}
# R에서는 for 및 while 루프가 느리게 실행된다는 점을 명심하십시오.
# 전체 벡터(즉, 전체 행, 전체 열)에 대한 연산
# 또는 apply() 유형 함수(나중에 논의할 것)가 선호됩니다.

# IF/ELSE
# 다시 말하지만, 꽤 표준적입니다.
if (4 > 3) {
	print("4 is greater than 3")
} else {
	print("4 is not greater than 3")
}
# =>
# [1] "4 is greater than 3"

# 함수
# 다음과 같이 정의됩니다:
jiggle <- function(x) {
	x = x + rnorm(1, sd=.1) # 약간의 (제어된) 노이즈 추가
	return(x)
}
# 다른 R 함수와 마찬가지로 호출됩니다:
jiggle(5)   # 5±ε. set.seed(2716057) 후, jiggle(5)==5.005043



###########################################################################
# 데이터 구조: 벡터, 행렬, 데이터 프레임 및 배열
###########################################################################

# 1차원

# 맨 처음부터, 그리고 이미 알고 있는 것부터 시작합시다: 벡터.
vec <- c(8, 9, 10, 11)
vec     #  8  9 10 11
# 대괄호로 부분 집합을 만들어 특정 요소를 요청합니다.
# (R은 1부터 계산을 시작합니다)
vec[1]          # 8
letters[18]     # "r"
LETTERS[13]     # "M"
month.name[9]   # "September"
c(6, 8, 7, 5, 3, 0, 9)[3] # 7
# 특정 구성 요소의 인덱스를 검색할 수도 있습니다.
which(vec %% 2 == 0) # 1 3
# 벡터의 처음 또는 마지막 몇 개 항목만 가져옵니다.
head(vec, 1)    # 8
tail(vec, 2)    # 10 11
# 또는 특정 값이 벡터에 있는지 확인합니다.
any(vec == 10)  # TRUE
# 인덱스가 "넘어가면" NA를 얻습니다:
vec[6]      # NA
# length()로 벡터의 길이를 찾을 수 있습니다.
length(vec) # 4
# 전체 벡터 또는 벡터의 부분 집합에 대해 연산을 수행할 수 있습니다.
vec * 4             # 32 36 40 44
vec[2:3] * 5        # 45 50
any(vec[2:3] == 8)  # FALSE
# 그리고 R에는 벡터를 요약하는 많은 내장 함수가 있습니다.
mean(vec)   # 9.5
var(vec)    # 1.666667
sd(vec)     # 1.290994
max(vec)    # 11
min(vec)    # 8
sum(vec)    # 38
# 더 멋진 내장 함수:
5:15        # 5  6  7  8  9 10 11 12 13 14 15
seq(from = 0, to = 31337, by = 1337)
# =>
#  [1]     0  1337  2674  4011  5348  6685  8022  9359 10696 12033 13370 14707
# [13] 16044 17381 18718 20055 21392 22729 24066 25403 26740 28077 29414 30751

# 2차원 (모두 한 클래스)

# 다음과 같이 동일한 유형의 항목으로 행렬을 만들 수 있습니다:
mat <- matrix(nrow = 3, ncol = 2, c(1, 2, 3, 4, 5, 6))
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# 벡터와 달리 행렬의 클래스는 내용에 관계없이 "matrix"입니다.
class(mat)      # "matrix" "array"
# 첫 번째 행 요청
mat[1, ]        # 1 4
# 첫 번째 열에 대한 연산 수행
3 * mat[, 1]    # 3 6 9
# 특정 셀 요청
mat[3, 2]       # 6

# 전체 행렬 전치
t(mat)
# =>
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6

# 행렬 곱셈
mat %*% t(mat)
# =>
#      [,1] [,2] [,3]
# [1,]   17   22   27
# [2,]   22   29   36
# [3,]   27   36   45

# cbind()는 벡터를 열 단위로 붙여 행렬을 만듭니다.
mat2 <- cbind(1:4, c("dog", "cat", "bird", "dog"))
mat2
# =>
#      [,1] [,2]
# [1,] "1"  "dog"
# [2,] "2"  "cat"
# [3,] "3"  "bird"
# [4,] "4"  "dog"
class(mat2) # matrix
# 다시 말하지만, 무슨 일이 일어났는지 주목하십시오!
# 행렬은 모두 동일한 클래스의 항목을 포함해야 하므로,
# 모든 것이 문자 클래스로 변환되었습니다.
c(class(mat2[, 1]), class(mat2[, 2]))

# rbind()는 벡터를 행 단위로 붙여 행렬을 만듭니다.
mat3 <- rbind(c(1, 2, 4, 5), c(6, 7, 0, 4))
mat3
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    4    5
# [2,]    6    7    0    4
# 아, 모두 같은 클래스입니다. 강제 변환이 없습니다. 훨씬 낫습니다.

# 2차원 (다른 클래스)

# 다른 유형의 열에는 데이터 프레임을 사용하십시오.
# 이 데이터 구조는 통계 프로그래밍에 매우 유용하며,
# "pandas" 패키지의 Python에 버전이 추가되었습니다.

students <- data.frame(c("Cedric", "Fred", "George", "Cho", "Draco", "Ginny"),
                       c(       3,      2,        2,     1,       0,      -1),
                       c(     "H",    "G",      "G",   "R",     "S",     "G"))
names(students) <- c("name", "year", "house") # 열 이름 지정
class(students) # "data.frame"
students
# =>
#     name year house
# 1 Cedric    3     H
# 2   Fred    2     G
# 3 George    2     G
# 4    Cho    1     R
# 5  Draco    0     S
# 6  Ginny   -1     G
class(students$year)    # "numeric"
class(students[,3])     # "factor"
# 차원 찾기
nrow(students)  # 6
ncol(students)  # 3
dim(students)   # 6 3
# data.frame() 함수는 기본적으로 문자 벡터를 요인 벡터로
# 변환하는 데 사용되었습니다. 이것은 R 4.0.0에서 변경되었습니다.
# R 버전이 더 오래된 경우 data.frame을 만들 때
# stringsAsFactors = FALSE를 설정하여 이 기능을 끄십시오.
?data.frame

# 미묘하게 다른 데이터 프레임을 부분 집합으로 만드는 여러 가지 방법이 있습니다.
students$year       # 3  2  2  1  0 -1
students[, 2]       # 3  2  2  1  0 -1
students[, "year"]  # 3  2  2  1  0 -1

# data.frame 구조의 증강 버전은 data.table입니다.
# 거대하거나 패널 데이터를 다루거나 몇 개의 데이터 세트를 병합해야 하는 경우
# data.table이 좋은 선택이 될 수 있습니다. 다음은 간략한 둘러보기입니다:
install.packages("data.table") # CRAN에서 패키지 다운로드
require(data.table) # 로드
students <- as.data.table(students)
students # 약간 다른 출력을 주목하십시오
# =>
#      name year house
# 1: Cedric    3     H
# 2:   Fred    2     G
# 3: George    2     G
# 4:    Cho    1     R
# 5:  Draco    0     S
# 6:  Ginny   -1     G
students[name == "Ginny"] # 이름이 "Ginny"인 행 가져오기
# =>
#     name year house
# 1: Ginny   -1     G
students[year == 2] # 연도가 2인 행 가져오기
# =>
#      name year house
# 1:   Fred    2     G
# 2: George    2     G
# data.table은 두 데이터 세트를 병합하기 쉽습니다.
# 학생들과 병합할 다른 data.table을 만들어 봅시다.
founders <- data.table(house   = c("G"     , "H"    , "R"     , "S"),
                       founder = c("Godric", "Helga", "Rowena", "Salazar"))
founders
# =>
#    house founder
# 1:     G  Godric
# 2:     H   Helga
# 3:     R  Rowena
# 4:     S Salazar
setkey(students, house)
setkey(founders, house)
students <- founders[students] # "house"를 일치시켜 두 데이터 세트 병합
setnames(students, c("house", "houseFounderName", "studentName", "year"))
students[, order(c("name", "year", "house", "houseFounderName")), with = F]
# =>
#    studentName year house houseFounderName
# 1:        Fred    2     G           Godric
# 2:      George    2     G           Godric
# 3:       Ginny   -1     G           Godric
# 4:      Cedric    3     H            Helga
# 5:         Cho    1     R           Rowena
# 6:       Draco    0     S          Salazar

# data.table은 요약 테이블을 쉽게 만듭니다.
students[, sum(year), by = house]
# =>
#    house V1
# 1:     G  3
# 2:     H  3
# 3:     R  1
# 4:     S  0

# data.frame 또는 data.table에서 열을 삭제하려면
# NULL 값을 할당하십시오.
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

# 부분 집합으로 행 삭제
# data.table 사용:
students[studentName != "Draco"]
# =>
#    house studentName year
# 1:     G        Fred    2
# 2:     G      George    2
# 3:     G       Ginny   -1
# 4:     H      Cedric    3
# 5:     R         Cho    1
# data.frame 사용:
students <- as.data.frame(students)
students[students$house != "G", ]
# =>
#   house houseFounderName studentName year
# 4     H            Helga      Cedric    3
# 5     R           Rowena         Cho    1
# 6     S          Salazar       Draco    0

# 다차원 (모든 요소가 한 유형)

# 배열은 n차원 테이블을 만듭니다.
# 모든 요소는 동일한 유형이어야 합니다.
# 2차원 테이블(행렬과 유사)을 만들 수 있습니다.
array(c(c(1, 2, 4, 5), c(8, 9, 3, 6)), dim = c(2, 4))
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    4    8    3
# [2,]    2    5    9    6
# 배열을 사용하여 3차원 행렬도 만들 수 있습니다.
array(c(c(c(2, 300, 4), c(8, 9, 0)), c(c(5, 60, 0), c(66, 7, 847))), dim = c(3, 2, 2))
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

# 리스트 (다차원, 들쭉날쭉할 수 있음, 다른 유형)

# 마지막으로 R에는 리스트(벡터의)가 있습니다.
list1 <- list(time = 1:40)
list1$price = c(rnorm(40, .5*list1$time, 4)) # 무작위
list1
# 다음과 같이 리스트에서 항목을 가져올 수 있습니다.
list1$time # 한 가지 방법
list1[["time"]] # 다른 방법
list1[[1]] # 또 다른 방법
# =>
#  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33
# [34] 34 35 36 37 38 39 40
# 다른 벡터와 마찬가지로 리스트 항목을 부분 집합으로 만들 수 있습니다.
list1$price[4]

# 리스트는 R에서 작업하기에 가장 효율적인 데이터 구조가 아닙니다.
# 아주 좋은 이유가 없다면 data.frames를 사용하는 것이 좋습니다.
# 리스트는 종종 선형 회귀를 수행하는 함수에서 반환됩니다.

##################################################
# apply() 계열 함수
##################################################

# mat를 기억하시나요?
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# apply(X, MARGIN, FUN)을 사용하여 행(MAR = 1) 또는 열(MAR = 2)에 대해
# 함수 FUN을 행렬 X에 적용합니다.
# 즉, R은 X의 각 행(또는 열)에 대해 FUN을 수행하며,
# for 또는 while 루프보다 훨씬 빠릅니다.
apply(mat, MAR = 2, jiggle)
# =>
#      [,1] [,2]
# [1,]    3   15
# [2,]    7   19
# [3,]   11   23
# 다른 함수: ?lapply, ?sapply

# 너무 겁먹지 마세요. 모두가 그것들이 다소 혼란스럽다는 데 동의합니다.

# plyr 패키지는 *apply() 계열을 대체(및 개선!)하는 것을 목표로 합니다.
install.packages("plyr")
require(plyr)
?plyr



#########################
# 데이터 로드
#########################

# "pets.csv"는 인터넷에 있는 파일입니다.
# (하지만 컴퓨터에 있는 파일일 수도 있습니다)
require(RCurl)
pets <- read.csv(textConnection(getURL("https://learnxinyminutes.com/pets.csv")))
pets
head(pets, 2) # 처음 두 행
tail(pets, 1) # 마지막 행

# 데이터 프레임 또는 행렬을 .csv 파일로 저장하려면
write.csv(pets, "pets2.csv") # 새 .csv 파일 만들기
# setwd()로 작업 디렉토리 설정, getwd()로 조회

# 자세한 내용은 ?read.csv 및 ?write.csv를 참조하십시오.



#########################
# 통계 분석
#########################

# 선형 회귀!
linearModel <- lm(price ~ time, data = list1)
linearModel # 회귀 결과 출력
# =>
# Call:
# lm(formula = price ~ time, data = list1)
#
# Coefficients:
# (Intercept)         time
#      0.1453       0.4943
summary(linearModel) # 회귀에서 더 자세한 출력
# =>
# Call:
# lm(formula = price ~ time, data = list1)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -8.3134 -3.0131 -0.3606  2.8016 10.3992
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.14527    1.50084   0.097    0.923
# time         0.49435    0.06379   7.749 2.44e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 4.657 on 38 degrees of freedom
# Multiple R-squared:  0.6124,	Adjusted R-squared:  0.6022
# F-statistic: 60.05 on 1 and 38 DF,  p-value: 2.44e-09
coef(linearModel) # 추정된 매개변수 추출
# =>
# (Intercept)        time
#   0.1452662   0.4943490
summary(linearModel)$coefficients # 결과를 추출하는 다른 방법
# =>
#              Estimate Std. Error    t value     Pr(>|t|)
# (Intercept) 0.1452662 1.50084246 0.09678975 9.234021e-01
# time        0.4943490 0.06379348 7.74920901 2.440008e-09
summary(linearModel)$coefficients[, 4] # p-값
# =>
#  (Intercept)         time
# 9.234021e-01 2.440008e-09

# 일반화 선형 모델
# 로지스틱 회귀
set.seed(1)
list1$success = rbinom(length(list1$time), 1, .5) # 무작위 이진
glModel <- glm(success  ~ time, data = list1, family=binomial(link="logit"))
glModel # 로지스틱 회귀 결과 출력
# =>
# Call:  glm(formula = success ~ time,
#	family = binomial(link = "logit"), data = list1)
#
# Coefficients:
# (Intercept)         time
#     0.17018     -0.01321
#
# Degrees of Freedom: 39 Total (i.e. Null);  38 Residual
# Null Deviance:	    55.35
# Residual Deviance: 55.12 	 AIC: 59.12
summary(glModel) # 회귀에서 더 자세한 출력
# =>
# Call:
# glm(
#	formula = success ~ time,
#	family = binomial(link = "logit"),
#	data = list1)

# Deviance Residuals:
#    Min      1Q  Median      3Q     Max
# -1.245  -1.118  -1.035   1.202   1.327
#
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  0.17018    0.64621   0.263    0.792
# time        -0.01321    0.02757  -0.479    0.632
#
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 55.352  on 39  degrees of freedom
# Residual deviance: 55.121  on 38  degrees of freedom
# AIC: 59.121
#
# Number of Fisher Scoring iterations: 3


#########################
# 플롯
#########################

# 내장 플로팅 함수
# 산점도!
plot(list1$time, list1$price, main = "fake data")
# 기존 플롯에 회귀선 플로팅
abline(linearModel, col = "red")
# 다양한 멋진 진단 얻기
plot(linearModel)
# 히스토그램!
hist(rpois(n = 10000, lambda = 5), col = "thistle")
# 막대 그래프!
barplot(c(1, 4, 5, 1, 2), names.arg = c("red", "blue", "purple", "green", "yellow"))

# GGPLOT2
# 하지만 이것들은 R의 가장 예쁜 플롯조차 아닙니다.
# 더 많고 더 나은 그래픽을 위해 ggplot2 패키지를 사용해 보십시오.
install.packages("ggplot2")
require(ggplot2)
?ggplot2
pp <- ggplot(students, aes(x = house))
pp + geom_bar()
ll <- as.data.table(list1)
pp <- ggplot(ll, aes(x = time, price))
pp + geom_point()
# ggplot2에는 훌륭한 문서가 있습니다 (http://docs.ggplot2.org/current/에서 사용 가능)
```

## R을 어떻게 얻나요?

* R 및 R GUI는 [http://www.r-project.org/](http://www.r-project.org/)에서 얻을 수 있습니다.
* [RStudio](http://www.rstudio.com/ide/)는 또 다른 GUI입니다.
