---
language: R
contributors:
    - ["e99n09", "http://github.com/e99n09"]
    - ["isomorphismes", "http://twitter.com/isomorphisms"]
    - ["kalinn", "http://github.com/kalinn"]
translators:
    - ["Marcel Ribeiro-Dantas", "http://github.com/mribeirodantas"]
lang: pt-br    
filename: learnr-pt.r
---

R é uma linguagem de programação estatística. Ela tem muitas bibliotecas para carregar e limpar conjuntos de dados, executar análises estatísticas e produzir gráficos. Você também pode executar comandos do `R` dentro de um documento LaTeX.

```r
# Comentários começam com o símbolo de Cerquilha, também conhecido como
# jogo da velha

# Não existe um símbolo especial para comentários em várias linhas
# mas você pode escrever várias linhas de comentários adicionando a
# cerquilha (#) ao início de cada uma delas.

# No Windows e Linux, você pode usar CTRL-ENTER para executar uma linha.
# No MacOS, o equivalente é COMMAND-ENTER



#############################################################################
# Coisas que você pode fazer sem entender nada sobre programação
#############################################################################

# Nesta seção, mostramos algumas das coisas legais que você pode fazer em
# R sem entender nada de programação. Não se preocupe em entender tudo o
# que o código faz. Apenas aproveite!

data()	          # navegue pelos conjuntos de dados pré-carregados
data(rivers)	  # carregue este: "Comprimentos dos principais rios norte-americanos"
ls()	          # observe que "rivers" apareceu na área de trabalho (workspace)
head(rivers)	  # dê uma espiada no conjunto de dados
# 735 320 325 392 524 450

length(rivers)	  # quantos rios foram medidos?
# 141
summary(rivers)   # consulte um sumário de estatísticas básicas
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  135.0   310.0   425.0   591.2   680.0  3710.0

# faça um diagrama de ramos e folhas (uma visualização de dados semelhante a um histograma)
stem(rivers)

#  A vírgula está 2 dígito(s) à direita do símbolo |
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

stem(log(rivers)) # Observe que os dados não são normais nem log-normais!
# Tome isso, fundamentalistas da curva normal!

#  O ponto decimal está 1 dígito(s) à esquerda do símbolo |
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

# faça um histograma:
hist(rivers, col="#333333", border="white", breaks=25)      # brinque com estes parâmetros
hist(log(rivers), col="#333333", border="white", breaks=25) # você fará mais gráficos mais tarde

# Aqui está outro conjunto de dados que vem pré-carregado. O R tem toneladas deles.
data(discoveries)
plot(discoveries, col="#333333", lwd=3, xlab="Ano",
     main="Número de descobertas importantes por ano")
plot(discoveries, col="#333333", lwd=3, type = "h", xlab="Ano",
     main="Número de descobertas importantes por ano")

# Em vez de deixar a ordenação padrão (por ano),
# também podemos ordenar para ver o que é mais comum:
sort(discoveries)
#  [1]  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1  1  1  1  1  2  2  2  2
# [26]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  3  3  3
# [51]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  4  4  4  4  4  4  4  4
# [76]  4  4  4  4  5  5  5  5  5  5  5  6  6  6  6  6  6  7  7  7  7  8  9 10 12

stem(discoveries, scale=2)
#
#  O ponto decimal está no símbolo |
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

# Role um dado algumas vezes
round(runif(7, min=.5, max=6.5))
# 1 4 6 1 4 6 4
# Seus números serão diferentes dos meus, a menos que definamos a mesma semente aleatória com o set.seed

# Obtenha 9 números de forma aleatória a partir de uma distribuição normal
rnorm(9)
# [1]  0.07528471  1.03499859  1.34809556 -0.82356087  0.61638975 -1.88757271
# [7] -0.59975593  0.57629164  1.08455362



##################################################
# Tipos de dados e aritmética básica
##################################################

# Agora para a parte orientada a programação do tutorial.
# Nesta seção você conhecerá os tipos de dados importantes do R:
# integers, numerics, characters, logicals, e factors.
# Existem outros, mas estes são o mínimo que você precisa para
# iniciar.

# INTEGERS
# Os inteiros de armazenamento longo são escritos com L
5L # 5
class(5L) # "integer"
# (Experimente ?class para obter mais informações sobre a função class().)
# Em R, todo e qualquer valor, como 5L, é considerado um vetor de comprimento 1
length(5L) # 1
# Você pode ter um vetor inteiro com comprimento > 1 também:
c(4L, 5L, 8L, 3L) # 4 5 8 3
length(c(4L, 5L, 8L, 3L)) # 4
class(c(4L, 5L, 8L, 3L)) # "integer"

# NUMERICS
# Um "numeric" é um número de ponto flutuante de precisão dupla
5 # 5
class(5) # "numeric"
# De novo, tudo em R é um vetor;
# você pode fazer um vetor numérico com mais de um elemento
c(3,3,3,2,2,1) # 3 3 3 2 2 1
# Você também pode usar a notação científica
5e4 # 50000
6.02e23 # Número de Avogadro
1.6e-35 # Comprimento de Planck
# Você também pode ter números infinitamente grandes ou pequenos
class(Inf)	# "numeric"
class(-Inf)	# "numeric"
# Você pode usar "Inf", por exemplo, em integrate(dnorm, 3, Inf)
# isso evita as tabelas de escores-Z.

# ARITMÉTICA BÁSICA
# Você pode fazer aritmética com números
# Fazer aritmética com uma mistura de números inteiros (integers) e com
# ponto flutuante (numeric) resulta em um numeric
10L + 66L # 76      # integer mais integer resulta em integer
53.2 - 4  # 49.2    # numeric menos numeric resulta em numeric
2.0 * 2L  # 4       # numeric vezes integer resulta em numeric
3L / 4    # 0.75    # integer dividido por numeric resulta em numeric
3 %% 2	  # 1       # o resto de dois numeric é um outro numeric
# Aritmética ilegal produz um "não-é-um-número" (do inglês Not-a-Number):
0 / 0 # NaN
class(NaN) # "numeric"
# Você pode fazer aritmética em dois vetores com comprimento maior que 1,
# desde que o comprimento do vetor maior seja um múltiplo inteiro do menor
c(1,2,3) + c(1,2,3) # 2 4 6
# Como um único número é um vetor de comprimento um, escalares são aplicados
# elemento a elemento com relação a vetores
(4 * c(1,2,3) - 2) / 2 # 1 3 5
# Exceto para escalares, tenha cuidado ao realizar aritmética em vetores com
# comprimentos diferentes. Embora possa ser feito,
c(1,2,3,1,2,3) * c(1,2) # 1 4 3 2 2 6
# ter comprimentos iguais é uma prática melhor e mais fácil de ler
c(1,2,3,1,2,3) * c(1,2,1,2,1,2) 

# CHARACTERS
# Não há diferença entre strings e caracteres em R
"Horatio" # "Horatio"
class("Horatio") # "character"
class('H') # "character"
# São ambos vetores de caracteres de comprimento 1
# Aqui está um mais longo:
c('alef', 'bet', 'gimmel', 'dalet', 'he')
# "alef"   "bet"    "gimmel" "dalet"  "he"
length(c("Call","me","Ishmael")) # 3
# Você pode utilizar expressões regulares (regex) em vetores de caracteres:
substr("Fortuna multis dat nimis, nulli satis.", 9, 15) # "multis "
gsub('u', 'ø', "Fortuna multis dat nimis, nulli satis.") # "Fortøna møltis dat nimis, nølli satis."
# R tem vários vetores de caracteres embutidos:
letters
#  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z"
month.abb # "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

# LOGICALS
# Em R, um "logical" é um booleano
class(TRUE)	# "logical"
class(FALSE)	# "logical"
# O comportamento deles é normal
TRUE == TRUE	# TRUE
TRUE == FALSE	# FALSE
FALSE != FALSE	# FALSE
FALSE != TRUE	# TRUE
# Dados ausentes (NA) são logical, também
class(NA)	# "logical"
# Use | e & para operações lógicas.
# OR
TRUE | FALSE	# TRUE
# AND
TRUE & FALSE	# FALSE
# Aplicar | e & a vetores retorna operações lógicas elemento a elemento
c(TRUE,FALSE,FALSE) | c(FALSE,TRUE,FALSE) # TRUE TRUE FALSE
c(TRUE,FALSE,TRUE) & c(FALSE,TRUE,TRUE) # FALSE FALSE TRUE
# Você pode testar se x é TRUE
isTRUE(TRUE)	# TRUE
# Aqui obtemos um vetor logical com muitos elementos:
c('Z', 'o', 'r', 'r', 'o') == "Zorro" # FALSE FALSE FALSE FALSE FALSE
c('Z', 'o', 'r', 'r', 'o') == "Z" # TRUE FALSE FALSE FALSE FALSE

# FACTORS
# A classe factor é para dados categóricos
# Os fatores podem ser ordenados (como as avaliações de crianças) ou
# não ordenados (como as cores)
factor(c("azul", "azul", "verde", NA, "azul"))
#  azul azul verde   <NA>   azul
# Levels: azul verde
# Os "levels" são os valores que os dados categóricos podem assumir
# Observe que os dados ausentes não entram nos levels
levels(factor(c("verde", "verde", "azul", NA, "azul"))) # "azul" "verde"
# Se um vetor de factor tem comprimento 1, seus levels também terão comprimento 1
length(factor("green")) # 1
length(levels(factor("green"))) # 1
# Os fatores são comumente vistos em data frames, uma estrutura de dados que abordaremos
# mais tarde
data(infert) # "Infertilidade após aborto espontâneo e induzido"
levels(infert$education) # "0-5yrs"  "6-11yrs" "12+ yrs"

# NULL
# "NULL" é um valor estranho; use-o para "apagar" um vetor
class(NULL)	# NULL
parakeet = c("bico", "penas", "asas", "olhos")
parakeet
# [1] "bico"  "penas" "asas"  "olhos"
parakeet <- NULL
parakeet
# NULL

# COERÇÃO DE TIPO
# Coerção de tipo é quando você força um valor a assumir um tipo diferente
as.character(c(6, 8)) # "6" "8"
as.logical(c(1,0,1,1)) # TRUE FALSE  TRUE  TRUE
# Se você colocar elementos de diferentes tipos em um vetor, coerções estranhas acontecem:
c(TRUE, 4) # 1 4
c("cachorro", TRUE, 4) # "cachorro"  "TRUE" "4"
as.numeric("Bilbo")
# [1] NA
# Warning message:
# NAs introduced by coercion

# Observe também: esses são apenas os tipos de dados básicos
# Existem muitos outros tipos de dados, como datas, séries temporais, etc.



##################################################
# Variáveis, laços, expressões condicionais
##################################################

# Uma variável é como uma caixa na qual você armazena um valor para uso posterior.
# Chamamos isso de "atribuir" o valor à variável.
# Ter variáveis nos permite escrever laços, funções e instruções com condição

# VARIÁVEIS
# Existem muitas maneiras de atribuir valores:
x = 5 # é possível fazer assim
y <- "1" # mas é preferível fazer assim
TRUE -> z # isso funciona, mas é estranho

# LAÇOS
# Nós temos laços com for
for (i in 1:4) {
  print(i)
}
# [1] 1
# [1] 2
# [1] 3
# [1] 4
# Nós temos laços com while
a <- 10
while (a > 4) {
	cat(a, "...", sep = "")
	a <- a - 1
}
# 10...9...8...7...6...5...
# Tenha em mente que os laços for e while são executados lentamente em R
# Operações em vetores inteiros (por exemplo, uma linha inteira, uma coluna inteira)
# ou funções do tipo apply() (discutiremos mais tarde) são mais indicadas

# IF/ELSE
# Novamente, bastante padrão
if (4 > 3) {
	print("4 é maior que 3")
} else {
	print("4 não é maior que 3")
}
# [1] "4 é maior que 3"

# FUNÇÕES
# Definidas assim:
jiggle <- function(x) {
	x = x + rnorm(1, sd=.1)	# adicione um pouco de ruído (controlado)
	return(x)
}
# Chamada como qualquer outra função R:
jiggle(5)	# 5±ε. Após set.seed(2716057), jiggle(5)==5.005043



###########################################################################
# Estruturas de dados: Vetores, matrizes, data frames e arranjos (arrays)
###########################################################################

# UNIDIMENSIONAL

# Vamos começar do início, e com algo que você já sabe: vetores.
vec <- c(8, 9, 10, 11)
vec	#  8  9 10 11
# Consultamos elementos específicos utilizando colchetes
# (Observe que R começa a contar a partir de 1)
vec[1]		# 8
letters[18]	# "r"
LETTERS[13]	# "M"
month.name[9]	# "September"
c(6, 8, 7, 5, 3, 0, 9)[3]	# 7
# Também podemos pesquisar os índices de componentes específicos,
which(vec %% 2 == 0)	# 1 3
# pegue apenas as primeiras ou últimas entradas no vetor,
head(vec, 1)	# 8
tail(vec, 2)	# 10 11
# ou descubra se um determinado valor está no vetor
any(vec == 10) # TRUE
# Se um índice for além do comprimento de um vetor, você obterá NA:
vec[6]	# NA
# Você pode encontrar o comprimento do seu vetor com length()
length(vec)	# 4
# Você pode realizar operações em vetores inteiros ou subconjuntos de vetores
vec * 4	# 32 36 40 44
vec[2:3] * 5	# 45 50
any(vec[2:3] == 8) # FALSE
# e R tem muitas funções internas para sumarizar vetores
mean(vec)	# 9.5
var(vec)	# 1.666667
sd(vec)		# 1.290994
max(vec)	# 11
min(vec)	# 8
sum(vec)	# 38
# Mais alguns recursos embutidos:
5:15	# 5  6  7  8  9 10 11 12 13 14 15
seq(from=0, to=31337, by=1337)
#  [1]     0  1337  2674  4011  5348  6685  8022  9359 10696 12033 13370 14707
# [13] 16044 17381 18718 20055 21392 22729 24066 25403 26740 28077 29414 30751

# BIDIMENSIONAL (ELEMENTOS DA MESMA CLASSE)

# Você pode fazer uma matriz com entradas do mesmo tipo assim:
mat <- matrix(nrow = 3, ncol = 2, c(1,2,3,4,5,6))
mat
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# Ao contrário de um vetor, a classe de uma matriz é "matrix" independente do que ela contém
class(mat) # "matrix"
# Consulte a primeira linha
mat[1,]	# 1 4
# Execute uma operação na primeira coluna
3 * mat[,1]	# 3 6 9
# Consulte uma célula específica
mat[3,2]	# 6

# Transponha toda a matriz
t(mat)
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6

# Multiplicação de matrizes
mat %*% t(mat)
#      [,1] [,2] [,3]
# [1,]   17   22   27
# [2,]   22   29   36
# [3,]   27   36   45

# cbind() une vetores em colunas para formar uma matriz
mat2 <- cbind(1:4, c("cachorro", "gato", "passaro", "cachorro"))
mat2
#      [,1] [,2]
# [1,] "1"  "cachorro"
# [2,] "2"  "gato"
# [3,] "3"  "passaro"
# [4,] "4"  "cachorro"
class(mat2)	# matrix
# Mais uma vez, observe o que aconteceu!
# Como as matrizes devem conter todas as entradas da mesma classe,
# tudo foi convertido para a classe character
c(class(mat2[,1]), class(mat2[,2]))

# rbind() une vetores linha a linha para fazer uma matriz
mat3 <- rbind(c(1,2,4,5), c(6,7,0,4))
mat3
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    4    5
# [2,]    6    7    0    4
# Ah, tudo da mesma classe. Sem coerções. Muito melhor.

# BIDIMENSIONAL (CLASSES DIFERENTES)

# Para colunas de tipos diferentes, use um data frame
# Esta estrutura de dados é tão útil para programação estatística,
# que uma versão dela foi adicionada ao Python através do pacote "pandas".

estudantes <- data.frame(c("Cedric","Fred","George","Cho","Draco","Ginny"),
                         c(3,2,2,1,0,-1),
                         c("H", "G", "G", "R", "S", "G"))
names(estudantes) <- c("nome", "ano", "casa") # nomeie as colunas
class(estudantes)	# "data.frame"
estudantes
#     nome  ano  casa
# 1 Cedric    3     H
# 2   Fred    2     G
# 3 George    2     G
# 4    Cho    1     R
# 5  Draco    0     S
# 6  Ginny   -1     G
class(estudantes$ano)	# "numeric"
class(estudantes[,3])	# "factor"
# encontre as dimensões
nrow(estudantes)	# 6
ncol(estudantes)	# 3
dim(estudantes)	# 6 3
# A função data.frame() converte vetores de caracteres em vetores de fator
# por padrão; desligue isso definindo stringsAsFactors = FALSE quando
# você criar um data frame
?data.frame

# Existem muitas maneiras particulares de consultar partes de um data frame,
# todas sutilmente diferentes
estudantes$ano	# 3  2  2  1  0 -1
estudantes[,2]	# 3  2  2  1  0 -1
estudantes[,"ano"]	# 3  2  2  1  0 -1

# Uma versão extendida da estrutura data.frame é a data.table
# Se você estiver trabalhando com dados enormes ou em painel, ou precisar mesclar
# alguns conjuntos de dados, data.table pode ser uma boa escolha. Aqui está um tour
# relâmpago:
install.packages("data.table") # baixe o pacote a partir do CRAN
require(data.table) # carregue ele
estudantes <- as.data.table(estudantes)
estudantes # observe a saída ligeiramente diferente
#      nome  ano  casa
# 1: Cedric    3     H
# 2:   Fred    2     G
# 3: George    2     G
# 4:    Cho    1     R
# 5:  Draco    0     S
# 6:  Ginny   -1     G
estudantes[nome=="Ginny"] # Consulte estudantes com o nome == "Ginny"
#     nome  ano  casa
# 1: Ginny   -1     G
estudantes[ano==2] # Consulte estudantes com o ano == 2
#      nome  ano  casa
# 1:   Fred    2     G
# 2: George    2     G
# data.table facilita a fusão de dois conjuntos de dados
# vamos fazer outro data.table para mesclar com os alunos
fundadores <- data.table(casa=c("G","H","R","S"),
                         fundador=c("Godric","Helga","Rowena","Salazar"))
fundadores
#     casa fundador
# 1:     G  Godric
# 2:     H   Helga
# 3:     R  Rowena
# 4:     S Salazar
setkey(estudantes, casa)
setkey(fundadores, casa)
estudantes <- fundadores[estudantes] # mescle os dois conjuntos de dados com base na "casa"
setnames(estudantes, c("casa","nomeFundadorCasa","nomeEstudante","ano"))
estudantes[,order(c("nome","ano","casa","nomeFundadorCasa")), with=F]
#  nomeEstudante  ano  casa nomeFundadorCasa
# 1:        Fred    2     G           Godric
# 2:      George    2     G           Godric
# 3:       Ginny   -1     G           Godric
# 4:      Cedric    3     H            Helga
# 5:         Cho    1     R           Rowena
# 6:       Draco    0     S          Salazar

# O data.table torna as tabelas de sumário fáceis
estudantes[,sum(ano),by=casa]
#     casa V1
# 1:     G  3
# 2:     H  3
# 3:     R  1
# 4:     S  0

# Para remover uma coluna de um data.frame ou data.table,
# atribua a ela o valor NULL
estudantes$nomeFundadorCasa <- NULL
estudantes
#  nomeEstudante  ano  casa
# 1:        Fred    2     G
# 2:      George    2     G
# 3:       Ginny   -1     G
# 4:      Cedric    3     H
# 5:         Cho    1     R
# 6:       Draco    0     S

# Remova uma linha consultando parte dos dados
# Usando data.table:
estudantes[nomeEstudante != "Draco"]
#     casa estudanteNome  ano
# 1:     G          Fred    2
# 2:     G        George    2
# 3:     G         Ginny   -1
# 4:     H        Cedric    3
# 5:     R           Cho    1
# Usando data.frame:
estudantes <- as.data.frame(estudantes)
estudantes[estudantes$casa != "G",]
#    casa nomeFundadorCasa nomeEstudante  ano
# 4     H            Helga        Cedric    3
# 5     R           Rowena           Cho    1
# 6     S          Salazar         Draco    0

# MULTIDIMENSIONAL (TODOS OS ELEMENTOS DE UM TIPO)

# Arranjos (arrays) criam tabelas n-dimensionais
# Todos os elementos devem ser do mesmo tipo
# Você pode fazer uma tabela bidimensional (como uma matriz)
array(c(c(1,2,4,5),c(8,9,3,6)), dim=c(2,4))
#      [,1] [,2] [,3] [,4]
# [1,]    1    4    8    3
# [2,]    2    5    9    6
# Você pode usar array para fazer matrizes tridimensionais também
array(c(c(c(2,300,4),c(8,9,0)),c(c(5,60,0),c(66,7,847))), dim=c(3,2,2))
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

# LISTAS (MULTIDIMENSIONAIS, POSSIVELMENTE IMPERFEITAS, DE DIFERENTES TIPOS)

# Finalmente, R tem listas (de vetores)
lista1 <- list(tempo = 1:40)
lista1$preco = c(rnorm(40,.5*lista1$tempo,4)) # aleatória
lista1
# Você pode obter itens na lista assim
lista1$tempo # um modo
lista1[["tempo"]] # um outro modo
lista1[[1]] # e ainda um outro modo
#  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33
# [34] 34 35 36 37 38 39 40
# Você pode obter itens de uma lista como qualquer outro vetor
lista1$preco[4]

# Listas não são a estrutura de dados mais eficiente para se trabalhar em R;
# a menos que você tenha um bom motivo, você deve se ater a data.frames
# As listas geralmente são retornadas por funções que realizam regressões lineares

##################################################
# A família de funções apply()
##################################################

# Lembra de mat?
mat
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# Use apply(X, MARGIN, FUN) para aplicar a função FUN a uma matriz X
# sobre linhas (MARGIN = 1) ou colunas (MARGIN = 2)
# Ou seja, R faz FUN para cada linha (ou coluna) de X, muito mais rápido que um
# laço for ou while faria
apply(mat, MAR = 2, jiggle)
#      [,1] [,2]
# [1,]    3   15
# [2,]    7   19
# [3,]   11   23
# Outras funções: ?lappy, ?sapply

# Não as deixe te intimidar; todos concordam que essas funções são bem confusas

# O pacote plyr visa substituir (e melhorar!) a família *apply().
install.packages("plyr")
require(plyr)
?plyr



#########################
# Carregando dados
#########################

# "pets.csv" é um arquivo hospedado na internet
# (mas também poderia tranquilamente ser um arquivo no seu computador)
require(RCurl)
pets <- read.csv(textConnection(getURL("https://learnxinyminutes.com/docs/pets.csv")))
pets
head(pets, 2) # primeiras duas linhas
tail(pets, 1) # última linha

# Para salvar um data frame ou matriz como um arquivo .csv:
write.csv(pets, "pets2.csv") # para criar um novo arquivo .csv
# Define o diretório de trabalho com setwd(), confirme em qual você está com getwd()

# Experimente ?read.csv e ?write.csv para obter mais informações



#########################
# Análise estatística
#########################

# Regressão linear!
modeloLinear <- lm(preco  ~ tempo, data = lista1)
modeloLinear # imprime na tela o resultado da regressão
# Call:
# lm(formula = preco ~ tempo, data = lista1)
# 
# Coefficients:
# (Intercept)        tempo  
#      0.1453       0.4943  
summary(modeloLinear) # saída mais detalhada da regressão
# Call:
# lm(formula = preco ~ tempo, data = lista1)
#
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -8.3134 -3.0131 -0.3606  2.8016 10.3992 
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.14527    1.50084   0.097    0.923    
# tempo        0.49435    0.06379   7.749 2.44e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 4.657 on 38 degrees of freedom
# Multiple R-squared:  0.6124,	Adjusted R-squared:  0.6022 
# F-statistic: 60.05 on 1 and 38 DF,  p-value: 2.44e-09
coef(modeloLinear) # extrai os parâmetros estimados
# (Intercept)       tempo 
#   0.1452662   0.4943490 
summary(modeloLinear)$coefficients # um outro meio de extrair os resultados
#              Estimate Std. Error    t value     Pr(>|t|)
# (Intercept) 0.1452662 1.50084246 0.09678975 9.234021e-01
# tempo       0.4943490 0.06379348 7.74920901 2.440008e-09
summary(modeloLinear)$coefficients[,4] # the p-values 
#  (Intercept)        tempo
# 9.234021e-01 2.440008e-09 

# MODELOS LINEARES GERAIS
# Regressão logística
set.seed(1)
lista1$sucesso = rbinom(length(lista1$tempo), 1, .5) # binário aleatório
modeloLg <- glm(sucesso  ~ tempo, data = lista1, 
	family=binomial(link="logit"))
modeloLg # imprime na tela o resultado da regressão logística
# Call:  glm(formula = sucesso ~ tempo, 
#	family = binomial(link = "logit"), data = lista1)
#
# Coefficients:
# (Intercept)        tempo  
#     0.17018     -0.01321  
# 
# Degrees of Freedom: 39 Total (i.e. Null);  38 Residual
# Null Deviance:	    55.35 
# Residual Deviance: 55.12 	 AIC: 59.12
summary(modeloLg) # saída mais detalhada da regressão
# Call:
# glm(formula = sucesso ~ tempo, 
#	family = binomial(link = "logit"), data = lista1)

# Deviance Residuals: 
#    Min      1Q  Median      3Q     Max  
# -1.245  -1.118  -1.035   1.202   1.327  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  0.17018    0.64621   0.263    0.792
# tempo       -0.01321    0.02757  -0.479    0.632
# 
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 55.352  on 39  degrees of freedom
# Residual deviance: 55.121  on 38  degrees of freedom
# AIC: 59.121
# 
# Number of Fisher Scoring iterations: 3


#########################
# Gráficos
#########################

# FUNÇÕES DE PLOTAGEM INTEGRADAS
# Gráficos de dispersão!
plot(lista1$tempo, lista1$preco, main = "dados falsos")
# Trace a linha de regressão em um gráfico existente!
abline(modeloLinear, col = "red")
# Obtenha uma variedade de diagnósticos legais
plot(modeloLinear)
# Histogramas!
hist(rpois(n = 10000, lambda = 5), col = "thistle")
# Gráficos de barras!
barplot(c(1,4,5,1,2), names.arg = c("red","blue","purple","green","yellow"))

# GGPLOT2
# Mas estes não são nem os mais bonitos dos gráficos no R
# Experimente o pacote ggplot2 para gráficos diferentes e mais bonitos
install.packages("ggplot2")
require(ggplot2)
?ggplot2
pp <- ggplot(estudantes, aes(x=casa))
pp + geom_bar()
ll <- as.data.table(lista1)
pp <- ggplot(ll, aes(x=tempo,preco))
pp + geom_point()
# ggplot2 tem uma excelente documentação (disponível em http://docs.ggplot2.org/current/)
```

## Como faço para obter R?

* Obtenha o R e uma interface gráfica para o R em [http://www.r-project.org/](http://www.r-project.org/)
* [RStudio](http://www.rstudio.com/ide/) é uma outra interface gráfica
