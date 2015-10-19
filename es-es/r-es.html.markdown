---
language: R
contributors:
    - ["e99n09", "http://github.com/e99n09"]
    - ["isomorphismes", "http://twitter.com/isomorphisms"]
translators:
    - ["David Hsieh", "http://github.com/deivuh"]
lang: es-es    
filename: learnr-es.r
---

R es un lenguaje de computación estadística. Tiene muchas librerías para cargar
y limpiar sets de datos, ejecutar procedimientos estadísticos y generar
gráficas. También puedes ejecutar comandos `R` dentro de un documento de
LaTeX.

```r

# Los comentariso inician con símbolos numéricos.

# No puedes hacer comentarios de múltiples líneas
# pero puedes agrupar múltiples comentarios de esta manera. 

# En Windows puedes utilizar CTRL-ENTER para ejecutar una línea.
# En Mac utilizas COMMAND-ENTER


#############################################################################
# Cosas que puedes hacer sin entender nada acerca de programación
#############################################################################

# En esta sección, mostramos algunas cosas chileras / cool que puedes hacer en
# R sin entender nada de programación. No te preocupes en entender nada 
# de lo que hace este código. Solo disfruta!

data()	        # Examinar sets de datos pre-cargados
data(rivers)	# Obtiene este: Lengths of Major North American Rivers"
ls()	        # Fijarse que "rivers" ahora aparece en el workspace
head(rivers)	# Echarle un ojo al set de datos
# 735 320 325 392 524 450

length(rivers)	# ¿Cuántos ríos fueron medidos?
# 141
summary(rivers) # ¿Cuáles son algunas estadísticas generales?
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  135.0   310.0   425.0   591.2   680.0  3710.0

# Generar una gráfica tallo-y-hoja (Visualización de datos tipo histograma)
stem(rivers)

# El punto decimal son 2 dígitos a la derecha de | 
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

stem(log(rivers)) # Fijarse que la data no es normal ni log-normal!
# Toma eso, fundamentalistas de la curva de campana!

# El punto decimal está a 1 dígito a la izquierda del |
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

# Generar un histograma:
hist(rivers, col="#333333", border="white", breaks=25) # Juega con los estos parámetros
hist(log(rivers), col="#333333", border="white", breaks=25) # Generarás más gráficas después

# Aquí hay otro set de datos pre-cargado. R tiene bastantes de éstos.
data(discoveries)
plot(discoveries, col="#333333", lwd=3, xlab="Year",
     main="Number of important discoveries per year")
plot(discoveries, col="#333333", lwd=3, type = "h", xlab="Year",
     main="Number of important discoveries per year")

# En lugar de dejar el orden por defecto (por año),
# podemos ordenar de tal manera que muestre qué es típico:
sort(discoveries)
#  [1]  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1  1  1  1  1  2  2  2  2
# [26]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  3  3  3
# [51]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  4  4  4  4  4  4  4  4
# [76]  4  4  4  4  5  5  5  5  5  5  5  6  6  6  6  6  6  7  7  7  7  8  9 10 12

stem(discoveries, scale=2)
#
#  El punto decimal se encuentra en |
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

# Tirar los dados varias veces
round(runif(7, min=.5, max=6.5))
# 1 4 6 1 4 6 4
# Tus números será diferente de los míos, a menos que tengamos el mismo valor 
# de random.seed(31337)

# Dibuja de un Gaussian 9 veces
rnorm(9)
# [1]  0.07528471  1.03499859  1.34809556 -0.82356087  0.61638975 -1.88757271
# [7] -0.59975593  0.57629164  1.08455362



##################################################
# Tipos de datos y aritmética básica
##################################################

# Ahora para la parte de programación orientada a objetos del tutorial.
# En esta sección conocerás los tipos de datos importantes de R:
# Enteros, numéricos, caracteres, lógicos, y factores.
# Hay otros, pero esos son los que menos necesitas para empezar.

# ENTEROS
# Enteros de almacenamiento largo son escritos con L
5L # 5
class(5L) # "integer"
# (Try ?class para más información en la función class().)
# En R, cada valor único, como 5L, es considerado un vector de logitud 1
length(5L) # 1
# También puedes tener un vector de enteros con longitud > 1:
c(4L, 5L, 8L, 3L) # 4 5 8 3
length(c(4L, 5L, 8L, 3L)) # 4
class(c(4L, 5L, 8L, 3L)) # "integer"

# NUMÉRICOS
# Un "numérico" es un número de punto flotante de doble precisión.
5 # 5
class(5) # "numeric"
# Nuevamente, todo en R es un vector;
# puedes hacer un vector numérico con más de un elemento
c(3,3,3,2,2,1) # 3 3 3 2 2 1
# También puedes utilizar el notación científica
5e4 # 50000
6.02e23 # Número de Avogadro
1.6e-35 # Logintud Planck 
# También puedes tener números infinitamente grandes o pequeños
class(Inf)	# "numeric"
class(-Inf)	# "numeric"
# Puede que uses "Inf", por ejemplo, en integrate(dnorm, 3, Inf);
# esto obvia las tablas de puntos Z.

# ARITMÉTICA BÁSICA
# Puedes hacer aritmética con números
# Haciendo aritmética en un mix de enteros y numéricos, te da otro numérico
10L + 66L # 76      # entero mas entero da entero
53.2 - 4  # 49.2    # entero menos entero da numérico
2.0 * 2L  # 4       # numérico veces entero da numérico
3L / 4    # 0.75    # entero sobre numérico da numérico
3 %% 2	  # 1       # el residuo de dos numéricos es otro numérico
# La aritmética ilegal rinde un "not-a-number"
0 / 0 # NaN
class(NaN) # "numeric"
# Puedes hacer aritmética con dos vectores con longitud mayor a 1,
# siempre que la longitud del vector mayor es un entero múltiplo del menor.
c(1,2,3) + c(1,2,3) # 2 4 6

# CARACTERES
# No hay diferencia entre strings y caracteres en R
"Horatio" # "Horatio"
class("Horatio") # "character"
class('H') # "character"
# Ambos eran vectores de caracteres de longitud 1
# Aquí hay uno más largo:
c('alef', 'bet', 'gimmel', 'dalet', 'he')
# =>
# "alef"   "bet"    "gimmel" "dalet"  "he"
length(c("Call","me","Ishmael")) # 3
# Puedes hacer operaciones regex en vectores de caracteres:
substr("Fortuna multis dat nimis, nulli satis.", 9, 15) # "multis "
gsub('u', 'ø', "Fortuna multis dat nimis, nulli satis.") # "Fortøna møltis dat nimis, nølli satis."
# R tiene varios vectores predefinidos de caracteres 
letters
# =>
#  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z"
month.abb # "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

# LÓGICOS
# En R, un "logical" es un boolean
class(TRUE)	# "logical"
class(FALSE)	# "logical"
# Ese comportamiento es normal
TRUE == TRUE	# TRUE
TRUE == FALSE	# FALSE
FALSE != FALSE	# FALSE
FALSE != TRUE	# TRUE
# El dato faltante (NA) es lógico también
class(NA)	# "logical"
# Utiliza | y & para operaciones lógicas
# OR
TRUE | FALSE	# TRUE
# AND
TRUE & FALSE	# FALSE
# Puedes probar si x es TRUE (verdadero)
isTRUE(TRUE)	# TRUE
# Aquí tenemos un vector lógico con varios elementos:
c('Z', 'o', 'r', 'r', 'o') == "Zorro" # FALSE FALSE FALSE FALSE FALSE
c('Z', 'o', 'r', 'r', 'o') == "Z" # TRUE FALSE FALSE FALSE FALSE

# FACTORES
# La clase factor es para datos de categoría
# Los factores pueden ser ordenados (como las calificaciones de los niños) 
# o sin orden (como el género)
factor(c("female", "female", "male", NA, "female"))
#  female female male   <NA>   female
# Levels: female male
# Los "levels" son los valores que los datos categóricos pueden tener
# Tomar nota que los datos faltantes no entran a los niveles
levels(factor(c("male", "male", "female", NA, "female"))) # "female" "male"
# Si un vector de factores tiene longitud 1, sus niveles también tendrán
# una longitud de 1 también

length(factor("male")) # 1
length(levels(factor("male"))) # 1
# Los factores son comúnmente vistos en marcos de dato, y una estructura de 
# datos que cubriremos después
data(infert) # "Infertility after Spontaneous and Induced Abortion"
levels(infert$education) # "0-5yrs"  "6-11yrs" "12+ yrs"

# NULL
# "NULL" es uno raro; utilízalo para "limpiar" un vector
class(NULL)	# NULL
parakeet = c("beak", "feathers", "wings", "eyes")
parakeet
# =>
# [1] "beak"     "feathers" "wings"    "eyes"
parakeet <- NULL
parakeet
# =>
# NULL

# COERCIÓN DE TIPO
# La coerción de tipos es cuando forzas un valor diferente tipo al que puede tomar.
as.character(c(6, 8)) # "6" "8"
as.logical(c(1,0,1,1)) # TRUE FALSE  TRUE  TRUE
# Si pones elementos de diferentes tipos en un vector, coerciones raras pasan:
c(TRUE, 4) # 1 4
c("dog", TRUE, 4) # "dog"  "TRUE" "4"
as.numeric("Bilbo")
# =>
# [1] NA
# Warning message:
# NAs introduced by coercion

# También tomar nota: Esos solo eran datos de tipos básicos
# Hay mucho más tipos de datos, como las fechas, series de tiempo, etc.


##################################################
# Variables, ciclos, condiciones (if/else)
##################################################

# A variable is like a box you store a value in for later use.
# We call this "assigning" the value to the variable.
# Having variables lets us write loops, functions, and if/else statements

# VARIABLES
# Muchas maneras de asignar valores:
x = 5 # esto es posible
y <- "1" # esto es preferido
TRUE -> z # estos funciona pero es raro

# CICLOS
# Tenemos ciclos 'for'
for (i in 1:4) {
  print(i)
}
# Tenemos ciclos 'while'
a <- 10
while (a > 4) {
	cat(a, "...", sep = "")
	a <- a - 1
}
# Ten en mente que los ciclos 'for' y 'while' son lentos en R
# Operaciones con vectores enteros (i.e. una fila o columna completa)
# o tipos de función apply() (que discutiremos después) son preferidos

# CONDICIONES (IF/ELSE)
# De nuevo, bastante normal
if (4 > 3) {
	print("4 is greater than 3")
} else {
	print("4 is not greater than 3")
}
# =>
# [1] "4 is greater than 3"

# FUNCIONES
# Definidos de la siguiente manera:
jiggle <- function(x) {
	x = x + rnorm(1, sd=.1)	#agregar un poco de ruido (controlado)
	return(x)
}
# Llamados como cualquier otra función de R
jiggle(5)	# 5±ε. luego de set.seed(2716057), jiggle(5)==5.005043



###########################################################################
# Estructura de datos: Vectores, matrices, marcos da datos y arreglos
###########################################################################

# UNIDIMENSIONAL

# Empecemos desde el principio, y con algo que ya conoces: vectores.
vec <- c(8, 9, 10, 11)
vec	#  8  9 10 11
# Preguntamos por elementos específicos poniendo un subconjunto en corchetes
# (Toma nota de que R empieza los conteos desde 1)
vec[1]		# 8
letters[18]	# "r"
LETTERS[13]	# "M"
month.name[9]	# "September"
c(6, 8, 7, 5, 3, 0, 9)[3]	# 7
# También podes buscar por los índices de componentes específicos,
which(vec %% 2 == 0)	# 1 3
# obtener la primera o las últimas entradas de un vector,
head(vec, 1)	# 8
tail(vec, 2)	# 10 11
# o averiguar si cierto valor se encuentra dentro de un vector
any(vec == 10) # TRUE
# Si un índice "se pasa", obtendrás un NA:
vec[6]	# NA
# Puedes encontrar la longitud de un vector con length()
length(vec)	# 4
# Puedes realizar operaciones con vectores enteros o con subconjuntos de vectores
vec * 4	# 16 20 24 28
vec[2:3] * 5	# 25 30
any(vec[2:3] == 8) # FALSE
# y R tiene muchas funciones pre-definidas para resumir vectores
mean(vec)	# 9.5
var(vec)	# 1.666667
sd(vec)		# 1.290994
max(vec)	# 11
min(vec)	# 8
sum(vec)	# 38
# Otras funciones pre-definidas:
5:15	# 5  6  7  8  9 10 11 12 13 14 15
seq(from=0, to=31337, by=1337)
# =>
#  [1]     0  1337  2674  4011  5348  6685  8022  9359 10696 12033 13370 14707
# [13] 16044 17381 18718 20055 21392 22729 24066 25403 26740 28077 29414 30751

# BIDIMENCIONAL (TODO EN UNA CLASE)

# Puedes hacer una matriz de las entradas todos de un mismo tipo como:
mat <- matrix(nrow = 3, ncol = 2, c(1,2,3,4,5,6))
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# A diferencia de un vector, una clase matriz es una 'matriz', 
# sin importar qué contiene
class(mat) # => "matrix"
# Consulta la primera fila
mat[1,]	# 1 4
# Realiza una operación en la primera columna
3 * mat[,1]	# 3 6 9
# Consulta por una celda específica
mat[3,2]	# 6

# Transpone una matriz entera
t(mat)
# =>
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6

# Multiplicación de matrices
mat %*% t(mat)
# =>
#      [,1] [,2] [,3]
# [1,]   17   22   27
# [2,]   22   29   36
# [3,]   27   36   45

# cbind() une vectores como columnas para hacer una matriz
mat2 <- cbind(1:4, c("dog", "cat", "bird", "dog"))
mat2
# =>
#      [,1] [,2]
# [1,] "1"  "dog"
# [2,] "2"  "cat"
# [3,] "3"  "bird"
# [4,] "4"  "dog"
class(mat2)	# matrix
# De nuevo, ten en cuenta lo que sucedió
# Debido a que las matrices deben de contener todas las entradas del mismo tipo,
# todo fue convertido a la clase caracter
c(class(mat2[,1]), class(mat2[,2]))

# rbind() une vectores como filas para hacer una matriz
mat3 <- rbind(c(1,2,4,5), c(6,7,0,4))
mat3
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    4    5
# [2,]    6    7    0    4
# Ah, todo es de la misma clase. No hay coerciones. Mucho mejor.

# BIDIMENSIONAL (DIFERENTES CLASES)

# Para columnas de tipos diferentes, utiliza un data frame
# Esta estructura de datos es muy útil para programación estadística,
# una versión de ésta fue agregada a Python en el paquete "pandas".

students <- data.frame(c("Cedric","Fred","George","Cho","Draco","Ginny"),
                       c(3,2,2,1,0,-1),
                       c("H", "G", "G", "R", "S", "G"))
names(students) <- c("name", "year", "house") # name the columns
class(students)	# "data.frame"
students
# =>
#     name year house
# 1 Cedric    3     H
# 2   Fred    2     G
# 3 George    2     G
# 4    Cho    1     R
# 5  Draco    0     S
# 6  Ginny   -1     G
class(students$year)	# "numeric"
class(students[,3])	# "factor"
# encontrar las dimensiones
nrow(students)	# 6
ncol(students)	# 3
dim(students)	# 6 3
# La función data.frame() convierte vectores de caracteres en vectores 
# de factores por defecto; deshabilita este atributo
# stringsAsFactors = FALSE cuando vayas a crear el data.frame
?data.frame

# Hay otras formas de hacer subconjuntos de data frames
students$year	# 3  2  2  1  0 -1
students[,2]	# 3  2  2  1  0 -1
students[,"year"]	# 3  2  2  1  0 -1

# Una versión aumentada de la estructura data.frame es el data.table
# Si estás trabajando huge o panel data, o necesitas unificar algunos 
# subconjuntos de datos, data.table puede ser una buena elección.
# Aquí un tour:
install.packages("data.table") # Descarga el paquete de CRAN
require(data.table) # Cárgalo
students <- as.data.table(students)
students # Tomar en cuenta la diferencia de la impresión
# =>
#      name year house
# 1: Cedric    3     H
# 2:   Fred    2     G
# 3: George    2     G
# 4:    Cho    1     R
# 5:  Draco    0     S
# 6:  Ginny   -1     G
students[name=="Ginny"] # obtener filas con name == "Ginny"
# =>
#     name year house
# 1: Ginny   -1     G
students[year==2] # obtener filas con year == 2
# =>
#      name year house
# 1:   Fred    2     G
# 2: George    2     G
# data.table hace que la unificación de dos sets de datos sea fácil
# Hagamos otro data.table para unifiar a los estudiantes
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
students <- founders[students] # Unifica los dos sets de datos comparando "house"
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

# data.table hace que sea fácil obtener resúmenes de las tablas
students[,sum(year),by=house]
# =>
#    house V1
# 1:     G  3
# 2:     H  3
# 3:     R  1
# 4:     S  0

# Para eliminar una columna de un data.frame o data.table, 
# asignarle el valor NULL.
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

# Elimina una fila poniendo un subconjunto
# Usando data.table:
students[studentName != "Draco"]
# =>
#    house studentName year
# 1:     G        Fred    2
# 2:     G      George    2
# 3:     G       Ginny   -1
# 4:     H      Cedric    3
# 5:     R         Cho    1
# Usando data.frame:
students <- as.data.frame(students)
students[students$house != "G",]
# =>
#   house houseFounderName studentName year
# 4     H            Helga      Cedric    3
# 5     R           Rowena         Cho    1
# 6     S          Salazar       Draco    0

# MULTI-DIMENSIONAL (TODOS LOS ELEMENTOS DE UN TIPO)

# Arreglos crean una tabla de dimensión n
# Todos los elementos deben de ser del mismo tipo
# Puedes hacer una tabla bi-dimensional (como una matriz)
array(c(c(1,2,4,5),c(8,9,3,6)), dim=c(2,4))
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    4    8    3
# [2,]    2    5    9    6
# Puedes utilizar un arreglo para hacer una matriz tri-dimensional también
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

# LISTAS (MULTI-DIMENSIONAL, POSIBLEMENTE DESIGUALES, DE DIFERENTES TIPOS)

# Finalmente, R tiene listas (de vectores)
list1 <- list(time = 1:40)
list1$price = c(rnorm(40,.5*list1$time,4)) # aleatorio
list1
# Puedes obtener elementos de una lista de la siguiente manera
list1$time # Una manera
list1[["time"]] # Otra manera
list1[[1]] # Y otra manera
# =>
#  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33
# [34] 34 35 36 37 38 39 40
# Puedes crear una lista de subconjuntos como cualquier otro vector
list1$price[4]

# Las listas no son la estructura de datos más eficiente para trabajar en R;
# a menos de que tengas una buena razón, deberías de quedarte con data.frames
# Las listas son usualmente devueltas por funciones que realizan regresiones 
# lineales

##################################################
# La familia de funciones apply()
##################################################

# Te recuerdas de mat?
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# Utiliza apply(X, MARGIN, FUN) paraaplicar una función FUN a la matriz X
# sobre las filas (MAR = 1) o las columnas (MAR = 2)
# Eso es, R aplica FUN sobre cada fila (o columna) de X, mucho más rápido que
# lo que haría un ciclo 'for' o 'loop'
apply(mat, MAR = 2, jiggle)
# =>
#      [,1] [,2]
# [1,]    3   15
# [2,]    7   19
# [3,]   11   23
# Otras funciones: ?lapply, ?sapply

# No te sientas muy intimidado; todos están de acuerdo que son confusas

# El paquete plyr busca reemplazar (y mejorar) la familiar *apply()
install.packages("plyr")
require(plyr)
?plyr



#########################
# Carga de datos
#########################

# "pets.csv" es un archivo en internet
# (pero puede ser tan fácil como tener el archivo en tu computadora)
pets <- read.csv("http://learnxinyminutes.com/docs/pets.csv")
pets
head(pets, 2) # primeras dos filas
tail(pets, 1) # última fila

# Para guardar un data frame o una matriz como un archivo .csv
write.csv(pets, "pets2.csv") # para hacer un nuevo archivo .csv
# definir el directorio de trabajo con setwd(), búscalo con getwd()

# Prueba ?read.csv ?write.csv para más información


#########################
# Gráficas
#########################

# FUNCIONES PREDEFINIDAS DE GRAFICACIÓN
# Gráficos de dispersión!
plot(list1$time, list1$price, main = "fake data")
# Regresiones!
linearModel <- lm(price  ~ time, data = list1)
linearModel # Muestra el resultado de la regresión
# Grafica la línea de regresión
abline(linearModel, col = "red")
# Obtiene una veridad de diagnósticos
plot(linearModel)
# Histogramas!
hist(rpois(n = 10000, lambda = 5), col = "thistle")
# Barras!
barplot(c(1,4,5,1,2), names.arg = c("red","blue","purple","green","yellow"))

# GGPLOT2
# Pero éstas no son las gráficas más bonitas de R
# Prueba el paquete ggplot2 para mayor variedad y mejores gráficas
install.packages("ggplot2")
require(ggplot2)
?ggplot2
pp <- ggplot(students, aes(x=house))
pp + geom_histogram()
ll <- as.data.table(list1)
pp <- ggplot(ll, aes(x=time,price))
pp + geom_point()
# ggplot2 tiene una excelente documentación 
# (disponible en http://docs.ggplot2.org/current/)



```

## ¿Cómo obtengo R?

* Obtén R y R GUI de [http://www.r-project.org/](http://www.r-project.org/)
* [RStudio](http://www.rstudio.com/ide/) es otro GUI
