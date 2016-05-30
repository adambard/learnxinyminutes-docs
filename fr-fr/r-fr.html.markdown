---
language: R
contributors:
    - ["e99n09", "http://github.com/e99n09"]
    - ["isomorphismes", "http://twitter.com/isomorphisms"]
translators:
    - ["Anne-Catherine Dehier", "https://github.com/spellart"]
filename: learnr-fr.r
lang: fr-fr
---

R est un langage de programmation statistique. Il dispose de nombreuses
bibliothèques pour le téléchargement et le nettoyage d'ensembles de données,
l'exécution de procédures statistiques, et la réalisation de graphiques.
On peut également exécuter des commmandes `R` au sein d'un document LaTeX.


```r

# Les commentaires commencent avec des symboles numériques.

# Il n'est pas possible de faire des commentaires multilignes,
# mais on peut placer plusieurs commentaires les uns en dessous
# des autres comme ceci.

# Sur Mac, taper COMMAND-ENTER pour exécuter une ligne
# et sur Windows taper CTRL-ENTER



########################################################################
# Les choses que vous pouvez faire sans rien comprendre
# à la programmation
########################################################################

# Dans cette section, nous vous montrons quelques trucs cools que vous
# pouvez faire avec R sans rien comprendre à la programmation.
# Ne vous inquiétez pas si vous ne comprenez pas tout ce que le code fait.
# Profitez simplement !

data()          # parcours les ensembles de données préchargées
data(rivers)	# récupère ceci : "Lengths of Major North American Rivers"
ls()            # notez que "rivers" apparaît maintenant dans votre espace de travail
head(rivers)	# donne un aperçu des données
# 735 320 325 392 524 450

length(rivers)	# Combien de rivers ont été mesurées ?
# 141
summary(rivers) # Quelles sont les principales données statistiques ?
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  135.0   310.0   425.0   591.2   680.0  3710.0

# Fait un diagramme à tiges et à feuilles (visualisation de données de
# types histogramme)
stem(rivers)


#  Le point décimal est de 2 chiffres à droite du |
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

stem(log(rivers)) # Notez que les données ne sont ni normales
# ni lognormales !
# Prenez-ça, la courbe en cloche

#  Le point décimal est à 1 chiffre à gauche du |
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

# Fait un histogramme :
hist(rivers, col="#333333", border="white", breaks=25) # amusez-vous avec ces paramètres
hist(log(rivers), col="#333333", border="white", breaks=25) # vous ferez plus de tracés plus tard

# Ici d'autres données qui viennent préchargées. R en a des tonnes.
data(discoveries)
plot(discoveries, col="#333333", lwd=3, xlab="Year",
     main="Number of important discoveries per year")
plot(discoveries, col="#333333", lwd=3, type = "h", xlab="Year",
     main="Number of important discoveries per year")

# Plutôt que de laisser l'ordre par défaut (par année)
# Nous pourrions aussi trier pour voir ce qu'il y a de typique
sort(discoveries)
#  [1]  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1  1  1  1  1  2  2  2  2
# [26]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  3  3  3
# [51]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  4  4  4  4  4  4  4  4
# [76]  4  4  4  4  5  5  5  5  5  5  5  6  6  6  6  6  6  7  7  7  7  8  9 10 12

stem(discoveries, scale=2)
#
#  Le point décimale est à la |
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

# Lance un dé plusieurs fois
round(runif(7, min=.5, max=6.5))
# 1 4 6 1 4 6 4
# Vos numéros diffèreront des miens à moins que nous mettions
# le même random.seed(31337)

# Dessine à partir d'une normale Gaussienne 9 fois
rnorm(9)
# [1]  0.07528471  1.03499859  1.34809556 -0.82356087  0.61638975 -1.88757271
# [7] -0.59975593  0.57629164  1.08455362



##############################################################
# les types de données et l'arithmétique de base
##############################################################

# Maintenant pour la partie orientée programmation du tutoriel.
# Dans cette section vous rencontrerez les types de données importants de R :
# les entiers, les numériques, les caractères, les logiques, et les facteurs.

# LES ENTIERS
# Les entiers de type long sont écrits avec L
5L # 5
class(5L) # "integer"
# (Essayez ?class pour plus d'informations sur la fonction class().)
# Avec R, chaque valeur seule, comme 5L, est considérée comme
# un vecteur de longueur 1
length(5L) # 1
# On peut avoir un vecteur d'entiers avec une longueur > 1 :
c(4L, 5L, 8L, 3L) # 4 5 8 3
length(c(4L, 5L, 8L, 3L)) # 4
class(c(4L, 5L, 8L, 3L)) # "integer"

# LES NUMÉRIQUES
# Un "numeric" est un nombre à virgule flottante d'une précision double
5 # 5
class(5) # "numeric"
# Encore une fois, tout dans R est un vecteur ;
# Vous pouvez faire un vecteur numérique avec plus d'un élément
c(3,3,3,2,2,1) # 3 3 3 2 2 1
# Vous pouvez aussi utiliser la notation scientifique
5e4 # 50000
6.02e23 # nombre d'Avogadro
1.6e-35 # longueur de Planck
# Vous pouvez également avoir des nombres infiniments grands ou petits
class(Inf)	# "numeric"
class(-Inf)	# "numeric"
# Vous pouvez utiliser "Inf", par exemple, dans integrate(dnorm, 3, Inf);
# Ça permet d'éviter de réaliser une table de la loi normale.

# ARITHMÉTIQUES DE BASE
# Vous pouvez faire de l'arithmétique avec des nombres
# Faire des opérations arithmétiques en mixant des entiers
# et des numériques
# donne un autre numérique
10L + 66L # 76      # un entier plus un entier donne un entier
53.2 - 4  # 49.2    # un numérique moins un numérique donne un numérique
2.0 * 2L  # 4       # un numérique multiplié par un entier donne un numérique
3L / 4    # 0.75    # un entier sur un numérique donne un numérique
3 %% 2    # 1       # le reste de deux numériques est un autre numérique
# Les opérations arithmétiques illégales donnent un "Not A Number" :
0 / 0 # NaN
class(NaN) # "numeric"
# Vous pouvez faire des opérations arithmétiques avec deux vecteurs d'une
# longueur plus grande que 1, à condition que la longueur du plus grand
# vecteur soit un multiple entier du plus petit
c(1,2,3) + c(1,2,3) # 2 4 6

# LES CARACTÈRES
# Il n'y a pas de différences entre les chaînes de caractères et
# les caractères en R
"Horatio" # "Horatio"
class("Horatio") # "character"
class('H') # "character"
# Ceux-ci sont tous les deux des vecteurs de longueur 1
# Ici un plus long :
c('alef', 'bet', 'gimmel', 'dalet', 'he')
# =>
# "alef"   "bet"    "gimmel" "dalet"  "he"
length(c("Call","me","Ishmael")) # 3
# Vous pouvez utiliser des expressions rationnelles sur les vecteurs de caractères :
substr("Fortuna multis dat nimis, nulli satis.", 9, 15) # "multis "
gsub('u', 'ø', "Fortuna multis dat nimis, nulli satis.") # "Fortøna møltis dat nimis, nølli satis."
# R possède plusieurs vecteurs de caractères préconstruits :
letters
# =>
#  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z"
month.abb # "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

# LES TYPES BOOLÉENS
# En R, un "logical" est un booléen
class(TRUE)	# "logical"
class(FALSE)	# "logical"
# Leur comportement est normal
TRUE == TRUE	# TRUE
TRUE == FALSE	# FALSE
FALSE != FALSE	# FALSE
FALSE != TRUE	# TRUE
# Les données manquantes (NA) sont logiques également
class(NA)	# "logical"
# On utilise | et & pour les operations logiques.
# OR
TRUE | FALSE	# TRUE
# AND
TRUE & FALSE	# FALSE
# Vous pouvez tester si x est TRUE
isTRUE(TRUE)	# TRUE
# Ici nous avons un vecteur de type logique avec plusieurs éléments :
c('Z', 'o', 'r', 'r', 'o') == "Zorro" # FALSE FALSE FALSE FALSE FALSE
c('Z', 'o', 'r', 'r', 'o') == "Z" # TRUE FALSE FALSE FALSE FALSE

# LES FACTEURS
# Les facteurs sont généralement utilisés pour y stocker des
# variables qualitatives (catégorielles).
# Les facteurs peuvent être ordonnés (comme le niveau scolaire
# des enfants) ou non ordonnés (comme le sexe)
factor(c("female", "female", "male", NA, "female"))
#  female female male   <NA>   female
# Les niveaux : female male
# Les facteurs possèdent un attribut appelé niveau ("level").
# Les niveaux sont des vecteurs contenant toutes les valeurs
# que peuvent prendre les données catégorielles.
# Notez que les données manquantes n'entrent pas dans le niveau
levels(factor(c("male", "male", "female", NA, "female"))) # "female" "male"
# Si le vecteur de facteurs a une longueur 1, ses niveaux seront
# de longueur 1 également
length(factor("male")) # 1
length(levels(factor("male"))) # 1
# On rencontre communément des facteurs dans des "data frame",
# un type de données que nous couvrirons plus tard
data(infert) # "Infertility after Spontaneous and Induced Abortion"
levels(infert$education) # "0-5yrs"  "6-11yrs" "12+ yrs"

# NULL
# "NULL" est bizarre ; on l'utilise pour effacer un vecteur
class(NULL)	# NULL
parakeet = c("beak", "feathers", "wings", "eyes")
parakeet
# =>
# [1] "beak"     "feathers" "wings"    "eyes"
parakeet <- NULL
parakeet
# =>
# NULL

# LES CONVERSIONS DE TYPES
# Les conversions de types servent à forcer une valeur à prendre
# un type différent
as.character(c(6, 8)) # "6" "8"
as.logical(c(1,0,1,1)) # TRUE FALSE  TRUE  TRUE
# Si vous mettez des éléments de différents types dans un vecteur,
# des coercitions bizarres se produisent :
c(TRUE, 4) # 1 4
c("dog", TRUE, 4) # "dog"  "TRUE" "4"
as.numeric("Bilbo")
# =>
# [1] NA
# Message d'avertissement :
# NAs est introduit par coercition

# Notez également : ce n'étaient que des types de données basiques
# Il y a beaucoup d'autres types de données, comme les dates,
# les séries temporelles, etc ...



#######################################
# Variables, boucles , if/else
#######################################

# Une variable est comme une boîte dans laquelle on garde une valeur
# pour l'utiliser plus tard.
# Nous appellons ça "assigner" une valeur à une variable.
# Avoir des variables nous permet d'écrire des boucles, des fonctions, et
# des instructions conditionnelles (if/else)

# LES VARIABLES
# Beaucoup de façons d'assigner des choses :
x = 5 # c'est correct
y <- "1" # c'est préféré
TRUE -> z # ça marche mais c'est bizarre

# LES BOUCLES
# Il y a les boucles for :
for (i in 1:4) {
  print(i)
}
# Il y a les boucles while :
a <- 10
while (a > 4) {
    cat(a, "...", sep = "")
    a <- a - 1
}
# Gardez à l'esprit que les boucles for et while s'exécutent lentement
# en R.
# Des opérations sur la totalité d'un vecteur (ex une ligne entière,
# une colonne entière),
# ou les fonctions de type apply() (nous en parlerons plus tard),
# sont préférées.

# IF/ELSE
# Encore une fois assez standard
if (4 > 3) {
    print("4 is greater than 3")
} else {
    print("4 is not greater than 3")
}
# =>
# [1] "4 is greater than 3"

# LES FONCTIONS
# se définissent comme ceci :
jiggle <- function(x) {
    x = x + rnorm(1, sd=.1)	# ajoute un peu de bruit (contrôlé)
    return(x)
}
# Appelées comme n'importe quelles autres fonction R :
jiggle(5)	# 5±ε. After set.seed(2716057), jiggle(5)==5.005043



##########################################################################
# Les structures de données : les vecteurs, les matrices,
# les data frames et les tableaux
##########################################################################

# À UNE DIMENSION

# Commençons par le tout début, et avec quelque chose que
# vous connaissez déjà : les vecteurs.
vec <- c(8, 9, 10, 11)
vec	#  8  9 10 11
# Nous demandons des éléments spécifiques en les mettant entre crochets
# (Notez que R commence à compter à partir de 1)
vec[1]		# 8
letters[18]	# "r"
LETTERS[13]	# "M"
month.name[9]	# "September"
c(6, 8, 7, 5, 3, 0, 9)[3]	# 7
# Nous pouvons également rechercher des indices de composants spécifiques,
which(vec %% 2 == 0)	# 1 3
# Récupèrer seulement les premières ou dernières entrées du vecteur,
head(vec, 1)	# 8
tail(vec, 2)	# 10 11
# ou vérifier si un certaine valeur est dans le vecteur
any(vec == 10) # TRUE
# Si un index "dépasse" vous obtiendrez NA :
vec[6]	# NA
# Vous pouvez trouver la longueur de votre vecteur avec length()
length(vec)	# 4
# Vous pouvez réaliser des opérations sur des vecteurs entiers ou des
# sous-ensembles de vecteurs
vec * 4	# 16 20 24 28
vec[2:3] * 5	# 25 30
any(vec[2:3] == 8) # FALSE
# Et R a beaucoup de méthodes statistiques pré-construites pour les vecteurs :
mean(vec)	# 9.5
var(vec)	# 1.666667
sd(vec)		# 1.290994
max(vec)	# 11
min(vec)	# 8
sum(vec)	# 38
# Quelques fonctions préconstruites sympas supplémentaires :
5:15	# 5  6  7  8  9 10 11 12 13 14 15
seq(from=0, to=31337, by=1337)
# =>
#  [1]     0  1337  2674  4011  5348  6685  8022  9359 10696 12033 13370 14707
# [13] 16044 17381 18718 20055 21392 22729 24066 25403 26740 28077 29414 30751

# À DEUX DIMENSIONS (TOUT DANS UNE CLASSE)

# Vous pouvez créer une matrice à partir d'entrées du même type comme ceci :
mat <- matrix(nrow = 3, ncol = 2, c(1,2,3,4,5,6))
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# Différemment du vecteur, la classe d'une matrice est "matrix",
# peut importe ce qu'elle contient
class(mat) # => "matrix"
# Récupérer la première ligne
mat[1,]	# 1 4
# Réaliser une opération sur la première colonne
3 * mat[,1]	# 3 6 9
# Demander une cellule spécifique
mat[3,2]	# 6

# Transposer la matrice entière
t(mat)
# =>
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6

# La multiplication de matrices
mat %*% t(mat)
# =>
#      [,1] [,2] [,3]
# [1,]   17   22   27
# [2,]   22   29   36
# [3,]   27   36   45

# cbind() colle des vecteurs ensemble en colonne pour faire une matrice
mat2 <- cbind(1:4, c("dog", "cat", "bird", "dog"))
mat2
# =>
#      [,1] [,2]
# [1,] "1"  "dog"
# [2,] "2"  "cat"
# [3,] "3"  "bird"
# [4,] "4"  "dog"
class(mat2)	# matrix
# Encore une fois regardez ce qui se passe !
# Parce que les matrices peuvent contenir des entrées de toutes sortes de
# classes, tout sera converti en classe caractère
c(class(mat2[,1]), class(mat2[,2]))

# rbind() colle des vecteurs ensemble par lignes pour faire une matrice
mat3 <- rbind(c(1,2,4,5), c(6,7,0,4))
mat3
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    4    5
# [2,]    6    7    0    4
# Ah, tout de la même classe. Pas de coercitions. Beaucoup mieux.

# À DEUX DIMENSIONS (DE CLASSES DIFFÉRENTES)

# Pour des colonnes de différents types, utiliser une data frame
# Cette structure de données est si utile pour la programmation statistique,
# qu'une version a été ajoutée à Python dans le paquet "pandas".

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
# Trouver les dimensions
nrow(students)	# 6
ncol(students)	# 3
dim(students)	# 6 3
# La fonction data.frame() convertit les vecteurs caractères en vecteurs de
# facteurs par défaut; désactiver cette fonction en règlant
# stringsAsFactors = FALSE quand vous créer la data.frame
?data.frame

# Il y a plusieurs façons de subdiviser les data frames,
# toutes subtilement différentes
students$year	# 3  2  2  1  0 -1
students[,2]	# 3  2  2  1  0 -1
students[,"year"]	# 3  2  2  1  0 -1

# Une version améliorée de la structure data.frame est data.table.
# Si vous travaillez avec des données volumineuses ou des panels, ou avez
# besoin de fusionner quelques ensembles de données, data.table peut être
# un bon choix. Ici un tour éclair :
install.packages("data.table") # télécharge le paquet depuis CRAN
require(data.table) # le charge
students <- as.data.table(students)
students # regardez la différence à l'impression
# =>
#      name year house
# 1: Cedric    3     H
# 2:   Fred    2     G
# 3: George    2     G
# 4:    Cho    1     R
# 5:  Draco    0     S
# 6:  Ginny   -1     G
students[name=="Ginny"] # obtiens les lignes avec name == "Ginny"
# =>
#     name year house
# 1: Ginny   -1     G
students[year==2] # obtiens les lignes avec year == 2
# =>
#      name year house
# 1:   Fred    2     G
# 2: George    2     G
# data.table facilite la fusion entre deux ensembles de données
# Faisons une autre data.table pour fusionner students
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
students <- founders[students] # merge les deux ensembles de données qui matchent "house"
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

# data.table facilite le résumé des tableaux
students[,sum(year),by=house]
# =>
#    house V1
# 1:     G  3
# 2:     H  3
# 3:     R  1
# 4:     S  0

# Pour supprimer une colonne d'une data.frame ou data.table,
# assignez-lui la valeur NULL
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

# Supprimer une ligne en subdivisant
# En utilisant data.table :
students[studentName != "Draco"]
# =>
#    house studentName year
# 1:     G        Fred    2
# 2:     G      George    2
# 3:     G       Ginny   -1
# 4:     H      Cedric    3
# 5:     R         Cho    1
# En utilisant data.frame :
students <- as.data.frame(students)
students[students$house != "G",]
# =>
#   house houseFounderName studentName year
# 4     H            Helga      Cedric    3
# 5     R           Rowena         Cho    1
# 6     S          Salazar       Draco    0

# MULTI-DIMENSIONNELLE (TOUS ÉLÉMENTS D'UN TYPE)

# Les arrays créent des tableaux de n dimensions.
# Tous les éléments doivent être du même type.
# Vous pouvez faire un tableau à 2 dimensions (une sorte de matrice)
array(c(c(1,2,4,5),c(8,9,3,6)), dim=c(2,4))
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    4    8    3
# [2,]    2    5    9    6
# Vous pouvez aussi utiliser array pour faire des matrices à 3 dimensions :
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

# LES LISTES (MULTI-DIMENSIONNELLES, ÉVENTUELLEMMENT DÉCHIRÉES,
# DE DIFFÉRENTS TYPES)

# Enfin, R a des listes (de vecteurs)
list1 <- list(time = 1:40)
list1$price = c(rnorm(40,.5*list1$time,4)) # random
list1
# Vous pouvez obtenir des éléments de la liste comme ceci
list1$time # une façon
list1[["time"]] # une autre façon
list1[[1]] # encore une façon différente
# =>
#  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33
# [34] 34 35 36 37 38 39 40
# Vous pouvez subdiviser les éléments d'une liste comme n'importe quel vecteur
list1$price[4]

# Les listes ne sont pas les structures de données les plus efficaces
# à utiliser avec R ;
# À moins d'avoir une très bonne raison, vous devriez utiliser data.frames
# Les listes sont souvent retournées par des fonctions qui effectuent
# des régressions linéaires.

##########################################
# La famille de fonction apply()
##########################################

# Vous vous rappelez mat ?
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# Utilisez apply(X, MARGIN, FUN) pour appliquer la fonction FUN à la matrice X
# sur les lignes (MAR = 1) ou les colonnes (MAR = 2)
# R exécute FUN à chaque lignes (ou colonnes) de X, beaucoup plus rapidement
# que le ferait une boucle for ou while
apply(mat, MAR = 2, jiggle)
# =>
#      [,1] [,2]
# [1,]    3   15
# [2,]    7   19
# [3,]   11   23
# D'autres fonctions : ?lapply, ?sapply

# Ne soyez pas trop intimidé ; tout le monde reconnaît que c'est un peu déroutant

# Le paquet plyr vise à remplacer (et améliorer !) la famille *apply().
install.packages("plyr")
require(plyr)
?plyr



############################
# Charger des données
############################

# "pets.csv" est un fichier sur internet
# (mais il pourrait être tout aussi facilement sur votre ordinateur)
pets <- read.csv("http://learnxinyminutes.com/docs/pets.csv")
pets
head(pets, 2) # first two rows
tail(pets, 1) # last row

# Pour sauvegarder une data frame ou une matrice en fichier .csv
write.csv(pets, "pets2.csv") # to make a new .csv file
# définir le répertoire de travail avec setwd(), le récupérer avec getwd()

# Essayez ?read.csv et ?write.csv pour plus d'informations



################
# Les tracés
################

# LES FONCTIONS DE TRACÉ PRÉCONSTRUITES
# Les diagrammes de dispersion !
plot(list1$time, list1$price, main = "fake data")
# Les régressions !
linearModel <- lm(price  ~ time, data = list1)
linearModel # sort le résultat de la régression
# Tracer une ligne de regression sur une tracé existant
abline(linearModel, col = "red")
# Obtenir une variété de diagnostiques sympas
plot(linearModel)
# Les histogrammes !
hist(rpois(n = 10000, lambda = 5), col = "thistle")
# Les diagrammes en bâtons !
barplot(c(1,4,5,1,2), names.arg = c("red","blue","purple","green","yellow"))

# GGPLOT2
# Mais ceux-ci ne sont même pas les plus jolis tracés de R
# Essayez le paquet ggplot2 pour d'avantages de graphiques
install.packages("ggplot2")
require(ggplot2)
?ggplot2
pp <- ggplot(students, aes(x=house))
pp + geom_histogram()
ll <- as.data.table(list1)
pp <- ggplot(ll, aes(x=time,price))
pp + geom_point()
# ggplot2 a une documentation excellente
#(disponible sur http://docs.ggplot2.org/current/)



```

## Comment obtenir R ?

* Obtiens R et R GUI depuis [http://www.r-project.org/](http://www.r-project.org/)
* [RStudio](http://www.rstudio.com/ide/) est un autre GUI
