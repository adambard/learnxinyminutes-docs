---
category: tool
tool: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
    - ["Denis Arh", "https://github.com/darh"]
    - ["akirahirose", "https://twitter.com/akirahirose"]
    - ["Anton Strömkvist", "http://lutic.org/"]
    - ["Rahil Momin", "https://github.com/iamrahil"]
    - ["Gregrory Kielian", "https://github.com/gskielian"]
translators:
    - ["Baptiste Fontaine", "http://bfontaine.net"]
filename: LearnBash-fr.sh
lang: fr-fr
---

Bash est le nom du shell UNIX, qui était aussi distribué avec le système
d’exploitation GNU et est le shell par défaut sur Linux et macOS.

Presque tous les exemples ci-dessous peuvent être écrits dans un script shell
ou exécutés directement dans le terminal.

[Plus d’informations ici.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# La première ligne du script s’appelle le « shebang », elle indique au système
# comment exécuter le script : http://fr.wikipedia.org/wiki/Shebang
# Comme vous pouvez le remarquer, les commentaires commencent par un « # ». 
# Le shebang est donc aussi un commentaire

# Un exemple simple qui affiche « Hello world! » :
echo Hello world!

# Chaque commande commence sur une nouvelle ligne ou après un point-virgule :
echo 'Ceci est la première ligne'; echo 'Ceci est la seconde ligne'

# La déclaration d’une variable ressemble à ça :
VARIABLE="Du texte"

# Mais pas comme ça :
VARIABLE = "Du texte" # => renvoie une erreur : "Variable: command not found"
# Bash va penser que VARIABLE est une commande qu’il doit exécuter et va
# afficher une erreur parce qu’elle est introuvable.

# Ni comme ça :
VARIABLE= 'Some string' # => renvoie une erreur : "Du texte: command not found"
# Bash va penser que 'Di texte' est une commande qu’il doit exécuter et va
# afficher une erreur parce qu’elle est introuvable. (Dans ce cas, la partie 
# 'VARIABLE=' est considérée comme une affectation de variable valable uniquement
# pour la portée de la commande 'Du texte').

# Utiliser une variable :
echo $VARIABLE # => Du texte
echo "$VARIABLE" # => Du texte
echo '$VARIABLE' # => $VARIABLE
# Quand vous utilisez la variable en elle-même – en lui assignant une valeur,
# en l’exportant ou autre – vous écrivez son nom sans $. Si vous voulez
# utiliser sa valeur, vous devez utiliser $.
# Notez qu’entourer une variable de deux guillemets simples (') empêche
# l’expansion des variables !

# Expansion des paramètres (Parameter expansion) ${ } :
echo ${VARIABLE} # => Du texte
# Ceci est une utilisation simple de l'expansion de paramètre.
# L'expansion de paramètre récupère la valeur d'une variable.
# Elle « déploie » ou imprime la valeur.
# Lors de l'expansion, la valeur ou le paramètre peut être modifié.
# Voici d'autres modifications qui s'ajoutent à cette expansion.

# Substitution de chaîne de caractères dans une variable
echo ${VARIABLE/Some/A}
# Ceci va remplacer la première occurrence de « Some » par « A »

# Sous-chaîne d’une variable
LONGUEUR=5
echo ${VARIABLE:0:LONGUEUR} # => Du te
# Ceci va retourner les 5 premiers caractères de la valeur
echo ${VARIABLE:LONGUEUR} # => xte
# Ceci va retourner la valeur sans les 5 premiers caractères
echo ${VARIABLE: -4} # => exte
# Ceci va retourner les 4 derniers caractères de la valeur
# (l'espace avant -4 est nécessaire)

# Longueur d'une chaîne
echo ${#VARIABLE} # => 8

# Expansion indirecte
AUTRE_VARIABLE="VARIABLE"
echo ${!AUTRE_VARIABLE} # => Du texte
# Cela va utiliser la valeur comme un nom de variable

# Valeur par défaut d’une variable
echo ${FOO:-"ValeurParDefaut"}
# Retourne 'ValeurParDefaut' si FOO n'a pas été déclarée, si
# FOO est null (FOO=) ou si FOO est une chaîne vide (FOO="").
# En revanche Zéro (FOO=0) retourne 0

# Les tableaux (array)

array0=(un deux trois quatre cinq six)
# Déclarer un tableau de 6 éléments  

echo $array0 # => "un"
# Affiche le premier élément

echo ${array0[0]} # => "un"
# Affiche le premier élément

echo ${array0[@]} # => "un deux trois quatre cinq six"
# Affiche tous les éléments

echo ${#array0[@]} # => "6"
# Attiche le nombre d'éléments

echo ${#array0[2]} # => "5"
# Affiche le nombre de caractères du troisième élément

echo ${array0[@]:3:2} # => "quatre cinq"
# Affiche 2 element en à partir du quatrième

for i in "${array0[@]}"; do
    echo "$i"
done
# Affiche tous éléments, chacun sur un nouvelle ligne

# Brace Expansion { }
# Permet de générer des tableau
echo {1..12} # => 1 2 3 4 5 6 7 8 9 10 11 12
echo {100..95} # => 100 99 98 97 96 95
echo {a..z} # => a b c d e f g h i j k l m n o p q r s t u v w x y z

# Variables pré-remplies :
# Il y a quelques variables pré-remplies utiles, comme :
echo "La valeur de retour du dernier programme : $?"
echo "Le PID du script : $$"
echo "Nombre d’arguments : $#"
echo "Arguments du script : $@"
echo "Arguments du script séparés en plusieurs variables : $1 $2..."

# Maintenant que nous savons comment utiliser et afficher des variables,
# apprenons d'autres fonctionnalités basiques de bash !

# Notre répertoire courant est accessible par la commande `pwd`.
# `pwd` signifie « imprimer le répertoire de travail ».
# On peut aussi utiliser la variable intégrée `$PWD`.
# Observez que les commandes suivantes sont équivalentes :
echo "Je suis dans $(pwd)" # exécute `pwd` et interpole la sortie
echo "Je suis dans $PWD" # interprète la variable

# Si vous avez trop de données dans votre terminal, la commande
# `clear` efface votre écran
clear
# Ctrl-L fonctionne aussi pour effacer la sortie.

# Lire une valeur depuis l’entrée standard :
echo "Quel est votre nom ?"
read NAME # Notez que l’on n’a pas eu à déclarer une nouvelle variable
echo Bonjour, $NAME!

# Nous avons l’habituelle structure « if » :
# Utilisez 'man test' pour plus d’informations à propos des conditions
if [ $NAME != $USER ] # remarque : $USER est le nom l'utilisateur en cours.
then
    echo "Votre nom n’est pas votre pseudo"
else
    echo "Votre nom est votre pseudo"
fi

# Remarque : si $NAME est vide, bash verra la condition précédente comme :
if [ != $USER ]
# ce qui est syntaxiquement invalide
# la bonne façon d'utiliser une variable potentiellement vide en bash est :
if [ "$NAME" != $USER ]
# ce qui sera interprété par bash si $NAME est vide comme :
if [ "" != $USER ]

# Il y a aussi l’exécution conditionnelle
echo "Toujours exécuté" || echo "Exécuté si la première commande ne réussit pas"
# Toujours exécuté
echo "Toujours exécuté" && echo "Exécuté si la première commande réussit"
# Toujours exécuté
# Exécuté si la première commande réussit

# Pour utiliser && et || avec des commandes « if, » vous devez utiliser
# plusieurs paires de crochets :
if [ $NAME == "Steve" ] && [ $AGE -eq 15 ]
then
    echo "Ceci sera exécuté si $NAME est Steve ET $AGE est 15."
fi

if [ $NAME == "Daniya" ] || [ $NAME == "Zach" ]
then
    echo "Ceci sera exécuté si $NAME est Daniya OU Zach."
fi

# Les expressions sont écrites dans le format suivant :
echo $(( 10 + 5 ))

# Contrairement aux autres langages de programmation, Bash est un shell — il
# est donc exécuté dans le contexte du répertoire courant. Vous pouvez lister
# les fichiers et dossiers dans le répertoire courant avec la commande `ls` :
ls

# Ces commandes ont des options qui contrôlent leur exécution :
ls -l # Liste tous les fichiers et répertoires sur des lignes séparées

# Les résultat de la commande précédente peuvent être passés à la commande
# suivante en entrée.
# La commande grep filtre l’entrée avec les motifs donnés. On peut ainsi lister
# les fichiers .txt dans le répertoire courant :
ls -l | grep "\.txt"

# Vous pouvez aussi rediriger l’entrée et les sorties standards et d’erreur
# d’une commande :
python2 hello.py < "entrée.in"
python2 hello.py > "sortie.out"
python2 hello.py 2> "erreur.err"
# Ceci va écraser le fichier s'il existe; si vous préférez écrire à la fin de
# celui-ci, utilisez >> à la place.

# Les commandes peuvent se substituer à l’intérieur d’autres commandes en
# utilisant $( ) :
# La commande ci-dessous affiche le nombre de fichiers et répertoires dans le
# répertoire courant :
echo "Il y a $(ls | wc -l) choses ici."

# On peut faire la même chose avec les accents graves `` mais on ne peut pas
# les imbriquer — la façon la plus courante est d’utiliser $( ).
echo "There are `ls | wc -l` items here."

# Bash a une commande case qui marche de façon similaire au switch de Java et
# C++ :
case "$VARIABLE" in
    #List patterns for the conditions you want to meet
    0) echo "There is a zero.";;
    1) echo "There is a one.";;
    *) echo "It is not null.";;
esac

# La boucle for itère autant de fois qu’elle a d’arguments :
# Le contenu de $VARIABLE est affiché trois fois.
for VARIABLE in {1..3}
do
    echo "$VARIABLE"
done

# Ou écrivez-la de façon « traditionnelle » :
for ((a=1; a <= 3; a++))
do
    echo $a
done

# Elles peuvent aussi être utilisées pour agir sur des fichiers :
# Cette boucle va exécuter la commande 'cat' sur fichier1 et fichier2
for VARIABLE in fichier1 fichier2
do
    cat "$VARIABLE"
done

# …ou la sortie d’une commande :
# Ceci va afficher la sortie de ls.
for OUTPUT in $(ls)
do
    cat "$OUTPUT"
done

# Boucle while :
while [ true ]
do
    echo "corps de la boucle ..."
    break
done

# Vous pouvez aussi définir des fonctions
# Définition :
function foo ()
{
    echo "Les arguments fonctionnent comme les arguments de script : $@"
    echo "Et : $1 $2..."
    echo "Ceci est une fonction"
    return 0
}

# Ou plus simplement :
bar ()
{
    echo "Une autre façon de définir des fonctions !"
    return 0
}

# Appeler votre fonction
foo "Mon nom est" $NAME

# Il y a plein de commandes utiles que vous devriez apprendre :
# affiche les 10 dernières lignes de fichier.txt
tail -n 10 fichier.txt
# affiche les 10 premières lignes de fichier.txt
head -n 10 fichier.txt
# trie les lignes de fichier.txt
sort fichier.txt
# montre ou omet les lignes répétées, avec -d pour les montrer
uniq -d fichier.txt
# affiche uniquement la première colonne avant le caractère « , »
cut -d ',' -f 1 fichier.txt
# remplace chaque occurrence de 'okay' par 'super' dans fichier.txt
# (compatible avec les expression rationnelles)
sed -i 's/okay/super/g' fichier.txt
# affiche toutes les lignes de fichier.txt qui correspondent à une expression
# rationnelle, dans cet exemple les lignes qui commencent par « foo » et
# finissent par « bar »
grep "^foo.*bar$" fichier.txt
# ajoutez l’option « -c » pour afficher le nombre de lignes concernées
grep -c "^foo.*bar$" fichier.txt
# Si vous voulez vraiment chercher une chaîne de caractères, et non
# l’expression rationnelle, utilisez fgrep (ou grep -F)
fgrep "^foo.*bar$" fichier.txt
```
