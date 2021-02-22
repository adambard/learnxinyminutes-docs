---
category: tool
tool: make
contributors:
    - ["Robert Steed", "https://github.com/robochat"]
translators:
    - ["altaris", "https://github.com/altaris"]
filename: Makefile-fr
lang: fr-fr
---

Un makefile est un fichier qui définit un ensemble de règles liées entre elles
pour créer une ou plusieurs cibles. L'idée est d'effectuer le moins de travail
possible afin de mettre à jour la ou les cibles en fonction des dépendances.

Écrit en un week-end par Stuart Feldman en 1976, le make et les
makefiles sont encore très utilisés (principalement dans les systèmes Unix),
malgré la concurrence et les critiques faites à son égard.

Le programme make a plusieurs variantes. Dans ce tutoriel, nous utiliserons
l'implémentation standard : GNU make.

```make

# Ceci est un commentaire.

# Un makefile devrait être nommé "Makefile" (avec ou sans la
# majuscule). Il peut alors être exécuté par `make <cible>`.
# Ce nommage n'est toutefois pas obligatoire : utiliser
# `make -f "fichier" <cible>`.

# ATTENTION : l'indentation est quant à elle obligatoire, et se fait avec des
# tabulations, pas avec des espaces !

#-----------------------------------------------------------------------
# Les basiques
#-----------------------------------------------------------------------

# Une règle. Elle ne sera exécutée que si fichier0.txt n'existe pas.
fichier0.txt:
	echo "truc" > fichier0.txt
	# Même les commentaires sont transférés dans le terminal.

# Cette règle ne sera exécutée que si fichier0.txt est plus récent que
# fichier1.txt.
fichier1.txt: fichier0.txt
	cat fichier0.txt > fichier1.txt
	# Utiliser la même syntaxe que dans un terminal.
	@cat fichier0.txt >> fichier1.txt
	# @ empêche l'affichage de la sortie texte d'une commande.
	-@echo 'hello'
	# - signifie que la règle devrait continuer à s'exécuter si cette commande
	# échoue.

# Une règle peut avoir plusieurs cibles et plusieurs dépendances.
fichier2.txt fichier3.txt: fichier0.txt fichier1.txt
	touch fichier2.txt
	touch fichier3.txt

# Make affichera un avertissement si le makefile comporte plusieurs règles pour
# une même cible. Cependant les règles vides ne comptent pas, et peuvent être
# utilisées pour ajouter des dépendances plus facilement.

#-----------------------------------------------------------------------
# Fausses règles
#-----------------------------------------------------------------------

# Une fausse règle est une règle qui ne correspond pas à un fichier.
# Par définition, elle ne peut pas être à jour, et donc make l’exécutera à
# chaque demande.
all: maker process

# La déclaration des règles peut être faite dans n'importe quel ordre.
maker:
	touch ex0.txt ex1.txt

# On peut transformer une règle en fausse règle grâce à la cible spéciale
# suivante :
.PHONY: all maker process

# Une règle dépendante d'une fausse règle sera toujours exécutée.
ex0.txt ex1.txt: maker

# Voici quelques exemples fréquents de fausses règles : all, make, clean,
# install...

#-----------------------------------------------------------------------
# Variables automatiques et wildcards
#-----------------------------------------------------------------------

# Utilise un wildcard pour des noms de fichier
process: fichier*.txt
	@echo $^    # $^ est une variable contenant la liste des dépendances de la
				# cible actuelle.
	@echo $@    # $@ est le nom de la cible actuelle. En cas de cibles
				# multiples, $@ est le nom de la cible ayant causé l'exécution
				# de cette règle.
	@echo $<    # $< contient la première dépendance.
	@echo $?    # $? contient la liste des dépendances qui ne sont pas à jour.
	@echo $+    # $+ contient la liste des dépendances avec d'éventuels
				# duplicatas, contrairement à $^.
	@echo $|    # $| contient la liste des cibles ayant préséance sur la cible
				# actuelle.

# Même si la définition de la règle est scindée en plusieurs morceaux, $^
# listera toutes les dépendances indiquées.
process: ex1.txt fichier0.txt
# Ici, fichier0.txt est un duplicata dans $+.

#-----------------------------------------------------------------------
# Pattern matching
#-----------------------------------------------------------------------

# En utilisant le pattern matching, on peut par exemple créer des règles pour
# convertir les fichiers d'un certain format dans un autre.
%.png: %.svg
	inkscape --export-png $^

# Make exécute une règle même si le fichier correspondant est situé dans un sous
# dossier. En cas de conflit, la règle avec la meilleure correspondance est
# choisie.
small/%.png: %.svg
	inkscape --export-png --export-dpi 30 $^

# Dans ce type de conflit (même cible, même dépendances), make exécutera la
# dernière règle déclarée...
%.png: %.svg
	@echo cette règle est choisie

# Dans ce type de conflit (même cible mais pas les mêmes dépendances), make
# exécutera la première règle pouvant être exécutée.
%.png: %.ps
	@echo cette règle n\'est pas choisie si *.svg et *.ps sont présents

# Make a des règles pré établies. Par exemple, il sait comment créer la cible
# *.o à partir de *.c.

# Les makefiles plus vieux utilisent un matching par extension de fichier.
.png.ps:
	@echo cette règle est similaire à une règle par pattern matching

# Utiliser cette règle spéciale pour déclarer une règle comme ayant un
# matching par extension de fichier.
.SUFFIXES: .png

#-----------------------------------------------------------------------
# Variables, ou macros
#-----------------------------------------------------------------------

# Les variables sont des chaînes de caractères.

variable = Ted
variable2="Sarah"

echo:
	@echo $(variable)
	@echo ${variable2}
	@echo $variable    # Cette syntaxe signifie $(n)ame et non pas $(variable) !
	@echo $(variable3) # Les variables non déclarées valent "" (chaîne vide).

# Les variables sont déclarées de 4 manières, de la plus grande priorité à la
# plus faible :
# 1 : dans la ligne de commande qui invoque make,
# 2 : dans le makefile,
# 3 : dans les variables d’environnement du terminal qui invoque make,
# 4 : les variables prédéfinies.

# Assigne la variable si une variable d’environnement du même nom n'existe pas
# déjà.
variable4 ?= Jean

# Empêche cette variable d'être modifiée par la ligne de commande.
override variable5 = David

# Concatène à une variable (avec un espace avant).
variable4 +=gris

# Assignations de variable pour les règles correspondant à un pattern
# (spécifique à GNU make).
*.png: variable2 = Sara # Pour toutes les règles correspondant à *.png, et tous
						# leurs descendants, la variable variable2 vaudra
						# "Sara".
# Si le jeux des dépendances et descendances devient vraiment trop compliqué,
# des incohérences peuvent survenir.

# Certaines variables sont prédéfinies par make :
affiche_predefinies:
	echo $(CC)
	echo ${CXX}
	echo $(FC)
	echo ${CFLAGS}
	echo $(CPPFLAGS)
	echo ${CXXFLAGS}
	echo $(LDFLAGS)
	echo ${LDLIBS}

#-----------------------------------------------------------------------
# Variables : le retour
#-----------------------------------------------------------------------

# Les variables sont évaluées à chaque instance, ce qui peut être coûteux en
# calculs. Pour parer à ce problème, il existe dans GNU make une seconde
# manière d'assigner des variables pour qu'elles ne soient évaluées qu'une seule
# fois seulement.

var := A B C
var2 ::=  $(var) D E F # := et ::= sont équivalents.

# Ces variables sont évaluées procéduralement (i.e. dans leur ordre
# d'apparition), contrairement aux règles par exemple !

# Ceci ne fonctionne pas.
var3 ::= $(var4) et fais de beaux rêves
var4 ::= bonne nuit

#-----------------------------------------------------------------------
# Fonctions
#-----------------------------------------------------------------------

# Make a une multitude de fonctions. La syntaxe générale est
# $(fonction arg0,arg1,arg2...).

# Quelques exemples :

fichiers_source = $(wildcard *.c */*.c)
fichiers_objet  = $(patsubst %.c,%.o,$(fichiers_source))

ls: * src/*
	@echo $(filter %.txt, $^)
	@echo $(notdir $^)
	@echo $(join $(dir $^),$(notdir $^))

#-----------------------------------------------------------------------
# Directives
#-----------------------------------------------------------------------

# Inclut d'autres makefiles.
include meuh.mk

# Branchements conditionnels.
sport = tennis
report:
ifeq ($(sport),tennis)                  # Il y a aussi ifneq.
	@echo 'jeu, set et match'
else
	@echo "C'est pas ici Wimbledon ?"
endif

truc = true
ifdef $(truc)                           # Il y a aussi ifndef.
	machin = 'salut'
endif
```

## Quelques références

### En français

+ [Introduction à Makefile (developpez.com)]
(http://gl.developpez.com/tutoriel/outil/makefile/),
+ [Compilez sous GNU/Linux ! (openclassrooms)]
(https://openclassrooms.com/courses/compilez-sous-gnu-linux).

### En anglais

+ [Documentation de GNU make](https://www.gnu.org/software/make/manual/),
+ [Software carpentry tutorial](http://swcarpentry.github.io/make-novice/),
+ Learn C the hard way [ex2](http://c.learncodethehardway.org/book/ex2.html)
[ex28](http://c.learncodethehardway.org/book/ex28.html).
