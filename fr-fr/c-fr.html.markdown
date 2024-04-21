---
language: C
filename: learnc-fr.c
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
    - ["Cyril Jovet", "https://twitter.com/CyrilJovet"]
lang: fr-fr
---

Le C est le langage de plus bas niveau que la plupart des programmeurs seront
amenés à utiliser, mais ceci est largement conpensé par sa vitesse brute.

> **À propos des options de compilation**
>
> Par défaut, gcc et clang sont assez silencieux sur les avertissements et
> les erreurs de compilation, qui peuvent être des informations très utiles.
> L'utilisation explicite d'options de compilation plus strictes est recommandée.
> Voici quelques valeurs par défaut recommandées:
>
> `-Wall -Wextra -Werror -O2 -std=c99 -pedantic`
>
> Pour plus d'informations sur ce que font ces options ainsi que sur d'autres,
> vous pouvez consulter la page du manuel de votre compilateur C (par exemple `man 1 gcc`)
> ou recherchez simplement en ligne.

```c
// Les commentaires sur une ligne commencent par // - valable seulement pour C99 et plus tard.

/*
Les commentaires multilignes resemblent à ceci. Ils restent valables en C89.
*/

/*
Les commentaires multilignes ne s'emboîtent pas /* Attention */  // Le commentaire se termine sur cette ligne...
*/ // ...pas ici !

// Constante : #define <nom>
// Les constantes sont écrites en majuscules par convention, pas d'obligation
#define DAYS_IN_YEAR 365

// Les constantes d'énumeration sont aussi une façon de déclarer des valeurs.
// Toutes les instructions doivent se terminer par un point-virgule.
enum days {SUN = 1, MON, TUE, WED, THU, FRI, SAT};
// MON vaut 2 automatiquement, TUE vaut 3, etc.

// Import de fichiers d'en-tête avec #include
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// Pour vos propres fichiers d'en-tête, utilisez les doubles quotes au lieu des crochets sup. et inf.:
//#include "my_header.h"

// Declarez les signatures des fonctions auparavant dans un fichier .h, ou en haut de votre
// fichier .c.
void function_1();
int function_2(void);

// Si vous voulez appeler une fonction avant de la définir,
// vous pouvez déclarer son prototype
// (types des arguments et de la valeur renvoyée)
int add_two_ints(int x1, int x2); // prototype de la fonction
// bien que `int add_two_ints (int, int);` soit également valide (pas besoin de nommer les arguments),
// il est recommandé de nommer les arguments dans le prototype pour faciliter l'inspection.

// Le point d'entrée de votre programme est une fonction appélée
// main avec une sortie de type entier.
int main(void) {
  // votre programme
}

// Les arguments de la ligne de commande utilisés pour exécuter votre programme sont également passés à la fonction main
// argc étant le nombre d'arguments - le nom de votre programme compte pour 1
// argv est un tableau de tableaux de caractères - contenant les arguments eux-mêmes
// argv[0] = nom de votre programme, argv[1] = premier argument, etc.
int main (int argc, char** argv)
{
  // écriture en sortie à l'aide de printf, pour "print formatted"
  // %d est un entier, \n est une nouvelle ligne
  printf("%d\n", 0); // => Prints 0

  ///////////////////////////////////////
  // Types
  ///////////////////////////////////////

  // Les compilateurs qui ne sont pas conformes C99 nécessitent de déclarer
  // les variables en début de portée du bloc de code courant.
  // Les compilateurs qui SONT conformes C99 acceptent les déclarations
  // plus tard dans les fonctions.

  // Les entiers font généralement de 4 octets
  int x_int = 0;

  // Les entiers courts font généralement 2 octets
  short x_short = 0;

  // Les caractères ont une taille fixée à 1 octet
  char x_char = 0;
  char y_char = 'y'; // les caractères littéraux sont entre apostrophes ''

  // Les entiers longs font généralement 4 à 8 octets; les entiers longs de type long font
  // 8 octets
  long x_long = 0;
  long long x_long_long = 0;

  // les nombres à virgule sont généralement des nombres à virgule flottantes de 32 bits
  float x_float = 0.0f; // 'f' est le suffixe qui indique ici le littéral à virgule flottante

  // les nombres de type double sont généralement des nombres à virgule flottante de 64 bits
  double x_double = 0.0; // les nombres réels sans suffixe sont de type double

  // Les types entier peuvent être non signés (plus grand ou égal à zero)
  unsigned short ux_short;
  unsigned int ux_int;
  unsigned long long ux_long_long;

  // Les caractères entre guillemets simples sont des entiers d'un jeu de caractères machine.
  '0'; // => 48 dans le jeu de caractères ASCII.
  'A'; // => 65 dans le jeu de caractères ASCII.

  // sizeof(T) vous rend la taille d'une variable de type T en octets.
  // sizeof obj revoie la taille en sortie de l'expression (variable, littéral, etc.).
  printf("%zu\n", sizeof(int)); // => 4 (sur la plupart des machines les mots font 4 octets)

  // Si l'argument de l'opérateur `sizeof` est une expression, alors son argument
  // n'est pas évalué (sauf les VLA - voir ci-dessous).
  // La valeur qu'elle donne dans ce cas est une constante évaluée à la compilation.
  int a = 1;
  // size_t est un type entier non signé d'au moins 2 octets utilisé pour représenter
  // la taille d'un objet.
  size_t size = sizeof(a++); // a++ n'est pas évalué
  printf("sizeof(a++) = %zu où a = %d\n", size, a);
  // affiche "sizeof(a++) = 4 où a = 1" (sur une architecture 32 bits)

  // Si l'argument de l'opérateur `sizeof` est une expression, alors son argument
  // n'est pas évalué (sauf les VLAs (voir ci-dessous)).
  // La valeur qu'elle donne dans ce cas est une constante évaluée au moment de la compilation.
  int a = 1;
  // size_t est un nombre de type entier non signé dont au moins 2 octets sont utilisés pour représenter
  // la taille d'un objet.
  size_t size = sizeof(a++); // a++ n'est pas évalué
  printf("sizeof(a++) = %zu where a = %d\n", size, a);
  // affiche "sizeof(a++) = 4 where a = 1" (sur une architecture 32-bit)

  // Les tableaux doivent être initialisés avec une taille concrète.
  char my_char_array[20]; // Ce tableau occupe 1 * 20 = 20 octets
  int my_int_array[20]; // Ce tableau occupe 4 * 20 = 80 octets
  // (en considérant des mots de 4 octets)

  // Vous pouvez ainsi initialiser un tableau à 0:
  char my_array[20] = {0};
  // où la partie "{0}" est appelée "initialiseur de tableau".
  // NOTEZ que vous vous en sortez sans déclarer explicitement la taille du tableau,
  // SI vous initialisez le tableau sur la même ligne. Ainsi, la déclaration suivante
  // est équivalent:
  char my_array[] = {0};
  // MAIS, alors vous devez évaluer la taille du tableau au moment de l'exécution, comme ceci:
  size_t my_array_size = sizeof(my_array) / sizeof(my_array[0]);
  // ATTENTION Si vous adoptez cette approche, vous devez évaluer la taille *avant*
  // qui vous commenciez à transmettre le tableau à la fonction (voir la discussion ultérieure), car
  // les tableaux sont "rétrogradés" en pointeurs bruts lorsqu'ils sont passés à des fonctions
  // (donc l'instruction ci-dessus produira le mauvais résultat à l'intérieur de la fonction).

  // L'indexation commence à zero et utilise []
  my_array[0]; // => 0

  // Les tableaux sont modifiables ; c'est juste de la mémoire!
  my_array[1] = 2;
  printf("%d\n", my_array[1]); // => 2

  // En C99 (et optionnellement en C11), des tableaux à taille variable (VLAs)
  // peuvent également être déclarés. La taille d'un tel tableau n'a pas besoin d'être
  // une constante définie lors de la compilation:
  printf("Enter the array size: "); // demande à l'utilisateur la taille du tableau
  int array_size;
  fscanf(stdin, "%d", &array_size);
  int var_length_array[array_size]; // déclare le VLA
  printf("sizeof array = %zu\n", sizeof var_length_array);

  // Exemple:
  // > Enter the array size: 10
  // > sizeof array = 40

  // Les chaînes de caractères sont juste des tableaux de caractères de terminant par octet NULL (0x00),
  // représenté dans les chaînes de caractères par le caractère spécial '\0'.
  // (Nous n'avons pas besoin d'ajouter l'octet NULL dans les chaînes de caractères littérales; le compilateur
  // l'ajoute à la fin du tableau pour nous.)
  char a_string[20] = "This is a string";
  printf("%s\n", a_string); // %s insère une chaîne de caractères

  printf("%d\n", a_string[16]); // => 0
  // i.e., l'octet #17 est 0 (comme le sont 18, 19, et 20)

  // Si nous avons un caractère entre apostrophes, c'est un caractère littéral.
  // Ils sont de types `int`, et *non* `char` (pour des raisons historiques).
  int cha = 'a'; // bon
  char chb = 'a'; // bon aussi (conversion implicite de int vers char)

  // Tableaux dimension multiple:
  int multi_array[2][5] = {
    {1, 2, 3, 4, 5},
    {6, 7, 8, 9, 0}
  };
  // Accés au éléments:
  int array_int = multi_array[0][2]; // => 3

  ///////////////////////////////////////
  // Operateurs
  ///////////////////////////////////////

  // Raccourcis pour plusieurs déclarations:
  int i1 = 1, i2 = 2;
  float f1 = 1.0, f2 = 2.0;

  int b, c;
  b = c = 0;

  // Arithmétique
  i1 + i2; // => 3
  i2 - i1; // => 1
  i2 * i1; // => 2
  i1 / i2; // => 0 (0.5, mais tronqué après 0)

  // Vous devez convertir un entier en type 'float' pour obtenir un résultat en virgule flottante
  (float)i1 / i2; // => 0.5f
  i1 / (double)i2; // => 0.5 // Pareil avec double
  f1 / f2; // => 0.5, plus ou moins epsilon
  // Les nombres à virgule flottante et leurs calculs ne sont pas exacts

  // Modulo est aussi là
  11 % 3; // => 2

  // Les opérateurs de comparaison sont probablement familiers, mais
  // il n'y a pas de type booléen en C. On utilise plutôt des entiers
  // (C99 introduit _Bool ou bool).
  // 0 est faux, tout le reste est vrai.
  // (La comparaison les opérateurs donnent toujours 0 ou 1)
  3 == 2; // => 0 (faux)
  3 != 2; // => 1 (vrai)
  3 > 2; // => 1
  3 < 2; // => 0
  2 <= 2; // => 1
  2 >= 2; // => 1

  // C n'est pas Python - les comparaisons ne s'enchaînent pas.
  // Attention : la ligne ci-dessous se compilera, mais cela signifie `(0 < a) <2`.
  // Cette expression est toujours vraie, car (0 < a) peut être 1 ou 0.
  // Dans ce cas, c'est 1, car (0 < 1).
  int between_0_and_2 = 0 < a < 2;
  // Utilisez plutôt:
  int between_0_and_2 = 0 < a && a < 2;

  // La logique fonctionne avec les entiers
  !3; // => 0 (non logique)
  !0; // => 1
  1 && 1; // => 1 (et logique)
  0 && 1; // => 0
  0 || 1; // => 1 (ou logique)
  0 || 0; // => 0

  // Expression conditionnelle ternaire ( ? : )
  int e = 5;
  int f = 10;
  int z;
  z = (e > f) ? e : f; // => 10 "if e > f return e, else return f."

  // Opérateurs d'incrémentation et de décrémentation :
  int j = 0;
  int s = j++; // Renvoi j PUIS augmente j. (s = 0, j = 1)
  s = ++j; // Augmente j PUIS revoi j. (s = 2, j = 2)
  // pareil avec j-- et --j

  // Opérateurs de manipulation des bits !
  ~0x0F; // => 0xFFFFFFF0 (opérateur de négation, "complémentaire de 1", exemple d'un résultat pour un entier 32-bit)
  0x0F & 0xF0; // => 0x00 (opérateur AND)
  0x0F | 0xF0; // => 0xFF (opérateur OR)
  0x04 ^ 0x0F; // => 0x0B (opérateur XOR)
  0x01 << 1; // => 0x02 (opérateur de décalage à gauche - de 1)
  0x02 >> 1; // => 0x01 (opérateur de décalage à droite - de 1)

  // Soyez prudent lorsque vous effectuez un décalage sur des entiers signés - les éléments suivants ne sont pas définis:
  // - décalage du bit de signe d'un entier signé (int a = 1 << 31)
  // - décalage à gauche d'un nombre négatif (int a = -1 << 2)
  // - décalage par un offset qui est >= à la largeur du type LHS (partie de gauche de l'affectation):
  // int a = 1 << 32; // UB (comportement indéfini) si int a une taille de 32 bits

  ///////////////////////////////////////
  // Structures de contrôle
  ///////////////////////////////////////

  if (0) {
    printf("Je ne serai jamais exécuté\n");
  } else if (0) {
    printf("Moi aussi, je ne serai jamais exécuté\n");
  } else {
    printf("Je suis affiché\n");
  }

  // Les boucles while
  int ii = 0;
  while (ii < 10) { // TOUTE valeur plus petite que dix est vraie.
    printf("%d, ", ii++); // ii++ augmente ii APRES avoir utilisé sa valeur actuelle.
  } // => affiche "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

  printf("\n");

  int kk = 0;
  do {
    printf("%d, ", kk);
  } while (++kk < 10); // ++kk augmente kk AVANT d'utiliser sa valeur actuelle.
  // => affiche "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

  printf("\n");

  // Les boucles for
  int jj;
  for (jj=0; jj < 10; jj++) {
    printf("%d, ", jj);
  } // => affiche "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

  printf("\n");

  // *NOTES* :
  // Les boucles et les fonctions DOIVENT posséder un corps.
  // Vous pouvez toutefois utiliser un bloc vide ({}) ou un point-virgule.
  int i;
  for (i = 0; i <= 5; i++) {
  }
  // Ou
  for (i = 0; i <= 5; i++); // Déconseillé, car facilement confondable

  // condition à choix multiples : switch()
  switch (a) {
  case 0: // les options doivent être des expressions intégrales *constantes* (telles que des énumérations)
    printf("Hey, 'a' equals 0!\n");
    break; // si vous ne mettez pas 'break', le cas d'après est exécuté sans tester le `case`
  case 1:
    printf("Huh, 'a' equals 1!\n");
    break;
    // Attention - sans le "break", l'execution se poursuit jusqu'au
    // prochain "break" atteint.
  case 3:
  case 4:
    printf("Look at that.. 'a' is either 3, or 4\n");
    break;
  default:
    // si `a` ne correspond à aucune option
    fputs("Erreur !\n", stderr);
    exit(-1);
    break;
  }
  
  // Utilisation de "goto" en C
  typedef enum { false, true } bool;
  bool disaster = false;
  int i, j;
  for(i=0; i<100; ++i)
  for(j=0; j<100; ++j)
  {
    if (i + j >= 150)
        disaster = true;
    if (disaster)
        goto error;
  }
  error:
  printf("Error occurred at i = %d & j = %d.\n", i, j);
  // Ceci affichera "Error occurred at i = 51 & j = 99."

  ///////////////////////////////////////
  // Transtypage
  ///////////////////////////////////////

  // Chaque valeur de C a un type, mais vous pouvez convertir le type d'une valeur en un autre type
  // si vous le souhaitez (avec quelques contraintes).

  int x_hex = 0x01; // Vous pouvez définir des variables avec des littéraux hexadécimaux

  // La conversion entre les types tentera de conserver leurs valeurs numériques
  printf("%d\n", x_hex); // => Affiche 1
  printf("%d\n", (short) x_hex); // => Affiche 1
  printf("%d\n", (char) x_hex); // => Affiche 1

  // Les types déborderont sans avertissement
  printf("%d\n", (unsigned char) 257); // => 1 (max = 255 si le caractère mesure 8 bits)

  // Pour déterminer la valeur maximale d'un `char`, d'un `signed char` et d'un `unsigned char`,
  // respectivement, utilisez les macros CHAR_MAX, SCHAR_MAX et UCHAR_MAX de <limits.h>

  // Les types entiers peuvent être convertis en types à virgule flottante et vice versa.
  printf("%f\n", (double) 100); // %f toujours utilisé pour un type double...
  printf("%f\n", (float)  100); // ...même avec un float.
  printf("%d\n", (char)100.0);

  ///////////////////////////////////////
  // Pointeurs
  ///////////////////////////////////////

  // Un pointeur est une variable déclarée pour stocker une adresse mémoire. Sa déclaration
  // vous indique également le type de données vers lequel il pointe. Vous pouvez récupérer l'adresse mémoire
  // de vos variables, puis jouer avec.

  int x = 0;
  printf("%p\n", (void *)&x); // Utilisez le caractère & pour récupérer l'adresse d'une variable
  // (%p met en forme un objet pointeur de type void *)
  // => Affiche une adresse en mémoire;

  // Les pointeurs sont préfixés du caractère * lors de la déclaration
  int *px, not_a_pointer; // px est un pointeur sur un entier
  px = &x; // Sauvegarde de l'adressse mémoire de x dans px
  printf("%p\n", (void *)px); // => Affiche une adresse mémoire
  printf("%zu, %zu\n", sizeof(px), sizeof(not_a_pointer));
  // => Affiche "8, 4" sur un système classique 64-bit

  // Pour avoir la valeur située à l'adresse du pointeur,
  // on met * devant pour le déréférencer.
  // Remarque: oui, il peut être déroutant que '*' soit utilisé pour _les deux_ déclarer un
  // pointeur et le déréférencement.
  printf("%d\n", *px); // => Affiche 0, la valeur de x

  // Vous pouvez également modifier la valeur vers laquelle pointe le pointeur.
  // Nous devons mettre la déréférence entre parenthèses car
  // ++ a une priorité plus élevée que *.
  (*px)++; // Augmente de 1 la valeur pointée par px
  printf("%d\n", *px); // => Affiche 1
  printf("%d\n", x); // => Affiche 1

  // Les tableaux sont un bon moyen d'allouer un bloc de mémoire contigu
  int x_array[20]; //déclare un tableau de taille 20 (la taille ne pourra être changée)
  int xx;
  for (xx = 0; xx < 20; xx++) {
    x_array[xx] = 20 - xx;
  } // Initialise x_array à 20, 19, 18,... 2, 1

  // Declare un pointeur sur un type entier et l'initialise pour pointer sur x_array
  int* x_ptr = x_array;
  // x_ptr pointe maintenant vers le premier élément du tableau (l'entier 20).
  // Cela fonctionne car les tableaux s'interprètent souvent comme pointeurs vers leur premier élément.
  // Par exemple, lorsqu'un tableau est transmis à une fonction ou affecté à un pointeur,
  // il est converti implicitement en pointeur.
  // Exceptions : lorsque le tableau est l'argument de l'opérateur `&` (adresse de):
  int arr[10];
  int (*ptr_to_arr)[10] = &arr; // &arr n'est PAS de type `int *`!
  // Il est de type "pointeur sur tableau" (de dix entiers).
  // ou losqu'une chaine de caractères littérale est utilisée pour initialiser un tableau de caractères:
  char otherarr[] = "foobarbazquirk";
  // ou lorsqu'il est l'argument de l'opérateur `sizeof` ou `alignof`:
  int arraythethird[10];
  int *ptr = arraythethird; // équivalent à int *ptr = &arr[0];
  printf("%zu, %zu\n", sizeof(arraythethird), sizeof(ptr));
  // affiche probablement "40, 4" ou "40, 8"

  // L'augmentation ou la diminution d'un pointeur se fait suivant son type
  // (c'est ce qu'on appelle l'arithmétique du pointeur)
  printf("%d\n", *(x_ptr + 1)); // => Affiche 19
  printf("%d\n", x_array[1]); // => Affiche 19

  // You can also dynamically allocate contiguous blocks of memory with the
  // standard library function malloc, which takes one argument of type size_t
  // representing the number of bytes to allocate (usually from the heap, although this
  // may not be true on e.g. embedded systems - the C standard says nothing about it).
  // Vous pouvez également allouer dynamiquement des blocs de mémoire contigus avec la
  // fonction malloc de la bibliothèque standard, qui prend un argument de type size_t
  // représentant le nombre d'octets à allouer (généralement à partir du tas, bien que cela
  // peut ne pas être vrai par exemple dans les systèmes embarqués - la norme C ne dit rien à ce sujet).
  int *my_ptr = malloc(sizeof(*my_ptr) * 20);
  for (xx = 0; xx < 20; xx++) {
    *(my_ptr + xx) = 20 - xx; // my_ptr[xx] = 20-xx
  } // Initialise la mémoire à 20, 19, 18, 17... 2, 1 (comme entiers)

  // Soyez prudent en passant des valeurs fournies par l'utilisateur à malloc! Si vous le souhaitez
  // pour plus de sécurité, vous pouvez utiliser calloc à la place (qui, contrairement à malloc, met également à zéro la mémoire)
  int* my_other_ptr = calloc(20, sizeof(int));

  // Notez qu'il n'y a pas de méthode standard pour obtenir la longueur d'un
  // tableau alloué dynamiquement en C. Pour cette raison, si vos tableaux sont
  // manipulés par votre programme, vous avez besoin d'une autre variable
  // pour garder une trace du nombre d'éléments (taille) d'un tableau. Voir la
  // section fonctions pour plus d'informations.
  size_t size = 10;
  int *my_arr = calloc(size, sizeof(int));
  // Add an element to the array
  size++;
  my_arr = realloc(my_arr, sizeof(int) * size);
  if (my_arr == NULL) {
    //N'oubliez pas de vérifier l'échec de la fonction realloc!
    return 0;
  }
  my_arr[10] = 5;

  // Déréférencer de la mémoire que vous n'avez pas allouée donne des
  // UBs "undefined behaviors" ou "comportement imprévisibles",
  printf("%d\n", *(my_ptr + 21)); // => Peut crash, afficher n'importe quoi, ou autre

  // Lorsque vous avez terminé avec un bloc de mémoire alloué via malloc, vous devez le libérer,
  // ou bien personne d'autre ne pourra l'utiliser jusqu'à la fin de votre programme :
  // (cela s'appelle une "fuite de mémoire")
  free(my_ptr);

  // Les chaînes de caractères sont des tableaux de caractères, mais ils sont généralement représentés comme
  // pointeur-vers-caractère (qui est un pointeur vers le premier élément du tableau).
  // Il est recommandé d'utiliser un `const char *' pour référencer une chaîne littérale de caractères,
  // car les chaînes de caractères littérales ne doivent pas être modifiées (i.e. "foo"[0] = 'a' est INCORRECT.)
  const char *my_str = "This is my very own string literal";
  printf("%c\n", *my_str); // => 'T'

  // Ce n'est pas le cas si la chaîne de caractères est un tableau
  // (potentiellement initialisée avec une chaîne littérale)
  // qui réside dans la mémoire réinscriptible, comme dans:
  char foo[] = "foo";
  foo[0] = 'a'; // ceci est correct, foo contient maintenant "aoo"

  function_1();
} // fin de la fonction main

///////////////////////////////////////
// Fonctions
///////////////////////////////////////

// Sytaxe pour déclarer une fonction:
// <type de sortie> <nom de la fonstion>(<arguments>)

int add_two_ints(int x1, int x2)
{
  return x1 + x2; // Utilisez return pour retourner une valeur de sortie
}

/*
Les fonctions sont appelées par valeur. Lorsqu'une fonction est appelée, les arguments passés à
à la fonction sont une copie des arguments originaux (sauf pour les tableaux). Tout ce que vous
allez faire aux arguments de la fonction ne changera pas la valeur des arguments
originaux avant appel de la fonction.

Utilisez les pointeurs si vous avez besoin de modifier la valeur d'origine des arguments.

Exemple : inversion sur place d'une chaîne de caractères
*/

// Une fonction void ne retourne aucune valeur
void str_reverse(char *str_in)
{
  char tmp;
  size_t ii = 0;
  size_t len = strlen(str_in); // `strlen()` fait partie de la bibliothèque standard c
                               // REMARQUE : la longueur renvoyée par `strlen` N'INCLUT PAS
                               //            l'octet NULL de fin ('\0').
  for (ii = 0; ii < len / 2; ii++) { // en C99 vous pouvez déclarer directement le type de `ii` ici
    tmp = str_in[ii];
    str_in[ii] = str_in[len - ii - 1]; // ii-ème caractère depuis la fin
    str_in[len - ii - 1] = tmp;
  }
}
// REMARQUE : le fichier d'entête string.h a besoin d'être inclus pour utiliser strlen()

/*
char c[] = "This is a test.";
str_reverse(c);
printf("%s\n", c); // => ".tset a si sihT"
*/
/*
car nous ne pouvons renvoyer qu'une seule variable
pour changer les valeurs de plusieurs variables, nous passons des pointeurs
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
// les valeurs seront échangées
*/

/*
En ce qui concerne les tableaux, ils seront toujours transmis aux fonctions
comme pointeurs. Même si vous allouez statiquement un tableau comme `arr[10]`,
il est toujours passé en tant que pointeur vers le premier élément de tout appel de fonction.
Encore une fois, il n'y a pas de moyen standard pour connaître la taille d'un tableau
alloué dynamiquement en C.
*/
// La taille doit être transmise!
// Sinon, cette fonction n'a aucun moyen de connaître la taille du tableau.
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
// affichera "arr[0] is: 1" etc
*/

// si vous faites référence à des variables externes en dehors de la fonction, vous devez utiliser le mot clé extern.
int i = 0;
void testFunc() {
  extern int i; //i ici utilise la variable externe i
}

// rendre les variables external privées au fichier source avec static:
static int j = 0; //les autres fichiers utilisant testFunc2() n'auront pas accés à la variable j
void testFunc2() {
  extern int j;
}

// Le mot clé static rend une variable inaccessible au code en dehors de
// l'unité de compilation. (Sur presque tous les systèmes, une "unité de compilation" est un
// fichier .c.) static peut être appliqué aussi bien aux variables globales (à l'unité de compilation),
// aux fonctions et aux variables locales aux fonctions. Lorsque vous utilisez static avec
// une variable locale à une fonction, la variable est effectivement globale et conserve sa
// valeur sur les appels de la fonction, mais n'est accessible que dans la fonction où
// elle a été déclarée. De plus, les variables statiques sont initialisées à 0 si pas
// déjà déclaré avec une autre valeur de départ.
// ** Vous pouvez également déclarer des fonctions statiques pour les rendre privées **

/////////////////////////////////////////////////
// Types et structures définis par l'utilisateur
/////////////////////////////////////////////////

// Typedef peuvent être utilisé pour créer un alias de type
typedef int my_type;
my_type my_type_var = 0;

// Struct est juste une collection de données, dont les membres sont positionnés séquentiellement,
// dans l'ordre où ils sont écrits::
struct rectangle {
  int width;
  int height;
};

// Il n'est généralement aps vrai que
// sizeof(struct rectangle) == sizeof(int) + sizeof(int)
// en raison du remplissage potentiel entre les membres de la structure (c'est pour des raisons
// d'alignement). [1]

void function_1()
{
  struct rectangle my_rec;

  // L'accés aux membres de la structure se fait avec .
  my_rec.width = 10;
  my_rec.height = 20;

  // Vous pouvez déclarer un pointeur vers une structure
  struct rectangle *my_rec_ptr = &my_rec;

  // Utiliser le déréférencement pour définir les membres du pointeur de structure...
  (*my_rec_ptr).width = 30;

  // préférez le raccourci -> par souci de lisibilité
  my_rec_ptr->height = 10; // Identique à (*my_rec_ptr).height = 10;
}

// Vous pouvez appliquer un typedef à une structure pour plus de commodité
typedef struct rectangle rect;

int area(rect r)
{
  return r.width * r.height;
}

// si vous avez de grandes structures, vous pouvez les passer "par pointeur" afin d'éviter la copie
// de toute la structure:
int areaptr(const rect *r)
{
  return r->width * r->height;
}

///////////////////////////////////////
// Pointeurs sur fonction
///////////////////////////////////////
/*
Au moment de l'exécution, les fonctions sont situées à des adresses de mémoire connues. Les pointeurs de fonction sont
un peu comme n'importe quel autre pointeur (ils stockent juste une adresse mémoire), mais peuvent être utilisés
pour appeler directement des fonctions et pour transmettre des gestionnaires de fonctions (ou des fonctions de rappel).
Cependant, la syntaxe de définition peut être source de confusion au départ.

Exemple: Utilisation de str_reverse à partir d'un pointeur
*/
void str_reverse_through_pointer(char *str_in) {
  // Définit une variable pointeur sur fonction, nommée f.
  void (*f)(char *); // La signature doit être la même que la fonction cible.
  f = &str_reverse; // Assigne l'adresse de la fonction réelle (déterminé au runtime)
  // f = str_reverse; marcherait aussi - les fonctions s'interprètent en pointeurs, comme les tableaux
  (*f)(str_in); // Il suffit d'appeler la fonction via le pointeur
  // f(str_in); // C'est une syntaxe alternative mais tout aussi valide pour l'appel.
}

/*
Tant que les signatures de fonction correspondent, vous pouvez affecter n'importe quelle fonction au même pointeur.
Les pointeurs de fonction sont généralement typés via typedef pour plus de simplicité et de lisibilité, comme suit:
*/

typedef void (*my_fnp_type)(char *);

// Puis utilisé lors de la déclaration de la variable réelle de pointeur:
// ...
// my_fnp_type f;


//Caractères spéciaux:
/*
'\a'; // caractère d'alerte (cloche)
'\n'; // caractère de nouvelle ligne
'\t'; // caractère de tabulation (texte justifié à gauche)
'\v'; // tabulation verticale
'\f'; // nouvelle page (flux de formulaire)
'\r'; // retour chariot
'\b'; // caractère de retour arrière
'\0'; // Caractère NULL. Habituellement mis à la fin des chaînes en C.
// bonjour\n\0. \0 utilisé par convention pour marquer la fin de la chaîne.
'\\'; // barre oblique inverse
'\?'; // point d'interrogation
'\' '; // simple guillemet
'\"'; // double quillemets
'\xhh'; // nombre hexadécimal. Exemple: '\xb' = caractère de tabulation verticale
'\0oo'; // nombre octal. Exemple: '\013' = caractère de tabulation verticale

//Format d'affichage:
"%d";    // entier
"%3d";   // entier avec une taille minimum de 3 digits (texte justifié à droite)
"%s";    // chaîne de caractères
"%f";    // nombre à virgule flottante
"%ld";   // nombre long
"%3.2f"; // minimum de 3 digits à gauche et 2 digits à droite nombre décimal à virgule flottante
"%7.4s"; // (peut être fait aussi avec une chaîne de caractères)
"%c";    // caratère
"%p";    // pointeur. REMARQUE: il est nécessaire de caster en (void *) le pointeur, avant de la passer
         //                comme argument à `printf`.
"%x";    // hexadecimal
"%o";    // octal
"%%";    // affiche %
*/

///////////////////////////////////////
// Ordre des évaluations
///////////////////////////////////////

//--------------------------------------------------------//
//        Operateurs                 | Associativité      //
//--------------------------------------------------------//
// () [] -> .                        | de gauche à droite //
// ! ~ ++ -- + = *(type)sizeof       | de droite à gauche //
// * / %                             | de gauche à droite //
// + -                               | de gauche à droite //
// << >>                             | de gauche à droite //
// < <= > >=                         | de gauche à droite //
// == !=                             | de gauche à droite //
// &                                 | de gauche à droite //
// ^                                 | de gauche à droite //
// |                                 | de gauche à droite //
// &&                                | de gauche à droite //
// ||                                | de gauche à droite //
// ?:                                | de droite à gauche //
// = += -= *= /= %= &= ^= |= <<= >>= | de droite à gauche //
// ,                                 | de gauche à droite //
//--------------------------------------------------------//

/******************************* Fichiers en-têtes **********************************

Les fichiers d'en-tête sont une partie importante de C car ils permettent l'interconnexion
des fichiers source C ce qui permet de simplifier le code et les définitions en les séparant
dans des fichiers séparés.

Les fichiers d'en-tête sont syntaxiquement similaires aux fichiers source C mais résident dans
des fichiers ".h". Ils peuvent être inclus dans votre fichier source C en utilisant la commande
du précompilateur #include "example.h", avec example.h existant dans le même répertoire
que le fichier C.
*/

/* Un garde-fou pour éviter que l'en-tête ne soit défini trop de fois. Ce          */
/* qui se produit dans le cas de dépendance cyclique, et que le contenu du fichier */
/* d'en-tête est déjà défini.                                                      */
#ifndef EXAMPLE_H /* si EXAMPLE_H n'est pas déjà défini. */
#define EXAMPLE_H /* Definit la macro EXAMPLE_H. */

/* Des en-têtes peuvent être inclus dans d'autres en-têtes et donc par transitivité */
/* être inclus dans des fichiers qui incluent un en-tête.                           */
#include <string.h>

/* Les macros de fichiers source c peuvent être définies dans les en-têtes et utilisées dans les fichiers */
/* qui incluent ce fichier d'en-tête.                                                                     */
#define EXAMPLE_NAME "Dennis Ritchie"

/* Les macros de fonction peuvent aussi être définies.  */
#define ADD(a, b) ((a) + (b))

/* Remarquez les parenthèses entourant les arguments - c'est important pour                 */
/* assurer que a et b ne soient pas développés de manière inattendue (par exemple, pensez à */
/* MUL (x, y) (x * y); MUL (1 + 2, 3) s'étendrait à (1 + 2 * 3), produisant un              */
/* résultat incorrect)                                                                      */

/* Les structures et les typedefs peuvent être utilisés pour la cohérence entre les fichiers. */
typedef struct Node
{
    int val;
    struct Node *next;
} Node;

/* Il en va de même pour les énumérations. */
enum traffic_light_state {GREEN, YELLOW, RED};

/* Les prototypes de fonctions peuvent également être définis ici pour une utilisation dans plusieurs fichiers, */
/* mais c'est une mauvaise pratique de définir la fonction dans l'en-tête. Les définitions                      */
/* devraient plutôt être placées dans un fichier C.                                                             */
Node createLinkedList(int *vals, int len);

/* Au-delà des éléments ci-dessus, les autres définitions devraient être placées dans un fichier source C */
/* Les inclusions ou définitions excessives ne devraient pas non plus figurer dans                        */
/* un fichier d'en-tête mais placées plutôt dans des en-têtes séparés ou un fichier C.                    */

#endif /* Fin de la directive if du precompilateur. */

```
## Lectures complémentaires

- [Learn C The Hard Way](http://learncodethehardway.org/c/).
- Si vous avez une question, lisez le [compl.lang.c Frequently Asked Questions](http://c-faq.com).

Il est très important d'utiliser un espacement et une indentation appropriés et d'être cohérent avec votre style de codage en général.
Un code lisible est meilleur qu'un code intelligent et un code rapide. Pour un bon style de codage sain à adopter, consultez le
[Linux kernel coding style](https://www.kernel.org/doc/Documentation/process/coding-style.rst).

[1] [Why isn't sizeof for a struct equal to the sum of sizeof of each member?](http://stackoverflow.com/questions/119123/why-isnt-sizeof-for-a-struct-equal-to-the-sum-of-sizeof-of-each-member)
