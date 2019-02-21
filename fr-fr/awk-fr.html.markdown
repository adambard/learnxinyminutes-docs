---
language: awk
filename: learnawk-fr.awk
contributors:
     - ["Marshall Mason", "http://github.com/marshallmason"]
translators:
    - ["GannonTdW", "https://github.com/GannonTdW"]
lang: fr-fr

---

AWK est un outil standard présent sur chaque système UNIX conforme aux normes POSIX.
C’est un outil en ligne de commande qui ressemble au Perl et qui excelle dans les tâches de traitement de fichiers plat.
Il a une syntaxe proche de langage C, mais sans point virgule obligatoires (lorsque la commande est écrite sur plusieurs ligne),sans de gestion manuelle de la mémoire, ou de typage statique.
Vous pouvez l’appeler à partir d’un script shell, ou l’utiliser comme un langage de script autonome.

Pourquoi utiliser AWK au lieu du langage Perl?
Principalement car awk fait partie d'UNIX et est donc présent par défaut sur une très grande partie des systèmes d'exploitation Unix et Linux.
AWK est aussi plus facile à lire que le langage Perl.
AWK est l'outil idéal pour ce qui concerne le traitement de texte simples. En particulier ceux qui necéssitent de lire des fichiers ligne par ligne; chaque ligne comportant des champs séparé par des délimiteurs.


```awk
#!/usr/bin/awk -f

# Les commentaires commencent par un #


# les programmes AWK consistent en une collection de modèle et d'actions
pattern1 { action; }
pattern2 { action; }

# Il y  une boucle implicite et AWK lit et analise automatiquement chaque lignes de chaque fichiers fournis.
# Chaque ligne est divisées par un délimiteur FS qui est par défaut l'espace (plusieurs espace ou une tabulation compte pour un espace). Ce délimiteur peut être changer grâce à l'option -F ou être renseigné début d'un bloc (exemple: FS = " ").

# BEGIN est un modèle spécifique présent au début du programme. C'est là où il faut mettre tout le code préliminaire avant de traiter les fichiers texte. Si vous ne disposez pas de fichiers texte, considérez BEGIN comme le point d’entrée principal du script.
# A l'opposé  de BEGIN, il existe le modèle END. Ce modèle est présent après chaque fin de fichier (EOF : End Of File).

BEGIN {

    # Les variables sont globales. Pas besoin de les déclarer.
    count = 0;

    # les operateurs sont comme ceux  du langes C et de sa famille.
    a = count + 1; # addition
    b = count - 1; # soustraction
    c = count * 1; # multiplication
    d = count / 1; # division entière
    e = count % 1; # modulo, reste de la division entière
    f = count ^ 1; # exponentiel

    a += 1;
    b -= 1;
    c *= 1;
    d /= 1;
    e %= 1;
    f ^= 1;

    # Incrementer et decrementer par un
    a++;
    b--;

    # En tant qu'operateur préfixé, il retourne la valeur incrémentée
    ++a;
    --b;

    # Notez qu'il n'y a pas de ponctuation telle que les points virgule pour terminer les instructions.

    # Instruction de contrôle
    if (conteur == 0)
        print "Nombre de départ 0";
    else
        print "Hein?";

    # Ou vous pouvez utilisez utiliser l'opérateur ternaire
    print (compteur == 0) ? "Nombre de départ 0" : "Hein?";

    # Les blocs sont composé d'une multitude de ligne entre accolades
    while (a < 10) {
        print "La concaténation de chaînes de caractères" " se fait avec des séries de chaînes "    " séparées par des espaces";
        print a;

        a++;
    }

    for (i = 0; i < 10; i++)
        print "le bon vieux for pour les boucles";

    # Les opérateurs de comparaison sont standard
    # a < b   # plus petit que
    # a <= b  # plus petit ou égale à
    # a != b  # non égale
    # a == b  # égale
    # a > b   # Plus grand que
    # a >= b  # Plus grand ou égale à

    # Les opérateurs logiques sont
    # a && b  # ET
    # a || b  # OU

    # En plus, il y a les expression régulières
    if ("foo" ~ "^fo+$")
        print "Fooey!";
    if ("boo" !~ "^fo+$")
        print "Boo!";

    # Les Tableaux
    arr[0] = "foo";
    arr[1] = "bar";

    # Vous pouvez alors initialiser un tableau avec la fonction split()

    n = split("foo:bar:baz", arr, ":");

    # Il y a aussi les tableaux associatifs
    assoc["foo"] = "bar";
    assoc["bar"] = "baz";

    # et les tableaux multi-dimention, avec certaines limitations que l'on ne mentionnera pas ici
    multidim[0,0] = "foo";
    multidim[0,1] = "bar";
    multidim[1,0] = "baz";
    multidim[1,1] = "boo";

    # Vous pourvez tester l'appartenance à un tableau
    if ("foo" in assoc)
        print "Fooey!";

    # Vous pouvez aussi utilisez l'opérateur 'in' pour parcourir les clés d'un teableau
    for (key in assoc)
        print assoc[key];

    # La ligne de commande est dans un tableau spécifique appelé ARGV
    for (argnum in ARGV)
        print ARGV[argnum];

    # Vous pouvez supprimer des éléments d'un tableau
    # C'est utile pour empêcher AWK de supposer que certains arguments sont des fichiers à traiter.
    delete ARGV[1];

    # Le nombre d'argument de la ligne de commande est dans une variable appellé ARGC
    print ARGC;

    # AWK inclue trois catégorie de fonction.
    # On les examinera plus tard

    return_value = arithmetic_functions(a, b, c);
    string_functions();
    io_functions();
}

# Voici comment définir une fonction
function arithmetic_functions(a, b, c,     d) {

    # La partie la plus ennuyante de AWK est probablement l’absence de variables locales.
    # Tout est global. Pour les scripts courts, c'est très utile, voire utile,
    # mais pour les scripts plus longs, cela peut poser problème.

    # Il y a cepandant une solution de contournement (enfin ... une bidouille).
    # Les arguments d'une fonction sont locaux à cette fonction.
    # Et AWK vous permet de définir plus d'arguments à fonction que nécessaire.
    # Il suffit donc de mettre une variable locale dans la déclaration de fonction,
    # comme je l’ai fait ci-dessus. En tant que convention,
    # mettez quelques espaces supplémentaires pour faire la distinction entre
    # les paramètres de fonction réels et les variables locales.
    # Dans cet exemple, a, b et c sont des paramètres réels,
    # alors que d est simplement une variable locale.

    # Maintenant, les fonctions arithmétiques

    # La plupart des implémentations de AWK ont des fonctions trigonométrique standard
    localvar = sin(a);
    localvar = cos(a);
    localvar = atan2(b, a); # arc tangente de b / a

    # Les exponentiels et logarithmes decimaux sont aussi là
    localvar = exp(a);
    localvar = log(a);

    # Les racines carrées
    localvar = sqrt(a);

    # Tronquer un flotant en entier
    localvar = int(5.34); # localvar => 5

    # Les nombres aléatoire
    srand(); # La graine est l'argument. Par défaut, il utilise l'heure du système du jour
    localvar = rand(); # Nombres aléatoire entre 0 et 1.

    # Maintenant on retourne la valeur
    return localvar;
}

function string_functions(    localvar, arr) {

    # AWK a certaines fonctions pour le traitement des chaînes de caractères,
    # dont beaucoup reposent fortement sur des expressions régulières.

    # Chercher et remplacer, la première occurence (sub) ou toutes les occurences (gsub)
    # Les deux renvoient le nombre de correspondances remplacées
    localvar = "fooooobar";
    sub("fo+", "Meet me at the ", localvar); # localvar => "Meet me at the bar"
    gsub("e+", ".", localvar); # localvar => "m..t m. at th. bar"

    # Rechercher une chaîne qui correspond à une expression régulière
    # index() fait la même chose, mais n'autorise pas les expressions régulières
    match(localvar, "t"); # => 4, puisque 't' est le quatrième caractères

    # Séparer par un délimiteur
    n = split("foo-bar-baz", arr, "-"); # a[1] = "foo"; a[2] = "bar"; a[3] = "baz"; n = 3

    # Autre astuces utiles
    sprintf("%s %d %d %d", "Testing", 1, 2, 3); # => "Testing 1 2 3"
    substr("foobar", 2, 3); # => "oob"
    substr("foobar", 4); # => "bar"
    length("foo"); # => 3
    tolower("FOO"); # => "foo"
    toupper("foo"); # => "FOO"
}

function io_functions(    localvar) {

    # Vous avez déjà vu print
    print "Hello world";

    # Mais il y a aussi printf
    printf("%s %d %d %d\n", "Testing", 1, 2, 3);

    # AWK n'a pas de descripteur de fichier en soi. Il ouvrira automatiquement
    # un descripteur de fichier lorsque vous utilisez quelque chose qui en a besoin.
    # La chaîne de caractères que vous avez utilisée pour cela peut être traitée
    # comme un descripteur de fichier à des fins d'entrée / sortie.

    outfile = "/tmp/foobar.txt";

    print "foobar" > outfile;

    # Maintenant, la chaîne de caractères "outfile" est un descripteur de fichier.
    # Vous pouvez la fermer
    close(outfile);

    # Voici comment executer quelque chose dans le shell
    system("echo foobar"); # => affiche foobar

    # Lire quelque chose depuis l'entrée standard et la stoquer dans une variable locale
    getline localvar;

    # Lire quelque chose à partir d'un pipe (encore une fois, utilisez une chaine de caractère
    # que vous fermerez proprement)
    "echo foobar" | getline localvar # localvar => "foobar"
    close("echo foobar")

    # Lire une ligne d'un fichier et la stoquer dans une variable locale
    infile = "/tmp/foobar.txt";
    getline localvar < infile;
    close(infile);
}

# Comme dit au début, , AWK consiste en une collection de modèles et d'actions.
#  Vous connaissiez déjà le modèle BEGIN. Les autres modèles ne sont utilisés
# que si vous traitez des lignes à partir de fichiers ou de l'entrée standard (stdin).
# Quand vous passez des arguments à AWK, ils sont considéré comme des noms de fichiers
# à traiter.
# AWK les traitera tous dans l'ordre. Voyez les comme dans à une boucle implicite,
# parcourant les lignes de ces fichiers.
# Ces modèles et ces actions ressemblent à des instructions switch dans la boucle.

/^fo+bar$/ {

    # Cette action sera exécutée pour chaque ligne qui correspond à l'expression régulière,
    # /^fo+bar$/, et sera ignorée pour toute ligne qui ne correspond pas.
    # Imprimons simplement la ligne:

    print;

    # Whoa, pas d'argument! C'est parce que print a un défaut: $0.
    # $0 est le nom de la ligne en cours de traitement. Il est créé automatiquement pour vous.

    # Vous devinez probablement qu'il existe d'autres variables $.
    # Chaque ligne est divisée implicitement avant que chaque action soit executée, comme
    # le fait le shell. Et, comme le shell, chaque champ est accessible avec un signe dollar

     # Ceci affichera les deuxième et quatrième champs de la ligne.
    print $2, $4;

    # AWK définie automatiquement beaucoup d'autre variable qui peuvent vous aider
    #à inspecter et traiter chaques lignes. La plus importante est NF

    # Affiche le nombre de champs de la ligne
    print NF;

    # Afficher le dernier champ de la ligne
    print $NF;
}

# Chaque modèle est en réalité un test vrai / faux.
# L'expression régulière dans le dernier champ est également un test vrai / faux,
# mais une partie de celle-ci était cachée. Si vous ne donnez pas de chaîne de caractères
# à tester, cela signifie $ 0, la ligne en cours de traitement.
# La version complète de ceci est:

$0 ~ /^fo+bar$/ {
    print "Equivalent du dernier champ";
}

a > 0 {
    # Ceci s’exécutera une fois pour chaque ligne, tant que le test est positif
}

# Vous avez déjà une idée de ce que peut faire AWK.
# Lire ligne par ligne des fichiers texte et executer un traitement à chacune de ces lignes.
# Cela est si courant sous UNIX qu'AWK est un langage de script
# qui fait tout cela pour vous sans que vous ayez à le demander.
# Écrivez simplement les modèles et les actions en fonction de ce que vous avez en entrée
# et ce que vous voulez en faire.

# Ce qui suit est un exemple rapide d'un petit script, pour lequel AWK est parfait.
# for. It will read a name from standard input and then will print the average
# age of everyone with that first name. Let's say you supply as an argument the
# name of a this data file:
# Le script lit un nom à partir de l'entrée standard, puis affiche l'âge moyen de toutes les
# personnes portant ce prénoms. Supposons que vous fournissiez comme argument
# le nom d'un fichier comportant ces données:
#
# Bob Jones 32
# Jane Doe 22
# Steve Stevens 83
# Bob Smith 29
# Bob Barker 72
#
# Le script est le suivant :

BEGIN {

    # Premièrement, on demande à l'utilisateur le prénom voulue
    print "Pour quel prénom vouldriez vous savoir l'age moyen ?";

    # On récupère la ligne à partir de l'entrée standard, pas de la ligne de commande
    getline name < "/dev/stdin";
}

# Maintenant, pour chaque ligne dont le premier champs est le prénom donné
$1 == name {

    # Ici, nous avons accès à certain nombre de
    # variables utiles déjà préchargées:
    # $0 est la ligne entière
    # $3 est le troisième champ. Ici il correspond à l'age qui nous intéraisse
    # NF est le nombre de champs qui devrait être trois
    # NR est le nombre d'enregistrement (lignes) vus jusqu'à présent
    # FILENAME est le nom du fichier en cours de traitement
    # FS est le champ séparateur, ici c'est " " (un espace)
    # ...etc. Et beaucoup d'autre que vous pouvez connaître le manuel de man.
    # Pour cela executer "man awk" dans votre terminal

    # Garder une trace du total accumulé et du nombre de ligne correspondantes.
    sum += $3;
    nlines++;
}

# Another special pattern is called END. It will run after processing all the
# text files. Unlike BEGIN, it will only run if you've given it input to
# process. It will run after all the files have been read and processed
# according to the rules and actions you've provided. The purpose of it is
# usually to output some kind of final report, or do something with the
# aggregate of the data you've accumulated over the course of the script.

# Un autre motif spécial est END. Il fonctionnera après le traitement de tous
# les fichiers texte. Contrairement à BEGIN, il ne fonctionne que si vous lui
# donnez une entrée à traiter. Il sera exécuté une fois que tous les fichiers
# auront été lus et traités conformément aux règles et aux actions que vous
# avez fournies. Le but est généralement de produire un rapport final
# ou de faire quelque chose avec l'ensemble des données que vous avez
# accumulées au cours du script.


END {
    if (nlines)
        print "L'age moyen pour le prénom " name " est " sum / nlines;
}

```
Pour plus d'information :

* [Awk tutorial](http://www.grymoire.com/Unix/Awk.html)
* [Awk man page](https://linux.die.net/man/1/awk)
* [The GNU Awk User's Guide](https://www.gnu.org/software/gawk/manual/gawk.html) GNU Awk est dans la majorité des systèmes Linux.
* [AWK one-liner collection](http://tuxgraphics.org/~guido/scripts/awk-one-liner.html)
