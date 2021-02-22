---
category: tool
tool: awk
filename: learnawk-fr.awk
contributors:
     - ["Marshall Mason", "http://github.com/marshallmason"]
translators:
    - ["GannonTdW", "https://github.com/GannonTdW"]
lang: fr-fr

---

AWK est un outil standard présent dans chaque système UNIX conforme aux normes POSIX.
C’est un outil en ligne de commande qui ressemble au Perl et qui est excellent dans les tâches de traitement de fichiers texte.
Vous pouvez l’appeler à partir d’un script shell, ou l’utiliser comme un langage de script autonome.

Pourquoi utiliser AWK au lieu du langage Perl ?
Principalement, car AWK fait partie d'UNIX et est donc présent par défaut sur une très grande partie des systèmes d'exploitation UNIX et Linux. 
AWK est aussi plus facile à lire que le langage Perl ; et est l'outil idéal pour ce qui concerne le traitement de texte simple. Notamment le traitement de ceux qui necéssitent de lire des fichiers ligne par ligne ; chaque ligne comportant des champs séparés par des délimiteur.


```awk
#!/usr/bin/awk -f

# Les commentaires commencent par un #


# les programmes AWK consistent en une collection de règles et d'actions
règle1 { action; }
règle2 { action; }

# AWK lit et analyse automatiquement chaque ligne de chaque fichier fourni.
# Chaque ligne est divisée par un délimiteur FS qui est par défaut l'espace (plusieurs espaces ou une tabulation comptent pour un espace). Ce délimiteur peut être changer grâce à l'option -F ou être renseigné au début d'un bloc (exemple: FS = " ").

# BEGIN est une règle spécifique exécutée au début du programme. C'est à cet endroit que vous mettrez tout le code à exécuter avant de traiter les fichiers texte. Si vous ne disposez pas de fichiers texte, considérez BEGIN comme le point d’entrée principal du script.
# A l'opposé de BEGIN, il existe la règle END. Cette règle est présente après chaque fin de fichier (EOF : End Of File).

BEGIN {

    # Les variables sont globales. Pas besoin de les déclarer.
    count = 0;

    # les opérateurs sont identiques au langage C et aux langages similaires (exemple: C#, C++)
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

    # Incrémenter et décrémenter par un
    a++;
    b--;

    # En tant qu'opérateur préfixé, c'est la valeur incrémentée qui est retournée
    ++a;
    --b;

    # Instruction de contrôle
    if (conteur == 0)
        print "Nombre de départ 0";
    else
        print "Hein?";

    # Vous pouvez aussi utiliser l'opérateur ternaire
    print (compteur == 0) ? "Nombre de départ 0" : "Hein?";

    # Les blocs sont composés d'une multitude de lignes entre accolades
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

    # En plus, il y a les expressions régulières
    if ("foo" ~ "^fo+$")
        print "Fooey!";
    if ("boo" !~ "^fo+$")
        print "Boo!";

    # Les Tableaux
    arr[0] = "foo";
    arr[1] = "bar";

    # Vous pouvez aussi initialiser un tableau avec la fonction split()

    n = split("foo:bar:baz", arr, ":");

    # Il y a aussi les tableaux associatifs
    assoc["foo"] = "bar";
    assoc["bar"] = "baz";

    # et les tableaux multi-dimentions, avec certaines limitations que l'on ne mentionnera pas ici
    multidim[0,0] = "foo";
    multidim[0,1] = "bar";
    multidim[1,0] = "baz";
    multidim[1,1] = "boo";

    # Vous pouvez tester l'appartenance à un tableau
    if ("foo" in assoc)
        print "Fooey!";

    # Vous pouvez aussi utilisez l'opérateur 'in' pour parcourir les clés d'un tableau
    for (key in assoc)
        print assoc[key];

    # La ligne de commande est dans un tableau spécifique appelé ARGV
    for (argnum in ARGV)
        print ARGV[argnum];

    # Vous pouvez supprimer des éléments d'un tableau
    # C'est utile pour empêcher AWK de supposer que certains arguments soient des fichiers à traiter.
    delete ARGV[1];

    # Le nombre d'arguments de la ligne de commande est dans une variable appellée ARGC
    print ARGC;

    # AWK inclue trois catégories de fonction.
    # On les examinera plus tard

    return_value = arithmetic_functions(a, b, c);
    string_functions();
    io_functions();
}

# Voici comment définir une fonction
function arithmetic_functions(a, b, c,     d) {

    # La partie la plus ennuieuse de AWK est probablement l’absence de variables locales.
    # Tout est global. Pour les scripts courts, c'est très utile, mais pour les scripts plus longs,
    # cela peut poser problème.

    # Il y a cepandant une solution de contournement (enfin ... une bidouille).
    # Les arguments d'une fonction sont locaux à cette fonction.
    # Et AWK vous permet de définir plus d'arguments à la fonction que nécessaire.
    # Il suffit donc de mettre une variable locale dans la déclaration de fonction,
    # comme ci-dessus. La convention veut que vous mettiez quelques espaces supplémentaires
    # pour faire la distinction entre les paramètres réels et les variables locales.
    # Dans cet exemple, a, b et c sont des paramètres réels,
    # alors que d est simplement une variable locale.

    # Maintenant, les fonctions arithmétiques

    # La plupart des implémentations de AWK ont des fonctions trigonométriques standards
    localvar = sin(a);
    localvar = cos(a);
    localvar = atan2(b, a); # arc tangente de b / a

    # Les exponentiels et logarithmes décimaux sont aussi là
    localvar = exp(a);
    localvar = log(a);

    # Les racines carrées
    localvar = sqrt(a);

    # Tronquer un nombre décimal en nombre entier
    localvar = int(5.34); # localvar => 5

    # Les nombres aléatoires
    srand(); 
    # L'argument de la fonction srand() est la valeur de départ pour générer
    # les nombres aléatoires . Par défaut, il utilise l'heure du système
    
    localvar = rand(); # Nombre aléatoire entre 0 et 1.

    # Maintenant on retourne la valeur
    return localvar;
}

function string_functions(    localvar, arr) {

    # AWK a plusieurs fonctions pour le traitement des chaînes de caractères,
    # dont beaucoup reposent sur des expressions régulières.

    # Chercher et remplacer, la première occurence (sub) ou toutes les occurences (gsub)
    # Les deux renvoient le nombre de correspondances remplacées
    localvar = "fooooobar";
    sub("fo+", "Meet me at the ", localvar); # localvar => "Meet me at the bar"
    gsub("e+", ".", localvar); # localvar => "m..t m. at th. bar"

    # Rechercher une chaîne de caractères qui correspond à une expression régulière
    # index() fait la même chose, mais n'autorise pas les expressions régulières
    match(localvar, "t"); # => 4, puisque 't' est le quatrième caractère

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
    # Vous pouvez le fermer
    close(outfile);

    # Voici comment exécuter quelque chose dans le shell
    system("echo foobar"); # => affiche foobar

    # Lire quelque chose depuis l'entrée standard et la stocker dans une variable locale
    getline localvar;

    # Lire quelque chose à partir d'un pipe (encore une fois, utilisez une chaine de caractère
    # que vous fermerez proprement)
    "echo foobar" | getline localvar # localvar => "foobar"
    close("echo foobar")

    # Lire une ligne d'un fichier et la stocker dans une variable locale
    infile = "/tmp/foobar.txt";
    getline localvar < infile;
    close(infile);
}

# Comme dit au début, AWK consiste en une collection de règles et d'actions.
# Vous connaissez déjà les règles BEGIN et END. Les autres règles ne sont utilisées que si vous traitez 
# des lignes à partir de fichiers ou l'entrée standard (stdin).
# Quand vous passez des arguments à AWK, ils sont considérés comme des noms de fichiers à traiter.
# AWK les traitera tous dans l'ordre. Voyez les comme dans à une boucle implicite,
# parcourant les lignes de ces fichiers.
# Ces règles et ces actions ressemblent à des instructions switch dans la boucle.

/^fo+bar$/ {

    # Cette action sera exécutée pour chaque ligne qui correspond à l'expression régulière,
    # /^fo+bar$/, et sera ignorée pour toute ligne qui n'y correspond pas.
    # Imprimons simplement la ligne:

    print;

    # Pas d'argument ! C'est parce que print a un défaut : $0.
    # $0 est le nom de la ligne en cours de traitement. Il est créé automatiquement.

    # Vous devinez probablement qu'il existe d'autres variables $.
    # Chaque ligne est divisée implicitement avant que chaque action soit exécutée, comme
    # le fait le shell. Et, comme le shell, chaque champ est accessible avec un signe dollar

     # Ceci affichera les deuxième et quatrième champs de la ligne.
    print $2, $4;

    # AWK défini automatiquement beaucoup d'autres variables qui peuvent vous aider
    # à inspecter et traiter chaque ligne. La plus importante est NF

    # Affiche le nombre de champs de la ligne
    print NF;

    # Afficher le dernier champ de la ligne
    print $NF;
}

# Chaque règle est en réalité un test conditionel. 

a > 0 {
    # Ceci s’exécutera une fois pour chaque ligne, tant que le test est positif
}

# Les expressions régulières sont également des tests conditionels.
#Si le test de l'expression régulières n'est pas vrais alors le bloc n'est pas executé
$0 /^fobar/ { 
   print "la ligne commance par fobar"  
}

# Dans le cas où vous voulez tester votre chaine de caractères sur la ligne en cours de traitement
# $0 est optionnelle.

/^[a-zA-Z0-9]$/ {
    print "La ligne courante ne contient que des caractères alphanumériques.";
}


# AWK peut parcourir un fichier texte ligne par ligne et exécuter des actions en fonction de règles établies
# Cela est si courant sous UNIX qu'AWK est un langage de script.

# Ce qui suit est un exemple rapide d'un petit script, pour lequel AWK est parfait.
# Le script lit un nom à partir de l'entrée standard, puis affiche l'âge moyen de toutes les
# personnes portant ce prénom.
# Supposons que vous fournissiez comme argument le nom d'un fichier comportant ces données:
#
# Bob Jones 32
# Jane Doe 22
# Steve Stevens 83
# Bob Smith 29
# Bob Barker 72
#
# Le script est le suivant :

BEGIN {

    # Premièrement, on demande à l'utilisateur le prénom voulu
    print "Pour quel prénom vouldriez vous savoir l'age moyen ?";

    # On récupère la ligne à partir de l'entrée standard, pas de la ligne de commande
    getline name < "/dev/stdin";
}

# Maintenant, pour chaque ligne dont le premier champ est le prénom donné
$1 == name {

    # Ici, nous avons accès à un certain nombre de variables utiles déjà préchargées :
    # $0 est la ligne entière
    # $3 est le troisième champ. Ici il correspond à l'age qui nous intéresse
    # NF est le nombre de champs et vaut 3
    # NR est le nombre d'enregistrements (lignes) vus jusqu'à présent
    # FILENAME est le nom du fichier en cours de traitement
    # FS est séparateur de champs, ici c'est " " (un espace)
    # ...etc. Et beaucoup d'autre que vous pouvez connaître dans le manuel de man.
    # Pour cela exécutez "man awk" dans votre terminal

    # Garder une trace du total accumulé et du nombre de lignes correspondant.
    sum += $3;
    nlines++;
}

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
Pour plus d'informations :

* [Awk tutorial](http://www.grymoire.com/Unix/Awk.html)
* [Awk man page](https://linux.die.net/man/1/awk)
* [The GNU Awk User's Guide](https://www.gnu.org/software/gawk/manual/gawk.html) GNU Awk est dans la majorité des systèmes Linux.
* [AWK one-liner collection](http://tuxgraphics.org/~guido/scripts/awk-one-liner.html)
