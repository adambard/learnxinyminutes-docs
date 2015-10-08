---
language: c#
contributors:
    - ["Irfan Charania", "https://github.com/irfancharania"]
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Melvyn Laïly", "http://x2a.yt"]
    - ["Shaun McCarthy", "http://www.shaunmccarthy.com"]
translators:
    - ["Olivier Hoarau", "https://github.com/Olwaro"]
filename: LearnCSharp-fr.cs
lang: fr-fr
---

C# est un langage de programmation orienté objet à typage fort qui permet aux développeurs de créer une grande variété d'applications fiables et robustes s'appuyant sur le framework .NET.

[Plus d'infos](http://msdn.microsoft.com/fr-fr/library/67ef8sbd.aspx)

```c#
// Les commentaires sur une seule ligne commencent par //
/*
Les
commentaires
multi-lignes
ressemblent
à
ceci
*/
/// <summary>
/// Ceci est un commentaire de documentation XML
/// </summary>

// Importez des namespaces avec l'instruction 'using'
using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Dynamic;
using System.Linq;
using System.Linq.Expressions;
using System.Net;
using System.Threading.Tasks;

// Définit la portée du code pour une meilleure organisation
namespace Learning
{
    // Chaque fichier .cs devrait au moins contenir une classe avec le même nom 
    // que celui du fichier. Ce n'est pas une obligation mais c'est mieux ! 
    public class LearnCSharp
    {
        // LES BASES - si vous avez déjà de l'expérience en Java ou C++
        // passez directement à la partie FONCTIONNALITÉS INTERÉSSANTES 
        public static void Syntax() 
        {
            // Utilisez Console.WriteLine pour écrire sur la sortie
            Console.WriteLine("Hello World");
            Console.WriteLine(
                "Entier: " + 10 +
                " Double: " + 3.14 +
                " Booleen: " + true);

            // Pour omettre le retour à la ligne : Console.Write
            Console.Write("Hello ");
            Console.Write("World");

            ///////////////////////////////////////////////////
            // Types et Variables
            // Déclarez une variable avec la syntaxe <type> <nom>
            ///////////////////////////////////////////////////

            // Sbyte - Entier signé sur 8 bits
            // (-128 <= sbyte <= 127)
            sbyte fooSbyte = 100;

            // Byte - Entier non-signé sur 8 bits
            // (0 <= byte <= 255)
            byte fooByte = 100;

            // Short - Entier sur 16 bits
            // Signé - (-32,768 <= short <= 32,767)
            // Non-signé - (0 <= ushort <= 65,535)
            short fooShort = 10000;
            ushort fooUshort = 10000;

            // Int - Entier sur 32 bits
            int fooInt = 1; // (-2,147,483,648 <= int <= 2,147,483,647)
            uint fooUint = 1; // (0 <= uint <= 4,294,967,295)

            // Long - Entier sur 64 bits
            long fooLong = 100000L; // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
            ulong fooUlong = 100000L; // (0 <= ulong <= 18,446,744,073,709,551,615)
            // Par défaut le type d'un littéral entier est int ou uint
            // on ajoute 'L' pour spécifier la création d'un long

            // Double - Réel sur 64 bits en virgule flottante (norme IEEE 754) 
            double fooDouble = 123.4; // Precision : 15-16 chiffres

            // Float - Réel sur 32 bits en virgule flottante (norme IEEE 754) 
            float fooFloat = 234.5f; // Precision : 7 chiffres
            // Par défaut le type d'un littéral réel est double
            // on ajoute 'f' pour spécifier la création d'un float

            // Decimal - Type de donnée numérique sur 128 bits, fournit une plus 
            // grande précision et une plage de valeurs réduite.
            // Approprié aux calculs financiers et monétaires
            decimal fooDecimal = 150.3m;

            // Booléen - vrai / faux
            bool fooBoolean = true; // ou false

            // Char - Un unique caractère Unicode sur 16 bits
            char fooChar = 'A';

            // String -- contrairement aux types précédents qui sont des types valeurs,
            // string est un type référence. Il peut donc avoir la valeur null
            string fooString = "\"échappement\" de guillemets et ajout de \n (nouvelle ligne) et  de \t (tabulation)";
            Console.WriteLine(fooString);

            // Il est possible d'accéder à chaque caractère d'une chaîne de caractères via son index
            char charFromString = fooString[1]; // 'é'
            // une chaîne de caractères est immuable : impossible de faire fooString[1] = 'X';

            // Comparaison de chaînes de caractères avec la culture courrante en ignorant la casse
            string.Compare(fooString, "x", StringComparison.CurrentCultureIgnoreCase);

            // Formatage
            string fooFs = string.Format("Check Check, {0} {1}, {0} {1:0.0}", 1, 2);

            // Dates et formatage
            DateTime fooDate = DateTime.Now;
            Console.WriteLine(fooDate.ToString("hh:mm, dd MMM yyyy"));

            // Il est possible d'étaler une chaîne de caractères sur plusieurs lignes avec le symbole @.
            // Pour échapper " utilisez ""
            string bazString = @"Voici quelques trucs
sur une nouvelle ligne! ""Wow!"", quel style";

            // Utilisez const ou read-only pour rendre une variable immuable.
            // Les valeurs constantes sont calculées au moment de la compilation
            const int HOURS_I_WORK_PER_WEEK = 9001;

            ///////////////////////////////////////////////////
            // Structures de données
            ///////////////////////////////////////////////////

            // Tableaux - indexé à partir de zéro
            // La taille d'un tableau doit être décidée à la déclaration
            // La syntaxe pour déclarer un tableau est la suivante :
            // <type>[] <nom> = new <type>[<taille>]
            int[] intArray = new int[10];

            // Une autre méthode de déclaration et d'initialisation
            int[] y = { 9000, 1000, 1337 };

            // Indexer un tableau - Accéder à un élément
            Console.WriteLine("intArray à 0: " + intArray[0]);
            // Les tableaux sont muables.
            intArray[1] = 1;

            // Listes
            // Elles sont plus souvent utilisées que les tableaux car plus souples
            // La syntaxe pour déclarer une liste est la suivante :
            // List<type> <nom> = new List<type>();
            List<int> intList = new List<int>();
            List<string> stringList = new List<string>();
            List<int> z = new List<int> { 9000, 1000, 1337 }; // intialisation
            // Les <> indiquent un type générique 
            // Pus d'info dans la partie FONCTIONNALITÉS INTERÉSSANTES

            // Les éléments d'une liste ne sont pas null par défaut
            // Il faut ajouter une valeur avant d'y accéder par index
            intList.Add(1);
            Console.WriteLine("intList à 0: " + intList[0]);

            // Autres structures de données à étudier :
            // Stack/Queue (Pile/File)
            // Dictionary (une implémentation de hash map)
            // HashSet (représente un ensemble)
            // Collections en lecture seule
            // Tuple (.Net 4+)

            ///////////////////////////////////////
            // Opérateurs
            ///////////////////////////////////////
            Console.WriteLine("\n->Opérateurs");

            int i1 = 1, i2 = 2; // Raccourci pour des déclarations multiples

            // Arithmétique classique
            Console.WriteLine(i1 + i2 - i1 * 3 / 7); // => 3

            // Modulo
            Console.WriteLine("11%3 = " + (11 % 3)); // => 2

            // Opérateurs de comparaison
            Console.WriteLine("3 == 2? " + (3 == 2)); // => False
            Console.WriteLine("3 != 2? " + (3 != 2)); // => True
            Console.WriteLine("3 > 2? " + (3 > 2)); // => True
            Console.WriteLine("3 < 2? " + (3 < 2)); // => False
            Console.WriteLine("2 <= 2? " + (2 <= 2)); // => True
            Console.WriteLine("2 >= 2? " + (2 >= 2)); // => True

            // Opérateurs bit à bit !
            /*
            ~       Compément unaire
            <<      Décalage à gauche
            >>      Décalage à droite
            &       ET logique
            ^       OU exclusif
            |       OU inclusif
            */

            // Incrémentations
            int i = 0;
            Console.WriteLine("\n->Inc/Dec-rementation");
            Console.WriteLine(i++); //i = 1. Post-Incrémentation
            Console.WriteLine(++i); //i = 2. Pre-Incrémentation
            Console.WriteLine(i--); //i = 1. Post-Decrémentation
            Console.WriteLine(--i); //i = 0. Pre-Decrémentation

            ///////////////////////////////////////
            // Structures de contrôle
            ///////////////////////////////////////
            Console.WriteLine("\n->Structures de contrôle");

            // Structure conditionnelle
            int j = 10;
            if (j == 10)
            {
                Console.WriteLine("Je serai affiché");
            }
            else if (j > 10)
            {
                Console.WriteLine("Pas moi");
            }
            else
            {
                Console.WriteLine("Moi non plus");
            }

            // Opérateur ternaire
            // Un simple if/else peut s'écrire :
            // <condition> ? <valeur si true> : <valeur si false>
            int toCompare = 17;
            string isTrue = toCompare == 17 ? "True" : "False";

            // Boucle while
            int fooWhile = 0;
            while (fooWhile < 100)
            {
                // 100 passages, de 0 à 99
                fooWhile++;
            }

            // Boucle Do While
            int fooDoWhile = 0;
            do
            {
                // 100 passages, de 0 à 99
                fooDoWhile++;
            } while (fooDoWhile < 100);

            // Boucle for 
            // Structure : for(<etat_initial>; <condition>; <pas>)
            for (int fooFor = 0; fooFor < 10; fooFor++)
            {
                // 10 passages, de 0 à 9
            }

            // La boucle foreach
            // Structure : foreach(<type_iterateur> <nom_iterateur> in <enumerable>)
            // Cette boucle est utilisable sur des objets implémentant IEnumerable ou IEnumerable<T>
            // Toutes les collections du framework .NET (Tableaux, Listes, ...) implémentent ces interfaces.
            // (Notez que dans l'exemple suivant .ToCharArray() peut être omit car
            //  string implémente IEnumerable)
            foreach (char character in "Hello World".ToCharArray())
            {
                //Itération sur chaque caractère
            }

            // La structure Switch Case
            // Un switch fonctionne avec les types : byte, short, char et int.
            // Les enums sont aussi supportés ainsi que les chaînes de caractères et quelques
            // classes spéciales basées sur les types primitifs : Character, Byte, Short et Integer.
            int mois = 3;
            string moisString;
            switch (mois)
            {
                case 1:
                    moisString = "Janvier";
                    break;
                case 2:
                    moisString = "Février";
                    break;
                case 3:
                    moisString = "Mars";
                    break;

                // Vous pouvez assigner plus d'un 'case' à une action
                // Mais vous ne pouvez pas ajouter une action sans 'break' avant un 'case'
                // (pour ce faire, il faudrait ajouter explicitement un 'goto case x')
                case 6:
                case 7:
                case 8:
                    moisString = "C'est l'été!";
                    break;
                default:
                    moisString = "Un autre mois oO";
                    break;
            }

            ///////////////////////////////////////
            // conversion de type de donnée et transtypage
            ///////////////////////////////////////

            // conversion de string vers int
            // lève une exception en cas d'erreur
            int.Parse("123"); //retourne la valeur entière de "123"

            // TryParse affecte la valeur par défaut du type en cas d'erreur
            // dans ce cas : 0
            int tryInt;
            if (int.TryParse("123", out tryInt)) // La fonction retourne un booléen
                Console.WriteLine(tryInt);       // => 123

            // conversion d'un entier vers une chaîne de caractères
            // La classe Convert possède plusieurs méthodes pour faciliter la conversion
            Convert.ToString(123);
            // ou
            tryInt.ToString();
        }

        ///////////////////////////////////////
        // CLASSES - voir les définitions à la fin du fichier
        ///////////////////////////////////////

        public static void Classes()
        {
            // voir les déclarations à la fin du fichier

            // Utilisez 'new' pour instancier une classe
            Bicycle trek = new Bicycle();

            // Appel des méthodes de l'objet
            trek.SpeedUp(3); // Il est toujours bon d'utiliser des accesseurs
            trek.Cadence = 100;

            // Affichage de la valeur de retour d'une méthode.
            Console.WriteLine("trek info: " + trek.Info());

            // Instanciation d'un nouveau PennyFarthing
            PennyFarthing funbike = new PennyFarthing(1, 10);
            Console.WriteLine("funbike info: " + funbike.Info());

            Console.Read();
        }

        // POINT D'ENTRÉE - Une application console doit avoir une méthode main comme point d'entrée
        public static void Main(string[] args)
        {
            OtherInterestingFeatures();
        }

        //
        // FONCTIONNALITÉS INTÉRÉSSANTES
        //
        
        // SIGNATURE DE METHODE
        public // Visibilité
        static // Permet un appel direct par la classe (sans instanciation) 
        int // Type de retour,
        MethodSignatures(
            int maxCount, // Premier paramètre, de type int
            int count = 0, // Valeur par défaut si aucun argument n'est passé
            int another = 3,
            params string[] otherParams // Capture tous les arguments passés à la méthode
        )
        { 
            return -1;
        }

        // Des méthodes peuvent avoir le même nom tant que leur signature est unique
        public static void MethodSignature(string maxCount)
        {
        }

        // TYPE GÉNÉRIQUE
        
        // Les types TKey et TValue sont spécifiés par l'utilisateur lors de l'appel de la fonction
        // Cette méthode émule SetDefaut de Python
        public static TValue SetDefault<TKey, TValue>(
            IDictionary<TKey, TValue> dictionary, 
            TKey key, 
            TValue defaultItem)
        {
            TValue result;
            if (!dictionary.TryGetValue(key, out result))
                return dictionary[key] = defaultItem;
            return result;
        }

        // Vous pouvez limiter les types autorisés
        public static void IterateAndPrint<T>(T toPrint) where T: IEnumerable<int>
        {
            // Nous sommes sûrs de pouvoir itérer, car T implémente IEnumerable<int>
            foreach (var item in toPrint)
                // Item sera de type int
                Console.WriteLine(item.ToString());
        }

        public static void OtherInterestingFeatures()
        {
            // PARAMÈTERES OPTIONNELS
            MethodSignatures(3, 1, 3, "Des", "Paramètres", "En plus");
            MethodSignatures(3, another: 3); // affectation explicite, les autres 
                                             // paramètres ont la valeur par défaut

            // MÉTHODE D'EXTENSION
            int i = 3;
            i.Print(); // Définit plus bas

            // TYPES NULLABLE - idéal pour les interactions avec une base de données ou pour les valeurs de retour
            // Tous les types valeurs peuvent être rendus nullable en les suffixant par '?'
            // <type>? <nom> = <value>
            int? nullable = null; // raccourci pour Nullable<int>
            Console.WriteLine("Nullable variable: " + nullable);
            bool hasValue = nullable.HasValue; // retourne vrai si la valeur n'est pas null

            // ?? est un sucre syntaxique pour spécifier une valeur par défaut
            // au cas ou une autre valeur serait nulle
            int notNullable = nullable ?? 0; // 0

            // VARIABLES IMPLICITEMENT TYPÉES - vous pouvez laisser le compilateur deviner le type d'une variable
            var magic = "magic est de type string à la compilation. On a toujours un typage fort !";
            // magic = 9; // ne fonctionnera pas car magic est désormais une chaîne de caractères

            // TYPES GÉNÉRIQUES
            var agenda = new Dictionary<string, string>() { 
                {"Sarah", "212 555 5555"} // Ajout d'une entrée à notre agenda
            };

            // Appel de la fonction SetDefault (définie plus haut)
            Console.WriteLine(SetDefault<string,string>(agenda, "Shaun", "Pas de numéro")); // => Pas de numéro
            // Notez que vous n'avez pas à spécifier TKey et TValue car le compilateur saura les inférer.
            Console.WriteLine(SetDefault(agenda, "Sarah", "No Phone")); // => 212 555 5555

            // EXPRESSION LAMBDA - permet d'écrire une fonction en tant qu'expression
            Func<int, int> square = (x) => x * x; // La dernière expression est la valeur de retour
            Console.WriteLine(square(3)); // => 9

            // GESTION AUTOMATIQUE DES RESSOURCES - vous permet de manipuler facilement des resources non-managées
            // La plus part des objets qui accèdent à des ressources non-managées (handle de fichier, périphérique, etc.)
            // implémentent l'interface IDisposable. L'instruction using prend soin
            // de libérer les objets IDisposable proprement à votre place.
            using (StreamWriter writer = new StreamWriter("log.txt"))
            {
                writer.WriteLine("Rien à signaler");
                // À la fin de cette portée les ressources seront libérées.
                // Même si une exception est levée.
            } 

            // BIBLIOTHÈQUE DE TÂCHES PARALLÈLES (TPL)
            // http://msdn.microsoft.com/fr-fr/library/dd460717.aspx
            var websites = new string[] { 
                "http://www.google.com", "http://www.reddit.com", 
                "http://www.shaunmccarthy.com"
            };
            var responses = new Dictionary<string, string>();
            
            // L'exemple suivant exécutera chaque requête dans un thread séparé,
            // et attendra la fin de chacun d'entre eux avant de continuer
            Parallel.ForEach(websites, 
                new ParallelOptions() {MaxDegreeOfParallelism = 3}, // maximum de 3 threads
                website =>
            {
                // Fait quelque chose de long
                using (var r = WebRequest.Create(new Uri(website)).GetResponse())
                {
                    responses[website] = r.ContentType;
                }
            });

            // Ceci ne s'exécutera pas tant que les threads n'auront pas fini leur travail
            foreach (var key in responses.Keys)
                Console.WriteLine("{0}:{1}", key, responses[key]);

            // TYPE DYNAMIQUE - idéal pour travailler avec d'autres langages
            dynamic student = new ExpandoObject();
            student.FirstName = "Mon prénom"; // Pas besoin de définir l'objet

            // Vous pouvez même ajouter des méthodes (dans cet exemple : la méthode prend une chaîne de caractères et retourne une chaîne de caractères)
            student.Introduce = new Func<string, string>(
                (introduceTo) => string.Format("Hey {0}, c'est {1}", student.FirstName, introduceTo));
            Console.WriteLine(student.Introduce("Beth"));

            // IQUERYABLE<T> - quasiment toutes les collections implémentent cette interface
            // ce qui permet d'utiliser des méthodes de style 'Filter' / 'Map' / 'Reduce' 
            var bikes = new List<Bicycle>();
            bikes.Sort(); // Trie le tableau sur place
            bikes.Sort((b1, b2) => b1.Wheels.CompareTo(b2.Wheels)); // Trie en se basant sur la propriété Wheels
            var result = bikes
                .Where(b => b.Wheels > 3) // 'Filter' - enchaînable (retourne un IQueryable du type précédent)
                .Where(b => b.IsBroken && b.HasTassles)
                .Select(b => b.ToString()); // 'Map' - on retourne le .ToString() de chaque élément filtré,
                                            // le résultat est un IQueryable<string>

            var sum = bikes.Sum(b => b.Wheels); // 'Reduce' - fait la somme de tous les Wheels de la liste

            // Creation d'une liste d'objet anonymes basés sur des paramètres de la classe Bike
            var bikeSummaries = bikes.Select(b=>new { Name = b.Name, IsAwesome = !b.IsBroken && b.HasTassles });
            // Le compilateur peut inférer le type de ces objets anonymes, permettant à certains IDE d'effectuer 
            // des autos-complétion.
            foreach (var bikeSummary in bikeSummaries.Where(b => b.IsAwesome))
                Console.WriteLine(bikeSummary.Name);


            // ASPARALLEL
            // C'est ici que les choses se compliquent - un mélange de LINQ et de TPL
            var threeWheelers = bikes.AsParallel().Where(b => b.Wheels == 3).Select(b => b.Name);
            // La ligne précédente s'exécute en parallèle ! Des threads seront gérés automatiquement
            // et les données y seront réparties. Idéal sur de grosses données (et si votre 
            // machine dispose de plusieurs coeurs)


            // LINQ - lie une source de données à des objets IQueryable<T>
            // ex : LindToSql => liaison avec une base de données, LinqToXml => liaison avec un document xml
            var db = new BikeRespository();

            // l'exécution est décalée, ce qui est préférable quand on travaille sur une base données
            var filter = db.Bikes.Where(b => b.HasTassles); // pas de requête exécutée
            if (42 > 6) //  Vous pouvez continuer à affiner la recherche
                filter = filter.Where(b => b.IsBroken); // pas de requête exécutée

            var query = filter
                .OrderBy(b => b.Wheels)
                .ThenBy(b => b.Name)
                .Select(b => b.Name); // toujours pas de requête exécutée

            // Maintenant la requête est exécutée, mais retourne des données uniquement au fil de l'itération
            foreach (string bike in query) 
                Console.WriteLine(result);
            
        }

    } // Fin de la classe LearnCSharp

    // Il est possible d'inclure plusieurs classes dans un fichier .cs

    public static class Extensions
    {
        // EXTENSION DE FONCTIONS
        public static void Print(this object obj)
        {
            Console.WriteLine(obj.ToString());
        }
    }

    // Syntaxe de déclaration de classe :
    // <public/private/protected/internal> class <class name>{
    //    // champs, constructeurs, fonctions
    //    // tout est déclaré et implémenté à l'intérieur
    // }

    public class Bicycle
    {
        // Propriétés et variable de la classe
        public int Cadence // Public : peut être accédé de partout
        {
            get // get - définit une méthode pour lire la propriété
            {
                return _cadence;
            }
            set // set - définit une méthode pour affecter une valeur à la propriété
            {
                _cadence = value; // 'value' est la valeur passée en argument au setteur
            }
        }
        private int _cadence;

        protected virtual int Gear // Protected : accessible depuis la classe et ses classes filles
        {
            get; // crée une propriété automatique, pas besoin de créer une variable de stockage
            set;
        }

        internal int Wheels // Internal : accessible depuis l'assembly
        {
            get;
            private set; // Il est possible de choisir la portée d'un accesseur
        }

        int _speed; // Par défaut tout est privé au sein d'une classe : accessible uniquement depuis la classe
                    // on peut ajouter explicitement le mot clé 'private'

        public string Name { get; set; }


        // Enum est un type valeur formé par un ensemble de constantes nommées
        // C'est simplement une manière de mettre un nom sur une valeur (int par défaut).
        // Les types compatibles pour un enum sont : byte, sbyte, short, ushort, int, uint, long et ulong.
        // Un enum ne peut pas contenir deux fois la même valeur
        public enum BikeBrand
        {
            AIST,
            BMC,
            Electra = 42, // il est possible de donner explicitement une valeur
            Gitane // 43
        }
        // Nous avons défini cet enum à l'intérieur de la classe Bicycle, c'est donc un type imbriqué
        // Pour le référencer à l'extérieur, il faudra utiliser Bicycle.BikeBrand

        public BikeBrand Brand; // Après avoir déclaré notre type enum, on peut créer un champ de ce type

        // Les membres statiques appartiennent à une classe plutôt qu'à une instance particulière
        // Il est possible d'y accéder sans passer par un objet :
        // ex : Console.WriteLine("Bicycles créés : " + Bicycle.bicyclesCreated);
        static public int BicyclesCreated = 0;

        // Les valeurs en lecture seule sont affectées lors de l'exécution
        // Elles ne peuvent être assignées que lors de leur déclaration ou dans un constructeur
        readonly bool _hasCardsInSpokes = false; // variable en lecture et privée

        // Les constructeurs sont un moyen de créer des objets
        // Voici un constructeur par défaut (pas d'arguments)
        public Bicycle() 
        {
            this.Gear = 1; // accès aux membres de la classe via le mot clé this
            Cadence = 50;  // qui est souvent implicite
            _speed = 5;
            Name = "Bontrager";
            Brand = BikeBrand.AIST;
            BicyclesCreated++;
        }

        // Voici un constructeur spécifique (qui prend des arguments)
        public Bicycle(int startCadence, int startSpeed, int startGear,
                       string name, bool hasCardsInSpokes, BikeBrand brand) 
            : base() // possibilité d'appeler le constructeur de la classe mère (ici Object)
        {
            Gear = startGear; 
            Cadence = startCadence;
            _speed = startSpeed;
            Name = name; 
            _hasCardsInSpokes = hasCardsInSpokes;
            Brand = brand;
        }

        // Les constructeurs peuvent s'enchaîner
        public Bicycle(int startCadence, int startSpeed, BikeBrand brand) :
            this(startCadence, startSpeed, 0, "big wheels", true, brand)
        {
        }

        // Syntaxe de méthode :
        // <public/private/protected> <type de retour> <nom de methode>(<args>)

        // Les classes peuvent implémenter des accesseurs pour leurs champs
        // ou implémenter des propriétés (c'est la méthode dominante en C#)

        // Les paramètres de méthodes peuvent avoir des valeurs par défaut
        // Dans ce cas, la méthode peut être appelée sans arguments
        public void SpeedUp(int increment = 1)
        {
            _speed += increment;
        }

        public void SlowDown(int decrement = 1)
        {
            _speed -= decrement;
        }

        // Les propriétés se chargent de lire/modifier des valeurs
        // elles peuvent être en lecture(get), en écriture(set) ou les deux
        private bool _hasTassles; // variable privée
        public bool HasTassles // propriété publique
        {
            get { return _hasTassles; }
            set { _hasTassles = value; }
        }

        // Il est possible de définir une propriété automatique sur une ligne
        // cette syntaxe créera une variable de stockage automatiquement.
        // Il est possible de modifier l'accèsibilité des getter/setter pour limiter leur utilisation
        public bool IsBroken { get; private set; }

        // La même chose sur plusieurs lignes
        public int FrameSize
        {
            get;
            // Notez que seule la classe Bicycle peut changer la valeur de FrameSize
            private set;
        }

        // Méthode qui affiche la valeur des champs de cet objet
        public virtual string Info()
        {
            return "Gear: " + Gear +
                    " Cadence: " + Cadence +
                    " Speed: " + _speed +
                    " Name: " + Name +
                    " Cards in Spokes: " + (_hasCardsInSpokes ? "yes" : "no") +
                    "\n------------------------------\n"
                    ;
        }

        // Les méthodes peuvent aussi être statiques. Utile pour les méthodes d'aide.
        public static bool DidWeCreateEnoughBycles()
        {
            // À l'intérieur d'une méthode statique on ne peut que référencer des membres statiques !
            return BicyclesCreated > 9000;
        } // Si votre classe n'a que des membres statiques, marquez la comme statique 

    } // fin de la classe Bicycle

    // PennyFarthing est une classe dérivée de Bicycle
    class PennyFarthing : Bicycle
    {
        // Appel au constructeur de la classe mère
        public PennyFarthing(int startCadence, int startSpeed) :
            base(startCadence, startSpeed, 0, "PennyFarthing", true, BikeBrand.Electra)
        {
        }

        protected override int Gear
        {
            get
            {
                return 0;
            }
            set
            {
                // Lève une exception
                throw new ArgumentException("Impossible de modifier Gear sur un PennyFarthing");
            }
        }

        public override string Info()
        {
            string result = "PennyFarthing bicycle ";
            result += base.ToString(); // Appel à la version de base de cette méthode
            return result;
        }
    }

    // Les interfaces contiennent uniquement la signature de leurs membres, sans implémentation.
    interface IJumpable
    {
        void Jump(int meters); // Tous les membres d'interface sont publics par défaut
    }

    interface IBreakable
    {
        bool Broken { get; } // Les interfaces peuvent contenir des propriétés, 
                             // des méthodes et des évènements
    }

    // Une classe ne peut hériter que d'une seule autre classe, mais peut implémenter plusieurs interfaces
    class MountainBike : Bicycle, IJumpable, IBreakable
    {
        int damage = 0;

        public void Jump(int meters)
        {
            damage += meters;
        }

        public bool Broken
        {
            get
            {
                return damage > 100;
            }
        }
    }

    /// <summary>
    /// Utilisé pour illustrer la connexion à une base donnée dans l'exemple LinqToSql
    /// L'approche code first d'EntityFramework est très pratique (un peu comme ActiveRecord de Ruby) 
    /// http://msdn.microsoft.com/fr-fr/data/jj193542.aspx
    /// </summary>
    public class BikeRespository : DbSet
    {
        public BikeRespository()
            : base()
        {
        }

        public DbSet<Bicycle> Bikes { get; set; }
    }
} // Fin du namespace
```

## Sujets non-abordés

 * Flags
 * Attribus
 * Propriétés statiques
 * Exceptions, Abstraction
 * ASP.NET (Web Forms/MVC/WebMatrix)
 * Winforms
 * Windows Presentation Foundation (WPF)

## Lectures Complémentaires

 * [DotNetPerls](http://www.dotnetperls.com)
 * [C# in Depth](http://manning.com/skeet2)
 * [Programming C#](http://shop.oreilly.com/product/0636920024064.do)
 * [LINQ](http://shop.oreilly.com/product/9780596519254.do)
 * [MSDN Library](http://msdn.microsoft.com/en-us/library/618ayhy6.aspx)
 * [ASP.NET MVC Tutorials](http://www.asp.net/mvc/tutorials)
 * [ASP.NET Web Matrix Tutorials](http://www.asp.net/web-pages/tutorials)
 * [ASP.NET Web Forms Tutorials](http://www.asp.net/web-forms/tutorials)
 * [Windows Forms Programming in C#](http://www.amazon.com/Windows-Forms-Programming-Chris-Sells/dp/0321116208)

[Convention de codage C#](http://msdn.microsoft.com/library/vstudio/ff926074)
