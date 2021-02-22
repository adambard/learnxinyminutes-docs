---
language: c++
filename: learncpp-fr.cpp
contributors:
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Matt Kline", "https://github.com/mrkline"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Connor Waters", "http://github.com/connorwaters"]
translators:
    - ["Xuan-thi Nguyen", "http://github.com/mellenguyen"]
lang: fr-fr
---

C++ est un langage de programmation système qui,
[selon son créateur Bjarne Stroustrup](http://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2014/Keynote),
fut créé pour

- être un "C amélioré"
- gérer l'abstraction des données
- gérer la programmation orienté objet
- gérer la programmation générique

Bien que sa syntaxe puisse être plus difficile ou complexe que des langages
récents, il est largement utilisé car il compile en instructions natives qui
peuvent être directement exécutées par le processeur et offre un contrôle
rigoureux du matériel (comme le C) tout en fournissant des caractéristiques de
haut niveau telles que la généricité, les exceptions et les classes.
Cette combinaison de vitesse et de fonctionnalités rend le C++ un des langages
de programmation les plus utilisés au monde.

```c++
/////////////////////////////////
// Comparaison avec le C
/////////////////////////////////

// C++ est _presque_ un sur-ensemble du C et partage sa syntaxe basique pour les
// déclarations de variables, les types primitifs et les fonctions.

// Tout comme en C, le point d'entrée de votre programme est une fonction
// appelée main, avec un integer comme type de retour.
// Cette valeur constitue l'état de fin d'exécution du programme.
// Voir http://en.wikipedia.org/wiki/Exit_status pour plus d'informations.
int main(int argc, char** argv)
{
    // Les arguments de ligne de commande sont passés avec argc et argv de la
    // même manière qu'en C.
    // argc indique le nombre d'arguments,
    // et argv est un tableau de chaînes façon C (char*)
    // représentant les arguments.
    // Le premier argument est le nom par lequel le programme est appelé.
    // argc et argv peuvent être omis si vous ne vous souciez pas des
    // arguments, nous donnant comme signature de fonction int main()

    // Un état de fin d'exécution 0 indique le succès.
    return 0;
}

// Cependant, C++ varie du C selon certains éléments:

// En C++, les caractères littéraux sont des chars
sizeof('c') == sizeof(char) == 1

// En C, les caractères littéraux sont des ints
sizeof('c') == sizeof(int)

// C++ a un prototypage strict
void func(); // fonction qui ne prend aucun argument

// En C
void func(); // fonction qui peut prendre n'importe quel nombre d'arguments

// Utilise nullptr au lieu de NULL en C++
int* ip = nullptr;

// Les en-têtes standards du C sont disponibles en C++,
// mais sont préfixés avec "c" et n'ont pas de suffixe .h
#include <cstdio>

int main()
{
    printf("Bonjour tout le monde!\n");
    return 0;
}

/////////////////////////////////
// Surchage de fonctions
/////////////////////////////////

// C++ gère la surchage de fonctions
// Chaque fonction fournie prend différents paramètres.

void print(char const* maChaine)
{
    printf("Chaîne %s\n", maChaine);
}

void print(int monEntier)
{
    printf("Mon entier est %d", monEntier);
}

int main()
{
    print("Bonjour"); // Utilise void print(const char*)
    print(15); // Utilise void print(int)
}

/////////////////////////////////////////////
// Arguments par défaut de fonctions
/////////////////////////////////////////////

// Vous pouvez fournir des arguments par défaut pour une fonction s'ils ne sont
// pas fournis par l'appelant.

void faitDesChosesAvecDesEntiers(int a = 1, int b = 4)
{
    // Do something with the ints here
}

int main()
{
    faitDesChosesAvecDesEntiers();      // a = 1,  b = 4
    faitDesChosesAvecDesEntiers(20);    // a = 20, b = 4
    faitDesChosesAvecDesEntiers(20, 5); // a = 20, b = 5
}

// Les arguments par défaut doivent être à la fin de la liste des arguments.

void invalidDeclaration(int a = 1, int b) // Erreur !
{
}


//////////////////////////
// Espaces de nom
//////////////////////////

// Les espaces de nom fournissent une séparation des portées pour les
// variables, fonctions, et autres déclarations.
// Les espaces de nom peuvent être imbriqués.

namespace Premier {
    namespace Imbrique {
        void foo()
        {
            printf("Ceci est le Premier::Imbrique::foo\n");
        }
    } // fin de l'espace de nom Imbrique
} // fin de l'espace de nom Premier

namespace Second {
    void foo()
    {
        printf("Ceci est le Second::foo\n")
    }
}

void foo()
{
    printf("Ceci est un foo global\n");
}

int main()
{
    // Inclut tous les symboles de l'espace de nom Second dans la portée
    // actuelle. Notez que le foo() simple ne marche plus, car l'appel est
    // ambigu entre le foo de l'espace de nom Second et celui de premier
    // niveau.
    using namespace Second;

    Second::foo(); // imprime "Ceci est le Second::foo"
    Premier::Imbrique::foo(); // imprime "Ceci est le Premier::Imbrique::foo"
    ::foo(); // imprime "Ceci est un foo global"
}

/////////////////////////
// Entrée/Sortie
/////////////////////////

// Les entrées et sorties en C++ utilisent des flux (streams)
// cin, cout et cerr représentent stdin, stdout et stderr.
// << est l'opérateur d'insertion et >> est l'opérateur d'extraction.

#include <iostream> // Inclusion pour les flux d'entrée/sortie

// Les flux sont dans l'espace de nom std (librairie standard)
using namespace std;

int main()
{
   int monEntier;

   // Affiche sur stdout (ou le terminal/l'écran)
   cout << "Entrez votre chiffre favori:\n";
   // Prend l'entrée clavier
   cin >> monEntier;

   // cout peut également être formaté
   cout << "Votre chiffre favori est " << monEntier << "\n";
   // imprime "Votre chiffre favori est <monEntier>"

    cerr << "Utilisé pour les messages d'erreurs";
}

/////////////////////////////////
// Chaînes de caractères
/////////////////////////////////

// Les chaînes de caractères en C++ sont des objets et ont plusieurs fonctions
// membres
#include <string>

// Les chaînes de caractères sont aussi dans l'espace de
// nom std (librairie standard)
using namespace std;

string maChaine = "Bonjour";
string monAutreChaine = " tout le monde !";

// + est utilisé pour la concaténation.
cout << maChaine + monAutreChaine; // Bonjour tout le monde !"

cout << maChaine + " toi !"; // "Bonjour toi !"

// Les chaînes de caractères C++ sont mutables.
maChaine.append(" le chien !");
cout << maChaine; // "Bonjour le chien !"


//////////////////////
// Références
//////////////////////

// En plus des pointeurs comme ceux en C,
// C++ possède des _références_.
// Ce sont des types de pointeurs qui ne peuvent pas être réassignés
// une fois initialisés, et ne peuvent pas être nulles.
// Ils partagent la même syntaxe que les variables elles-mêmes:
// les * ne sont pas nécessaires pour les déréférencer et
// & (addresse de) n'est pas utilisé pour l'assignement.

using namespace std;

string foo = "Je suis foo";
string bar = "Je suis bar";


string& fooRef = foo; // Ceci créé une référence à foo
fooRef += ". Salut!"; // Modifie foo à travers la référence
cout << fooRef; // Affiche "Je suis foo. Salut!"

// Ne réassigne pas "fooRef". Ceci revient à faire "foo = bar", et
// foo == "I am bar"
// après cette ligne.
cout << &fooRef << endl; // Affiche l'adresse de foo
fooRef = bar;
cout << &fooRef << endl; // Affiche toujours l'adresse de foo
cout << fooRef;  // Affiche "Je suis bar"

// L'adresse de fooRef reste la même, c.-à-d. référence toujours foo.


const string& barRef = bar; // Créé une référence constante de bar.
// Comme en C, les valeurs constantes (et pointeurs et références) ne peuvent
// être modifiées.

// Erreur, les valeurs constantes ne peuvent être modifiées.
barRef += ". Salut!";

// Parenthèse: avant de développer le sujet des références, nous devons
// introduire un concept appelé un objet temporaire. Supposons que nous ayons
// le code suivant :
string objetTemporaireFun() { ... }
string valeurRetenu = objetTemporaireFun();

// Les différents événements se déroulant à la seconde ligne sont :
//   - un objet chaîne de caractères est retourné de objetTemporaireFun
//   - une nouvelle chaîne de caractères est construite avec la valeur
//     retournée comme argument du constructeur
//   - l'objet retourné est détruit.
// L'objet retourné est appelé un objet temporaire. Les objets temporaires sont
// créés chaque fois qu'une fonction retourne un objet, et sont détruits à la
// fin de l'évaluation de l'expression fermante (c'est ce que le standard
// énonce, mais les compilateurs sont autorisés à changer ce comportement.
// Cherchez "optimisation valeur de retour" si vous êtes intéressé par ce genre
// de détails).
// Dans cette ligne de code :
foo(bar(objetTemporaireFun()))

// en supposant que foo et bar existent, l'objet retourné de objetTemporaireFun
// est passé à bar, et est détruit avant que foo soit appelé.

// Revenons maintenant aux références. L'exception à la règle "objet détruit à
// la fin de l'expression fermante" s'applique dans le cas d'un objet
// temporaire lié à une référence constante, où sa durée de vie se voit
// prolongée à la portée courante :

void referenceConstanteObjetTemporaireFun() {
  // referenceConst prend l'objet temporaire, et est valide jusqu'à la fin de
  // la fonction.
  const string& referenceConst = objetTemporaireFun();
  ...
}

// Un autre type de référence introduit en C++11 est spécifiquement pour les
// objets temporaires. Vous ne pouvez pas avoir de variable de ce type, mais
// il prime dans la résolution de surcharge :

void fonctionFun(string& s) { ... }  // Référence régulière
void fonctionFun(string&& s) { ... }  // Référence un objet temporaire

string foo;
// Appelle la version avec référence régulière
fonctionFun(foo);

// Appelle la version avec référence temporaire
fonctionFun(objetTemporaireFun());

// Par exemple, vous aurez ces deux versions de constructeurs pour
// std::basic_string :
basic_string(const basic_string& other);
basic_string(basic_string&& other);

// L'idéal étant de construire une nouvelle chaîne de caractères avec un objet
// temporaire (qui sera détruit de toute façon), nous pouvons ainsi avoir un
// constructeur qui "sauve" des parties de cette chaîne de caractères
// temporaire. Vous verrez ce concept sous le nom de "sémantique de mouvement".

////////////////////////
// Enumérations
////////////////////////

// Les énumérations sont un moyen d'assigner une valeur à une constante
// fréquemment utilisée pour une meilleure visualisation et lecture du code.
enum ETypesDeVoitures
{
  Berline,
  Hayon,
  4x4,
  Break
};

ETypesDeVoitures ObtenirVoiturePreferee()
{
	return ETypesDeVoitures::Hayon;
}

// En C++11, il existe une manière simple d'assigner un type à une énumération,
// ce qui peut-être utile en sérialisation de données et conversion
// d'énumérations entre le type voulu et ses constantes respectives.
enum ETypesDeVoitures : uint8_t
{
  Berline, // 0
  Hayon, // 1
  4x4 = 254, // 254
  Hybride // 255
};

void EcrireOctetDansLeFichier(uint8_t ValeurEntree)
{
	// Sérialise la valeur d'entrée dans un fichier
}

void EcrireTypeVoiturePrefereDansLeFichier(ETypesDeVoitures TypeVoitureEntree)
{
	// L'énumération est implicitement convertie en uint8_t du à la déclaration
    // de son type d'énumération
	EcrireOctetDansLeFichier(TypeVoitureEntree);
}

// D'autre part, vous pourriez ne pas vouloir que des énumérations soient
// accidentellement converties en entiers ou en d'autres énumérations. Il est
// donc possible de créer une classe d'énumération qui ne sera pas
// implicitement convertie.
enum class ETypesDeVoitures : uint8_t
{
  Berline, // 0
  Hayon, // 1
  4x4 = 254, // 254
  Hybride // 255
};

void EcrireOctetDansLeFichier(uint8_t ValeurEntree)
{
	// Sérialise la valeur d'entrée dans un fichier
}

void EcrireTypeVoiturePrefereDansLeFichier(ETypesDeVoitures TypeVoitureEntree)
{
	// Ne compilera pas même si ETypesDeVoitures est un uint8_t car
	// l'énumération est déclarée en tant que "classe d'énumération" !
	EcrireOctetDansLeFichier(TypeVoitureEntree);
}

///////////////////////////////////////////////////
// Classes et programmation orientée objet
///////////////////////////////////////////////////

#include <iostream>

// Déclare une classe.
// Les classes sont habituellement déclarées dans les fichiers d'en-tête (.h ou .hpp).
class Chien {
    // Les variables et fonctions membres sont privées par défaut.
    std::string nom;
    int poids;

// Tous les membres suivants sont publiques jusqu'à ce que "private:" ou
// "protected:" soit trouvé
public:

    // Constructeur par défaut
    Chien();

    // Déclaractions de fonctions membres (implémentations à suivre)
    // Notez que nous utilisons std::string ici au lieu de placer
    // using namespace std;
    // au-dessus.
    // Ne jamais utiliser une instruction "using namespace" dans l'en-tête.
    void initialiserNom(const std::string& nomDuChien);

    void initialiserPoids(int poidsDuChien);

    // Les fonctions qui ne modifient pas l'état de l'objet devraient être
    // marquées en constantes avec const.
    // Ceci vous permet de les appeler avec une référence constante vers l'objet.
    // Notez aussi que les fonctions devant être surchargées dans des classes
    // dérivées doivent être explicitement déclarées avec _virtual_.
    // Les fonctions ne sont pas virtuelles par défault pour des raisons de
    // performances.
    virtual void imprimer() const;

    // Les fonctions peuvent également être définies à l'intérieur du corps de
    // la classe. Ces fonctions sont automatiquement "inline".
    void aboyer() const { std::cout << nom << " fait ouaf !\n"; }

    // En plus des constructeurs, C++ fournit des destructeurs.
    // Ils sont appelés quand l'objet est supprimé ou dépasse le cadre de sa
    // portée. Ceci permet de puissants paradigmes tels que RAII
    // (voir plus loin)
    // Le destructeur devrait être virtuel si la classe est abstraite;
    // s'il n'est pas virtuel, alors le destructeur de la classe dérivée ne
    // sera pas appelé si l'objet est détruit par le biais d'une référence à la
    // classe de base ou d'un pointeur.
    virtual ~Chien();

}; // Un point virgule doit clôre la définition de la classe.

// Les fonctions membres de la classe sont habituellement implémentées dans des
// fichiers .cpp.
Chien::Chien()
{
    std::cout << "Un chien a été construit\n";
}

// Les objets (comme les chaînes de caractères) devraient être passés par
// référence si vous les modifiez ou par référence constante si vous ne les
// modifiez pas.
void Chien::initialiserNom(const std::string& nomDuChien)
{
    nom = nomDuChien;
}

void Chien::initialiserPoids(int poidsDuChien)
{
    poids = poidsDuChien;
}

// Notez que le mot-clé "virtual" est nécessaire uniquement à la déclaration,
// et non à la définition.
void Chien::imprimer() const
{
    std::cout << "Le chien s'appelle " << nom << " et pèse " << poids << "kg\n";
}

Chien::~Chien()
{
    cout << "Au revoir " << nom << " !\n";
}

int main() {
    Chien monChien; // imprime "Un chien a été construit"
    monChien.initialiserNom("Barkley");
    monChien.initialiserPoids(10);
    monChien.imprime(); // imprime "Le chien s'appelle Barkley et pèse 10 kg"
    return 0;
} // prints "Au revoir Barkley !"

// Héritage :

// Cette classe hérite de toutes les propriétés publiques et protégées de la
// classe Chien ainsi que celles privées, mais n'ont pas accès direct aux
// membres et méthodes privés sans l'aide d'une méthode publique ou protégée
class ChienDomestique : public ChienDomestique {

    void definirProprietaire(const std::string& proprietaireDuChien);

    // Surcharge le comportement de la fonction d'impression pour tous les
    // ChienDomestiques.
    // Voir https://fr.wikipedia.org/wiki/Polymorphisme_(informatique)#Polymorphisme_par_sous-typage
    // pour une introduction plus générale si vous n'êtes pas familier avec le
    // concept de polymorphisme par sous-typage (appelé aussi polymorphisme
    // d'inclusion).
    // Le mot-clé "override" est optionnel mais assure que vous surchargez bien
    // la méthode de la classe de base.
    void imprimer() const override;

private:
    std::string proprietaire;
};

// Pendant ce temps, dans le fichier .cpp correspondant :

void ChienDomestique::definirProprietaire(const std::string& proprietaireDuChien)
{
    proprietaire = proprietaireDuChien;
}

void ChienDomestique::imprimer() const
{
    // Appelle la fonction "imprimer" dans la classe de base Chien
    Chien::imprimer();
    std::cout << "Le chien appartient à " << proprietaire << "\n";
    // Affiche "Le chien est <nom> et pèse <poids>"
    //         "Le chien appartient à <proprietaire>"
}

////////////////////////////////////////////////////
// Initialisation et opérateur de surcharge
////////////////////////////////////////////////////

// En C++, vous pouvez surcharger le comportement d'opérateurs tels
// que +, -, *, /, etc.
// La surcharge se fait en définissant une fonction qui sera appelée à chaque
// fois que l'opérateur sera utilisé.

#include <iostream>
using namespace std;

class Point {
public:
    // Les variables membres peuvent avoir des valeurs par défaut
    double x = 0;
    double y = 0;

    // Définit un constructeur par défaut qui ne fait rien
    // mais initialise le Point à la valeur par défaut (0, 0)
    Point() { };

    // La syntaxe suivante s'appelle une liste d'initialisation et est
    // la façon correcte d'initialiser les valeurs des membres d'une classe.
    Point (double a, double b) :
        x(a),
        y(b)
    { /* Ne fait rien à part initialiser les valeurs */ }

    // Surcharge l'opérateur +
    Point operator+(const Point& rhs) const;

    // Surcharge l'opérateur +=
    Point& operator+=(const Point& rhs);

    // Il serait également logique d'ajouter les opérateurs - et -=,
    // mais nous les éclipsons par soucis de concision.
};

Point Point::operator+(const Point& rhs) const
{
    // Créé un nouveau point qui est la somme de celui-ci de rhs.
    return Point(x + rhs.x, y + rhs.y);
}

Point& Point::operator+=(const Point& rhs)
{
    x += rhs.x;
    y += rhs.y;
    return *this;
}

int main () {
    Point haut (0,1);
    Point droite (1,0);
    // Appelle l'opérateur + du Point
    // Le point "haut" appelle la fonction + avec "droite" comme paramètre
    Point resultat = haut + droite;
    // Affiche "Le résultat est haut-droite (1,1)"
    cout << "Le résultat est haut-droite (" << resultat.x << ','
         << resultat.y << ")\n";
    return 0;
}

////////////////////////////////
// Patrons (templates)
////////////////////////////////

// Les templates (patrons) en C++ sont majoritairement
// utilisés pour la programmation générique, bien qu'ils soient bien plus
// puissants que les constructeurs génériques dans d'autres langages.
// Ils gèrent également la spécialisation explicite et partielle ainsi que
// les classes fonctionnelles; en fait, ils sont un langage fonctionnelles
// Turing-complete embedded in C++ !

// Nous commencons avec le genre de programmation générique auquel vous êtes
// peut-être familier. Pour définir une classe ou fonction qui prend un type de
// paramètre particulier :
template<class T>
class Boite {
public:
    // Dans cette classe, T représente n'importe quel type possible.
    void inserer(const T&) { ... }
};

// Pendant la compilation, le compilateur génère des copies de chaque template
// avec les paramètres substitués; ainsi, la définition complète de chaque
// classe doit être présente à chaque appel. C'est pourquoi vous verrez les
// classes de templates définies entièrement dans les fichiers d'en-tête.

// Pour instancier une classe de template sur la pile ("stack") :
Boite<int> boiteDEntiers;

// et vous pouvez l'utiliser comme prévu :
boiteDEntiers.inserer(123);

// Vous pouvez, bien sûr, imbriquer les templates :
Boite<Boite<int> > boiteDeBoites;
boiteDeBoites.inserer(boiteDEntiers);

// Jusqu'à C++11, il était nécessaire de placer un espace entre les deux '>'s,
// sinon '>>' était parsé en tant qu'opérateur de décalage vers la droite.

// Vous croiserez peut-être cette syntaxe
//   template<typename T>
// à la place. Les mot-clé 'class' et 'typename' sont _généralement_
// interchangeables. Pour plus d'explications, allez à
//   http://en.wikipedia.org/wiki/Typename
// ou
// https://fr.wikibooks.org/wiki/Programmation_C-C%2B%2B/Les_templates/Mot-cl%C3%A9_typename
// (oui, ce mot-clé a sa propre page Wikipedia).

// De manière similaire, un patron de fonction :
template<class T>
void aboyerTroisFois(const T& entree)
{
    entree.aboyer();
    entree.aboyer();
    entree.aboyer();
}

// Remarquez ici que rien n'est spécifié à propos du type du paramètre. Le
// compilateur va générer et vérifier le type à chaque appel du patron, c'est
// pourquoi l'appel de fonction suivant marche pour n'importe quel type 'T' qui
// a une méthode constante 'aboyer' !

Chien docile;
docile.initialiserNom("Docile")
aboyerTroisFois(docile); // Affiche "Docile fait ouaf !" trois fois.

// Les paramètres génériques (ou paramètres template) ne sont pas forcément des
// classes :
template<int Y>
void imprimerMessage() {
  cout << "Apprenez le C++ en " << Y << " minutes !" << endl;
}

// Vous pouvez explicitement spécialiser les templates pour un code plus
// optimisé. Bien sûr, les utilisations effectives de la spécialisation ne sont
// pas aussi triviales que celle-ci.
// Notez que vous avez toujours besoin de déclarer la fonction (ou classe)
// comme template, même si vous spécifiez explicitement tous les paramètres.
template<>
void imprimerMessage<10>() {
  cout << "Apprenez le C++ plus vite en seulement 10 minutes !" << endl;
}

// Affiche "Apprenez le C++ en 20 minutes !"
imprimerMessage<20>();
// Affiche "Apprenez le C++ plus vite en seulement 10 minutes !"
imprimerMessage<10>();

//////////////////////////////////
// Gestion des exceptions
//////////////////////////////////

// La bibliothèque standard fournit quelques types d'exception
// (voir http://en.cppreference.com/w/cpp/error/exception)
// mais n'importe quel type peut être lancé en tant qu'exception.
#include <exception>
#include <stdexcept>

// Toutes les exceptions lancées à l'intérieur d'un block _try_ peuvent être
// attrapées par les blocs de traitement d'erreurs (_catch_ handlers).
try {
    // N'allouez pas des exceptions sur le tas (heap) en utilisant _new_.
    throw std::runtime_error("Un problème s'est produit");
}

// Attrapez les exceptions avec des références constantes si ce sont des objets
catch (const std::exception& ex)
{
    std::cout << ex.what();
}

// Attrape n'importe quelle exception non attrapée par les blocs _catch_
// précédents
catch (...)
{
    std::cout << "Exception inconnue attrapée";
    throw; // Re-lance l'exception
}

////////////////
// RAII
////////////////

// RAII signifie "Resource Acquisition Is Initialization", soit l'Acquisition
// d'une Ressource est une Initialisation en français.
// Il est souvent considéré comme le paradigme le plus puissant en C++ et
// est le concept simple qu'un constructeur d'un objet acquiert les ressources
// d'un objet et que le destructeur les libère.

// Afin de comprendre son utilité, considérons une fonction qui utilise la
// gestion d'un fichier C :
void faireQuelqueChoseAvecUnFichier(const char* nomDuFichier)
{
    // Pour commencer, supposons que rien ne peut échouer.

    FILE* fh = fopen(nomDuFichier, "r"); // Ouvre le fichier en lecture

    faireQuelqueChoseAvecLeFichier(fh);
    faireAutreChoseAvec(fh);

    fclose(fh); // Ferme la gestion du fichier.
}

// Malheureusement, les choses deviennent compliquées avec la gestion
// d'erreurs. Supposons que fopen échoue, et que faireQuelqueChoseAvecLeFichier
// et faireAutreChoseAvec retournent des codes d'erreur si elles échouent.
//  (Les exceptions sont le meilleur moyen de gérer l'échec, mais des
//   programmeurs, surtout avec un passif en C,
//   sont en désaccord avec l'utilité des exceptions).
// Nous devons maintenant vérifier chaque appel en cas d'échec et fermer la
// gestion du fichier si un problème se produit.
bool faireQuelqueChoseAvecUnFichier(const char* nomDuFichier)
{
    FILE* fh = fopen(nomDuFichier, "r"); // Ouvre le fichier en mode lecture.
    if (fh == nullptr) // Le pointeur retourné est null à un échec.
        return false; // Signale cet échec à l'appelant.

    // Suppose que chaque fonction retourne faux si elle échoue
    if (!faireQuelqueChoseAvecLeFichier(fh)) {
        fclose(fh); // Ferme le flux d'entrée du fichier pour empêcher les fuites
        return false; // Propage l'erreur
    }
    if (!faireAutreChoseAvec(fh)) {
        fclose(fh);
        return false;
    }

    fclose(fh);
    return true;
}

// Les programmeurs en C clarifient souvent tout cela en utilisant goto :
bool faireQuelqueChoseAvecUnFichier(const char* nomDuFichier)
{
    FILE* fh = fopen(nomDuFichier, "r");
    if (fh == nullptr)
        return false;

    if (!faireQuelqueChoseAvecLeFichier(fh))
        goto echec;

    if (!faireAutreChoseAvec(fh))
        goto echec;

    fclose(fh); // Ferme la gestion du fichier
    return true; // Indique le succès

echec:
    fclose(fh);
    return false; // Propage l'erreur
}

// Si les fonctions indiquent des erreurs en utilisant des exceptions,
// les choses sont un peu plus claires, mais toujours sous-optimales.
void faireQuelqueChoseAvecUnFichier(const char* nomDuFichier)
{
    FILE* fh = fopen(nomDuFichier, "r"); // Ouvre le fichier en lecture
    if (fh == nullptr)
        throw std::runtime_error("Ouverture du fichier impossible.");

    try {
        faireQuelqueChoseAvecLeFichier(fh);
        faireAutreChoseAvec(fh);
    }
    catch (...) {
        // Assurez-vous de bien fermer le fichier si une erreur arrive
        fclose(fh);
        throw; // Puis re-lancer l'exception
    }

    fclose(fh); // Ferme le fichier
    // Tout s'est déroulé correctement
}

// Comparez ceci à l'utilisation de la classe de flux de fichier
// en C++ (fstream).
// fstream utilise son destructeur pour fermer le fichier.
// Pour rappel, les destructeurs sont automatiquement appelée dès qu'un objet
// sort du cadre de sa portée.
void faireQuelqueChoseAvecUnFichier(const std::string& nomDuFichier)
{
    // ifstream is short for input file stream
    std::ifstream fh(nomDuFichier); // Ouvre le fichier

    // Faire des choses avec le fichier
    faireQuelqueChoseAvecLeFichier(fh);
    faireAutreChoseAvec(fh);

} // Le fichier est automatiquement fermé ici par le destructeur

// Ceci a des avantages _énormes_ :
// 1. Peu importe la situation, la ressource (dans ce cas précis la gestion
//    de fichier) sera libérée. Si le destructeur est écrit correctement,
//    il est _impossible_ d'oublier de fermer la gestion et d'entraîner une
//    une fuite de ressources (si l'objet est sur la pile).
// 2. Remarquez que le code est beaucoup plus clair.
//    Le destructeur gère la fermeture du fichier discrètement sans avoir
//    besoin de s'en préoccuper.
// 3. Le code est fiable par rapport aux exceptions.
//    Une exception peut être lancée n'importe où dans la fonction, le
//    nettoyage se fera toujours.

// Tout code C++ idiomatique utilise considérablement RAII pour toutes les
// ressources.
// Des exemples additionnels inclus :
// - La mémoire utilisant unique_ptr et shared_ptr
// - Des conteneurs (containers) - la liste chaînée de la librairie standard,
//   des vecteurs (c.-à-d. tableaux auto-redimensionnés), tables de hachage, et
//   ainsi de suite. Tous détruisent leur contenu quand ils sortent du cadre
//   de leur portée.
// - Les mutex utilisant lock_guard et unique_lock


//////////////////
// Divers
//////////////////

// Ici sont regroupés des aspects du C++ qui peuvent être surprenants aux
// novices (et même à quelques habitués).
// Cette section est, malheureusement, grandement incomplète; C++ est un des
// langages où il est très facile de se tirer soi-même dans le pied.

// Vous pouvez surcharger des méthodes privées !
class Foo {
  virtual void bar();
};
class FooSub : public Foo {
  virtual void bar();  // Surcharge Foo::bar!
};

// 0 == false == NULL (la plupart du temps) !
bool* pt = new bool;
*pt = 0; // Affecte false à la valeur de la variable pointée par 'pt'.
pt = 0;  // Affecte le pointeur null à 'pt'.
// Les deux lignes compilent sans avertissement.

// nullptr est supposé régler un peu ce problème :
int* pt2 = new int;
*pt2 = nullptr; // Ne compile pas
pt2 = nullptr;  // Affecte null à pt2

// Il y a une exception faite pour les booléens.
// Ceci vous permet de tester les pointeurs null avec if(!ptr),
// mais par conséquent, vous pouvez assigner nullptr à un booléen directement !
*pt = nullptr;  // Ceci compile toujours, même si '*pt' est un booléen !

// '=' != '=' != '='!
// Appelle Foo::Foo(const Foo&) ou une variante du (voir sémantiques de mouvement)
// constructeur par copie.
Foo f2;
Foo f1 = f2;

// Appelle Foo::Foo(const Foo&) ou une variante, mais copie seulement la partie
// 'Foo' de 'fooSub'. Tout membre extra de 'fooSub' est ignoré.
// Ce comportement parfois horrifiant est appelé "object slicing".
FooSub fooSub;
Foo f1 = fooSub;

// Appelle Foo::operator=(Foo&) ou une variante.
Foo f1;
f1 = f2;

// Comment vraiment nettoyer un conteneur :
class Foo { ... };
vector<Foo> v;
for (int i = 0; i < 10; ++i)
  v.push_back(Foo());

// La ligne suivante affecte la taille de v à 0, mais les destructeurs ne sont
// appelés et les ressources ne sont pas libérées !
v.empty();
// La nouvelle valeur est copiée dans le premier Foo que nous avons inséré
v.push_back(Foo());

// Ceci nettoie toutes les valeurs de v. Voir la section à propos des objets
// temporaires pour comprendre pourquoi cela fonctionne.
v.swap(vector<Foo>());

```
Lecture complémentaire :

* Une référence à jour du langage est disponible à [CPP Reference](http://cppreference.com/w/cpp).
* Des ressources supplémentaires sont disponibles à [CPlusPlus](http://cplusplus.com).
* Un tutoriel couvrant les bases du langage et la configuration d'un environnement de codage est disponible à l'adresse [TheChernoProject - C ++](https://www.youtube.com/playlist?list=PLlrATfBNZ98dudnM48yfGUldqGD0S4FFb).
