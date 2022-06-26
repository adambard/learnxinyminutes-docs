---
language: Objective-C
contributors:
    - ["Eugene Yagrushkin", "www.about.me/yagrushkin"]
    - ["Yannick Loriot", "https://github.com/YannickL"]
    - ["Levi Bostian", "https://github.com/levibostian"]
translators:
    - ["Yannick Loriot", "https://github.com/YannickL"]
filename: LearnObjectiveC-fr.m
lang: fr-fr
---

L'Objective-C est un langage de programmation orienté objet réflexif principalement utilisé par Apple pour les systèmes d'exploitations macOS et iOS et leurs frameworks respectifs, Cocoa et Cocoa Touch.

```objective-c
// Les commentaires sur une seule ligne commencent par //

/*
Les
commentaires
multi-lignes
ressemblent
à
ceci
*/

// #import permet d'importer les en-têtes d'autres fichiers
// Utilisez <> pour importer des fichiers globaux (en général des frameworks)
// Utilisez "" pour importer des fichiers locaux (du projet)
#import <Foundation/Foundation.h>
#import "MaClasse.h"

// Si vous activez les modules dans les projets iOS >= 7 ou OS X >= 10.9
// dans Xcode 5, vous pouvez importer les frameworks comme cela :
@import Foundation;

// Le point d'entrée de votre programme est une fonction qui s'appelle main
// et qui retourne un entier comme type
int main (int argc, const char * argv[])
{
    // Créer un groupe de libération automatique de la mémoire pour l'ensemble
    // du programme
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    // Si vous utilisez le comptage de référence automatique (ARC), utilisez
    // @autoreleasepool à la place :
    @autoreleasepool {

    // NSLog() permet d'afficher une chaine de caractères dans la console
    // Affiche la chaine de caractères "Bonjour Tous Le Monde !"
    NSLog(@"Bonjour tous le Monde !"); 
 
    ///////////////////////////////////////
    // Les Types & Les Variables
    ///////////////////////////////////////
    
    // La déclaration de primitive
    int maPrimitive1  = 1;
    long maPrimitive2 = 234554664565;
    
    // La déclaration d'objet
    // Il faut mettre un astérisque devant la déclaration d'objet fortement typé
    MaClasse *monObject1 = nil;  // Typage fort
    id       monObject2  = nil;  // Typage faible
    // 'description' est une méthode qui permet de retourner un aperçut de l'objet sous forme textuelle
    // La méthode 'description' est appelée par défaut quand on utilise le paramètre '%@'
    NSLog(@"%@ and %@", monObject1, [monObject2 description]); // Affiche "(null) et (null)"
    
    // Les chaines de caractères
    NSString *chaineMonde = @"Monde";
    NSLog(@"Bonjour tous le %@ !", chaineMonde); // affiche => "Bonjour Tous Le Monde !" 
    // NSMutableString est une chaine mutable
    NSMutableString *chaineMutable = [NSMutableString stringWithString:@"Bonjour tous le"];
    [chaineMutable appendString:@" Monde !"];
    NSLog(@"%@", chaineMutable); // affiche => "Bonjour Tous Le Monde !"
    
    // Les caractères
    NSNumber *laLettreZSousFormeDeNombre = @'Z';
    char laLettreZ                       = [laLettreZSousFormeDeNombre charValue]; // ou 'Z'
    NSLog(@"%c", laLettreZ);

    // Les nombres
    NSNumber *nombreQuaranteDeux = @42;
    int quaranteDeux             = [nombreQuaranteDeux intValue]; // ou 42
    NSLog(@"%i", quaranteDeux);
    
    NSNumber *nombreQuaranteDeuxnonSigne = @42U;
    unsigned int quaranteDeuxnonSigne    = [nombreQuaranteDeuxnonSigne unsignedIntValue];
    NSLog(@"%u", fortyTwoUnsigned);
    
    NSNumber *nombreQuaranteDeuxCourt = [NSNumber numberWithShort:42];
    short quaranteDeuxCourt           = [nombreQuaranteDeuxCourt shortValue]; // ou 42
    NSLog(@"%hi", fortyTwoShort);
    
    NSNumber *nombreQuaranteDeuxLong = @42L;
    long quaranteDeuxLong            = [nombreQuaranteDeuxLong longValue]; // ou 42
    NSLog(@"%li", fortyTwoLong);

    // Les nombres flottants
    NSNumber *nombrePiFlottant = @3.141592654F;
    float piFlottant           = [nombrePiFlottant floatValue]; // ou 3.141592654f
    NSLog(@"%f", piFlottant); // affiche => 3.141592654
    NSLog(@"%5.2f", piFlottant); // affiche => " 3.14"
    
    NSNumber *nombrePiDouble = @3.1415926535;
    double piDouble          = [nombrePiDouble doubleValue]; // ou 3.1415926535
    NSLog(@"%f", piDouble);
    NSLog(@"%4.2f", piDouble); // affiche => "3.14"

    // NSDecimalNumber est une classe pour avoir plus de précision sur les nombres
    // flottants et les doubles
    NSDecimalNumber *decNumUn   = [NSDecimalNumber decimalNumberWithString:@"10.99"];
    NSDecimalNumber *decNumDeux = [NSDecimalNumber decimalNumberWithString:@"5.002"];
    // NSDecimalNumber ne permet pas d'utiliser les opérations standards (+, -, *, /)
    // Il faut utiliser les méthodes suivantes à la place :
    [decNumUn decimalNumberByAdding:decNumDeux]; 
    [decNumUn decimalNumberBySubtracting:decNumDeux];
    [decNumUn decimalNumberByMultiplyingBy:decNumDeux];
    [decNumUn decimalNumberByDividingBy:decNumDeux];
    NSLog(@"%@", decNumUn); // affiche => 10.99 comme NSDecimalNumber est immuable

    // Les booléens
    NSNumber *ouiNumber = @YES;
    NSNumber *nonNumber = @NO;
    // ou
    BOOL ouiBool = YES;
    BOOL nonBool  = NO;
    NSLog(@"%i", ouiBool); // affiche => 1

    // Les listes
    // Une liste peut contenir uniquement des objets
    NSArray *uneListe         = @[@1, @2, @3, @4];
    NSNumber *troisiemeNombre = uneListe[2];
    NSLog(@"Troisième nombre = %@", troisiemeNombre); // affiche "Troisième nombre = 3"
    // NSMutableArray est une version mutable de NSArray
    // Cela permet de modifier la liste en ajoutant de nouveaux éléments et en supprimant ou
    // changeant de place des objets déjà présent
    // C'est très pratique, mais pas aussi performant que l'utilisation de la classe NSArray
    NSMutableArray *listeMutable = [NSMutableArray arrayWithCapacity:2];
    [listeMutable addObject:@"Bonjour tous le"];
    [listeMutable addObject:@"Monde"];
    [listeMutable removeObjectAtIndex:0];
    NSLog(@"%@", [listeMutable objectAtIndex:0]); // affiche => "Monde"

    // Les dictionnaires
    // Un dictionnaire est un ensemble de clés : valeurs
    NSDictionary *unDictionnaire = @{ @"cle1" : @"valeur1", @"cle2" : @"valeur2" };
    NSObject *valeur             = unDictionnaire[@"Une clé"];
    NSLog(@"Objet = %@", valeur); // affiche "Objet = (null)"
    // NSMutableDictionary est un dictionnaire mutable, c-à-d que l'on peut modifier
    NSMutableDictionary *dictionnaireMutable = [NSMutableDictionary dictionaryWithCapacity:2];
    [dictionnaireMutable setObject:@"valeur1" forKey:@"cle1"];
    [dictionnaireMutable setObject:@"valeur2" forKey:@"cle2"];
    [dictionnaireMutable removeObjectForKey:@"cle1"];

    // Les ensembles
    // Un ensemble ne peut contenir de duplicatas contrairement aux NSArray
    NSSet *ensemble = [NSSet setWithObjects:@"Salut", @"Salut", @"Monde", nil];
    NSLog(@"%@", ensemble); // affiche => {(Salut, Monde)} (Pas forcément dans le même ordre)
    // NSMutableSet est un ensemble mutable 
    NSMutableSet *ensembleMutable = [NSMutableSet setWithCapacity:2];
    [ensembleMutable addObject:@"Salut"];
    [ensembleMutable addObject:@"Salut"];
    NSLog(@"%@", ensembleMutable); // affiche => {(Salut)}

    ///////////////////////////////////////
    // Les Operateurs
    ///////////////////////////////////////
    
    // Les opérateurs sont pareil qu'en C
    // Par exemple :
    2 + 5; // => 7
    4.2f + 5.1f; // => 9.3f
    3 == 2; // => 0 (NO)
    3 != 2; // => 1 (YES)
    1 && 1; // => 1 (et logique)
    0 || 1; // => 1 (ou logique)
    ~0x0F; // => 0xF0 (négation bit à bit)
    0x0F & 0xF0; // => 0x00 (et bit à bit)
    0x01 << 1; // => 0x02 (décalage à gauche (par 1))

    ///////////////////////////////////////
    // Les Structures de Contrôle
    ///////////////////////////////////////

    // Structure "Si-Sinon" (If-Else) 
    if (NO)
    {
        NSLog(@"Je ne suis jamais affiché");
    } else if (0)
    {
        NSLog(@"Je ne suis jamais affiché aussi");
    } else
    {
        NSLog(@"Je suis affiché");
    }

    // Structure "Selon" (Switch)
    switch (2)
    {
        case 0:
        {
            NSLog(@"Je ne suis jamais affiché");
        } break;
        case 1:
        {
            NSLog(@"Je ne suis jamais affiché aussi");
        } break;
        default:
        {
            NSLog(@"Je suis affiché");
        } break;
    }
    
    // Structure de boucle "Tant Que" (While)
    int ii = 0;
    while (ii < 4)
    {
        NSLog(@"%d,", ii++); // ii++ incrémente ii après avoir utilisé sa valeur
    } // => affiche "0," 
      //            "1,"
      //            "2,"
      //            "3,"

    // Structure de boucle "Pour" (For)
    int jj;
    for (jj=0; jj < 4; jj++)
    {
        NSLog(@"%d,", jj);
    } // => affiche "0," 
      //            "1,"
      //            "2,"
      //            "3,"
     
    // Structure de boucle "Pour Chaque" (Foreach)
    NSArray *valeurs = @[@0, @1, @2, @3];
    for (NSNumber *valeur in valeurs)
    {
        NSLog(@"%@,", valeur);
    } // => affiche "0," 
      //            "1,"
      //            "2,"
      //            "3,"

    // Structure "Essayer-Attraper-Finalement" (Try-Catch-Finally)
    @try
    {
        @throw [NSException exceptionWithName:@"FileNotFoundException"
                            reason:@"Fichier non trouvé" userInfo:nil];
    } @catch (NSException * e)
    {
        NSLog(@"Une exception est survenue : %@", e);
    } @finally
    {
        NSLog(@"Finalement");
    } // => affiche "Une exception est survenue : Fichier non trouvé"
      //            "Finalement"
 
    ///////////////////////////////////////
    // Les Objets
    ///////////////////////////////////////
    
    // Définis et créé une instance d'objet en allouant un espace mémoire puis en
    // l'initialisant. Un objet n'est pas complétement fonctionnel tant que les deux
    // étapes précédentes ne sont pas terminées
    MaClass *monObjet = [[MaClass alloc] init];
        
    // L'Objet en Objective-C est basé sur le principe d'envoie de message et non sur
    // celui d'appel de méthode comme la plupart des autres langages
    // C'est un détail important car cela veut dire que l'on peut envoyer un message
    // à un objet qui ne possède pas la méthode demandée sans aucune incidence sur le
    // fonctionnement du programme (aucune exception ne sera levée)
    [myObject instanceMethodWithParameter:@"Steve Jobs"];

    // Nettoie la mémoire qui a été utilisée dans le programme
    [pool drain];

    // Fin de l'@autoreleasepool
    }
    
    // Fin du programme
    return 0;
}

///////////////////////////////////////
// Les Classes et Les Fonctions
///////////////////////////////////////

// Déclaration d'une classe dans un en-tête de fichier (MaClasse.h) :
// La déclaration d'une classe en Objective-C commence par la déclaration de son interface :
// @interface NomDeLaClasse : NomDeLaClasseParent <ListeDesProtocoles>
// {
//    type nom; // Déclaration d'une variable;
// }
// @property type nom; // Déclaration d'une propriété
// -/+ (type)nomDeLaMethode; // Déclaration d'une methode
// @end // Termine la déclaration
// NSObject est la classe de base (super classe) en Objective-C
@interface MaClasse : NSObject <MonProtocole>
{
    int nombre; // Accès protégé par défaut (équivalent à '@protected int nombre;')
    @private id donnee; // Accès privé (il est plus pratique de le faire dans l'implémentation)
    NSString *nom; 
}
// Les propriétés permettent de générer les accésseurs/affecteurs publiques à la compilation
// Par défaut, le nom de l'affecteur est la chaine 'set' suivi par le nom de la @property
@property int propInt; // Nom de l'affecteur = 'setPropInt'
@property (copy) id copyId; // (copy) => Copie l'objet pendant l'affectation
// (readonly) => Ne peut pas affecté la variable en dehors de l'@interface
// Il faut utiliser le mot clé '@synthesize' dans l'@implementation pour créer l'accésseur
@property (readonly) NSString *roString;
// Vous pouvez aussi personnaliser les noms des accésseurs ou des affecteurs
@property (getter=longeurGet, setter=longeurSet:) int longeur;
 
// Methodes
+/- (TypeDeRetour)signatureDeLaMethode:(TypeDuParametre *)nomDuParametre;

// '+' signifie que c'est une méthode de classe (statique) :
+ (NSString *)methodeDeClasse;
+ (MaClasse *)maClasseDepuisLaHauteur:(NSNumber *)hauteurParDefaut;

// '-' pour les méthodes d'instances :
- (NSString *)methodeInstanceAvecUnParametre:(NSString *)string;
- (NSNumber *)methodeInstanceAvecUnParametre:(NSString*)string puisUnDeuxieme:(NSNumber *)number;

// Contructeur avec des arguments :
- (id)initAvecDistance:(int)distanceParDefault;
// Les méthodes en Objective-C sont très descriptives

@end // Fin de l'interface


// Voici un exemple d'utilisation de MaClasse
MaClasse *maClasse = [[MaClasse alloc] init]; // créer une instance de MaClasse
[maClasse setNombre:10]; 
NSLog(@"%d", [maClasse nombre]); // affiche => 10
[myClass longeurSet:32];
NSLog(@"%i", [maClasse longeurGet]); // affiche => 32
// Pour des raisons pratiques vous pouvez aussi utiliser la notation en point pour accéder aux 
// variables d'instances :
maClasse.nombre = 45;
NSLog(@"%i", maClasse.nombre); // maClasse => 45

// Pour appeler une méthode de classe :
NSString *s1 = [MaClasse methodeDeClasse];
MaClasse *m2 = [MaClasse maClasseDepuisLaHauteur:38];

// Pour appeler une méthode d'instance :
MaClasse *maClasse = [[MaClasse alloc] init]; // Créer une instance de MaClasse
NSString *stringDepuisUneInstanceDeMethode = [maClasse methodeInstanceAvecUnParametre:@"Salut"];

// Les sélecteurs sont un moyen de décrire les méthodes dynamiquement
// Ils sont utilisés pour appeler des méthodes de classe et avoir des pointeurs de fonctions
// facilement manipulable
// SEL est un type de donnée et @selector retourne un selecteur à partir d'un nom de methode
SEL selecteur = @selector(methodeInstanceAvecUnParametre:puisUnDeuxieme:); 
if ([maClasse respondsToSelector:selecteur]) { // Vérifie si la classe possède la méthode
    // Met les arguments de la méthode dans un seul objet pour l'envoyer via la fonction
    // performSelector:withObject: 
    NSArray *arguments = [NSArray arrayWithObjects:@"Hello", @4, nil];
    [myClass performSelector:selectorVar withObject:arguments]; // Appele la méthode via le sélecteur
}
else {
    // NSStringFromSelector() retourne une chaine de caractères à partir d'un sélecteur
    NSLog(@"MaClasse ne possède pas de méthode : %@", NSStringFromSelector(selecteur));
}

// Le fichier d'implémentation de la classe MaClasse (MaClasse.m) doit commencer comme ceci :
@implementation MaClasse {
    long distance; // Variable d'instance privée (équivalent à @private dans l'interface)
    NSNumber hauteur;
}

// Pour accéder à une variable depuis l'implémentation on peut utiliser le _ (tiret bas) devant le nom
// de la variable :
_nombre = 5;
// Accès d'une variable définie dans le fichier d'implémentation :
distance = 18;
// Pour utiliser la variable définie par l'intermédiaire de @property, il faut utiliser @synthesize 
// qui permet de créer les affecteurs et les accésseurs correspondants :
@synthesize roString = _roString; // _roString est maintenant disponible dans l'implementation

// A l'inverse dela méthode 'init' qui est appelée lors de la création d'un objet, la fonction 
// 'dealloc' est appelée quand l'objet est supprimé
- (void)dealloc
{
    [hauteur release]; // Si vous n'utilisez pas ARC, pensez bien à supprimer l'objet 
    [super dealloc];   // et à appeler la méthode 'dealloc' de la classe parent 
}

// Les constructeurs sont des fonctions qui permettent d'instancier un objet
// 'init' est le constructeur par défaut en Objective-C
- (id)init
{
    if ((self = [super init])) // 'super' est utilisé pour appeler la méthode de la classe parent
    {
        self.count = 1; // 'self' permet d'appeler la méthode de l'instance courante
    }
    return self;
}

// Vous pouvez aussi créer des constructeurs avec des arguments
// Attention : chaque nom de constructeur doit absolument commencer par 'init'
- (id)initAvecUneDistance:(int)distanceParDefault 
{
    if ((self = [super init]))
    {
        distance = distanceParDefault;
        return self;
    }
}

// Implémentation d'une méthode de classe
+ (NSString *)methodDeClasse
{
    return [[self alloc] init];
}

+ (MaClasse *)maClasseDepuisUneHauteur:(NSNumber *)hauteurParDefaut
{
    hauteur = hauteurParDefaut;
    return [[self alloc] init];
}

// Implémentation d'une méthode d'instance
- (NSString *)methodeInstanceAvecUnParametre:(NSString *)string
{
    return @"Ma chaine de caractère";
}

- (NSNumber *)methodeInstanceAvecUnParametre:(NSString*)string puisUnDeuxieme:(NSNumber *)number
{
    return @42;
}

// Pour créer une méthode privée, il faut la définir dans l'implementation et non pas dans 
// l'interface
- (NSNumber *)methodePrivee
{
    return @72;
}

[self methodePrivee]; // Appel de la méthode privée

// Implémentation d'une méthode qui est déclarée dans <MonProtocole>
- (void)methodeDuProtocole
{
    // expressions
}

@end // Fin de l'implémentation

/*
 * Un protocole déclare des méthodes et propriétés que chaque implémentation doit avoir afin de se
 * conformer à celui-ci
 * Un protocole n'est pas une classe, c'est juste une interface
 */
@protocol MonProtocole
    - (void)methodeDuProtocole;
@end


///////////////////////////////////////
// Gestion de la mémoire
///////////////////////////////////////
/* 
À chaque fois qu'un objet est créé dans l'application, un bloc mémoire doit être alloué.
Quand l'application en a fini avec cet objet, la mémoire doit être libérée afin d'éviter les fuites
mémoires
Il n'y a pas de ramasse-miettes en Objective-C contrairement à Java par exemple. La gestion de la
mémoire repose sur le comptage de référence qui, pour chaque objet, assigne un compteur qui permet
de connaitre le nombre de référence qui l'utilise.

Le principe est le suivant :
Lorsque l'objet est créé, le compteur est initialisé à 1. Quand une instance détient un objet, le
compteur est incrémenté de un. Quand l'objet est libéré, le compteur est décrémenté de un.  Au moment
où le compteur arrive à zéro, l'objet est supprimé de la mémoire

Une bonne pratique à suivre quand on travaille avec des objets est la suivante :
(1) créer un objet, (2) utiliser l'objet, (3) supprimer l'objet de la mémoire
*/

MaClasse *classeVar = [MyClass alloc]; // 'alloc' incrémente le compteur de référence
[classeVar release]; // Décrémente le compteur de rérence
// 'retain' incrémente le compteur de référence
// Si 'classeVar' est libéré, l'objet reste en mémoire car le compteur de référence est non nul
MaClasse *nouvelleVar = [classVar retain];
[classeVar autorelease]; // Supprime l'appartenance de l'objet à la fin du bloc

// Les @property peuvent utiliser 'retain' et 'assign' 
@property (retain) MaClasse *instance; // Libère l'ancienne valeur et retient la nouvelle
@property (assign) NSSet *set; // Pointeur vers la valeur sans retenir/libérer l'ancienne valeur

// Automatic Reference Counting (ARC)
// La gestion de la mémoire étant pénible, depuis iOS 4 et Xcode 4.2, Apple a introduit le comptage de
// référence automatique (Automatic Reference Counting en anglais)
// ARC est une fonctionnalité du compilateur qui permet d'ajouter les 'retain', 'release' et 'autorelease'
// automatiquement. Cela veut dire que lorsque vous utilisez ARC vous ne devez plus utiliser ces mot-clés
MaClasse *arcMaClasse = [[MaClasse alloc] init]; 
// ... code utilisant arcMaClasse
// Sans ARC, vous auriez dû appeler [arcMaClasse release] après avoir utilisé l'objet. Mais avec ARC
// activé il n'est plus nécessaire de le faire car le compilateur ajoutera l'expréssion automatiquement
// pour vous

// Les mots clés 'assign' et 'retain', avec ARC sont respectivement remplacé par 'weak' et 'strong'
@property (weak) MaClasse *weakVar; // 'weak' ne retient pas l'objet. Si le compteur de référence
// descend à zero, weakVar sera automatiquement mis à nil
@property (strong) MaClasse *strongVar; // 'strong' prend possession de l'objet comme le ferait avec
// le mot-clé 'retain'

// Pour l'instanciation des variables (en dehors de @property), vous pouvez utiliser les instructions
// suivantes :
__strong NSString *strongString; // Par défaut. La variable est retenue en mémoire jusqu'à la fin
//  de sa portée
__weak NSSet *weakSet; // Maintient une référence vers l'objet sans incrémenter son compteur de référence :
// Lorsque l'objet sera supprimé, weakSet sera mis à nil automatiquement
__unsafe_unretained NSArray *unsafeArray; // Comme __weak, mais la variable n'est pas mis à nil quand
// l'objet est supprimé

```

##  Lectures Complémentaires

[La Page Wikipedia de l'Objective-C](http://fr.wikipedia.org/wiki/Objective-C)

[iOS pour les écoliers : Votre première app iOS](http://www.raywenderlich.com/fr/39272/ios-pour-les-ecoliers-votre-premiere-app-ios-partie-12)

[Programming with Objective-C. Apple PDF book](https://developer.apple.com/library/ios/documentation/cocoa/conceptual/ProgrammingWithObjectiveC/ProgrammingWithObjectiveC.pdf)
