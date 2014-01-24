---

language: Objective-C
contributors:
    - ["Eugene Yagrushkin", "www.about.me/yagrushkin"]
    - ["Yannick Loriot", "https://github.com/YannickL"]
    - ["Levi Bostian", "https://github.com/levibostian"]
translators:
    - ["Yannick Loriot", "https://github.com/YannickL"]
filename: LearnObjectiveC.m

---

L'Objective-C est un langage de programmation orienté objet réflexif principalement utilisé par Apple pour les systèmes d'exploitations Mac OS X et iOS et leurs framworks respectifs, Cocoa et Cocoa Touch.

```objective-c
// Les commentaires unilignes commencent par //

/*
Les commentaires multilignes ressemblent à ça
*/

// Importe les en-têtes en utilisant #import
// Utilisez <> pour importer des fichiers globaux (en général des frameworks)
// Utilisez "" pour importer des fichiers locaux (du projet)
#import <Foundation/Foundation.h>
#import "MaClasse.h"

// Si vous activez les modules pour les projects iOS >= 7 ou Mac OS X >= 10.9
// dans Xcode 5 vous pouvez importer les frameworks comme cela :
@import Foundation;

// Le point d'entrée de votre programme est une fonction qui s'appelle main
// et qui return un entier comme type
int main (int argc, const char * argv[])
{
    // Créer un groupe de libération automatique de la mémoire pour l'ensemble
    // du programme
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    // Si vous utilisez le comptage de référence automatique (ARC), utilisez
    // @autoreleasepool à la place :
    @autoreleasepool {

    // Utilisez NSLog pour afficher les lignes sur la console
    // Affiche la chaine de caractères "Bonjour Tous Le Monde !"
    NSLog(@"Bonjour tous le Monde !"); 
 
    ///////////////////////////////////////
    // Les Types & Les Variables
    ///////////////////////////////////////
    
    // Déclarations de primitive
    int maPrimitive1  = 1;
    long maPrimitive2 = 234554664565;
    
    // Declarations d'objets
    // Il faut mettre l'* devant la déclaration d'objets fortement typés
    MaClasse *monObject1 = nil;  // Typage fort
    id       monObject2  = nil;  // Typage faible
    // %@ est un objet
    // 'description' est une convention pour afficher la valeur des objets
    NSLog(@"%@ and %@", monObject1, [monObject2 description]); // Affiche "(null) et (null)"
    
    // Chaines de caractères
    NSString *chaineMonde = @"Monde";
    NSLog(@"Bonjour tous le %@ !", chaineMonde); // affiche => "Bonjour Tous Le Monde !" 
    // NSMutableString est une chaine mutable
    NSMutableString *chaineMutable = [NSMutableString stringWithString:@"Bonjour tous le"];
    [chaineMutable appendString:@" Monde !"];
    NSLog(@"%@", chaineMutable); // affiche => "Bonjour Tous Le Monde !"
    
    // Les littéraux pour les caratères
    NSNumber *laLettreZSousFormeDeNombre = @'Z';
    char laLettreZ                       = [laLettreZSousFormeDeNombre charValue]; // ou 'Z'
    NSLog(@"%c", laLettreZ);

    // Les littéraux pour les nombres
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

    // Les littéraux pour les flottans
    NSNumber *nombrePiFlottan = @3.141592654F;
    float piFlottan           = [nombrePiFlottan floatValue]; // ou 3.141592654f
    NSLog(@"%f", piFlottan); // affiche => 3.141592654
    NSLog(@"%5.2f", piFlottan); // affiche => " 3.14"
    
    NSNumber *nombrePiDouble = @3.1415926535;
    double piDouble          = [nombrePiDouble doubleValue]; // ou 3.1415926535
    NSLog(@"%f", piDouble);
    NSLog(@"%4.2f", piDouble); // affiche => "3.14"

    // NSDecimalNumber est une classe pour avoir plus de précision sur les flottans
    // et les doubles
    NSDecimalNumber *decNumUn   = [NSDecimalNumber decimalNumberWithString:@"10.99"];
    NSDecimalNumber *decNumDeux = [NSDecimalNumber decimalNumberWithString:@"5.002"];
    // NSDecimalNumber n'est pas capable d'utiliser les opérations standards : 
    // +, -, *, /, il utilise donc ses propres fonctions :
    [decNumUn decimalNumberByAdding:decNumDeux]; 
    [decNumUn decimalNumberBySubtracting:decNumDeux];
    [decNumUn decimalNumberByMultiplyingBy:decNumDeux];
    [decNumUn decimalNumberByDividingBy:decNumDeux];
    NSLog(@"%@", decNumUn); // affiche => 10.99 comme NSDecimalNumber is immuable

    // Les littéraux pour les booléens
    NSNumber *ouiNumber = @YES;
    NSNumber *nonNumber = @NO;
    // ou
    BOOL ouiBool = YES;
    BOOL nonBool  = NO;
    NSLog(@"%i", ouiBool); // affiche => 1

    // Les listes
    // Ils peuvent contenir différents types de données, mais ils doivent absolument 
    // être des objets
    NSArray *uneListe         = @[@1, @2, @3, @4];
    NSNumber *troisiemeNombre = uneListe[2];
    NSLog(@"Troisième nombre = %@", troisiemeNombre); // affiche "Troisième nombre = 3"
    // NSMutableArray est une version mutable de NSArray qui permet de changer les
    // objets dans la liste et l'étendre ou  la réduire
    // C'est très pratique, mais pas aussi performant que l'utilsation de la classe
    // NSArray
    NSMutableArray *listeMutable = [NSMutableArray arrayWithCapacity:2];
    [listeMutable addObject:@"Bonjour tous le"];
    [listeMutable addObject:@"Monde"];
    [listeMutable removeObjectAtIndex:0];
    NSLog(@"%@", [listeMutable objectAtIndex:0]); // affiche => "Monde"

    // Les dictionnaires
    NSDictionary *unDictionnaire = @{ @"cle1" : @"valeur1", @"cle2" : @"valeur2" };
    NSObject *valeur             = unDictionnaire[@"Une clé"];
    NSLog(@"Objet = %@", valeur); // affiche "Objet = (null)"
    // NSMutableDictionary est un dictionnaire mutable
    NSMutableDictionary *dictionnaireMutable = [NSMutableDictionary dictionaryWithCapacity:2];
    [dictionnaireMutable setObject:@"valeur1" forKey:@"cle1"];
    [dictionnaireMutable setObject:@"valeur2" forKey:@"cle2"];
    [dictionnaireMutable removeObjectForKey:@"cle1"];

    // Les ensembles
    NSSet *ensemble = [NSSet setWithObjects:@"Salut", @"Salut", @"Monde", nil];
    NSLog(@"%@", ensemble); // affiche => {(Salut, Monde)} (peut être dans un ordre différente)
    // NSMutableSet est un ensemble mutable 
    NSMutableSet *ensembleMutable = [NSMutableSet setWithCapacity:2];
    [ensembleMutable addObject:@"Salut"];
    [ensembleMutable addObject:@"Salut"];
    NSLog(@"%@", ensembleMutable); // affiche => {(Salut)}

    ///////////////////////////////////////
    // Operateurs
    ///////////////////////////////////////
    
    // Les opérateurs sont les mêmes que ceux du langage C
    // Par exemple :
    2 + 5; // => 7
    4.2f + 5.1f; // => 9.3f
    3 == 2; // => 0 (NO)
    3 != 2; // => 1 (YES)
    1 && 1; // => 1 (et logique)
    0 || 1; // => 1 (ou logique)
    ~0x0F; // => 0xF0 (négation bit à bit)
    0x0F & 0xF0; // => 0x00 (et bit à bit)
    0x01 << 1; // => 0x02 (décale à gauche (par 1))

    ///////////////////////////////////////
    // Structures de controle
    ///////////////////////////////////////

    // Expression If-Else 
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

    // Expression Switch
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
    
    // Expression de boucle While
    int ii = 0;
    while (ii < 4)
    {
        NSLog(@"%d,", ii++); // ii++ incrémente ii après avoir utilisé sa valeure
    } // => affiche "0," 
      //            "1,"
      //            "2,"
      //            "3,"

    // Expression de boucle For loops
    int jj;
    for (jj=0; jj < 4; jj++)
    {
        NSLog(@"%d,", jj);
    } // => affiche "0," 
      //            "1,"
      //            "2,"
      //            "3,"
     
    // Expression de boucle Foreach
    NSArray *valeurs = @[@0, @1, @2, @3];
    for (NSNumber *valeur in valeurs)
    {
        NSLog(@"%@,", valeur);
    } // => affiche "0," 
      //            "1,"
      //            "2,"
      //            "3,"

    // Expressions Try-Catch-Finally
    @try
    {
        @throw [NSException exceptionWithName:@"FileNotFoundException"
                            reason:@"Fichier non trouvé" userInfo:nil];
    } @catch (NSException * e)
    {
        NSLog(@"Exception : %@", e);
    } @finally
    {
        NSLog(@"Finalement");
    } // => affiche "Exceptio : Fichier non trouvé"
      //            "Finalement"
 
    ///////////////////////////////////////
    // Objets
    ///////////////////////////////////////
    
    // Créez une instance d'objet en allouant un espace mémoire puis en l'initialisant
    // Un objet n'est pas complétement fonctionnel jusqu'à ce que les deux étapes précédente
    // ne sont pas fini
    MaClass *monObjet = [[MaClass alloc] init];
        
    // Le modèle Objective-C est basé sur l'envoie de message et non sur les appels de
    // méthodes comme la plupart des autres langage de programmation
    [myObject instanceMethodWithParameter:@"Steve Jobs"];

    // Nettoie la mémoire que vous avez utilisé dans votre programme
    [pool drain];

    // Fin the l'@autoreleasepool
    }
    
    // Fin du programme
    return 0;
}

///////////////////////////////////////
// Classes et Fonctions
///////////////////////////////////////

// Déclarez votre classe dans une en-tête de fichier (MaClasse.h) :
// La syntaxe de déclaration :
// @interface NomDeLaClasse : NomDeLaClasseParent <ProtocolesImplemente>
// {
//    type nom; <= declarations de variable;
// }
// @property type nom; <= declarations de propriété.
// -/+ (type) Methode declarations; <= Declarations de methodes. 
// @end
// NSObject est la classe de base de l'Objective-C
@interface MaClasse : NSObject <MonProtocole>
{
    // Déclaration des variables d'instances (peut existé soit dans l'interface soir dans 
    // l'implémentation)
    int nombre; // Accès protégé par défaut
    @private id donnee; // Accès privé (il est plus pratique de le faire dans l'implémentation)
    NSString *nom; 
}
// Notation pratique pour l'accès aux variable public et pour générrer les 
// accésseurs/affecteurs
// Par défaut, le nom de l'affecteur vaut 'set' suivi par le nom de la @property
@property int propInt; // Nom du setter = 'setPropInt'
@property (copy) id copyId; // (copy) => Copie l'objet pendant l'affectation
// (readonly) => Ne peut pas affecté la variable en dehors de l'@interface
// Utilisez @synthesize dans l'@implementation pour créer l'accésseur
@property (readonly) NSString *roString;
// Vous pouvez personnaliser les noms des accésseurs et des affecteurs au lieu d'utiliser les
// noms par défauts
@property (getter=longeurGet, setter=longeurSet:) int longeur;
 
// Methodes
+/- (type de retour)signatureDeLaMethode:(Type Du Parametre *)nomDuParametre;

// + pour les méthodes de classe :
+ (NSString *)methodeDeClasse;
+ (MaClasse *)maClasseDepuisLaHauteur:(NSNumber *)hauteurParDefaut;

// - pour les méthodes d'instances :
- (NSString *)methodeInstanceAvecUnParametre:(NSString *)string;
- (NSNumber *)methodeInstanceAvecUnParametre:(NSString*)string puisUnDeuxieme:(NSNumber *)number;

// Contructeur avec des arguments :
- (id)initAvecDistance:(int)distanceParDefault;
// Les méthodes en Objective-C sont très descriptive

@end // Définit la fin de l'interface


// Exemple d'utilisation de MaClasse
MaClasse *maClasse = [[MaClasse alloc] init]; // créer une instance de MaClasse
[maClasse setNombre:10]; 
NSLog(@"%d", [maClasse nombre]); // affiche => 10
[myClass longeurSet:32];
NSLog(@"%i", [maClasse longeurGet]); // affiche => 32
// Pour des raisons pratiques vous pouvez aussi utiliser la notation en point pour accéder aux 
// variables d'instances :
maClasse.nombre = 45;
NSLog(@"%i", maClasse.nombre); // maClasse => 45

// Appeler un méthode de classe :
NSString *s1 = [MaClasse methodeDeClasse];
MaClasse *m2 = [MaClasse maClasseDepuisLaHauteur:38];

// Appeler un méthode d'instance :
MaClasse *maClasse = [[MaClasse alloc] init]; // Créer une instance de MaClasse
NSString *stringDepuisUneInstanceDeMethode = [maClasse methodeInstanceAvecUnParametre:@"Salut"];

// Sélecteurs sont un moyen de représenté les méthodes dynamiquement
// Ils sont utilisé pour appeller des méthodes de classe, passer des methodes au travers de fonctions
// pour notifier les autres classes qu'elle peuvent les appellé, et pour sauvegarder les méthodes en
// tant que variables
// SEL est un type de donnée. @selected retourne un selecteur à partir d'un nom de methode
SEL selecteur = @selector(methodeInstanceAvecUnParametre:puisUnDeuxieme:); 
if ([maClasse respondsToSelector:selecteur]) { // Vérifie si la classe contient la méthode
    // Doit mettre tous les arguments de la méthode dans un seul objet pour l'envoyer via la fonction
    // performSelector:withObject: 
    NSArray *arguments = [NSArray arrayWithObjects:@"Hello", @4, nil];
    [myClass performSelector:selectorVar withObject:arguments]; // Appele la méthode
} else {
    // NSStringFromSelector() retourne une chaine de charactères à partir d'un sélecteur
    NSLog(@"MaClasse ne possède pas de méthode : %@", NSStringFromSelector(selecteur));
}

// Implement la méthode dans le fichier d'impémentation (MaClasse.m)
@implementation MaClasse {
    long distance; // Variable d'instance privé
    NSNumber hauteur;
}

// Pour accéder à une variable depuis le fichier d'implémentation on peut utiliser le _ devant le nom
// de la variable :
_nombre = 5;
// Accès d'une variable définit dans le fichier d'implémentation :
distance = 18;
// Pour utiliser la varible @property dans l'implémentation, utiliser @synthesize qui créer les 
// accésseurs :
@synthesize roString = _roString; // _roString est disponible dans l'@implementation

// En contre-partie de l'initialisation, la fonction dealloc est appelé quand l'objet est n'est plus
// utilisé
- (void)dealloc
{
    [hauteur release]; // Si vous n'utilisez par l'ARC, pensez bien à supprimer l'objet 
    [super dealloc];  // et à appeler la méthode de la classe parent 
}

// Les constructeurs sont une manière de créer des instances de classes
// Ceci est le constructeur par défaut; il est appelé quand l'objet est créé
- (id)init
{
    if ((self = [super init])) // 'super' est utilisé pour appeler la méthode de la classe parent
    {
        self.count = 1; // 'self' est utilisé pour appeler la méthodes de l'objet courrant
    }
    return self;
}

// Vous pouvez créer des constructeurs qui possèdent des arguments :
- (id)initAvecUneDistance:(int)distanceParDefault 
{
    if ((self = [super init]))
    {
        distance = distanceParDefault;
        return self;
    }
}

+ (NSString *)methodDeClasse
{
    return [[self alloc] init];
}

+ (MaClasse *)maClasseDepuisUneHauteur:(NSNumber *)hauteurParDefaut
{
    hauteur = hauteurParDefaut;
    return [[self alloc] init];
}

- (NSString *)methodeInstanceAvecUnParametre:(NSString *)string
{
    return @"Ma chaine de charactère";
}

- (NSNumber *)methodeInstanceAvecUnParametre:(NSString*)string puisUnDeuxieme:(NSNumber *)number
{
    return @42;
}

// Pour créer une méthode privée, il faut la définir dans l'@implementation et non pas dans 
// l'@interface
- (NSNumber *)methodePrivee
{
    return @72;
}

[self methodePrivee]; // Appel de la méthode privée

// Méthode déclarée dans MonProtocole
- (void)methodeDuProtocole
{
    // expressions
}

@end // Fin de l'implémentation

/*
 * Un protocole déclare les méthodes qu'ils doivent implémenter afin de se conformer celui-ci
 * Un protocole n'est pas une classe, c'est juste une interface
 */
@protocol MonProtocole
    - (void)methodeDuProtocole;
@end


///////////////////////////////////////
// Gestion de la mémoire
///////////////////////////////////////
/* 
Pour chaque objet utilisé dans une application, la mémoire doit être alloué pour chacun d'entre eux.
Quand l'application en a fini avec cet objet, la mémoire doit être libéré pour assurer la performane.
Il n'y a pas de ramasse-miettes en Objective-C, il utilise à la place le comptage de référence à la
place. Tant que le compteur de référence est supérieur à 1 sur un objet, l'objet ne sera pas supprimé

Quand une instance détient un objet, le compteur de référence est incrémenté de un. Quand l'objet est
libéré, le compteur est décrémenté de un. Quand le compteur est égale à zéro, l'objet est supprimé de
la mémoire

Une bonne pratique à suivre quand on travaille avec des objets est la suivante :
(1) créer un objet, (2) utiliser l'objet, (3) supprimer l'objet de la mémoire
*/

MaClasse *classeVar = [MyClass alloc]; // 'alloc' incrémente le compteur de référence
[classeVar release]; // Décrémente le compteur de rérence
// 'retain' incrémente le compteur de référence
// Si 'classeVar' est libéré, l'objet reste en mémoire car le compteur de référence est non nulle
MaClasse *nouvelleVar = [classVar retain];
[classeVar autorelease]; // Supprime l'appartenance de l'objet à la fin du block

// @property peuvent utiliser 'retain' et 'assign' 
@property (retain) MaClasse *instance; // Libère l'ancienne valeur et retient la nouvelle
@property (assign) NSSet *set; // Pointeur vers la valeur sans retenir/libérer l'ancienne valeur

// Automatic Reference Counting (ARC)
// La gestion de la mémoire étant pénible, depuis iOS 4 et Xcode 4.2, Apple a introduit le comptage de référence
// automatique (Automatic Reference Counting en anglais).
// ARC est une fonctionnalité du compilateur qui lui permet d'ajouter les 'retain', 'release' et 'autorelease'
// automatiquement. Donc quand utilisez ARC vous de devez plus utiliser ces mots clés
MaClasse *arcMaClasse = [[MaClasse alloc] init]; 
// ... code utilisant arcMaClasse
// Sans ARC, vous auriez dû appeler [arcMaClasse release] après avoir utilisé l'objet. Mais avec ARC il n'y a plus
// besoin car le compilateur ajoutera l'expréssion automatiquement pour vous

// Les mots clés 'assign' et 'retain', avec ARC sont respectivement remplacé par 'weak' et 'strong'
@property (weak) MaClasse *weakVar; // 'weak' ne retient pas l'objet. Si l'instance original descend à zero, weakVar
// sera automatiquement mis à nil
@property (strong) MaClasse *strongVar; // 'strong' prend posséssion de l'objet comme le ferai le mot clé 'retain'

// Pour l'instanciation des variables (en dehors de @property), vous pouvez utiliser les instructions suivantes :
__strong NSString *strongString; // Par defaut. La variable est retenu en mémoire jusqu'à la fin de sa portée
__weak NSSet *weakSet; // Réfère la variable en utilisant le mot clé '__weak'
__unsafe_unretained NSArray *unsafeArray; // Comme __weak, mais quand la variable n'est pas mis à nil quand l'objet
// est supprimé ailleurs

```
##  Lectures Complémentaires

[La Page Wikipedia de l'Objective-C](http://fr.wikipedia.org/wiki/Objective-C)

[iOS pour les écoliers : Votre première app iOS](http://www.raywenderlich.com/fr/39272/ios-pour-les-ecoliers-votre-premiere-app-ios-partie-12)

[Programming with Objective-C. Apple PDF book](https://developer.apple.com/library/ios/documentation/cocoa/conceptual/ProgrammingWithObjectiveC/ProgrammingWithObjectiveC.pdf)