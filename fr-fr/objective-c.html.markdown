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
// Memory Management
///////////////////////////////////////
/* 
For each object used in an application, memory must be allocated for that object. When the application
is done using that object, memory must be deallocated to ensure application efficiency. 
Objective-C does not use garbage collection and instead uses reference counting. As long as 
there is at least one reference to an object (also called "owning" an object), then the object
will be available to use (known as "ownership"). 

When an instance owns an object, its reference counter is increments by one. When the
object is released, the reference counter decrements by one. When reference count is zero,
the object is removed from memory. 

With all object interactions, follow the pattern of: 
(1) create the object, (2) use the object, (3) then free the object from memory. 
*/

MyClass *classVar = [MyClass alloc]; // 'alloc' sets classVar's reference count to one. Returns pointer to object.
[classVar release]; // Decrements classVar's reference count.  
// 'retain' claims ownership of existing object instance and increments reference count. Returns pointer to object.
MyClass *newVar = [classVar retain]; // If classVar is released, object is still in memory because newVar is owner.
[classVar autorelease]; // Removes ownership of object at end of @autoreleasepool block. Returns pointer to object.

// @property can use 'retain' and 'assign' as well for small convenient definitions. 
@property (retain) MyClass *instance; // Release old value and retain a new one (strong reference).
@property (assign) NSSet *set; // Pointer to new value without retaining/releasing old (weak reference).

// Automatic Reference Counting (ARC)
// Because memory management can be a pain, Xcode 4.2 and iOS 4 introduced Automatic Reference Counting (ARC).
// ARC is a compiler feature that inserts retain, release, and autorelease automatically for you, so when using ARC, 
// you must not use retain, relase, or autorelease.
MyClass *arcMyClass = [[MyClass alloc] init]; 
// ... code using arcMyClass
// Without ARC, you will need to call: [arcMyClass release] after you're done using arcMyClass. But with ARC, 
// there is no need. It will insert this release statement for you. 

// As for the 'assign' and 'retain' @property attributes, with ARC you use 'weak' and 'strong'. 
@property (weak) MyClass *weakVar; // 'weak' does not take ownership of object. If original instance's reference count
// is set to zero, weakVar will automatically receive value of nil to avoid application crashing.
@property (strong) MyClass *strongVar; // 'strong' takes ownership of object. Ensures object will stay in memory to use.

// For regular variables (not @property declared variables), use the following:
__strong NSString *strongString; // Default. Variable is retained in memory until it leaves it's scope. 
__weak NSSet *weakSet; // Weak reference to existing object. When existing object is released, weakSet is set to nil.
__unsafe_unretained NSArray *unsafeArray; // Like __weak, but unsafeArray not set to nil when existing object is released.

```
## Further Reading

[La Page Wikipedia de l'Objective-C](http://fr.wikipedia.org/wiki/Objective-C)

[Programming with Objective-C. Apple PDF book](https://developer.apple.com/library/ios/documentation/cocoa/conceptual/ProgrammingWithObjectiveC/ProgrammingWithObjectiveC.pdf)

[iOS For High School Students: Getting Started](http://www.raywenderlich.com/5600/ios-for-high-school-students-getting-started)
