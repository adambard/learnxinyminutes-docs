---
language: Objective-C
contributors:
    - ["Eugene Yagrushkin", "www.about.me/yagrushkin"]
    - ["Yannick Loriot", "https://github.com/YannickL"]
    - ["Levi Bostian", "https://github.com/levibostian"]
translators:
    - ["David Hsieh", "http://github.com/deivuh"]
lang: es-es
filename: LearnObjectiveC-es.m
---
Objective C es el lenguaje de programación principal utilizado por Apple para los sistemas operativos macOS y iOS y sus respectivos frameworks, Cocoa y Cocoa Touch.
Es un lenguaje de programación para propósito general que le agrega al lenguaje de programación C una mensajería estilo "Smalltalk".


```objectivec
// Los comentarios de una sola línea inician con //

/*
Los comentarios de múltiples líneas se ven así.
*/

// Importa los encabezados de Foundation con #import
// Utiliza <> para importar archivos globales (generalmente frameworks)
// Utiliza "" para importar archivos locales (del proyecto)
#import <Foundation/Foundation.h>
#import "MyClass.h"

// Si habilitas módulos para proyectos de iOS >= 7.0 u OS X >= 10.9 en
// Xcode 5, puedes importarlos de la siguiente manera:
@import Foundation;

// El punto de entrada de tu programa es una función llamada 
// main con un tipo de retorno entero. 
int main (int argc, const char * argv[])
{
    // Crear un autorelease pool para manejar la memoria al programa
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    // Si se utiliza el conteo automático de referencias (ARC), 
    // utiliza @autoreleasepool:    
    @autoreleasepool {

    // Utiliza NSLog para imprimir líneas a la consola
    NSLog(@"Hello World!"); // Imprimir el string "Hello World!"
 
    ///////////////////////////////////////
    // Tipos y variables
    ///////////////////////////////////////
    
    // Declaraciones de primitivos
    int myPrimitive1  = 1;
    long myPrimitive2 = 234554664565;
    
    // Declaraciones de objetos
    // Pon el * como prefijo de los nombre de las variables para declaraciones
    // de objetos de tipos fuertes 
    MyClass *myObject1 = nil;  // Tipo fuerte
    id       myObject2 = nil;  // Tipo débil
    // %@ es un objeto
    // 'description' es una convención para mostrar el valor de los objetos
    NSLog(@"%@ and %@", myObject1, [myObject2 description]); 
    // imprime => "(null) and (null)"
    
    // String
    NSString *worldString = @"World";
    NSLog(@"Hello %@!", worldString); // imprime => "Hello World!" 
    // NSMutableString es una versión mutable del objeto NSString
    NSMutableString *mutableString = [NSMutableString stringWithString:@"Hello"];
    [mutableString appendString:@" World!"];
    NSLog(@"%@", mutableString); // imprime => "Hello World!"
    
    // Literales de caracteres
    NSNumber *theLetterZNumber = @'Z';
    char theLetterZ            = [theLetterZNumber charValue]; // o 'Z'
    NSLog(@"%c", theLetterZ);

    // Literales de enteros
    NSNumber *fortyTwoNumber = @42;
    int fortyTwo             = [fortyTwoNumber intValue]; // o 42
    NSLog(@"%i", fortyTwo);
    
    NSNumber *fortyTwoUnsignedNumber = @42U;
    unsigned int fortyTwoUnsigned    = [fortyTwoUnsignedNumber unsignedIntValue]; // o 42
    NSLog(@"%u", fortyTwoUnsigned);
    
    NSNumber *fortyTwoShortNumber = [NSNumber numberWithShort:42];
    short fortyTwoShort           = [fortyTwoShortNumber shortValue]; // o 42
    NSLog(@"%hi", fortyTwoShort);

    NSNumber *fortyOneShortNumber   = [NSNumber numberWithShort:41];
    unsigned short fortyOneUnsigned = [fortyOneShortNumber unsignedShortValue]; // o 41
    NSLog(@"%u", fortyOneUnsigned);
    
    NSNumber *fortyTwoLongNumber = @42L;
    long fortyTwoLong            = [fortyTwoLongNumber longValue]; // o 42
    NSLog(@"%li", fortyTwoLong);

    NSNumber *fiftyThreeLongNumber   = @53L;
    unsigned long fiftyThreeUnsigned = [fiftyThreeLongNumber unsignedLongValue]; // o 53
    NSLog(@"%lu", fiftyThreeUnsigned);

    // Literales de punto flotante
    NSNumber *piFloatNumber = @3.141592654F;
    float piFloat           = [piFloatNumber floatValue]; // o 3.141592654f
    NSLog(@"%f", piFloat); // imprime => 3.141592654
    NSLog(@"%5.2f", piFloat); // imprime => " 3.14"
    
    NSNumber *piDoubleNumber = @3.1415926535;
    double piDouble          = [piDoubleNumber doubleValue]; // o 3.1415926535
    NSLog(@"%f", piDouble);
    NSLog(@"%4.2f", piDouble); // imprime => "3.14"

    // NSDecimalNumber es una clase de punto-fijo que es más preciso que float o double    
    NSDecimalNumber *oneDecNum = [NSDecimalNumber decimalNumberWithString:@"10.99"];
    NSDecimalNumber *twoDecNum = [NSDecimalNumber decimalNumberWithString:@"5.002"];
    // NSDecimalNumber no tiene la capacidad de utilizar los operadores estándares 
    // +, -, * , /, por lo que cuenta con sus propios operadores:    
    [oneDecNum decimalNumberByAdding:twoDecNum]; 
    [oneDecNum decimalNumberBySubtracting:twoDecNum];
    [oneDecNum decimalNumberByMultiplyingBy:twoDecNum];
    [oneDecNum decimalNumberByDividingBy:twoDecNum];
    NSLog(@"%@", oneDecNum); // imprime => 10.99 como NSDecimalNumber es inmutable

    // Literales BOOL
    NSNumber *yesNumber = @YES;
    NSNumber *noNumber  = @NO;
    // o
    BOOL yesBool = YES;
    BOOL noBool  = NO;
    NSLog(@"%i", yesBool); // prints => 1

    // Objecto arreglo
    // Puede contener diferentes tipos de datos, pero deben de ser un objeto de
    // Objective-C    
    NSArray *anArray      = @[@1, @2, @3, @4];
    NSNumber *thirdNumber = anArray[2];
    NSLog(@"Third number = %@", thirdNumber); // imprime => "Third number = 3"
    // NSMutableArray es una versión mutable de NSArray, permitiendo el cambio
    // de los elementos del arreglo y el agrandado o encojimiento del objeto arreglo.
    // Conveniente, pero no tan eficiente como NSArray en cuanto a rendimiento. 
    NSMutableArray *mutableArray = [NSMutableArray arrayWithCapacity:2];
    [mutableArray addObject:@"Hello"];
    [mutableArray addObject:@"World"];
    [mutableArray removeObjectAtIndex:0];
    NSLog(@"%@", [mutableArray objectAtIndex:0]); // imprime => "World"

    // Objecto Diccionario
    NSDictionary *aDictionary = @{ @"key1" : @"value1", @"key2" : @"value2" };
    NSObject *valueObject     = aDictionary[@"A Key"];
    NSLog(@"Object = %@", valueObject); // imprime => "Object = (null)"
    // NSMutableDictionary también está disponible como un objeto mutable
    NSMutableDictionary *mutableDictionary = [NSMutableDictionary dictionaryWithCapacity:2];
    [mutableDictionary setObject:@"value1" forKey:@"key1"];
    [mutableDictionary setObject:@"value2" forKey:@"key2"];
    [mutableDictionary removeObjectForKey:@"key1"];

    // Objeto de Set
    NSSet *set = [NSSet setWithObjects:@"Hello", @"Hello", @"World", nil];
    NSLog(@"%@", set); // imprime => {(Hello, World)} (el orden puede variar)
    // NSMutableSet también está disponible como un objeto mutable
    NSMutableSet *mutableSet = [NSMutableSet setWithCapacity:2];
    [mutableSet addObject:@"Hello"];
    [mutableSet addObject:@"Hello"];
    NSLog(@"%@", mutableSet); // prints => {(Hello)}

    ///////////////////////////////////////
    // Operadores
    ///////////////////////////////////////
    
    // Los operadores funcionan como en el lenguaje C
    // Por ejemplo:
    2 + 5; // => 7
    4.2f + 5.1f; // => 9.3f
    3 == 2; // => 0 (NO)
    3 != 2; // => 1 (YES)
    1 && 1; // => 1 (and lógico)
    0 || 1; // => 1 (or lógico)
    ~0x0F; // => 0xF0 (negación bitwise)
    0x0F & 0xF0; // => 0x00 (AND bitwise)
    0x01 << 1; // => 0x02 (acarreamiento a la izquierda bitwise (por 1))

    ///////////////////////////////////////
    // Estructuras de control
    ///////////////////////////////////////

    // Declaraciones If-Else
    if (NO)
    {
        NSLog(@"I am never run");
    } else if (0)
    {
        NSLog(@"I am also never run");
    } else
    {
        NSLog(@"I print");
    }

    // Declaración Switch
    switch (2)
    {
        case 0:
        {
            NSLog(@"I am never run");
        } break;
        case 1:
        {
            NSLog(@"I am also never run");
        } break;
        default:
        {
            NSLog(@"I print");
        } break;
    }
    
    // Declaración de ciclos While
    int ii = 0;
    while (ii < 4)
    {
        NSLog(@"%d,", ii++); // ii++ incrementa ii en la misma línea, luego de 
                             // utilizar su valor
    } // imprime => "0," 
      //           "1,"
      //           "2,"
      //           "3,"

    // Declaración de ciclos For
    int jj;
    for (jj=0; jj < 4; jj++)
    {
        NSLog(@"%d,", jj);
    } // imprime => "0," 
      //           "1,"
      //           "2,"
      //           "3,"
     
    // Declaraciones foreach
    NSArray *values = @[@0, @1, @2, @3];
    for (NSNumber *value in values)
    {
        NSLog(@"%@,", value);
    } // imprime => "0," 
      //           "1,"
      //           "2,"
      //           "3,"

    // Objeto de ciclos For. Puede ser utilizado con cualquier tipo de objecto de 
    // Objective-C
    for (id item in values) { 
        NSLog(@"%@,", item); 
    } // imprime => "0," 
      //           "1,"
      //           "2,"
      //           "3,"

    // Declaraciones Try-Catch-Finally
    @try
    {
        // Tus declaraciones aquí
        @throw [NSException exceptionWithName:@"FileNotFoundException"
                            reason:@"File Not Found on System" userInfo:nil];
    } @catch (NSException * e) // utiliza: @catch (id exceptionName) para atrapar 
                               // todos los objetos
    {
        NSLog(@"Exception: %@", e);
    } @finally
    {
        NSLog(@"Finally. Time to clean up.");
    } // imprime => "Exception: File Not Found on System"
      //           "Finally. Time to clean up."

    // Los objetos NSError son útiles para argumentos de función para los 
    // errores de usuario.    
    NSError *error = [NSError errorWithDomain:@"Invalid email." code:4 userInfo:nil];
 
    ///////////////////////////////////////
    // Objetos
    ///////////////////////////////////////
    
    // Crea una instancia de objeto alocando memoria e inicializándola
    // Un objeto no es completamente funcional hasta que ambos pasos hayan sido
    // completados
    MyClass *myObject = [[MyClass alloc] init];
        
    // El modelo de programación orientada a objetos de Objective-C es basada en
    // el envío de mensajes a instancias de objetos
    // En Objective-C no se llama a un método; se envía un mensaje    
    [myObject instanceMethodWithParameter:@"Steve Jobs"];

    // Limpiar la memoria que se utilizó en el programa    
    [pool drain];

    // Fin de @autoreleasepool
    }
    
    // Fin del programa
    return 0;
}

///////////////////////////////////////
// Clases y funciones
///////////////////////////////////////

// Declara tu clase en archivo de encabezado (MyClass.h)
// Sintaxis de declaración de clase:
// @interface NombreDeClase : NombreDeClasePadre <ProtocolosImplementados>
// {
//    type nombre; <= declaraciones de variables;
// }
// @property tipo nombre; <= declaración de propiedades
// -/+ (tipo) Declaración de método; <= Declaración de método
// @end
@interface MyClass : NSObject <MyProtocol> // NSObject es la clase de objeto 
                                           // base de  Objective-C.
{
    // Declaraciones de variables de instancia (puede existir en el archivo de 
    // interfaz o de implementación)    
    int count; // Acceso protegido por defecto. 
    @private id data; // Acceso privado (Más conveniente de declarar en el 
                      // archivo de implementación)
    NSString *name; 
}
// Notación conveneinte para acceso público de las variables para generar un 
// método setter
// Por defecto, el nombre del método setter 'set' seguido del nombre de 
// variable @property
@property int propInt; // Nombre del método 'setter' = 'setPropInt'
@property (copy) id copyId; // (copy) => Copia el objeto durante la asignación
// (readonly) => No se le puede asignar un valor fuera de @interface
@property (readonly) NSString *roString; // utiliza @synthesize en 
                                         // @implementation para crear un accesor
// Puedes personalizar el nombre del getter y del setter en lugar de utilizar
// el nombre por defecto "set".
@property (getter=lengthGet, setter=lengthSet:) int length;
 
// Métodos
+/- (return type)methodSignature:(Parameter Type *)parameterName;

// + Para métodos de clase:
+ (NSString *)classMethod;
+ (MyClass *)myClassFromHeight:(NSNumber *)defaultHeight;

// - Para métodos de instancia:
- (NSString *)instanceMethodWithParameter:(NSString *)string;
- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number;

// Métodos de constructor con argumentos
- (id)initWithDistance:(int)defaultDistance;
// Los nombres de los métodos de Objective-C son muy descriptivos. 
// Siempre nombra los métodos de acuerdo con sus argumentos

@end // Define el final de la interfaz


// Para acceder a las variables públicas desde el archivo de implementación, 
// @property genera un método setter automáticamente. El nombre del método 
// es 'set' seguido de un nombre de variable @property:
MyClass *myClass = [[MyClass alloc] init]; // Crea una instancia del objeto MyClass 
[myClass setCount:10]; 
NSLog(@"%d", [myClass count]); // imprime => 10
// O utilizando los métodos getter y setter personalizados en @interface:
[myClass lengthSet:32];
NSLog(@"%i", [myClass lengthGet]); // imprime => 32
// Por conveniencia, puedes utilizar la notación de punto para asignar y 
// acceder a las variables de una instancia de objeto.
myClass.count = 45;
NSLog(@"%i", myClass.count); // imprime => 45

// Llama a métodos de clase:
NSString *classMethodString = [MyClass classMethod];
MyClass *classFromName = [MyClass myClassFromName:@"Hello"];

// Llama a métodos de instancia:
MyClass *myClass = [[MyClass alloc] init]; // Crea una instancia de objeto Myclass
NSString *stringFromInstanceMethod = [myClass instanceMethodWithParameter:@"Hello"];

// Selectors
// Una forma dinámica de representar métodos. Utilizados para llamar métodos 
// de una clase, pasar métodos a través de funciones para avisar a otras clases 
// para que lo llamen, y para guardar métodos como una variable.
// SEL es el tipo de dato. @selector() devuelve un selector del nombre de 
// método proveído methodAparameterAsString:andAParameterAsNumber: es un nombre 
// para un método en MyClass
SEL selectorVar = @selector(methodAParameterAsString:andAParameterAsNumber:); 
if ([myClass respondsToSelector:selectorVar]) { // Revisa si la clase contiene el método
    // Debe de poner todos los argumentos de método en un solo objeto para mandar una 
    // función performSelector.    
    NSArray *arguments = [NSArray arrayWithObjects:@"Hello", @4, nil];
    [myClass performSelector:selectorVar withObject:arguments]; // Calls the method
} else {
    // NSStringFromSelector() devuelve un NSString del nombre de método de un selector dado
    NSLog(@"MyClass does not have method: %@", NSStringFromSelector(selectedVar));
}

// Implementa los métodos de un archivo de implementación (MyClass.m):
@implementation MyClass {
    long distance; // Variable de instancia de acceso privado
    NSNumber height;
}

// Para acceder a una variable pública del archivo de interfaz, utiliza '_' seguido del
// nombre de la variable:
_count = 5; // Hace referencia a "int count" de la interfaz de MyClass
// Accede variables definidas en el archivo de implementación:
distance = 18; // Hace referencia a "long distance" de la implementación de MyClass
// Para utilizar una variable @property en el archivo de implementación, utiliza 
// @synthesize para crear una variable de acceso:
@synthesize roString = _roString; // _roString ahora está disponible en @implementation

// Lamado antes de llamar algún método o instanciar cualquier objeto
+ (void)initialize 
{
    if (self == [MyClass class]) {
        distance = 0;
    }
}

// Contraparte para inicializar un método. Llamado cuando el contador de referencias
// del objeto es cero
- (void)dealloc
{
    [height release]; // Si no utilizas ARC, asegúrate de liberar las variables de 
                      // objeto de las clases
    [super dealloc];  // y llama el método dealloc de la clase padre
}

// Los constructores son una manera de crear instancias de una clase
// Este es el constructor por defecto que es llamado cuando el objeto es inicializado.
- (id)init
{
    if ((self = [super init])) // 'super' es utilizado para acceder a los 
                               // métodos de la clase padre.
    {
        self.count = 1; // 'self' es utilizado para que el objeto se llame a sí mismo.
    }
    return self;
}
// Se pueden crear constructores que contiene argumentos
- (id)initWithDistance:(int)defaultDistance 
{
    distance = defaultDistance;
    return self;
}

+ (NSString *)classMethod
{
    return @"Some string";
}

+ (MyClass *)myClassFromHeight:(NSNumber *)defaultHeight 
{
    height = defaultHeight;
    return [[self alloc] init];
}

- (NSString *)instanceMethodWithParameter:(NSString *)string
{
    return @"New string";
}

- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number
{
    return @42;
}

// Objective-C no tiene declaraciones de métodos privados, pero pueden ser simulados.
// Para simular un método privado, crea un método en @implementation pero no en @interface.
- (NSNumber *)secretPrivateMethod {
    return @72;
}
[self secretPrivateMethod]; // Calls private method

// Métodos declarados dentro de MyProtocol
- (void)myProtocolMethod
{
    // statements
}

@end // Declara el final de la implementación

///////////////////////////////////////
// Categorías
///////////////////////////////////////
// Una categoría es un grupo de métodos diseñados para extender una clase. 
// Te permiten agregar nuevos métodos a una clase existente por propósitos 
// de organización. Éstos no deben de serconfundidos con subclases. 
// Las subclases existen para CAMBIAR la funcionalidad de un objeto mientras 
// que las categoríasle AGREGAN funcionalidad de un objeto.
// Las categorías te permiten:
// -- Agregar métodos a una clase existente por propósitos de oganización.
// -- Extender clases de objetos de Objective-C (ejemplo: NSString) para 
//    agregar tus propios métodos.
// -- Agregar la habilidad de crear métodos protegidos y privados para las clases.
// NOTA: No sobreescribas los métodos de las clases base en una categoría 
// aunque tengas la habilidad de poder hacerlo. Sobreescribir métodos puede 
// causar errores en la compilación después entre diferentes categorías y 
// puede arruinar el propósito de las categorías de solo AGREGAR funcionalidad. 
// Utiliza subclass para sobreescribir métodos.

// Aquí una clase base simple, Car.
@interface Car : NSObject

@property NSString *make;
@property NSString *color;

- (void)turnOn;
- (void)accelerate;

@end

// Y la implementación de la clase simple, Car
#import "Car.h"

@implementation Car

@synthesize make = _make;
@synthesize color = _color;

- (void)turnOn {
    NSLog(@"Car is on.");
}
- (void)accelerate {
    NSLog(@"Accelerating.");
}

@end

// Ahora, si quisieramos crear un objeto Truck (Camión), crearíamos una 
// subclase de Car (Carro) como si le cambiaramos de funcionalidad de Car 
// para que se comporte como un camión. Pero digamos que únicamente queremos 
// agregar funcionalidad al Car (Carro) existente. Un buen ejemplo sería 
// limpiar el carro. Así que crearíamos una cateog®iea para agregar los 
// métodos de limpieza: 
// Archivo @interface: Car+Clean.h (NombreBaseDeClase+NombreDeCategoria.h)
#import "Car.h" // Asegúrate de improtar la clase que deseas extener.

@interface Car (Clean) // El nombre de la categoría está dentro de (), 
                       // seguido del nombre de la clase base

- (void)washWindows; // Nombres de los nuevos métodos que le agregamos 
                     // a nuestro objeto Car
- (void)wax;

@end

// Archivo @implementation: Car+Clean.m (NombreBaseDeClase+NombreDeCategoria.m)
#import "Car+Clean.h" // Importa el archivo de @interface de la categoría Clean

@implementation Car (Clean)

- (void)washWindows {
    NSLog(@"Windows washed.");
}
- (void)wax {
    NSLog(@"Waxed.");
}

@end 

// Cualquier instancia del objeto Car tiene la habilidad de utilizar una 
// categoría. Todo lo que necesitan es importarlo: 
#import "Car+Clean.h" // Importa todas las diferentes categorías que 
                      // necesites utilizar
#import "Car.h" // También debes de importar la clase base para su 
                // funcionalidad original 

int main (int argc, const char * argv[]) {
    @autoreleasepool {
        Car *mustang = [[Car alloc] init];
        mustang.color = @"Red";
        mustang.make = @"Ford";

        [mustang turnOn]; // Utiliza métodos de la clase base Car.
        [mustang washWindows]; // Utiliza métodos de la categoría Clean de Car.
    }
    return 0; 
}

// Objective-C no tiene declaraciones para métodos protegidos, pero los puedes
// simular. Crea una categoría conteniendo todos los métodos protegidos, 
// luego importa ÚNICAMENTE al archivo @implementation de una clase que 
// pertenece a la clase Car.
@interface Car (Protected) // Nombrando la categoría 'Protected' para 
                           // recordar que los métodos están protegidos
    
- (void)lockCar; // Los métodos enlistados aquí solo puedens ser creados 
                 // por objetos Car

@end
// Para utilizar los métodos protegidos, importa la categoría, 
// luego implementa sus métodos:
#import "Car+Protected.h" // Recuerda, importa únicamente el archivo 
                          // de @implementation

@implementation Car 

- (void)lockCar {
    NSLog(@"Car locked."); // Las instancias de Car no puede utilizar 
                           // lockCar porque no se encuentra en @interface
}

@end

///////////////////////////////////////
// Extensiones
///////////////////////////////////////
// Las Extensions te permiten sobreescribir atributos de propiedades de 
// acceso público y métodos de un @interface
// Archivo @interface: Shape.h
@interface Shape : NSObject 

@property (readonly) NSNumber *numOfSides;

- (int)getNumOfSides;

@end
// Puedes sobreescribir la variable numOfSides o el métodos getNumOfSlides 
// para modificarlos con una extensión:
// Archivo @implementation: Shape.m
#import "Shape.h"
// Las extensiones se encuentran en el mismo archivo que el archivo 
// de @implementation
@interface Shape () // () después del nombre de la clase base declara 
                    // una extensión

@property (copy) NSNumber *numOfSides; // Hacer numOfSlides copy en lugar
                                       // de readonly.
-(NSNumber)getNumOfSides; // Hacer que getNumOfSides devuelva un NSNumber 
                          // en lugar de un int.
-(void)privateMethod; // También puedes crear una nuevos métodos privados 
                      // dentro de las extensiones

@end
// @implementation principal:
@implementation Shape 

@synthesize numOfSides = _numOfSides;

-(NSNumber)getNumOfSides { // Todas las declaraciones dentro de extensions 
                           // deben de ser dentro de @implementation
    return _numOfSides;
}
-(void)privateMethod {
    NSLog(@"Private method created by extension. Shape instances cannot call me.");
}

@end

///////////////////////////////////////
// Protocolos
///////////////////////////////////////
// Un protocolo declara métodos que pueden ser implementados por cualquier otra
// clase. Los protocolos no son clases. Simplementen define una interfaz que 
// otros objetos deben de implementar.
// Archivo @protocol: "CarUtilities.h"
@protocol CarUtilities <NSObject> // <NSObject> => Nombre de otro protocolo 
                                  // que se incluye en éste
    @property BOOL engineOn; // La clase que lo adopta debe de utilizar 
                            // @synthesize para todas las @properties definidas 
    - (void)turnOnEngine; // y todos los métodos definidos
@end
// A continuación una clase ejemplo que implementa el protcolo
#import "CarUtilities.h" // Importar el archivo @protocol.

@interface Car : NSObject <CarUtilities> // El nombre del protocolo dentro de <>
    // No necesitas los nombres de @property o métodos aquí para CarUtilities.
    // Estos solo es requerido por @implementation.
- (void)turnOnEngineWithUtilities:(id <CarUtilities>)car; // También Puedes 
                                                          // utilizar protocolos 
                                                          // como datos.
@end
// El @implementation necesita que se implementen @properties y métodos 
// del protocolo.
@implementation Car : NSObject <CarUtilities>

@synthesize engineOn = _engineOn; // Crear una declaración @synthesize para el 
                                  // @property engineOn.

- (void)turnOnEngine { // Implementa turnOnEngine como quieras. Los 
                       // protocolos no definen
    _engineOn = YES;   // cómo implementas un método, con tal de que lo implementes.
}
// Puedes utilizar un protocolo como data mientras sepas quee métodos y variables 
// tiene implementado.
- (void)turnOnEngineWithCarUtilities:(id <CarUtilities>)objectOfSomeKind { 
    [objectOfSomeKind engineOn]; // Tienes acceso a las variables 
    [objectOfSomeKind turnOnEngine]; // y los métodos del objeto
    [objectOfSomeKind engineOn]; // Puede o no puede ser YES. La clase lo 
                                 // implementa como se quiera.
}

@end
// Las instancias de Car ahora tienen acceso al protocolo.
Car *carInstance = [[Car alloc] init];
[carInstance setEngineOn:NO];
[carInstance turnOnEngine];
if ([carInstance engineOn]) {
    NSLog(@"Car engine is on."); // imprime => "Car engine is on."
}
// Asegúrate de revisar si un objeto de tipo 'id' implementa un protocolo antes
// de llamar a sus métodos:
if ([myClass conformsToProtocol:@protocol(CarUtilities)]) {
    NSLog(@"This does not run as the MyClass class does not implement the CarUtilities protocol.");
} else if ([carInstance conformsToProtocol:@protocol(CarUtilities)]) {
    NSLog(@"This does run as the Car class implements the CarUtilities protocol.");
}
// Las categorías también pueden implementar protcolos: @interface Car 
// (CarCategory) <CarUtilities>
// Puedes implementar varios protocolos: 
// @interface Car : NSObject <CarUtilities, CarCleaning>
// NOTA: Si dos o más protocolos dependen entre sí, asegúrate de declararlos 
// de manera adelantada:
#import "Brother.h"

@protocol Brother; // Declaración adelantada. Sin ésto, el compilador 
                   // tira un error.

@protocol Sister <NSObject>

- (void)beNiceToBrother:(id <Brother>)brother;

@end

// Ver si el problema es que Sister depende de Brother, 
// y Brother dependa de Sister.
#import "Sister.h"

@protocol Sister; // Estas líneas detienen la recursión, resolviendo el problema.

@protocol Brother <NSObject>
 
- (void)beNiceToSister:(id <Sister>)sister;

@end


///////////////////////////////////////
// Bloques
///////////////////////////////////////
// Los bloques son declaraciones de código, tal como una función, pueden 
// ser utilizados como data.
// A continuación un bloque simple con un argumento entero que devuelve 
// un el argumento más 4.
int (^addUp)(int n); // Declarar una variable para almacenar el bloque.
void (^noParameterBlockVar)(void); // Ejemplo de una declaración de variable 
                                   // de bloque sin argumentos.
// Los bloques tienen acceso a variables del mismo ámbito. Pero las variables 
// son solo readonly y el valor pasado al bloque es el valor de la variable 
// cuando el bloque es creado.
int outsideVar = 17; // Si modificamos outsideVar después de declarar addUp,
                     // outsideVar AÚN es 17.
__block long mutableVar = 3; // __block hace que las variables se puedan 
                             // escribir en bloques.
addUp = ^(int n) { // Remueve (int n) para tener un bloque que no recibe 
                   // ningún parámetro
    NSLog(@"You may have as many lines in a block as you would like.");
    NSSet *blockSet; // También puedes declarar variables locales.
    mutableVar = 32; // Asignar un nuevo valor a la variable __block.
    return n + outsideVar; // Declaraciones de retorno son opcionales.
}
int addUp = add(10 + 16); // Llama al bloque de código con argumentos.
// Los bloques son usualmente utilizados como argumentos a funciones que 
// son llamados más adelante o para callbacks.
@implementation BlockExample : NSObject 
 
 - (void)runBlock:(void (^)(NSString))block {
    NSLog(@"Block argument returns nothing and takes in a NSString object.");
    block(@"Argument given to block to execute."); // Calling block.
 }

 @end


///////////////////////////////////////
// Manejo de memoria
///////////////////////////////////////
/* 
Para cada objeto utilizado en una aplicación, la memoria debe de ser alocada 
para ese objeto. Cuando la aplicación termina de utilizar ese objeto, la 
memoria debe de ser desalocada para asegurar la  eficiencia de la aplicación.
Objetive-C no utiliza garbage collection, y en lugar de eso utiliza conteos 
de referencias. Mientras haya al menos una referencia del objeto (también 
conocido como tener un objeto de adueñado), entonces el objeto estará 
disponible para su uso.

Cuando una instancia es dueña un objeto, su contador de referencia incrementa
por uno. Cuando el objeto es liberado, el contador de referencia decrementa uno. 
Cuando el conteo de referencia es cero, el objeto es removido de la memoria.

Con todas las interacciones de los objetos, sigue el patrón de:
(1) Crear e lobjeto, (2) utiliza el objeto, (3) libera el objeto de la memoria.
*/

MyClass *classVar = [MyClass alloc]; // 'alloc' asigna uno al conteo de 
                                     // referencias de classVar. Devuelve un 
                                     // puntero al objeto
[classVar release]; // Decrementa el conteo de referencias de classVar's
// 'retain'
// 'retain' adueña la instancia de objeto existente e incrementa el conteo de 
// referencia por uno. Devuelve un puntero al objeto.
MyClass *newVar = [classVar retain]; // Si classVar es liberado, el objeto 
                                     // aún se queda en memoria porque newVar
                                     // es el dueño.
[classVar autorelease]; // Remueve el adueñamiento de un objeto al final del 
                        // bloque @autoreleasepool. Devuelve un puntero al objeto.

// @property puede utilizar 'retain' y 'assign' también para pequeñas 
// definiciones convenientes
@property (retain) MyClass *instance; // Libera el valor viejo y retiene 
                                      // uno nuevo (referencia fuerte)
@property (assign) NSSet *set; // Apunta a un nuevo valor sin retener/liberar 
                               // una referencia vieja (débil)

// Conteo Automático de Referencias (ARC)
// Debido a que el manejo de memoria puede ser un dolor, en Xcode 4.2 y iOS 4 
// se introdujo el Conteo Automático de Referencias (ARC).
// ARC es una funcionalidad del compilador que agrega retain, release y
// autorealase automáticamente, así que al
// utilizar ARC, no se debe de utilizar retain, release o autorelease.
MyClass *arcMyClass = [[MyClass alloc] init]; 
// ... código utilizando arcMyClass
// Sin ARC, necesitarás llamar: [arcMyClass release] luego de terminar de 
// utilizar arcMyClass. Pero con ARC, no hay necesidad. Insertará 
// automáticamente la declaración de liberación.

// Mientras que para los atributos de @property 'assign' y 'retain', con ARC 
// utilizarás 'weak' y 'strong'
@property (weak) MyClass *weakVar; // 'weak' no adueña el objeto. El conteo de 
                                   // referencias de la instancia original
// es fijado a ceor, weakVar automáticamente recibe el valor de  nil para 
// evitar cualquier 'crashing'.
@property (strong) MyClass *strongVar; // 'strong' se adueña del objeto. 
                                       // Asegura que el objeto se quede en memoria.

// Para variables regulares (no variables de @property), utiliza lo siguiente:
__strong NSString *strongString; // Por defecto. La variables de retenida en 
                                 // memoria hasta que se salga del ámbito.
__weak NSSet *weakSet; // Referencia débil a un objeto existente. Cuando el 
                       // objeto existente es liberado, weakSet le es asginado
                       // un valor nil
__unsafe_unretained NSArray *unsafeArray; // Como __weak, pero unsafeArray no 
                                          // es asginado a nil cuando el objeto
                                          // existente es liberado.

```
## Lecturas sugeridas

[Wikipedia Objective-C](http://es.wikipedia.org/wiki/Objective-C)

[Programming with Objective-C. Libro PDF de Apple](https://developer.apple.com/library/ios/documentation/cocoa/conceptual/ProgrammingWithObjectiveC/ProgrammingWithObjectiveC.pdf)

[iOS For High School Students: Getting Started](http://www.raywenderlich.com/5600/ios-for-high-school-students-getting-started)
