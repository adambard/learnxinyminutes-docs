---
name: Groovy
category: language
language: Groovy
lang: ca-es
filename: learngroovy-ca.groovy
contributors:
    - ["Roberto Pérez Alcolea", "http://github.com/rpalcolea"]
translations:
    - ["Xavier Sala Pujolar", "http://github.com/utrescu"]
---

Groovy - Un llenguatge dinàmic per la plataforma Java [Llegir-ne més.](http://www.groovy-lang.org/)

```groovy

/*
  Posa'l en marxa tu mateix:

  1) Instal.la SDKMAN - http://sdkman.io/
  2) Instal.la Groovy: sdk install groovy
  3) Inicia la consola groovy escrivint: groovyConsole

*/

//  Els comentaris d'una sola línia comencen amb dues barres inverses
/*
Els comentaris multilínia són com aquest.
*/

// Hola món
println "Hola món!"

/*
  Variables:

  Es poden assignar valors a les variables per fer-los servir més tard
*/

def x = 1
println x

x = new java.util.Date()
println x

x = -3.1499392
println x

x = false
println x

x = "Groovy!"
println x

/*
  Col.leccions i mapes
*/

// Crear una llista buida
def technologies = []

/*** Afegir elements a la llista ***/

// Com en Java
technologies.add("Grails")

// El shift a l'esquerra afegeix i retorna la llista
technologies << "Groovy"

// Afegir múltiples elements
technologies.addAll(["Gradle","Griffon"])

/*** Eliminar elements de la llista ***/

// Com en Java
technologies.remove("Griffon")

// La resta també funciona
technologies = technologies - 'Grails'

/*** Iterar per les llistes ***/

// Iterar per tots els elements de la llista
technologies.each { println "Technology: $it"}
technologies.eachWithIndex { it, i -> println "$i: $it"}

/*** Comprovar el contingut de la llista ***/

//Comprovar si la llista conté un o més elements (resultat boolea)
contained = technologies.contains( 'Groovy' )

// O
contained = 'Groovy' in technologies

// Comprovar diversos elements
technologies.containsAll(['Groovy','Grails'])

/*** Ordenar llistes ***/

// Ordenar una llista (canvia la original)
technologies.sort()

// Per ordenar sense canviar la original es pot fer:
sortedTechnologies = technologies.sort( false )

/*** Manipular llistes ***/

//Canvia tots els elements de la llista
Collections.replaceAll(technologies, 'Gradle', 'gradle')

// Desordena la llista
Collections.shuffle(technologies, new Random())

// Buida la llista
technologies.clear()

// Crear un mapa buit
def devMap = [:]

// Afegir valors al mapa
devMap = ['name':'Roberto', 'framework':'Grails', 'language':'Groovy']
devMap.put('lastName','Perez')

// Iterar per tots els elements del mapa
devMap.each { println "$it.key: $it.value" }
devMap.eachWithIndex { it, i -> println "$i: $it"}

// Comprovar si la clau hi és
assert devMap.containsKey('name')

// Comprova si el mapa conté un valor concret
assert devMap.containsValue('Roberto')

// Obtenir les claus del mapa
println devMap.keySet()

// Obtenir els valors del mapa
println devMap.values()

/*
  Groovy Beans

  Els GroovyBeans són JavaBeans però amb una sintaxi molt més senzilla

  Quan Groovy es compila a bytecode es fan servir les regles següents.

    * Si el nom és declarat amb un modificador (public, private o protected)
      es genera el camp

    * Un nom declarat sense modificadors genera un camp privat amb un getter
      i un setter públics (per exemple una propietat)

    * Si la propietat és declarada final el camp privat es crea i no es
      genera cap setter.

    * Es pot declarar una propietat i també declarar un getter i un setter.

    * Es pot declarar una propietat i un camp amb el mateix nom, la propietat
      farà servir el camp.

    * Si es vol una propietat private o protected s'ha de definir el getter i
      el setter que s'han de declarar private o protected.

    * Si s'accedeix a una propietat de la classe que està definida en temps
      de compilació amb un this implícit o explícit (per exemple this.foo, o
      bé només foo), Groovy accedirà al camp directament en comptes de fer-ho
      a través del getter i el setter.

    * Si s'accedeix a una propietat que no existeix tant implícita com
      explicitament, llavors Groovy accedirà a la propietat a través de la
      meta classe, que pot fer que falli en temps d'execució.

*/

class Foo {
    // Propietat només de lectura
    final String name = "Roberto"

    // Propietat de només lectura amb getter públic i un setter protected
    String language
    protected void setLanguage(String language) { this.language = language }

    // Propietat amb el tipus definit dinàmicament
    def lastName
}

/*
  Bucles i estructres de control
*/

//Groovy té el format tradicional de if -else
def x = 3

if(x==1) {
    println "One"
} else if(x==2) {
    println "Two"
} else {
    println "X greater than Two"
}

// Groovy també té l'operador ternari
def y = 10
def x = (y > 1) ? "worked" : "failed"
assert x == "worked"

//I també té 'l'Operador Elvis'!
//En comptes de fer servir l'operador ternari:
displayName = user.name ? user.name : 'Anonymous'

// Es pot escriure d'aquesta forma:
displayName = user.name ?: 'Anonymous'

//Bucle for
//Itera en un rang
def x = 0
for (i in 0 .. 30) {
    x += i
}

//Itera per una llista
x = 0
for( i in [5,3,2,1] ) {
    x += i
}

//Itera per un array
array = (0..20).toArray()
x = 0
for (i in array) {
    x += i
}

//Itera per un mapa
def map = ['name':'Roberto', 'framework':'Grails', 'language':'Groovy']
x = ""
for ( e in map ) {
    x += e.value
    x += " "
}
assert x.equals("Roberto Grails Groovy ")

/*
  Operadors

  Hi ha una llista d'operadors que poden ser sobreescrits en Groovy:
  http://www.groovy-lang.org/operators.html#Operator-Overloading

  Operadors útils de Groovy
*/
//Spread operator:  Invoca una acció en tots els ítems d'un grup d'objectes.
def technologies = ['Groovy','Grails','Gradle']
technologies*.toUpperCase() // = a technologies.collect { it?.toUpperCase() }

//Safe navigation operator: fet servir per evitar el NullPointerException.
def user = User.get(1)
def username = user?.username


/*
  Closures
  Un Closure és com un "bloc de codi" o un punter a un mètode. És un troç de
  codi que està definit i que s podrà executar més tard.

  Més informació a: http://www.groovy-lang.org/closures.html
*/
//Exemple:
def clos = { println "Hola món!" }

println "Executant el Closure:"
clos()

// Passar paràmetres a un Closure
def sum = { a, b -> println a+b }
sum(2,4)

//Els Closures poden fer referència a variables que no formen part de la
// llista dels seus paràmetres.
def x = 5
def multiplyBy = { num -> num * x }
println multiplyBy(10)

// Si es té un Closure que agafa un element com a argument, se'n pot ignorar
// la definició
def clos = { print it }
clos( "hi" )

/*
  Groovy pot recordar els resultats dels Closures [1][2][3]
*/
def cl = {a, b ->
    sleep(3000) // simula un procés llarg
    a + b
}

mem = cl.memoize()

def callClosure(a, b) {
    def start = System.currentTimeMillis()
    mem(a, b)
    println "(a = $a, b = $b) - en ${System.currentTimeMillis() - start} ms"
}

callClosure(1, 2)
callClosure(1, 2)
callClosure(2, 3)
callClosure(2, 3)
callClosure(3, 4)
callClosure(3, 4)
callClosure(1, 2)
callClosure(2, 3)
callClosure(3, 4)

/*
  Expando

  La classe Expando és un bean dinàmic al que se li poden afegir propietats i
  closures com a mètodes d'una instància d'aquesta classe.

http://mrhaki.blogspot.mx/2009/10/groovy-goodness-expando-as-dynamic-bean.html
*/
  def user = new Expando(name:"Roberto")
  assert 'Roberto' == user.name

  user.lastName = 'Pérez'
  assert 'Pérez' == user.lastName

  user.showInfo = { out ->
      out << "Name: $name"
      out << ", Last name: $lastName"
  }

  def sw = new StringWriter()
  println user.showInfo(sw)


/*
  Metaprogrammació (MOP)
*/

// Fent servir ExpandoMetaClass per afegir comportament
String.metaClass.testAdd = {
    println "he afegit això"
}

String x = "test"
x?.testAdd()

//Intercepting method calls
class Test implements GroovyInterceptable {
    def sum(Integer x, Integer y) { x + y }

    def invokeMethod(String name, args) {
        System.out.println "Invoca el mètode $name amb arguments: $args"
    }
}

def test = new Test()
test?.sum(2,3)
test?.multiply(2,3)

//Groovy supporta propertyMissing per gestionar la resolució de propietats
class Foo {
   def propertyMissing(String name) { name }
}
def f = new Foo()

assertEquals "boo", f.boo

/*
  TypeChecked i CompileStatic
  Groovy, by nature, és i sempre serà un llenguatge dinàmic però també té
  comprovació de tipus i definicions estàtiques

  More info: http://www.infoq.com/articles/new-groovy-20
*/
//TypeChecked
import groovy.transform.TypeChecked

void testMethod() {}

@TypeChecked
void test() {
    testMeethod()

    def name = "Roberto"

    println naameee

}

//Un altre exemple:
import groovy.transform.TypeChecked

@TypeChecked
Integer test() {
    Integer num = "1"

    Integer[] numbers = [1,2,3,4]

    Date date = numbers[1]

    return "Test"

}

//exemple de CompileStatic
import groovy.transform.CompileStatic

@CompileStatic
int sum(int x, int y) {
    x + y
}

assert sum(2,5) == 7


```

## Per aprendre'n més

[documentació de Groovy](http://www.groovy-lang.org/documentation.html)

[Cònsola de Groovy](http://groovyconsole.appspot.com/)

Uneix-te a un [grup d'usuaris Groovy]
(http://www.groovy-lang.org/usergroups.html)

## Llibres

* [Groovy Goodness] (https://leanpub.com/groovy-goodness-notebook)

* [Groovy in Action] (http://manning.com/koenig2/)

* [Programming Groovy 2: Dynamic Productivity for the Java Developer] (http://shop.oreilly.com/product/9781937785307.do)

[1] http://roshandawrani.wordpress.com/2010/10/18/groovy-new-feature-closures-can-now-memorize-their-results/
[2] http://www.solutionsiq.com/resources/agileiq-blog/bid/72880/Programming-with-Groovy-Trampoline-and-Memoize
[3] http://mrhaki.blogspot.mx/2011/05/groovy-goodness-cache-closure-results.html
