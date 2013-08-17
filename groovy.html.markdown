---
language: Groovy
filename: learngroovy.groovy
contributors:
    - ["Roberto Perez Alcolea", "http://github.com/rpalcolea"]
filename: learngroovy.groovy
---

Groovy - A dynamic language for the Java platform [Read more here.](http://groovy.codehaus.org)

```cpp

/*
  Set yourself up:

  1) Install GVM - http://gvmtool.net/
  2) Install Groovy: gvm install groovy
  3) Start the groovy console by typing: groovyConsole

*/

//  Single line comments start with two forward slashes
/*
Multi line comments look like this.
*/

// Hello World
println "Hello world!"

/*
  Variables:

  You can assign values to variables for later use
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
  Collections and maps
*/
//Creating an empty list
def technologies = []

//Add an element to the list
technologies << "Groovy"
technologies.add("Grails")
technologies.addAll(["Gradle","Griffon"])

//Remove an element from the list
technologies.remove("Griffon")

//Iterate over elements of a list
technologies.each { println "Technology: $it"}
technologies.eachWithIndex { it, i -> println "$i: $it"}

//Evaluate if a list contains element(s) (boolean)
technologies.contains('Groovy')
technologies.containsAll(['Groovy','Grails'])

//Sort a list
technologies.sort()

//Replace all elements in the list
Collections.replaceAll(technologies, 'Gradle', 'gradle')

//Shuffle a list
Collections.shuffle(technologies, new Random())

//Clear a list
technologies.clear()

//Creating an empty map
def devMap = [:]

//Add values
devMap = ['name':'Roberto', 'framework':'Grails', 'language':'Groovy']
devMap.put('lastName','Perez')

//Iterate over elements of a map
devMap.each { println "$it.key: $it.value" }
devMap.eachWithIndex { it, i -> println "$i: $it"}

//Evaluate if a map contains a key
assert devMap.containsKey('name')

//Evaluate if a map contains a value
assert devMap.containsValue('Roberto')

//Get the keys of a map
println devMap.keySet()

//Get the values of a map
println devMap.values()

/*
  Groovy Beans

  GroovyBeans are JavaBeans but using a much simpler syntax

  When Groovy is compiled to bytecode, the following rules are used.

    * If the name is declared with an access modifier (public, private or protected) then a field is generated.
    * A name declared with no access modifier generates a private field with public getter and setter (i.e. a property).
    * If a property is declared final the private field is created final and no setter is generated.
    * You can declare a property and also declare your own getter or setter.
    * You can declare a property and a field of the same name, the property will use that field then.
    * If you want a private or protected property you have to provide your own getter and setter which must be declared private or protected.
    * If you access a property from within the class the property is defined in at compile time with implicit or explicit this (for example this.foo, or simply foo), Groovy will access the field directly instead of going though the getter and setter.
    * If you access a property that does not exist using the explicit or implicit foo, then Groovy will access the property through the meta class, which may fail at runtime.

*/

class Foo {
    // read only property
    final String name = "Roberto"

    // read only property with public getter and protected setter
    String language
    protected void setLanguage(String language) { this.language = language }

    // dynamically typed property
    def lastName
}

/*
  Logical Branching and Looping
*/

//Groovy supports the usual if - else syntax
def x = 3

if(x==1) {
    println "One"
} else if(x==2) {
    println "Two"
} else {
    println "X greater than Two"
}

//Groovy also supports the ternary operator:
def y = 10
def x = (y > 1) ? "worked" : "failed"
assert x == "worked"

//For loop
//Iterate over a range
def x = 0
for (i in 0 .. 30) {
    x += i
}

//Iterate over a list
x = 0
for( i in [5,3,2,1] ) {
    x += i
}

//Iterate over an array
array = (0..20).toArray()
x = 0
for (i in array) {
    x += i
}

//Iterate over a map
def map = ['name':'Roberto', 'framework':'Grails', 'language':'Groovy']
x = 0
for ( e in map ) {
    x += e.value
}

```

## Further resources

[Groovy documentation](http://groovy.codehaus.org/Documentation)

[Groovy web console](http://groovyconsole.appspot.com/)

Join a [Groovy user group](http://groovy.codehaus.org/User+Groups)

## Books

* [Groovy Goodness] (https://leanpub.com/groovy-goodness-notebook)

* [Groovy in Action] (http://manning.com/koenig2/)

* [Programming Groovy 2: Dynamic Productivity for the Java Developer] (http://shop.oreilly.com/product/9781937785307.do)





