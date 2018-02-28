---
language: dart
filename: learndart-pt.dart
contributors:
    - ["Joao Pedrosa", "https://github.com/jpedrosa/"]
translators:
    - ["Junior Damacena", "https://github.com/jdamacena/"]
lang: pt-br
---

Dart é uma novata no reino das linguagens de programação.
Ela empresta muito de outras linguagens mais conhecidas, e tem a meta de não se diferenciar muito de seu irmão, JavaScript. Assim como JavaScript, Dart foi pensada para oferecer grande integração com o Browser.

A característica mais controversa da Dart é a sua Tipagem Opcional, ou seja, não é obrigatório declarar tipos.

```dart
import "dart:collection";
import "dart:math" as DM;

// Bem vindo ao Aprenda Dart em 15 minutos. http://www.dartlang.org/
// Este é um tutorial executável. Você pode rodar esse tutorial com Dart ou no
// site Try Dart!, é só copiar e colar este código lá. http://try.dartlang.org/

// Declarações de funções e métodos são iguais. Declarações de funções 
// podem ser aninhadas. A declaração é feita das seguintes formas
// nome() {} ou nome() => expressaoDeUmaLinhaSo;
// A declaração feita com a seta tem um return implícito para o resultado da
// expressão.
example1() {
  example1nested1() {
    example1nested2() => print("Example1 nested 1 nested 2");
    example1nested2();
  }
  example1nested1();
}

// Funções anônimas são criadas sem um nome.
example2() {
  example2nested1(fn) {
    fn();
  }
  example2nested1(() => print("Example2 nested 1"));
}

// Quando uma função é declarada como parâmetro, a declaração pode incluir o número
// de parâmetros que a função recebe, isso é feito especificando o nome de cada um dos 
// parâmetros que serão recebidos pela função.
example3() {
  example3nested1(fn(informSomething)) {
    fn("Example3 nested 1");
  }
  example3planB(fn) { // Ou não declare o número de parâmetros.
    fn("Example3 plan B");
  }
  example3nested1((s) => print(s));
  example3planB((s) => print(s));
}

// Funções têm acesso à variáveis fora de seu escopo
var example4Something = "Example4 nested 1";
example4() {
  example4nested1(fn(informSomething)) {
    fn(example4Something);
  }
  example4nested1((s) => print(s));
}

// Declaração de classe com um método chamado sayIt, que também tem acesso 
// à variável externa, como se fosse uma função como se viu antes.
var example5method = "Example5 sayIt";
class Example5Class {
  sayIt() {
    print(example5method);
  }
}
example5() {
  // Criar uma instância anônima de Example5Class e chamar o método sayIt
  // nela.
  new Example5Class().sayIt();
}

// A declaração de uma classe é feita da seguinte maneira: class name { [classBody] }.
// onde classBody pode incluir métodos e variáveis de instância, assim como
// métodos e variáveis de classe.
class Example6Class {
  var example6InstanceVariable = "Example6 instance variable";
  sayIt() {
    print(example6InstanceVariable);
  }
}
example6() {
  new Example6Class().sayIt();
}

// Métodos e variáveis de classe são declarados como "static".
class Example7Class {
  static var example7ClassVariable = "Example7 class variable";
  static sayItFromClass() {
    print(example7ClassVariable);
  }
  sayItFromInstance() {
    print(example7ClassVariable);
  }
}
example7() {
  Example7Class.sayItFromClass();
  new Example7Class().sayItFromInstance();
}

// Literais são ótimos, mas há uma limitação para o que eles podem ser
// quando estão fora do corpo de uma função/método. Literais fora do escopo da classe
// ou fora da classe têm que ser constantes. Strings e números são constantes
// por padrão. Mas arrays e mapas não. Eles podem ser declarados como constantes
// usando o comando "const".
var example8A = const ["Example8 const array"],
  example8M = const {"someKey": "Example8 const map"};
example8() {
  print(example8A[0]);
  print(example8M["someKey"]);
}

// Loops em Dart são criados com  for () {} ou while () {},
// um pouco mais moderno temos for (.. in ..) {}, ou funções de callbacks com muitas
// funcionalidades, começando com o forEach.
var example9A = const ["a", "b"];
example9() {
  for (var i = 0; i < example9A.length; i++) {
    print("Example9 for loop '${example9A[i]}'");
  }
  var i = 0;
  while (i < example9A.length) {
    print("Example9 while loop '${example9A[i]}'");
    i++;
  }
  for (var e in example9A) {
    print("Example9 for-in loop '${e}'");
  }
  example9A.forEach((e) => print("Example9 forEach loop '${e}'"));
}

// Para percorrer os caracteres de uma string ou extrair uma substring.
var example10S = "ab";
example10() {
  for (var i = 0; i < example10S.length; i++) {
    print("Example10 String character loop '${example10S[i]}'");
  }
  for (var i = 0; i < example10S.length; i++) {
    print("Example10 substring loop '${example10S.substring(i, i + 1)}'");
  }
}

// Int e double são os dois formatos de número suportados.
example11() {
  var i = 1 + 320, d = 3.2 + 0.01;
  print("Example11 int ${i}");
  print("Example11 double ${d}");
}

// DateTime traz operações com data/hora.
example12() {
  var now = new DateTime.now();
  print("Example12 now '${now}'");
  now = now.add(new Duration(days: 1));
  print("Example12 tomorrow '${now}'");
}

// Expressões regulares são suportadas.
example13() {
  var s1 = "some string", s2 = "some", re = new RegExp("^s.+?g\$");
  match(s) {
    if (re.hasMatch(s)) {
      print("Example13 regexp matches '${s}'");
    } else {
      print("Example13 regexp doesn't match '${s}'");
    }
  }
  match(s1);
  match(s2);
}

// Expressões booleanas precisam retornar ou true ou false, já que
// Dart não faz a conversão implicitamente.
example14() {
  var v = true;
  if (v) {
    print("Example14 value is true");
  }
  v = null;
  try {
    if (v) {
      // Nunca seria executada
    } else {
      // Nunca seria executada
    }
  } catch (e) {
    print("Example14 null value causes an exception: '${e}'");
  }
}

// try/catch/finally e throw são usados para tratamento de exceções.
// throw aceita qualquer objeto como parâmetro;
example15() {
  try {
    try {
      throw "Some unexpected error.";
    } catch (e) {
      print("Example15 an exception: '${e}'");
      throw e; // Re-throw
    }
  } catch (e) {
    print("Example15 catch exception being re-thrown: '${e}'");
  } finally {
    print("Example15 Still run finally");
  }
}

// Para mais eficiência ao criar strings longas dinamicamente, use o
// StringBuffer. Ou você pode também concatenar um array de strings.
example16() {
  var sb = new StringBuffer(), a = ["a", "b", "c", "d"], e;
  for (e in a) { sb.write(e); }
  print("Example16 dynamic string created with "
    "StringBuffer '${sb.toString()}'");
  print("Example16 join string array '${a.join()}'");
}

// Strings podem ser concatenadas apenas colocando strings literais uma perto
// da outra, sem necessidade de nenhum outro operador.
example17() {
  print("Example17 "
      "concatenar "
      "strings "
      "é simples assim");
}

// Strings podem ser delimitadas por apóstrofos ou aspas e não há
// diferença entre os dois. Essa flexibilidade pode ser boa para
// evitar a necessidade de escapar conteúdos que contenham o delimitador da string.
// Por exemplo, aspas dos atributos HTMLse a string conter HTML.
example18() {
  print('Example18 <a href="etc">'
      "Don't can't I'm Etc"
      '</a>');
}

// Strings com três apóstrofos ou aspas podem
// ter muitas linhas e incluem os delimitadores de linha (ou seja, os enter).
example19() {
  print('''Example19 <a href="etc">
Example19 Don't can't I'm Etc
Example19 </a>''');
}

// Strings têm a função de interpolação que é chamada com o caractere $.
// Com $ { [expression] }, o retorno da expressão é interpolado.
// $ seguido pelo nome de uma variável interpola o conteúdo dessa variável.
// $ pode ser escapedo assim \$.
example20() {
  var s1 = "'\${s}'", s2 = "'\$s'";
  print("Example20 \$ interpolation ${s1} or $s2 works.");
}

// A tipagem opcional permite que APIs usem anotações e também ajuda os
// IDEs na hora das refatorações, auto-complete e checagem de
// erros. Note que até agora não declaramos nenhum tipo e o programa está
// funcionando normalmente. De fato, os tipos são ignorados em tempo de execução.
// Os tipos podem até mesmo estarem errados e o programa ainda vai dar o
// benefício da dúvida e rodar, visto que os tipos não importam.
// Existe um parâmetro que checa erros de tipagem que é o
// checked mode, dizem que é útil enquanto se está desenvolvendo,
// mas também é mais lento devido às checagens extras e por isso
// é evitado em ambiente de produção.
class Example21 {
  List<String> _names;
  Example21() {
    _names = ["a", "b"];
  }
  List<String> get names => _names;
  set names(List<String> list) {
    _names = list;
  }
  int get length => _names.length;
  void add(String name) {
    _names.add(name);
  }
}
void example21() {
  Example21 o = new Example21();
  o.add("c");
  print("Example21 names '${o.names}' and length '${o.length}'");
  o.names = ["d", "e"];
  print("Example21 names '${o.names}' and length '${o.length}'");
}

// Herança em classes é feito assim: class name extends AnotherClassName {}.
class Example22A {
  var _name = "Some Name!";
  get name => _name;
}
class Example22B extends Example22A {}
example22() {
  var o = new Example22B();
  print("Example22 class inheritance '${o.name}'");
}

// Mistura de classes também é possível, e é feito assim:
// class name extends SomeClass with AnotherClassName {}
// É necessário extender uma classe para poder misturar com outra.
// No momento, classes misturadas não podem ter construtor.
// Mistura de classes é mais usado para compartilhar métodos com classes distantes, então
// a herança comum não fica no caminho do reuso de código.
// As misturas aparecem após o comando "with" na declaração da classe.
class Example23A {}
class Example23Utils {
  addTwo(n1, n2) {
    return n1 + n2;
  }
}
class Example23B extends Example23A with Example23Utils {
  addThree(n1, n2, n3) {
    return addTwo(n1, n2) + n3;
  }
}
example23() {
  var o = new Example23B(), r1 = o.addThree(1, 2, 3),
    r2 = o.addTwo(1, 2);
  print("Example23 addThree(1, 2, 3) results in '${r1}'");
  print("Example23 addTwo(1, 2) results in '${r2}'");
}

// O método construtor da classe usa o mesmo nome da classe e
// é feito assim SomeClass() : super() {}, onde a parte ": super()"
// é opcional e é usada para passar parâmetros estáticos para o
// construtor da classe pai.
class Example24A {
  var _value;
  Example24A({value: "someValue"}) {
    _value = value;
  }
  get value => _value;
}
class Example24B extends Example24A {
  Example24B({value: "someOtherValue"}) : super(value: value);
}
example24() {
  var o1 = new Example24B(),
    o2 = new Example24B(value: "evenMore");
  print("Example24 calling super during constructor '${o1.value}'");
  print("Example24 calling super during constructor '${o2.value}'");
}

// Há um atalho para passar parâmetros para o construtor no caso de classes mais simples.
// Simplesmente use o prefixo this.nomeDoParametro e isso irá passar o parâmetro para uma
// instância de variável de mesmo nome.
class Example25 {
  var value, anotherValue;
  Example25({this.value, this.anotherValue});
}
example25() {
  var o = new Example25(value: "a", anotherValue: "b");
  print("Example25 shortcut for constructor '${o.value}' and "
    "'${o.anotherValue}'");
}

// Parâmetros com nome estão disponíveis quando declarados entre {}.
// Quando os parâmetros têm nomes, eles podem ser passados em qualquer ordem.
// Parâmetros declarados entre [] são opcionais.
example26() {
  var _name, _surname, _email;
  setConfig1({name, surname}) {
    _name = name;
    _surname = surname;
  }
  setConfig2(name, [surname, email]) {
    _name = name;
    _surname = surname;
    _email = email;
  }
  setConfig1(surname: "Doe", name: "John");
  print("Example26 name '${_name}', surname '${_surname}', "
    "email '${_email}'");
  setConfig2("Mary", "Jane");
  print("Example26 name '${_name}', surname '${_surname}', "
  "email '${_email}'");
}

// Variáveis declaradas com um final só podem receber valor uma vez.
// No caso de classes, variáveis final podem ter valor atribuido através
// de um parâmetro no construtor
class Example27 {
  final color1, color2;
  // Um pouco de flexibilidade ao criar variáveis final com a sintaxe
  // que é a seguinte:
  Example27({this.color1, color2}) : color2 = color2;
}
example27() {
  final color = "orange", o = new Example27(color1: "lilac", color2: "white");
  print("Example27 color is '${color}'");
  print("Example27 color is '${o.color1}' and '${o.color2}'");
}

// para importar uma biblioteca, use import "libraryPath" ou se for uma biblioteca da linguagem,
// import "dart:libraryName". Também tem o gerenciador de pacotes "pub"que tem 
// sua própria convenção de import "package:packageName".
// Veja o import "dart:collection"; no início do arquivo. Imports devem vir no início
// do arquivo. IterableBase vem de dart:collection.
class Example28 extends IterableBase {
  var names;
  Example28() {
    names = ["a", "b"];
  }
  get iterator => names.iterator;
}
example28() {
  var o = new Example28();
  o.forEach((name) => print("Example28 '${name}'"));
}

// Para controle de fluxo nós temos:
// * switch com comandos break obrigatórios
// * if-else if-else e se-ternário ..?..:.. 
// * closures e funções anônimas
// * comandos break, continue e return
example29() {
  var v = true ? 30 : 60;
  switch (v) {
    case 30:
      print("Example29 switch statement");
      break;
  }
  if (v < 30) {
  } else if (v > 30) {
  } else {
    print("Example29 if-else statement");
  }
  callItForMe(fn()) {
    return fn();
  }
  rand() {
    v = new DM.Random().nextInt(50);
    return v;
  }
  while (true) {
    print("Example29 callItForMe(rand) '${callItForMe(rand)}'");
    if (v != 30) {
      break;
    } else {
      continue;
    }
    // Nunca chega aqui.
  }
}

// Você pode converter string para int, double para int, ou só pegar a parte inteira da divisão
// usando o comando ~/. Vamos jogar um jogo de adivinhação.
example30() {
  var gn, tooHigh = false,
    n, n2 = (2.0).toInt(), top = int.parse("123") ~/ n2, bottom = 0;
  top = top ~/ 6;
  gn = new DM.Random().nextInt(top + 1); // +1 porque o máximo do nextInt conta o número passado - 1 
  print("Example30 Diga um número entre 0 e ${top}");
  guessNumber(i) {
    if (n == gn) {
      print("Example30 Você acertou! O número é ${gn}");
    } else {
      tooHigh = n > gn;
      print("Example30 O número ${n} é muito "
        "${tooHigh ? 'alto' : 'baixo'}. Tente de novo");
    }
    return n == gn;
  }
  n = (top - bottom) ~/ 2;
  while (!guessNumber(n)) {
    if (tooHigh) {
      top = n - 1;
    } else {
      bottom = n + 1;
    }
    n = bottom + ((top - bottom) ~/ 2);
  }
}

// Programas em Dart só têm um ponto de entrada, que é a função main.
// Nada será executado antes da funcão main de um programa.
// Isso ajuda a carregar o programa mais rapidamente, até mesmo quando o
// carregamento é "Lazy".
// O programa deve começar com:
main() {
  print("Aprenda Dart em 15 minutos!");
  [example1, example2, example3, example4, example5, example6, example7,
    example8, example9, example10, example11, example12, example13, example14,
    example15, example16, example17, example18, example19, example20,
    example21, example22, example23, example24, example25, example26,
    example27, example28, example29, example30
    ].forEach((ef) => ef());
}

```

## Continue lendo

Dart tem um site bastante fácil de entender. Ele tem os docs da API, tutoriais, artigos e muito mais, incluindo uma
opção muito útil de testar o Dart online.
* [https://www.dartlang.org](https://www.dartlang.org)
* [https://try.dartlang.org](https://try.dartlang.org)




