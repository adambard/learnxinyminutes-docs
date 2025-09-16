---
contributors:
    - ["Joao Pedrosa", "https://github.com/jpedrosa/"]
    - ["Hélio Oliveira", "https://github.com/insign/"]
translators:
    - ["Junior Damacena", "https://github.com/jdamacena/"]
    - ["Lays Leal Correia", "https://github.com/laysleal/"]
---

**Dart** é uma linguagem de programação de propósito geral e com uma única thread.
Ela herda muitos conceitos de outras linguagens populares.
Oferece suporte a Streams, Futures (conhecidas como Promises no JavaScript), Generics, funções de primeira classe (closures) e verificação de tipos estática.
O Dart pode ser executado em qualquer plataforma, como Web, CLI, Desktop, Mobile e dispositivos IoT.

As variáveis em Dart tem tipos, mas não é obrigatório declarar devido à função de [detecção automática](https://dart.dev/guides/language/type-system#type-inference), adicionalmente, possui um sistema de verificação para que valores sempre sejam compatíveis com as variáveis [Dart type system](https://dart.dev/language/type-system).

```dart
import "dart:collection";
import "dart:math" as DM;

// Bem vindo ao Aprenda Dart em 15 minutos. http://www.dartlang.org/
// Este é um tutorial executável. Você pode rodar esse tutorial com Dart ou no
// site Try Dart!, é só copiar e colar este código lá. http://dartpad.dev/
// É possível usar Flutter no DartPad, clique em `Create` > `Flutter snippet`.

/// Em Dart, tudo é um objeto.
/// Toda declaração de um objeto é uma instância de Null.
/// Null também é um objeto.

/// 3 Tipos de Comentários em Dart
// Comentário em uma única linha
/**
* Comentário em
* múltiplas
* linhas
*/
/// Comentário para documentação  
/// Usa a sintaxe Markdown para gerar a documentação ao criar uma API.
/// É a opção recomendada para suas APIs, classes e métodos.

/// 4 Tipos de Declaração de Variáveis
/// Constantes são variáveis imutáveis, portanto não podem ser alteradas.
/// `const` em dart é tradicionalmente declarado utilizando SCREAMING_SNAKE_CASE.
/// Exemplo:
const VALOR_CONSTANTE = "NÃO POSSO SER ALTERADO";
VALOR_CONSTANTE = "EU MUDEI?"; //Resulta em erro

/// Final é outra variável que não pode ser alterada depois de declarada.
/// Geralmente é usada para classe e funções.
/// `final` é tradicionalmente declarada em pascalCase.
final valorFinal = "valor não pode ser alterado depois de declarado";
valorFinal = "Alterado"; //Resulta em erro

/// `var` é uma variável de valor mutável.
/// Caso seu tipo não seja declarado (String, int, etc.) o Dart conseguirá
/// inferir qual é e não permitirá que esse tipo seja alterado.
var valorAlteravel = "String variável";
valorAlteravel = "esse comando é válido";
valorAlteravel = false; //Resulta em erro.

/// `dynamic` é outra variável de valor mutável, mas seu tipo não será inferido.
/// Tanto o seu valor quanto o seu tipo podem ser alterados.
/// Programadores utilizam dynamic de forma cautelosa já que nem sempre é
/// possível saber qual o seu tipo em determinada situação.
/// Utilize por sua própria conta e risco.
dynamic valorDinamico = "Eu sou uma string";
valorDinamico = false; // O comando é válido

/// Funções podem ser declaradas em um espaço global.
/// Declarações de funções e métodos são iguais.
/// Declarações de funções podem ser aninhadas.
/// A declaração é feita das seguintes formas nome() {} ou
/// nome() => expressaoDeUmaLinhaSo;
/// Utilizando a seta o return pode ser implícito ou explícito.
/// O Dart vai executar a função `main()` independente da localização dela.
///
exemplo1() {
  exemplo1aninhado1() {
    exemplo1aninhado2() => print("Exemplo1 aninhado 1 aninhado 2");
    exemplo1aninhado2();
  }
  exemplo1aninhado1();
}

/// Funções anônimas são criadas sem um nome.
exemplo2() {
  // Return explícito.
  aninhado1(void function() fn) {
    fn();
  }
  aninhado1(() => print("Exemplo2 aninhado 1"));
}

/// Quando a função possui parâmetros, a declaração pode incluir o número
/// de parâmetros que a função recebe, isso é feito especificando o nome de cada um dos 
/// parâmetros que serão recebidos pela função.
exemplo3() {
  planoA(fn(String informarAlgo)) {
    fn("Exemplo3 plano A");
  }
  planoB(fn) {
    // Ou não declare o número de parâmetros.
    fn("Exemplo3 plano B");
  }
  planoA((s) => print(s));
  planoB((s) => print(s));
}

/// Funções têm acesso à variáveis fora de seu escopo
var exemplo4 = "Example4 aninhado 1";
example4() {
  aninhado1(fn(informarAlgo)) {
    fn(example4);
  }
  aninhado1((s) => print(s));
}

/// Declaração de classe com um método chamado sayIt, que também tem acesso 
/// a variáveis externas, assim como visto acima.
var exemplo5 = "Exemplo5 sayIt";
class Exemplo5Class {
  sayIt() {
    print(exemplo5);
  }
}
exemplo5() {
  // Criar uma instância anônima de Exemplo5Class e chamar o método sayIt.
  new Exemplo5Class().sayIt();
}

/// A declaração de uma classe é feita da seguinte maneira: class name { [classBody] }.
/// onde classBody pode incluir métodos e variáveis de instância, assim como
/// métodos e variáveis de classe.
class Exemplo6Class {
  var variavelInstancia = "Exemplo6 variável instância";
  sayIt() {
    print(variavelInstancia);
  }
}
exemplo6() {
  Exemplo6Class().sayIt();
}

/// Métodos e variáveis de classe são declarados como "static".
class Exemplo7Class {
  static var classVariable = "Exemplo7 class variable";
  static sayItDoClass() {
    print(classVariable);
  }
  sayItDaInstancia() {
    print(classVariable);
  }
}
example7() {
  Exemplo7Class.sayItDoClass();
  new Example7Class().sayItFromInstance();
}

/// O Dart oferece suporte a Genéricos.  
/// Genéricos se referem à técnica de escrever o código de uma classe  
/// sem especificar o(s) tipo(s) de dados com os quais a classe irá trabalhar.  
/// Fonte: [Stack Overflow](https://stackoverflow.com/questions/4560890/what-are-generics-in-c)

/// O tipo `T` se refere a qualquer tipo que tenha sido instanciado,  
/// você pode chamá-lo como quiser.  
/// Os programadores usam a seguinte convenção:  
/// T - Type (usado para classes e tipos primitivos)  
/// E - Element (usado para List, Set ou Iterable)  
/// K,V - Key e Value (usados para Map)  

class ExemploGenerico<T>{
  void printType(){
    print("$T");
  }
  // Métodos também podem ter genéricos
  metodoGenerico<M>(){
    print("classe:$T, metodo: $M");
  }
}

/// Listas são semelhantes a arrays, mas `List` é filha de `Iterable<E>`.  
/// Portanto, Maps, List e LinkedList são todos filhos de `Iterable<E>`  
/// para que seja possível iterar usando a palavra-chave `for`.  
/// Pontos importantes a lembrar:  
/// () - Iterable<E>  
/// [] - List<E>  
/// {} - Map<K,V>

/// Listas são ótimas, mas há uma limitação para o que elas podem ser quando
/// estão fora do corpo de uma função/método. Listas fora do escopo da classe
/// ou fora da classe precisam que ser constantes. Strings e números são
/// constantes por padrão. Mas arrays e mapas não são. Eles podem ser declarados
/// como constantes usando o comando "const".
const exemplo8List = ["Exemplo8 const array"];
const exemplo8Map = {"chave": "Exemplo8 const map"};
/// Declare listas ou mapas como objetos.
 List<String> listaExplicita = new List<String>.empty();
 Map<String,dynamic> mapaExplicito = new Map<String,dynamic>();

exemplo8() {
  listaExplicita.add("listaQualquer");
  print(exemplo8Map["mapaQualquer"]);
  print(listaExplicita[0]);

  /// Atribuir uma lista de uma variável para outra não terá o mesmo resultado.  
  /// Isso acontece porque o Dart utiliza [pass-reference-by-value](https://stackoverflow.com/questions/25170094/what-is-the-true-meaning-of-pass-by-reference-in-modern-languages-like-dart).
  /// Então, quando você atribui uma lista existente a uma nova variável,  
  /// em vez de continuar sendo uma `List`, ela se torna um `Iterable`.  
  var iterableExplicitList = explicitList;
  print(iterableExplicitList); // ("SomeArray"); "[]" becomes "()"
  var newExplicitLists = explicitList.toList(); // Converts Iterable<E> to List<E>
}

/// Loops em Dart são criados com for () {}, while () {} ou for (.. in ..) {}, ou
/// funções de callbacks com muitas funcionalidades, começando com
/// forEach, map e where.
var exemplo9 = const ["a", "b"];
exemplo9() {
  for (int i = 0; i < exemplo9.length; i++) {
    print("Exemplo9 for loop '${exemplo9[i]}'");
  }
  var i = 0;
  while (i < exemplo9.length) {
    print("Exemplo9 while loop '${exemplo9[i]}'");
    i++;
  }
  for (final e in exemplo9) {
    print("Example9 for-in loop '${e}'");
  }

  exemplo9.forEach((e) => print("Exemplo9 forEach loop '${e}'"));
}

/// Para percorrer os caracteres de uma string ou extrair uma substring.
var exemplo10 = "ab";
exemplo10() {
  for (var i = 0; i < exemplo10.length; i++) {
    print("exemplo10 caracteres de String'${exemplo10[i]}'");
  }
  for (var i = 0; i < exemplo10.length; i++) {
    print("exemplo10 substring '${exemplo10.substring(i, i + 1)}'");
  }
}

/// `int`, `double` e `num` são os três formatos de número suportados.
/// `num` pode ser tanto `int` quanto `double`.
/// `int` e `double` são filhos do tipo `num`.
exemplo11() {
  var i = 1 + 320, d = 3.2 + 0.01;
  final num meuFinalNumDouble = 2.2;
  final num meuFinalNumInt = 2;
  final int meuFinalInt = 1;
  final double meuFinalDouble = 0.1;
  num meuNumDouble = 2.2;
  num meuNumInt = 2;
  int meuInt = 1;
  double meuDouble = 0; // O Dart transformará ele em 0.0;
  meuNumDouble = meuFinalInt; // valido
  meuNumDouble = meuFinalDouble; // valido
  meuNumDouble = meuFinalNumInt; // valido

  meuInt = meuNumDouble; // erro
  meuInt = meuFinalDouble; // erro
  meuInt = meuFinalNumInt; // erro
  meuInt = meuFinalNumInt as int; // valido

  meuDouble = meuFinalInt; // erro
  meuDouble = meuFinalNumInt; // erro
  meuDouble = meuFinalNumDouble; // erro
  meuDouble = meuFinalNumDouble as double; // valido

  print("Exemplo11 int ${i}");
  print("Exemplo11 double ${d}");

}

/// DateTime traz operações com data/hora.
example12() {
  var now = new DateTime.now();
  print("Exemplo12 agora '${now}'");
  now = now.add(new Duration(days: 1));
  print("Exemplo12 amanhã '${now}'");
}

/// Expressões regulares são suportadas.
exemplo13() {
  var s1 = "uma string", s2 = "uma", re = new RegExp("^s.+?g\$");
  match(s) {
    if (re.daMatch(s)) {
      print("Exemplo13 regexp dá match '${s}'");
    } else {
      print("Exemplo13 regexp não dá match '${s}'");
    }
  }
  match(s1);
  match(s2);
}

/// Expressões booleanas tem suporte a conversões implícitas e tipos dinâmicos
exemplo14() {
  var a = true;
  if (a) {
    print("verdadeiro, a é $a");
  }
  a = false;
  if (a) {
    print("verdadeiro, a é $a"); 
  } else {
    print("falso, a é $a");
  }

  /// Tipo dinâmico que assume valor nulo não pode ser convertido para booleano
  var b; // b é de tipo dinâmico
  b = "abc";
  try {
    if (b) {
      print("verdadeiro, b é $b");
    } else {
      print("falso, b é $b");
    }
  } catch (e) {
    print("erro, b é $b");
  }
  b = null; 
  if (b) { // Afirmação errônea: uma expressão boolena não pode ser nula
    print("verdadeiro, b é $b");
  } else {
    print("falso, b é $b"); 
  }
}

/// try/catch/finally e throw são usados para tratamento de exceções.
/// throw aceita qualquer objeto como parâmetro;
exemplo15() {
  try {
    try {
      throw "Erro inesperado.";
    } catch (e) {
      print("Exemplo15 tem exceção: '${e}'");
      throw e; // Re-throw
    }
  } catch (e) {
    print("Exemplo15 encontra exceção passando por re-thrown: '${e}'");
  } finally {
    print("Exemplo15 passa pelo finally");
  }
}

/// Para mais eficiência ao criar strings longas dinamicamente, use o
/// StringBuffer. Ou você pode também concatenar um array de strings.
exemplo16() {
  var sb = new StringBuffer(), a = ["a", "b", "c", "d"], e;
  for (e in a) { sb.write(e); }
  print("Exemplo16 string dinâmica criada com "
    "StringBuffer '${sb.toString()}'");
  print("Exemplo16 juntando array de string '${a.join()}'");
}

/// Strings podem ser concatenadas apenas colocando strings literais uma perto
/// da outra, sem necessidade de nenhum outra operação.
exemplo17() {
  print("Exemplo17 "
      "concatenar "
      "strings "
      "é simples assim");
}

/// Strings podem ser delimitadas por apóstrofos ou aspas e não há
/// diferença entre os dois. Essa flexibilidade pode ser boa para
/// evitar a necessidade de adaptar conteúdos que contenham um um caractere que
/// é delimitador (Ex.: algumas palavras em inglês).
/// Por exemplo, aspas dos atributos HTMLse a string conter HTML.
exemplo18() {
  print('Exemplo18 <a href="etc">'
      "Don't can't I'm Etc"
      '</a>');
}

/// Strings com três apóstrofos ou aspas podem
/// ter muitas linhas e incluem os delimitadores de linha (ou seja, os enter).
example19() {
  print('''Exemplo19 <a href="etc">
Exemplo19 Don't can't I'm Etc
Exemplo19 </a>''');
}

/// Strings têm a função de interpolação que é chamada com o caractere $.
/// Utilizando ${[expression]}, o retorno da expressão é interpolado.
/// $ seguido pelo nome de uma variável interpola o conteúdo dessa variável.
/// $ pode ser escapedo assim \$.
exemplo20() {
  var s1 = "'\${s}'", s2 = "'\$s'";
  print("Exemplo20 \$ interpolação: ${s1} e $s2 funcionam da mesma forma.");
}

/// A tipagem opcional permite que APIs usem anotações e também ajuda as
/// IDEs na hora das refatorações, auto-complete e checagem de
/// erros. Note que até agora não declaramos nenhum tipo e o programa está
/// funcionando normalmente. De fato, os tipos são ignorados em tempo de execução.
/// Os tipos podem até mesmo estar errados e o programa ainda vai dar o
/// benefício da dúvida e compilar, visto que os tipos não importam.
/// Existe um parâmetro que checa erros de tipagem que é o
/// checked mode, é útil enquanto se está desenvolvendo,
/// mas também é mais lento devido às checagens extras e por isso
/// é evitado em ambiente de produção.
class Exemplo21 {
  List<String> _nomes;
  Exemplo21() {
    _nomes = ["a", "b"];
  }
  List<String> get nomes => _nomes;
  set nomes(List<String> list) {
    _nomes = list;
  }
  int get tamanho => _nomes.length;
  void add(String nome) {
    _nomes.add(nome);
  }
}
void exemplo21() {
  Exemplo21 o = new Example21();
  o.add("c");
  print("Exemplo21 nomes '${o.names}' and tamanho '${o.tamanho}'");
  o.names = ["d", "e"];
  print("Exemplo21 nomes '${o.names}' and tamanho '${o.tamanho}'");
}

// Herança em classes é feito assim: class name extends NomeDeOutraClasse {}.
class Exemplo22A {
  var _nome = "Um Nome!";
  get nome => _nome;
}
class Exemplo22B extends Exemplo22A {}
exemplo22() {
  var o = new Exemplo22B();
  print("Exemplo22 herança da classe '${o.nome}'");
}

/// Mistura de classes também é possível, e é feito assim:
/// class name extends UmaClasse with NomeDeOutraClasse {}
/// É necessário extender uma classe para poder misturar com outra.
/// No momento, classes misturadas não podem ter construtor.
/// Mistura de classes é mais usado para compartilhar métodos com classes distantes, então
/// a herança comum não fica no caminho do reuso de código.
/// As misturas aparecem após o comando "with" na declaração da classe.
class Exemplo23A {}
class Exemplo23Utils {
  addTwo(n1, n2) {
    return n1 + n2;
  }
}
class Exemplo23B extends Exemplo23A with Exemplo23Utils {
  addTres(n1, n2, n3) {
    return addDois(n1, n2) + n3;
  }
}
exemplo23() {
  var o = new Exemplo23B(), r1 = o.addTres(1, 2, 3),
    r2 = o.addDois(1, 2);
  print("Exemplo23 addTres(1, 2, 3) resultado: '${r1}'");
  print("Exemplo23 addDois(1, 2) resultado: '${r2}'");
}

/// O método construtor da classe usa o mesmo nome da classe e
/// é feito assim: UmaClasse() : super() {}, onde a parte ": super()"
/// é opcional e é usada para passar parâmetros estáticos para o
/// construtor da classe pai.
class Exemplo24A {
  var _valor;
  Exemplo24A({valor: "umValor"}) {
    _valor = valor;
  }
  get valor => _valor;
}
class Exemplo24B extends Exemplo24A {
  Exemplo24B({value: "umOutroValor"}) : super(value: valor);
}
exemplo24() {
  var o1 = new Exemplo24B(),
    o2 = new Exemplo24B(value: "aindaMais");
  print("Exemplo24 puxando o super do construtor '${o1.value}'");
  print("Exemplo24 puxando o super do construtor '${o2.value}'");
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




