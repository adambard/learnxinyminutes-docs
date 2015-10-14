---
language: dart
filename: learndart.dart
contributors:
    - ["Joao Pedrosa", "https://github.com/jpedrosa/"]
translators:
    - ["Rodrigo Muniz", "http://github.com/muniz95"]
---

Dart é recém chegada ao domínio das linguagens de programação.
Ela empresta muito de outras linguagens mais conhecidas, tendo como meta não desviar muito
de seu irmão JavaScript. Assim como JavaScript, Dart busca uma boa integração ao browser.

A funcionalidade mais controversa do Dart deve ser sua tipagem opcional.

```javascript
import "dart:collection";
import "dart:math" as DM;

// Bem vindo ao Aprenda Dart em 15 minutos. http://www.dartlang.org/
// Este é um tutorial executável. Você pode rodá-lo com Dart ou no
// site Try Dart! se você copiar/colar ele lá. http://try.dartlang.org/

// Declaração de função e declaração de método parecem a mesma coisa. Declarações de
// função podem ser aninhadas. A declaração toma a forma de
// nome() {} or nome() => expressaoDeUmaLinha;
// A declaração com sinal de igual-maior tem o retorno implícito para o resultado
// da expressão.
exemplo1() {
  exemplo1aninhado1() {
    exemplo1aninhado2() => print("Exemplo1: aninhado 1 aninhado 2");
    exemplo1aninhado2();
  }
  exemplo1aninhado1();
}

// Funções anônimas não incluem um nome.
exemplo2() {
  exemplo2aninhado1(fn) {
    fn();
  }
  exemplo2aninhado1(() => print("Exemplo2: aninhado 1"));
}

// Quando um parâmetro da função é declarado, a declaração pode incluir o
// número de parâmetros que a função recebe especificando os nomes dessese
// parâmetros.
exemplo3() {
  exemplo3aninhado1(fn(informarAlgo)) {
    fn("Exemplo3: aninhado 1");
  }
  exemplo3planB(fn) { // Ou não declare o número of parâmetros.
    fn("Exemplo3: plan B");
  }
  exemplo3aninhado1((s) => print(s));
  exemplo3planB((s) => print(s));
}

// Funções tem accesso à variáveis externas.
var exemplo4Algo = "Exemplo4: aninhado 1";
exemplo4() {
  exemplo4aninhado1(fn(informarAlgo)) {
    fn(exemplo4Algo);
  }
  exemplo4aninhado1((s) => print(s));
}

// Declaração de classe com o método faleAi, que também tem acesso a uma
// variável externa como se fosse uma função, como visto antes.
var exemplo5method = "Exemplo5: faleAi";
class Exemplo5Class {
  faleAi() {
    print(exemplo5method);
  }
}
exemplo5() {
  // Cria uma instância anõnima do Exemplo5Class: e chama o método faleAi
  // nesta instância.
  new Exemplo5Class().faleAi();
}

// Declaração de classe toma a forma do nome da classe { [classBody] }.
// Onde classBody pode incluir métodos de instância e variáveis, bem como
// métodos de classe e variáveis.
class Exemplo6Class {
  var exemplo6InstanceVariable = "Exemplo6: variável de instância"; 
  faleAi() {
    print(exemplo6InstanceVariable);
  }
}
exemplo6() {
  new Exemplo6Class().faleAi();
}

// Métodos de classe e variáveis são declarados com a palavra "static".
class Exemplo7Class {
  static var exemplo7ClassVariable = "Exemplo7: variável de classe"; 
  static faleAiFromClass() {
    print(exemplo7ClassVariable);
  }
  faleAiFromInstance() {
    print(exemplo7ClassVariable);
  }
}
exemplo7() {
  Exemplo7Class.faleAiFromClass();
  new Exemplo7Class().faleAiFromInstance();
}

// Literais são ótimas, mas há uma restrição para o que literais podem ser
// fora do corpo da função/método. Literais fora do escopo da classe
// ou fora da classe devem ser constantes. Strings e números são constantes
// por padrão. Porém arrays e maps não são. Podem se tornar constantes ao
// declará-las como "const".
var exemplo8A = const ["Exemplo8: array"],
    exemplo8M = const {"algumaChave": "Exemplo8: map"}; 
exemplo8() {
  print(exemplo8A[0]);
  print(exemplo8M["algumaChave"]);
}

// Loops em Dart tomam a forma do padrão de: for () {} ou while () {},
// padrão alternativo de for (.. in ..) {}, ou callbacks funcionais com muitas
// funcionalidades suportadas, começando com forEach.
var exemplo9A = const ["a", "b"];
exemplo9() {
  for (var i = 0; i < exemplo9A.length; i++) {
    print("Exemplo9: for '${exemplo9A[i]}'");
  }
  var i = 0;
  while (i < exemplo9A.length) {
    print("Exemplo9: while '${exemplo9A[i]}'");
    i++;
  }
  for (var e in exemplo9A) {
    print("Exemplo9: for-in '${e}'");
  }
  exemplo9A.forEach((e) => print("Exemplo9: forEach '${e}'"));
}

// Para iterar sobre os caracteres de uma string ou extrair uma substring.
var exemplo10S = "abcdefghij";
exemplo10() {
  for (var i = 0; i < exemplo10S.length; i++) {
    print("Exemplo10: Loop em uma string '${exemplo10S[i]}'");
  }
  for (var i = 0; i < exemplo10S.length; i++) {
    print("Exemplo10: Loop em uma substring '${exemplo10S.substring(i, i + 1)}'");
  }
}

// Int e double são os dois formatos de número suportados.
exemplo11() {
  var i = 1 + 320, d = 3.2 + 0.01;
  print("Exemplo11: int ${i}");
  print("Exemplo11: double ${d}");
}

// DateTime fornece aritimética de data/hora.
exemplo12() {
  var now = new DateTime.now();
  print("Exemplo12: agora '${now}'");
  now = now.add(new Duration(days: 1));
  print("Exemplo12: amanhã '${now}'");
}

// Expressões regulares são suportadas.
exemplo13() {
  var s1 = "Alguma string", s2 = "alguma", re = new RegExp("^s.+?g\$");
  match(s) {
    if (re.hasMatch(s)) {
      print("Exemplo13: regex bate com '${s}'");
    } else {
      print("Exemplo13: regex não bate com '${s}'");
    }
  }
  match(s1);
  match(s2);
}

// Expressões booleanas precisam resultar em true ou false, já que
// conversões implícitas não são suportadas.
exemplo14() {
  var v = true;
  if (v) {
    print("Exemplo14: valor é true");
  }
  v = null;
  try {
    if (v) {
      // Nunca executa
    } else {
      // Nunca executa
    }
  } catch (e) {
    print("Exemplo14: valor nulo causou uma exceção: '${e}'");
  }
}

// try/catch/finally e throw são usados para tratamento de exceções.
// throw leva um objeto como parâmetro;
exemplo15() {
  try {
    try {
      throw "Erro inesperado.";
    } catch (e) {
      print("Exemplo15: exceção: '${e}'");
      throw e; // Re-throw
    }
  } catch (e) {
    print("Exemplo15: exceção sendo relançada: '${e}'");
  } finally {
    print("Exemplo15: execução do bloco finally");
  }
}

// Para ser eficiente ao criar uma string longa dinamicamente, use
// StringBuffer. Or você pode juntar um array de string.
exemplo16() {
  var sb = new StringBuffer(), a = ["a", "b", "c", "d"], e;
  for (e in a) { sb.write(e); }
  print("Exemplo16: string dinâmica criada com "
    "StringBuffer '${sb.toString()}'");
  print("Exemplo16: junção de array de string '${a.join()}'");
}

// Strings podem ser concatenadas apenas por haver strings literais próximas a
// alguma outra sem que seja necessário nenhum operador.
exemplo17() {
  print("Exemplo17: "
      "concatenação "
      "de strings "
      "simples");
}

// Strings usam aspas simples ou duplas como delimitadores sem nenhuma
// diferença de fato entre as duas. Essa flexibilidade pode ser boa
// para evitar a necessidade de escapar conteúdo que corresponda ao delimitador
// usado. Por exemplo, aspas duplas de atributos HTML se a string contém
// código HTML.
exemplo18() {
  print('Exemplo18: <a href="etc">'
      "Olho d'água em São João d'Oeste"
      '</a>');
}

// Strings com aspas duplas ou simples triplas separam
// múltiplas linhas e incluem delimitadores.
exemplo19() {
  print('''Exemplo19: <a href="etc"> 
Exemplo19 Olho d'água em São João d'Oeste
Exemplo19 </a>''');
}

// Strings tem a funcionalidade de interpolação com o caractere $.
// Com $ { [expressão] }, o retorno da expressão é interpolado.
// $ seguido do nome de uma variável interpola o conteúdo desta variável.
// $ pode ser escapado assim \$ para que seja impresso o caractere em si, ao invés de uma expressão.
exemplo20() {
  var s1 = "'\${s}'", s2 = "'\$s'";
  print("Exemplo20: \$ interpolação em ${s1} ou $s2 funciona.");
}

// Tipos adicionais permitem a anotação de APIs e ajudam as 
// IDE's a melhorar a refatoração, auto-complemento e checagem de
// erros. Por tempos não declaramos nenhum tipo e os programas
// funcionavam corretamente. De fato, tipos são desconsiderados em tempo de execução.
// Tipos podem até mesmo estar errados e ao programa ainda será dado o
// benefício da dúvida e ser rodado mesmo que os tipos não tenham importância.
// Há um parâmetro em tempo de execução que checa por erros de tipo 
// que é o checked mode, o qual é dito ser útil durante o desenvolvimento,
// mas ao mesmo tempo lento devido à checagem extra e é, desta maneira
// evitado durante a implantação.
class Exemplo21 {
  List<String> _names;
  Exemplo21() {
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
void exemplo21() {
  Exemplo21 o = new Exemplo21();
  o.add("c");
  print("Exemplo21: nomes '${o.names}' e tamanho '${o.length}'");
  o.names = ["d", "e"];
  print("Exemplo21: nomes '${o.names}' e tamanho '${o.length}'");
}

// Herança de classes toma a forma de: class name extends AnotherClassName {}.
class Exemplo22A {
  var _name = "Algum Nome!";
  get name => _name;
}
class Exemplo22B extends Exemplo22A {}
exemplo22() {
  var o = new Exemplo22B();
  print("Exemplo22: herança da classe '${o.name}'");
}

// Mixin de classes também está disponível, e toma a forma de:
// class name extends SomeClass with AnotherClassName {}.
// É necessário herdar de alguma classe para poder efetuar mixin de outra.
// A classe modelo do mixin atualmente não pode ter um construtor.
// Mixin é, em sua maioria, usado para compartilhar métodos com classes distantes, assim
// a herança simples não impede a escrita de código reutilizável.
// Mixins seguem a declaração "with" durante a declaração de classe.
class Exemplo23A {}
class Exemplo23Utils {
  addTwo(n1, n2) {
    return n1 + n2;
  }
}
class Exemplo23B extends Exemplo23A with Exemplo23Utils {
  addThree(n1, n2, n3) {
    return addTwo(n1, n2) + n3;
  }
}
exemplo23() {
  var o = new Exemplo23B(), r1 = o.addThree(1, 2, 3),
    r2 = o.addTwo(1, 2);
  print("Exemplo23: addThree(1, 2, 3) resulta em '${r1}'");
  print("Exemplo23: addTwo(1, 2) resulta em '${r2}'");
}

// O construtor da classe usa o mesmo nome da classe e
// toma a forma de: SomeClass() : super() {}, onde a parte ": super()"
// é opcional e é usado para delegar parâmetros constantes ao
// construtor da classe mãe.
class Exemplo24A {
  var _value;
  Exemplo24A({value: "algumValor"}) {
    _value = value;
  }
  get value => _value;
}
class Exemplo24B extends Exemplo24A {
  Exemplo24B({value: "algumOutroValor"}) : super(value: value);
}
exemplo24() {
  var o1 = new Exemplo24B(),
    o2 = new Exemplo24B(value: "maisAinda");
  print("Exemplo24: chamando super no construtor de '${o1.value}'");
  print("Exemplo24: chamando super no construtor de '${o2.value}'");
}

// Há um atalho para definir os parâmetros do construtor em classes mais simples.
// Apenas use o prefixo this.parameterName e isso definirá o valor do parâmetro em
// uma variável de instância de mesmo nome.
class Exemplo25 {
  var value, anotherValue;
  Exemplo25({this.value, this.anotherValue});
}
exemplo25() {
  var o = new Exemplo25(value: "a", anotherValue: "b");
  print("Exemplo25: atalho para o construtor '${o.value}' e "
    "'${o.anotherValue}'");
}

// Parâmetros nomeados estão disponíveis quando declarados entre {}.
// A ordem dos parâmetros pode ser opcional quando declarada entre {}.
// Parâmetros podem ser opcionais quando declarados entre [].
exemplo26() {
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
  print("Exemplo26: nome '${_name}', sobrenome '${_surname}', "
    "email '${_email}'");
  setConfig2("Mary", "Jane");
  print("Exemplo26: nome '${_name}', sobrenome '${_surname}', "
  "email '${_email}'");
}

// Variáveis declaradas com a expressão "final" podem ser definidas apenas uma vez.
// No caso das classes, variáveis de instância final podem ser definidas via parâmetro de
// construtor constante.
class Exemplo27 {
  final color1, color2;
  // Uma pequena flexibilidade para definir uma variável de instância final com a sintaxe
  // que segue o :
  Exemplo27({this.color1, color2}) : color2 = color2;
}
exemplo27() {
  final color = "laranja", o = new Exemplo27(color1: "lilás", color2: "branco");
  print("Exemplo27: a cor é '${color}'");
  print("Exemplo27: a cor é '${o.color1}' e '${o.color2}'");
}

// Para importar uma biblioteca, use import "caminho/da/lib" ou se for uma biblioteca do core do Dart,
// import "dart:nomeDaLib". Há também o gerenciador de pacotes "pub" com
// sua própria convenção de importação "package:packageName".
// Veja import "dart:collection"; no topo. Imports devem vir antes de outras
// declarações de código. IterableBase vem de dart:collection.
class Exemplo28 extends IterableBase {
  var names;
  Exemplo28() {
    names = ["a", "b"];
  }
  get iterator => names.iterator;
}
exemplo28() {
  var o = new Exemplo28();
  o.forEach((name) => print("Exemplo28: '${name}'"));
}

// Para controle de fluxo temos:
// * switch padrão com break obrigatório
// * if-else if-else e operador ternário ..?..:.. 
// * closures e funções anônimas
// * break, continue e return
exemplo29() {
  var v = true ? 30 : 60;
  switch (v) {
    case 30:
      print("Exemplo29: switch");
      break;
  }
  if (v < 30) {
  } else if (v > 30) {
  } else {
    print("Exemplo29: if-else");
  }
  callItForMe(fn()) {
    return fn();
  }
  re() {
    v = new DM.Random().nextInt(50);
    return v;
  }
  while (true) {
    print("Exemplo29: callItForMe(re) '${callItForMe(re)}'");
    if (v != 30) {
      break;
    } else {
      continue;
    }
    // Never gets here.
  }
}

// Analise um int, converta double para int, ou apenas mantenha um int ao dividir números
// usando a operação ~/ . Jogaremos um jogo de adivinhação também.
exemplo30() {
  var gn, tooHigh = false,
    n, n2 = (2.0).toInt(), top = int.parse("123") ~/ n2, bottom = 0;
  top = top ~/ 6;
  gn = new DM.Random().nextInt(top + 1); // +1 porque nextInt top é exclusivo
  print("Exemplo30: Pense num número entre 0 e ${top}");
  guessNumber(i) {
    if (n == gn) {
      print("Exemplo30: Acertou! O número é ${gn}");
    } else {
      tooHigh = n > gn;
      print("Exemplo30: o número ${n} é "
        "${tooHigh ? 'maior' : 'menor'}. Tente novamente");
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

// Programas tem apenas um ponto de entrada na função principal.
// Não se espera nada para ser executado no escopo externo antes de 
// um programa rodar com o que estiver em sua função principal.
// Isso ajuda num carregamento mais rápido e até mesmo em um lazy loading apenas do que
// o programa precisa para iniciar.
main() {
  print("Aprenda Dart em 15 minutos!");
  [exemplo1, exemplo2, exemplo3, exemplo4, exemplo5, exemplo6, exemplo7,
    exemplo8, exemplo9, exemplo10, exemplo11, exemplo12, exemplo13, exemplo14,
    exemplo15, exemplo16, exemplo17, exemplo18, exemplo19, exemplo20,
    exemplo21, exemplo22, exemplo23, exemplo24, exemplo25, exemplo26,
    exemplo27, exemplo28, exemplo29, exemplo30
    ].forEach((ef) => ef());
}

```

## Leitura Adicional

Dart tem um site compreensivo. Ele cobre a referência da API, tutoriais, artigos e mais, incluindo uma
ferramenta de REPL online.
http://www.dartlang.org/
http://try.dartlang.org/



