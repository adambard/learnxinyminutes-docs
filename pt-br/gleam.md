---
name: Gleam
contributors:
    - ["Antonio Ognio", "https://github.com/aognio/"]
filename: learngleam.gleam
---

Gleam é uma nova linguagem para a máquina virtual BEAM de Erlang que depende do
poder de um sistema de tipos robusto, a expressividade da programação funcional,
e o tempo de execução Erlang altamente simultâneo e tolerante a falhas usando
sintaxe moderna inspirada em linguagens como OCaml, Rust e Elixir.

Sendo um desenvolvimento bastante moderno, o Gleam vem com um compilador, uma ferramenta de construção,
um formatador de código, várias integrações de editores e um gerenciador de pacotes.

Fazendo parte do ecossistema BEAM mais amplo, os programas criados com Gleam
também podem aproveitar milhares de pacotes publicados escritos em Erlang ou Elixir.

O design da linguagem é muito conciso, por isso não apresenta valores nulos,
sem exceções, mensagens de erro claras e um sistema de tipos prático.

JavaScript também é suportado como alvo de compilação, então você pode executar o Gleam
código no navegador ou qualquer outro tempo de execução habilitado para JS. Ao usar esse recurso,
As definições TypeScript são criadas para que você possa interagir com seu código Gleam
com confiança, mesmo de fora.

Para executar este código, primeiro crie um projeto Gleam:

```sh
gleam new my_project
cd my_project
```

Substitua o código gerado por este exemplo e execute:

```sh
gleam run
```


```gleam
//// Este comentário com quatro barras é de nível de módulo.
//// Este tipo de comentários são usados para descrever todo o módulo.

import gleam/bool
import gleam/io
import gleam/int
import gleam/float
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/dict

// O nome de um tipo sempre começa com letra maiúscula, contrastando com variáveis
// e funções, que começam com letra minúscula.

// Quando a palavra-chave pub é usada, o alias do tipo é público e pode ser consultado
// por outros módulos.

pub type UserId =
  Int

pub fn main() {
  io.println("Hello from learnxinmyminutes.com!")
  // io.println("Esta declaração foi comentada por um comentário de duas barras.!")

  // Módulos são as unidades nas quais todo o código Gleam é organizado.
  //Em um módulo você encontrará várias definições de tipos, funções, etc.
  // que parecem pertencer um ao outro.
  // Por exemplo, o módulo gleam/io contém uma variedade de funções para
  // impressão, como println.

  // Todo código Gleam está em algum módulo ou outro, cujo nome vem do nome
  // do arquivo em que está.
  // Por exemplo, gleam/io está em um arquivo chamado io.gleam em um diretório
  // chamado gleam.

  // Gleam possui um sistema robusto de tipos estáticos que ajuda você enquanto você escreve e edita
  // código, detectando erros e mostrando onde fazer alterações.
  // io.println(10)
  // Se você descomentar a linha anterior, receberá um erro de tempo de compilação relatado
  // já que a função io.println só funciona com strings, não com ints.

  // A compilação irá gerar um erro parecido com este:
  // error: Type mismatch
  //  ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:21:14
  //  │
  // 21 │   io.println(10)
  //  │              ^^
  //
  // Expected type:
  //
  //     String
  //
  // Found type:
  //
  //     Int

  // Trabalhando com números

  // Ao executar na máquina virtual Erlang, os ints não têm máximo e mínimo
  // tamanho.
  // Ao executar em tempos de execução JavaScript, os ints são representados usando JavaScript's
  // Números de ponto flutuante de 64 bits.

  // Podemos usar `echo` para depurar a impressão de um valor.

  // Int arithmetic
  echo 1 + 1
  echo 5 - 1
  echo 5 / 2
  echo 3 * 3
  echo 5 % 2

  // Int comparisons using variables to prevent compiler static analysis warnings
  let two = 2
  let one = 1
  let same_one = 1
  let same_two = 2
  let _ = echo two > one
  let _ = echo two < one
  let _ = echo two >= one
  let _ = echo two <= one

  // A igualdade funciona para qualquer tipo e é verificada estruturalmente, o que significa que dois
  // os valores são iguais se tiverem a mesma estrutura e não se estiverem no mesmo nível.
  // o mesmo local de memória.
  let _ = echo one == same_one
  // True
  let _ = echo two != same_two
  // False

  //Funções int da biblioteca padrão
  echo int.min(142, 137)
  // 137
  echo int.clamp(-80, min: 0, max: 100) // clamp(value, min, max) especifica o valor que deve cair em determinado intervalo
  // 0
  let _ = echo int.base_parse("10", 2) // analisa a string binária como um número inteiro de base 2. 
  // Ok(2) : 10 in binary equals to 2 in integer

  // Literais Int binários, octais e hexadecimais
  echo 0b00001111
  echo 0o17
  echo 0xF

  // Use sublinhados para melhorar a legibilidade de números inteiros
  echo 1_000_000

  // Os operadores numéricos do Gleam não estão sobrecarregados, portanto existem
  // operadores para trabalhar com carros alegóricos.

  // Float arithmetic
  echo 1.0 +. 1.5
  echo 5.0 -. 1.5
  echo 5.0 /. 2.5
  echo 3.0 *. 3.5

  // Float comparisons using variables to prevent compiler static analysis warnings
  let two_point_two = 2.2
  let one_point_three = 1.3
  let _ = echo two_point_two >. one_point_three
  let _ = echo two_point_two <. one_point_three
  let _ = echo two_point_two >=. one_point_three
  let _ = echo two_point_two <=. one_point_three

  // Os floats são representados como números de ponto flutuante de 64 bits no Erlang
  // e tempos de execução JavaScript.
  // O comportamento do ponto flutuante é nativo de seus respectivos tempos de execução, portanto
  // seu comportamento exato será ligeiramente diferente nos dois tempos de execução.

  // No tempo de execução do JavaScript, excedendo o máximo (ou mínimo)
  // valor representável para um valor de ponto flutuante resultará em Infinito
  // (ou -Infinito). Se você tentar dividir dois infinitos, obterá NaN
  // como resultado.

  // Ao executar no BEAM, qualquer estouro gerará um erro. Então não há
  // Valor flutuante NaN ou Infinity no tempo de execução Erlang.

  // A divisão por zero não é um erro e é definida como zero
  echo 3.14 /. 0.0
  // 0.0

  // Funções flutuantes da biblioteca padrão
  echo float.max(2.0, 9.5)
  // 9.5
  echo float.ceiling(5.4)
  // 6.0

  // Sublinhados para carros flutuantes também são suportados
  echo 10_000.01

  // Trabalhando com strings
  echo "⭐ Gleam ⭐ - 별"
  echo "this
    is
    a
    multi
    line
    string"

  echo "\u{1F600}"
  // Produz um smiley 😀

  // Aspas duplas podem ser escapadas
  io.println("\"X\" marks the spot")

  // String concatenation
  echo "One " <> "Two"

  // String functions
  echo string.reverse("1 2 3 4 5")
  echo "abc" <> "def"

  io.println(string.reverse("!desrever tog gnirts sihT"))
  // Saídas "Esta string foi invertida!"

  // Várias sequências de escape são suportadas:

  // \" - aspas duplas
  // \\ - barra invertida
  // \f - feed de formulário
  // \n - nova linha
  // \r - retorno de carro
  // \t - aba

  // Bool operators
  // O || e os operadores && funcionam por curto-circuito

  echo True && False
  // False

  echo True && True
  // True

  echo False || False
  // False

  echo False || True
  // True

  // Bool functions
  echo bool.to_string(True)
  // "True"

  // Atribuições
  let x = "Original value"
  echo x

  // Atribuir `y` ao valor de `x`
  let y = x
  echo y

  // Atribuir `x` a um novo valor
  let x = "New value"
  echo x

  // O `y` ainda se refere ao valor original
  echo y

  // No Gleam, os nomes de variáveis e funções são escritos em snake_case.
  let answer_to_the_universe = 42
  echo answer_to_the_universe

  let and_everything = answer_to_the_universe
  // Agora, usar uma variável produz um aviso

  // warning: Unused variable
  //     ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:199:7
  //     │
  // 199 │   let and_everything = answer_to_the_universe
  //     │       ^^^^^^^^^^^^^^ This variable is never used
  // Hint: You can ignore it with an underscore: `_and_everything`.

  // Digite anotações

  let _name: String = "Gleam"

  let _is_cool: Bool = True

  let _version: Int = 1
  // Úteis para fins de documentação, mas não alteram a forma como o compilador
  // verifica o código além de garantir que a anotação corresponda ao tipo;
  // caso contrário, você receberá um erro.

  // let _has_wrong_type_annotation: Int = True

  //  error: Type mismatch
  //      ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:219:41
  //      │
  //  219 │   let _has_wrong_type_annotation: Int = True
  //      │                                         ^^^^
  //
  //  Expected type:
  //
  //      Int
  //
  //  Found type:
  //
  //      Bool

  // Aliases de tipo
  let one: UserId = 1
  // Consulte o início do arquivo para a definição do tipo UserId

  let two: Int = 2

  // Aliases servem apenas para criar código mais legível e mais preciso
  //documentação.
  // Nos bastidores, eles ainda são valores do mesmo tipo, então as operações
  // ainda trabalho
  echo one + two
  // 3

  // Blocos: escopo e valor
  let radius = {
    let value = 100.0
    value
  }
  // echo value // <- Isto não será compilado porque "value" está fora do escopo

  let area = 3.14159 *. radius *. radius
  echo area

  // Use blocos para agrupar operações em vez de parênteses
  let n1 = { 3 + 2 } * 5
  let n2 = 3 + { 2 * 5 }
  echo n1 != n2
  // True

  // Listas

  // Sobrinhos do Tio Patinhas
  let nephews = ["Huey", "Dewey", "Louie"]
  echo nephews
  // ["Huey", "Dewey", "Louie"]

  // Anexar imutavelmente para que a lista original não seja alterada
  echo ["Donald", ..nephews]
  // ["Donald", "Huey", "Dewey", "Louie"]

  // Algumas funções de biblioteca padrão para listas

  list.each(nephews, io.println)
  // Huey
  // Dewey
  // Louie

  echo list.drop(nephews, 2)
  // ["Louie"]

  more_examples()
  more_function_examples()
  generic_typing_examples()
  beloved_pipelines_demo()
  labels_in_function_calls()
  showcase_flow_control()
  more_on_recursion()
  more_on_pattern_matching()
  showcase_types()
  more_on_types()
  more_on_callbacks()
  showcase_externals()
  showcase_constants()
  showcase_dicts()
  showcase_bit_arrays()
  showcase_opaque_types()
  showcase_panic()
}

// A palavra-chave fn é usada para definir novas funções.
fn multiply(a: Int, b: Int) -> Int {
  // Sem retorno explícito
  // A última expressão é retornada
  a * b
}

// As funções dupla e multiplicada são definidas sem a palavra-chave pub.
// Isso as torna funções privadas, elas só podem ser usadas dentro deste módulo.
// Se outro módulo tentasse usá-los, isso resultaria em um erro do compilador.
fn double(a: Int) -> Int {
  multiply(a, 2)
}

// Somente funções públicas são exportadas e podem ser chamadas de fora do módulo.

// As anotações de tipo são opcionais para argumentos de função e valores de retorno
// mas são consideradas boas práticas para maior clareza e para encorajar
// design intencional e pensativo.

pub fn is_leap_year(year: Int) -> Bool {
  { year % 4 == 0 } && { { year % 100 != 0 } || { year % 400 == 0 } }
}

fn more_examples() {
  //Debug também retorna um valor, então sua saída é o valor de retorno de
  // esta função
  echo double(10)
  // 20
  echo is_leap_year(2000)
  // True
}

// Gleam oferece suporte a funções de ordem superior:
// Eles podem ser atribuídos a variáveis, passados como argumentos para outras funções
// ou até mesmo ser retornados como valores de blocos ou outras funções
fn call_func_on_int(func: fn(Int) -> Int, value: Int) -> Int {
  func(value)
}

fn more_function_examples() -> Int {
  echo call_func_on_int(double, 2)
  // 4

  let square = fn(x: Int) -> Int { x * x }
  echo square(3)
  // 9

  // Chamar uma função anônima imediatamente após defini-la
  echo fn(x: Int) { x + 1 }(1)

  // Exemplo de fechamento
  let make_adder = fn(n: Int) -> fn(Int) -> Int {
    fn(argument: Int) -> Int { argument + n }
  }

  let adder_of_fives = make_adder(5)
  echo adder_of_fives(10)
  // 15

  // Funções anônimas podem ser usadas de forma intercambiável com funções nomeadas.
  echo call_func_on_int(fn(x: Int) -> Int { x + 100 }, 900)
  // 1000

  // Vamos criar um decorador de função
  let twice = fn(wrapped_func: fn(Int) -> Int) -> fn(Int) -> Int {
    fn(argument: Int) -> Int { wrapped_func(wrapped_func(argument)) }
  }
  let quadruple = twice(double)
  echo quadruple(1)

  let quadruple_2 = fn(a: Int) -> Int { multiply(4, a) }
  echo quadruple_2(2)
  // 8

  // Uma captura de função é uma sintaxe abreviada para criar funções anônimas
  // que recebe um argumento e imediatamente chama outra função com esse
  // argumento
  let quadruple_3 = multiply(4, _)
  echo quadruple_3(4)
  // 16
}

// Funções genéricas são suportadas usando variáveis de tipo.
fn generic_twice(func: fn(value) -> value, argument: value) -> value {
  func(func(argument))
}

// Em generic_twice o valor era a variável de tipo.
// Em generic_twice_decorator the_type é a variável de tipo.
// Como em qualquer outra variável você escolhe o nome.
fn generic_twice_decorator(
  func: fn(the_type) -> the_type,
) -> fn(the_type) -> the_type {
  fn(argument: the_type) -> the_type { func(func(argument)) }
}

fn generic_typing_examples() {
  let double_integers = fn(a: Int) -> Int { a * 2 }
  let double_floats = fn(a: Float) -> Float { a *. 2.0 }
  echo generic_twice(double_integers, 3)
  echo generic_twice(double_floats, 3.0)

  let quadruple_integers = generic_twice_decorator(double_integers)
  let quadruple_floats = generic_twice_decorator(double_floats)
  echo quadruple_integers(1)
  // 4
  echo quadruple_floats(1.0)
  // 4.0
}

// O operador pipe do Gleam |> pega o resultado da expressão à sua esquerda
// e passa-o como argumento para a função à sua direita.
fn beloved_pipelines_demo() {
  // Sejamos honestos: você quer usar o Gleam apenas para esse operador legal, certo?
  ["hello", "world"]
  |> list.intersperse(" ")
  |> list.append(["!"])
  |> string.concat
  |> string.capitalise
  |> echo

  // Muito mais limpo do que isso, certo?
  echo 
    string.capitalise(
      string.concat(
        list.append(list.intersperse(["hello", "world"], " "), ["!"]),
      ),
    )

  // Solução para o primeiro problema do Projeto Euler:
  // URL: https://projecteuler.net/problem=1
  // Description: Find the sum of all the multiples of 3 and 5 below 1000.
  // Usando int.range de gleam/int para iterar de forma recursiva.
  int.range(from: 1, to: 999, with: 0, run: fn(acc, n) {
    case n % 3 == 0 || n % 5 == 0 {
      True -> acc + n
      False -> acc
    }
  })
  |> int.to_string
  |> fn(sum_as_text: String) {
    "Solution to Project Euler's problem #1: " <> sum_as_text
  }
  |> echo
  // Solution to Project Euler's problem #1: 233168
}

// Rótulos podem ser adicionados antes de cada argumento
fn call_func_on_int_with_labels(
  func passed_func: fn(Int) -> Int,
  value n: Int,
) -> Int {
  passed_func(n)
}

// O rótulo e o argumento podem ter o mesmo nome
fn add_one(number number: Int) -> Int {
  number + 1
}

fn add_two_integers(first n: Int, second m: Int) -> Int {
  n + m
}

fn labels_in_function_calls() -> Int {
  //Como estamos rotulando os argumentos, podemos mudar a ordem
  // se quisermos
  echo call_func_on_int_with_labels(value: 8, func: double)
  echo add_one(number: 1)
  // 2
  echo string.contains(does: "theme", contain: "the")
  // True
  // Argumentos não rotulados devem ir primeiro
  echo add_two_integers(2, second: 2)
  // 4
}

fn showcase_flow_control() {
  // Caso de uso se você quiser usar correspondência de padrões para
  // selecione qual código executar.
  // A Gleam garantirá que todos os valores possíveis sejam cobertos
  // realizando verificações de exaustividade.
  // Caso contrário, você receberá erros de compilação.
  let puppies = ["Bear", "Frisco", "Ranger"]
  let count = list.length(of: puppies)
  {
    "We have "
    <> int.to_string(count)
    <> " "
    <> // O sublinhado corresponde a qualquer outro valor
    case count {
      1 -> "puppy"
      _ -> "puppies"
    }
  }
  |> echo

  // O Gleam permite que padrões em expressões case também atribuam variáveis.
  {
    "Puppy count: "
    <> case list.length(puppies) {
      0 -> "None."
      1 -> "Just one."
      other -> "As many as " <> int.to_string(other) <> " puppies."
    }
  }
  |> echo

  // Considere que as linguagens BEAM são funcionais em design e o Gleam não é exceção
  // portanto, não há construções if, for ou while disponíveis.

  // Use correspondência de padrões para condicionais
  let answer = 42
  case answer == 42 {
    True -> {
      echo "This is the answer to the universe."
    }
    False -> {
      echo "This is the answer to something else."
    }
  }

  // Use recursão em vez de loop
  from_one_to_ten(1)
}

// Função recursiva
fn from_one_to_ten(n: Int) {
  echo n
  case n {
    10 -> Nil
    _ -> from_one_to_ten(n + 1)
  }
}

// Para evitar o esgotamento da memória devido à criação excessiva
// empilhar frames ao chamar funções recursivamente, Gleam suporta
// "otimização da chamada final", o que significa que o compilador pode reutilizar
// o quadro de pilha para a função atual se uma chamada de função for
// a última coisa que a função faz.

pub fn fib(x: Int) -> Int {
  // A função pública chama a função recursiva de cauda privada
  fib_loop(x, 1)
}

fn fib_loop(x: Int, accumulator: Int) -> Int {
  case x {
    1 -> accumulator

    // A última coisa que esta função faz é chamar a si mesma
    // Na lição anterior, a última coisa que fizemos foi multiplicar dois inteiros
    _ -> fib_loop(x - 1, accumulator + x)
  }
}

// Gleam suporta correspondência de padrões do primeiro elemento e do restante
//de uma lista com o padrão [x, ..y] dentro de uma expressão case.
fn reverse_list(the_list: List(value)) -> List(value) {
  case the_list {
    [head, ..tail] -> list.flatten([reverse_list(tail), [head]])
    [] -> []
  }
}

fn more_on_recursion() {
  echo fib(10)
  // 55
  echo reverse_list([1, 2, 3])
}

fn more_on_pattern_matching() {
  // Ao combinar padrões em strings, o operador <> corresponde a strings
  // com um prefixo específico e atribui o lembrete a uma variável
  let lucy = "Hello, Lucy"
  let _ = echo case lucy {
    "Hello, " <> name -> "Greetings for " <> name
    _ -> "Potentially no greetings"
  }

  // Padrões alternativos são suportados para que a mesma cláusula seja usada
  // para vários valores
  let month = 2
  let year = 2024
  let number_of_days = case month {
    2 ->
      case is_leap_year(year) {
        False -> 28
        True -> 29
      }
    4 | 6 | 9 | 11 -> 30
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    _ -> 0
  }
  echo "Number of days: " <> int.to_string(number_of_days)
  // 29

  // Protetores na correspondência de padrões:
  // Ao usar a palavra-chave if, uma expressão deve ser avaliada como True
  // para que o padrão corresponda.
  let list_starts_with = fn(the_list: List(value), the_value: value) -> Bool {
    case the_list {
      [head, ..] if head == the_value -> True
      _ -> False
    }
  }
  echo list_starts_with([10, 20, 30], 10)
  // True
}

pub type Gender {
  Male
  Female
  Other
}

// Registros:
// - Variantes de suporte
// - Cada variante é semelhante a uma estrutura com campos
pub type Shape {
  Rectangle(base: Float, height: Float)
  Triangle(base: Float, height: Float)
}

// Registros com uma variante se assemelham a estruturas
pub type Point {
  Point(x: Float, y: Float)
}

fn showcase_types() {
  // Tuplas:
  // - Pode misturar elementos de diferentes tipos
  // - Seu tipo está implícito, por ex. #{1, "Olá"} é do tipo #{Int, String}
  // - Seus elementos podem ser acessados por índices numéricos
  let tuple_01 = #(1, "Ferris", "rustacean", True)
  let tuple_02 = #(1, "Lucy", "starfish", True)
  echo tuple_01
  echo tuple_01.0
  // 1
  echo tuple_02.1
  // Lúcia
  let #(_, name, species, _) = tuple_01
  echo name <> " the " <> species

  // Correspondência de padrões com tuplas, incluindo atribuição de variáveis
  let _ = print_mascot_status(tuple_02)

  // Usando um tipo personalizado com correspondência de padrões
  let gender = Other
  let _ = echo gender_to_string(gender)

  // Usando registros
  let rectangle_1 = Rectangle(base: 10.0, height: 20.0)
  echo rectangle_1.height
  // 10.3

  let point_1 = Point(x: 3.2, y: 4.3)
  echo point_1

  // Atualizando um registro
  let point_2 = Point(..point_1, y: 5.7)
  echo point_2

  // No Gleam, os valores não são anuláveis.
  // Nil é o único valor desse tipo.
  let some_var = Nil
  let result = io.println("Hello!")
  echo some_var == result
  // True
}

fn print_mascot_status(mascot: #(Int, String, String, Bool)) {
  case mascot {
    #(_, name, _, True) -> echo name <> " is a mascot."
    #(_, name, _, False) -> echo name <> " is not a mascot."
  }
}

fn gender_to_string(gender: Gender) -> String {
  case gender {
    Male -> "Boy"
    Female -> "Girl"
    Other -> "Undetermined"
  }
}

pub type Mineral {
  Gold
  Silver
  Copper
}

// Tipos personalizados genéricos com tipos contidos como parâmetros
pub type Purity(inner_type) {
  Pure(inner_type)
  Impure(inner_type)
}

pub type Beverage {
  Water
  Juice
}

// Tipos personalizados existentes dos módulos gleam/option e gleam/result
// facilitar o trabalho com valores anuláveis e o tratamento de possíveis erros
pub type Person {
  Person(name: String, nickname: Option(String))
}

pub type DiceError {
  DiceValueOutOfRange
}

fn checked_dice_value(value: Int) -> Result(Int, DiceError) {
  case value {
    1 | 2 | 3 | 4 | 5 | 6 -> Ok(value)
    _ -> Error(DiceValueOutOfRange)
  }
}

fn double_dice_value(value: Int) -> Result(Int, DiceError) {
  case value {
    1 | 2 | 3 -> Ok(value * 2)
    _ -> Error(DiceValueOutOfRange)
  }
}

fn more_on_types() {
  let mineral_sample_01: Purity(Mineral) = Pure(Gold)
  let mineral_sample_02 = Impure(Silver)
  echo mineral_sample_01
  echo mineral_sample_02

  // Um copo pode estar vazio ou não
  let glass_01: Option(Beverage) = Some(Water)
  let glass_02 = None
  echo glass_01
  echo glass_02

  // Uma pessoa pode ter um apelido ou não
  let person_01 = Person(name: "John", nickname: Some("The Ripper"))
  let person_02 = Person(name: "Martin", nickname: None)
  echo person_01
  echo person_02

  // Trabalhando com funções que retornam valores do tipo Result
  let dice_01 = 5
  case checked_dice_value(dice_01) {
    Ok(checked_value) ->
      echo "The value of " <> int.to_string(checked_value) <> " is OK."
    Error(DiceValueOutOfRange) ->
      echo "The value of the dice is out of range"
  }

  // Vamos tentar dobrar o valor se o valor resultante ainda for
  // um número em qualquer um dos lados do dado.
  // Caso contrário, vamos colocar o valor máximo.
  2
  |> checked_dice_value
  |> result.try(double_dice_value)
  |> result.unwrap(or: 6)
  |> echo
}

pub fn throw_dice_as_result() {
  Ok(int.random(6) + 1)
}

pub fn sum_dice_values(a: Int, b: Int) {
  Ok(a + b)
}

// Apostando em funções de primeira classe e correspondência de padrões
// pode facilmente levar a toneladas de recuo
fn roll_two_dices_without_use() {
  result.try(throw_dice_as_result(), fn(first_dice) {
    result.try(throw_dice_as_result(), fn(second_dice) {
      sum_dice_values(first_dice, second_dice)
    })
  })
}

// A expressão use ainda nos permite escrever código que usa retornos de chamada
// mas limpa o recuo excessivo:
// - Uma chamada para uma função de ordem superior vai para o lado direito do operador <-
// - Os nomes dos argumentos para a função de retorno de chamada ficam no lado esquerdo de
//   o operador <-
// - Todo o código restante no bloco {} envolvente se torna o corpo do
//   função de retorno de chamada.
fn roll_two_dices_with_use() {
  use first_dice <- result.try(throw_dice_as_result())
  use second_dice <- result.try(throw_dice_as_result())
  sum_dice_values(first_dice, second_dice)
}

fn more_on_callbacks() {
  let _ = echo roll_two_dices_without_use()
  let _ = echo roll_two_dices_with_use()
  Nil
}

// Desde a v1.14.0, a anotação @external também pode ser usada em tipos externos para
// aponte-os para os tipos Erlang ou TypeScript.
pub type DateTime

// Funções externas devem anotar um tipo de retorno.
// Esta função é definida apenas no alvo Erlang; compilando para JavaScript
// exigiria um corpo substituto ou uma anotação @external(javascript, ...).
@external(erlang, "calendar", "local_time")
pub fn now() -> DateTime {
  todo
}

fn showcase_externals() {
  let _ = echo now()
  // #(#(2024, 4, 6), #(14, 4, 16))
  Nil
}

// Constantes em nível de módulo (suportadas desde v1.0.0, com lista anexada desde v1.16.0)
pub const prime_numbers = [2, 3, 5]
pub const more_primes = [7, 11, ..prime_numbers]

fn showcase_constants() {
  let _ = echo prime_numbers
  let _ = echo more_primes

  //Desde a v1.17.0, você também pode usar `todo` dentro de declarações constantes:
  // pub const next_constant = todo
  Nil
}

fn showcase_dicts() {
  // Dicionários são coleções de valores-chave (stdlib v0.33.0)
  let scores = dict.new()
    |> dict.insert("Alice", 10)
    |> dict.insert("Bob", 15)

  let _ = echo dict.get(scores, "Alice")
  // Ok(10)
  let _ = echo dict.get(scores, "Charlie")
  // Error(Nil)
  Nil
}

fn showcase_bit_arrays() {
  // Matrizes de bits representam sequências de dados binários (stdlib v0.32.0)
  let binary_data = <<0x41, 0x42, 0x43>> // "ABC" em ASCII
  let _ = echo binary_data

  // No Gleam, a correspondência de padrões com `let` deve ser exaustiva (deve cobrir todos
  // valores possíveis). Se um padrão não corresponder em tempo de execução, devemos usar
  // `let assert` em vez de `let`. Se o match falhar, o programa entra em pânico:
  let assert <<first_byte, rest:bytes>> = binary_data
  let _ = echo first_byte // 65
  let _ = echo rest // <<66, 67>>
  Nil
}

// Tipos opacos são tipos cujos construtores são privados do módulo de definição,
// habilitando o encapsulamento e ocultando detalhes de implementação.
pub opaque type EncryptedData {
  EncryptedData(value: String)
}

pub fn encrypt(data: String) -> EncryptedData {
  EncryptedData(data <> "_encrypted")
}

fn showcase_opaque_types() {
  let secret = encrypt("my_password")
  // Podemos passar o segredo, mas não podemos combinar padrões em EncryptedData
  // ou acesse seu campo .value de fora deste módulo.
  let _ = echo secret
  Nil
}

fn showcase_panic() {
  // Podemos abortar deliberadamente a execução usando a palavra-chave panic
  // para fazer nosso programa travar imediatamente
  let three = 3
  let two = 2
  case three == two {
    True -> panic as "The equality operator is broken!"
    False -> "Equality operator works for integers"
  }
  // Chamar uma função que usa a palavra-chave todo também trava
  // lição de casa()
}

pub fn homework() {
  todo as "This function is not implemented yet"
}
```

## Leitura adicional

* [Site oficial do Gleam](https://gleam.run/)
* [Tour de idioma](https://tour.gleam.run/) - Inclui editor de código ao vivo
* [Documentação oficial](https://gleam.run/documentation/)
* [Lista incrível do Gleam](https://github.com/gleam-lang/awesome-gleam)
* [Faixa de exercícios para Gleam](https://exercism.org/tracks/gleam)

Os documentos oficiais possuem cheatsheets para pessoas familiarizadas com:

* [Elixir](https://gleam.run/cheatsheets/gleam-for-elixir-users)
* [Elm](https://gleam.run/cheatsheets/gleam-for-elm-users)
* [Erlang](https://gleam.run/cheatsheets/gleam-for-erlang-users)
* [PHP](https://gleam.run/cheatsheets/gleam-for-php-users)
* [Python](https://gleam.run/cheatsheets/gleam-for-python-users)
* [Rust](https://gleam.run/cheatsheets/gleam-for-rust-users)
