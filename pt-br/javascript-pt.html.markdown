---
language: javascript
filename: javascript-pt.js
contributors:
    - ["Leigh Brenecki", "https://leigh.net.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
translators:
    - ["Willian Justen", "http://willianjusten.com.br"]
lang: pt-br
---

JavaScript foi criada por Brendan Eich, funcionário da Netscape na época, em 1995. Ela
foi originalmente criada para ser uma linguagem de script para websites,
complementando o uso de Java para aplicações web mais complexas, mas a sua
integração com páginas web e seu suporte nativo nos browsers fez com que
ela se tornasse mais comum que Java no frontend web.

Javascript não é somente limitada a browsers web, existindo o Node.js,
que é um projeto que fornece um interpretador baseado no motor V8 do Google 
Chrome e está se tornando cada vez mais famoso.

Feedback são muito apreciados! Você me encontrar em
[@ExcitedLeigh](https://twitter.com/ExcitedLeigh), ou
[l@leigh.net.au](mailto:l@leigh.net.au).

```js
// Comentários são como em C. Comentários de uma linha começam com duas barras,
/* e comentários de múltiplas linhas começam com barra-asterisco
   e fecham com asterisco-barra */

// comandos podem ser terminados com  ;
facaAlgo();

// ... mas eles não precisam ser, o ponto-e-vírgula é automaticamente
// inserido quando há uma nova linha, exceto alguns casos.
facaAlgo()

// Como esses casos podem causar resultados inesperados, vamos continuar 
// a usar ponto-e-vírgula neste guia.

///////////////////////////////////
// 1. Números, Strings e Operadores

// Javascript tem um tipo de número (que é o 64-bit IEEE 754 double).
// Doubles tem uma mantissa 52-bit, que é suficiente para guardar inteiros
// acima de 9✕10¹⁵ precisamente.
3; // = 3
1.5; // = 1.5

// A aritmética básica funciona como seria de se esperar.
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// Inclusive divisão desigual.
5 / 2; // = 2.5

// Operadores Bitwise também funcionam; quando você faz uma operação bitwise
// seu float é convertido para um int de até 32 bits.
1 << 2; // = 4

// A precedência é aplicada com parênteses.
(1 + 3) * 2; // = 8

// Existem três especiais valores não-é-número-real:
Infinity; // resultado de 1/0
-Infinity; // resultado de -1/0
NaN; // resultado de 0/0

// Existe também o tipo booleano.
true;
false;

// Strings são criados com ' ou ".
'abc';
"Olá, mundo";

// Negação usa o símbolo !
!true; // = false
!false; // = true

// Igualdade é o sinal de ===
1 === 1; // = true
2 === 1; // = false

// Desigualdade é o sinal de !==
1 !== 1; // = false
2 !== 1; // = true

// Mais comparações
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// Strings são concatenadas com +
"Olá " + "mundo!"; // = "Olá mundo!"

// e comparadas com < e >
"a" < "b"; // = true

// A comparação de tipos não é feita com o uso de ==...
"5" == 5; // = true
null == undefined; // = true

// ...a menos que use ===
"5" === 5; // = false
null === undefined; // = false 

// ...isso pode resultar em comportamentos estranhos...
13 + !0; // 14
"13" + !0; // '13true'

// Você pode acessar caracteres de uma String usando o `charAt`
"Isto é uma String".charAt(0);  // = 'I'

// ...ou usar `substring` para pegar pedaços maiores.
"Olá mundo".substring(0, 3); // = "Olá"

// `length` é uma propriedade, portanto não use ().
"Olá".length; // = 3

// Existe também o `null` e o `undefined`.
null;      // usado para indicar um valor não considerado
undefined; // usado para indicar um valor que não é a atualmente definido
           // (entretando `undefined` é considerado de fato um valor

// false, null, undefined, NaN, 0 and "" são valores falsos;
// qualquer outro valor é verdadeiro
// Note que 0 é falso e "0" é verdadeiro, até mesmo 0 == "0".

///////////////////////////////////
// 2. Variáveis, Arrays e Objetos

// Variáveis são declaradas com a palavra-chave `var`. O Javascript é 
// dinâmicamente tipado, portanto você não precisa especificar o tipo.
// Atribuições usam um simples caracter de `=`.
var someVar = 5;

// se você deixar de colocar a palavra-chave var, você não irá receber um erro...
someOtherVar = 10;

// ...mas sua variável será criada no escopo global, não no escopo em que você
// definiu ela.

// Variáveis declaradas sem receberem um valor são definidas como `undefined`.
var someThirdVar; // = undefined

// Existe um shorthand para operações matemáticas em variáveis:
someVar += 5; // equivalente a someVar = someVar + 5; someVar é 10 agora
someVar *= 10; // agora someVar é 100

// e um para adição e subtração de 1
someVar++; // agora someVar é 101
someVar--; // volta para 100

// Arrays são listas ordenadas de valores, de qualquer tipo.
var myArray = ["Olá", 45, true];

// Seus membros podem ser acessados usando a sintaxe de colchetes.
// O indíce de um Array começa pelo 0.
myArray[1]; // = 45

// Arrays são mutáveis e de tamanho variável.
myArray.push("World");
myArray.length; // = 4

// Adicionar/modificar em um índice específico
myArray[3] = "Hello";

// Objetos de Javascript são equivalentes aos dicionários ou maps de outras
// linguagens: uma coleção não ordenada de pares chave-valor.
var myObj = {chave1: "Olá", chave2: "Mundo"};

// Chaves são strings, mas as aspas não são necessárias se elas são
// identificadores válidos no Javascript. Valores podem ser de qualquer tipo.
var myObj = {myKey: "myValue", "my other key": 4};

// Atributos de objetos também podem ser acessados com a sintaxe de colchetes.
myObj["my other key"]; // = 4

// ... ou usando a sintaxe de ponto, passando a chave que é um identificador
// válido.
myObj.myKey; // = "myValue"

// Objetos são mutáveis, valores podem ser modificados e novas chaves 
// adicionadas.
myObj.myThirdKey = true;

// Se você tentar acessar um valor que não foi determinado ainda, você irá
// receber `undefined`.
myObj.myFourthKey; // = undefined

///////////////////////////////////
// 3. Lógica e Estruturas de Controle

// A sintaxe para essa seção é quase idêntica a maioria das linguagens.

// A estrutura `if` funciona como deveria ser.
var count = 1
if (count == 3){
    // executa se count é 3
} else if (count == 4){
    // executa se count é 4
} else {
    // executa se count não é 3 nem 4
}

// Como se faz um `while`.
while (true){
    // Um loop infinito!
}

// Os loops do-while são como os loops de while, exceto quando eles sempre
// executam pelo menos uma vez.
do {
    input = getInput();
} while (!isValid(input))

// O loop `for` é o mesmo de C e Java:
// inicialização, condição para continuar; iteração
for (var i = 0; i < 5; i++){
    // vai rodar cinco vezes
}

// && é o `e` lógico , || é o `ou` lógico
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
}
if (cor == "red" || cor == "blue"){
    // cor é vermelha OU azul
}

// && e || "pequeno circuito", é útil para determinar valores padrões.
var name = otherName || "padrão";

// O `switch` checa pela igualdade com `===`.
// Use `break` após cada `case`
grade = 'B';
switch (grade) {
  case 'A':
    console.log("Great job");
    break;
  case 'B':
    console.log("OK job");
    break;
  case 'C':
    console.log("You can do better");
    break;
  default:
    console.log("Oy vey");
    break;
}


///////////////////////////////////
// 4. Funções, Escopos e Closures

// Funções Javascript são declaradas com a palavra-chave `function`.
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"

// Repare que o valor a ser retornado deve começar na mesma linha que
// a palavra-chave `return`, senão você sempre irá retornar `undefined` 
// visto que o ponto-e-vírgula é inserido automáticamente nas quebras de 
// linha. Preste atenção quando usar o estilo Allman.
function myFunction()
{
    return // <- ponto-e-vírgula adicionado automaticamente aqui
    {
        thisIsAn: 'object literal'
    }
}
myFunction(); // = undefined

// Funções Javascript são objetos de primeira classe, portanto elas podem
// ser atribuídas a nomes de variáveis e serem passadas para outras funções
// como argumentos - por exemplo, quando criamos um manipulador de eventos:
function myFunction(){
    // este código será chamado em 5 segundos
}
setTimeout(myFunction, 5000);
// Nota: `setTimeout` não é parte da linguagem Javascript, mas é provido pelos
// browsers e o Node.js.

// Objetos de funções não precisam nem serem declarados com nome - você pode 
// escrever a definição de uma função anônima diretamente nos argumentos de 
// outra função.
setTimeout(function(){
    // este código será chamado em 5 segundos
}, 5000);

// O Javascript tem escopo de função; as funções tem seu próprio escopo, 
// mas outros blocos não.
if (true){
    var i = 5;
}
i; // = 5 - não `undefined` como você esperaria numa linguagem de blogo-escopo

// Isso levou a padrão comum chamado de IIFE (Imediately Invoked Function 
// Expression) ou (Expressão de Função Invocada Imediatamente), que previne
// que variáveis temporárias vazem para o escopo global.
(function(){
    var temporary = 5;
    // Nós podemos acessar o escopo global definindo o "objeto global", que
    // no browser vai ser sempre `window`. O objeto global pode ter um nome
    // diferente para ambiente não-browser como o Node.js.
    window.permanent = 10;
})();
temporary; // levanta um erro de referência inexiste
permanent; // = 10

// Uma das principais características do Javascript é a closure. Que é
// uma função definida dentro de outra função, a função interna pode acessar
// todas as variáveis da função externa, mesmo depois da função de fora
// finalizar sua execução.
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";

    // Funções internas são colocadas no escopo local por padrão, assim como
    // se fossem declaradas com `var`. 
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // `setTimeout` é assíncrono, portanto a função `sayHelloInFiveSeconds`
    // vai sair imediatamente, e o `setTimeout` irá chamar a interna depois.
    // Entretanto. como a interna é fechada dentro de "sayHelloInFiveSeconds",
    // a interna permanece podendo acessar a variável `prompt` quando depois
    // de chamada.
}
sayHelloInFiveSeconds("Adam"); // Vai abrir um popup com "Hello, Adam!" em 5s

///////////////////////////////////
// 5. Mais sobre Objetos; Construtores e Prototypes

// Objetos podem conter funções.
var myObj = {
    myFunc: function(){
        return "Olá mundo!";
    }
};
myObj.myFunc(); // = "Olá mundo!"

// Quando uma função ligada a um objeto é chamada, ela pode acessar o objeto
// da qual foi ligada usando a palavra-chave `this`. 
myObj = {
    myString: "Olá mundo!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Olá mundo!"

// O `this` só funciona para dentro do escopo do objeto, portanto, se chamarmos 
// um método do objeto fora de seu escopo, este não irá funcionar.
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// Inversamente, uma função pode ser atribuída à um objeto e ganhar a acesso
// através do `this`, até mesmo se ela não for chamada quando foi definida.
var myOtherFunc = function(){
    return this.myString.toUpperCase();
}
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "OLÁ MUNDO!"

// Nós podemos também especificar um contexto onde a função irá executar, 
// usando o `call` ou `apply`.

var anotherFunc = function(s){
    return this.myString + s;
}
anotherFunc.call(myObj, " E Olá Lua!"); // = "Olá mundo! E Olá Lua!"

// A função `apply` é praticamente a mesma coisa,  mas ela pega um array
// como lista de argumentos.

anotherFunc.apply(myObj, [" E Olá Sol!"]); // = "Olá mundo! E Olá Sol!"

// Isto é util quando trabalhamos com uma função que aceita uma sequência de
// argumentos e você quer passar um array.

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// Mas, o `call` e `apply` são somente temporários. Quando você quiser que 
// permaneça sempre no escopo, use `bind`.

var boundFunc = anotherFunc.bind(myObj);
boundFunc(" E Olá Saturno!"); // = "Olá mundo! E Olá Saturno!"

// `bind` também pode ser usado para parcialmente aplicar (curry) uma função.

var product = function(a, b){ return a * b; }
var doubler = product.bind(this, 2);
doubler(8); // = 16

// Quando você invoca uma função com a palavra-chave `new`, um novo objeto
// é criado, e fica disponível para a função pela palavra-chave `this`. 
// Funções são desenhadas para serem invocadas como se invocam os construtores.

var MyConstructor = function(){
    this.myNumber = 5;
}
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// Todo objeto JavaScript possui um `prototype`. Quando você tenta acessar 
// uma propriedade de um objeto que não existe no objeto atual, o interpretador
// vai olhar imediatamente para o seu prototype.

// Algumas implementações em JS deixam você acessar o objeto prototype com a 
// propriedade mágica `__proto__`. Enquanto isso é útil para explicar 
// prototypes, não é parte de um padrão; nós vamos falar de algumas formas de 
// usar prototypes depois.

var myObj = {
    myString: "Olá Mundo!"
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase()
    }
};

myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42

// Isto funciona para funções, também.
myObj.myFunc(); // = "olá mundo!"

// É claro, se sua propriedade não está em seu prototype, 
// o prototype do prototype será procurado e por aí vai.
myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true

// Não há cópia envolvida aqui; cada objeto guarda uma referência do
// prototype. Isso significa que podemos alterar o prototype e nossas mudanças
// serão refletidas em qualquer lugar.
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43


// Nós mencionamos que o `__proto__` não é uma forma padrão, e não há uma 
// forma padrão de mudar o prototype de um objeto já existente. Entretanto, 
// existem duas formas de se criar um objeto com um dado prototype.

// A primeira forma é `Object.create`, que é uma adição recente do JS,
// e ainda não está disponível em todas as implementações.
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// A segunda forma, que funciona em qualquer lugar, é feita com construtores.
// Construtores tem uma propriedade chamada prototype. Este *não* é o prototype
// do construtor em si; ao invés disso, ele é o prototype dos novos objetos
// criados pelo construtor.
MyConstructor.prototype = {
    myNumber: 5,
    getMyNumber: function(){
        return this.myNumber;
    }
};
var myNewObj2 = new MyConstructor();
myNewObj2.getMyNumber(); // = 5
myNewObj2.myNumber = 6
myNewObj2.getMyNumber(); // = 6

// Tipos originais da linguagem como strings e números também possuem
// construtores equivalentes. 
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// Exceto, que eles não são totalmente equivalentes.
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // O código não vai executar, porque 0 é um valor falso.
}

// Entretanto, esses objetos encapsulados e as funções originais compartilham
// um mesmo prototype, portanto você pode adicionar funcionalidades à uma string,
// por exemplo.
String.prototype.firstCharacter = function(){
    return this.charAt(0);
}
"abc".firstCharacter(); // = "a"

// Esse fato é usado para criar os chamados `polyfills`, que implementam
// uma nova característica do Javascript em uma versão mais velha, para que
// assim funcionem em ambientes mais velhos como browsers ultrapassados.

// Havíamos mencionado que `Object.create` não estava ainda disponível em 
// todos as implementações, mas nós podemos usá-lo com esse polyfill:
if (Object.create === undefined){ // Não o sobrescreve se já existir
    Object.create = function(proto){
        // faz um construtor temporário com o prototype certo
        var Constructor = function(){};
        Constructor.prototype = proto;
        // então utiliza o new para criar um objeto prototype apropriado
        return new Constructor();
    }
}
```

## Leitura Adicional

O [Mozilla Developer
Network](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript) dispõe de uma
excelente documentação sobre Javascript e seu uso nos browsers. E mais, 
é uma wiki, portanto conforme você vai aprendendo, mais você pode ir ajudando
os outros compartilhando do seu conhecimento.

[Uma re-introdução do JavaScript pela MDN]
(https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
cobre muito dos conceitos abordados aqui em mais detalhes. Este guia fala
somente sobre a linguagem JavaScript em si; se você quiser aprender mais
sobre e como usar o JavaScript em páginas na web, comece aprendendo sobre
[Document Object
Model](https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core)

[Aprenda Javascript por Exemplos e com Desafios](http://www.learneroo.com/modules/64/nodes/350) é uma
variação desse guia com desafios.

[JavaScript Garden](http://bonsaiden.github.io/JavaScript-Garden/) é um guia 
profundo de todas as partes do JavaScript.

[JavaScript: The Definitive Guide](http://www.amazon.com/gp/product/0596805527/) é o guia clássico
/ livro de referência. 

Parte desse artigo foi adaptado do tutorial de Python do Louie Dinh que está
nesse site e do [Tutorial de JS](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
da Mozilla Developer Network.
