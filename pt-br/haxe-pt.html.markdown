---
language: haxe
filename: LearnHaxe3-br.hx
contributors:
    - ["Justin Donaldson", "https://github.com/jdonaldson/"]
    - ["Dan Korostelev", "https://github.com/nadako/"]
translators:
    - ["David Lima", "https://github.com/davelima/"]
lang: pt-br
---

Haxe é uma linguagem baseada na web que provê suporte a C++, C#, SWF/ActionScript,
Java e Neko byte code (também desenvolvida pelo autor de Haxe). Observe que
este guia é para a versão 3 de Haxe. Alguns pontos do guia são aplicáveis
para versões anteriores, mas é recomendado que você busque outras referências
para essas versões.


```csharp
/*
   Bem vindo ao Aprenda Haxe 3 em 15 minutos. http://www.haxe.org
   Este é um tutorial executável. Você pode compilar e rodar este tutorial
   usando o compilador haxe, estando no mesmo diretório de LearnHaxe.hx:
   $> haxe -main LearnHaxe3 -x out

   Olhe para os sinais de /* e */ em volta desses parágrafos. Nós estamos
   dentro de um "Comentário multilinha". Nós podemos colocar observações aqui
   e elas serão ignoradas pelo compilador.

   Comentários multilinha também são utilizados para gerar documentação haxedoc,
   seguindo o estilo javadoc. Eles serão usados pelo haxedoc se precerem imediatamente
   uma classe, uma função de uma classe ou uma variável de uma classe.

 */

// Duas barras, como as dessa linha, farão um comentário de linha única.


/*
   Este será o primeiro código haxe de verdade, e está declarando um pacote vazio.
   Não é necessário usar um pacote, mas ele será útil se você quiser criar
   um namespace para o seu código (exemplo: org.seuapp.SuaClasse).
   
   Omitir a declaração de pacote é a mesma coisa que declarar um pacote vazio.
 */
package; // pacote vazio, sem namespace.

/*
   Pacotes são diretórios que contém módulos. Cada módulo é um arquivo .hx que
   contém tipos definidos em um pacote. Nomes de pacotes (ex. org.seuapp)
   devem estar em letras minúsculas, enquanto nomes de módulos devem começar
   com uma letra maiúscula. Um módulo contem um ou mais tipos, cujo os nomes
   também devem começar com uma letra maiúscula.

   Exemplo: a classe "org.seuapp.Foo" deve ter a estrutura de diretório org/module/Foo.hx,
   sendo acessível do diretório do compilador ou caminho da classe.   

   Se você importar código de outros arquivos, isso deve ser declarado antes
   do restante do código. Haxe disponibiliza várias classes padrões para você
   começar:
 */
import haxe.ds.ArraySort;

// você pode importar várias classes/módulos de uma vez usando "*"
import haxe.ds.*;

// você pode importar campos estáticos
import Lambda.array;

// você também pode usar "*" para importar todos os campos estáticos
import Math.*;

/*
   Você também pode importar classes de uma forma diferente, habilitando-as para
   extender a funcionalidade de outras classes, como um "mixin". Falaremos sobre
   "using" em breve.
 */
using StringTools;

/*
   Typedefs são como variáveis... para tipos. Eles devem ser declarados antes
   de qualquer código. Veremos isso em breve.
 */
typedef FooString = String;

// Typedefs também podem referenciar tipos "estruturais". Também veremos isso em breve.
typedef FooObject = { foo: String };

/*
   Esta é a definição da classe. É a classe principal do arquivo, visto que
   possui o mesmo nome (LearnHaxe3)
 */
class LearnHaxe3{
    /*
       Se você quiser que um determinado código rode automaticamente, você
       precisa colocá-lo em uma função estática "main", e especificar a classe
       nos argumentos do compilador.
       Nesse caso, nós especificamos a classe "LearnHaxe3" no nos argumentos
       do compilador acima.
     */
    static function main(){

        /*
           Trace é o método padrão para imprimir expressões haxe na tela.
           Temos diferentes métodos para conseguir isso em diferentes destinos.
           Por exemplo: Java, C++, C#, etc. irão imprimir para stdout.
           Javascript irá imprimir no console.log, e Flash irá imprimir para um
           TextField anexado. Todos os "traces" imprimem também uma linha em branco
           por padrão. Por fim, é possível prevenir um trace de ser exibido usando
           o argumento "--no-traces" no compilador.
         */
        trace("Olá mundo, com trace()!");

        /*
           Trace pode tratar qualquer tipo de valor ou objeto. O método tentará
           imprimir a representação de uma expressão da melhor forma. Você também
           pode concatenar strings usando o operador "+":
         */
        trace( " Integer: " + 10 + " Float: " + 3.14 + " Boolean: " + true);

        /*
           Em Haxe, é obrigatório separar expressões no mesmo bloco com ';'. Mas
           é possível colocar duas expressões na mesma linha, dessa forma:
         */
        trace('duas expressões..'); trace('uma linha');


        //////////////////////////////////////////////////////////////////
        // Tipos & Variáveis
        //////////////////////////////////////////////////////////////////
        trace("***Tipos & Variáveis***");

        /*
           Vcoê pode atrelar valores e referências à estruturas usando a
           palavra-chave "var":
         */
        var um_inteiro:Int = 1;
        trace(um_inteiro + " é o valor de um_inteiro");


        /*
           Haxe é tipada estaticamente, então "um_inteiro" temos que declarar
           um valor do tipo "Int", e o restante da expressão atrela o valor "1"
           a esta variável. Em muitos casos, não é necessário declarar o tipo.
           Aqui, o compilador haxe assume que o tipo de "outro_inteiro" deve
           ser "Int"
         */
        var outro_inteiro = 2;
        trace(outro_inteiro + " é o valor de outro_inteiro");

        // O método $type() imprime o tipo que o compilador assume:
        $type(outro_inteiro);

        // Você também pode representar inteiros em hexadecimal:
        var hex_inteiro = 0xffffff;

        /*
           Haxe usa precisão de pltaforma para os tamanhos de Int e Float.
           Ele também usa o comportamento de plataforma para sobrecarga.
           (É possível ter outros tipos numéricos e comportamentos usando
           bibliotecas especiais)
         */

        /*
           Em adição a valores simples como Integers, Floats e Booleans,
           Haxe disponibiliza implementações padrões de bibliotecas para
           dados comuns de estrutura como strings, arrays, lists e maps:
         */

        var uma_string = "alguma" + 'string';  // strings podem estar entre aspas simples ou duplas
        trace(uma_string + " é o valor de uma_string");

        /*
           Strings podem ser "interpoladas" se inserirmos variáveis em
           posições específicas. A string deve estar entre aspas simples, e as
           variáveis devem ser precedidas por "$". Expressões podem estar entre
           ${...}.
         */
        var x = 1;
        var uma_string_interpolada = 'o valor de x é $x';
        var outra_string_interpolada = 'o valor de x + 1 é ${x + 1}';

        /*
           Strings são imutáveis, métodos retornarão uma cópia de partes
           ou de toda a string. (Veja também a classe StringBuf)
         */
        var uma_sub_string = a_string.substr(0,4);
        trace(uma_sub_string + " é o valor de a_sub_string");

        /*
           Regex também são suportadas, mas não temos espaço suficiente para
           entrar em muitos detalhes.
         */
        var re = ~/foobar/;
        trace(re.match('foo') + " é o valor de (~/foobar/.match('foo')))");

        /*
           Arrays são indexadas a partir de zero, dinâmicas e mutáveis. Valores
           faltando são definidos como null.
         */
        var a = new Array<String>(); // um array que contém Strings
        a[0] = 'foo';
        trace(a.length + " é o valor de a.length");
        a[9] = 'bar';
        trace(a.length + " é o valor de a.length (depois da modificação)");
        trace(a[3] + " é o valor de a[3]"); // null

        /*
           Arrays são *genéricas*, então você pode indicar quais valores elas
           contém usando um parâmetro de tipo:
         */
        var a2 = new Array<Int>(); // um Array de Ints
        var a3 = new Array<Array<String>>(); // um Array de Arrays (de Strings).

        /*
           Mapas são estruturas simples de chave/valor. A chave e o valor podem
           ser de qualquer tipo.
         */
        var m = new Map<String, Int>();  // As chaves são strings, os valores são Ints.
        m.set('foo', 4);
        // Você também pode usar a notação de array;
        m['bar'] = 5;
        trace(m.exists('bar') + " é o valor de m.exists('bar')");
        trace(m.get('bar') + " é o valor de m.get('bar')");
        trace(m['bar'] + " é o valor de m['bar']");

        var m2 =  ['foo' => 4, 'baz' => 6]; // Syntaxe alternativa de map
        trace(m2 + " é o valor de m2");

        /*
           Lembre-se, você pode usar descoberta de tipo. O compilador
           Haxe irá decidir o tipo da variável assim que você passar um
           argumento que define um parâmetro de tipo.
         */
        var m3 = new Map();
        m3.set(6, 'baz'); // m3 agora é Map<Int,String>
        trace(m3 + " é o valor de m3");

        /*
           Haxe possui mais algumas estruturas de dados padrões no módulo haxe.ds,
           tais como List, Stack e BalancedTree
         */


        //////////////////////////////////////////////////////////////////
        // Operadores
        //////////////////////////////////////////////////////////////////
        trace("***OPERADORES***");

        // aritmética básica
        trace((4 + 3) + " é o valor de (4 + 3)");
        trace((5 - 1) + " é o valor de (5 - 1)");
        trace((2 * 4) + " é o valor de (2 * 4)");
        trace((8 / 3) + " é o valor de (8 / 3) (divisão sempre produz Floats)");
        trace((12 % 4) + " é o valor de (12 % 4)");


        // comparação básica
        trace((3 == 2) + " é o valor de 3 == 2");
        trace((3 != 2) + " é o valor de 3 != 2");
        trace((3 >  2) + " é o valor de 3 > 2");
        trace((3 <  2) + " é o valor de 3 < 2");
        trace((3 >= 2) + " é o valor de 3 >= 2");
        trace((3 <= 2) + " é o valor de 3 <= 2");

        // operadores bit-a-bit padrões
        /*
        ~       Complemento bit-a-bit unário
        <<      Deslocamento a esquerda
        >>      Deslocamento a direita
        >>>     Deslocamento a direita com preenchimento de 0
        &       Bit-a-bit AND
        ^       Bit-a-bit OR exclusivo
        |       Bit-a-bit OR inclusivo
        */

        // incrementos
        var i = 0;
        trace("Incrementos e decrementos");
        trace(i++); //i = 1. Pós-incremento
        trace(++i); //i = 2. Pré-incremento
        trace(i--); //i = 1. Pós-decremento
        trace(--i); //i = 0. Pré-decremento

        //////////////////////////////////////////////////////////////////
        // Estruturas de controle
        //////////////////////////////////////////////////////////////////
        trace("***ESTRUTURAS DE CONTROLE***");

        // operadores if
        var j = 10;
        if (j == 10){
            trace("isto é impresso");
        } else if (j > 10){
            trace("não é maior que 10, então não é impresso");
        } else {
            trace("também não é impresso.");
        }

        // temos também um if "ternário":
        (j == 10) ?  trace("igual a 10") : trace("diferente de 10");

        /*
           Por fim, temos uma outra forma de estrutura de controle que opera
           e na hora da compilação: compilação condicional.
         */
#if neko
        trace('olá de neko');
#elseif js
        trace('olá de js');
#else
        trace('olá de outra plataforma!');
#end
        /*
           O código compilado irá mudar de acordo com o alvo de plataforma.
           Se estivermos compilando para neko (-x ou -neko), só teremos a
           saudação de neko.
         */


        trace("Loops e Interações");

        // loop while
        var k = 0;
        while(k < 100){
            // trace(counter); // irá iprimir números de 0 a 99
            k++;
        }

        // loop do-while
        var  l = 0;
        do{
            trace("do sempre rodará pelo menos uma vez");
        } while (l > 0);

        // loop for
        /*
           Não há loop for no estilo C para Haxe, pois é propenso
           a erros e não é necessário. Ao invés disso, Haxe possui
           uma versão muito mais simples e segura que usa Iterators
           (veremos isso logo mais).
         */
        var m = [1,2,3];
        for (val in m){
            trace(val + " é o valor de val no array m");
        }

        // Perceba que você pode iterar em um índice usando uma lista limitada
        // (veremos isso em breve também)
        var n = ['foo', 'bar', 'baz'];
        for (val in 0...n.length){
            trace(val + " é o valor de val (um índice de n)");
        }


        trace("Compreensões de array");

        // Compreensões de array servem para que você posse iterar um array
        // enquanto cria filtros e modificações
        var n_filtrado = [for (val in n) if (val != "foo") val];
        trace(n_filtrado + " é o valor de n_filtrado");

        var n_modificado = [for (val in n) val += '!'];
        trace(n_modificado + " é o valor de n_modificado");

        var n_filtrado_e_modificado = [for (val in n) if (val != "foo") val += "!"];
        trace(n_filtrado_e_modificado + " é o valor de n_filtrado_e_modificado");

        //////////////////////////////////////////////////////////////////
        // Blocos Switch (Tipos de valor)
        //////////////////////////////////////////////////////////////////
        trace("***BLOCOS SWITCH (Tipos de valor)***");

        /*
           Blocos Switch no Haxe são muito poderosos. Além de funcionar com
           valores básicos como strings e ints, também funcionam com tipos
           algébricos em enums (falaremos sobre enums depois).
           Veja alguns exemplos de valor básico por enquanto:
         */
        var meu_cachorro = "fido";
        var coisa_favorita  = "";
        switch(meu_cachorro){
            case "fido" : favorite_thing = "osso";
            case "rex"  : favorite_thing = "sapato";
            case "spot" : favorite_thing = "bola de tênis";
            default     : favorite_thing = "algum brinquedo desconhecido";
            // case _   : favorite_thing = "algum brinquedo desconhecido"; // mesma coisa que default
        }
        // O case "_" acima é um valor "coringa" que
        // que funcionará para qualquer coisa.

        trace("O nome do meu cachorro é " + meu_cachorro
                + ", e sua coisa favorita é: "
                + coisa_favorita);

        //////////////////////////////////////////////////////////////////
        // Declarações de expressão
        //////////////////////////////////////////////////////////////////
        trace("***DECLARAÇÕES DE EXPRESSÃO***");

        /*
           As declarações de controle em Haxe são muito poderosas pois
           toda declaração também é uma expressão, considere o seguinte:
        */

        // declarações if
        var k = if (true) 10 else 20;

        trace("k igual a ", k); // retorna 10

        var outra_coisa_favorita = switch(meu_cachorro) {
            case "fido" : "ursinho";
            case "rex"  : "graveto";
            case "spot" : "bola de futebol";
            default     : "algum brinquedo desconhecido";
        }

        trace("O nome do meu cachorro é " + meu cachorro
                + ", e sua outra coisa favorita é: "
                + outra_coisa_favorita);

        //////////////////////////////////////////////////////////////////
        // Convertendo tipos de valores
        //////////////////////////////////////////////////////////////////
        trace("***CONVERTENDO TIPOS DE VALORES***");

        // Você pode converter strings em ints de forma bem fácil.

        // string para int
        Std.parseInt("0"); // retorna 0
        Std.parseFloat("0.4"); // retorna 0.4;

        // int para string
        Std.string(0); // retorna "0";
        // concatenar com strings irá converter automaticamente em string.
        0 + "";  // retorna "0";
        true + ""; // retorna "true";
        // Veja a documentação de parseamento em Std para mais detalhes.


        //////////////////////////////////////////////////////////////////
        // Lidando com Tipos
        //////////////////////////////////////////////////////////////////

        /*
           Como mencionamos anteriormente, Haxe é uma linguagem tipada
           estaticamente. Tipagem estática é uma coisa maravilhosa. Isto
           permite autocompletar mais preciso, e pode ser usado para checar
           completamente o funcionamento de um programa. Além disso, o compilador
           Haxe é super rápido.

           *ENTRETANTO*, há momentos em que você espera que o compilador apenas
           deixe algo passar, e não lance um "type error" em um determinado caso.

           Para fazer isso, Haxe tem duas palavras-chave separadas. A primeira
           é o tipo "Dynamic":
         */
        var din: Dynamic = "qualquer tipo de variável, assim como essa string";

        /*
           Tudo o que você sabe sobre uma variável Dynamic é que o compilador
           não irá mais se preocupar com o tipo dela. É como uma variável
           "coringa": você pode usar isso ao invés de qualquer tipo de variável,
           e você pode atrelar qualquer valor a essa variável.

           A outra (e mais extrema) opção é a palavra-chave "untyped":
         */

            untyped {
                var x:Int = 'foo'; // não faz sentido!
                var y:String = 4; // loucura!
            }

        /*
           A palavra-chave "untyped" opera em *blocos* inteiros de código,
           ignorando qualquer verificação de tipo que seria obrigatória em
           outros casos. Essa palavra-chave deve ser usada com muita cautela,
           como em situações limitadas de compilação condicional onde a
           verificação de tipo pode ser um obstáculo.

           No geral, ignorar verificações de tipo *não* é recomendado. Use
           os modelos de enum, herança ou estrutural para garantir o correto
           funcionamento do seu programa. Só quando você tiver certeza de que
           nenhum desses modelos funcionam no seu caso, você deve usar "Dynamic"
           ou "untyped".
         */

        //////////////////////////////////////////////////////////////////
        // Programação básica orientada a objetos
        //////////////////////////////////////////////////////////////////
        trace("***PROGRAMAÇÃO BÁSICA ORIENTADA A OBJETOS***");


        /*
           Cria uma instância da FooClass. As definicções dessas classes
           estão no final do arquivo.
         */
        var instancia_foo = new FooClass(3);

        // lê a variável pública normalmente
        trace(instancia_foo.variavel_publica + " é o valor de instancia_foo.variavel_publica");

        // nós podemos ler essa variável
        trace(instancia_foo.publica_leitura + " é o valor de instancia_foo.publica_leitura");
        // mas não podemos escrever nela
        // instancia_foo.publica_leitura = 4; // isso irá causar um erro se descomentado:
        // trace(instancia_foo.public_escrita); // e isso também.

        // chama o método toString:
        trace(instancia_foo + " é o valor de instancia_foo");
        // mesma coisa:
        trace(instancia_foo.toString() + " é o valor de instancia_foo.toString()");


        /*
           A instancia_foo é do tipo "FooClass", enquanto acceptBarInstance
           é do tipo BarClass. Entretanto, como FooClass extende BarClass,
           ela é aceita.
         */
        BarClass.acceptBarInstance(instancia_foo);

        /*
           As classes abaixo têm mais alguns exemplos avançados, o método
           "example()" executará esses exemplos aqui:
         */
        SimpleEnumTest.example();
        ComplexEnumTest.example();
        TypedefsAndStructuralTypes.example();
        UsingExample.example();

    }

}

/*
   Essa é a "classe filha" do classe principal LearnHaxe3
 */
class FooClass extends BarClass implements BarInterface{
    public var variavel_publica:Int; // variáveis públicas são acessíveis de qualquer lugar
    public var publica_leitura (default, null): Int; // somente leitura pública habilitada
    public var publica_escrita (null, default): Int; // somente escrita pública habilitada
    public var property (get, set): Int; // use este estilo para habilitar getters e setters

    // variáveis privadas não estão disponíveis fora da classe.
    // veja @:allow para formas de fazer isso.
    var _private:Int; // variáveis são privadas se não forem marcadas como públicas

    // um construtor público
    public function new(arg:Int){
        // chama o construtor do objeto pai, já que nós extendemos a BarClass:
        super();

        this.variavel_publica = 0;
        this._private = arg;

    }

    // getter para _private
    function get_property() : Int {
        return _private;
    }

    // setter para _private
    function set_property(val:Int) : Int {
        _private = val;
        return val;
    }

    // função especial que é chamada sempre que uma instância é convertida em string.
    public function toString(){
        return _private + " com o método toString()!";
    }

    // essa classe precisa ter essa função definida, pois ela implementa
    // a interface BarInterface
    public function baseFunction(x: Int) : String{
        // converte o int em string automaticamente
        return x + " foi passado pela baseFunction!";
    }
}

/*
    Uma classe simples para extendermos
*/
class BarClass {
    var base_variable:Int;
    public function new(){
        base_variable = 4;
    }
    public static function acceptBarInstance(b:BarClass){
    }
}

/*
    Uma interface simples para implementarmos
*/
interface BarInterface{
    public function baseFunction(x:Int):String;
}

//////////////////////////////////////////////////////////////////
// Declarações Enum e Switch
//////////////////////////////////////////////////////////////////

/*
   Enums no Haxe são muito poderosos. Resumidamente, enums são
   um tipo com um número limitado de estados:
 */

enum SimpleEnum {
    Foo;
    Bar;
    Baz;
}

//   Uma classe que faz uso desse enum:

class SimpleEnumTest{
    public static function example(){
        var e_explicit:SimpleEnum = SimpleEnum.Foo; // você pode especificar o nome "completo"
        var e = Foo; // bas descoberta de tipo também funciona.
        switch(e){
            case Foo: trace("e era Foo");
            case Bar: trace("e era Bar");
            case Baz: trace("e era Baz"); // comente esta linha e teremos um erro.
        }

        /*
           Isso não parece tão diferente de uma alteração simples de valor em strings.
           Entretanto, se nós não incluirmos *todos* os estados, o compilador
           reclamará. Você pode testar isso comentando a linha mencionada acima.
           
           Você também pode especificar um valor padrão (default) para enums:
         */
        switch(e){
            case Foo: trace("e é Foo outra vez");
            default : trace("default funciona aqui também");
        }
    }
}

/*
    Enums vão muito mais além que estados simples, nós também
    podemos enumerar *construtores*, mas nós precisaremos de um
    exemplo mais complexo de enum:
 */
enum ComplexEnum{
    IntEnum(i:Int);
    MultiEnum(i:Int, j:String, k:Float);
    SimpleEnumEnum(s:SimpleEnum);
    ComplexEnumEnum(c:ComplexEnum);
}
// Observação: O enum acima pode incluir *outros* enums também, incluindo ele mesmo!
// Observação: Isto é o que chamamos de *Tipos de dado algébricos* em algumas outras linguagens.

class ComplexEnumTest{
    public static function example(){
        var e1:ComplexEnum = IntEnum(4); // especificando o parâmetro enum
        /*
           Agora nós podemos usar switch no enum, assim como extrair qualquer
           parâmetros que ele possa ter.
         */
        switch(e1){
            case IntEnum(x) : trace('$x foi o parâmetro passado para e1');
            default: trace("Isso não deve ser impresso");
        }

        // outro parâmetro aqui que também é um enum... um enum enum?
        var e2 = SimpleEnumEnum(Foo);
        switch(e2){
            case SimpleEnumEnum(s): trace('$s foi o parâmetro passado para e2');
            default: trace("Isso não deve ser impresso");
        }

        // enum dentro de enum dentro de enum
        var e3 = ComplexEnumEnum(ComplexEnumEnum(MultiEnum(4, 'hi', 4.3)));
        switch(e3){
            // Você pode buscar por certos enums aninhados especificando-os
            // explicitamente:
            case ComplexEnumEnum(ComplexEnumEnum(MultiEnum(i,j,k))) : {
                trace('$i, $j, e $k foram passados dentro desse monstro aninhado.');
            }
            default: trace("Isso não deve ser impresso");
        }
        /*
           Veja outros "tipos de dado algébricos" (GADT, do inglês) para mais
           detalhes sobre o porque eles são tão úteis.
         */
    }
}

class TypedefsAndStructuralTypes {
    public static function example(){
        /*
           Aqui nós usaremos tipos typedef, ao invés de tipos base.
           Lá no começo, nós definimos que o tipo "FooString" é um tipo "String". 
         */
        var t1:FooString = "alguma string";

        /*
           Aqui nós usamos typedefs para "tipos estruturais" também. Esses tipos
           são definidos pela sua estrutura de campos, não por herança de classe.
           Aqui temos um objeto anônimo com um campo String chamado "foo":
         */

        var anon_obj = { foo: 'hi' };

        /*
           A variável anon_obj não tem um tipo declarado, e é um objeto anônimo
           de acordo com o compilador. Entretanto, lembra que lá no início nós
           declaramos a typedef FooObj? Visto que o anon_obj tem a mesma estrutura,
           nós podemos usar ele em qualquer lugar que um "FooObject" é esperado.
         */

        var f = function(fo:FooObject){
            trace('$fo foi passado para esta função');
        }
        f(anon_obj); // chama a assinatura de FooObject com anon_obj.

        /*
           Note que typedefs podem ter campos opcionais também, marcados com "?"

           typedef OptionalFooObj = {
                ?optionalString: String,
                requiredInt: Int
           }
         */

        /*
           Typedefs também funcionam com compilação condicional. Por exemplo,
           nós poderíamos ter incluído isso no topo deste arquivo:

#if( js )
        typedef Surface = js.html.CanvasRenderingContext2D;
#elseif( nme )
        typedef Surface = nme.display.Graphics;
#elseif( !flash9 )
        typedef Surface = flash8.MovieClip;
#elseif( java )
        typedef Surface = java.awt.geom.GeneralPath;
#end

        E teríamos apenas um tipo "Surface" para funcionar em todas
        essas plataformas.
        */
    }
}

class UsingExample {
    public static function example() {

        /*
           A palavra-chave "using" é um tipo especial de import de classe que
           altera o comportamento de qualquer método estático na classe.

           Neste arquivo, nós aplicamos "using" em "StringTools", que contém
           alguns métodos estáticos para tratar tipos String.
         */
        trace(StringTools.endsWith("foobar", "bar") + " deve ser verdadeiro!");

        /*
           Com um import "using", o primeiro argumento é extendido com o método.
           O que isso significa? Bem, como "endsWith" tem um primeiro argumento
           de tipo "String", isso significa que todos os tipos "String" agora
           possuem o método "endsWith":
         */
        trace("foobar".endsWith("bar") + " deve ser verdadeiro!");

        /*
           Essa técnica habilita uma grande quantidade de expressões para certos
           tipos, e limita o escopo de modificações para um único arquivo.

           Note que a instância String *não* é modificada em tempo de execução.
           O novo método adicionado não é uma parte da instância anexada, e o
           compilador ainda irá gerar o código equivalente ao método estático.
         */
      }

}

```
Isso foi apenas o começo do que Haxe pode fazer. Para uma documentação de todos
os recursos de Haxe, veja o [manual](https://haxe.org/manual) e a
[documentação de API](https://api.haxe.org/). Para um diretório de bibliotecas de terceiros
disponíveis, veja a [Haxelib](https://lib.haxe.org/)

Para tópicos mais avançados, dê uma olhada em:

* [Tipos abstratos](https://haxe.org/manual/types-abstract.html)
* [Macros](https://haxe.org/manual/macro.html)
* [Recursos do compilador](https://haxe.org/manual/cr-features.html)

Por fim, participe do [forum Haxe](https://community.haxe.org/),
ou no IRC [#haxe onfreenode](http://webchat.freenode.net/), ou no
[Chat Gitter](https://gitter.im/HaxeFoundation/haxe).
