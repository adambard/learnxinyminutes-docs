---

language: java
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Madison Dickson", "http://github.com/mix3d"]
translators:
    - ["Victor Kléber Santos L. Melo", "http://victormelo.com.br/blog"]
    - ["Renê Douglas N. de Morais", "mailto:rene.douglas.bsi@gmail.com"]
lang: pt-br
filename: LearnJava-pt.java

---

Java é uma linguagem de programação de propósito geral, concorrente, baseada em classes e orientada a objetos.
[Leia mais aqui](http://docs.oracle.com/javase/tutorial/java/index.html)

```java
// Comentários de uma linha começam com //
/*
Comentários de várias linhas são feitos dessa forma.
*/
/**
Comentários JavaDoc são feitos assim. São usados para descrever a Classe ou os atributos da Classe.
*/

// Importa a classe ArrayList que está dentro do pacote java.util
import java.util.ArrayList;
// Importa todas as classes que estão dentro do pacote java.security
import java.security.*;

// Cada arquivo .java contém uma classe pública, com o mesmo nome do arquivo.
public class LearnJava {

    // Um programa precisa ter um método main como um ponto de entrada.
    public static void main (String[] args) {

        // O System.out.println é usado para imprimir no console
        System.out.println("Olá Mundo!");
        System.out.println(
            "Integer: " + 10 +
            " Double: " + 3.14 +
            " Boolean: " + true);

        // Para imprimir sem inserir uma nova lina, use o System.out.print
        System.out.print("Olá ");
        System.out.print("Mundo");


        ///////////////////////////////////////
        // Tipos & Variáveis
        ///////////////////////////////////////

        // Declara-se variáveis usando <tipo> <nome> [
        // Byte - inteiro de 8 bits com sinal complementado a dois
        // (-128 <= byte <= 127)
        byte fooByte = 100;

        // Short - inteiro de 16 bits com sinal complementado a dois
        // (-32,768 <= short <= 32,767)
        short fooShort = 10000;

        // Integer - inteiro de 32 bits com sinal complementado a dois
        // (-2,147,483,648 <= int <= 2,147,483,647)
        int fooInt = 1;

        // Long - inteiro de 64 bits com sinal complementado a dois
        // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
        long fooLong = 100000L;
        // L é usado para indicar que o valor da variável é do tipo Long;
        // sem o L, tudo é tratado como inteiro por padrão.

        // Nota: Java não tem tipos sem sinal

        // Float - Ponto Flutuante 32-bits, de precisão simples no padrão IEEE 754
        float fooFloat = 234.5f;
        // f é usado para indicar que o valor da variável é do tipo float;
        // caso contrário, ela é tratada como double.

        // Double - Ponto Flutuante 64-bits, de precisão dupla no padrão IEEE 754
        double fooDouble = 123.4;

        // Boolean - true & false
        boolean fooBoolean = true;
        boolean barBoolean = false;

        // Char - Um caractere Unicode de 16 bits
        char fooChar = 'A';

        // Usa-se o final para fazer com que a variável seja imutável.
        final int HORAS_QUE_TRABALHEI_POR_SEMANA = 9001;

        // Strings
        String fooString = "Aqui está minha String!";

        // \n é um caractere de escape que inicia uma nova linha
        String barString = "Imprimir em uma nova linha?\nSem problemas!";
        // \t é um caractere de escape que adiciona um caractere de tabulação
        String bazString = "Você quer adicionar tabulação?\tSem problemas!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // Arrays
        //O tamanho do array precisa ser determinado na sua declaração
        //O formato para declarar um array é:
        //<tipo de dado> [] <nome da variável> = new <tipo de dado>[<tamanho do array>];
        int [] intArray = new int[10];
        String [] stringArray = new String[1];
        boolean [] booleanArray = new boolean[100];

        // Outra maneira de declarar e inicializar um array
        int [] y = {9000, 1000, 1337};

        // Indexando um array - Acessando um elemento
        System.out.println("intArray no índice 0: " + intArray[0]);

        // O primeiro termo de um array é o 0 e eles são mutáveis.
        intArray[1] = 1;
        System.out.println("intArray no índice 1: " + intArray[1]); // => 1

        // Outras estruturas que devem ser vistas
        // ArrayLists - São parecidos com os arrays, porém oferecem mais funcionalidades
        //             e o tamanho é mutável.
        // LinkedLists
        // Maps
        // HashMaps

        ///////////////////////////////////////
        // Operadores
        ///////////////////////////////////////
        System.out.println("\n->Operadores");

        int i1 = 1, i2 = 2; // Forma abreviada de escrever múltiplas declarações.

        // Aritmética é feita da forma convencional
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (0.5 arredondado para baixo)

        // Módulo
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // Operadores de comparação
        System.out.println("3 == 2? " + (3 == 2)); // => false
        System.out.println("3 != 2? " + (3 != 2)); // => true
        System.out.println("3 > 2? " + (3 > 2)); // => true
        System.out.println("3 < 2? " + (3 < 2)); // => false
        System.out.println("2 <= 2? " + (2 <= 2)); // => true
        System.out.println("2 >= 2? " + (2 >= 2)); // => true

        // Operadores bit-a-bit!
        /*
        ~       Complemento de um
        <<      Deslocamento a esquerda com sinal
        >>      Deslocamento a direita com sinal
        >>>     Deslocamento a direita sem sinal
        &       E bit-a-bit
        |       OU bit-a-bit
        ^       OU exclusivo bit-a-bit
        */

        // Incrementações
        int i = 0;
        System.out.println("\n->Inc/Dec-rementação");
        System.out.println(i++); //i = 1. Pós-Incrementação
        System.out.println(++i); //i = 2. Pre-Incrementação
        System.out.println(i--); //i = 1. Pós-Decrementação
        System.out.println(--i); //i = 0. Pre-Decrementação

        ///////////////////////////////////////
        // Estruturas de Controle
        ///////////////////////////////////////
        System.out.println("\n->Estruturas de Controle");

        // Os comandos If são parecidos com o da linguagem C
        int j = 10;
        if (j == 10){
            System.out.println("Eu serei impresso");
        } else if (j > 10) {
            System.out.println("Eu não");
        } else {
            System.out.println("Eu também não");
        }

        // O Loop While
        int fooWhile = 0;
        while(fooWhile < 100)
        {
            //System.out.println(fooWhile);
            //Incrementando o contador
            //Iteração feita 99 vezes, fooWhile 0->99
            fooWhile++;
        }
        System.out.println("Valor do fooWhile: " + fooWhile);

        // O Loop Do While
        int fooDoWhile = 0;
        do
        {
            //System.out.println(fooDoWhile);
            //Incrementando o contador
            //Iteração feita 99 vezes, fooDoWhile 0->99
            fooDoWhile++;
        }while(fooDoWhile < 100);
        System.out.println("Valor do fooDoWhile: " + fooDoWhile);

        // O Loop For
        int fooFor;
        //estrutura do loop for => for(<operação_de_início>; <condição>; <passo>)
        for(fooFor=0; fooFor<10; fooFor++){
            //System.out.println(fooFor);
            //Iteração feita 10 vezes, fooFor 0->9
        }
        System.out.println("Valor do fooFor: " + fooFor);

        // O Loop For Each
        // Itera automaticamente por um array ou lista de objetos.
        int[] fooList = {1,2,3,4,5,6,7,8,9};
        //estrutura do loop for each => for(<objeto> : <array_de_objeto>)
        //lê-se: para cada objeto no array
        //nota: o tipo do objeto deve ser o mesmo do array.

        for( int bar : fooList ){
            //System.out.println(bar);
            //Itera 9 vezes e imprime 1-9 em novas linhas
        }

        // Switch
        // Um switch funciona com os tipos de dados: byte, short, char e int
        // Ele também funciona com tipos enumerados (vistos em tipos Enum)
        // como também a classe String e algumas outras classes especiais
        // tipos primitivos: Character, Byte, Short e Integer
        int mes = 3;
        String mesString;
        switch (mes){
            case 1:
                    mesString = "Janeiro";
                    break;
            case 2:
                    mesString = "Fevereiro";
                    break;
            case 3:
                    mesString = "Março";
                    break;
            default:
                    mesString = "Algum outro mês";
                    break;
        }
        System.out.println("Resultado do Switch: " + mesString);

        // Condição de forma abreviada.
        // Você pode usar o operador '?' para atribuições rápidas ou decisões lógicas.
        // Lê-se "Se (declaração) é verdadeira, use <primeiro valor>
        // caso contrário, use <segundo valor>".
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println(bar); //Imprime A, pois a condição é verdadeira.


        ///////////////////////////////////////
        // Convertendo tipos de dados e Casting
        ///////////////////////////////////////

        //Conversão de Dados

        //Convertendo String para Inteiro.
        Integer.parseInt("123");//retorna uma versão inteira de "123".

        //Convertendo Inteiro para String
        Integer.toString(123);//retorna uma versão String de 123.

        // Para outras conversões confira as seguintes classes
        // Double
        // Long
        // String

        // Casting
        // Você pode também converter objetos java, há vários detalhes e
        // lida com alguns conceitos intermediários
        // Dê uma olhada no link:
        // http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


        ///////////////////////////////////////
        // Classes e Métodos
        ///////////////////////////////////////

        System.out.println("\n->Classes e Métodos");

        // (segue a definição da classe Bicicleta)

        // Use o new para instanciar uma classe
        Bicicleta caloi = new Bicicleta(); // Objeto caloi criado.

        // Chame os métodos do objeto
        caloi.aumentarVelocidade(3); // Você deve sempre usar métodos para modificar variáveis
        caloi.setRitmo(100);

        // toString é uma convenção para mostrar o valor deste objeto.
        System.out.println("informações de caloi: " + caloi.toString());

    } // Fim do método main
} // Fim da classe LearnJava


// Você pode incluir outras classes que não são públicas num arquivo .java


// Sintaxe de declaração de Classe.
// <public/private/protected> class <nome da classe>{
//   // atributos, construtores e todos os métodos.
//   // funções são chamadas de métodos em Java.
// }

class Bicicleta {

    // Atributos/Variáveis da classe Bicicleta.
    public int ritmo;          // Public: Pode ser acessada em qualquer lugar.
    private int velocidade;    // Private: Apenas acessível a classe.
    protected int catraca; // Protected: Acessível a classe e suas subclasses.
    String nome;               // default: Apenas acessível ao pacote.

    // Construtores são uma forma de criação de classes
    // Este é o construtor padrão.
    public Bicicleta() {
        catraca = 1;
        ritmo = 50;
        velocidade = 5;
        nome = "Bontrager";
    }

    // Este é um construtor específico (ele contém argumentos).
    public Bicicleta (int ritmoInicial, int velocidadeInicial, int catracaInicial, String nome) {
        this.catraca = catracaInicial;
        this.ritmo = ritmoInicial;
        this.velocidade = velocidadeInicial;
        this.nome = nome;
    }

    // Sintaxe de um método:
    // <public/private/protected> <tipo de retorno> <nome do método>(<args>) //  

    // Classes em Java costumam implementar métodos getters e setters para seus campos.

    // Sintaxe de declaração de métodos
    // <escopo> <tipo de retorno> <nome do método>(<args>) //   
    public int getRitmo() {
        return ritmo;
    }

    //  Métodos do tipo void não requerem declaração de retorno.
    public void setRitmo(int novoValor) {
        ritmo = novoValor;
    }

    public void setEquipamento(int novoValor) {
        catraca = novoValor;
    }

    public void aumentarVelocidade(int incremento) {
        velocidade += incremento;
    }

    public void diminuirVelocidade(int decremento) {
        velocidade -= decremento;
    }

    public void setNome(String novoNome) {
        nome = novoNome;
    }

    public String getNome() {
        return nome; // retorna um dado do tipo String.
    }

    //Método para mostrar os valores dos atributos deste objeto.
    @Override
    public String toString() {
        return "catraca: " + catraca +
                " ritmo: " + ritmo +
                " velocidade: " + velocidade +
                " nome: " + nome;
    }
} // fim classe Bicicleta

// Velocipede é uma subclasse de bicicleta.
class Velocipede extends Bicicleta {
    // (Velocípedes são bicicletas com rodas dianteiras grandes
    // Elas não possuem catraca.)

    public Velocipede(int ritmoInicial, int velocidadeInicial){
        // Chame o construtor do pai (construtor de Bicicleta) com o comando super.
        super(ritmoInicial, velocidadeInicial, 0, "PennyFarthing");
    }

    // Você pode marcar um método que você está substituindo com uma @annotation
    // Para aprender mais sobre o que são as annotations e sua finalidade
    // dê uma olhada em: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setEquipamento(int catraca) {
        catraca = 0;
    }

}

// Interfaces
// Sintaxe de declaração de Interface
// <nível de acesso> interface <nome-da-interface> extends <super-interfaces> {
// // Constantes
// // Declarações de método
//}

// Exemplo - Comida:
public interface Comestivel {
    public void comer(); // Qualquer classe que implementa essa interface, deve
                         // implementar este método.
}

public interface Digestivel {
    public void digerir();
    // Em Java 8, interfaces podem ter métodos default.
    // public void digerir() {
    //     System.out.println("digerindo ...");
    // }
}


// Agora podemos criar uma classe que implementa ambas as interfaces.
public class Fruta implements Comestivel, Digestivel {

    @Override
    public void comer() {
        // ...
    }

    @Override
    public void digerir() {
        // ...
    }
}

// Em Java, você pode estender somente uma classe, mas você pode implementar muitas
// interfaces. Por exemplo:
public class ClasseExemplo extends ExemploClassePai implements InterfaceUm,
    InterfaceDois {

    @Override
    public void InterfaceUmMetodo() {
    }

    @Override
    public void InterfaceDoisMetodo() {
    }

}

// Classes abstratas

// Sintaxe de declaração de classe abstrata
// <Nível de acesso> abstract <nome-da-classe-abstrata> extends <estende super-abstratas-classes> {
// // Constantes e variáveis
// // Declarações de método
//}

// Marcar uma classe como abstrata significa que ela contém métodos abstratos que devem
// ser definidos em uma classe filha. Semelhante às interfaces, classes abstratas não podem
// ser instanciadas, ao invés disso devem ser estendidas e os métodos abstratos
// definidos. Diferente de interfaces, classes abstratas podem conter uma mistura de
// métodos concretos e abstratos. Métodos em uma interface não podem ter um corpo,
// a menos que o método seja estático, e as variáveis sejam finais, por padrão, ao contrário de um
// classe abstrata. Classes abstratas também PODEM ter o método "main".

public abstract class Animal
{
    public abstract void fazerSom();

    // Método pode ter um corpo
    public void comer()
    {
        System.out.println("Eu sou um animal e estou comendo.");  
        //Nota: Nós podemos acessar variáveis privadas aqui.
        idade = 30;
    }

    // Não há necessidade de inicializar, no entanto, em uma interface
    // a variável é implicitamente final e, portanto, tem
    // de ser inicializada.
    protected int idade;

    public void mostrarIdade()
    {
        System.out.println(idade);  
    }

    //Classes abstratas podem ter o método main.
    public static void main(String[] args)
    {
        System.out.println("Eu sou abstrata");
    }
}

class Cachorro extends Animal
{

    // Nota: ainda precisamos substituir os métodos abstratos na
    // classe abstrata.
    @Override
    public void fazerSom()
    {
        System.out.println("Bark");
        // idade = 30;    ==> ERRO!  idade é privada de Animal
    }

    // NOTA: Você receberá um erro se usou a
    // anotação Override aqui, uma vez que java não permite
    // sobrescrita de métodos estáticos.
    // O que está acontecendo aqui é chamado de "esconder o método".
    // Vejá também este impressionante SO post: http://stackoverflow.com/questions/16313649/
    public static void main(String[] args)
    {
        Cachorro pluto = new Cachorro();
        pluto.fazerSom();
        pluto.comer();
        pluto.mostrarIdade();
    }
}

// Classes Finais

// Sintaxe de declaração de classe final
// <nível de acesso> final <nome-da-classe-final> {
// // Constantes e variáveis
// // Declarações de método
//}

// Classes finais são classes que não podem ser herdadas e são, portanto, um
// filho final. De certa forma, as classes finais são o oposto de classes abstratas,
// porque classes abstratas devem ser estendidas, mas as classes finais não podem ser
// estendidas.
public final class TigreDenteDeSabre extends Animal
{
    // Nota: Ainda precisamos substituir os métodos abstratos na
    // classe abstrata.
    @Override
    public void fazerSom();
    {
        System.out.println("Roar");
    }
}

// Métodos Finais
public abstract class Mamifero()
{
    // Sintaxe de Métodos Finais:
    // <modificador-de-acesso> final <tipo-de-retorno> <nome-do-método>(<argumentos>)

    // Métodos finais, como classes finais, não podem ser substituídos por uma classe filha,
    // e são, portanto, a implementação final do método.
    public final boolean EImpulsivo()
    {
        return true;
    }
}


// Tipo Enum
//
// Um tipo enum é um tipo de dado especial que permite a uma variável ser um conjunto de constantes predefinidas. A 
// variável deve ser igual a um dos valores que foram previamente definidos para ela.
// Por serem constantes, os nomes dos campos de um tipo de enumeração estão em letras maiúsculas.
// Na linguagem de programação Java, você define um tipo de enumeração usando a palavra-chave enum. Por exemplo, você poderia
// especificar um tipo de enum dias-da-semana como:

public enum Dia {
    DOMINGO, SEGUNDA, TERÇA, QUARTA,
    QUINTA, SEXTA, SABADO 
}

// Nós podemos usar nosso enum Dia assim:

public class EnumTeste {

    // Variável Enum
    Dia dia;

    public EnumTeste(Dia dia) {
        this.dia = dia;
    }

    public void digaComoE() {
        switch (dia) {
            case SEGUNDA:
                System.out.println("Segundas são ruins.");
                break;

            case SEXTA:
                System.out.println("Sextas são melhores.");
                break;

            case SABADO: 
            case DOMINGO:
                System.out.println("Finais de semana são os melhores.");
                break;

            default:
                System.out.println("Dias no meio da semana são mais ou menos.");
                break;
        }
    }

    public static void main(String[] args) {
        EnumTeste primeiroDia = new EnumTeste(Dia.SEGUNDA);
        primeiroDia.digaComoE(); // => Segundas-feiras são ruins.
        EnumTeste terceiroDia = new EnumTeste(Dia.QUARTA);
        terceiroDia.digaComoE(); // => Dias no meio da semana são mais ou menos.
    }
}

// Tipos Enum são muito mais poderosos do que nós mostramos acima.
// O corpo de um enum pode incluir métodos e outros campos.
// Você pode ver mais em https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

```

## Leitura Recomendada

Os links fornecidos aqui abaixo são apenas para ter uma compreensão do tema, use o Google e encontre exemplos específicos.

Outros tópicos para pesquisar:

* [Tutorial Java para Sun Trail / Oracle](http://docs.oracle.com/javase/tutorial/index.html)

* [Modificadores de acesso do Java](http://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)

* [Coceitos de Programação Orientada à Objetos](http://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [Herança](http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [Polimorfismo](http://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [Abstração](http://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [Exceções](http://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [Interfaces](http://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [Tipos Genéricos](http://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [Conversões de código Java](http://www.oracle.com/technetwork/java/codeconv-138413.html)

Livros:

* [Use a cabeça, Java] (http://www.headfirstlabs.com/books/hfjava/)

Apostila:

* [Java e Orientação a Objetos] (http://www.caelum.com.br/apostila-java-orientacao-objetos/)

* [Java para Desenvolvimento Web] (https://www.caelum.com.br/apostila-java-web/)
