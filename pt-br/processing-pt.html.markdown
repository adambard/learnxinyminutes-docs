---
language: processing
filename: learnprocessing.pde
contributors:
    - ["Phone Thant Ko", "http://github.com/phonethantko"]
    - ["Divay Prakash", "https://github.com/divayprakash"]
translators:
	- ["Kemel Zaidan", "https://github.com/kemelzaidan"]
lang: pt-br
---

## Introdução

Processing é uma linguagem de programação para criação de artes digitais e
conteúdo multimídia, permitindo que não programadores aprendam os fundamentos da 
programação computacional em um contexto visual.

Embora a linguagem seja baseada na linguagem Java, sua sintaxe foi amplamente
influenciado por ambas as sintaxes Java e Javascript. [Veja mais aqui](https://processing.org/reference/)

A linguagem é tipada estaticamente e também vem com a sua IDE oficial para
compilar e executar os scripts.

```
/* ---------
   Comentários
   ---------
*/

// Comentário de linha única começa com //

/*
   Como o Processing é baseado em Java,
   a sintaxe para seus comentários é a mesma do Java (como você deve ter notado acima)!
   Comentários de várias linhas são agrupados como visto aqui.
*/

/* ---------------------------------------
   Escrevendo e executando programas em Processing
   ---------------------------------------
*/

// No Processing, o ponto de entrada do programa é uma função chamada setup() com um
// tipo de retorno void.
// Observação! A sintaxe é muito semelhante à do C++.
void setup() {
  // Isso imprime a saída clássica "Hello World!" para o console quando executado.
  println("Olá Mundo!"); // Outra linguagem com cilada para ponto-e-vígula, não é?
}

// Normalmente, colocamos todos os códigos estáticos dentro do método setup() como o próprio nome
// sugere, uma vez que é executado apenas uma vez.
// Pode variar da definição das cores de fundo, ou o tamanho da tela.
background(color); //define a cor do fundo
size(largura,altura,[renderizador]); // define o tamanho da tela com parâmetro
// opcional para definir o renderizador
// Você verá mais sobre isso ao longo deste documento.

// Se você deseja executar os códigos indefinidamente, eles devem ser colocados dentro do método draw()
// draw() deve existir caso você queira que o código seja executado continuamente e, obviamente,
// só pode haver um método draw().
int = 0;
void draw(){
  // Este bloco de código faz um loop para sempre até parar
  imprima(i);
  i++; // Operador de incremento!
}

// Agora que sabemos como escrever o script de trabalho e como executá-lo,
// continuaremos a explorar quais tipos de dados e coleções são suportados no
// Processing.

/* ------------------------
   Tipos de dados e coleções
   ------------------------
*/

// De acordo com as Referências do Processing, ele suporta 8 tipos primitivos
// de dados da seguinte forma.

boolean valorBoleano = true; // Boleano
byte valorByteDeA = 23; // Byte
char valorCharDeA = 'A'; // Caractere
color valorDeCorBrancoM = color(255, 255, 255); // Cor (especificada usando
// método color())
color valorDeCorBrancoH = #FFFFFF; // Cor (especificada usando valor de hash)
int valor = 5; // Inteiro (Número sem decimais)
long valorLongo = 2147483648L; // "L" é adicionado ao número para marcá-lo como um longo
float valorFloat = 1,12345; // Float (números de ponto flutuante de 32 bits)
double valorDouble = 1,12345D; // Double (números de ponto flutuante de 64 bits)

// NOTA!
// Embora os tipos de dados "long" e "double" funcionem na linguagem,
// funções do Processing não usam esses tipos de dados, portanto
// eles precisam ser convertidos em tipos de dados "int" e "float", respectivamente,
// usando a sintaxe (int) e (float) antes de passar para uma função.

// Existem vários tipos de dados compostos que estão disponíveis por padrão para uso
// no Processing.
// Primeiramente, farei um resumo dos mais usados ​​para economizar tempo.

// String
// Enquanto o tipo de dados char usa '', o tipo de dados String usa "" - aspas duplas.
string stringExemplo = "Olá, Processing!";
// String também pode ser construída a partir de um array de tipos de dados char. Nós vamos
// discutir array muito em breve.
char fonte = {'H', 'E', 'L', 'L', 'O'};
string stringDeFonte = new String(source); // HELLO
// Como em Java, strings podem ser concatenadas usando o operador "+".
print("Olá " + "Mundo!"); // Olá Mundo!

// Array
// Arrays em Processing podem conter quaisquer tipos de dados, incluindo os próprios objetos.
// Como os arrays são semelhantes aos objetos, eles devem ser criados com a palavra-chave
// "new".
int[] arrayInt = new int[5];
int[] arrayIntComValores ​​= {1, 2, 3}; // Você também pode preencher com dados.

// Lista de Arrays
// As funções são semelhantes às do array; arraylists podem conter qualquer tipo de dados.
// A única diferença é que as listas de matrizes são redimensionadas dinamicamente, pois é uma forma de
// implementação de matriz redimensionável da interface "List" do Java .
ArrayList<Integer> intArrayList = new ArrayList<Integer>();

// Objeto
// Como é baseado em Java, o Processing suporta programação orientada a objetos.
// Isso significa que você pode basicamente definir quaisquer tipos de dados de sua preferência e manipulá-los
// para suas necessidades.
// Claro, uma classe tem que ser definida antes para o objeto que você quer.
// Formato --> NomeClasse NameInstancia
UmaClasseQualquer meuObjeto // então instancia mais tarde
//ou
UmaClasseQualquer meuObjetoInstanciado = new UmaClasseQualquer();

// O Processing surge com mais coleções (ex. - Dicionários e Listas) por
// padrão, por uma questão de simplicidade, vou deixá-los fora de discussão aqui.

/* ------------
   Matemática
   ------------
*/

// Aritmética
1 + 1 // 2
2 - 1 // 1
2 * 3 // 6
3/2 // 1
3,0 / 2 // 1,5
3,0% 2 // 1,0

// O Processing também vem com um conjunto de funções que simplificam operações matemáticas.
float f = sq(3); // f = 9.0
float p = pow(3, 3); // p = 27.0
int a = abs(-13); // a = 13
int r1 = round(3.1); // r1 = 3
int r2 = round(3.7); // r2 = 4
float sr = sqrt(25); // sr = 5.0

// Vetores
// O Processing fornece uma maneira fácil de implementar vetores em seu ambiente
// usando a classe PVector. Ela pode descrever um vetor bi ou tridimensional e
// vem com um conjunto de métodos que são úteis para operações com matrizes.
// Você pode encontrar mais informações sobre a classe PVector e suas funções aqui.
// (https://processing.org/reference/PVector.html)

// Trigonometria
// O processamento também suporta operações trigonométricas fornecendo um conjunto de
// funções. sin(), cos(), tan(), asin(), acos(), atan() e também degrees()
// e radians() para conversão conveniente.
// No entanto, essas funções usam o ângulo em radianos como parâmetro, então é necessário
// converter previamente.
float um = sin(PI/2); // um = 1.0
// Como você deve ter notado, existe um conjunto de constantes para usos trigonométricos;
// PI, HALF_PI, QUARTER_PI e assim por diante...

/* -------------
   Controle de fluxo
   -------------
*/

// Declarações Condicionais
// Instruções If - A mesma sintaxe das instruções if em Java.
if (autor.getAppearance().equals("quente")) {
  print("Narcisismo no seu melhor!");
} senão {
  // Você pode verificar outras condições aqui.
  print("Algo está realmente errado aqui!");
}
// Um ​​atalho para instruções if-else também pode ser usado.
int = 3;
Valor da string = (i > 5) ? "Grande" : "Pequena"; // "Pequena"

// A estrutura switch-case pode ser usada para verificar várias condições de forma concisa.
// É importante usar a instrução break. Se a instrução `break`
// não existe o programa executa todos os casos a seguir após um caso ser verdadeiro.
int valor = 2;
switch(valor) {
  case 0:
    print("Nada!"); // Isso não é executado.
    break; // Salta para a próxima instrução
  case 1:
    print("Chegando lá..."); // Isso novamente não é executado.
    break;
  case 2:
    print("Bravo!"); // Esta linha é executada.
    break;
  default:
    print("Não encontrado!"); // Esta linha é executada se nosso valor for algum outro valor.
    break;
}

// Declarações iterativas
// Declarações For - Novamente, a mesma sintaxe que em Java
for(int i = 0; i < 5; i++){
  print(i); // imprime de 0 a 4
}

// Declarações While - Novamente, nada de novo se você estiver familiarizado com a sintaxe Java.
int j = 3;
while(j > 0) {
  print(j);
  j--; // Isso é importante para evitar que o código seja executado indefinidamente.
}

// loop()| noLoop() | redraw() | exit()
// Estas são mais funções específicas do Processing para configurar o fluxo do programa.
loop(); // permite que o método draw() seja executado para sempre enquanto
noLoop(); // só permite que ele seja executado uma vez.
redraw(); // executa o método draw() mais uma vez.
exit(); // Isso para o programa. É útil para programas com draw()
// rodando continuamente.
```

## Desenho com Processing

Como você já deve ter entendido o básico da linguagem, vamos agora
veja a melhor parte do Processing - DESENHAR.

```
/* ------
   Formas
   ------
*/

// Formas 2D

// Ponto
point(x, y); // No espaço 2D
point(x, y, z); // No espaço 3D
// Desenha um ponto no espaço de coordenadas.

// Linha
line(x1, y1, x2, y2); // No espaço 2D
line(x1, y1, z1, x2, y2, z2); // No espaço 3D
// Desenha uma linha conectando dois pontos definidos por (x1, y1) e (x2, y2).

// Triângulo
triangle(x1, y1, x2, y2, x3, y3);
// Desenha um triângulo conectando três pontos definidos por parâmetros de coordenadas.

// Retângulo
rect(a, b, c, d, [r]); // Com parâmetro opcional definindo o raio de todos os cantos
rect(a, b, c, d, [tl, tr, br, bl]); // Com conjunto opcional de parâmetros definindo
// raio de cada canto
// Desenha um retângulo com {a, b} como coordenada superior esquerda e c e d como largura
// e altura respectivamente.

// Quad
quad(x, y, x2, y2, x3, y3, x4, y4);
// Desenha um quadrilátero com parâmetros que definem as coordenadas de cada canto
// ponto.

// Elipse
ellipse(x, y, largura, altura);
// Desenha um eclipse no ponto {x, y} com largura e altura especificadas.

// Arco
arc(x, y, largura, altura, inicio, fim, [modo]);
// Enquanto os primeiros quatro parâmetros são autoexplicativos,
// início e fim definem os ângulos que o arco começa e termina (em radianos).
// O parâmetro opcional [mode] define o preenchimento;
// PIE dá o contorno de torta, CHORD dá o contorno reto e OPEN é como
// CHORD porém sem contorno

// Curvas
// O processamento fornece duas implementações de curvas; usando curve() e bezier().
// Como pretendo manter isso simples, não vou discutir mais detalhes.
// No entanto, se você quiser implementá-lo em seu sketch, aqui estão as referências:
// (https://processing.org/reference/curve_.html)
// (https://processing.org/reference/bezier_.html)

// Formas 3D

// espaço 3D
pode ser configurado definindo "P3D" para o parâmetro do renderizador no
// método size().
size(largura, altura, P3D);
// No espaço 3D, você terá que traduzir para a coordenada específica para
// renderiza as formas 3D.

// Caixa
box(tamanho); // Cubo com o mesmo comprimento definido pelo tamanho
caixa(w, h, d); // Caixa com largura, altura e profundidade definidas separadamente

// Esfera
sphere(raio); // Seu tamanho é definido usando o parâmetro raio
// O mecanismo por trás da renderização das esferas é implementado por triângulos em mosaico.
// Dito isso, o nível de detalhe sendo renderizado é controlado pela função
// sphereDetail(res)
// Mais informações aqui: (https://processing.org/reference/sphereDetail_.html)

// Formas irregulares
// E se você quiser desenhar algo que não foi disponibilizado pelo Processing
// funções?
// Você pode usar beginShape(), endShape(), vertex(x,y) para definir formas por
// especificando cada ponto. Mais informações aqui:
// (https://processing.org/reference/beginShape_.html)
// Você também pode usar formas personalizadas usando a classe PShape:
// (https://processing.org/reference/PShape.html)

/* ---------------
   Transformações
   ---------------
*/

// As transformações são particularmente úteis para acompanhar o espaço de coordenadas
// e os vértices das formas que você desenhou. Particularmente;
// métodos de pilha de matrizes; pushMatrix(), popMatrix() e translate(x,y)
pushMatriz(); // Salva o sistema de coordenadas atual na pilha
// ... aplique todas as transformações aqui ...
popMatriz(); // Restaura o sistema de coordenadas salvo
// Usando-os, o sistema de coordenadas pode ser preservado e visualizado sem
// causar qualquer conflito.

// Traduzir
translate(x,y); // Traduz para o ponto{x, y} ou seja - configurando a origem para esse ponto
translate(x, y, z); // Contraparte 3D da função

// Rotacionar
rotate(ângulo); // Gira a quantidade especificada pelo parâmetro ângulo
// Possui 3 contrapartes 3D para realizar a rotação, uma para cada dimensão: rotateX(ângulo), rotateY(ângulo), rotateZ(ângulo)

// Escala
scale(s); // Dimensiona o sistema de coordenadas expandindo ou contraindo-o.

/* --------------------
   Estilo e texturas
   --------------------
*/

// Cores
// Como discuti anteriormente, a cor de fundo pode ser configurada usando a função
// background(). Você pode definir a cor de um objeto de antemão e depois
// passar para a função como um argumento.
color c = cor(255, 255, 255); // BRANCO!
// Por padrão, o Processing usa o esquema de cores RGB, mas pode ser configurado para
// HSB usando colorMode(). Leia mais aqui:
// (https://processing.org/reference/colorMode_.html)
background(c); // Até agora, a cor de fundo deve ser branca.
// Você pode usar a função fill() para selecionar a cor para preencher as formas.
// Tem que ser configurado antes de você começar a desenhar formas para que as cores fiquem
// aplicadas.
fill(color(0, 0, 0));
// Se você quiser apenas colorir os contornos das formas, você pode usar
// função stroke().
stroke(255, 255, 0, 200); // cor do traço definida para amarelo com transparência
// definido para um valor menor.

// Imagens
// O processamento pode renderizar imagens e usá-las de várias maneiras. Principalmente armazenado como
// Tipo de dados PImage.
filter(sombreador); // O processamento suporta várias funções de filtro para manipulação de imagens.
texture(imagem); // PImage pode ser passado em argumentos para mapeamento de textura das formas.
```

Se você quiser levar as coisas adiante, há mais coisas que o Processing tem o poder de fazer.
Renderizar modelos, shaders e outros enfeites. Há muito para se cobrir em uma
documentação curta, então vou deixá-los aqui. Se você se interessar,
por favor verifique as referências.

```
// Antes de prosseguirmos, vou falar um pouco mais sobre como importar bibliotecas
// para que você possa estender a funcionalidade do Processing para outros horizontes.

/* -------
   Importações
   -------
*/

// As possibilidades do Processing pode ser extendidas ainda mais quando importamos bibliotecas
// e pacotes em nossos esboços.
// A instrução de importação pode ser escrita como abaixo na parte superior do código-fonte.
import processing.something.*;
```

## VAC?

Vamos ao código? Vamos sujar as mãos!

Vamos ver um exemplo do openprocessing para visualizar o quanto o Processing é
capaz de fazer com poucas linhas de código.

Copie o código abaixo em seu IDE do Processing e veja a mágica.

```
// Isenção de responsabilidade: eu não escrevi este programa porque atualmente estou ocupado com meu
// estágio e este sketch é adaptado do openprocessing pois mostra
// algo legal com um código simples.
// Recuperado de: (https://www.openprocessing.org/sketch/559769)

float theta;
float a;
float col;
float num;

void setup() {
  size(600,600);
}

void draw() {
  background(#F2F2F2);
  translate(width/2, height/2);
  theta = map(sin(millis()/1000.0), -1, 1, 0, PI/6);

  float num=6;
  for (int i=0; i<num; i++) {
    a =350;
    rotate(TWO_PI/num);
    branch(a);
  }

}

void branch(float len) {
  col=map(len, 0, 90, 150, 255);
  fill(col, 0, 74);
  stroke (col, 0, 74);
  line(0, 0, 0, -len);
  ellipse(0, -len, 3, 3);
  len *= 0.7;

  if (len>30) {
    pushMatrix();
    translate(0, -30);
    rotate(theta);
    branch(len);
    popMatrix();

    pushMatrix();
    translate(0, -30);
    rotate(-theta);
    branch(len);
    popMatrix();

  }
}
```

O processamento é fácil de aprender e é particularmente útil para criar
conteúdo (mesmo em 3D) sem ter que digitar muitos códigos. É tão simples
que você pode ler o código e ter uma ideia aproximada do fluxo do programa.

No entanto, isso não se aplica quando você introduz bibliotecas externas, pacotes
e até mesmo suas próprias aulas. (Confie em mim! O processamento de projetos pode ficar realmente monstruoso...)

## Alguns recursos úteis

  - [Site de processamento](http://processing.org)
  - [Processando esboços](http://openprocessing.org)