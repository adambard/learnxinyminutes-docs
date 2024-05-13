---
category: tool
tool: p5
filename: p5-pt.js
contributors:
    - ['Amey Bhavsar', 'https://github.com/ameybhavsar24']
    - ['Claudio Busatto', 'https://github.com/cjcbusatto']
translators:
    - ["Adaías Magdiel", "https://adaiasmagdiel.com/"]
lang: pt-br
---

p5.js é biblioteca JavaScript que carrega o princípio original do [Processing](https://processing.org), tornar a programação acessível à artistas, designers, professores e alunos e traz esse lema para os dias atuais.
Como o p5.js é uma biblioteca JavaScript, você precisa aprender [Javascript](https://learnxinyminutes.com/docs/pt-br/javascript-pt/) primeiro.

Para rodar códigos com p5.js online, você pode acessar o [editor online](https://editor.p5js.org/).

```js
///////////////////////////////////
// p5.js possui duas funções importantes para se trabalhar.

function setup() {
  // a função setup é carregada apenas uma vez, quando a janela é carregada
}
function draw() {
  // a função draw é chamada a cada frame
  // se o framerate é definido como 30, essa função vai ser chamada 30 vezes por segundo
}

// o seguinte código explica todas as funcionalidades

function setup() {
  createCanvas(640, 480); // cria um novo elemento canvas com 640px de largura e 480px de altura
  background(128); // muda a cor do background para rgb(128, 128, 128)
  // background('#aaf') // Você pode usar hexadecimal como código de cor, também
}

function draw() {
  // normalmente, a função `background` é chamada na função `draw` para limpar a tela
  background('#f2f2fc');

  // cria uma elipse a partir de 10px do topo e 10px da esquerda, com uma largura e altura de 37
  ellipse(10, 10, 37, 37);
  // no p5.js a origem é sempre no canto à esquerda e no topo do canvas

  if (mouseIsPressed) {
    // mouseIsPressed é uma variável booleana que é verdadeira quando o mouse está pressionado e falso quando é liberado

    fill(0); // define a cor de preenchimento, que permanece até ser modificada novamente
  } else {
    fill(255, 255, 255, 240); // fill(a, b, c, d) define a cor de preenchimento para rgba(a, b, c, d)
  }

  ellipse(mouseX, mouseY, 80, 80);
  // mouseX e mouseY são as coordenadas da posição do mouse, x e y respectivamente
  // o código acima cria uma elipse embaixo da posição do mouse e preenche com branco ou preto


  // algumas outras formas primitivas em 2d que você pode utilizar:
  rect(9, 3, 23, 26); // x, y, largura, altura
  noFill(); // define a cor de preenchimento como transparente
  triangle(100, 400, 130, 200, 200, 300); // x1, y1, x2, y2, x3, y3
  point(100, 300); // cria um ponto na posição x, y
  // existem outras formas, mas elas são mais complexas.
}

/** Animação de bolas quicando
 * Você pode copiar e colar esse código no editor online, acessando:
 * https://editor.p5js.org/
 */
class Ball {
  constructor(x, y, xvel, yvel, radius, col) {
    this.position = createVector(x, y); // cria um objeto p5.Vector que armazena os valores de x e y
    this.velocity = createVector(xvel, yvel); // criar um objeto p5.Vector armazenando a velocidade
    this.radius = radius;
    this.col = col; // p5.js já utiliza a palavra color, então usaremos "col" no nosso exemplo
  }

  update() {
    this.position.add(this.velocity); // você pode somar vetores usando a função p5.Vector.add(p5.Vector)
    if (this.position.x + this.radius > width) {
      // muda a direção da bola se ela bater nas paredes
      this.velocity.x *= -1;
    }
    if (this.position.x - this.radius < 0) {
      this.velocity.x *= -1;
    }
    if (this.position.y + this.radius > height) {
      this.velocity.y *= -1;
    }
    if (this.position.y - this.radius < 0) {
      this.velocity.y *= -1;
    }
  }

  render() {
    // com base nesses exemplos, você já deve ser capaz de entender o que essa função está fazendo
    fill(this.col);
    ellipse(this.position.x, this.position.y, this.radius);
  }
}

let numBalls = 23;
let balls = [];

function setup() {
  createCanvas(400, 400); // largura, altura
  for (let i = 0; i < numBalls; i++) {
    let r = random(255); // um número aleatório entre 0 e 255
    let g = random(255);
    let b = random(255);

    balls.push(
      new Ball(
        random(30, width), // posição x
        random(height), // posição y
        random(-4, 4), // velocidade x
        random(-4, 4), // velocidade y
        random(4, 10), // raio
        color(r, g, b) // cor de preenchimento para a bola
      )
    );
  }
}

function draw() {
  background(255);
  for (let ball of balls) {
    ball.update();
    ball.render();
  }
}

// Até agora, só vimos o modo de renderização padrão.
// Dessa vez, usaremos o modo de renderização "webgl".


function setup() {
  createCanvas(400, 400, WEBGL); // largura, altura, modo de renderização
}

function draw() {
  background(0);

  stroke('#000');
  fill('#aaf');

  // rotaciona entorno dos eixos x, y e z com base na quantidade de frames dividido por 50
  rotateX(frameCount / 50);
  rotateY(frameCount / 50);
  rotateZ(frameCount / 50);
  // frameCount é uma variável do p5.js que armazena a quantidade de frames que já ocorreu

  box(50, 50, 50); // largura, altura, profundidade
}
```

## Saiba Mais

- [Começando com p5.js](http://p5js.org/get-started/) A documentação oficial
- [Code! Programming for Beginners with p5.js - YouTube](https://www.youtube.com/watch?v=yPWkPOfnGsw&vl=en) Introdução e desafios usando Processing e p5.js por Coding Train
- [The Coding Train](https://codingtra.in/) Um site com exemplos feito utilizando p5.js e Processing

## Fonte

- [p5.js - Código Fonte](https://github.com/processing/p5.js)
- [p5.sound.js](https://github.com/processing/p5.js-sound)
