---
category: tool
tool: p5
contributors:
    - ['Amey Bhavsar', 'https://github.com/ameybhavsar24']
    - ['Claudio Busatto', 'https://github.com/cjcbusatto']
translator:
    - ['Victor Williams',"https://github.com/Vaporjawn"]
filename: p5.js
---

p5.js es una biblioteca de JavaScript que comienza con el objetivo original de [Processing] (https://processing.org), hacer que la codificación sea accesible para artistas, diseñadores, educadores y principiantes, y reinterpreta esto para la web de hoy.
Como p5 es una biblioteca de JavaScript, primero debe aprender [Javascript] (https://learnxinyminutes.com/docs/javascript/).

```js
///////////////////////////////////
// p5.js tiene dos funciones importantes para trabajar.

function setup() {
    // la función de configuración se ejecuta solo una vez cuando se carga la ventana
}
function draw() {
    //  la función de dibujar se llama cada cuadro individual. Esto significa que para un frameRate (30) se llamaría 30 veces por segundo.
}

// el siguiente código explica todas las funciones

function setup() {
    createCanvas(640, 480); // crea un nuevo elemento de lienzo con 640 px de ancho como 480 px de alto
    background(128); // cambia el color de fondo del lienzo, puede aceptar valores rgb como fondo (100,200,20) o valores de escala de grises como fondo (0) = negro o fondo (255) = blanco
}

function draw() {
    ellipse(10, 10, 50, 50); // crea una elipse en los 10 píxeles desde la izquierda y 10 píxeles desde la parte superior con un ancho y una altura de 50 cada uno, por lo que es básicamente un círculo.
    //recuerde que en p5.js el origen está en la esquina superior izquierda del lienzo

    if (mouseIsPressed) {
        // mouseIsPressed es una variable booleana que cambia a verdadero si se presiona el botón del mouse en ese instante

        fill(0); // relleno se refiere al color interno o al color de relleno de cualquier forma que vaya a dibujar a continuación
    } else {
        fill(255); // puede proporcionar valores rgb como relleno (72, 240, 80) para obtener colores, de lo contrario, un solo valor determina la escala de grises donde el relleno (255) representa #FFF (blanco) y el relleno (0) representa # 000 (negro)
    }

    ellipse(mouseX, mouseY, 80, 80);
    // mouseX es la coordenada x de la posición actual del mouse y mouseY es la coordenada y de la posición actual del mouse

    // el código anterior crea un círculo donde sea que se encuentre la posición actual del mouse y lo llena en blanco o negro según el mouse
}
```

## Otras lecturas

- [p5.js | empezar] (http://p5js.org/get-started/) La documentación oficial
- [Código! Programación para principiantes con p5.js - YouTube] (https://www.youtube.com/watch?v=yPWkPOfnGsw&vl=en) Introducción y desafíos de codificación usando Processing y p5.js de Coding Train