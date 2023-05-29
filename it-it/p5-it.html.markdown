---
category: tool
tool: p5
filename: p5.js
contributors:
    - ['Amey Bhavsar', 'https://github.com/ameybhavsar24']
    - ['Claudio Busatto', 'https://github.com/cjcbusatto']
translators:
    - ["Alessio Benvenuti", "https://github.com/alessiobenvenuti"]
lang: it-it
---

p5.js è una libreria JavaScript che nasce con l'obiettivo originale di [Processing] (http://processing.org), per rendere il codice accessibile a artisti, designer, educatori e principianti e reinterpretarlo per il web di oggi.
Poiché p5 è una libreria JavaScript, dovresti prima imparare [Javascript] (https://learnxinyminutes.com/docs/javascript/).

```js
///////////////////////////////////
//p5.js ha due importanti funzioni con cui lavorare.
function setup() {
	// la funzione setup viene eseguita una sola volta quando viene caricata la finestra
}
function draw() {
    // la funzione draw viene chiamata per ogni singolo frame. Questo significa che per un frameRate(30) verrebbe chiamata 30 volte ogni secondo.
}
// il codice seguente spiega tutte le funzionalità 

function setup() {
    createCanvas(640, 480); // crea un nuovo elemento canvas con 640 px di larghezza e 480 px di altezza
    background(128); // cambia il colore di sfondo del canvas, può accettare valori rgb come background(100,200,20) altrimenti valori in scala di grigi come background(0) = nero o background(255) = bianco
}

function draw() {
    ellipse(10, 10, 50, 50); // crea un'ellisse a 10px da sinistra e 10px dall'alto con larghezza e altezza pari a 50 ciascuna, quindi sostanzialmente è un cerchio.
    //Ricorda, in p5.js l'origine è nell'angolo in alto a sinistra del canvas

    if (mouseIsPressed) {
        // mouseIsPressed è una variabile booleana che diventa vera se il pulsante del mouse viene premuto in quel momento

        fill(0); // la funzione fill() si riferisce al colore interno o al colore di riempimento di qualsiasi forma definita dopo la funzione stessa
    } else {
        fill(255); // puoi dare valori rgb come fill(72, 240, 80) per ottenere i colori, altrimenti un singolo valore determina la scala di grigi dove fill(255) sta per #FFF (bianco) e fill(0) sta per #000 (nero)
    }

    ellipse(mouseX, mouseY, 80, 80);
    // mouseX è la coordinata x della posizione corrente del mouse e mouseY è la coordinata y della posizione corrente del mouse

    // il codice sovrastante crea un cerchio in base alla posizione del mouse e lo riempie di bianco o nero in base allo stato di mouseIsPressed
}
```

## Approfondimenti

- [p5.js | Inizia](http://p5js.org/get-started/) documentazione ufficiale
- [Code! Programmazione per principianti con p5.js - YouTube](https://www.youtube.com/watch?v=yPWkPOfnGsw&vl=en) Introduzione e sfide di programmazione utilizzando Processing e p5.js di Coding Train



