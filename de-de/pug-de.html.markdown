---
language: Pug
contributors:
  - ["Michael Warner", "https://github.com/MichaelJGW"]
filename: lernepug-de.pug
translators:
    - ["denniskeller", "https://github.com/denniskeller"]
lang: de-de
---

## Erste Schritte mit Pug

Pug ist eine kleine Sprache, die zu HTML kompiliert. Sie hat eine
saubere Syntax mit zusätzlichen Funktionen wie if Anweisungen und Schleifen.
Sie kann auch als serverseitige Templatingsprache für Serversprachen
wie NodeJS verwendet werden.

### Die Sprache
```pug

//- Einzeilenkommentar

//- Mehrzeiliger
    Kommentar

//- ---TAGS---
//- Grundlagen
div
//- <div></div>
h1
//- <h1></h1>
mein-benutzerdefiniertesTag
//- <mein-benutzerdefiniertesTag></mein-benutzerdefiniertesTag>

//- Geschwister
div
div
//- <div></div>
    <div></div>

//- Kind
div
  div
//- <div>
      <div></div>
    </div>

//- Text
h1 Hallo Welt
//- <h1>Hallo Welt</h1>

//- Multizeilentext
div.
  Hallo
  Welt
//- <div>
      Hallo
      Welt
    </div>

//- ---ATTRIBUTE---
div(class="meine-klasse" id="meine-id" mein-benutzerdefiniertes-attr="data" enabled)
//- <div class="meine-klasse" id="meine-id" mein-benutzerdefiniertes-attr="data" enabled></div>

//- Kurzhand
span.meine-klasse
//- <span class="meine-klasse"></span>
.meine-klasse
//- <div class="meine-klasse"></div>
div#meine-id
//- <div id="meine-id"></div>
div#meine-id.meine-klasse
//- <div class="meine-klasse" id="meine-id"></div>


//- ---JS---
- const sprache = "pug";

//- Multizeilen JS
-
  const srache = "pug";
  const cool = true;

//- JS Klassen
- const meineKlasse = ['class1', 'class2', 'class3']
div(class=meineKlasse)
//- <div class="class1 class2 class3"></div>

//- JS Stil
- const meineStile = {'color':'white', 'background-color':'blue'}
div(styles=meineStile)
//- <div styles="{&quot;color&quot;:&quot;white&quot;,&quot;background-color&quot;:&quot;blue&quot;}"></div>

//- JS Attributte
- const meineAttribute = {"src": "foto.png", "alt": "meine Bilder"}
img&attributes(meineAttribute)
//- <img src="foto.png" alt="meine Bilder">
- let deaktiviert = false
input(type="text" disabled=deaktiviert)
//- <input type="text">
- deaktiviert = true
input(type="text" disabled=deaktiviert)
//- <input type="text" disabled>

//- JS Templating
- const name = "Bob";
h1 Hi #{name}
h1= name
//- <h1>Hi Bob</h1>
//- <h1>Bob</h1>

//- ---Schleifen---

//- 'each' und 'for' machen das Selbe. Wir werden nur 'each' verwenden.

each value, i in [1,2,3]
  p=value
//-
  <p>1</p>
  <p>2</p>
  <p>3</p>

each value, index in [1,2,3]
  p=value + '-' + index
//-
  <p>1-0</p>
  <p>2-1</p>
  <p>3-2</p>

each value in []
  p=value
//-

each value in []
  p=value
else
  p Keine Werte sind hier

//- <p>Keine Werte sind hier</p>

//- ---BEDINGUNGEN---

- const zahl = 5
if zahl < 5
  p zahl ist kleiner als 5
else if zahl > 5
  p zahl ist größer als  5
else
  p zahl ist 5
//- <p>zahl ist 5</p>

- const bestellungsStatus = "Ausstehend";
case bestellungsStatus
  when "Ausstehend"
    p.warn Deine Bestellung steht noch aus
  when "Abgeschlossen"
    p.success Bestellung ist abgeschlossen.
  when -1
    p.error Ein Fehler ist aufgetreten
  default
    p kein Bestellprotokoll gefunden
//- <p class="warn">Deine Bestellung steht noch aus</p>

//- --INCLUDE--
//- File path -> "includes/nav.png"
h1 Firmenname
nav
  a(href="index.html") Home
  a(href="about.html") Über uns

//- Dateipfad -> "index.png"
html
  body
    include includes/nav.pug
//-
  <html>
    <body>
      <h1>Firmenname</h1>
      <nav><a href="index.html">Home</a><a href="about.html">Über uns</a></nav>
    </body>
  </html>

//- Importiere JS und CSS
script
  include scripts/index.js
style
  include styles/theme.css

//- ---MIXIN---
mixin basic()
  div Hallo
+basic("Bob")
//- <div>Hallo</div>

mixin comment(name, kommentar)
  div
    span.comment-name= name
    div.comment-text= kommentar
+comment("Bob", "Das ist super")
//- <div>Hallo</div>

```


### Zusätzliche Ressourcen
- [The Site](https://pugjs.org/)
- [The Docs](https://pugjs.org/api/getting-started.html)
- [Github Repo](https://github.com/pugjs/pug)
