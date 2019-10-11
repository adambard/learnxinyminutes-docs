---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
    - ["Jacob Ward", "http://github.com/JacobCWard/"]
translators:
    - ["Jacopo Andrea Giola", "http://geekpanda.net"]
    - ["Ale46", "https://github.com/Ale46"]
filename: markdown-it.md
lang: it-it
---

Markdown è stato creato da John Gruber nel 2004. Il suo scopo è quello di essere una sintassi facile da leggere e scrivere, e che può essere convertita in HTML (ad oggi anche in molti altri formati).

Markdown varia nelle sue implementazioni da un parser all'altro. Questa guida cercherà di chiarire quali caratteristiche esistono a livello globale o quando sono disponibili solo per un determinato parser.

- [Elementi HTML](#elementi-html)
- [Titoli](#titoli)
- [Stili di testo semplici](#stili-di-testo-semplici)
- [Paragrafi](#paragrafi)
- [Liste](#liste)
- [Estratti di codice](#estratti-di-codice)
- [Linea orizzontale](#linea-orizzontale)
- [Links](#links)
- [Immagini](#immagini)
- [Miscellanea](#miscellanea)

## Elementi HTML
Markdown è un superset di HTML, quindi ogni file HTML è a sua volta un file Markdown valido.

```md
<!-- Questo significa che possiamo usare elementi di HTML in Markdown, come per esempio i commenti,
e questi non saranno modificati dal parser di Markdown. State attenti però, 
se inserite un elemento HTML nel vostro file Markdown, non potrete usare la sua sintassi 
all'interno del contenuto dell'elemento. -->
```

## Titoli

Potete creare gli elementi HTML da `<h1>` a `<h6>` facilmente, basta che inseriate un egual numero di caratteri cancelletto (#) prima del testo che volete all'interno dell'elemento

```md
# Questo è un <h1>
## Questo è un <h2>
### Questo è un <h3>
#### Questo è un <h4>
##### Questo è un <h5>
###### Questo è un <h6>
```
Markdown inoltre fornisce due alternative per indicare gli elementi h1 e h2

```md
Questo è un h1
==============

Questo è un h2
--------------
```

## Stili di testo semplici
Il testo può essere stilizzato in corsivo o grassetto usando markdown

```md
*Questo testo è in corsivo.*
_Come pure questo._

**Questo testo è in grassetto.**
__Come pure questo.__

***Questo testo è stilizzato in entrabmi i modi.***
**_Come questo!_**
*__E questo!__*
```

In Github Flavored Markdown, che è utilizzato per renderizzare i file markdown su Github, è presente anche lo stile barrato:

```md
~~Questo testo è barrato.~~
```
## Paragrafi

```md
I paragrafi sono una o più linee di testo adiacenti separate da una o più righe vuote.

Questo è un paragrafo. Sto scrivendo in un paragrafo, non è divertente?

Ora sono nel paragrafo 2.
Anche questa linea è nel paragrafo 2!


Qui siamo nel paragrafo 3!
```

Se volete inserire l'elemento HTML `<br />`, potete terminare la linea con due o più spazi e poi iniziare un nuovo paragrafo.

```md
Questa frase finisce con due spazi (evidenziatemi per vederli).  

C'è un <br /> sopra di me!
```

Le citazioni sono semplici da inserire, basta usare il carattere >.

```md
> Questa è una citazione. Potete
> mandare a capo manualmente le linee e inserire un `>` prima di ognuna, oppure potete usare una sola linea e lasciare che vada a capo automaticamente.
> Non c'è alcuna differenza, basta che iniziate ogni riga con `>`.

> Potete utilizzare anche più di un livello
>>  di indentazione!
> Quanto è comodo?

```

## Liste
Le liste non ordinate possono essere inserite usando gli asterischi, il simbolo più o dei trattini

```md
* Oggetto
* Oggetto
* Altro oggetto

oppure

+ Oggetto
+ Oggetto
+ Un altro oggetto

oppure

- Oggetto
- Oggetto
- Un ultimo oggetto
```

Le liste ordinate invece, sono inserite con un numero seguito da un punto.

```md
1. Primo oggetto
2. Secondo oggetto
3. Terzo oggetto
```

Non dovete nemmeno mettere i numeri nell'ordine giusto, markdown li visualizzerà comunque nell'ordine corretto, anche se potrebbe non essere una buona idea.

```md
1. Primo oggetto
1. Secondo oggetto
1. Terzo oggetto
```
(Questa lista verrà visualizzata esattamente come quella dell'esempio prima)

Potete inserire anche sotto liste

```md
1. Primo oggetto
2. Secondo oggetto
3. Terzo oggetto
    * Sotto-oggetto
    * Sotto-oggetto
4. Quarto oggetto
```

Sono presenti anche le task list. In questo modo è possibile creare checkbox in HTML.

```md
I box senza la 'x' sono checkbox HTML ancora da completare.
- [ ] Primo task da completare.
- [ ] Secondo task che deve essere completato.
Il box subito sotto è una checkbox HTML spuntata.
- [x] Questo task è stato completato.
```
## Estratti di codice

Potete inserire un estratto di codice (che utilizza l'elemento `<code>`) indentando una linea con quattro spazi oppure con un carattere tab.

```md
    Questa è una linea di codice
    Come questa
```

Potete inoltre inserire un altro tab (o altri quattro spazi) per indentare il vostro codice

```md
    my_array.each do |item|
        puts item
    end
```

Codice inline può essere inserito usando il carattere backtick `

```md
Giovanni non sapeva neppure a cosa servisse la funzione `go_to()`!
```

In Github Flavored Markdown, potete inoltre usare una sintassi speciale per il codice
<pre>
<code class="highlight">&#x60;&#x60;&#x60;ruby
def foobar
    puts "Hello world!"
end
&#x60;&#x60;&#x60;</code></pre>
Se usate questa sintassi, il testo non richiederà di essere indentato, inoltre Github userà l'evidenziazione della sintassi del linguaggio specificato dopo i \`\`\` iniziali

## Linea orizzontale
Le linee orizzontali (`<hr/>`) sono inserite facilmente usanto tre o più asterischi o trattini, con o senza spazi. 

```md
***
---
- - -
****************
```

## Links
Una delle funzionalità migliori di markdown è la facilità con cui si possono inserire i link. Mettete il testo da visualizzare fra parentesi quadre [] seguite dall'url messo fra parentesi tonde ()

```md
[Cliccami!](http://test.com/)
```

Potete inoltre aggiungere al link un titolo mettendolo fra doppi apici dopo il link

```md
[Cliccami!](http://test.com/ "Link a Test.com")
```

La sintassi funziona anche con i path relativi.

```md
[Vai a musica](/music/).
```

Markdown supporta inoltre anche la possibilità di aggiungere i link facendo riferimento ad altri punti del testo.
```md
[Apri questo link][link1] per più informazioni!
[Guarda anche questo link][foobar] se ti va.

[link1]: http://test.com/ "Bello!"
[foobar]: http://foobar.biz/ "Va bene!"
```
l titolo può anche essere inserito in apici singoli o in parentesi, oppure omesso interamente. Il riferimento può essere inserito in un punto qualsiasi del vostro documento e l'identificativo del riferimento può essere lungo a piacere a patto che sia univoco.

Esiste anche un "identificativo implicito" che vi permette di usare il testo del link come id.
```md
[Questo][] è un link.

[Questo]: http://thisisalink.com/
```
Ma non è comunemente usato.

## Immagini
Le immagini sono inserite come i link ma con un punto esclamativo inserito prima delle parentesi quadre!

```md
![Qeusto è il testo alternativo per l'immagine](http://imgur.com/myimage.jpg "Il titolo opzionale")
```

E la modalità a riferimento funziona esattamente come ci si aspetta

```md
![Questo è il testo alternativo.][myimage]

[myimage]: relative/urls/cool/image.jpg "Se vi serve un titolo, lo mettete qui"
```
## Miscellanea 
### Auto link

```md
<http://testwebsite.com/> è equivalente ad
[http://testwebsite.com/](http://testwebsite.com/)
```
### Auto link per le email

```md
<foo@bar.com>
```
### Caratteri di escaping

```md
Voglio inserire *questo testo circondato da asterischi* ma non voglio che venga renderizzato in corsivo, quindi lo inserirò così: \*questo testo è circondato da asterischi\*.
```

### Combinazioni di tasti
In Github Flavored Markdown, potete utilizzare il tag `<kbd>` per raffigurare i tasti della tastiera.

```md
Il tuo computer è crashato? Prova a premere
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Canc</kbd>
```

### Tabelle
Le tabelle sono disponibili solo in Github Flavored Markdown e sono leggeremente complesse, ma se proprio volete inserirle fate come segue:

```md
| Col1                 | Col2     | Col3               |
| :------------------- | :------: | -----------------: |
| Allineato a sinistra | Centrato | Allineato a destra |
| blah                 | blah     | blah               |
```
oppure, per lo stesso risultato

```md
Col 1 | Col2 | Col3
:-- | :-: | --:
È una cosa orrenda | fatela | finire in fretta
```

---
Per altre informazioni, leggete il post ufficiale di John Gruber sulla sintassi [qui](http://daringfireball.net/projects/markdown/syntax) e il magnifico cheatsheet di Adam Pritchard [qui](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
