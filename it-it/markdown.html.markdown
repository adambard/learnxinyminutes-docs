---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
translators:
    - ["Jacopo Andrea Giola", "http://geekpanda.net"]
filename: markdown-it.md
lang: it-it
---

Markdown è stato creato da John Gruber nel 2004. Il suo scopo è quello di essere una sintassi facile da leggere e scrivere, e che può essere convertita in HTML (ad oggi anche in molti altri formati).

Mandate tutto il feedback che volete! / Sentitevi liberi di forkare o di mandare pull request!


```markdown
<!-- Markdown è un superset di HTML, quindi ogni file HTML è a sua volta un file Markdown valido. Questo significa che possiamo usare elementi di HTML in Markdown, come per esempio i commenti, e questi non saranno modificati dal parser di Markdown. State attenti però, se inserite un elemento HTML nel vostro file Markdown, non potrete usare la sua sintassi all'interno del contenuto dell'elemento. -->

<!-- L'implementazione di Markdown inoltre cambia da parser a parser. In questa guida cercheremo di indicare quando una feature è universale e quando sono specifiche ad un certo parser. -->

<!-- Titoli -->
<!-- Potete creare gli elementi HTML da <h1> ad <h6> facilmente, basta che inseriate un egual numero di caratteri cancelletto (#) prima del testo che volete all'interno dell'elemento -->
# Questo è un <h1>
## Questo è un <h2>
### Questo è un <h3>
#### Questo è un <h4>
##### Questo è un <h5>
###### Questo è un <h6>

<!-- Markdown inoltre fornisce due alternative per indicare gli elementi h1 e h2 -->
Questo è un h1
==============

Questo è un h2
--------------

<!-- Stili di testo semplici -->
<!-- Il testo può essere stilizzato in corsivo o grassetto usando markdown -->

*Questo testo è in corsivo.*
_Come pure questo._

**Questo testo è in grassetto.**
__Come pure questo.__

***Questo testo è stilizzato in entrabmi i modi.***
**_Come questo!_**
*__E questo!__*

<!-- In Github Flavored Markdown, che è utilizzato per renderizzare i file markdown su
Github, è presente anche lo stile barrato -->

~~Questo testo è barrato.~~

<!-- I paragrafi sono uno o più linee di testo addiacenti separate da una o più righe vuote. -->

Qeusto è un paragrafo. Sto scrivendo in un paragrafo, non è divertente?

Ora sono nel paragrafo 2.
Anche questa linea è nel paragrafo 2!


Qui siamo nel paragrafo 3!

<!-- Se volete inserire l'elemento HTML <br />, potete terminare la linea con due o più spazi e poi iniziare un nuovo paragrafo. -->

Questa frase finisce con due spazi (evidenziatemi per vederli).  

C'è un <br /> sopra di me!

<!-- Le citazioni sono semplici da inserire, basta usare il carattere >. -->

> Questa è una citazione. Potete
> mandare a capo manualmente le linee e inserire un `>` prima di ognuna, oppure potete usare una sola linea e lasciare che vada a capo automaticamente.
> Non c'è alcuna differenza, basta che iniziate ogni riga con `>`.

> Potete utilizzare anche più di un livello
>>  di indentazione!
> Quanto è comodo?

<!-- Liste -->
<!-- Le liste non ordinate possono essere inserite usando gli asterischi, il simbolo più o dei trattini -->

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

<!-- Le liste ordinate invece, sono inserite con un numero seguito da un punto. -->

1. Primo oggetto
2. Secondo oggetto
3. Terzo oggetto

<!-- Non dovete nemmeno mettere i numeri nell'ordine giusto, markdown li visualizzerà comunque nell'ordine corretto, anche se potrebbe non essere una buona idea. -->

1. Primo oggetto
1. Secondo oggetto
1. Terzo oggetto
<!-- (Questa lista verrà visualizzata esattamente come quella dell'esempio prima) -->

<!-- Potete inserire anche sotto liste -->

1. Primo oggetto
2. Secondo oggetto
3. Terzo oggetto
    * Sotto-oggetto
    * Sotto-oggetto
4. Quarto oggetto

<!-- Sono presenti anche le task list. In questo modo è possibile creare checkbox in HTML. -->

I box senza la 'x' sono checkbox HTML ancora da completare.
- [ ] Primo task da completare.
- [ ] Secondo task che deve essere completato.
Il box subito sotto è una checkbox HTML spuntata.
- [x] Questo task è stato completato.

<!-- Estratti di codice -->
<!-- Potete inserire un estratto di codice (che utilizza l'elemento <code>) indentando una linea con quattro spazi oppure con un carattere tab -->

    Questa è una linea di codice
    Come questa

<!-- Potete inoltre inserire un altro tab (o altri quattro spazi) per indentare il vostro codice -->

    my_array.each do |item|
        puts item
    end

<!-- Codice inline può essere inserito usando il carattere backtick ` -->

Giovanni non sapeva neppure a cosa servisse la funzione `go_to()`!

<!-- In Github Flavored Markdown, potete inoltre usare una sintassi speciale per il codice -->

\`\`\`ruby <!-- In realtà dovete rimuovere i backslash, usate solo ```ruby ! -->
def foobar
    puts "Hello world!"
end
\`\`\` <!-- Anche qui, niente backslash, solamente ``` -->

<!-- Se usate questa sintassi, il testo non richiederà di essere indentanto, inoltre Github userà la  syntax highlighting del linguaggio specificato dopo i ``` iniziali -->

<!-- Linea orizzontale (<hr />) -->
<!-- Le linee orizzontali sono inserite facilemtne usanto tre o più asterischi o trattini senza spazi consecutivi e senza spazi. -->

***
---
- - -
****************

<!-- Link -->
<!-- Una delle funzionalità migliori di markdown è la facilità con cui si possono inserire i link. Mettete il testo da visualizzare fra parentesi quadre [] seguite dall'url messo fra parentesi tonde () -->

[Cliccami!](http://test.com/)

<!-- Potete inoltre aggiungere al link un titolo mettendolo fra doppie apici dopo il link -->

[Cliccami!](http://test.com/ "Link a Test.com")

<!-- La sintassi funziona anche i path relativi. -->

[Vai a musica](/music/).

<!-- Markdown supporta inoltre anche la possibilità di aggiungere i link facendo riferimento ad altri punti del testo -->

[Apri questo link][link1] per più informazioni!
[Guarda anche questo link][foobar] se ti va.

[link1]: http://test.com/ "Bello!"
[foobar]: http://foobar.biz/ "Va bene!"

<!-- Il titolo può anche essere inserito in apici singoli o in parentesi, oppure omesso interamente. Il riferimento può essere inserito in un punto qualsiasi del vostro documento e l'identificativo del riferimento può essere lungo a piacere a patto che sia univoco. -->

<!-- Esiste anche un "identificativo implicito" che vi permette di usare il testo del link come id -->

[Questo][] è un link.

[Questo]: http://thisisalink.com/

<!-- Ma non è comunemente usato. -->

<!-- Immagini -->
<!-- Le immagini sono inserite come i link ma con un punto esclamativo inserito prima delle parentesi quadre! -->

![Qeusto è il testo alternativo per l'immagine](http://imgur.com/myimage.jpg "Il titolo opzionale")

<!-- E la modalità a riferimento funziona esattamente come ci si aspetta -->

![Questo è il testo alternativo.][myimage]

[myimage]: relative/urls/cool/image.jpg "Se vi serve un titolo, lo mettete qui"

<!-- Miscellanea -->
<!-- Auto link -->

<http://testwebsite.com/> è equivalente ad
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Auto link per le email -->

<foo@bar.com>

<!-- Caratteri di escaping -->

Voglio inserire *questo testo circondato da asterischi* ma non voglio che venga renderizzato in corsivo, quindi lo inserirò così: \*questo testo è circondato da asterischi\*.

<!-- Combinazioni di tasti -->
<!-- In Github Flavored Markdown, potete utilizzare il tag <kbd> per raffigurare i tasti della tastiera -->

Il tuo computer è crashato? Prova a premere
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Canc</kbd>

<!-- Tabelle -->
<!-- Le tabelle sono disponibili solo in Github Flavored Markdown e sono leggeremente complesse, ma se proprio volete inserirle fate come segue: -->

| Col1                 | Col2     | Col3               |
| :------------------- | :------: | -----------------: |
| Allineato a sinistra | Centrato | Allineato a destra |
| blah                 | blah     | blah               |

<!-- oppure, per lo stesso risultato -->

Col 1 | Col2 | Col3
:-- | :-: | --:
È una cosa orrenda | fatela | finire in fretta

<!-- Finito! -->

```

Per altre informazioni, leggete il post ufficiale di John Gruber sulla sintassi [qui](http://daringfireball.net/projects/markdown/syntax) e il magnifico cheatsheet di Adam Pritchard [qui](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
