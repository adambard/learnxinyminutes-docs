---
language: PCRE
filename: pcre-it.txt
contributors:
    - ["Sachin Divekar", "http://github.com/ssd532"]
translators:
    - ["Christian Grasso", "https://grasso.io"]
lang: it-it
---

Un'espressione regolare (regex o regexp in breve) è una speciale stringa
utilizzata per definire un pattern, ad esempio per cercare una sequenza di
caratteri; ad esempio, `/^[a-z]+:/` può essere usato per estrarre `http:`
dall'URL `http://github.com/`.

PCRE (Perl Compatible Regular Expressions) è una libreria per i regex in C.
La sintassi utilizzata per le espressioni è molto simile a quella di Perl, da
cui il nome. Si tratta di una delle sintassi più diffuse per la scrittura di
regex.

Esistono due tipi di metacaratteri (caratteri con una funzione speciale):

* Caratteri riconosciuti ovunque tranne che nelle parentesi quadre

```
  \      carattere di escape
  ^      cerca all'inizio della stringa (o della riga, in modalità multiline)
  $      cerca alla fine della stringa (o della riga, in modalità multiline)
  .      qualsiasi carattere eccetto le newline
  [      inizio classe di caratteri
  |      separatore condizioni alternative
  (      inizio subpattern
  )      fine subpattern
  ?      quantificatore "0 o 1"
  *      quantificatore "0 o più"
  +      quantificatore "1 o più"
  {      inizio quantificatore numerico
```

* Caratteri riconosciuti nelle parentesi quadre

```
  \      carattere di escape
  ^      nega la classe se è il primo carattere
  -      indica una serie di caratteri
  [      classe caratteri POSIX (se seguita dalla sintassi POSIX)
  ]      termina la classe caratteri
```

PCRE fornisce inoltre delle classi di caratteri predefinite:

```
  \d     cifra decimale
  \D     NON cifra decimale
  \h     spazio vuoto orizzontale
  \H     NON spazio vuoto orizzontale
  \s     spazio
  \S     NON spazio
  \v     spazio vuoto verticale
  \V     NON spazio vuoto verticale
  \w     parola
  \W     "NON parola"
```

## Esempi

Utilizzeremo la seguente stringa per i nostri test:

```
66.249.64.13 - - [18/Sep/2004:11:07:48 +1000] "GET /robots.txt HTTP/1.0" 200 468 "-" "Googlebot/2.1"
```

Si tratta di una riga di log del web server Apache.

| Regex | Risultato          | Commento |
| :---- | :-------------- | :------ |
| `GET`   | GET | Cerca esattamente la stringa "GET" (case sensitive) |
| `\d+.\d+.\d+.\d+` | 66.249.64.13 | `\d+` identifica uno o più (quantificatore `+`) numeri [0-9], `\.` identifica il carattere `.` |
| `(\d+\.){3}\d+` | 66.249.64.13 | `(\d+\.){3}` cerca il gruppo (`\d+\.`) esattamente 3 volte. |
| `\[.+\]` | [18/Sep/2004:11:07:48 +1000] | `.+` identifica qualsiasi carattere, eccetto le newline; `.` indica un carattere qualsiasi |
| `^\S+` | 66.249.64.13 | `^` cerca all'inizio della stringa, `\S+` identifica la prima stringa di caratteri diversi dallo spazio |
| `\+[0-9]+` | +1000 | `\+` identifica il carattere `+`. `[0-9]` indica una cifra da 0 a 9. L'espressione è equivalente a `\+\d+` |

## Altre risorse
[Regex101](https://regex101.com/) - tester per le espressioni regolari
