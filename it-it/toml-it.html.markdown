---
language: toml
filename: learntoml-it.toml
contributors:
  - ["Alois de Gouvello", "https://github.com/aloisdg"]
translators:
  - ["Christian Grasso", "https://grasso.io"]
lang: it-it
---

TOML è l'acronimo di _Tom's Obvious, Minimal Language_. È un linguaggio per la
serializzazione di dati, progettato per i file di configurazione.

È un'alternativa a linguaggi come YAML e JSON, che punta ad essere più leggibile
per le persone. Allo stesso tempo, TOML può essere utilizzato in modo abbastanza
semplice nella maggior parte dei linguaggi di programmazione, in quanto è
progettato per essere tradotto senza ambiguità in una hash table.

Tieni presente che TOML è ancora in fase di sviluppo, e la sua specifica non è
ancora stabile. Questo documento utilizza TOML 0.4.0.

```toml
# I commenti in TOML sono fatti così.

################
# TIPI SCALARI #
################

# Il nostro oggetto root (corrispondente all'intero documento) sarà una mappa,
# anche chiamata dizionario, hash o oggetto in altri linguaggi.

# La key, il simbolo di uguale e il valore devono trovarsi sulla stessa riga,
# eccetto per alcuni tipi di valori.
key = "value"
stringa = "ciao"
numero = 42
float = 3.14
boolean = true
data = 1979-05-27T07:32:00-08:00
notazScientifica = 1e+12
"puoi utilizzare le virgolette per la key" = true # Puoi usare " oppure '
"la key può contenere" = "lettere, numeri, underscore e trattini"

############
# Stringhe #
############

# Le stringhe possono contenere solo caratteri UTF-8 validi.
# Possiamo effettuare l'escape dei caratteri, e alcuni hanno delle sequenze
# di escape compatte. Ad esempio, \t corrisponde al TAB.
stringaSemplice = "Racchiusa tra virgolette. \"Usa il backslash per l'escape\"."

stringaMultiriga = """
Racchiusa da tre virgolette doppie all'inizio e
alla fine - consente di andare a capo."""

stringaLiteral = 'Virgolette singole. Non consente di effettuare escape.'

stringaMultirigaLiteral = '''
Racchiusa da tre virgolette singole all'inizio e
alla fine - consente di andare a capo.
Anche in questo caso non si può fare escape.
Il primo ritorno a capo viene eliminato.
   Tutti gli altri spazi aggiuntivi
   vengono mantenuti.
'''

# Per i dati binari è consigliabile utilizzare Base64 e
# gestirli manualmente dall'applicazione.

##########
# Interi #
##########

## Gli interi possono avere o meno un segno (+, -).
## Non si possono inserire zero superflui all'inizio.
## Non è possibile inoltre utilizzare valori numerici
## non rappresentabili con una sequenza di cifre.
int1 = +42
int2 = 0
int3 = -21

## Puoi utilizzare gli underscore per migliorare la leggibilità.
## Fai attenzione a non inserirne due di seguito.
int4 = 5_349_221
int5 = 1_2_3_4_5     # VALIDO, ma da evitare

#########
# Float #
#########

# I float permettono di rappresentare numeri decimali.
flt1 = 3.1415
flt2 = -5e6
flt3 = 6.626E-34

###########
# Boolean #
###########

# I valori boolean (true/false) devono essere scritti in minuscolo.
bool1 = true
bool2 = false

############
# Data/ora #
############

data1 = 1979-05-27T07:32:00Z # Specifica RFC 3339/ISO 8601 (UTC)
data2 = 1979-05-26T15:32:00+08:00 # RFC 3339/ISO 8601 con offset

######################
# TIPI DI COLLECTION #
######################

#########
# Array #
#########

array1 = [ 1, 2, 3 ]
array2 = [ "Le", "virgole", "sono", "delimitatori" ]
array3 = [ "Non", "unire", "tipi", "diversi" ]
array4 = [ "tutte", 'le stringhe', """hanno lo stesso""", '''tipo''' ]
array5 = [
  "Gli spazi vuoti", "sono", "ignorati"
]

###########
# Tabelle #
###########

# Le tabelle (o hash table o dizionari) sono collection di coppie key/value.
# Iniziano con un nome tra parentesi quadre su una linea separata. 
# Le tabelle vuote (senza alcun valore) sono valide.
[tabella]

# Tutti i valori che si trovano sotto il nome della tabella
# appartengono alla tabella stessa (finchè non ne viene creata un'altra).
# L'ordine di questi valori non è garantito.
[tabella-1]
key1 = "una stringa"
key2 = 123

[tabella-2]
key1 = "un'altra stringa"
key2 = 456

# Utilizzando i punti è possibile creare delle sottotabelle.
# Ogni parte suddivisa dai punti segue le regole delle key per il nome.
[tabella-3."sotto.tabella"]
key1 = "prova"

# Ecco l'equivalente JSON della tabella precedente:
# { "tabella-3": { "sotto.tabella": { "key1": "prova" } } }

# Gli spazi non vengono considerati, ma è consigliabile
# evitare di usare spazi superflui.
[a.b.c]            # consigliato
[ d.e.f ]          # identico a [d.e.f]

# Non c'è bisogno di creare le tabelle superiori per creare una sottotabella.
# [x] queste
# [x.y] non
# [x.y.z] servono
[x.y.z.w] # per creare questa tabella

# Se non è stata già creata prima, puoi anche creare
# una tabella superiore più avanti.
[a.b]
c = 1

[a]
d = 2

# Non puoi definire una key o una tabella più di una volta.

# ERRORE
[a]
b = 1

[a]
c = 2

# ERRORE
[a]
b = 1

[a.b]
c = 2

# I nomi delle tabelle non possono essere vuoti.
[]     # NON VALIDO
[a.]   # NON VALIDO
[a..b] # NON VALIDO
[.b]   # NON VALIDO
[.]    # NON VALIDO

##################
# Tabelle inline #
##################

tabelleInline = { racchiuseData = "{ e }", rigaSingola = true }
punto = { x = 1, y = 2 }

####################
# Array di tabelle #
####################

# Un array di tabelle può essere creato utilizzando due parentesi quadre.
# Tutte le tabelle con questo nome saranno elementi dell'array.
# Gli elementi vengono inseriti nell'ordine in cui si trovano.

[[prodotti]]
nome = "array di tabelle"
sku = 738594937
tabelleVuoteValide = true

[[prodotti]]

[[prodotti]]
nome = "un altro item"
sku = 284758393
colore = "grigio"

# Puoi anche creare array di tabelle nested. Le sottotabelle con doppie
# parentesi quadre apparterranno alla tabella più vicina sopra di esse.

[[frutta]]
  nome = "mela"

  [frutto.geometria]
    forma = "sferica"
    nota = "Sono una proprietà del frutto"

  [[frutto.colore]]
    nome = "rosso"
    nota = "Sono un oggetto di un array dentro mela"

  [[frutto.colore]]
    nome = "verde"
    nota = "Sono nello stesso array di rosso"

[[frutta]]
  nome = "banana"

  [[frutto.colore]]
    nome = "giallo"
    nota = "Anche io sono un oggetto di un array, ma dentro banana"
```

Ecco l'equivalente JSON dell'ultima tabella:

```json
{
  "frutta": [
    {
      "nome": "mela",
      "geometria": { "forma": "sferica", "nota": "..."},
      "colore": [
        { "nome": "rosso", "nota": "..." },
        { "nome": "verde", "nota": "..." }
      ]
    },
    {
      "nome": "banana",
      "colore": [
        { "nome": "giallo", "nota": "..." }
      ]
    }
  ]
}
```

### Altre risorse

+ [Repository ufficiale di TOML](https://github.com/toml-lang/toml)
