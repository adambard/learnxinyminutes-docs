---
contributors:
    - ["Wichamir", "https://github.com/Wichamir/"]
    - ["zacryol", "https://github.com/zacryol"]
translators:
    - ["lele25811", "https://github.com/lele25811"]
---

GDScript un linguaggio di scripting dinamicamente e staticamente
tipizzato per il motore di gioco gratuito e open source Godot. La sua
sintassi è vagamente simile a quella di Python. I suoi principali
vantaggi sono la facilità d'uso e la stretta integrazione con il motore di gioco.
È perfetto per lo sviluppo di giochi.

## Basi

```gdscript
# I commenti a linea singola sono scritti usando il simbolo hash.
"""
  Commenti
  su
  più
  linee
  sono 
  scritti
  usando
  i triplici
  apici
  doppi
"""

# I commenti del documento possono aggiungere una descrizione a classi e campi
# che può essere visualizzato nella documentazione del engine.

## Questa classe è una dimostrazione di GDScript.

# Il file script è una classe e puoi opzionalmente definire un nome.
class_name MyClass

# Eredita
extends Node2D

# Variabili
var x = 8 # int
var y = 1.2 # float
var b = true # bool
var s = "Hello World!" # String
var a = [1, false, "brown fox"] # Array - simile alle liste in Python,
                                # può contenere diversi tipi
                                # di variabili contemporaneamente.
var d = {
  "key" : "value",
  42 : true
} # Dizionario - contiene coppie chiave-valore.
var p_arr = PackedStringArray(["Hi", "there", "!"]) # Array impacchettato può
                                                    # contenere solo un tipo di dato.

# Commenti della documentazione applicati alle proprietà.

## Quante volte questo oggetto ha saltato
var jump_count = 0

# Tipi vettoriali integrati:
var v2 = Vector2(1, 2)
var v3 = Vector3(1, 2, 3)

# Costanti
const ANSWER_TO_EVERYTHING = 42
const BREAKFAST = "Spam and eggs!"

# Enumerazioni
enum { ZERO, ONE , TWO, THREE }
enum NamedEnum { ONE = 1, TWO, THREE }

# Le variabili esportate sono visibili nell'ispettore.
#
# oppure un tipo suggerimento (spiegato in seguito) o un valore predefinito che è necessario in seguito
# l'editor suggerisce diverse opzioni 
@export var age: int
@export var height: float
@export var person_name = "Bob"
# Ma entrambe sono accettate
@export var favorite_color: String = "Green"
@export var favorite_food := "Pizza"

# Funzioni
func foo():
  pass # pass è la parola chiave per passare al codice successivo

func add(first, second):
  return first + second

# Commenti documentazione sulle funzioni

## Incrementa il conteggio dei salti
func jump():
  jump_count += 1

# Stampa valori
func printing():
  print("GDScript ", "is ", " awesome.")
  prints("These", "words", "are", "divided", "by", "spaces.")
  printt("These", "words", "are", "divided", "by", "tabs.")
  printraw("This gets printed to system console.")

  # Funzioni Lambda
  var my_lambda = func(): print("hello from lambda!")

  my_lambda.call()

# Matematica
func doing_math():
  var first = 8
  var second = 4
  print(first + second) # 12
  print(first - second) # 4
  print(first * second) # 32
  print(first / second) # 2
  print(first % second) # 0
  # Ci sono anche +=, -=, *=, /=, %= etc.,
  # Anche operatori ++ o --.
  print(pow(first, 2)) # 64
  print(sqrt(second)) # 2
  printt(PI, TAU, INF, NAN) # Costanti Integrate

# Controllo del flusso
func control_flow():
  x = 8
  y = 2 # y era inizialmente un float,
        # ma possiamo cambiare il tipo in int
        # stiamo usando la potenza della programmazione dinamica!

  if x < y:
    print("x is smaller than y")
  elif x > y:
    print("x is bigger than y")
  else:
    print("x and y are equal")

  var a = true
  var b = false
  var c = false
  if a and b or not c: # in alternativa puoi utilizzare &&, || e !
    print("This is true!")

  for i in range(20): # L'intervallo di GDScript è simile a quello di Python
    print(i) # questo stamperà i numeri da 0 a 29

  for i in 20: # differentemente da Python puoi ciclare direttamente su un int
    print(i) # questo stamperà i numeri da 0 a 19

  for i in ["two", 3, 1.0]: # iterazione su un array
    print(i)

  while x > y:
    printt(x, y)
    y += 1

  x = 2
  y = 10
  while x < y:
    x += 1
    if x == 6:
      continue # 6 non verrà stampato causa la dichiarazione: continue
    prints("x is equal to:", x)
    if x == 7:
      break # il ciclo si fermerà a 7, quindi 8, 9 e 10 non verranno stampati

  match x:
    1:
      print("Match is similar to switch.")
    2:
      print("However you don't need to put cases before each value.")
    3:
      print("Furthermore each case breaks on default.")
      break # ERRORE! la dichiarazione break non è necessaria 
    4:
      print("If you need fallthrough use continue.")
      continue
    _:
      print("Underscore is a default case.")

  # operatore ternario (la dichiarazione if-else è su una linea)
  prints("x is", "positive" if x >= 0 else "negative")

# Assegnazione delle parti (Casting)
func casting_examples():
  var i = 42
  var f = float(42) # assegnazione utilizzando il costruttore di variabili
  var b = i as bool # oppure utilizzando la parola "as"

# Funzioni di sovraccarico (Override functions)
# Per convenzione le funzione di Override iniziano con un trattino basso ( _ ),
# ma in pratica puoi fare l'Override di qualsiasi funzione.

# _init è chiamata quando un oggetto viene inizializzato
# È il costuttore del oggetto
func _init():
  # Inizializza le cose interne dell'oggetto
  pass

# _ready viene chiamato quando il nodo di script e
# i suoi figli sono entrati nell'albero della scena.
func _ready():
  pass

# _process viene chiamato ogni frame
func _process(delta):
  # L'argomento Delta passato a questa funzione è il numero di secondi
  # che passa tra l'ultimo frame e quello attuale.
  print("Delta time equals: ", delta)

# _physics_process viene chiamato ogni frame di fisica.
# Ciò significa che delta dovrebbe essere una costante.
func _physics_process(delta):
  # Semplice movimento mediante la somma vettoriale e la moltiplicazione.
  var direction = Vector2(1, 0) # oppure Vector2.RIGHT
  var speed = 100.0
  self.global_position += direction * speed * delta
  # self si riferisce a se stesso nel attuale istanza di classe

# Quando si fa l'override di una funzione si può chiamare la funzione padre 
# tramite la funzione super()
# come qui:
func get_children():
  # Fai cose in più qui.
  var r = super() # implementazione della chiamata alla funzione padre
  return r

# Classe annidata
class InnerClass:
  extends Object

  func hello():
    print("Hello from inner class!")

func use_inner_class():
  var ic = InnerClass.new()
  ic.hello()
  ic.free() # utilizza free per pulire la memoria
```

## Accesso ad altri nodi nel albero di scena

```gdscript
extends Node2D

var sprite # La variabile conterrà il riferimento.

# Puoi ottenere le referenze per gli altri nodi in _ready.
func _ready() -> void:
  # NodePath è utile per accedere ai nodi.
  # Crea NodePath passando il percorso al suo costruttore:
  var path1 = NodePath("path/to/something")
  # alternativamente puoi usare NodePath così:
  var path2 = ^"path/to/something"
  # NodePath esempi:
  var path3 = ^"Sprite" # percorso relativo, primo figlio dell'attuale nodo
  var path4 = ^"Timers/Firerate" # percorso relativo, figlio di suo figlio
  var path5 = ^".." # genitore del nodo corrente
  var path6 = ^"../Enemy" # fratello del nodo corrente
  var path7 = ^"/root" # percorso assoluto, equivalente a get_tree().get_root()
  var path8 = ^"/root/Main/Player/Sprite" # percorso assoluto alla sprite del giocatore
  var path9 = ^"Timers/Firerate:wait_time" # accesso alle proprietà
  var path10 = ^"Player:position:x" # accesso alle sottoproprietà

  # Infine per ottenere un riferimento usa uno di questi:
  sprite = get_node(^"Sprite") as Sprite # sempre usata con il tipo di dati che ti aspetti
  sprite = get_node("Sprite") as Sprite # qui ottiene una stringa
                                        # implicitamente lanciato come NodePath
  sprite = get_node(path3) as Sprite
  sprite = get_node_or_null("Sprite") as Sprite
  sprite = $Sprite as Sprite

func _process(delta):
  # Ora possiamo riutilizzare il riferimento ad altri luoghi
  prints("Sprite has global_position of", sprite.global_position)

# Usiamo l'annotazione @onready per assegnare un valore
# a una variabile prima del esecuzione di _ready.
# Questa è la sintassi comunemente utilizzata.
@onready var other_sprite = $Sprite as Sprite

# Puoi esportare NodePath, così da poterlo assegnare all'interno dell'ispettore.
@export var nodepath = ^""
@onready var reference = get_node(nodepath) as Node

# oppure esportare un nodo direttamente
@export var other_reference: Node
```

## Segnali (Signals)

Il sistema del segnali in Godot è una implementazione del pattern observer.
Ecco un esempio:

```gdscript
class_name Player extends Node2D

var hp = 10

# La documentazione può andare anche sui segnali

## Emesso quando il giocatore muore
signal died() # definisci un segnale
signal hurt(hp_old, hp_new) # il segnale prende un argomento

func apply_damage(dmg):
  var hp_old = hp
  hp -= dmg
  hurt.emit(hp_old, hp) # emette un segnale e passa un argomento
  if hp <= 0:
    died.emit()

func _ready():
  # connette il segnale "died" alla funzione "_on_death" definita in automatico
  died.connect(_on_death)
  # Alternativamente
  # se l'oggetto target non è se stesso, ma un altro oggetto
  # died.connect(Callable(self, &"_on_death"))

func _on_death():
  queue_free() # distrugge il giocatore alla morte
```

## Tipi suggeriti (Type hints)

GDScript opzionalmente può utilizzare la tipizzazione statica, sia per la chiarezza del codice che per i vantaggi delle prestazioni.

```gdscript
extends Node

var x: int # definizione del tipo di variabile
var y: float = 4.2
var z := 1.0 # accenno del tipo per i valori di default usiamo l'operatore := 

var a: Array[int] = [1, 2, 3] # gli array possono anche avere un tipo specifico per il contenuto

enum NamedEnum { ONE = 1, TWO, THREE }
var n: NamedEnum = NamedEnum.ONE # l'enumerazione può essere usata anche come tipo

@onready var node_ref_typed := $Child as Node

@export var speed := 50.0

const CONSTANT := "Typed constant."

signal example(arg: int)

func _ready() -> void:
  # funzione che ritorna nulla
  x = "string" # ERRORE! Il tipo non può essere cambiato
  a.append("q") # ERRORE! Array[int] non può contenere stringhe
  return

func join(arg1: String, arg2: String) -> String:
  # la funzione prende in input due stringhe e ritorna una stringa
  return arg1 + arg2

func get_child_at(index: int) -> Node:
  # funzione prende in input un int e ritorna un nodo
  return get_children()[index]
```

## Ulteriori letture

* [Sito web di Godot](https://godotengine.org/)
* [Documentazione Godot](https://docs.godotengine.org/en/stable/)
* [Inizia con GDScript](https://docs.godotengine.org/en/stable/getting_started/scripting/gdscript/index.html)
* [NodePath](https://docs.godotengine.org/en/stable/classes/class_nodepath.html)
* [Signals](https://docs.godotengine.org/en/stable/getting_started/step_by_step/signals.html)
* [GDQuest](https://www.gdquest.com/)
* [GDScript.com](https://gdscript.com/)
