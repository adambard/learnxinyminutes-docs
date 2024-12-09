---
language: GDScript
contributors:
    - ["Wichamir", "https://github.com/Wichamir/"]
    - ["zacryol", "https://github.com/zacryol"]
filename: learngdscript.gd
translators:
    - ["R1cHero", "https://github.com/R1cHero"]
lang: tr-tr
---

GDScript, özgür ve açık kaynaklı oyun motoru olan Godot için dinamik ve statik olarak 
yazılmış bir kodlama dilidir. Yazılış şekli Python'a biraz benzerdir. 
Başlıca avantajları kullanım kolaylığı ve motorla olan uyumudur. 
Oyun geliştirme için mükemmel bir uyum sağlar.

## Temeller

```gdscript
# Tek satırlık yorumlar, '#' simgesi kullanılarak yazılır.
"""
  Çok 
  satırlı
  yorumlar
  üç
  tırnak
  kullanılarak
  yazılır
"""

# Belge yorumları (Doc Comments) sınıflara ve alanlara açıklama ekleyebilir.
# bunlar, motor içi dokümanlarda görüntülenebilir.

## Bu sınıf, GDScript'in bir gösterimidir

# Komut dosyası kendi başına bir sınıftır ve isteğe bağlı olarak bunun için bir ad tanımlayabilirsiniz.
class_name MyClass

# Kalıtım (Inheritance) komut dosyasının bağlı olduğu Node'a göre 'Node2D' kısmı değişkenlik gösterir.
extends Node2D

# Değişken türleri
var x = 8 # int
var y = 1.2 # float
var b = true # bool
var s = "Merhaba Dünya!" # String
var a = [1, false, "turuncu kedi"] # Array - Python'daki listeye benzer,
                                # aynı anda farklı tipte 
                                # değişkenleri tutabilir.
var d = {
  "key" : "value",
  42 : true
} # Sözlük anahtar-değer (key-value) çiftlerini tutar.
var p_arr = PackedStringArray(["Hey", "selamlar", "!"]) # Paketlenmiş diziler yalnızca
                                                    # belirli bir türü tutabilir.

# Doküman yorumları, özelliklere uygulanabilir.

## Bu değişken nesnenin kaç kez zıpladığını tutar.
var jump_count = 0

# Motorda bulunan yerleşik vektör türleri:
var v2 = Vector2(1, 2)
var v3 = Vector3(1, 2, 3)

# Sabitler (Constants)
const ANSWER_TO_EVERYTHING = 42
const BREAKFAST = "Sucuklu yumurta!"

# Enumlar
enum { ZERO, ONE , TWO, THREE }
enum NamedEnum { ONE = 1, TWO, THREE }

# Dışa aktarılan değişkenler 'inspector' de görünür.
#
# Editörün hangi seçenekleri vereceğini bilmesi için bir tür ipucu (daha sonra açıklanacaktır) 
# veya varsayılan bir değere ihtiyaç vardır.
@export var age: int
@export var height: float
@export var person_name = "Ahmet"
# Ama ikisi de kabul edilebilir.
@export var favorite_color: String = "Kırmızı"
@export var favorite_food := "Lahmacun"

# Fonksiyonlar
func foo():
  pass # pass anahtar sözcüğü gelecekteki yazılacak olan kod için bir yer tutucudur. 
       # Editörde hata olarak göstermesini engellemek için de kullanılır.

func add(first, second):
  return first + second

# Fonksiyonlar üzerinde doküman yorumları yazma

## Zıplama Sayısını Artırma
func jump():
  jump_count += 1

# Değerleri yazdırma
func printing():
  print("GDScript ", "mükemmel.")
  prints("Bu", "kelimeler", "boşluklarla", "ayrıldı.")
  printt("Bu", "kelimeler", "tablarla", "ayrıldı.")
  printraw("Bu sistem konsoluna yazdırılır.")
  printerr("Bu konsolda hata olarak gösterilir.")

  # İsimsiz fonksiyonlar (Lambdas)
  var my_lambda = func(): print("lambda'dan merhaba!")

  my_lambda.call()

# Matematik
func doing_math():
  var first = 8
  var second = 4
  print(first + second) # 12
  print(first - second) # 4
  print(first * second) # 32
  print(first / second) # 2
  print(first % second) # 0
  # Ayrıca bunlar var +=, -=, *=, /=, %= vb.,
  # ++ veya -- operatörleri yok.
  print(pow(first, 2)) # 64
  print(sqrt(second)) # 2
  printt(PI, TAU, INF, NAN) # Yerleşik (built-in) sabitler (constants)

# Kontrol akışı
func control_flow():
  x = 8
  y = 2 # y orjinalde float'dı,
        # ancak "dynamic typing" gücünü kullanarak 
        # tipini int'e değiştirebiliriz!

  if x < y:
    print("x, y'den küçüktür.")
  elif x > y:
    print("x, y'den büyüktür.")
  else:
    print("x ve y eşittir.")

  var a = true
  var b = false
  var c = false
  if a and b or not c: # alternatif olarak bunları kullanabilirsiniz: &&, || ve !
    print("Değer: true")

  for i in range(20): # GDScript'de "range" Python'dakine benzerdir.
    print(i) # bu 0'dan 19'a kadar sayıları yazdıracaktır.

  for i in 20: # Python'dan farklı olarak, doğrudan bir int üzerinde "range" kullanmadan döngüye girebilirsiniz.
    print(i) # bu da 0'dan 19'a kadar olan sayıları da yazdıracaktır.

  for i in ["two", 3, 1.0]: # Bir dizi üzerinde yineleme (iterasyon) yapma.
    print(i)

  while x > y:
    printt(x, y)
    y += 1

  x = 2
  y = 10
  while x < y:
    x += 1
    if x == 6:
      continue # 6, "continue" ifadesi nedeniyle yazdırılmayacak.
    prints("x eşittir:", x)
    if x == 7:
      break # döngü 7'de duracak, bu yüzden 8, 9 ve 10 yazdırılmayacak.

  match x:
    1:
      print("Match, switch'e benzerdir.")
    2:
      print("Ancak her değerden önce "case" koymanıza gerek yok.")
    3:
      print("Ayrıca her "case" varsayılan olarak durdurulur.")
      break # HATA! "Break" ifadesi gereksiz!
    4:
      print("Eğer "fallthrough" kullanmaya ihtiyacınız varsa "continue" kullanın.")
      continue
    _:
      print("Alt çizgi varsayılan bir "case"dir.")

  # Üç terimli operatör (tek satırda yazılabilen if-else)
  prints("x", "pozitif" if x >= 0 else "negatif")

# Bir veri tipini başka bir veri tipine dönüştürme (Casting)
func casting_examples():
  var i = 42
  var f = float(42) # Değişkenlerin kurucusunu (constructor) kullanarak
  var b = i as bool # veya "as" anahtar kelimesini kullanarak tür dönüştürme (casting)

# Override fonksiyonlar
# Built-in kurallı "overridable functions" bir alt çizgi ile başlar
# ama pratikte hemen hemen her fonksiyonu geçersiz kılabilirsiniz.

# _init nesne başlatıldığında çağrılır.
# Bu, nesnenin kurucusudur (constructor).
func _init():
  # Burada nesnenin iç öğelerini başlatın.
  pass

# _ready, komut dosyasında
# node ve children node sahneye girdiğinde çağrılır.
func _ready():
  pass

# _process her karede çağrılır.
func _process(delta):
  # Bu fonksiyona iletilen delta argümanı,
  # son kare ile anlık kare arasında geçen saniye sayısıdır.
  print("Delta zamanı: ", delta)

# _physics_process her fizik karesinde çağrılır.
# Deltanın sabit olması gerektiği anlamına gelir.
func _physics_process(delta):
  # Vektör toplama ve çarpma kullanarak hareket ettirme.
  var direction = Vector2(1, 0) # ya da Vector2.RIGHT
  var speed = 100.0
  self.global_position += direction * speed * delta
  # self geçerli sınıf örneğini belirtir.

# Geçersiz kılma sırasında, buradaki gibi nokta operatörünü kullanarak 
# parent işlevini çağırabilirsiniz:
func get_children():
  # Bazı ek şeyler ekleyebilirsiniz.
  var r = super() # Parent implementasyonunu çağırma 
  return r

# Dahili sınıflar (Inner class)
class InnerClass:
  extends Object

  func hello():
    print("Dahili sınıftan merhabalar!")

func use_inner_class():
  var ic = InnerClass.new()
  ic.hello()
  ic.free() # Ramde yer açmak için "free" kullanın.
```

## Sahne ağacındaki (Scene Tree) diğer node'lara erişim

```gdscript
extends Node2D

var sprite # Bu değişken referansı tutacak.

# _ready'de diğer node'lara referanslar alabilirsiniz.
func _ready() -> void:
  # "NodePath" node'lara erişmek için kullanışlıdır.
  var path1 = NodePath("path/to/something")
  # Ya da böyle kullanabilirsiniz:
  var path2 = ^"path/to/something"
  # NodePath örnekleri:
  var path3 = ^"Sprite" # geçerli node'un child node'u
  var path4 = ^"Timers/Firerate" # child node'un child node'u
  var path5 = ^".." # geçerli node'un parent node'u
  var path6 = ^"../Enemy" # geçerli node'un kardeşi
  var path7 = ^"/root" # ana yol, get_tree().get_root() ile eşdeğerdir.
  var path8 = ^"/root/Main/Player/Sprite" # Player'ın Sprite'ına giden ana yol
  var path9 = ^"Timers/Firerate:wait_time" # özelliklere erişme
  var path10 = ^"Player:position:x" # alt özelliklere erişim

  # Son olarak, referans almak için bunlardan birini kullanabilirsiniz:
  sprite = get_node(^"Sprite") as Sprite # her zaman belirttiğiniz türü yazın
  sprite = get_node("Sprite") as Sprite # burada String
                                        # NodePath'e dönüştürülür
  sprite = get_node(path3) as Sprite
  sprite = get_node_or_null("Sprite") as Sprite
  sprite = $Sprite as Sprite

func _process(delta):
  # Artık referansı başka yerlerde tekrar kullanabiliriz.
  prints("Sprite has global_position of", sprite.global_position)

# _ready çalıştırılmadan hemen önce 
# bir değişkene değer atamak için @onready kullanın.
@onready var other_sprite = $Sprite as Sprite

# NodePath'i dışa aktarabilir, böylece onu "inspector" içinde atama yapabilirsiniz.
@export var nodepath = ^""
@onready var reference = get_node(nodepath) as Node

# Veya Node'u doğrudan dışa aktarın
@export var other_reference: Node
```

## Sinyaller (Signals)

Sinyal sistemi Godot'nun gözlemci programlama modelinin uygulamasıdır. 
İşte bir örnek:

```gdscript
class_name Player extends Node2D

var hp = 10

# Belge yorumları sinyallere de yansıyabilir

## Oyuncu öldüğünde sinyal gönderir (emit).
signal died() # sinyali tanımlama
signal hurt(hp_old, hp_new) # sinyallere değişken tanımlanabilir.

func apply_damage(dmg):
  var hp_old = hp
  hp -= dmg
  hurt.emit(hp_old, hp) # sinyal gönderme ve değişkenleri iletme
  if hp <= 0:
    died.emit()

func _ready():
  # "died" sinyali kendi içinde tanımlanan "_on_death" fonksiyonuna bağlama 
  died.connect(_on_death)
  # Hedef nesne kendisi değilse 
  # alternatif bir yol gerekir.
  # died.connect(Callable(self, &"_on_death"))

func _on_death():
  queue_free() # oyuncu öldüğünde sahneden siler
```

## Faydalı İpuçları

GDScript'te hem kod netliği hem de performans avantajları için
opsiyonel olarak "static typing" kullanılabilir.

```gdscript
extends Node

var x: int # değişkenin türünü belirtme
var y: float = 4.2
var z := 1.0 # ":=" operatörünü kullanmak üstteki tanımlama ile aynıdır.

var a: Array[int] = [1, 2, 3] # Dizinin tür içeriğini belirtme

enum NamedEnum { ONE = 1, TWO, THREE }
var n: NamedEnum = NamedEnum.ONE # Enum da bir tür olarak kullanılabilir.

@onready var node_ref_typed := $Child as Node

@export var speed := 50.0

const CONSTANT := "Bu bir sabit atamadır (constant)."

signal example(arg: int)

func _ready() -> void:
  # fonksiyon hiçbir şey döndürmez
  x = "string" # HATA! Tür değiştirilemez!
  a.append("q") # HATA! Array[int] string tutamaz!
  return

func join(arg1: String, arg2: String) -> String:
  # Fonksiyon iki tane string alır ve bir tane string döndürür.
  return arg1 + arg2

func get_child_at(index: int) -> Node:
  # Fonksiyon int alır ve node döndürür.
  return get_children()[index]
```

## Daha fazla bilgi almak için

* [Godot's Website](https://godotengine.org/)
* [Godot Docs](https://docs.godotengine.org/en/stable/)
* [Getting started with GDScript](https://docs.godotengine.org/en/stable/getting_started/scripting/gdscript/index.html)
* [NodePath](https://docs.godotengine.org/en/stable/classes/class_nodepath.html)
* [Signals](https://docs.godotengine.org/en/stable/getting_started/step_by_step/signals.html)
* [GDQuest](https://www.gdquest.com/)
* [GDScript.com](https://gdscript.com/)
