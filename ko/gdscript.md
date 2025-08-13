---
name: GDScript
contributors:
    - ["Wichamir", "https://github.com/Wichamir/"]
    - ["zacryol", "https://github.com/zacryol"]
filename: learngdscript.gd
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

GDScript는 무료 오픈 소스 게임 엔진 Godot를 위한 동적 및 정적 유형 스크립팅 언어입니다. 구문은 Python과 막연하게 유사합니다. 주요 장점은 사용 편의성과 엔진과의 긴밀한 통합입니다. 게임 개발에 완벽하게 적합합니다.

## 기본

```gdscript
# 한 줄 주석은 해시 기호를 사용하여 작성됩니다.
"""
  여러 줄
  주석은
  삼중
  따옴표
  문자열을
  사용하여
  작성됩니다.
"""

# 문서 주석은 클래스 및 필드에 설명을 추가할 수 있으며
# 엔진 내 문서에서 볼 수 있습니다.

## 이 클래스는 GDScript의 데모입니다.

# 스크립트 파일은 그 자체로 클래스이며 선택적으로 이름을 정의할 수 있습니다.
class_name MyClass

# 상속
extends Node2D

# 멤버 변수
var x = 8 # int
var y = 1.2 # float
var b = true # bool
var s = "Hello World!" # String
var a = [1, false, "brown fox"] # Array - Python의 목록과 유사하며,
                                # 한 번에 다른 유형의 변수를
                                # 보유할 수 있습니다.
var d = {
  "key" : "value",
  42 : true
} # Dictionary는 키-값 쌍을 보유합니다.
var p_arr = PackedStringArray(["Hi", "there", "!"]) # Packed Arrays는
                                                    # 특정 유형만 보유할 수 있습니다.

# 문서 주석은 속성에 적용할 수 있습니다.

## 이 개체가 점프한 횟수
var jump_count = 0

# 내장 벡터 유형:
var v2 = Vector2(1, 2)
var v3 = Vector3(1, 2, 3)

# 상수
const ANSWER_TO_EVERYTHING = 42
const BREAKFAST = "Spam and eggs!"

# 열거형
enum { ZERO, ONE , TWO, THREE }
enum NamedEnum { ONE = 1, TWO, THREE }

# 내보낸 변수는 검사기에서 볼 수 있습니다.
#
# 편집기가 어떤 옵션을 제공해야 하는지 알기 위해서는
# 유형 힌트(나중에 설명) 또는 기본값이 필요합니다.
@export var age: int
@export var height: float
@export var person_name = "Bob"
# 하지만 둘 다 허용됩니다.
@export var favorite_color: String = "Green"
@export var favorite_food := "Pizza"

# 함수
func foo():
  pass # pass 키워드는 향후 코드를 위한 자리 표시자입니다.

func add(first, second):
  return first + second

# 함수에 대한 문서 주석

## 점프 횟수 증가
func jump():
  jump_count += 1

# 값 인쇄
func printing():
  print("GDScript ", "is ", " awesome.")
  prints("These", "words", "are", "divided", "by", "spaces.")
  printt("These", "words", "are", "divided", "by", "tabs.")
  printraw("This gets printed to system console.")

  # 람다
  var my_lambda = func(): print("hello from lambda!")

  my_lambda.call()

# 수학
func doing_math():
  var first = 8
  var second = 4
  print(first + second) # 12
  print(first - second) # 4
  print(first * second) # 32
  print(first / second) # 2
  print(first % second) # 0
  # +=, -=, *=, /=, %= 등도 있지만,
  # ++ 또는 -- 연산자는 없습니다.
  print(pow(first, 2)) # 64
  print(sqrt(second)) # 2
  printt(PI, TAU, INF, NAN) # 내장 상수

# 제어 흐름
func control_flow():
  x = 8
  y = 2 # y는 원래 float였지만,
        # 동적 타이핑의 힘을 사용하여
        # 유형을 int로 변경할 수 있습니다!

  if x < y:
    print("x is smaller than y")
  elif x > y:
    print("x is bigger than y")
  else:
    print("x and y are equal")

  var a = true
  var b = false
  var c = false
  if a and b or not c: # 또는 &&, || 및 !를 사용할 수 있습니다.
    print("This is true!")

  for i in range(20): # GDScript의 범위는 Python과 유사합니다.
    print(i) # 따라서 이것은 0에서 19까지의 숫자를 인쇄합니다.

  for i in 20: # Python과 달리 int를 직접 반복할 수 있습니다.
    print(i) # 따라서 이것은 0에서 19까지의 숫자를 인쇄합니다.

  for i in ["two", 3, 1.0]: # 배열 반복
    print(i)

  while x > y:
    printt(x, y)
    y += 1

  x = 2
  y = 10
  while x < y:
    x += 1
    if x == 6:
      continue # 6은 continue 문 때문에 인쇄되지 않습니다.
    prints("x is equal to:", x)
    if x == 7:
      break # 루프는 7에서 중단되므로 8, 9, 10은 인쇄되지 않습니다.

  match x:
    1:
      print("Match is similar to switch.")
    2:
      print("However you don't need to put cases before each value.")
    3:
      print("Furthermore each case breaks on default.")
      break # 오류! Break 문은 불필요합니다!
    4:
      print("If you need fallthrough use continue.")
      continue
    _:
      print("Underscore is a default case.")

  # 삼항 연산자 (한 줄 if-else 문)
  prints("x is", "positive" if x >= 0 else "negative")

# 캐스팅
func casting_examples():
  var i = 42
  var f = float(42) # 변수 생성자를 사용하여 캐스팅
  var b = i as bool # 또는 "as" 키워드를 사용하여

# 재정의 함수
# 규칙에 따라 내장 재정의 가능 함수는 밑줄로 시작하지만,
# 실제로는 거의 모든 함수를 재정의할 수 있습니다.

# _init은 개체가 초기화될 때 호출됩니다.
# 이것은 개체의 생성자입니다.
func _init():
  # 여기에서 개체의 내부 항목을 초기화합니다.
  pass

# _ready는 스크립트의 노드와
# 해당 자식이 장면 트리에 들어갈 때 호출됩니다.
func _ready():
  pass

# _process는 모든 프레임에서 호출됩니다.
func _process(delta):
  # 이 함수에 전달된 델타 인수는 초 단위의 숫자이며,
  # 마지막 프레임과 현재 프레임 사이에 경과된 시간입니다.
  print("Delta time equals: ", delta)

# _physics_process는 모든 물리 프레임에서 호출됩니다.
# 즉, 델타는 일정해야 합니다.
func _physics_process(delta):
  # 벡터 덧셈 및 곱셈을 사용한 간단한 이동.
  var direction = Vector2(1, 0) # 또는 Vector2.RIGHT
  var speed = 100.0
  self.global_position += direction * speed * delta
  # self는 현재 클래스 인스턴스를 참조합니다.

# 재정의할 때 점 연산자를 사용하여 부모의 함수를 호출할 수 있습니다.
# 여기처럼:
func get_children():
  # 여기에서 추가 작업을 수행합니다.
  var r = super() # 부모의 구현 호출
  return r

# 내부 클래스
class InnerClass:
  extends Object

  func hello():
    print("Hello from inner class!")

func use_inner_class():
  var ic = InnerClass.new()
  ic.hello()
  ic.free() # 메모리 정리를 위해 free 사용
```

## 장면 트리의 다른 노드에 액세스

```gdscript
extends Node2D

var sprite # 이 변수는 참조를 보유합니다.

# _ready에서 다른 노드에 대한 참조를 얻을 수 있습니다.
func _ready() -> void:
  # NodePath는 노드에 액세스하는 데 유용합니다.
  # 생성자에 문자열을 전달하여 NodePath를 만듭니다:
  var path1 = NodePath("path/to/something")
  # 또는 NodePath 리터럴을 사용합니다:
  var path2 = ^"path/to/something"
  # NodePath 예제:
  var path3 = ^"Sprite" # 상대 경로, 현재 노드의 직계 자식
  var path4 = ^"Timers/Firerate" # 상대 경로, 자식의 자식
  var path5 = ^".." # 현재 노드의 부모
  var path6 = ^"../Enemy" # 현재 노드의 형제
  var path7 = ^"/root" # 절대 경로, get_tree().get_root()와 동일
  var path8 = ^"/root/Main/Player/Sprite" # 플레이어의 스프라이트에 대한 절대 경로
  var path9 = ^"Timers/Firerate:wait_time" # 속성 액세스
  var path10 = ^"Player:position:x" # 하위 속성 액세스

  # 마지막으로 참조를 얻으려면 다음 중 하나를 사용하십시오:
  sprite = get_node(^"Sprite") as Sprite # 항상 예상하는 유형으로 캐스팅
  sprite = get_node("Sprite") as Sprite # 여기서 문자열은
                                        # 암시적으로 NodePath로 캐스팅됩니다.
  sprite = get_node(path3) as Sprite
  sprite = get_node_or_null("Sprite") as Sprite
  sprite = $Sprite as Sprite

func _process(delta):
  # 이제 다른 곳에서 참조를 재사용할 수 있습니다.
  prints("Sprite has global_position of", sprite.global_position)

# @onready 주석을 사용하여
# _ready가 실행되기 직전에 변수에 값을 할당합니다.
# 이것은 일반적으로 사용되는 구문 설탕입니다.
@onready var other_sprite = $Sprite as Sprite

# NodePath를 내보낼 수 있으므로 검사기 내에서 할당할 수 있습니다.
@export var nodepath = ^""
@onready var reference = get_node(nodepath) as Node

# 또는 노드를 직접 내보냅니다.
@export var other_reference: Node
```

## 신호

신호 시스템은 Godot의 관찰자 프로그래밍 패턴 구현입니다. 다음은 예입니다:

```gdscript
class_name Player extends Node2D

var hp = 10

# 문서 주석은 신호에도 적용될 수 있습니다.

## 플레이어가 죽을 때 발생
signal died() # 신호 정의
signal hurt(hp_old, hp_new) # 신호는 인수를 가질 수 있습니다.

func apply_damage(dmg):
  var hp_old = hp
  hp -= dmg
  hurt.emit(hp_old, hp) # 신호를 발생시키고 인수 전달
  if hp <= 0:
    died.emit()

func _ready():
  # 신호 "died"를 self에 정의된 함수 "_on_death"에 연결
  died.connect(_on_death)
  # 대체 방법
  # 대상 개체가 self가 아닌 경우 필요
  # died.connect(Callable(self, &"_on_death"))

func _on_death():
  queue_free() # 죽을 때 플레이어 파괴
```

## 유형 힌트

GDScript는 코드 명확성과 성능 이점을 위해 선택적으로 정적 타이핑을 사용할 수 있습니다.

```gdscript
extends Node

var x: int # 유형이 지정된 변수 정의
var y: float = 4.2
var z := 1.0 # := 연산자를 사용하여 기본값을 기반으로 유형 추론

var a: Array[int] = [1, 2, 3] # 배열은 유형 콘텐츠도 지정할 수 있습니다.

enum NamedEnum { ONE = 1, TWO, THREE }
var n: NamedEnum = NamedEnum.ONE # 열거형은 유형으로도 사용할 수 있습니다.

@onready var node_ref_typed := $Child as Node

@export var speed := 50.0

const CONSTANT := "Typed constant."

signal example(arg: int)

func _ready() -> void:
  # 함수는 아무것도 반환하지 않습니다.
  x = "string" # 오류! 유형을 변경할 수 없습니다!
  a.append("q") # 오류! Array[int]는 문자열을 보유할 수 없습니다!
  return

func join(arg1: String, arg2: String) -> String:
  # 함수는 두 개의 문자열을 사용하고 문자열을 반환합니다.
  return arg1 + arg2

func get_child_at(index: int) -> Node:
  # 함수는 int를 사용하고 노드를 반환합니다.
  return get_children()[index]
```

## 추가 자료

* [Godot 웹사이트](https://godotengine.org/)
* [Godot 문서](https://docs.godotengine.org/en/stable/)
* [GDScript 시작하기](https://docs.godotengine.org/en/stable/getting_started/scripting/gdscript/index.html)
* [NodePath](https://docs.godotengine.org/en/stable/classes/class_nodepath.html)
* [신호](https://docs.godotengine.org/en/stable/getting_started/step_by_step/signals.html)
* [GDQuest](https://www.gdquest.com/)
* [GDScript.com](https://gdscript.com/)