---
language: GDScript
contributors:
    - ["Wichamir", "https://github.com/Wichamir/"]
translators:
    - ["ShiftWatchOut", "https://github.com/ShiftWatchOut"]
filename: learngdscript-cn.gd
lang: zh-cn
---

GDScript 是一种动态类型的脚本语言，专门为免费开源游戏引擎 Godot 制作。 GDScript 的语法类似 Python。
它的主要优点是易于使用和与引擎深度集成。 它非常适合游戏开发。

## 基础

```gdscript
# 单行注释使用 # 号书写。
"""
  多行
  注释
  是
  使用
  文档字符串(docstring)
  书写。
"""

# 脚本文件本身默认是一个类，文件名为类名，您也可以为其定义其他名称。
class_name MyClass

# 继承
extends Node2D

# 成员变量
var x = 8 # 整型
var y = 1.2 # 浮点型
var b = true # 布尔型
var s = "Hello World!" # 字符串
var a = [1, false, "brown fox"] # 数组(Array) - 类似于 Python 的列表(list),
                                # 它可以同时保存不同类型的变量。
var d = {
  "key" : "value",
  42 : true
} # 字典包含键值对。
var p_arr = PoolStringArray(["Hi", "there", "!"]) # 池数组只能包含单一类型。
                                                  # 放入其他类型会被转换为目标类型

# 内置向量类型:
var v2 = Vector2(1, 2)
var v3 = Vector3(1, 2, 3)

# 常量
const ANSWER_TO_EVERYTHING = 42
const BREAKFAST = "Spam and eggs!"

# 枚举
enum { ZERO, ONE , TWO, THREE }
enum NamedEnum { ONE = 1, TWO, THREE }

# 导出的变量将在检查器中可见。
export(int) var age
export(float) var height
export var person_name = "Bob" # 如果设置了默认值，则不需要类型注解。

# 函数
func foo():
  pass # pass 关键字是未书写的代码的占位符

func add(first, second):
  return first + second

# 打印值
func printing():
  print("GDScript ", "简直", "棒呆了")
  prints("这", "些", "字", "被", "空", "格", "分", "割")
  printt("这", "些", "字", "被", "制", "表", "符", "分", "割")
  printraw("这句话将被打印到系统控制台。")

# 数学
func doing_math():
  var first = 8
  var second = 4
  print(first + second) # 12
  print(first - second) # 4
  print(first * second) # 32
  print(first / second) # 2
  print(first % second) # 0
  # 还有 +=, -=, *=, /=, %= 等操作符，但并没有 ++ 和 -- .
  print(pow(first, 2)) # 64
  print(sqrt(second)) # 2
  printt(PI, TAU, INF, NAN) # 内置常量

# 控制流
func control_flow():
  x = 8
  y = 2 # y 最初被设为一个浮点数，
        # 但我们可以利用语言提供的动态类型能力将它的类型变为整型！
  
  if x < y:
    print("x 小于 y")
  elif x > y:
    print("x 大于 y")
  else:
    print("x 等于 y")
  
  var a = true
  var b = false
  var c = false
  if a and b or not c: # 你也可以用 &&, || 和 !
    print("看到这句说明上面的条件判断为真！")

  for i in range(20): # GDScript 有类似 Python 的 range 函数
    print(i) # 所以这句将打印从 0 到 19 的数字

  for i in 20: # 与 Python 略有不同的是，你可以直接用一个整型数开始循环
    print(i) # 所以这行代码也将打印从 0 到 19 的数字

  for i in ["two", 3, 1.0]: # 遍历数组
    print(i)
  
  while x > y:
    printt(x, y)
    y += 1

  x = 2
  y = 10
  while x < y:
    x += 1
    if x == 6:
      continue # continue 语句使 x 等于 6 时，程序跳过这次循环后面的代码，不会打印 6。
    prints("x 等于：", x)
    if x == 7:
      break # 循环将在 x 等于 7 处跳出，后续所有循环不再执行，因此不会打印 8、9 和 10

  match x:
    1:
      print("match 很像其他语言中的 switch.")
    2:
      print("但是，您不需要在每个值之前写一个 case 关键字。")
    3:
      print("此外，每种情况都会默认跳出。")
      break # 错误！不要在 match 里用 break 语句！
    4:
      print("如果您需要跳过后续代码，这里也使用 continue 关键字。")
      continue
    _:
      print("下划线分支，在其他分支都不满足时，在这里书写默认的逻辑。")
  
  # 三元运算符 (写在一行的 if-else 语句)
  prints("x 是", "正值" if x >= 0 else "负值")

# 类型转换
func casting_examples():
  var i = 42
  var f = float(42) # 使用变量构造函数强制转换
  var b = i as bool # 或使用 as 关键字

# 重载函数
# 通常，我们只会重载以下划线开头的内置函数，
# 但实际上您可以重载几乎任何函数。

# _init 在对象初始化时被调用。
# 这是对象的构造函数。
func _init():
  # 在此处初始化对象的内部属性。
  pass

# _ready 在脚本节点及其子节点进入场景树时被调用。
func _ready():
  pass

# _process 在每一帧上都被调用。
func _process(delta):
  # 传递给此函数的 delta 参数是时间，即从上一帧到当前帧经过的秒数。
  print("Delta 时间为：", delta)

# _physics_process 在每个物理帧上都被调用。
# 这意味着 delta 应该是恒定的。
func _physics_process(delta):
  # 使用向量加法和乘法进行简单移动。
  var direction = Vector2(1, 0) # 或使用 Vector2.RIGHT
  var speed = 100.0
  self.global_position += direction * speed * delta
  # self 指向当前类的实例

# 重载函数时，您可以使用 . 运算符调用父函数
# like here:
func get_children():
  # 在这里做一些额外的事情。
  var r = .get_children() # 调用父函数的实现
  return r

# 内部类
class InnerClass:
  extends Object

  func hello():
    print("来自内部类的 Hello！")

func use_inner_class():
  var ic = InnerClass.new()
  ic.hello()
  ic.free() # 可以自行释放内存
```

## 访问场景树中其他节点

```gdscript
extends Node2D

var sprite # 该变量将用来保存引用。

# 您可以在 _ready 中获取对其他节点的引用。
func _ready() -> void:
  # NodePath 对于访问节点很有用。
  # 将 String 传递给其构造函数来创建 NodePath：
  var path1 = NodePath("path/to/something")
  # 或者使用 NodePath 字面量:
  var path2 = @"path/to/something"
  # NodePath 示例:
  var path3 = @"Sprite" # 相对路径，当前节点的直接子节点
  var path4 = @"Timers/Firerate" # 相对路径，子节点的子节点
  var path5 = @".." # 当前节点的父节点
  var path6 = @"../Enemy" # 当前节点的兄弟节点
  var path7 = @"/root" # 绝对路径，等价于 get_tree().get_root()
  var path8 = @"/root/Main/Player/Sprite" # Player 的 Sprite 的绝对路径
  var path9 = @"Timers/Firerate:wait_time" # 访问属性
  var path10 = @"Player:position:x" # 访问子属性

  # 最后，获取节点引用可以使用以下方法:
  sprite = get_node(@"Sprite") as Sprite # 始终转换为您期望的类型
  sprite = get_node("Sprite") as Sprite # 这里 String 被隐式转换为 NodePath
  sprite = get_node(path3) as Sprite
  sprite = get_node_or_null("Sprite") as Sprite
  sprite = $Sprite as Sprite

func _process(delta):
  # 现在我们就可以在别处使用 sprite 里保存的引用了。
  prints("Sprite 有一个全局位置 ", sprite.global_position)

# 在 _ready 执行之前，使用 onready 关键字为变量赋值。
# 这是一种常用的语法糖。
onready var tween = $Tween as Tween

# 您可以导出这个 NodePath，以便在检查器中给它赋值。
export var nodepath = @""
onready var reference = get_node(nodepath) as Node
```

## 信号(Signals)

信号系统是 Godot 对观察者编程模式的实现。例子如下:

```gdscript
class_name Player extends Node2D

var hp = 10

signal died() # 定义一个信号
signal hurt(hp_old, hp_new) # 信号可以带参数

func apply_damage(dmg):
  var hp_old = hp
  hp -= dmg
  emit_signal("hurt", hp_old, hp) # 发出信号并传递参数
  if hp <= 0:
    emit_signal("died")

func _ready():
  # 将信号 "died" 连接到 self 中定义的 _on_death 函数
  self.connect("died", self, "_on_death")

func _on_death():
  self.queue_free() # 死亡时销毁 Player
```

## 类型注解

GDScript 可以选择性地使用静态类型。

```gdscript
extends Node

var x: int # 定义带有类型的变量
var y: float = 4.2
var z := 1.0 # 使用 := 运算符根据默认值推断类型

onready var node_ref_typed := $Child as Node

export var speed := 50.0

const CONSTANT := "Typed constant."

func _ready() -> void:
  # 此函数不返回任何东西
  x = "string" # 错误！不要更改类型！
  return

func join(arg1: String, arg2: String) -> String:
  # 此函数接受两个 String 并返回一个 String。
  return arg1 + arg2

func get_child_at(index: int) -> Node:
  # 此函数接受一个 int 并返回一个 Node
  return get_children()[index]

signal example(arg: int) # 错误！信号不能接受类型参数！
```

## 延展阅读

* [Godot's Website](https://godotengine.org/)
* [Godot Docs](https://docs.godotengine.org/en/stable/)
* [Getting started with GDScript](https://docs.godotengine.org/en/stable/getting_started/scripting/gdscript/index.html)
* [NodePath](https://docs.godotengine.org/en/stable/classes/class_nodepath.html)
* [Signals](https://docs.godotengine.org/en/stable/getting_started/step_by_step/signals.html)
* [GDQuest](https://www.gdquest.com/)
* [GDScript.com](https://gdscript.com/)
