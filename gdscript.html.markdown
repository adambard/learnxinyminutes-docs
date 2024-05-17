---
language: GDScript
contributors:
    - ["Wichamir", "https://github.com/Wichamir/"]
filename: learngdscript.gd
---

GDScript is a dynamically typed scripting language made specifically for
free and open source game engine Godot. GDScript's syntax is similar to
Python's. Its main advantages are ease of use and tight integration with
the engine. It's a perfect fit for game development.

## Basics

```gdscript
# Single-line comments are written using hash symbol.
"""
  Multi-line
  comments
  are
  written
  using
  docstrings.
"""

# Script file is a class in itself and you can optionally define a name for it.
class_name MyClass

# Inheritance
extends Node2D

# Member variables
var x = 8 # int
var y = 1.2 # float
var b = true # bool
var s = "Hello World!" # String
var a = [1, false, "brown fox"] # Array - similar to list in Python,
                                # it can hold different types
                                # of variables at once.
var d = {
  "key" : "value",
  42 : true
} # Dictionary holds key-value pairs.
var p_arr = PoolStringArray(["Hi", "there", "!"]) # Pool arrays can
                                                  # only hold a certain type.

# Built-in vector types:
var v2 = Vector2(1, 2)
var v3 = Vector3(1, 2, 3)

# Constants
const ANSWER_TO_EVERYTHING = 42
const BREAKFAST = "Spam and eggs!"

# Enums
enum { ZERO, ONE , TWO, THREE }
enum NamedEnum { ONE = 1, TWO, THREE }

# Exported variables are visible in the inspector.
export(int) var age
export(float) var height
export var person_name = "Bob" # Export type hints are unnecessary
                        # if you set a default value.

# Functions
func foo():
  pass # pass keyword is a placeholder for future code

func add(first, second):
  return first + second

# Printing values
func printing():
  print("GDScript ", "is ", " awesome.")
  prints("These", "words", "are", "divided", "by", "spaces.")
  printt("These", "words", "are", "divided", "by", "tabs.")
  printraw("This gets printed to system console.")

# Math
func doing_math():
  var first = 8
  var second = 4
  print(first + second) # 12
  print(first - second) # 4
  print(first * second) # 32
  print(first / second) # 2
  print(first % second) # 0
  # There are also +=, -=, *=, /=, %= etc.,
  # however no ++ or -- operators.
  print(pow(first, 2)) # 64
  print(sqrt(second)) # 2
  printt(PI, TAU, INF, NAN) # built-in constants

# Control flow
func control_flow():
  x = 8
  y = 2 # y was originally a float,
        # but we can change its type to int
        # using the power of dynamic typing!
  
  if x < y:
    print("x is smaller than y")
  elif x > y:
    print("x is bigger than y")
  else:
    print("x and y are equal")
  
  var a = true
  var b = false
  var c = false
  if a and b or not c: # alternatively you can use &&, || and !
    print("This is true!")

  for i in range(20): # GDScript's range is similar to Python's
    print(i) # so this will print numbers from 0 to 19

  for i in 20: # unlike Python, you can loop over an int directly
    print(i) # so this will also print numbers from 0 to 19

  for i in ["two", 3, 1.0]: # iterating over an array
    print(i)
  
  while x > y:
    printt(x, y)
    y += 1

  x = 2
  y = 10
  while x < y:
    x += 1
    if x == 6:
      continue # 6 won't get printed because of continue statement
    prints("x is equal to:", x)
    if x == 7:
      break # loop will break on 7, so 8, 9 and 10 won't get printed

  match x:
    1:
      print("Match is similar to switch.")
    2:
      print("However you don't need to put cases before each value.")
    3:
      print("Furthermore each case breaks on default.")
      break # ERROR! Break statement is unnecessary!
    4:
      print("If you need fallthrough use continue.")
      continue
    _:
      print("Underscore is a default case.")
  
  # ternary operator (one line if-else statement)
  prints("x is", "positive" if x >= 0 else "negative")

# Casting
func casting_examples():
  var i = 42
  var f = float(42) # cast using variables constructor
  var b = i as bool # or using "as" keyword

# Override functions
# By a convention built-in overridable functions start with an underscore,
# but in practice you can override virtually any function.

# _init is called when object gets initialized
# This is the object's constructor.
func _init():
  # Initialize object's internal stuff here.
  pass

# _ready gets called when script's node and
# its children have entered the scene tree.
func _ready():
  pass

# _process gets called on every frame.
func _process(delta):
  # The delta argument passed to this function is a number of seconds,
  # which passed between the last frame and the current one.
  print("Delta time equals: ", delta)

# _physics_process gets called on every physics frame.
# That means delta should be constant.
func _physics_process(delta):
  # Simple movement using vector addition and multiplication.
  var direction = Vector2(1, 0) # or Vector2.RIGHT
  var speed = 100.0
  self.global_position += direction * speed * delta
  # self refers to current class instance

# When overriding you can call parent's function using the dot operator
# like here:
func get_children():
  # Do some additional things here.
  var r = .get_children() # call parent's implementation
  return r

# Inner class
class InnerClass:
  extends Object

  func hello():
    print("Hello from inner class!")

func use_inner_class():
  var ic = InnerClass.new()
  ic.hello()
  ic.free() # use free for memory cleanup
```

## Accessing other nodes in the scene tree

```gdscript
extends Node2D

var sprite # This variable will hold the reference.

# You can get references to other nodes in _ready.
func _ready() -> void:
  # NodePath is useful for accessing nodes.
  # Create NodePath by passing String to its constructor:
  var path1 = NodePath("path/to/something")
  # Or by using NodePath literal:
  var path2 = @"path/to/something"
  # NodePath examples:
  var path3 = @"Sprite" # relative path, immediate child of the current node
  var path4 = @"Timers/Firerate" # relative path, child of the child
  var path5 = @".." # current node's parent
  var path6 = @"../Enemy" # current node's sibling
  var path7 = @"/root" # absolute path, equivalent to get_tree().get_root()
  var path8 = @"/root/Main/Player/Sprite" # absolute path to Player's Sprite
  var path9 = @"Timers/Firerate:wait_time" # accessing properties
  var path10 = @"Player:position:x" # accessing subproperties

  # Finally, to get a reference use one of these:
  sprite = get_node(@"Sprite") as Sprite # always cast to the type you expect
  sprite = get_node("Sprite") as Sprite # here String gets
                                        # implicitly casted to NodePath
  sprite = get_node(path3) as Sprite
  sprite = get_node_or_null("Sprite") as Sprite
  sprite = $Sprite as Sprite

func _process(delta):
  # Now we can reuse the reference in other places.
  prints("Sprite has global_position of", sprite.global_position)

# Use onready keyword to assign a value to
# a variable just before _ready executes.
# This is a commonly used syntax sugar.
onready var tween = $Tween as Tween

# You can export NodePath, so you can assign it within the inspector.
export var nodepath = @""
onready var reference = get_node(nodepath) as Node
```

## Signals

Signal system is Godot's implementation of the observer programming
pattern. Here's an example:

```gdscript
class_name Player extends Node2D

var hp = 10

signal died() # define signal
signal hurt(hp_old, hp_new) # signals can take arguments

func apply_damage(dmg):
  var hp_old = hp
  hp -= dmg
  emit_signal("hurt", hp_old, hp) # emit signal and pass arguments
  if hp <= 0:
    emit_signal("died")

func _ready():
  # connect signal "died" to function "_on_death" defined in self
  self.connect("died", self, "_on_death")

func _on_death():
  self.queue_free() # destroy Player on death
```

## Type hints

GDScript can optionally use static typing.

```gdscript
extends Node

var x: int # define typed variable
var y: float = 4.2
var z := 1.0 # infer type based on default value using := operator

onready var node_ref_typed := $Child as Node

export var speed := 50.0

const CONSTANT := "Typed constant."

func _ready() -> void:
  # function returns nothing
  x = "string" # ERROR! Type can't be changed!
  return

func join(arg1: String, arg2: String) -> String:
  # function takes two Strings and returns a String
  return arg1 + arg2

func get_child_at(index: int) -> Node:
  # function takes an int and returns a Node
  return get_children()[index]

signal example(arg: int) # ERROR! Signals can't take typed arguments!
```

## Further Reading

* [Godot's Website](https://godotengine.org/)
* [Godot Docs](https://docs.godotengine.org/en/stable/)
* [Getting started with GDScript](https://docs.godotengine.org/en/stable/getting_started/scripting/gdscript/index.html)
* [NodePath](https://docs.godotengine.org/en/stable/classes/class_nodepath.html)
* [Signals](https://docs.godotengine.org/en/stable/getting_started/step_by_step/signals.html)
* [GDQuest](https://www.gdquest.com/)
* [GDScript.com](https://gdscript.com/)
