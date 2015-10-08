---
language: clojure
filename: learnclojure-cn.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Bill Zhang", "http://jingege.github.io/"]
lang: zh-cn
---

Clojure是运行在JVM上的Lisp家族中的一员。她比Common Lisp更强调纯[函数式编程](https://en.wikipedia.org/wiki/Functional_programming)，且自发布时便包含了一组工具来处理状态。

这种组合让她能十分简单且自动地处理并发问题。

(你需要使用Clojure 1.2或更新的发行版)

```clojure
; 注释以分号开始。

; Clojure代码由一个个form组成， 即写在小括号里的由空格分开的一组语句。
; Clojure解释器会把第一个元素当做一个函数或者宏来调用，其余的被认为是参数。

; Clojure代码的第一条语句一般是用ns来指定当前的命名空间。
(ns learnclojure)

; 更基本的例子:

; str会使用所有参数来创建一个字符串
(str "Hello" " " "World") ; => "Hello World"

; 数学计算比较直观
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2

; 等号是 =
(= 1 1) ; => true
(= 2 1) ; => false

; 逻辑非
(not true) ; => false

; 嵌套的form工作起来应该和你预想的一样
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

; 类型
;;;;;;;;;;;;;

; Clojure使用Java的Object来描述布尔值、字符串和数字
; 用函数 `class` 来查看具体的类型
(class 1) ; 整形默认是java.lang.Long类型
(class 1.); 浮点默认是java.lang.Double类型的
(class ""); String是java.lang.String类型的，要用双引号引起来
(class false) ; 布尔值是java.lang.Boolean类型的
(class nil); "null"被称作nil

; 如果你想创建一组数据字面量，用单引号(')来阻止form被解析和求值
'(+ 1 2) ; => (+ 1 2)
; (单引号是quote的简写形式，故上式等价于(quote (+ 1 2)))

; 可以对一个引用列表求值
(eval '(+ 1 2)) ; => 3

; 集合（Collection）和序列
;;;;;;;;;;;;;;;;;;;

; List的底层实现是链表，Vector的底层实现是数组
; 二者也都是java类
(class [1 2 3]); => clojure.lang.PersistentVector
(class '(1 2 3)); => clojure.lang.PersistentList

; list本可以写成(1 2 3), 但必须用引用来避免被解释器当做函数来求值。
; (list 1 2 3)等价于'(1 2 3)

; 集合其实就是一组数据
; List和Vector都是集合:
(coll? '(1 2 3)) ; => true
(coll? [1 2 3]) ; => true

; 序列 (seqs) 是数据列表的抽象描述
; 只有列表才可称作序列。
(seq? '(1 2 3)) ; => true
(seq? [1 2 3]) ; => false

; 序列被访问时只需要提供一个值，所以序列可以被懒加载——也就意味着可以定义一个无限序列：
(range 4) ; => (0 1 2 3)
(range) ; => (0 1 2 3 4 ...) (无限序列)
(take 4 (range)) ;  (0 1 2 3)

; cons用以向列表或向量的起始位置添加元素
(cons 4 [1 2 3]) ; => (4 1 2 3)
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; conj将以最高效的方式向集合中添加元素。
; 对于列表，数据会在起始位置插入，而对于向量，则在末尾位置插入。
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; 用concat来合并列表或向量
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; 用filter来过滤集合中的元素，用map来根据指定的函数来映射得到一个新的集合
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3]) ; => (2)

; recuce使用函数来规约集合
(reduce + [1 2 3 4])
; = (+ (+ (+ 1 2) 3) 4)
; => 10

; reduce还能指定一个初始参数
(reduce conj [] '(3 2 1))
; = (conj (conj (conj [] 3) 2) 1)
; => [3 2 1]

; 函数
;;;;;;;;;;;;;;;;;;;;;

; 用fn来创建函数。函数的返回值是最后一个表达式的值
(fn [] "Hello World") ; => fn

; (你需要再嵌套一组小括号来调用它)
((fn [] "Hello World")) ; => "Hello World"

; 你可以用def来创建一个变量（var）
(def x 1)
x ; => 1

; 将函数定义为一个变量（var）
(def hello-world (fn [] "Hello World"))
(hello-world) ; => "Hello World"

; 你可用defn来简化函数的定义
(defn hello-world [] "Hello World")

; 中括号内的内容是函数的参数。
(defn hello [name]
  (str "Hello " name))
(hello "Steve") ; => "Hello Steve"

; 你还可以用这种简写的方式来创建函数：
(def hello2 #(str "Hello " %1))
(hello2 "Fanny") ; => "Hello Fanny"

; 函数也可以有多个参数列表。
(defn hello3
  ([] "Hello World")
  ([name] (str "Hello " name)))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"

; 可以定义变参函数，即把&后面的参数全部放入一个序列
(defn count-args [& args]
  (str "You passed " (count args) " args: " args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"

; 可以混用定参和变参（用&来界定）
(defn hello-count [name & args]
  (str "Hello " name ", you passed " (count args) " extra args"))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"


; 哈希表
;;;;;;;;;;

; 基于hash的map和基于数组的map（即arraymap）实现了相同的接口，hashmap查询起来比较快，
; 但不保证元素的顺序。
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; arraymap在足够大的时候，大多数操作会将其自动转换成hashmap，
; 所以不用担心(对大的arraymap的查询性能)。

; map支持很多类型的key，但推荐使用keyword类型
; keyword类型和字符串类似，但做了一些优化。
(class :a) ; => clojure.lang.Keyword

(def stringmap {"a" 1, "b" 2, "c" 3})
stringmap  ; => {"a" 1, "b" 2, "c" 3}

(def keymap {:a 1, :b 2, :c 3})
keymap ; => {:a 1, :c 3, :b 2}

; 顺便说一下，map里的逗号是可有可无的，作用只是提高map的可读性。

; 从map中查找元素就像把map名作为函数调用一样。
(stringmap "a") ; => 1
(keymap :a) ; => 1

; 可以把keyword写在前面来从map中查找元素。
(:b keymap) ; => 2

; 但不要试图用字符串类型的key来这么做。
;("a" stringmap)
; => Exception: java.lang.String cannot be cast to clojure.lang.IFn

; 查找不存在的key会返回nil。
(stringmap "d") ; => nil

; 用assoc函数来向hashmap里添加元素
(def newkeymap (assoc keymap :d 4))
newkeymap ; => {:a 1, :b 2, :c 3, :d 4}

; 但是要记住的是clojure的数据类型是不可变的！
keymap ; => {:a 1, :b 2, :c 3}

; 用dissoc来移除元素
(dissoc keymap :a :b) ; => {:c 3}

; 集合（Set）
;;;;;;

(class #{1 2 3}) ; => clojure.lang.PersistentHashSet
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}

; 用conj新增元素
(conj #{1 2 3} 4) ; => #{1 2 3 4}

; 用disj移除元素
(disj #{1 2 3} 1) ; => #{2 3}

; 把集合当做函数调用来检查元素是否存在:
(#{1 2 3} 1) ; => 1
(#{1 2 3} 4) ; => nil

; 在clojure.sets模块下有很多相关函数。

; 常用的form
;;;;;;;;;;;;;;;;;

; clojure里的逻辑控制结构都是用宏（macro）实现的，这在语法上看起来没什么不同。
(if false "a" "b") ; => "b"
(if false "a") ; => nil

; 用let来创建临时的绑定变量。
(let [a 1 b 2]
  (> a b)) ; => false

; 用do将多个语句组合在一起依次执行
(do
  (print "Hello")
  "World") ; => "World" (prints "Hello")

; 函数定义里有一个隐式的do
(defn print-and-say-hello [name]
  (print "Saying hello to " name)
  (str "Hello " name))
(print-and-say-hello "Jeff") ;=> "Hello Jeff" (prints "Saying hello to Jeff")

; let也是如此
(let [name "Urkel"]
  (print "Saying hello to " name)
  (str "Hello " name)) ; => "Hello Urkel" (prints "Saying hello to Urkel")

; 模块
;;;;;;;;;;;;;;;

; 用use来导入模块里的所有函数
(use 'clojure.set)

; 然后就可以使用set相关的函数了
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; 你也可以从一个模块里导入一部分函数。
(use '[clojure.set :only [intersection]])

; 用require来导入一个模块
(require 'clojure.string)

; 用/来调用模块里的函数
; 下面是从模块`clojure.string`里调用`blank?`函数。
(clojure.string/blank? "") ; => true

; 在`import`里你可以给模块名指定一个较短的别名。
(require '[clojure.string :as str])
(str/replace "This is a test." #"[a-o]" str/upper-case) ; => "THIs Is A tEst."
; (#""用来表示一个正则表达式)

; 你可以在一个namespace定义里用:require的方式来require（或use，但最好不要用）模块。
; 这样的话你无需引用模块列表。
(ns test
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

; Java
;;;;;;;;;;;;;;;;;

; Java有大量的优秀的库，你肯定想学会如何用clojure来使用这些Java库。

; 用import来导入java类
(import java.util.Date)

; 也可以在ns定义里导入
(ns test
  (:import java.util.Date
           java.util.Calendar))

; 用类名末尾加`.`的方式来new一个Java对象
(Date.) ; <a date object>

; 用`.`操作符来调用方法，或者用`.method`的简化方式。
(. (Date.) getTime) ; <a timestamp>
(.getTime (Date.)) ; 和上例一样。

; 用`/`调用静态方法
(System/currentTimeMillis) ; <a timestamp> (system is always present)

; 用`doto`来更方便的使用（可变）类。
(import java.util.Calendar)
(doto (Calendar/getInstance)
  (.set 2000 1 1 0 0 0)
  .getTime) ; => A Date. set to 2000-01-01 00:00:00

; STM
;;;;;;;;;;;;;;;;;

; 软件内存事务（Software Transactional Memory）被clojure用来处理持久化的状态。
; clojure里内置了一些结构来使用STM。
; atom是最简单的。给它传一个初始值
(def my-atom (atom {}))

; 用`swap!`更新atom。
; `swap!`会以atom的当前值为第一个参数来调用一个指定的函数，
; `swap`其余的参数作为该函数的第二个参数。
(swap! my-atom assoc :a 1) ; Sets my-atom to the result of (assoc {} :a 1)
(swap! my-atom assoc :b 2) ; Sets my-atom to the result of (assoc {:a 1} :b 2)

; 用`@`读取atom的值
my-atom  ;=> Atom<#...> (返回Atom对象)
@my-atom ; => {:a 1 :b 2}

; 下例是一个使用atom实现的简单计数器
(def counter (atom 0))
(defn inc-counter []
  (swap! counter inc))

(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)

@counter ; => 5

; 其他STM相关的结构是ref和agent.
; Refs: http://clojure.org/refs
; Agents: http://clojure.org/agents
```

### 进阶读物

本文肯定不足以讲述关于clojure的一切，但是希望足以让你迈出第一步。

Clojure.org官网有很多文章:
[http://clojure.org/](http://clojure.org/)

Clojuredocs.org有大多数核心函数的文档，还带了示例哦:
[http://clojuredocs.org/quickref/Clojure%20Core](http://clojuredocs.org/quickref/Clojure%20Core)

4Clojure是个很赞的用来练习clojure/FP技能的地方:
[http://www.4clojure.com/](http://www.4clojure.com/)

Clojure-doc.org (你没看错)有很多入门级的文章:
[http://clojure-doc.org/](http://clojure-doc.org/)
