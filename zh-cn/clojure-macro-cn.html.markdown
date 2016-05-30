---
language: "clojure macros"
filename: learnclojuremacros-zh.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Jakukyo Friel", "http://weakish.github.io"]
lang: zh-cn
---

和所有Lisp一样，Clojure内在的[同构性](https://en.wikipedia.org/wiki/Homoiconic)使得你可以穷尽语言的特性，编写生成代码的子过程——“宏”。宏是一种按需调制语言的强大方式。

小心！可以用函数完成的事用宏去实现可不是什么好事。你应该仅在需要控制参数是否或者何时eval的时候使用宏。

你应该熟悉Clojure.确保你了解[Y分钟学Clojure](http://learnxinyminutes.com/docs/zh-cn/clojure-cn/)中的所有内容。

```clojure
;; 使用defmacro定义宏。宏应该输出一个可以作为clojure代码演算的列表。
;;
;; 以下宏的效果和直接写(reverse "Hello World")一致。

(defmacro my-first-macro []
  (list reverse "Hello World"))

;; 使用macroexpand或macroexpand-1查看宏的结果。
;;
;; 注意，调用需要引用。
(macroexpand '(my-first-macro))
;; -> (#<core$reverse clojure.core$reverse@xxxxxxxx> "Hello World")

;; 你可以直接eval macroexpand的结果
(eval (macroexpand '(my-first-macro)))
; -> (\d \l \o \r \W \space \o \l \l \e \H)

;; 不过一般使用以下形式，更简短，更像函数：
(my-first-macro)  ; -> (\d \l \o \r \W \space \o \l \l \e \H)

;; 创建宏的时候可以使用更简短的引用形式来创建列表
(defmacro my-first-quoted-macro []
  '(reverse "Hello World"))

(macroexpand '(my-first-quoted-macro))
;; -> (reverse "Hello World")
;; 注意reverse不再是一个函数对象，而是一个符号。

;; 宏可以传入参数。
(defmacro inc2 [arg]
  (list + 2 arg))

(inc2 2) ; -> 4

;; 不过，如果你尝试配合使用引用列表，会导致错误，
;; 因为参数也会被引用。
;; 为了避免这个问题，clojure提供了引用宏的另一种方式：`
;; 在`之内，你可以使用~获得外圈作用域的变量。
(defmacro inc2-quoted [arg]
  `(+ 2 ~arg))

(inc2-quoted 2)

;; 你可以使用通常的析构参数。用~@展开列表中的变量。
(defmacro unless [arg & body]
  `(if (not ~arg)
     (do ~@body))) ; 别忘了 do!

(macroexpand '(unless true (reverse "Hello World")))

;; ->
;; (if (clojure.core/not true) (do (reverse "Hello World")))

;; 当第一个参数为假时，(unless)会演算、返回主体。 
;; 否则返回nil。

(unless true "Hello") ; -> nil
(unless false "Hello") ; -> "Hello"

;; 需要小心，宏会搞乱你的变量
(defmacro define-x []
  '(do
     (def x 2)
     (list x)))

(def x 4)
(define-x) ; -> (2)
(list x) ; -> (2)

;; 使用gensym来获得独有的标识符
(gensym 'x) ; -> x1281 (or some such thing)

(defmacro define-x-safely []
  (let [sym (gensym 'x)]
    `(do
       (def ~sym 2)
       (list ~sym))))

(def x 4)
(define-x-safely) ; -> (2)
(list x) ; -> (4)

;; 你可以在 ` 中使用 # 为每个符号自动生成gensym
(defmacro define-x-hygenically []
  `(do
     (def x# 2)
     (list x#)))

(def x 4)
(define-x-hygenically) ; -> (2)
(list x) ; -> (4)

;; 通常会配合宏使用帮助函数。
;; 让我们创建一些帮助函数来支持（无聊的）算术语法：

(declare inline-2-helper)
(defn clean-arg [arg]
  (if (seq? arg)
    (inline-2-helper arg)
    arg))

(defn apply-arg
  "Given args [x (+ y)], return (+ x y)"
  [val [op arg]]
  (list op val (clean-arg arg)))

(defn inline-2-helper
  [[arg1 & ops-and-args]]
  (let [ops (partition 2 ops-and-args)]
    (reduce apply-arg (clean-arg arg1) ops)))

;; 在创建宏前，我们可以先测试
(inline-2-helper '(a + (b - 2) - (c * 5))) ; -> (- (+ a (- b 2)) (* c 5))

; 然而，如果我们希望它在编译期执行，就需要创建宏
(defmacro inline-2 [form]
  (inline-2-helper form)))

(macroexpand '(inline-2 (1 + (3 / 2) - (1 / 2) + 1)))
; -> (+ (- (+ 1 (/ 3 2)) (/ 1 2)) 1)

(inline-2 (1 + (3 / 2) - (1 / 2) + 1))
; -> 3 (事实上，结果是3N, 因为数字被转化为带/的有理分数）
```

## 扩展阅读

[Clojure for the Brave and True](http://www.braveclojure.com/)系列的编写宏
http://www.braveclojure.com/writing-macros/

官方文档
http://clojure.org/macros

何时使用宏？
http://dunsmor.com/lisp/onlisp/onlisp_12.html
