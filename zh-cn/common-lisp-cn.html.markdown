---
language: "Common Lisp"
filename: commonlisp-cn.lisp
contributors:
  - ["Paul Nathan", "https://github.com/pnathan"]
translators:
  - ["Mac David", "http://macdavid313.com"]
  - ["mut0u", "http://github.com/mut0u"]
lang: zh-cn
---

ANSI Common Lisp 是一个广泛通用于各个工业领域的、支持多种范式的编程语言。
这门语言也经常被引用作“可编程的编程语言”（可以写代码的代码）。

免费的经典的入门书籍[《实用 Common Lisp 编程》](http://www.gigamonkeys.com/book/)

许多人都抱怨上面这本书的翻译。[《ANSI Common Lisp》](http://acl.readthedocs.org/en/latest/)也许对中文读者更友好一些。

另外还有一本热门的近期出版的
[Land of Lisp](http://landoflisp.com/).

```common-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 0. 语法
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 一般形式

;; Lisp有两个基本的语法单元：原子（atom），以及S-表达式。
;; 一般的，一组S-表达式被称为“组合式”。

10  ; 一个原子; 它对自身进行求值

:THING ;同样是一个原子；它被求值为一个符号 :thing

t  ;还是一个原子，代表逻辑真值。

(+ 1 2 3 4) ; 一个S-表达式。

'(4 :foo  t)  ;同样是一个S-表达式。


;;; 注释

;; 一个分号开头的注释表示仅用于此行（单行）；两个分号开头的则表示一个所谓标准注释；
;; 三个分号开头的意味着段落注释；
;; 而四个分号开头的注释用于文件头注释（译者注：即对该文件的说明）。

#| 块注释
   可以涵盖多行，而且...
    #|
       他们可以被嵌套！
    |#
|#

;;; 运行环境

;; 有很多不同的Common Lisp的实现；并且大部分的实现是一致（可移植）的。
;; 对于入门学习来说，CLISP是个不错的选择。

;; 可以通过QuickLisp.org的Quicklisp系统管理你的库。

;; 通常，使用文本编辑器和“REPL”来开发Common Lisp；
;; （译者注：“REPL”指读取-求值-打印循环）。
;; “REPL”允许对程序进行交互式的运行、调试，就好像在系统“现场”操作。


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. 基本数据类型以及运算符
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 符号

'foo ; => FOO  注意到这个符号被自动转换成大写了。

;; `intern`由一个给定的字符串而创建相应的符号

(intern "AAAA") ; => AAAA

(intern "aaa") ; => |aaa|

;;; 数字
9999999999999999999999 ; 整型数
#b111                  ; 二进制 => 7
#o111                  ; 八进制 => 73
#x111                  ; 十六进制 => 273
3.14159s0              ; 单精度
3.14159d0              ; 双精度
1/2                    ; 分数
#C(1 2)                ; 复数


;; 使用函数时，应当写成这样的形式：(f x y z ...)；
;; 其中，f是一个函数（名），x, y, z为参数；
;; 如果你想创建一个“字面”意义上（即不求值）的列表， 只需使用单引号 ' ，
;; 从而避免接下来的表达式被求值。即，只“引用”这个数据（而不求值）。
'(+ 1 2) ; => (+ 1 2)
;; 你同样也可以手动地调用一个函数（译者注：即使用函数对象来调用函数）：
(funcall #'+ 1 2 3) ; => 6
;; 一些算术运算符
(+ 1 1)              ; => 2
(- 8 1)              ; => 7
(* 10 2)             ; => 20
(expt 2 3)           ; => 8
(mod 5 2)            ; => 1
(/ 35 5)             ; => 7
(/ 1 3)              ; => 1/3
(+ #C(1 2) #C(6 -4)) ; => #C(7 -2)

                     ;;; 布尔运算
t                    ; 逻辑真（任何不是nil的值都被视为真值）
nil                  ; 逻辑假，或者空列表
(not nil)            ; => t
(and 0 t)            ; => t
(or 0 nil)           ; => 0

                     ;;; 字符
#\A                  ; => #\A
#\λ                  ; => #\GREEK_SMALL_LETTER_LAMDA（希腊字母Lambda的小写）
#\u03BB              ; => #\GREEK_SMALL_LETTER_LAMDA（Unicode形式的小写希腊字母Lambda）

;;; 字符串被视为一个定长字符数组
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ;反斜杠用作转义字符

;; 可以拼接字符串
(concatenate 'string "Hello " "world!") ; => "Hello world!"

;; 一个字符串也可被视作一个字符序列
(elt "Apple" 0) ; => #\A

;; `format`被用于格式化字符串
(format nil "~a can be ~a" "strings" "formatted")

;; 利用`format`打印到屏幕上是非常简单的
;;（译者注：注意到第二个参数是t，不同于刚刚的nil）；~% 代表换行符
(format t "Common Lisp is groovy. Dude.~%")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. 变量
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 你可以通过`defparameter`创建一个全局（动态）变量
;; 变量名可以是除了：()[]{}",'`;#|\ 这些字符之外的其他任何字符

;; 动态变量名应该由*号开头与结尾！
;; (译者注：这个只是一个习惯)

(defparameter *some-var* 5)
*some-var* ; => 5

;; 你也可以使用Unicode字符：
(defparameter *AΛB* nil)


;; 访问一个在之前从未被绑定的变量是一种不规范的行为（即使依然是可能发生的）；
;; 不要尝试那样做。


;; 局部绑定：在(let ...)语句内，'me'被绑定到"dance with you"上。
;; `let`总是返回在其作用域内最后一个表达式的值

(let ((me "dance with you"))
  me)
;; => "dance with you"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. 结构体和集合
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 结构体
(defstruct dog name breed age)
(defparameter *rover*
    (make-dog :name "rover"
              :breed "collie"
              :age 5))
*rover* ; => #S(DOG :NAME "rover" :BREED "collie" :AGE 5)

(dog-p *rover*) ; => t  ;; ewww)
(dog-name *rover*) ; => "rover"

;; Dog-p，make-dog，以及 dog-name都是由defstruct创建的！

;;; 点对单元(Pairs)
;; `cons`可用于生成一个点对单元， 利用`car`以及`cdr`将分别得到第一个和第二个元素
(cons 'SUBJECT 'VERB) ; => '(SUBJECT . VERB)
(car (cons 'SUBJECT 'VERB)) ; => SUBJECT
(cdr (cons 'SUBJECT 'VERB)) ; => VERB

;;; 列表

;; 所有列表都是由点对单元构成的“链表”。它以'nil'（或者'()）作为列表的最后一个元素。
(cons 1 (cons 2 (cons 3 nil))) ; => '(1 2 3)
;; `list`是一个生成列表的便利途径
(list 1 2 3) ; => '(1 2 3)
;; 并且，一个引用也可被用做字面意义上的列表值
'(1 2 3) ; => '(1 2 3)

;; 同样的，依然可以用`cons`来添加一项到列表的起始位置
(cons 4 '(1 2 3)) ; => '(4 1 2 3)

;; 而`append`也可用于连接两个列表
(append '(1 2) '(3 4)) ; => '(1 2 3 4)

;; 或者使用`concatenate`

(concatenate 'list '(1 2) '(3 4))

;; 列表是一种非常核心的数据类型，所以有非常多的处理列表的函数
;; 例如：
(mapcar #'1+ '(1 2 3))             ; => '(2 3 4)
(mapcar #'+ '(1 2 3) '(10 20 30))  ; => '(11 22 33)
(remove-if-not #'evenp '(1 2 3 4)) ; => '(2 4)
(every #'evenp '(1 2 3 4))         ; => nil
(some #'oddp '(1 2 3 4))           ; => T
(butlast '(subject verb object))   ; => (SUBJECT VERB)


;;; 向量

;; 向量的字面意义是一个定长数组
;;（译者注：此处所谓“字面意义”，即指#(......)的形式，下文还会出现）
#(1 2 3) ; => #(1 2 3)

;; 使用`concatenate`来将两个向量首尾连接在一起
(concatenate 'vector #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)

;;; 数组

;; 向量和字符串只不过是数组的特例

;; 二维数组

(make-array (list 2 2))

;; (make-array '(2 2)) 也是可以的

; => #2A((0 0) (0 0))

(make-array (list 2 2 2))

; => #3A(((0 0) (0 0)) ((0 0) (0 0)))

;; 注意：数组的默认初始值是可以指定的
;; 下面是如何指定的示例：

(make-array '(2) :initial-element 'unset)

; => #(UNSET UNSET)

;; 若想获取数组[1][1][1]上的元素：
(aref (make-array (list 2 2 2)) 1 1 1)

; => 0

;;; 变长向量

;; 若将变长向量打印出来，那么它的字面意义上的值和定长向量的是一样的

(defparameter *adjvec* (make-array '(3) :initial-contents '(1 2 3)
      :adjustable t :fill-pointer t))
      
*adjvec* ; => #(1 2 3)

;; 添加新的元素:
(vector-push-extend 4 *adjvec*) ; => 3

*adjvec* ; => #(1 2 3 4)



;;; 不怎么严谨地说，集合也可被视为列表

(set-difference '(1 2 3 4) '(4 5 6 7)) ; => (3 2 1)
(intersection '(1 2 3 4) '(4 5 6 7)) ; => 4
(union '(1 2 3 4) '(4 5 6 7))        ; => (3 2 1 4 5 6 7)
(adjoin 4 '(1 2 3 4))     ; => (1 2 3 4)

;; 然而，你可能想使用一个更好的数据结构，而并非一个链表

;;; 在Common Lisp中，“字典”和哈希表的实现是一样的。

;; 创建一个哈希表
(defparameter *m* (make-hash-table))

;; 给定键，设置对应的值
(setf (gethash 'a *m*) 1)

;; （通过键）检索对应的值
(gethash 'a *m*) ; => 1, t

;; 注意此处有一细节：Common Lisp往往返回多个值。`gethash`返回的第二个值是t，代表找到了这个元素；返回nil表示没有找到这个元素。
;;（译者注：返回的第一个值表示给定的键所对应的值或者nil；）
;;（第二个是一个布尔值，表示在哈希表中是否存在这个给定的键）
;; 例如，如果可以找到给定的键所对应的值，则返回一个t，否则返回nil

;; 由给定的键检索一个不存在的值，则返回nil
;;（译者注：这个nil是第一个nil，第二个nil其实是指该键在哈希表中也不存在）
 (gethash 'd *m*) ;=> nil, nil

;; 给定一个键，你可以指定其对应的默认值：
(gethash 'd *m* :not-found) ; => :NOT-FOUND

;; 在此，让我们看一看怎样处理`gethash`的多个返回值。

(multiple-value-bind
      (a b)
    (gethash 'd *m*)
  (list a b))
; => (NIL NIL)

(multiple-value-bind
      (a b)
    (gethash 'a *m*)
  (list a b))
; => (1 T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. 函数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 使用`lambda`来创建一个匿名函数。
;; 一个函数总是返回其形式体内最后一个表达式的值。
;; 将一个函数对象打印出来后的形式是多种多样的...

(lambda () "Hello World") ; => #<FUNCTION (LAMBDA ()) {1004E7818B}>

;; 使用`funcall`来调用lambda函数
(funcall (lambda () "Hello World")) ; => "Hello World"

;; 或者使用`apply`
(apply (lambda () "Hello World") nil) ; => "Hello World"

;; 显式地定义一个函数（译者注：即非匿名的）
(defun hello-world ()
   "Hello World")
(hello-world) ; => "Hello World"

;; 刚刚上面函数名"hello-world"后的()其实是函数的参数列表
(defun hello (name)
   (format nil "Hello, ~a " name))

(hello "Steve") ; => "Hello, Steve"

;; 函数可以有可选形参并且其默认值都为nil

(defun hello (name &optional from)
    (if from
        (format t "Hello, ~a, from ~a" name from)
        (format t "Hello, ~a" name)))

 (hello "Jim" "Alpacas") ;; => Hello, Jim, from Alpacas

;; 你也可以指定那些可选形参的默认值
(defun hello (name &optional (from "The world"))
   (format t "Hello, ~a, from ~a" name from))

(hello "Steve")
; => Hello, Steve, from The world

(hello "Steve" "the alpacas")
; => Hello, Steve, from the alpacas


;; 当然，你也可以设置所谓关键字形参；
;; 关键字形参往往比可选形参更具灵活性。

(defun generalized-greeter (name &key (from "the world") (honorific "Mx"))
    (format t "Hello, ~a ~a, from ~a" honorific name from))

(generalized-greeter "Jim")   ; => Hello, Mx Jim, from the world

(generalized-greeter "Jim" :from "the alpacas you met last summer" :honorific "Mr")
; => Hello, Mr Jim, from the alpacas you met last summer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. 等式
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Common Lisp具有一个十分复杂的用于判断等价的系统，下面只是其中一部分的例子

;; 若要比较数值是否等价，使用`=`
(= 3 3.0) ; => t
(= 2 1) ; => nil

;; 若要比较对象的类型，则使用`eql`
;;（译者注：抱歉，翻译水平实在有限，下面是我个人的补充说明）
;;（`eq` 返回真，如果对象的内存地址相等）
;;（`eql` 返回真，如果两个对象内存地址相等，或者对象的类型相同，并且值相等）
;;（例如同为整形数或浮点数，并且他们的值相等时，二者`eql`等价）
;;（想要弄清`eql`，其实有必要先了解`eq`)
;;（[可以参考](http://stackoverflow.com/questions/547436/whats-the-difference-between-eq-eql-equal-and-equalp-in-common-lisp)）
;;（可以去CLHS上分别查看两者的文档）
;;（另外，《实用Common Lisp编程》的4.8节也提到了两者的区别）
(eql 3 3) ; => t
(eql 3 3.0) ; => nil
(eql (list 3) (list 3)) ; => nil

;; 对于列表、字符串、以及位向量，使用`equal`
(equal (list 'a 'b) (list 'a 'b)) ; => t
(equal (list 'a 'b) (list 'b 'a)) ; => nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. 控制流
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 条件判断语句

(if t                ; “test”，即判断语句
    "this is true"   ; “then”，即判断条件为真时求值的表达式
    "this is false") ; “else”，即判断条件为假时求值的表达式
; => "this is true"

;; 在“test”（判断）语句中，所有非nil或者非()的值都被视为真值
(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(GROUCHO ZEPPO)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'YEP

;; `cond`将一系列测试语句串联起来，并对相应的表达式求值
(cond ((> 2 2) (error "wrong!"))
      ((< 2 2) (error "wrong again!"))
      (t 'ok)) ; => 'OK

;; 对于给定值的数据类型，`typecase`会做出相应地判断
(typecase 1
  (string :string)
  (integer :int))

; => :int

;;; 迭代

;; 当然，递归是肯定被支持的：

(defun walker (n)
  (if (zerop n)
      :walked
      (walker (1- n))))

(walker) ; => :walked

;; 而大部分场合下，我们使用`DOLIST`或者`LOOP`来进行迭代


(dolist (i '(1 2 3 4))
  (format t "~a" i))

; => 1234

(loop for i from 0 below 10
      collect i)

; => (0 1 2 3 4 5 6 7 8 9)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. 可变性
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 使用`setf`可以对一个已经存在的变量进行赋值；
;; 事实上，刚刚在哈希表的例子中我们已经示范过了。

(let ((variable 10))
    (setf variable 2))
 ; => 2


;; 所谓好的Lisp编码风格就是为了减少使用破坏性函数，防止发生副作用。

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. 类与对象
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 我们就不写什么有关动物的类了，下面给出的人力车的类

(defclass human-powered-conveyance ()
  ((velocity
    :accessor velocity
    :initarg :velocity)
   (average-efficiency
    :accessor average-efficiency
   :initarg :average-efficiency))
  (:documentation "A human powered conveyance"))

;; `defclass`，后面接类名，以及超类列表
;; 再接着是槽的列表（槽有点像Java里的成员变量），最后是一些可选的特性
;; 例如文档说明“:documentation”

;; 如果超类列表为空，则默认该类继承于“standard-object”类（standard-object又是T的子类）
;; 这种默认行为是可以改变的，但你最好有一定的基础并且知道自己到底在干什么；
;; 参阅《The Art of the Metaobject Protocol》来了解更多信息。

(defclass bicycle (human-powered-conveyance)
  ((wheel-size
    :accessor wheel-size
    :initarg :wheel-size
    :documentation "Diameter of the wheel.")
   (height
    :accessor height
    :initarg :height)))

(defclass recumbent (bicycle)
  ((chain-type
    :accessor chain-type
    :initarg  :chain-type)))

(defclass unicycle (human-powered-conveyance) nil)

(defclass canoe (human-powered-conveyance)
  ((number-of-rowers
    :accessor number-of-rowers
    :initarg :number-of-rowers)))


;; 在REPL中对human-powered-conveyance类调用`DESCRIBE`后结果如下：

(describe 'human-powered-conveyance)

; COMMON-LISP-USER::HUMAN-POWERED-CONVEYANCE
;  [symbol]
;
; HUMAN-POWERED-CONVEYANCE names the standard-class #<STANDARD-CLASS
;                                                    HUMAN-POWERED-CONVEYANCE>:
;  Documentation:
;    A human powered conveyance
;  Direct superclasses: STANDARD-OBJECT
;  Direct subclasses: UNICYCLE, BICYCLE, CANOE
;  Not yet finalized.
;  Direct slots:
;    VELOCITY
;      Readers: VELOCITY
;      Writers: (SETF VELOCITY)
;    AVERAGE-EFFICIENCY
;      Readers: AVERAGE-EFFICIENCY
;      Writers: (SETF AVERAGE-EFFICIENCY)

;; 注意到这些有用的返回信息——Common Lisp一直是一个交互式的系统。

;; 若要定义一个方法；
;; 注意，我们计算自行车轮子周长时使用了这样一个公式：C = d * pi

(defmethod circumference ((object bicycle))
  (* pi (wheel-size object)))

;; pi在Common Lisp中已经是一个内置的常量。

;; 假设我们已经知道了效率值（“efficiency value”）和船桨数大概呈对数关系；
;; 那么效率值的定义应当在构造器/初始化过程中就被完成。

;; 下面是一个Common Lisp构造实例时初始化实例的例子：

(defmethod initialize-instance :after ((object canoe) &rest args)
  (setf (average-efficiency object)  (log (1+ (number-of-rowers object)))))

;; 接着初构造一个实例并检查平均效率...

(average-efficiency (make-instance 'canoe :number-of-rowers 15))
; => 2.7725887


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. 宏
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 宏可以让你扩展语法

;; 例如，Common Lisp并没有自带WHILE循环——所以让我们自己来为他添加一个；
;; 如果按照汇编程序的直觉来看，我们会这样写：

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.

`condition` is tested prior to each execution of `body`"
    (let ((block-name (gensym)))
        `(tagbody
           (unless ,condition
               (go ,block-name))
           (progn
           ,@body)
           ,block-name)))

;; 让我们来看看它的高级版本：

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.

`condition` is tested prior to each execution of `body`"
  `(loop while ,condition
         do
         (progn
            ,@body)))

;; 然而，在一个比较现代化的编译环境下，这样的WHILE是没有必要的；
;; LOOP形式的循环和这个WHILE同样的好，并且更易于阅读。

;; 注意反引号'`'，逗号','以及'@'这三个符号； 
;; 反引号'`'是一种所谓“quasiquote”的引用类型的运算符，有了它，之后的逗号“,”才有意义。
;; 逗号“,”意味着解除引用（unquote，即开始求值）；
;; “@”符号则表示将当前的参数插入到当前整个列表中。
;;（译者注：要想真正用好、用对这三个符号，需要下一番功夫）
;;（甚至光看《实用 Common Lisp 编程》中关于宏的介绍都是不够的）
;;（建议再去读一读Paul Graham的两本著作《ANSI Common Lisp》和《On Lisp》）

;; 函数`gensym`创建一个唯一的符号——这个符号确保不会出现在其他任何地方。
;; 这样做是因为，宏是在编译期展开的
;; 而在宏中声明的变量名极有可能和常规代码中使用的变量名发生冲突。

;; 可以去《实用 Common Lisp 编程》中阅读更多有关宏的内容。
```


## 拓展阅读

[继续阅读《实用 Common Lisp 编程》一书](http://www.gigamonkeys.com/book/)


## 致谢

非常感谢Scheme社区的人们，我基于他们的成果得以迅速的写出这篇有关Common Lisp的快速入门
同时也感谢
- [Paul Khuong](https://github.com/pkhuong) ，他提出了很多有用的点评。

##译者寄语
“祝福那些将思想镶嵌在重重括号之内的人们。”
