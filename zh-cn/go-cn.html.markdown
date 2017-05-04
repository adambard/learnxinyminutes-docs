---
language: Go
lang: zh-cn
filename: learngo-cn.go
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]
    - ["pantaovay", "https://github.com/pantaovay"]
    - ["lidashuang", "https://github.com/lidashuang"]
    - ["Tim Zhang", "https://github.com/ttimasdf"]

---

发明Go语言是出于更好地完成工作的需要。Go不是计算机科学的最新发展潮流，但它却提供了解决现实问题的最新最快的方法。

Go拥有命令式语言的静态类型，编译很快，执行也很快，同时加入了对于目前多核CPU的并发计算支持，也有相应的特性来实现大规模编程。

Go语言有非常棒的标准库，还有一个充满热情的社区。

```go
// 单行注释
/* 多行
    注释 */

// 导入包的子句在每个源文件的开头。
// Main比较特殊，它用来声明可执行文件，而不是一个库。
package main

// Import语句声明了当前文件引用的包。
import (
    "fmt"       // Go语言标准库中的包
    "io/ioutil" // 包含一些输入输出函数
    m "math"    // 数学标准库，在此文件中别名为m
    "net/http"  // 一个web服务器包
    "os"        // 系统底层函数，如文件读写
    "strconv"   // 字符串转换
)

// 函数声明：Main是程序执行的入口。
// 不管你喜欢还是不喜欢，反正Go就用了花括号来包住函数体。
func main() {
    // 往标准输出打印一行。
    // 用包名fmt限制打印函数。
    fmt.Println("Hello world!")

    // 调用当前包的另一个函数。
    beyondHello()
}

// 函数可以在括号里加参数。
// 如果没有参数的话，也需要一个空括号。
func beyondHello() {
    var x int   // 变量声明，变量必须在使用之前声明。
    x = 3       // 变量赋值。
    // 可以用:=来偷懒，它自动把变量类型、声明和赋值都搞定了。
    y := 4
    sum, prod := learnMultiple(x, y)        // 返回多个变量的函数
    fmt.Println("sum:", sum, "prod:", prod) // 简单输出
    learnTypes()                            // 少于y分钟，学的更多！
}

/* <- 快看快看我是跨行注释_(:з」∠)_
Go语言的函数可以有多个参数和 *多个* 返回值。
在这个函数中， `x`、`y` 是参数，
`sum`、`prod` 是返回值的标识符（可以理解为名字）且类型为int
*/
func learnMultiple(x, y int) (sum, prod int) {
    return x + y, x * y // 返回两个值
}

// 内置变量类型和关键词
func learnTypes() {
    // 短声明给你所想。
    str := "Learn Go!" // String类型

    s2 := `A "raw" string literal
can include line breaks.` // 同样是String类型

    // 非ascii字符。Go使用UTF-8编码。
    g := 'Σ' // rune类型，int32的别名，使用UTF-8编码

    f := 3.14195 // float64类型，IEEE-754 64位浮点数
    c := 3 + 4i  // complex128类型，内部使用两个float64表示

    // Var变量可以直接初始化。
    var u uint = 7  // unsigned 无符号变量，但是实现依赖int型变量的长度
    var pi float32 = 22. / 7

    // 字符转换
    n := byte('\n') // byte是uint8的别名

    // 数组类型编译的时候大小固定。
    var a4 [4] int              // 有4个int变量的数组，初始为0
    a3 := [...]int{3, 1, 5}     // 有3个int变量的数组，同时进行了初始化

    // Slice 可以动态的增删。Array和Slice各有千秋，但是使用slice的地方更多些。
    s3 := []int{4, 5, 9}        // 和a3相比，这里没有省略号
    s4 := make([]int, 4)        // 分配一个有4个int型变量的slice，全部被初始化为0
    var d2 [][]float64          // 声明而已，什么都没有分配
    bs := []byte("a slice")     // 类型转换的语法

    // Slice 是动态的，因此它的大小可以按需增长
    // 用内置函数 append() 向 slice 末尾添加元素
    // 目标 slice 是 append 函数第一个函数，由例可见，
    // 数组变量在原地增长
    s3 := []int{4, 5, 9}    // Compare to a3. No ellipsis here.
    s4 := make([]int, 4)    // Allocates slice of 4 ints, initialized to all 0.
    var d2 [][]float64      // Declaration only, nothing allocated here.
    bs := []byte("a slice") // Type conversion syntax.

    // Because they are dynamic, slices can be appended to on-demand.
    // To append elements to a slice, the built-in append() function is used.
    // First argument is a slice to which we are appending. Commonly,
    // the array variable is updated in place, as in example below.
    s := []int{1, 2, 3}     // Result is a slice of length 3.
    s = append(s, 4, 5, 6)  // Added 3 elements. Slice now has length of 6.
    fmt.Println(s) // Updated slice is now [1 2 3 4 5 6]

    // To append another slice, instead of list of atomic elements we can
    // pass a reference to a slice or a slice literal like this, with a
    // trailing ellipsis, meaning take a slice and unpack its elements,
    // appending them to slice s.
    s = append(s, []int{7, 8, 9}...) // Second argument is a slice literal.
    fmt.Println(s)  // Updated slice is now [1 2 3 4 5 6 7 8 9]

    p, q := learnMemory()       // 声明p,q为int型变量的指针
    fmt.Println(*p, *q)         // * 取值

    // Map是动态可增长关联数组，和其他语言中的hash或者字典相似。
    m := map[string]int{"three": 3, "four": 4}
    m["one"] = 1

    // 在Go语言中未使用的变量在编译的时候会报错，而不是warning。
    // 下划线 _ 可以使你“使用”一个变量，但是丢弃它的值。
    _, _, _, _, _, _, _, _, _, _ = str, s2, g, f, u, pi, n, a3, s4, bs
    // Usually you use it to ignore one of the return values of a function
    // For example, in a quick and dirty script you might ignore the
    // error value returned from os.Create, and expect that the file
    // will always be created.
    file, _ := os.Create("output.txt")
    fmt.Fprint(file, "This is how you write to a file, by the way")
    file.Close()

    // 输出变量
    fmt.Println(s, c, a4, s3, d2, m)

    learnFlowControl() // 回到流程控制
}

// It is possible, unlike in many other languages for functions in go
// to have named return values.
// Assigning a name to the type being returned in the function declaration line
// allows us to easily return from multiple points in a function as well as to
// only use the return keyword, without anything further.
func learnNamedReturns(x, y int) (z int) {
    z = x * y
    return // z is implicit here, because we named it earlier.

// Go全面支持垃圾回收。Go有指针，但是不支持指针运算。
// 你会因为空指针而犯错，但是不会因为增加指针而犯错。
func learnMemory() (p, q *int) {
    // 返回int型变量指针p和q
    p = new(int)    // 内置函数new分配内存
    // 自动将分配的int赋值0，p不再是空的了。
    s := make([]int, 20)    // 给20个int变量分配一块内存
    s[3] = 7                // 赋值
    r := -2                 // 声明另一个局部变量
    return &s[3], &r        // & 取地址
}

func expensiveComputation() int {
    return 1e6
}

func learnFlowControl() {
    // If需要花括号，括号就免了
    if true {
        fmt.Println("told ya")
    }
    // 用go fmt 命令可以帮你格式化代码，所以不用怕被人吐槽代码风格了，
    // 也不用容忍别人的代码风格。
    if false {
        // pout
    } else {
        // gloat
    }
    // 如果太多嵌套的if语句，推荐使用switch
    x := 1
    switch x {
    case 0:
    case 1:
        // 隐式调用break语句，匹配上一个即停止
    case 2:
        // 不会运行
    }
    // 和if一样，for也不用括号
    for x := 0; x < 3; x++ { // ++ 自增
        fmt.Println("iteration", x)
    }
    // x在这里还是1。为什么？

    // for 是go里唯一的循环关键字，不过它有很多变种
    for { // 死循环
        break    // 骗你的
        continue // 不会运行的
    }

    // You can use range to iterate over an array, a slice, a string, a map, or a channel.
    // range returns one (channel) or two values (array, slice, string and map).
    for key, value := range map[string]int{"one": 1, "two": 2, "three": 3} {
        // for each pair in the map, print key and value
        fmt.Printf("key=%s, value=%d\n", key, value)
    }
    // If you only need the value, use the underscore as the key
    for _, name := range []string{"Bob", "Bill", "Joe"} {
        fmt.Printf("Hello, %s\n", name)
    }

    // 和for一样，if中的:=先给y赋值，然后再和x作比较。
    if y := expensiveComputation(); y > x {
        x = y
    }
    // 闭包函数
    xBig := func() bool {
        return x > 100 // x是上面声明的变量引用
    }
    fmt.Println("xBig:", xBig()) // true （上面把y赋给x了）
    x /= 1e5                     // x变成10
    fmt.Println("xBig:", xBig()) // 现在是false

    // What's more is function literals may be defined and called inline,
    // acting as an argument to function, as long as:
    // a) function literal is called immediately (),
    // b) result type matches expected type of argument.
    fmt.Println("Add + double two numbers: ",
        func(a, b int) int {
            return (a + b) * 2
        }(10, 2)) // Called with args 10 and 2
    // => Add + double two numbers: 24

    // 当你需要goto的时候，你会爱死它的！
    goto love
love:

    learnFunctionFactory() // func returning func is fun(3)(3)
    learnDefer()      // A quick detour to an important keyword.
    learnInterfaces() // 好东西来了！
}

func learnFunctionFactory() {
    // Next two are equivalent, with second being more practical
    fmt.Println(sentenceFactory("summer")("A beautiful", "day!"))

    d := sentenceFactory("summer")
    fmt.Println(d("A beautiful", "day!"))
    fmt.Println(d("A lazy", "afternoon!"))
}

// Decorators are common in other languages. Same can be done in Go
// with function literals that accept arguments.
func sentenceFactory(mystring string) func(before, after string) string {
    return func(before, after string) string {
        return fmt.Sprintf("%s %s %s", before, mystring, after) // new string
    }
}

func learnDefer() (ok bool) {
    // Deferred statements are executed just before the function returns.
    defer fmt.Println("deferred statements execute in reverse (LIFO) order.")
    defer fmt.Println("\nThis line is being printed first because")
    // Defer is commonly used to close a file, so the function closing the
    // file stays close to the function opening the file.
    return true
}

// 定义Stringer为一个接口类型，有一个方法String
type Stringer interface {
    String() string
}

// 定义pair为一个结构体，有x和y两个int型变量。
type pair struct {
    x, y int
}

// 定义pair类型的方法，实现Stringer接口。
func (p pair) String() string { // p被叫做“接收器”
    // Sprintf是fmt包中的另一个公有函数。
    // 用 . 调用p中的元素。
    return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
    // 花括号用来定义结构体变量，:=在这里将一个结构体变量赋值给p。
    p := pair{3, 4}
    fmt.Println(p.String()) // 调用pair类型p的String方法
    var i Stringer          // 声明i为Stringer接口类型
    i = p                   // 有效！因为p实现了Stringer接口（类似java中的塑型）
    // 调用i的String方法，输出和上面一样
    fmt.Println(i.String())

    // fmt包中的Println函数向对象要它们的string输出，实现了String方法就可以这样使用了。
    // （类似java中的序列化）
    fmt.Println(p) // 输出和上面一样，自动调用String函数。
    fmt.Println(i) // 输出和上面一样。

    learnVariadicParams("great", "learning", "here!")
}

// Functions can have variadic parameters.
func learnVariadicParams(myStrings ...interface{}) {
    // Iterate each value of the variadic.
    // The underbar here is ignoring the index argument of the array.
    for _, param := range myStrings {
        fmt.Println("param:", param)
    }

    // Pass variadic value as a variadic parameter.
    fmt.Println("params:", fmt.Sprintln(myStrings...))

    learnErrorHandling()
}

func learnErrorHandling() {
    // ", ok"用来判断有没有正常工作
    m := map[int]string{3: "three", 4: "four"}
    if x, ok := m[1]; !ok { // ok 为false，因为m中没有1
        fmt.Println("no one there")
    } else {
        fmt.Print(x) // 如果x在map中的话，x就是那个值喽。
    }
    // 错误可不只是ok，它还可以给出关于问题的更多细节。
    if _, err := strconv.Atoi("non-int"); err != nil { // _ discards value
        // 输出"strconv.ParseInt: parsing "non-int": invalid syntax"
        fmt.Println(err)
    }
    // 待会再说接口吧。同时，
    learnConcurrency()
}

// c是channel类型，一个并发安全的通信对象。
func inc(i int, c chan int) {
    c <- i + 1 // <-把右边的发送到左边的channel。
}

// 我们将用inc函数来并发地增加一些数字。
func learnConcurrency() {
    // 用make来声明一个slice，make会分配和初始化slice，map和channel。
    c := make(chan int)
    // 用go关键字开始三个并发的goroutine，如果机器支持的话，还可能是并行执行。
    // 三个都被发送到同一个channel。
    go inc(0, c) // go is a statement that starts a new goroutine.
    go inc(10, c)
    go inc(-805, c)
    // 从channel中读取结果并打印。
    // 打印出什么东西是不可预知的。
    fmt.Println(<-c, <-c, <-c) // channel在右边的时候，<-是读操作。

    cs := make(chan string)       // 操作string的channel
    cc := make(chan chan string)  // 操作channel的channel
    go func() { c <- 84 }()       // 开始一个goroutine来发送一个新的数字
    go func() { cs <- "wordy" }() // 发送给cs
    // Select类似于switch，但是每个case包括一个channel操作。
    // 它随机选择一个准备好通讯的case。
    select {
    case i := <-c: // 从channel接收的值可以赋给其他变量
        fmt.Println("it's a", i)
    case <-cs: // 或者直接丢弃
        fmt.Println("it's a string")
    case <-cc: // 空的，还没作好通讯的准备
        fmt.Println("didn't happen.")
    }
    // 上面c或者cs的值被取到，其中一个goroutine结束，另外一个一直阻塞。

    learnWebProgramming() // Go很适合web编程，我知道你也想学！
}

// http包中的一个简单的函数就可以开启web服务器。
func learnWebProgramming() {
    // ListenAndServe第一个参数指定了监听端口，第二个参数是一个接口，特定是http.Handler。
    go func() {
        err := http.ListenAndServe(":8080", pair{})
        fmt.Println(err) // 不要无视错误。
    }()

    requestServer()
}

// 使pair实现http.Handler接口的ServeHTTP方法。
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
    // 使用http.ResponseWriter返回数据
    w.Write([]byte("You learned Go in Y minutes!"))
}

func requestServer() {
    resp, err := http.Get("http://localhost:8080")
    fmt.Println(err)
    defer resp.Body.Close()
    body, err := ioutil.ReadAll(resp.Body)
    fmt.Printf("\nWebserver said: `%s`", string(body))
}
```

## 更进一步

Go的根源在[Go官方网站](http://golang.org/)。
在那里你可以学习入门教程，通过浏览器交互式地学习，而且可以读到很多东西。
Aside from a tour, [the docs](https://golang.org/doc/) contain information on how to write clean and effective Go code, package and command docs, and release history.

强烈推荐阅读语言定义部分，很简单而且很简洁！(as language definitions go these days.)

You can play around with the code on [Go playground](https://play.golang.org/p/tnWMjr16Mm). Try to change it and run it from your browser! Note that you can use [https://play.golang.org](https://play.golang.org) as a [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop) to test things and code in your browser, without even installing Go.

学习Go还要阅读Go[标准库的源代码](http://golang.org/src/)，全部文档化了，可读性非常好，可以学到go，go style和go idioms。在[文档](http://golang.org/pkg/)中点击函数名，源代码就出来了！

Another great resource to learn Go is [Go by example](https://gobyexample.com/).

Go Mobile adds support for mobile platforms (Android and iOS). You can write all-Go native mobile apps or write a library that contains bindings from a Go package, which can be invoked via Java (Android) and Objective-C (iOS). Check out the [Go Mobile page](https://github.com/golang/go/wiki/Mobile) for more information.
