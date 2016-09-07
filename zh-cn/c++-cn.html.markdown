---
language: c++
filename: learncpp-cn.cpp
contributors:
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Matt Kline", "https://github.com/mrkline"]
translators:
    - ["Arnie97", "https://github.com/Arnie97"]
lang: zh-cn
---

C++是一种系统编程语言。用它的发明者，
[Bjarne Stroustrup的话](http://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2014/Keynote)来说，C++的设计目标是：

- 成为“更好的C语言”
- 支持数据的抽象与封装
- 支持面向对象编程
- 支持泛型编程

C++提供了对硬件的紧密控制（正如C语言一样），
能够编译为机器语言，由处理器直接执行。
与此同时，它也提供了泛型、异常和类等高层功能。
虽然C++的语法可能比某些出现较晚的语言更复杂，它仍然得到了人们的青睞——
功能与速度的平衡使C++成为了目前应用最广泛的系统编程语言之一。

```c++
////////////////
// 与C语言的比较
////////////////

// C++_几乎_是C语言的一个超集，它与C语言的基本语法有许多相同之处，
// 例如变量和函数的声明，原生数据类型等等。

// 和C语言一样，在C++中，你的程序会从main()开始执行，
// 该函数的返回值应当为int型，这个返回值会作为程序的退出状态值。
// 不过，大多数的编译器（gcc，clang等）也接受 void main() 的函数原型。
// （参见 http://en.wikipedia.org/wiki/Exit_status 来获取更多信息）
int main(int argc, char** argv)
{
    // 和C语言一样，命令行参数通过argc和argv传递。
    // argc代表命令行参数的数量，
    // 而argv是一个包含“C语言风格字符串”（char *）的数组，
    // 其中每个字符串代表一个命令行参数的内容，
    // 首个命令行参数是调用该程序时所使用的名称。
    // 如果你不关心命令行参数的值，argc和argv可以被忽略。
    // 此时，你可以用int main()作为函数原型。

    // 退出状态值为0时，表示程序执行成功
    return 0;
}

// 然而，C++和C语言也有一些区别：

// 在C++中，字符字面量的大小是一个字节。
sizeof('c') == 1

// 在C语言中，字符字面量的大小与int相同。
sizeof('c') == sizeof(10)


// C++的函数原型与函数定义是严格匹配的
void func(); // 这个函数不能接受任何参数

// 而在C语言中
void func(); // 这个函数能接受任意数量的参数

// 在C++中，用nullptr代替C语言中的NULL
int* ip = nullptr;

// C++也可以使用C语言的标准头文件，
// 但是需要加上前缀“c”并去掉末尾的“.h”。
#include <cstdio>

int main()
{
    printf("Hello, world!\n");
    return 0;
}

///////////
// 函数重载
///////////

// C++支持函数重载，你可以定义一组名称相同而参数不同的函数。

void print(char const* myString)
{
    printf("String %s\n", myString);
}

void print(int myInt)
{
    printf("My int is %d", myInt);
}

int main()
{
    print("Hello"); // 解析为 void print(const char*)
    print(15); // 解析为 void print(int)
}

///////////////////
// 函数参数的默认值
///////////////////

// 你可以为函数的参数指定默认值，
// 它们将会在调用者没有提供相应参数时被使用。

void doSomethingWithInts(int a = 1, int b = 4)
{
    // 对两个参数进行一些操作
}

int main()
{
    doSomethingWithInts();      // a = 1,  b = 4
    doSomethingWithInts(20);    // a = 20, b = 4
    doSomethingWithInts(20, 5); // a = 20, b = 5
}

// 默认参数必须放在所有的常规参数之后。

void invalidDeclaration(int a = 1, int b) // 这是错误的！
{
}


///////////
// 命名空间
///////////

// 命名空间为变量、函数和其他声明提供了分离的的作用域。
// 命名空间可以嵌套使用。

namespace First {
    namespace Nested {
        void foo()
        {
            printf("This is First::Nested::foo\n");
        }
    } // 结束嵌套的命名空间Nested
} // 结束命名空间First

namespace Second {
    void foo()
    {
        printf("This is Second::foo\n")
    }
}

void foo()
{
    printf("This is global foo\n");
}

int main()
{
    // 如果没有特别指定，就从“Second”中取得所需的内容。
    using namespace Second;

    foo(); // 显示“This is Second::foo”
    First::Nested::foo(); // 显示“This is First::Nested::foo”
    ::foo(); // 显示“This is global foo”
}

////////////
// 输入/输出
////////////

// C++使用“流”来输入输出。<<是流的插入运算符，>>是流提取运算符。
// cin、cout、和cerr分别代表
// stdin（标准输入）、stdout（标准输出）和stderr（标准错误）。

#include <iostream> // 引入包含输入/输出流的头文件

using namespace std; // 输入输出流在std命名空间（也就是标准库）中。

int main()
{
   int myInt;

   // 在标准输出（终端/显示器）中显示
   cout << "Enter your favorite number:\n";
   // 从标准输入（键盘）获得一个值
   cin >> myInt;

   // cout也提供了格式化功能
   cout << "Your favorite number is " << myInt << "\n";
   // 显示“Your favorite number is <myInt>”

   cerr << "Used for error messages";
}

/////////
// 字符串
/////////

// C++中的字符串是对象，它们有很多成员函数
#include <string>

using namespace std; // 字符串也在std命名空间（标准库）中。

string myString = "Hello";
string myOtherString = " World";

// + 可以用于连接字符串。
cout << myString + myOtherString; // "Hello World"

cout << myString + " You"; // "Hello You"

// C++中的字符串是可变的，具有“值语义”。
myString.append(" Dog");
cout << myString; // "Hello Dog"


/////////////
// 引用
/////////////

// 除了支持C语言中的指针类型以外，C++还提供了_引用_。
// 引用是一种特殊的指针类型，一旦被定义就不能重新赋值，并且不能被设置为空值。
// 使用引用时的语法与原变量相同：
// 也就是说，对引用类型进行解引用时，不需要使用*；
// 赋值时也不需要用&来取地址。

using namespace std;

string foo = "I am foo";
string bar = "I am bar";


string& fooRef = foo; // 建立了一个对foo的引用。
fooRef += ". Hi!"; // 通过引用来修改foo的值
cout << fooRef; // "I am foo. Hi!"

// 这句话的并不会改变fooRef的指向，其效果与“foo = bar”相同。
// 也就是说，在执行这条语句之后，foo == "I am bar"。
fooRef = bar;

const string& barRef = bar; // 建立指向bar的常量引用。
// 和C语言中一样，（指针和引用）声明为常量时，对应的值不能被修改。
barRef += ". Hi!"; // 这是错误的，不能修改一个常量引用的值。

///////////////////
// 类与面向对象编程
///////////////////

// 有关类的第一个示例
#include <iostream>

// 声明一个类。
// 类通常在头文件（.h或.hpp）中声明。
class Dog {
    // 成员变量和成员函数默认情况下是私有（private）的。
    std::string name;
    int weight;

// 在这个标签之后，所有声明都是公有（public）的，
// 直到重新指定“private:”（私有继承）或“protected:”（保护继承）为止
public:

    // 默认的构造器
    Dog();

    // 这里是成员函数声明的一个例子。
    // 可以注意到，我们在此处使用了std::string，而不是using namespace std
    // 语句using namespace绝不应当出现在头文件当中。
    void setName(const std::string& dogsName);

    void setWeight(int dogsWeight);

    // 如果一个函数不对对象的状态进行修改，
    // 应当在声明中加上const。
    // 这样，你就可以对一个以常量方式引用的对象执行该操作。
    // 同时可以注意到，当父类的成员函数需要被子类重写时，
    // 父类中的函数必须被显式声明为_虚函数（virtual）_。
    // 考虑到性能方面的因素，函数默认情况下不会被声明为虚函数。
    virtual void print() const;

    // 函数也可以在class body内部定义。
    // 这样定义的函数会自动成为内联函数。
    void bark() const { std::cout << name << " barks!\n" }

    // 除了构造器以外，C++还提供了析构器。
    // 当一个对象被删除或者脱离其定义域时，它的析构函数会被调用。
    // 这使得RAII这样的强大范式（参见下文）成为可能。
    // 为了衍生出子类来，基类的析构函数必须定义为虚函数。
    virtual ~Dog();

}; // 在类的定义之后，要加一个分号

// 类的成员函数通常在.cpp文件中实现。
void Dog::Dog()
{
    std::cout << "A dog has been constructed\n";
}

// 对象（例如字符串）应当以引用的形式传递，
// 对于不需要修改的对象，最好使用常量引用。
void Dog::setName(const std::string& dogsName)
{
    name = dogsName;
}

void Dog::setWeight(int dogsWeight)
{
    weight = dogsWeight;
}

// 虚函数的virtual关键字只需要在声明时使用，不需要在定义时重复
void Dog::print() const
{
    std::cout << "Dog is " << name << " and weighs " << weight << "kg\n";
}

void Dog::~Dog()
{
    cout << "Goodbye " << name << "\n";
}

int main() {
    Dog myDog; // 此时显示“A dog has been constructed”
    myDog.setName("Barkley");
    myDog.setWeight(10);
    myDog.printDog(); // 显示“Dog is Barkley and weighs 10 kg”
    return 0;
} // 显示“Goodbye Barkley”

// 继承：

// 这个类继承了Dog类中的公有（public）和保护（protected）对象
class OwnedDog : public Dog {

    void setOwner(const std::string& dogsOwner)

    // 重写OwnedDogs类的print方法。
    // 如果你不熟悉子类多态的话，可以参考这个页面中的概述：
    // http://zh.wikipedia.org/wiki/%E5%AD%90%E7%B1%BB%E5%9E%8B

    // override关键字是可选的，它确保你所重写的是基类中的方法。
    void print() const override;

private:
    std::string owner;
};

// 与此同时，在对应的.cpp文件里：

void OwnedDog::setOwner(const std::string& dogsOwner)
{
    owner = dogsOwner;
}

void OwnedDog::print() const
{
    Dog::print(); // 调用基类Dog中的print方法
    // "Dog is <name> and weights <weight>"

    std::cout << "Dog is owned by " << owner << "\n";
    // "Dog is owned by <owner>"
}

/////////////////////
// 初始化与运算符重载
/////////////////////

// 在C++中，通过定义一些特殊名称的函数，
// 你可以重载+、-、*、/等运算符的行为。
// 当运算符被使用时，这些特殊函数会被调用，从而实现运算符重载。

#include <iostream>
using namespace std;

class Point {
public:
    // 可以以这样的方式为成员变量设置默认值。
    double x = 0;
    double y = 0;

    // 定义一个默认的构造器。
    // 除了将Point初始化为(0, 0)以外，这个函数什么都不做。
    Point() { };

    // 下面使用的语法称为初始化列表，
    // 这是初始化类中成员变量的正确方式。
    Point (double a, double b) :
        x(a),
        y(b)
    { /* 除了初始化成员变量外，什么都不做 */ }

    // 重载 + 运算符
    Point operator+(const Point& rhs) const;

    // 重载 += 运算符
    Point& operator+=(const Point& rhs);

    // 增加 - 和 -= 运算符也是有意义的，但这里不再赘述。
};

Point Point::operator+(const Point& rhs) const
{
    // 创建一个新的点，
    // 其横纵坐标分别为这个点与另一点在对应方向上的坐标之和。
    return Point(x + rhs.x, y + rhs.y);
}

Point& Point::operator+=(const Point& rhs)
{
    x += rhs.x;
    y += rhs.y;
    return *this;
}

int main () {
    Point up (0,1);
    Point right (1,0);
    // 这里使用了Point类型的运算符“+”
    // 调用up（Point类型）的“+”方法，并以right作为函数的参数
    Point result = up + right;
    // 显示“Result is upright (1,1)”
    cout << "Result is upright (" << result.x << ',' << result.y << ")\n";
    return 0;
}

///////////
// 异常处理
///////////

// 标准库中提供了一些基本的异常类型
// （参见http://en.cppreference.com/w/cpp/error/exception）
// 但是，其他任何类型也可以作为一个异常被拋出
#include <exception>

// 在_try_代码块中拋出的异常可以被随后的_catch_捕获。
try {
    // 不要用 _new_关键字在堆上为异常分配空间。
    throw std::exception("A problem occurred");
}
// 如果拋出的异常是一个对象，可以用常量引用来捕获它
catch (const std::exception& ex)
{
  std::cout << ex.what();
// 捕获尚未被_catch_处理的所有错误
} catch (...)
{
    std::cout << "Unknown exception caught";
    throw; // 重新拋出异常
}

///////
// RAII
///////

// RAII指的是“资源获取就是初始化”（Resource Allocation Is Initialization），
// 它被视作C++中最强大的编程范式之一。
// 简单说来，它指的是，用构造函数来获取一个对象的资源，
// 相应的，借助析构函数来释放对象的资源。

// 为了理解这一范式的用处，让我们考虑某个函数使用文件句柄时的情况：
void doSomethingWithAFile(const char* filename)
{
    // 首先，让我们假设一切都会顺利进行。

    FILE* fh = fopen(filename, "r"); // 以只读模式打开文件

    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

    fclose(fh); // 关闭文件句柄
}

// 不幸的是，随着错误处理机制的引入，事情会变得复杂。
// 假设fopen函数有可能执行失败，
// 而doSomethingWithTheFile和doSomethingElseWithIt会在失败时返回错误代码。
// （虽然异常是C++中处理错误的推荐方式，
// 但是某些程序员，尤其是有C语言背景的，并不认可异常捕获机制的作用）。
// 现在，我们必须检查每个函数调用是否成功执行，并在问题发生的时候关闭文件句柄。
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // 以只读模式打开文件
    if (fh == nullptr) // 当执行失败是，返回的指针是nullptr
        return false; // 向调用者汇报错误

    // 假设每个函数会在执行失败时返回false
    if (!doSomethingWithTheFile(fh)) {
        fclose(fh); // 关闭文件句柄，避免造成内存泄漏。
        return false; // 反馈错误
    }
    if (!doSomethingElseWithIt(fh)) {
        fclose(fh); // 关闭文件句柄
        return false; // 反馈错误
    }

    fclose(fh); // 关闭文件句柄
    return true; // 指示函数已成功执行
}

// C语言的程序员通常会借助goto语句简化上面的代码：
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r");
    if (fh == nullptr)
        return false;

    if (!doSomethingWithTheFile(fh))
        goto failure;

    if (!doSomethingElseWithIt(fh))
        goto failure;

    fclose(fh); // 关闭文件
    return true; // 执行成功

failure:
    fclose(fh);
    return false; // 反馈错误
}

// 如果用异常捕获机制来指示错误的话，
// 代码会变得清晰一些，但是仍然有优化的余地。
void doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // 以只读模式打开文件
    if (fh == nullptr)
        throw std::exception("Could not open the file.");

    try {
        doSomethingWithTheFile(fh);
        doSomethingElseWithIt(fh);
    }
    catch (...) {
        fclose(fh); // 保证出错的时候文件被正确关闭
        throw; // 之后，重新抛出这个异常
    }

    fclose(fh); // 关闭文件
    // 所有工作顺利完成
}

// 相比之下，使用C++中的文件流类（fstream）时，
// fstream会利用自己的析构器来关闭文件句柄。
// 只要离开了某一对象的定义域，它的析构函数就会被自动调用。
void doSomethingWithAFile(const std::string& filename)
{
    // ifstream是输入文件流（input file stream）的简称
    std::ifstream fh(filename); // 打开一个文件

    // 对文件进行一些操作
    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

} // 文件已经被析构器自动关闭

// 与上面几种方式相比，这种方式有着_明显_的优势：
// 1. 无论发生了什么情况，资源（此例当中是文件句柄）都会被正确关闭。
//    只要你正确使用了析构器，就_不会_因为忘记关闭句柄，造成资源的泄漏。
// 2. 可以注意到，通过这种方式写出来的代码十分简洁。
//    析构器会在后台关闭文件句柄，不再需要你来操心这些琐事。
// 3. 这种方式的代码具有异常安全性。
//    无论在函数中的何处拋出异常，都不会阻碍对文件资源的释放。

// 地道的C++代码应当把RAII的使用扩展到各种类型的资源上，包括：
// - 用unique_ptr和shared_ptr管理的内存
// - 各种数据容器，例如标准库中的链表、向量（容量自动扩展的数组）、散列表等；
//   当它们脱离作用域时，析构器会自动释放其中储存的内容。
// - 用lock_guard和unique_lock实现的互斥
```
扩展阅读：

<http://cppreference.com/w/cpp> 提供了最新的语法参考。

可以在 <http://cplusplus.com> 找到一些补充资料。
