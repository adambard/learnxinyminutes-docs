---
language: c++
filename: learncpp.cpp
contributors:
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Matt Kline", "https://github.com/mrkline"]
translators:
    - ["Arnie97", "https://github.com/Arnie97"]
lang: zh-cn
---

C++是一種系統編程語言。用它的發明者，
[Bjarne Stroustrup的話](http://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2014/Keynote)來說，C++的設計目標是：

- 成爲「更好的C語言」
- 支持數據的抽象與封裝
- 支持面向對象編程
- 支持泛型編程

C++提供了對硬件的緊密控制（正如C語言一樣），
能夠編譯爲機器語言，由處理器直接執行。
與此同時，它也提供了泛型、異常和類等高層功能。
雖然C++的語法可能比某些出現較晚的語言更複雜，它仍然得到了人們的青睞——
功能與速度的平衡使C++成爲了目前應用最廣泛的系統編程語言之一。

```c++
////////////////
// 與C語言的比較
////////////////

// C++_幾乎_是C語言的一個超集，它與C語言的基本語法有許多相同之處，
// 例如變量和函數的聲明，原生數據類型等等。

// 和C語言一樣，在C++中，你的程序會從main()開始執行，
// 該函數的返回值應當爲int型，這個返回值會作爲程序的退出狀態值。
// 不過，大多數的編譯器（gcc，clang等）也接受 void main() 的函數原型。
// （參見 http://en.wikipedia.org/wiki/Exit_status 來獲取更多信息）
int main(int argc, char** argv)
{
    // 和C語言一樣，命令行參數通過argc和argv傳遞。
    // argc代表命令行參數的數量，
    // 而argv是一個包含“C語言風格字符串”（char *）的數組，
    // 其中每個字符串代表一個命令行參數的內容，
    // 首個命令行參數是調用該程序時所使用的名稱。
    // 如果你不關心命令行參數的值，argc和argv可以被忽略。
    // 此時，你可以用int main()作爲函數原型。

    // 退出狀態值爲0時，表示程序執行成功
    return 0;
}

// 然而，C++和C語言也有一些區別：

// 在C++中，字符字面量的大小是一個字節。
sizeof('c') == 1

// 在C語言中，字符字面量的大小與int相同。
sizeof('c') == sizeof(10)


// C++的函數原型與函數定義是嚴格匹配的
void func(); // 這個函數不能接受任何參數

// 而在C語言中
void func(); // 這個函數能接受任意數量的參數

// 在C++中，用nullptr代替C語言中的NULL
int* ip = nullptr;

// C++也可以使用C語言的標準頭文件，
// 但是需要加上前綴“c”並去掉末尾的“.h”。
#include <cstdio>

int main()
{
    printf("Hello, world!\n");
    return 0;
}

///////////
// 函數重載
///////////

// C++支持函數重載，provided each function takes different parameters.

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
    print("Hello"); // 解析爲 void print(const char*)
    print(15); // 解析爲 void print(int)
}

///////////////////
// 函數參數的默認值
///////////////////

// 你可以爲函數的參數指定默認值，
// 它們將會在調用者沒有提供相應參數時被使用。

void doSomethingWithInts(int a = 1, int b = 4)
{
    // 對兩個參數進行一些操作
}

int main()
{
    doSomethingWithInts();      // a = 1,  b = 4
    doSomethingWithInts(20);    // a = 20, b = 4
    doSomethingWithInts(20, 5); // a = 20, b = 5
}

// 默認參數必須放在所有的常規參數之後。

void invalidDeclaration(int a = 1, int b) // 這是錯誤的！
{
}


///////////
// 命名空間
///////////

// 命名空間爲變量、函數和其他聲明提供了【separate】的作用域。
// 命名空間可以嵌套使用。

namespace First {
    namespace Nested {
        void foo()
        {
            printf("This is First::Nested::foo\n");
        }
    } // end namespace Nested
} // end namespace First

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
    // 如果沒有特別指定，所有【對象】都使用【取自】"Second"中的【聲明】。
    using namespace Second;

    foo(); // 顯示 "This is Second::foo"
    First::Nested::foo(); // 顯示 "This is First::Nested::foo"
    ::foo(); // 顯示 "This is global foo"
}

////////////
// 輸入/輸出
////////////

// C++使用“流”來輸入輸出。
// cin、cout、和cerr分別代表stdin（標準輸入）、stdout（標準輸出）和stderr（標準錯誤）。
// <<是流的插入運算符，>>是流提取運算符。

#include <iostream> // Include for I/O streams

using namespace std; // 輸入輸出流在std命名空間（也就是標準庫）中。

int main()
{
   int myInt;

   // 在標準輸出（終端/顯示器）中顯示
   cout << "Enter your favorite number:\n";
   // 從標準輸入（鍵盤）獲得一個值
   cin >> myInt;

   // cout can also be formatted
   cout << "Your favorite number is " << myInt << "\n";
   // 顯示 "Your favorite number is <myInt>"

    cerr << "Used for error messages";
}

/////////
// 字符串
/////////

// C++中的字符串是對象，它們有很多成員函數
#include <string>

using namespace std; // 字符串也在std命名空間（標準庫）中。

string myString = "Hello";
string myOtherString = " World";

// + 可以用於連接字符串。
cout << myString + myOtherString; // "Hello World"

cout << myString + " You"; // "Hello You"

// C++中的字符串是可變的，具有“值語義”。
myString.append(" Dog");
cout << myString; // "Hello Dog"


/////////////
// 引用
/////////////

// 除了支持C語言中的指針類型以外，C++還提供了_引用_。
// 引用是一種特殊的指針類型，一旦被定義就不能重新賦值，並且引用不能被設置爲空值。
// 使用引用時的語法與原變量相同：
// 也就是說，對引用類型進行解引用時，不需要使用*；
// 賦值時也不需要用&來取地址。

using namespace std;

string foo = "I am foo";
string bar = "I am bar";


string& fooRef = foo; // 建立了一個對foo的引用。
fooRef += ". Hi!"; // 通過引用來修改foo的值
cout << fooRef; // "I am foo. Hi!"

// 這句話的並不會改變fooRef的指向，其效果與“foo = bar”相同。
// 也就是說，在執行這條語句之後，foo == "I am bar"。
fooRef = bar;

const string& barRef = bar; // 建立指向bar的【const ref】。
// 和C語言中一樣，聲明爲常數的值（包括指針和引用）不能被修改。
barRef += ". Hi!"; // 這是錯誤的，【const ref】不能被修改。

///////////////////
// 類與面向對象編程
///////////////////

// 有關類的第一個示例
#include <iostream>

// 聲明一個類。
// 類通常在頭文件（.h或.hpp）中聲明。
class Dog {
    // 成員變量和成員函數默認情況下是私有（private）的。
    std::string name;
    int weight;

// 在這個標籤之後，所有聲明都是公有（public）的，
// 直到重新指定“private:”（私有繼承）或“protected:”（保護繼承）爲止
public:

    // 默認的構造器
    Dog();

    // Member function declarations (implementations to follow)
    // Note that we use std::string here instead of placing
    // using namespace std;
    // above.
    // Never put a "using namespace" statement in a header.
    void setName(const std::string& dogsName);

    void setWeight(int dogsWeight);

    // Functions that do not modify the state of the object
    // should be marked as const.
    // This allows you to call them if given a const reference to the object.
    // Also note the functions must be explicitly declared as _virtual_
    // in order to be overridden in derived classes.
    // Functions are not virtual by default for performance reasons.
    virtual void print() const;

    // 函數也可以在class body內部定義。
    // 這樣定義的函數會自動成爲內聯函數。
    void bark() const { std::cout << name << " barks!\n" }

    // 除了構造器以外，C++還提供了析構器。
    // These are called when an object is deleted or falls out of scope.
    // 這使得如同下文中的RAII這樣的強大範式成爲可能。
    // Destructors must be virtual to allow classes to be derived from this one.
    virtual ~Dog();

}; // 在類的定義後必須加一個分號

// 類的成員函數通常在.cpp文件中實現。
void Dog::Dog()
{
    std::cout << "A dog has been constructed\n";
}

// 對象（例如字符串）應當以引用的形式傳遞，
// 不需要修改的對象則應當作爲【const ref】。
void Dog::setName(const std::string& dogsName)
{
    name = dogsName;
}

void Dog::setWeight(int dogsWeight)
{
    weight = dogsWeight;
}

// Notice that "virtual" is only needed in the declaration, not the definition.
void Dog::print() const
{
    std::cout << "Dog is " << name << " and weighs " << weight << "kg\n";
}

void Dog::~Dog()
{
    cout << "Goodbye " << name << "\n";
}

int main() {
    Dog myDog; // 此時顯示“A dog has been constructed”
    myDog.setName("Barkley");
    myDog.setWeight(10);
    myDog.printDog(); // 顯示“Dog is Barkley and weighs 10 kg”
    return 0;
} // 顯示“Goodbye Barkley”

// 繼承：

// 這個類繼承了Dog類中的公有（public）和保護（protected）對象
class OwnedDog : public Dog {

    void setOwner(const std::string& dogsOwner)

    // 重寫OwnedDogs類的print方法。
    // 如果你不熟悉子類多態的話，可以參考這個頁面中的概述：
    // http://en.wikipedia.org/wiki/Polymorphism_(computer_science)#Subtyping

    // override關鍵字是可選的，它確保你是在重寫基類中的方法。
    void print() const override;

private:
    std::string owner;
};

// 與此同時，在對應的.cpp文件裏：

void OwnedDog::setOwner(const std::string& dogsOwner)
{
    owner = dogsOwner;
}

void OwnedDog::print() const
{
    Dog::print(); // 調用基類Dog中的print方法
    // "Dog is <name> and weights <weight>"

    std::cout << "Dog is owned by " << owner << "\n";
    // "Dog is owned by <owner>"
}

/////////////////////
// 初始化與運算符重載
/////////////////////

// 在C++中，你可以重載+、-、*、/等運算符的行爲。
// This is done by defining a function
// which is called whenever the operator is used.

#include <iostream>
using namespace std;

class Point {
public:
    // 可以以這樣的方式爲成員變量設置默認值。
    double x = 0;
    double y = 0;

    // Define a default constructor which does nothing
    // but initialize the Point to the default value (0, 0)
    Point() { };

    // The following syntax is known as an initialization list
    // and is the proper way to initialize class member values
    Point (double a, double b) :
        x(a),
        y(b)
    { /* Do nothing except initialize the values */ }

    // 重載 + 運算符
    Point operator+(const Point& rhs) const;

    // 重載 += 運算符
    Point& operator+=(const Point& rhs);

    // 增加 - 和 -= 運算符也是有意義的，這裏不再贅述。
};

Point Point::operator+(const Point& rhs) const
{
    // Create a new point that is the sum of this one and rhs.
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
    // 這裏調用了Point類型的運算符“+”
    // 調用up（Point類型）的“+”方法，並以right作爲函數的參數
    Point result = up + right;
    // 顯示“Result is upright (1,1)”
    cout << "Result is upright (" << result.x << ',' << result.y << ")\n";
    return 0;
}

///////////
// 異常處理
///////////

// 標準庫中提供了a few exception types
// （參見http://en.cppreference.com/w/cpp/error/exception）
// but any type can be thrown an as exception
#include <exception>

// All exceptions thrown inside the _try_ block can be caught by subsequent
// _catch_ handlers.
try {
    // Do not allocate exceptions on the heap using _new_.
    throw std::exception("A problem occurred");
}
// Catch exceptions by const reference if they are objects
catch (const std::exception& ex)
{
  std::cout << ex.what();
// Catches any exception not caught by previous _catch_ blocks
} catch (...)
{
    std::cout << "Unknown exception caught";
    throw; // Re-throws the exception
}

///////
// RAII
///////

// RAII指的是“资源获取就是初始化”（Resource Allocation Is Initialization）。
// It is often considered the most powerful paradigm in C++,
// and is the simple concept that a constructor for an object
// acquires that object's resources and the destructor releases them.

// 爲了理解這一範式的用處，讓我們考慮某個函數使用文件句柄時的情況：
void doSomethingWithAFile(const char* filename)
{
    // 首先，讓我們假設一切都會順利進行。

    FILE* fh = fopen(filename, "r"); // 以只讀模式打開文件

    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

    fclose(fh); // 關閉文件句柄
}

// 不幸的是，隨着錯誤處理機制的引入，事情會變得複雜。
// 假設fopen有可能執行失敗，
// 而doSomethingWithTheFile和doSomethingElseWithIt會在失敗時返回錯誤代碼。
// （雖然【Exceptions】是處理錯誤的推薦方式，
// 但是某些程序員，尤其是有C語言背景的，並不認可【exceptions】的效用）。
// 現在，我們必須檢查每個函數調用是否成功執行，並在問題發生的時候關閉文件句柄。
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // 以只讀模式打開文件
    if (fh == nullptr) // 當執行失敗是，返回的指針是nullptr
        return false; // 向調用者彙報錯誤

    // 假設每個函數會在執行失敗時返回false
    if (!doSomethingWithTheFile(fh)) {
        fclose(fh); // Close the file handle so it doesn't leak.
        return false; // 反饋錯誤
    }
    if (!doSomethingElseWithIt(fh)) {
        fclose(fh); // Close the file handle so it doesn't leak.
        return false; // 反饋錯誤
    }

    fclose(fh); // Close the file handle so it doesn't leak.
    return true; // 指示函數已成功執行
}

// C語言的程序員通常會借助goto語句簡化上面的代碼：
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r");
    if (fh == nullptr)
        return false;

    if (!doSomethingWithTheFile(fh))
        goto failure;

    if (!doSomethingElseWithIt(fh))
        goto failure;

    fclose(fh); // 關閉文件
    return true; // 執行成功

failure:
    fclose(fh);
    return false; // 反饋錯誤
}

// If the functions indicate errors using exceptions,
// things are a little cleaner, but still sub-optimal.
void doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // 以只讀模式打開文件
    if (fh == nullptr)
        throw std::exception("Could not open the file.");

    try {
        doSomethingWithTheFile(fh);
        doSomethingElseWithIt(fh);
    }
    catch (...) {
        fclose(fh); // 保證出錯的時候文件被正確關閉
        throw; // Then re-throw the exception.
    }

    fclose(fh); // 關閉文件
    // 所有工作順利完成
}

// Compare this to the use of C++'s file stream class (fstream)
// fstream利用自己的析構器來關閉文件句柄。
// Recall from above that destructors are automatically called
// whenver an object falls out of scope.
void doSomethingWithAFile(const std::string& filename)
{
    // ifstream is short for input file stream
    std::ifstream fh(filename); // Open the file

    // 對文件進行一些操作
    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

} // 文件已經被析構器自動關閉

// 與上面幾種方式相比，這種方式有着_明顯_的優勢：
// 1. 無論發生了什麼情況，資源（此例當中是文件句柄）都會被正確關閉。
//    只要你正確使用了析構器，就_不會_因爲忘記關閉句柄，造成資源的泄漏。
// 2. Note that the code is much cleaner.
//    The destructor handles closing the file behind the scenes
//    without you having to worry about it.
// 3. The code is exception safe.
//    An exception can be thrown anywhere in the function and cleanup
//    will still occur.

// All idiomatic C++ code uses RAII extensively for all resources.
// Additional examples include
// - Memory using unique_ptr and shared_ptr
// - Containers - the standard library linked list,
//   vector (i.e. self-resizing array), hash maps, and so on
//   all automatically destroy their contents when they fall out of scope.
// - Mutexes using lock_guard and unique_lock
```
擴展閱讀：

<http://cppreference.com/w/cpp> 提供了最新的語法參考。

可以在 <http://cplusplus.com> 找到一些補充資料。
