---
language: c
filename: learnc-cn.c
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Chenbo Li", "http://binarythink.net/"]
    - ["Jakukyo Friel", "http://weakish.github.io"]
lang: zh-cn
---

C语言在今天仍然是高性能计算的主要选择。

C大概是大多数程序员用到的最接近底层的语言了，C语言原生的速度就很高了，但是别忘了C的手动内存管理，它会让你将性能发挥到极致。

```c
// 单行注释以//开始。（仅适用于C99或更新的版本。）

/*
多行注释是这个样子的。（C89也适用。）
*/

// 常数： #define 关键词
#define DAYS_IN_YEAR 365

// 以枚举的方式定义常数
enum days {SUN = 1, MON, TUE, WED, THU, FRI, SAT};
// MON自动被定义为2，TUE被定义为3，以此类推。

// 用#include来导入头文件
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// <尖括号>间的文件名是C标准库的头文件。
// 标准库以外的头文件，使用双引号代替尖括号。
#include "my_header.h"

// 函数的签名可以事先在.h文件中定义，
// 也可以直接在.c文件的头部定义。
void function_1(char c);
void function_2(void);

// 如果函数调用在main()之后，那么必须声明在main()之前
// 先声明一个函数原型
int add_two_ints(int x1, int x2); // 函数原型

// 你的程序的入口是一个返回值为整型的main函数
int main() {

// 用printf打印到标准输出，可以设定格式，
// %d 代表整数, \n 代表换行
printf("%d\n", 0); // => 打印 0
// 所有的语句都要以分号结束

///////////////////////////////////////
// 类型
///////////////////////////////////////

// 在使用变量之前我们必须先声明它们。
// 变量在声明时需要指明其类型，而类型能够告诉系统这个变量所占用的空间

// int型（整型）变量一般占用4个字节
int x_int = 0;

// short型（短整型）变量一般占用2个字节
short x_short = 0;

// char型（字符型）变量会占用1个字节
char x_char = 0;
char y_char = 'y'; // 字符变量的字面值需要用单引号包住

// long型（长整型）一般需要4个字节到8个字节; 而long long型则至少需要8个字节（64位）

long x_long = 0;
long long x_long_long = 0; 

// float一般是用32位表示的浮点数字
float x_float = 0.0;

// double一般是用64位表示的浮点数字
double x_double = 0.0;

// 整数类型也可以有无符号的类型表示。这样这些变量就无法表示负数
// 但是无符号整数所能表示的范围就可以比原来的整数大一些

unsigned short ux_short;
unsigned int ux_int;
unsigned long long ux_long_long;

// 单引号内的字符是机器的字符集中的整数。
'0' // => 在ASCII字符集中是48
'A' // => 在ASCII字符集中是65

// char类型一定会占用1个字节，但是其他的类型却会因具体机器的不同而各异
// sizeof(T) 可以返回T类型在运行的机器上占用多少个字节 
// 这样你的代码就可以在各处正确运行了
// sizeof(obj)返回表达式（变量、字面量等）的尺寸
printf("%zu\n", sizeof(int)); // => 4 (大多数的机器字长为4)

// 如果`sizeof`的参数是一个表达式，那么这个参数不会被演算（VLA例外，见下）
// 它产生的值是编译期的常数
int a = 1;
// size_t是一个无符号整型，表示对象的尺寸，至少2个字节
size_t size = sizeof(a++); // a++ 不会被演算
printf("sizeof(a++) = %zu where a = %d\n", size, a);
// 打印 "sizeof(a++) = 4 where a = 1" （在32位架构上）

// 数组必须要被初始化为具体的长度
char my_char_array[20]; // 这个数组占据 1 * 20 = 20 个字节
int my_int_array[20]; // 这个数组占据 4 * 20 = 80 个字节
                      // (这里我们假设字长为4)


// 可以用下面的方法把数组初始化为0:
char my_array[20] = {0};

// 索引数组和其他语言类似 -- 好吧，其实是其他的语言像C
my_array[0]; // => 0

// 数组是可变的，其实就是内存的映射！
my_array[1] = 2;
printf("%d\n", my_array[1]); // => 2

// 在C99 （C11中是可选特性），变长数组（VLA）也可以声明长度。
// 其长度不用是编译期常量。
printf("Enter the array size: "); // 询问用户数组长度
char buf[0x100];
fgets(buf, sizeof buf, stdin);

// strtoul 将字符串解析为无符号整数
size_t size = strtoul(buf, NULL, 10);
int var_length_array[size]; // 声明VLA
printf("sizeof array = %zu\n", sizeof var_length_array);

// 上述程序可能的输出为：
// > Enter the array size: 10
// > sizeof array = 40

// 字符串就是以 NUL (0x00) 这个字符结尾的字符数组,
// NUL可以用'\0'来表示.
// (在字符串字面量中我们不必输入这个字符，编译器会自动添加的)
char a_string[20] = "This is a string";
printf("%s\n", a_string); // %s 可以对字符串进行格式化
/*
也许你会注意到 a_string 实际上只有16个字节长.
第17个字节是一个空字符(NUL) 
而第18, 19 和 20 个字符的值是未定义。
*/

printf("%d\n", a_string[16]); // => 0
//  byte #17值为0（18，19，20同样为0）

// 单引号间的字符是字符字面量
// 它的类型是`int`，而 *不是* `char`
// （由于历史原因）
int cha = 'a'; // 合法
char chb = 'a'; // 同样合法 (隐式类型转换

// 多维数组
int multi_array[2][5] = {
        {1, 2, 3, 4, 5},
        {6, 7, 8, 9, 0}
    }
// 获取元素
int array_int = multi_array[0][2]; // => 3

///////////////////////////////////////
// 操作符
///////////////////////////////////////

// 多个变量声明的简写
int i1 = 1, i2 = 2;
float f1 = 1.0, f2 = 2.0;

int a, b, c;
a = b = c = 0;

// 算数运算直截了当
i1 + i2; // => 3
i2 - i1; // => 1
i2 * i1; // => 2
i1 / i2; // => 0 (0.5，但会被化整为 0)

f1 / f2; // => 0.5, 也许会有很小的误差
// 浮点数和浮点数运算都是近似值

// 取余运算
11 % 3; // => 2

// 你多半会觉得比较操作符很熟悉, 不过C中没有布尔类型
// 而是用整形替代
// (C99中有_Bool或bool。)
// 0为假, 其他均为真. (比较操作符的返回值总是返回0或1)
3 == 2; // => 0 (false)
3 != 2; // => 1 (true)
3 > 2; // => 1
3 < 2; // => 0
2 <= 2; // => 1
2 >= 2; // => 1

// C不是Python —— 连续比较不合法
int a = 1;
// 错误
int between_0_and_2 = 0 < a < 2;
// 正确
int between_0_and_2 = 0 < a && a < 2;

// 逻辑运算符适用于整数
!3; // => 0 (非)
!0; // => 1
1 && 1; // => 1 (且)
0 && 1; // => 0
0 || 1; // => 1 (或)
0 || 0; // => 0

// 条件表达式 （ ? : ）
int a = 5;
int b = 10;
int z;
z = (a > b) ? a : b; //  10 “若a > b返回a，否则返回b。”

// 增、减
char *s = "iLoveC"
int j = 0;
s[j++]; // "i" 返回s的第j项，然后增加j的值。
j = 0;
s[++j]; // => "L"  增加j的值，然后返回s的第j项。
// j-- 和 --j 同理

// 位运算
~0x0F; // => 0xF0 (取反)
0x0F & 0xF0; // => 0x00 (和)
0x0F | 0xF0; // => 0xFF (或)
0x04 ^ 0x0F; // => 0x0B (异或)
0x01 << 1; // => 0x02 (左移1位)
0x02 >> 1; // => 0x01 (右移1位)

// 对有符号整数进行移位操作要小心 —— 以下未定义：
// 有符号整数位移至符号位 int a = 1 << 32
// 左移位一个负数 int a = -1 << 2
// 移位超过或等于该类型数值的长度
// int a = 1 << 32; // 假定int32位


///////////////////////////////////////
// 控制结构
///////////////////////////////////////

if (0) {
  printf("I am never run\n");
} else if (0) {
  printf("I am also never run\n");
} else {
  printf("I print\n");
}

// While循环
int ii = 0;
while (ii < 10) { // 任何非0的值均为真
    printf("%d, ", ii++); // ii++ 在取值过后自增
} // =>  打印 "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

int kk = 0;
do {
    printf("%d, ", kk);
} while (++kk < 10); // ++kk 先自增，再被取值
// => 打印 "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

// For 循环
int jj;
for (jj=0; jj < 10; jj++) {
    printf("%d, ", jj);
} // => 打印 "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

// *****注意*****:
// 循环和函数必须有主体部分，如果不需要主体部分：
int i;
    for (i = 0; i <= 5; i++) {
    ; // 使用分号表达主体（null语句）
}

// 多重分支：switch()
switch (some_integral_expression) {
case 0: // 标签必须是整数常量表达式
    do_stuff();
    break; // 如果不使用break，控制结构会继续执行下面的标签
case 1:
    do_something_else();
    break;
default:
    // 假设 `some_integral_expression` 不匹配任何标签
    fputs("error!\n", stderr);
    exit(-1);
    break;
    }

///////////////////////////////////////
// 类型转换
///////////////////////////////////////

// 在C中每个变量都有类型，你可以将变量的类型进行转换
// (有一定限制)

int x_hex = 0x01; // 可以用16进制字面量赋值

// 在类型转换时，数字本身的值会被保留下来
printf("%d\n", x_hex); // => 打印 1
printf("%d\n", (short) x_hex); // => 打印 1
printf("%d\n", (char) x_hex); // => 打印 1

// 类型转换时可能会造成溢出，而且不会抛出警告
printf("%d\n", (char) 257); // => 1 (char的最大值为255，假定char为8位长)

// 使用<limits.h>提供的CHAR_MAX、SCHAR_MAX和UCHAR_MAX宏可以确定`char`、`signed_char`和`unisigned char`的最大值。


// 整数型和浮点型可以互相转换
printf("%f\n", (float)100); // %f 格式化单精度浮点
printf("%lf\n", (double)100); // %lf 格式化双精度浮点
printf("%d\n", (char)100.0);

///////////////////////////////////////
// 指针
///////////////////////////////////////

// 指针变量是用来储存内存地址的变量
// 指针变量的声明也会告诉它所指向的数据的类型
// 你可以使用得到你的变量的地址，并把它们搞乱，;-)

int x = 0;
printf("%p\n", &x); // 用 & 来获取变量的地址
// (%p 格式化一个类型为 void *的指针)
// => 打印某个内存地址

// 指针类型在声明中以*开头
int* px, not_a_pointer; // px是一个指向int型的指针
px = &x; // 把x的地址保存到px中
printf("%p\n", (void *)px); // => 输出内存中的某个地址
printf("%zu, %zu\n", sizeof(px), sizeof(not_a_pointer));
// => 在64位系统上打印“8， 4”。

// 要得到某个指针指向的内容的值，可以在指针前加一个*来取得（取消引用）
// 注意： 是的，这可能让人困惑，'*'在用来声明一个指针的同时取消引用它。
printf("%d\n", *px); // => 输出 0, 即x的值

// 你也可以改变指针所指向的值
// 此时你需要取消引用上添加括号，因为++比*的优先级更高
(*px)++; // 把px所指向的值增加1
printf("%d\n", *px); // => 输出 1
printf("%d\n", x); // => 输出 1

// 数组是分配一系列连续空间的常用方式
int x_array[20];
int xx;
for (xx=0; xx<20; xx++) {
    x_array[xx] = 20 - xx;
} // 初始化 x_array 为 20, 19, 18,... 2, 1

// 声明一个整型的指针，并初始化为指向x_array
int* x_ptr = x_array;
// x_ptr现在指向了数组的第一个元素(即整数20). 
// 这是因为数组通常衰减为指向它们的第一个元素的指针。
// 例如，当一个数组被传递给一个函数或者绑定到一个指针时，
//它衰减为(隐式转化为）一个指针。
// 例外： 当数组是`&`操作符的参数：
int arr[10];
int (*ptr_to_arr)[10] = &arr; // &arr的类型不是`int *`！
                              // 它的类型是指向数组的指针（数组由10个int组成）
// 或者当数组是字符串字面量（初始化字符数组）
char arr[] = "foobarbazquirk";
// 或者当它是`sizeof`或`alignof`操作符的参数时：
int arr[10];
int *ptr = arr; // 等价于 int *ptr = &arr[0];
printf("%zu, %zu\n", sizeof arr, sizeof ptr); // 应该会输出"40, 4"或"40, 8"

// 指针的增减多少是依据它本身的类型而定的
// （这被称为指针算术）
printf("%d\n", *(x_ptr + 1)); // => 打印 19
printf("%d\n", x_array[1]); // => 打印 19

// 你也可以通过标准库函数malloc来实现动态分配
// 这个函数接受一个代表容量的参数，参数类型为`size_t`
// 系统一般会从堆区分配指定容量字节大小的空间
// （在一些系统，例如嵌入式系统中这点不一定成立
// C标准对此未置一词。）
int *my_ptr = malloc(sizeof(*my_ptr) * 20);
for (xx=0; xx<20; xx++) {
    *(my_ptr + xx) = 20 - xx; // my_ptr[xx] = 20-xx
} // 初始化内存为 20, 19, 18, 17... 2, 1 (类型为int）

// 对未分配的内存进行取消引用会产生未定义的结果
printf("%d\n", *(my_ptr + 21)); // => 谁知道会输出什么

// malloc分配的区域需要手动释放
// 否则没人能够再次使用这块内存，直到程序结束为止
free(my_ptr);

// 字符串通常是字符数组，但是经常用字符指针表示
// (它是指向数组的第一个元素的指针)
// 一个优良的实践是使用`const char *`来引用一个字符串字面量，
// 因为字符串字面量不应当被修改（即"foo"[0] = 'a'犯了大忌）
const char* my_str = "This is my very own string";
printf("%c\n", *my_str); // => 'T'

// 如果字符串是数组，（多半是用字符串字面量初始化的）
// 情况就不一样了，字符串位于可写的内存中
char foo[] = "foo";
foo[0] = 'a'; // 这是合法的，foo现在包含"aoo"

function_1();
} // main函数结束

///////////////////////////////////////
// 函数
///////////////////////////////////////

// 函数声明语法:
// <返回值类型> <函数名称>(<参数>)

int add_two_ints(int x1, int x2){
    return x1 + x2; // 用return来返回一个值
}

/*
函数是按值传递的。当调用一个函数的时候，传递给函数的参数
是原有值的拷贝（数组除外）。你在函数内对参数所进行的操作
不会改变该参数原有的值。

但是你可以通过指针来传递引用，这样函数就可以更改值

例子：字符串本身翻转
*/

// 类型为void的函数没有返回值
void str_reverse(char *str_in){
    char tmp;
    int ii = 0;
    size_t len = strlen(str_in); // `strlen()`` 是C标准库函数
    for(ii = 0; ii < len / 2; ii++){
        tmp = str_in[ii];
        str_in[ii] = str_in[len - ii - 1]; // 从倒数第ii个开始
        str_in[len - ii - 1] = tmp;
    }
}

/*
char c[] = "This is a test.";
str_reverse(c);
printf("%s\n", c); // => ".tset a si sihT"
*/

// 如果引用函数之外的变量，必须使用extern关键字
int i = 0;
void testFunc() {
    extern int i; // 使用外部变量 i
}

// 使用static确保external变量为源文件私有
static int i = 0; // 其他使用 testFunc()的文件无法访问变量i
void testFunc() {
    extern int i;
}
//**你同样可以声明函数为static**


///////////////////////////////////////
// 用户自定义类型和结构
///////////////////////////////////////

// Typedefs可以创建类型别名
typedef int my_type;
my_type my_type_var = 0;

// struct是数据的集合，成员依序分配，按照
// 编写的顺序
struct rectangle {
    int width;
    int height;
};

// 一般而言，以下断言不成立：
// sizeof(struct rectangle) == sizeof(int) + sizeof(int)
//这是因为structure成员之间可能存在潜在的间隙（为了对齐）[1]

void function_1(){

    struct rectangle my_rec;

    // 通过 . 来访问结构中的数据
    my_rec.width = 10;
    my_rec.height = 20;

    // 你也可以声明指向结构体的指针
    struct rectangle *my_rec_ptr = &my_rec;

    // 通过取消引用来改变结构体的成员...
    (*my_rec_ptr).width = 30;

    // ... 或者用 -> 操作符作为简写提高可读性
    my_rec_ptr->height = 10; // Same as (*my_rec_ptr).height = 10;
}

// 你也可以用typedef来给一个结构体起一个别名
typedef struct rectangle rect;

int area(rect r){
    return r.width * r.height;
}

// 如果struct较大，你可以通过指针传递，避免
// 复制整个struct。
int area(const rect *r)
{
    return r->width * r->height;
}

///////////////////////////////////////
// 函数指针
///////////////////////////////////////
/*
在运行时，函数本身也被存放到某块内存区域当中
函数指针就像其他指针一样（不过是存储一个内存地址） 但却可以被用来直接调用函数,
并且可以四处传递回调函数
但是，定义的语法初看令人有些迷惑

例子：通过指针调用str_reverse
*/
void str_reverse_through_pointer(char *str_in) {
    // 定义一个函数指针 f. 
    void (*f)(char *); // 签名一定要与目标函数相同
    f = &str_reverse; // 将函数的地址在运行时赋给指针
    (*f)(str_in); // 通过指针调用函数
    // f(str_in); // 等价于这种调用方式
}

/*
只要函数签名是正确的，任何时候都能将任何函数赋给某个函数指针
为了可读性和简洁性，函数指针经常和typedef搭配使用：
*/

typedef void (*my_fnp_type)(char *);

// 实际声明函数指针会这么用:
// ...
// my_fnp_type f; 

// 特殊字符
'\a' // bell
'\n' // 换行
'\t' // tab
'\v' // vertical tab
'\f' // formfeed
'\r' // 回车
'\b' // 退格
'\0' // null，通常置于字符串的最后。
     //   hello\n\0. 按照惯例，\0用于标记字符串的末尾。
'\\' // 反斜杠
'\?' // 问号
'\'' // 单引号
'\"' // 双引号
'\xhh' // 十六进制数字. 例子: '\xb' = vertical tab
'\ooo' // 八进制数字. 例子: '\013' = vertical tab

// 打印格式：
"%d"    // 整数
"%3d"   // 3位以上整数 （右对齐文本）
"%s"    // 字符串
"%f"    // float
"%ld"   // long
"%3.2f" // 左3位以上、右2位以上十进制浮
"%7.4s" // (字符串同样适用)
"%c"    // 字母
"%p"    // 指针
"%x"    // 十六进制
"%o"    // 八进制
"%%"    // 打印 %

///////////////////////////////////////
// 演算优先级
///////////////////////////////////////
//---------------------------------------------------//
//        操作符                     | 组合          //
//---------------------------------------------------//
// () [] -> .                        | 从左到右      //
// ! ~ ++ -- + = *(type)sizeof       | 从右到左      //
// * / %                             | 从左到右      //
// + -                               | 从左到右      //
// << >>                             | 从左到右      //
// < <= > >=                         | 从左到右      //
// == !=                             | 从左到右      //
// &                                 | 从左到右      //
// ^                                 | 从左到右      //
// |                                 | 从左到右      //
// &&                                | 从左到右      //
// ||                                | 从左到右      //
// ?:                                | 从右到左      //
// = += -= *= /= %= &= ^= |= <<= >>= | 从右到左      //
// ,                                 | 从左到右      //
//---------------------------------------------------//

```

## 更多阅读

最好找一本 [K&R, aka "The C Programming Language", “C程序设计语言”](https://en.wikipedia.org/wiki/The_C_Programming_Language)。它是关于C最重要的一本书，由C的创作者撰写。不过需要留意的是它比较古老了，因此有些不准确的地方。


另一个比较好的资源是 [Learn C the hard way](http://learncodethehardway.org/c/)

如果你有问题，请阅读[compl.lang.c Frequently Asked Questions](http://c-faq.com/)。

使用合适的空格、缩进，保持一致的代码风格非常重要。可读性强的代码比聪明的代码、高速的代码更重要。可以参考下[Linux内核编码风格](https://www.kernel.org/doc/Documentation/CodingStyle)
。
除了这些，多多Google吧

[1] http://stackoverflow.com/questions/119123/why-isnt-sizeof-for-a-struct-equal-to-the-sum-of-sizeof-of-each-member
