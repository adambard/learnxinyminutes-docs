---
language: c
filename: learnc-cn.c
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Chenbo Li", "http://binarythink.net/"]
lang: zh-cn
---

C语言在今天仍然是高性能计算的主要选择。

C大概是大多数程序员用到的最接近底层的语言了，但是C语言本身不仅可以用来提升程序运行的速度
注意看看C语言的文档，你就会知道C语言在内存管理方面的强大也是其他语言无法比拟的。

```c
// 用“//”来实现单行注释

/*
多行注释是这个样子的
*/

// 用#include来导入头文件
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// 函数的标签(signature)应该放在.h文件中，并且引入到程序顶部
// 也可以直接放到你的.c文件的最上面
void function_1();
void function_2();

// c程序的入口是一个返回值为int型的函数，名字叫做main
int main() {

// 用printf来实现标准输出，这种输出也可以用格式来控制
// %d 代表一个整数, \n 代表一个新行
printf("%d\n", 0); // => 输出 0
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

unsigned char ux_char;
unsigned short ux_short;
unsigned int ux_int;
unsigned long long ux_long_long;

// char类型一定会占用1个字节，但是其他的类型却会因具体机器的不同而各异
// sizeof(T) 可以返回T类型在运行的机器上占用多少个字节 
// 这样你的代码就可以在各处正确运行了
// 比如
printf("%lu\n", sizeof(int)); // => 4 (字长为4的机器上)

// 数组必须要在开始被初始化为特定的长度
char my_char_array[20]; // 这个数组占据 1 * 20 = 20 个字节
int my_int_array[20]; // 这个数组占据 4 * 20 = 80 个字节
                      // (这里我们假设字长为4)


// 可以用下面的方法把数组初始化为0:
char my_array[20] = {0};

// 对数组任意存取就像其他语言的方式 -- 其实是其他的语言像C
my_array[0]; // => 0

// 数组是可变的，其实就是内存的映射！
my_array[1] = 2;
printf("%d\n", my_array[1]); // => 2

// 字符串就是以 NUL (0x00) 这个字符结尾的字符数组,
// 这个字符可以用'\0'来表示.
// (在字符串字面值中我们不必输入这个字符，编译器会自动添加的)
char a_string[20] = "This is a string";
printf("%s\n", a_string); // %s 可以对字符串进行格式化

/*
也许你会注意到 a_string 实际上只有16个字节长.
第17个字节是一个空字符(NUL) 
而第18, 19 和 20 个字符的值是不确定的。
*/

printf("%d\n", a_string[16]); // => 0

///////////////////////////////////////
// 操作符
///////////////////////////////////////

int i1 = 1, i2 = 2; // 多个变量声明的简写
float f1 = 1.0, f2 = 2.0;

// 算数运算
i1 + i2; // => 3
i2 - i1; // => 1
i2 * i1; // => 2
i1 / i2; // => 0 (0.5 会被化整为 0)

f1 / f2; // => 0.5, 也许会有很小的误差

// 取余运算
11 % 3; // => 2

// 比较操作符我们也很熟悉, 但是有一点，C中没有布尔类型
// 而是用整形替代
// 0 就是 false, 其他的就是 true. (比较操作符的返回值则仅有0和1)
3 == 2; // => 0 (false)
3 != 2; // => 1 (true)
3 > 2; // => 1
3 < 2; // => 0
2 <= 2; // => 1
2 >= 2; // => 1

// 逻辑运算符需要作用于整数
!3; // => 0 (非)
!0; // => 1
1 && 1; // => 1 (且)
0 && 1; // => 0
0 || 1; // => 1 (或)
0 || 0; // => 0

// 位运算
~0x0F; // => 0xF0 (取反)
0x0F & 0xF0; // => 0x00 (和)
0x0F | 0xF0; // => 0xFF (或)
0x04 ^ 0x0F; // => 0x0B (异或)
0x01 << 1; // => 0x02 (左移1位)
0x02 >> 1; // => 0x01 (右移1位)

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
while (ii < 10) {
    printf("%d, ", ii++); // ii++ 在取值过后自增
} // => 输出 "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

int kk = 0;
do {
    printf("%d, ", kk);
} while (++kk < 10); // ++kk 先自增，在被取值
// => 输出 "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

// For 循环
int jj;
for (jj=0; jj < 10; jj++) {
    printf("%d, ", jj);
} // => 输出 "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

///////////////////////////////////////
// 类型转换
///////////////////////////////////////

// 在C中每个变量都有类型，你可以将变量的类型进行转换

int x_hex = 0x01; // 可以用16进制赋值

// 在类型转换时，数字本身的值会被保留下来
printf("%d\n", x_hex); // => 输出 1
printf("%d\n", (short) x_hex); // => 输出 1
printf("%d\n", (char) x_hex); // => 输出 1

// 类型转换时可能会造成溢出，而且不会抛出警告
printf("%d\n", (char) 257); // => 1 (char的最大值为255)

// 整数型和浮点型可以互相转换
printf("%f\n", (float)100); // %f 表示单精度浮点
printf("%lf\n", (double)100); // %lf 表示双精度浮点
printf("%d\n", (char)100.0);

///////////////////////////////////////
// 指针
///////////////////////////////////////

// 指针变量是用来储存内存地址的变量
// 指针变量的定义也会告诉你指向的地址的变量的类型 
// 你也可以得到某个变量的地址，并对它们进行操作

int x = 0;
printf("%p\n", &x); // 用 & 来获取变量的地址
// (%p 表示一个指针)
// => 输出某个内存地址

// 指针类型在定义是需要以*结束
int* px; // px是一个指向int型的指针
px = &x; // 把x的地址保存到px中
printf("%p\n", px); // => 输出内存中的某个地址

// 要得到某个指针指向的内容的值，可以在指针前加一个*来取得（去引用）
printf("%d\n", *px); // => 输出 0, 即x的值

// 你也可以改变指针所指向的值
// 此时你需要在*运算符后添加一个括号，因为++比*的优先级更高
(*px)++; // 把px所指向的值增加2
printf("%d\n", *px); // => 输出 1
printf("%d\n", x); // => 输出 1

int x_array[20]; // 数组是分配一系列连续空间的常用方式
int xx;
for (xx=0; xx<20; xx++) {
    x_array[xx] = 20 - xx;
} // 初始化 x_array 为 20, 19, 18,... 2, 1

// 生命一个变量为指向整型的指针类型，并初始化为指向x_array
int* x_ptr = x_array;
// x_ptr现在指向了数组的第一个元素(即整数20). 

// 事实上数组本身就是指向它的第一个元素的指针
printf("%d\n", *(x_ptr)); // => 输出 20
printf("%d\n", x_array[0]); // => 输出 20

// 指针的增减多少是依据它本身的类型而定的
printf("%d\n", *(x_ptr + 1)); // => 输出 19
printf("%d\n", x_array[1]); // => 输出 19

// 你也可以通过标准库函数malloc来实现动态分配
// 这个函数接受一个代表容量的参数 
// 系统会从堆区分配指定容量字节大小的空间
int* my_ptr = (int*) malloc(sizeof(int) * 20);
for (xx=0; xx<20; xx++) {
    *(my_ptr + xx) = 20 - xx; // my_ptr[xx] = 20-xx 也可以
} // 初始化内存为 20, 19, 18, 17... 2, 1 (as ints)

// 如果对一些未分配的内存取值则会得到未知的结果
printf("%d\n", *(my_ptr + 21)); // => 谁知道会输出什么

// 当你通过malloc得到一块区域后，你需要释放它
// 否则没人能够再次使用这块内存，直到程序结束为止
free(my_ptr);

// 字符串通常是字符数组，但是经常用字符指针表示
// 指针:
char* my_str = "This is my very own string";

printf("%c\n", *my_str); // => 'T'

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
函数是按值传递的, 但是你可以通过传递参数来传递引用，这样函数就可以更改值

例子：字符串本身翻转
*/

// 类型为void的函数没有返回值
void str_reverse(char* str_in){
    char tmp;
    int ii=0, len = strlen(str_in); // Strlen 是C标准库函数
    for(ii=0; ii<len/2; ii++){
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

///////////////////////////////////////
// 用户自定义类型和结构
///////////////////////////////////////

// Typedefs可以创建类型别名
typedef int my_type;
my_type my_type_var = 0;

// 结构是一系列数据的集合
struct rectangle {
    int width;
    int height;
};


void function_1(){

    struct rectangle my_rec;

    // 通过 . 来访问结构中的数据
    my_rec.width = 10;
    my_rec.height = 20;

    // 你也可以声明指向结构体的指针
    struct rectangle* my_rec_ptr = &my_rec;

    // 通过取值来改变结构体的成员...
    (*my_rec_ptr).width = 30;

    // ... 或者用 -> 操作符作为简写
    my_rec_ptr->height = 10; // Same as (*my_rec_ptr).height = 10;
}

// 你也可以用typedef来给一个结构体起一个别名
typedef struct rectangle rect;

int area(rect r){
    return r.width * r.height;
}

///////////////////////////////////////
// 函数指针
///////////////////////////////////////
/*
在运行时，函数本身也被存放到某块内存区域当中
函数指针就像其他指针一样, 但却可以被用来直接调用函数,
并且可以被四处传递（就像回调函数那样）
但是，定义的语法有可能在一开始会有些误解

例子：通过指针调用str_reverse
*/
void str_reverse_through_pointer(char * str_in) {
    // 定义一个函数指针 f. 
    void (*f)(char *); // 签名一定要与目标函数相同
    f = &str_reverse; // 将函数的地址在运行时赋给指针
    (*f)(str_in); // 通过指针调用函数
    // f(str_in); // 等价于这种调用方式
}

/*
只要函数签名是正确的，任何时候都能将正确的函数赋给某个函数指针
为了可读性和简洁性，函数指针经常和typedef搭配使用：
*/

typedef void (*my_fnp_type)(char *);

// 实际声明函数指针会这么用:
// ...
// my_fnp_type f; 

```

## 更多阅读

最好找一本 [K&R, aka "The C Programming Language", “C程序设计语言”](https://en.wikipedia.org/wiki/The_C_Programming_Language)

其他一些比较好的资源 [Learn C the hard way](http://c.learncodethehardway.org/book/)

除了这些，多多Google吧
