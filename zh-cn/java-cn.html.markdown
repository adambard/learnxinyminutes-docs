---
name: java
category: language
language: java
lang: zh-cn
filename: LearnJava-zh.java
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translators:
    - ["Chenbo Li", "http://binarythink.net"]
---

Java是一个通用的程序语言, 包含并发, 基于类的面向对象等特性
[阅读更多](http://docs.oracle.com/javase/tutorial/java/index.html)

```java
// 单行注释
/*
多行注释
*/
/**
JavaDoc（Java文档）注释是这样的。可以用来描述类和类的属性。
*/

// 导入 java.util中的 ArrayList 类
import java.util.ArrayList;
// 导入 java.security 包中的所有类
import java.security.*;

// 每个 .java 文件都包含一个public类，这个类的名字必须和这个文件名一致。
public class LearnJava {

    // 每个程序都需要有一个main函数作为入口
    public static void main (String[] args) {

        // 使用 System.out.println 来输出到标准输出
        System.out.println("Hello World!");
        System.out.println(
            "Integer: " + 10 +
            " Double: " + 3.14 +
            " Boolean: " + true);

        // 如果要在输出后不自动换行，可以使用System.out.print方法。
        System.out.print("Hello ");
        System.out.print("World");


        ///////////////////////////////////////
        // 类型与变量
        ///////////////////////////////////////

        // 用 <type> <name> 来声明变量
        // 字节类型 - 8位补码表示
        // (-128 <= 字节 <= 127)
        byte fooByte = 100;

        // 短整型 - 16位补码表示
        // (-32,768 <= 短整型 <= 32,767)
        short fooShort = 10000;

        // 整型 - 32位补码表示
        // (-2,147,483,648 <= 整型 <= 2,147,483,647)
        int fooInt = 1;

        // 长整型 - 64位补码表示
        // (-9,223,372,036,854,775,808 <= 长整型 <= 9,223,372,036,854,775,807)
        long fooLong = 100000L;
        // L可以用来表示一个数字是长整型的。
        // 其他的数字都默认为整型。

        // 注意：Java中没有无符号类型

        // 浮点型 - 即 IEEE 754 规定的32位单精度浮点类型
        float fooFloat = 234.5f;
        // f用来表示一个数字是浮点型的。
        // 否则会被默认当做是双精度浮点型。

        // 双精度浮点型 - 即 IEEE 754 规定的64位双精度浮点类型
        double fooDouble = 123.4;

        // 布尔类型 - true 与 false
        boolean fooBoolean = true;
        boolean barBoolean = false;

        // 字符类型 - 16位 Unicode编码字符
        char fooChar = 'A';

        // 用 final 可以使一个常量不可更改
        final int HOURS_I_WORK_PER_WEEK = 9001;

        // 字符串
        String fooString = "My String Is Here!";

        // \n 代表一个新的换行
        String barString = "Printing on a new line?\nNo Problem!";
        // \t 代表一个新的制表符
        String bazString = "Do you want to add a tab?\tNo Problem!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // 数组
        // 数组在声明时大小必须已经确定
        // 数组的声明格式:
        //<数据类型> [] <变量名> = new <数据类型>[<数组大小>];
        int [] intArray = new int[10];
        String [] stringArray = new String[1];
        boolean [] booleanArray = new boolean[100];

        // 声明并初始化数组也可以这样:
        int [] intArray = {9000, 1000, 1337};

        // 随机访问数组中的元素
        System.out.println("intArray @ 0: " + intArray[0]);

        // 数组下标从0开始并且可以被更改
        intArray[1] = 1;
        System.out.println("intArray @ 1: " + intArray[1]); // => 1

        // 其他数据类型
        // ArrayLists - 类似于数组，但是功能更多，并且大小也可以改变
        // LinkedLists
        // Maps
        // HashMaps

        ///////////////////////////////////////
        // 操作符
        ///////////////////////////////////////
        System.out.println("\n->Operators");

        int i1 = 1, i2 = 2; // 多重声明可以简化

        // 算数运算
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (0.5 truncated down)

        // 取余
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // 比较操作符
        System.out.println("3 == 2? " + (3 == 2)); // => false
        System.out.println("3 != 2? " + (3 != 2)); // => true
        System.out.println("3 > 2? " + (3 > 2)); // => true
        System.out.println("3 < 2? " + (3 < 2)); // => false
        System.out.println("2 <= 2? " + (2 <= 2)); // => true
        System.out.println("2 >= 2? " + (2 >= 2)); // => true

        // 位运算操作符
        /*
        ~       取反，求反码
        <<      带符号左移
        >>      带符号右移
        >>>     无符号右移
        &       和
        ^       异或
        |       相容或
        */

        // 自增
        int i = 0;
        System.out.println("\n->Inc/Dec-rementation");
        // ++ 和 -- 操作符使变量加或减1。放在变量前面或者后面的区别是整个表达
        // 式的返回值。操作符在前面时，先加减，后取值。操作符在后面时，先取值
        // 后加减。
        System.out.println(i++); // 后自增 i = 1, 输出0
        System.out.println(++i); // 前自增 i = 2, 输出2
        System.out.println(i--); // 后自减 i = 1, 输出2
        System.out.println(--i); // 前自减 i = 0, 输出0

        ///////////////////////////////////////
        // 控制结构
        ///////////////////////////////////////
        System.out.println("\n->Control Structures");

        // If语句和C的类似
        int j = 10;
        if (j == 10){
            System.out.println("I get printed");
        } else if (j > 10) {
            System.out.println("I don't");
        } else {
            System.out.println("I also don't");
        }

        // While循环
        int fooWhile = 0;
        while(fooWhile < 100)
        {
            //System.out.println(fooWhile);
            //增加计数器
            //遍历99次， fooWhile 0->99
            fooWhile++;
        }
        System.out.println("fooWhile Value: " + fooWhile);

        // Do While循环
        int fooDoWhile = 0;
        do
        {
            //System.out.println(fooDoWhile);
            //增加计数器
            //遍历99次, fooDoWhile 0->99
            fooDoWhile++;
        }while(fooDoWhile < 100);
        System.out.println("fooDoWhile Value: " + fooDoWhile);

        // For 循环
        int fooFor;
        //for 循环结构 => for(<起始语句>; <循环进行的条件>; <步长>)
        for(fooFor=0; fooFor<10; fooFor++){
            //System.out.println(fooFor);
            //遍历 10 次, fooFor 0->9
        }
        System.out.println("fooFor Value: " + fooFor);

        // Switch Case 语句
        // switch可以用来处理 byte, short, char, 和 int 数据类型
        // 也可以用来处理枚举类型,字符串类,和原始数据类型的包装类：
        // Character, Byte, Short, 和 Integer
        int month = 3;
        String monthString;
        switch (month){
            case 1:
                    monthString = "January";
                    break;
            case 2:
                    monthString = "February";
                    break;
            case 3:
                    monthString = "March";
                    break;
            default:
                    monthString = "Some other month";
                    break;
        }
        System.out.println("Switch Case Result: " + monthString);


        ///////////////////////////////////////
        // 类型转换
        ///////////////////////////////////////

        // 数据转换

        // 将字符串转换为整型
        Integer.parseInt("123");//返回整数123

        // 将整型转换为字符串
        Integer.toString(123);//返回字符串"123"

        // 其他的数据也可以进行互相转换:
        // Double
        // Long
        // String

        // 类型转换
        // 你也可以对java对象进行类型转换, 但其中会牵扯到很多概念
        // 在这里可以查看更详细的信息:
        // http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


        ///////////////////////////////////////
        // 类与函数
        ///////////////////////////////////////

        System.out.println("\n->Classes & Functions");

        // (Bicycle类定义如下)

        // 用new来实例化一个类
        Bicycle trek = new Bicycle();

        // 调用对象的方法
        trek.speedUp(3); // 需用getter和setter方法
        trek.setCadence(100);

        // toString 可以把对象转换为字符串
        System.out.println("trek info: " + trek.toString());

    } // main 方法结束
} // LearnJava 类结束


// 你也可以把其他的非public类放入到.java文件中


// 类定义的语法:
// <public/private/protected> class <类名>{
//    //成员变量, 构造函数, 函数
//    //Java中函数被称作方法
// }

class Bicycle {

    // Bicycle 类的成员变量和方法
    public int cadence; // Public: 任意位置均可访问
    private int speed;  // Private: 只在同类中可以访问
    protected int gear; // Protected: 可以在同类与子类中可以访问
    String name; // default: 可以在包内中可以访问

    // 构造函数是初始化一个对象的方式
    // 以下是一个默认构造函数
    public Bicycle() {
        gear = 1;
        cadence = 50;
        speed = 5;
        name = "Bontrager";
    }

    // 以下是一个含有参数的构造函数
    public Bicycle(int startCadence, int startSpeed, int startGear, String name) {
        this.gear = startGear;
        this.cadence = startCadence;
        this.speed = startSpeed;
        this.name = name;
    }

    // 函数语法:
    // <public/private/protected> <返回值类型> <函数名称>(<参数列表>)

    // Java类中经常会用getter和setter来对成员变量进行操作

    // 方法声明的语法:
    // <作用域> <返回值类型> <方法名>(<参数列表>)
    public int getCadence() {
        return cadence;
    }

    // void返回值函数没有返回值
    public void setCadence(int newValue) {
        cadence = newValue;
    }

    public void setGear(int newValue) {
        gear = newValue;
    }

    public void speedUp(int increment) {
        speed += increment;
    }

    public void slowDown(int decrement) {
        speed -= decrement;
    }

    public void setName(String newName) {
        name = newName;
    }

    public String getName() {
        return name;
    }

    //返回对象属性的方法
    @Override
    public String toString() {
        return "gear: " + gear +
                " cadence: " + cadence +
                " speed: " + speed +
                " name: " + name;
    }
} // Bicycle 类结束

// PennyFarthing 是 Bicycle 的子类
class PennyFarthing extends Bicycle {
    // (Penny Farthings 是前轮很大的 Bicycle， 并且没有齿轮)

    public PennyFarthing(int startCadence, int startSpeed){
        // 通过super调用父类的构造函数
        super(startCadence, startSpeed, 0, "PennyFarthing");
    }

    // 你可以用@注释来表示需要重载的方法
    // 了解更多的注释使用方法，可以访问下面的地址：
    // http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setGear(int gear) {
        gear = 0;
    }

}

```

## 更多阅读

下面的链接只是为了便于大家理解这些主题而给出的，对于具体的例子请大家自行Google

其他主题：

* [Java 官方教程](http://docs.oracle.com/javase/tutorial/index.html)

* [Java 访问修饰符](http://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)

* [面向对象程序设计概念](http://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [继承](http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [多态](http://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [抽象](http://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [异常](http://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [接口](http://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [泛型](http://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [Java代码规范](http://www.oracle.com/technetwork/java/codeconvtoc-136057.html)
