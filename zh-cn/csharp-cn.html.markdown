---
language: c#
contributors:
    - ["Irfan Charania", "https://github.com/irfancharania"]
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Melvyn Laïly", "http://x2a.yt"]
    - ["Shaun McCarthy", "http://www.shaunmccarthy.com"]
translators:
    - ["Jakukyo Friel", "http://weakish.github.io"]
filename: LearnCSharp-cn.cs
lang: zh-cn
---


C#是一个优雅的、类型安全的面向对象语言。使用C#，开发者可以在.NET框架下构建安全、健壮的应用程序。

[更多关于C#的介绍](http://msdn.microsoft.com/en-us/library/vstudio/z1zx9t92.aspx)

```c#
// 单行注释以 // 开始
/*
多行注释是这样的
*/
/// <summary>
/// XML文档注释
/// </summary>

// 声明应用用到的命名空间
using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Dynamic;
using System.Linq;
using System.Linq.Expressions;
using System.Net;
using System.Threading.Tasks;
using System.IO;

// 定义作用域，将代码组织成包
namespace Learning
{
    // 每个 .cs 文件至少需要包含一个和文件名相同的类
    // 你可以不这么干，但是这样不好。
    public class LearnCSharp
    {
        // 基本语法 -  如果你以前用过 Java 或 C++ 的话，可以直接跳到后文「有趣的特性」
        public static void Syntax() 
        {
            // 使用 Console.WriteLine 打印信息
            Console.WriteLine("Hello World");
            Console.WriteLine(
                "Integer: " + 10 +
                " Double: " + 3.14 +
                " Boolean: " + true);

            // 使用 Console.Write 打印，不带换行符号
            Console.Write("Hello ");
            Console.Write("World");

            ///////////////////////////////////////////////////
            // 类型和变量
            //
            // 使用 <type> <name> 定义变量
            ///////////////////////////////////////////////////

            // Sbyte - 有符号 8-bit 整数
            // (-128 <= sbyte <= 127)
            sbyte fooSbyte = 100;

            // Byte - 无符号 8-bit 整数
            // (0 <= byte <= 255)
            byte fooByte = 100;

            // Short - 16-bit 整数
            // 有符号 - (-32,768 <= short <= 32,767)
            // 无符号 - (0 <= ushort <= 65,535)
            short fooShort = 10000;
            ushort fooUshort = 10000;

            // Integer - 32-bit 整数
            int fooInt = 1; // (-2,147,483,648 <= int <= 2,147,483,647)
            uint fooUint = 1; // (0 <= uint <= 4,294,967,295)

            // Long - 64-bit 整数
            long fooLong = 100000L; // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
            ulong fooUlong = 100000L; // (0 <= ulong <= 18,446,744,073,709,551,615)
            // 数字默认为 int 或 uint （取决于尺寸）
            // 使用 L 标明变量值类型为long 或 ulong

            // Double - 双精度 64-bit IEEE 754 浮点数
            double fooDouble = 123.4; // 精度: 15-16 位

            // Float - 单精度 32-bit IEEE 754 浮点数
            float fooFloat = 234.5f; // 精度: 7 位
            // 使用 f 标明变量值类型为float

            // Decimal - 128-bits 数据类型，比其他浮点类型精度更高
            // 适合财务、金融
            decimal fooDecimal = 150.3m;

            // 布尔值 - true & false
            bool fooBoolean = true; // 或 false

            // Char - 单个 16-bit Unicode 字符
            char fooChar = 'A';

            // 字符串 -- 和前面的基本类型不同，字符串不是值，而是引用。
            // 这意味着你可以将字符串设为null。
            string fooString = "\"escape\" quotes and add \n (new lines) and \t (tabs)";
            Console.WriteLine(fooString);

            // 你可以通过索引访问字符串的每个字符：
            char charFromString = fooString[1]; // => 'e'
            // 字符串不可修改:  fooString[1] = 'X' 是行不通的;

            // 根据当前的locale设定比较字符串，大小写不敏感
            string.Compare(fooString, "x", StringComparison.CurrentCultureIgnoreCase);

            // 基于sprintf的字符串格式化
            string fooFs = string.Format("Check Check, {0} {1}, {0} {1:0.0}", 1, 2);

            // 日期和格式
            DateTime fooDate = DateTime.Now;
            Console.WriteLine(fooDate.ToString("hh:mm, dd MMM yyyy"));

            // 使用 @  符号可以创建跨行的字符串。使用 "" 来表示 "
            string bazString = @"Here's some stuff
on a new line! ""Wow!"", the masses cried";

            // 使用const或read-only定义常量
            // 常量在编译期演算
            const int HOURS_I_WORK_PER_WEEK = 9001;

            ///////////////////////////////////////////////////
            // 数据结构
            ///////////////////////////////////////////////////

            // 数组 - 从0开始计数
            // 声明数组时需要确定数组长度
            // 声明数组的格式如下：
            // <datatype>[] <var name> = new <datatype>[<array size>];
            int[] intArray = new int[10];

            // 声明并初始化数组的其他方式：
            int[] y = { 9000, 1000, 1337 };

            // 访问数组的元素
            Console.WriteLine("intArray @ 0: " + intArray[0]);
            // 数组可以修改
            intArray[1] = 1;

            // 列表
            // 列表比数组更常用，因为列表更灵活。
            // 声明列表的格式如下：
            // List<datatype> <var name> = new List<datatype>();
            List<int> intList = new List<int>();
            List<string> stringList = new List<string>();
            List<int> z = new List<int> { 9000, 1000, 1337 }; // i
            // <>用于泛型 - 参考下文

            // 列表无默认值
            // 访问列表元素时必须首先添加元素
            intList.Add(1);
            Console.WriteLine("intList @ 0: " + intList[0]);

            // 其他数据结构：
            // 堆栈/队列
            // 字典 (哈希表的实现)
            // 哈希集合
            // 只读集合
            // 元组 (.Net 4+)

            ///////////////////////////////////////
            // 操作符
            ///////////////////////////////////////
            Console.WriteLine("\n->Operators");

            int i1 = 1, i2 = 2; // 多重声明的简写形式

            // 算术直截了当
            Console.WriteLine(i1 + i2 - i1 * 3 / 7); // => 3

            // 取余
            Console.WriteLine("11%3 = " + (11 % 3)); // => 2

            // 比较操作符
            Console.WriteLine("3 == 2? " + (3 == 2)); // => false
            Console.WriteLine("3 != 2? " + (3 != 2)); // => true
            Console.WriteLine("3 > 2? " + (3 > 2)); // => true
            Console.WriteLine("3 < 2? " + (3 < 2)); // => false
            Console.WriteLine("2 <= 2? " + (2 <= 2)); // => true
            Console.WriteLine("2 >= 2? " + (2 >= 2)); // => true

            // 位操作符
            /*
            ~       取反
            <<      左移（有符号）
            >>      右移（有符号）
            &       与
            ^       异或
            |       或
            */

            // 自增、自减
            int i = 0;
            Console.WriteLine("\n->Inc/Dec-rementation");
            Console.WriteLine(i++); //i = 1. 事后自增
            Console.WriteLine(++i); //i = 2. 事先自增
            Console.WriteLine(i--); //i = 1. 事后自减
            Console.WriteLine(--i); //i = 0. 事先自减

            ///////////////////////////////////////
            // 控制结构
            ///////////////////////////////////////
            Console.WriteLine("\n->Control Structures");

            // 类似C的if语句
            int j = 10;
            if (j == 10)
            {
                Console.WriteLine("I get printed");
            }
            else if (j > 10)
            {
                Console.WriteLine("I don't");
            }
            else
            {
                Console.WriteLine("I also don't");
            }

            // 三元表达式
            // 简单的 if/else 语句可以写成：
            // <条件> ? <真> : <假>
            int toCompare = 17;
            string isTrue = toCompare == 17 ? "True" : "False";

            // While 循环
            int fooWhile = 0;
            while (fooWhile < 100)
            {
                //迭代 100 次, fooWhile 0->99
                fooWhile++;
            }

            // Do While 循环
            int fooDoWhile = 0;
            do
            {
                //迭代 100 次, fooDoWhile 0->99
                fooDoWhile++;
            } while (fooDoWhile < 100);

            //for 循环结构 => for(<初始条件>; <条件>; <步>)
            for (int fooFor = 0; fooFor < 10; fooFor++)
            {
                //迭代10次, fooFor 0->9
            }

            // foreach循环
            // foreach 循环结构 => foreach(<迭代器类型> <迭代器> in <可枚举结构>)
            // foreach 循环适用于任何实现了 IEnumerable 或 IEnumerable<T> 的对象。
            // .Net 框架下的集合类型(数组, 列表, 字典...)
            // 都实现了这些接口
            // (下面的代码中，ToCharArray()可以删除，因为字符串同样实现了IEnumerable)
            foreach (char character in "Hello World".ToCharArray())
            {
                //迭代字符串中的所有字符
            }

            // Switch 语句
            // switch 适用于 byte、short、char和int 数据类型。
            // 同样适用于可枚举的类型
            // 包括字符串类, 以及一些封装了原始值的类：
            // Character、Byte、Short和Integer。
            int month = 3;
            string monthString;
            switch (month)
            {
                case 1:
                    monthString = "January";
                    break;
                case 2:
                    monthString = "February";
                    break;
                case 3:
                    monthString = "March";
                    break;
                // 你可以一次匹配多个case语句
                // 但是你在添加case语句后需要使用break
                // （否则你需要显式地使用goto case x语句）
                case 6:
                case 7:
                case 8:
                    monthString = "Summer time!!";
                    break;
                default:
                    monthString = "Some other month";
                    break;
            }

            ///////////////////////////////////////
            // 转换、指定数据类型
            ///////////////////////////////////////

            // 转换类型

            // 转换字符串为整数
            // 转换失败会抛出异常
            int.Parse("123");//返回整数类型的"123"

            // TryParse会尝试转换类型，失败时会返回缺省类型
            // 例如 0
            int tryInt;
            if (int.TryParse("123", out tryInt)) // Funciton is boolean
                Console.WriteLine(tryInt);       // 123

            // 转换整数为字符串
            // Convert类提供了一系列便利转换的方法
            Convert.ToString(123);
            // or
            tryInt.ToString();
        }

        ///////////////////////////////////////
        // 类
        ///////////////////////////////////////
        public static void Classes()
        {
            // 参看文件尾部的对象声明

            // 使用new初始化对象
            Bicycle trek = new Bicycle();

            // 调用对象的方法
            trek.SpeedUp(3); // 你应该一直使用setter和getter方法
            trek.Cadence = 100;

            // 查看对象的信息.
            Console.WriteLine("trek info: " + trek.Info());

            // 实例化一个新的Penny Farthing
            PennyFarthing funbike = new PennyFarthing(1, 10);
            Console.WriteLine("funbike info: " + funbike.Info());

            Console.Read();
        } // 结束main方法

        // 终端程序 终端程序必须有一个main方法作为入口
        public static void Main(string[] args)
        {
            OtherInterestingFeatures();
        }

        //
        // 有趣的特性
        //
        
        // 默认方法签名

        public // 可见性
        static // 允许直接调用类，无需先创建实例
        int, //返回值
        MethodSignatures(
            int maxCount, // 第一个变量，类型为整型
            int count = 0, // 如果没有传入值，则缺省值为0
            int another = 3,
            params string[] otherParams // 捕获其他参数
        )
        { 
            return -1;
        }

        // 方法可以重名，只要签名不一样
        public static void MethodSignature(string maxCount)
        {
        }

        //泛型
        // TKey和TValue类由用用户调用函数时指定。
        // 以下函数模拟了Python的SetDefault
        public static TValue SetDefault<TKey, TValue>(
            IDictionary<TKey, TValue> dictionary, 
            TKey key, 
            TValue defaultItem)
        {
            TValue result;
            if (!dictionary.TryGetValue(key, out result))
                return dictionary[key] = defaultItem;
            return result;
        }

        // 你可以限定传入值的范围
        public static void IterateAndPrint<T>(T toPrint) where T: IEnumerable<int>
        {
            // 我们可以进行迭代，因为T是可枚举的
            foreach (var item in toPrint)
                // ittm为整数
                Console.WriteLine(item.ToString());
        }

        public static void OtherInterestingFeatures()
        {
            // 可选参数  
            MethodSignatures(3, 1, 3, "Some", "Extra", "Strings");
            MethodSignatures(3, another: 3); // 显式指定参数，忽略可选参数

            // 扩展方法
            int i = 3;
            i.Print(); // 参见下面的定义 

            // 可为null的类型 对数据库交互、返回值很有用
            // 任何值类型 (i.e. 不为类) 添加后缀 ? 后会变为可为null的值
            // <类型>? <变量名> = <值>
            int? nullable = null; // Nullable<int> 的简写形式
            Console.WriteLine("Nullable variable: " + nullable);
            bool hasValue = nullable.HasValue; // 不为null时返回真
            // ?? 是用于指定默认值的语法糖
            // 以防变量为null的情况
            int notNullable = nullable ?? 0; // 0

            // 变量类型推断 - 你可以让编译器推断变量类型:
            var magic = "编译器确定magic是一个字符串，所以仍然是类型安全的";
            // magic = 9; // 不工作，因为magic是字符串，而不是整数。 

            // 泛型
            //
            var phonebook = new Dictionary<string, string>() { 
                {"Sarah", "212 555 5555"} // 在电话簿中加入新条目
            };

            // 调用上面定义为泛型的SETDEFAULT
            Console.WriteLine(SetDefault<string,string>(phonebook, "Shaun", "No Phone")); // 没有电话
            // 你不用指定TKey、TValue，因为它们会被隐式地推导出来
            Console.WriteLine(SetDefault(phonebook, "Sarah", "No Phone")); // 212 555 5555

            // lambda表达式 - 允许你用一行代码搞定函数
            Func<int, int> square = (x) => x * x; // 最后一项为返回值
            Console.WriteLine(square(3)); // 9

            // 可抛弃的资源管理 - 让你很容易地处理未管理的资源
            // 大多数访问未管理资源 (文件操作符、设备上下文, etc.)的对象
            // 都实现了IDisposable接口。 
            // using语句会为你清理IDisposable对象。
            using (StreamWriter writer = new StreamWriter("log.txt"))
            {
                writer.WriteLine("这里没有什么可疑的东西");
                // 在作用域的结尾，资源会被回收
                // （即使有异常抛出，也一样会回收）
            } 

            // 并行框架
            // http://blogs.msdn.com/b/csharpfaq/archive/2010/06/01/parallel-programming-in-net-framework-4-getting-started.aspx
            var websites = new string[] { 
                "http://www.google.com", "http://www.reddit.com", 
                "http://www.shaunmccarthy.com"
            };
            var responses = new Dictionary<string, string>();
            
            // 为每个请求新开一个线程
            // 在运行下一步前合并结果
            Parallel.ForEach(websites, 
                new ParallelOptions() {MaxDegreeOfParallelism = 3}, // max of 3 threads
                website =>
            {
                // Do something that takes a long time on the file
                using (var r = WebRequest.Create(new Uri(website)).GetResponse())
                {
                    responses[website] = r.ContentType;
                }
            });

            // 直到所有的请求完成后才会运行下面的代码
            foreach (var key in responses.Keys)
                Console.WriteLine("{0}:{1}", key, responses[key]);

            // 动态对象（配合其他语言使用很方便）
            dynamic student = new ExpandoObject();
            student.FirstName = "First Name"; // 不需要先定义类！

            // 你甚至可以添加方法（接受一个字符串，输出一个字符串）
            student.Introduce = new Func<string, string>(
                (introduceTo) => string.Format("Hey {0}, this is {1}", student.FirstName, introduceTo));
            Console.WriteLine(student.Introduce("Beth"));

            // IQUERYABLE<T> - 几乎所有的集合都实现了它，
            // 带给你 Map / Filter / Reduce 风格的方法
            var bikes = new List<Bicycle>();
            bikes.Sort(); // Sorts the array
            bikes.Sort((b1, b2) => b1.Wheels.CompareTo(b2.Wheels)); // 根据车轮数排序
            var result = bikes
                .Where(b => b.Wheels > 3) // 筛选 - 可以连锁使用 （返回IQueryable)
                .Where(b => b.IsBroken && b.HasTassles)
                .Select(b => b.ToString()); // Map - 这里我们使用了select，所以结果是IQueryable<string>

            var sum = bikes.Sum(b => b.Wheels); // Reduce - 计算集合中的轮子总数

            // 创建一个包含基于自行车的一些参数生成的隐式对象的列表
            var bikeSummaries = bikes.Select(b=>new { Name = b.Name, IsAwesome = !b.IsBroken && b.HasTassles });
            // 很难演示，但是编译器在代码编译完成前就能推导出以上对象的类型
            foreach (var bikeSummary in bikeSummaries.Where(b => b.IsAwesome))
                Console.WriteLine(bikeSummary.Name);

            // ASPARALLEL
            // 邪恶的特性 —— 组合了linq和并行操作
            var threeWheelers = bikes.AsParallel().Where(b => b.Wheels == 3).Select(b => b.Name);
            // 以上代码会并发地运行。会自动新开线程，分别计算结果。
            // 适用于多核、大数据量的场景。

            // LINQ - 将IQueryable<T>映射到存储，延缓执行
            // 例如 LinqToSql 映射数据库, LinqToXml 映射XML文档
            var db = new BikeRespository();

            // 执行被延迟了，这对于查询数据库来说很好
            var filter = db.Bikes.Where(b => b.HasTassles); // 不运行查询
            if (42 > 6) // 你可以不断地增加筛选，包括有条件的筛选，例如用于“高级搜索”功能
                filter = filter.Where(b => b.IsBroken); // 不运行查询 

            var query = filter
                .OrderBy(b => b.Wheels)
                .ThenBy(b => b.Name)
                .Select(b => b.Name); // 仍然不运行查询

            // 现在运行查询，运行查询的时候会打开一个读取器，所以你迭代的是一个副本
            foreach (string bike in query) 
                Console.WriteLine(result);
            


        }

    } // 结束LearnCSharp类

    // 你可以在同一个 .cs 文件中包含其他类

    public static class Extensions
    {
        // 扩展函数
        public static void Print(this object obj)
        {
            Console.WriteLine(obj.ToString());
        }
    }
    // 声明类的语法：
    // <public/private/protected/internal> class <类名>{
    //    //数据字段, 构造器, 内部函数.
    /     // 在Java中函数被称为方法。
    // }

    public class Bicycle
    {
        // 自行车的字段、变量
        public int Cadence // Public: 任何地方都可以访问
        {
            get // get - 定义获取属性的方法
            {
                return _cadence;
            }
            set // set - 定义设置属性的方法
            {
                _cadence = value; // value是被传递给setter的值
            }
        }
        private int _cadence;

        protected virtual int Gear // 类和子类可以访问
        {
            get; // 创建一个自动属性，无需成员字段
            set;
        }

        internal int Wheels // Internal:在同一程序集内可以访问
        {
            get;
            private set; // 可以给get/set方法添加修饰符
        }

        int _speed; // 默认为private: 只可以在这个类内访问，你也可以使用`private`关键词
        public string Name { get; set; }

        // enum类型包含一组常量
        // 它将名称映射到值（除非特别说明，是一个整型）
        // enmu元素的类型可以是byte、sbyte、short、ushort、int、uint、long、ulong。
        // enum不能包含相同的值。
        public enum BikeBrand
        {
            AIST,
            BMC,
            Electra = 42, //你可以显式地赋值
            Gitane // 43
        }
        // 我们在Bicycle类中定义的这个类型，所以它是一个内嵌类型。
        // 这个类以外的代码应当使用`Bicycle.Brand`来引用。

        public BikeBrand Brand; // 声明一个enum类型之后，我们可以声明这个类型的字段

        // 静态方法的类型为自身，不属于特定的对象。
        // 你无需引用对象就可以访问他们。
        // Console.WriteLine("Bicycles created: " + Bicycle.bicyclesCreated);
        static public int BicyclesCreated = 0;
        
        // 只读值在运行时确定
        // 它们只能在声明或构造器内被赋值
        readonly bool _hasCardsInSpokes = false; // read-only private

        // 构造器是创建类的一种方式
        // 下面是一个默认的构造器
        public Bicycle() 
        {
            this.Gear = 1; // 你可以使用关键词this访问对象的成员
            Cadence = 50;  // 不过你并不总是需要它
            _speed = 5;
            Name = "Bontrager";
            Brand = BikeBrand.AIST;
            BicyclesCreated++;
        }

        // 另一个构造器的例子（包含参数）
        public Bicycle(int startCadence, int startSpeed, int startGear,
                       string name, bool hasCardsInSpokes, BikeBrand brand) 
            : base() // 首先调用base
        {
            Gear = startGear; 
            Cadence = startCadence;
            _speed = startSpeed;
            Name = name; 
            _hasCardsInSpokes = hasCardsInSpokes;
            Brand = brand;
        }

        // 构造器可以连锁使用
        public Bicycle(int startCadence, int startSpeed, BikeBrand brand) :
            this(startCadence, startSpeed, 0, "big wheels", true, brand)
        {
        }

        // 函数语法
        // <public/private/protected> <返回值> <函数名称>(<参数>)

        // 类可以为字段实现 getters 和 setters 方法 for their fields
        // 或者可以实现属性（C#推荐使用这个）
        // 方法的参数可以有默认值
        // 在有默认值的情况下，调用方法的时候可以省略相应的参数
        public void SpeedUp(int increment = 1)
        {
            _speed += increment;
        }

        public void SlowDown(int decrement = 1)
        {
            _speed -= decrement;
        }

        // 属性可以访问和设置值
        // 当只需要访问数据的时候，考虑使用属性。
        // 属性可以定义get和set，或者是同时定义两者
        private bool _hasTassles; // private variable
        public bool HasTassles // public accessor
        {
            get { return _hasTassles; }
            set { _hasTassles = value; }
        }
        
        // 你可以在一行之内定义自动属性
        // 这个语法会自动创建后备字段
        // 你可以给getter或setter设置访问修饰符
        // 以便限制它们的访问
        public bool IsBroken { get; private set; }

        // 属性的实现可以是自动的
        public int FrameSize
        {
            get;
            // 你可以给get或set指定访问修饰符
            // 以下代码意味着只有Bicycle类可以调用Framesize的set
            private set;
        }

        //显示对象属性的方法
        public virtual string Info()
        {
            return "Gear: " + Gear +
                    " Cadence: " + Cadence +
                    " Speed: " + _speed +
                    " Name: " + Name +
                    " Cards in Spokes: " + (_hasCardsInSpokes ? "yes" : "no") +
                    "\n------------------------------\n"
                    ;
        }

        // 方法可以是静态的。通常用于辅助方法。
        public static bool DidWeCreateEnoughBycles()
        {
            // 在静态方法中，你只能引用类的静态成员
            return BicyclesCreated > 9000;
        } // 如果你的类只需要静态成员，考虑将整个类作为静态类。


    } //  Bicycle类结束

    // PennyFarthing是Bicycle的一个子类
    class PennyFarthing : Bicycle
    {
        // (Penny Farthings是一种前轮很大的自行车。没有齿轮。）

        // 调用父构造器
        public PennyFarthing(int startCadence, int startSpeed) :
            base(startCadence, startSpeed, 0, "PennyFarthing", true, BikeBrand.Electra)
        {
        }

        protected override int Gear
        {
            get
            {
                return 0;
            }
            set
            {
                throw new ArgumentException("你不可能在PennyFarthing上切换齿轮");
            }
        }

        public override string Info()
        {
            string result = "PennyFarthing bicycle ";
            result += base.ToString(); // 调用父方法
            return result;
        }
    }

    // 接口只包含成员的签名，而没有实现。
    interface IJumpable
    {
        void Jump(int meters); // 所有接口成员是隐式地公开的
    }

    interface IBreakable
    {
        bool Broken { get; } // 接口可以包含属性、方法和事件
    }

    // 类只能继承一个类，但是可以实现任意数量的接口
    {
        int damage = 0;

        public void Jump(int meters)
        {
            damage += meters;
        }

        public bool Broken
        {
            get
            {
                return damage > 100;
            }
        }
    }

    /// <summary>
    /// 连接数据库，一个 LinqToSql的示例。
    /// EntityFramework Code First 很棒 (类似 Ruby的 ActiveRecord, 不过是双向的)
    /// http://msdn.microsoft.com/en-us/data/jj193542.aspx
    /// </summary>
    public class BikeRespository : DbSet
    {
        public BikeRespository()
            : base()
        {
        }

        public DbSet<Bicycle> Bikes { get; set; }
    }
} // 结束 Namespace
```

## 没有涉及到的主题

 * Flags
 * Attributes
 * 静态属性
 * Exceptions, Abstraction
 * ASP.NET (Web Forms/MVC/WebMatrix)
 * Winforms
 * Windows Presentation Foundation (WPF)

## 扩展阅读

 * [DotNetPerls](http://www.dotnetperls.com)
 * [C# in Depth](http://manning.com/skeet2)
 * [Programming C#](http://shop.oreilly.com/product/0636920024064.do)
 * [LINQ](http://shop.oreilly.com/product/9780596519254.do)
 * [MSDN Library](http://msdn.microsoft.com/en-us/library/618ayhy6.aspx)
 * [ASP.NET MVC Tutorials](http://www.asp.net/mvc/tutorials)
 * [ASP.NET Web Matrix Tutorials](http://www.asp.net/web-pages/tutorials)
 * [ASP.NET Web Forms Tutorials](http://www.asp.net/web-forms/tutorials)
 * [Windows Forms Programming in C#](http://www.amazon.com/Windows-Forms-Programming-Chris-Sells/dp/0321116208)
 * [C# Coding Conventions](http://msdn.microsoft.com/en-us/library/vstudio/ff926074.aspx)
