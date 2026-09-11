---
name: C#
contributors:
    - ["Irfan Charania", "https://github.com/irfancharania"]
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Melvyn Laïly", "http://x2a.yt"]
    - ["Shaun McCarthy", "http://www.shaunmccarthy.com"]
    - ["Wouter Van Schandevijl", "http://github.com/laoujin"]
    - ["Jo Pearce", "http://github.com/jdpearce"]
    - ["Chris Zimmerman", "https://github.com/chriszimmerman"]
    - ["Shawn McGuire", "https://github.com/bigbash"]
filename: LearnCSharp.cs   
translators:
    - ["Jakukyo Friel", "http://weakish.github.io"]
    - ["CatfishWen", "http://catfishwen.github.io"]
---

C# 是一种优雅且类型安全的面向对象的语言，使开发人员能够构建运行在跨平台的 .NET 框架上，安全且健壮的应用程序。

[更多关于 C# 的介绍](https://learn.microsoft.com/zh-cn/dotnet/csharp/tour-of-csharp/)

```c#
// 单行注释以 // 开始

/*
多行注释是这样的
*/

/// <summary>
/// 这是 XML文档注释
/// 可用于生成外部文档或在 IDE 中提供上下文帮助
/// </summary>
/// <param name="firstParam">这是 firstParam 参数的文档</param>
/// <returns>这是函数返回值的信息</returns>
public void MethodOrClassOrOtherWithParsableHelp(string firstParam) { }

// 声明这段源码使用到的命名空间
// 下面的命名空间都是标准 .NET Framework 类库的一部分
using System;
using System.Collections.Generic;
using System.Dynamic;
using System.Linq;
using System.Net;
using System.Threading.Tasks;
using System.IO;

// 但是这个并不是标准类库:
using System.Data.Entity;
// 为了在下方使用它，你需要添加一个 dll 引用
// 你可以使用 NuGet 包管理器进行安装
// `Install-Package EntityFramework`

// 命名空间 Namespaces 可用于将一定的代码组织为 “包” 或者 “模块”
// 你可以在其他文件中这样引用：using Learning.CSharp;

// 在 C# 10 以后，你也可以这样定义命名空间。这被称为 file-scoped namespaces（文件范围的命名空间）.
// namespace Learning.CSharp;

namespace Learning
{
    // 每个 .cs 文件应该至少包含一个和文件名相同的类
    // 你可以不这么干，但是这样并不推荐。
    public class LearnCSharp
    {
        // 基本语法 - 如果你以前用过 Java 或 C++ 的话，可以直接跳到后文「有趣的特性」
        public static void Syntax() 
        {
            // 使用 Console.WriteLine 打印信息
            Console.WriteLine("Hello World");
            Console.WriteLine(
                "Integer: " + 10 +
                " Double: " + 3.14 +
                " Boolean: " + true);

            // 使用 Console.Write 打印将不会换行
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
            // 使用 L 标明变量值类型为 long 或 ulong

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

            // 字符串 -- 和前面的基本类型不同，字符串不是值类型，而是引用类型。
            // 这意味着你可以将字符串设为 null。
            string fooString = "\"escape\" quotes and add \n (new lines) and \t (tabs)";
            Console.WriteLine(fooString);

            // 你可以通过索引器访问字符串的每个字符：
            char charFromString = fooString[1]; // => 'e'
            // 字符串不可修改:  fooString[1] = 'X' 是行不通的;

            // 根据当前的区域格式设置比较字符串，并忽略大小写
            string.Compare(fooString, "x", StringComparison.CurrentCultureIgnoreCase);

            // 基于 sprintf 的字符串格式化
            string fooFs = string.Format("Check Check, {0} {1}, {0} {1:0.0}", 1, 2);

            // 日期和格式
            DateTime fooDate = DateTime.Now;
            Console.WriteLine(fooDate.ToString("hh:mm, dd MMM yyyy"));

            // 逐字字符串
            // 你可以在字符串字面值前使用 @ 符号来转义字符串中的所有字符。
            string path = "C:\\Users\\User\\Desktop";
            string verbatimPath = @"C:\Users\User\Desktop";
            Console.WriteLine(path == verbatimPath);  // => true

            // 使用 @ 符号后可以创建跨行的字符串。使用使用 "" 来转义 "
            string bazString = @"Here's some stuff
on a new line! ""Wow!"", the masses cried";

            // 使用 const 或 read-only 定义常量
            // 常量在编译阶段被计算
            const int HOURS_I_WORK_PER_WEEK = 9001;

            ///////////////////////////////////////////////////
            // 数据结构
            ///////////////////////////////////////////////////

            // 数组 - 从 0 开始计数
            // 声明数组时需要指定数组长度
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
            List<int> z = new List<int> { 9000, 1000, 1337 }; // 声明并初始化
            // <> 是泛型 - 参考下文

            // 列表无默认值
            // 访问列表元素时必须首先添加元素
            intList.Add(1);
            Console.WriteLine("intList at 0: " + intList[0]);

            // 其他数据结构：
            // Stack 堆栈 / Queue 队列
            // Dictionary 字典 (一个哈希表的实现)
            // HashSet 哈希集合
            // Read-only Collections 只读集合
            // Tuple 元组 (.Net 4+)

            ///////////////////////////////////////
            // 运算符
            ///////////////////////////////////////
            Console.WriteLine("\n->Operators");

            int i1 = 1, i2 = 2; // 声明多个变量的简写形式

            // 四则运算很直接
            Console.WriteLine(i1 + i2 - i1 * 3 / 7); // => 3

            // 取余
            Console.WriteLine("11%3 = " + (11 % 3)); // => 2

            // 比较运算符
            Console.WriteLine("3 == 2? " + (3 == 2)); // => false
            Console.WriteLine("3 != 2? " + (3 != 2)); // => true
            Console.WriteLine("3 > 2? " + (3 > 2)); // => true
            Console.WriteLine("3 < 2? " + (3 < 2)); // => false
            Console.WriteLine("2 <= 2? " + (2 <= 2)); // => true
            Console.WriteLine("2 >= 2? " + (2 >= 2)); // => true

            // 位运算符
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

            // 类似 C 语言的 if 语句
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

            // 三元运算符
            // 简单的 if/else 语句可以写成：
            // <条件> ? <真> : <假>
            int toCompare = 17;
            string isTrue = toCompare == 17 ? "True" : "False";

            // While 循环
            int fooWhile = 0;
            while (fooWhile < 100)
            {
                // 迭代 100 次, fooWhile 0->99
                fooWhile++;
            }

            // Do While 循环
            int fooDoWhile = 0;
            do
            {
                // 迭代 100 次, fooDoWhile 0->99
                if (false)
                    continue; // 跳过本次迭代

                fooDoWhile++;

                if (fooDoWhile == 50)
                    break; // 结束整个循环

            } while (fooDoWhile < 100);

            // for 循环结构 => for(<初始条件>; <条件>; <步>)
            for (int fooFor = 0; fooFor < 10; fooFor++)
            {
                // 迭代10次, fooFor 0->9
            }

            // foreach循环
            // foreach 循环结构 => foreach(<迭代器类型> <迭代器> in <可枚举结构>)
            // foreach 循环适用于任何实现了 IEnumerable 或 IEnumerable<T> 的对象。
            // .Net 框架下的集合类型(数组, 列表, 字典...)
            // 都实现了一个或多个这些接口
            // (下面的代码中，ToCharArray()可以删除，因为字符串同样实现了 IEnumerable 接口)
            foreach (char character in "Hello World".ToCharArray())
            {
                //迭代字符串中的所有字符
            }

            // Switch Case 语句
            // switch 适用于 byte、short、char 和 int 数据类型。
            // 同样适用于可枚举的类型 (详见“枚举类型”部分),
            // 包括字符串类型, 以及一些封装了以下基本类型的特殊类：
            // Character、Byte、Short 和 Integer。
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
                // 你可以一次匹配多个 case 语句
                // 但是你在添加 case 语句后需要使用 break
                // （否则你需要显式地使用 goto case x 语句）
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
            // 转换数据类型及类型转换
            ///////////////////////////////////////

            // 转换数据

            // 转换字符串为整数
            // 转换失败会抛出异常
            int.Parse("123"); // 返回整数类型的"123"

            // TryParse 会尝试转换类型，失败时会返回类型的默认值
            // 例如 0
            int tryInt;
            if (int.TryParse("123", out tryInt)) // Funciton is boolean
                Console.WriteLine(tryInt);       // 123

            // 转换整数为字符串
            // Convert 类提供了一系列方便转换类型的方法

            // 比如 字符串 与 int 之间

            // 最佳方法
            bool result = int.TryParse(string, out var integer);
            int.Parse(string);

            // 不推荐
            Convert.ToString(123);

            // Int 到字符串
            tryInt.ToString();

            // 类型转换
            // 显式转换 decimal 类型的 15 为 int 类型
            // 然后隐式转换为 long 类型
            long x = (int) 15M;
        }

        ///////////////////////////////////////
        // 类 - 请参阅文件末尾的定义
        ///////////////////////////////////////
        public static void Classes()
        {
            // 参看文件尾部的对象声明

            // 使用 new 实例化一个类对象
            Bicycle trek = new Bicycle();

            // 调用对象的方法
            trek.SpeedUp(3); // 你应该一直使用 setter 和 getter 方法
            trek.Cadence = 100;

            // ToString 是一种用于显示此对象值的约定
            Console.WriteLine("trek info: " + trek.Info());

            // 实例化一个新的 Penny Farthing 对象
            PennyFarthing funbike = new PennyFarthing(1, 10);
            Console.WriteLine("funbike info: " + funbike.Info());

            Console.Read();
        } // 结束 main 方法

        // record 在 C# 9 及以后可用, 这基本上是类的语法糖. record 对象是不可变的 immutable*.
        public record ARecord(string Csharp);

        // 终端程序入口 终端程序必须有一个 main 方法作为入口
        public static void Main(string[] args)
        {
            OtherInterestingFeatures();
        }

        //
        // 有趣的特性
        //
        
        // 默认方法签名

        public // 可见性
        static // 允许从类直接调用，无需先创建实例
        int // 返回值类型
        MethodSignatures(
            int maxCount, // 第一个参数，类型为整型
            int count = 0, // 如果没有传入值，则缺省值为 0
            int another = 3,
            params string[] otherParams // 捕获其他参数
        )
        { 
            return -1;
        }

        // 方法可以重名，只要方法签名不一样
        // 仅返回类型不同的方法并非唯一方法
        public static void MethodSignatures(
            ref int maxCount, // 通过引用传递
            out int count)
        {
            // 传入的参数 “count” 在函数外部将保持值为 15
            count = 15; // 必须在离开方法之前为 out 参数赋值
        }

        // 泛型
        // TKey 和 TValue 由用户调用方法时指定
        // 以下函数模拟了 Python 的 dict.setdefault()
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

        // 你可以限定传入对象的范围
        public static void IterateAndPrint<T>(T toPrint) where T: IEnumerable<int>
        {
            // 我们可以进行迭代，因为 T 是可枚举的
            foreach (var item in toPrint)
                // item 为整数
                Console.WriteLine(item.ToString());
        }

        // YIELD
        // 使用 "yield" 关键字表明它出现的方法是一个迭代器 Iterator
        // (这意味着你可以在 foreach 循环中使用它)
        public static IEnumerable<int> YieldCounter(int limit = 10)
        {
            for (var i = 0; i < limit; i++)
                yield return i;
        }

        // 你可以这样调用它
        public static void PrintYieldCounterToConsole()
        {
            foreach (var counter in YieldCounter())
                Console.WriteLine(counter);
        }

        // 你可以在一个方法中使用多个 "yield return"
        public static IEnumerable<int> ManyYieldCounter()
        {
            yield return 0;
            yield return 1;
            yield return 2;
            yield return 3;
        }

        // 你也可以使用 "yield break" 停止该迭代器
        // 此方法只会返回从 0 到 limit 的一半值。
        public static IEnumerable<int> YieldCounterWithBreak(int limit = 10)
        {
            for (var i = 0; i < limit; i++)
            {
                if (i > limit / 2) yield break;
                yield return i;
            }
        }

        public static void OtherInterestingFeatures()
        {
            // 可选参数
            MethodSignatures(3, 1, 3, "Some", "Extra", "Strings");
            MethodSignatures(3, another: 3); // 显式指定参数，忽略可选参数

            // 使用 ref 和 out 参数
            int maxCount = 0, count; // ref 参数必须有值
            MethodSignatures(ref maxCount, out count);

            // 扩展方法
            int i = 3;
            i.Print(); // 参见下面的定义

            // 可空类型 对数据库交互、返回值很有用
            // 任何值类型 (即不是类 class 的类型) 添加后缀 ? 后会变为可空类型
            // <类型>? <变量名> = <值>
            int? nullable = null; // Nullable<int> 的简写形式
            Console.WriteLine("Nullable variable: " + nullable);
            bool hasValue = nullable.HasValue; // 不为 null 时返回真

            // ?? 是用于指定默认值的语法糖
            // 以防变量为 null 的情况
            int notNullable = nullable ?? 0; // 0

            // ?. 是另一个可空类型的运算符 - 简写 null 检查
            nullable?.Print(); // 当可空类型值不为 null 的时候调用 Print() 拓展方法

            // 变量类型推断 - 你可以让编译器推断变量类型:
            var magic = "编译器确定 magic 是一个字符串，所以仍然是类型安全的";
            // magic = 9; // 不工作，因为magic是字符串，而不是整数。

            // 泛型
            //
            var phonebook = new Dictionary<string, string>() {
                {"Sarah", "212 555 5555"} // 在电话簿中加入新条目
            };

            // 调用上面定义为泛型的 SETDEFAULT
            Console.WriteLine(SetDefault<string, string>(phonebook, "Shaun", "No Phone")); // No Phone
            // 你不用指定 TKey、TValue 的类型
            // 因为它们会被隐式地推导出来
            Console.WriteLine(SetDefault(phonebook, "Sarah", "No Phone")); // 212 555 5555

            // lambda表达式 - 允许你用一行代码搞定函数
            Func<int, int> square = (x) => x * x; // 最后一项为返回值
            Console.WriteLine(square(3)); // 9

            // 错误处理 - 应对不确定的世界
            try
            {
                var funBike = PennyFarthing.CreateWithGears(6);

                // 将不再执行，因为 CreateWithGears 抛出异常
                string some = "";
                if (true) some = null;
                some.ToLower(); // 抛出 NullReferenceException
            }
            catch (NotSupportedException)
            {
                Console.WriteLine("Not so much fun now!");
            }
            catch (Exception ex) // 捕获所有其他异常
            {
                throw new ApplicationException("It hit the fan", ex);
                // throw; // 重新抛出异常并保留调用堆栈
            }
            // catch { } // 捕获所有没有捕获的异常
            finally
            {
                // 在 try 或 catch 之后执行
            }

            // 可被清理的资源管理 - 让你很容易地处理未托管的资源
            // 大多数访问未托管资源 (文件句柄、设备上下文, etc.) 的对象
            // 都实现了 IDisposable 接口。
            // using语句会为你清理 IDisposable 对象
            using (StreamWriter writer = new StreamWriter("log.txt"))
            {
                writer.WriteLine("这里没有什么可疑的东西");
                // 在作用域的结尾，资源会被回收
                // （即使有异常抛出，也一样会回收）
            } 

            // 并行框架
            // https://learn.microsoft.com/zh-cn/dotnet/standard/parallel-programming/data-parallelism-task-parallel-library

            var words = new List<string> {"dog", "cat", "horse", "pony"};

            Parallel.ForEach(words,
                new ParallelOptions() { MaxDegreeOfParallelism = 4 },
                word =>
                {
                    Console.WriteLine(word);
                }
            );

            // 运行它会产生不同的输出
            // 因为每个线程在不同的时间完成
            // 一些可能的输出是：
            // cat dog horse pony
            // dog horse pony cat

            // 动态对象（配合其他语言使用很方便）
            dynamic student = new ExpandoObject();
            student.FirstName = "First Name"; // 不需要预先定义类！

            // 你甚至可以添加方法（接受一个字符串，输出一个字符串）
            student.Introduce = new Func<string, string>(
                (introduceTo) => string.Format("Hey {0}, this is {1}", student.FirstName, introduceTo));
            Console.WriteLine(student.Introduce("Beth"));

            // IQUERYABLE<T> - 几乎所有的集合都实现了它，
            // 带给你 Map / Filter / Reduce 风格的方法
            var bikes = new List<Bicycle>();
            bikes.Sort(); // 排序 array
            bikes.Sort((b1, b2) => b1.Wheels.CompareTo(b2.Wheels)); // 根据车轮数排序
            var result = bikes
                .Where(b => b.Wheels > 3) // 筛选 - 可以连锁使用 （返回 IQueryable)
                .Where(b => b.IsBroken && b.HasTassles)
                .Select(b => b.ToString()); // Map - 这里我们使用了select，所以结果是IQueryable<string>

            var sum = bikes.Sum(b => b.Wheels); // Reduce - 计算集合中的轮子总数

            // 创建一个包含基于自行车的一些参数生成的隐式对象的列表
            var bikeSummaries = bikes.Select(b=>new { Name = b.Name, IsAwesome = !b.IsBroken && b.HasTassles });
            // 很难演示，但是编译器在代码编译完成前就能推导出以上对象的类型
            foreach (var bikeSummary in bikeSummaries.Where(b => b.IsAwesome))
                Console.WriteLine(bikeSummary.Name);

            // ASPARALLEL
            // 这就是事情开始棘手的地方 —— 组合了 linq 和并行操作
            var threeWheelers = bikes.AsParallel().Where(b => b.Wheels == 3).Select(b => b.Name);
            // 以上代码会并发地运行。会自动新开线程，分别计算结果。
            // 适用于多核、大数据量的场景。

            // LINQ - 映射一组 IQueryable<T> 对象，并延迟执行
            // 例如 LinqToSql 映射数据库, LinqToXml 映射XML文档
            var db = new BikeRespository();

            // 执行被延迟了，这对于查询数据库来说非常好
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


    // 委托 和 事件
    public class DelegateTest
    {
        public static int count = 0;
        public static int Increment()
        {
            // 增加 count 然后返回它
            return ++count;
        }

        // 委托是一个方法的引用.
        // 要引用 Increment 方法,
        // 首先声明一个具有相同签名的委托,
        // i.e. 不带参数并返回 int
        public delegate int IncrementDelegate();

        // 事件也可用于触发委托
        // 使用委托类型创建事件
        public static event IncrementDelegate MyEvent;

        static void Main(string[] args)
        {
            // 通过实例化委托来引用 Increment 方法
            // 并将方法本身作为参数传递
            IncrementDelegate inc = new IncrementDelegate(Increment);
            Console.WriteLine(inc());  // => 1

            // 委托可以用 + 运算符组合
            IncrementDelegate composedInc = inc;
            composedInc += inc;
            composedInc += inc;

            // composedInc 将执行 Increment 3 次
            Console.WriteLine(composedInc());  // => 4


            // 为事件订阅与委托
            MyEvent += new IncrementDelegate(Increment);
            MyEvent += new IncrementDelegate(Increment);

            // 触发事件
            // ie. 运行这个事件所有的委托订阅
            Console.WriteLine(MyEvent());  // => 6
        }
    }


    // 声明类的语法：
    // <public/private/protected/internal> class <类名>{
    //    // 数据字段, 构造器, 内部函数
    //    // 在Java中函数被称为方法
    // }

    public class Bicycle
    {
        // 自行车的字段/变量
        public int Cadence // Public: 任何地方都可以访问
        {
            get // get - 定义获取属性的方法
            {
                return _cadence;
            }
            set // set - 定义设置属性的方法
            {
                _cadence = value; // value 是被传递给 setter 的值
            }
        }
        private int _cadence;

        protected virtual int Gear // Protected: 类和子类可以访问
        {
            get; // 创建一个自动属性，无需成员字段
            set;
        }

        internal int Wheels // Internal: 在同一程序集内可以访问
        {
            get;
            private set; // 可以给 get/set 方法添加修饰符
        }

        int _speed; // 类中的任何内容默认为 private: 只可以在这个类内访问
                    // 你也可以使用 `private` 关键词显式指定
        public string Name { get; set; }

        // 属性还有特殊的语法，用于创建只读属性
        // 这会简单地返回表达式的结果
        public string LongName => Name + " " + _speed + " speed";

        // enum 枚举是一种值类型，由一组命名常量组成
        // 它将名称映射到值（除非特别说明，是一个整型）
        // enmu 元素的类型可以是 byte、sbyte、short、ushort、int、uint、long、ulong
        // enum 不能包含相同的值
        public enum BikeBrand
        {
            AIST,
            BMC,
            Electra = 42, //你可以显式地赋值
            Gitane // 43
        }
        // 我们在 Bicycle 类中定义的这个类型，所以它是一个内嵌类型
        // 这个类以外的代码应当使用 `Bicycle.Brand` 来引用

        public BikeBrand Brand; // 声明一个 enum 类型之后，我们可以声明这个类型的字段

        // 使用 FlagsAttribute 特性定义枚举，表示有多个值可以被匹配
        // 任何从 Attribute 派生的类都可以用来修饰类型、方法、参数等
        // 位运算符 & 和 | 可用于 与/或 操作

        [Flags]
        public enum BikeAccessories
        {
            None = 0,
            Bell = 1,
            MudGuards = 2, // 需要手动设定值!
            Racks = 4,
            Lights = 8,
            FullPackage = Bell | MudGuards | Racks | Lights
        }

        // 用法: aBike.Accessories.HasFlag(Bicycle.BikeAccessories.Bell)
        // 在 .NET 4 之前: (aBike.Accessories & Bicycle.BikeAccessories.Bell) == Bicycle.BikeAccessories.Bell
        public BikeAccessories Accessories { get; set; }

        // 静态成员属于类型自身，不属于特定的对象
        // 你无需通过对象就可以访问他们
        // Console.WriteLine("Bicycles created: " + Bicycle.bicyclesCreated);
        public static int BicyclesCreated { get; set; }
        
        // 只读值在运行时确定
        // 它们只能在声明或构造器内被赋值
        readonly bool _hasCardsInSpokes = false; // read-only private

        // 构造器是创建类的一种方式
        // 下面是一个默认的构造器
        public Bicycle() 
        {
            this.Gear = 1; // 你可以使用关键词 this 访问对象的成员
            Cadence = 50;  // 不过你并不总是需要它
            _speed = 5;
            Name = "Bontrager";
            Brand = BikeBrand.AIST;
            BicyclesCreated++;
        }

        // 另一个构造器的例子（包含参数）
        public Bicycle(int startCadence, int startSpeed, int startGear,
                       string name, bool hasCardsInSpokes, BikeBrand brand) 
            : base() // 显式调用基类的无参构造方法
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

        // 类可以为其字段实现 getters 和 setters 方法
        // 或者可以使用属性封装字段（C#推荐使用这个）

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
        // 当只需要外部访问数据的时候，考虑使用属性。
        // 属性可以定义 get 和 set，或者是同时定义两者
        private bool _hasTassles; // private variable
        public bool HasTassles // public accessor
        {
            get { return _hasTassles; }
            set { _hasTassles = value; }
        }
        
        // 你可以在一行之内定义自动属性
        // 这个语法会自动创建后备字段
        // 你可以给 getter 或 setter 设置访问修饰符
        // 以便限制它们的访问
        public bool IsBroken { get; private set; }

        // 属性的实现可以是自动的
        public int FrameSize
        {
            get;
            // 你可以给 get 或 set 指定访问修饰符
            // 以下代码意味着只有 Bicycle 类可以调用 Framesize 的 set
            private set;
        }

        // 还可以在对象上定义自定义索引器
        // 尽管这在本例中并不完全有用, 
        // 你可以使用 bicycle[0] 来获得第一项，返回 "chris" 
        // 或者 bicycle[1] = "lisa" 来设定值
        private string[] passengers = { "chris", "phil", "darren", "regina" };

        public string this[int i]
        {
            get {
                return passengers[i];
            }

            set {
                passengers[i] = value;
            }
        }

        // 显示对象属性的方法
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


    } //  Bicycle 类结束

    // PennyFarthing 是 Bicycle 的一个子类
    class PennyFarthing : Bicycle
    {
        // (Penny Farthings 是一种前轮很大的自行车。没有变速器。）

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
                throw new ArgumentException("你不可能在 PennyFarthing 上切换变速器");
            }
        }

        public static PennyFarthing CreateWithGears(int gears)
        {
            var penny = new PennyFarthing(1, 1);
            penny.Gear = gears; // 你不能这样做!
            return penny;
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
        void Jump(int meters); // 所有接口成员是隐式地公开的 public
    }

    interface IBreakable
    {
        bool Broken { get; } // 接口可以包含属性、方法和事件
    }

    // 类只能继承一个类，但是可以实现任意数量的接口
    // 但是基类名称必须是列表中的第一个，所有接口都在后面
    class MountainBike : Bicycle, IJumpable, IBreakable
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
    /// 连接数据库，一个 LinqToSql 的示例。
    /// EntityFramework Code First 很棒 (类似 Ruby的 ActiveRecord, 不过是双向的)
    /// https://learn.microsoft.com/zh-cn/ef/ef6/modeling/code-first/workflows/new-database
    /// </summary>
    public class BikeRepository : DbContext
    {
        public BikeRepository()
            : base()
        {
        }

        public DbSet<Bicycle> Bikes { get; set; }
    }

    // 一个类可以通过 partial 关键字分别写在多个 .cs 文件中
    // A1.cs
    public partial class A
    {
        public static void A1()
        {
            Console.WriteLine("Method A1 in class A");
        }
    }

    // A2.cs
    public partial class A
    {
        public static void A2()
        {
            Console.WriteLine("Method A2 in class A");
        }
    }

    // 使用 partial 类 "A"
    public class Program
    {
        static void Main()
        {
            A.A1();
            A.A2();
        }
    }

    // 通过在字符串前加上 $ 前缀来进行字符串插值
    // 并用 { 大括号 } 包裹要插值的表达式
    // 您还可以将插值字符串和逐字字符串与 $@ 组合起来
    public class Rectangle
    {
        public int Length { get; set; }
        public int Width { get; set; }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Rectangle rect = new Rectangle { Length = 5, Width = 3 };
            Console.WriteLine($"The length is {rect.Length} and the width is {rect.Width}");

            string username = "User";
            Console.WriteLine($@"C:\Users\{username}\Desktop");
        }
    }

    // C# 6 新特性
    class GlassBall : IJumpable, IBreakable
    {
        // 自动属性设置初始值
        public int Damage { get; private set; } = 0;

        // 为仅有 getter 的自动属性设定初始值
        public string Name { get; } = "Glass ball";

        // 在构造函数中初始化的仅有 getter 的自动属性
        public string GenieName { get; }

        public GlassBall(string genieName = null)
        {
            GenieName = genieName;
        }

        public void Jump(int meters)
        {
            if (meters < 0)
                // 新的 nameof() 表达式; 编译器将检查标识符是否存在
                // nameof(x) == "x"
                // 例如 预防参数名称已更改但错误消息中未更新
                throw new ArgumentException("Cannot jump negative amount!", nameof(meters));

            Damage += meters;
        }

        // 表达式主体 expression-bodied 属性 ...
        public bool Broken
            => Damage > 100;

        // ... 和方法
        public override string ToString()
            // 字符串插值
            => $"{Name}. Damage taken: {Damage}";

        public string SummonGenie()
            // Null 条件运算符
            // x?.y 如果 x 为 null 将立即返回 null; y 将不会求值
            => GenieName?.ToUpper();
    }

    static class MagicService
    {
        private static bool LogException(Exception ex)
        {
            // 记录某处的异常
            return false;
        }

        public static bool CastSpell(string spell)
        {
            try
            {
                // 假设我们在这里调用 API
                throw new MagicServiceException("Spell failed", 42);

                // Spell succeeded
                return true;
            }
            // 仅当 Code 为 42（Spell failed）时才捕获
            catch(MagicServiceException ex) when (ex.Code == 42)
            {
                // Spell failed
                return false;
            }
            // 其他异常，或 MagicServiceException 的 Code 不是 42
            catch(Exception ex) when (LogException(ex))
            {
                // 永远不会执行到这块代码
                // 堆栈未展开
            }
            return false;
            // 请注意，捕获 MagicServiceException
            // 并在 Code 不是 42 或 117 时重新抛出是不同的
            // 因为最终的 catch-all 块将不会捕获重新抛出的异常
        }
    }

    public class MagicServiceException : Exception
    {
        public int Code { get; }

        public MagicServiceException(string message, int code) : base(message)
        {
            Code = code;
        }
    }

    public static class PragmaWarning {
        // 过时的属性
        [Obsolete("Use NewMethod instead", false)]
        public static void ObsoleteMethod()
        {
            // 过时的代码
        }

        public static void NewMethod()
        {
            // 新的代码
        }

        public static void Main()
        {
            ObsoleteMethod(); // CS0618: 'ObsoleteMethod 已过时：使用 NewMethod 代替'
#pragma warning disable CS0618
            ObsoleteMethod(); // no warning
#pragma warning restore CS0618
            ObsoleteMethod(); // CS0618: 'ObsoleteMethod 已过时：使用 NewMethod 代替'
        }
    }
} // 结束 Namespace

using System;
// C# 6, 静态引用
using static System.Math;

namespace Learning.More.CSharp
{
    class StaticUsing
    {
        static void Main()
        {
            // 不使用静态引用时..
            Console.WriteLine("The square root of 4 is {}.", Math.Sqrt(4));
            // 使用时
            Console.WriteLine("The square root of 4 is {}.", Sqrt(4));
        }
    }
}

// C# 7 新特性
// 使用 Nuget 安装 Microsoft.Net.Compilers 最新版
// 使用 Nuget 安装 System.ValueTuple 最新版
using System;
namespace Csharp7
{
    // 元组 TUPLES, 析构 DECONSTRUCTION 和 弃元 DISCARDS
    class TuplesTest
    {
        public (string, string) GetName()
        {
            // 元组中的字段默认命名为 Item1、Item2...
            var names1 = ("Peter", "Parker");
            Console.WriteLine(names1.Item2);  // => Parker

            // 字段可以显式命名
            // 第 1 种声明
            (string FirstName, string LastName) names2 = ("Peter", "Parker");

            // 第 2 种声明
            var names3 = (First:"Peter", Last:"Parker");

            Console.WriteLine(names2.FirstName);  // => Peter
            Console.WriteLine(names3.Last);  // => Parker

            return names3;
        }

        public string GetLastName() {
            var fullName = GetName();

            // 元组可以被析构
            (string firstName, string lastName) = fullName;

            // 析构获得的字段可以使用 弃元 _ 丢弃
            var (_, last) = fullName;
            return last;
        }

        // 通过指定析构方法，
        // 可以以相同的方式解构任何类型
        public int randomNumber = 4;
        public int anotherRandomNumber = 10;

        public void Deconstruct(out int randomNumber, out int anotherRandomNumber)
        {
            randomNumber = this.randomNumber;
            anotherRandomNumber = this.anotherRandomNumber;
        }

        static void Main(string[] args)
        {
            var tt = new TuplesTest();
            (int num1, int num2) = tt;
            Console.WriteLine($"num1: {num1}, num2: {num2}");  // => num1: 4, num2: 10

            Console.WriteLine(tt.GetLastName());
        }
    }

    // 模式匹配
    class PatternMatchingTest
    {
        public static (string, int)? CreateLogMessage(object data)
        {
            switch(data)
            {
                // 使用 when 进行附加过滤
                case System.Net.Http.HttpRequestException h when h.Message.Contains("404"):
                    return (h.Message, 404);
                case System.Net.Http.HttpRequestException h when h.Message.Contains("400"):
                    return (h.Message, 400);
                case Exception e:
                    return (e.Message, 500);
                case string s:
                    return (s, s.Contains("Error") ? 500 : 200);
                case null:
                    return null;
                default:
                    return (data.ToString(), 500);
            }
        }
    }

    // 引用局部变量 (Reference Locals)
    // 允许返回对象的引用而不仅仅是其值
    class RefLocalsTest
    {
        // 返回值前标明 ref
        public static ref string FindItem(string[] arr, string el)
        {
            for(int i=0; i<arr.Length; i++)
            {
                if(arr[i] == el) {
                    // 返回引用
                    return ref arr[i];
                }
            }
            throw new Exception("Item not found");
        }

        public static void SomeMethod()
        {
            string[] arr = {"this", "is", "an", "array"};

            // 要在所有地方使用 ref
            ref string item = ref FindItem(arr, "array");
            item = "apple";
            Console.WriteLine(arr[3]);  // => apple
        }
    }

    // 本地函数 (LOCAL FUNCTIONS)
    class LocalFunctionTest
    {
        private static int _id = 0;
        public int id;
        public LocalFunctionTest()
        {
            id = generateId();

            // 这个本地函数只能在此作用域中被访问
            int generateId()
            {
                return _id++;
            }
        }

        public static void AnotherMethod()
        {
            var lf1 = new LocalFunctionTest();
            var lf2 = new LocalFunctionTest();
            Console.WriteLine($"{lf1.id}, {lf2.id}");  // => 0, 1

            int id = generateId();
            // error CS0103: 当前上下文中不存在名称 “generateId”
        }
    }
}
```

## 没有涉及到的主题
✨ 新的, 👍 旧的, 🎈 长期支持的, 🔥 跨平台的, 🎁 只支持 Windows 的

 * 特性 Attributes

 * 异步编程

 * Web 开发
    * ASP.NET Core ✨

 * 桌面应用开发 Development
    * Windows Presentation Foundation (WPF) 👍 🎈 🎁
    * Universal Windows Platform (UWP) ✨ 🎁
    * Uno Platform 🔥 ✨
    * WinForms 👍 🎈 🎁
    * Avalonia 🔥 ✨
    * WinUI ✨ 🎁

 * 跨平台开发
    * Xamarin.Forms 👍
    * MAUI ✨

## 扩展阅读

 * [C# language reference](https://docs.microsoft.com/dotnet/csharp/language-reference/)
 * [Learn .NET](https://dotnet.microsoft.com/learn)
 * [C# Coding Conventions](https://learn.microsoft.com/zh-cn/dotnet/csharp/fundamentals/coding-style/coding-conventions)
 * [DotNetPerls](http://www.dotnetperls.com)
 * [C# in Depth](http://manning.com/skeet2)
 * [Programming C# 5.0](http://shop.oreilly.com/product/0636920024064.do)
 * [LINQ Pocket Reference](http://shop.oreilly.com/product/9780596519254.do)
 * [Windows Forms Programming in C#](http://www.amazon.com/Windows-Forms-Programming-Chris-Sells/dp/0321116208)
 * [freeCodeCamp - C# Tutorial for Beginners](https://www.youtube.com/watch?v=GhQdlIFylQ8)
