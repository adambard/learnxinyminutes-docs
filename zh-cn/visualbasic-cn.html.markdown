---
language: Visual Basic
contributors:
    - ["Brian Martin", "http://brianmartin.biz"]
translators:
    - ["Abner Chou", "http://cn.abnerchou.me"]
lang: zh-cn
filename: learnvisualbasic.vb-cn
---

```vb
Module Module1

    Sub Main()
        ' 让我们先从简单的终端程序学起。
        ' 单引号用来生成注释（注意是半角单引号，非全角单引号’）
        ' 为了方便运行此示例代码，我写了个目录索引。
        ' 可能你还不了解以下代码的意义，但随着教程的深入，
        ' 你会渐渐理解其用法。
        Console.Title = ("Learn X in Y Minutes")
        Console.WriteLine("NAVIGATION") ' 显示目录
        Console.WriteLine("")
        Console.ForegroundColor = ConsoleColor.Green
        Console.WriteLine("1. Hello World Output") ' Hello world 输出示例
        Console.WriteLine("2. Hello World Input") ' Hello world 输入示例
        Console.WriteLine("3. Calculating Whole Numbers") ' 求整数之和
        Console.WriteLine("4. Calculating Decimal Numbers") ' 求小数之和
        Console.WriteLine("5. Working Calculator") ' 计算器
        Console.WriteLine("6. Using Do While Loops") ' 使用 Do While 循环
        Console.WriteLine("7. Using For While Loops") ' 使用 For While 循环
        Console.WriteLine("8. Conditional Statements") ' 条件语句
        Console.WriteLine("9. Select A Drink") ' 选饮料
        Console.WriteLine("50. About") ' 关于
        Console.WriteLine("Please Choose A Number From The Above List")
        Dim selection As String = Console.ReadLine
        Select Case selection
            Case "1" ' Hello world 输出示例
                Console.Clear() ' 清空屏幕
                HelloWorldOutput() ' 调用程序块
            Case "2" ' Hello world 输入示例
                Console.Clear()
                HelloWorldInput()
            Case "3" ' 求整数之和
                Console.Clear()
                CalculatingWholeNumbers()
            Case "4" ' 求小数之和
                Console.Clear()
                CalculatingDecimalNumbers()
            Case "5" ' 计算器
                Console.Clear()
                WorkingCalculator()
            Case "6" ' 使用 do while 循环
                Console.Clear()
                UsingDoWhileLoops()
            Case "7" ' 使用 for while 循环
                Console.Clear()
                UsingForLoops()
            Case "8" ' 条件语句
                Console.Clear()
                ConditionalStatement()
            Case "9" ' If/Else 条件语句
                Console.Clear()
                IfElseStatement() ' 选饮料
            Case "50" ' 关于本程序和作者
                Console.Clear()
                Console.Title = ("Learn X in Y Minutes :: About")
                MsgBox("This tutorial is by Brian Martin (@BrianMartinn")
                Console.Clear()
                Main()
                Console.ReadLine()

        End Select
    End Sub

    ' 一、对应程序目录1，下同

    ' 使用 private subs 声明函数。 
    Private Sub HelloWorldOutput()
        ' 程序名
        Console.Title = "Hello World Ouput | Learn X in Y Minutes"
        ' 使用 Console.Write("") 或者 Console.WriteLine("") 来输出文本到屏幕上
        ' 对应的 Console.Read() 或 Console.Readline() 用来读取键盘输入
        Console.WriteLine("Hello World")
        Console.ReadLine() 
        ' Console.WriteLine()后加Console.ReadLine()是为了防止屏幕输出信息一闪而过
        ' 类似平时常见的“单击任意键继续”的意思。
    End Sub

    ' 二
    Private Sub HelloWorldInput()
        Console.Title = "Hello World YourName | Learn X in Y Minutes"
        ' 变量
        ' 用来存储用户输入的数据
        ' 变量声明以 Dim 开始，结尾为 As VariableType （变量类型）.

        ' 此教程中，我们希望知道你的姓名，并让程序记录并输出。
        Dim username As String
        ' 我们定义username使用字符串类型（String）来记录用户姓名。
        Console.WriteLine("Hello, What is your name? ") ' 询问用户输入姓名
        username = Console.ReadLine() ' 存储用户名到变量 username
        Console.WriteLine("Hello " + username) ' 输出将是 Hello + username
        Console.ReadLine() ' 暂停屏幕并显示以上输出
        ' 以上程序将询问你的姓名，并和你打招呼。
        ' 其它变量如整型（Integer）我们用整型来处理整数。
    End Sub

    ' 三
    Private Sub CalculatingWholeNumbers()
        Console.Title = "Calculating Whole Numbers | Learn X in Y Minutes"
        Console.Write("First number: ") ' 输入一个整数：1，2，50，104，等等
        Dim a As Integer = Console.ReadLine()
        Console.Write("Second number: ") ' 输入第二个整数
        Dim b As Integer = Console.ReadLine()
        Dim c As Integer = a + b
        Console.WriteLine(c)
        Console.ReadLine()
        ' 以上程序将两个整数相加
    End Sub

    ' 四
    Private Sub CalculatingDecimalNumbers()
        Console.Title = "Calculating with Double | Learn X in Y Minutes"
        ' 当然，我们还需要能够处理小数。
        ' 只需要要将整型（Integer）改为小数（Double）类型即可。

        ' 输入一个小数： 1.2， 2.4， 50.1， 104.9，等等
        Console.Write("First number: ")
        Dim a As Double = Console.ReadLine
        Console.Write("Second number: ") ' 输入第二个数
        Dim b As Double = Console.ReadLine
        Dim c As Double = a + b
        Console.WriteLine(c)
        Console.ReadLine()
        ' 以上代码能实现两个小数相加
    End Sub

    ' 五
    Private Sub WorkingCalculator()
        Console.Title = "The Working Calculator| Learn X in Y Minutes"
        ' 但是如果你希望有个能够处理加减乘除的计算器呢？
        ' 只需将上面代码复制粘帖即可。
        Console.Write("First number: ") ' 输入第一个数
        Dim a As Double = Console.ReadLine
        Console.Write("Second number: ") ' 输入第二个数
        Dim b As Integer = Console.ReadLine
        Dim c As Integer = a + b
        Dim d As Integer = a * b
        Dim e As Integer = a - b
        Dim f As Integer = a / b

        ' 通过以下代码我们可以将以上所算的加减乘除结果输出到屏幕上。
        Console.Write(a.ToString() + " + " + b.ToString())
        ' 我们希望答案开头能有3个空格，可以使用String.PadLeft(3)方法。
        Console.WriteLine(" = " + c.ToString.PadLeft(3))
        Console.Write(a.ToString() + " * " + b.ToString())
        Console.WriteLine(" = " + d.ToString.PadLeft(3))
        Console.Write(a.ToString() + " - " + b.ToString())
        Console.WriteLine(" = " + e.ToString.PadLeft(3))
        Console.Write(a.ToString() + " / " + b.ToString())
        Console.WriteLine(" = " + e.ToString.PadLeft(3))
        Console.ReadLine()

    End Sub

    ' 六
    Private Sub UsingDoWhileLoops()
        ' 如同以上的代码一样
        ' 这次我们将询问用户是否继续 （Yes or No?)
        ' 我们将使用Do While循环，因为我们不知到用户是否需要使用一次以上。
        Console.Title = "UsingDoWhileLoops | Learn X in Y Minutes"
        Dim answer As String ' 我们使用字符串变量来存储answer（答案）
        Do ' 循环开始
            Console.Write("First number: ")
            Dim a As Double = Console.ReadLine
            Console.Write("Second number: ")
            Dim b As Integer = Console.ReadLine
            Dim c As Integer = a + b
            Dim d As Integer = a * b
            Dim e As Integer = a - b
            Dim f As Integer = a / b

            Console.Write(a.ToString() + " + " + b.ToString())
            Console.WriteLine(" = " + c.ToString.PadLeft(3))
            Console.Write(a.ToString() + " * " + b.ToString())
            Console.WriteLine(" = " + d.ToString.PadLeft(3))
            Console.Write(a.ToString() + " - " + b.ToString())
            Console.WriteLine(" = " + e.ToString.PadLeft(3))
            Console.Write(a.ToString() + " / " + b.ToString())
            Console.WriteLine(" = " + e.ToString.PadLeft(3))
            Console.ReadLine()
            ' 询问用户是否继续，注意大小写。 
            Console.Write("Would you like to continue? (yes / no)")
            ' 程序读入用户输入
            answer = Console.ReadLine() ' added a bracket here
        ' 当用户输入"yes"时，程序将跳转到Do，并再次执行
        Loop While answer = "yes"

    End Sub

    ' 七
    Private Sub UsingForLoops()
        ' 有一些程序只需要运行一次。
        ' 这个程序我们将实现从10倒数计数.

        Console.Title = "Using For Loops | Learn X in Y Minutes"
        ' 声明变量和Step (步长,即递减的速度，如-1，-2，-3等）。 
        For i As Integer = 10 To 0 Step -1 
            Console.WriteLine(i.ToString) ' 将计数结果输出的屏幕
        Next i ' 计算新的i值
        Console.WriteLine("Start") 
        Console.ReadLine() 
    End Sub

    ' 八
    Private Sub ConditionalStatement()
        Console.Title = "Conditional Statements | Learn X in Y Minutes"
        Dim userName As String = Console.ReadLine
        Console.WriteLine("Hello, What is your name? ") ' 询问用户姓名
        userName = Console.ReadLine() ' 存储用户姓名
        If userName = "Adam" Then
            Console.WriteLine("Hello Adam")
            Console.WriteLine("Thanks for creating this useful site")
            Console.ReadLine()
        Else
            Console.WriteLine("Hello " + userName)
            Console.WriteLine("Have you checked out www.learnxinyminutes.com")
            Console.ReadLine() ' 程序停止，并输出以上文本
        End If
    End Sub

    ' 九
    Private Sub IfElseStatement()
    Console.Title = "If / Else Statement | Learn X in Y Minutes"
        ' 有时候我们需要考虑多于两种情况。
        ' 这时我们就需要使用If/ElesIf条件语句。
        ' If语句就好似个自动售货机，当用户输入A1，A2，A3，等去选择物品时，
        ' 所有的选择可以合并到一个If语句中

        Dim selection As String = Console.ReadLine() ' 读入用户选择
            Console.WriteLine("A1. for 7Up") ' A1 七喜
            Console.WriteLine("A2. for Fanta") ' A2 芬达
            Console.WriteLine("A3. for Dr. Pepper") ' A3 胡椒医生
            Console.WriteLine("A4. for Diet Coke") ' A4 无糖可乐
            Console.ReadLine()
            If selection = "A1" Then
                Console.WriteLine("7up")
                Console.ReadLine()
            ElseIf selection = "A2" Then
                Console.WriteLine("fanta")
                Console.ReadLine()
            ElseIf selection = "A3" Then
                Console.WriteLine("dr. pepper")
                Console.ReadLine()
            ElseIf selection = "A4" Then
                Console.WriteLine("diet coke")
                Console.ReadLine()
            Else
                Console.WriteLine("Please select a product") ' 请选择你需要的产品
                Console.ReadLine()
            End If

    End Sub

End Module

```

## 参考

我（译注：原作者）在命令行下学习的VB。命令行编程使我能够更好的了解程序编译运行机制，并使学习其它语言变得容易。 

如果希望进一步学习VB，这里还有更深层次的 <a href="http://www.vbbootcamp.co.uk/" Title="VB教学">VB教学（英文）</a>。 

所有代码均通过测试。只需复制粘帖到Visual Basic中，并按F5运行即可。 
