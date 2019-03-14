---
language: Visual Basic
contributors:
    - ["Brian Martin", "http://brianmartin.biz"]
translators:
    - ["satory-ra", "https://github.com/satory-ra"]
filename: learnvisualbasic-ru.vb
lang: ru-ru
---

```vbnet
Module Module1

    Sub Main()
        'Краткий обзор консольных приложений Visual Basic перед более
        'глубоким изучением.
        'Апостроф начинает строку комментария.
        'Чтобы изучить это руководство в компиляторе Visual Basic,
        'я создал систему навигации.
        'Эта система будет объяснена при прохождении этого урока.
        'Постепенно вы всё поймете.
        Console.Title = ("Выучи Х за Y минут")
        Console.WriteLine("НАВИГАЦИЯ") 'Display
        Console.WriteLine("")
        Console.ForegroundColor = ConsoleColor.Green
        Console.WriteLine("1. Вывод данных")
        Console.WriteLine("2. Ввод данных")
        Console.WriteLine("3. Расчёт целых чисел")
        Console.WriteLine("4. Расчёт десятичных дробей")
        Console.WriteLine("5. Калькулятор")
        Console.WriteLine("6. Использование циклов Do While")
        Console.WriteLine("7. Использование циклов For")
        Console.WriteLine("8. Условные выражения")
        Console.WriteLine("9. Выберите напиток")
        Console.WriteLine("50. О приложении")
        Console.WriteLine("Выберите номер из списка")
        Dim selection As String = Console.ReadLine
        '«Case» в операторе Select не является обязательным.
        'Например, "Select selection" вместо "Select Case selection"
        'также будет работать.
        Select Case selection
            Case "1" 'Вывод данных
                Console.Clear() 'Очищает окно консоли
                HelloWorldOutput() 'Открывает приватную подпрограмму.
            Case "2" 'Ввод данных
                Console.Clear()
                HelloWorldInput()
            Case "3" 'Расчёт целых чисел
                Console.Clear()
                CalculatingWholeNumbers()
            Case "4" 'Расчёт десятичных дробей
                Console.Clear()
                CalculatingDecimalNumbers()
            Case "5" 'Калькулятор
                Console.Clear()
                WorkingCalculator()
            Case "6" 'Использование циклов Do While
                Console.Clear()
                UsingDoWhileLoops()
            Case "7" 'Использование циклов For
                Console.Clear()
                UsingForLoops()
            Case "8" 'Условные выражения
                Console.Clear()
                ConditionalStatement()
            Case "9" 'Выражения If/Else
                Console.Clear()
                IfElseStatement() 'Выберите напиток
            Case "50" 'Окно сообщения «О приложении»
                Console.Clear()
                Console.Title = ("Выучи Х за Y минут :: О приложении")
                MsgBox("Это руководство от Брайана Мартина (@BrianMartinn")
                Console.Clear()
                Main()
                Console.ReadLine()

        End Select
    End Sub

    'Один - Я использую эти цифры для того, чтобы было проще
    'соотносить код с системой навигации.

    'Мы используем частные подпрограммы для разделения различных
    'разделов программы.
    Private Sub HelloWorldOutput()
        'Название консольного приложения
        Console.Title = "Вывод данных | Выучи Х за Y минут"
        'Используйте Console.Write ("") или Console.WriteLine ("")
        'для отображения результатов.
        'Затем следует Console.Read () или Console.Readline ()
        'Console.ReadLine () показывает вывод в консоли.
        Console.WriteLine("Hello World")
        Console.ReadLine()
    End Sub

    'Два
    Private Sub HelloWorldInput()
        Console.Title = "Ввод данных | Выучи Х за Y минут"
        'Переменная
        'используется для хранения пользовательских данных.
        'Объявление переменных начинается с Dim и заканчиваются
        'As VariableType (тип переменной).

        'В этом уроке мы хотим узнать ваше имя и заставить программу 
        'реагировать на это.
        Dim username As String
        'Мы используем тип «string», так как ваше имя - это текстовая переменная.
        Console.WriteLine("Привет, как тебя зовут? ") 'Просит ввести имя.
        username = Console.ReadLine() 'Сохраняет имя в переменной username.
        Console.WriteLine("Пирвет, " + username) 'Выводит: «Привет, 'имя'»
        Console.ReadLine() 'Отображает вышеуказанный вывод.

        'Вышеуказанная программа спросит ваше имя и скажет вам привет.
        'Есть и другие типы переменных, такие как целые числа (Integer),
        'мы используем Integer для обработки целых чисел.
    End Sub

    'Три
    Private Sub CalculatingWholeNumbers()
        Console.Title = "Расчёт целых чисел | Выучи Х за Y минут"
        Console.Write("Первое число: ") 'Введите первое целое число: 1, 2, 50, 104 и т.д.
        Dim a As Integer = Console.ReadLine()
        Console.Write("Второе число: ") 'Введите второе целое число.
        Dim b As Integer = Console.ReadLine()
        Dim c As Integer = a + b
        Console.WriteLine(c)
        Console.ReadLine()
        'Приведенная программа сумирует два целых числа
    End Sub

    'Четыре
    Private Sub CalculatingDecimalNumbers()
        Console.Title = "Расчёт десятичных дробей | Выучи Х за Y минут"
        'Мы также должны уметь обрабатывать десятичные дроби.
        'Просто измените тип переменной с Integer на Double.

        'Введите число с плавающей точкой: 1.2, 2.4, 50.1, 104.9 и т.д.
        Console.Write("Первое число: ")
        Dim a As Double = Console.ReadLine
        Console.Write("Второе число: ") 'Введите второе число с плавающей точкой.
        Dim b As Double = Console.ReadLine
        Dim c As Double = a + b
        Console.WriteLine(c)
        Console.ReadLine()
        'Приведенный выше код может сложить две десятичных дроби.
    End Sub

    'Пять
    Private Sub WorkingCalculator()
        Console.Title = "Калькулятор | Выучи Х за Y минут"
        'Но что, если вам нужен калькулятор, который может обрабатывать сложение, 
        'вычитание, умножение и деление?
        'Просто скопируйте и вставьте приведенный код.
        Console.Write("Первое число: ")
        Dim a As Double = Console.ReadLine
        Console.Write("Второе число: ")
        Dim b As Double = Console.ReadLine
        Dim c As Double = a + b
        Dim d As Double = a * b
        Dim e As Double = a - b
        Dim f As Double = a / b

        'С помощью следующего кода мы можем вывести результат сложения,
        'вычитания, умножения и деления, рассчитанный выше, на экран.
        Console.Write(a.ToString() + " + " + b.ToString())
        'Мы хотим, чтобы в начале ответа было 3 пробела, для этого 
        'вы можете использовать метод String.PadLeft (3).
        Console.WriteLine(" = " + c.ToString.PadLeft(3))
        Console.Write(a.ToString() + " * " + b.ToString())
        Console.WriteLine(" = " + d.ToString.PadLeft(3))
        Console.Write(a.ToString() + " - " + b.ToString())
        Console.WriteLine(" = " + e.ToString.PadLeft(3))
        Console.Write(a.ToString() + " / " + b.ToString())
        Console.WriteLine(" = " + f.ToString.PadLeft(3))
        Console.ReadLine()

    End Sub

    'Шесть
    Private Sub UsingDoWhileLoops()
        'Код такой же, как и в предидущей подпрограмме
        'На этот раз мы спрашиваем, хочет ли пользователь продолжить (да или нет?)
        'Мы будем использовать цикл Do While, потому что не знаем,
        'понадобиться ли пользователю калькулятор болше одного раза.
        Console.Title = "Использование циклов Do While | Выучи Х за Y минут"
        Dim answer As String
        'Мы используем тип переменной "String", так как её значение текст.
        Do 'Мы начаем программу с
            Console.Write("Первое число: ")
            Dim a As Double = Console.ReadLine
            Console.Write("Второе число: ")
            Dim b As Double = Console.ReadLine
            Dim c As Double = a + b
            Dim d As Double = a * b
            Dim e As Double = a - b
            Dim f As Double = a / b

            Console.Write(a.ToString() + " + " + b.ToString())
            Console.WriteLine(" = " + c.ToString.PadLeft(3))
            Console.Write(a.ToString() + " * " + b.ToString())
            Console.WriteLine(" = " + d.ToString.PadLeft(3))
            Console.Write(a.ToString() + " - " + b.ToString())
            Console.WriteLine(" = " + e.ToString.PadLeft(3))
            Console.Write(a.ToString() + " / " + b.ToString())
            Console.WriteLine(" = " + f.ToString.PadLeft(3))
            Console.ReadLine()
            'Спросите пользователя, хочет ли он продолжить,
            'в ответе учитывается регистр букв.
            Console.Write("Желаете ли вы продолжить? (да / нет)")
            'Программа берет значение и записывает в переменную answer.
            answer = Console.ReadLine()
            'Когда пользователь вводит «да», программа переходит к Do и снова запускается.
        Loop While answer = "yes"

    End Sub

    'Семь
    Private Sub UsingForLoops()
        'Иногда программу нужно запускать только один раз.
        'В этой программе мы осуществим обратный отсчет от 10.

        Console.Title = "Использование циклов For | Выучи Х за Y минут"
        'Объявите переменные и Step (размер шага, то есть скорость уменьшения,
        'например, -1, -2, -3 и т.д.).
        For i As Integer = 10 To 0 Step -1
            Console.WriteLine(i.ToString) 'Показывает значение счетчика.
        Next i 'Рассчитать новое значение i.
        Console.WriteLine("Поехали")
        Console.ReadLine()
    End Sub

    'Восемь
    Private Sub ConditionalStatement()
        Console.Title = "Условные выражения | Выучи Х за Y минут"
        Dim userName As String
        Console.WriteLine("Привет, как тебя зовут? ") 'Спросите имя пользователя.
        userName = Console.ReadLine() 'Записать имя в переменную userName.
        If userName = "Адам" Then
            Console.WriteLine("Привет, Адам")
            Console.WriteLine("Спасибо за создание этого полезного сайта")
            Console.ReadLine()
        Else
            Console.WriteLine("Привет " + userName)
            Console.WriteLine("Вы заглянули на сайт www.learnxinyminutes.com")
            Console.ReadLine() 'Программа останавливается и выводит вышеуказанный текст.
        End If
    End Sub

    'Девять
    Private Sub IfElseStatement()
        Console.Title = "Выражения If/Else | Выучи Х за Y минут"
        'Иногда важно рассмотреть более двух альтернатив.
        'Иногда некоторые из них лучше других.
        'Когда это произойдет, нам потребуется более одного утверждения «if» (если).
        'Оператор «if» подобен торговому автомату.
        'В котором пользователь пишет код (A1, A2, A3 и т.д.), чтобы выбрать элементы.
        'Все варианты могут быть объединены в одном утверждении «if».

        Dim selection As String 'Объявить переменную для выбора
        Console.WriteLine("Пожалуйста, выберите продукт из нашего прекрасного торгового автомата.")
        Console.WriteLine("A1. для 7Up")
        Console.WriteLine("A2. для Fanta")
        Console.WriteLine("A3. для Dr. Pepper")
        Console.WriteLine("A4. для Diet Coke")

        selection = Console.ReadLine() 'Сохранить выбор пользователя
        If selection = "A1" Then
            Console.WriteLine("7up")
        ElseIf selection = "A2" Then
            Console.WriteLine("Fanta")
        ElseIf selection = "A3" Then
            Console.WriteLine("Dr. Pepper")
        ElseIf selection = "A4" Then
            Console.WriteLine("Diet Coke")
        Else
            Console.WriteLine("Извините, у меня нет " + selection)
        End If
        Console.ReadLine()

    End Sub

End Module

```
