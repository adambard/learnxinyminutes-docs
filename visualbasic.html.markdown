---
language: Visual Basic
contributors:
    - ["Brian Martin", "http://brianmartin.biz"]
filename: learnvisualbasic.vb
---

```vb
Module Module1

    Sub Main()
        'A Quick Overview of Visual Basic Console Applications before we dive
        'in to the deep end.
        'Apostrophe starts comments.
        'To Navigate this tutorial within the Visual Basic Complier, I've put
        'together a navigation system.
        'This navigation system is explained however as we go deeper into this
        'tutorial, you'll understand what it all means.
        Console.Title = ("Learn X in Y Minutes")
        Console.WriteLine("NAVIGATION") 'Display
        Console.WriteLine("")
        Console.ForegroundColor = ConsoleColor.Green
        Console.WriteLine("1. Hello World Output")
        Console.WriteLine("2. Hello World Input")
        Console.WriteLine("3. Calculating Whole Numbers")
        Console.WriteLine("4. Calculating Decimal Numbers")
        Console.WriteLine("5. Working Calculator")
        Console.WriteLine("6. Using Do While Loops")
        Console.WriteLine("7. Using For While Loops")
        Console.WriteLine("8. Conditional Statements")
        Console.WriteLine("9. Select A Drink")
        Console.WriteLine("50. About")
        Console.WriteLine("Please Choose A Number From The Above List")
        Dim selection As String = Console.ReadLine
        'The "Case" in the Select statement is optional.
        'For example, "Select selection" instead of "Select Case selection"
        'will also work.
        Select Case selection
            Case "1" 'HelloWorld Output
                Console.Clear() 'Clears the application and opens the private sub
                HelloWorldOutput() 'Name Private Sub, Opens Private Sub
            Case "2" 'Hello Input
                Console.Clear()
                HelloWorldInput()
            Case "3" 'Calculating Whole Numbers
                Console.Clear()
                CalculatingWholeNumbers()
            Case "4" 'Calculating Decimal Numbers
                Console.Clear()
                CalculatingDecimalNumbers()
            Case "5" 'Working Calculator
                Console.Clear()
                WorkingCalculator()
            Case "6" 'Using Do While Loops
                Console.Clear()
                UsingDoWhileLoops()
            Case "7" 'Using For While Loops
                Console.Clear()
                UsingForLoops()
            Case "8" 'Conditional Statements
                Console.Clear()
                ConditionalStatement()
            Case "9" 'If/Else Statement
                Console.Clear()
                IfElseStatement() 'Select a drink
            Case "50" 'About msg box
                Console.Clear()
                Console.Title = ("Learn X in Y Minutes :: About")
                MsgBox("This tutorial is by Brian Martin (@BrianMartinn")
                Console.Clear()
                Main()
                Console.ReadLine()

        End Select
    End Sub

    'One - I'm using numbers to help with the above navigation when I come back
    'later to build it.

    'We use private subs to separate different sections of the program.
    Private Sub HelloWorldOutput()
        'Title of Console Application
        Console.Title = "Hello World Output | Learn X in Y Minutes"
        'Use Console.Write("") or Console.WriteLine("") to print outputs.
        'Followed by Console.Read() alternatively Console.Readline()
        'Console.ReadLine() prints the output to the console.
        Console.WriteLine("Hello World")
        Console.ReadLine()
    End Sub

    'Two
    Private Sub HelloWorldInput()
        Console.Title = "Hello World YourName | Learn X in Y Minutes"
        'Variables
        'Data entered by a user needs to be stored.
        'Variables also start with a Dim and end with an As VariableType.

        'In this tutorial, we want to know what your name, and make the program
        'respond to what is said.
        Dim username As String
        'We use string as string is a text based variable.
        Console.WriteLine("Hello, What is your name? ") 'Ask the user their name.
        username = Console.ReadLine() 'Stores the users name.
        Console.WriteLine("Hello " + username) 'Output is Hello 'Their name'
        Console.ReadLine() 'Outputs the above.
        'The above will ask you a question followed by printing your answer.
        'Other variables include Integer and we use Integer for whole numbers.
    End Sub

    'Three
    Private Sub CalculatingWholeNumbers()
        Console.Title = "Calculating Whole Numbers | Learn X in Y Minutes"
        Console.Write("First number: ") 'Enter a whole number, 1, 2, 50, 104, etc
        Dim a As Integer = Console.ReadLine()
        Console.Write("Second number: ") 'Enter second whole number.
        Dim b As Integer = Console.ReadLine()
        Dim c As Integer = a + b
        Console.WriteLine(c)
        Console.ReadLine()
        'The above is a simple calculator
    End Sub

    'Four
    Private Sub CalculatingDecimalNumbers()
        Console.Title = "Calculating with Double | Learn X in Y Minutes"
        'Of course we would like to be able to add up decimals.
        'Therefore we could change the above from Integer to Double.

        'Enter a floating-point number, 1.2, 2.4, 50.1, 104.9, etc
        Console.Write("First number: ")
        Dim a As Double = Console.ReadLine
        Console.Write("Second number: ") 'Enter second floating-point number.
        Dim b As Double = Console.ReadLine
        Dim c As Double = a + b
        Console.WriteLine(c)
        Console.ReadLine()
        'Therefore the above program can add up 1.1 - 2.2
    End Sub

    'Five
    Private Sub WorkingCalculator()
        Console.Title = "The Working Calculator| Learn X in Y Minutes"
        'However if you'd like the calculator to subtract, divide, multiple and
        'add up.
        'Copy and paste the above again.
        Console.Write("First number: ")
        Dim a As Double = Console.ReadLine
        Console.Write("Second number: ") 'Enter second floating-point number.
        Dim b As Double = Console.ReadLine
        Dim c As Double = a + b
        Dim d As Double = a * b
        Dim e As Double = a - b
        Dim f As Double = a / b

        'By adding the below lines we are able to calculate the subtract,
        'multiply as well as divide the a and b values
        Console.Write(a.ToString() + " + " + b.ToString())
        'We want to pad the answers to the left by 3 spaces.
        Console.WriteLine(" = " + c.ToString.PadLeft(3))
        Console.Write(a.ToString() + " * " + b.ToString())
        Console.WriteLine(" = " + d.ToString.PadLeft(3))
        Console.Write(a.ToString() + " - " + b.ToString())
        Console.WriteLine(" = " + e.ToString.PadLeft(3))
        Console.Write(a.ToString() + " / " + b.ToString())
        Console.WriteLine(" = " + f.ToString.PadLeft(3))
        Console.ReadLine()

    End Sub

    'Six
    Private Sub UsingDoWhileLoops()
        'Just as the previous private sub
        'This Time We Ask If The User Wishes To Continue (Yes or No?)
        'We're using Do While Loop as we're unsure if the user wants to use the
        'program more than once.
        Console.Title = "UsingDoWhileLoops | Learn X in Y Minutes"
        Dim answer As String 'We use the variable "String" as the answer is text
        Do 'We start the program with
            Console.Write("First number: ")
            Dim a As Double = Console.ReadLine
            Console.Write("Second number: ")
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
            'Ask the question, does the user wish to continue? Unfortunately it
            'is case sensitive.
            Console.Write("Would you like to continue? (yes / no) ")
            'The program grabs the variable and prints and starts again.
            answer = Console.ReadLine
            'The command for the variable to work would be in this case "yes"
        Loop While answer = "yes"

    End Sub

    'Seven
    Private Sub UsingForLoops()
        'Sometimes the program only needs to run once.
        'In this program we'll be counting down from 10.

        Console.Title = "Using For Loops | Learn X in Y Minutes"
        'Declare Variable and what number it should count down in Step -1,
        'Step -2, Step -3, etc.
        For i As Integer = 10 To 0 Step -1
            Console.WriteLine(i.ToString) 'Print the value of the counter
        Next i 'Calculate new value
        Console.WriteLine("Start") 'Lets start the program baby!!
        Console.ReadLine() 'POW!! - Perhaps I got a little excited then :)
    End Sub

    'Eight
    Private Sub ConditionalStatement()
        Console.Title = "Conditional Statements | Learn X in Y Minutes"
        Dim userName As String
        Console.WriteLine("Hello, What is your name? ") 'Ask the user their name.
        userName = Console.ReadLine() 'Stores the users name.
        If userName = "Adam" Then
            Console.WriteLine("Hello Adam")
            Console.WriteLine("Thanks for creating this useful site")
            Console.ReadLine()
        Else
            Console.WriteLine("Hello " + userName)
            Console.WriteLine("Have you checked out www.learnxinyminutes.com")
            Console.ReadLine() 'Ends and prints the above statement.
        End If
    End Sub

    'Nine
    Private Sub IfElseStatement()
        Console.Title = "If / Else Statement | Learn X in Y Minutes"
        'Sometimes it is important to consider more than two alternatives.
        'Sometimes there are a good few others.
        'When this is the case, more than one if statement would be required.
        'An if statement is great for vending machines. Where the user enters a code.
        'A1, A2, A3, etc to select an item.
        'All choices can be combined into a single if block.

        Dim selection As String 'Declare a variable for selection
        Console.WriteLine("Please select a product form our lovely vending machine.")
        Console.WriteLine("A1. for 7Up")
        Console.WriteLine("A2. for Fanta")
        Console.WriteLine("A3. for Dr. Pepper")
        Console.WriteLine("A4. for Diet Coke")

        selection = Console.ReadLine() 'Store a selection from the user
        If selection = "A1" Then
            Console.WriteLine("7up")
        ElseIf selection = "A2" Then
            Console.WriteLine("fanta")
        ElseIf selection = "A3" Then
            Console.WriteLine("dr. pepper")
        ElseIf selection = "A4" Then
            Console.WriteLine("diet coke")
        Else
            Console.WriteLine("Sorry, I don't have any " + selection)
        End If
        Console.ReadLine()

    End Sub

End Module

```
