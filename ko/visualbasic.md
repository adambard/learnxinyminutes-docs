---
name: Visual Basic
contributors:
    - ["Brian Martin", "http://brianmartin.biz"]
filename: learnvisualbasic.vb
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

```vbnet
Module Module1

    Sub Main()
        '본격적으로 시작하기 전에 Visual Basic 콘솔 애플리케이션에 대한 간략한 개요입니다.
        '아포스트로피는 주석을 시작합니다.
        'Visual Basic 컴파일러 내에서 이 튜토리얼을 탐색하기 위해
        '내비게이션 시스템을 구성했습니다.
        '이 내비게이션 시스템은 이 튜토리얼을 더 깊이 파고들면서
        '설명되므로 모든 의미를 이해하게 될 것입니다.
        Console.Title = ("Learn X in Y Minutes")
        Console.WriteLine("NAVIGATION") '표시
        Console.WriteLine("")
        Console.ForegroundColor = ConsoleColor.Green
        Console.WriteLine("1. Hello World 출력")
        Console.WriteLine("2. Hello World 입력")
        Console.WriteLine("3. 정수 계산")
        Console.WriteLine("4. 소수 계산")
        Console.WriteLine("5. 작동하는 계산기")
        Console.WriteLine("6. Do While 루프 사용")
        Console.WriteLine("7. For While 루프 사용")
        Console.WriteLine("8. 조건문")
        Console.WriteLine("9. 음료 선택")
        Console.WriteLine("50. 정보")
        Console.WriteLine("위 목록에서 번호를 선택하십시오")
        Dim selection As String = Console.ReadLine
        'Select 문의 "Case"는 선택 사항입니다.
        '예를 들어, "Select Case selection" 대신 "Select selection"도
        '작동합니다.
        Select Case selection
            Case "1" 'HelloWorld 출력
                Console.Clear() '애플리케이션을 지우고 private sub를 엽니다.
                HelloWorldOutput() 'Private Sub 이름 지정, Private Sub 열기
            Case "2" 'Hello 입력
                Console.Clear()
                HelloWorldInput()
            Case "3" '정수 계산
                Console.Clear()
                CalculatingWholeNumbers()
            Case "4" '소수 계산
                Console.Clear()
                CalculatingDecimalNumbers()
            Case "5" '작동하는 계산기
                Console.Clear()
                WorkingCalculator()
            Case "6" 'Do While 루프 사용
                Console.Clear()
                UsingDoWhileLoops()
            Case "7" 'For While 루프 사용
                Console.Clear()
                UsingForLoops()
            Case "8" '조건문
                Console.Clear()
                ConditionalStatement()
            Case "9" 'If/Else 문
                Console.Clear()
                IfElseStatement() '음료 선택
            Case "50" '정보 메시지 상자
                Console.Clear()
                Console.Title = ("Learn X in Y Minutes :: About")
                MsgBox("This tutorial is by Brian Martin (@BrianMartinn")
                Console.Clear()
                Main()
                Console.ReadLine()

        End Select
    End Sub

    '하나 - 나중에 빌드할 때 위의 내비게이션을 돕기 위해 숫자를 사용하고 있습니다.

    '프로그램의 다른 섹션을 분리하기 위해 private sub를 사용합니다.
    Private Sub HelloWorldOutput()
        '콘솔 애플리케이션 제목
        Console.Title = "Hello World Output | Learn X in Y Minutes"
        '출력을 인쇄하려면 Console.Write("") 또는 Console.WriteLine("")을 사용하십시오.
        '그 다음에 Console.Read() 또는 Console.Readline()이 옵니다.
        'Console.ReadLine()은 출력을 콘솔에 인쇄합니다.
        Console.WriteLine("Hello World")
        Console.ReadLine()
    End Sub

    '둘
    Private Sub HelloWorldInput()
        Console.Title = "Hello World YourName | Learn X in Y Minutes"
        '변수
        '사용자가 입력한 데이터는 저장해야 합니다.
        '변수도 Dim으로 시작하고 As VariableType으로 끝납니다.

        '이 튜토리얼에서는 사용자의 이름을 알고 프로그램이
        '말한 내용에 응답하도록 하려고 합니다.
        Dim username As String
        '문자열은 텍스트 기반 변수이므로 문자열을 사용합니다.
        Console.WriteLine("Hello, What is your name? ") '사용자에게 이름을 묻습니다.
        username = Console.ReadLine() '사용자 이름을 저장합니다.
        Console.WriteLine("Hello " + username) '출력은 Hello '사용자 이름'입니다.
        Console.ReadLine() '사용자가 읽을 수 있도록 실행을 일시 중지합니다.

        '위의 내용은 질문을 한 다음 답변을 인쇄합니다.
        '다른 변수에는 Integer가 포함되며 정수에는 Integer를 사용합니다.
    End Sub

    '셋
    Private Sub CalculatingWholeNumbers()
        Console.Title = "Calculating Whole Numbers | Learn X in Y Minutes"
        Console.Write("First number: ") '정수를 입력하십시오. 1, 2, 50, 104 등
        Dim a As Integer = Console.ReadLine()
        Console.Write("Second number: ") '두 번째 정수를 입력하십시오.
        Dim b As Integer = Console.ReadLine()
        Dim c As Integer = a + b
        Console.WriteLine(c)
        Console.ReadLine()
        '위는 간단한 계산기입니다.
    End Sub

    '넷
    Private Sub CalculatingDecimalNumbers()
        Console.Title = "Calculating with Double | Learn X in Y Minutes"
        '물론 소수를 더할 수 있기를 원할 것입니다.
        '따라서 위의 내용을 Integer에서 Double로 변경할 수 있습니다.

        '부동 소수점 숫자를 입력하십시오. 1.2, 2.4, 50.1, 104.9 등
        Console.Write("First number: ")
        Dim a As Double = Console.ReadLine
        Console.Write("Second number: ") '두 번째 부동 소수점 숫자를 입력하십시오.
        Dim b As Double = Console.ReadLine
        Dim c As Double = a + b
        Console.WriteLine(c)
        Console.ReadLine()
        '따라서 위의 프로그램은 1.1 - 2.2를 더할 수 있습니다.
    End Sub

    '다섯
    Private Sub WorkingCalculator()
        Console.Title = "The Working Calculator | Learn X in Y Minutes"
        '그러나 계산기가 빼기, 나누기, 곱하기 및
        '더하기를 원한다면.
        '위의 내용을 다시 복사하여 붙여넣으십시오.
        Console.Write("First number: ")
        Dim a As Double = Console.ReadLine
        Console.Write("Second number: ") '두 번째 부동 소수점 숫자를 입력하십시오.
        Dim b As Double = Console.ReadLine
        Dim c As Double = a + b
        Dim d As Double = a * b
        Dim e As Double = a - b
        Dim f As Double = a / b

        '아래 줄을 추가하여 빼기,
        '곱하기 및 a와 b 값을 나눌 수 있습니다.
        Console.Write(a.ToString() + " + " + b.ToString())
        '답변을 왼쪽으로 3칸 채우고 싶습니다.
        Console.WriteLine(" = " + c.ToString.PadLeft(3))
        Console.Write(a.ToString() + " * " + b.ToString())
        Console.WriteLine(" = " + d.ToString.PadLeft(3))
        Console.Write(a.ToString() + " - " + b.ToString())
        Console.WriteLine(" = " + e.ToString.PadLeft(3))
        Console.Write(a.ToString() + " / " + b.ToString())
        Console.WriteLine(" = " + f.ToString.PadLeft(3))
        Console.ReadLine()

    End Sub

    '여섯
    Private Sub UsingDoWhileLoops()
        '이전 private sub와 마찬가지로
        '이번에는 사용자가 계속할지 묻습니다(예 또는 아니요?).
        '사용자가 프로그램을 두 번 이상 사용하기를 원하는지
        '확실하지 않으므로 Do While 루프를 사용하고 있습니다.
        Console.Title = "UsingDoWhileLoops | Learn X in Y Minutes"
        Dim answer As String '답변이 텍스트이므로 "String" 변수를 사용합니다.
        Do '프로그램을 시작합니다.
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
            '질문을 합니다. 사용자가 계속하기를 원합니까? 안타깝게도
            '대소문자를 구분합니다.
            Console.Write("Would you like to continue? (yes / no) ")
            '프로그램이 변수를 가져와 인쇄하고 다시 시작합니다.
            answer = Console.ReadLine
            '이 경우 변수가 작동하려면 "yes" 명령이 필요합니다.
        Loop While answer = "yes"

    End Sub

    '일곱
    Private Sub UsingForLoops()
        '때로는 프로그램이 한 번만 실행하면 됩니다.
        '이 프로그램에서는 10부터 카운트다운합니다.

        Console.Title = "Using For Loops | Learn X in Y Minutes"
        '변수를 선언하고 Step -1,
        'Step -2, Step -3 등에서 카운트다운할 숫자를 선언합니다.
        For i As Integer = 10 To 0 Step -1
            Console.WriteLine(i.ToString) '카운터 값 인쇄
        Next i '새 값 계산
        Console.WriteLine("Start") '프로그램을 시작합시다!!
        Console.ReadLine() 'POW!! - 아마도 그때 좀 흥분했나 봅니다 :)
    End Sub

    '여덟
    Private Sub ConditionalStatement()
        Console.Title = "Conditional Statements | Learn X in Y Minutes"
        Dim userName As String
        Console.WriteLine("Hello, What is your name? ") '사용자에게 이름을 묻습니다.
        userName = Console.ReadLine() '사용자 이름을 저장합니다.
        If userName = "Adam" Then
            Console.WriteLine("Hello Adam")
            Console.WriteLine("Thanks for creating this useful site")
            Console.ReadLine()
        Else
            Console.WriteLine("Hello " + userName)
            Console.WriteLine("Have you checked out www.learnxinyminutes.com")
            Console.ReadLine() '위 문을 끝내고 인쇄합니다.
        End If
    End Sub

    '아홉
    Private Sub IfElseStatement()
        Console.Title = "If / Else Statement | Learn X in Y Minutes"
        '때로는 두 가지 이상의 대안을 고려하는 것이 중요합니다.
        '때로는 다른 좋은 대안이 몇 가지 있습니다.
        '이 경우 둘 이상의 if 문이 필요합니다.
        'if 문은 사용자가 코드를 입력하는 자동 판매기에 적합합니다.
        'A1, A2, A3 등을 사용하여 항목을 선택합니다.
        '모든 선택은 단일 if 블록으로 결합할 수 있습니다.

        Dim selection As String '선택 변수 선언
        Console.WriteLine("Please select a product form our lovely vending machine.")
        Console.WriteLine("A1. for 7Up")
        Console.WriteLine("A2. for Fanta")
        Console.WriteLine("A3. for Dr. Pepper")
        Console.WriteLine("A4. for Diet Coke")

        selection = Console.ReadLine() '사용자로부터 선택 저장
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
