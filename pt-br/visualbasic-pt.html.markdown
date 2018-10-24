---
language: Visual Basic
contributors:
    - ["Brian Martin", "http://brianmartin.biz"]
translators:
    - ["AdrianoJP", "https://github.com/AdrianoJP"]
lang: pt-br
filename: learnvisualbasic-pt.vb
---

```
Module Module1

module Module1

    Sub Main ()
        ' Uma visão geral de console de aplicativos do Visual Basic antes de 
        ' mergulharmos mais profundamente na linguagem.
        ' Aspas simples começam comentários.
        ' Para navegar neste tutorial dentro do compilador do Visual Basic, 
        ' eu criei um sistema de navegação.
        ' Este sistema de navegação vai ser explicado conforme avançarmos no
        ' tutorial, e você vai entender o que isso significa.
        Console.Title = (" Saiba X em Y Minutes" )
        Console.WriteLine ( "NAVEGAÇÃO" ) 'Mostrar
        Console.ForegroundColor = ConsoleColor.Green
        Console.WriteLine ("1. Saída Olá Mundo" )
        Console.WriteLine ("2. Entrada Olá Mundo" )
        Console.WriteLine ("3. Cálculando números inteiros " )
        Console.WriteLine ("4. Calculando números decimais " )
        Console.WriteLine ("5 . Calculadora de Trabalho " )
        Console.WriteLine ("6. Usando Do While Loops " )
        Console.WriteLine ("7. Usando Para While Loops " )
        Console.WriteLine ("8 . Declarações condicionais " )
        Console.WriteLine ("9. Selecione uma bebida" )
        Console.WriteLine ("50 . About" )
        Console.WriteLine ("Por favor, escolha um número da lista acima " )
        Seleção Dim As String = Console.ReadLine
        Select A seleção dos casos
            Caso "1" 'Output HelloWorld
                Console.clear () ' Limpa a aplicação e abre o sub privado
                HelloWorldOutput () ' Nome Private Sub, Abre Private Sub
            Caso "2" 'Olá entrada
                Console.clear ( )
                HelloWorldInput ( )
            Caso de "3" 'Calculando Números Inteiros
                Console.clear ( )
                CalculatingWholeNumbers ( )
            Caso "4" ' Números decimais Calculting
                Console.clear ( )
                CalculatingDecimalNumbers ( )
            Caso "5" ' Calcculator Trabalho
                Console.clear ( )
                WorkingCalculator ( )
            Caso "6" 'Usando Do While Loops
                Console.clear ( )
                UsingDoWhileLoops ( )
            Caso de "7" 'Usando pois enquanto Loops
                Console.clear ( )
                UsingForLoops ( )
            Caso "8" ' Instruções condicionais
                Console.clear ( )
                ConditionalStatement ( )
            Caso "9" "Declaração If / Else
                Console.clear ( )
                IfElseStatement () ' Selecione uma bebida
            Caso "50" 'Quem caixa de msg
                Console.clear ( )
                Console.Title = (" Saiba X em Y Minutos :: Quem " )
                MsgBox (" Este tutorial é de Brian Martin ( @ BrianMartinn " )
                Console.clear ( )
                Main ()
                Console.ReadLine ()

        End Select
    End Sub

    ' Um - Eu estou usando números para ajudar com a navegação acima quando eu voltar
    ' depois de construí-lo .

    " Nós usamos subs privadas para separar diferentes seções do programa.
    Private Sub HelloWorldOutput ()
        ' Título de aplicativo do console
        Console.Title = " Olá Mundo Ouput | Saiba X em Y Minutes"
        'Use Console.Write ("") ou Console.WriteLine ("") para imprimir saídas.
        " Seguido por Console.Read () alternativamente Console.ReadLine ()
        ' Console.ReadLine () imprime a saída para o console.
        Console.WriteLine ( "Olá Mundo" )
        Console.ReadLine ()
    End Sub

    ' Dois
    Private Sub HelloWorldInput ()
        Console.Title = " Olá Mundo YourName | Saiba X em Y Minutes"
        ' Variáveis
        'Os dados inseridos por um usuário precisam ser armazenados.
        ' As variáveis ​​também começar com um Dim e terminar com um Como VariableType .

        ' Neste tutorial, nós queremos saber qual é o seu nome, e faça o programa
        ' Responder ao que é dito.
        Nome de usuário Dim As String
        " Nós usamos string como string é uma variável de texto baseado .
        Console.WriteLine (" Olá, Qual é o seu nome? ") ' Peça ao usuário seu nome.
        username = Console.ReadLine () ' armazena o nome do usuário.
        Console.WriteLine (" Olá " + username) ' A saída é "Olá < seu nome >".
        Console.ReadLine () ' Outsputs acima.
        ' O código acima irá lhe fazer uma pergunta seguiu imprimindo sua resposta.
        " Outras variáveis ​​incluem Integer e usamos inteiro para números inteiros.
    End Sub

    "Três
    Sub CalculatingWholeNumbers particulares ()
        Console.Title = " Cálculo de Números Inteiros | Saiba X em Y Minutes"
        Console.Write ("Primeiro número:") 'Digite um número inteiro, 1, 2, 50, 104 ect
        Dim a As Integer = Console.ReadLine ()
        Console.Write ("Segundo número:") 'Enter segundo número inteiro.
        Dim b As Integer = Console.ReadLine ()
        Dim c As Integer = a + b
        Console.WriteLine ( c)
        Console.ReadLine ()
        " O texto acima é uma calculadora simples
    End Sub

    'Quatro
    Sub CalculatingDecimalNumbers particulares ()
        Console.Title = " Calculando com duplo | Saiba X em Y Minutes"
        ' Claro que gostaria de ser capaz de somar decimais .
        " Por isso, poderia mudar o acima de Integer para Double.

        " Digite um número inteiro , 1,2 , 2,4 , 50,1 , 104,9 ect
        Console.Write ("Primeiro número:")
        Dim a As Double = Console.ReadLine
        Console.Write ("Segundo número:") 'Enter segundo número inteiro.
        Dim b As Double = Console.ReadLine
        Dim c As Double = a + b
        Console.WriteLine ( c)
        Console.ReadLine ()
        " Portanto, o programa acima pode adicionar até 1,1-2,2
    End Sub

    ' Cinco
    Private Sub WorkingCalculator ()
        Console.Title = " A Calculadora de Trabalho | Saiba X em Y Minutes"
        " No entanto, se você gostaria que a calculadora para subtrair, dividir , múltiplos e
        ' somar.
        ' Copie e cole o código acima novamente .
        Console.Write ("Primeiro número:")
        Dim a As Double = Console.ReadLine
        Console.Write ("Segundo número:") 'Enter segundo número inteiro.
        Dim b As Integer = Console.ReadLine
        Dim c As Integer = a + b
        Dim d As Integer = a * b
        Dim e As Integer = a - b
        Dim f As Integer = a / b

        " Ao adicionar as linhas abaixo , somos capazes de calcular a subtração ,
        ' multply bem como dividir os valores de a e b
        Console.Gravar ( a.ToString ( ) + " + " + b.ToString ( ) )
        'Queremos pad as respostas para a esquerda por três espaços.
        Console.WriteLine (" =" + c.ToString.PadLeft (3) )
        Console.Gravar ( a.ToString ( ) + " * " + b.ToString ( ) )
        Console.WriteLine (" =" + d.ToString.PadLeft (3) )
        Console.Gravar ( a.ToString ( ) + " - " + b.ToString ( ) )
        Console.WriteLine (" =" + e.ToString.PadLeft (3) )
        Console.Write ( a.ToString () + "/" + b.ToString ())
        Console.WriteLine (" =" + e.ToString.PadLeft (3) )
        Console.ReadLine ()

    End Sub

    ' Seis
    Sub UsingDoWhileLoops particulares ()
        ' Assim como o sub privado anterior
        ' Desta vez, perguntar se o usuário deseja continuar ( Sim ou Não ? )
        ' Estamos usando Do While Loop , como não temos certeza se o usuário quer usar o
        'programa mais de uma vez .
        Console.Title = " UsingDoWhileLoops | Saiba X em Y Minutes"
        Dim resposta As String ' Nós usamos a variável " String" como a resposta é um texto
        Do ' Começamos o programa com
            Console.Write ("Primeiro número:")
            Dim a As Double = Console.ReadLine
            Console.Write ("Segundo número:")
            Dim b As Integer = Console.ReadLine
            Dim c As Integer = a + b
            Dim d As Integer = a * b
            Dim e As Integer = a - b
            Dim f As Integer = a / b

            Console.Gravar ( a.ToString ( ) + " + " + b.ToString ( ) )
            Console.WriteLine (" =" + c.ToString.PadLeft (3) )
            Console.Gravar ( a.ToString ( ) + " * " + b.ToString ( ) )
            Console.WriteLine (" =" + d.ToString.PadLeft (3) )
            Console.Gravar ( a.ToString ( ) + " - " + b.ToString ( ) )
            Console.WriteLine (" =" + e.ToString.PadLeft (3) )
            Console.Write ( a.ToString () + "/" + b.ToString ())
            Console.WriteLine (" =" + e.ToString.PadLeft (3) )
            Console.ReadLine ()
            ' Faça a pergunta , se o usuário deseja continuar? Infelizmente,
            "é sensível a maiúsculas.
            Console.Write ( "Deseja continuar? (Sim / não )")
            " O programa pega a variável e imprime e começa de novo.
            answer = Console.ReadLine
        " O comando para a variável para trabalhar seria , neste caso, " sim "
        Loop While resposta = "yes"

    End Sub

    ' Sete
    Sub UsingForLoops particulares ()
        ' Às vezes, o programa só precisa ser executado uma vez.
        " Neste programa vamos estar em contagem regressiva a partir de 10.

        Console.Title = " Usando Para Loops | Saiba X em Y Minutes"
        'Declare variável e qual o número que deve contar para baixo na etapa 1,
        ' Passo -2, -3 Passo ect.
        Para i As Integer = 10 para 0 passo -1
            Console.WriteLine ( i.ToString ) ' Imprime o valor do contador
        Next i ' Calcular novo valor
        Console.WriteLine ( "Start ") ' Vamos começar o bebê programa !
        Console.ReadLine () ' POW ! - Talvez eu fiquei um pouco animado, então :)
    End Sub

    ' Oito
    Private Sub ConditionalStatement ()
        Console.Title = " Instruções condicionais | Saiba X em Y Minutes"
        UserName Dim As String = Console.ReadLine
        Console.WriteLine (" Olá, Qual é o seu nome? ") ' Peça ao usuário seu nome.
        username = Console.ReadLine () ' armazena o nome usuários.
        Se userName = " Adam " Então
            Console.WriteLine (" Olá Adam " )
            Console.WriteLine (" Obrigado por criar este site útil " )
            Console.ReadLine ()
        outro
            Console.WriteLine (" Olá " + nome do usuário)
            Console.WriteLine (" Você check-out www.learnxinyminutes.com " )
            Console.ReadLine () ' Fins e imprime a declaração acima .
        End If
    End Sub

    'Nove
    Private Sub IfElseStatement ()
    Console.Title = "Se Declaração / Else | Saiba X em Y Minutes"
        'Às vezes é importante ter em conta mais de duas alternativas.
        'Às vezes, há um bom número de outros.
        'Quando este for o caso , mais do que uma if seria necessária .
        'Uma instrução if é ótimo para máquinas de venda automática . Quando o usuário digita um código.
        ' A1 , A2, A3 , ect para selecionar um item.
        'Todas as opções podem ser combinadas em uma única if.

        Seleção Dim Valor String = Console.ReadLine ' para a seleção
            Console.WriteLine (" A1. Para Soda " )
            Console.WriteLine (" A2. Para Fanta " )
            Console.WriteLine (" A3 . Para Guaraná" )
            Console.WriteLine (" A4. Para Coca Diet" )
            Console.ReadLine ()
            Se a seleção = "A1" Então
                Console.WriteLine (" soda " )
                Console.ReadLine ()
            Seleção ElseIf = " A2 " Então
                Console.WriteLine (" fanta " )
                Console.ReadLine ()
            Seleção ElseIf = " A3 " Então
                Console.WriteLine ( "guaraná" )
                Console.ReadLine ()
            Seleção ElseIf = " A4 " Então
                Console.WriteLine ( "coca diet" )
                Console.ReadLine ()
            outro
                Console.WriteLine (" Por favor seleccione um produto" )
                Console.ReadLine ()
            End If

    End Sub

End Module

```

##Referências

Aprendi Visual Basic no aplicativo de console. Isso me permitiu entender os princípios da programação de computador para continuar a aprender outras linguagens de programação facilmente.

Eu criei um tutorial mais aprofundado do <a href="http://www.vbbootcamp.co.uk/" Title="Visual Basic Tutorial">Visual Basic</a> para aqueles que gostariam de saber mais.

Toda a sintaxe deste tutorial é válida. Copie e cole o código no compilador do Visual Basic e execute (com o F5) o programa.
