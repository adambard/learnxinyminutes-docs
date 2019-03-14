---
language: Visual Basic
contributors:
    - ["Brian Martin", "http://brianmartin.biz"]
translators:
    - ["Adolfo Jayme Barrientos", "https://github.com/fitojb"]
filename: learnvisualbasic-es.vb
lang: es-es
---

```
Module Module1

    Sub Main()
        ' Un vistazo rápido a las aplicaciones de consola de Visual Basic antes
        ' de que profundicemos en el tema.
        ' El apóstrofo inicia una línea de comentario.
        ' Para explorar este tutorial dentro del Compilador de Visual Basic,
        ' he creado un sistema de navegación.
        ' Dicho sistema se explicará a medida que avancemos en este
        ' tutorial; gradualmente entenderás lo que significa todo.
        Console.Title = ("Aprende X en Y minutos")
        Console.WriteLine("NAVEGACIÓN") 'Mostrar 
        Console.WriteLine("")
        Console.ForegroundColor = ConsoleColor.Green
        Console.WriteLine("1. Salida «Hola, mundo»")
        Console.WriteLine("2. Entrada «Hola, mundo»")
        Console.WriteLine("3. Calcular números enteros")
        Console.WriteLine("4. Calcular números decimales")
        Console.WriteLine("5. Una calculadora funcional")
        Console.WriteLine("6. Uso de bucles «Do While»")
        Console.WriteLine("7. Uso de bucles «For While»")
        Console.WriteLine("8. Declaraciones condicionales")
        Console.WriteLine("9. Selecciona una bebida")
        Console.WriteLine("50. Acerca de")
        Console.WriteLine("Elige un número de la lista anterior")
        Dim selection As String = Console.ReadLine
        Select Case selection
            Case "1" 'Salida «hola, mundo»
                Console.Clear() 'Limpia la consola y abre la subrutina privada
                SalidaHolaMundo() 'Abre la subrutina privada nombrada
            Case "2" 'Entrada «hola, mundo»
                Console.Clear()
                EntradaHolaMundo()
            Case "3" 'Calcular números enteros 
                Console.Clear()
                CalcularNumerosEnteros()
            Case "4" 'Calcular números decimales
                Console.Clear()
                CalcularNumerosDecimales()
            Case "5" 'Una calculadora funcional
                Console.Clear()
                CalculadoraFuncional()
            Case "6" 'Uso de bucles «Do While»
                Console.Clear()
                UsoBuclesDoWhile()
            Case "7" 'Uso de bucles «For While»
                Console.Clear()
                UsoBuclesFor()
            Case "8" 'Declaraciones condicionales
                Console.Clear()
                DeclaracionCondicional()
            Case "9" 'Declaración «If/Else»
                Console.Clear()
                DeclaracionIfElse() 'Selecciona una bebida
            Case "50" 'Cuadro de mensaje «Acerca de»
                Console.Clear()
                Console.Title = ("Aprende X en Y minutos :: Acerca de")
                MsgBox("Tutorial escrito por Brian Martin (@BrianMartinn")
                Console.Clear()
                Main()
                Console.ReadLine()

        End Select
    End Sub

    'Uno - He usado números para guiarme por el sistema de navegación anterior
    'cuando regrese posteriormente a implementarlo.

    'Usamos subrutinas privadas para separar distintas secciones del programa. 
    Private Sub SalidaHolaMundo()
        'Título de la aplicación de consola
        Console.Title = "Salida «Hola, mundo» | Aprende X en Y minutos"
        'Usa Console.Write("") o Console.WriteLine("") para mostrar salidas.
        'Seguido por Console.Read(), o bien, Console.Readline()
        'Console.ReadLine() muestra la salida en la consola.
        Console.WriteLine("Hola, mundo")
        Console.ReadLine()
    End Sub

    'Dos
    Private Sub EntradaHolaMundo()
        Console.Title = "«Hola, mundo, soy...» | Aprende X en Y minutos"
        ' Variables
        ' Los datos que introduzca un usuario deben almacenarse.
        ' Las variables también empiezan por Dim y terminan por As VariableType.

        ' En este tutorial queremos conocer tu nombre y hacer que el programa
        ' responda a este.
        Dim nombredeusuario As String
        'Usamos «string» porque es una variable basada en texto.
        Console.WriteLine("Hola, ¿cómo te llamas? ") 'Preguntar nombre de usuario.
        nombredeusuario = Console.ReadLine() 'Almacenar nombre del usuario.
        Console.WriteLine("Hola, " + nombredeusuario) 'La salida es Hola, nombre
        Console.ReadLine() 'Muestra lo anterior.
        'El código anterior te hará una pregunta y mostrará la respuesta.
        'Entre otras variables está Integer, la cual usaremos para números enteros.
    End Sub

    'Tres
    Private Sub CalcularNumerosEnteros()
        Console.Title = "Calcular números enteros | Aprende X en Y minutos"
        Console.Write("Primer número: ") 'Escribe un núm. entero, 1, 2, 104, etc
        Dim a As Integer = Console.ReadLine()
        Console.Write("Segundo número: ") 'Escribe otro número entero.
        Dim b As Integer = Console.ReadLine()
        Dim c As Integer = a + b
        Console.WriteLine(c)
        Console.ReadLine()
        'Lo anterior es una calculadora sencilla
    End Sub

    'Cuatro
    Private Sub CalcularNumerosDecimales()
        Console.Title = "Calcular con tipo doble | Aprende X en Y minutos"
        'Por supuesto, nos gustaría sumar decimales.
        'Por ello podríamos cambiar del tipo Integer al Double.

        'Escribe un número fraccionario, 1.2, 2.4, 50.1, 104.9 etc
        Console.Write("Primer número: ")
        Dim a As Double = Console.ReadLine
        Console.Write("Segundo número: ") 'Escribe el segundo número.
        Dim b As Double = Console.ReadLine
        Dim c As Double = a + b
        Console.WriteLine(c)
        Console.ReadLine()
        'Este programa puede sumar 1.1 y 2.2
    End Sub

    'Cinco
    Private Sub CalculadoraFuncional()
        Console.Title = "La calculadora funcional | Aprende X en Y minutos"
        'Pero si quieres que la calculadora reste, divida, multiplique y
        'sume.
        'Copia y pega lo anterior.
        Console.Write("Primer número: ")
        Dim a As Double = Console.ReadLine
        Console.Write("Segundo número: ")
        Dim b As Integer = Console.ReadLine
        Dim c As Integer = a + b
        Dim d As Integer = a * b
        Dim e As Integer = a - b
        Dim f As Integer = a / b

        'Mediante las líneas siguientes podremos restar,
        'multiplicar y dividir los valores a y b
        Console.Write(a.ToString() + " + " + b.ToString())
        'Queremos dar un margen izquierdo de 3 espacios a los resultados.
        Console.WriteLine(" = " + c.ToString.PadLeft(3))
        Console.Write(a.ToString() + " * " + b.ToString())
        Console.WriteLine(" = " + d.ToString.PadLeft(3))
        Console.Write(a.ToString() + " - " + b.ToString())
        Console.WriteLine(" = " + e.ToString.PadLeft(3))
        Console.Write(a.ToString() + " / " + b.ToString())
        Console.WriteLine(" = " + f.ToString.PadLeft(3))
        Console.ReadLine()

    End Sub

    'Seis
    Private Sub UsoBuclesDoWhile()
        'Igual que la subrutina privada anterior
        'Esta vez preguntaremos al usuario si quiere continuar (¿sí o no?)
        'Usamos el bucle Do While porque no sabemos si el usuario quiere
        'usar el programa más de una vez.
        Console.Title = "Uso de bucles «Do While» | Aprende X en Y minutos"
        Dim respuesta As String 'Usamos la variable «String» porque la resp. es texto
        Do 'Comenzamos el programa con
            Console.Write("Primer número: ")
            Dim a As Double = Console.ReadLine
            Console.Write("Segundo número: ")
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
            Console.WriteLine(" = " + f.ToString.PadLeft(3))
            Console.ReadLine()
            'Preguntar si el usuario quiere continuar. Desafortunadamente,
            'distingue entre mayúsculas y minúsculas.
            Console.Write("¿Quieres continuar? (s / n)")
            'El programa toma la variable, la muestra y comienza de nuevo.
            respuesta = Console.ReadLine
        'La orden que hará funcionar esta variable es en este caso «s»
        Loop While respuesta = "s"

    End Sub

    'Siete
    Private Sub UsoBuclesFor()
        'A veces el programa debe ejecutarse solo una vez.
        'En este programa contaremos a partir de 10.

        Console.Title = "Uso de bucles «For» | Aprende X en Y minutos"
        'Declarar Variable y desde qué número debe contar en Step -1,
        'Step -2, Step -3, etc.
        For i As Integer = 10 To 0 Step -1 
            Console.WriteLine(i.ToString) 'Muestra el valor del contador
        Next i 'Calcular el valor nuevo
        Console.WriteLine("Iniciar") '¡¡Comencemos el programa, nene!!
        Console.ReadLine() '¡¡ZAS!! - Quizá me he emocionado bastante :)
    End Sub

    'Ocho
    Private Sub DeclaracionCondicional()
        Console.Title = "Declaraciones condicionales | Aprende X en Y minutos"
        Dim nombredeUsuario As String = Console.ReadLine
        Console.WriteLine("Hola, ¿cómo te llamas? ") 'Preguntar nombre de usuario.
        nombredeUsuario = Console.ReadLine() 'Almacena el nombre de usuario.
        If nombredeUsuario = "Adam" Then
            Console.WriteLine("Hola, Adam")
            Console.WriteLine("Gracias por crear este útil sitio web")
            Console.ReadLine()
        Else
            Console.WriteLine("Hola, " + nombredeUsuario)
            Console.WriteLine("¿Has visitado www.learnxinyminutes.com?")
            Console.ReadLine() 'Termina y muestra la declaración anterior.
        End If
    End Sub

    'Nueve
    Private Sub DeclaracionIfElse()
    Console.Title = "Declaración «If / Else» | Aprende X en Y minutos"
        'A veces es importante considerar más de dos alternativas.
        'A veces, algunas de estas son mejores.
        'Cuando esto sucede, necesitaríamos más de una declaración «if».
        'Una declaración «if» es adecuada para máquinas expendedoras.
        'En las que el usuario escribe un código (A1, A2, A3) para elegir.
        'Pueden combinarse todas las elecciones en una sola declaración «if».

        Dim seleccion As String = Console.ReadLine 'Valor de la selección
            Console.WriteLine("A1. para 7Up")
            Console.WriteLine("A2. para Fanta")
            Console.WriteLine("A3. para Dr. Pepper")
            Console.WriteLine("A4. para Coca-Cola")
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
                Console.WriteLine("coca-cola")
                Console.ReadLine()
            Else
                Console.WriteLine("Selecciona un producto")
                Console.ReadLine()
            End If

    End Sub

End Module

```

## Referencias

Aprendí Visual Basic en la aplicación de consola. Esta me permitió entender los principios de la programación para, posteriormente, aprender otros lenguajes con facilidad.

He creado un <a href="http://www.vbbootcamp.co.uk/" Title="Tutorial de Visual Basic">tutorial de Visual Basic</a> más exhaustivo para quienes quieran saber más.

Toda la sintaxis es válida. Copia el código y pégalo en el compilador de Visual Basic y ejecuta (F5) el programa. 
