---
language: Visual Basic
contributors:
    - ["Brian Martin", "http://brianmartin.biz"]
translators:
    - ["Enno Nagel", "https://github.com/konfekt"]
filename: learnvisualbasic-de.vb
lang: de-de
---

```vbnet
Module Modul1

Sub Main()
  ' Ein kurzer Blick auf Visual Basic-Konsolenanwendungen
  ' bevor wir tiefer in das Thema eintauchen.
  ' Das Hochkomma leitet eine Kommentarzeile ein.
  ' Um dieses Tutorial innerhalb des Visual Basic Compilers zu erkunden,
  ' habe ich ein Navigationssystem erstellt.
  ' Dieses System wird im weiteren Verlauf des Tutorials erklärt;
  ' Sie werden nach und nach verstehen, was das alles bedeutet.
  Console.Title = ("Lerne X in Y Minuten")
  Console.WriteLine ("NAVIGATION") 'Anzeige
  Console.WriteLine ("")
  Console.ForegroundColor = ConsoleColor.Green
  Console.WriteLine ("1. Ausgabe von 'Hallo, Welt'")
  Console.WriteLine ("2. Eingabe 'Hallo, Welt'")
  Console.WriteLine ("3. ganze Zahlen berechnen")
  Console.WriteLine ("4. Berechne Dezimalzahlen")
  Console.WriteLine ("5. ein funktionaler Taschenrechner")
  Console.WriteLine ("6. 'Do While'-Schleifen verwenden")
  Console.WriteLine ("7. Verwendung von 'For While'-Schleifen")
  Console.WriteLine ("8. Bedingte Anweisungen")
  Console.WriteLine ("9. Ein Getränk auswählen")
  Console.WriteLine ("50. Über")
  Console.WriteLine ("Wählen Sie eine Zahl aus der obigen Liste")
  Dim selection As String = Console.Readline()
  Select Case auswahl
    Case "1" 'Ausgabe "Hallo, Welt"
      Console.Clear() 'Löscht die Konsole und öffnet die private Subroutine
      AusgabeHalloWelt() 'Öffnet die genannte private Subroutine
    Case "2" 'Eingabe "hallo, Welt"
      Console.Clear()
      EingabeHalloWelt()
    Case "3" 'Berechne ganze Zahlen
      Console.Clear()
      BerechneGanzeZahlen()
    Case "4" 'Dezimalzahlen berechnen
      Console.Clear()
      BerechneDezimalZahlen()
    Case "5" 'Ein funktionaler Taschenrechner
      Console.Clear()
      Taschenrechner()
    Case "6" 'Verwendung von "Do While"-Schleifen
      Console.Clear()
      WhileSchleife()
    Case "7" 'Verwendung von "For While"-Schleifen
      Console.Clear()
      ForSchleife()
    Case "8" 'Bedingte Anweisungen
      Console.Clear()
      BedingteAnweisung()
    Case "9" 'If/Else-Anweisung
      Console.Clear()
      IfElseAnweisung() 'Ein Getränk auswählen
    Case "50" '"Über" Infobox
      Console.Clear()
      Console.Title = ("Lernen Sie X in Y Minuten :: Über")
      MsgBox ("Tutorial geschrieben von Brian Martin (@BrianMartinn)")
      Console.Clear()
      Main()
      Console.ReadLine()

  End Select
End Sub

'Eins - Ich habe Zahlen verwendet, um mich durch das obige Navigationssystem zu
'führen auf das ich später zurückkomme, um es zu implementieren.

'wir verwenden private Unterprogramme, um verschiedene Abschnitte des Programms
'zu trennen.
Private Sub AusgabeHalloWelt()
  'Titel der Konsolenanwendung
  Console.Title = "Ausgabe 'Hallo, Welt' | Lerne X in Y Minuten"
  'Verwenden Sie Console.Write("") oder Console.WriteLine(""), um die Ausgabe
  'anzuzeigen, gefolgt von Console.Read(), oder Console.Readline()
  'Console.ReadLine() zeigt die Ausgabe auf der Konsole an.
  Console.WriteLine ("Hallo, Welt")
  Console.ReadLine()
End Sub

'Zwei
Private Sub EingabeHalloWelt()
  Console.Title = "Hallo, Welt, ich bin.. | Lerne X in Y Minuten"
  'Variablen
  'Vom Benutzer eingegebene Daten müssen gespeichert werden.
  'Variablen beginnen ebenfalls mit Dim und enden mit As VariableType.

  'In diesem Lernprogramm wollen wir Ihren Namen wissen und das Programm
  'auf ihn antworten.
  Dim nutzername As String
  ' Wir verwenden "String", weil es sich um eine textbasierte Variable handelt.
  Console.WriteLine ("Hallo, wie ist Ihr Name?") 'Frage nach dem Benutzernamen.
  nutzername = Console.ReadLine() 'Benutzernamen speichern.
  Console.WriteLine ("Hallo, " + nutzername) 'Ausgabe ist Hallo, Name
  Console.ReadLine() 'Die obige Ausgabe anzeigen.
  'Der obige Code stellt Ihnen eine Frage und zeigt die Antwort an.
  'Neben anderen Variablentypen gibt es Integer, den wir für ganze Zahlen
  'verwenden werden.
End Sub

'Drei
Private Sub BerechneGanzeZahlen()
  Console.Title = "Berechne ganze Zahlen | Lerne X in Y Minuten"
  Console.Write ("Erste Zahl: ") 'Schreiben Sie eine ganze Zahl, 1, 2, 104, usw
  Dim a As Integer = Console.ReadLine()
  Console.Write ("Zweite Zahl: ") 'Schreiben Sie eine weitere ganze Zahl.
  Dim b As Integer = Console.ReadLine()
  Dim c As Integer = a + b
  Console.WriteLine (c)
  Console.ReadLine()
  'Dies ist ein einfacher Taschenrechner
End Sub

'Vier
Private Sub BerechneDezimalZahlen()
  Console.Title = "Berechne mit dem Typ Double | Lerne X in Y Minuten"
  'Natürlich würden wir gerne Dezimalzahlen addieren.
  'Also könnten wir von Integer auf Double umstellen.

  'Schreiben Sie eine Bruchzahl, 1.2, 2.4, 50.1, 104.9 usw
  Console.Write ("Erste Zahl: ")
  Dim a As Double = Console.Readline()
  Console.Write ("Zweite Zahl: ") 'Schreiben Sie die zweite Zahl.
  Dim b As Double = Console.Readline()
  Dim c As Double = a + b
  Console.WriteLine (c)
  Console.ReadLine()
  'Dieses Programm kann 1.1 und 2.2 addieren
End Sub

'Fünf
Private Sub Taschenrechner()
  Console.Title = "Der Funktionsrechner | Lerne X in Y Minuten"
  'Wenn Sie aber wollen, dass der Rechner subtrahiert, dividiert,
  'multipliziert und addiert.
  'Kopieren Sie den obigen Text und fügen Sie ihn ein.
  Console.Write ("Erste Zahl: ")
  Dim a As Double = Console.Readline()
  Console.Write ("Zweite Zahl: ")
  Dim b As Integer = Console.Readline()
  Dim c As Integer = a + b
  Dim d As Integer = a * b
  Dim e As Integer = a - b
  Dim f As Integer = a / b

  'Mit den folgenden Zeilen können wir  die Werte a und b
  'subtrahieren, multiplizieren und dividieren
  Console.Write (a.ToString() + " + " + b.ToString())
  'Wir wollen den Ergebnissen einen linken Rand von 3 Leerzeichen geben.
  Console.WriteLine (" = " + c.ToString.PadLeft(3))
  Console.Write (a.ToString() + " * " + b.ToString())
  Console.WriteLine (" = " + d.ToString.PadLeft(3))
  Console.Write (a.ToString() + " - " + b.ToString())
  Console.WriteLine (" = " + e.ToString.PadLeft(3))
  Console.Write (a.ToString() + " / " + b.ToString())
  Console.WriteLine (" = " + f.ToString.PadLeft(3))
  Console.ReadLine()

End Sub

'Sechs
Private Sub WhileSchleife()
  'Gleich zur vorherigen privaten Subroutine.
  'Diesmal fragen wir den Benutzer, ob er fortfahren möchte (ja oder nein?).
  'Wir verwenden die Do While-Schleife, weil wir nicht wissen, ob der Benutzer
  'das Programm mehr als einmal verwenden möchte.
  Console.Title = "Do While-Schleifen verwenden | X in Y Minuten lernen"
  Dim antwort As String 'Wir verwenden "String", weil die Antwort ein Text ist
  Do 'Wir beginnen das Programm mit
    Console.Write ("Erste Zahl: ")
    Dim a As Double = Console.Readline()
    Console.Write ("Zweite Zahl: ")
    Dim b As Integer = Console.Readline()
    Dim c As Integer = a + b
    Dim d As Integer = a * b
    Dim e As Integer = a - b
    Dim f As Integer = a / b

    Console.Write (a.ToString() + " + " + b.ToString())
    Console.WriteLine (" = " + c.ToString.PadLeft(3))
    Console.Write (a.ToString() + " * " + b.ToString())
    Console.WriteLine (" = " + d.ToString.PadLeft(3))
    Console.Write (a.ToString() + " - " + b.ToString())
    Console.WriteLine (" = " + e.ToString.PadLeft(3))
    Console.Write (a.ToString() + " / " + b.ToString())
    Console.WriteLine (" = " + f.ToString.PadLeft(3))
    Console.ReadLine()
    'Fragen Sie den Benutzer, ob er fortfahren möchte. Unglücklicherweise
    'werden Groß- und Kleinschreibung unterschieden.
    Console.Write ("Möchten Sie fortfahren? (j / n)")
    'Das Programm nimmt die Variable, zeigt sie an und beginnt von vorne.
    antwort = Console.Readline()
    'Der Befehl, der diese Variable zum Laufen bringt, ist in diesem Fall "j"
  Loop While antwort = "j"

End Sub

'Sieben
Private Sub ForSchleife()
  'Manchmal muss das Programm nur einmal ausgeführt werden.
  'In diesem Programm werden wir von 10 loszählen.

  Console.Title = "Mit "For"-Schleifen | X in Y Minuten lernen"
  'Deklarieren Sie die Variable und ab welcher Zahl in Schritt -1 gezählt
  'werden soll, Schritt -2, Schritt -3, usw.
  For i As Integer = 10 To 0 Schritt -1
    Console.WriteLine (i.ToString) 'Zählerwert anzeigen
  Next i 'Berechne den neuen Wert
  Console.WriteLine ("Start") 'Starten wir das Programm, baby!!!!
  Console.ReadLine() 'BANG!!!! - Vielleicht war ich zu aufgeregt :)
End Sub

'Acht
Private Sub BedingteAnweisung()
  Console.Title = "Bedingte Anweisungen | X in Y Minuten lernen"
  Dim username As String = Console.Readline()
  'Aufforderung zur Eingabe des Benutzernamens.
  Console.WriteLine ("Hallo, wie ist Ihr Name?")
  username = Console.ReadLine() 'Benutzernamen speichern.
  If username = "Adam" Then
    Console.WriteLine ("Hallo, Adam")
    Console.WriteLine ("Danke, dass Sie diese nützliche Website erstellt haben")
    Console.ReadLine()
  Else
    Console.WriteLine ("Hallo, " + Benutzername)
    Console.WriteLine ("Haben Sie www.learnxinyminutes.com besucht?")
    Console.ReadLine() 'Beendet und zeigt die obige Anweisung an.
  End If
End Sub

  'Neun
Private Sub IfElseAnweisung()
    Console.Title = "If / Else-Anweisung | X in Y Minuten lernen"
    'Manchmal ist es wichtig, mehr als zwei Alternativen in Betracht zu ziehen.
    'Manchmal sind einige von ihnen besser.
    'In diesem Fall brauchen wir mehr als eine "if"-Anweisung.
    'Eine "if"-Anweisung ist für Verkaufsautomaten geeignet.
    'Der Benutzer gibt einen Code ein (A1, A2, A3), aus dem er wählen kann.
    'Alle Auswahlmöglichkeiten können in einer einzigen "if"-Anweisung
    'kombiniert werden.

    Dim auswahl As String = Console.ReadLine 'Der Wert der Auswahl
    Console.WriteLine ("A1. für 7Up")
    Console.WriteLine ("A2. für Fanta")
    Console.WriteLine ("A3. für Dr. Pepper")
    Console.WriteLine ("A4. für Coca-Cola")
    Console.ReadLine()
  If auswahl = "A1" Dann
    Console.WriteLine ("7up")
    Console.ReadLine()
  ElseIf auswahl = "A2" Then
    Console.WriteLine ("fanta")
    Console.ReadLine()
  ElseIf auswahl = "A3" Then
    Console.WriteLine ("Dr. Pfeffer")
    Console.ReadLine()
  ElseIf auswahl = "A4" Then
    Console.WriteLine ("Coca-Cola")
    Console.ReadLine()
  Else
    Console.WriteLine ("Ein Produkt auswählen")
    Console.ReadLine()
  End If

End Sub

End Module
```

## Referenzen

Für diejenigen, die mehr wissen wollen, hat Brian Martin ein umfassenderes
[Visual Basic Tutorial](http://www.vbbootcamp.co.uk/ "Visual Basic Tutorial")
erstellt.

Die gesamte Syntax sollte gültig sein.
Kopieren Sie den Code und fügen Sie ihn in den Visual Basic Compiler ein und
führen Sie das Programm aus (F5).

