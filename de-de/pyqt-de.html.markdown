---
category: tool
tool: PyQT
filename: learnpyqt-de.py
contributors:
    - ["Nathan Hughes", "https://github.com/sirsharpest"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
lang: de-de
---

**Qt** ist eine weit bekanntes Framework mit den man plattformunabhängige Programme schreiben kann,
die auf verschiedenen Sotfware und Hardware Plattformen laufen mit kleinen oder keinen Änderungen im Code.
Dabei besitzen sie trozdem die Power und Geschwindigkeit von nativen Anwendungen.
**Qt** wurde ursprünglich in *C++** geschrieben.

Das ist eine Adaption von dem C++ Intro für QT von [Aleksey Kholovchuk](https://github.com/vortexxx192),
manche der Codebeispiele sollte in der selben Funktionalität resultieren.
Diese Version wurde in pyqt erstellt.

```python
import sys
from PyQt4 import QtGui
	
def window():
	# Erschafft ein Anwendungsobjekt.
    app = QtGui.QApplication(sys.argv)
	# Erschafft ein Widget, auf dem unser Label platziert wird.
    w = QtGui.QWidget()
	# Fügt ein Label zu dem Widget hinzu.
    b = QtGui.QLabel(w)
	# Setzt einen Text für das Label.
    b.setText("Hello World!")
	# Setzt die Größe und die Platzierungsinfomationen.
    w.setGeometry(100, 100, 200, 50)
    b.move(50, 20)
	# Setzt unserem Fenster einen schönen Titel.
    w.setWindowTitle("PyQt")
	# Lässt alles anzeigen.
    w.show()
	# Führe alles aus, nachdem wir alles aufgebaut haben.
    sys.exit(app.exec_())

if __name__ == '__main__':
    window()

```

Damit wir weitere fortgeschrittene Funktionen in **pyqt** verwenden können,
müssen wir anfangen zusätzliche Elemente zu bauen.
Hier zeigen wir wie man eine Dialog Popup Box einführt. 
Diese ist nützlich, um den Benutzer eine Entscheidung zu bestätigen oder um Informationen anzuzeigen.

```Python 
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *


def window():
    app = QApplication(sys.argv)
    w = QWidget()
    # Erschafft einen Knopf und fügt das Widget w hinzu
    b = QPushButton(w)
    b.setText("drücke mich")
    b.move(50, 50)
    # Wenn b gedrückt wird, wird diese Funktion aufgerufen.
    # Bemerke das Fehlen von () bei dem Funktionsaufruf.
    b.clicked.connect(showdialog)
    w.setWindowTitle("PyQt Dialog")
    w.show()
    sys.exit(app.exec_())
	
# Diese Funktion soll ein Dialogfenster mit einem Knopf erschaffen.
# Der Knopf wartet bis er geklickt wird und beendet das Programm
def showdialog():
    d = QDialog()
    b1 = QPushButton("ok", d)
    b1.move(50, 50)
    d.setWindowTitle("Dialog")
    # Diese Modalität sagt dem Popup, dass es den Parent blocken soll, solange es aktiv ist.
    d.setWindowModality(Qt.ApplicationModal)
    # Beim klicken möchte ich, dass der gesamte Prozess beendet wird.
    b1.clicked.connect(sys.exit)
    d.exec_()

if __name__ == '__main__':
    window()
```
