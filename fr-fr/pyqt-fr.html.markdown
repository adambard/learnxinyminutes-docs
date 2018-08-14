---
category: tool
tool: PyQT
filename: learnpyqt-fr.py
contributors:
    - ["Nathan Hughes", "https://github.com/sirsharpest"]
translators:
    - ["DevHugo", "http://twitter.com/devhugo"]
lang: fr-fr
---

**Qt** est un framework très connu pour le developpement de logiciel cross-platform qui peuvent être lancé sur différents systèmes avec de petit ou aucun changement dans le code, tout en ayant la puissance et la vitesse des applications natives. Bien que **Qt** ait été écrit à l'origine en *C++*.


C'est une adaptation de l'intro du C++ vers QT par [Aleksey Kholovchuk](https://github.com/vortexxx192
), certains exemples du code doivent avoir la même fonctionnalité
cette version ayant juste été faite en utilisant pyqt! 

```python
import sys
from PyQt4 import QtGui
	
def window():
	# Création de l'objet application 
    app = QtGui.QApplication(sys.argv)
	# Création d'un widget où notre label sera placé
    w = QtGui.QWidget()
	# Ajout d'un label au widget
    b = QtGui.QLabel(w)
	# Assignation de texte au label
    b.setText("Hello World!")
	# Assignation des tailles et des informations de placement
    w.setGeometry(100, 100, 200, 50)
    b.move(50, 20)
	# Assignation d'un nom à notre fenêtre
    w.setWindowTitle("PyQt")
	# Affichage de la fenêtre
    w.show()
	# Exécution de l'application
    sys.exit(app.exec_())

if __name__ == '__main__':
    window()

```

In order to get some of the more advanced features in **pyqt** we need to start looking at building additional elements. 
Here we show how to introduce a dialog popup box, useful for asking the user to confirm a decision or to provide information.

```Python 
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *


def window():
    app = QApplication(sys.argv)
    w = QWidget()
    # Create a button and attach to widget w
    b = QPushButton(w)
    b.setText("Press me")
    b.move(50, 50)
    # Tell b to call this function when clicked
    # notice the lack of "()" on the function call
    b.clicked.connect(showdialog)
    w.setWindowTitle("PyQt Dialog")
    w.show()
    sys.exit(app.exec_())
	
# This function should create a dialog window with a button
# that waits to be clicked and then exits the program
def showdialog():
    d = QDialog()
    b1 = QPushButton("ok", d)
    b1.move(50, 50)
    d.setWindowTitle("Dialog")
    # This modality tells the popup to block the parent whilst it's active
    d.setWindowModality(Qt.ApplicationModal)
    # On click I'd like the entire process to end
    b1.clicked.connect(sys.exit)
    d.exec_()

if __name__ == '__main__':
    window()
```
