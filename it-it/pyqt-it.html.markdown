---
category: tool
tool: PyQT
filename: learnpyqt-it.py
contributors:
    - ["Nathan Hughes", "https://github.com/sirsharpest"]
translators:
    - ["Ale46", "https://github.com/ale46"]
lang: it-it
---

**Qt** è un framework ampiamente conosciuto per lo sviluppo di software multipiattaforma che può essere eseguito su varie piattaforme software e hardware con modifiche minime o nulle nel codice, pur avendo la potenza e la velocità delle applicazioni native. Sebbene **Qt** sia stato originariamente scritto in *C++*.


Questo è un adattamento sull'introduzione di C ++ a QT di [Aleksey Kholovchuk] (https://github.com/vortexxx192
), alcuni degli esempi di codice dovrebbero avere la stessa funzionalità
che avrebbero se fossero fatte usando pyqt! 

```python
import sys
from PyQt4 import QtGui
	
def window():
	# Crea un oggetto applicazione
    app = QtGui.QApplication(sys.argv)
	# Crea un widget in cui verrà inserita la nostra etichetta
    w = QtGui.QWidget()
	# Aggiungi un'etichetta al widget
    b = QtGui.QLabel(w)
	# Imposta del testo per l'etichetta
    b.setText("Ciao Mondo!")
	# Fornisce informazioni su dimensioni e posizionamento
    w.setGeometry(100, 100, 200, 50)
    b.move(50, 20)
	# Dai alla nostra finestra un bel titolo
    w.setWindowTitle("PyQt")
	# Visualizza tutto
    w.show()
	# Esegui ciò che abbiamo chiesto, una volta che tutto è stato configurato
    sys.exit(app.exec_())

if __name__ == '__main__':
    window()

```

Per ottenere alcune delle funzionalità più avanzate in **pyqt**, dobbiamo iniziare a cercare di creare elementi aggiuntivi.
Qui mostriamo come creare una finestra popup di dialogo, utile per chiedere all'utente di confermare una decisione o fornire informazioni

```Python 
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *


def window():
    app = QApplication(sys.argv)
    w = QWidget()
    # Crea un pulsante e allegalo al widget w
    b = QPushButton(w)
    b.setText("Premimi")
    b.move(50, 50)
    # Indica a b di chiamare questa funzione quando si fa clic
    # notare la mancanza di "()" sulla chiamata di funzione
    b.clicked.connect(showdialog)
    w.setWindowTitle("PyQt Dialog")
    w.show()
    sys.exit(app.exec_())
	
# Questa funzione dovrebbe creare una finestra di dialogo con un pulsante
# che aspetta di essere cliccato e quindi esce dal programma
def showdialog():
    d = QDialog()
    b1 = QPushButton("ok", d)
    b1.move(50, 50)
    d.setWindowTitle("Dialog")
    # Questa modalità dice al popup di bloccare il genitore, mentre è attivo
    d.setWindowModality(Qt.ApplicationModal)
    # Al click vorrei che l'intero processo finisse
    b1.clicked.connect(sys.exit)
    d.exec_()

if __name__ == '__main__':
    window()
```
