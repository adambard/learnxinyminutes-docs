---
category: tool
tool: PyQT
filename: learnpyqt.py
contributors:
    - ["Nathan Hughes", "https://github.com/sirsharpest"]
---

Το **Qt** είναι ένα ευρέως γνωστό framework για ανάπτυξη cross-platform εφαρμογών που μπορεί να τρέχει σε διάφορες software/hardware πλατφόρμες με 
μικρές εώς καθόλου αλλαγές στον κώδικα, ενώ έχει την απόδοση των native εφαρμογών. Το **Qt** αρχικά ήταν γραμμένο σε *C++*.


Αυτό είναι μια προσαρμογή της εισαγωγής σε C++ για το QT από τον [Aleksey Kholovchuk](https://github.com/vortexxx192), 
τα παραδείγματα κώδικα έχουν την ίδια λειτουργικότητα απλά χρησιμοποιούν το pyqt.

```python
import sys
from PyQt4 import QtGui
	
def window():
	# Φτιάχνουμε ένα Application object
    app = QtGui.QApplication(sys.argv)
	# Φτιάχνουμε ένα widget όπου θα βάλουμε το label μας
    w = QtGui.QWidget()
	# Βάζουμε το label στο widget
    b = QtGui.QLabel(w)
	# Βάζουμε κείμενο στο label
    b.setText("Hello World!")
	# Δίνουμε πληροφορίες για το μέγεθος και τη θέση
    w.setGeometry(100, 100, 200, 50)
    b.move(50, 20)
	# Έτσι αλλάζουμε το όνομα του παραθύρου
    w.setWindowTitle("PyQt")
	# Τα δείχνουμε όλα
    w.show()
	# Όταν όλα είναι έτοιμα εκτελούμε την εφαρμογή
    sys.exit(app.exec_())

if __name__ == '__main__':
    window()

```

Για να κάνουμε πιο προχωρημένα πράγματα με το **pyqt** πρέπει να χτίσουμε κι άλλα στοιχεία.
Παρακάτω φαίνεται πώς να παράξουμε ένα κουτί διαλόγου, που χρησιμεύει για να πάρουμε είσοδο από τον χρήστη.

```Python 
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *


def window():
    app = QApplication(sys.argv)
    w = QWidget()
    # Φτιάχνουμε ένα κουμπί b και το βάζουμε σε ένα widget w
    b = QPushButton(w)
    b.setText("Press me")
    b.move(50, 50)
    # Όταν ο χρήστης κάνει κλικ το b, καλείται η συνάρτηση.
    # περνάμε ως παράμετρο τη συνάρτηση, δεν την καλούμε και γι αυτό λείπει το "()"
    b.clicked.connect(showdialog)
    w.setWindowTitle("PyQt Dialog")
    w.show()
    sys.exit(app.exec_())
	
# Αυτή η συνάρτηση εμφανίζει ένα παράθυρο διαλόγου με ένα κουμπί
# που όταν κάνει κλικ ο χρήστης κλείνει το πρόγραμμα
def showdialog():
    d = QDialog()
    b1 = QPushButton("ok", d)
    b1.move(50, 50)
    d.setWindowTitle("Dialog")
    # Αυτό το modality λέει στο popup να κάνει block το παράθυρο από το οποίο 
    # δημιουργήθηκε μέχρι να κλείσει
    d.setWindowModality(Qt.ApplicationModal)
    # Όταν γίνει κλικ θέλουμε να τερματίσει η διεργασία
    b1.clicked.connect(sys.exit)
    d.exec_()

if __name__ == '__main__':
    window()
```
