---
category: tool
tool: PyQT
filename: learnpyqt.py
contributors:
    - ["Nathan Hughes", "https://github.com/sirsharpest"]
---

**Qt** is a widely-known framework for developing cross-platform software that can be run on various software and hardware platforms with little or no change in the code, while having the power and speed of native applications. Though **Qt** was originally written in *C++*.


This is an adaption on the C++ intro to QT by [Aleksey Kholovchuk](https://github.com/vortexxx192
), some of the code examples should result in the same functionality
this version just having been done using pyqt! 

```python
import sys
from PyQt4 import QtGui
	
def window():
	# Create an application object 
    app = QtGui.QApplication(sys.argv)
	# Create a widget where our label will be placed in
    w = QtGui.QWidget()
	# Add a label to the widget 
    b = QtGui.QLabel(w)
	# Set some text for the label 
    b.setText("Hello World!")
	# Give some size and placement information 
    w.setGeometry(100, 100, 200, 50)
    b.move(50, 20)
	# Give our window a nice title 
    w.setWindowTitle("PyQt")
	# Have everything display
    w.show()
	# Execute what we have asked for, once all setup
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
