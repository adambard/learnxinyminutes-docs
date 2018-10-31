---
category: tool
tool: PyQT
filename: learnpyqt-es.py
contributors:
    - ["Nathan Hughes", "https://github.com/sirsharpest"]
translators:
    - ["Adrian Rocamora", "https://github.com/adrianrocamora"]
lang: es-es
---

**Qt** es un sistema altamente reconocido que permite desarrollar software multiplataforma que puede correr en diferentes entornos de software y hardware con pocos o ningún cambio. Aun así conserva la velocidad y poder de una aplicación nativa. **Qt** fue originalmente escrito en *C++*.

Esta es una adaptación de la introducción a QT con C++ por [Aleksey Kholovchuk](https://github.com/vortexxx192), parte del código ejemplo debería resultar en la misma funcionalidad ¡pero usando python con PyQT!

```python
import sys
from PyQt4 import QtGui
	
def window():
    # Crear el objeto de la aplicación
    app = QtGui.QApplication(sys.argv)
    # Crear un widget en el que colocaremos nuestra etiqueta
    w = QtGui.QWidget()
    # Agregamos nuestra etiqueta al widget
    b = QtGui.QLabel(w)
    # Agregamos texto a nuestra etiqueta
    b.setText("Hello World!")
    # Fijemos información de posición y tamaño del widget
    w.setGeometry(100, 100, 200, 50)
    b.move(50, 20)
    # Proporcionemos un título a nuestra ventana
    w.setWindowTitle("PyQt")
    # Mostremos todo
    w.show()
    # Ejecutemos lo que hayamos solicitado ya inicializado el resto
    sys.exit(app.exec_())

if __name__ == '__main__':
    window()

```

Para poder hacer uso de las funciones más avanzades en **pyqt** necesitamos agregar elementos adicionales.
Aquí mostramos cómo introducir una caja de diálogo popup, útil para permitir al usuario confirmar su decisión o para brindarnos información.

```Python 
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *


def window():
    app = QApplication(sys.argv)
    w = QWidget()
    # Crear un botón y adjuntarlo al widget w
    b = QPushButton(w)
    b.setText("Press me")
    b.move(50, 50)
    # Indicar al botón b que llame esta función cuando reciba un click
    # Nótese la falta de "()" en la llamada de la función
    b.clicked.connect(showdialog)
    w.setWindowTitle("PyQt Dialog")
    w.show()
    sys.exit(app.exec_())
	
# Esta función debería crear una ventana de diálogo con un botón
# que espera a recibir un click y luego sale del programa
def showdialog():
    d = QDialog()
    b1 = QPushButton("ok", d)
    b1.move(50, 50)
    d.setWindowTitle("Dialog")
    # Esta modalidad le indica al popup que bloquee al padre mientras activo
    d.setWindowModality(Qt.ApplicationModal)
    # Al recibir un click me gustaría que el proceso termine
    b1.clicked.connect(sys.exit)
    d.exec_()

if __name__ == '__main__':
    window()
```
