---
category: tool
tool: PyQT
filename: learnpyqt-pt.py
contributors:
    - ["Nathan Hughes", "https://github.com/sirsharpest"]
translators:
    - ["Lucas Pugliesi", "https://github.com/fplucas"]
lang: pt-br
---

**Qt** é amplamente conhecido como um framework para desenvolvimento de
software multi-plataforma que pode rodar em várias outras plataformas de
softwares e hardwares com pouca ou nenhuma alteração no código, enquanto mantém
o poder e a velocidade de uma aplicação nativa. Embora o **Qt** tenha sido
originalmente escrito em *C++*.


Essa é uma adaptação de uma introdução ao QT em C++ por
[Aleksey Kholovchuk](https://github.com/vortexxx192), alguns dos exemplos de
código podem resultar na mesma funcionalidade que essa versão, apenas usando
o pyqt!

```python
import sys
from PyQt4 import QtGui
	
def window():
	# Cria um objeto para a aplicação
    app = QtGui.QApplication(sys.argv)
	# Cria um widget onde o nosso label será inserido
    w = QtGui.QWidget()
	# Adiciona um label ao widget
    b = QtGui.QLabel(w)
	# Informa algum texto ao label
    b.setText("Hello World!")
	# Define os tamanhos e posições dos objetos
    w.setGeometry(100, 100, 200, 50)
    b.move(50, 20)
	# Define o título da janela
    w.setWindowTitle("PyQt")
	# Exibe a janela
    w.show()
	# Executa tudo o que foi pedido, apenas uma vez
    sys.exit(app.exec_())

if __name__ == '__main__':
    window()

```

Para utilizar mais funcionalidades no **pyqt** veremos a construção de alguns
outros elementos.
Aqui mostraremos como criar uma janela popup, muito útil para perguntar ao
usuário qual decisão tomar ou exibir alguma informação.

```Python 
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *


def window():
    app = QApplication(sys.argv)
    w = QWidget()
    # Cria um botão e o anexa ao widget w
    b = QPushButton(w)
    b.setText("Press me")
    b.move(50, 50)
    # Informa b a chamar essa função quando for clicado
    # observe que a função chamada não necessita de "()"
    b.clicked.connect(showdialog)
    w.setWindowTitle("PyQt Dialog")
    w.show()
    sys.exit(app.exec_())
	
# Essa função deve criar uma janela de diálogo com um botão,
# aguarda ser clicado e encerra o programa
def showdialog():
    d = QDialog()
    b1 = QPushButton("ok", d)
    b1.move(50, 50)
    d.setWindowTitle("Dialog")
    # Essa modalidade define que o popup deve bloquear as outras janelas quando ativo
    d.setWindowModality(Qt.ApplicationModal)
    # Ao ser clicado deve encerrar o processo
    b1.clicked.connect(sys.exit)
    d.exec_()

if __name__ == '__main__':
    window()
```
