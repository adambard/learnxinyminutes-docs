---
category: tool
tool: PyQT
lang: ru-ru
filename: learnpyqt-ru.py
contributors:
    - ["Nathan Hughes", "https://github.com/sirsharpest"]
translators:
    - ["Vadim Toptunov", "https://github.com/VadimToptunov"]
---

**Qt** - широко известный кросс-платформенный фреймворк для разработки программного обеспечения,
который может быть использован на различных софтварных и хардварных платформах без какого-либо
изменения в коде. Данный фреймворк при этом обладает мощью и скоростью нативных приложений. 
Qt и был изначально написан на *C++*.

Данный текст является адаптацией введения в Qt на C++ под авторством Алексея Ковальчука для pyqt.


```python

def window():
    # Создайте объект приложения 
    app = QtGui.QApplication(sys.argv)
    # Создайте виджет, где будет находиться наш лейбл
    w = QtGui.QWidget()
    # Добавьте лейбл в виджет
    b = QtGui.QLabel(w)
    # Задайте текст для лейбла
    b.setText("Hello World!")
    # Задайте информация о размере и расположении 
    w.setGeometry(100, 100, 200, 50)
    b.move(50, 20)
    # Задайте заголовок окна 
    w.setWindowTitle("PyQt")
    # Все ранее написанное выводится на экран
    w.show()
    # Настройка
    sys.exit(app.exec_())

if __name__ == '__main__':
    window()

```

Для того, чтобы получить более продвинутые функции приложения в pyqt, нам необходимо 
обратить внимание на создание дополнительных элементов. Ниже представлено создание всплывающего диалогового окна, которое просит пользователя подтвердить его решение или предоставить какую-либо 
информацию.

```Python 
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *


def window():
    app = QApplication(sys.argv)
    w = QWidget()
    # Создайте кнопку и прикрепите ее к виджету w
    b = QPushButton(w)
    b.setText("Press me")
    b.move(50, 50)
    # Укажите b вызвать эту функцию при клике мышкой
    # Заметьте, что в вызове функции отсутствуют "()"
    b.clicked.connect(showdialog)
    w.setWindowTitle("PyQt Dialog")
    w.show()
    sys.exit(app.exec_())

Данная функция должна создавать диалоговое окно с кнопкой, которая ждет клика по себе 
и затем завершает программу.

def showdialog():
    d = QDialog()
    b1 = QPushButton("ok", d)
    b1.move(50, 50)
    d.setWindowTitle("Dialog")
    # Эта модальность сообщает всплывающему окну блокировать родительский элемент, пока он активен
    d.setWindowModality(Qt.ApplicationModal)
    # Процесс завершается по клику мышкой
    b1.clicked.connect(sys.exit)
    d.exec_()

if __name__ == '__main__':
    window()
```
