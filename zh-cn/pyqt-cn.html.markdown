---
category: tool
tool: PyQT
filename: learnpyqt.py
contributors:
    - ["Nathan Hughes", "https://github.com/sirsharpest"]
translators:
    - ["kdxcxs", "https://github.com/kdxcxs"]
lang: zh-cn
---

**Qt** 是一个著名的用C++实现的跨平台软件开发框架。只需更改少量甚至有时候不需要更改代码就能在多个软硬件平台上运行同时拥有原生应用程序的功能和速度。


这是[Aleksey Kholovchuk](https://github.com/vortexxx192)对QT的C++简介的改编，用pyqt实现了一些相同功能的代码！

```python
import sys
from PyQt4 import QtGui
	
def window():
	# 创建应用对象
    app = QtGui.QApplication(sys.argv)
	# 创建一个widget，label将会被放置在里面
    w = QtGui.QWidget()
	# 在widget中添加一个label
    b = QtGui.QLabel(w)
	# 设置label的文字
    b.setText("Hello World!")
	# 设置尺寸和位置
    w.setGeometry(100, 100, 200, 50)
    b.move(50, 20)
	# 设置窗口的标题
    w.setWindowTitle("PyQt")
	# 将所有东西都显示出来
    w.show()
	# 完成所有设置后，执行我们要求的操作
    sys.exit(app.exec_())
if __name__ == '__main__':
    window()
```

为了获得pyqt中的一些更高级的功能，我们需要开始研究构建其他元素。在这里，我们展示了如何使用弹出对话框，该对话框对于要求用户确认操作或提供信息很有用。

```Python 
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *
def window():
    app = QApplication(sys.argv)
    w = QWidget()
    # 创建一个按钮并附加到widget w
    b = QPushButton(w)
    b.setText("Press me")
    b.move(50, 50)
    # 当按钮b被点击时调用下面这个函数
    # 注意函数调用时没有“()”，这样函数就能以对象的方式传入而非调用它所得到的返回值
    # 更多关于pyqt函数调用、传参等的内容见pyqt的信号机制
    b.clicked.connect(showdialog)
    w.setWindowTitle("PyQt Dialog")
    w.show()
    sys.exit(app.exec_())
	
# 此函数将会创建一个带有按钮的对话框窗口
# 当按钮被点击时会退出这个程序
def showdialog():
    d = QDialog()
    b1 = QPushButton("ok", d)
    b1.move(50, 50)
    d.setWindowTitle("Dialog")
    # 这里的模态实现了在对话框弹出时阻塞程序同时屏蔽父窗口
    # This modality tells the popup to block the parent whilst it's active
    d.setWindowModality(Qt.ApplicationModal)
    # 当按钮被点击时整个进程将会结束
    b1.clicked.connect(sys.exit)
    d.exec_()
if __name__ == '__main__':
    window()
```