# pyqt.md (번역)

---
category: framework
name: PyQt
filename: learnpyqt.py
contributors:
    - ["Nathan Hughes", "https://github.com/sirsharpest"]
---

**Qt**는 코드 변경이 거의 또는 전혀 없이 다양한 소프트웨어 및 하드웨어 플랫폼에서 실행할 수 있는 크로스 플랫폼 소프트웨어를 개발하기 위한 널리 알려진 프레임워크이며, 네이티브 애플리케이션의 성능과 속도를 가지고 있습니다. **Qt**는 원래 *C++*로 작성되었습니다.


이것은 [Aleksey Kholovchuk](https://github.com/vortexxx192)의 C++ QT 소개를 각색한 것으로, 일부 코드 예제는 동일한 기능을 가져야 하며 이 버전은 pyqt를 사용하여 작성되었습니다!

```python
import sys
from PyQt4 import QtGui

def window():
	# 애플리케이션 객체 생성
    app = QtGui.QApplication(sys.argv)
	# 레이블이 배치될 위젯 생성
    w = QtGui.QWidget()
	# 위젯에 레이블 추가
    b = QtGui.QLabel(w)
	# 레이블에 텍스트 설정
    b.setText("Hello World!")
	# 크기 및 배치 정보 제공
    w.setGeometry(100, 100, 200, 50)
    b.move(50, 20)
	# 창에 멋진 제목 부여
    w.setWindowTitle("PyQt")
	# 모든 것을 표시
    w.show()
	# 모든 설정이 완료되면 요청한 내용 실행
    sys.exit(app.exec_())

if __name__ == '__main__':
    window()
```

**pyqt**에서 더 고급 기능을 사용하려면 추가 요소를 구축하는 것을 고려해야 합니다.
여기서는 사용자에게 결정 확인을 요청하거나 정보를 제공하는 데 유용한 대화 상자 팝업 상자를 도입하는 방법을 보여줍니다.

```python
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *


def window():
    app = QApplication(sys.argv)
    w = QWidget()
    # 버튼을 생성하고 위젯 w에 연결
    b = QPushButton(w)
    b.setText("Press me")
    b.move(50, 50)
    # b를 클릭했을 때 이 함수를 호출하도록 지시
    # 함수 호출에 "()"가 없는 점에 유의
    b.clicked.connect(showdialog)
    w.setWindowTitle("PyQt Dialog")
    w.show()
    sys.exit(app.exec_())

# 이 함수는 버튼이 있는 대화 상자 창을 생성해야 하며,
# 버튼을 클릭하면 프로그램이 종료됩니다
def showdialog():
    d = QDialog()
    b1 = QPushButton("ok", d)
    b1.move(50, 50)
    d.setWindowTitle("Dialog")
    # 이 양식은 팝업이 활성화되어 있는 동안 부모를 차단하도록 지시합니다
    d.setWindowModality(Qt.ApplicationModal)
    # 클릭 시 전체 프로세스가 종료되기를 원합니다
    b1.clicked.connect(sys.exit)
    d.exec_()

if __name__ == '__main__':
    window()
```
