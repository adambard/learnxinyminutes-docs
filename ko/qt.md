---
category: framework
name: Qt
filename: learnqt.cpp
contributors:
    - ["Aleksey Kholovchuk", "https://github.com/vortexxx192"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

**Qt**는 다양한 소프트웨어 및 하드웨어 플랫폼에서 코드 변경이 거의 또는 전혀 없이 실행될 수 있는 크로스 플랫폼 소프트웨어 개발을 위한 널리 알려진 프레임워크이며, 네이티브 애플리케이션의 성능과 속도를 가지고 있습니다. **Qt**는 원래 *C++*로 작성되었지만, *[PyQt](../pyqt/)*, *QtRuby*, *PHP-Qt* 등 다른 언어로도 포팅되었습니다.

**Qt**는 그래픽 사용자 인터페이스(GUI)가 있는 애플리케이션을 만드는 데 훌륭합니다. 이 튜토리얼은 *C++*에서 수행하는 방법입니다.

```c++
/*
 * 고전적으로 시작하겠습니다
 */

// Qt 프레임워크의 모든 헤더는 대문자 'Q'로 시작합니다
#include <QApplication>
#include <QLineEdit>

int main(int argc, char *argv[]) {
	 // 애플리케이션 전체 리소스를 관리하는 객체 생성
    QApplication app(argc, argv);

    // 라인 편집 위젯을 만들고 화면에 표시
    QLineEdit lineEdit("Hello world!");
    lineEdit.show();

    // 애플리케이션의 이벤트 루프 시작
    return app.exec();
}
```

**Qt**의 GUI 관련 부분은 모두 *위젯*과 그들 사이의 *연결*에 관한 것입니다.

[위젯에 대해 더 알아보기](http://doc.qt.io/qt-5/qtwidgets-index.html)

```c++
/*
 * 레이블과 버튼을 만들어 봅시다.
 * 버튼을 누르면 레이블이 나타나야 합니다.
 *
 * Qt 코드는 그 자체로 말합니다.
 */

#include <QApplication>
#include <QDialog>
#include <QVBoxLayout>
#include <QPushButton>
#include <QLabel>

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    QDialog dialogWindow;
    dialogWindow.show();

    // 수직 레이아웃 추가
    QVBoxLayout layout;
    dialogWindow.setLayout(&layout);

    QLabel textLabel("Thanks for pressing that button");
    layout.addWidget(&textLabel);
    textLabel.hide();

    QPushButton button("Press me");
    layout.addWidget(&button);

    // 버튼을 누르면 숨겨진 레이블 표시
    QObject::connect(&button, &QPushButton::pressed,
                     &textLabel, &QLabel::show);

    return app.exec();
}
```

*QObject::connect* 부분을 주목하십시오. 이 메서드는 한 객체의 *시그널*을 다른 객체의 *슬롯*에 연결하는 데 사용됩니다.

**시그널**은 사용자가 QPushButton 객체를 누를 때 *pressed* 시그널이 방출되는 것과 같이 객체에 특정 이벤트가 발생할 때 방출됩니다.

**슬롯**은 수신된 시그널에 대한 응답으로 수행될 수 있는 *액션*입니다.

[슬롯과 시그널에 대해 더 알아보기](http://doc.qt.io/qt-5/signalsandslots.html)


다음으로, 표준 위젯을 사용할 뿐만 아니라 상속을 사용하여 동작을 확장할 수 있다는 것을 배워봅시다. 버튼을 만들고 몇 번 눌렸는지 세어 봅시다. 이를 위해 자체 클래스 *CounterLabel*을 정의합니다. 특정 Qt 아키텍처 때문에 별도의 파일에 선언해야 합니다.

```c++
// counterlabel.hpp

#ifndef COUNTERLABEL
#define COUNTERLABEL

#include <QLabel>

class CounterLabel : public QLabel {
    Q_OBJECT  // 모든 사용자 정의 위젯에 있어야 하는 Qt 정의 매크로

public:
    CounterLabel() : counter(0) {
        setText("Counter has not been increased yet");  // QLabel의 메서드
    }

public slots:
    // 버튼 누름에 대한 응답으로 호출될 액션
    void increaseCounter() {
        setText(QString("Counter value: %1").arg(QString::number(++counter)));
    }

private:
    int counter;
};

#endif // COUNTERLABEL
```

```c++
// main.cpp
// 이전 예제와 거의 동일

#include <QApplication>
#include <QDialog>
#include <QVBoxLayout>
#include <QPushButton>
#include <QString>
#include "counterlabel.hpp"

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    QDialog dialogWindow;
    dialogWindow.show();

    QVBoxLayout layout;
    dialogWindow.setLayout(&layout);

    CounterLabel counterLabel;
    layout.addWidget(&counterLabel);

    QPushButton button("Push me once more");
    layout.addWidget(&button);
    QObject::connect(&button, &QPushButton::pressed,
                     &counterLabel, &CounterLabel::increaseCounter);

    return app.exec();
}
```

이게 다입니다! 물론 Qt 프레임워크는 이 튜토리얼에서 다룬 부분보다 훨씬 더 크므로 읽고 연습할 준비를 하십시오.

## 더 읽을거리

- [Qt 4.8 튜토리얼](http://doc.qt.io/qt-4.8/tutorials.html)
- [Qt 5 튜토리얼](http://doc.qt.io/qt-5/qtexamplesandtutorials.html)

행운을 빌며 재미있게 보내세요!
