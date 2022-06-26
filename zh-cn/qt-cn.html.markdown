---
category: tool
tool: Qt Framework
language: c++
filename: learnqt-cn.cpp
contributors:
    - ["Aleksey Kholovchuk", "https://github.com/vortexxx192"]
translators:
    - ["GengchenXU", "https://github.com/GengchenXU"]
lang: zh-cn
    
---

**Qt** Qt是一个广为人知的框架，用于开发跨平台软件，该软件可以在各种软件和硬件平台上运行，代码几乎没有变化，同时具有本机应用程序的能力和速度。虽然**Qt**最初是用*C*++，但也有其他语言的端口: *[PyQt](https://learnxinyminutes.com/docs/pyqt/)*, *QtRuby*, *PHP-Qt*, 等等.

**Qt** 非常适合使用图形用户界面 （GUI） 创建应用程序。本教程是关于如何用*C++*去实现。

```c++
/*
 * 让我们从最经典的开始
 */

// Qt框架的所有标头均以大写字母'Q'开头
#include <QApplication>
#include <QLineEdit>

int main(int argc, char *argv[]) {
	 // 创建一个对象来管理应用程序范围内的资源
    QApplication app(argc, argv);

    // 创建行编辑widgets并在屏幕上显示
    QLineEdit lineEdit("Hello world!");
    lineEdit.show();

    // 启动应用程序的事件循环
    return app.exec();
}
```

**Qt**与 GUI 相关的部分与*widgets*及其之间的*connection*有关。

[阅读更多有关widgets的信息](http://doc.qt.io/qt-5/qtwidgets-index.html)

```c++
/*
 * 让我们创建一个标签和一个按钮。
 * 按下按钮时应显示一个标签。
 * Qt代码本身就可以说明问题。
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
    
    // 添加垂直布局
    QVBoxLayout layout;
    dialogWindow.setLayout(&layout);  

    QLabel textLabel("Thanks for pressing that button");
    layout.addWidget(&textLabel);
    textLabel.hide();

    QPushButton button("Press me");
    layout.addWidget(&button);
    
    // 按下按钮时显示隐藏标签
    QObject::connect(&button, &QPushButton::pressed,
                     &textLabel, &QLabel::show);

    return app.exec();
}
```

注意，*QObject :: connect*部分。 此方法用于将一个对象的*SIGNAL*连接到另一个对象的*SLOTS*。

**Signals** 会被发出当对象发生某些事情时，例如当用户按下QPushButton对象时会发出*push*的信号。

**Slots** 是可以响应于接收到的信号而执行的*action*。

[阅读有关SLOTS和SIGNALS的更多信息](http://doc.qt.io/qt-5/signalsandslots.html)


接下来，让我们了解到我们不仅可以使用标准的wigets，而且可以通过继承扩展其行为。 让我们创建一个按钮并计算其被按下的次数。 为此，我们定义了自己的类* CounterLabel *。 由于特定的Qt体系结构，必须在单独的文件中声明它。

```c++
// counterlabel.hpp

#ifndef COUNTERLABEL
#define COUNTERLABEL

#include <QLabel>

class CounterLabel : public QLabel {
    Q_OBJECT  // 在每个自定义wiget中必须存在的Qt定义的宏

public:
    CounterLabel() : counter(0) {
        setText("Counter has not been increased yet");  // QLabel方法
    }

public slots:
    // 将响应按钮按下而调用的操作
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
// 与前面的示例几乎相同

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

当然，Qt框架比本教程介绍的部分要复杂得多，因此请仔细阅读和练习。

## 进一步阅读
- [Qt 4.8 tutorials](http://doc.qt.io/qt-4.8/tutorials.html)
- [Qt 5 tutorials](http://doc.qt.io/qt-5/qtexamplesandtutorials.html)

祝你好运，生活愉快!
