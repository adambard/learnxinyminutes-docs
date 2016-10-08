---
category: tool
tool: Qt Framework
language: c++
filename: learnqt.cpp
contributors:
    - ["Aleksey Kholovchuk", "https://github.com/vortexxx192"]
lang: en
---

**Qt** is a widely-known framework for developing cross-platform software that can be run on various software and hardware platforms with little or no change in the code, while having the power and speed of native applications. Though **Qt** was originally written in *C++*, there are its ports to other languages: *[PyQt](https://learnxinyminutes.com/docs/pyqt/)*, *QtRuby*, *PHP-Qt*, etc.

**Qt** is great for creating applications with graphical user interface (GUI). This tutorial is how to do it in *C++*.

```c++
/*
 * Let's start clasically
 */

// all headers from Qt framework start with capital letter 'Q'
#include <QApplication>
#include <QLineEdit>

int main(int argc, char *argv[]) {
	 // create an object to manage application-wide resources
    QApplication app(argc, argv);

    // create line edit widget and show it on screen
    QLineEdit lineEdit("Hello world!");
    lineEdit.show();

    // start the application's event loop
    return app.exec();
}
```

GUI-related part of **Qt** is all about *widgets* and *connections* between them.

[READ MORE ABOUT WIDGETS](http://doc.qt.io/qt-5/qtwidgets-index.html)

```c++
/*
 * Let's create a label and a button.
 * A label should appear when a button is pressed.
 * 
 * Qt code is speaking for itself.
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
    
    // add vertical layout 
    QVBoxLayout layout;
    dialogWindow.setLayout(&layout);  

    QLabel textLabel("Thanks for pressing that button");
    layout.addWidget(&textLabel);
    textLabel.hide();

    QPushButton button("Press me");
    layout.addWidget(&button);
    
    // show hidden label when the button is pressed
    QObject::connect(&button, &QPushButton::pressed,
                     &textLabel, &QLabel::show);

    return app.exec();
}
```

Notice that *QObject::connect* part. This method is used to connect *SIGNALS* of one objects to *SLOTS* of another.

**Signals** are being emited when certain things happen with objects, like *pressed* signal is emited when user presses on QPushButton object.

**Slots** are *actions* that might be performed in response to received signals.

[READ MORE ABOUT SLOTS AND SIGNALS](http://doc.qt.io/qt-5/signalsandslots.html)


Next, let's learn that we can not only use standard widgets but also extend their behaviour using inheritance. Let's create a button and count how many times it was pressed. For this purpose we define our own class *CounterLabel*.  It must be declared in separate file because of specific Qt architecture.

```c++
// counterlabel.hpp

#ifndef COUNTERLABEL
#define COUNTERLABEL

#include <QLabel>

class CounterLabel : public QLabel {
    Q_OBJECT  // Qt-defined macros that must be present in every custom widget

public:
    CounterLabel() : counter(0) {
        setText("Counter has not been increased yet");  // method of QLabel
    }

public slots:
    // action that will be called in response to button press
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
// Almost the same as in previous example

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

That's it! Of course, Qt framework is much much larger than the part that was covered in this tutorial, so be ready to read and practice.

## Further reading

- [Qt 4.8 tutorials](http://doc.qt.io/qt-4.8/tutorials.html)
- [Qt 5 tutorials](http://doc.qt.io/qt-5/qtexamplesandtutorials.html)

Good luck and have fun!
