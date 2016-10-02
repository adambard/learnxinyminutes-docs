---
category: tool
tool: Qt Framework
language: c++
filename: learnqt-ru.cpp
contributors:
    - ["Aleksey Kholovchuk", "https://github.com/vortexxx192"]
translators:
    - ["Evan K.", "https://github.com/justblah"]
lang: ru-ru
---

**Qt** является широко известным фреймворком для разработки кросс-платформенного программного обеспечения, которое может быть запущено на различных программно-аппаратных платформах практически без изменений в коде, сохраняя при этом мощность и скорость нативных приложений. Хоть **Qt** и был изначально написан на *C++*, у него есть реализации и на других языках: *PyQt*, *QtRuby*, *PHP-Qt* и т.д.

**Qt** отлично подходит для создания приложений с графическим пользовательским интерфейсом (GUI). Это руководство о том, как сделать это на *C++*.

```c++
/*
 * Начнём по-старинке
 */

// все header файлы импортированные из Qt начинаются с заглавной 'Q'
#include <QApplication>
#include <QLineEdit>

int main(int argc, char *argv[]) {
    // создаем объект для управления данными приложения
    QApplication app(argc, argv);

    // создаем редактируемую строку и отобразим её на экране
    QLineEdit lineEdit("Hello world!");
    lineEdit.show();

    // запускаем цикл для обработки событий (event loop)
    return app.exec();
}
```

GUI часть **Qt** полностью состоит из *виджетов* и *связей* между ними.

[(EN) ПОДРОБНЕЕ О ВИДЖЕТАХ](http://doc.qt.io/qt-5/qtwidgets-index.html)

```c++
/*
 * В этом примере мы отобразим надпись с кнопкой.
 * Надпись будет появляться после нажатия на кнопку.
 *
 * Код на Qt говорит сам за себя.
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

    // добавляем вертикальное расположение
    QVBoxLayout layout;
    dialogWindow.setLayout(&layout);  

    QLabel textLabel("Thanks for pressing that button");
    layout.addWidget(&textLabel);
    textLabel.hide();

    QPushButton button("Press me");
    layout.addWidget(&button);

    // отображаем скрытую надпись после нажатия на кнопку
    QObject::connect(&button, &QPushButton::pressed,
                     &textLabel, &QLabel::show);

    return app.exec();
}
```
Обратите внимание на метод *QObject::connect*. Этот метод соединяет *СИГНАЛЫ* одного объекта со *СЛОТАМИ* другого.

**Сигналы** отправляются когда с объектами происходят отпределённые события, например, сигнал *нажатие* отправляется когда пользователь нажимает на объект типа QPushButton.

**Слоты** это *действия*, которые могут быть выполнены в ответ на полученные сигналы.

[(EN) ПОДРОБНЕЕ О СЛОТАХ И СИГНАЛАХ](http://doc.qt.io/qt-4.8/signalsandslots.html)


Далее рассмотрим, как можно не только использовать стандартные виджеты, но и расширять их поведение с помощью наследования. Давайте создадим кнопку и посчитаем, сколько раз она была нажата. Для этого мы определяем наш собственный класс *CounterLabel*. Он должен быть объявлен в отдельном файле из-за специфической архитектуры Qt.

```c++
// counterlabel.hpp

#ifndef COUNTERLABEL
#define COUNTERLABEL

#include <QLabel>

class CounterLabel : public QLabel {
    Q_OBJECT  // макрос Qt, обязателен для всех виджетов

public:
    CounterLabel() : counter(0) {
        setText("Counter has not been increased yet");  // метод QLabel
    }

public slots:
    // действие, которое будет вызвано в ответ на нажатие
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
// Почти тоже самое, что и в предыдущем примере

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

## На почитать
Это всё! Конечно, фреймворк Qt намного объемнее, чем часть, которая была рассмотрена в этом руководстве, так что будьте готовы читать и практиковаться.

[(EN) ДОКУМЕНТАЦИЯ](http://wiki.qt.io/Main/ru)

Удачи!
