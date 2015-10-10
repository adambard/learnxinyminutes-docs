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