---
category: tool
tool: Qt Framework
language: c++
filename: learnqt-de.cpp
contributors:
    - ["Aleksey Kholovchuk", "https://github.com/vortexxx192"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
lang: de-de    
---

**Qt** ist ein weithin bekanntes Framework zum Entwickeln von cross-platform Software,
die auf verschiedenen Hard- und Softwareplatformen mit wenig oder keinen Veränderungen im Code läuft.
Dabei besitzt man die Power und Geschiwindigkeit von nativen Anwendungen.
Obwohl **Qt** ursprünglich in *C++* geschrieben wurde,
gibt es verschiedene Ports für andere Sprachen: *[PyQt](https://learnxinyminutes.com/docs/pyqt/)*, *QtRuby*, *PHP-Qt*, etc.

**Qt** eignet sich hervorragend zum Erstellen von Anwendungen mit grafischer Benutzeroberfläche (GUI).
Dieses Tutorial zeigt, wie man das in *C++* macht.

```c++
/*
 * Lass uns klassisch starten
 */

// Alle Header vom Qt Framework starten mit dem Großbuchstaben 'Q'.
#include <QApplication>
#include <QLineEdit>

int main(int argc, char *argv[]) {
    // Erstellt ein Objekt um applikationsweit die Resourcen zu managen.
    QApplication app(argc, argv);

    // Erstellt ein Line edit Widget und zeigt es auf dem Bildschirm
    QLineEdit lineEdit("Hello world!");
    lineEdit.show();

    // Startet die Event Loop der Anwendung.
    return app.exec();
}
```

Die GUI bezogene Teile von **Qt** bestehen aus *Widgets* und den *Verbindungen*
dazwischen.

[Lies mehr über Widgets](http://doc.qt.io/qt-5/qtwidgets-index.html)

```c++
/*
 * Lass uns Label und einen Button machen.
 * Ein Label soll auftauchen, wenn der Button gedrückt wird.
 * 
 * Der Qt Code spricht für sich selbst.
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
    
    // Füge ein vertikales Layout hinzu
    QVBoxLayout layout;
    dialogWindow.setLayout(&layout);  

    QLabel textLabel("Danke für das Knopf drücken");
    layout.addWidget(&textLabel);
    textLabel.hide();

    QPushButton button("Drück mich");
    layout.addWidget(&button);
    
    // Zeigt verstecktes Label, wenn der Button gedrückt wird.
    QObject::connect(&button, &QPushButton::pressed,
                     &textLabel, &QLabel::show);

    return app.exec();
}
```

Beachte den *QObject::connect* Teil. Diese Methode wird verwendet,
um *Signale* eines Objekts mit den *Slots* eines Objektes zu verbinden.

**Signale** werden ausgegeben, wenn bestimmte Dinge mit Objekten passieren.
Beispielsweise wird das *pressed* Signal ausgegeben,
wenn der Benutzer auf das QPushButton Objekt drückt.

**Slots** sind Aktionen, die als Reaktion auf empfangene Signale ausgeführt werden können.

[Lies mehr über Slots und Signale](http://doc.qt.io/qt-5/signalsandslots.html)


Als Nächstes lernen wir, dass wir nicht nur Standard Widgets verwenden können,
sondern auch ihr Verhalten mithilfe von Vererbung verändern können.
Lass uns einen Button erschaffen, der zählt, wie häufig er gedrückt wird.
Dafür definieren wir unsere eigene Klasse *CounterLabel*.
Diese muss wegen der speziellen Qt Architektur in einer seperaten Datei deklariert werden.

```c++
// counterlabel.hpp

#ifndef COUNTERLABEL
#define COUNTERLABEL

#include <QLabel>

class CounterLabel : public QLabel {
    Q_OBJECT  // Qt definiertes Makro, welches in jedem modifizierten Widget vorhanden sein muss.

public:
    CounterLabel() : counter(0) {
        setText("Zähler wurde noch nicht erhöht.");  // Methode von QLabel
    }

public slots:
    // Aktion, die ausgeführt wird, wenn der Button gedrückt wird.
    void increaseCounter() {
        setText(QString("Zähler Wert: %1").arg(QString::number(++counter)));
    }

private:
    int counter;
};

#endif // Zähllabel
```

```c++
// main.cpp
// Fast das Gleiche, wie das vorherige Beispiel

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

    QPushButton button("Drück mich nochmal.");
    layout.addWidget(&button);
    QObject::connect(&button, &QPushButton::pressed,
                     &counterLabel, &CounterLabel::increaseCounter);

    return app.exec();
}
```

Das wars! Natürlich ist das Qt Framework erheblich größer, als der der Teil der in diesem Tutorial behandelt wurde.
Das heißt, es gibt viel zu lesen und zu üben.

## Further reading

- [Qt 4.8 tutorials](http://doc.qt.io/qt-4.8/tutorials.html)
- [Qt 5 tutorials](http://doc.qt.io/qt-5/qtexamplesandtutorials.html)

Viel Erfolg und viel Spaß!
