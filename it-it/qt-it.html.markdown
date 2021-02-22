---
category: tool
tool: Qt Framework
language: c++
filename: learnqt-it.cpp
contributors:
    - ["Aleksey Kholovchuk", "https://github.com/vortexxx192"]
translators:
    - ["Ale46", "https://gihub.com/ale46"]
lang: it-it
---

**Qt** è un framework ampiamente conosciuto per lo sviluppo di software multipiattaforma che può essere eseguito su varie piattaforme software e hardware con modifiche minime o nulle nel codice, pur avendo la potenza e la velocità delle applicazioni native. Sebbene **Qt** sia stato originariamente scritto in *C++*, ci sono diversi porting in altri linguaggi: *[PyQt](https://learnxinyminutes.com/docs/pyqt/)*, *QtRuby*, *PHP-Qt*, etc.

**Qt** è ottimo per la creazione di applicazioni con interfaccia utente grafica (GUI). Questo tutorial descrive come farlo in *C++*.

```c++
/*
 * Iniziamo classicamente
 */

// tutte le intestazioni dal framework Qt iniziano con la lettera maiuscola 'Q'
#include <QApplication>
#include <QLineEdit>

int main(int argc, char *argv[]) {
	  // crea un oggetto per gestire le risorse a livello di applicazione
    QApplication app(argc, argv);

    // crea un widget di campo di testo e lo mostra sullo schermo
    QLineEdit lineEdit("Hello world!");
    lineEdit.show();

    // avvia il ciclo degli eventi dell'applicazione
    return app.exec();
}
```

La parte relativa alla GUI di **Qt** riguarda esclusivamente *widget* e le loro *connessioni*.

[LEGGI DI PIÙ SUI WIDGET](http://doc.qt.io/qt-5/qtwidgets-index.html)

```c++
/*
 * Creiamo un'etichetta e un pulsante.
 * Un'etichetta dovrebbe apparire quando si preme un pulsante.
 * 
 * Il codice Qt parla da solo.
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

    QLabel textLabel("Grazie per aver premuto quel pulsante");
    layout.addWidget(&textLabel);
    textLabel.hide();

    QPushButton button("Premimi");
    layout.addWidget(&button);
    
    // mostra l'etichetta nascosta quando viene premuto il pulsante
    QObject::connect(&button, &QPushButton::pressed,
                     &textLabel, &QLabel::show);

    return app.exec();
}
```

Si noti la parte relativa a *QObject::connect*. Questo metodo viene utilizzato per connettere *SEGNALI* di un oggetto agli *SLOTS* di un altro.

**I SEGNALI** vengono emessi quando certe cose accadono agli oggetti, come il segnale *premuto* che viene emesso quando l'utente preme sull'oggetto QPushButton.

**Gli slot** sono *azioni* che potrebbero essere eseguite in risposta ai segnali ricevuti.

[LEGGI DI PIÙ SU SLOT E SEGNALI](http://doc.qt.io/qt-5/signalsandslots.html)


Successivamente, impariamo che non possiamo solo usare i widget standard, ma estendere il loro comportamento usando l'ereditarietà. Creiamo un pulsante e contiamo quante volte è stato premuto. A tale scopo definiamo la nostra classe *CounterLabel*. Deve essere dichiarato in un file separato a causa dell'architettura Qt specifica.

```c++
// counterlabel.hpp

#ifndef COUNTERLABEL
#define COUNTERLABEL

#include <QLabel>

class CounterLabel : public QLabel {
    Q_OBJECT  // Macro definite da Qt che devono essere presenti in ogni widget personalizzato
    
public:
    CounterLabel() : counter(0) {
        setText("Il contatore non è stato ancora aumentato");  // metodo di QLabel
    }

public slots:
    // azione che verrà chiamata in risposta alla pressione del pulsante
    void increaseCounter() {
        setText(QString("Valore contatore: %1").arg(QString::number(++counter)));
    }

private:
    int counter;
};

#endif // COUNTERLABEL
```

```c++
// main.cpp
// Quasi uguale all'esempio precedente

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

    QPushButton button("Premimi ancora una volta");
    layout.addWidget(&button);
    QObject::connect(&button, &QPushButton::pressed,
                     &counterLabel, &CounterLabel::increaseCounter);

    return app.exec();
}
```

Questo è tutto! Ovviamente, il framework Qt è molto più grande della parte che è stata trattata in questo tutorial, quindi preparatevi a leggere e fare pratica.

## Ulteriori letture

- [Qt 4.8 tutorials](http://doc.qt.io/qt-4.8/tutorials.html)
- [Qt 5 tutorials](http://doc.qt.io/qt-5/qtexamplesandtutorials.html)

Buona fortuna e buon divertimento!
