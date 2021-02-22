---
category: tool
tool: Qt Framework
language: c++
filename: learnqt-pt.cpp
contributors:
    - ["Aleksey Kholovchuk", "https://github.com/vortexxx192"]
translators:
    - ["Lucas Pugliesi", "https://github.com/fplucas"]
lang: pt-br
---

**Qt** é amplamente conhecido como um framework para desenvolvimento de
software multi-plataforma que pode rodar em vários outras plataformas de
softwares e hardwares com pouca ou nenhuma alteração no código, enquanto mantém
o poder e a velocidade de uma aplicação nativa. Embora o **Qt** tenha sido
originalmente escrito em *C++*, é possível utilizá-lo em outras linguagens:
*[PyQt](https://learnxinyminutes.com/docs/pyqt/)*, *QtRuby*, *PHP-Qt*, etc.

**Qt** é ótimo para criar aplicações com interface gráfica (GUI). Esse tutorial
será feito em *C++*.

```c++
/*
 * Vamos começar
 */

// Todos as dependências do framework Qt iniciam com a letra 'Q' maiúscula
#include <QApplication>
#include <QLineEdit>

int main(int argc, char *argv[]) {
	 // Cria um objeto para utilizar todos os recursos da aplicação
    QApplication app(argc, argv);

    // Cria um widget com linha editável e exibe na tela
    QLineEdit lineEdit("Hello world!");
    lineEdit.show();

    // Inicia a aplicação em um evento de loop
    return app.exec();
}
```

A parte gráfica do **Qt** é toda composta de *widgets* e *conexões* entre eles.

[LEIA MAIS SOBRE WIDGETS](http://doc.qt.io/qt-5/qtwidgets-index.html)

```c++
/*
 * Vamos criar um label e um botão.
 * Um label irá aparecer quando o botão for clicado
 *
 * O próprio código do Qt é autoexplicativo.
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

    // Adiciona um layout vertical
    QVBoxLayout layout;
    dialogWindow.setLayout(&layout);

    QLabel textLabel("Thanks for pressing that button");
    layout.addWidget(&textLabel);
    textLabel.hide();

    QPushButton button("Press me");
    layout.addWidget(&button);

    // Exibe o label oculto quando o botão é clicado
    QObject::connect(&button, &QPushButton::pressed,
                     &textLabel, &QLabel::show);

    return app.exec();
}
```

Veja o *QObject::connect*. O método é usado para conectar o *SINAL* de um objeto
ao *ENCAIXE* outro.

**Sinais** são emitidos quando algo ocorre com o objeto, como quando o sinal de
*clique* é acionado apertando o QPushButton.

**Encaixes** são *ações* que são executadas em resposta aos sinais recebidos.

[LEIA MAIS SOBRE SINAIS E ENCAIXES](http://doc.qt.io/qt-5/signalsandslots.html)


A seguir vamos aprender como usar não somente o comportamento padrão dos
widgets, mas também extender seus comportamentos usando herança. Vamos criar um
botão e contar quantas vezes é pressionado. Para esse propósito definiremos
nossa própria classe *CounterLabel*. Ela deve ser declarada em um arquivo
diferente devido a estrutura específica do Qt.

```c++
// counterlabel.hpp

#ifndef COUNTERLABEL
#define COUNTERLABEL

#include <QLabel>

class CounterLabel : public QLabel {
    Q_OBJECT  // Define os macros presente em todo objeto Qt

public:
    CounterLabel() : counter(0) {
        setText("Counter has not been increased yet");  // método do QLabel
    }

public slots:
    // Ação que será chamada em resposta ao clique do botão
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
// Quase igual ao exemplo anterior

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

É isso! Claro, o framework Qt é muito maior do que exemplificamos no tutorial,
então esteja preparado para ler e praticar mais.

## Leitura complementar

- [Tutoriais Qt 4.8](http://doc.qt.io/qt-4.8/tutorials.html)
- [Tutoriais Qt 5](http://doc.qt.io/qt-5/qtexamplesandtutorials.html)

Boa sorte e divirta-se!
