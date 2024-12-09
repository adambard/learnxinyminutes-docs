---
language: QML
contributors:
    - ["Furkan Uzumcu", "https://zmc.space/"]
filename: learnqml.qml
lang: en-us
---

```qml
// This is a completely valid QML file that you can run using `qmlscene` if you copy the contents
// into a *.qml file.
// Comments start with double forward slashes.
/* Or you
   can have
   multi line
   comments
 */

// Import statement syntax is
// import ${MODULE_NAME} [${VERSION_NUMBER}] [as ${QUALIFIER}]
import QtQuick 2.15
import QtQuick.Window 2.15
import QtQuick.Controls 2.15 as QQC
import QtQuick.Layouts 1.15
import Qt.labs.platform 1.1

// Each QML document can contain only one top level type
Window {
    // Each object has a special and optional `id` attribute that can be used to refer to the
    // declared objects. An `id` has to be unique in the same document.
    id: root
    width: 400
    height: 600
    title: "Learn QML in Y Minutes"

    Item {
        // Every object that can be declared inherits from QObject and contains at
        // least one property, which is `objectName`. All the other properties are
        // added by extending `QObject` type. This is an `Item` type and it contains
        // the additional `width` and `height` properties and more.
        objectName: "My Item"
        // `id`s in the same document can be used anywhere in the same file.
        // You cannot access an `id` from a different file.
        width: root.width
    }

    // Signals are used to communicate that a certain event happened.
    // Some types have built-in signals
    Timer {
        id: timer
        interval: 500
        onTriggered: {
            console.log("Timer triggered!")
        }
    }

    QtObject {
        id: objSignals
        // You can also declare your own signals.
        signal clicked()
        // Signals can also have arguments.
        signal mousePositionChanged(int x, int y)
        // The way to react to a signal emission is by adding signal handlers to
        // the immediate object that the signal belongs to.
        onClicked: () => {
            // Do stuff here.
            console.log("objSignals.clicked() signal is emitted.")
        }
        // Signal handlers must explicitly declare the arguments.
        onMousePositionChanged: (x, y) => {
            // Do stuff here.
            console.log("objSignals.mousePositionChanged() signal is emitted. x=", x, "y=", y)
        }
    }

    // If you want to declare signal handlers for other objects, you can use
    // `Connections`.
    Connections {
        target: objSignals

        // You can then declare functions with the same name as the signal
        // handler.
        function onClicked() {
            console.log("objSignals.clicked() signal is handled from Connections.")
        }
    }

    Item {
        visible: false

        // An object can support having child objects. You can add child objects
        // by declaring types as follows:
        Rectangle {
            width: 16
            height: 16
            color: "red"
        }
    }

    Item {
        id: objProperties
        // You can also declare your own properties.
        // Syntax for declaring is
        // [default] [required] [readonly] property ${TYPE} ${NAME}
        property color nextColor
        // Read only properties have to be initialized when declared.
        readonly property color defaultColor: "red"
        // Required properties have to be initialized where the reusable type is
        // used.
        required property color initialColor

        // NOTE: Although the initial assignment can be done in the same file,
        // it is not often the use case.
        initialColor: "green"

        // Properties are type safe and a property can only be assigned a value
        // that matches the property type.
        // property int volume: "four" // ERROR!

        Item {
            // You can create alias properties that hold a reference to another
            // property.

            property alias parentNextColor: objProperties.nextColor

            // Assignments to alias properties alter the property that it holds
            // a reference to.
            parentNextColor: "blue" // Changes objProperties.nextColor
            // Since `parentNextColor` is an alias to `nextColor`, any changes
            // to `nextColor` will also be reflected to `parentNextColor`.
        }
    }

    Item {
        // Property assignment values can either be static or binding
        // expressions.
        // Static value
        property int radius: 32
        // Binding expressions describe a property's relationship to other
        // properties. When the value of `radius` changes, the expression here
        // will be re-evaluated.
        property int diameter: radius * 2

        onDiameterChanged: {
            console.log("onDiameterChanged:", diameter)
        }
    }

    ListView {
        // Attached properties and signal handlers provide a way to extend an
        // existing object and provide more information that is otherwise not
        // immediately available.
        width: 100
        height: 30
        model: 3
        delegate: Rectangle {
            // ListView provides an attached property for its children that can
            // be used to access more information.
            color: ListView.isCurrentItem ? "green" : "red"
        }
        // Attached types can also have signal handlers.
        // `Component` is attached to every type that's available in QML.
        Component.onCompleted: {
            console.log("This signal handler is called after object is created.")
        }
    }

    Rectangle {
        // Since this rectangle is not created by the ListView, the attached
        // type is not available.
        color: ListView.isCurrentItem ? "green" : "red"
    }

    QtObject {
        id: calculator

        // Objects can also declare methods. Function declarations can annotate
        // the arguments, or have no arguments at all.
        function add(a: int, b: int): int {
            // Semicolon at the end of a line is optional.
            return a + b
        }

        function multiply(a: real, b: real): real {
            return a * b;
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: (mouse) => {
            console.log("2 + 2 =", calculator.add(2, 2))
        }
    }

    Item {
        width: 100
        // Methods can also be used as binding expressions. When `width`
        // changes, the binding expression will evaluate and call `multiply`.
        height: calculator.multiply(width, 0.5)
        opacity: calculateOpacity()

        function calculateOpacity() {
            // If the function declaration contains references to other
            // properties, changes to those properties also trigger a binding
            // evaluation.
            return height < 50 ? 0.5 : 1
        }
    }

    // Each QML file that starts with an upper case name declares a re-usable
    // component, e.g "RedRectangle.qml".
    // In addition, reusable components can be declared in-line.
    component RedRectangle: Rectangle {
        color: "red"
    }

    // This inline component can then be used in the same file, or in other
    // files by prefixing the type name with the file name that it belongs to.
    //
    // ${FILE_NAME}.RedRectangle { }
    // or
    RedRectangle {
    }

    // QML also supports enumeration declarations.
    component MyText: Text {
        enum TextType {
            Normal,
            Heading
        }

        // Enum types are assigned to integer properties.
        property int textType: MyText.TextType.Normal

        font.bold: textType == MyText.TextType.Heading
        font.pixelSize: textType == MyText.TextType.Heading ? 24 : 12
    }

    // ----- Interactive Area

    QQC.ScrollView {
        anchors.fill: parent
        contentWidth: container.implicitWidth
        contentHeight: container.implicitHeight

        Column {
            id: container
            spacing: 6

            Row {
                spacing: 2

                QQC.Label {
                    width: 200
                    anchors.verticalCenter: parent.verticalCenter
                    text: "Click to start the timer.\nCheck the logs!"
                    wrapMode: QQC.Label.WordWrap
                }

                QQC.Button {
                    text: timer.running ? "Timer Running" : "Start Timer"
                    onClicked: {
                        timer.start()
                    }
                }
            }

            Row {
                spacing: 2

                QQC.Label {
                    width: 200
                    anchors.verticalCenter: parent.verticalCenter
                    text: "Click to emit objSignals.clicked() signal"
                    wrapMode: QQC.Label.WordWrap
                }

                QQC.Button {
                    property int emissionCount: 0

                    text: "Emitted " + emissionCount + " times."
                    onClicked: {
                        objSignals.clicked()
                        emissionCount++
                    }
                }
            }

            Row {
                spacing: 2

                QQC.Label {
                    width: 200
                    anchors.verticalCenter: parent.verticalCenter
                    text: "Click to emit objSignals.mousePositionChanged() signal"
                    wrapMode: QQC.Label.WordWrap
                }

                QQC.Button {
                    property int emissionCount: 0

                    text: "Emitted " + emissionCount + " times."
                    onClicked: {
                        objSignals.mousePositionChanged(32, 32)
                        emissionCount++
                    }
                }
            }

            Rectangle {
                width: 200
                height: 80
                color: objProperties.nextColor

                QQC.Label {
                    width: 200
                    anchors.verticalCenter: parent.verticalCenter
                    text: "Click to change nextColor property."
                    wrapMode: QQC.Label.WordWrap
                }

                TapHandler {
                    onTapped: {
                        colorDialog.open()
                    }
                }

                ColorDialog {
                    id: colorDialog
                    currentColor: objProperties.initialColor
                    onColorChanged: {
                        objProperties.nextColor = color

                    }
                }
            }

            Row {
                spacing: 2

                Rectangle {
                    width: 200
                    height: 80
                    color: "red"
                    radius: radiusSlider.value

                    QQC.Label {
                        width: parent.width
                        anchors.centerIn: parent
                        text: "Use slider to change radius"
                        wrapMode: QQC.Label.WordWrap
                        horizontalAlignment: Qt.AlignHCenter
                    }
                }

                QQC.Slider {
                    id: radiusSlider
                    width: 100
                    anchors.verticalCenter: parent.verticalCenter
                    from: 0
                    to: 80
                }
            }
        }
    }
}
```
