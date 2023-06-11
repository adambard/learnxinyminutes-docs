---
language: Qml
contributors:
    - ["Savenkov Igor", "https://github.com/SavenkovIgor"]
filename: learnQml.qml
---

**QML** is a declarative language that allows user interfaces to be described in
terms of their visual elements and how they interact with each other.
It's a part of the [Qt framework](https://learnxinyminutes.com/docs/qt/), and
it's great for cross-platform application development.

```qml
// This is a comment

import QtQuick  // This is a module import

Text {                     // This is a Text element from QtQuick module
    text: "Hello, world!"  // The text property sets the text to be displayed
}
```

# Elements and Properties

```qml
import QtQuick
// In case of QT5 you also need an import version number
// (e.g. import QtQuick 2.15)

// This is a Rectangle element (from QtQuick)
// It's a visual element with a set of its own properties
Rectangle {
    width: 200    // property and its value are separated by a colon
    height: 200   // The order of properties doesn't matter
    color: "blue" // Colors can be given as strings or as RGB values
}
```

Elements are the basic building blocks of QML. Each element has a set of
properties that defines its behavior.

Every QML document has a single **root** element.

Some elements are visual, like **Rectangle**, **Text**, **Image**, etc. Others
are non-visual and provide only logic, like **Timer**, **ListModel**, etc.

All visual elements are inherited from **Item** element and therefore have a set
of common properties, like **x**, **y**, **width**, **height**, **visible**,
**enabled**, **opacity**, **rotation**, **scale**, **z**, etc.

```qml
import QtQuick

Rectangle {
    // property can be set to a constant value
    // and updated only when explicitly assigned
    width: 200

    // property can be bound to other properties
    // and updated automatically on every dependency change
    height: width + 200

    // property can be bound to a JavaScript expression
    // and it will be automatically updated whenever dependencies change
    // In this case, the color property will update on every
    // change of width or height
    color: {
        if (width < 100) {
            return "red"
        } else if (height < 200) {
            return "#00FF00"
        } else {
            return Qt.rgba(0, 0, 1, 0.5)
        }
    }
}
```

Qt properties are a special kind of class members based on the QObject system.
They has exact type, can be read and written and have a notification signal
(\<propertyName\>Changed), that is emitted when the property value changes.

By default all element properties are public and can be accessed from outside.

```qml
import QtQuick

Rectangle {
    // You can give an element an id to access it from another element
    // This is not a property and has a special meaning for QML engine
    // It must be unique within the document, and cannot be an expression,
    // only a simple identifier
    id: myRectangle

    // You can add your own properties to any element
    // It can have no value, in which case it will be default-initialized
    property int myEmptyProperty

    // It could be a simple value
    property int myProperty: 100

    // Or a complex JS object
    property var myObject: { "name": "John", "age": 17 }

    // Or array
    property var myArray: [1, 2, 3, 4, 5]

    // Or even another element
    property Rectangle myCoralRectangle: Rectangle {
        width: 100
        height: 100
        color: "coral"
    }

    // Also you can add a property alias
    // It allows you to access the property of another element
    // as if it was a property of the current element
    property alias myCoralRectangleWidth: myRectangle.myCoralRectangle.width

    // You can make a readonly property, which is useful for computed properties
    readonly property real square: width * height
    readonly property bool johnCanDrive: myObject.age >= 18

    // You can also add a required property
    // It must be set when the element is created or you'll get an error
    required property string myRequiredProperty
}
```

# Signals and Handlers

QML makes it easy to respond to user interaction with signals and signal
handlers

```qml
import QtQuick
import QtQuick.Controls
import QtQuick.Window

Window {
    id: window
    width: 200
    height: 200
    visible: true

    // All Qml properties have corresponding change signals
    // named <propertyName>Changed. This signals are generated automatically
    // by the QML engine when the property created and emits on every
    // property value change.

    // For example, declaration of square property
    readonly property real square: width * height
    // also implicitly generates squareChanged signal with such signature:
    // signal squareChanged(real square)

    // You can add a handler for any signal with on<SignalName> handler syntax
    // Note: signal name must be capitalized in handler name
    onSquareChanged: console.log("Square changed to " + square)

    // This is a handler for signal widthChanged of Window width property.
    // Handler can explicitly declare its input arguments (which taken from
    // signal signature) but it's not required.
    onWidthChanged: (newWidth) => {
        console.log("Width changed to " + newWidth)
    }

    // You also can create and emit your own signals
    signal newTitle(string title)

    // Handler for newTitle signal
    onNewTitle: (title) => {
        console.log("Setting new title: " + title);
        window.title = title;
    }

    // You can also handle signals from other elements
    Button {
        text: "Click me!"

        // Here we add a handler for clicked signal of Button element
        // and emit newTitle signal of Window element
        // which will be handled by onNewTitle handler above
        onClicked: {
            window.newTitle("Hello, world!")
        }

        // The above example is just for demonstration purposes.
        // Of course, it's much easier to just write a direct assignment:
        // onClicked: window.title = "Hello, world!"
    }
}
```

[//]: <> (# Basic visual elements)

[//]: <> (# Custom Elements and nesting)

[//]: <> (# sizing and positioning)

[//]: <> (# Advanced QML)

That's it for a basic introduction to QML. Of course, there's a lot more to
learn, but this should give you a good starting point. Happy coding!

Further reading:

- A more detailed introduction to QML in [Qt6 Qml Book](https://www.qt.io/product/qt6/qml-book)
