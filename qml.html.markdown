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

import QtQuick         // This is a modules import
import QtQuick.Window

// This is a Window element. All elements start with uppercase.
// Qml document can only have one root element (at zero nesting level)
// If Qml document is directly loaded by the Qml engine,
// it's root element must be a Window element
Window {
    // This is a bool property `visible` of Window element
    // All properties start with lowercase
    // By default it's false, so we override it here to make the window visible
    visible: true

    // This is a Text element from QtQuick module
    // Qml elements can be nested inside each other and form a tree
    // And so the Window and Text elements are related as parent and child
    Text {
        // It's a string property `text` and it sets the text
        // to be displayed in the Text element
        text: "Hello, world!"
    }
}
```

# Elements and Properties

```qml
import QtQuick
import QtQuick.Window
// In case of QT5 you also need an import version number
// (e.g. import QtQuick 2.15)

Window {
    visible: true
    // This is a Rectangle element (from QtQuick)
    // It's a visual element with a set of its own properties
    Rectangle {
        width: 200    // property and its value are separated by a colon
        height: 200   // The order of properties doesn't matter
        color: "cyan" // Colors can be given as strings or as RGB values
    }
}
```

Elements are the basic building blocks of QML. Each element has a set of
properties that defines its behavior.

Some elements are visual, like **Rectangle**, **Text**, **Image**, etc. Others
are non-visual and provide only logic, like **Timer**, **ListModel**,
**QtObject**, etc.

All visual elements are inherited from **Item** element and therefore have a set
of common properties, like **x**, **y**, **width**, **height**, **visible**,
**enabled**, **opacity**, **rotation**, **scale**, **z**, etc.

Also visual elements have a **parent** and **children** properties,
which are used to build a tree of elements.

```qml
import QtQuick
import QtQuick.Window

Window {
    visible: true

    Rectangle {
        // property can be set to a constant value
        // and updated only when explicitly assigned
        width: 200

        // the property can be bound to other properties
        // and will be automatically updated on each dependency change
        // This is powerful magic and should be used responsibly
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
}
```

Qt properties are a special kind of class members based on the QObject system.
They has exact type, can be read and written and have a notification signal
(\<propertyName\>Changed), that is emitted when the property value changes.

By default all element properties are public and can be accessed from outside.

```qml
import QtQuick
import QtQuick.Window

Window {
    visible: true

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

# Custom Elements, Components and Nesting

You can create new element out of existing ones by defining a QML file
that describes it's interface and implementation. These custom elements, also
known as **Components**, are reusable and can be nested within other elements.

This is a powerful feature that allows to build complex user interfaces out of
simpler components.

## Defining a Custom Element

To create new element the easy way, you should create a QML file and put it
near your main QML file, where you want to use it.

The filename (without the .qml extension) would be the element's type name,
and it must start with an uppercase letter. The root item in this file defines
the properties, functions, and signals that are part of the element's interface
and would be accessible from outside.

```qml
// Filename: MyButton.qml
import QtQuick

// Rectangle is the root element of our custom element
// All properties, signals etc. of Rectangle will be part of MyButton interface
// and also be accessible from outside
Rectangle {
    // We give a root element an id to access it from inside
    // Because root element is responsible for the whole element interactions,
    // it's a common practice to give it id. It's not required, but recommended.
    id: root

    width: 200
    height: 100
    color: "cyan"

    // We add a text property that can be set from outside
    property string text: "Default Text"

    // We add a clicked signal that we will emit from inside,
    // but it could be handled from outside.
    // This helps to make some level of abstraction and encapsulation
    signal clicked

    Text {
        // This thing just makes the text to fill size of the parent
        // Would be explained later
        anchors.centerIn: parent
        // Here we bind text property of Text element to text property of
        // root element
        text: root.text
    }

    MouseArea {
        // Because it is a second child of root element, it will be drawn
        // over the Text element. Without special layouting, all siblings
        // are drawn in order of their declaration in left top corner of
        // parent element.

        // Also make the area fill the whole element
        anchors.fill: parent

        // Here we handle clicked signal of MouseArea and emit clicked signal
        // of root element
        onClicked: root.clicked()
    }
}
```

## Using Custom Elements

Once a custom element has been defined in a QML file, it can be used in other
QML files.

```qml
import QtQuick
import QtQuick.Window

Window {
    visible: true

    // Here we use the custom element MyButton
    // We set its text property and add a handler for its clicked signal
    MyButton {
        text: "Click me!"
        onClicked: console.log("Button was clicked!")
    }
}
```

## Creating Components

A **Component** is a special non-visual element that can wrap other elements,
configure them and define the time of their creation.
It is useful for simple elements that don't need a separate QML file and would
be reused only in the same QML file. Or in case when you need to create a lot
of similar elements.

```qml
import QtQuick
import QtQuick.Window

Window {
    visible: true

    // This is an Component of green circle
    Component {
        id: myButton

        Rectangle {
            width: 100
            height: 100
            radius: 50
            color: "green"
        }
    }

    // Here we use a Loader element to create an instance of the element,
    // wrapped by the Component
    Loader {
        sourceComponent: myButton
    }
    // Here we create another instance of the same element
    // but place it lower
    Loader {
        y: 120
        sourceComponent: myButton
    }
    // And just as a syntax example, you can write it on one line
    // but split properties with semicolon
    Loader { y: 240; sourceComponent: myButton; }
}
```

[//]: <> (# Basic visual elements)

[//]: <> (# sizing and positioning)

[//]: <> (# Advanced QML)

That's it for a basic introduction to QML. Of course, there's a lot more to
learn, but this should give you a good starting point. Happy coding!

Further reading:

- A more detailed introduction to QML in [Qt6 Qml Book](https://www.qt.io/product/qt6/qml-book)
