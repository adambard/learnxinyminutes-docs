*--
language: QML
filename: learnQml.qml
contributors:
    - ["Savenkov Igor", "https://github.com/SavenkovIgor"]
*--

**QML** is a declarative language that allows user interfaces to be described in
terms of their visual elements and how they interact with each other.
It's a part of the [Qt framework](https://learnxinyminutes.com/docs/qt/), and
it's great for cross-platform application development.

Best way to learn QML is to make quick and simple experiments with it.
Tiny pet-projects and Qt documentation are your best friends.

All QML examples in this tutorial have been tested with Qt 6.5.
To play with them, create a new Qt Quick Application template project in
QtCreator and replace the contents of the Main.qml file with the example code.

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

Some elements are visual, like `Rectangle`, `Text`, `Image`, etc.

Others are non-visual and provide only logic, like `Timer`, `ListModel`,
`QtObject`, etc.

All visual elements are inherited from `Item` element and therefore have a set
of common properties, like `x`, `y`, `width`, `height`, `visible`, `enabled`,
`opacity`, `rotation`, `scale`, `z`, etc.

Also visual elements have a `parent` and `children` properties,
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
known as `Components`, are reusable and can be nested within other elements.

This is a powerful feature that allows to build complex user interfaces out of
simpler components.

It could be done in three ways:

- Defining a custom element in a separate QML file
- Defining a element in same QML file with Component/Loader elements
- Defining an inline element in same QML file

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

A `Component` is a special non-visual element that can wrap other elements,
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

## Inline components

Also you can create inline components that are defined in same QML file where
they are used. They are different from work of Component element and mostly
used not for controlling creation time of elements, but for customizing some
frequently used elements 'in place'.

Almost same example as above, but with inline component:

```qml
import QtQuick
import QtQuick.Window

Window {
    visible: true

    // This is an inline component of green circle
    component Circle: Rectangle {
        width: 100
        height: 100
        radius: 50
        color: "green"
    }

    // First circle with green color
    Circle { }

    // Second circle with overridden color
    Circle { x: 50; y: 50; color: "yellow"; }

    // Third red circle
    Circle { x: 100; y: 100; color: "red"; }

    // You still can combine it with Loader if you need
    Loader {
        x: 150; y: 150;
        sourceComponent: Circle {}
    }
}
```

As you can see, inline components are very similar to regular Components,
but short and lightweight.

# Sizing and Positioning

Sizing and positioning in QML allow you to control the layout of your elements
precisely.

QML provides four main ways of defining the size and position of items:

- specifying `x`, `y`, `width`, and `height`
- using `anchors`
- using layout items such as `Column`/`ColumnLayout` or `Row`/`RowLayout`

## Positioning and Sizing by Hand

QML elements have `x`, `y`, `width`, and `height` properties that allow manual
positioning and sizing of elements.

The `x` and `y` properties determine the top-left corner of the element relative
to its parent, while `width` and `height` control the size.

```qml
import QtQuick
import QtQuick.Window

Window {
    visible: true
    width: 500
    height: 500

    Rectangle {
        x: 50
        y: 50
        width: 400
        height: 200
        color: "green"
    }
}
```

In this example, a green `Rectangle` is placed 50 units from the top and left
edges of the window, and it's sized 400x200 units.

## Positioning and Sizing with Anchors

Anchors provide a way to define relationships between the edges or center points
of items. They are used in style of sticking some key points of one element to
some key points of another element.

Anchors work only between sibling or direct parent-child elements. You can't
anchor elements that are not related.

Anchors can be used to define both the position and size of items.

```qml
import QtQuick
import QtQuick.Window

Window {
    visible: true
    width: 500
    height: 500

    // Small rectangle with random color
    component Rc: Rectangle {
        readonly property var colors: ["cyan", "aquamarine", "coral", "lime"]
        width: 30
        height: 30
        color: colors[Math.floor(Math.random() * colors.length)]
    }

    // Background rectangle
    // anchors.fill: parent - fill the whole parent size
    Rc {
        color: "#eee"
        anchors.fill: parent
    }

    // Rectangle in left-top corner of window with 2 anchors
    Rc {
        anchors.left: parent.left
        anchors.top: parent.top
    }

    // Rectangle in right-top corner
    Rc {
        anchors.top: parent.top
        anchors.right: parent.right
    }

    // Rectangles in left-bottom and right-bottom corners
    // You also can define anchors in one line
    Rc { anchors { left: parent.left;     bottom: parent.bottom; } }
    Rc { anchors { bottom: parent.bottom; right:  parent.right;  } }

    // Rectangle anchored to center of window
    Rc {
        id: centralRect
        anchors.centerIn: parent
    }

    // Rectangle anchored to right of central rectangle
    // Anchors has margins and separate properties for
    // horizontal and vertical centering
    // Here we use centralRect id to create sibling anchoring
    Rc {
        anchors.left: centralRect.right
        anchors.leftMargin: 10
        anchors.verticalCenter: centralRect.verticalCenter
    }

    // Simultaneous anchoring to opposite sides of parent
    // would lead to stretching of rectangle with parent resize
    // In this case on window resize.
    Rc {
        width:  1;
        anchors {
            top: parent.top;
            bottom: parent.bottom;
            horizontalCenter: parent.horizontalCenter;
        }
    }

    Rc {
        height: 1;
        anchors {
            left: parent.left;
            right: parent.right;
            verticalCenter: parent.verticalCenter;
        }
    }
}
```

Simultaneous anchoring and sizing by hand are possible only when sizing
strategies doesn't conflict. For example, if you anchoring and sizing by hand
along different axes, it will work. But usually it's better to use only one way

## Positioning with Layouts

Layouts allow arranging items in a column, row, or grid. Layout items
automatically position and possibly resize their child items.

You can use `Column`, `Row`, or `Grid` for simple layouts, and `ColumnLayout`, `RowLayout`, or `GridLayout` for more complex ones.

```qml
import QtQuick
import QtQuick.Window

Window {
    visible: true
    width: 500
    height: 500

    // Column by default aligns it's children to left
    // Column by itself would have height of sum of its children + spacings
    // and width of the widest child
    Column {
        spacing: 10

        Rectangle {
            width: 200
            height: 100
            color: "red"
        }

        Rectangle {
            width: 180
            height: 70
            color: "yellow"
        }

        // Aligned to the right with anchor.
        // Because this anchor acts along another axis than Column,
        // it doesn't conflict with Column's layout
        Rectangle {
            width: 160
            height: 100
            anchors.right: parent.right
            color: "purple"
        }
    }
}
```

Don't mix this with anchors on the same nesting level and along the same axis.
Columns and Rows don't like it.

In this example, `Column` is used to arrange three `Rectangle` items vertically
with 10 units of space between them. The `Column` automatically positions its
children along its vertical axis. The `width` and `height` of each `Rectangle`
are manually specified.

# Data-Driven Layouts

Data-driven layouts in QML are useful when you have a set of similar items
to display, possibly backed by dynamic data. This is where `ListView`,
`GridView`, `TableView`, and `TreeView` come in.
They provide efficient and flexible ways to present and interact with large
datasets.

- `ListView`: Presents data items in a scrollable vertical list.
- `GridView`: Lays out data items in a grid within the available space.
- `TableView`: Displays data from a model as a table. Each row corresponds to
a model item and each column represents a data field.
- `TreeView`: Allows presentation of data from a model where each item can
have a tree of child items.

Here's an example of `ListView`:

```qml
import QtQuick
import QtQuick.Controls
import QtQuick.Window

Window {
    visible: true
    width: 200
    height: 500

    // ListView is a view that presents data from a model
    // in a scrollable list of items
    ListView {
        anchors.fill: parent

        // It takes data from model property
        // Data could be in ListModel format or js array
        // or inner QAbstractItemModel
        model: ListModel {
            ListElement { name: "Alice";   age: 25 }
            ListElement { name: "Bob";     age: 30 }
            ListElement { name: "Charlie"; age: 35 }
        }

        // Delegate describes what element should be created for each
        // model item
        // Here we create a Control with Text inside
        // and lightgrey background
        // You can read about Control in qt documentation
        // They are widely used in QML
        delegate: Control {
            width: parent.width
            height: 50

            contentItem: Text {
                anchors.centerIn: parent
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment:   Text.AlignVCenter
                text: model.name + ", " + model.age
            }

            background: Rectangle { color: "lightgrey" }
        }
    }
}
```

This `ListView` presents a list of names and ages. The `model` specifies the
data to be displayed, and the `delegate` describes how to present an individual
item of data from the model. In this case, each item is a `Control` with a
`Text` item inside it.

The `model.name` and `model.age` properties are implicitly created by the
`ListView` for each delegate on initialization. These properties contains data
of the current item in the model.

The result is a scrollable list of names and ages.

In summary, QML offers flexible ways to size and position your elements,
whether you need precision control or automatic layout. You can use these
properties and mechanisms to create complex and responsive UI layouts.

That's it for a basic introduction to QML. Of course, there's a lot more to
learn, like Animations, States, Shapes, attached and default properties, etc.
But this should give you a good starting point.

Happy coding!

## Further reading

- A more detailed introduction to QML in [Qt6 Qml Book](https://www.qt.io/product/qt6/qml-book)
- And of course, the [Qt documentation](https://doc.qt.io/qt-6/qmlapplications.html)
