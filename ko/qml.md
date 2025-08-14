---
name: QML
contributors:
    - ["Furkan Uzumcu", "https://zmc.space/"]
filename: learnqml.qml
---

```qml
// 이것은 완전히 유효한 QML 파일이며, 내용을 *.qml 파일에 복사하면 `qmlscene`을 사용하여 실행할 수 있습니다.
// 주석은 이중 슬래시로 시작합니다.
/* 또는
   여러 줄
   주석을
   사용할 수 있습니다.
 */

// import 문 구문은 다음과 같습니다.
// import ${MODULE_NAME} [${VERSION_NUMBER}] [as ${QUALIFIER}]
import QtQuick 2.15
import QtQuick.Window 2.15
import QtQuick.Controls 2.15 as QQC
import QtQuick.Layouts 1.15
import Qt.labs.platform 1.1

// 각 QML 문서는 최상위 유형을 하나만 포함할 수 있습니다.
Window {
    // 각 객체에는 선언된 객체를 참조하는 데 사용할 수 있는 특별하고 선택적인 `id` 속성이 있습니다.
    // `id`는 동일한 문서 내에서 고유해야 합니다.
    id: root
    width: 400
    height: 600
    title: "Y분 안에 QML 배우기"

    Item {
        // 선언할 수 있는 모든 객체는 QObject에서 상속되며,
        // `objectName`이라는 속성을 하나 이상 포함합니다. 다른 모든 속성은
        // `QObject` 유형을 확장하여 추가됩니다. 이것은 `Item` 유형이며
        // 추가적인 `width` 및 `height` 속성 등을 포함합니다.
        objectName: "My Item"
        // 동일한 문서의 `id`는 동일한 파일의 어디에서나 사용할 수 있습니다.
        // 다른 파일에서는 `id`에 액세스할 수 없습니다.
        width: root.width
    }

    // 시그널은 특정 이벤트가 발생했음을 알리는 데 사용됩니다.
    // 일부 유형에는 내장 시그널이 있습니다.
    Timer {
        id: timer
        interval: 500
        onTriggered: {
            console.log("타이머가 트리거되었습니다!")
        }
    }

    QtObject {
        id: objSignals
        // 자신만의 시그널을 선언할 수도 있습니다.
        signal clicked()
        // 시그널에는 인수가 있을 수도 있습니다.
        signal mousePositionChanged(int x, int y)
        // 시그널 방출에 반응하는 방법은 시그널이 속한
        // 즉각적인 객체에 시그널 핸들러를 추가하는 것입니다.
        onClicked: () => {
            // 여기서 작업을 수행합니다.
            console.log("objSignals.clicked() 시그널이 방출되었습니다.")
        }
        // 시그널 핸들러는 인수를 명시적으로 선언해야 합니다.
        onMousePositionChanged: (x, y) => {
            // 여기서 작업을 수행합니다.
            console.log("objSignals.mousePositionChanged() 시그널이 방출되었습니다. x=", x, "y=", y)
        }
    }

    // 다른 객체에 대한 시그널 핸들러를 선언하려면
    // `Connections`를 사용할 수 있습니다.
    Connections {
        target: objSignals

        // 그런 다음 시그널 핸들러와 동일한 이름의 함수를
        // 선언할 수 있습니다.
        function onClicked() {
            console.log("objSignals.clicked() 시그널이 Connections에서 처리되었습니다.")
        }
    }

    Item {
        visible: false

        // 객체는 자식 객체를 가질 수 있습니다. 다음과 같이
        // 유형을 선언하여 자식 객체를 추가할 수 있습니다.
        Rectangle {
            width: 16
            height: 16
            color: "red"
        }
    }

    Item {
        id: objProperties
        // 자신만의 속성을 선언할 수도 있습니다.
        // 선언 구문은 다음과 같습니다.
        // [default] [required] [readonly] property ${TYPE} ${NAME}
        property color nextColor
        // 읽기 전용 속성은 선언 시 초기화해야 합니다.
        readonly property color defaultColor: "red"
        // 필수 속성은 재사용 가능한 유형이
        // 사용되는 곳에서 초기화해야 합니다.
        required property color initialColor

        // 참고: 초기 할당은 동일한 파일에서 수행할 수 있지만,
        // 일반적인 사용 사례는 아닙니다.
        initialColor: "green"

        // 속성은 유형에 안전하며 속성에는 속성 유형과
        // 일치하는 값만 할당할 수 있습니다.
        // property int volume: "four" // 오류!

        Item {
            // 다른 속성에 대한 참조를 보유하는 별칭 속성을
            // 만들 수 있습니다.

            property alias parentNextColor: objProperties.nextColor

            // 별칭 속성에 대한 할당은 참조하는 속성을
            // 변경합니다.
            parentNextColor: "blue" // objProperties.nextColor 변경
            // `parentNextColor`가 `nextColor`의 별칭이므로 `nextColor`에
            // 대한 모든 변경 사항은 `parentNextColor`에도 반영됩니다.
        }
    }

    Item {
        // 속성 할당 값은 정적이거나 바인딩 표현식일 수 있습니다.
        // 정적 값
        property int radius: 32
        // 바인딩 표현식은 속성과 다른 속성 간의 관계를 설명합니다.
        // `radius` 값이 변경되면 여기의 표현식이
        // 다시 평가됩니다.
        property int diameter: radius * 2

        onDiameterChanged: {
            console.log("onDiameterChanged:", diameter)
        }
    }

    ListView {
        // 연결된 속성 및 시그널 핸들러는 기존 객체를 확장하고
        // 즉시 사용할 수 없는 추가 정보를 제공하는 방법을 제공합니다.
        width: 100
        height: 30
        model: 3
        delegate: Rectangle {
            // ListView는 자식에 대해 더 많은 정보에 액세스하는 데
            // 사용할 수 있는 연결된 속성을 제공합니다.
            color: ListView.isCurrentItem ? "green" : "red"
        }
        // 연결된 유형에는 시그널 핸들러가 있을 수도 있습니다.
        // `Component`는 QML에서 사용할 수 있는 모든 유형에 연결됩니다.
        Component.onCompleted: {
            console.log("이 시그널 핸들러는 객체가 생성된 후 호출됩니다.")
        }
    }

    Rectangle {
        // 이 사각형은 ListView에서 생성되지 않았으므로 연결된
        // 유형을 사용할 수 없습니다.
        color: ListView.isCurrentItem ? "green" : "red"
    }

    QtObject {
        id: calculator

        // 객체는 메서드를 선언할 수도 있습니다. 함수 선언은
        // 인수를 주석으로 달거나 인수가 없을 수도 있습니다.
        function add(a: int, b: int): int {
            // 줄 끝의 세미콜론은 선택 사항입니다.
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
        // 메서드는 바인딩 표현식으로도 사용할 수 있습니다. `width`가
        // 변경되면 바인딩 표현식이 평가되고 `multiply`가 호출됩니다.
        height: calculator.multiply(width, 0.5)
        opacity: calculateOpacity()

        function calculateOpacity() {
            // 함수 선언에 다른 속성에 대한 참조가 포함된 경우
            // 해당 속성에 대한 변경 사항도 바인딩
            // 평가를 트리거합니다.
            return height < 50 ? 0.5 : 1
        }
    }

    // 대문자 이름으로 시작하는 각 QML 파일은 재사용 가능한
    // 구성 요소를 선언합니다(예: "RedRectangle.qml").
    // 또한 재사용 가능한 구성 요소를 인라인으로 선언할 수 있습니다.
    component RedRectangle: Rectangle {
        color: "red"
    }

    // 이 인라인 구성 요소는 동일한 파일에서 사용하거나 다른
    // 파일에서 유형 이름 앞에 속한 파일 이름을 접두사로 붙여 사용할 수 있습니다.
    //
    // ${FILE_NAME}.RedRectangle { }
    // 또는
    RedRectangle {
    }

    // QML은 열거형 선언도 지원합니다.
    component MyText: Text {
        enum TextType {
            Normal,
            Heading
        }

        // 열거형 유형은 정수 속성에 할당됩니다.
        property int textType: MyText.TextType.Normal

        font.bold: textType == MyText.TextType.Heading
        font.pixelSize: textType == MyText.TextType.Heading ? 24 : 12
    }

    // ----- 대화형 영역

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
                    text: "타이머를 시작하려면 클릭하십시오.\n로그를 확인하십시오!"
                    wrapMode: QQC.Label.WordWrap
                }

                QQC.Button {
                    text: timer.running ? "타이머 실행 중" : "타이머 시작"
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
                    text: "objSignals.clicked() 시그널을 방출하려면 클릭하십시오."
                    wrapMode: QQC.Label.WordWrap
                }

                QQC.Button {
                    property int emissionCount: 0

                    text: emissionCount + "번 방출됨."
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
                    text: "objSignals.mousePositionChanged() 시그널을 방출하려면 클릭하십시오."
                    wrapMode: QQC.Label.WordWrap
                }

                QQC.Button {
                    property int emissionCount: 0

                    text: emissionCount + "번 방출됨."
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
                    text: "nextColor 속성을 변경하려면 클릭하십시오."
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
                        text: "반지름을 변경하려면 슬라이더를 사용하십시오."
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
