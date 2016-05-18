import QtQuick 2.0
import MuseScore 1.0


MuseScore {
    menuPath:    "Plugins.phraseFinder"
    version:     "0.01"
    description: qsTr("Find musical phrases, starting with repeats.")
    pluginType: "dialog"

    width:  150
    height: 75
    onRun: {
        console.log(qsTr("Phrase Finder"));
        if (typeof curScore === 'undefined')
            Qt.quit();

        var score = curScore
        console.log(curScore)
        console.log(score.name)
        var m
        m = score.firstMeasure()
        while (m) {
            console.log(qsTr("measure"))
            var segment = m.first()
            while (segment) {
                var element
                element = segment.elementAt(0)
                if (element && element.type == Element.CHORD) {
                    console.log(qsTr("    element"))
                    console.log(element.beamMode)
                    if (element.beamMode == BeamMode.NO)
                        console.log("  beam no")
                    }
                segment = segment.next()
                }
            m = m.nextMeasure()
            }
        }

    Rectangle {
        color: "grey"
        anchors.fill: parent

        Text {
            anchors.centerIn: parent
            text: qsTr("Hello Qml")
            }

        MouseArea {
            anchors.fill: parent
            onClicked: Qt.quit()
            }
        }
    }
