import QtQuick 2.0
import MuseScore 1.0

// Find duplicate phrases within a piece of music.
// Possibly extend this 
MuseScore {
    menuPath:      "Plugins.repeatFinder"
    version:       "0.01"
    description:   "Find repeated musical phrases"
    // requiresScore: true

    pluginType:    "dialog" // vs. dock
    // dockArea:      "left"

    width:  150
    height: 75

    onRun: {
        console.log(qsTr("Repeat Finder"));
        if (typeof curScore === 'undefined')
            Qt.quit();
    }

    Rectangle {
        color: "white"
        anchors.fill: parent

        Text {
            anchors.centerIn: parent
            text: qsTr("Phrase Finder")
        }
        MouseArea {
            anchors.fill: parent
            onClicked: Qt.quit()
        }
    }
}


//    GroupBox {
//        id: instrumentGroup
//        title: qsTr("Instruments")
//        Layout.fillWidth: true
//        Column {
//            // ExclusiveGroup { id: instrumentSelectionGroup }
//            RadioButton {
//                text: "All instruments"
//                checked: true
//                // exclusiveGroup: instrumentSelectionGroup
//            }
//            RadioButton {
//                text: "Select instruments"
//                // exclusiveGroup: instrumentSelectionGroup
//            }
//        }
//    }


//        GroupBox {
//            // title: "Bars"
//            ExclusiveGroup { id: barSearchGroup }
//            RowLayout {
//                text: "Search all bars"
//                checked: true
//                exclusiveGroup: barSearchGroup
//            }
//            RowLayout {
//                text: "Select range"
//                exclusiveGroup: barSearchGroup
//            }
//        }


//        GroupBox {
//            // title: "Phrase Duration"
//            // ExclusiveGroup { id: phraseDurationGroup }
//            ColumnLayout {
//                text: "Min duration (bars)"
//                SpinBox {
//                    id: minDuration
//                }
//                // exclusiveGroup: phraseDurationGroup
//            }
//            ColumnLayout {
//                text: "Max duration (bars)"
//                SpinBox {
//                    id: maxDuration
//                }
//                // exclusiveGroup: phraseDurationGroup
//            }
//        }
