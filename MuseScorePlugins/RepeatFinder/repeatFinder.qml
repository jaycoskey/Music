// import Qt.labs.calendar
// import Qt.labs.controls
import Qt.labs.folderlistmodel 2.1
import Qt.labs.settings 1.0
// import Qt.labs.templates

// import Qt3D.Core
// import Qt3D.Input
// import Qt3D.Logic
// import Qt3D.Render

// import QtAudioEngine
// Not present: import QtBluetooth
// import QtCanvas3D
// import QtGraphicalEffects
// Not present: import QtLocation
// import QtMultimedia
// Not present: import QtNfc
// Not present: import QtPositioning
import QtQml 2.2 
// Not present: import QtQml.Models
import QtQuick 2.0
import QtQuick.Controls 1.1
// Not present: import QtQuick.Controls.Styles
import QtQuick.Dialogs 1.2
// import QtQuick.Extras
import QtQuick.Layouts 1.1
// Version?: import QtQuick.LocalStorage
// Not present: import QtQuick.Particles
import QtQuick.Window 2.2
// Version?: import QtQuick.XmlListModel
// import QtSensors
// import QtTest
// import QtWebSockets
// import QtWebView
// Not present: import QtWinExtras

import FileIO 1.0

import MuseScore 1.0

// Find duplicate phrases within a piece of music.
// Possibly extend this 
MuseScore {
    menuPath:      "Plugins.repeatFinder"
    version:       "0.01"
    description:   "Find repeated musical phrases"
    // requiresScore: true

    pluginType:    "dock" // vs. dialog
    dockArea:      "bottom"

    anchors.fill: parent
    // width:  1600
    // height: 1000

    onRun: {
        console.log(qsTr("Repeat Finder"));
        if (typeof curScore === 'undefined')
            Qt.quit();
    }

    Layout.fillWidth: true
    ColumnLayout {
        GroupBox {
            Layout.fillWidth: true
            Text {
                anchors.centerIn: parent 
                text: qsTr("Find repeated musical phrases")
                horizontalAlignment: Text.AlignHCenter
                width: 800
            }
        }

        GroupBox {
            id: barGroup
            Layout.fillWidth: true
            ExclusiveGroup { id: barSearchGroup }
            ColumnLayout {
                RowLayout {
                    Text {
                        text: qsTr("Bars:")
                        Layout.minimumWidth: 80
                    }
                    RadioButton {
                        text: "Search all bars"
                        checked: true
                        Layout.minimumWidth: 160
                        onClicked:
                        {
                            barRange.visible = false
                            barRangeSpacer.visible = true
                        }
                        exclusiveGroup: barSearchGroup
                    }
                    RadioButton {
                        text: "Select range"
                        onClicked:
                        {
                            barRange.visible = true 
                            barRangeSpacer.visible = false
                        }
                        exclusiveGroup: barSearchGroup
                    }
                }
                RowLayout {
                    id: barRangeSpacer
                    visible: true
                    Text {
                        text: qsTr("")
                        Layout.minimumWidth: 550
                    }
                }
                RowLayout {
                    id: barRange
                    visible: false
                    Text {
                        text: qsTr("")
                        Layout.minimumWidth: 285
                    }
                    Label {
                        text: qsTr("From bar #")
                        Layout.minimumWidth: 60
                    }
                    SpinBox {
                        id: fromBarNum
                        minimumValue: 0
                        maximumValue: 10000000
                        value: 0
                        stepSize: 1
                        Layout.minimumWidth: 60
                    }
                    Label {
                        text: qsTr("")
                        Layout.minimumWidth: 5
                    }
                    Label {
                        text: qsTr("To bar #")
                    }
                    SpinBox {
                        id: toBarNum
                        minimumValue: 0
                        maximumValue: 10000000
                        value: 9999
                        stepSize: 1
                    }
                } // RowLayout
            } // ColumnLayout
        } // GroupBox

        GroupBox {
            id: phraseDurationGroup
            Layout.fillWidth: true
            RowLayout {
                Text {
                    id: phraseDurationLabel
                    text: "Phrase duration: "
                    Layout.minimumWidth: 80
                }
                Label {
                    text: qsTr("Min duration:")
                    Layout.minimumWidth: 60
                }
                SpinBox {
                    id: minDuration
                    minimumValue: 0
                    maximumValue: 10000000
                    value: 0
                    stepSize: 1
                    Layout.minimumWidth: 60
                }
                Label {
                    text: qsTr("")
                    Layout.minimumWidth: 5
                }
                Label {
                    text: qsTr("Max duration:")
                }
                SpinBox {
                    id: maxDuration
                    minimumValue: 0
                    maximumValue: 10000000
                    value: 9999
                    stepSize: 1
                }
            } // RowLayout
        } // GroupBox

        GroupBox {
            id: accidentalGroup
            Layout.fillWidth: true
            ExclusiveGroup { id: accidentalEqualityGroup }
            RowLayout {
                Text {
                    text: qsTr("Accidentals:")
                    Layout.minimumWidth: 80
                }
                RadioButton {
                    text: qsTr("Include accientals")
                    checked: true
                    Layout.minimumWidth: 160
                    exclusiveGroup: accidentalEqualityGroup
                }
                RadioButton {
                    text: "Ignore accidentals"
                    exclusiveGroup: accidentalEqualityGroup
                }
            } // RowLayout
        } // GroupBox

        GroupBox {
            id: enharmonicGroup
            Layout.fillWidth: true
            ExclusiveGroup { id: enharmonicEqualityGroup }
            RowLayout {
                Text {
                    text: qsTr("Enharmonics:")
                    Layout.minimumWidth: 80
                }
                RadioButton {
                    text: "Treat enharmonics as equal"
                    checked: true
                    Layout.minimumWidth: 160
                    exclusiveGroup: enharmonicEqualityGroup
                }
                RadioButton {
                    text: "Treat enharmonics as distinct"
                    exclusiveGroup: enharmonicEqualityGroup
                }
            } // RowLayout
        } // GroupBox

        GroupBox {
            // Layout.fillWidth: true
            ExclusiveGroup { id: instrumentSelectionGroup }
            ColumnLayout {
                RowLayout { 
                    Text {
                        text: qsTr("Instruments:")
                        Layout.minimumWidth: 80
                    } 
                    RadioButton {
                        text: "Include all instruments"
                        x: 1000
                        checked: true
                        Layout.minimumWidth: 160
                        onClicked:
                        {
                            instrumentList.visible = false
                            instrumentListSpacer.visible = true
                        }
                        exclusiveGroup: instrumentSelectionGroup
                    }
                    RadioButton {
                        text: "Include select instruments"
                        function populateInstrumentList() {
                            instrumentModel.clear()
                            console.log("Current score has " + curScore.parts.length + " parts")
                            for(var i = 0; i < curScore.parts.length; i++) {
                                try {
                                    var part = curScore.parts[i];
                                    var instrumentId = part.instrumentId;
                                    // var partName = part.partName;
                                    // var partKnownTest = new RegExp("[^\?]");
                                    // var isPartKnown = partKnownTest.exec(partName);
                                    var name = /* isPartKnown ? partName : */ instrumentId;
                                    console.log("Instrument #" + i + " is " + name)
                                    instrumentModel.append({"name": name});
                                }
                                catch(e) {
                                    console.log("Instrument #" + i + " is unknown: " + e.message)
                                    // namelessPartList += i;
                                }
                            }
                        }
                        onClicked:
                        {
                            populateInstrumentList();
                            instrumentList.height = 13 * curScore.parts.length;
                            instrumentList.visible = true;
                            instrumentListSpacer.visible = false;
                        }
                        exclusiveGroup: instrumentSelectionGroup
                    }
                }
                RowLayout {
                    id: instrumentListSpacer
                    visible: true
                    height: 1
                    Text {
                        text: qsTr(" ")
                        Layout.minimumWidth: 550
                    }
                }

                RowLayout { 
                    id: instrumentList
                    visible: false
                    Rectangle {
                        anchors.fill: parent
                        anchors.leftMargin: 285
                        Layout.minimumWidth: 550
                        ListView {
                            id: instrumentListView
                            anchors.fill: parent
                            model: instrumentModel
                            highlight: Rectangle {
                                color:   "black"
                                radius:  5
                                opacity: 0.7
                                focus:   true 
                            }
                            // currentIndex: instrumentList.current
                            preferredHighlightBegin: 80; preferredHighlightEnd: 220
                            highlightRangeMode: ListView.ApplyRange
                            delegate:
                                Item {
                                    id: instrumentListItem
                                    height: 13
                                    Text {
                                        text: name

                                        MouseArea {
                                            id: instrumentMouseArea
                                            anchors.fill: parent
                                            hoverEnabled: true
                                            onClicked: {
                                                console.log("Instrument item clicked")
                                                instrumentListItem.ListView.view.currentIndex = index;
                                                instrumentListItem.forceActiveFocus();
                                            }
                                            onEntered: {
                                                parent.color = "#0000ff"
                                            }
                                            onExited: {
                                                parent.color = "#000000"
                                            }
                                        }
                                    }
                                }
                        }
                    }
                    ListModel {
                        id: instrumentModel
                    }
                } // RowLayout
            } // ColumnLayout
        } // GroupBox

        GroupBox {
            Layout.fillWidth: true
            Button {
                id: findRepeats
                text: qsTr("Find Repeats") 
                width: 550
                onClicked: Qt.quit() 
            } // Button
        } // GroupBox
    } // ColumnLayout
}
