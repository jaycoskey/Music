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
import QtQuick 2.2
import QtQuick.Controls 1.3
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

  // anchors.fill: parent
  width:  1600
  height: 1000

  onRun: {
    console.log(qsTr("Repeat Finder"));
    if (typeof curScore === 'undefined') {
      Qt.quit();
    }
  }

  ScrollView {
    width:  1600
    height: 1000
    verticalScrollBarPolicy: Qt.ScrollBarAlwaysOn  // BUG: No vertical ScrollBar
    Rectangle {
      // width:  1600
      // height: 1000
      ColumnLayout {
        id: repeatFinderPane
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
                id: doSearchAllBars
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
                id: doSearchSelectBars
                text: "Select range"
                onClicked:
                {
                  barRange.visible = true 
                  barRangeSpacer.visible = false
                }
                exclusiveGroup: barSearchGroup
              }
            }  // Bars RowLayout

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
                id: searchBarNumBegin
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
                id: searchBarNumEnd
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
              id: phraseMinDuration
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
              id: phraseMaxDuration
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
              id: doIncludeAccidentals
              text: qsTr("Include accientals")
              checked: true
              Layout.minimumWidth: 160
              exclusiveGroup: accidentalEqualityGroup
            }
            RadioButton {
              id: doIgnoreAccidentals
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
              id: doTreatEnharmonicsAsEqual
              text: "Treat enharmonics as equal"
              checked: true
              Layout.minimumWidth: 160
              exclusiveGroup: enharmonicEqualityGroup
            }
            RadioButton {
              id: doTreatEnharmonicsAsDistinct
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
                id: doIncludeAllInstruments
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
                id: doIncludeSelectInstruments
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
                  instrumentList.height = 18 * curScore.parts.length;
                  instrumentList.visible = true;
                  instrumentListSpacer.visible = false;
                }
                exclusiveGroup: instrumentSelectionGroup
              }  // RadioButton
            }  // RowLayout


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
              ListModel {
                id: instrumentModel
              }
              Rectangle {
                anchors.fill: parent
                anchors.leftMargin: 285
                Layout.minimumWidth: 550
                ListView {
                  id: instrumentListView
                  anchors.fill: parent
                  focus: true
                  clip: true
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
                  delegate: instrumentListDelegate
                  Component {
                    id: instrumentListDelegate

                    Item {
                      id: instrumentListItem
                      height: 17
                      RowLayout {
                        id: instrumentRow
                        CheckBox {
                          id: instrumentCheckBox
                          checked: false
                          function instrumentCheckBoxClick() {
                            instrumentModel.set( index
                                               , { "selected":
                                                     instrumentCheckBox.checked
                                                 }
                                               )
                          }
                          onClicked: instrumentCheckBoxClick()
                        }
                        Text {
                          id: instrumentName
                          text: name
                        }
                      }
                    }  // Item
                  }  // Component
                }  // ListView
              }  // Rectangle
            } // RowLayout
          } // ColumnLayout
        } // GroupBox


        GroupBox {
          Layout.fillWidth: true
          Button {
            id: findRepeats
            text: qsTr("Find Repeats") 
            width: 550
            onClicked: {
              // TODO: Add more menu options:
              //   * Paint all notes black before repeat-coloring.
              //   * Max # of repeats
              //   * Coloring options
              // ================================
              // ===== Collect info from UI =====
              // ================================
              // ===== Bars =====
                 // * doSearchAllBars
                 // * <strike>doSearchSelectBars</strike>
                 // * searchBarNumBegin
                 // * searchBarNumEnd
              var valDoSearchAllBars    = doSearchAllBars.checked;
              var valDoSearchSelectBars = doSearchSelectBars.checked;
              var valSearchBarNumBegin  = parseInt(searchBarNumBegin.value);
              var valSearchBarNumEnd    = parseInt(searchBarNumEnd.value);
              // TODO: Validate that begin <= end

              // ===== Phrase duration =====
                 // * phraseMinDuration
                 // * phraseMaxDuration
              var valPhraseMinDuration      = parseInt(phraseMinDuration.value);
              var valPhraseMaxDuration      = parseInt(phraseMaxDuration.value);
              // TODO: Validate that min <= max

              // ===== Accidentals =====
                 // * doIncludeAccidentals
                 // * <strike>doIgnoreAccidentals</strike>
              var valDoIncludeAccidentals = doIncludeAccidentals.checked;
              var valDoIgnoreAccidentals  = doIgnoreAccidentals.checked;

              // ===== Enharmonics =====
                 // * doTreatEnharmonicsAsEqual
                 // * <strike>doTreatEnharmonicsAsDistinct</strike>
              var valDoTreatEnharmonicsAsEqual    = doTreatEnharmonicsAsEqual.checked;
              var valDoTreatEnharmonicsAsDistinct = doTreatEnharmonicsAsDistinct.checked;

              // ===== Instruments =====
                 // * doIncludeAllInstruments  // TODO: Change to Within each instrument
                 // * includedInstruments      // TODO: Change to Within select instruments
              var valDoIncludeAllInstruments = doIncludeAllInstruments.checked;
              var valDoIncludeSelectInstruments = doIncludeSelectInstruments.checked;
              var includedInstruments = [];
              for (var i = 0; i < instrumentModel.count; i++) {
                var instr = instrumentModel.get(i);
                var instrName = instr.name;
                if (instr["selected"] == true) {
                  console.log("Checked: " + instrName);
                  includedInstruments.push(instrName);
                }
              }

              // ===== Dump options =====    
              console.log("Dump: valDoSearchAllBars=" + valDoSearchAllBars);
              console.log("Dump: valDoSearchSelectBars=" + valDoSearchSelectBars);
              console.log("Dump: valSearchBarNumBegin=" + valSearchBarNumBegin);
              console.log("Dump: valSearchBarNumEnd=" + valSearchBarNumEnd);
                   
              console.log("Dump: valPhraseMinDuration=" + valPhraseMinDuration);
              console.log("Dump: valPhraseMaxDuration=" + valPhraseMaxDuration);
                
              console.log("Dump: valDoIncludeAccidentals=" + valDoIncludeAccidentals);
              console.log("Dump: valDoIgnoreAccidentals=" + valDoIgnoreAccidentals);

              console.log("Dump: valDoTreatEnharmonicsAsEqual=" + valDoTreatEnharmonicsAsEqual);
              console.log("Dump: valDoTreatEnharmonicsAsDistinct=" + valDoTreatEnharmonicsAsDistinct);

              console.log("Dump: valDoIncludeAllInstruments=" + valDoIncludeAllInstruments);
              console.log("Dump: valDoIncludeSelectInstruments=" + valDoIncludeSelectInstruments);
              for (var i = 0; i < includedInstruments.length; i++)
              {
                console.log("Dump: Instrument: " + includedInstruments[i]);
              }

              // ===== Construct data structures (suffix tree(s)) =====
              var keysig = parseInt(curScore.keysig);
              console.log("keysig=" + keysig);
              // for(var partNum = 0; partNum < curScore.parts.length; partNum++) {
              // }

              function CursorStats(startStaff, endStaff, endTick, fullScore) {
                console.log("Entering CursorStats");
                var obj = {
                  startStaff : startStaff,
                  endStaff   : endStaff,
                  endTick    : endTick,
                  fullScore  : fullScore
                };
                return obj;
              }
              // See MuseScore plugin: notenames.qml
              function getCursorStats(cursor) {
                console.log("Entering getCursorStats");
                var startStaff;
                var endStaff;
                var endTick   = 0;
                var fullScore = false;
                if (cursor.segment) {
                  startStaff = cursor.staffIdx;
                  cursor.rewind(2);
                  if (cursor.tick == 0) {
                    endTick = curScore.lastSegment.tick + 1;
                  } else {
                    endTick = cursor.tick;
                  }
                  endStaff = cursor.staffIdx;
                } else {
                  startStaff = 0;
                  endStaff  = curScore.nstaves - 1;
                  fullScore = true;
                } 
                return CursorStats(startStaff, endStaff, endTick, fullScore);
              }

              var cursor = curScore.newCursor();
              cursor.rewind(1); // beginning of selection
              var cursorStats = getCursorStats(cursor)
              var startStaff  = cursorStats.startStaff;
              var endStaff    = cursorStats.endStaff;
              var endTick     = cursorStats.endTick;
              var fullScore   = cursorStats.fullScore;

              for (var staff = startStaff; staff <= endStaff; staff++) {
                console.log("Staff: " + staff);
                for (var voiceNum = 0; voiceNum < 4; voiceNum++) {
                  console.log("Voice #" + voiceNum);
                  cursor.staffIdx = staff;
                  cursor.voice    = voiceNum;
                  if (fullScore) {
                    cursor.rewind(0);
                  }
                  while (cursor.segment && (fullScore || cursor.tick < endTick)) {
                    console.log("Iterating cursor...");
                    if (cursor.element && cursor.element.type == Element.CHORD) {
                      // var text = newElement(Element.STAFF_TEXT);
                      var notes = cursor.element.notes;
                      var noteCount = notes.length;
                      console.log("staff,voice,tick,noteCount="
                        + staff + "," + voiceNum + "," + cursor.tick + "," + noteCount + "\n");
                    }
                    cursor.next();
                  }
                }
              }

/*
              for (var bar = curScore.firstMeasure; bar; bar = bar.nextMeasure) {
                console.log("BAR:")
                for (var seg = bar.firstSegment; seg; seg = seg.nextInMeasure) {
                  console.log("  Segment: ")
                  // var element = seg.elementAt(0)
                  var elem  = seg               // Often Element.SEGMENT (73)
                  var elem0 = seg.elementAt(0)  // BAR_LINE(6), CLEF(13), KEYSIG(14), TIMESIG(16), REST(17) 
                  var elem1 = seg.elementAt(1)  // Always null?
                  var elem2 = seg.elementAt(2)  // Always null?
                  var elem3 = seg.elementAt(3)  // Always null?
                  console.log("      Element: " + elem.getName() + " / " + elem.type)
                  elem0 && console.log("      Element0: " + elem0.getName() + " / " + elem0.type)
                  elem1 && console.log("      Element1: " + elem1.getName() + " / " + elem1.type)
                  elem2 && console.log("      Element2: " + elem2.getName() + " / " + elem2.type)
                  elem3 && console.log("      Element3: " + elem3.getName() + " / " + elem3.type)
                  if (elem && elem.type == Element.CHORD) {
                    console.log(qsTr("      element(chord)"))
                  }
                }
              }
*/
              // Note: Suffix tree of: Part, Chord=[Pitch, Accidental], Duration
              //       (w/ info on time=Bar/Tick)

              // ===== Find repeated phrases matching constraints =====

              // ===== Output results =====
              // TODO: Log results

              // Qt.quit()
            } // Onclicked 
          } // Button
        } // GroupBox
      } // ColumnLayout
    } // Rectangle
  } // ScrollView
} // MuseScore
