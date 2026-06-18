import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import QtQuick

Scope {
    id: root
    property int batteryPercent: 0
    property string batteryStatus: "Unknown"
    property string currentTime: Qt.formatDateTime(new Date(), "HH:mm")

    readonly property color normalColor: "#379"
    readonly property color chargingColor: "#99ff00"
    readonly property color lowColor: "#dd4747"
    readonly property int lowBattery: 15

    property color barColor: {
        if (batteryStatus === "Charging") return chargingColor;
        if (batteryPercent <= lowBattery) return lowColor;
        return normalColor;
    }

    function leftmostScreenX() {
        var best = null;

        for (var i = 0; i < Quickshell.screens.length; i++) {
            var screen = Quickshell.screens[i];
            if (screen && (best === null || screen.x < best)) {
                best = screen.x;
            }
        }

        return best;
    }

    function isLeftmostScreen(screen) {
        return screen !== null
            && typeof screen !== "undefined"
            && screen.x === root.leftmostScreenX();
    }

    FileView {
        id: capacityFile
        path: "/sys/class/power_supply/BAT1/capacity"
        onTextChanged: {
            root.batteryPercent = parseInt(capacityFile.text().trim()) || 0;
        }
    }

    FileView {
        id: statusFile
        path: "/sys/class/power_supply/BAT1/status"
        onTextChanged: {
            root.batteryStatus = statusFile.text().trim();
        }
    }

    Timer {
        interval: 15000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: {
            capacityFile.reload();
            statusFile.reload();
        }
    }

    Variants {
        model: Quickshell.screens

        PanelWindow {
            required property var modelData

            screen: modelData
            visible: root.isLeftmostScreen(modelData)
            anchors { top: true; right: true }
            margins { top: 6; right: 8 }
            implicitWidth: 112
            implicitHeight: 12
            exclusionMode: ExclusionMode.Ignore

            WlrLayershell.layer: WlrLayer.Overlay
            WlrLayershell.namespace: "battery-widget"

            Rectangle {
                anchors.fill: parent
                color: "#000"

                Row {
                    anchors.fill: parent
                    anchors.margins: 2
                    spacing: 4

                    Text {
                        color: "#ddd"
                        font.family: "Liberation Mono"
                        font.pixelSize: 11
                        anchors.verticalCenter: parent.verticalCenter
                        text: root.currentTime
                    }

                    Rectangle {
                        width: 72
                        height: 10
                        anchors.verticalCenter: parent.verticalCenter
                        color: "#933"

                        Rectangle {
                            width: parent.width * (root.batteryPercent / 100)
                            height: parent.height
                            color: root.barColor
                        }
                    }
                }
            }
        }
    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        onTriggered: {
            root.currentTime = Qt.formatDateTime(new Date(), "HH:mm");
        }
    }
}
