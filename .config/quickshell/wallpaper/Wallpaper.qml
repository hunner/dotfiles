import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import QtQuick

Scope {
    id: root

    property var wallpapers: []

    function screenIndex(screen) {
        var screens = [];

        for (var i = 0; i < Quickshell.screens.length; i++) {
            if (Quickshell.screens[i]) {
                screens.push(Quickshell.screens[i]);
            }
        }

        screens.sort((a, b) => a.x === b.x ? a.y - b.y : a.x - b.x);

        for (var j = 0; j < screens.length; j++) {
            if (screens[j].name === screen.name) {
                return j;
            }
        }

        return 0;
    }

    function sourceForScreen(screen) {
        if (screen === null || typeof screen === "undefined" || root.wallpapers.length === 0) {
            return "";
        }

        return "file://" + root.wallpapers[root.screenIndex(screen) % root.wallpapers.length];
    }

    Process {
        id: listWallpapers
        command: [
            "find",
            "/home/hunner/Pictures/wallpapers",
            "-maxdepth", "1",
            "-type", "f",
            "-regextype", "posix-extended",
            "-iregex", ".*\\.(jpe?g|png|gif)"
        ]
        running: true

        stdout: StdioCollector {
            id: collector

            onStreamFinished: {
                var paths = collector.text.trim().split("\n").filter(path => path.length > 0);
                root.wallpapers = paths.sort(() => Math.random() - 0.5);
            }
        }
    }

    Variants {
        model: Quickshell.screens

        PanelWindow {
            required property var modelData

            screen: modelData
            anchors { top: true; bottom: true; left: true; right: true }
            exclusionMode: ExclusionMode.Ignore

            WlrLayershell.layer: WlrLayer.Background
            WlrLayershell.namespace: "wallpaper"

            Image {
                anchors.fill: parent
                source: root.sourceForScreen(modelData)
                fillMode: Image.PreserveAspectCrop
            }
        }
    }
}
