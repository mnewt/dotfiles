import AppKit

final class IndicatorDelegate: NSObject, NSApplicationDelegate {
    private var statusItem: NSStatusItem?

    func applicationDidFinishLaunching(_ notification: Notification) {
        let item = NSStatusBar.system.statusItem(withLength: NSStatusItem.squareLength)
        if let button = item.button {
            let image = NSImage(
                systemSymbolName: "cup.and.saucer.fill",
                accessibilityDescription: "Caffeinate is active"
            )
            image?.isTemplate = true
            button.image = image
            button.contentTintColor = .systemOrange
            button.toolTip = "Sleep is disabled by caffeinate"
        }
        statusItem = item
    }
}

let app = NSApplication.shared
let delegate = IndicatorDelegate()
app.delegate = delegate
app.setActivationPolicy(.accessory)
app.run()
