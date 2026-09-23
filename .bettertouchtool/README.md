# Unified BetterTouchTool configuration

This preset replaces the automation currently supplied by Hammerspoon,
Karabiner-Elements, and Alfred in this repository. The generated preset is
`.bettertouchtool/dotfiles-unified.bttpreset`.

## Install

1. Install and launch BetterTouchTool, then grant Accessibility and Automation
   permissions.
2. In BetterTouchTool, enable **Settings → Scripting BTT → Command Line**, then
   restart BetterTouchTool once.
3. Run `bin/update-bettertouchtool`. It regenerates the preset and replaces the
   existing `dotfiles-unified` preset through BTT's supported CLI.
4. Make `dotfiles-unified` the master preset and disable the old Hammerspoon,
   Karabiner-Elements, and Alfred login items after confirming the shortcuts.
5. In System Settings, leave Caps Lock mapped to Caps Lock. BetterTouchTool needs
   to receive the original key to turn it into Hyper.
6. Keep BetterTouchTool's "Restore window size if dragged away" snapping option
   enabled so Hyper+Z can restore the previous frame.

After any source change, update the running configuration with one command:

```sh
bin/update-bettertouchtool
```

Use `bin/update-bettertouchtool --generate-only` for validation without changing
the running BetterTouchTool configuration.

## Main shortcuts

`Hyper` is Caps Lock held as Control+Option+Command. Tapping Caps Lock sends
Escape, matching the Karabiner configuration.

| Shortcut | Action |
| --- | --- |
| Option+Space | Toggle BetterTouchTool Launcher (Alfred replacement) |
| Hyper+, / . / E | System Settings / Finder / Emacs |
| Hyper+M / N / B / C | Mail / Outlook / Messages / Calendar |
| Hyper+P / F / T | 1Password / Firefox / Terminal |
| Hyper+U / Y / W / R | Microsoft To Do / Teams / Word / Excel |
| Hyper+J / H | Jump Desktop / Cisco AnyConnect |
| Hyper+S / K / G | Safari / Google Chrome / ChatGPT |
| Hyper+L / O / X | Preview / Obsidian / Visual Studio Code |
| Hyper+arrows | Left, right, top, or bottom half |
| Hyper+Shift+H/J/K/L | Make the focused window thinner/taller/shorter/wider |
| Hyper+= / Hyper+- | Widen / narrow the focused window |
| Hyper+Shift+= / Hyper+Shift+- | Make the focused window taller / shorter |
| Hyper+Z | Restore its previous frame |
| Hyper+; | Center the focused window |
| Hyper+' | Arrange four visible windows |
| Hyper+Q | Toggle a system `caffeinate` process |
| Hyper+Shift+T | Toggle dark mode |
| Hyper+V | Type clipboard text into paste-blocking fields |
| Hyper+/ | Show the compact shortcut cheat sheet |
| Hyper+Shift+, or Hyper+` | Open BetterTouchTool settings |
| Hyper+Shift+R | Restart BetterTouchTool |

The Karabiner Emacs and Bash editing bindings are included globally with the
same terminal/editor/remote-desktop exclusions, plus explicit Visual Studio
Code, Microsoft Office, and Eclipse variants. The duplicated Karabiner rule
sets are intentionally emitted only once.

Jump Desktop and its web-session helper contain disabled app-specific overrides
for ordinary editing shortcuts and key sequences. This gives remote Windows
sessions the original events for chords such as Control+N and Control+P, while
Caps Lock, Hyper, and Hyper+Shift shortcuts remain active.

The Karabiner Fn mappings are preserved: Fn+F3 Mission Control, Fn+F4 Launchpad,
Fn+F5/F6 keyboard illumination, and Fn+F9 next track.

## Intentional modernization

- BetterTouchTool's Launcher supplies application, file, web, calculation,
  clipboard, and action search. The repository contains no Alfred preferences
  or workflows to migrate beyond its installation entry.
- BetterTouchTool's continuous window snapping replaces Hammerspoon's manual
  "snap to 18x18 grid" commands. Hyper+; now centers one window and Hyper+'
  arranges four. Hyper+H/J/K/L are reserved for the requested app launchers.
- Incremental resizing changes the current dimension by 1/18 (5.56%) rather
  than one absolute screen-grid cell. This behaves more consistently across
  mixed-DPI displays.
- Hammerspoon-only console/reload/preferences commands now open or restart
  BetterTouchTool.

The old source files are retained as migration reference. Do not run the old
tools at the same time as this preset; they would register overlapping hotkeys.

If shortcuts appear in BTT but do not fire, verify that BetterTouchTool has both
Accessibility and Input Monitoring permission, then quit and reopen it. App
launcher actions use absolute `.app` paths because `BTTLaunchPath` does not
resolve bundle identifiers.
