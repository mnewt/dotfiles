#!/usr/bin/env node

import { createHash } from "node:crypto";
import { writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const output = resolve(here, "dotfiles-unified.bttpreset");

const MOD = {
  shift: 131072,
  control: 262144,
  option: 524288,
  command: 1048576,
  fn: 8388608,
};

const KEY = {
  a: 0, s: 1, d: 2, f: 3, h: 4, g: 5, z: 6, x: 7, c: 8, v: 9,
  b: 11, q: 12, w: 13, e: 14, r: 15, y: 16, t: 17, equal: 24, minus: 27,
  closeBracket: 30, o: 31, u: 32, openBracket: 33, i: 34, p: 35,
  return: 36, l: 37, j: 38, quote: 39, k: 40, semicolon: 41, slash: 44,
  n: 45, m: 46, period: 47, tab: 48, space: 49, backtick: 50, backspace: 51,
  escape: 53, capsLock: 57, f5: 96, f6: 97, f3: 99, f9: 101, f4: 118,
  home: 115, pageUp: 116, forwardDelete: 117, end: 119, pageDown: 121,
  left: 123, right: 124, down: 125, up: 126, comma: 43,
};

const LAYOUT_CHAR = {
  0: "a", 1: "s", 2: "d", 3: "f", 4: "h", 5: "g", 6: "z", 7: "x",
  8: "c", 9: "v", 11: "b", 12: "q", 13: "w", 14: "e", 15: "r",
  16: "y", 17: "t", 24: "=", 27: "-", 30: "]", 31: "o", 32: "u",
  33: "[", 34: "i", 35: "p", 36: "RETURN", 37: "l", 38: "j", 39: "'",
  40: "k", 41: ";", 43: ",", 44: "/", 45: "n", 46: "m", 47: ".",
  48: "TAB", 49: "SPACE", 50: "`", 51: "DELETE", 53: "ESCAPE",
  96: "F5", 97: "F6", 99: "F3", 101: "F9", 118: "F4",
  115: "HOME", 116: "PAGEUP", 117: "FORWARDDELETE", 119: "END",
  121: "PAGEDOWN", 123: "LEFT", 124: "RIGHT", 125: "DOWN", 126: "UP",
};

const ACTION_KEY = {
  shift: 56,
  control: 59,
  option: 58,
  command: 55,
  fn: 63,
};

const HYPER = MOD.control | MOD.option | MOD.command;
const HYPER_SHIFT = HYPER | MOD.shift;

function uuid(seed) {
  const h = createHash("sha256").update(`dotfiles-btt:${seed}`).digest("hex").slice(0, 32).split("");
  h[12] = "5";
  h[16] = ((parseInt(h[16], 16) & 3) | 8).toString(16);
  const value = h.join("").toUpperCase();
  return `${value.slice(0, 8)}-${value.slice(8, 12)}-${value.slice(12, 16)}-${value.slice(16, 20)}-${value.slice(20)}`;
}

function action(type, extra = {}) {
  return { BTTPredefinedActionType: type, ...extra };
}

function send(shortcut) {
  return action(264, { BTTShortcutToSend: shortcut });
}

function launch(applicationPath) {
  return action(49, { BTTLaunchPath: applicationPath });
}

function advancedModifierFlags(modifierFlags) {
  let flags = modifierFlags;
  if (modifierFlags & MOD.shift) flags += 2;
  if (modifierFlags & MOD.command) flags += 8;
  if (modifierFlags & MOD.option) flags += 32;
  if (modifierFlags & MOD.control) flags += 8192;
  return String(flags);
}

function shortcut(name, keyCode, modifierFlags, actions, group = "global") {
  return {
    BTTTriggerType: 0,
    BTTTriggerClass: "BTTTriggerTypeKeyboardShortcut",
    BTTTriggerTypeDescription: name,
    BTTGestureNotes: name,
    BTTNotesInsteadOfDescription: 1,
    BTTUUID: uuid(`${group}:${name}:${keyCode}:${modifierFlags}`),
    BTTEnabled: 1,
    BTTEnabled2: 1,
    BTTTriggerOnDown: 1,
    BTTShortcutKeyCode: keyCode,
    BTTShortcutModifierKeys: modifierFlags,
    BTTLayoutIndependentChar: LAYOUT_CHAR[keyCode] ?? "",
    BTTShortcutAdvancedModifierKeys: advancedModifierFlags(modifierFlags),
    BTTAdditionalConfiguration: advancedModifierFlags(modifierFlags),
    BTTShortcutScope: 0,
    BTTAutoAdaptToKeyboardLayout: 0,
    BTTActionsToExecute: Array.isArray(actions) ? actions : [actions],
  };
}

function actionShortcut(modifiers, keyCode) {
  const codes = modifiers.map((modifier) => ACTION_KEY[modifier]).sort((a, b) => b - a);
  return [...codes, keyCode].join(",");
}

function modifiersPowerSet(optional) {
  return optional.reduce(
    (sets, modifier) => [...sets, ...sets.map((set) => [...set, modifier])],
    [[]],
  );
}

function flags(modifiers) {
  return modifiers.reduce((sum, modifier) => sum | MOD[modifier], 0);
}

function remapShortcuts(name, fromKey, mandatory, optional, to, group = "global") {
  return modifiersPowerSet(optional).map((extras) => {
    const inputModifiers = [...mandatory, ...extras];
    const output = to.map((step) => actionShortcut([...extras, ...(step.modifiers ?? [])], step.key));
    return shortcut(
      `${name}${extras.length ? ` (${extras.join("+")} held)` : ""}`,
      fromKey,
      flags(inputModifiers),
      output.map(send),
      group,
    );
  });
}

const excludedBundlePatterns = [
  "^org\\.gnu\\.Emacs$", "^org\\.gnu\\.Aquamacs", "^org\\.pqrs\\.unknownapp\\.conkeror$",
  "^com\\.microsoft\\.rdc", "^net\\.sf\\.cord$", "^com\\.thinomenon\\.RemoteDesktopConnection$",
  "^com\\.itap-mobile\\.qmote$", "^com\\.nulana\\.remotixmac$", "^com\\.p5sys\\.jump\\.mac\\.viewer",
  "^com\\.teamviewer\\.TeamViewer$", "^com\\.vmware\\.", "^com\\.2X\\.Client\\.Mac$",
  "^com\\.apple\\.Terminal$", "^com\\.googlecode\\.iterm2$", "^co\\.zeit\\.hyper",
  "^io\\.alacritty$", "^net\\.kovidgoyal\\.kitty$", "^org\\.vim\\.", "^com\\.qvacua\\.VimR$",
  "^com\\.parallels\\.", "^org\\.virtualbox\\.app\\.VirtualBoxVM$", "^org\\.x\\.X11$",
  "^com\\.apple\\.x11$", "^org\\.macosforge\\.xquartz\\.X11$", "^org\\.macports\\.X11$",
  "^com\\.sublimetext\\.", "^com\\.microsoft\\.VSCode$",
];

function conditionalRemapAction(passThrough, replacement) {
  const source = `
(async () => {
  const bundleID = String(await callBTT("get_string_variable", { variable_name: "BTTActiveAppBundleIdentifier" }) || "");
  const excluded = ${JSON.stringify(excludedBundlePatterns)}.some((pattern) => new RegExp(pattern).test(bundleID));
  const shortcuts = excluded ? ${JSON.stringify(passThrough)} : ${JSON.stringify(replacement)};
  for (const value of shortcuts) {
    await callBTT("trigger_action", { json: JSON.stringify({ BTTPredefinedActionType: 264, BTTShortcutToSend: value }) });
  }
  returnToBTT("");
})();`.trim();
  return action(281, {
    BTTAdditionalActionData: {
      BTTScriptType: 3,
      BTTAppleScriptUsePath: false,
      BTTScriptLocation: 0,
      BTTAppleScriptRunInBackground: true,
      BTTAppleScriptString: source,
    },
  });
}

function conditionalRemapShortcuts(name, fromKey, mandatory, optional, to) {
  return modifiersPowerSet(optional).map((extras) => {
    const inputModifiers = [...mandatory, ...extras];
    const passThrough = [actionShortcut(inputModifiers, fromKey)];
    const replacement = to.map((step) => actionShortcut([...extras, ...(step.modifiers ?? [])], step.key));
    return shortcut(
      `${name}${extras.length ? ` (${extras.join("+")} held)` : ""}`,
      fromKey,
      flags(inputModifiers),
      conditionalRemapAction(passThrough, replacement),
      "emacs-global",
    );
  });
}

function moveResize(name, config) {
  return action(251, {
    BTTActionMoveResizeName: name,
    BTTActionMoveResizeConfig: JSON.stringify({
      BTTMoveResizeVersion: 3,
      BTTActiveOrHoveredWindow: 0,
      BTTScreensToMove: 0,
      BTTOriginDisplay: 1,
      BTTUseVisibleFrame: 1,
      BTTConstrainToScreen: 1,
      ...config,
    }),
  });
}

function stepResize(name, axis, amount) {
  return moveResize(name, {
    BTTChangeXOrigin: 0,
    BTTChangeYOrigin: 0,
    BTTChangeWindowWidth: axis === "width" ? 1 : 0,
    BTTChangeWindowHeight: axis === "height" ? 1 : 0,
    ...(axis === "width"
      ? { BTTChangeWidthRelative: 3, BTTAddToWidth: amount, BTTWidthChangeUnit: 1 }
      : { BTTChangeHeightRelative: 3, BTTAddToHeight: amount, BTTHeightChangeUnit: 1 }),
  });
}

const caffeinateLabel = "com.mnewt.bettertouchtool.caffeinate";
const caffeinateIndicatorLabel = "com.mnewt.bettertouchtool.caffeinate-indicator";
const caffeinateIndicatorBinary = resolve(here, "caffeinate-indicator");
const caffeinateToggleScript = `label="${caffeinateLabel}"
service="gui/\${UID}/$label"
indicator_label="${caffeinateIndicatorLabel}"
indicator_service="gui/\${UID}/$indicator_label"
indicator_binary="${caffeinateIndicatorBinary}"
if /bin/launchctl print "$service" >/dev/null 2>&1; then
  /bin/launchctl remove "$label"
  /bin/launchctl remove "$indicator_label" >/dev/null 2>&1 || true
  osascript -e 'display notification "Sleep enabled" with title "BetterTouchTool"'
else
  /bin/launchctl submit -l "$label" -- /usr/bin/caffeinate -dimsu
  if /bin/launchctl print "$service" >/dev/null 2>&1; then
    /bin/launchctl remove "$indicator_label" >/dev/null 2>&1 || true
    /bin/launchctl submit -l "$indicator_label" -- "$indicator_binary"
    if /bin/launchctl print "$indicator_service" >/dev/null 2>&1; then
      osascript -e 'display notification "Sleep disabled" with title "BetterTouchTool"'
    else
      osascript -e 'display notification "Sleep disabled, but indicator failed" with title "BetterTouchTool"'
    fi
  else
    osascript -e 'display notification "Could not start caffeinate" with title "BetterTouchTool"'
    exit 1
  fi
fi`;

function caffeinateToggleAction() {
  return action(206, {
    BTTShellTaskActionConfig: "/bin/bash:::-c:::-:::",
    BTTShellTaskActionScript: caffeinateToggleScript,
  });
}

function keySequence(name, secondKey, replacement, releaseControlBetween = false) {
  const event = (character, keyCode, down, time) => ({
    BTTKEYCharacter: character,
    BTTKEYCode: keyCode,
    ...(down ? { BTTKEYDown: 1, BTTKEYOrderRelevant: 1 } : {}),
    BTTKEYRequired: 1,
    BTTKEYTime: time,
  });
  const mixed = releaseControlBetween
    ? [
        event("⌃", 59, true, 0), event("x", KEY.x, true, 1), event("x", KEY.x, false, 2), event("⌃", 59, false, 3),
        event("⌃", 59, true, 4), event(secondKey, KEY[secondKey], true, 5), event(secondKey, KEY[secondKey], false, 6), event("⌃", 59, false, 7),
      ]
    : [
        event("⌃", 59, true, 0), event("x", KEY.x, true, 1), event("x", KEY.x, false, 2),
        event(secondKey, KEY[secondKey], true, 3), event(secondKey, KEY[secondKey], false, 4), event("⌃", 59, false, 5),
      ];
  return {
    BTTTriggerType: 624,
    BTTTriggerClass: "BTTTriggerTypeKeySequence",
    BTTTriggerTypeDescription: name,
    BTTGestureNotes: name,
    BTTNotesInsteadOfDescription: 1,
    BTTUUID: uuid(`sequence:${name}:${releaseControlBetween ? "released" : "held"}`),
    BTTEnabled: 1,
    BTTEnabled2: 1,
    BTTKeySequence: {
      BTTAllowRetrigger: 1,
      BTTKeySequenceAllowContinueFor: 0.8,
      BTTPauseBetween: 0.8,
      BTTKeyCount: mixed.length,
      BTTKeySequenceDownKeys: mixed.filter((item) => item.BTTKEYDown === 1),
      BTTKeySequenceMixedKeys: mixed,
      BTTKeySequenceUpKeys: mixed.filter((item) => item.BTTKEYDown !== 1),
    },
    BTTActionsToExecute: [conditionalRemapAction([], [replacement])],
  };
}

const global = [];

const capsLockHyper = shortcut("Caps Lock: Hyper; tap for Escape", KEY.capsLock, 0, {
  ...action(293, {
    BTTAdditionalActionData: { BTTActionHyperkeyExcludeShift: true },
    BTTActionHyperkeyExcludeShift: "1",
  }),
  BTTActionCategory: 0,
});
// BTT's shortcut recorder represents Caps Lock as its special key code 1004,
// not the raw macOS virtual key code 57 used by ordinary keyboard shortcuts.
capsLockHyper.BTTShortcutKeyCode = 1004;
capsLockHyper.BTTShortcutKeyboardType = 2302;
capsLockHyper.BTTActionCategoryHyperKeyRelease = [{ ...action(189), BTTActionCategory: 5 }];
global.push(capsLockHyper);

global.push(
  shortcut("Alfred replacement: toggle BTT Launcher", KEY.space, MOD.option, action(596, {
    BTTAdditionalActionData: { BTTActionLauncherID: "dotfiles-main", BTTActionLauncherShortcut: "cmd+p" },
  })),
  shortcut("Open System Settings", KEY.comma, HYPER, launch("/System/Applications/System Settings.app")),
  shortcut("Open Finder", KEY.period, HYPER, launch("/System/Library/CoreServices/Finder.app")),
  shortcut("Open Emacs", KEY.e, HYPER, launch("/Applications/Emacs.app")),
  shortcut("Open Mail", KEY.m, HYPER, launch("/System/Applications/Mail.app")),
  shortcut("Open Microsoft Outlook", KEY.n, HYPER, launch("/Applications/Microsoft Outlook.app")),
  shortcut("Open Messages", KEY.b, HYPER, launch("/System/Applications/Messages.app")),
  shortcut("Open Calendar", KEY.c, HYPER, launch("/System/Applications/Calendar.app")),
  shortcut("Open 1Password", KEY.p, HYPER, launch("/Applications/1Password.app")),
  shortcut("Open Firefox", KEY.f, HYPER, launch("/Applications/Firefox.app")),
  shortcut("Open Terminal", KEY.t, HYPER, launch("/System/Applications/Utilities/Terminal.app")),
  shortcut("Open Microsoft To Do", KEY.u, HYPER, launch("/Applications/Microsoft To Do.app")),
  shortcut("Open Microsoft Teams", KEY.y, HYPER, launch("/Applications/Microsoft Teams.app")),
  shortcut("Open Microsoft Word", KEY.w, HYPER, launch("/Applications/Microsoft Word.app")),
  shortcut("Open Microsoft Excel", KEY.r, HYPER, launch("/Applications/Microsoft Excel.app")),
  shortcut("Open Jump Desktop", KEY.j, HYPER, launch("/Applications/Jump Desktop.app")),
  shortcut("Open Cisco AnyConnect", KEY.h, HYPER, launch("/Applications/Cisco/Cisco AnyConnect Secure Mobility Client.app")),
  shortcut("Open Safari", KEY.s, HYPER, launch("/Applications/Safari.app")),
  shortcut("Open Google Chrome", KEY.k, HYPER, launch("/Applications/Google Chrome.app")),
  shortcut("Open ChatGPT", KEY.g, HYPER, launch("/Applications/ChatGPT.app")),
  shortcut("Open Preview", KEY.l, HYPER, launch("/System/Applications/Preview.app")),
  shortcut("Open Obsidian", KEY.o, HYPER, launch("/Applications/Obsidian.app")),
  shortcut("Open Visual Studio Code", KEY.x, HYPER, launch("/Applications/Visual Studio Code.app")),
  shortcut("Window: left half", KEY.left, HYPER, action(19)),
  shortcut("Window: right half", KEY.right, HYPER, action(20)),
  shortcut("Window: top half", KEY.up, HYPER, action(96)),
  shortcut("Window: bottom half", KEY.down, HYPER, action(95)),
  shortcut("Window: maximize", KEY.f, HYPER_SHIFT, action(21)),
  shortcut("Window: restore previous frame", KEY.z, HYPER, action(84)),
  shortcut("Window: center (replaces grid snap)", KEY.semicolon, HYPER, action(97)),
  shortcut("Windows: arrange four (replaces snap all)", KEY.quote, HYPER, action(322)),
  shortcut("Window: thinner via H", KEY.h, HYPER_SHIFT, stepResize("Thinner", "width", -100 / 18)),
  shortcut("Window: taller via J", KEY.j, HYPER_SHIFT, stepResize("Taller", "height", 100 / 18)),
  shortcut("Window: shorter via K", KEY.k, HYPER_SHIFT, stepResize("Shorter", "height", -100 / 18)),
  shortcut("Window: wider via L", KEY.l, HYPER_SHIFT, stepResize("Wider", "width", 100 / 18)),
  shortcut("Window: widen 5.56%", KEY.equal, HYPER, stepResize("Wider", "width", 100 / 18)),
  shortcut("Window: narrow 5.56%", KEY.minus, HYPER, stepResize("Narrower", "width", -100 / 18)),
  shortcut("Window: taller 5.56%", KEY.equal, HYPER_SHIFT, stepResize("Taller", "height", 100 / 18)),
  shortcut("Window: shorter 5.56%", KEY.minus, HYPER_SHIFT, stepResize("Shorter", "height", -100 / 18)),
  shortcut("System: toggle dark mode", KEY.t, HYPER_SHIFT, action(197)),
  shortcut("BetterTouchTool: restart", KEY.r, HYPER_SHIFT, action(55)),
  shortcut("BetterTouchTool: open settings", KEY.comma, HYPER_SHIFT, action(105)),
  shortcut("BetterTouchTool: open settings (former console)", KEY.backtick, HYPER, action(105)),
  shortcut("Keyboard: show shortcut cheat sheet", KEY.slash, HYPER, action(254, {
    BTTAdditionalActionData: {
      BTTActionHUDTitle: "Dotfiles shortcuts",
      BTTActionHUDDetail: "Hyper+letter app launchers  •  Hyper+arrows window halves  •  Hyper+⇧H/J/K/L resize\nHyper+⇧F maximize  •  Hyper+Z restore  •  Hyper+Q caffeinate  •  Hyper+⇧T dark mode",
      BTTActionHUDDuration: 5,
      BTTActionHUDBlur: true,
    },
  })),
  shortcut("Clipboard: type contents (paste-blocking workaround)", KEY.v, HYPER, action(172, {
    BTTAdditionalActionData: {
      BTTScriptType: 0,
      BTTAppleScriptUsePath: false,
      BTTScriptLocation: 0,
      BTTAppleScriptRunInBackground: false,
      BTTAppleScriptString: "set clipText to the clipboard as text\ntell application \"System Events\" to keystroke clipText",
    },
  })),
  shortcut("System: toggle caffeinate", KEY.q, HYPER, caffeinateToggleAction()),
  shortcut("Fn+F3: Mission Control", KEY.f3, MOD.fn, action(7)),
  shortcut("Fn+F4: Launchpad", KEY.f4, MOD.fn, action(115)),
  shortcut("Fn+F5: keyboard illumination down", KEY.f5, MOD.fn, action(32)),
  shortcut("Fn+F6: keyboard illumination up", KEY.f6, MOD.fn, action(31)),
  shortcut("Fn+F9: next track", KEY.f9, MOD.fn, action(27)),
  keySequence("Emacs C-x C-c: quit", "c", actionShortcut(["command"], KEY.q)),
  keySequence("Emacs C-x C-c: quit", "c", actionShortcut(["command"], KEY.q), true),
  keySequence("Emacs C-x C-f: open", "f", actionShortcut(["command"], KEY.o)),
  keySequence("Emacs C-x C-f: open", "f", actionShortcut(["command"], KEY.o), true),
  keySequence("Emacs C-x C-s: save", "s", actionShortcut(["command"], KEY.s)),
  keySequence("Emacs C-x C-s: save", "s", actionShortcut(["command"], KEY.s), true),
);

const genericMappings = [
  ["Emacs C-d: forward delete", KEY.d, ["control"], ["option"], [{ key: KEY.forwardDelete }]],
  ["Emacs C-h: backspace", KEY.h, ["control"], ["option"], [{ key: KEY.backspace }]],
  ["Emacs C-i: tab", KEY.i, ["control"], ["shift"], [{ key: KEY.tab }]],
  ["Emacs C-m: return", KEY.m, ["control"], ["shift", "option"], [{ key: KEY.return }]],
  ["Emacs C-b: left", KEY.b, ["control"], ["shift", "option"], [{ key: KEY.left }]],
  ["Emacs C-f: right", KEY.f, ["control"], ["shift", "option"], [{ key: KEY.right }]],
  ["Emacs C-n: down", KEY.n, ["control"], ["shift", "option"], [{ key: KEY.down }]],
  ["Emacs C-p: up", KEY.p, ["control"], ["shift", "option"], [{ key: KEY.up }]],
  ["Emacs C-v: page down", KEY.v, ["control"], ["shift"], [{ key: KEY.pageDown }]],
  ["Emacs M-v: page up", KEY.v, ["option"], ["shift"], [{ key: KEY.pageUp }]],
  ["Emacs M-b: previous word", KEY.b, ["option"], ["shift"], [{ key: KEY.left, modifiers: ["option"] }]],
  ["Emacs M-f: next word", KEY.f, ["option"], ["shift"], [{ key: KEY.right, modifiers: ["option"] }]],
  ["Emacs M-d: delete word", KEY.d, ["option"], [], [{ key: KEY.forwardDelete, modifiers: ["option"] }]],
  ["Bash C-w: delete previous word", KEY.w, ["control"], [], [{ key: KEY.backspace, modifiers: ["option"] }]],
  ["Bash C-u: delete to line start", KEY.u, ["control"], [], [
    { key: KEY.left, modifiers: ["command", "shift"] }, { key: KEY.backspace },
  ]],
];

for (const mapping of genericMappings) global.push(...conditionalRemapShortcuts(...mapping));
global.push(
  ...remapShortcuts("Emacs C-[: escape", KEY.openBracket, ["control"], [], [{ key: KEY.escape }]),
  ...conditionalRemapShortcuts("Emacs C-g: escape", KEY.g, ["control"], [], [{ key: KEY.escape }]),
);

const vscode = [];
const vscodeMappings = [
  ...genericMappings,
  ["Emacs C-a: line start", KEY.a, ["control"], ["shift"], [{ key: KEY.left, modifiers: ["command"] }]],
  ["Emacs C-e: line end", KEY.e, ["control"], ["shift"], [{ key: KEY.right, modifiers: ["command"] }]],
  ["Emacs C-[: escape", KEY.openBracket, ["control"], [], [{ key: KEY.escape }]],
  ["Emacs C-g: escape", KEY.g, ["control"], [], [{ key: KEY.escape }]],
];
for (const mapping of vscodeMappings) vscode.push(...remapShortcuts(...mapping, "vscode"));

function officeTriggers(bundle) {
  return [
    ...remapShortcuts("Emacs C-a: document line start", KEY.a, ["control"], ["shift"], [{ key: KEY.home }], `office:${bundle}`),
    ...remapShortcuts("Emacs C-e: document line end", KEY.e, ["control"], ["shift"], [{ key: KEY.end }], `office:${bundle}`),
  ];
}
const eclipse = [
  ...remapShortcuts("Emacs C-a: line start", KEY.a, ["control"], ["shift"], [{ key: KEY.left, modifiers: ["command"] }], "eclipse"),
  ...remapShortcuts("Emacs C-e: line end", KEY.e, ["control"], ["shift"], [{ key: KEY.right, modifiers: ["command"] }], "eclipse"),
];

function jumpPassthroughTriggers(bundle) {
  return global
    .filter((trigger) => {
      if (trigger.BTTTriggerType === 624) return true;
      if (trigger.BTTTriggerType !== 0) return false;

      const isCapsLockHyper = trigger.BTTShortcutKeyCode === 1004;
      const usesHyper = (trigger.BTTShortcutModifierKeys & HYPER) === HYPER;
      return !isCapsLockHyper && !usesHyper;
    })
    .map((trigger) => ({
      ...structuredClone(trigger),
      BTTTriggerTypeDescription: `Pass through: ${trigger.BTTTriggerTypeDescription}`,
      BTTGestureNotes: `Pass through in Jump Desktop: ${trigger.BTTGestureNotes}`,
      BTTUUID: uuid(`jump-pass-through:${bundle}:${trigger.BTTUUID}`),
      BTTEnabled: 0,
      BTTEnabled2: 0,
    }));
}

const preset = {
  BTTPresetCreatorNotes: "Generated from this repository. Replaces Hammerspoon, Karabiner-Elements, and the repo's Alfred installation with BetterTouchTool.",
  BTTPresetInfoURL: "https://github.com/mnewt/dotfiles",
  BTTPresetName: "dotfiles-unified",
  BTTPresetColor: "42.000000, 122.000000, 255.000000, 255.000000",
  BTTPresetUUID: uuid("preset"),
  BTTPresetContent: [
    {
      BTTAppBundleIdentifier: "BT.G",
      BTTAppName: "Global",
      BTTAppAutoInvertIcon: 1,
      BTTAppSpecificSettings: { BTTDisableGlobalTriggers: false },
      BTTTriggers: global,
    },
    ...[
      ["com.p5sys.jump.mac.viewer", "Jump Desktop"],
      ["com.p5sys.jump.mac.viewer.web", "Jump Desktop Web"],
    ].map(([bundle, name]) => ({
      BTTAppBundleIdentifier: bundle,
      BTTAppName: name,
      BTTAppAutoInvertIcon: 1,
      // Keep Caps Lock / Hyper shortcuts active, while allowing ordinary
      // editing chords and key sequences to reach the remote session intact.
      BTTAppSpecificSettings: { BTTDisableGlobalTriggers: false },
      BTTTriggers: jumpPassthroughTriggers(bundle),
    })),
    {
      BTTAppBundleIdentifier: "com.microsoft.VSCode",
      BTTAppName: "Visual Studio Code",
      BTTAppAutoInvertIcon: 1,
      BTTTriggers: vscode,
    },
    ...["com.microsoft.Excel", "com.microsoft.Powerpoint", "com.microsoft.Word"].map((bundle) => ({
      BTTAppBundleIdentifier: bundle,
      BTTAppName: bundle.split(".").at(-1),
      BTTAppAutoInvertIcon: 1,
      BTTTriggers: officeTriggers(bundle),
    })),
    {
      BTTAppBundleIdentifier: "org.eclipse.platform.ide",
      BTTAppName: "Eclipse",
      BTTAppAutoInvertIcon: 1,
      BTTTriggers: eclipse,
    },
  ],
  BTTPresetSnapAreas: [],
};

const allTriggers = preset.BTTPresetContent.flatMap((section) => section.BTTTriggers);
const allUUIDs = allTriggers.map((trigger) => trigger.BTTUUID);
if (new Set(allUUIDs).size !== allUUIDs.length) throw new Error("Generated trigger UUIDs are not unique");
for (const section of preset.BTTPresetContent) {
  const keyboardShortcuts = section.BTTTriggers
    .filter((trigger) => trigger.BTTTriggerClass === "BTTTriggerTypeKeyboardShortcut");
  if (keyboardShortcuts.some((trigger) => !Number.isInteger(trigger.BTTShortcutKeyCode))) {
    throw new Error(`Keyboard shortcut without a key code in ${section.BTTAppName}`);
  }
  const chords = keyboardShortcuts
    .map((trigger) => `${trigger.BTTShortcutModifierKeys}:${trigger.BTTShortcutKeyCode}`);
  if (new Set(chords).size !== chords.length) throw new Error(`Duplicate keyboard shortcut in ${section.BTTAppName}`);
}

writeFileSync(output, `${JSON.stringify(preset, null, 2)}\n`);
console.log(`Wrote ${output}`);
console.log(`${global.length} global, ${vscode.length} VS Code, 4 per Office app, ${eclipse.length} Eclipse triggers`);
