;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../../Dropbox/dotfiles/emacs.d/el-get/paredit/paredit"
;;;;;;  "paredit/paredit.el" "c685943530a419bbd2f9936b452a8927")
;;; Generated autoloads from paredit/paredit.el

(autoload 'paredit-mode "../../Dropbox/dotfiles/emacs.d/el-get/paredit/paredit" "\
Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  unbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are unbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing unbalanced parentheses instead.
\\<paredit-mode-map>

\(fn &optional ARG)" t nil)

(autoload 'enable-paredit-mode "../../Dropbox/dotfiles/emacs.d/el-get/paredit/paredit" "\
Turn on pseudo-structural editing of Lisp code.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "auto-complete/auto-complete" "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "0f564b17b1ef2e700470197f0218dc22")
;;; Generated autoloads from ../../../../.emacs.d/el-get/auto-complete/auto-complete.el

(autoload 'auto-complete "auto-complete/auto-complete" "\
Start auto-completion at current point.

\(fn &optional SOURCES)" t nil)

(autoload 'auto-complete-mode "auto-complete/auto-complete" "\
AutoComplete mode

\(fn &optional ARG)" t nil)

(defvar global-auto-complete-mode nil "\
Non-nil if Global Auto-Complete mode is enabled.
See the `global-auto-complete-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-auto-complete-mode'.")

(custom-autoload 'global-auto-complete-mode "auto-complete/auto-complete" nil)

(autoload 'global-auto-complete-mode "auto-complete/auto-complete" "\
Toggle Auto-Complete mode in all buffers.
With prefix ARG, enable Global Auto-Complete mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Complete mode is enabled in all buffers where
`auto-complete-mode-maybe' would do it.
See `auto-complete-mode' for more information on Auto-Complete mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "auto-complete/auto-complete-config" "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "ff41e174795402bf5f86dfbc7e9060d2")
;;; Generated autoloads from ../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el

(autoload 'ac-config-default "auto-complete/auto-complete-config" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "better-defaults/better-defaults" "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "18fbd1077010a42a690d1ad58907958d")
;;; Generated autoloads from ../../../../.emacs.d/el-get/better-defaults/better-defaults.el

(ido-mode t)

(setq ido-enable-flex-matching t)

(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(autoload 'zap-up-to-char "misc" "\
Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)

(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)

(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-M-s") 'isearch-forward)

(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

(setq x-select-enable-clipboard t x-select-enable-primary t save-interprogram-paste-before-kill t apropos-do-all t mouse-yank-at-point t require-final-newline t visible-bell t load-prefer-newer t ediff-window-setup-function 'ediff-setup-windows-plain save-place-file (concat user-emacs-directory "places") backup-directory-alist `(("." \, (concat user-emacs-directory "backups"))))

;;;***

;;;### (autoloads nil "buffer-move/buffer-move" "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "8b3b9aa7e25093a3634c48e27e836bc3")
;;; Generated autoloads from ../../../../.emacs.d/el-get/buffer-move/buffer-move.el

(autoload 'buf-move-up "buffer-move/buffer-move" "\
Swap the current buffer and the buffer above the split.
   If there is no split, ie now window above the current one, an
   error is signaled.

\(fn)" t nil)

(autoload 'buf-move-down "buffer-move/buffer-move" "\
Swap the current buffer and the buffer under the split.
   If there is no split, ie now window under the current one, an
   error is signaled.

\(fn)" t nil)

(autoload 'buf-move-left "buffer-move/buffer-move" "\
Swap the current buffer and the buffer on the left of the split.
   If there is no split, ie now window on the left of the current
   one, an error is signaled.

\(fn)" t nil)

(autoload 'buf-move-right "buffer-move/buffer-move" "\
Swap the current buffer and the buffer on the right of the split.
   If there is no split, ie now window on the right of the current
   one, an error is signaled.

\(fn)" t nil)

(autoload 'buf-move "buffer-move/buffer-move" "\
Begin moving the current buffer to different windows.

Use the arrow keys to move in the desired direction.  Pressing
any other key exits this function.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider/cider" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "0c292ca22788f1cbbd7981a00c99c43d")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider.el

(autoload 'cider-version "cider/cider" "\
Display CIDER's version.

\(fn)" t nil)

(autoload 'cider-jack-in "cider/cider" "\
Start an nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.
If CLJS-TOO is non-nil, also start a ClojureScript REPL session with its
own buffer.

\(fn &optional PROMPT-PROJECT CLJS-TOO)" t nil)

(autoload 'cider-jack-in-clojurescript "cider/cider" "\
Start an nREPL server and connect to it both Clojure and ClojureScript REPLs.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.

\(fn &optional PROMPT-PROJECT)" t nil)

(autoload 'cider-connect "cider/cider" "\
Connect to an nREPL server identified by HOST and PORT.
Create REPL buffer and start an nREPL client connection.

When the optional param PROJECT-DIR is present, the connection
gets associated with it.

\(fn HOST PORT &optional PROJECT-DIR)" t nil)

(eval-after-load 'clojure-mode '(progn (define-key clojure-mode-map (kbd "C-c M-j") #'cider-jack-in) (define-key clojure-mode-map (kbd "C-c M-J") #'cider-jack-in-clojurescript) (define-key clojure-mode-map (kbd "C-c M-c") #'cider-connect)))

;;;***

;;;### (autoloads nil "cider/cider-apropos" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "aaa9e346fecacc0caa09eda195db1cf1")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-apropos.el

(autoload 'cider-apropos "cider/cider-apropos" "\
Show all symbols whose names match QUERY, a regular expression.
QUERY can also be a list of space-separated words (e.g. take while) which
will be converted to a regular expression (like take.+while) automatically
behind the scenes.  The search may be limited to the namespace NS, and may
optionally search doc strings (based on DOCS-P), include private vars
\(based on PRIVATES-P), and be case-sensitive (based on CASE-SENSITIVE-P).

\(fn QUERY &optional NS DOCS-P PRIVATES-P CASE-SENSITIVE-P)" t nil)

(autoload 'cider-apropos-documentation "cider/cider-apropos" "\
Shortcut for (cider-apropos <query> nil t).

\(fn)" t nil)

(autoload 'cider-apropos-select "cider/cider-apropos" "\
Similar to `cider-apropos', but presents the results in a completing read.

Show all symbols whose names match QUERY, a regular expression.
QUERY can also be a list of space-separated words (e.g. take while) which
will be converted to a regular expression (like take.+while) automatically
behind the scenes.  The search may be limited to the namespace NS, and may
optionally search doc strings (based on DOCS-P), include private vars
\(based on PRIVATES-P), and be case-sensitive (based on CASE-SENSITIVE-P).

\(fn QUERY &optional NS DOCS-P PRIVATES-P CASE-SENSITIVE-P)" t nil)

(autoload 'cider-apropos-documentation-select "cider/cider-apropos" "\
Shortcut for (cider-apropos-select <query> nil t).

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider/cider-browse-ns" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "b991a1ead972e9c10953ddb928675196")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-browse-ns.el

(autoload 'cider-browse-ns "cider/cider-browse-ns" "\
List all NAMESPACE's vars in BUFFER.

\(fn NAMESPACE)" t nil)

(autoload 'cider-browse-ns-all "cider/cider-browse-ns" "\
List all loaded namespaces in BUFFER.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider/cider-classpath" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "317cb16a02388857bc010d5d314c4837")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-classpath.el

(autoload 'cider-classpath "cider/cider-classpath" "\
List all classpath entries.

\(fn)" t nil)

(autoload 'cider-open-classpath-entry "cider/cider-classpath" "\
Open a classpath entry.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider/cider-debug" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "c5f365dcde58995a7bb8cf02fd19107f")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-debug.el

(autoload 'cider-debug-defun-at-point "cider/cider-debug" "\
Instrument the \"top-level\" expression at point.
If it is a defn, dispatch the instrumented definition.  Otherwise,
immediately evaluate the instrumented expression.

While debugged code is being evaluated, the user is taken through the
source code and displayed the value of various expressions.  At each step,
a number of keys will be prompted to the user.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider/cider-grimoire" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "19e4ee859ff89633d8e5c4c72281b4f8")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-grimoire.el

(autoload 'cider-grimoire-web "cider/cider-grimoire" "\
Open grimoire documentation in the default web browser.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates.

\(fn &optional ARG)" t nil)

(autoload 'cider-grimoire "cider/cider-grimoire" "\
Open grimoire documentation in a popup buffer.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "cider/cider-inspector" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "4de67239ffbd614b3a60f8b05ba024b2")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-inspector.el

(autoload 'cider-inspect-last-sexp "cider/cider-inspector" "\
Inspect the result of the the expression preceding point.

\(fn)" t nil)

(autoload 'cider-inspect-defun-at-point "cider/cider-inspector" "\
Inspect the result of the \"top-level\" expression at point.

\(fn)" t nil)

(autoload 'cider-inspect-last-result "cider/cider-inspector" "\
Inspect the most recent eval result.

\(fn)" t nil)

(autoload 'cider-inspect "cider/cider-inspector" "\
Inspect the result of the preceding sexp.

With a prefix argument ARG it inspects the result of the \"top-level\" form.
With a second prefix argument it prompts for an expression to eval and inspect.

\(fn &optional ARG)" t nil)

(autoload 'cider-inspect-expr "cider/cider-inspector" "\
Evaluate EXPR in NS and inspect its value.
Interactively, EXPR is read from the minibuffer, and NS the
current buffer's namespace.

\(fn EXPR NS)" t nil)

(define-obsolete-function-alias 'cider-inspect-read-and-inspect 'cider-inspect-expr "0.13.0")

;;;***

;;;### (autoloads nil "cider/cider-macroexpansion" "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "c054e58f4368fc4462d6aa158a6f3fcc")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-macroexpansion.el

(autoload 'cider-macroexpand-1 "cider/cider-macroexpansion" "\
Invoke \\=`macroexpand-1\\=` on the expression preceding point.
If invoked with a PREFIX argument, use \\=`macroexpand\\=` instead of
\\=`macroexpand-1\\=`.

\(fn &optional PREFIX)" t nil)

(autoload 'cider-macroexpand-all "cider/cider-macroexpansion" "\
Invoke \\=`macroexpand-all\\=` on the expression preceding point.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider/cider-mode" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "f0658cbd8ba594568d5305275b766d0b")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-mode.el

(defvar cider-mode-line '(:eval (format " cider[%s]" (cider--modeline-info))) "\
Mode line lighter for `cider-mode'.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
details about mode line templates.

Customize this variable to change how `cider-mode' displays its
status in the mode line.  The default value displays the current connection.
Set this variable to nil to disable the mode line
entirely.")

(custom-autoload 'cider-mode-line "cider/cider-mode" t)

(eval-after-load 'clojure-mode '(easy-menu-define cider-clojure-mode-menu-open clojure-mode-map "Menu for Clojure mode.\n  This is displayed in `clojure-mode' buffers, if `cider-mode' is not active." `("CIDER" :visible (not cider-mode) ["Start a REPL" cider-jack-in :help "Starts an nREPL server (with lein, boot, or maven) and connects a REPL to it."] ["Connect to a REPL" cider-connect :help "Connects to a REPL that's already running."] ["Start a Clojure REPL, and a ClojureScript REPL" cider-jack-in-clojurescript :help "Starts an nREPL server, connects a Clojure REPL to it, and then a ClojureScript REPL.\n  Configure `cider-cljs-lein-repl', `cider-cljs-boot-repl' and `cider-cljs-gradle-repl' to change the ClojureScript REPL to use."] "--" ["View manual online" cider-view-manual])))

(autoload 'cider-mode "cider/cider-mode" "\
Minor mode for REPL interaction from a Clojure buffer.

\\{cider-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "cider/cider-repl-history" "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "f5327d1c39beaf6aebbea71916fdbeed")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-repl-history.el

(autoload 'cider-repl-history "cider/cider-repl-history" "\
Display items in the CIDER command history in another buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider/cider-scratch" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "644b2ecdc3da2e9c902b26d42727d58a")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-scratch.el

(autoload 'cider-scratch "cider/cider-scratch" "\
Go to the scratch buffer named `cider-scratch-buffer-name'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider/cider-selector" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "84a1036e4764e9a86e6e62aec24f0dc2")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-selector.el

(autoload 'cider-selector "cider/cider-selector" "\
Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer.  The `?' character describes then
available methods.  OTHER-WINDOW provides an optional target.

See `def-cider-selector-method' for defining new methods.

\(fn &optional OTHER-WINDOW)" t nil)

;;;***

;;;### (autoloads nil "cider/cider-test" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "e13bde6008291237c500578362af9884")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-test.el

(defvar cider-auto-test-mode nil "\
Non-nil if Cider-Auto-Test mode is enabled.
See the `cider-auto-test-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `cider-auto-test-mode'.")

(custom-autoload 'cider-auto-test-mode "cider/cider-test" nil)

(autoload 'cider-auto-test-mode "cider/cider-test" "\
Toggle automatic testing of Clojure files.

When enabled this reruns tests every time a Clojure file is loaded.
Only runs tests corresponding to the loaded file's namespace and does
nothing if no tests are defined or if the file failed to load.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "cider/cider-util" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "60e08b6de1e55fe04fb986c52a416f91")
;;; Generated autoloads from ../../../../.emacs.d/el-get/cider/cider-util.el

(autoload 'cider-view-manual "cider/cider-util" "\
View the manual in your default browser.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "clj-refactor/clj-refactor" "../../../../.emacs.d/el-get/clj-refactor/clj-refactor.el"
;;;;;;  "f8d2a20c6c8b2476c06953484f894d6c")
;;; Generated autoloads from ../../../../.emacs.d/el-get/clj-refactor/clj-refactor.el

(autoload 'cljr-add-keybindings-with-prefix "clj-refactor/clj-refactor" "\
Bind keys in `cljr--all-helpers' under a PREFIX key.

\(fn PREFIX)" nil nil)

(autoload 'cljr-add-keybindings-with-modifier "clj-refactor/clj-refactor" "\
Bind keys in `cljr--all-helpers' under a MODIFIER key.

\(fn MODIFIER)" nil nil)

(autoload 'cljr-rename-file-or-dir "clj-refactor/clj-refactor" "\
Rename a file or directory of files.
Buffers visiting any affected file are killed and the
corresponding files are revisited.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-rename-file-or-dir

\(fn OLD-PATH NEW-PATH)" t nil)

(autoload 'cljr-rename-file "clj-refactor/clj-refactor" "\


\(fn NEW-PATH)" t nil)

(autoload 'cljr-add-require-to-ns "clj-refactor/clj-refactor" "\
Add a require statement to the ns form in current buffer.

With a prefix act on the cljs part of the ns declaration.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-require-to-ns

\(fn CLJS\\=\\?)" t nil)

(autoload 'cljr-add-use-to-ns "clj-refactor/clj-refactor" "\
Add a use statement to the buffer's ns form.

With a prefix act on the cljs part of the ns declaration.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-use-to-ns

\(fn CLJS\\=\\?)" t nil)

(autoload 'cljr-add-import-to-ns "clj-refactor/clj-refactor" "\
Add an import statement to the buffer's ns form.

With a prefix act on the cljs part of the ns declaration.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-import-to-ns

\(fn &optional CLJS\\=\\?)" t nil)

(autoload 'cljr-require-macro "clj-refactor/clj-refactor" "\
Add a require statement for a macro to the ns form in current buffer.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-require-macro

\(fn)" t nil)

(autoload 'cljr-stop-referring "clj-refactor/clj-refactor" "\
Stop referring to vars in the namespace at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-stop-referring

\(fn)" t nil)

(autoload 'cljr-move-form "clj-refactor/clj-refactor" "\
Move the form containing POINT to a new namespace.

If REGION is active, move all forms contained by region.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-form

\(fn)" t nil)

(autoload 'cljr-add-declaration "clj-refactor/clj-refactor" "\
Add a declare for the current def near the top of the buffer.

With a prefix add a declaration for the symbol under the cursor instead.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-declaration

\(fn FOR-THING-AT-POINT-P)" t nil)

(autoload 'cljr-extract-constant "clj-refactor/clj-refactor" "\
Extract form at (or above) point as a constant.
Create a def for it at the top level, and replace its current
occurrence with the defined name.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-constant

\(fn)" t nil)

(autoload 'cljr-extract-def "clj-refactor/clj-refactor" "\
Extract form at (or above) point as a def.
Create a def for it at the top level, and replace its current
occurrence with the defined name.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-def

\(fn)" t nil)

(autoload 'cljr-cycle-thread "clj-refactor/clj-refactor" "\
Cycle a threading macro between -> and ->>.
Also applies to other versions of the macros, like cond->.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-thread

\(fn)" t nil)

(autoload 'cljr-introduce-let "clj-refactor/clj-refactor" "\
Create a let form, binding the form at point.
The resulting let form can then be expanded with `\\[cljr-expand-let]'.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-introduce-let

\(fn &optional N)" t nil)

(autoload 'cljr-expand-let "clj-refactor/clj-refactor" "\
Expand the let form above point by one level.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-expand-let

\(fn)" t nil)

(autoload 'cljr-move-to-let "clj-refactor/clj-refactor" "\
Move the form at point to a binding in the nearest let.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-to-let

\(fn)" t nil)

(autoload 'cljr-destructure-keys "clj-refactor/clj-refactor" "\
Change a symbol binding at point to a destructuring bind.
Keys to use in the destructuring are inferred from the code, and
their usage is replaced with the new local variables.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-destructure-keys

\(fn)" t nil)

(autoload 'cljr-raise-sexp "clj-refactor/clj-refactor" "\
Like paredit-raise-sexp, but removes # in front of function literals and sets.

\(fn &optional ARGUMENT)" t nil)

(autoload 'cljr-splice-sexp-killing-backward "clj-refactor/clj-refactor" "\
Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets.

\(fn &optional ARGUMENT)" t nil)

(autoload 'cljr-splice-sexp-killing-forward "clj-refactor/clj-refactor" "\
Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets.

\(fn &optional ARGUMENT)" t nil)

(autoload 'cljr-slash "clj-refactor/clj-refactor" "\
Inserts / as normal, but also checks for common namespace shorthands to require.
If `cljr-magic-require-namespaces' is non-nil, typing one of the
short aliases listed in `cljr-magic-requires' followed by this
command will add the corresponding require statement to the ns
form.

\(fn)" t nil)

(autoload 'cljr-project-clean "clj-refactor/clj-refactor" "\
Run `cljr-project-clean-functions' on every clojure file, then
sorts the project's dependency vectors.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-project-clean

\(fn)" t nil)

(autoload 'cljr-sort-project-dependencies "clj-refactor/clj-refactor" "\
Sorts all dependency vectors in project.clj

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-sort-project-dependencies

\(fn)" t nil)

(autoload 'cljr-add-project-dependency "clj-refactor/clj-refactor" "\
Add a dependency to the project.clj file.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-project-dependency

\(fn FORCE)" t nil)

(autoload 'cljr-update-project-dependency "clj-refactor/clj-refactor" "\
Update the version of the dependency at point.

\(fn)" t nil)

(autoload 'cljr-update-project-dependencies "clj-refactor/clj-refactor" "\
Update all project dependencies.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-update-project-dependencies

\(fn)" t nil)

(autoload 'cljr-promote-function "clj-refactor/clj-refactor" "\
Promote a function literal to an fn, or an fn to a defn.
With prefix PROMOTE-TO-DEFN, promote to a defn even if it is a
function literal.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-promote-function

\(fn PROMOTE-TO-DEFN)" t nil)

(autoload 'cljr-find-usages "clj-refactor/clj-refactor" "\
Find all usages of the symbol at point in the project.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-find-usages

\(fn)" t nil)

(autoload 'cljr-rename-symbol "clj-refactor/clj-refactor" "\
Rename the symbol at point and all of its occurrences.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-rename-symbol

\(fn &optional NEW-NAME)" t nil)

(autoload 'cljr-clean-ns "clj-refactor/clj-refactor" "\
Clean the ns form for the current buffer.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-clean-ns

\(fn)" t nil)

(autoload 'cljr-add-missing-libspec "clj-refactor/clj-refactor" "\
Requires or imports the symbol at point.

If the symbol at point is of the form str/join then the ns
containing join will be aliased to str.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-missing-libspec

\(fn)" t nil)

(autoload 'cljr-hotload-dependency "clj-refactor/clj-refactor" "\
Download a dependency (if needed) and hotload it into the current repl session.

Defaults to the dependency vector at point, but prompts if none is found.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-hotload-dependency

\(fn)" t nil)

(autoload 'cljr-extract-function "clj-refactor/clj-refactor" "\
Extract the form at (or above) point as a top-level defn.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-function

\(fn)" t nil)

(autoload 'cljr-add-stubs "clj-refactor/clj-refactor" "\
Adds implementation stubs for the interface or protocol at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-stubs

\(fn)" t nil)

(autoload 'cljr-inline-symbol "clj-refactor/clj-refactor" "\
Inline the symbol at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-inline-symbol

\(fn)" t nil)

(autoload 'cljr-version "clj-refactor/clj-refactor" "\
Returns the version of the middleware as well as this package.

\(fn)" t nil)

(autoload 'cljr-toggle-debug-mode "clj-refactor/clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-create-fn-from-example "clj-refactor/clj-refactor" "\
Create a top-level defn for the symbol at point.
The context in which symbol is being used should be that of a
function, and the arglist of the defn is guessed from this
context.

For instance, if the symbol is the first argument of a `map'
call, the defn is created with one argument. If it is the first
argument of a `reduce', the defn will take two arguments.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-create-fn-from-example

\(fn)" t nil)

(autoload 'cljr-describe-refactoring "clj-refactor/clj-refactor" "\
Show the wiki page, in emacs, for one of the available refactorings.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-describe-refactoring

\(fn CLJR-FN)" t nil)

(autoload 'cljr-change-function-signature "clj-refactor/clj-refactor" "\
Change the function signature of the function at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-change-function-signature

\(fn)" t nil)

(autoload 'cljr--inject-jack-in-dependencies "clj-refactor/clj-refactor" "\
Inject the REPL dependencies of clj-refactor at `cider-jack-in'.
If injecting the dependencies is not preferred set `cljr-inject-dependencies-at-jack-in' to nil.

\(fn)" nil nil)

(eval-after-load 'cider '(cljr--inject-jack-in-dependencies))

(autoload 'clj-refactor-mode "clj-refactor/clj-refactor" "\
A mode to keep the clj-refactor keybindings.

\\{clj-refactor-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "clojure-mode/clojure-mode" "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "236032e2958696fefae7999eb7eb2066")
;;; Generated autoloads from ../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el

(autoload 'clojure-mode "clojure-mode/clojure-mode" "\
Major mode for editing Clojure code.

\\{clojure-mode-map}

\(fn)" t nil)

(autoload 'clojure-unwind "clojure-mode/clojure-mode" "\
Unwind thread at point or above point by one level.
Return nil if there are no more levels to unwind.

\(fn)" t nil)

(autoload 'clojure-unwind-all "clojure-mode/clojure-mode" "\
Fully unwind thread at point or above point.

\(fn)" t nil)

(autoload 'clojure-thread "clojure-mode/clojure-mode" "\
Thread by one more level an existing threading macro.

\(fn)" t nil)

(autoload 'clojure-thread-first-all "clojure-mode/clojure-mode" "\
Fully thread the form at point using ->.
When BUT-LAST is passed the last expression is not threaded.

\(fn BUT-LAST)" t nil)

(autoload 'clojure-thread-last-all "clojure-mode/clojure-mode" "\
Fully thread the form at point using ->>.
When BUT-LAST is passed the last expression is not threaded.

\(fn BUT-LAST)" t nil)

(autoload 'clojure-cycle-privacy "clojure-mode/clojure-mode" "\
Make public the current private def, or vice-versa.
See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-privacy

\(fn)" t nil)

(autoload 'clojure-convert-collection-to-list "clojure-mode/clojure-mode" "\
Convert collection at (point) to list.

\(fn)" t nil)

(autoload 'clojure-convert-collection-to-quoted-list "clojure-mode/clojure-mode" "\
Convert collection at (point) to quoted list.

\(fn)" t nil)

(autoload 'clojure-convert-collection-to-map "clojure-mode/clojure-mode" "\
Convert collection at (point) to map.

\(fn)" t nil)

(autoload 'clojure-convert-collection-to-vector "clojure-mode/clojure-mode" "\
Convert collection at (point) to vector.

\(fn)" t nil)

(autoload 'clojure-convert-collection-to-set "clojure-mode/clojure-mode" "\
Convert collection at (point) to set.

\(fn)" t nil)

(autoload 'clojure-cycle-if "clojure-mode/clojure-mode" "\
Change a surrounding if to if-not, or vice-versa.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-if

\(fn)" t nil)

(autoload 'clojure-cycle-when "clojure-mode/clojure-mode" "\
Change a surrounding when to when-not, or vice-versa.

\(fn)" t nil)

(autoload 'clojure-let-backward-slurp-sexp "clojure-mode/clojure-mode" "\
Slurp the s-expression before the let form into the let form.
With a numberic prefix argument slurp the previous N s-expression into the let form.

\(fn &optional N)" t nil)

(autoload 'clojure-let-forward-slurp-sexp "clojure-mode/clojure-mode" "\
Slurp the next s-expression after the let form into the let form.
With a numeric prefix argument slurp the next N s-expressions into the let form.

\(fn &optional N)" t nil)

(autoload 'clojure-introduce-let "clojure-mode/clojure-mode" "\
Create a let form, binding the form at point.
With a numeric prefix argument the let is introduced N lists up.

\(fn &optional N)" t nil)

(autoload 'clojure-move-to-let "clojure-mode/clojure-mode" "\
Move the form at point to a binding in the nearest let.

\(fn)" t nil)

(autoload 'clojurescript-mode "clojure-mode/clojure-mode" "\
Major mode for editing ClojureScript code.

\\{clojurescript-mode-map}

\(fn)" t nil)

(autoload 'clojurec-mode "clojure-mode/clojure-mode" "\
Major mode for editing ClojureC code.

\\{clojurec-mode-map}

\(fn)" t nil)

(autoload 'clojurex-mode "clojure-mode/clojure-mode" "\
Major mode for editing ClojureX code.

\\{clojurex-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode))

(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojurex-mode))

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))

(add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))

;;;***

;;;### (autoloads nil "edn/edn" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "7955179f00f2ff454ccfb1feb8282022")
;;; Generated autoloads from ../../../../.emacs.d/el-get/edn/edn.el

(autoload 'edn-time-to-inst "edn/edn" "\
Turn a `time-date' TIME into our internal representation of an inst.

\(fn TIME)" nil nil)

(autoload 'edn-inst-to-time "edn/edn" "\
Turn an `edn-inst', INST, into a TIME from `time-date'.

\(fn INST)" nil nil)

(autoload 'edn-string-to-uuid "edn/edn" "\
Create an `edn-uuid' from a string, S, containing a uuid.

\(fn S)" nil nil)

(autoload 'edn-uuid-to-string "edn/edn" "\
Turn our internal representation of a UUID into a string.

\(fn UUID)" nil nil)

(autoload 'edn-read "edn/edn" "\
Read one edn value from SOURCE.

SOURCE is either a string of edn data or nil.  If no source is
given the next edn value will be read from POINT in the current
buffer.

You can use `edn-add-reader' to add your own readers for unknown
tags.

\(fn &optional SOURCE)" nil nil)

(autoload 'edn-list-to-set "edn/edn" "\
Turn a list into `edn''s internal set representation.

If COMPARE-FN is provided this function is used to uniquify the
list.  Otherwise it's expected that l is without duplicates.

\(fn L &optional COMPARE-FN)" nil nil)

(autoload 'edn-set-to-list "edn/edn" "\
Turn `edn''s internal set representation into a list.

\(fn S)" nil nil)

(autoload 'edn-add-reader "edn/edn" "\
Add a READER function for TAG.

TAG is either a string, symbol or keyword. e.g. :my/type

\(fn TAG READER)" nil nil)

(autoload 'edn-add-writer "edn/edn" "\
Add a WRITER function for types satisfying PRED.

\(fn PRED WRITER)" nil nil)

(autoload 'edn-remove-reader "edn/edn" "\
Remove a previously registered handler for TAG.

\(fn TAG)" nil nil)

(autoload 'edn-remove-writer "edn/edn" "\
The remove the writer WRITER.

\(fn WRITER)" nil nil)

(autoload 'edn-print-string "edn/edn" "\
Serialize the lisp form DATUM into edn.

You can use `edn-add-writer' to add writers capable of writing
your own tagged data.

\(fn DATUM)" nil nil)

;;;***

;;;### (autoloads nil "el-get/el-get" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "725730d0425ec018bd83aa83226d5e9e")
;;; Generated autoloads from ../../../../.emacs.d/el-get/el-get/el-get.el

(autoload 'el-get-version "el-get/el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-install "el-get/el-get" "\
Cause the named PACKAGE to be installed after all of its
dependencies (if any).

PACKAGE may be either a string or the corresponding symbol.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update "el-get/el-get" "\
Update PACKAGE.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update-all "el-get/el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-update-packages-of-type "el-get/el-get" "\
Update all installed packages of type TYPE.

\(fn TYPE)" t nil)

(autoload 'el-get-self-update "el-get/el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-remove "el-get/el-get" "\
Remove any PACKAGE that is know to be installed or required.

\(fn PACKAGE)" t nil)

(autoload 'el-get-reinstall "el-get/el-get" "\
Remove PACKAGE and then install it again.

\(fn PACKAGE)" t nil)

(autoload 'el-get-cd "el-get/el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "el-get/el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "el-get/el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE)" t nil)

(autoload 'el-get-self-checksum "el-get/el-get" "\
Compute the checksum of the running version of el-get itself.

Also put the checksum in the kill-ring.

\(fn)" t nil)

(autoload 'el-get "el-get/el-get" "\
Ensure that packages have been downloaded once and init them as needed.

This will not update the sources by using `apt-get install' or
`git pull', but it will ensure that:

* the packages have been installed
* load-path is set so their elisp files can be found
* Info-directory-list is set so their info files can be found
* Autoloads have been prepared and evaluated for each package
* Any post-installation setup (e.g. `(require 'feature)') happens

When SYNC is nil (the default), all installations run
concurrently, in the background.

When SYNC is 'sync, each package will be installed synchronously,
and any error will stop it all.

Please note that the `el-get-init' part of `el-get' is always
done synchronously. There's `byte-compile' support though, and
the packages you use are welcome to use `autoload' too.

PACKAGES is expected to be a list of packages you want to install
or init.  When PACKAGES is omited (the default), the list of
already installed packages is considered.

\(fn &optional SYNC &rest PACKAGES)" nil nil)

;;;***

;;;### (autoloads nil "el-get/el-get-bundle" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "c8dec168e93c1d40df86351c317b33bc")
;;; Generated autoloads from ../../../../.emacs.d/el-get/el-get/el-get-bundle.el

(autoload 'el-get-bundle-el-get "el-get/el-get-bundle" "\


\(fn SRC SYNC)" nil nil)

(autoload 'el-get-bundle "el-get/el-get-bundle" "\
Install PACKAGE and run initialization FORM.

PACKAGE can be either a simple package name or a package name
with a modifier before the name to specify local recipe source
information:

* `<owner>/' : specifies a Github owner name
* `gist:<id>' : specifies a Gist ID
* `<type>:' : specifies a type of the package source

If `FEATURE in PACKAGE' form is used instead of PACKAGE, then
that FEATURE is `require'd after installing PACKAGE.  You can
also use `el-get-bundle!' macro if FEATURE and PACKAGE are the
same.  If you wish to `require' more than one feature, then use
`:features' property in FORM.

The initialization FORM may start with a property list that
describes a local recipe.  The property list may include the keyword
`:bundle-sync' with a value of either `t' or `nil' to request that
`el-get-bundle' invoke `el-get' synchronously (respectively asynchronously).
The keyword `:bundle-async' is the inverse of `:bundle-sync'.
\(Note that the request to run el-get synchronously may not be respected in all
circumstances: see the definition of `el-get-bundle-el-get' for details.)
The FORM after the property list is treated as initialization code,
which is actually an `:after' property of the local recipe.

A copy of the initialization code is stored in a directory
specified by `el-get-bundle-init-directory' and its byte-compiled
version is used if `el-get-bundle-byte-compile' is non-nil.

\(fn PACKAGE &rest FORM)" nil t)

(function-put 'el-get-bundle 'lisp-indent-function 'defun)

(autoload 'el-get-bundle! "el-get/el-get-bundle" "\
Install PACKAGE and run initialization form.
It is the same as `el-get-bundle' except that PACKAGE is explicitly
required.

\(fn PACKAGE &rest ARGS)" nil t)

(function-put 'el-get-bundle! 'lisp-indent-function 'defun)

;;;***

;;;### (autoloads nil "el-get/el-get-check" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "6145ed8054e30cf9530ba7c381446a14")
;;; Generated autoloads from ../../../../.emacs.d/el-get/el-get/el-get-check.el

(autoload 'el-get-check-recipe "el-get/el-get-check" "\
Check the format of the recipe.
Please run this command before sending a pull request.
Usage: M-x el-get-check-recipe RET

You can run this function from checker script like this:
    test/check-recipe.el PATH/TO/RECIPE.rcp

When used as a lisp function, FILE-OR-BUFFER must be a buffer
object or a file path.

\(fn FILE-OR-BUFFER)" t nil)

;;;***

;;;### (autoloads nil "el-get/el-get-list-packages" "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "6758aae2b1f25eba5c586f5035d93279")
;;; Generated autoloads from ../../../../.emacs.d/el-get/el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "el-get/el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emacs-async/async" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "9746b0627e5830ae81438d55a70b6bdb")
;;; Generated autoloads from ../../../../.emacs.d/el-get/emacs-async/async.el

(autoload 'async-start-process "emacs-async/async" "\
Start the executable PROGRAM asynchronously.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory.

\(fn NAME PROGRAM FINISH-FUNC &rest PROGRAM-ARGS)" nil nil)

(autoload 'async-start "emacs-async/async" "\
Execute START-FUNC (often a lambda) in a subordinate Emacs process.
When done, the return value is passed to FINISH-FUNC.  Example:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message \"This is a test\")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message \"Async process done, result should be 222: %s\"
                  result)))

If FINISH-FUNC is nil or missing, a future is returned that can
be inspected using `async-get', blocking until the value is
ready.  Example:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message \"This is a test\")
                     (sleep-for 3)
                     222))))

        (message \"I'm going to do some work here\") ;; ....

        (message \"Waiting on async process, result should be 222: %s\"
                 (async-get proc)))

If you don't want to use a callback, and you don't care about any
return value from the child process, pass the `ignore' symbol as
the second argument (if you don't, and never call `async-get', it
will leave *emacs* process buffers hanging around):

    (async-start
     (lambda ()
       (delete-file \"a remote file on a slow link\" nil))
     'ignore)

Note: Even when FINISH-FUNC is present, a future is still
returned except that it yields no value (since the value is
passed to FINISH-FUNC).  Call `async-get' on such a future always
returns nil.  It can still be useful, however, as an argument to
`async-ready' or `async-wait'.

\(fn START-FUNC &optional FINISH-FUNC)" nil nil)

;;;***

;;;### (autoloads nil "emacs-async/async-bytecomp" "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "c3f92425d089a0a8527673e0882c2fb5")
;;; Generated autoloads from ../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el

(autoload 'async-byte-recompile-directory "emacs-async/async-bytecomp" "\
Compile all *.el files in DIRECTORY asynchronously.
All *.elc files are systematically deleted before proceeding.

\(fn DIRECTORY &optional QUIET)" nil nil)

(defvar async-bytecomp-package-mode nil "\
Non-nil if Async-Bytecomp-Package mode is enabled.
See the `async-bytecomp-package-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `async-bytecomp-package-mode'.")

(custom-autoload 'async-bytecomp-package-mode "emacs-async/async-bytecomp" nil)

(autoload 'async-bytecomp-package-mode "emacs-async/async-bytecomp" "\
Byte compile asynchronously packages installed with package.el.
Async compilation of packages can be controlled by
`async-bytecomp-allowed-packages'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "emacs-async/dired-async" "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "b973c6ed3d35bf31323ef3f72b6d6137")
;;; Generated autoloads from ../../../../.emacs.d/el-get/emacs-async/dired-async.el

(defvar dired-async-mode nil "\
Non-nil if Dired-Async mode is enabled.
See the `dired-async-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dired-async-mode'.")

(custom-autoload 'dired-async-mode "emacs-async/dired-async" nil)

(autoload 'dired-async-mode "emacs-async/dired-async" "\
Do dired actions asynchronously.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "find-file-in-project/find-file-in-project"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "c410792a6e8d1294ddde1ca5916bd3a1")
;;; Generated autoloads from ../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el

(autoload 'ffip-diff-backend-git-show-commit "find-file-in-project/find-file-in-project" "\


\(fn)" nil nil)

(autoload 'ffip-diff-backend-hg-show-commit "find-file-in-project/find-file-in-project" "\


\(fn)" nil nil)

(autoload 'ffip-copy-without-change "find-file-in-project/find-file-in-project" "\


\(fn P)" nil nil)

(autoload 'ffip-copy-reactjs-import "find-file-in-project/find-file-in-project" "\


\(fn P)" nil nil)

(autoload 'ffip-copy-org-file-link "find-file-in-project/find-file-in-project" "\


\(fn P)" nil nil)

(defvar ffip-find-relative-path-callback 'ffip-copy-without-change)

(autoload 'ffip-project-root "find-file-in-project/find-file-in-project" "\
Return the root of the project.

\(fn)" nil nil)

(autoload 'ffip-save-ivy-last "find-file-in-project/find-file-in-project" "\
Save `ivy-last' into `ffip-ivy-last-saved'.  Requires `ivy-mode'.

\(fn)" t nil)

(autoload 'ffip-get-project-root-directory "find-file-in-project/find-file-in-project" "\
Get the full path of project root directory.

\(fn)" nil nil)

(autoload 'ffip-ivy-resume "find-file-in-project/find-file-in-project" "\
Wrapper of `ivy-resume'.  Resume the search saved at `ffip-ivy-last-saved'.

\(fn)" t nil)

(autoload 'ffip-filename-identity "find-file-in-project/find-file-in-project" "\
Return identical KEYWORD.

\(fn KEYWORD)" nil nil)

(autoload 'ffip-filename-camelcase-to-dashes "find-file-in-project/find-file-in-project" "\
Convert KEYWORD from camel cased to dash seperated.
If CHECK-ONLY is true, only do the check.

\(fn KEYWORD &optional CHECK-ONLY)" nil nil)

(autoload 'ffip-filename-dashes-to-camelcase "find-file-in-project/find-file-in-project" "\
Convert KEYWORD from dash seperated to camel cased.
If CHECK-ONLY is true, only do the check.

\(fn KEYWORD &optional CHECK-ONLY)" nil nil)

(autoload 'ffip-completing-read "find-file-in-project/find-file-in-project" "\


\(fn PROMPT COLLECTION ACTION)" nil nil)

(autoload 'ffip-project-search "find-file-in-project/find-file-in-project" "\
Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique.

If KEYWORD is string, it's the file name or file path to find file.
If KEYWORD is list, it's the list of file names.

\(fn KEYWORD FIND-DIRECTORY)" nil nil)

(autoload 'ffip-find-files "find-file-in-project/find-file-in-project" "\
The API to find files.

\(fn KEYWORD OPEN-ANOTHER-WINDOW &optional FIND-DIRECTORY FN)" nil nil)

(autoload 'ffip-create-project-file "find-file-in-project/find-file-in-project" "\
Create or Append .dir-locals.el to set up per directory.
You can move .dir-locals.el to root directory.
See (info \"(Emacs) Directory Variables\") for details.

\(fn)" t nil)

(autoload 'ffip-current-full-filename-match-pattern-p "find-file-in-project/find-file-in-project" "\
Is current full file name (including directory) match the REGEX?

\(fn REGEX)" nil nil)

(autoload 'find-file-in-project "find-file-in-project/find-file-in-project" "\
Prompt with a completing list of all files in the project to find one.

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.

The project's scope is defined as the first directory containing
a `ffip-project-file' whose value is \".git\" by default.

You can override this by setting the variable `ffip-project-root'.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'find-file-in-current-directory "find-file-in-project/find-file-in-project" "\
Like `find-file-in-project'.  But search only in current directory.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'find-file-in-project-by-selected "find-file-in-project/find-file-in-project" "\
Similar to `find-file-in-project'.
But use string from selected region to search files in the project.
If no region is selected, you need provide keyword.

Keyword could be ANY part of the file's full path and support wildcard.
For example, to find /home/john/proj1/test.js, below keywords are valid:
- test.js
- roj1/tes
- john*test

If keyword contains line number like \"hello.txt:32\" or \"hello.txt:32:\",
we will move to that line in opened file.

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'find-file-in-current-directory-by-selected "find-file-in-project/find-file-in-project" "\
Like `find-file-in-project-by-selected'.  But search only in current directory.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'find-relative-path "find-file-in-project/find-file-in-project" "\
Find file/directory and copy its relative path into `kill-ring'.
Optional prefix FIND-DIRECTORY copy the directory path; file path by default.

You can set `ffip-find-relative-path-callback' to format the string before copying,
  (setq ffip-find-relative-path-callback 'ffip-copy-reactjs-import)
  (setq ffip-find-relative-path-callback 'ffip-copy-org-file-link)

\(fn &optional FIND-DIRECTORY)" t nil)

(autoload 'find-directory-in-project-by-selected "find-file-in-project/find-file-in-project" "\
Similar to `find-file-in-project-by-selected'.
Use string from selected region to find directory in the project.
If no region is selected, you need provide keyword.

Keyword could be directory's base-name only or parent-directoy+base-name
For example, to find /home/john/proj1/test, below keywords are valid:
- test
- roj1/test
- john*test

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(defalias 'ffip 'find-file-in-project)

(autoload 'ffip-diff-quit "find-file-in-project/find-file-in-project" "\


\(fn)" t nil)

(autoload 'ffip-diff-find-file "find-file-in-project/find-file-in-project" "\
File file(s) in current hunk.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'ffip-show-diff "find-file-in-project/find-file-in-project" "\
Show the diff output by excuting selected `ffip-diff-backends'.
NUM is the index selected backend from `ffip-diff-backends'.
NUM is zero based.  Its default value is zero.

\(fn &optional NUM)" t nil)

;;;***

;;;### (autoloads nil "goto-last-change/goto-last-change" "../../../../.emacs.d/el-get/goto-last-change/goto-last-change.el"
;;;;;;  "660ece6d9d67cd10b66c93be60f71782")
;;; Generated autoloads from ../../../../.emacs.d/el-get/goto-last-change/goto-last-change.el

(autoload 'goto-last-change "goto-last-change/goto-last-change" "\
Set point to the position of the last change.
Consecutive calls set point to the position of the previous change.
With a prefix arg (optional arg MARK-POINT non-nil), set mark so \\[exchange-point-and-mark]
will return point to the current position.

\(fn &optional MARK-POINT MINIMAL-LINE-DISTANCE)" t nil)

;;;***

;;;### (autoloads nil "hydra/hydra" "../../../../.emacs.d/el-get/hydra/hydra.el"
;;;;;;  "6f3b20ee73405bd7386c7d943609d22c")
;;; Generated autoloads from ../../../../.emacs.d/el-get/hydra/hydra.el

(autoload 'defhydra "hydra/hydra" "\
Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `hydra--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.  BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit and :bind.
:exit can be:

- nil (default): this head will continue the Hydra state.
- t: this head will stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'.

\(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t)

(function-put 'defhydra 'lisp-indent-function 'defun)

;;;***

;;;### (autoloads nil "pkg-info/pkg-info" "pkg-info/pkg-info.el"
;;;;;;  (22747 63741 0 0))
;;; Generated autoloads from pkg-info/pkg-info.el

(autoload 'pkg-info-library-original-version "pkg-info/pkg-info" "\
Get the original version in the header of LIBRARY.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no X-Original-Version
header.

See Info node `(elisp)Library Headers' for more information
about library headers.

\(fn LIBRARY &optional SHOW)" t nil)

(autoload 'pkg-info-library-version "pkg-info/pkg-info" "\
Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers.

\(fn LIBRARY &optional SHOW)" t nil)

(autoload 'pkg-info-defining-library-original-version "pkg-info/pkg-info" "\
Get the original version of the library defining FUNCTION.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

\(fn FUNCTION &optional SHOW)" t nil)

(autoload 'pkg-info-defining-library-version "pkg-info/pkg-info" "\
Get the version of the library defining FUNCTION.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

\(fn FUNCTION &optional SHOW)" t nil)

(autoload 'pkg-info-package-version "pkg-info/pkg-info" "\
Get the version of an installed PACKAGE.

If SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed.

\(fn PACKAGE &optional SHOW)" t nil)

(autoload 'pkg-info-version-info "pkg-info/pkg-info" "\
Obtain complete version info for LIBRARY and PACKAGE.

LIBRARY is a symbol denoting a named feature, or a library name
as string.  PACKAGE is a symbol denoting an ELPA package.  If
omitted or nil, default to LIBRARY.

If SHOW is non-nil, show the version in the minibuffer.

When called interactively, prompt for LIBRARY.  When called
interactively with prefix argument, prompt for PACKAGE as well.

Return a string with complete version information for LIBRARY.
This version information contains the version from the headers of
LIBRARY, and the version of the installed PACKAGE, the LIBRARY is
part of.  If PACKAGE is not installed, or if the PACKAGE version
is the same as the LIBRARY version, do not include a package
version.

\(fn LIBRARY &optional PACKAGE SHOW)" t nil)

;;;***

;;;### (autoloads nil "queue/queue" "queue/queue.el" (22747 63737
;;;;;;  0 0))
;;; Generated autoloads from queue/queue.el

(defalias 'make-queue 'queue-create "\
Create an empty queue data structure.")

;;;***

;;;### (autoloads nil "smartparens/smartparens" "smartparens/smartparens.el"
;;;;;;  (22747 64358 0 0))
;;; Generated autoloads from smartparens/smartparens.el

(autoload 'sp-cheat-sheet "smartparens/smartparens" "\
Generate a cheat sheet of all the smartparens interactive functions.

Without a prefix argument, print only the short documentation and examples.

With non-nil prefix argument ARG, show the full documentation for each function.

You can follow the links to the function or variable help page.
To get back to the full list, use \\[help-go-back].

You can use `beginning-of-defun' and `end-of-defun' to jump to
the previous/next entry.

Examples are fontified using the `font-lock-string-face' for
better orientation.

\(fn &optional ARG)" t nil)

(defvar smartparens-mode-map (make-sparse-keymap) "\
Keymap used for `smartparens-mode'.")

(autoload 'sp-use-paredit-bindings "smartparens/smartparens" "\
Initiate `smartparens-mode-map' with `sp-paredit-bindings'.

\(fn)" t nil)

(autoload 'sp-use-smartparens-bindings "smartparens/smartparens" "\
Initiate `smartparens-mode-map' with `sp-smartparens-bindings'.

\(fn)" t nil)

(autoload 'smartparens-mode "smartparens/smartparens" "\
Toggle smartparens mode.

You can enable pre-set bindings by customizing
`sp-base-key-bindings' variable.  The current content of
`smartparens-mode-map' is:

 \\{smartparens-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'smartparens-strict-mode "smartparens/smartparens" "\
Toggle the strict smartparens mode.

When strict mode is active, `delete-char', `kill-word' and their
backward variants will skip over the pair delimiters in order to
keep the structure always valid (the same way as `paredit-mode'
does).  This is accomplished by remapping them to
`sp-delete-char' and `sp-kill-word'.  There is also function
`sp-kill-symbol' that deletes symbols instead of words, otherwise
working exactly the same (it is not bound to any key by default).

When strict mode is active, this is indicated with \"/s\"
after the smartparens indicator in the mode list.

\(fn &optional ARG)" t nil)

(defvar smartparens-global-strict-mode nil "\
Non-nil if Smartparens-Global-Strict mode is enabled.
See the `smartparens-global-strict-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-strict-mode'.")

(custom-autoload 'smartparens-global-strict-mode "smartparens/smartparens" nil)

(autoload 'smartparens-global-strict-mode "smartparens/smartparens" "\
Toggle Smartparens-Strict mode in all buffers.
With prefix ARG, enable Smartparens-Global-Strict mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Smartparens-Strict mode is enabled in all buffers where
`turn-on-smartparens-strict-mode' would do it.
See `smartparens-strict-mode' for more information on Smartparens-Strict mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-smartparens-strict-mode "smartparens/smartparens" "\
Turn on `smartparens-strict-mode'.

\(fn)" t nil)

(defvar smartparens-global-mode nil "\
Non-nil if Smartparens-Global mode is enabled.
See the `smartparens-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-mode'.")

(custom-autoload 'smartparens-global-mode "smartparens/smartparens" nil)

(autoload 'smartparens-global-mode "smartparens/smartparens" "\
Toggle Smartparens mode in all buffers.
With prefix ARG, enable Smartparens-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Smartparens mode is enabled in all buffers where
`turn-on-smartparens-mode' would do it.
See `smartparens-mode' for more information on Smartparens mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-smartparens-mode "smartparens/smartparens" "\
Turn on `smartparens-mode'.

This function is used to turn on `smartparens-global-mode'.

By default `smartparens-global-mode' ignores buffers with
`mode-class' set to special, but only if they are also not comint
buffers.

Additionally, buffers on `sp-ignore-modes-list' are ignored.

You can still turn on smartparens in these mode manually (or
in mode's startup-hook etc.) by calling `smartparens-mode'.

\(fn)" t nil)

(autoload 'turn-off-smartparens-mode "smartparens/smartparens" "\
Turn off `smartparens-mode'.

\(fn)" t nil)

(autoload 'show-smartparens-mode "smartparens/smartparens" "\
Toggle visualization of matching pairs.  When enabled, any
matching pair is highlighted after `sp-show-pair-delay' seconds
of Emacs idle time if the point is immediately in front or after
a pair.  This mode works similarly to `show-paren-mode', but
support custom pairs.

\(fn &optional ARG)" t nil)

(defvar show-smartparens-global-mode nil "\
Non-nil if Show-Smartparens-Global mode is enabled.
See the `show-smartparens-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `show-smartparens-global-mode'.")

(custom-autoload 'show-smartparens-global-mode "smartparens/smartparens" nil)

(autoload 'show-smartparens-global-mode "smartparens/smartparens" "\
Toggle Show-Smartparens mode in all buffers.
With prefix ARG, enable Show-Smartparens-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Show-Smartparens mode is enabled in all buffers where
`turn-on-show-smartparens-mode' would do it.
See `show-smartparens-mode' for more information on Show-Smartparens mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-show-smartparens-mode "smartparens/smartparens" "\
Turn on `show-smartparens-mode'.

\(fn)" t nil)

(autoload 'turn-off-show-smartparens-mode "smartparens/smartparens" "\
Turn off `show-smartparens-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "smex/smex" "smex/smex.el" (22747 14337 0 0))
;;; Generated autoloads from smex/smex.el

(autoload 'smex "smex/smex" "\


\(fn)" t nil)

(autoload 'smex-major-mode-commands "smex/smex" "\
Like `smex', but limited to commands that are relevant to the active major mode.

\(fn)" t nil)

(autoload 'smex-initialize "smex/smex" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "smooth-scrolling/smooth-scrolling" "smooth-scrolling/smooth-scrolling.el"
;;;;;;  (22747 60583 0 0))
;;; Generated autoloads from smooth-scrolling/smooth-scrolling.el

(defvar smooth-scrolling-mode nil "\
Non-nil if Smooth-Scrolling mode is enabled.
See the `smooth-scrolling-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smooth-scrolling-mode'.")

(custom-autoload 'smooth-scrolling-mode "smooth-scrolling/smooth-scrolling" nil)

(autoload 'smooth-scrolling-mode "smooth-scrolling/smooth-scrolling" "\
Make emacs scroll smoothly

\(fn &optional ARG)" t nil)

(defvar smooth-scroll-margin 10 "\
Number of lines of visible margin at the top and bottom of a window.
If the point is within these margins, then scrolling will occur
smoothly for `previous-line' at the top of the window, and for
`next-line' at the bottom.

This is very similar in its goal to `scroll-margin'.  However, it
is implemented by activating `smooth-scroll-down' and
`smooth-scroll-up' advise via `defadvice' for `previous-line' and
`next-line' respectively.  As a result it avoids problems
afflicting `scroll-margin', such as a sudden jump and unexpected
highlighting of a region when the mouse is clicked in the margin.

Scrolling only occurs when the point is closer to the window
boundary it is heading for (top or bottom) than the middle of the
window.  This is to intelligently handle the case where the
margins cover the whole buffer (e.g. `smooth-scroll-margin' set
to 5 and `window-height' returning 10 or less).

See also `smooth-scroll-strict-margins'.")

(custom-autoload 'smooth-scroll-margin "smooth-scrolling/smooth-scrolling" t)

(defvar smooth-scroll-strict-margins t "\
If true, the advice code supporting `smooth-scroll-margin'
will use `count-screen-lines' to determine the number of
*visible* lines between the point and the window top/bottom,
rather than `count-lines' which obtains the number of actual
newlines.  This is because there might be extra newlines hidden
by a mode such as folding-mode, outline-mode, org-mode etc., or
fewer due to very long lines being displayed wrapped when
`truncate-lines' is nil.

However, using `count-screen-lines' can supposedly cause
performance issues in buffers with extremely long lines.  Setting
`cache-long-line-scans' may be able to address this;
alternatively you can set this variable to nil so that the advice
code uses `count-lines', and put up with the fact that sometimes
the point will be allowed to stray into the margin.")

(custom-autoload 'smooth-scroll-strict-margins "smooth-scrolling/smooth-scrolling" t)

(autoload 'enable-smooth-scroll-for-function "smooth-scrolling/smooth-scrolling" "\
Define advice on FUNC to do smooth scrolling.

This adds after advice with name `smooth-scroll' to FUNC.

Note that the advice will not have an effect unless
`smooth-scrolling-mode' is enabled.

\(fn FUNC)" nil t)

;;;***

;;;### (autoloads nil "spinner/spinner" "spinner/spinner.el" (22747
;;;;;;  63715 0 0))
;;; Generated autoloads from spinner/spinner.el

(autoload 'spinner-create "spinner/spinner" "\
Create a spinner of the given TYPE.
The possible TYPEs are described in `spinner--type-to-frames'.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If BUFFER-LOCAL is non-nil, the spinner will be automatically
deactivated if the buffer is killed.  If BUFFER-LOCAL is a
buffer, use that instead of current buffer.

When started, in order to function properly, the spinner runs a
timer which periodically calls `force-mode-line-update' in the
curent buffer.  If BUFFER-LOCAL was set at creation time, then
`force-mode-line-update' is called in that buffer instead.  When
the spinner is stopped, the timer is deactivated.

DELAY, if given, is the number of seconds to wait after starting
the spinner before actually displaying it. It is safe to cancel
the spinner before this time, in which case it won't display at
all.

\(fn &optional TYPE BUFFER-LOCAL FPS DELAY)" nil nil)

(autoload 'spinner-start "spinner/spinner" "\
Start a mode-line spinner of given TYPE-OR-OBJECT.
If TYPE-OR-OBJECT is an object created with `make-spinner',
simply activate it.  This method is designed for minor modes, so
they can use the spinner as part of their lighter by doing:
    '(:eval (spinner-print THE-SPINNER))
To stop this spinner, call `spinner-stop' on it.

If TYPE-OR-OBJECT is anything else, a buffer-local spinner is
created with this type, and it is displayed in the
`mode-line-process' of the buffer it was created it.  Both
TYPE-OR-OBJECT and FPS are passed to `make-spinner' (which see).
To stop this spinner, call `spinner-stop' in the same buffer.

Either way, the return value is a function which can be called
anywhere to stop this spinner.  You can also call `spinner-stop'
in the same buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

DELAY, if given, is the number of seconds to wait until actually
displaying the spinner. It is safe to cancel the spinner before
this time, in which case it won't display at all.

\(fn &optional TYPE-OR-OBJECT FPS DELAY)" nil nil)

;;;***

;;;### (autoloads nil "switch-window/switch-window" "switch-window/switch-window.el"
;;;;;;  (22747 14316 0 0))
;;; Generated autoloads from switch-window/switch-window.el

(autoload 'switch-window-then-delete "switch-window/switch-window" "\
Display an overlay in each window showing a unique key, then
ask user which window to delete

\(fn)" t nil)

(autoload 'switch-window-then-maximize "switch-window/switch-window" "\
Display an overlay in each window showing a unique key, then
ask user which window to maximize

\(fn)" t nil)

(autoload 'switch-window "switch-window/switch-window" "\
Display an overlay in each window showing a unique key, then
ask user for the window where move to

\(fn)" t nil)

(autoload 'switch-window-then-split-horizontally "switch-window/switch-window" "\
Select a window then split it horizontally.

\(fn ARG)" t nil)

(autoload 'switch-window-then-split-vertically "switch-window/switch-window" "\
Select a window then split it vertically.

\(fn ARG)" t nil)

(autoload 'switch-window-then-split-below "switch-window/switch-window" "\
Select a window then split it with split-window-below's mode.

\(fn ARG)" t nil)

(autoload 'switch-window-then-split-right "switch-window/switch-window" "\
Select a window then split it with split-window-right's mode.

\(fn ARG)" t nil)

(autoload 'switch-window-then-swap-buffer "switch-window/switch-window" "\
Select a window then swap it buffer with current window's buffer.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "yasnippet/yasnippet" "yasnippet/yasnippet.el"
;;;;;;  (22747 14327 0 0))
;;; Generated autoloads from yasnippet/yasnippet.el

(autoload 'yas-minor-mode "yasnippet/yasnippet" "\
Toggle YASnippet mode.

When YASnippet mode is enabled, `yas-expand', normally bound to
the TAB key, expands snippets of code depending on the major
mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{yas-minor-mode-map}

\(fn &optional ARG)" t nil)

(defvar yas-global-mode nil "\
Non-nil if Yas-Global mode is enabled.
See the `yas-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas-global-mode'.")

(custom-autoload 'yas-global-mode "yasnippet/yasnippet" nil)

(autoload 'yas-global-mode "yasnippet/yasnippet" "\
Toggle Yas minor mode in all buffers.
With prefix ARG, enable Yas-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Yas minor mode is enabled in all buffers where
`yas-minor-mode-on' would do it.
See `yas-minor-mode' for more information on Yas minor mode.

\(fn &optional ARG)" t nil)

(autoload 'snippet-mode "yasnippet/yasnippet" "\
A mode for editing yasnippets

\(fn)" t nil)

;;;***

;;;### (autoloads nil "zencoding-mode/zencoding-mode" "zencoding-mode/zencoding-mode.el"
;;;;;;  (22747 14332 0 0))
;;; Generated autoloads from zencoding-mode/zencoding-mode.el

(autoload 'zencoding-expand-line "zencoding-mode/zencoding-mode" "\
Replace the current line's zencode expression with the corresponding expansion.
If prefix ARG is given or region is visible call `zencoding-preview' to start an
interactive preview.

Otherwise expand line directly.

For more information see `zencoding-mode'.

\(fn ARG)" t nil)

(autoload 'zencoding-mode "zencoding-mode/zencoding-mode" "\
Minor mode for writing HTML and CSS markup.
With zen coding for HTML and CSS you can write a line like

  ul#name>li.item*2

and have it expanded to

  <ul id=\"name\">
    <li class=\"item\"></li>
    <li class=\"item\"></li>
  </ul>

This minor mode defines keys for quick access:

\\{zencoding-mode-keymap}

Home page URL `http://www.emacswiki.org/emacs/ZenCoding'.

See also `zencoding-expand-line'.

\(fn &optional ARG)" t nil)

(autoload 'zencoding-expand-yas "zencoding-mode/zencoding-mode" "\


\(fn)" t nil)

(autoload 'zencoding-preview "zencoding-mode/zencoding-mode" "\
Expand zencode between BEG and END interactively.
This will show a preview of the expanded zen code and you can
accept it or skip it.

\(fn BEG END)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete-pkg.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/better-defaults/better-defaults.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/buffer-move/buffer-move.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-apropos.el" "../../../../.emacs.d/el-get/cider/cider-apropos.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-browse-ns.el" "../../../../.emacs.d/el-get/cider/cider-browse-ns.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-classpath.el" "../../../../.emacs.d/el-get/cider/cider-classpath.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-client.el" "../../../../.emacs.d/el-get/cider/cider-common.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-compat.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-debug.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-debug.el" "../../../../.emacs.d/el-get/cider/cider-doc.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-eldoc.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-grimoire.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-grimoire.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-inspector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-inspector.el" "../../../../.emacs.d/el-get/cider/cider-interaction.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-macroexpansion.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-mode.el" "../../../../.emacs.d/el-get/cider/cider-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-overlays.el" "../../../../.emacs.d/el-get/cider/cider-popup.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl-history.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-repl.el" "../../../../.emacs.d/el-get/cider/cider-resolve.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-scratch.el" "../../../../.emacs.d/el-get/cider/cider-scratch.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-selector.el" "../../../../.emacs.d/el-get/cider/cider-selector.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-stacktrace.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-test.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-test.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider-util.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider-util.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/cider.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/cider.el" "../../../../.emacs.d/el-get/cider/nrepl-client.el"
;;;;;;  "../../../../.emacs.d/el-get/cider/nrepl-dict.el" "../../../../.emacs.d/el-get/clj-refactor/clj-refactor-compat.el"
;;;;;;  "../../../../.emacs.d/el-get/clj-refactor/clj-refactor.el"
;;;;;;  "../../../../.emacs.d/el-get/clj-refactor/clj-refactor.el"
;;;;;;  "../../../../.emacs.d/el-get/clj-refactor/clj-refactor.el"
;;;;;;  "../../../../.emacs.d/el-get/clj-refactor/clj-refactor.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode-extra-font-locking.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/clojure-mode/clojure-mode.el"
;;;;;;  "../../../../.emacs.d/el-get/dash/dash-functional.el" "../../../../.emacs.d/el-get/dash/dash.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/edn/edn.el" "../../../../.emacs.d/el-get/edn/edn.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-autoloading.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-build.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-bundle.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-bundle.el" "../../../../.emacs.d/el-get/el-get/el-get-byte-compile.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-check.el" "../../../../.emacs.d/el-get/el-get/el-get-check.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-core.el" "../../../../.emacs.d/el-get/el-get/el-get-custom.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-dependencies.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-install.el" "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-list-packages.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-methods.el" "../../../../.emacs.d/el-get/el-get/el-get-notify.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get-recipes.el" "../../../../.emacs.d/el-get/el-get/el-get-status.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/el-get/el-get.el" "../../../../.emacs.d/el-get/el-get/el-get.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-bytecomp.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async-pkg.el" "../../../../.emacs.d/el-get/emacs-async/async-test.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/async.el" "../../../../.emacs.d/el-get/emacs-async/async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/dired-async.el"
;;;;;;  "../../../../.emacs.d/el-get/emacs-async/smtpmail-async.el"
;;;;;;  "../../../../.emacs.d/el-get/epl/epl.el" "../../../../.emacs.d/el-get/escreen/escreen.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/find-file-in-project/find-file-in-project.el"
;;;;;;  "../../../../.emacs.d/el-get/fuzzy/fuzzy.el" "../../../../.emacs.d/el-get/goto-last-change/goto-last-change.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/hydra-examples.el" "../../../../.emacs.d/el-get/hydra/hydra-ox.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/hydra-test.el" "../../../../.emacs.d/el-get/hydra/hydra.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/hydra.el" "../../../../.emacs.d/el-get/hydra/hydra.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/hydra.el" "../../../../.emacs.d/el-get/hydra/hydra.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/hydra.el" "../../../../.emacs.d/el-get/hydra/hydra.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/hydra.el" "../../../../.emacs.d/el-get/hydra/hydra.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/hydra.el" "../../../../.emacs.d/el-get/hydra/hydra.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/hydra.el" "../../../../.emacs.d/el-get/hydra/hydra.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/hydra.el" "../../../../.emacs.d/el-get/hydra/hydra.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/hydra.el" "../../../../.emacs.d/el-get/hydra/hydra.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/hydra.el" "../../../../.emacs.d/el-get/hydra/hydra.el"
;;;;;;  "../../../../.emacs.d/el-get/hydra/lv.el" "auto-complete/auto-complete-pkg.el"
;;;;;;  "cider/cider-client.el" "cider/cider-common.el" "cider/cider-compat.el"
;;;;;;  "cider/cider-doc.el" "cider/cider-eldoc.el" "cider/cider-interaction.el"
;;;;;;  "cider/cider-overlays.el" "cider/cider-popup.el" "cider/cider-repl.el"
;;;;;;  "cider/cider-resolve.el" "cider/cider-stacktrace.el" "cider/nrepl-client.el"
;;;;;;  "cider/nrepl-dict.el" "clj-refactor/clj-refactor-compat.el"
;;;;;;  "clojure-mode/clojure-mode-extra-font-locking.el" "dash/dash-functional.el"
;;;;;;  "dash/dash.el" "el-get/el-get-autoloading.el" "el-get/el-get-build.el"
;;;;;;  "el-get/el-get-byte-compile.el" "el-get/el-get-core.el" "el-get/el-get-custom.el"
;;;;;;  "el-get/el-get-dependencies.el" "el-get/el-get-install.el"
;;;;;;  "el-get/el-get-methods.el" "el-get/el-get-notify.el" "el-get/el-get-recipes.el"
;;;;;;  "el-get/el-get-status.el" "emacs-async/async-pkg.el" "emacs-async/async-test.el"
;;;;;;  "emacs-async/smtpmail-async.el" "epl/epl.el" "fuzzy/fuzzy.el"
;;;;;;  "hydra/hydra-examples.el" "hydra/hydra-ox.el" "hydra/hydra-test.el"
;;;;;;  "hydra/lv.el" "inflections/inflections-autoloads.el" "inflections/inflections-pkg.el"
;;;;;;  "magit/lisp/magit-autoloads.el" "magit/lisp/magit-core.el"
;;;;;;  "magit/lisp/magit-git.el" "magit/lisp/magit-margin.el" "magit/lisp/magit-mode.el"
;;;;;;  "magit/lisp/magit-obsolete.el" "magit/lisp/magit-popup.el"
;;;;;;  "magit/lisp/magit-process.el" "magit/lisp/magit-section.el"
;;;;;;  "moe-theme/moe-dark-theme.el" "moe-theme/moe-light-theme.el"
;;;;;;  "moe-theme/moe-theme-pkg.el" "moe-theme/moe-theme-switcher.el"
;;;;;;  "moe-theme/moe-theme.el" "multiple-cursors/mc-cycle-cursors.el"
;;;;;;  "multiple-cursors/multiple-cursors-pkg.el" "multiple-cursors/multiple-cursors.el"
;;;;;;  "paredit/paredit.el" "paredit/paredit.el" "paredit/paredit.el"
;;;;;;  "paredit/paredit.el" "paredit/paredit.el" "paredit/paredit.el"
;;;;;;  "paredit/paredit.el" "paredit/paredit.el" "paredit/paredit.el"
;;;;;;  "paredit/paredit.el" "paredit/paredit.el" "paredit/paredit.el"
;;;;;;  "paredit/paredit.el" "paredit/paredit.el" "paredit/paredit.el"
;;;;;;  "paredit/paredit.el" "paredit/paredit.el" "paredit/paredit.el"
;;;;;;  "paredit/paredit.el" "paredit/paredit.el" "paredit/paredit.el"
;;;;;;  "paredit/paredit.el" "paredit/paredit.el" "parinfer-mode/parinfer-mode.el"
;;;;;;  "parinfer-mode/parinferlib.el" "peg/peg.el" "popup/popup.el"
;;;;;;  "queue/queue-autoloads.el" "queue/queue-pkg.el" "s/s.el"
;;;;;;  "smartparens/smartparens-clojure.el" "smartparens/smartparens-config.el"
;;;;;;  "smartparens/smartparens-elixir.el" "smartparens/smartparens-ess.el"
;;;;;;  "smartparens/smartparens-haskell.el" "smartparens/smartparens-html.el"
;;;;;;  "smartparens/smartparens-latex.el" "smartparens/smartparens-lua.el"
;;;;;;  "smartparens/smartparens-ml.el" "smartparens/smartparens-pkg.el"
;;;;;;  "smartparens/smartparens-python.el" "smartparens/smartparens-racket.el"
;;;;;;  "smartparens/smartparens-ruby.el" "smartparens/smartparens-rust.el"
;;;;;;  "smartparens/smartparens-scala.el" "with-editor/with-editor.el"
;;;;;;  "yasnippet/yasnippet-debug.el" "yasnippet/yasnippet-tests.el")
;;;;;;  (22788 3728 0 0))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
