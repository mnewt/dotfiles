;;; Init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:
;; Single, monolithic Emacs init file. Uses straight.el for package management
;; and use-package for as much package configuration as possible.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top Level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Give Emacs 1GB of heap and run gc on idle.
(setq gc-cons-threshold 1073741824)
(run-with-idle-timer 30 t (lambda () (garbage-collect)))

(require 'gnutls)
(require 'nsm)
(setq gnutls-verify-error t
      network-security-level 'high
      load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

;; https://github.com/raxod502/straight.el/issues/41
(defvar straight-check-for-modifications)
(setq straight-check-for-modifications 'live)

;; Try to get package.el to work better with straight.el
;; https://github.com/raxod502/straight.el/issues/128
(defvar straight--recipe-cache)
(defun straight--advice-package-installed-p (f &rest args)
  "Call F with ARGS. Return t if package is installed via `straight' package manager."
  (or (gethash (symbol-name (car args)) straight--recipe-cache)
      (apply f args)))

(advice-add 'package-installed-p :around 'straight--advice-package-installed-p)

;; (defun m-straight-merge-all (&optional from-upstream)
;;   "Try to merge all packages from their primary remotes.
;; With prefix argument FROM-UPSTREAM, merge not just from primary
;; remotes but also from configured upstreams.

;; Do not merge packages listed in `m-pinned-packages'."
;;   (interactive "P")
;;   (straight-merge-all
;;    from-upstream
;;    (lambda (package)
;;      (not (member package m-straight-pinned-packages)))))

;; Packages in this list do not get updated when `update-packages' runs.
;; Therefore, they stay at their current version until manually updated in some
;; way, perhaps with `straight-merge-package'. See
;; https://github.com/raxod502/straight.el/issues/246#issuecomment-415085772.
;; (setq m-straight-pinned-packages
;;       '(org-mode))

(defun update-packages ()
  "Use straight.el to update all packages."
  (interactive)
  (straight-normalize-all)
  (straight-fetch-all)
  (straight-merge-all))

(use-package epkg
  :commands
  (epkg epkg-describe-package epkg-list-packages))

;; (use-package use-package-ensure-system-package)

;; (use-package benchmark-init
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cl is assumed to be present in this config and some packages too.
(require 'cl-lib)
(require 'seq)

;; All external packages and many built in ones are configured using use-package.
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; dash.el is required by many things, firstly the Environment section.
(use-package dash
  :config
  (dash-enable-font-lock))

;; Packages go here.
(add-to-list 'load-path "~/.emacs.d/elisp/")

(defun add-multiple-to-list (list items)
  "Run `add-to-list' for all ITEMS in the LIST."
  (seq-do (apply-partially #'add-to-list list) items))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Path
(defvar set-path-unix nil
  "Defines a list of path entries to add to *NIX systems.")

(defvar set-path-windows
  '("C:/bin"
    "C:/Program Files/Emacs/bin")
  "Defines a list of path entries to add to Windows systems.")

(defvar set-path-user nil
  "Defines a list of path entries to add to all systems.")

(defun source-sh (filename)
  "Sources FILENAME using the user's login shell.
Update environment variables from a shell source file."
  (interactive "fSource file: ")
  (message "Sourcing environment from `%s'..." filename)
  (with-temp-buffer
    (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))
    (let ((envvar-re "declare -x \\([^=]+\\)=\\(.*\\)$"))
      ;; Remove environment variables
      (while (search-forward-regexp (concat "^-" envvar-re) nil t)
        (let ((var (match-string 1)))
          ;; (message "%s" (prin1-to-string `(setenv ,var nil)))
          (setenv var nil)))
      ;; Update environment variables
      (goto-char (point-min))
      (while (search-forward-regexp (concat "^+" envvar-re) nil t)
        (let ((var (match-string 1))
              (value (read (match-string 2))))
          (message "%s" (prin1-to-string `(setenv ,var ,value)))
          (setenv var value)))))
  (message "Sourcing environment from `%s'... done." filename))

(defun set-path ()
  "Set path variables correctly for Linux, macOS, or Windows."
  (let* ((os-specific-paths (if (eq system-type 'windows-nt)
                                set-path-windows
                              set-path-unix))
         (sep (if (eq system-type 'windows-nt) ";" ":"))
         (old-path (split-string (getenv "PATH") sep))
         ;; De-dupe and validate new path
         (new-path
          (-map 'expand-file-name
                (-filter 'file-directory-p
                         (-distinct (append set-path-user
                                            os-specific-paths
                                            old-path))))))
    (setenv "PATH" (apply 'concat (-interpose sep new-path)))
    (message "New path: %s" new-path)
    (setq exec-path new-path)))

(source-sh "~/.env")
(source-sh "~/.bin/start-ssh-agent")
(set-path)

(defun expand-environment-variable ()
  "Insert contents of an envionment variable at point."
  (interactive)
  (insert (getenv (read-envvar-name "Insert Environment Variable: "))))

(bind-key "C-c C-v" 'expand-environment-variable)

(add-hook 'after-init-hook (lambda () (unless (server-running-p) (server-start))))

(use-package restart-emacs
  :commands
  (restart-emacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We don't set a frame title because Emacs on macOS renders the frame title
;; face terribly.
(setq frame-title-format nil)

;; Default frame settings. This is actually maximized, but not full screen.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(cursor-color . "#F60"))

;; eww uses this as its default font, among others.
(set-face-font 'variable-pitch "Georgia-18")

;; `a-theme'
(defface a-theme-active-0
  '((t (:inherit default)))
  ""
  :group 'a-theme)

(defface a-theme-inactive-0
  '((t (:inherit default)))
  ""
  :group 'a-theme)

(defface a-theme-active-1
  '((t (:inherit default)))
  ""
  :group 'a-theme)

(defface a-theme-inactive-1
  '((t (:inherit default)))
  ""
  :group 'a-theme)

(defface a-theme-active-2
  '((t (:inherit default)))
  ""
  :group 'a-theme)

(defface a-theme-inactive-2
  '((t (:inherit default)))
  ""
  :group 'a-theme)

(defface a-theme-active-3
  '((t (:inherit default)))
  ""
  :group 'a-theme)

(defface a-theme-inactive-3
  '((t (:inherit default)))
  ""
  :group 'a-theme)

(defface a-theme-active-4
  '((t (:inherit default)))
  ""
  :group 'a-theme)

(defface a-theme-inactive-4
  '((t (:inherit default)))
  ""
  :group 'a-theme)

(defvar a-theme-hook '()
  "Run whenever a theme is activated.")

(defvar a-theme-themes '()
  "Alist where car is the theme and cdr can be:

* A function to run after loading the theme.
* An alist specifying additional arguments. Possible arguments:
** hook - A function, as above.
** specs
** preset
** mouse-color
**")

(defvar a-theme-current-theme nil
  "Defines the currently loaded theme. Use it like this.

\(setq a-theme-current-theme
      \(if \(bound-and-true-p a-theme-current-theme)
          a-theme-current-theme
        'doom-dracula))

\(a-theme a-theme-current-theme)")

(defvar a-theme-specs-common '()
  "List of default face specs to apply when a theme is activated.
The attributes specified in `a-theme-themes' overrides these.

For details on face specs see `defface'.")

(defun alist-get-all (key alist &optional default testfn)
  "Return a list of the elements of ALIST with matching KEY.
Modeled on `alist-get', which only returns the first match.

DEFAULT returns a default value if nothing matches.

REMOVE is not implemented on account of I don't care and it's
dumb.

TESTFN is an equality function, *not* an alist function as with
`alist-get'. Default is `eq'."
  (let* ((testfn (or testfn #'eq))
         (matches (seq-filter
                   (lambda (e) (funcall testfn key (car e)))
                   alist)))
    (if matches
        (car (mapcar #'cadr matches))
      default)))

(defun maybe-expand-symbol (x)
  "If X is a symbol, return its value. Else, return X."
  (if (symbolp x) (symbol-value x) x))

(defun a-theme-get-attr (attribute name)
  "Get the ATTRIBUTE identified by NAME from the current theme settings.

Example usage

\(plist-get
  \(face-spec-choose \(a-theme-get-attr 'theme-face 'smerge-lower))
  \:background)"
  (let ((name (if (stringp name) (intern name) name)))
    (cl-some (lambda (e) (when (and (eq attribute (car e)) (eq name (cadr e)))
                           (cadddr e)))
             (get (car custom-enabled-themes) 'theme-settings))))

(defun a-theme-get-face (face)
  "Get the FACE from the current theme. See `a-theme-get-attr'."
  (a-theme-get-attr 'theme-face face))

(defun a-theme-get-value (value)
  "Get the VALUE from the current theme. See `a-theme-get-attr'."
  (a-theme-get-attr 'theme-value value))

(defun a-theme-search-attrs (regexp)
  "Return the attributes in the current theme which match the REGEXP."
  (seq-filter (lambda (e) (string-match-p regexp (symbol-name (cadr e))))
              (get (car custom-enabled-themes) 'theme-settings)))

(defun a-theme-generate-specs (face1 face2 face3 face4)
  "Generate theme specs from faces FACE1, FACE2, FACE3, and FACE4."
  (let* ((face1 (face-spec-choose (a-theme-get-face face1)))
         (face2 (face-spec-choose (a-theme-get-face face2)))
         (face3 (face-spec-choose (a-theme-get-face face3)))
         (face4 (face-spec-choose (a-theme-get-face face4)))
         (active-bg (plist-get face1 :background))
         (active-fg (plist-get face1 :foreground))
         (inactive-bg (doom-blend active-bg active-fg 0.95))
         (inactive-fg (doom-blend active-bg active-fg 0.4)))
    `((default ((t :background ,inactive-bg)))
      (fringe ((t :background ,inactive-bg)))
      (window-highlight-focused-window ((t :background ,active-bg)))
      (a-theme-active-0 ((t :background ,inactive-bg
                            :foreground ,(doom-blend active-fg active-bg 0.9))))
      (a-theme-active-1 ((t :background ,(doom-blend active-fg active-bg 0.8)
                            :foreground ,inactive-bg)))
      (a-theme-active-2 ((t :background ,(plist-get face2 :foreground)
                            :foreground ,active-bg)))
      (a-theme-active-3 ((t :background ,(plist-get face3 :foreground)
                            :foreground ,active-bg)))
      (a-theme-active-4 ((t :background ,(plist-get face4 :foreground)
                            :foreground ,active-bg)))
      (a-theme-inactive-0 ((t :background ,inactive-bg
                              :foreground ,inactive-fg)))
      (a-theme-inactive-1 ((t :background ,inactive-bg
                              :foreground ,inactive-bg))))))

(defun a-theme-activate (theme)
  "Switch the current Emacs theme to THEME.
Handle some housekeeping that comes with switching themes and try
to prevent Emacs from barfing on your screen."
  (custom-set-variables '(custom-enabled-themes nil))
  (load-theme (if (stringp theme) (intern theme) theme) t)
  (let* ((opts (alist-get theme a-theme-themes)))
    ;; Append presets to tail of `opts' alist
    ;; (setq preset (alist-get 'preset opts))

    ;; Dynamically set up window highlight mode.
    (when (bound-and-true-p window-highlight-mode)
      (setq opts (append opts
                         `((specs ,(a-theme-generate-specs 'default
                                                           'outline-1
                                                           'outline-2
                                                           'outline-3))))))

    ;; Feed face specs to `custom-set-faces' in reverse because last write wins.
    ;; We do it this way so additional specs can be specified when adding the
    ;; theme to `a-theme-themes'.
    (apply #'custom-set-faces
           (append
            a-theme-specs-common
            (reverse (alist-get-all 'specs opts))))
    (let-alist opts
      (set-mouse-color
       (cond
        ((boundp '.mouse-color) .mouse-color)
        ((equal 'dark (frame-parameter nil 'background-mode)) "white")
        (t "black")))
      (when (boundp '.hook) (mapc #'funcall .hook)))
    (when (fboundp #'powerline-reset) (powerline-reset))))

(defun a-theme-choose ()
  "Interactively choose a theme from `a-theme-themes' and activate it."
  (interactive)
  (ivy-read "Load custom theme: "
            (mapcar #'car a-theme-themes)
            :action #'a-theme-activate
            :caller #'a-theme-choose))

(bind-key "M-s-t" #'a-theme-choose)

(setq
 a-theme-current-theme
 (if (bound-and-true-p a-theme-current-theme)
     a-theme-current-theme
   'doom-dracula)

 a-theme-specs-common
 '((cursor ((t :background "#F60")))))

(add-hook 'a-theme-hook #'doom-themes-visual-bell-config)
(add-hook 'a-theme-hook #'doom-themes-org-config)

(use-package doom-themes
  :config
  (add-multiple-to-list 'a-theme-themes
                        '((doom-one)
                          (doom-vibrant)
                          (doom-one-light)
                          (doom-solarized-light)
                          (doom-dracula)
                          (doom-molokai)
                          (doom-tomorrow-day))))

(use-package powerline
  :custom
  (powerline-default-separator nil)
  (powerline-narrowed-indicator "n")
  (mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face0 (if active 'a-theme-active-0 'a-theme-inactive-1))
             (face1 (if active 'a-theme-active-1 'a-theme-inactive-1))
             (face2 (if active 'a-theme-active-2 'a-theme-inactive-1))
             (face3 (if active 'a-theme-active-3 'a-theme-inactive-0))
             (face4 (if active 'a-theme-active-4 'a-theme-inactive-1))
             (lhs (when active
                    (list (powerline-raw " " face1)
                          (powerline-major-mode face1 'l)
                          ;; (powerline-vc face1 'r)
                          (powerline-raw " %* " face1 'l)
                          (when (eq major-mode 'term-mode)
                            (powerline-raw
                             (cond
                              ((term-in-char-mode) " (char-mode) ")
                              ((term-in-line-mode) " (line-mode) ")
                              (t ""))
                             face1)))))
             (center (list (when (file-remote-p default-directory)
                             (powerline-raw
                              (concat " "
                                      (tramp-file-name-host
                                       (tramp-dissect-file-name
                                        default-directory))
                                      " ")
                              face4))
                           (powerline-raw " " face3)
                           (powerline-raw (buffer-name) face3 'm)
                           (powerline-raw " " face3)))
             (rhs (when active
                    (list (when (fboundp #'eyebrowse-mode-line-indicator)
                            (concat (eyebrowse-mode-line-indicator)
                                    (powerline-raw " " face0)))
                          (when (bound-and-true-p outline-minor-mode)
                            (powerline-raw " o" face1))
                          (when (bound-and-true-p hs-minor-mode)
                            (powerline-raw " h" face1))
                          (powerline-narrow face1)
                          (powerline-raw " " face1)
                          (powerline-raw global-mode-string face1 'r)
                          (powerline-raw " " face1)
                          (powerline-raw "%l" face1 'r)
                          (powerline-raw ":" face1)
                          (powerline-raw "%c" face1 'r)
                          (powerline-hud face3 face3)))))
        (concat (powerline-render lhs)
                (powerline-fill-center mode-line (/ (powerline-width center) 2.0))
                (powerline-render center)
                (powerline-fill mode-line (powerline-width rhs))
                (powerline-render rhs))))))
  :init
  (set-face-attribute 'mode-line nil :box nil))


(use-package window-highlight
  :if (>= emacs-major-version 27)
  :straight
  (:type git :host github :repo "dcolascione/emacs-window-highlight")
  :config
  (window-highlight-mode 1))

(a-theme-activate a-theme-current-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure the frame
(when window-system
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1)))

(setq frame-resize-pixelwise t
      inhibit-splash-screen t)

(defun display-startup-echo-area-message ()
  "Run when Emacs has finished starting."
  (message "Emacs has finished starting up."))

(defun echo-area-visible-bell ()
  "A more pleasant bell. No sound. Simply flash the echo area."
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist '((default highlight))))
  (run-with-timer 0.15 nil (lambda ()
                             (with-current-buffer (get-buffer " *Echo Area 0*")
                               (setq-local face-remapping-alist '((default)))))))

;; Blinking is NOT OK
(blink-cursor-mode -1)

;; Beeping is REALLY NOT OK
(setq visible-bell t
      ring-bell-function 'echo-area-visible-bell
      ;; Show keystrokes right away, don't show the message in the scratch buffer
      echo-keystrokes 0.1)

(pixel-scroll-mode)

;; Use the system clipboard
(setq select-enable-clipboard t
      ;; Save existing system clipboard text into kill ring before replacing it,
      ;; ensuring it doesn't get irrevocably destroyed.
      save-interprogram-paste-before-kill t
      ;; use mouse to kill/yank
      mouse-yank-at-point t
      mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cut-when-buffers-differ t
      ;; No GUI dialogs
      use-dialog-box nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

(defun next-line-4 ()
  "Scroll 4 lines down."
  (interactive)
  (forward-line 4))

(defun previous-line-4 ()
  "Scroll 4 lines up."
  (interactive)
  (forward-line -4))

(bind-keys
 ("C-S-p" . previous-line-4)
 ("C-S-n" . next-line-4))

;; Smoother and nicer scrolling
(setq scroll-margin 6
      scroll-step 1
      scroll-conservatively 10000
      ;; Reduce scrolling lag
      ;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
      scroll-preserve-screen-position 1
      auto-window-vscroll nil
      mouse-wheel-follow-mouse 't
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on disk.
(require 'autorevert)
(global-auto-revert-mode 1)
;; Auto refresh dired
(setq global-auto-revert-non-file-buffers t)

;; Full screen
(defun fullscreen ()
  "Toggle fullscreen mode."
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(bind-keys ("C-s-f" . fullscreen))

;; Change yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

(defun clipboard-yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (if (and delete-selection-mode (use-region-p)) (delete-active-region))
  (clipboard-yank)
  (call-interactively 'indent-region))

(defun cut-line-or-region ()
  "Cut the current line or text selection.

When `universal-argument' is called first, cut whole the
buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun copy-line-or-region ()
  "Copy the current line or text selection.
When called repeatedly, append copy subsequent lines. When
`universal-argument' is called first, copy whole buffer (respects
`narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2017-12-04"
  (interactive)
  (if current-prefix-arg
      (progn
        (kill-ring-save (point-min) (point-max)))
    (if (use-region-p)
        (progn
          (kill-ring-save (region-beginning) (region-end)))
      (if (eq last-command this-command)
          (if (eobp)
              (progn)
            (progn
              (kill-append "\n" nil)
              (kill-append
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               nil)
              (progn
                (end-of-line)
                (forward-char))))
        (if (eobp)
            (if (eq (char-before) 10)
                (progn)
              (progn
                (kill-ring-save (line-beginning-position) (line-end-position))
                (end-of-line)))
          (progn
            (kill-ring-save (line-beginning-position) (line-end-position))
            (end-of-line)
            (forward-char)))))))

(defun comment-toggle ()
  "Toggle comments for the region.
If no region is selected, toggles comments for the line."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end))
  (if (bound-and-true-p parinfer-mode) (parinfer--invoke-parinfer)))

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (beginning-of-line)
  (set-mark (line-end-position)))

;; Key bindings to make moving between Emacs and other appliations a bit less
;; jarring. These are mostly based on macOS defaults but an effor has been made
;; to work on Windows and Linux. That is why there are multiple bindings for
;; many commands. They can be overridden by the OS specific configurations
;; below.
(bind-keys
 ("s-o" . find-file)
 ("s-O" . find-file-other-window)
 ("s-s" . save-buffer)
 ("s-S" . write-file)
 ("s-q" . save-buffers-kill-emacs)
 ("s-z" . undo)
 ("C-z" . undo)
 ("s-x" . cut-line-or-region)
 ("s-c" . copy-line-or-region)
 ("s-v" . clipboard-yank-and-indent)
 ("s-a" . mark-whole-buffer)
 ("s-g" . isearch-repeat-forward)
 ("s-G" . isearch-repeat-backward)
 ("C-S-s" . isearch-forward-symbol-at-point)
 ("s-l" . select-current-line)
 ("C-S-L" . select-current-line)
 ("s-\`" . other-frame)
 ("C-\`" . other-frame)
 ("s-N" . make-frame-command)
 ("s-w" . delete-window)
 ("s-W" . delete-other-windows)
 ("s-C-w" . delete-frame)
 ("s-/" . comment-toggle)
 ("s-h" . ns-do-hide-emacs)
 ("s-H" . ns-do-hide-others)
 ("s-i" . os-reveal-file)
 ("C-c i" . os-reveal-file))

(use-package goto-addr
  :hook
  ((compilation-mode . goto-address-mode)
   (prog-mode . goto-address-prog-mode)
   (eshell-mode . goto-address-mode)
   (shell-mode . goto-address-mode))
  :commands
  (goto-address-prog-mode
   goto-address-mode)
  :bind
  (:map goto-address-highlight-keymap
        ("C-c C-o" . goto-address-at-point)))

(defvar os-open-file-executable nil)

(defun os-open-file (file)
  "Open FILE using the operating system's GUI file opener."
  (interactive)
  (message "Opening %s..." file)
  (call-process os-open-file-executable nil 0 nil file))

(defun config-unix ()
  "Configure Emacs for common Unix (Linux and macOS) settings."
  "")
;; The default for unix is /bin/bash but on macOS, brew installs bash to
;; /usr/local/bin.
;; This is commented out because it messes with Eshell/tramp.
;; (setq-default shell-file-name (executable-find "bash")
;;               explicit-shell-file-name shell-file-name))

(defun config-linux ()
  "Configure Emacs for Linux."
  (config-unix))

(defun config-macos ()
  "Configure Emacs for macOS."
  (config-unix)
  (setq ns-alternate-modifier 'meta
        ns-right-alternate-modifier 'none
        ns-command-modifier 'super
        ns-right-command-modifier 'left
        ns-control-modifier 'control
        ns-right-control-modifier 'left
        ns-function-modifier 'hyper
        ;; Open files from Finder in same frame.
        ns-pop-up-frames nil
        os-open-file-executable "open")
  (when window-system (menu-bar-mode +1))
  (set-face-font 'default "Monaco-13")
  (set-face-attribute 'default nil
                      :weight 'light)
  ;; Use system trash
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash")

  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
    (call-process (or (executable-find "trash") (executable-find "rm"))
                  nil 0 nil
                  file))

  (use-package reveal-in-osx-finder
    :config
    (defalias 'os-reveal-file #'reveal-in-osx-finder)))

(defun reveal-in-windows-explorer (&optional file)
  "Reveal the current file in the operating system's file manager."
  (interactive)
  (unless file (setq file buffer-file-name))
  (os-open-file (concat "/select," (dired-replace-in-string "/" "\\" file))))

(defun config-windows ()
  "Configure Emacs for Windows."
  (menu-bar-mode -1)
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super
        os-open-file-executable "explorer")
  (set-face-font 'default "Lucida Console-12")

  (defalias 'os-reveal-file #'reveal-in-windows-explorer))


;; OS specific configuration
(pcase system-type
  ('darwin (config-macos))
  ('gnu/linux (config-linux))
  ('windows-nt (config-windows))
  ('cygwin (config-windows)))

(use-package ryo-modal
  :commands ryo-modal-mode
  :bind
  ("s-m" . ryo-modal-mode)
  :config
  (ryo-modal-keys
   ("," ryo-modal-repeat)
   ("q" ryo-modal-mode)
   ("b" backward-char)
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char))

  (ryo-modal-keys
   ;; First argyment to ryo-modal-keys may be a list of keywords.
   ;; These keywords will be applied to all keybindings.
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; savehist
(require 'savehist)
(setq savehist-autosave-interval 60
      history-length t
      history-delete-duplicates t
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring
                                      file-name-history
                                      magit-read-rev-history
                                      read-expression-history
                                      command-history
                                      extended-command-history
                                      ivy-history))
(savehist-mode 1)

;; save-place
(save-place-mode 1)

;; recentf
(require 'recentf)
(setq recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; Disable recentf-cleanup on Emacs start because it can cause problems
      ;; with remote files.
      recentf-auto-cleanup 'never)

(recentf-mode 1)

;; Track directories in the recentf list
(defun recentd-track-opened-file ()
  "Insert the name of the directory just opened into the recent list."
  (and (derived-mode-p 'dired-mode) default-directory
       (recentf-add-file default-directory))
  ;; Must return nil because it is run from `write-file-functions'.
  nil)
(add-hook 'dired-after-readin-hook #'recentd-track-opened-file)

;; Store all backup and autosave files in their own directory since it is bad to
;; clutter project directories.
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))
      ;; Don't clobber symlinks.
      backup-by-copying t
      ;; Don't break multiple hardlinks.
      backup-by-copying-when-linked t
      ;; Use version numbers for backup files.
      version-control t
      ;; Backup even if file is in vc.
      vc-make-backup-files t
      ;; Keep all versions forever.
      delete-old-versions -1
      auto-save-list-file-prefix "~/.emacs.d/autosave/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
      ;; Don't create `#filename' lockfiles in $PWD. Lockfiles are useful but it
      ;; generates too much activity from tools watching for changes during
      ;; development.
      create-lockfiles nil)

;; Desktop
(require 'desktop)
(add-to-list 'desktop-globals-to-save 'kill-ring)
(add-to-list 'desktop-globals-to-save 'a-theme-current-theme)
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq suggest-key-bindings 5
      ;; Select help window so it's easy to quit it with `q'
      help-window-select t)

(bind-keys
 ("C-h C-i" . #'elisp-index-search)
 ("C-h M-i" . #'info-apropos))

;; ELDoc
(seq-do (lambda (list) (add-hook list #'turn-on-eldoc-mode))
        '(emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          ielm-mode-hook))

;; Whenever the listed commands are used, ElDoc will automatically refresh the
;; minibuffer.
(eldoc-add-command 'paredit-backward-delete 'paredit-close-round)

(use-package help-fns+)

(use-package helpful
  :bind
  ("C-h ." . helpful-at-point)
  ("C-h f" . helpful-callable)
  ("C-h c" . helpful-command)
  ("C-h F" . helpful-function)
  ("C-h k" . helpful-key)
  ("C-h M" . helpful-macro)
  ("C-h M-s" . helpful-symbol)
  ("C-h v" . helpful-variable))

(use-package elisp-demos
  :straight
  (:type git :host github :repo "xuchunyang/elisp-demos" :files ("elisp-demos.*"))
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package which-key
  :demand t
  :config
  (which-key-mode t)
  :bind
  ("M-s-h" . which-key-show-top-level))

(use-package man
  :custom
  ;; Make the manpage the current buffer in the current window
  (Man-notify-method 'pushy)
  :config
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

;; (use-package info-colors
;;   :commands
;;   (info-colors-fontify-node)
;;   :config
;;   (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;; (use-package define-word
;;   :bind
;;   ("C-c W" . define-word)
;;   ("C-c w" . define-word-at-point))

(use-package tldr
  ;; :ensure-system-package tldr
  :init
  (unbind-key "C-h t")
  :custom
  (tldr-enabled-categories '("common" "linux" "osx"))
  :bind
  ("C-h t t" . tldr)
  ("C-h t u" . tldr-update-docs))

;; (defface eg-h1
;;   '((((class color) (background light))
;;      (:foreground "#ff8700" :bold t :height 2.0))
;;     (((class color) (background dark))
;;      (:foreground "#ffa722" :bold t :height 2.0)))
;;   ""
;;   :group 'eg)

;; (defface eg-h2
;;   '((((class color) (background light))
;;      (:foreground "#1f5bff" :bold t :height 1.2))
;;     (((class color) (background dark))
;;      (:foreground "#6faaff" :bold t :height 1.2)))
;;   ""
;;   :group 'eg)

;; (defface eg-h3
;;   '((((class color) (background light))
;;      (:foreground "#5a5a5a" :bold t))
;;     (((class color) (background dark))
;;      (:foreground "#d7ff87" :bold t)))
;;   ""
;;   :group 'eg)

;; (defface eg-code-block
;;   '((((class color) (background light))
;;      (:foreground "#555" :background "#d7ff87"))
;;     (((class color) (background dark))
;;      (:foreground "#eee" :background "#5a5a5a")))
;;   ""
;;   :group 'eg)

;; (defun eg (command)
;;   "Run the `eg' command (https://github.com/srsudar/eg) and
;; display as if from a terminal."
;;   (interactive
;;    (list (completing-read
;;           "eg: "
;;           (let ((l (split-string (shell-command-to-string "eg --list"))))
;;             (nthcdr (1+ (position "eg:" l :test #'string=)) l)))))
;;   (pop-to-buffer (get-buffer-create (concat "*eg: " command "*")))
;;   (insert
;;    (mapconcat (lambda (line)
;;                 (cond
;;                  ((equal "" line)
;;                   "")
;;                  ((string-prefix-p "# " line)
;;                   (propertize (substring line 2) 'face 'eg-h1))
;;                  ((string-prefix-p "## " line)
;;                   (propertize (substring line 3) 'face 'eg-h2))
;;                  ((string-prefix-p "### " line)
;;                   (propertize (substring line 4) 'face 'eg-h3))
;;                  ((string-prefix-p "    " line)
;;                   (concat "    " (propertize (substring line 4) 'face 'eg-code-block)))
;;                  (t line)))
;;               (split-string
;;                (shell-command-to-string
;;                 (concat "eg --no-color --pager-cmd cat " command))
;;                "\n")
;;               "\n"))
;;   (goto-char (point-min))
;;   (help-mode))

(use-package dash-docs
  :straight
  (:type git :host github :repo "gilbertw1/dash-docs")
  :custom
  (dash-docs-docsets-path "~/code/docsets")
  (dash-docs-browser-func #'eww)
  (dash-docs-common-docsets '("Bash"
                              "CSS"
                              "ClojureDocs"
                              "ClojureScript"
                              "Docker"
                              "Emacs Lisp"
                              "Hammerspoon"
                              "JavaScript"
                              "Lua"
                              "NodeJS"
                              "PouchDB"
                              "Python 3"
                              "React"
                              "SQLite"
                              "caniuse"
                              "jsdoc"
                              "use-package")))

(use-package counsel-dash
  ;; :ensure-system-package sqlite3
  :straight
  (:type git :host github :repo "gilbertw1/counsel-dash")
  :commands
  (counsel-dash counsel-dash-at-point counsel-dash-install-docset)
  :bind
  ("M-s-l" . counsel-dash)
  ("M-s-." . counsel-dash-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer Navigation and Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filter-buffers-by-name (regexp)
  "Return a list of buffers whose names match REGEXP."
  (seq-filter (lambda (b) (string-match-p regexp (buffer-name b)))
              (buffer-list)))

(defun filter-buffers-by-mode (mode)
  "Return a list of buffers whose major mode is MODE."
  (when (stringp mode) (setq mode (intern mode)))
  (seq-filter (lambda (b) (eq (buffer-local-value 'major-mode b) mode))
              (buffer-list)))

(defun some-buffer (regexp)
  "Return the first buffer found with a name matching REGEXP, or nil."
  (cl-some (lambda (b) (when (string-match-p regexp (buffer-name b)) b))
           (buffer-list)))

(defun list-buffer-major-modes ()
  "Return a list of all major modes currently in use in open buffers."
  (delete-dups
   (cl-loop for b in (buffer-list)
            collect (buffer-local-value 'major-mode b))))

(defun switch-to-buffer-by-mode (mode)
  "Interactively choose a major MODE, then choose a buffer of that mode."
  (interactive
   (list (ivy-read "Choose buffers for major mode: "
                   (list-buffer-major-modes)
                   :history 'switch-to-buffer-by-mode-history
                   :action 'switch-to-buffer-by-mode)))
  (when (stringp mode) (setq mode (intern mode)))
  (let ((buffers (mapcar #'buffer-name (filter-buffers-by-mode mode))))
    (ivy-read (format "%s buffers: " mode) buffers
              :keymap ivy-switch-buffer-map
              :action #'ivy--switch-buffer-action
              :matcher #'ivy--switch-buffer-matcher
              :preselect (when (eq major-mode mode) (cadr buffers))
              ;; Use the `ivy-switch-buffer' hydra.
              :caller #'ivy-switch-buffer)))

;; Quick switch buffers
(bind-keys ("C-x C-b" . ibuffer)
           ("s-}" . next-buffer)
           ("C-c }" . next-buffer)
           ("s-{" . previous-buffer)
           ("C-c {" . previous-buffer)
           ("C-s-j" . switch-to-buffer-by-mode)
           ("C-c M-j" . switch-to-buffer-by-mode))

(bind-keys ("H-a" . windmove-left)
           ("H-h" . windmove-left)
           ("H-d" . windmove-right)
           ("H-l" . windmove-right)
           ("H-w" . windmove-up)
           ("H-j" . windmove-up)
           ("H-s" . windmove-down)
           ("H-k" . windmove-down)
           ("M-]" . windmove-right)
           ("M-[" . windmove-left))

;; Navigating with mark
(bind-keys ("M-s-," . pop-to-mark-command)
           ("C-c ," . pop-to-mark-command)
           ("s-," . pop-global-mark)
           ("C-c C-," . pop-global-mark))

;; scratch
(setq initial-scratch-message nil
      initial-major-mode 'lisp-interaction-mode)

(defun new-scratch-buffer ()
  "Create or go to a scratch buffer in the current mode.
If ARG is provided then use `initial-major-mode'. Try these
  things in succession\:

1. Select an existing window containing the scratch buffer.
2. Switch to an existing scratch buffer.
3. Create a new scratch buffer and switch to it."
  (interactive)
  (let* ((mode (if current-prefix-arg initial-major-mode major-mode))
         (name (format "<%s>" (symbol-name mode)))
         (win (get-buffer-window name)))
    (cond
     (win (select-window win))
     (t (switch-to-buffer (get-buffer-create name))
        (setq buffer-file-name name)
        (funcall mode)))))

(bind-keys ("s-n" . new-scratch-buffer)
           ("C-c C-n" . new-scratch-buffer))

(setq display-buffer-alist
      `((,(rx bos
              (or "*Apropos*" "*eww*" "*Help*" "*helpful" "*info*" "*Summary*")
              (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-pop-up-window)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

;; kill buffer and window
(defun kill-other-buffer-and-window ()
  "Kill the buffer in the other window."
  (interactive)
  (select-window (next-window))
  (kill-buffer-and-window))

(bind-keys ("M-s-w" . kill-buffer-and-window)
           ("M-s-W" . kill-other-buffer-and-window))

;; https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  "Toggle windows between horizontal and vertical split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(bind-keys ("M-s-<up>" . shrink-window)
           ("M-s-<down>" . enlarge-window)
           ("M-s-<left>" . shrink-window-horizontally)
           ("M-s-<right>" . enlarge-window-horizontally)
           :map ctl-x-4-map
           ("t" . toggle-window-split))

;; Tags
(bind-keys ("s-R" . xref-find-definitions-other-window)
           ("C-c M-r" . xref-find-definitions-other-window))

;; Create friendly names for buffers with the same name
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(require 'ffap)

(defun find-file-at-point-with-line (&optional filename)
  "Open FILENAME at point and move point to line specified next to file name."
  (interactive)
  (let* ((filename (or filename (if current-prefix-arg (ffap-prompter) (ffap-guesser))))
         (line-number
          (and (or (looking-at ".* line \\(\[0-9\]+\\)")
                   (looking-at "[^:]*:\\(\[0-9\]+\\)"))
               (string-to-number (match-string-no-properties 1))))
         (column-number
          (or
           (and (looking-at "[^:]*:\[0-9\]+:\\(\[0-9\]+\\)")
                (string-to-number (match-string-no-properties 1)))
           0)))
    (message "%s --> %s:%s" filename line-number column-number)
    (cond ((ffap-url-p filename)
           (let (current-prefix-arg)
             (funcall ffap-url-fetcher filename)))
          ((and line-number
                (file-exists-p filename))
           (progn (find-file-other-window filename)
                  (goto-char (point-min))
                  (forward-line (1- line-number))
                  (forward-char column-number)))
          ((and ffap-pass-wildcards-to-dired
                ffap-dired-wildcards
                (string-match ffap-dired-wildcards filename))
           (funcall ffap-directory-finder filename))
          ((and ffap-dired-wildcards
                (string-match ffap-dired-wildcards filename)
                find-file-wildcards
                ;; Check if it's find-file that supports wildcards arg
                (memq ffap-file-finder '(find-file find-alternate-file)))
           (funcall ffap-file-finder (expand-file-name filename) t))
          ((or (not ffap-newfile-prompt)
               (file-exists-p filename)
               (y-or-n-p "File does not exist, create buffer? "))
           (funcall ffap-file-finder
                    ;; expand-file-name fixes "~/~/.emacs" bug sent by CHUCKR.
                    (expand-file-name filename)))
          ;; User does not want to find a non-existent file:
          ((signal 'file-error (list "Opening file buffer"
                                     "no such file or directory"
                                     filename))))))

(bind-key "C-c C-f" #'find-file-at-point-with-line)

(defun parse-colon-notation (filename)
  "Parse FILENAME in the format expected by `server-visit-files'.
Modify it so that `filename:line:column' is is reformatted the
way Emacs expects."
  (let ((name (car filename)))
    (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
        (cons
         (match-string 1 name)
         (cons (string-to-number (match-string 2 name))
               (string-to-number (or (match-string 3 name) ""))))
      filename)))

(defun wrap-colon-notation (f &rest args)
  "Wrap F (`server-visit-files') and modify ARGS to support colon notation.
Open files with emacsclient with cursors according to colon
notation. When the file name has line numbers and optionally
columns specified like `filename:line:column', parse those and
return them in the Emacs format."
  (message "%s" args)
  (apply f (cons (mapcar #'parse-colon-notation (car args)) (cdr args))))

(advice-add 'server-visit-files :around #'wrap-colon-notation)

;; If this variable is t, VC follows the link and visits the real file, telling you about it in the echo area.
(setq vc-follow-symlinks t)

(use-package winner
  :init
  (winner-mode)
  :bind
  (("C-c [" . winner-undo)
   ("s-[" . winner-undo)
   ("C-c ]" . winner-redo)
   ("s-]" . winner-redo)))

(use-package buffer-move
  :bind
  (("C-H-W" . buf-move-up)
   ("C-H-S" . buf-move-down)
   ("C-H-A" . buf-move-left)
   ("C-H-D" . buf-move-right)))

(use-package ace-window
  :bind
  (("M-o" . ace-window)))

(use-package winum
  :custom
  (winum-auto-setup-mode-line nil)
  :config
  (winum-mode)
  :bind
  ("s-1" . winum-select-window-1)
  ("C-c 1" . winum-select-window-1)
  ("s-2" . winum-select-window-2)
  ("C-c 2" . winum-select-window-2)
  ("s-3" . winum-select-window-3)
  ("C-c 3" . winum-select-window-3)
  ("s-4" . winum-select-window-4)
  ("C-c 4" . winum-select-window-4)
  ("s-5" . winum-select-window-5)
  ("C-c 5" . winum-select-window-5)
  ("s-6" . winum-select-window-6)
  ("C-c 6" . winum-select-window-6)
  ("s-7" . winum-select-window-7)
  ("C-c 7" . winum-select-window-7)
  ("s-8" . winum-select-window-8)
  ("C-c 8" . winum-select-window-8)
  ("s-9" . winum-select-window-9)
  ("C-c 9" . winum-select-window-9)
  ("s-0" . winum-select-window-0)
  ("C-c 0" . winum-select-window-0))

(use-package eyebrowse
  :custom
  (eyebrowse-new-workspace t)
  (eyebrowse-mode-line-separator " ")
  :config
  (eyebrowse-mode t)
  :bind
  ("H-1" . eyebrowse-switch-to-window-config-1)
  ("C-c C-1" . eyebrowse-switch-to-window-config-1)
  ("H-2" . eyebrowse-switch-to-window-config-2)
  ("C-c C-2" . eyebrowse-switch-to-window-config-2)
  ("H-3" . eyebrowse-switch-to-window-config-3)
  ("C-c C-3" . eyebrowse-switch-to-window-config-3)
  ("H-4" . eyebrowse-switch-to-window-config-4)
  ("C-c C-4" . eyebrowse-switch-to-window-config-4)
  ("H-5" . eyebrowse-switch-to-window-config-5)
  ("C-c C-5" . eyebrowse-switch-to-window-config-5)
  ("H-6" . eyebrowse-switch-to-window-config-6)
  ("C-c C-6" . eyebrowse-switch-to-window-config-6)
  ("H-7" . eyebrowse-switch-to-window-config-7)
  ("C-c C-7" . eyebrowse-switch-to-window-config-7)
  ("H-8" . eyebrowse-switch-to-window-config-8)
  ("C-c C-8" . eyebrowse-switch-to-window-config-8)
  ("H-9" . eyebrowse-switch-to-window-config-9)
  ("C-c C-9" . eyebrowse-switch-to-window-config-9)
  ("H-0" . eyebrowse-switch-to-window-config-0)
  ("C-c C-0" . eyebrowse-switch-to-window-config-0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wrap text.
(setq-default fill-column 80)

;; Newline at end of file.
(setq require-final-newline t
      ;; Sentences end with one space.
      sentence-end-double-space nil)


;; Delete selection on insert or yank
(delete-selection-mode 1)

;; Tabs
(setq-default indent-tabs-mode nil
              tab-width 2
              tab-stop-list (number-sequence tab-width 120 tab-width))

;; Replace `delete-horizontal-space' with the more useful `cycle-spacing'.
(bind-key "M-\\" #'cycle-spacing)

;; sh-mode
(use-package sh-script
  :custom
  (sh-basic-offset tab-width)
  (sh-indentation tab-width)
  ;; Tell `executable-set-magic' to insert #!/usr/bin/env interpreter
  (executable-prefix-env t))

;; Make a shell script executable automatically on save
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(defun maybe-reset-major-mode ()
  "Reset the buffer's `major-mode' if a different mode seems like a better fit.
Mostly useful as a `before-save-hook', to guess mode when saving a
new file for the first time.
https://github.com/NateEag/.emacs.d/blob/9d4a2ec9b5c22fca3c80783a24323388fe1d1647/init.el#L137"
  (when (and
         ;; The buffer's visited file does not exist.
         (eq (file-exists-p (buffer-file-name)) nil)
         (eq major-mode 'fundamental-mode))
    (normal-mode)))

(add-hook 'before-save-hook #'maybe-reset-major-mode)

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("\\DWfile.*\\'" . sh-mode))

;; Move by whole words rather than sub-words
;; (global-superword-mode)

;; Automatically indent after RET
(electric-indent-mode +1)

;; Guess the indentation of the file and continue to use that.
;; (use-package dtrt-indent
;;   :hook
;;   ((prog-mode text-mode) . dtrt-indent-mode))

;; http://whattheemacsd.com/key-bindings.el-03.html
(defun join-line-previous ()
  "Like `delete-indentation', but bring the line below up to the current line."
  (interactive)
  (join-line -1))

;; It's the reverse of `delete-indentation'.
(bind-key "C-^" #'join-line-previous)

(defun dos2unix ()
  "Convert DOS line endings to Unix ones."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t)))
  (set-buffer-file-coding-system 'unix 't))

(use-package string-inflection
  :bind
  ("C-c C-u" . string-inflection-all-cycle))

(defun unix2dos ()
  "Convert Unix encoded buffer to DOS encoding.
https://edivad.wordpress.com/2007/04/03/emacs-convert-dos-to-unix-and-vice-versa/"
  (interactive)
  (set-buffer-file-coding-system 'dos))

(defun touch (cmd)
  "Run `touch CMD' in `default-directory'."
  (interactive
   (list (read-shell-command "Run touch (like this): "
                             "touch "
                             'touch-history
                             "touch ")))
  (shell-command cmd))

;; (use-package editorconfig
;;   :config
;;   (editorconfig-mode 1))

;; Just set up 3 windows, no fancy frames or whatever
(require 'ediff)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; Continue comment on next line (default binding is "C-M-j")
(bind-key "M-RET" #'indent-new-comment-line)

(defun configure-auto-fill-mode ()
  "Automatically fill comments.
Wraps on `fill-column' columns."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook #'configure-auto-fill-mode)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for line number N."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (forward-line)
        (let ((n (1- (read-number "Go to line: "))))
          (goto-char (point-min))
          (forward-line n)))
    (linum-mode -1)))

(bind-key [remap goto-line] #'goto-line-with-feedback)

;; One key binding to move/copy text from current other window
;; https://emacs.stackexchange.com/questions/3743/how-to-move-region-to-other-window
(defun move-region-to-other-window (beg end)
  "Move selected text (from BEG to END) to other window."
  (interactive "r")
  (if (use-region-p)
      (let ((count (count-words-region beg end)))
        (save-excursion
          (kill-region beg end)
          (other-window 1)
          (yank)
          (newline))
        (other-window -1)
        (message "Moved %s words" count))
    (message "No region selected")))

(defun copy-region-to-other-window (beg end)
  "Move selected text (from BEG to END) to other window."
  (interactive "r")
  (if (use-region-p)
      (let ((count (count-words-region beg end)))
        (save-excursion
          (copy-region-as-kill beg end)
          (other-window 1)
          (yank)
          (newline))
        (other-window -1)
        (message "Moved %s words" count))
    (message "No region selected")))

(bind-keys ("s-C" . copy-region-to-other-window)
           ("s-X" . move-region-to-other-window))

(use-package undohist
  :config
  (undohist-initialize)
  :defer 1)

(use-package undo-propose
  :bind
  ("C-s-z" . undo-propose))

(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

(use-package ace-jump-zap
  :bind
  ("M-z" . ace-jump-zap-up-to-char-dwim)
  ("C-M-z" . ace-jump-zap-to-char-dwim))

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
  ([remap move-end-of-line] . mwim-end-of-code-or-line))

(use-package expand-region
  :custom
  (expand-region-fast-keys-enabled nil)
  :bind
  ("s-d" . er/expand-region)
  ("C-=" . er/expand-region)
  ("s-D" . er/contract-region)
  ("C-M-=" . er/contract-region))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("M-s-m" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/unmark-next-like-this)
  ("C-M-<" . mc/mark-previous-like-this)
  ("C-M->" . mc/unmark-previous-like-this)
  ("C-c >" . mc/mark-all-dwim)
  ("C-c C-a"  . mc/mark-all-dwim))

(use-package mc-extras
  :bind
  (:map mc/keymap
        ("C-. M-C-f" . mc/mark-next-sexps)
        ("C-. M-C-b" . mc/mark-previous-sexps)
        ("C-. <" . mc/mark-all-above)
        ("C-. >" . mc/mark-all-below)
        ("C-. C-d" . mc/remove-current-cursor)
        ("C-. C-k" . mc/remove-cursors-at-eol)
        ("C-. d" . mc/remove-duplicated-cursors)
        ("C-. C-." . mc/freeze-fake-cursors-dwim)
        ("C-. ." . mc/move-to-column)
        ("C-. =" . mc/compare-chars)))

(use-package move-text
  :bind
  (:map prog-mode-map
        ("M-S-<up>" . move-text-up)
        ("M-S-<down>" . move-text-down)))

(use-package goto-last-change
  :bind
  ("C-\\" . goto-last-change))

(defun outline-show-current-sublevel ()
  "Show only the current top level section."
  (interactive)
  (unless outline-minor-mode
    (outline-minor-mode t))
  (outline-hide-sublevels 1)
  (outline-show-subtree))

(defun outline-subtree-previous ()
  "Go to and expand previous sublevel."
  (interactive)
  (unless outline-minor-mode
    (outline-minor-mode t))
  (outline-hide-sublevels 1)
  (outline-previous-visible-heading 1)
  (outline-show-subtree))

(defun outline-subtree-next ()
  "Go to and expand previous sublevel."
  (interactive)
  (unless outline-minor-mode
    (outline-minor-mode t))
  (outline-hide-sublevels 1)
  (outline-next-visible-heading 1)
  (outline-show-subtree))

;; outline-mode extension for navigating by sections. in Emacs Lisp that is defined by
;; `;;; ', `;;;; ', etc. Everywhere else it is like `;; * ' `;; ** ', and so on.
(use-package outshine
  :init
  (defvar outline-minor-mode-prefix "\M-#")
  :config
  ;; Narrowing now works within the headline rather than requiring to be on it
  (advice-add 'outshine-narrow-to-subtree :before
              (lambda (&rest _args) (unless (outline-on-heading-p t)
                                      (outline-previous-visible-heading 1))))
  :hook
  (prog-mode . outshine-mode)
  :bind
  (:map outline-minor-mode-map
        ;; Don't shadow smarparens or org bindings
        ("M-<up>" . nil)
        ("M-<down>" . nil)
        ("M-=" . outline-show-current-sublevel)
        ("M-p" . outline-subtree-previous)
        ("M-n" . outline-subtree-next)))

;; hs-minor-mode for folding top level forms
(use-package hideshow
  :custom
  (hs-hide-comments-when-hiding-all nil)
  :bind
  ("C-<tab>" . hs-toggle-hiding)
  :hook
  (hs-minor-mode . hs-hide-all))

(use-package unfill
  :bind
  ("M-q" . unfill-toggle))

;; (use-package visual-regexp-steroids
;;   :bind
;;   (("C-c r" . vr/replace)
;;    ("C-c q" . vr/query-replace)
;;    ("C-c m" . vr/mc-mark)))

(use-package rainbow-mode
  :hook
  ((css-mode emacs-lisp-mode help-mode sass-mode) . rainbow-mode))

;; (use-package hl-todo
;;   :commands
;;   (global-hl-todo-mode)
;;   :config
;;   (global-hl-todo-mode))

(use-package yasnippet
  :defer 2
  :config
  (yas-global-mode 1)
  :bind
  (("C-c C-s" . yas-insert-snippet)))

(use-package yasnippet-snippets
  :defer 2)

(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(save mode-enable))
  :hook
  (js2-mode . flycheck-mode)
  :bind
  ("C-c ! !" . flycheck-mode))

(use-package format-all
  :commands
  (format-all-buffer format-all-mode))

(defun indent-buffer-or-region ()
  "Indent the region if one is active, otherwise format the buffer.
Some modes have special treatment."
  (interactive)
  (if (use-region-p)
      (progn
        (indent-region (region-beginning) (region-end))
        (message "Region indented."))
    (progn
      (format-all-buffer)
      (message "Buffer formatted."))))

(bind-key "C-M-\\" #'indent-buffer-or-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; S-Expressions, Parentheses, Brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sp-sh-post-handler (_id action _context)
  "Bash post handler ID, ACTION, CONTEXT."
  (-let (((&plist :arg arg :enc enc) sp-handler-context))
    (when (equal action 'barf-backward)
      (sp-ruby-delete-indentation 1)
      (indent-according-to-mode)
      (save-excursion
        (sp-backward-sexp)              ; move to begining of current sexp
        (sp-backward-sexp arg)
        (sp-ruby-maybe-one-space)))

    (when (equal action 'barf-forward)
      (sp-get enc
        (let ((beg-line (line-number-at-pos :beg-in)))
          (sp-forward-sexp arg)
          (sp-ruby-maybe-one-space)
          (when (not (= (line-number-at-pos) beg-line))
            (sp-ruby-delete-indentation -1))
          (indent-according-to-mode))))))

(defun sp-sh-block-post-handler (id action context)
  "Handler for bash block insertions.
ID, ACTION, CONTEXT."
  (when (equal action 'insert)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))
  (sp-sh-post-handler id action context))

(defun sp-sh-pre-handler (_id action _context)
  "Handler for bash slurp and barf.
ID, ACTION, CONTEXT."
  (let ((enc (plist-get sp-handler-context :enc)))
    (sp-get enc
      (let ((beg-line (line-number-at-pos :beg-in))
            (end-line (line-number-at-pos :end-in)))

        (when (equal action 'slurp-backward)
          (save-excursion
            (sp-forward-sexp)
            (when (looking-at-p ";") (forward-char))
            (sp-ruby-maybe-one-space)
            (when (not (= (line-number-at-pos) end-line))
              (sp-ruby-delete-indentation -1)))
          (while (thing-at-point-looking-at "\\.[[:blank:]\n]*")
            (sp-backward-sexp))
          (when (looking-back "[@$:&?!]")
            (backward-char)
            (when (looking-back "[@&:]")
              (backward-char)))
          (just-one-space)
          (save-excursion
            (if (= (line-number-at-pos) end-line)
                (insert " ")
              (newline))))

        (when (equal action 'barf-backward)
          ;; Barf whole method chains
          (while (thing-at-point-looking-at "[(.:[][\n[:blank:]]*")
            (sp-forward-sexp))
          (if (looking-at-p " *$")
              (newline)
            (save-excursion (newline))))

        (when (equal action 'slurp-forward)
          (save-excursion
            (sp-backward-sexp)
            (when (looking-back "\." nil) (backward-char))
            (sp-ruby-maybe-one-space)
            (when (not (= (line-number-at-pos) beg-line))
              (if (thing-at-point-looking-at "\\.[[:blank:]\n]*")
                  (progn
                    (forward-symbol -1)
                    (sp-ruby-delete-indentation -1))
                (sp-ruby-delete-indentation))))
          (while (looking-at-p "::") (sp-forward-symbol))
          (when (looking-at-p "[?!;]") (forward-char))
          (if (= (line-number-at-pos) beg-line)
              (insert " ")
            (newline)))

        (when (equal action 'barf-forward)
          (when (looking-back "\\." nil) (backward-char))
          (while (looking-back "::" nil) (sp-backward-symbol))
          (if (= (line-number-at-pos) end-line)
              (insert " ")
            (if (looking-back "^[[:blank:]]*" nil)
                (save-excursion (newline))
              (newline))))))))

(defun sp-backward-slurp-into-previous-sexp ()
  "Add the sexp at point into the preceeding list."
  (interactive)
  (save-excursion
    (sp-down-sexp)
    (sp-backward-symbol)
    (sp-forward-slurp-sexp)))

;; See https://github.com/Fuco1/smartparens/issues/80
(defun sp-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package smartparens
  :demand t
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-hybrid-kill-entire-symbol nil)
  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (sp-cancel-autoskip-on-backward-movement nil)
  :config
  (bind-key [remap kill-line] #'sp-kill-hybrid-sexp smartparens-mode-map
            (apply #'derived-mode-p sp-lisp-modes))
  (sp-with-modes
      '(c-mode c++-mode css-mode javascript-mode js2-mode json-mode objc-mode
               python-mode java-mode sh-mode web-mode)
    (sp-local-pair "{" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET")))
    (sp-local-pair "[" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET")))
    (sp-local-pair "(" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET"))))
  (sp-with-modes
      '(python-mode)
    (sp-local-pair "\"\"\"" "\"\"\""
                   :post-handlers '((sp-create-newline-and-enter-sexp "RET"))))
  (sp-with-modes
      '(sh-mode)
    (sp-local-pair "do" "done"
                   :when '(("SPC" "RET"))
                   :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                   :actions '(insert navigate)
                   :pre-handlers '(sp-sh-pre-handler)
                   :post-handlers '(sp-sh-block-post-handler))
    (sp-local-pair "then" "fi"
                   :when '(("SPC" "RET"))
                   :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                   :actions '(insert navigate)
                   :pre-handlers '(sp-sh-pre-handler)
                   :post-handlers '(sp-sh-block-post-handler))
    (sp-local-pair "case" "esac"
                   :when '(("SPC" "RET"))
                   :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                   :actions '(insert navigate)
                   :pre-handlers '(sp-sh-pre-handler)
                   :post-handlers '(sp-sh-block-post-handler)))
  (smartparens-global-mode)
  :hook
  (smartparens-mode . (lambda ()
                        (require 'smartparens-config)
                        (sp-use-smartparens-bindings)
                        (sp-use-paredit-bindings)
                        ;; Don't shadow global binding
                        (bind-key "M-<backspace>" nil smartparens-mode-map)
                        (turn-on-show-smartparens-mode)))
  ((css-mode emacs-lisp-mode hy-mode sass-mode sh-mode) . turn-on-smartparens-mode)
  (clojure-mode . (lambda () (require 'smartparens-clojure)))
  ((ruby-mode enh-ruby-mode) . (lambda () (require 'smartparens-ruby)))
  ((javascript-mode js2-mode json-mode rjsx-mode) . (lambda () (require 'smartparens-javascript)))
  (lua-mode . (lambda () (require 'smartparens-lua)))
  (markdown-mode . (lambda () (require 'smartparens-markdown)))
  (org-mode . (lambda () (require 'smartparens-org)))
  ((python-mode elpy-mode) . (lambda () (require 'smartparens-python)))
  (text-mode . (lambda () (require 'smartparens-text)))
  (web-mode . (lambda () (require 'smartparens-html)))
  :bind
  (:map smartparens-mode-map
        ("C-M-k" . sp-kill-sexp)
        ("C-s-<backspace>" . sp-backward-kill-sexp)
        ("C-c <backspace>" . sp-backward-kill-sexp)
        ("C-M-(" . sp-backward-slurp-into-previous-sexp)
        ("C-s-a" . sp-backward-down-sexp)
        ("C-s-e" . sp-up-sexp)))

(bind-keys ("C-M-a" . beginning-of-defun)
           ("C-M-e" . end-of-defun))

(use-package parinfer
  :custom
  (parinfer-extensions
   '(defaults       ; should be included.
      pretty-parens ; different paren styles for different modes.
      smart-tab     ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank))  ; Yank behavior depends on mode.
  :config
  (parinfer-strategy-add 'default 'newline-and-indent)
  :hook
  ((clojure-mode common-lisp-mode emacs-lisp-mode hy-mode lisp-interaction-mode
                 lisp-mode scheme-mode) . parinfer-mode)
  :bind
  (:map parinfer-mode-map
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)
        ("C-i" . parinfer--reindent-sexp)
        ("C-M-i" . parinfer-auto-fix)
        ("C-," . parinfer-toggle-mode)
        ;; Don't interfere with smartparens quote handling
        ("\"" . nil)
        ;; sp-newline seems to offer a better experience for lisps
        ("RET" . sp-newline)
        :map parinfer-region-mode-map
        ("C-i" . indent-for-tab-command)
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)
(require 'dired-x)

;; try to use GNU ls on macOS since BSD ls doesn't explicitly support Emacs
(setq insert-directory-program (or (executable-find "gls")
                                   (executable-find "ls"))
      ;; don't prompt to kill buffers of deleted directories
      dired-clean-confirm-killing-deleted-buffers nil)

(defun dired-to-default-directory ()
  "Open directory containing the current file."
  (interactive)
  (dired default-directory))

(defun dired-open-file ()
  "Open file at point in OS default program."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (os-open-file file)))

(defvar-local dired-dotfiles-show-p t
  "If non-nil, show files beginning with `.' in dired.")

(defun dired-dotfiles-toggle ()
  "Toggle display of dot files."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer)            ; otherwise just revert to re-show
             (setq-local dired-dotfiles-show-p t)))))

(use-package dired+)

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-listing-switches "-alh"
      dired-dwim-target t
      dired-omit-mode t
      dired-omit-files "\\`[#.].*")

(setq-default dired-omit-files-p t)

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode t)))

(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

(bind-keys
 ("C-x C-d" . dired-to-default-directory)
 ("C-x d" . dired)
 :map dired-mode-map
 ("C-c o" . dired-open-file)
 ("T" . touch)
 ("C-." . dired-omit-mode)
 ("C-c C-p" . wdired-change-to-wdired-mode))

(use-package diredfl
  :config
  (diredfl-global-mode))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("I" . dired-subtree-cycle)
        ("TAB" . dired-subtree-cycle)
        ("C-, i" . dired-subtree-insert)
        ("C-, r" . dired-subtree-remove)
        ("C-, R" . dired-subtree-revert)
        ("C-, n" . dired-subtree-narrow)
        ("C-, ^" . dired-subtree-up)
        ("C-, v" . dired-subtree-down)))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

(use-package dired-sidebar
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  :bind
  (("C-x M-d" . dired-sidebar-toggle-sidebar)))

(use-package disk-usage
  :straight
  (:type git :host gitlab :repo "Ambrevar/emacs-disk-usage")
  :bind
  (:map dired-mode-map
        (")" . disk-usage-here)
        ("C-)" . disk-usage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell, Terminal, SSH, Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tramp)

;; password-cache
(setq password-cache-expiry nil)

;; Configure TRAMP to respect the PATH variable on the remote machine (for
;; remote eshell sessions)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(defun list-hosts-from-known-hosts ()
  "Return a list of hosts from `~/.ssh/known_hosts'."
  (with-temp-buffer
    (insert-file-contents "~/.ssh/known_hosts")
    (-remove (lambda (host) (string=  "" host))
             (mapcar (lambda (line) (replace-regexp-in-string "\\]\\|\\[" "" (car (split-string line "[, :]"))))
                     (split-string (buffer-string) "\n")))))

(defun list-hosts-from-ssh-config ()
  "Return a list of hosts from `~/.ssh/config'."
  (with-temp-buffer
    (insert-file-contents "~/.ssh/config")
    (keep-lines "^Host")
    (-remove (lambda (host) (or (string=  "" host) (string= "*" host)))
             (mapcar (lambda (line) (replace-regexp-in-string "Host +" "" line))
                     (split-string (buffer-string) "\n")))))

(defun list-hosts-from-etc-hosts ()
  "Return a list of hosts from `/etc/hosts'."
  (with-temp-buffer
    (insert-file-contents "/etc/hosts")
    (flush-lines "^#")
    (flush-lines "^$")
    (-remove (lambda (host) (or (string= host "localhost")
                                (string= host "broadcasthost")
                                (eq host nil)))
             (mapcar (lambda (line) (cadr (split-string line "[ \t]+")))
                     (split-string (buffer-string) "\n")))))

(defun list-hosts-from-recentf ()
  "Return a list of hosts from the `recentf-list'."
  (-distinct
   (mapcar (lambda (s)
             (replace-regexp-in-string
              ":.*" ""
              (replace-regexp-in-string "^/sshx\?:" "" s)))
           (-filter
            (apply-partially #'string-match "^/sshx\?:\\([a-z]+\\):")
            recentf-list))))

(defun ssh-choose-host (&optional prompt)
  "Make a list of recent ssh hosts and interactively choose one with optional PROMPT."
  (completing-read (or prompt "SSH to Host: ")
                   (-distinct
                    (append
                     (list-hosts-from-recentf)
                     (list-hosts-from-known-hosts)
                     (list-hosts-from-ssh-config)
                     (list-hosts-from-etc-hosts)))
                   nil t))

(defun tramp-dired (host)
  "Choose an ssh HOST and then open it with dired."
  (interactive (list (ssh-choose-host "Hostname or tramp string: ")))
  (find-file
   (if (tramp-file-name-p host)
       host
     (find-file (concat "/ssh:" host ":")))))

(defun tramp-dired-sudo (host)
  "SSH to HOST, sudo to root, open dired."
  (interactive (list (ssh-choose-host "Hostname or tramp string: ")))
  (find-file
   (if (tramp-file-name-p host)
       host
     (concat "/ssh:" host "|sudo:root@" host ":"))))

(eval-after-load 'sh
  (lambda ()
    (bind-keys
     :map sh-mode-map
     ("s-<ret>" . eshell-send-current-line))))

;; http://whattheemacsd.com/setup-shell.el-01.html
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "`C-d' on an empty line in the shell terminates the process, accepts ARG."
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (bind-keys :map shell-mode-map ("C-d" . comint-delchar-or-eof-or-kill-buffer))
            (bind-keys :map shell-mode-map ("SPC" . comint-magic-space))))

(defun shell-command-exit-code (program &rest args)
  "Run PROGRAM with ARGS and return the exit code."
  (with-temp-buffer
    (apply 'call-process program nil (current-buffer) nil args)))

;; dtach (https://github.com/crigler/dtach)
;; https://emacs.stackexchange.com/questions/2283/attach-to-running-remote-shell-with-eshell-tramp-dtach
(defvar explicit-dtach-args
  '("-A" "/tmp/emacs.dtach" "-z" "bash" "--noediting" "--login")
  "Args for dtach.")

(defun ssh-dtach (host)
  "Open SSH connection to HOST and create or attach to dtach session."
  (interactive (list (ssh-choose-host "SSH using dtach to host: ")))
  (let ((explicit-shell-file-name "dtach")
        (default-directory (format  "/sshx:%s:" host))
        (explicit-dtach-args explicit-dtach-args))
    (shell (format "*ssh (dtach) %s*" host))))

;; https://www.emacswiki.org/emacs/ShellMode
(defun term-switch-to-shell-mode ()
  "Switch a term session to shell."
  (interactive)
  (if (equal major-mode 'term-mode)
      (progn
        (shell-mode)
        (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter)
        (local-set-key (kbd "C-M-j") 'term-switch-to-shell-mode)
        (compilation-shell-minor-mode 1)
        (comint-send-input))
    (progn
      (compilation-shell-minor-mode -1)
      (font-lock-mode -1)
      (set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
      (term-mode)
      (term-char-mode)
      (term-send-raw-string (kbd "C-l")))))

;; Apply colors to `shell-command' minibuffer output.
;; Adapted from https://stackoverflow.com/a/42666026/1588358
(defun xterm-color-apply-on-minibuffer ()
  "Apply xterm color filtering on minibuffer output."
  (let ((bufs (cl-remove-if-not
               (lambda (x) (string-prefix-p " *Echo Area" (buffer-name x)))
               (buffer-list))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (xterm-color-colorize-buffer)))))

(defun xterm-color-apply-on-minibuffer-advice (_proc &rest _rest)
  "Wrap `xterm-color-apply-on-minibuffer'."
  (xterm-color-apply-on-minibuffer))

(advice-add 'shell-command :after #'xterm-color-apply-on-minibuffer-advice)

(let ((vterm-dir "~/code/emacs-libvterm"))
  (when (file-exists-p vterm-dir)
    (add-to-list 'load-path "~/code/emacs-libvterm")
    (let (vterm-install)
      (require 'vterm))))

(use-package term
  :bind (("C-c t" . vterm)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         ("C-M-j" . term-switch-to-shell-mode)))

;; xterm colors
(use-package xterm-color
  :custom
  (comint-output-filter-functions
   (remove 'ansi-color-process-output comint-output-filter-functions))
  :config
  (setenv "TERM" "xterm-256color")
  :hook
  (shell-mode . (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (compilation-start-hook . (lambda (proc)
                              ;; We need to differentiate between compilation-mode buffers
                              ;; and running as part of comint (which at this point we assume
                              ;; has been configured separately for xterm-color)
                              (when (eq (process-filter proc) 'compilation-filter)
                                ;; This is a process associated with a compilation-mode buffer.
                                ;; We may call `xterm-color-filter' before its own filter function.
                                (set-process-filter
                                 proc
                                 (lambda (proc string)
                                   (funcall 'compilation-filter proc
                                            (xterm-color-filter string))))))))

;; (use-package eterm-256color
;;   ;; `devel' branch is needed to support Emacs 27.
;;   ;; https://github.com/dieggsy/eterm-256color/pull/9#issuecomment-403229541
;;   :straight
;;   (:type git :host github :repo "dieggsy/eterm-256color" :branch "devel")
;;   :hook
;;   (term-mode . eterm-256color-mode))

(use-package bash-completion
  :custom
  ;; So that it doesn't sometimes insert a space ('\ ') after the file name.
  (bash-completion-nospace t)
  :hook
  (shell-dynamic-complete-functions . bash-completion-dynamic-complete))

(use-package fish-mode
  :custom (fish-indent-offset tab-width)
  :mode "\\.fish\\'")

(use-package fish-completion
  :custom
  (fish-completion-fallback-on-bash-p t)
  :config
  (global-fish-completion-mode))

(defun eshell-other-window (arg)
  "Opens an eshell in another window, creating a new one if ARG is specified."
  (interactive "p")
  (if (= arg 4)
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             ;; `eshell' uses this variable as the new buffer name
             (eshell-buffer-name (concat "*eshell: " (car (last (split-string parent "/" t))) "*")))
        (switch-to-buffer-other-window "*eshell-here-temp*")
        (eshell)
        (kill-buffer "*eshell-here-temp*")
        (insert (concat "ls"))
        (eshell-queue-input))
    (progn
      (switch-to-buffer-other-window "*eshell*")
      (eshell)
      (message (number-to-string arg)))))

(defun eshell-maybe-bol ()
  "Smarter `beginning-of-line' for Eshell."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(defun eshell-quit-or-delete-char (arg)
  "Quit Eshell if `C-d' is specified, passing ARG on."
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (progn
        (eshell-life-is-too-much)
        (ignore-errors
          (delete-window)))
    (delete-char  arg)))

(defun eshell-send-previous-input (&optional arg)
  "Re-run the previous command with ARG in the last used eshell buffer."
  (interactive "*p")
  (with-current-buffer
      (-first (lambda (b) (eq 'eshell-mode (with-current-buffer b major-mode)))
              (buffer-list))
    (with-selected-window (get-buffer-window)
      (goto-char (point-max))
      (recenter 4))
    (eshell-previous-input arg)
    (eshell-send-input)))

(defun eshell-send-current-line ()
  "Insert text of current line in eshell and execute."
  (interactive)
  (require 'eshell)
  (let ((command (buffer-substring
                  (save-excursion
                    (beginning-of-line)
                    (point))
                  (save-excursion
                    (end-of-line)
                    (point)))))
    (let ((buf (current-buffer)))
      (unless (get-buffer eshell-buffer-name)
        (eshell))
      (display-buffer eshell-buffer-name t)
      (switch-to-buffer-other-window eshell-buffer-name)
      (goto-char (point-max))
      (eshell-kill-input)
      (insert command)
      (eshell-send-input)
      (goto-char (point-max))
      (switch-to-buffer-other-window buf))))

(defun m-eshell-prompt-function ()
  "Produce a highlighted prompt for Eshell."
  (mapconcat
   (lambda (list)
     (when list
       (propertize (concat " " (car list) " ")
                   'read-only t
                   'font-lock-face (cdr list)
                   'front-sticky '(font-lock-face read-only)
                   'rear-nonsticky '(font-lock-face read-only))))
   `(,(unless (eshell-exit-success-p)
        `(,(number-to-string eshell-last-command-status)
          :background "red" :foreground "white" :weight bold))
     (,(abbreviate-file-name (eshell/pwd)) :background "cyan" :foreground "black")
     (,(if (zerop (user-uid)) "\n(#)" "\n()")
      :foreground ,(if (equal 'light (frame-parameter nil 'background-mode))
                       "black"
                     "white")
      :weight bold))
   ""))

(defun eshell/s (host)
  "Change directory to HOST via tramp."
  (eshell/cd (concat "/ssh:" host ":")))

(defun eshell/e (&optional path)
  "Eshell alias for `find-file', passing optional PATH."
  (find-file path))

(defun eshell/ee (&optional path)
  "Eshell alias for `find-file-other-window', passing optional PATH."
  (find-file-other-window path))

(defun eshell/d (&optional path)
  "Eshell alias for `dired', passing optional PATH."
  (dired path))

(defun eshell/do (&optional path)
  "Eshell alias for `dired-other-window', passing optional PATH."
  (dired-other-window path))

(defun eshell-path-advice (f &rest paths)
  "For each element in F PATHS, return path relative to the host.
If the element starts with `:' and we are on a remote host).

Examples: > cd /etc -> /etc > cd :/etc -> /sshx:host:/etc > cd :
-> /sshx:host:/home/user"
  (apply f
         (cl-loop for path in (-flatten paths) collect
                  (if-let* ((remote (and (string-prefix-p ":" path)
                                         (file-remote-p path))))
                      (concat remote (substring path 1))
                    path))))

;; Advise functions to work with ":" path syntax (see `eshell-path-advice').
(seq-do (lambda (f) (advice-add f :around #'eshell-path-advice))
        '(eshell/cd eshell/cp eshell/mv eshell/rm eshell/e eshell/ee eshell/d
                    eshell/do))

(defun tramp-insert-remote-part ()
  "Insert current tramp prefix at point."
  (interactive)
  (if-let* ((remote (file-remote-p default-directory)))
      (insert remote)))

(bind-key "C-:" #'tramp-insert-remote-part)

(defun eshell/really-clear ()
  "Call `eshell/clear' with an argument to really clear the buffer.
Call it a second time to print the prompt."
  (interactive)
  (eshell/clear 1)
  (eshell/clear))

(defun eshell/info (&rest args)
  "Invoke `info', optionally opening the Info system to car ARGS."
  (Info-directory)
  (let ((subject (car args)))
    (if (not (null subject))
        (let ((node-exists (ignore-errors (Info-menu subject))))
          (if (not node-exists)
              (format "No menu item `%s' in node `(dir)Top'." subject))))))

(defun eshell-create-send (cmd &optional name)
  "Create an eshell buffer named NAME and run CMD in it."
  (let ((eshell-buffer-name (or name cmd)))
    (eshell))
  (insert cmd)
  (eshell-queue-input))

(defun eshell-vertical-create-send (cmd &optional name)
  "Create an eshell buffer named NAME and run CMD in it.
Split the window vertically."
  (split-window-vertically)
  (eshell-create-send cmd name))

(defun eshell-kill-previous-output ()
  "Kill the output of the previous command."
  (interactive)
  (let ((inhibit-read-only t)
        (lines (count-lines (eshell-beginning-of-output)
                            (eshell-end-of-output))))
    ;; Kill region
    (kill-region (eshell-beginning-of-output) (eshell-end-of-output))
    (save-excursion
      ;; Write something in place of the text so we know what happened.
      (goto-char (eshell-beginning-of-output))
      (insert (format "--- Killed %d lines ---\n" lines)))))

(defun eshell-kill-previous-output-to-buffer ()
  "Move output of the previous command to a new buffer."
  (interactive)
  (eshell-kill-previous-output)
  (switch-to-buffer-other-window "*eshell-stdout*")
  (yank))

(defun eshell-copy-previous-output ()
  "Copies the output of the previous command to the kill ring."
  (interactive)
  (let ((lines (count-lines (eshell-beginning-of-output)
                            (eshell-end-of-output))))
    ;; Copy region to kill ring
    (copy-region-as-kill (eshell-beginning-of-output) (eshell-end-of-output))
    (message "Copied %d lines" lines)))

(defun local-set-minor-mode-key (mode key def)
  "Override MODE KEY binding with DEF.
Override a minor mode keybinding for the local buffer. Create or
alter keymaps, storing them in buffer-local variable
`minor-mode-overriding-map-alist' so that we get the bindings we
want and they are now shadowed.'. See
https://stackoverflow.com/a/14769115/1588358"
  (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(,mode . ,map) minor-mode-overriding-map-alist)
                       map))))
    (define-key newmap key def)))

(defun eshell/import-aliases ()
  "Retrieve bash aliases and format them for import into Eshell."
  (shell-command ". ~/.env && . ~/.aliases && alias | sed -E \"s/^alias ([^=]+)='(.*)'$/alias \\1 \\2 \\$*/g; s/'\\\\''/'/g;\""
                 "*bash aliases*"))

(defun eshell/init ()
  "Initialize the Eshell environment."
  (source-sh "~/.env")
  (setq eshell-path-env (getenv "PATH"))
  ;; Path to shell executable. Set it this way to work with tramp.
  (setenv "ESHELL" "/bin/bash")
  ;; (setenv "TERM" "eterm-color")
  (setenv "EDITOR" "emacsclient")
  (setenv "PAGER" "cat")
  (setenv "MANPAGER" "cat")

  (bind-keys
   ("M-P" . eshell-send-previous-input)
   :map eshell-mode-map
   ("C-a" . eshell-maybe-bol)
   ("C-d" . eshell-quit-or-delete-char)
   ("<tab>" . completion-at-point)
   ("M-r" . counsel-esh-history)
   ("C-L" . eshell/really-clear)
   ("C-w" . eshell-kill-previous-output)
   ("C-M-w" . eshell-kill-previous-output-to-buffer)
   ("M-w" . eshell-copy-previous-output)
   ("s-v" . clipboard-yank)
   ("C-S-<backspace>" . eshell-kill-input)
   ("C-M-S-p" . eshell-previous-prompt)
   ("M-<up>" . eshell-previous-prompt)
   ("C-M-S-n" . eshell-next-prompt)
   ("M-<down>" . eshell-next-prompt)
   ("C-h C-e" . esh-help-run-help))

  ;; xterm colors
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

  ;; Commands that use curses get launched in their own `term' buffer
  (seq-do (-partial #'add-to-list 'eshell-visual-commands)
          '("dasht" "htop" "mbsync" "ncdu" "nnn" "nvim" "ssh" "tail" "tmux"
            "top" "vim" "w3m"))
  (seq-do (-partial #'add-to-list 'eshell-visual-subcommands)
          '(("git" "log" "diff" "show")
            ("dw" "log" "runshell" "shell")))

  ;; Load the Eshell versions of `su' and `sudo'
  (require 'em-tramp)
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(defun ibuffer-show-eshell-buffers ()
  "Open an ibuffer window and display all Eshell buffers."
  (interactive)
  (ibuffer nil "Eshell Buffers" '((mode . eshell-mode)) nil t nil
           '(((name 64 64 :left)
              " "
              (process 0 -1 :right)))))

(defun eshell-create-in-background ()
  "Create a new Eshell buffer but don't display it."
  (let ((eshell-buffer-name (generate-new-buffer "*Eshell*")))
    (save-window-excursion (eshell))))

(defun eshell-get-or-create ()
  "Get or create an Eshell buffer."
  (interactive)
  (or (when current-prefix-arg (eshell-create-in-background))
      (some-buffer "*Eshell")
      (eshell-create-in-background)))

(defun eshell-switch-to-buffer ()
  "Switch to the most recent Eshell buffer or create a new one.
This is different than the normal `eshell' command in my setup
because I dynamically rename the buffer according to
`default-directory'."
  (interactive)
  (switch-to-buffer (eshell-get-or-create)))

(defun eshell-switch-to-buffer-other-window ()
  "Get or create an Eshell buffer, then switch to it."
  (interactive)
  (switch-to-buffer-other-window (eshell-get-or-create)))

(defun switch-to-eshell-buffer ()
  "Interactively choose an Eshell buffer."
  (interactive)
  (switch-to-buffer-by-mode 'eshell-mode))

(use-package eshell
  :custom
  (eshell-banner-message "")
  (eshell-buffer-shorthand t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-error-if-no-glob t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-prompt-function 'm-eshell-prompt-function)
  (eshell-prompt-regexp "^(#?) ")
  (eshell-highlight-prompt nil)
  :hook
  ((eshell-mode . eshell/init)
   ;; For using xterm-256color properties in the prompt
   (eshell-before-prompt . (lambda ()
                             (setq xterm-color-preserve-properties t)
                             (rename-buffer
                              (format "*Eshell: %s*" default-directory) t))))
  :bind
  (("s-e" . eshell-switch-to-buffer)
   ("C-c e" . eshell-switch-to-buffer)
   ("s-E" . eshell-switch-to-buffer-other-window)
   ("C-c E" . eshell-switch-to-buffer-other-window)
   ("C-s-e" . switch-to-eshell-buffer)
   ("M-E" . ibuffer-show-eshell-buffers)
   ("C-c M-e" . ibuffer-show-eshell-buffers)
   :map prog-mode-map
   ("M-P" . eshell-send-previous-input)))

;; ElDoc and topical help in Eshell.
(use-package esh-help
  :config
  (setup-esh-help-eldoc))

;; Fish-like autosuggestions.
(use-package esh-autosuggest
  :hook
  (eshell-mode . esh-autosuggest-mode)
  :bind
  (:map esh-autosuggest-active-map
        ("C-e" . company-complete-selection)))

(use-package pinentry
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  ;; TODO: Don't know how to get pinentry to work with Windows. Maybe a TCP socket?
  (unless (eq system-type 'windows-nt)
    (pinentry-start)))

(defun sudo-toggle--add-sudo (path)
  "Add sudo to file PATH string."
  (if (file-remote-p path)
      (with-parsed-tramp-file-name (expand-file-name path) nil
        (concat "/" method ":"
                (when user (concat user "@"))
                host "|sudo:root@" host ":" localname))
    (concat "/sudo:root@localhost:" (expand-file-name path))))

(defun sudo-toggle--remove-sudo (path)
  "Remove sudo from file PATH string."
  (cond
   ((string-match-p "/sudo:root@localhost:" path)
    (replace-regexp-in-string (getenv "HOME") "~" (substring path 21)))

   ((string-match-p "|sudo:root@" path)
    (replace-regexp-in-string "|sudo:root@[^:]*" "" path))))

(defun sudo-toggle ()
  "Reopen the current file, directory, or shell as root.  For))))
files and dired buffers, the non-sudo buffer is replaced with a
sudo buffer.  For shells, a sudo shell is opened but the non-sudo
shell is left intact."
  (interactive)
  (let* ((position (point))
         (f (expand-file-name (or buffer-file-name default-directory)))
         (newf (if (string-match-p "sudo:" f)
                   (sudo-toggle--remove-sudo f)
                 (sudo-toggle--add-sudo f)))
         ;; so that you don't get method overrides
         (tramp-default-proxies-alist nil))
    (cond ((or buffer-file-name (derived-mode-p 'dired-mode))
           (find-alternate-file newf)
           (goto-char position))
          ((derived-mode-p 'shell-mode)
           (if (string-match-p "*shell/sudo:root@" (buffer-name))
               (kill-buffer-and-window)
             (with-temp-buffer
               (cd newf)
               (shell (format "*shell/sudo:root@%s*"
                              (with-parsed-tramp-file-name newf nil host))))))
          ((derived-mode-p 'eshell-mode)
           (eshell-return-to-prompt)
           (insert (concat "cd '" newf "'"))
           (eshell-send-input))
          (t (message "Can't sudo this buffer")))))

(defun tramp-disable-file-accesses ()
  "Disable file accesses when visiting buffers accessed via tramp for performance reasons."
  (when (file-remote-p default-directory)
    (setq-local company-backends company-backends-remote)))

(use-package tramp
  :hook
  (find-file . tramp-disable-file-accesses))

;; (defun tramp-term--create-vterm (name cmd &rest args)
;;   "Create a `vterm' with NAME, running CMD with ARGS."
;;   (vterm)
;;   (rename-buffer name)
;;   (insert (mapconcat 'identity (cons cmd args) " "))
;;   (insert "\n"))

(use-package tramp-term
  :commands
  (tramp-term tramp-term--create-term))

;; (advice-add 'tramp-term--create-term :around #'tramp-term--create-vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mount
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These functions execute the `mnt' utility, which uses config
;; profiles to mount smb shares (even through ssh tunnels).

(defun mnt-cmd (cmd)
  "Interactively Run a `mnt/umnt' utility (CMD).
The config is specified in the config file in `~/.mnt/'."
  (let ((config (completing-read (format "Run %s using config: " cmd)
                                 (directory-files "~/.mnt" nil "^[^.]")
                                 nil t)))
    (setq config (expand-file-name config "~/.mnt"))
    (if (async-shell-command (concat cmd " " config) "*mnt*")
        (message (format "%s succeeded with config file: %s" cmd config))
      (message (format "%s FAILED with config file: %s" cmd config)))))

(defun mnt ()
  "Mount a share using the `mnt' utility."
  (interactive)
  (mnt-cmd "sudo_mnt"))

(defun umnt ()
  "Unmount a share using the `umnt' utility."
  (interactive)
  (mnt-cmd "umnt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automate communication with services, such as nicserv
(require 'erc-services)
(erc-services-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes, Journal, and Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; visual-line-mode
;; Don't shadow mwim and org-mode bindings
(bind-key [remap move-beginning-of-line] nil visual-line-mode-map)
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

;; Org-mode

;; Install Org from git
;; https://github.com/raxod502/straight.el#installing-org-with-straightel
;; (require 'subr-x)
;; (straight-use-package 'git)

;; (defun org-git-version ()
;;   "The Git version of org-mode.
;; Inserted by installing org-mode or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;;                    "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (git-run "describe"
;;               "--match=release\*"
;;               "--abbrev=6"
;;               "HEAD"))))

;; (defun org-release ()
;;   "The release version of org-mode.
;; Inserted by installing org-mode or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;;                    "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (string-remove-prefix
;;       "release_"
;;       (git-run "describe"
;;                "--match=release\*"
;;                "--abbrev=0"
;;                "HEAD")))))

;; (provide 'org-version)

(defun search-org-files ()
  "Search ~/org using `counsel-rg'."
  (interactive)
  (counsel-rg nil "~/org"))

(defun org-todo-todo ()
  "Create or update Org todo entry to TODO status."
  (interactive)
  (org-todo "TODO"))

(defun org-todo-to-int (todo)
  "Get the number of the TODO based on its status."
  (first (-non-nil
          (mapcar (lambda (keywords)
                    (let ((todo-seq
                           (-map (lambda (x) (first (split-string  x "(")))
                                 (rest keywords))))
                      (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                  org-todo-keywords))))

(defun org-sort-entries--todo-status-key ()
  "Sort Org TODO entries by their status."
  (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
         (todo (org-entry-get (point) "TODO"))
         (todo-int (if todo (org-todo-to-int todo) todo-max))
         (priority (org-entry-get (point) "PRIORITY"))
         (priority-int (if priority (string-to-char priority) org-default-priority)))
    (format "%03d %03d" todo-int priority-int)))

(defun org-sort-entries-by-todo-status ()
  "Sort Org TODO entries by their status."
  (interactive)
  (org-sort-entries nil ?f #'org-sort-entries--todo-status-key))

(require 'org)

(setq
 org-directory "~/org"
 ;; Clean view
 org-startup-indented t
 ;; Smart C-a/e
 org-special-ctrl-a/e t
 ;; Smart C-k
 org-special-ctrl-k t
 ;; Insert a row in tables
 org-special-ctrl-o t
 ;; Tab in source blocks should act like in major mode
 org-src-tab-acts-natively t
 ;; Code highlighting in code blocks
 org-src-fontify-natively t
 ;; Customize todo keywords
 ;; (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "|" "DONE(d!)")))
 ;; (org-todo-keyword-faces '(("TODO" (:foreground "orange" :weight bold))
 ;;                           ("NEXT" (:foreground "red" :weight bold))
 ;;                           ("WIP" (:foreground "green" :weight bold))
 ;;                           ("DONE" (:foreground "gray"))))
 org-agenda-files '("~/org" "~/TODO.org"))

(bind-keys
 ("C-c l" . org-store-link)
 ("C-c a" . org-agenda)
 ("C-c c" . org-capture)
 ("C-c b" . org-switchb)
 ("C-c s" . search-org-files)
 :map org-mode-map
 ("s-;" . org-shiftright))

;; (use-package org-preview-html
;;   :commands
;;   (org-preview-html-mode))

;; Calendar and Journal

(require 'calendar)

(defun calendar-iso8601-date-string (date)
  "Create an ISO8601 date string from DATE."
  (destructuring-bind (month day year) date
    (concat (format "%4i" year)
            "-"
            (format "%02i" month)
            "-"
            (format "%02i" day))))

(defun calendar-date-add-days (date days)
  "Add DAYS to DATE."
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian date)
      days)))

(defun calendar-choose-date ()
  "Interactively choose DATE and return it as an ISO 8601 string."
  (let* ((today (calendar-current-date))
         (day-offsets '(0 -1 -2 -3 -4 -5 -6 -7))
         (dates (mapcar (apply-partially #'calendar-date-add-days today) day-offsets))
         (date-strings (mapcar #'calendar-iso8601-date-string dates)))
    (completing-read "Date: " date-strings nil nil (substring (car date-strings) 0 7))))

(defun calendar-insert-date (date)
  "Interactively choose a DATE in ISO 8601 format and insert it at point."
  (interactive (list (calendar-choose-date)))
  (insert date))

(defun calendar-insert-date-today ()
  "Insert today's date in ISO 8601 format."
  (interactive)
  (insert (calendar-iso8601-date-string (calendar-current-date))))

(defun journal-new-entry ()
  "Create a new journal entry."
  (interactive)
  (let ((date (calendar-choose-date)))
    (find-file (expand-file-name (concat date ".md") journal-directory))
    (if (= 0 (buffer-size))
        (progn
          (insert "journal")
          (yas-expand)))))

;; (use-package nov
;;   :mode ("\\.epub\\'" . nov-mode))

(defvar mode-line-format-backup nil
  "Backup of `mode-line-format'.")

(defun hide-mode-line ()
  "Hide the mode line."
  (interactive)
  (setq mode-line-format-backup mode-line-format)
  (setq-default mode-line-format nil))

(defun show-mode-line ()
  "Show the mode line."
  (interactive)
  (setq-default mode-line-format mode-line-format-backup))

;; (use-package darkroom-mode
;;   :commands
;;   (darkroom-mode darkroom-tentative-mode))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  :bind
  (:map pdf-view-mode-map
        ("s-f" . isearch-forward)))

;; Doesn't seem to work. Probably API changed?
;; (use-package org-wunderlist
;;   :commands
;;   (org-wunderlist-fetch))
;; :bind
;; ("C-c o w" . org-wunderlist-fetch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vlf
  :custom
  (vlf-application 'dont-ask)
  :config
  (require 'vlf-setup))

;; (use-package logview)

(defun tail-file (file)
  "Run `tail -f' on FILE.
Tries to find a file at point."
  (interactive (list (completing-read "Tail file: "
                                      'read-file-name-internal
                                      'file-exists-p t nil 'file-name-history
                                      (thing-at-point 'filename))))
  (async-shell-command (concat "tail -f " file)))

(bind-key "F" #'tail-file dired-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hydra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra)

(defhydra hydra-multiple-cursors (:hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil)
  ("<mouse-1>" mc/add-cursor-on-click)
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore))

(defhydra hydra-outline (:color pink :hint nil)
  "
Outline

^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_ sublevels     _a_ all         _u_ up
_t_ body          _e_ entry       _n_ next visible
_o_ other         _i_ children    _p_ previous visible
_c_ entry         _k_ branches    _f_ forward same level
_l_ leaves        _s_ subtree     _b_ backward same level
_d_ subtree

"
  ;; Hide
  ("q" outline-hide-sublevels)    ; Hide everything but the top-level headings
  ("t" outline-hide-body)         ; Hide everything but headings (all body lines)
  ("o" outline-hide-other)        ; Hide other branches
  ("c" outline-hide-entry)        ; Hide this entry's body
  ("l" outline-hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" outline-hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" outline-show-all)          ; Show (expand) everything
  ("e" outline-show-entry)        ; Show this heading's body
  ("i" outline-show-children)     ; Show this heading's immediate child sub-headings
  ("k" outline-show-branches)     ; Show all sub-headings under this heading
  ("s" outline-show-subtree)      ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("z" nil "leave"))

(defhydra hydra-hs (:color pink :hint nil)
  "
Hideshow

Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_ toggle    _n_ next line
_d_ hide block    _a_ show block                _p_ previous line
_l_ hide level

_q_ quit
"
  ("s" hs-show-all)
  ("h" hs-hide-all)
  ("a" hs-show-block)
  ("d" hs-hide-block)
  ("t" hs-toggle-hiding)
  ("l" hs-hide-level)
  ("n" forward-line)
  ("p" (forward-line -1))
  ("q" nil))

(defun occur-dwim ()
  "Call `occur' with a sane default, chosen as the thing under point or selected region."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

;; (advice-add 'occur-mode-goto-occurrence :after #'other-window-hydra-occur)

;; Focus on *Occur* window right away.
(add-hook 'occur-hook (lambda () (other-window 1)))

(defun reattach-occur ()
  "Switch to Occur buffer and launch the hydra."
  (if (get-buffer "*Occur*")
      (switch-to-buffer-other-window "*Occur*")
    (hydra-occur-dwim/body)))

;; Used in conjunction with occur-mode-goto-occurrence-advice this helps keep
;; focus on the *Occur* window and hides upon request in case needed later.
(defhydra hydra-occur-dwim ()
  "Occur mode"
  ("o" occur-dwim "Start occur-dwim" :color red)
  ("j" occur-next "Next" :color red)
  ("k" occur-prev "Prev":color red)
  ("h" delete-window "Hide" :color blue)
  ("r" (reattach-occur) "Re-attach" :color red))

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_ view         _m_ mark             _(_ details        _i_ insert-subdir  _W_  wdired
_C_ copy           _O_ view other   _U_ unmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_ delete         _o_ open other   _u_ unmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ rename         _M_ chmod        _t_ toggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_ extension mark   _s_ sort           _=_ pdiff
_S_ symlink        ^ ^              _F_ find marked      _._ toggle hydra   \\ flyspell
_r_ rsync          ^ ^              ^ ^                  ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

_q_ quit
"
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or single directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-rsync)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("W" wdired-change-to-wdired-mode)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
_m_ mark      _D_ delete       _g_ refresh    _q_ quit             _k_   ↑    _h_
_u_ unmark    _s_ save marked  _S_ sort       _TAB_ toggle         _RET_ visit
_*_ specific  _a_ all actions  _/_ filter     _o_ other window     _j_   ↓    _l_
_t_ toggle    _._ toggle hydra _H_ help       C-o other win no-select
"
  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)
  ("t" ibuffer-toggle-marks)

  ("D" ibuffer-do-delete)
  ("s" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("S" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)
  ("H" describe-mode :color blue)

  ("h" ibuffer-backward-filter-group)
  ("k" ibuffer-backward-line)
  ("l" ibuffer-forward-filter-group)
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)

  ("TAB" ibuffer-toggle-filter-group)

  ("o" ibuffer-visit-buffer-other-window :color blue)
  ("q" quit-window :color blue)
  ("." nil :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                                     :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                       :after-exit
                                       (if (eq major-mode 'ibuffer-mode)
                                           (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(require 'ibuffer)

(bind-keys ("C-c C-m" . hydra-multiple-cursors/body)
           ("C-c #" . hydra-outline/body)
           ("C-c @" . hydra-hs/body)
           ("C-c C-o" . hydra-occur-dwim/body)
           :map dired-mode-map
           ("." . hydra-dired/body)
           :map ibuffer-mode-map
           ("." . hydra-ibuffer-main/body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search, Completion, Symbols, Project Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package re-builder
  :custom
  ;; string syntax means you don't need to double escape things.
  (reb-re-syntax 'string))

(use-package wgrep
  :custom
  ;; Save changed buffers immediately when exiting wgrep mode
  (wgrep-auto-save-buffer t)
  :bind
  (("C-c C-p" . wgrep-change-to-wgrep-mode)))

(use-package rg
  ;; :ensure-system-package
  ;; (rg . ripgrep)
  :requires
  (wgrep-ag)
  :config
  (rg-enable-default-bindings (kbd "C-r"))
  :hook
  (rg-mode . wgrep-ag-setup))

;; (use-package counsel-etags
;;   :custom
;;   ;; TODO: Get this working with Clojure (ctags parses namespaces but
;;   ;; `counsel-etags-find-tag-at-point' doesn't. Wouldn't this be `clojure-mode's
;;   ;; responsibility? I'm pretty sure it keys off of sexp
;;   (tags-revert-without-query t)
;;   ;; Don't warn when TAGS files are large.
;;   (large-file-warning-threshold nil)
;;   :hook
;;   ;; Incrementally update TAGS file when the file is saved.
;;   (prog-mode . (lambda ()
;;                  (add-hook 'after-save-hook
;;                            'counsel-etags-virtual-update-tags 'append 'local)))
;;   :commands
;;   (counsel-etags-find-tag-at-point counsel-etags-scan-code counsel-etags-list-tag))

(use-package company
  :custom
  (company-backends
   '(company-capf company-gtags company-css company-elisp company-keywords
                  company-semantic company-yasnippet company-files
                  company-dabbrev-code company-dabbrev company-ispell))
  (company-backends-remote
   '((company-shell company-shell-env)
     company-capf company-css company-elisp company-keywords
     company-yasnippet company-dabbrev-code company-dabbrev company-ispell))
  (company-idle-delay 0.1)
  (company-dabbrev-ignore-case t)
  (company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend
     company-echo-metadata-frontend
     company-preview-frontend))
  :hook
  (after-init . global-company-mode)
  :bind
  (:map prog-mode-map
        ("<tab>" . company-indent-or-complete-common)
        :map company-active-map
        ;; TODO: The inconsistency between C-n and M-n to select company
        ;; completion in different contexts (e.g `emacs-lisp-mode' and
        ;; `eshell-mode') is aggravating. Not sure about the solution though.
        ;; ("C-n" . company-select-next) ("C-p" . company-select-previous)
        ("RET" . nil)
        ("C-e" . company-complete-selection)
        ("M-." . company-show-location)))

(use-package company-quickhelp
  :bind
  (:map company-active-map
        ("C-c h" . company-quickhelp-manual-begin))
  :hook
  (global-company-mode . company-quickhelp-mode))

(use-package company-shell
  :config
  (add-to-list
   'company-backends
   `(company-shell company-shell-env
                   ,(when (executable-find "fish") 'company-fish-shell))))

(use-package pcre2el
  :hook
  ((emacs-lisp-mode lisp-interaction-mode reb-mode) . rxt-mode))

(use-package ivy
  :custom
  (enable-recursive-minibuffers t)
  (ivy-display-style 'fancy)
  :config
  (ivy-mode 1)
  :bind
  (:map ivy-mode-map
        ("C-c C-r" . ivy-resume)
        ("s-b" . ivy-switch-buffer)
        ("s-B" . ivy-switch-buffer-other-window)
        :map ivy-minibuffer-map
        ("C-e" . ivy-partial-or-done)))

(use-package ivy-hydra
  :defer 1)

(defun replace-regexp-entire-buffer (pattern replacement)
  "Immediately replace PATTERN with REPLACEMENT throughout the buffer."
  (interactive
   (let ((args (query-replace-read-args "Replace in entire buffer" t)))
     (setcdr (cdr args) nil)    ; remove third value returned from query---args
     args))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

(defun ivy--replace-regexp-entire-buffer (replacement)
  "Replace the currently input via regexp with REPLACEMENT."
  (interactive (list (read-from-minibuffer (concat "Replace all occurrences of `" ivy--old-re "\' in entire buffer: "))))
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (replace-regexp-entire-buffer ivy--old-text replacement))
  (ivy-done)
  ;; * TODO: How to make this the last message displayed?
  (message (concat "Replaced `" ivy--old-text "\' with `" replacement"\' across entire buffer.")))

(use-package swiper
  :bind
  (("s-5" . replace-regexp-entire-buffer)
   :map ivy-minibuffer-map
   ("s-5" . ivy--replace-regexp-entire-buffer)))

(defun reloading (cmd)
  "Wrap CMD, reloading ivy."
  (lambda (x)
    (funcall cmd x)
    (ivy--reset-state ivy-last)))

(defun given-file (cmd prompt)
  "Wrap in a function and call CMD, with interactive PROMPT."
  (lambda (source)
    (let ((target
           (let ((enable-recursive-minibuffers t))
             (read-file-name
              (format "%s %s to:" prompt source)))))
      (funcall cmd source target 1))))

(defun confirm-delete-file (file)
  "Delete FILE with confirmation."
  (dired-delete-file file 'confirm-each-subdirectory))

(use-package counsel
  :custom
  (counsel-find-file-at-point t)
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :config
  (ivy-add-actions
   'counsel-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")))
  (ivy-add-actions
   'counsel-projectile-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))
  (setenv "FZF_DEFAULT_COMMAND" "rg --files")
  :bind
  ("C-h C-k" . counsel-descbinds)
  ("s-F" . counsel-rg)
  ("s-f" . counsel-grep-or-swiper)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-h <tab>" . counsel-info-lookup-symbol)
  ("C-h C-a" . counsel-apropos)
  ("C-c u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c o" . counsel-outline)
  ("M-s-v" . counsel-yank-pop)
  ("M-Y" . counsel-yank-pop)
  ([remap find-file] . counsel-find-file)
  (:map ivy-minibuffer-map
        ("M-y" . ivy-next-line-and-call))
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history)))

(defun counsel-rg-default-directory (f &rest args)
  "Call F (`counsel-rg') with ARGS from `default-directory'."
  (let ((initial-input (car args))
        (initial-directory (or (cadr args) default-directory))
        (extra-rg-args (caddr args))
        (rg-prompt (or (cadddr args) (format "(%s) rg: " default-directory))))
    (funcall f initial-input initial-directory extra-rg-args rg-prompt)))

(advice-add 'counsel-rg :around #'counsel-rg-default-directory)

(use-package ivy-dired-history
  :config
  (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
  :bind
  (:map dired-mode-map
        ("," . dired)))

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :config
  (company-prescient-mode))

(defun projectile-load-settings (&optional file)
  "Load project elisp settings from FILE.
Look in active project root directory, or if in the case of
  undefined root directory, file is otherwise path resolvable.

https://github.com/jfeltz/projectile-load-settings/blob/master/projectile-load-settings.el"
  (interactive)
  (let ((p (expand-file-name (or file "settings.el") (projectile-project-root))))
    (when (file-exists-p p)
      (load p)
      (message "%s" (concat "Loaded project settings from: " p)))))

(setq code-directory (if (file-exists-p "~/code") "~/code" "~"))

(use-package projectile
  :custom
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-completion-system 'ivy)
  (projectile-project-search-path (list code-directory))
  (projectile-switch-project-action 'projectile-dired)
  ;; Exclude untracked files because we use git workdirs in $HOME. Listing all
  ;; files takes too long.
  ;; (projectile-git-command "git ls-files -zc --exclude-standard")
  ;; (frame-title-format
  ;;  '(""
  ;;    (:eval
  ;;     (when (fboundp 'projectile-project-name)
  ;;       (let ((project-name (projectile-project-name)))
  ;;         (unless (string= "-" project-name)
  ;;           (format "[%s]" project-name)))))))
  :config
  (projectile-mode +1)
  :hook
  ((projectile-after-switch-project . projectile-load-settings)))

(use-package counsel-projectile
  :init
  (setq counsel-projectile-remove-current-buffer t
        counsel-projectile-remove-current-project t)
  :config
  (counsel-projectile-mode)
  ;; When switching projects, go straight to dired in the project root.
  (setq counsel-projectile-switch-project-action
        (cons 4 (cdr counsel-projectile-switch-project-action)))
  :bind
  ("M-s-p" . counsel-projectile-switch-to-buffer)
  ("s-p" . counsel-projectile)
  ("s-P" . counsel-projectile-switch-project)
  ("s-t" . counsel-imenu)
  ("M-s-f" . counsel-projectile-rg))

(use-package imenu-anywhere
  :bind
  ("s-r" . ivy-imenu-anywhere))

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg)
  :hook
  (prog-mode . dumb-jump-mode)
  ;; dumb-jump shadows some Eshell key bindings, and is not useful there anyway
  (eshell-mode . (lambda () (dumb-jump-mode -1)))
  :bind
  ("s-j" . dumb-jump-go-prompt)
  ("s-." . dumb-jump-go)
  ("s-<mouse-1>". dumb-jump-go)
  ("s-J" . dumb-jump-quick-look))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; git config files
(add-to-list 'auto-mode-alist '("\\.git\\(?:config\\|ignore\\).*" . conf-mode))

(defun dired-git-add ()
  "Run `git add' on the selected files in a dired buffer."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (message "> git add %s" files)
    (dired-do-shell-command "git add" nil files)))

(bind-key ";" #'dired-git-add dired-mode-map)

;; Magit dependencies. Unless these are included here, they don't get loaded.
;; Haven't investigated why.
(use-package graphql)
(use-package treepy)

(use-package magit
  :requires
  (graphql treepy)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :commands
  (magit-call-git)
  :bind
  (("C-x g" . magit-status)
   ("C-x C-g" . magit-dispatch)))

(use-package forge
  :after magit)

;; 
(defun git-worktree-link (gitdir worktree)
  "Link git WORKTREE at GITDIR.
https://github.com/magit/magit/issues/460#issuecomment-36139308"
  (interactive (list (read-directory-name "Gitdir: ")
                     (read-directory-name "Worktree: ")))
  (with-temp-file (expand-file-name ".git" worktree)
    (insert "gitdir: " (file-relative-name gitdir worktree) "\n"))
  (magit-call-git "config" "-f" (expand-file-name "config" gitdir)
                  "core.worktree" (file-relative-name worktree gitdir))
  ;; Configure projectile to only look at tracked files
  (if (boundp 'projectile-git-command)
      (setq projectile-git-command "git ls-files -zc --exclude-standard")))

(defun git-worktree-unlink (worktree)
  "Unlink git WORKTREE at GITDIR."
  (interactive (list (read-directory-name "Worktree: ")))
  ;; Configure projectile back to default
  (if (boundp 'projectile-git-command)
      (setq projectile-git-command "git ls-files -zco --exclude-standard"))
  ;; This does `git config --unset core.worktree'.  We don't actually
  ;; have to do this and not doing it would have some advantages, but
  ;; might be confusing.
  ;; (magit-set nil "core.worktree")
  ;; This causes an error if this actually is a directory, which is
  ;; a good thing, it saves us from having to do this explicitly :-)
  (delete-file (expand-file-name ".git" worktree)))

(setq git-home-repo-dir "~/.config/repos")

(defun git-home-link (repo)
  "Interactively link a git REPO's worktree to $HOME."
  (interactive (list (completing-read "Link git home repository: "
                                      (directory-files "~/.config/repos" nil "^[^.]")
                                      nil t)))
  (setq repo (expand-file-name repo git-home-repo-dir))
  ;; "Fix" repositories that were created with --bare.
  ;; (let ((default-directory (file-name-as-directory repo)))
  ;;   (magit-set "false" "core.bare"))
  ;; Regular link.
  (git-worktree-link repo (getenv "HOME"))
  (message "Linked repo at %s" repo))

(defun git-home-unlink ()
  "Unlink the current git repo's worktree from $HOME."
  (interactive)
  (let ((f (expand-file-name ".git" (getenv "HOME"))))
    (git-worktree-unlink (getenv "HOME"))
    (message "Unlinked repo at %s" f)))

(bind-keys
 ("C-c M-l" . git-home-link)
 ("C-c M-u" . git-home-unlink))

(defun projectile-git-ls-files (&optional dir)
  "List of the tracked files in the git repo, specified by DIR."
  (cd (or dir (projectile-project-root)))
  (-filter #'nil-blank-string
           (split-string (shell-command-to-string "git ls-files") "\n")))

(defun projectile-git-ls-files-dired (&optional dir)
  "Dired list of the tracked files in the git repo, specified by DIR."
  (interactive)
  (let ((dir (or dir (projectile-project-root))))
    (dired (cons dir (projectile-git-ls-files dir)))
    (rename-buffer (format "*git ls-files %s*" dir))))

(bind-key "C-x G" #'projectile-git-ls-files-dired)

(use-package git-timemachine
  :bind
  (("C-x t" . git-timemachine)))

;; (use-package magithub
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/code"))

(use-package gist
  :bind
  (("C-x M-g" . gist-list)))

(use-package git-link
  :commands
  (git-link git-link-commit git-link-homepage))

(use-package diff-hl
  :commands
  (diff-hl-magit-post-refresh diff-hl-mode diff-hl-dired-mode)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  ((prog-mode markdown-mode) . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Network and System Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eww
  :commands
  (eww))

(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight)))

;; (use-package w3m
;;   :ensure-system-package w3m
;;   :custom
;;   (w3m-search-default-engine "duckduckgo")
;;   :commands
;;   (w3m w3m-download w3m-goto-url w3m-search))

(straight-register-package
 '(vkill :repo "https://github.com/emacsattic/vkill.git"))

;; (use-package vkill
;;   :bind
;;   (("C-c T" . vkill)))

;; (use-package proc-net
;;   :bind
;;   (("C-c n" . list-network-processes)))

(defun public-ip ()
  "Display the local host's apparent public IP address."
  (interactive)
  (message
   (with-current-buffer (url-retrieve-synchronously "https://diagnostic.opendns.com/myip")
     (goto-char (point-min))
     (re-search-forward "^$")
     (delete-char 1)
     (delete-region (point) (point-min))
     (buffer-string))))

(defun df ()
  "Display the local host's disk usage in human readable form."
  (interactive)
  (print (shell-command-to-string "df -h")))

(defun dis (hostname)
  "Resolve a HOSTNAME to its IP address."
  (interactive "MHostname: ")
  (message (shell-command-to-string
            (concat "drill "
                    hostname
                    " | awk '/;; ANSWER SECTION:/{flag=1;next}/;;/{flag=0}flag'"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package crux
  :bind
  ("C-c O" . crux-open-with)
  ("C-x f" . crux-recentf-find-file)
  ("C-c C-e" . crux-eval-and-replace)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c R" . crux-rename-file-and-buffer)
  ("M-s-r" . crux-rename-file-and-buffer)
  ("C-c k" . crux-kill-other-buffers)
  ("C-M-X" . crux-indent-defun)
  ("C-c I" . crux-find-user-init-file)
  ("C-c S" . crux-find-shell-init-file)
  ("C-<backspace>" . crux-kill-line-backwards))

(defun eval-last-sexp-other-window (arg)
  "Run `eval-last-sexp' with ARG in the other window."
  (interactive "P")
  (save-window-excursion
    (other-window 1)
    (eval-last-sexp arg)))

(defun expression-to-register (register)
  "Interactively store an Emacs Lisp expression in a REGISTER.
If region is active, store that. Otherwise, store the sexp at
  point."
  (interactive (list (register-read-with-preview "Copy expression to register: ")))
  (set-register register
                (if (region-active-p)
                    (buffer-substring (mark) (point))
                  (destructuring-bind (start . end) (bounds-of-thing-at-point 'sexp)
                    (buffer-substring start end))))
  (setq deactivate-mark t)
  (when (called-interactively-p 'interactive) (indicate-copied-region)))

(defun eval-register (register)
  "Evaluate contents of register REGISTER as an Emacs Lisp expression.
REGISTER is a character and its contents are a string.

If called with a prefix arg, then insert the return value at
point.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (register-read-with-preview "Eval register: ")
                       current-prefix-arg)))
  (let* ((val (get-register register))
         (res (eval (car (read-from-string (format "(progn %s)" val))))))
    (when current-prefix-arg (register-val-insert res))))

(defun replace-last-sexp ()
  "Eval the preceeding sexp and replace it with the result."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(bind-keys ("C-c C-j" . replace-last-sexp)
           :map lisp-mode-shared-map
           ("s-<return>" . eval-last-sexp)
           ("C-s-<return>" . eval-last-sexp-other-window)
           ("C-c C-k" . eval-buffer)
           ("C-x C-r" . eval-region)
           ("C-x r E" . expression-to-register)
           ("C-x r e" . eval-register))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

(use-package elisp-format
  :commands
  (elisp-format-buffer elisp-format-file elisp-format-region))

;; (use-package lively
;;   :commands
;;   (lively-shell-command)
;;   :bind
;;   ("C-c C-l l" . lively)
;;   ("C-c C-l r" . lively-region)
;;   ("C-c C-l u" . lively-update)
;;   ("C-c C-l s" . lively-stop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode))
  :config
  (add-to-list 'interpreter-mode-alist '("inlein" . clojure-mode))
  (progn
    (define-clojure-indent
      (defroutes 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (context 2)
      (let-routes 1))

    (define-clojure-indent
      (form-to 1))

    (define-clojure-indent
      (match 1)
      (are 2)
      (checking 2)
      (async 1))

    (define-clojure-indent
      (select 1)
      (insert 1)
      (update 1)
      (delete 1))

    (define-clojure-indent
      (run* 1)
      (fresh 1))

    (define-clojure-indent
      (extend-freeze 2)
      (extend-thaw 1))

    (define-clojure-indent
      (go-loop 1))

    (define-clojure-indent
      (this-as 1)
      (specify 1)
      (specify! 1))

    (define-clojure-indent
      (s/fdef 1))

    (defun toggle-nrepl-buffer ()
      "Toggle the nREPL REPL on and off"
      (interactive)
      (if (string-match "cider-repl" (buffer-name (current-buffer)))
          (delete-window)
        (cider-switch-to-repl-buffer)))

    (defun cider-save-and-refresh ()
      (interactive)
      (save-buffer)
      (call-interactively 'cider-refresh))

    (defun cider-eval-last-sexp-and-append ()
      (interactive)
      (cider-eval-last-sexp '(1))))
  :hook
  ((clojure-mode clojurescript-mode) . turn-on-eldoc-mode))

(use-package clojure-mode-extra-font-locking
  :defer 1)

(use-package cider
  :config
  (add-hook 'cider-repl-mode-hook (lambda () (company-mode nil)))
  :bind
  (:map cider-mode-map
        ("s-<return>" . cider-eval-last-sexp)))

(defun inf-clojure-start-lumo ()
  "Start lumo as a subprocess and then connect to it over TCP.
This is preferable to starting it directly because lumo has lots
of problems in that context."
  (interactive)
  (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
  (inf-clojure-minor-mode)
  (shell-command "pkill -f 'lumo -d -n 2000'")
  (async-shell-command "lumo -d -n 2000")
  (run-with-idle-timer 2 nil (lambda () (inf-clojure-connect "localhost" 2000))))

(defun reinstate-comint-simple-send ()
  "Reinstate comint's default `comint-simple-send'.
`inf-clojure' clobbers `comint-send-input' on all comint buffers,
not just `inf-clojure-mode' ones. This function reinstates
default behavior. See:
https://github.com/clojure-emacs/inf-clojure/issues/154"
  (unless (bound-and-true-p inf-clojure-minor-mode)
    (setq-local comint-input-sender 'comint-simple-send)))

(use-package inf-clojure
  :hook
  (comint-mode . reinstate-comint-simple-send)
  :bind
  (:map inf-clojure-minor-mode-map
        ("s-<return>" . inf-clojure-eval-last-sexp)
        ("C-c C-k" . inf-clojure-eval-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other File Modes and Formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display nfo files in all their glory
;; https://github.com/wasamasa/dotemacs/blob/master/init.org#display-nfo-files-with-appropriate-code-page)
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

;; perl
(setq perl-indent-level tab-width)

;; systemd
(add-to-list 'auto-mode-alist '("\\(?:\\.service\\|\\.timer\\)\\'" . conf-mode))

;; DNS
(add-to-list 'auto-mode-alist '("\\.rpz\\'" . dns-mode))

;; (use-package quickrun
;;   :commands
;;   (quickrun quickrun-region quickrun-shell quickrun-autorun-mode))

;; (use-package sly
;;   ;; There are some problems building sly with straight.el in Windows
;;   :unless (eq system-type 'windows-nt)
;;   :custom
;;   (inferior-lisp-program (executable-find "sbcl"))
;;   :bind
;;   (:map sly-prefix-map
;;         ("M-h" . sly-documentation-lookup)))

;; (use-package sly-company
;;   :unless (eq system-type 'windows-nt)
;;   :hook
;;   (sly-mode . sly-company-mode)
;;   :config
;;   (add-to-list 'company-backends 'sly-company))

;; Configured to use CHICKEN Scheme
;; (use-package geiser
;;   :custom
;;   (geiser-default-implementation 'chicken)
;;   (geiser-mode-eval-last-sexp-to-buffer t)
;;   (scheme-program-name "csi -:c")
;;   :config
;;   (setq-default geiser-scheme-implementation 'chicken)

;;   ;; Indenting module body code at column 0
;;   (defun scheme-module-indent (state indent-point normal-indent) 0)
;;   (put 'module 'scheme-indent-function 'scheme-module-indent)
;;   (put 'and-let* 'scheme-indent-function 1)
;;   (put 'parameterize 'scheme-indent-function 1)
;;   (put 'handle-exceptions 'scheme-indent-function 1)
;;   (put 'when 'scheme-indent-function 1)
;;   (put 'unless 'scheme-indenfunction 1)
;;   (put 'match 'scheme-indent-function 1)
;;   :commands
;;   (geiser run-geiser run-chicken))

(use-package markdown-mode
  :mode "\\.md\\|markdown\\'"
  :custom
  (markdown-command "multimarkdown"))

;; (use-package htmlize
;;   :bind
;;   (("C-c h b" . htmlize-buffer)
;;    ("C-c h f" . htmlize-file)
;;    ("C-c h m" . htmlize-many-files)
;;    :map dired-mode-map
;;    ("C-c h m" . htmlize-many-files-dired)))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker
  :bind
  ("C-c M-d" . docker))

(use-package docker-tramp
  :defer 2)

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package nginx-mode
  :custom
  (nginx-indent-level tab-width))

(use-package caddyfile-mode
  :mode "\\`Caddyfile\\'")

(use-package yaml-mode
  :mode "\\.ya\?ml\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package web-mode
  :mode "\\.html\?\\'"
  :init
  ;; from web-mode FAQ to work with smartparens
  (defun m-web-mode-hook ()
    (setq web-mode-enable-auto-pairing nil))
  (defun sp-web-mode-is-code-context (_id action _context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))  :mode
  ("\\.phtml\\'"
   "\\.tpl\\.php\\'"
   "\\.[agj]sp\\'"
   "\\.as[cp]x\\'"
   "\\.erb\\'"
   "\\.mustache\\'"
   "\\.djhtml\\'"
   "\\.html?\\'")
  :custom
  (sgml-basic-offset tab-width)
  (web-mode-markup-indent-offset tab-width)
  (web-mode-css-indent-offset tab-width)
  (web-mode-code-indent-offset tab-width)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                               ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  :hook
  (web-mode . m-web-mode-hook))

;; CSS config
(setq-default css-indent-offset tab-width)

(defun toggle-sp-newline ()
  "Toggle whether `RET' is bound to `newline' or `sp-newline'."
  (interactive)
  (let* ((f (key-binding (kbd "RET")))
         (newf (if (eq f 'sp-newline) #'newline #'sp-newline)))
    (bind-key "RET" newf smartparens-mode-map)
    (message "<RET> now invokes to %s" newf)))

(bind-key "C-c C-<return>" #'toggle-sp-newline)

;; Pulls in `js2-mode' because it is derived from it.
(use-package rjsx-mode
  :mode "\\.jsx?\\'"
  :custom
  (js2-basic-offset tab-width)
  ;; Set tab width for js-mode and json-mode
  (js-indent-level tab-width)
  :hook
  (js2-mode . js2-imenu-extras-mode))

(use-package js2-refactor
  :hook
  (js2-mode . js2-refactor-mode)
  :bind
  (:map js2-mode-map
        ("C-k" . js2r-kill)))

(use-package xref-js2
  :hook
  (js2-mode . (lambda ()
                (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  :bind
  (:map js-mode-map
        ;; Don't shadow js2-mode-map
        ("M-." . nil)))

(use-package tide
  :custom
  (tide-format-options '(:indentSize 2 :tabSize 2))
  (tide-default-mode "JS")
  :hook
  ((js2-mode typescript-mode) . (lambda ()
                                  (tide-setup)
                                  (setq flycheck-checkers (remove 'jsx-tide flycheck-checkers))))
  ((js2-mode typescript-mode) . tide-hl-identifier-mode))

(use-package add-node-modules-path
  :hook
  (js2-mode . add-node-modules-path))

(use-package prettier-js
  :hook
  (js2-mode  . prettier-js-mode))

(use-package indium
  :custom
  (indium-chrome-executable "/Applications/Chromium.app/Contents/MacOS/Chromium")
  (indium-chrome-use-temporary-profile nil)
  :commands
  (indium-connect indium-launch))

(use-package json-mode
  :mode "\\.json\\'")

(use-package graphql-mode
  :mode "\\(?:\\.g\\(?:\\(?:raph\\)?ql\\)\\)\\'")

(use-package know-your-http-well
  :commands
  (http-header http-method http-relation http-status-code))

(use-package restclient
  :mode "\\.restclient\\'"
  :commands
  (restclient-mode restclient-outline-mode))

(use-package company-restclient
  :hook
  (restclient-mode . (lambda ()
                       (add-to-list 'company-backends 'company-restclient))))

(use-package elpy
  ;; :ensure-system-package
  ;; (jedi . "pip install jedi flake8 autopep8 yapf")
  :interpreter ("python3?" . python-mode)
  :custom
  ;; (python-shell-interpreter "ipython")
  ;; (python-shell-interpreter-args "-i --simple-prompt")
  (gud-pdb-command-name "python -m pdb")
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  :hook
  (python-mode . (lambda ()
                   (unless (bound-and-true-p elpy-version)
                     (elpy-enable))))
  :bind
  (:map python-mode-map
        ("s-<return>" . py-execute-expression)))

;; (use-package py-autopep8
;;   :hook
;;   ((elpy-mode . py-autopep8-enable-on-save)))

(use-package hy-mode
  ;; :ensure-system-package
  ;; (hy . "pip install git+https://github.com/hylang/hy.git")
  :mode "\\.hy\\'"
  :bind
  (:map hy-mode-map
        ("s-<return>" . hy-shell-eval-last-sexp)))

(use-package enh-ruby-mode
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'")

(use-package inf-ruby
  :hook enh-ruby-mode
  :commands
  (inf-ruby inf-ruby-console-auto)
  :bind
  (:map inf-ruby-minor-mode-map
        ("s-<return>". ruby-send-last-sexp)
        ("C-M-x" . ruby-send-block)))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package go-mode
  :mode "\\.go\\'")

(use-package company-go
  :hook
  (go-mode . (lambda () (set (make-local-variable 'company-backends) '(company-go)))))

(use-package sass-mode
  :mode "\\(?:s\\(?:[ac]?ss\\)\\)")

(use-package powershell
  :mode "\\.ps1\\'"
  :custom
  (powershell-indent tab-width)
  (powershell-continuation-indent tab-width))

(use-package php-mode
  :mode "\\.php\\'")

(use-package ios-config-mode
  :mode "\\.cfg\\'")

;; `eval-in-repl' requires a number of other packages so it's best to load it last
;; (use-package eval-in-repl
;;   :bind
;;   (:map
;;    emacs-lisp-mode-map
;;    ("C-<return>" . eir-eval-in-ielm)
;;    :map
;;    lisp-interaction-mode-map
;;    ("C-<return>" . eir-eval-in-ielm)
;;    :map
;;    Info-mode-map
;;    ("s-<return>" . eir-eval-in-ielm)
;;    :map
;;    python-mode-map
;;    ("s-<return>" . eir-eval-in-python)
;;    :map
;;    ruby-mode-map
;;    ("s-<return>" . eir-eval-in-ruby)
;;    :map
;;    sh-mode-map
;;    ("s-<return>" . eir-eval-in-shell)))

;; (use-package polymode
;;   :config
;;   (defcustom pm-host/rjsx
;;     (pm-host-chunkmode :name "rjsx"
;;                        :mode 'rjsx-mode)
;;     "React JSX host chunkmode"
;;     :group 'poly-hostmodes
;;     :type 'object)
;;   (defcustom pm-inner/graphql-fenced-code
;;     (pm-inner-chunkmode :name "graphql"
;;                         :head-matcher "graphql`"
;;                         :tail-matcher "`"
;;                         :mode 'graphql-mode
;;                         :head-mode 'host
;;                         :tail-mode 'host)
;;     "GraphQL fenced code block"
;;     :group 'poly-innermodes
;;     :type 'object)
;;   (define-polymode poly-rjsx-mode
;;     :hostmode 'pm-host/rjsx
;;     :innermodes '(pm-inner/graphql-fenced-code)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun advice-delete-other-windows (&rest _)
  "Advice that will delete other windows."
  (delete-other-windows))

(advice-add 'wttrin :before #'advice-delete-other-windows)

(use-package wttrin
  :custom
  (wttrin-default-cities '("Albany CA"
                           "San Francisco CA"
                           "Austin TX"
                           "Eugene OR"
                           "Truckee CA"
                           "Moon"))
  (wttrin-default-accept-language '("Accept-Language" . "en-US"))
  :bind
  ("C-c M-w" . wttrin))

;; (require 'highlight-things)

;; (require 'm-pointhistory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load private stuff if it exists
(require 'm-private nil 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(provide 'init)

;;; init.el ends here
