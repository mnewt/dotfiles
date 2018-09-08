;;; init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:
;; Single, monolithic Emacs init file. Uses straight.el for package management
;; and use-package for as much package configuration as possible.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top Level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Give Emacs 1GB of heap but run gc on idle.
(setq gc-cons-threshold 1073741824)
(run-with-idle-timer 30 t (lambda () (garbage-collect)))

(add-to-list 'load-path "~/.emacs.d/elisp/")

(setq tls-checktrust t
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
(setq straight-use-package-by-default t)

;; https://github.com/raxod502/straight.el/issues/41
(setq straight-check-for-modifications 'live)

;; Try to get package.el to work better with straight.el
;; https://github.com/raxod502/straight.el/issues/128
(defun straight--advice-package-installed-p (original-function &rest args)
  "Return t if package is installed via `straight' package manager."
  (or (gethash (symbol-name (car args)) straight--recipe-cache)
      (apply original-function args)))

(advice-add 'package-installed-p :around 'straight--advice-package-installed-p)

(defun m-straight-merge-all (&optional from-upstream)
  "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from configured upstreams.

Do not merge packages listed in `my-pinned-packages'."
  (interactive "P")
  (straight-merge-all
   from-upstream
   (lambda (package)
     (not (member package m-straight-pinned-packages)))))

;; Packages in this list do not get updated when `update-packages' runs.
;; Therefore, they stay at their current version until manually updated in some
;; way, perhaps with `straight-merge-package'. See
;; https://github.com/raxod502/straight.el/issues/246#issuecomment-415085772.
(setq m-straight-pinned-packages
      '(org-mode))

;; use-package is good
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(defun update-packages ()
  "Use straight.el to update all packages."
  (interactive)
  (straight-normalize-all)
  (straight-fetch-all)
  (m-straight-merge-all))

(use-package epkg
  :commands
  (epkg epkg-describe-package epkg-list-packages))

;; dash.el is required by many things, firstly the Environment section.
(use-package dash
  :config
  (dash-enable-font-lock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Path
(setq set-path-unix '("/usr/local/bin")
      set-path-windows '("C:/bin"
                         "C:/Program Files/Emacs/bin")
      set-path-user '("~/.bin"))

(defun source-sh (filename)
  "Update environment variables from a shell source file."
  (interactive "fSource file: ")
  (message "Sourcing environment from `%s'..." filename)
  (with-temp-buffer
    (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))
    (let ((envvar-re "declare -x \\([^=]+\\)=\\(.*\\)$"))
      ;; Remove environment variables
      (while (search-forward-regexp (concat "^-" envvar-re) nil t)
        (let ((var (match-string 1)))
          (message "%s" (prin1-to-string `(setenv ,var nil)))
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
  "Set path variables correctly for Linux, macOS, or Windows"
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
    (setq exec-path new-path)))

(source-sh "~/.env")
(source-sh "~/.bin/start-ssh-agent")
(set-path)

(defun expand-environment-variable ()
  "Insert contents of an envionment variable at point."
  (interactive)
  (insert (getenv (read-envvar-name "Insert Environment Variable: "))))

(bind-key "C-c C-v" 'expand-environment-variable)

(use-package server
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(use-package restart-emacs
  :commands
  (restart-emacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default frame settings. This is actually maximized, but not full screen.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(cursor-color . "#F60"))

;; eww uses this, among others.
(set-face-attribute 'variable-pitch nil
                    :family "Georgia")

;; Base faces for modeline
(defface m-inactive0 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 0."
  :group 'powerline)
(defface m-active0 '((t (:inherit default)))
  "Powerline active face 0."
  :group 'powerline)
(defface m-inactive1 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 1."
  :group 'powerline)
(defface m-active1 '((t (:inherit default)))
  "Powerline active face 1."
  :group 'powerline)
(defface m-inactive2 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 2."
  :group 'powerline)
(defface m-active2 '((t (:inherit default)))
  "Powerline active face 2."
  :group 'powerline)
(defface m-inactive3 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 3."
  :group 'powerline)
(defface m-active3 '((t (:inherit default)))
  "Powerline active face 3."
  :group 'powerline)
(defface m-inactive4 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 4."
  :group 'powerline)
(defface m-active4 '((t (:inherit default)))
  "Powerline active face 4."
  :group 'powerline)

(setq-default m-themes '())

(defun activate-theme-common ()
  "Run whenever a theme is activated."
  (set-face-attribute 'mode-line nil
                      :box nil
                      :overline nil
                      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :overline nil
                      :underline nil))

(defun activate-theme (x)
  "Disable current themes and load theme X."
  (let ((theme (if (stringp x) (intern x) x)))
    (condition-case nil
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme theme t)
          (activate-theme-common)
          (funcall (cdr (assoc theme m-themes)))
          (when (fboundp 'powerline-reset) (powerline-reset)))
      (error "Problem loading theme %s" x))))

(defun choose-theme ()
  "Forward to `load-theme'.))))
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'."
  (interactive)
  (ivy-read "Load custom theme: "
            (mapcar 'car m-themes)
            :action #'activate-theme
            :caller 'choose-theme))

(use-package dracula-theme
  :load-path "straight/build/dracula-theme"
  :config
  (defun activate-theme-dracula ()
    (setq face-remapping-alist
          '((m-inactive0 :background "#262834" :foreground "#565861")
            (m-active0 :background "#565861" :foreground "#9E9FA5")
            (m-inactive1 :background "#262834" :foreground "#565861")
            (m-active1 :background "#565861" :foreground "#E6E7E8")
            (m-inactive2 :background "#262834" :foreground "#565861")
            (m-active2 :background "#CECFD2" :foreground "#565861")
            (m-inactive3 :background "#565861" :foreground "#9E9FA5")
            (m-active3 :background "#A863C9" :foreground "#FFFFFF")
            (m-inactive4 :background "#565861" :foreground "#9E9FA5")
            (m-active4 :background "#00e5e5" :foreground "#262834")))
    (set-cursor-color "#F60")
    (set-mouse-color "white")
    (set-background-color "#2A2A2A")
    (set-face-attribute 'mode-line-emphasis nil :foreground "orange"))
  (add-to-list 'm-themes '(dracula . activate-theme-dracula))
  (activate-theme 'dracula))

(use-package solarized-theme
  :load-path "straight/build/solarized-theme"
  :config
  (defun activate-theme-solarized-light ()
    (setq face-remapping-alist
          '((m-inactive0 :background "#EDE8D7" :foreground "#EDE8D7")
            (m-active0 :background "#9E9FA5" :foreground "#E6E7E8")
            (m-inactive1 :background "#EDE8D7" :foreground "#EDE8D7")
            (m-active1 :background "#9E9FA5" :foreground "#EDE8D7")
            (m-inactive2 :background "#EDE8D7" :foreground "#EDE8D7")
            (m-active2 :background "#CECFD2" :foreground "#565861")
            (m-inactive3 :background "#EDE8D7" :foreground "#9E9FA5")
            (m-active3 :background "#A863C9" :foreground "#FFFFFF")
            (m-inactive4 :background "#EDE8D7" :foreground "#9E9FA5")
            (m-active4 :background "#00E5E5" :foreground "#262834")))
    (set-mouse-color "black"))

  (add-to-list 'm-themes '(solarized-light . activate-theme-solarized-light)))

(use-package powerline
  :custom
  (powerline-default-separator nil)
  (powerline-narrowed-indicator "n")
  :init
  (set-face-attribute 'mode-line nil :box nil)
  :config
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'm-active0 'm-inactive0))
                          (face1 (if active 'm-active1 'm-inactive1))
                          (face2 (if active 'm-active2 'm-inactive2))
                          (face3 (if active 'm-active3 'm-inactive3))
                          (face4 (if active 'm-active4 'm-inactive4))
                          (lhs (list (powerline-raw " " face1)
                                     (powerline-major-mode face2 'l)
                                     (powerline-raw " " face2)
                                     ;; (powerline-vc face1 'r)
                                     (if (eq major-mode 'term-mode)
                                         (powerline-raw
                                          (cond
                                           ((term-in-char-mode) " (char-mode) ")
                                           ((term-in-line-mode) " (line-mode) ")
                                           (t ""))
                                          face1))))
                          (center (list (powerline-raw "%*" face1 'l)
                                        (powerline-raw
                                         (if (file-remote-p default-directory)
                                             (concat " "
                                                     (tramp-file-name-host
                                                      (tramp-dissect-file-name
                                                       default-directory))
                                                     " ")
                                           "")
                                         face4)
                                        (powerline-raw " " face3)
                                        (powerline-raw (buffer-name) face3 'm)
                                        (powerline-raw " " face3)
                                        (powerline-raw "%*" face1 'r)))
                          (rhs (if active
                                   (list (if (and (boundp 'outline-minor-mode)
                                                  outline-minor-mode)
                                             (powerline-raw "o" face0))
                                         (if (and (boundp 'hs-minor-mode)
                                                  hs-minor-mode)
                                             (powerline-raw "h" face0))
                                         (powerline-narrow face0)
                                         (powerline-raw " " face0)
                                         (if (fboundp #'eyebrowse-mode-line-indicator)
                                             (eyebrowse-mode-line-indicator))
                                         (powerline-raw global-mode-string face0 'r)
                                         (powerline-raw " " face0)
                                         (powerline-raw "%l" face1 'r)
                                         (powerline-raw ":" face1)
                                         (powerline-raw "%c" face1 'r)
                                         (powerline-hud face3 face1)))))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

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

;; A more pleasant bell. No sound. Simply flash the echo area.
(defun mode-line-visible-bell ()
  "A friendlier visual bell effect."
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist '((default highlight))))
  (run-with-timer 0.15 nil (lambda ()
                             (with-current-buffer (get-buffer " *Echo Area 0*")
                               (setq-local face-remapping-alist '((default)))))))

;; Blinking is NOT OK
(blink-cursor-mode -1)

;; Beeping is REALLY NOT OK
(setq visible-bell t)
      ;; ring-bell-function #'mode-line-visible-bell)

;; Use the system clipboard
(setq select-enable-clipboard t
      ;; Save existing system clipboard text into kill ring before replacing it,
      ;; ensuring it doesn't get irrevocably destroyed.
      save-interprogram-paste-before-kill t
      ;; use mouse to kill/yank
      mouse-yank-at-point t
      mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cut-when-buffers-differ t)

;; Highlight current line
(global-hl-line-mode 1)

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

;; (defun scroll-down-4 ()
;;   "Scroll 4 lines down."
;;   (interactive)
;;   (setq this-command 'scroll-down)
;;   (scroll-down 4))

;; (defun scroll-up-4 ()
;;   "Scroll 4 lines up."
;;   (interactive)
;;   (setq this-command 'scroll-up)
;;   (scroll-up 4))

(defun next-line-4 ()
  "Scroll 4 lines down."
  (interactive)
  (setq this-command 'next-line)
  (next-line 4))

(defun previous-line-4 ()
  "Scroll 4 lines up."
  (interactive)
  (setq this-command 'previous-line)
  (previous-line 4))

(bind-keys
 ("C-S-p" . previous-line-4)
 ("C-S-n" . next-line-4))

;; Reduce scrolling lag
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

;; Enable every deactivated command, because some are handy and why not?
;; (setq disabled-command-function nil)

;; Whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on disk.
(global-auto-revert-mode 1)

;; Full screen
(defun fullscreen ()
  "Toggle fullscreen mode."
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(bind-keys ("C-s-f" . fullscreen))

;; Change yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

(defun cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

Adapted from: `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'"
  (interactive)
  (if current-prefix-arg
      (progn ; not using clipboard-kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (clipboard-kill-region (region-beginning) (region-end) t)
             (clipboard-kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

Adapted from: `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'"
  (interactive)
  (if current-prefix-arg
      (progn
        (kill-ring-save-keep-highlight (point-min) (point-max))
        (message "All visible buffer text copied"))
    (if (use-region-p)
        (progn
          (kill-ring-save-keep-highlight (region-beginning) (region-end))
          (message "Active region copied"))
      (if (eq last-command this-command)
          (if (eobp)
              (progn (message "empty line at end of buffer."))
            (progn
              (kill-append "\n" nil)
              (kill-append
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               nil)
              (message "Line copy appended")
              (progn
                (end-of-line)
                (forward-char))))
        (if (eobp)
            (if (eq (char-before) 10)
                (progn (message "empty line at end of buffer."))
              (progn
                (kill-ring-save-keep-highlight (line-beginning-position) (line-end-position))
                (end-of-line)
                (message "line copied")))
          (progn
            (kill-ring-save-keep-highlight (line-beginning-position) (line-end-position))
            (end-of-line)
            (forward-char)
            (message "line copied")))))))

(defun comment-toggle ()
  "Toggles comments for the region. If no region is selected, toggles comments
  for the line"
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

(defun config-macos ()
  "Configure Emacs for macOS."
  (setq ns-alternate-modifier 'meta
        ns-right-alternate-modifier 'none
        ns-command-modifier 'super
        ns-right-command-modifier 'left
        ns-control-modifier 'control
        ns-right-control-modifier 'left
        ns-function-modifier 'hyper)
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
    (call-process (executable-find "trash")
                  nil 0 nil
                  file))

  (use-package reveal-in-osx-finder
    :bind
    ("s-i" . reveal-in-osx-finder)))

(defun config-windows-nt ()
  "Configure Emacs for Windows NT."
  (menu-bar-mode -1)
  (setq w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)
  (set-face-font 'default "Lucida Console-12"))

;; Key bindings to make moving between Emacs and other appliations a bit less
;; jarring. These are mostly based on macOS defaults but many work on Windows
;; and Linux. They can be overridden by the OS specific configurations below.
(bind-keys
 ("s-o" . find-file)
 ("s-O" . find-file-other-window)
 ("s-s" . save-buffer)
 ("s-S" . write-file)
 ("s-q" . save-buffers-kill-emacs)
 ("s-z" . undo)
 ("s-x" . cut-line-or-region)
 ("s-c" . copy-line-or-region)
 ("s-v" . yank)
 ("s-a" . mark-whole-buffer)
 ("s-g" . isearch-repeat-forward)
 ("s-G" . isearch-repeat-backward)
 ("s-l" . select-current-line)
 ("s-\`" . other-frame)
 ("s-N" . make-frame-command)
 ("s-w" . kill-buffer)
 ("s-W" . delete-frame)
 ("s-/" . comment-toggle)
 ("s-h" . ns-do-hide-emacs)
 ("s-H" . ns-do-hide-others))

;; OS specific configuration
(pcase system-type
  ('darwin (config-macos))
  ('windows-nt (config-windows-nt)))

(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

;; Enable ido for the few functions that don't have ivy coverage.
(setq ido-enable-flex-matching t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; savehist
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

;; Store all backup and autosave files in their own directory.
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))
      version-control t
      vc-make-backup-files t
      ;; Always save old versions of backup files, don't prompt
      delete-old-versions -1
      auto-save-list-file-prefix "~/.emacs.d/autosave/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; Desktop
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq suggest-key-bindings 5
      apropos-do-all t)

;; About Emacs is not useful so replace binding with `apropos'
;; (bind-keys ("C-h C-a" . apropos))

;; ELDoc
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)

;; Whenever the listed commands are used, ElDoc will automatically refresh the
;; minibuffer.
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(use-package help-fns+)

(use-package helpful
  :bind
  (("C-h ." . helpful-at-point)
   ("C-h f" . helpful-callable)
   ("C-h c" . helpful-command)
   ("C-h F" . helpful-function)
   ("C-h k" . helpful-key)
   ("C-h M" . helpful-macro)
   ("C-h M-s" . helpful-symbol)
   ("C-h v" . helpful-variable)))

(use-package which-key
  :config
  (which-key-mode t)
  :bind
  (("M-s-h" . which-key-show-top-level)))

(use-package man
  :config
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

(use-package tldr
  :init
  (unbind-key "C-h t")
  :custom
  (tldr-enabled-categories '("common" "linux" "osx"))
  :bind
  (("C-h t t" . tldr)
   ("C-h t u" . tldr-update-docs)))

(use-package info-colors
  :commands
  (info-colors-fontify-node)
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer Navigation and Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "C-x C-b" #'ibuffer)

(setq windmove-wrap-around t)
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
           ("s-," . pop-global-mark))

;; Quick switch buffers using standard macOS tab movement bindings
(bind-keys ("s-}" . next-buffer)
           ("s-{" . previous-buffer))

;; scratch
(setq initial-scratch-message nil
      initial-major-mode 'org-mode)

(defun new-scratch-buffer ()
  "Create or go to a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "<untitled>"))
  (setq buffer-file-name "untitled")
  (org-mode))

(bind-key "s-n" #'new-scratch-buffer)

;; kill buffer and window
(defun kill-other-buffer-and-window ()
  "Kill the buffer in the other window."
  (interactive)
  (select-window (next-window))
  (kill-buffer-and-window))

(bind-keys ("C-s-w" . delete-window)
           ("M-s-w" . kill-buffer-and-window)
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
(bind-key "s-R" #'xref-find-definitions-other-window)

;; Create friendly names for buffers with the same name
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
                                        ; rename after killing uniquified
      uniquify-after-kill-buffer-p t
                                        ; don't muck with special buffers
      uniquify-ignore-buffers-re "^\\*")

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line. Most of
console-based utilities prints filename in format 'filename:linenumber'. So you
may wish to open filename in that format. Just call: emacsclient
filename:linenumber and file 'filename' will be opened and cursor set on line
'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn))) files)))

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

(use-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode t)
  (set-face-attribute 'auto-dim-other-buffers-face nil :background "#1F1F1F"))

(use-package winum
  :custom
  (winum-auto-setup-mode-line nil)
  :config
  (winum-mode)
  :bind
  ("s-1" . winum-select-window-1)
  ("s-2" . winum-select-window-2)
  ("s-3" . winum-select-window-3)
  ("s-4" . winum-select-window-4)
  ("s-5" . winum-select-window-5)
  ("s-6" . winum-select-window-6)
  ("s-7" . winum-select-window-7)
  ("s-8" . winum-select-window-8)
  ("s-9" . winum-select-window-9)
  ("s-0" . winum-select-window-0))

(use-package eyebrowse
  :custom
  (eyebrowse-new-workspace t)
  (eyebrowse-mode-line-separator " ")
  :config
  (eyebrowse-mode t)
  :bind
  ("H-1" . eyebrowse-switch-to-window-config-1)
  ("H-2" . eyebrowse-switch-to-window-config-2)
  ("H-3" . eyebrowse-switch-to-window-config-3)
  ("H-4" . eyebrowse-switch-to-window-config-4)
  ("H-5" . eyebrowse-switch-to-window-config-5)
  ("H-6" . eyebrowse-switch-to-window-config-6)
  ("H-7" . eyebrowse-switch-to-window-config-7)
  ("H-8" . eyebrowse-switch-to-window-config-8)
  ("H-9" . eyebrowse-switch-to-window-config-9)
  ("H-0" . eyebrowse-switch-to-window-config-0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wrap text
(setq-default fill-column 80)

;; Newline at end of file
(setq require-final-newline t)

;; Delete selection on insert or yank
(delete-selection-mode 1)

;; Tabs
(setq-default indent-tabs-mode nil
              tab-width 2
              tab-stop-list (number-sequence tab-width 120 tab-width))

;; Replace `delete-horizontal-space' with the more useful `cycle-spacing'. 
(bind-key "M-\\" #'cycle-spacing)

;; sh-mode
(setq sh-basic-offset tab-width
      sh-indentation tab-width)

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("\\DWfile.*\\'" . sh-mode))

;; Move by whole words rather than sub-words
;; (global-superword-mode)

(bind-key "RET" #'newline-and-indent)

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "Buffer indented."))

;; http://whattheemacsd.com/key-bindings.el-03.html
(defun join-line-previous ()
  (interactive)
  (join-line -1))

(bind-key "M-J" #'join-line-previous)

(defun dos2unix ()
  "Convert DOS aformatted buffer to Unix by removing the ^M at
the end of each line."
  (interactive)
  (let ((line (line-number-at-pos))
        (column (current-column)))
    (mark-whole-buffer)
    (replace-string "
" "")
    (mark-whole-buffer)
    (replace-string "" "
")
    (goto-line line)
    (move-to-column column)))

(defun touch (cmd)
  "Run touch in `default-directory'."
  (interactive
   (list (read-shell-command "Run touch (like this): "
                             "touch "
                             'touch-history
                             "touch ")))
  (shell-command cmd))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Just set up 3 windows, no fancy frames or whatever
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; Make a shell script executable automatically on save
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Tell `executable-set-magic' to insert #!/usr/bin/env interpreter
(setq executable-prefix-env t)

;; Continue comment on next line (default binding is "C-M-j")
(bind-key "M-RET" #'indent-new-comment-line)

;; Automatically fill comments. Wraps on `fill-column' columns
(defun configure-auto-fill-mode ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook #'configure-auto-fill-mode)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(bind-key [remap goto-line] #'goto-line-with-feedback)

(use-package diff
  :bind
  (:map diff-mode-map
        ("M-p" . diff-hunk-prev)
        ("M-n" . diff-hunk-next)))

;; One key binding to move/copy text from current other window
;; https://emacs.stackexchange.com/questions/3743/how-to-move-region-to-other-window
(defun move-region-to-other-window (start end)
  "Move selected text to other window."
  (interactive "r")
  (if (use-region-p)
      (let ((count (count-words-region start end)))
        (save-excursion
          (kill-region start end)
          (other-window 1)
          (yank)
          (newline))
        (other-window -1)
        (message "Moved %s words" count))
    (message "No region selected")))

(defun copy-region-to-other-window (start end)
  "Move selected text to other window."
  (interactive "r")
  (if (use-region-p)
      (let ((count (count-words-region start end)))
        (save-excursion
          (copy-region-as-kill start end)
          (other-window 1)
          (yank)
          (newline))
        (other-window -1)
        (message "Moved %s words" count))
    (message "No region selected")))

(bind-keys ("s-C" . copy-region-to-other-window)
           ("s-X" . move-region-to-other-window))

(use-package elisp-slime-nav
  :hook
  (emacs-lisp-mode . (lambda () (elisp-slime-nav-mode t))))

(use-package undo-tree
  :init
  ;; Keep region when undoing in region.
  ;; http://whattheemacsd.com/my-misc.el-02.html
  (defadvice undo-tree-undo (around keep-region activate)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it))
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  :config
  (global-undo-tree-mode)
  :bind
  (("s-z" . undo-tree-undo)
   ("s-Z" . undo-tree-redo)
   ("s-y" . undo-tree-redo)
   ("M-s-z" . undo-tree-visualize)))

(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

(use-package ace-jump-zap
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("C-M-z" . ace-jump-zap-to-char-dwim)))

(use-package mwim
  :bind
  (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
   ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package expand-region
  :init
  (setq expand-region-fast-keys-enabled nil)
  :bind
  (("s-d" . er/expand-region)
   ("s-D" . er/contract-region)))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("M-s-m" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/unmark-next-like-this)
   ("C-M-<" . mc/mark-previous-like-this)
   ("C-M->" . mc/unmark-previous-like-this)
   ("C-c >" . mc/mark-all-dwim)
   ("C-c C-a"  . mc/mark-all-dwim)))

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

;; outline-mode for folding sections (in lisps that is defined by `;;;')
(use-package outshine
  :custom
  (outline-minor-mode-prefix "\M-#")
  :config
  ;; Narrowing now works within the headline rather than requiring to be on it
  (advice-add 'outshine-narrow-to-subtree :before
              (lambda (&rest args) (unless (outline-on-heading-p t)
                                     (outline-previous-visible-heading 1))))
  :hook
  (prog-mode . outline-minor-mode)
  (outline-minor-mode . outshine-hook-function)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; S-Expressions, Parentheses, Brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sp-sh-post-handler (id action context)
  "Bash post handler.
ID, ACTION, CONTEXT."
  (-let (((&plist :arg arg :enc enc) sp-handler-context))
    (when (equal action 'barf-backward)
      (sp-ruby-delete-indentation 1)
      (indent-according-to-mode)
      (save-excursion
        (sp-backward-sexp) ; move to begining of current sexp
        (sp-backward-sexp arg)
        (sp-ruby-maybe-one-space)))

    (when (equal action 'barf-forward)
      (sp-get enc
        (let ((beg-line (line-number-at-pos :beg-in))
              (end-line (line-number-at-pos :end-in)))
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

(defun sp-sh-pre-handler (id action context)
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
            (when (looking-back "\.") (backward-char))
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
          (when (looking-back "\\.") (backward-char))
          (while (looking-back "::") (sp-backward-symbol))
          (if (= (line-number-at-pos) end-line)
              (insert " ")
            (if (looking-back "^[[:blank:]]*")
                (save-excursion (newline))
              (newline))))))))

(defun sp-backward-slurp-into-previous-sexp (&optional arg)
  "Add the sexp at point into the preceeding list."
  (interactive)
  (sp-backward-symbol)
  (sp-forward-slurp-sexp))

;; https://github.com/Fuco1/smartparens/issues/80
(defun sp-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (setq sp-base-key-bindings 'paredit)
    (setq sp-hybrid-kill-entire-symbol nil)
    ;; Enable some default keybindings for Smartparens.
    (sp-use-paredit-bindings)
    ;; Highlight matching delimiters.
    (show-smartparens-global-mode +1)
    ;; Disable Smartparens in Org-related modes, since the keybindings conflict.
    (with-eval-after-load 'org (add-to-list 'sp-ignore-modes-list #'org-mode))
    (with-eval-after-load 'org-agenda (add-to-list 'sp-ignore-modes-list #'org-agenda-mode))
    
    (sp-with-modes
        '(c-mode c++-mode css-mode javascript-mode js2-mode json-mode objc-mode
                 python-mode java-mode sh-mode web-mode)
      (sp-local-pair "{" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "[" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET"))))

    (sp-with-modes
        '(Python-Mode sh-mode)
      (sp-local-pair "(" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET")))
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
                     :post-handlers '(sp-sh-block-post-handler))))

  :hook
  ((conf-mode eshell-mode markdown-mode prog-mode text-mode) . turn-on-smartparens-mode)
  :bind
  ("C-M-(" . sp-backward-slurp-into-previous-sexp))

(use-package parinfer
  :custom
  (parinfer-extensions 
   '(defaults         ; should be included.
     pretty-parens    ; different paren styles for different modes.
     smart-tab        ; C-b & C-f jump positions and smart shift with tab & S-tab.
     smart-yank))     ; Yank behavior depends on mode.
  :config
  (parinfer-strategy-add 'default 'newline-and-indent)
  :hook
  ((clojure-mode common-lisp-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode) . parinfer-mode)
  :bind
  (:map parinfer-mode-map
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)
        ("C-i" . parinfer--reindent-sexp)
        ("C-M-i" . parinfer-auto-fix)
        ("C-," . parinfer-toggle-mode)
        ;; Don't interfere with smartparens quote handling
        ("\"" . nil)
        :map parinfer-region-mode-map
        ("C-i" . indent-for-tab-command)
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)

;; try to use GNU ls on macOS since BSD ls doesn't explicitly support Emacs
(setq insert-directory-program (or (executable-find "gls")
                                   (executable-find "ls"))
      ;; don't prompt to kill buffers of deleted directories
      dired-clean-confirm-killing-deleted-buffers nil)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(defun dired-to-default-directory ()
  "Open directory containing the current file."
  (interactive)
  (dired default-directory))

(defun dired-open-file ()
  "Open file at point in OS default program."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "open" nil 0 nil file)))

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-listing-switches "-alh"
      dired-dwim-target t)

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)))

(bind-keys
 ("C-x C-d" . dired-to-default-directory)
 ("C-x d" . dired)
 :map dired-mode-map
 ("C-c o" . dired-open-file)
 ("C-x f" . find-file-literally-at-point)
 ("T" . touch))

(use-package diredfl
  :config
  (diredfl-global-mode))

(use-package dired-subtree
  :bind
  ("C-, i" . dired-subtree-insert)
  ("C-, r" . dired-subtree-remove)
  ("C-, R" . dired-subtree-revert)
  ("C-, n" . dired-subtree-narrow)
  ("C-, ^" . dired-subtree-up)
  ("C-, v" . dired-subtree-down))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

(use-package dired-du
  :commands
  (dired-du-mode))

(use-package direx
  :bind
  (("C-x j" . direx:jump-to-directory)
   ("C-x C-j" . direx-project:jump-to-project-root)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell, SSH, Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The default is /bin/bash. On macOS, brew installs bash to /usr/local/bin.
(setq-default shell-file-name (executable-find "bash"))

(require 'tramp)

;; password-cache
(setq password-cache-expiry nil)

;; Configure TRAMP to respect the PATH variable on the remote machine (for
;; remote eshell sessions)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(defun list-hosts-from-known-hosts ()
  "Return a list of hosts from ~/.ssh/known_hosts"
  (with-temp-buffer
    (insert-file-contents "~/.ssh/known_hosts")
    (-remove (lambda (host) (string=  "" host))
             (mapcar (lambda (line) (replace-regexp-in-string "\\]\\|\\[" "" (car (split-string line "[, :]"))))
                     (split-string (buffer-string) "\n")))))

(defun list-hosts-from-ssh-config ()
  "Return a list of hosts from ~/.ssh/config"
  (with-temp-buffer
    (insert-file-contents "~/.ssh/config")
    (keep-lines "^Host")
    (-remove (lambda (host) (or (string=  "" host) (string= "*" host)))
             (mapcar (lambda (line) (replace-regexp-in-string "Host +" "" line))
                     (split-string (buffer-string) "\n")))))

(defun list-hosts-from-etc-hosts ()
  "Return a list of hosts from /etc/hosts"
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
  "return a list of hosts from the recentf-list"
  (-distinct
   (mapcar (lambda (s)
             (replace-regexp-in-string
              ":.*" "" 
              (replace-regexp-in-string "^/sshx\?:" "" s)))
           (-filter
            (apply-partially #'string-match "^/sshx\?:\\([a-z]+\\):")
            recentf-list))))

(defun ssh-choose-host ()
  "Make a list of recent ssh hosts and interactively choose one."
  (completing-read "SSH to Host: "
                   (-distinct
                    (append
                     (list-hosts-from-recentf)
                     (list-hosts-from-known-hosts)
                     (list-hosts-from-ssh-config)
                     (list-hosts-from-etc-hosts)))
                   nil t))

;; (defun ssh (host)
;;   "Choose an ssh HOST and then ssh to it."
;;   (interactive (list (ssh-choose-host)))
;;   (setq-local explicit-ssh-args (list "a"))
;;   (let ((explicit-shell-file-name (executable-find "ssh")))
;;     (shell "*ssh a*")))

(defun sshd (host)
  "Choose an ssh host and then open it with dired."
  (interactive (list (ssh-choose-host)))
  (find-file (concat "/sshx:" host ":")))

()

(use-package ssh
  :custom
  (ssh-directory-tracking-mode 'ftp)
  :hook
  (ssh-mode . (lambda ()
                (shell-dirtrack-mode t)
                (setq dirtrackp nil)))
  :commands
  (ssh))

(eval-after-load 'sh
  (lambda ()
    (bind-keys
     :map sh-mode-map
     ("s-<ret>" . eshell-send-current-line))))

;; http://whattheemacsd.com/setup-shell.el-01.html
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "`C-d' on an empty line in the shell terminates the process."
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
(setq explicit-dtach-args '("-A" "/tmp/emacs.dtach" "-z" "bash" "--noediting" "--login"))
(defun ssh-dtach (host)
  "Open SSH connection to remote host and attach to dtach session."
  (interactive "MSSH using dtach to host: ")
  (let ((explicit-shell-file-name "dtach")
        (default-directory (format  "/sshx:%s:" host)))
    (shell (format "*ssh %s*" host))))

;; Apply colors to `shell-command' minibuffer output.
;; Adapted from https://stackoverflow.com/a/42666026/1588358
(defun xterm-color-apply-on-minibuffer ()
  (let ((bufs (remove-if-not
               (lambda (x) (string-prefix-p " *Echo Area" (buffer-name x)))
               (buffer-list))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (xterm-color-colorize-buffer)))))

(defun xterm-color-apply-on-minibuffer-advice (proc &rest rest)
  (xterm-color-apply-on-minibuffer))

(advice-add 'shell-command :after #'xterm-color-apply-on-minibuffer-advice)

;; xterm colors
(use-package xterm-color
  :hook
  (shell-mode . (lambda ()
                  (add-hook 'comint-preoutput-filter-functions
                            'xterm-color-filter nil t))))

(use-package eterm-256color
  ;; `devel' branch is needed to support Emacs 27.
  ;; https://github.com/dieggsy/eterm-256color/pull/9#issuecomment-403229541
  :straight
  (:type git :host github :repo "dieggsy/eterm-256color" :branch "devel")
  :hook
  (term-mode . eterm-256color-mode))

(defun eshell-other-window (arg)
  "Opens an eshell in another window. Prefix argument opens a new eshell named for `default-directory'"
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
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(defun eshell-quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (progn
        (eshell-life-is-too-much)
        (ignore-errors
          (delete-window)))
    (delete-forward-char arg)))

(defun eshell-send-previous-input (&optional arg)
  "Re-run the previous command (or with prefix args the nth command) in last used eshell buffer"
  (interactive "*p")
  (with-current-buffer
      (-first (lambda (b) (eq 'eshell-mode (with-current-buffer b major-mode)))
              (buffer-list))
    (with-selected-window (get-buffer-window)
      (end-of-buffer)
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
      (end-of-buffer)
      (eshell-kill-input)
      (insert command)
      (eshell-send-input)
      (end-of-buffer)
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
   `(,(when (not (eshell-exit-success-p))
        `(,(number-to-string eshell-last-command-status)
          :background "red" :foreground "white" :weight bold))
     (,(abbreviate-file-name (eshell/pwd)) :background "cyan" :foreground "black")
     (,(if (zerop (user-uid)) "\n(#)" "\n()") :foreground "white" :weight bold))
   ""))

(defun eshell/s (hostname)
  "Change directory to host via tramp"
  (eshell/cd (concat "/ssh:" hostname ":")))

(defun eshell/e (&optional path)
  (find-file path))

(defun eshell/ee (&optional path)
  (find-file-other-window path))

(defun eshell-path-advice (f &optional directory)
  "Return a DIRECTORY using a path relative to the remote host (if we are on a
 remote host).
Examples:
  > cd /etc -> /sshx:host:/etc
  > cd : -> /sshx:host:/home/user"
  (funcall f
   (if (file-remote-p default-directory)
       (let ((remote-prefix (replace-regexp-in-string ":[^:]*$" ":" default-directory)))
         (cond
          ;; If `directory' starts with `$HOME' then we assume the original arg
          ;; started with "~" and we change directory to the remote `$HOME'. We
          ;; can always use `cd' with no args to return to local `$HOME'.
          ((string-prefix-p (getenv "HOME") directory)
           (concat remote-prefix
                   (replace-regexp-in-string (concat (getenv "HOME") "/?")
                                             ""
                                             directory)))
          ;; If `directory' starts with "/" then it's an absolute path but we
          ;; want to make it relative to the remote host, rather than localhost.
          ((string-prefix-p "/" directory)
           (concat remote-prefix directory))
          (t
           directory)))
     directory)))

(advice-add #'eshell/cd :around #'eshell-path-advice)
(advice-add #'eshell/e :around #'eshell-path-advice)
(advice-add #'eshell/ee :around #'eshell-path-advice)

(defun eshell/really-clear (f &rest args)
  "Call `eshell/clear' with an argument to really clear the buffer."
  (if args
      (apply f args)
    (funcall f 1)))

(advice-add #'eshell/clear :around #'eshell/really-clear)

(defun tramp-insert-remote-part ()
  "Insert current tramp prefix at point"
  (interactive)
  (if-let (remote (file-remote-p default-directory))
      (insert remote)))

(bind-key "C-c f" #'tramp-insert-remote-part)

(defun eshell/info (&optional subject)
  "Invoke `info', optionally opening the Info system to SUBJECT."
  (let ((buf (current-buffer)))
    (Info-directory)
    (if (not (null subject))
        (let ((node-exists (ignore-errors (Info-menu subject))))
          (if (not node-exists)
              (format "No menu item `%s' in node `(dir)Top'." subject))))))

(defun eshell-create-send (cmd &optional name)
  "Create an eshell buffer and run a command in it."
  (let ((eshell-buffer-name (or name cmd)))
    (eshell))
  (insert cmd)
  (eshell-queue-input))

(defun eshell-vertical-create-send (cmd &optional name)
  "Split the window vertically, create an eshell buffer there, and run a command"
  (split-window-vertically)
  (let ((eshell-buffer-name (or name cmd)))
    (eshell))
  (insert cmd)
  (eshell-queue-input))

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
  "Moves output of the previous command to a new buffer."
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

;; https://stackoverflow.com/a/14769115/1588358
(defun local-set-minor-mode-key (mode key def)
  "Overrides a minor mode keybinding for the local buffer, by
   creating or altering keymaps stored in buffer-local
   `minor-mode-overriding-map-alist'."
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
  "This function is called when Eshell starts in order to
initialize the Eshell environment."
  (source-sh "~/.env")
  (setq eshell-path-env (getenv "PATH"))
  (setenv "TERM" "eterm-color")
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
   ("C-l" . (lambda () (eshell/clear) (funcall eshell-prompt-function)))
   ("C-w" . eshell-kill-previous-output)
   ("C-M-w" . eshell-kill-previous-output-to-buffer)
   ("M-w" . eshell-copy-previous-output)
   ("s-v" . clipboard-yank)
   ("C-S-<backspace>" . eshell-kill-input)
   ("C-M-p" . eshell-previous-prompt)
   ("C-M-n" . eshell-next-prompt)
   ("C-h C-e" . esh-help-run-help))

  ;; xterm colors
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

  ;; Commands that use curses
  (add-to-list 'eshell-visual-commands "htop")
  (add-to-list 'eshell-visual-commands "mbsync")
  (add-to-list 'eshell-visual-commands "ncdu")
  (add-to-list 'eshell-visual-commands "nnn")
  (add-to-list 'eshell-visual-commands "nvim")
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "tail")
  (add-to-list 'eshell-visual-commands "tmux")
  (add-to-list 'eshell-visual-commands "top")
  (add-to-list 'eshell-visual-commands "vim")
  (add-to-list 'eshell-visual-commands "w3m")
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
  (add-to-list 'eshell-visual-subcommands '("dw" "log"))

  ;; Load the Eshell versions of `su' and `sudo'
  (require 'em-tramp)
  (add-to-list 'eshell-modules-list 'eshell-tramp))

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
   (eshell-before-prompt . (lambda ()
                             (setq xterm-color-preserve-properties t))))
  :bind
  (("s-e" . eshell)
   ("s-E" . eshell-other-window)
   :map prog-mode-map
   ("M-P" . eshell-send-previous-input)))

;; ElDoc and topical help in Eshell
(use-package esh-help
  :config
  (setup-esh-help-eldoc))

;; Fish-like autosuggestions
(use-package esh-autosuggest
  :hook
  (eshell-mode . esh-autosuggest-mode)
  :bind
  (:map esh-autosuggest-active-map
        ("C-e" . company-complete-selection)))

(use-package pinentry
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  ;; Don't know how to get pinentry to work with Windows. Maybe a TCP socket?
  (if (not (eq system-type 'windows-nt))
      (pinentry-start)))

(defun ssh-sudo (path)
  "ssh to host, sudo to root, open dired"
  (interactive "M [User@] Hostname: ")
  (let* ((at (string-match "@" path))
         (colon (string-match ":" path))
         (user (if at
                   (concat (substring path 0 at) "@")
                 ""))
         (host (if at
                   (substring path (+ 1 at))
                 path))
         (dir (if colon
                  (substring path (+ 1 colon))
                ":/")))
    (find-file (concat "/sshx:" user host "|sudo:root@" host dir))))

(defun sudo-toggle--add-sudo (f)
  "Add sudo to file path string"
  (if (file-remote-p f)
      (with-parsed-tramp-file-name (expand-file-name f) nil
        (concat "/" method ":"
                (when user (concat user "@"))
                host "|sudo:root@" host ":" localname))
    (concat "/sudo:root@localhost:" (expand-file-name f))))

(defun sudo-toggle--remove-sudo (f)
  "Remove sudo from file path string"
  (cond
   ((string-match-p "/sudo:root@localhost:" f)
    (replace-regexp-in-string (getenv "HOME") "~" (substring f 21)))

   ((string-match-p "|sudo:root@" f)
    (replace-regexp-in-string "|sudo:root@[^:]*" "" f))))

(defun sudo-toggle ()
  "Reopen the current file, directory, or shell as root.  For
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
           (insert (concat "cd " newf))
           (eshell-send-input))
          (t (message "Can't sudo this buffer")))))

;; Disable file accesses when visiting buffers accessed via tramp for performance reasons
(defun tramp-disable-file-accesses ()
  (when (file-remote-p default-directory)
    (setq-local projectile-mode-line "Projectile")
    (setq-local company-backends company-backends-remote)))

(use-package tramp
  :hook
  (find-file . tramp-disable-file-accesses))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mount  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These functions execute the `mnt' utility, which uses config
;; profiles to mount smb shares (even through ssh tunnels).

(defun mnt-cmd (cmd)
  "Interactively Run a `mnt/umnt' utility (CMD) with the
`config', the specified config file"
  (let ((config (completing-read (format "Run %s using config: " cmd)
                                 (directory-files "~/.mnt" nil "^[^.]")
                                 nil t)))
    (setq config (expand-file-name config "~/.mnt"))
    (if (async-shell-command (concat cmd " " config) "*mnt*")
        (message (format "%s succeeded with config file: %s" cmd config))
      (message (format "%s FAILED with config file: %s" cmd config)))))

(defun mnt ()
  (interactive)
  (mnt-cmd "sudo_mnt"))

(defun umnt ()
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

(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

;; Org-mode

;; Install Org from git
;; https://github.com/raxod502/straight.el#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(defun search-org-files ()
  "Search ~/org"
  (interactive)
  (counsel-rg nil "~/org"))

(use-package org
  :custom
  (org-directory "~/org")
  ;; Clean view
  (org-startup-indented t)
  ;; Smart C-a/e
  (org-special-ctrl-a/e t)
  ;; Smart C-k
  (org-special-ctrl-k t)
  ;; Insert a row in tables
  (org-special-ctrl-o t)
  ;; Customize todo keywords
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "|" "DONE(d!)")))
  (org-todo-keyword-faces '(("TODO" (:foreground "orange" :weight bold))
                            ("NEXT" (:foreground "red" :weight bold))
                            ("WIP" (:foreground "green" :weight bold))
                            ("DONE" (:foreground "gray"))))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switchb)
  ("C-c s" . search-org-files))

;; Calendar and Journal

(require 'calendar)

(defun calendar-iso8601-date-string (date)
  (destructuring-bind (month day year) date
    (concat (format "%4i" year)
            "-"
            (format "%02i" month)
            "-"
            (format "%02i" day))))

(defun calendar-date-add-days (date days)
  "Add DAYS to DATE"
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian (calendar-current-date))
      days)))

(defun calendar-choose-date ()
  "Interactively choose DATE and return it as an ISO 8601 string"
  (let* ((today (calendar-current-date))
         (day-offsets '(0 -1 -2 -3 -4 -5 -6 -7))
         (dates (mapcar (apply-partially #'calendar-date-add-days today) day-offsets))
         (date-strings (mapcar #'calendar-iso8601-date-string dates)))
    (completing-read "Date: " date-strings nil nil (substring (car date-strings) 0 7))))

(defun calendar-insert-date (date)
  "Interactively choose a DATE in ISO 8601 format and insert it at point"
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

(use-package nov
  :mode "\\.epub\\'")

(use-package pdf-tools
  :config
  (pdf-tools-install)
  :bind
  (:map pdf-view-mode-map
        ("s-f" . isearch-forward)))

;; Doesn't seem to work. Probably API changed?
;; (use-package org-wunderlist
;;   :bind
;;   ("C-c o w" . org-wunderlist-fetch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vlf
  :custom
  (vlf-application 'dont-ask)
  :config
  (require 'vlf-setup))

(use-package logview)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search, Completion, Symbols, Project Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package re-builder
  :custom
  ;; string syntax means you don't need to double escape things.
  (reb-re-syntax 'string))

(use-package wgrep
  :bind
  (("C-c C-p" . wgrep-change-to-wgrep-mode)))

;; some rg features require this
(use-package wgrep-ag)

(use-package rg
  :config
  (rg-enable-default-bindings (kbd "C-r"))
  :hook
  (rg-mode . wgrep-ag-setup))

(use-package counsel-etags
  :custom
  ;; TODO: Get this working with Clojure (ctags parses namespaces but
  ;; `counsel-etags-find-tag-at-point' doesn't. Wouldn't this be `clojure-mode's
  ;; responsibility? I'm pretty sure it keys off of sexp
  (tags-revert-without-query t)
  ;; Don't warn when TAGS files are large.
  (large-file-warning-threshold nil)
  :hook
  ;; Incrementally update TAGS file when the file is saved.
  (prog-mode . (lambda ()
                 (add-hook 'after-save-hook
                           'counsel-etags-virtual-update-tags 'append 'local)))
  :commands
  (counsel-etags-find-tag-at-point counsel-etags-scan-code counsel-etags-list-tag))

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
        ;; ("C-n" . company-select-next)
        ;; ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer)
        ("M-." . company-show-location)))

(use-package company-shell
  :config
  (let ((shell-backends '(company-shell company-shell-env)))
    (if (executable-find "fish")
        (add-to-list 'shell-backends 'company-fish-shell))
    (add-to-list 'company-backends shell-backends)))

(use-package pcre2el
  :hook
  ((emacs-lisp-mode lisp-interaction-mode reb-mode) . rxt-mode))

(use-package ivy
  :custom
  (enable-recursive-minibuffers t)
  ;; (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  :bind
  (("C-c C-r" . ivy-resume)
   ("s-b" . ivy-switch-buffer)
   ("s-B" . ivy-switch-buffer-other-window)
   :map ivy-switch-buffer-map
   ("C-k" . (lambda ()
              (interactive)
              (ivy-set-action 'kill-buffer)
              (ivy-done)
              (ivy-resume)))))

(use-package swiper
  :init
  (defun replace-regexp-entire-buffer (pattern replacement)
    "Perform regular-expression replacement throughout buffer."
    (interactive
     (let ((args (query-replace-read-args "Replace in entire buffer" t)))
       (setcdr (cdr args) nil)    ; remove third value returned from query---args
       args))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (replace-match replacement))))

  (defun ivy--replace-regexp-entire-buffer (replacement)
    (interactive (list (read-from-minibuffer (concat "Replace all occurrences of `" ivy--old-re "\' in entire buffer: "))))
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (replace-regexp-entire-buffer ivy--old-text replacement))
    (ivy-done))
  ;; This isn't the last message displayed so there isn't really a point in displaying it at all
  ;; (message (concat "Replaced `" ivy--old-text "\' with `" replacement"\' across entire buffer.")))
  :bind
  (("s-5" . replace-regexp-entire-buffer)))
;; :map ivy-minibuffer-map
;; ("s-5" . ivy--replace-regexp-entire-buffer)))

(use-package counsel
  :custom
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :bind
  (("s-F" . counsel-rg)
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
   ;; ([remap isearch-forward] . counsel-grep-or-swiper)
   ([remap find-file] . counsel-find-file)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line-and-call)))

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
  "https://github.com/jfeltz/projectile-load-settings/blob/master/projectile-load-settings.el
  Load project elisp settings file if they are found in active project root
  directory, or if in the case of undefined root directory, file is
  otherwise path resolvable."
  (interactive)
  (let ((p (expand-file-name (or file "settings.el") (projectile-project-root))))
    (when (file-exists-p p)
      (load p)
      (message "%s" (concat "Loaded project settings from: " p)))))

(use-package projectile
  :custom
  (projectile-completion-system 'ivy)
  (projectile-project-search-path '("~/code"))
  ;; We don't set a frame title because it renders the frame title face terribly.
  (frame-title-format nil)
  ;; (frame-title-format
  ;;  '(""
  ;;    (:eval
  ;;     (when (fboundp 'projectile-project-name)
  ;;       (let ((project-name (projectile-project-name)))
  ;;         (unless (string= "-" project-name)
  ;;           (format "[%s]" project-name)))))))
  :hook
  ((after-init . projectile-mode)
   (projectile-after-switch-project . projectile-load-settings)))

(use-package counsel-projectile
  :init
  (setq counsel-projectile-remove-current-buffer t
        counsel-projectile-remove-current-project t)
  :config
  (counsel-projectile-mode)
  :bind
  ("M-s-p" . counsel-projectile-switch-to-buffer)
  ("s-p" . counsel-projectile)
  ("s-P" . counsel-projectile-switch-project)
  ("s-t" . counsel-imenu)
  ("M-s-f" . counsel-projectile-rg))

(use-package imenu-anywhere
  :bind
  (("s-r" . ivy-imenu-anywhere)))

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg)
  :hook
  (prog-mode . dumb-jump-mode)
  ;; dumb-jump shadows some Eshell key bindings, and is not useful there anyway
  (eshell-mode . (lambda () (dumb-jump-mode -1)))
  :bind
  (("s-j" . dumb-jump-go-prompt)
   ("s-." . dumb-jump-go)
   ("s-<mouse-1>". dumb-jump-go)
   ("s-J" . dumb-jump-quick-look)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; git config files
(add-to-list 'auto-mode-alist '("\\.git\\(?:config\\|ignore\\).*" . conf-mode))

;; https://github.com/magit/magit/issues/460#issuecomment-36139308
(defun git-worktree-link (gitdir worktree)
  (require 'magit)
  (interactive (list (read-directory-name "Gitdir: ")
                     (read-directory-name "Worktree: ")))
  (with-temp-file (expand-file-name ".git" worktree)
    (insert "gitdir: " (file-relative-name gitdir worktree) "\n"))
  (magit-call-git "config" "-f" (expand-file-name "config" gitdir)
                  "core.worktree" (file-relative-name worktree gitdir))
  ;; Configure projectile to only look at tracked files
  (if (boundp 'projectile-git-command)
      (setq projectile-git-command "git ls-files -zc --exclude-standard"))
  (message "Linked worktree: %s from gitdir: %s" worktree gitdir))

(defun git-worktree-unlink (gitdir worktree)
  (interactive (list (read-directory-name "Gitdir: ")
                     (read-directory-name "Worktree: ")))
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
  (interactive (list (completing-read "Link git home repository: "
                                      (directory-files "~/.config/repos" nil "^[^.]")
                                      nil t)))
  (setq repo (expand-file-name repo git-home-repo-dir))
  ;; "Fix" repositories that were created with --bare.
  ;; (let ((default-directory (file-name-as-directory repo)))
  ;;   (magit-set "false" "core.bare"))
  ;; Regular link.
  (git-worktree-link repo (getenv "HOME")))

(defun git-home-unlink ()
  (interactive)
  (git-worktree-unlink (with-temp-buffer
                         (insert-file-contents (expand-file-name ".git" (getenv "HOME")))
                         (re-search-forward "gitdir: \\(.+\\)")
                         (expand-file-name (match-string 1)))
                       (getenv "HOME")))

(defun projectile-git-ls-files (&optional dir)
  "List all the tracked files in the current git repo, optionally
specified by DIR."
  (cd (or dir (projectile-project-root)))
  (-filter #'nil-blank-string
           (split-string (shell-command-to-string "git ls-files") "\n")))

(defun projectile-git-ls-files-dired (&optional dir)
  "Create a dired new buffer listing all the tracked files in the current
git repo, optionally specified by DIR."
  (interactive)
  (let ((dir (or dir (projectile-project-root))))
    (dired (cons dir (projectile-git-ls-files dir)))
    (rename-buffer (format "*git ls-files %s*" dir))))

(bind-key "C-x G" #'projectile-git-ls-files-dired)

(use-package magit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :bind
  (("C-x g" . magit-status)
   ("C-x C-g" . magit-dispatch-popup)))

(use-package git-timemachine
  :bind
  (("C-x t" . git-timemachine)))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/code"))

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

(use-package w3m
  :custom
  (w3m-search-default-engine "duckduckgo")
  :commands
  (w3m w3m-download w3m-goto-url w3m-search))

(straight-register-package
 '(vkill :repo "https://github.com/emacsattic/vkill.git"))

(use-package vkill
  :bind
  (("C-c t" . vkill)))

(use-package proc-net
  :bind
  (("C-c n" . list-network-processes)))

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
  "Display the local host's disk usage in human readable form"
  (interactive)
  (print (shell-command-to-string "df -h")))

(defun dis (hostname)
  "Resolve an IP address."
  (interactive "MHostname: ")
  (message (shell-command-to-string
            (concat "drill "
                    hostname
                    " | awk '/;; ANSWER SECTION:/{flag=1;next}/;;/{flag=0}flag'"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-keys :map emacs-lisp-mode-map
           ("s-<return>" . eval-last-sexp)
           ("C-c C-k" . eval-buffer)
           ("C-x e" . macrostep-expand)
           :map lisp-interaction-mode-map
           ("s-<return>" . eval-last-sexp)
           ("C-c C-k" . eval-buffer))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other File Modes and Formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; language detection

(defun eww-tag-pre (dom)
  (let ((shr-folding-mode 'none)
        (shr-current-font 'default))
    (shr-ensure-newline)
    (insert (eww-fontify-pre dom))
    (shr-ensure-newline)))

(defun eww-fontify-pre (dom)
  (with-temp-buffer
    (shr-generic dom)
    (let ((mode (eww-buffer-auto-detect-mode)))
      (when mode
        (eww-fontify-buffer mode)))
    (buffer-string)))

(defun eww-fontify-buffer (mode)
  (delay-mode-hooks (funcall mode))
  (font-lock-default-function mode)
  (font-lock-default-fontify-region (point-min)
                                    (point-max)
                                    nil))

(defun eww-buffer-auto-detect-mode ()
  (let* ((map '((ada ada-mode)
                (awk awk-mode)
                (c c-mode)
                (cpp c++-mode)
                (clojure clojure-mode lisp-mode)
                (csharp csharp-mode java-mode)
                (css css-mode)
                (dart dart-mode)
                (delphi delphi-mode)
                (emacslisp emacs-lisp-mode)
                (erlang erlang-mode)
                (fortran fortran-mode)
                (fsharp fsharp-mode)
                (go go-mode)
                (groovy groovy-mode)
                (haskell haskell-mode)
                (html html-mode)
                (java java-mode)
                (javascript javascript-mode)
                (json json-mode javascript-mode)
                (latex latex-mode)
                (lisp lisp-mode)
                (lua lua-mode)
                (matlab matlab-mode octave-mode)
                (objc objc-mode c-mode)
                (perl perl-mode)
                (php php-mode)
                (prolog prolog-mode)
                (python python-mode)
                (r r-mode)
                (ruby ruby-mode)
                (rust rust-mode)
                (scala scala-mode)
                (shell shell-script-mode)
                (smalltalk smalltalk-mode)
                (sql sql-mode)
                (swift swift-mode)
                (visualbasic visual-basic-mode)
                (xml sgml-mode)))
         (language (language-detection-string
                    (buffer-substring-no-properties (point-min) (point-max))))
         (modes (cdr (assoc language map)))
         (mode (cl-loop for mode in modes
                        when (fboundp mode)
                        return mode)))
    (message (format "%s" language))
    (when (fboundp mode)
      mode)))

;; Configure eww to detect and render code snippets embedded in html
(setq shr-external-rendering-functions
      '((pre . eww-tag-pre)))

(use-package language-detection
  :commands
  (language-detection-buffer language-detection-string))

;; display nfo files in all their glory
;; https://github.com/wasamasa/dotemacs/blob/master/init.org#display-nfo-files-with-appropriate-code-page)
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

;; systemd
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))

;; (use-package quickrun
;;   :commands
;;   (quickrun quickrun-region quickrun-shell quickrun-autorun-mode))

(use-package goto-chg
  :bind
  (("C-." . goto-last-change)
   ("C-;" . goto-last-change-reverse)))

(use-package sly
  :custom
  (inferior-lisp-program (executable-find "sbcl"))
  :bind
  (:map sly-prefix-map
        ("M-h" . sly-documentation-lookup)))

(use-package sly-company
  :hook
  (sly-mode . sly-company-mode)
  :config
  (add-to-list 'company-backends 'sly-company))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :bind
  (("C-c C-s" . yas-insert-snippet)))

(use-package yasnippet-snippets
  :defer 2)

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
  (clojure-mode . turn-on-eldoc-mode)
  (clojurescript-mode . turn-on-eldoc-mode))

(use-package clojure-mode-extra-font-locking
  :defer 1)

(use-package dash-at-point
  :config
  (progn
    (add-to-list 'dash-at-point-mode-alist '(clojure-mode . "clojuredocs"))
    (add-to-list 'dash-at-point-mode-alist '(clojurec-mode . "clojuredocs"))
    (add-to-list 'dash-at-point-mode-alist '(clojurescript-mode . "clojuredocs"))
    (add-to-list 'dash-at-point-mode-alist '(sh-mode . "bash"))
    (add-to-list 'dash-at-point-mode-alist '(fish-mode . "fish"))
    (add-to-list 'dash-at-point-mode-alist '(lisp-mode . "lisp"))
    (add-to-list 'dash-at-point-mode-alist '(slime-repl-mode . "lisp"))
    (add-to-list 'dash-at-point-mode-alist '(lisp-interaction-mode . "elisp"))
    (add-to-list 'dash-at-point-mode-alist '(inferior-emacs-lisp-mode . "elisp")))
  :bind
  ("M-s-." . dash-at-point))

(use-package visual-regexp-steroids
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)
   ("C-c m" . vr/mc-mark)))

(use-package elisp-format
  :commands
  (elisp-format-buffer elisp-format-file elisp-format-region))

(use-package cider
  :config
  (add-hook 'cider-repl-mode-hook (lambda () (company-mode nil)))
  :bind
  (:map cider-mode-map
        ("s-<return>" . cider-eval-last-sexp)))

(use-package inf-clojure
  :config
  (defun inf-clojure-start-lumo ()
    (interactive)
    (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
    (inf-clojure-minor-mode)
    (inf-clojure "lumo -d"))
  (defun reinstate-comint-simple-send ()
    (unless inf-clojure-minor-mode
      (setq-local comint-input-sender 'comint-simple-send)))
  :hook
  ;; `inf-clojure' clobbers `comint-send-input' on all comint buffers, not just
  ;; `inf-clojure-mode' ones.
  ;; https://github.com/clojure-emacs/inf-clojure/issues/154
  (comint-mode . reinstate-comint-simple-send)
  :bind
  (:map inf-clojure-minor-mode-map
        ("s-<return>" . inf-clojure-eval-last-sexp)
        ("C-c C-k" . inf-clojure-eval-buffer)))

;; Configured to use CHICKEN Scheme
(use-package geiser
  :custom
  (geiser-default-implementation 'chicken)
  (geiser-mode-eval-last-sexp-to-buffer t)
  (scheme-program-name "csi -:c")
  :config
  (setq-default geiser-scheme-implementation 'chicken)

  ;; Indenting module body code at column 0
  (defun scheme-module-indent (state indent-point normal-indent) 0)
  (put 'module 'scheme-indent-function 'scheme-module-indent)
  (put 'and-let* 'scheme-indent-function 1)
  (put 'parameterize 'scheme-indent-function 1)
  (put 'handle-exceptions 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1)
  (put 'unless 'scheme-indenfunction 1)
  (put 'match 'scheme-indent-function 1)
  :commands
  (geiser run-geiser run-chicken))

(use-package rainbow-mode
  :hook
  ((sass-mode emacs-lisp-mode) . rainbow-mode))

(use-package highlight-escape-sequences
  :commands
  (hes-mode turn-on-hes-mode)
  :config
  (add-to-list 'hes-mode-alist `(clojurescript-mode . ,hes-js-escape-sequence-re))
  (turn-on-hes-mode))

(use-package hl-todo
  :commands
  (global-hl-todo-mode)
  :config
  (global-hl-todo-mode))

(use-package markdown-mode
  :mode "\\.md\\|markdown\\'"
  :custom
  (markdown-command "multimarkdown"))

(use-package htmlize
  :bind
  (("C-c h b" . htmlize-buffer)
   ("C-c h f" . htmlize-file)
   ("C-c h m" . htmlize-many-files)
   :map dired-mode-map
   ("C-c h m" . htmlize-many-files-dired)))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker
  :bind
  ("C-c d" . docker))

(use-package docker-tramp
  :defer 2)

(use-package bash-completion
  :init
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
  :commands bash-completion-dynamic-complete)

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package fish-mode
  :custom (fish-indent-offset tab-width)
  :mode "\\.fish\\'")

;; (use-package fish-completion
;;   :custom
;;   (fish-completion-fallback-on-bash-p t)
;;   :config
;;   (when (and (executable-find "fish")
;;              (require 'fish-completion nil t))
;;     (global-fish-completion-mode)))

(use-package nginx-mode
  :defer 2
  :custom
  (nginx-indent-level tab-width))

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
  (defun sp-web-mode-is-code-context (id action context)
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
  (web-mode-markup-indent-offset tab-width)
  (web-mode-css-indent-offset tab-width)
  (web-mode-code-indent-offset tab-width)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                               ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  :hook
  (web-mode . m-web-mode-hook))

(use-package js2-mode
  :mode "\\.js\\'"
  :custom
  (js2-basic-offset tab-width)
  ;; Set tab width for js-mode and json-mode
  (js-indent-level tab-width)
  :hook
  (js2-mode . js2-imenu-extras-mode))

(use-package rjsx-mode
  :mode "\\.jsx\\'")

;; (use-package indium
;;   :defer 1)

;; (use-package nodejs-repl
;;   :bind
;;   (:map
;;    js2-mode-map
;;    ("C-x C-e" . nodejs-repl-send-last-expression)
;;    ("C-c C-j" . nodejs-repl-send-line)
;;    ("C-c C-r" . nodejs-repl-send-region)
;;    ("C-c C-l" . nodejs-repl-load-file)
;;    ("C-c C-z" . nodejs-repl-switch-to-repl)))

(use-package know-your-http-well
  :defer 2)

(use-package restclient
  :defer 2)

(use-package company-restclient
  :defer 2
  :config
  (add-to-list 'company-backends 'company-restclient))

;; (use-package robe
;;   :hook ruby-mode
;;   :config
;;   (eval-after-load 'company
;;     '(push 'company-robe company-backends)))

;; (use-package inf-ruby
;;   :defer 1)

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package sass-mode
  :mode "\\.sa?c?ss\\'")

(use-package flycheck
  :custom
  (sentence-end-double-space nil)
  :bind
  (("C-c ! !" . flycheck-mode)))

(use-package powershell-mode
  :mode "\\.ps1\\'"
  :custom
  (powershell-indent tab-width)
  (powershell-continuation-indent tab-width))

(use-package ios-config-mode
  :mode "\\.cfg\\'")

;; `eval-in-repl' requires a number of other packages so it's best to load it last
(use-package eval-in-repl
  :bind
  (:map
   emacs-lisp-mode-map
   ("C-<return>" . eir-eval-in-ielm)
   :map
   lisp-interaction-mode-map
   ("C-<return>" . eir-eval-in-ielm)
   :map
   Info-mode-map
   ("s-<return>" . eir-eval-in-ielm)
   :map
   python-mode-map
   ("s-<return>" . eir-eval-in-python)
   :map
   ruby-mode-map
   ("s-<return>" . eir-eval-in-ruby)
   :map
   sh-mode-map
   ("s-<return>" . eir-eval-in-shell)))

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
  :commands (wttrin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'highlight-things)

(require 'm-pointhistory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load private stuff
(let ((f "~/.emacs.d/elisp/private.el"))
  (when (file-exists-p f)
    (load-file f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
