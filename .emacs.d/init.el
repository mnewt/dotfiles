;;; init.el --- Emacs init file --- -*- lexical-binding: t -*-

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

Do not merge packages listed in `m-pinned-packages'."
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

(defun update-packages ()
  "Use straight.el to update all packages."
  (interactive)
  (straight-normalize-all)
  (straight-fetch-all)
  (m-straight-merge-all))

(use-package epkg
  :commands
  (epkg epkg-describe-package epkg-list-packages))

(use-package use-package-ensure-system-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cl is assumed to be present in this config and some packages too.
(require 'cl)

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

;; Function composition
(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(defsubst compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))

;; Anonymous function macro
;; TODO: Doesn't work inside `use-package'
;; https://gist.github.com/alphapapa/f9e4dceaada6c90c613cd83bdc9a2300
(defmacro $ (&rest body)
  (cl-labels ((collect-vars
               (&rest forms)
               (cl-loop for form in forms
                        append (cl-loop for atom in form
                                        if (and (symbolp atom)
                                                (string-match (rx bos "$")
                                                              (symbol-name atom)))
                                        collect atom
                                        else if (consp form)
                                        append (collect-vars atom)))))
    `(lambda ,(cl-sort (collect-vars body)
                       #'string<
                       :key #'symbol-name)
       ,@body)))

(defun add-multiple-to-list (list items)
  "Run `add-to-list' on each ITEM in the LIST"
  (seq-do (curry #'add-to-list list) items))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Path
(setq set-path-unix '()
      set-path-windows '("C:/bin"
                         "C:/Program Files/Emacs/bin")
      set-path-user '())

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
    (message "New path: %s" new-path)
    (setq exec-path new-path)))

(source-sh "~/.env")
(source-sh "~/.bin/start-ssh-agent")
(set-path)

(setq m-code-directory "~/code")
(if (not (file-directory-p m-code-directory))
    (make-directory m-code-directory))

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

;; We don't set a frame title because Emacs on macOS renders the frame title
;; face terribly. 
(setq frame-title-format nil)

;; Default frame settings. This is actually maximized, but not full screen.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(cursor-color . "#F60"))

;; eww uses this as its default font, among others.
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
  "Forward to `load-theme'. Usable with `ivy-resume',
`ivy-next-line-and-call' and `ivy-previous-line-and-call'."
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
    (set-background-color "#282828")
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
            (m-active4 :background "#00E5E5" :foreground "#262834")
            (default :foreground "#60767E")))
    (set-mouse-color "black"))
  (defun activate-theme-solarized-dark ()
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
    (set-mouse-color "white"))
  (add-to-list 'm-themes '(solarized-light . activate-theme-solarized-light))
  (add-to-list 'm-themes '(solarized-dark . activate-theme-solarized-dark)))

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
      ; Show keystrokes right away, don't show the message in the scratch buffer
      echo-keystrokes 0.1)

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
(global-auto-revert-mode 1)

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
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

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
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

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

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (beginning-of-line)
  (set-mark (line-end-position)))

;; Key bindings to make moving between Emacs and other appliations a bit less
;; jarring. These are mostly based on macOS defaults but many work on Windows
;; and Linux. They can be overridden by the OS specific configurations below.
(bind-keys
 ("s-o" . find-file)
 ("s-O" . find-file-other-window)
 ("C-c C-f" . find-file-at-point)
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
 ("s-H" . ns-do-hide-others))

(defun config-unix ()
  "Configure Emacs for common Unix (Linux and macOS) settings."
  ;; The default for unix is /bin/bash but on macOS, brew installs bash to /usr/local/bin.
  (setq-default shell-file-name (executable-find "bash")
                explicit-shell-file-name shell-file-name))

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

(defun config-windows ()
  "Configure Emacs for Windows."
  (menu-bar-mode -1)
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super
        directory-free-space-program nil)
  (set-face-font 'default "Lucida Console-12")

  (defun os-open-file (file)
    (interactive)
    (message "Opening %s..." file)
    (call-process "explorer" nil 0 nil file))

  (defun reveal-in-windows-explorer ()
    "Reveal the current file in Windows Explorer."
    (interactive)
    (os-open-file (concat "/select," (dired-replace-in-string "/" "\\" buffer-file-name))))

  (bind-key "C-c i" #'reveal-in-windows-explorer))
             
;; OS specific configuration
(pcase system-type
  ('darwin (config-macos))
  ('gnu/linux (config-linux))
  ('windows-nt (config-windows))
  ('cygwin (config-windows)))

(use-package goto-addr
  :hook
  ((compilation-mode . goto-address-mode)
   (prog-mode . goto-address-prog-mode)
   (eshell-mode . goto-address-mode)
   (shell-mode . goto-address-mode))
  :bind
  (:map goto-address-highlight-keymap
        ("C-c C-o" . goto-address-at-point))
  :commands
  (goto-address-prog-mode
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
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; Desktop
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq suggest-key-bindings 5
      apropos-do-all t
      ;; Select help window so it's easy to quit it with `q'
      help-window-select t)

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
  (("C-h ." . helpful-at-point)
   ("C-h f" . helpful-callable)
   ("C-h c" . helpful-command)
   ("C-h F" . helpful-function)
   ("C-h k" . helpful-key)
   ("C-h M" . helpful-macro)
   ("C-h M-s" . helpful-symbol)
   ("C-h v" . helpful-variable)))

(use-package which-key
  :demand t
  :config
  (which-key-mode t)
  :bind
  (("M-s-h" . which-key-show-top-level)))

(use-package man
  :config
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

(use-package tldr
  :ensure-system-package t
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

(use-package dash-at-point
  :if (eq system-type 'darwin)
  :ensure-system-package
  ("/Applications/Dash.app" . "brew cask install dash")
  :config
  (seq-do (curry #'add-to-list 'dash-at-point-mode-alist)
          '((clojure-mode . "clojuredocs")
            (clojurec-mode . "clojuredocs")
            (clojurescript-mode . "clojuredocs")
            (fish-mode . "fish")
            (inferior-emacs-lisp-mode . "elisp")
            (lisp-mode . "lisp")
            (lisp-interaction-mode . "elisp")
            (lua-mode . "lua")
            (sh-mode . "bash")
            (slime-repl-mode . "lisp")))
  :bind
  ("M-s-." . dash-at-point))

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
           ("C-c ," . pop-to-mark-command)
           ("s-," . pop-global-mark)
           ("C-c C-," . pop-global-mark))

;; Quick switch buffers
(bind-keys ("s-}" . next-buffer)
           ("C-c }" . next-buffer)
           ("s-{" . previous-buffer)
           ("C-c {" . previous-buffer))

;; scratch
(setq initial-scratch-message nil
      initial-major-mode 'org-mode)

(defun new-scratch-buffer ()
  "Create or go to a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "<untitled>"))
  (setq buffer-file-name "untitled")
  (org-mode))

(bind-keys ("s-n" . new-scratch-buffer)
           ("C-c C-n . new-scratch-buffer"))

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
(bind-keys ("s-R" . #'xref-find-definitions-other-window)
           ("C-c M-r" . #'xref-find-definitions-other-window))

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

;; Work with auto-save and backup files
(use-package backup-walker
  :commands
  (backup-walker-start))

(use-package backups-mode
  :config
  (backups-minor-mode))

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
(setq sh-basic-offset tab-width
      sh-indentation tab-width
      ;; Tell `executable-set-magic' to insert #!/usr/bin/env interpreter
      executable-prefix-env t)

;; Make a shell script executable automatically on save
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(defun maybe-reset-major-mode ()
  "Reset the buffer's major-mode if a different mode seems like a better fit.
Mostly useful as a before-save-hook, to guess mode when saving a
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

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "Buffer indented."))

;; Guess the indentation of the file and continue to use that.
;; (use-package dtrt-indent
;;   :hook
;;   ((prog-mode text-mode) . dtrt-indent-mode))

;; http://whattheemacsd.com/key-bindings.el-03.html
(defun join-line-previous ()
  (interactive)
  (join-line -1))

;; It's the reverse of `delete-indentation'.
;; (bind-key "C-^" #'join-line-previous)

(defun dos2unix ()
  "Convert a DOS formatted buffer to Unix by removing the ^M at
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

;; (defun dos2unix () "Convert Dos encoded buffer to Unix encoding.
;;   https://edivad.wordpress.com/2007/04/03/emacs-convert-dos-to-unix-and-vice-versa/"
;;   (interactive) (set-buffer-file-coding-system 'unix))

(defun unix2dos ()
  "Convert Unix encoded buffer to DOS encoding.
https://edivad.wordpress.com/2007/04/03/emacs-convert-dos-to-unix-and-vice-versa/"
  (interactive)
  (set-buffer-file-coding-system 'dos))

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
  (emacs-lisp-mode . ($ (elisp-slime-nav-mode t))))

;; (use-package undo-tree
;;   :init
;;   ;; Keep region when undoing in region.
;;   ;; http://whattheemacsd.com/my-misc.el-02.html
;;   (defadvice undo-tree-undo (around keep-region activate)
;;     (if (use-region-p)
;;         (let ((m (set-marker (make-marker) (mark)))
;;               (p (set-marker (make-marker) (point))))
;;           ad-do-it
;;           (goto-char p)
;;           (set-mark m)
;;           (set-marker p nil)
;;           (set-marker m nil))
;;       ad-do-it))
;;   :custom
;;   (undo-tree-auto-save-history t)
;;   (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
;;   (undo-tree-visualizer-timestamps t)
;;   (undo-tree-visualizer-diff t)
;;   :config
;;   (global-undo-tree-mode)
;;   :bind
;;   (("s-z" . undo-tree-undo)
;;    ("s-Z" . undo-tree-redo)
;;    ("s-y" . undo-tree-redo)
;;    ("M-s-z" . undo-tree-visualize)))

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
   ("C-=" . er/expand-region)
   ("s-D" . er/contract-region)
   ("C-M-=" . er/contract-region)))

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
  ((prog-mode . outline-minor-mode)
   (outline-minor-mode . outshine-hook-function))
  :commands
  (outshine-hook-function)
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

(use-package goto-chg
  :bind
  (("C-." . goto-last-change)
   ("C-;" . goto-last-change-reverse)))

(use-package visual-regexp-steroids
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)
   ("C-c m" . vr/mc-mark)))

(use-package rainbow-mode
  :hook
  ((sass-mode emacs-lisp-mode) . rainbow-mode))

(use-package hl-todo
  :commands
  (global-hl-todo-mode)
  :config
  (global-hl-todo-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :bind
  (("C-c C-s" . yas-insert-snippet)))

(use-package yasnippet-snippets
  :defer 2)

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
  (save-excursion
    (sp-down-sexp)
    (sp-backward-symbol)
    (sp-forward-slurp-sexp)))

;; See https://github.com/Fuco1/smartparens/issues/80
(defun sp-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
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
  ;; (sp-with-modes
  ;;     '(c-mode c++-mode css-mode javascript-mode js2-mode json-mode objc-mode
  ;;              python-mode java-mode sh-mode web-mode)
  ;;   (sp-local-pair "{" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET")))
  ;;   (sp-local-pair "[" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET")))
  ;;   (sp-local-pair "(" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET"))))
  ;; (sp-with-modes
  ;;     '(python-mode)
  ;;   (sp-local-pair "\"\"\"" "\"\"\""
  ;;                  :post-handlers '((sp-create-newline-and-enter-sexp "RET"))))
  ;; (sp-with-modes
  ;;     '(sh-mode)
  ;;   (sp-local-pair "do" "done"
  ;;                  :when '(("SPC" "RET"))
  ;;                  :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
  ;;                  :actions '(insert navigate)
  ;;                  :pre-handlers '(sp-sh-pre-handler)
  ;;                  :post-handlers '(sp-sh-block-post-handler))
  ;;   (sp-local-pair "then" "fi"
  ;;                  :when '(("SPC" "RET"))
  ;;                  :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
  ;;                  :actions '(insert navigate)
  ;;                  :pre-handlers '(sp-sh-pre-handler)
  ;;                  :post-handlers '(sp-sh-block-post-handler))
  ;;   (sp-local-pair "case" "esac"
  ;;                  :when '(("SPC" "RET"))
  ;;                  :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
  ;;                  :actions '(insert navigate)
  ;;                  :pre-handlers '(sp-sh-pre-handler)
  ;;                  :post-handlers '(sp-sh-block-post-handler)))
  :hook
  (smartparens-mode . (lambda ()
                        (require 'smartparens-config)
                        (sp-use-paredit-bindings)
                        (turn-on-show-smartparens-mode)))
  ((emacs-lisp-mode hy-mode sh-mode) . turn-on-smartparens-mode)
  (clojure-mode . (lambda () (require 'smartparens-clojure)
                    (turn-on-smartparens-mode)))
  ((ruby-mode enh-ruby-mode) . (lambda () (require 'smartparens-ruby)
                                 (turn-on-smartparens-mode)))
  ((javascript-mode js2-mode) . (lambda () (require 'smartparens-javascript)
                                  (turn-on-smartparens-mode)))
  (lua-mode . (lambda () (require 'smartparens-lua)
                (turn-on-smartparens-mode)))
  (markdown-mode . (lambda () (require 'smartparens-markdown)
                     (turn-on-smartparens-mode)))
  (org-mode . (lambda () (require 'smartparens-org)
                (turn-on-smartparens-mode)))
  ((python-mode elpy-mode) . (lambda () (require 'smartparens-python)
                               (turn-on-smartparens-mode)))
  (text-mode . (lambda () (require 'smartparens-text)
                 (turn-on-smartparens-mode)))
  (web-mode . (lambda () (require 'smartparens-html)
                (turn-on-smartparens-mode)))
  :bind
  (:map smartparens-mode-map
        ;; Causes problems in `clojure-mode'
        ;; ("RET" . sp-newline)
        ("C-M-(" . sp-backward-slurp-into-previous-sexp)))

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
    (os-open-file file)))

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
  (("C-x C-j" . dired-sidebar-toggle-sidebar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell, SSH, Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package term
  :bind (("C-c t" . term)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))

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

(use-package eterm-256color
  ;; `devel' branch is needed to support Emacs 27.
  ;; https://github.com/dieggsy/eterm-256color/pull/9#issuecomment-403229541
  :straight
  (:type git :host github :repo "dieggsy/eterm-256color" :branch "devel")
  :hook
  (term-mode . eterm-256color-mode))

(use-package bash-completion
  :custom
  ;; So that it doesn't sometimes insert a space ('\ ') after the file name.
  (bash-completion-nospace t)
  :init
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
  :commands
  (bash-completion-dynamic-complete))

(use-package fish-mode
  :custom (fish-indent-offset tab-width)
  :mode "\\.fish\\'")

(use-package fish-completion
  :custom
  (fish-completion-fallback-on-bash-p t)
  :config
  (global-fish-completion-mode))

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

(defun eshell/d (&optional path)
  (dired path))

(defun eshell/do (&optional path)
  (dired-other-window path))

(defun eshell-path-advice (f &rest paths)
  "For each element in PATHS, return path relative to the remote
 host, if element starts with `:' andwe are on a remote host).

Examples: > cd /etc -> /etc > cd :/etc -> /sshx:host:/etc > cd :
-> /sshx:host:/home/user"
  (apply f
         (loop for path in (-flatten paths) collect
               (if-let* ((remote (and (string-prefix-p ":" path)
                                      (file-remote-p path))))
                   (concat remote (substring path 1))
                 path))))

;; Advise functions to work with ":" path syntax (see `eshell-path-advice').
(seq-do (lambda (f) (advice-add f :around #'eshell-path-advice))
        '(eshell/cd eshell/cp eshell/mv eshell/rm eshell/e eshell/ee eshell/d
                    eshell/do))

(defun tramp-insert-remote-part ()
  "Insert current tramp prefix at point"
  (interactive)
  (if-let* ((remote (file-remote-p default-directory)))
      (insert remote)))

(bind-key "C-c f" #'tramp-insert-remote-part)

(defun eshell/really-clear (&rest args)
  "Call `eshell/clear' with an argument to really clear the
buffer, then a second time to print the prompt."
  (interactive)
  (eshell/clear 1)
  (eshell/clear))

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
  ;; Path to shell executable. Set it this way to work with tramp.
  (setenv "ESHELL" "/bin/bash")
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
  (seq-do (curry #'add-to-list 'eshell-visual-commands)
          '("htop" "mbsync" "ncdu" "nnn" "nvim" "ssh" "tail" "tmux" "top" "vim" "w3m"))
  (seq-do (curry #'add-to-list 'eshell-visual-subcommands)
          '(("git" "log" "diff" "show")
            ("dw" "log" "runshell" "shell")))

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
   ;; For using xterm-256color properties in the prompt
   (eshell-before-prompt . (lambda ()
                             (setq xterm-color-preserve-properties t))))
  :bind
  (("s-e" . eshell)
   ("C-c e" . eshell)
   ("s-E" . eshell-other-window)
   ("C-c E" . eshell-other-window)
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
  (if (not (eq system-type 'windows-nt))
      (pinentry-start)))

(defun sshd-sudo (path)
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
           (insert (concat "cd '" newf "'"))
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

;; visual-line-mode
;; Don't shadow mwim and org-mode bindings
(bind-key [remap move-beginning-of-line] nil visual-line-mode-map)
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

(defun org-todo-todo ()
  "Create or update Org todo entry to TODO status"
  (interactive)
  (org-todo "TODO"))

(defun org-todo-to-int (todo)
  (first (-non-nil
          (mapcar (lambda (keywords)
                    (let ((todo-seq
                           (-map (lambda (x) (first (split-string  x "(")))
                                 (rest keywords)))) 
                      (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                  org-todo-keywords))))

(defun org-sort-entries--todo-status-key ()
  (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
         (todo (org-entry-get (point) "TODO"))
         (todo-int (if todo (org-todo-to-int todo) todo-max))
         (priority (org-entry-get (point) "PRIORITY"))
         (priority-int (if priority (string-to-char priority) org-default-priority)))
    (format "%03d %03d" todo-int priority-int)))

(defun org-sort-entries-by-todo-status ()
  (interactive)
  (org-sort-entries nil ?f #'org-sort-entries--todo-status-key))

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
  ;; Tab in source blocks should act like in major mode
  (org-src-tab-acts-natively t)
  ;; Code highlighting in code blocks
  (org-src-fontify-natively t)
  ;; Customize todo keywords
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "|" "DONE(d!)")))
  (org-todo-keyword-faces '(("TODO" (:foreground "orange" :weight bold))
                            ("NEXT" (:foreground "red" :weight bold))
                            ("WIP" (:foreground "green" :weight bold))
                            ("DONE" (:foreground "gray"))))
  (org-agenda-files '("~/org" "~/TODO.org"))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switchb)
  ("C-c s" . search-org-files)
  ("s-;" . org-shiftright))

(use-package org-preview-html
  :commands
  (org-preview-html-mode))

(use-package ox-hugo
  :after ox
  :config
  (require 'ox-hugo-auto-export))

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
  :mode ("\\.epub\\'" . nov-mode))

(defun hide-mode-line ()
  "Hide the mode line."
  (interactive)
  (setq mode-line-format-backup mode-line-format)
  (setq-default mode-line-format nil))

(defun show-mode-line ()
  "Show the mode line."
  (interactive)
  (setq-default mode-line-format mode-line-format-backup))

(use-package darkroom-mode
  :commands
  (darkroom-mode darkroom-tentative-mode))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
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

(defun tail-file (file)
  "Run `tail -f' on FILE. Tries to find a file at point."
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

(defhydra hydra-straight-helper (:hint nil)
  "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |pru_n_e build"
  ("c" straight-check-all)
  ("C" straight-check-package)
  ("r" straight-rebuild-all)
  ("R" straight-rebuild-package)
  ("f" straight-fetch-all)
  ("F" straight-fetch-package)
  ("p" straight-pull-all)
  ("P" straight-pull-package)
  ("m" straight-merge-all)
  ("M" straight-merge-package)
  ("n" straight-normalize-all)
  ("N" straight-normalize-package)
  ("u" straight-push-all)
  ("U" straight-push-package)
  ("v" straight-freeze-versions)
  ("V" straight-thaw-versions)
  ("w" straight-watcher-start)
  ("W" straight-watcher-quit)
  ("g" straight-get-recipe)
  ("n" straight-prune-build)
  ("q" nil))

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-window ()
  "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_v_ertical    	_b_uffer		_q_ X←
_j_ ↓        	_x_ horizontal	_f_ind files	_w_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1		_e_ X↑
_l_ →        	_Z_ reset      	_s_wap		_r_ X→
_F_ollow		_D_lt Other   	_S_ave		max_i_mize
_SPC_ cancel	_o_nly this   	_d_elete	
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("q" hydra-move-splitter-left)
  ("w" hydra-move-splitter-down)
  ("e" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)
  ("b" helm-mini)
  ("f" helm-find-files)
  ("F" follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
   
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
   
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
   
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
   
  ("o" delete-other-windows)
  ("i" ace-maximize-window)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
   
  ("Z" winner-redo)
  ("SPC" nil))
  

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
  ("q" hide-sublevels)    ; Hide everything but the top-level headings
  ("t" hide-body)         ; Hide everything but headings (all body lines)
  ("o" hide-other)        ; Hide other branches
  ("c" hide-entry)        ; Hide this entry's body
  ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" show-all)          ; Show (expand) everything
  ("e" show-entry)        ; Show this heading's body
  ("i" show-children)     ; Show this heading's immediate child sub-headings
  ("k" show-branches)     ; Show all sub-headings under this heading
  ("s" show-subtree)      ; Show (expand) everything in this heading & below
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
  "Call `occur' with a sane default, chosen as the thing under point or selected region"
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

;; Keeps focus on *Occur* window, even when when target is visited via RETURN key.
;; See hydra-occur-dwim for more options.
(defadvice occur-mode-goto-occurrence (after occur-mode-goto-occurrence-advice activate)
  (other-window 1)
  (hydra-occur-dwim/body))

;; Focus on *Occur* window right away.
(add-hook 'occur-hook (lambda () (other-window 1)))

(defun reattach-occur ()
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
  ("\\" dired-do-ispell)
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
  ("r" dired-do-rsynch)
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

(bind-keys ("C-c C-h" . hydra-straight-helper/body)
           ("C-c C-w" . hydra-window/body)
           ("C-c C-m" . hydra-multiple-cursors/body)
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
  :bind
  (("C-c C-p" . wgrep-change-to-wgrep-mode)))

(use-package rg
  :ensure-system-package
  (rg . ripgrep)
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
        ;; * TODO: The inconsistency between C-n and M-n to select company
        ;; completion in different contexts (e.g `emacs-lisp-mode' and
        ;; `eshell-mode') is aggravating. Not sure about the solution though.
        ;; ("C-n" . company-select-next) ("C-p" . company-select-previous)
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
  :bind ("C-c C-r" . ivy-resume)
        ("s-b" . ivy-switch-buffer)
        ("s-B" . ivy-switch-buffer-other-window))

(use-package ivy-hydra
  :defer 1)

(defun replace-regexp-entire-buffer (pattern replacement)
  "Perform regular-expression replacement immedately throughout the buffer."
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
  (ivy-done)
  ;; * TODO: How to make this the last message displayed?
  (message (concat "Replaced `" ivy--old-text "\' with `" replacement"\' across entire buffer.")))

(use-package swiper
  :bind
  (("s-5" . replace-regexp-entire-buffer)
   :map ivy-minibuffer-map
   ("s-5" . ivy--replace-regexp-entire-buffer)))

(defun reloading (cmd)
  (lambda (x)
    (funcall cmd x)
    (ivy--reset-state ivy-last)))

(defun given-file (cmd prompt) ; needs lexical-binding
  (lambda (source)
    (let ((target
           (let ((enable-recursive-minibuffers t))
             (read-file-name
              (format "%s %s to:" prompt source)))))
      (funcall cmd source target 1))))

(defun confirm-delete-file (x)
  (dired-delete-file x 'confirm-each-subdirectory))

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
  :bind
  (("C-h C-b" . counsel-descbinds)
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
   ;; ([remap isearch-forward] . counsel-grep-or-swiper)
   ([remap find-file] . counsel-find-file)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line-and-call)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

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
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-completion-system 'ivy)
  (projectile-project-search-path '("~/code"))
  ;; Exclude untracked files because we use git workdirs in $HOME. Listing all
  ;; files takes too long.
  (projectile-git-command "git ls-files -zc --exclude-standard")
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

(defun dired-git-add ()
  "Run `git add' on the selected files in a dired buffer"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (dired-do-shell-command "git add" nil files)
    (message "Finished running git add on files: %s" files)))

(bind-key ";" #'dired-git-add dired-mode-map)

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

;; Magit dependencies. Unless these are included here, they don't get loaded.
;; Haven't investigated why.
(use-package graphql)
(use-package treepy)

(use-package magit
  :requires
  (graphql treepy)
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

(use-package eww
  :hook
  (eww-mode . (lambda () (text-scale-set 2))))

(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight)))

(use-package w3m
  :ensure-system-package t
  :custom
  (w3m-search-default-engine "duckduckgo")
  :commands
  (w3m w3m-download w3m-goto-url w3m-search))

(straight-register-package
 '(vkill :repo "https://github.com/emacsattic/vkill.git"))

(use-package vkill
  :bind
  (("C-c T" . vkill)))

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
           ("C-x C-r" . eval-region)
           :map lisp-interaction-mode-map
           ("s-<return>" . eval-last-sexp)
           ("C-c C-k" . eval-buffer)
           ("C-x C-r" . eval-region))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

(use-package elisp-format
  :commands
  (elisp-format-buffer elisp-format-file elisp-format-region))

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
  "`inf-clojure' clobbers `comint-send-input' on all comint
buffers, not just `inf-clojure-mode' ones. This function
reinstates default behavior. See:
https://github.com/clojure-emacs/inf-clojure/issues/154"
  (unless inf-clojure-minor-mode
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

(use-package sly
  ;; There are some problems building sly with straight.el in Windows
  :unless (eq system-type 'windows-nt)
  :custom
  (inferior-lisp-program (executable-find "sbcl"))
  :bind
  (:map sly-prefix-map
        ("M-h" . sly-documentation-lookup)))

(use-package sly-company
  :unless (eq system-type 'windows-nt)
  :hook
  (sly-mode . sly-company-mode)
  :config
  (add-to-list 'company-backends 'sly-company))

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

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package nginx-mode
  :mode "\\`Caddyfile\\'"
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

;; (use-package elpy
;;   :config
;;   (elpy-enable)
;;   (when (require 'flycheck nil t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode)))

;; (use-package py-autopep8
;;   :hook
;;   ((elpy-mode . py-autopep8-enable-on-save)))

;; (use-package python-mode
;;   :mode "\\.py\\'"
;;   :custom
;;   (python-indent-offset tab-width)
;;   (py-indent-offset tab-width)
;;   :bind
;;   (:map python-mode-map
;;         ("s-<return>" . py-execute-expression)))

(use-package hy-mode
  :ensure-system-package
  (hy . "pip install git+https://github.com/hylang/hy.git")
  :mode "\\.hy\\'")

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

(use-package robe
  :hook enh-ruby-mode
  :config
  (eval-after-load 'company '(push 'company-robe company-backends)))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package sass-mode
  :mode "\\.sa?c?ss\\'")

(use-package flycheck
  :bind
  (("C-c ! !" . flycheck-mode)))

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
