;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top Level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Give Emacs 1GB of heap but run gc on idle
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 3 t (lambda () (garbage-collect)))

(add-to-list 'load-path "~/.emacs.d/elisp/")

(setq tls-checktrust t
      load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; Bootstrap straight.el
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
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

;; suggested requires for `use-package'
(eval-when-compile
  (require 'use-package))
;; (require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(defun update-packages ()
  (interactive)
  (straight-normalize-all)
  (straight-fetch-all)
  (straight-merge-all))

;; Properly install seq package
;; https://github.com/raxod502/straight.el/issues/170
;; (straight-register-package
;;  '(seq :repo "https://git.savannah.gnu.org/git/emacs/elpa.git" :files ("packages/seq/*.el")))

;; (use-package seq
;;   :config
;;   (require 'seq-25))

(use-package epkg
  :defer 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "Forward to `load-theme'.
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
            (m-active0 :background "#565861" :foreground "#E6E7E8")
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
    (set-background-color "#2A2A2A"))
  (add-to-list 'm-themes '(dracula . activate-theme-dracula))
  (activate-theme 'dracula))

;; (use-package monokai-theme
;;   :load-path "straight/build/monokai-theme"
;;   :config
;;   (defun activate-theme-monokai ()
;;     (setq face-remapping-alist
;;           '((m-inactive0 :background "#262834" :foreground "#565861")
;;             (m-active0 :background "#565861" :foreground "#E6E7E8")
;;             (m-inactive1 :background "#262834" :foreground "#565861")
;;             (m-active1 :background "#565861" :foreground "#E6E7E8")
;;             (m-inactive2 :background "#262834" :foreground "#565861")
;;             (m-active2 :background "#CECFD2" :foreground "#565861")
;;             (m-inactive3 :background "#565861" :foreground "#9E9FA5")
;;             (m-active3 :background "#A863C9" :foreground "#FFFFFF")
;;             (m-inactive4 :background "#565861" :foreground "#9E9FA5")
;;             (m-active4 :background "#00e5e5" :foreground "#262834")))
;;     (setq monokai-user-variable-pitch t)
;;     (set-cursor-color "#F60"))
;;   (add-to-list 'm-themes '("monokai" . activate-theme-monokai)))

(use-package solarized-theme
  :load-path "straight/build/solarized-theme"
  :config
  (defun activate-theme-solarized-light ()
    (setq face-remapping-alist
          '((m-inactive0 :background "#EDE8D7" :foreground "#EDE8D7")
            (m-active0 :background "#565861" :foreground "#E6E7E8")
            (m-inactive1 :background "#EDE8D7" :foreground "#EDE8D7")
            (m-active1 :background "#565861" :foreground "#EDE8D7")
            (m-inactive2 :background "#EDE8D7" :foreground "#EDE8D7")
            (m-active2 :background "#CECFD2" :foreground "#565861")
            (m-inactive3 :background "#565861" :foreground "#9E9FA5")
            (m-active3 :background "#A863C9" :foreground "#FFFFFF")
            (m-inactive4 :background "#565861" :foreground "#9E9FA5")
            (m-active4 :background "#00e5e5" :foreground "#262834")))
    (set-mouse-color "black"))
  (add-to-list 'm-themes '(solarized-light . activate-theme-solarized-light)))

;; (use-package doom-themes
;;   ;; :load-path "straight/build/emacs-one-themes"
;;   :config
;;   (defun activate-doom-one ()
;;     (setq doom-themes-enable-bold t
;;           doom-themes-enable-italic t
;;           face-remapping-alist
;;           '((m-inactive0 :background "#262834" :foreground "#565861")
;;             (m-active0 :background "#565861" :foreground "#E6E7E8")
;;             (m-inactive1 :background "#262834" :foreground "#565861")
;;             (m-active1 :background "#565861" :foreground "#E6E7E8")
;;             (m-inactive2 :background "#262834" :foreground "#565861")
;;             (m-active2 :background "#CECFD2" :foreground "#565861")
;;             (m-inactive3 :background "#565861" :foreground "#9E9FA5")
;;             (m-active3 :background "#A863C9" :foreground "#FFFFFF")
;;             (m-inactive4 :background "#565861" :foreground "#9E9FA5")
;;             (m-active4 :background "#00e5e5" :foreground "#262834")))
;;     ;; (set-cursor-color "#F60")
;;     (set-mouse-color "white")
;;     (doom-themes-visual-bell-config))
;;   ;; (set-background-color "#2A2A2A"))
;;   (add-to-list 'm-themes '(doom-one . activate-doom-one))
;;   (add-to-list 'm-themes '(doom-one-light . activate-doom-one))
;;   (add-to-list 'm-themes '(doom-city-lights . activate-doom-one))
;;   (add-to-list 'm-themes '(doom-one . activate-doom-one)))
               
(use-package powerline
  :init
  (progn
    (setq powerline-default-separator nil)
    (set-face-attribute 'mode-line nil
                        :box nil))
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
                          (center (list (powerline-raw "%*" face1 'l)
                                        (powerline-raw (if (file-remote-p default-directory)
                                                           (concat " "
                                                                   (tramp-file-name-host (tramp-dissect-file-name default-directory))
                                                                   " ")
                                                         "")
                                                       face4)
                                        (powerline-raw " " face3)
                                        (powerline-raw (buffer-name) face3 'm)
                                        (powerline-raw " " face3)
                                        (powerline-raw "%*" face1 'r)))
                          (rhs (if active
                                   (list (powerline-raw global-mode-string face0 'r)
                                         (powerline-raw " " face0)
                                         (powerline-raw "%l" face0 'r)
                                         (powerline-raw ":" face0)
                                         (powerline-raw "%c" face0 'r)
                                         (powerline-hud face3 face0))))
                          (lhs (list (powerline-raw " " face1)
                                     (powerline-major-mode face2 'l)
                                     (powerline-raw " " face2)
                                     (powerline-vc face0 'r)
                                     (powerline-raw " " face1)
                                     (powerline-raw (if (eq major-mode 'term-mode)
                                                        (cond
                                                         ((term-in-char-mode) " (char-mode)")
                                                         ((term-in-line-mode) " (line-mode)")
                                                         (t ""))
                                                      "")
                                                    face1)
                                     (powerline-raw " " face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(desktop-save-mode 1)
(save-place-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq frame-resize-pixelwise t)
(toggle-frame-maximized)

(setq inhibit-splash-screen t)

(defun display-startup-echo-area-message ()
  (message "Emacs has finished starting up."))

;; A more pleasant bell. No sound. Simply flash the echo area.
(defun mode-line-visible-bell ()
  "A friendlier visual bell effect."
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist '((default highlight))))
  (run-with-timer 0.15 nil (lambda ()
                             (with-current-buffer (get-buffer " *Echo Area 0*")
                               (setq-local face-remapping-alist '((default)))))))

(setq visible-bell t ring-bell-function #'mode-line-visible-bell)

(setq-default fill-column 80)

;; wrap line at word boundary
;; unfortunately, this causes performance problems, notably with swiper
;; (global-visual-line-mode)

;; highlight current line
(global-hl-line-mode 1)

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

;; Use the system clipboard
(setq select-enable-clipboard t)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key (kbd "A-C-f") 'fullscreen)

;; change yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (setq initial-frame-alist '((top . 1)
                              (left . 75)
                              (width . 195)
                              (height . 58)))
  (setq default-frame-alist '((top . 60)
                              (left . 80)
                              (width . 190)
                              (height . 53))))

(cond ((eq system-type 'darwin)
       (setq ns-alternate-modifier 'meta)
       (setq ns-right-alternate-modifier 'none)
       (setq ns-command-modifier 'super)
       (setq ns-right-command-modifier 'left)
       (setq ns-control-modifier 'control)
       (setq ns-right-control-modifier 'left)
       (setq ns-function-modifier 'hyper)
       (when window-system (menu-bar-mode +1))
       (add-hook 'mac-key-mode-hook (lambda ()
                                      (interactive)
                                      (when mac-key-mode
                                        (setq mac-function-modifier 'hyper
                                              mac-option-modifier 'meta
                                              mac-command-modifier 'super))))
       (setq mac-key-advanced-setting t)
       (global-set-key (kbd "H-<backspace>") 'delete-char)
       (require 'mac-key-mode)
       (mac-key-mode +1)
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
                       file)))
      ((eq system-type 'windows-nt)
       (setq w32-pass-lwindow-to-system nil)
       (setq w32-pass-rwindow-to-system nil)
       (setq w32-lwindow-modifier 'super)
       (setq w32-rwindow-modifier 'super)
       (set-face-font 'default "Consolas-13")))

;; slow down mouse wheel scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                    ((control) . nil)))
(setq scroll-margin 1 scroll-conservatively 1 scroll-up-aggressively 0.01 scroll-down-aggressively
      0.01 mouse-wheel-progressive-speed 1)
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)
;; mouse wheel works horizontally as well
(setq mouse-wheel-tilt-scroll t)

;; Enable `goto-address-mode' globally
(define-globalized-minor-mode global-goto-address-mode goto-address-mode
  (lambda ()
    (goto-address-mode t)))
(global-goto-address-mode t)

;; ido
(setq ido-enable-flex-matching t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq suggest-key-bindings 5
      apropos-do-all t)

;; eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Navigation and Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq windmove-wrap-around t)
(global-set-key (kbd "H-w") 'windmove-up)
(global-set-key (kbd "H-s") 'windmove-down)
(global-set-key (kbd "H-a") 'windmove-left)
(global-set-key (kbd "H-d") 'windmove-right)
(global-set-key (kbd "M-]") 'windmove-right)
(global-set-key (kbd "M-[") 'windmove-left)

;; navigating with mark
(global-set-key (kbd "M-s-,") 'pop-to-mark-command)
(global-set-key (kbd "s-,") 'pop-global-mark)

;; quick switch buffers
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "s-{") 'previous-buffer)

;; kill buffer and window
(defun kill-other-buffer-and-window ()
  "Kill the buffer in the other window"
  (interactive)
  (select-window (next-window))
  (kill-buffer-and-window))

(global-set-key (kbd "M-s-w") 'kill-buffer-and-window)
(global-set-key (kbd "M-s-W") 'kill-other-buffer-and-window)

;; https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
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
                              (car (window-edges (next-window)))) 'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)
(global-set-key (kbd "<H-up>") 'shrink-window)
(global-set-key (kbd "<H-down>") 'enlarge-window)
(global-set-key (kbd "<H-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<H-right>") 'enlarge-window-horizontally)

;; tags
(global-set-key (kbd "s-R") 'find-tag-other-window)

;; dired - try to use GNU ls
(setq insert-directory-program (or (executable-find "gls")
                                   (executable-find "ls"))
      ;; don't prompt to kill buffers of deleted directories
      dired-clean-confirm-killing-deleted-buffers nil)

;; Create friendly names for buffers with the same name
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      ; rename after killing uniquified
      uniquify-after-kill-buffer-p t
      ; don't muck with special buffers
      uniquify-ignore-buffers-re "^\\*")

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:
  emacsclient filename:linenumber
and file 'filename' will be opened and cursor set on line 'linenumber'"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; blinking is NOT OK
(blink-cursor-mode -1)

;; Newline at end of file
(setq require-final-newline t)

;; delete selection on insert or yank
(delete-selection-mode 1)
(setq save-interprogram-paste-before-kill t
      ;; use mouse to kill/yank
      mouse-yank-at-point t
      mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cut-when-buffers-differ t)

;; store all backup and autosave files in their own directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq version-control t)
(setq delete-old-versions t)
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; Enable every deactivated command
(setq disabled-command-function nil)

;; tabs
                                        ; set tab to spaces
(setq-default indent-tabs-mode nil
              tab-width 2
              tab-stop-list (number-sequence tab-width 120 tab-width))

;; sh-mode
(setq sh-basic-offset tab-width
      sh-indentation tab-width)

;; git
(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . conf-mode))

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("\\DWfile.*\\'" . sh-mode))

;; move by whole words
(global-superword-mode)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; show-paren-mode
(defadvice show-paren-function (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
   echo area. Has no effect if the character before point is not of
   the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\))
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Tell `executable-set-magic' to insert #!/usr/bin/env interpreter
(setq executable-prefix-env t)

;; Automatically fill comments. Wraps on `fill-column' columns. 
(add-hook 'prog-mode (lambda ()
                       (set (make-local-variable 'comment-auto-fill-only-comments) t)
                       (auto-fill-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key emacs-lisp-mode-map (kbd "s-<return>") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-x e") 'macrostep-expand)
(define-key lisp-interaction-mode-map (kbd "s-<return>") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "C-c C-k") 'eval-buffer)

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display nfo files in all their glory
;; https://github.com/wasamasa/dotemacs/blob/master/init.org#display-nfo-files-with-appropriate-code-page)
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell, SSH, and Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Path
(setenv "PATH" (concat "~/.bin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("~/.bin" "/usr/local/bin")))

(add-hook 'shell-mode-hook
          (lambda () (define-key shell-mode-map (kbd "SPC") 'comint-magic-space)))

(defun expand-environment-variable ()
  "Insert contents of an envionment variable at point"
  (interactive)
  (insert (getenv (read-envvar-name "Insert Environment Variable: "))))
  
(global-set-key (kbd "C-c C-v") 'expand-environment-variable)

;; dtach (https://github.com/crigler/dtach)
;; https://emacs.stackexchange.com/questions/2283/attach-to-running-remote-shell-with-eshell-tramp-dtach
(setq explicit-dtach-args '("-A" "/tmp/emacs.dtach" "-z" "bash" "--noediting" "--login"))
(defun ssh-dtach (host)
  "Open SSH connection to remote host and attach to dtach session."
  (interactive "Mssh using dtach to host: ")
  (let ((explicit-shell-file-name "dtach")
        (default-directory (format  "/sshx:%s:" host)))
    (shell (format "*ssh %s*" host))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package help-macro+
  :defer 1)

(use-package help+
  :defer 1)

(use-package help-fns+
  :defer 1)

(use-package help-mode+
  :defer 1)

(use-package helpful
  :defer 2
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-h" . helpful-at-point)))

(use-package tldr
  :init
  (global-unset-key (kbd "C-h t"))
  :bind
  (("C-h t t" . tldr)
   ("C-h t u" . tldr-update-docs)))

(use-package info-colors
  :defer 1
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(use-package menu-bar+
  :defer 1)

(use-package which-key
  :defer 1
  :config
  (which-key-mode t)
  :bind
  (("M-s-h" . which-key-show-top-level)))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60)
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package server
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(use-package undo-tree
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode)
  :bind
  (("s-z" . undo-tree-undo)
   ("s-Z" . undo-tree-redo)
   ("s-y" . undo-tree-redo)
   ("M-s-z" . undo-tree-visualize)))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;; (use-package volatile-highlights
;;   :config
;;   (volatile-highlights-mode t))

;; (use-package ace-jump-mode
;;   :bind
;;   (("C-c SPC" . ace-jump-mode)
;;    ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package winner
  :init
  (winner-mode)
  :bind
  (("C-c [" . winner-undo)
   ("s-[" . winner-undo)
   ("C-c ]" . winner-redo)
   ("s-]" . winner-redo)))

(use-package ace-jump-zap
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("C-M-z" . ace-jump-zap-to-char-dwim)))

(use-package ace-window
  :bind
  (("M-o" . ace-window)))

(use-package buffer-move
  :bind
  (("C-H-W" . buf-move-up)
   ("C-H-S" . buf-move-down)
   ("C-H-A" . buf-move-left)
   ("C-H-D" . buf-move-right)))

(use-package mwim
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)
   :map mac-key-mode-map
   ("s-<right>" . mwim-end-of-code-or-line)
   ("s-<left>" . mwim-beginning-of-code-or-line)))

(use-package dired+
  :init
  (defun dired-to-default-directory ()
    "Open directory containing the current file"
    (interactive)
    (dired default-directory))
  
  (defun dired-open-file ()
    "Open file at point in OS default program"
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "open" nil 0 nil file)))
  :config
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-listing-switches "-alh"
        dired-dwim-target t)
  :hook
  ((dired-mode . (lambda ()
                   (define-key dired-mode-map (kbd "C-c o") #'dired-open-file)
                   (define-key dired-mode-map (kbd "C-x f") #'find-file-literally-at-point))))
  :bind
  (("C-x C-d" . dired-to-default-directory)
   ("C-x d" . dired)))

(use-package dired-rsync
  :hook
  ((dired-mode . (lambda ()
                   (define-key dired-mode-map (kbd "C-c C-r") #'dired-rsync)))))

(use-package direx
  :bind
  (("C-x C-j" . direx:jump-to-directory)))

(straight-register-package
 '(vkill :repo "https://github.com/emacsattic/vkill.git"))

(use-package vkill
  :bind
  (("C-c t" . vkill)))

(use-package proc-net
  :bind
  (("C-c n" . list-network-processes)))

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
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-c C-a"  . mc/mark-all-dwim)))

;; (use-package mc-extras
;;   :bind
;;   (:map mc/keymap
;;         ("C-. M-C-f" . mc/mark-next-sexps)
;;         ("C-. M-C-b" . mc/mark-previous-sexps)
;;         ("C-. <" . mc/mark-all-above)
;;         ("C-. >" . mc/mark-all-below)
;;         ("C-. C-d" . mc/remove-current-cursor)
;;         ("C-. C-k" . mc/remove-cursors-at-eol)
;;         ("C-. d" . mc/remove-duplicated-cursors)
;;         ("C-. C-." . mc/freeze-fake-cursors-dwim)
;;         ("C-. ." . mc/move-to-column)
;;         ("C-. =" . mc/compare-chars)))

(use-package origami
  :bind
  (:map origami-mode-map
        ("M-s-]" . origami-close-node-recursively)
        ("M-s-[" . origami-open-node-recursively)
        ("s-|" . origami-recursively-toggle-node)
        ("s-+" . origami-show-only-node)
        ("s-=" . origami-open-all-nodes)))

(use-package wgrep
  :bind
  (("C-c C-p" . wgrep-change-to-wgrep-mode)))

;; some rg features require this
(use-package wgrep-ag)

(use-package rg
  :config
  (progn
    (rg-enable-default-bindings (kbd "C-r"))
    (add-hook 'rg-mode-hook 'wgrep-ag-setup)))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :custom
  (company-backends
   '(company-capf company-gtags company-css company-elisp company-keywords
                  company-semantic company-yasnippet company-files
                  company-dabbrev-code company-dabbrev company-ispell))
  (company-idle-delay 0.1)
  (company-dabbrev-ignore-case t)
  (company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend
     company-echo-metadata-frontend
     company-preview-frontend))
  ;; (company-auto-complete t)
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
  (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell)))

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

(use-package ivy-hydra
  :defer 1)

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
  (("C-s" . swiper)
   ("s-5" . replace-regexp-entire-buffer)))
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
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h <tab>" . counsel-info-lookup-symbol)
   ("C-c u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c l" . counsel-locate)
   ("M-s-v" . counsel-yank-pop)
   ("M-Y" . counsel-yank-pop)
   ;; :map ivy-minibuffer-map
   ;; ("M-y" . ivy-next-line-and-call)
   :map mac-key-mode-map
   ("s-f" . counsel-grep-or-swiper)
   ("s-o" . counsel-find-file)))

(use-package projectile
  :init
  (defun projectile-load-settings (file)
    "https://github.com/jfeltz/projectile-load-settings/blob/master/projectile-load-settings.el
    Load project elisp settings file if they are found in active project root
    directory, or if in the case of undefined root directory, file is
    otherwise path resolvable."
    (interactive "sEnter elisp filename (in project root): ")
    (let
        ((p (concat (projectile-project-root) file)))
      (when (file-exists-p p)
        (load p)
        (message "%s" (concat "Loaded project settings from: " file)) nil)))
  (setq frame-title-format
        '(""
          ;; "%b"
          (:eval
           (when (fboundp 'projectile-project-name)
             (let ((project-name (projectile-project-name)))
               (unless (string= "-" project-name)
                 (format "[%s]" project-name))))))
        projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  :hook
  ((projectile-switch-project . (lambda () (projectile-load-settings "settings.el"))))
  :bind
  (("M-s-f" . counsel-projectile-rg)))

(use-package counsel-projectile
  :init
  (setq counsel-projectile-remove-current-buffer t
        counsel-projectile-remove-current-project t)
  :config
  (counsel-projectile-mode)
  :bind
  (("M-s-p" . counsel-projectile-switch-to-buffer)
   ("s-p" . counsel-projectile)
   ("s-P" . counsel-projectile-switch-project)
   ("s-t" . counsel-imenu)))

(use-package imenu-anywhere
  :bind
  (("s-r" . ivy-imenu-anywhere)))

(use-package dumb-jump
  :init
  (progn
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-prefer-searcher 'rg))
  :config
  (dumb-jump-mode)
  :bind
  (("s-j" . dumb-jump-go-prompt)
   ("s-." . dumb-jump-go)
   ("s-<mouse-1>". dumb-jump-go)
   ("s-J" . dumb-jump-quick-look)))

(use-package magit
  :init
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

  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :bind
  (("C-x g" . magit-status)
   ("C-x C-g" . magit-dispatch-popup)))

;; (use-package git-timemachine
;;   :bind
;;   (("C-x t" . git-timemachine)))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/code"))

(use-package gist
  :bind
  (("C-x M-g" . gist-list)))

(use-package git-link
  :defer 1)

(use-package diff-hl
  :defer 1
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  ((prog-mode markdown-mode) . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; (use-package goto-last-change
;;   :bind
;;   (("M-," . goto-last-change)))

(use-package sly
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  :bind
  (:map sly-prefix-map
        ("M-h" . sly-documentation-lookup)))

(use-package sly-company
  :hook
  (sly-mode . sly-company-mode)
  :config
  (add-to-list 'company-backends 'sly-company))

;; (use-package yasnippet
;;   :config
;;   (yas-global-mode 1))

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

(use-package smartparens
  :init
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
  :config
  (progn
    (require 'smartparens-config)
    (setq sp-base-key-bindings 'paredit)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol nil)
    (sp-use-paredit-bindings)

    (show-smartparens-global-mode +1)
    
    ;; https://github.com/Fuco1/smartparens/issues/80
    (defun m-create-newline-and-enter-sexp (&rest _ignored)
      "Open a new brace or bracket expression, with relevant newlines and indent. "
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode))

    (sp-with-modes
        '(sh-mode js2-mode javascript-mode)
      (sp-local-pair "{" nil :post-handlers '((m-create-newline-and-enter-sexp "RET"))))

    (sp-with-modes
        '(sh-mode)
      (sp-local-pair "do" "done"
                     :when '(("SPC" "RET" "<evil-ret>"))
                     :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                     :actions '(insert navigate)
                     :pre-handlers '(sp-sh-pre-handler)
                     :post-handlers '(sp-sh-block-post-handler))
      (sp-local-pair "then" "fi"
                     :when '(("SPC" "RET" "<evil-ret>"))
                     :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                     :actions '(insert navigate)
                     :pre-handlers '(sp-sh-pre-handler)
                     :post-handlers '(sp-sh-block-post-handler))
      (sp-local-pair "case" "esac"
                     :when '(("SPC" "RET" "<evil-ret>"))
                     :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                     :actions '(insert navigate)
                     :pre-handlers '(sp-sh-pre-handler)
                     :post-handlers '(sp-sh-block-post-handler))))
  :hook
  ((prog-mode conf-mode markdown-mode eshell-mode text-mode) . turn-on-smartparens-mode))

;; Trying out paredit with these 3 packages
;; (use-package paredit
;;   :init
;;   (progn
;;     (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;     (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;;     (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;     (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;;     (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;;     (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;     (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;;     (eldoc-add-command
;;      'paredit-backward-delete
;;      'paredit-close-round)))

;; (use-package aggressive-indent
;;   :config
;;   (progn
;;     (global-aggressive-indent-mode 1)
;;     (add-to-list 'aggressive-indent-excluded-modes 'html-mode)))

;; (use-package paren-face
;;   :config
;;   (progn
;;     (set-face-foreground 'parenthesis "#5C6370")))

(use-package parinfer
  :init
  (defun m-parinfer-yank-advice (f &rest args)
    "Make parinfer yanking work OK even when smartparens is enabled"
    (let ((current-mode parinfer--mode))
      ;; Insert something to make sure indent-mode puts parens after point 
      (insert ".")
      (parinfer--invoke-parinfer-instantly (point))
      (parinfer--switch-to-paren-mode)
      (parinfer-backward-delete-char)
      (apply f args)
      (if (eq 'indent current-mode)
          (parinfer--switch-to-indent-mode))))
  
  :custom
  (parinfer-extensions 
   '(defaults        ; should be included.
      pretty-parens   ; different paren styles for different modes.
      smart-tab       ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank))    ; Yank behavior depend on mode.
  
  :config
  (parinfer-strategy-add 'default 'newline-and-indent)

  (advice-add 'parinfer-smart-yank:yank :around #'m-parinfer-yank-advice)
  
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
        ;; Override mac-key-mode
        ("s-v" . parinfer-smart-yank:yank)
        :map parinfer-region-mode-map
        ("C-i" . indent-for-tab-command)
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)))

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
  (("C-c C-d" . dash-at-point)))

;; (use-package counsel-dash
;;   :custom
;;   (counsel-dash-docsets-path "~/Dash/DocSets")
;;   (counsel-dash-common-docsets '("bash")))

(use-package pcre2el
  :defer 1
  :config
  (rxt-global-mode t)
  (pcre-mode t))

(use-package visual-regexp-steroids
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)
   ("C-c m" . vr/mc-mark)))

(use-package elisp-format
  :defer 2)

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

(straight-use-package 'org)

(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

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
    (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode
              (inf-clojure-minor-mode)
              (inf-clojure "lumo -d")))
  (defun reinstate-comint-simple-send ()
    (unless inf-clojure-minor-mode
      (setq-local comint-send-input 'comint-simple-send)))
  :hook
                                        ; `inf-clojure' seems to clobber `comint-send-input' on all comint buffers
  (comint-mode . reinstate-comint-simple-send)
  :bind
  (:map inf-clojure-minor-mode-map
        ("s-<return>" . inf-clojure-eval-last-sexp)
        ("C-c C-k" . inf-clojure-eval-buffer)))

(use-package geiser
  :defer 2
  :config
  (progn
    ;; Use CHICKEN Scheme
    (setq scheme-program-name "csi -:c")

    ;; Indenting module body code at column 0
    (defun scheme-module-indent (state indent-point normal-indent) 0)
    (put 'module 'scheme-indent-function 'scheme-module-indent)

    (put 'and-let* 'scheme-indent-function 1)
    (put 'parameterize 'scheme-indent-function 1)
    (put 'handle-exceptions 'scheme-indent-function 1)
    (put 'when 'scheme-indent-function 1)
    (put 'unless 'scheme-indenfunction 1)
    (put 'match 'scheme-indent-function 1)))

(use-package rainbow-mode
  :hook
  ((sass-mode emacs-lisp-mode) . rainbow-mode))
(use-package highlight-escape-sequences
  :defer 1
  :config
  (turn-on-hes-mode))

(use-package hl-todo
  :defer 2
  :config
  (global-hl-todo-mode))

(use-package markdown-mode
  :mode "\\.md\\|markdown\\'"
  :init
  (setq markdown-command "multimarkdown"))

(use-package pinentry
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  (pinentry-start))

(use-package tramp
  :config
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
  (defun disable-file-accesses ()
    (when (file-remote-p default-directory)
      (setq-local projectile-mode-line "Projectile")
      (setq-local company-backends (remove 'company-files company-backends))))

  :hook
  (find-file . disable-file-accesses))

(use-package xterm-color
  :hook
  ((shell-mode . (lambda ()
                   (add-hook 'comint-preoutput-filter-functions
                             'xterm-color-filter nil t)))))

(use-package eterm-256color
  :hook
  (term-mode . eterm-256color-mode))

(use-package eshell
  :init
  (setenv "CLICOLOR" "1")
  (setenv "LSCOLORS" "ExFxCxDxBxegedabagacad")
  (setenv "LS_COLORS" "di=36:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34:su=0:sg=0:tw=0:ow=0:")
  (setenv "PAGER" "cat")
  (setenv "MANPAGER" "cat")
  (setenv "BOOT_JVM_OPTIONS" "-client -XX\:+TieredCompilation -XX\:TieredStopAtLevel\=1 -Xmx2g -XX\:+UseConcMarkSweepGC -XX\:+CMSClassUnloadingEnabled -Xverify\:none -XX\:+AggressiveOpts")

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
          (eshell-send-input))
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

  (defun source (filename)
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

  (defun eshell/exit-code (cmd &rest args)
    "Run PROGRAM with ARGS and return the exit code"
    (with-temp-buffer (apply 'call-process cmd nil (current-buffer) nil args)))

  (defun m-eshell-prompt-function ()
    (concat
     (when (not (eshell-exit-success-p))
       (propertize (concat " " (number-to-string eshell-last-command-status) " ")
                   'face `(:background "red")))
     (propertize (concat " " (replace-regexp-in-string (getenv "HOME") "~" (eshell/pwd)) " ")
                 'face `(:background "cyan" :foreground "black"))
     (propertize "\n() "
                 'face `(:foreground "white" :weight bold))))

  (defun eshell/s (hostname)
    "Change directory to host via tramp"
    (eshell/cd (concat "/sshx:" hostname ":")))

  (defun eshell/info (&optional subject)
    "Invoke `info', optionally opening the Info system to SUBJECT."
    (let ((buf (current-buffer)))
      (Info-directory)
      (if (not (null subject))
          (let ((node-exists (ignore-errors (Info-menu subject))))
            (if (not node-exists)
                (format "No menu item `%s' in node `(dir)Top'." subject))))))

  (defun eshell/init ()
    (source "~/.env")
    (setq eshell-path-env (getenv "PATH"))
    (setenv "TERM" "eterm-color")
    (setenv "EDITOR" "emacsclient")

    (global-key-binding (kbd "M-p") 'eshell-send-previous-input)
    ;; (add-hook sh-mode-hook
    ;;           (lambda () (define-key sh-mode-map (kbd "s-<ret>") 'eshell-send-current-line)))
    (define-key eshell-mode-map (kbd "C-a") 'eshell-maybe-bol)
    (define-key eshell-mode-map (kbd "C-d") 'eshell-quit-or-delete-char)
    (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
    (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history)
    (define-key eshell-mode-map (kbd "C-w") 'eshell/kill-previous-output)
    (define-key eshell-mode-map (kbd "M-w") 'eshell/copy-previous-output)
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
    (add-to-list 'eshell-visual-commands "ncdu")
    (add-to-list 'eshell-visual-commands "nvim")
    (add-to-list 'eshell-visual-commands "ssh")
    (add-to-list 'eshell-visual-commands "tail")
    (add-to-list 'eshell-visual-commands "top")
    (add-to-list 'eshell-visual-commands "vim")
    (eshell/alias "df" "df -h")
    (eshell/alias "whats-my-ip" "curl -s https://diagnostic.opendns.com/myip")
    (eshell/alias "dis" "drill +nocmd +noall +answer")
    (eshell/alias "show-hidden-files" "defaults write com.apple.finder AppleShowAllFiles -bool true; and killall Finder")
    (eshell/alias "hide-hidden-files" "defaults write com.apple.finder AppleShowAllFiles -bool false; and killall Finder")
    (eshell/alias "mergepdf" "/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py"))
  ;; TODO: These break if local has grc but tramp remote doesn't
  ;; (if (= 0 (eshell/exit-code "which" "grc"))
  ;;     (eshell/alias "colourify" "$(which grc) -es --colour=auto")
  ;;   (eshell/alias "configure" "colourify ./configure")
  ;;   (eshell/alias "diff" "colourify diff")
  ;;   (eshell/alias "make" "colourify make")
  ;;   (eshell/alias "gcc" "colourify gcc")
  ;;   (eshell/alias "g++" "colourify g++")
  ;;   (eshell/alias "as" "colourify as")
  ;;   (eshell/alias "gas" "colourify gas")
  ;;   (eshell/alias "ld" "colourify ld")
  ;;   (eshell/alias "netstat" "colourify netstat")
  ;;   (eshell/alias "ping" "colourify ping")
  ;;   (eshell/alias "traceroute" "colourify traceroute")
  ;;   (eshell/alias "tracepath" "colourify -c conf.traceroute tracepath")
  ;;   (eshell/alias "arp" "colourify -c conf.traceroute arp")
  ;;   (eshell/alias "tail" "colourify -c conf.log tail")
  ;;   (eshell/alias "ps" "colourify -c conf.ps ps")
  ;;   (eshell/alias "ifconfig" "colourify -c conf.traceroute ifconfig")
  ;;   (eshell/alias "nmap" "colourify -c conf.nmap nmap")
  ;;   (eshell/alias "lsof" "colourify -c conf.traceroute lsof")
  ;;   (eshell/alias "dig" "colourify -c conf.traceroute dig")
  ;;   (eshell/alias "host" "colourify -c conf.traceroute host")
  ;;   (eshell/alias "drill" "colourify -c conf.traceroute drill")
  ;;   (eshell/alias "curl" "colourify -c conf.curl curl")))

  (defun eshell/kill-previous-output (&optional nth)
    "Copies the output of the previous command to the kill ring.
    When nth is set, it will copy the nth previous command.
    Adapted from http://fasciism.com/2017/01/27/eshell-kill-previous-output/"
    (save-excursion
      ;; Move to the end of the eshell buffer.
      (goto-char (point-max))
      ;; Move to the start of the last prompt.
      (search-backward-regexp eshell-prompt-regexp nil nil nth)
      ;; Move to the start of the line, before the prompt.
      (beginning-of-line)
      ;; Remember this position as the end of the region.
      (let ((end (point)))
        ;; Move to the start of the last prompt.
        (search-backward-regexp eshell-prompt-regexp)
        ;; Move one line below the prompt, where the output begins.
        (next-line)
        ;; Find first line that's not blank.
        (while (looking-at "^[[:space:]]*$")
          (beginning-of-line)
          (next-line))
        (let ((count (count-words-region (point) end)))
          ;; Kill region
          (kill-region (point) end)
          ;; Output stats on what was copied as a sanity check.
          (format "Copied %s words to kill ring." count)))))

  (defun eshell/copy-previous-output (&optional nth)
    "Copies the output of the previous command to the kill ring.
When nth is set, it will copy the nth previous command.
    Stolen from http://fasciism.com/2017/01/27/eshell-kill-previous-output/"
    (save-excursion
      ;; Move to the end of the eshell buffer.
      (goto-char (point-max))
      ;; Move to the start of the last prompt.
      (search-backward-regexp eshell-prompt-regexp nil nil nth)
      ;; Move to the start of the line, before the prompt.
      (beginning-of-line)
      ;; Remember this position as the end of the region.
      (let ((end (point)))
        ;; Move to the start of the last prompt.
        (search-backward-regexp eshell-prompt-regexp)
        ;; Move one line below the prompt, where the output begins.
        (next-line)
        ;; Find first line that's not blank.
        (while (looking-at "^[[:space:]]*$")
          (beginning-of-line)
          (next-line))
        ;; Copy region to kill ring.
        (copy-region-as-kill (point) end)
        ;; Output stats on what was copied as a sanity check.
        (format "Copied %s words to kill ring." (count-words-region (point) end)))))

  :custom
  (eshell-buffer-shorthand t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-error-if-no-glob t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-prefer-lisp-functions t)
  (eshell-prefer-lisp-variables t)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-prompt-function 'm-eshell-prompt-function)
  (eshell-prompt-regexp "^() ")
  (eshell-highlight-prompt nil)
  :hook
  ((eshell-mode . eshell/init)
   (eshell-before-prompt . (lambda ()
                             (setq xterm-color-preserve-properties t))))
  ;;  (sh-mode . (lambda ()
  ;;               (define-key sh-mode-map (kbd "s-<ret>") 'eshell-send-current-line))))
  :bind
  (("s-e" . eshell-other-window)
   ("s-E" . eshell)
   :map prog-mode-map
   ("M-P" . eshell-send-previous-input)))

(use-package esh-autosuggest
  :hook ((eshell-mode . esh-autosuggest-mode))
  :bind (:map esh-autosuggest-active-map
              ("C-e" . company-complete-selection)))

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
  :defer 2
  :config
  (docker-global-mode))

(use-package docker-tramp
  :defer 2)

(use-package bash-completion
  :defer 2
  :config
  (bash-completion-setup))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package fish-mode
  :custom (fish-indent-offset tab-width)
  :mode "\\.fish\\'")

(use-package fish-completion
  :defer 2
  :custom (fish-completion-fallback-on-bash-p t)
  :config
  (when (and (executable-find "fish")
             (require 'fish-completion nil t)))
  (global-fish-completion-mode))

(use-package nginx-mode
  :defer 2
  :config
  (setq nginx-indent-level tab-width))

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
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :custom
  (js2-basic-offset tab-width))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra  
  :bind
  (("C-c f" . hydra-zoom/body)
   ("C-c G" . hydra-goto-line/body)
   ("C-c a" . hydra-apropos/body)
   ("C-c w" . hydra-window/body)
   ("C-c t" . hydra-tile/body)
   ("C-c e" . hydra-ediff/body)
   ("C-c C-o" . hydra-occur-dwim/body)
   ("C-c #" . hydra-outline/body)
   ("C-c o" . hydra-origami/body)
   ("C-c c" . hydra-multiple-cursors/body)
   ("C-c C-p" . hydra-projectile/body)
   ("C-c C-m" . hydra-markdown-mode/body)))

(define-key dired-mode-map "." 'hydra-dired/body)
;; (define-key ivy-minibuffer-map "\C-o" 'soo-ivy/body)

(defhydra hydra-zoom (:color pink)
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("q" nil "quit"))

(defhydra hydra-goto-line (goto-map ""
                                    :pre (linum-mode 1)
                                    :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))

(defhydra hydra-apropos (:color blue)
  "Apropos"
  ("a" apropos "apropos")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "option")
  ("u" apropos-user-option "option")
  ("v" apropos-variable "var")
  ("i" info-apropos "info")
  ("t" tags-apropos "tags")
  ("z" hydra-customize-apropos/body "customize"))

(defhydra hydra-window (:color red :hint nil)
  "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("v" split-window-right)
  ("x" split-window-below)
  ("t" transpose-frame "'")
  ;; winner-mode must be enabled
  ("u" winner-undo)
  ("r" winner-redo) ;;Fixme, not working?
  ("o" delete-other-windows :exit t)
  ("a" ace-window :exit t)
  ("f" new-frame :exit t)
  ("s" ace-swap-window)
  ("da" ace-delete-window)
  ("dw" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t)
  ("q" nil)
  ("i" ace-maximize-window "ace-one" :color blue)
  ("b" ido-switch-buffer "buf")
  ("m" headlong-bookmark-jump))

(defhydra hydra-tile
  (:hint nil :color pink)
  "
Strategy  ^^^^        | Other Options    ^  | Resize
----------^^^^--------+------------------^--+---------------
 _t_all    ^^         | _s_elect            | _}_ (enlarge horizontally)
 _w_ide    ^^         | _n_ext tile mode    | _{_ (shrink horizontally)
 _m_aster  ^^         | _u_ndo (winner)     | _>_ (enlarge vertically)
 _v_ertical split^^   | _V_eritcal delete   | _<_ (shrink vertically)
 _h_orizontal split^^ | _H_orizontal delete |
 _1_ (one) ^^         | _q_uit              |
"
  ("t" (tile :strategy (tile-split-n-tall 2)))
  ("w" (tile :strategy tile-wide))
  ("m" (tile :strategy tile-master-left-3))
  ("1" (tile :strategy tile-one))
  ("v" split-window-vertically)
  ("V" delete-other-windows-vertically)
  ("h" split-window-horizontally)
  ("H" delete-other-windows)
  ("s" tile-select)
  ("n" tile)
  ("u" winner-undo)
  ("q" keyboard-escape-quit :exit t)
  ("C-g" keyboard-escape-quit :exit t)
  ("<escape>" keyboard-escape-quit :exit t)
  ("}" enlarge-window-horizontally)
  ("{" shrink-window-horizontally)
  (">" enlarge-window)
  ("<" shrink-window))

(defhydra hydra-ediff (:color blue :hint nil)
  "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                  _c_urrent file
"
  ("b" ediff-buffers)
  ("B" ediff-buffers3)
  ("=" ediff-files)
  ("f" ediff-files)
  ("F" ediff-files3)
  ("c" ediff-current-file)
  ("r" ediff-revision)
  ("l" ediff-regions-linewise)
  ("w" ediff-regions-wordwise))

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

(defhydra hydra-outline (:color pink :hint nil)
  "
  ^Hide^             ^Show^           ^Move
  ^^^^^^------------------------------------------------------
  _q_: sublevels     _a_: all         _u_: up
  _t_: body          _e_: entry       _n_: next visible
  _o_: other         _i_: children    _p_: previous visible
  _c_: entry         _k_: branches    _f_: forward same level
  _l_: leaves        _s_: subtree     _b_: backward same level
  _d_: subtree

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

(defhydra hydra-origami
  (:hint nil :color pink)
  "
 Open           ^^   | Close             ^^  | Toggle          ^^   | Navigate
----------------^^---+-------------------^^--+-----------------^^---+-----------
 _o_pen              | _c_lose               | _t_oggle             | _p_revious 
 _O_pen recursively  | _C_lose recursively   | _T_oggle recursively | _n_ext
 _s_how node         |                    ^^ | _F_orward toggle     | _f_orward
 _S_how only node    |                    ^^ |                   ^^ | _u_ndo
 _+_ open all nodes  | _-_ close all nodes   | _=_ toggle all nodes ^| _r_edo
                                                          ^^^^^^      _z_ reset
                                                          ^^^^^^      _q_uit"
  ("o" origami-open-node)
  ("O" origami-open-node-recursively)
  ("s" origami-show-node)
  ("S" origami-show-only-node)
  ("c" origami-close-node)
  ("C" origami-close-node-recursively)
  ("t" origami-toggle-node)
  ("F" origami-forward-toggle-node)
  ("T" origami-recursively-toggle-node)
  ("+" origami-open-all-nodes)
  ("-" origami-close-all-nodes)
  ("=" origami-toggle-all-nodes)
  ("p" origami-previous-fold)
  ("n" origami-next-fold)
  ("f" origami-forward-fold)
  ("u" origami-undo)
  ("r" origami-redo)
  ("z" origami-reset)
  ("q" keyboard-escape-quit :exit t)
  ("C-g" keyboard-escape-quit :exit t)
  ("<escape>" keyboard-escape-quit :exit t))

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
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
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
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
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

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

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
_m_: mark      _D_: delete       _g_: refresh    _q_: quit             _k_:   ↑    _h_
_u_: unmark    _s_: save marked  _S_: sort       _TAB_: toggle         _RET_: visit
_*_: specific  _a_: all actions  _/_: filter     _o_: other window     _j_:   ↓    _l_
_t_: toggle    _._: toggle hydra _H_: help       C-o other win no-select
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

(define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
(add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)

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

(defhydra soo-ivy (:hint nil :color pink)
  "
Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
_g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
_G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
"
  ;; arrows
  ("j" ivy-next-line)
  ("k" ivy-previous-line)
  ("l" ivy-alt-done)
  ("h" ivy-backward-delete-char)
  ("g" ivy-beginning-of-buffer)
  ("G" ivy-end-of-buffer)
  ("d" ivy-scroll-up-command)
  ("u" ivy-scroll-down-command)
  ("e" ivy-scroll-down-command)
  ;; actions
  ("q" keyboard-escape-quit :exit t)
  ("C-g" keyboard-escape-quit :exit t)
  ("<escape>" keyboard-escape-quit :exit t)
  ("C-o" nil)
  ("i" nil)
  ("TAB" ivy-alt-done :exit nil)
  ("C-j" ivy-alt-done :exit nil)
  ;; ("d" ivy-done :exit t)
  ("RET" ivy-done :exit t)
  ("C-m" ivy-done :exit t)
  ("f" ivy-call)
  ("c" ivy-toggle-calling)
  ("m" ivy-toggle-fuzzy)
  (">" ivy-minibuffer-grow)
  ("<" ivy-minibuffer-shrink)
  ("w" ivy-prev-action)
  ("s" ivy-next-action)
  ("a" ivy-read-action)
  ("t" (setq truncate-lines (not truncate-lines)))
  ("C" ivy-toggle-case-fold)
  ("o" ivy-occur :exit t))

(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

(defhydra hydra-projectile (:color teal
                                   :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

(defhydra hydra-markdown-mode (:hint nil)
  "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code

Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4

Lists             C-c C-x    _m_: insert item   

Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down

Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference
 
"


  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("c" markdown-insert-code)

  ("h" markdown-insert-header-dwim) 
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("l" markdown-promote)
  ("r" markdown-demote)
  ("d" markdown-move-down)
  ("u" markdown-move-up)  

  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("R" markdown-insert-reference-link-dwim :color blue)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Get pointhistory to work
;; (require 'm-pointhistory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load private stuff
(let ((f "~/.emacs.d/elisp/private.el"))
  (when (file-exists-p f)
    (load-file f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
