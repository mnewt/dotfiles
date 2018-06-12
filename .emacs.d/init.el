;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top Level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

(add-to-list 'load-path "~/.emacs.d/elisp/")

(setq tls-checktrust t
      load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(desktop-save-mode 1)
(save-place-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Manager (straight.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'm-package-manager)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(toggle-frame-maximized)

(setq inhibit-splash-screen t)

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

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key emacs-lisp-mode-map (kbd "s-<return>") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key lisp-interaction-mode-map (kbd "s-<return>") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "C-c C-k") 'eval-buffer)

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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Load and configure Packages
(require 'm-theme)
(require 'm-packages)
(require 'm-pointhistory)
;; (require 'm-hydra)

;; Private settings
(when (file-exists-p "~/.private.el")
  (load-file "~/.private.el"))

(defun update-packages ()
  (interactive)
  (straight-normalize-all)
  (straight-fetch-all)
  (straight-merge-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
(put 'erase-buffer 'disabled nil)
