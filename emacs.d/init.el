;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top Level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 40000000)

(add-to-list 'load-path "~/.emacs.d/elisp/")

(setq tls-checktrust t
      load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move persistence files to keep them out of .emacs.d
;; this is is useful to keep dotfiles repo clean
;; and also so that multiple computers can share the same (sync'ed) .emacs.d
(setq emacs-persistence-directory "~/.local/emacs/")
(unless (file-exists-p emacs-persistence-directory)
  (make-directory emacs-persistence-directory t))
(setq ido-save-directory-list-file (concat emacs-persistence-directory "ido-last")
      desktop-path (list emacs-persistence-directory))

(desktop-save-mode 1)

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" emacs-persistence-directory))
;; activate it for all buffers
(if (< emacs-major-version 25)
    (progn (require 'saveplace)
           (setq-default save-place t))
  (save-place-mode 1))

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" emacs-persistence-directory))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" emacs-persistence-directory)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
(recentf-mode +1)

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
                           :weight 'light))
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


;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

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
(setq-default indent-tabs-mode nil)     ; set tab to spaces
(setq-default tab-width 2)
(setq-default tab-stop-list (number-sequence tab-width 120 tab-width))

;; sh-mode
(setq sh-basic-offset tab-width sh-indentation tab-width)

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
;; Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ssh-sudo (hostname) 
  "ssh to host, sudo to root, open dired" 
  (interactive "MHostname: ") 
  (find-file (concat "/sshx:" hostname "|sudo:" hostname ":/")))

;; (https://www.emacswiki.org/emacs/TrampMode#toc30)
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(defun sudo-edit-current-file () 
  (interactive) 
  (let ((position (point))) 
    (find-alternate-file (if (file-remote-p (buffer-file-name)) 
                             (let ((vec (tramp-dissect-file-name (buffer-file-name)))) 
                               (tramp-make-tramp-file-name "sudo" (tramp-file-name-user vec) 
                                                           (tramp-file-name-host vec) 
                                                           (tramp-file-name-localname vec))) 
                           (concat "/sudo:root@localhost:" (buffer-file-name)))) 
    (goto-char position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key emacs-lisp-mode-map (kbd "s-<return>") 'eval-last-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell and SSH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'shell-mode-hook
          (lambda () (define-key shell-mode-map (kbd "SPC") 'comint-magic-space)))

;; This is pretty, but really slow with colorful commands
;; Stolen from (http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html)
;; (require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region (point-min) (point-max))
;;   (toggle-read-only))

;; Filter out terminal escape sequences that emacs would otherwise inelegantly print
;; Stolen from (https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/)
(defun regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F")))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'server)
(unless (server-running-p) 
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Path
(setenv "PATH" (concat "~/.bin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("~/.bin" "/usr/local/bin")))

;; Load and configure Packages
(require 'm-packages)
(require 'm-pointhistory)
;; (require 'm-hydra)

;; Private settings
(when (file-exists-p "~/.private.el") 
  (load-file "~/.private.el"))

(defun update () 
  (interactive) 
  (straight-normalize-all) 
  (straight-fetch-all) 
  (straight-merge-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (custom-set-variables)
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  ;; '(powerline-default-separator nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "#21252b" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Monaco")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/Notes/keys.org"))))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
