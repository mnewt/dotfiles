;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 20000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)

;; A more pleasant bell. No sound. Simply flash the echo area.
(defun mode-line-visible-bell ()
  "A friendlier visual bell effect."
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist '((default highlight))))
  (run-with-timer
   0.5 nil
   (lambda ()
     (with-current-buffer (get-buffer " *Echo Area 0*")
       (setq-local face-remapping-alist '((default)))))))

(setq visible-bell nil
      ring-bell-function #'mode-line-visible-bell)

(setq-default fill-column 80)

(global-hl-line-mode 1)      ; highlight current line
;; (global-linum-mode 1)       ; add line numbers on the left

;; Use the system clipboard
(setq select-enable-clipboard t)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key (kbd "A-C-f") 'fullscreen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq suggest-key-bindings 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq initial-frame-alist
        '((top . 1)
          (left . 75)
          (width . 195)
          (height . 58)))
  (setq default-frame-alist
        '((top . 60)
          (left . 80)
          (width . 190)
          (height . 53))))

(cond
 ((eq system-type 'darwin)
  (setq ns-alternate-modifier 'meta)
  (setq ns-right-alternate-modifier 'none)
  (setq ns-command-modifier 'super)
  (setq ns-right-command-modifier 'left)
  (setq ns-control-modifier 'control)
  (setq ns-right-control-modifier 'left)
  (setq ns-function-modifier 'hyper)
  (when window-system
    (menu-bar-mode +1))
  (add-hook 'mac-key-mode-hook
            (lambda ()
              (interactive)
              (if mac-key-mode
                  (setq ns-function-modifier 'hyper
                        mac-option-modifier 'meta
                        mac-command-modifier 'super)
                (setq mac-option-modifier nil))))
  (setq mac-allow-anti-aliasing t
        mac-key-advanced-setting t)

  (load-file "~/.emacs.d/mac-key-mode.el")
  (mac-key-mode +1)

  (set-face-font 'default "Monaco-13")
  (set-face-attribute 'default nil :weight 'light))

 ((eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (setq w32-rwindow-modifier 'super)
  (set-face-font 'default "Consolas-13")))

;; slow down mouse wheel scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq scroll-margin 1
      scroll-conservatively 1
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      mouse-wheel-progressive-speed 1)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

(goto-address-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Navigation and Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save buffer config on exit and restore on startup
(desktop-save-mode 1)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)
(global-set-key (kbd "M-]") 'windmove-right)
(global-set-key (kbd "M-[") 'windmove-left)

;; winner-mode provides C-c <left> to get back to previous window layout
(winner-mode 1)

;; navigating with mark
(global-set-key (kbd "M-s-,") 'pop-to-mark-command)
(global-set-key (kbd "s-,") 'pop-global-mark)

;; quick switch buffers
(global-set-key (kbd "M-s-<right>") 'next-buffer)
(global-set-key (kbd "M-s-<left>") 'previous-buffer)

;; kill buffer and window
(defun kill-other-buffer-and-window ()
  "Kill the buffer in the other window"
  (interactive)
  (select-window (next-window))
  (kill-buffer-and-window))

(global-set-key (kbd "M-s-w") 'kill-buffer-and-window)
(global-set-key (kbd "M-s-W") 'kill-other-buffer-and-window)

;; tags
(global-set-key (kbd "s-R") 'find-tag-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; blinking is NOT OK
(blink-cursor-mode -1)

(delete-selection-mode 1)

;; tabs
(setq-default indent-tabs-mode nil)                  ; set tab to spaces
(setq-default tab-width 2)                           ; render tabs as two spaces
(setq-default tab-stop-list (number-sequence tab-width 120 tab-width))

;; sh-mode
(setq sh-basic-offset tab-width
      sh-indentation tab-width)

;; move by whole words
(global-superword-mode)

;; show-paren-mode
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\))
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

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
    (find-alternate-file
     (if (file-remote-p (buffer-file-name))
         (let ((vec (tramp-dissect-file-name (buffer-file-name))))
           (tramp-make-tramp-file-name
            "sudo"
            (tramp-file-name-user vec)
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

;; (setq explicit-shell-file-name "/bin/bash"
;;       shell-file-name "/bin/bash"
;;       explicit-bash-args '("--noediting" "--login" "-i")
;;       tramp-default-method "ssh")

;; (setenv "SHELL" shell-file-name)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(defun bash ()
  "Runs Bash in a `term' buffer."
  (interactive)
  (let* ((cmd "bash")
         (args "-l")
         (switches (split-string-and-unquote args))
         (termbuf (apply 'make-term "Bash" cmd nil switches)))
    (set-buffer termbuf)
    (term-mode)
    (term-char-mode)
    (switch-to-buffer termbuf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'server)
(unless (server-running-p) (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Path
(setenv "PATH" (concat (getenv "PATH") ":~/.bin:/usr/local/bin"))
(setq exec-path (append exec-path '("~/.bin" "/usr/local/bin")))

;; Load and configure Packages
(load-file "~/.emacs.d/packages.el")
(load-file "~/.emacs.d/hydra.el")
;; (load-file "~/.emacs.d/update.el")

;; Private settings
(let ((private "~/.private.el"))
  (if (file-exists-p private) (load-file private)))

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
