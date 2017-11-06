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
;(global-linum-mode 1)       ; add line numbers on the left

;; Use the system clipboard
(setq select-enable-clipboard t)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

(setq suggest-key-bindings 5)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)
(global-set-key (kbd "A-M-<up>") 'dired-jump)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key (kbd "A-C-f") 'fullscreen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (menu-bar-mode t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq initial-frame-alist
        '((width . 140)
          (height . 50)))
  (setq default-frame-alist
        '((width . 138)
          (height . 48))))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'alt mac-option-modifier 'meta)
  (load-file "~/.emacs.d/mac-key-mode.el")
  (mac-key-mode 1)
  (setq mac-allow-anti-aliasing t)
  (set-face-font 'default "Monaco-13"))

;; slow down mouse wheel scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
;(setq mouse-wheel-progressive-speed nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Navigation and Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(desktop-save-mode 1)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; winner-mode provides C-c <left> to get back to previous window layout
(winner-mode 1)

;; navigating with mark
(global-set-key (kbd "A-M-,") 'pop-global-mark)

;; quick switch buffers
(global-set-key (kbd "A-M-<right>") 'next-buffer)
(global-set-key (kbd "A-M-<left>") 'previous-buffer)

;; kill buffer and window
(defun kill-other-buffer-and-window ()
  "Kill the buffer in the other window"
  (interactive)
  (select-window (next-window))
  (kill-buffer-and-window))

(global-set-key (kbd "A-M-w") 'kill-buffer-and-window)
(global-set-key (kbd "A-M-W") 'kill-other-buffer-and-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tabs
(setq-default indent-tabs-mode nil)                  ; set tab to spaces
(setq-default tab-width 2)                           ; render tabs as two spaces
(setq-default tab-stop-list (number-sequence tab-width 120 tab-width))

;; sh-mode
(setq sh-basic-offset tab-width
      sh-indentation tab-width)

;; (setq indent-vars
;;       '(nginx-indent-level))

;; (defun setup-indent (n)
;;   "Set number of spaces to use for indentation for multiple modes at once"
;;   (dolist (v indent-vars)
;;     (if (boundp v)
;;       (set v n))))

;; (setup-indent tab-width)

(electric-pair-mode)

(global-set-key (kbd "C-c C-p") 'indent-pp-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "A-<return>") 'eval-last-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shells and SSH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "bash")
(setq explicit-bash-args '("--noediting" "--login" "-i"))
(setenv "SHELL" shell-file-name)

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

(defun ssh-sudo (hostname)
  "ssh to host, sudo to root, open dired"
  (interactive "MHostname: ")
  (find-file (concat "/ssh:" hostname "|sudo:" hostname ":/")))

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
(load-file "~/.emacs.d/update.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
