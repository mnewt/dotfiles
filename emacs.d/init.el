;; Performance

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq gc-cons-threshold 20000000)

;; Load and configure Packages
(load-file "~/.emacs.d/packages.el")
(load-file "~/.emacs.d/update.el")

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

(setq visible-bell       nil
      ring-bell-function #'mode-line-visible-bell)

(setq-default fill-column 80)

(global-hl-line-mode 1)      ; highlight current line
;(global-linum-mode 1)       ; add line numbers on the left

;; Use the system clipboard
(setq select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

(setq suggest-key-bindings 5)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

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

(when (eq system-type 'darwin) ;; mac specific settings
; (setq mac-command-modifier 'meta)
; (setq mac-option-modifier 'none)
  (setq mac-command-modifier 'alt mac-option-modifier 'meta)
  (load-file "~/.emacs.d/mac-key-mode.el")
  (mac-key-mode 1)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setq mac-allow-anti-aliasing t)
  (set-face-font 'default "Monaco-13"))

;; slow down mouse wheel scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
;(setq mouse-wheel-progressive-speed nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tabs
(setq-default indent-tabs-mode nil)                    ; set tab to spaces
(setq-default tab-width 2)                             ; render tabs as two spaces
(setq-default tab-stop-list (number-sequence 2 120 2)) ; set tab width to 2

(electric-pair-mode)

;; multiple-cursors-mode
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

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
    (comment-or-uncomment-region start end)))

(global-set-key "\M-;" 'comment-toggle)

(global-set-key "\C-c\C-p" 'indent-pp-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(put 'match 'scheme-indent-function 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shells and ssh
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "bash")
(setq explicit-bash-args '("--noediting" "--login" "-i"))
(setenv "SHELL" shell-file-name)

(defun bash ()
  (interactive)
  (ansi-term "/usr/local/bin/bash"))

(defun ssh-sudo (hostname)
  "ssh to host, sudo to root, open dired"
  (interactive "MHostname:")
  (find-file (concat "/ssh:" hostname "|sudo:" hostname ":/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
