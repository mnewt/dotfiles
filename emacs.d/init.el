;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top Level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 20000000)

(add-to-list 'load-path "~/.emacs.d/elisp/")

(setq tls-checktrust t)

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
  (run-with-timer 0.5 nil (lambda () 
                            (with-current-buffer (get-buffer " *Echo Area 0*") 
                              (setq-local face-remapping-alist '((default)))))))

(setq visible-bell nil ring-bell-function #'mode-line-visible-bell)

(setq-default fill-column 80)

;; wrap line at word boundary
(global-visual-line-mode)

(global-hl-line-mode 1)                 ; highlight current line

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq suggest-key-bindings 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system (menu-bar-mode -1) 
      (tool-bar-mode -1) 
      (scroll-bar-mode -1) 
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
                                      (if mac-key-mode 
                                          (setq ns-function-modifier 'hyper mac-option-modifier
                                                'meta mac-command-modifier 'super) 
                                        (setq mac-option-modifier nil)))) 
       (setq mac-allow-anti-aliasing t mac-key-advanced-setting t)
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

;; Enable `goto-address-mode' globally
(define-globalized-minor-mode global-goto-address-mode goto-address-mode 
  (lambda () 
    (goto-address-mode t)))
(global-goto-address-mode t)

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
(global-set-key (kbd "C-c [") 'winner-undo)
(global-set-key (kbd "C-M-,") 'winner-undo)
(global-set-key (kbd "C-c ]") 'winner-redo)

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

;; tags
(global-set-key (kbd "s-R") 'find-tag-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; blinking is NOT OK
(blink-cursor-mode -1)

(delete-selection-mode 1)

;; tabs
(setq-default indent-tabs-mode nil)     ; set tab to spaces
(setq-default tab-width 2)              ; render tabs as two spaces
(setq-default tab-stop-list (number-sequence tab-width 120 tab-width))

;; sh-mode
(setq sh-basic-offset tab-width sh-indentation tab-width)

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("\\DWfile.*\\'" . sh-mode))

;; move by whole words
(global-superword-mode)

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

(setq shell-file-name (executable-find "bash"))

                                        ; (define-key shell-mode-map (kbd "SPC") 'comint-magic-space)

;; Stolen from (http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html)
;; (require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region (point-min) (point-max))
;;   (toggle-read-only))

;; ;; Stolen from (https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/)
;; (defun regexp-alternatives (regexps)
;;   "Return the alternation of a list of regexps."
;;   (mapconcat (lambda (regexp)
;;                (concat "\\(?:" regexp "\\)"))
;;              regexps "\\|"))

;; (defvar non-sgr-control-sequence-regexp nil
;;   "Regexp that matches non-SGR control sequences.")

;; (setq non-sgr-control-sequence-regexp
;;       (regexp-alternatives
;;        '(;; icon name escape sequences
;;          "\033\\][0-2];.*?\007"
;;          ;; non-SGR CSI escape sequences
;;          "\033\\[\\??[0-9;]*[^0-9;m]"
;;          ;; noop
;;          "\012\033\\[2K\033\\[1F")))

;; (defun filter-non-sgr-control-sequences-in-region (begin end)
;;   (save-excursion
;;     (goto-char begin)
;;     (while (re-search-forward
;;             non-sgr-control-sequence-regexp end t)
;;       (replace-match ""))))

;; (defun filter-non-sgr-control-sequences-in-output (ignored)
;;   (let ((start-marker
;;          (or comint-last-output-start
;;              (point-min-marker)))
;;         (end-marker
;;          (process-mark
;;           (get-buffer-process (current-buffer)))))
;;     (filter-non-sgr-control-sequences-in-region
;;      start-marker
;;      end-marker)))

;; (add-hook 'comint-output-filter-functions
;;           'filter-non-sgr-control-sequences-in-output)

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
(require 'm-hydra)

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
