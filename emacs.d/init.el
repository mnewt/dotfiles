; Lots of this is based on emacs kicker
; (https://github.com/dimitri/emacs-kicker)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)        ; common lisp goodies, loop

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set local recipes
(setq
 el-get-sources
 '((:name buffer-move      ; have to add your own keys
          :after (progn
                   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name smex        ; a better (ido like) M-x
          :after (progn
                   (setq smex-save-file "~/.emacs.d/.smex-items")
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit        ; git meet emacs, and a binding
          :after (progn
                   (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change    ; move pointer back to last change
          :after (progn
                   ;; when using AZERTY keyboard, consider C-x C-_
                   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
   (:name smooth-scrolling
          :after (progn
                   (smooth-scrolling-mode 1)
                   (setq smooth-scroll-margin 5)))
   (:name find-file-in-project
          :after (setq ffip-prefer-ido-mode t))
   (:name clojure-mode
          :after (progn
                   (require 'clojure-mode-extra-font-locking)))
   (:name clj-refactor
          :after (progn
                   (defun my-clojure-mode-hook ()
                     (clj-refactor-mode 1)
                     (yas-minor-mode 1) ; for adding require/use/import statements
                     ;; This choice of keybinding leaves cider-macroexpand-1 unbound
                     (cljr-add-keybindings-with-prefix "C-c C-m"))
                   (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)))
   (:name smartparens
          :after (require 'smartparens-config))
   (:name moe-theme
          :after (progn
                   (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/moe-theme/")
                   (add-to-list 'load-path "~/.emacs.d/el-get/moe-theme/")
                   (require 'moe-theme)
                   (show-paren-mode t)
                   (setq show-paren-style 'expression)
                   (setq moe-theme-highlight-buffer-id t)
                   (moe-dark)))))

(el-get-bundle edpaget/parinfer-mode
  (progn
    (require 'parinfer-mode)
    (add-hook 'clojure-mode-hook 'parinfer-mode)))


;; now set our own packages
(setq
 my:el-get-packages
 '(el-get        ; el-get is self-hosting
   better-defaults
   escreen                  ; screen for emacs, C-\ C-h
   switch-window      ; takes over C-x o
   auto-complete      ; complete as you type with overlays
   yasnippet         ; powerful snippet mode
   zencoding-mode      ; http://www.emacswiki.org/emacs/ZenCoding
   cider))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; on to the visual settings
(setq inhibit-splash-screen t)    ; no splash screen, thanks
(line-number-mode 1)      ; have line numbers and
(column-number-mode 1)      ; column numbers in the mode line

(if window-system
    (progn
      (tool-bar-mode -1)    ; no tool bar with icons
      (scroll-bar-mode -1)))    ; no scroll bars
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-13")
  (set-face-font 'default "Monospace-10"))

;(global-hl-line-mode)			; highlight current line
;(global-linum-mode 1)			; add line numbers on the left

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; copy/paste with C-c and C-v and C-x, check out C-RET too
(cua-mode)

;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; If you do use M-x term, you will notice there's line mode that acts like
;; emacs buffers, and there's the default char mode that will send your
;; input char-by-char, so that curses application see each of your key
;; strokes.
;;
;; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
;; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") 'term-paste)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-default-buffer-method 'selected-window)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; have vertical ido completion lists
(setq ido-decorations
      '("\n-> " "" "\n   " "\n   ..." "[" "]"
        " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)) ;; sets fn-delete to be right-delete


;; (defun simple-mode-line-render (left right)
;;   "Return a string of `window-width' length containing LEFT, and RIGHT
;;    aligned respectively."
;;   (let* ((available-width (- (window-total-width) (length left) 2)))
;;     (format (format " %%s %%%ds " available-width) left right)))

(setq modelinepos-column-limit 80)

(set-face-background 'mode-line "gray30")
(set-face-foreground 'mode-line "cyan")

(setq-default mode-line-position
              '((-3 "%p") (size-indication-mode ("/" (-4 "%I")))
                " "
                (line-number-mode
                 ("%l" (column-number-mode ":%c")))))

(defun triple-mode-line-render (left middle right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
   aligned respectively."
  (let* ((space-between (ceiling (/ (- (window-total-width)
                                       (length left)
                                       (length middle)
                                       (length right)
                                       1)
                                    2.0))))
    (format (format "%%s%%%ds%%s%%%ds%%s" space-between space-between) left " " middle " " right)))

(setq mode-line-format
      '((:eval (triple-mode-line-render
                ;; left
                (format-mode-line '("%e "
                                    (:propertize "%*" 'face
                                                '(:background "gray50" :foreground "white"))
                                    " "
                                    mode-line-buffer-identification))
                ;; middle
                (format-mode-line '("%[ " mode-name minor-mode-alist " %]"))
                ;; right
                (format-mode-line '(""
                                    (vc-mode vc-mode)
                                    " "
                                    mode-line-misc-info
                                    global-mode-string
                                    mode-line-frame-identification
                                    "%3p%%  %l,%2c"))))))

;; (set-default mode-line-format
;;              '("%e"
;;                mode-line-modified
;;                mode-line-buffer-identification
;;                mode-line-position
;;                vc-mode
;;                mode-line-misc-info
;;                mode-line-modes
;;                mode-line-end-spaces))


;; (setq mode-line-align-left
;;       '(" "
;;         (:propertize "%e%* %b " face '(:background "gray40" :foreground "white"))
;;         "  "
;;         (vc-mode vc-mode)))
;; ;        (:propertize (:eval '("" (vc-mode vc-mode) " ")) face '(:background "purple"))))

;; (setq mode-line-align-middle
;;       '(" "
;;         mode-line-modes
;;         ))

;; (setq mode-line-align-right
;;       '("--"
;; ;        (:propertize (:eval (shorten-directory default-directory 30)) face font-lock-string-face)
;;         ))

;; (defun mode-line-fill-right (face reserve)
;;   "Return empty space using FACE and leaving RESERVE space on the right."
;;   (unless reserve
;;     (setq reserve 20))
;;   (when (and window-system (eq 'right (get-scroll-bar-mode)))
;;     (setq reserve (- reserve 3)))
;;   (propertize " "
;;               'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
;;               'face face))

;; (defun mode-line-fill-center (face reserve)
;;   "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
;;   (unless reserve
;;     (setq reserve 20))
;;   (when (and window-system (eq 'right (get-scroll-bar-mode)))
;;     (setq reserve (- reserve 3)))
;;   (propertize " "
;;               'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
;;                                              (.5 . left-margin))))
;;               'face face))

;; (defconst RIGHT_PADDING 1)

;; (defun reserve-left/middle ()
;;   (/ (length (format-mode-line mode-line-align-middle)) 2))

;; (defun reserve-middle/right ()
;;   (+ RIGHT_PADDING (length (format-mode-line mode-line-align-right))))


;; (setq-default mode-line-format
;;               (list
;;                mode-line-align-left
;;                '(:eval (mode-line-fill-center 'mode-line
;;                                               (reserve-left/middle)))
;;                mode-line-align-middle
;;                '(:eval
;;                  (mode-line-fill-right 'mode-line
;;                                        (reserve-middle/right)))
;;                mode-line-align-right
;;                ))
