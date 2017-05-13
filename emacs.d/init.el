; Lots of this is based on emacs kicker
; (https://github.com/dimitri/emacs-kicker)

;; Package system

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
; (package-initialize)

; (require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

; (require 'cl)        ; common lisp goodies, loop

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; Use MELPA
(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Be sure to run this before installing new packages
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

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
   (:name git-gutter
          :after (progn
                   (global-git-gutter-mode t)))
   (:name goto-last-change    ; move pointer back to last change
          :after (global-set-key (kbd "C-x C-/") 'goto-last-change))
   (:name smooth-scrolling
          :after (progn
                   (smooth-scrolling-mode 1)
                   (setq smooth-scroll-margin 5)))
   (:name find-file-in-project
          :after (setq ffip-prefer-ido-mode t))
   (:name clojure-mode
          :after (require 'clojure-mode-extra-font-locking))
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
   (:name parinfer
          :after (progn
                   (setq parinfer-extensions
                         '(defaults       ; should be included.
                           pretty-parens  ; different paren styles for different modes.
                          ;  evil           ; If you use Evil.
                          ;  lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
                           paredit        ; Introduce some paredit commands.
                           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
                           smart-yank))   ; Yank behavior depend on mode.
                   (add-hook 'clojure-mode-hook #'parinfer-mode)
                   (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
                   (add-hook 'common-lisp-mode-hook #'parinfer-mode)
                   (add-hook 'scheme-mode-hook #'parinfer-mode)
                   (add-hook 'lisp-mode-hook #'parinfer-mode)))
   (:name spaceline
          :after (progn
                   (require 'spaceline-config)
                   (spaceline-emacs-theme)
                   (setq powerline-default-separator nil)
                   (spaceline-compile)))
   (:name atom-one-dark-theme
          :after (progn
                   (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/atom-one-dark-theme/")
                   (load-theme 'atom-one-dark t)))))

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get             ; el-get is self-hosting
   better-defaults
   escreen            ; screen for emacs, C-\ C-h
   switch-window      ; takes over C-x o
   auto-complete      ; complete as you type with overlays
   yasnippet          ; powerful snippet mode
   zencoding-mode     ; http://www.emacswiki.org/emacs/ZenCoding
   cider))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; on to the visual settings
(setq inhibit-splash-screen t)    ; no splash screen, thanks
(line-number-mode 1)              ; have line numbers and
(column-number-mode 1)            ; column numbers in the mode line

(global-hl-line-mode 1)      ; highlight current line
;(global-linum-mode 1)       ; add line numbers on the left

;; copy/paste with C-c and C-v and C-x, check out C-RET too
(cua-mode)

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

(setq suggest-key-bindings 5)

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
  (setq mac-option-modifier 'none)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setq mac-allow-anti-aliasing t)
  (set-face-font 'default "Monaco-13"))

(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

(if window-system
    (progn
      (tool-bar-mode -1)          ; no tool bar with icons
      (scroll-bar-mode -1)        ; no scroll bars
      ;; initial window
      (setq initial-frame-alist
            '((width . 102)
              (height . 50)))
      ;; default/sebsequent window
      (setq default-frame-alist
            '((width . 100)
              (height . 48)))))
