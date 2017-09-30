(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; Use MELPA
(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Be sure to run this before installing new packages
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

;; set local recipes
(setq
 el-get-sources
 '((:name spaceline
          :after (progn
                   (require 'spaceline-config)
                   (spaceline-emacs-theme)
                   (setq powerline-default-separator nil)
                   (spaceline-compile)))
   (:name atom-one-dark-theme
          :after (progn
                   (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/atom-one-dark-theme/")
                   (load-theme 'atom-one-dark t)))
   (:name company-mode
          :after (add-hook 'after-init-hook 'global-company-mode))
   (:name buffer-move
          :after (progn
                   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))
   (:name magit
          :after (global-set-key (kbd "C-x C-z") 'magit-status))
   (:name git-gutter
          :after (global-git-gutter-mode t))
   (:name goto-last-change
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
   (:name paredit)
   (:name parinfer
          :after (progn
                   (setq parinfer-extensions
                         '(defaults       ; should be included.
                            pretty-parens  ; different paren styles for different modes.
                                        ;  lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
                            paredit        ; Introduce some paredit commands.
                            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
                            smart-yank))   ; Yank behavior depend on mode.
                   (add-hook 'clojure-mode-hook #'parinfer-mode)
                   (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
                   (add-hook 'common-lisp-mode-hook #'parinfer-mode)
                   (add-hook 'scheme-mode-hook #'parinfer-mode)
                   (add-hook 'lisp-mode-hook #'parinfer-mode)))
   ;; (:name scheme-complete
   ;;        :after (progn
   ;;                 (eval-after-load 'scheme
   ;;                   (progn
   ;;                     '(define-key scheme-mode-map "\e\t" 'scheme-smart-complete)
   ;;                     '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent)))))
   (:name ivy
          :after (progn
                   (ivy-mode 1)
                   (setq ivy-use-virtual-buffers t)
                   (setq ivy-count-format "(%d/%d) ")
                   (global-set-key (kbd "C-s") 'swiper)
                   (global-set-key (kbd "M-x") 'counsel-M-x)
                   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
                   (global-set-key (kbd "<f1> f") 'counsel-describe-function)
                   (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
                   (global-set-key (kbd "<f1> l") 'counsel-find-library)
                   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
                   (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
                   (global-set-key (kbd "C-c g") 'counsel-git)
                   (global-set-key (kbd "C-c j") 'counsel-git-grep)
                   (global-set-key (kbd "C-c k") 'counsel-ag)
                   (global-set-key (kbd "C-x l") 'counsel-locate)
                   (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
                   (global-set-key (kbd "C-c C-r") 'ivy-resume)))
   (:name markdown-mode
          :after (setq markdown-command "multimarkdown"))
   (:name geiser
          :after (setq geiser-active-implementations '(chicken)))))

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
   cider
   swiper
   counsel))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)
