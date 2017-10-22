(package-initialize)

;; Bootstrap straight.el
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-always-ensure t)

(use-package better-defaults)

(use-package redo+)

(use-package atom-one-dark-theme
  :load-path "straight/build/atom-one-dark-theme"
  :config
  (load-theme 'atom-one-dark t))

(use-package spaceline-config
  :ensure spaceline
  :config
  (progn
    (spaceline-spacemacs-theme)
    (setq
      powerline-default-separator nil
      spaceline-minor-modes-p nil
      spaceline-buffer-size-p nil
      spaceline-buffer-encoding-abbrev-p nil)
    (spaceline-compile)))

(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 5)
  :config
  (smooth-scrolling-mode 1))

(use-package fill-column-indicator
  :commands fci-mode
  :init
  (add-hook 'after-change-major-mode-hook 'fci-mode)
  :config
  (progn
    (defun on-off-fci-before-company(command)
      (when (string= "show" command)
        (turn-off-fci-mode))
      (when (string= "hide" command)
        (turn-on-fci-mode)))
    (advice-add 'company-call-frontends :before #'on-off-fci-before-company)))

(use-package highlight-indent-guides
  :init
  (progn
    (setq highlight-indent-guides-method 'character)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package projectile)

(use-package counsel-projectile
  :config
  (counsel-projectile-on))

(use-package flx)

(use-package swiper)

(use-package counsel)

(use-package ivy
  ;; :init
  ;; (setq ivy-use-virtual-buffers t
  ;;       ivy-re-builders-alist '((t . ivy--regex-fuzzy))
  ;;       ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)
  :bind
    (("C-s" . swiper)
     ("M-x" . counsel-M-x)
     ("C-x C-f" . counsel-find-file)
     ("M-p" . counsel-file-jump)
     ("<f1> f" . counsel-describe-function)
     ("<f1> v" . counsel-describe-variable)
     ("<f1> l" . counsel-find-library)
     ("<f2> i" . counsel-info-lookup-symbol)
     ("<f2> u" . counsel-unicode-char)
     ("C-c g" . counsel-git)
     ("C-c j" . counsel-git-grep)
     ("C-x l" . counsel-locate)
     ("C-c C-r" . ivy-resume)))

(use-package magit                           
  :bind 
  (("C-x C-z" . magit-status)))

(use-package git-gutter                      
  :config
  (global-git-gutter-mode t))

(use-package goto-last-change
  :bind
  (("C-x C-/" . goto-last-change)))

(use-package sly
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  :bind
  (("M-h" . sly-documentation-lookup)))

(use-package sly-company
  :config
  (progn
    (add-hook 'sly-mode-hook 'sly-company-mode)
    (add-to-list 'company-backends 'sly-company))
  :bind
  (("\C-n" . company-select-next)
   ("\C-p" . company-select-previous)
   ("\C-d" . company-show-doc-buffer)
   ("M-." . company-show-location)))

(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode))
  :config
  (progn
    (setq clojure-align-forms-automatically t)

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
      (cider-eval-last-sexp '(1)))))

(use-package clj-refactor
  :config 
    (progn
      (defun my-clojure-mode-hook ()
        (clj-refactor-mode 1)
        (yas-minor-mode 1) ; for adding require/use/import statements
        ;; This choice of keybinding leaves cider-macroexpand-1 unbound
        (cljr-add-keybindings-with-prefix "C-c C-m"))
      (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)))

(use-package paredit)

(use-package parinfer
  :init
  (setq parinfer-extensions
                 '(defaults       ; should be included.
                    pretty-parens  ; different paren styles for different modes.
                    ;paredit        ; Introduce some paredit commands.
                    smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
                    smart-yank))   ; Yank behavior depend on mode.
  :config (progn
           (add-hook 'clojure-mode-hook #'parinfer-mode)
           (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
           (add-hook 'common-lisp-mode-hook #'parinfer-mode)
           (add-hook 'scheme-mode-hook #'parinfer-mode)
           (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown"))

(use-package fish-mode)

(use-package nginx-mode)

(use-package yaml-mode)
