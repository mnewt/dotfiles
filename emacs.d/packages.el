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

;; Try to get package.el to work better with straight.el
;; https://github.com/raxod502/straight.el/issues/128
(defun straight--advice-package-installed-p (original-function &rest args)
  "Return t if package is installed via `straight' package manager."
  (or (gethash (symbol-name (car args)) straight--recipe-cache)
      (apply original-function args)))

(advice-add 'package-installed-p :around 'straight--advice-package-installed-p)


;; Properly install seq package
;; https://github.com/raxod502/straight.el/issues/170
(straight-register-package
  '(seq :repo "https://git.savannah.gnu.org/git/emacs/elpa.git" :files ("packages/seq/*.el")))
(use-package seq
  :config
  (require 'seq-25))

(use-package better-defaults)

(use-package redo+)

(use-package atom-one-dark-theme
  :load-path "straight/build/atom-one-dark-theme"
  :config
  (load-theme 'atom-one-dark t))

;; (use-package monokai-theme
;;   :load-path "straight/build/monokai-theme"
;;   :config
;;   (load-theme 'monokai t))

;; (use-package dracula-theme
;;   :load-path "straight/build/dracula-theme"
;;   :config
;;   (load-theme 'dracula t))

(use-package powerline
  :init
  (setq
   powerline-default-separator nil)
  :config
  (progn
    (defun my-powerline-theme ()
      "Setup a mode-line with the major mode centered."
      (interactive)
      (setq-default mode-line-format
        '("%e"
          (:eval
           (let* ((active (powerline-selected-window-active))
                  (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                  (mode-line (if active 'mode-line 'mode-line-inactive))
                  (face0 (if active 'powerline-active0 'powerline-inactive0))
                  (face1 (if active 'powerline-active1 'powerline-inactive1))
                  (face2 (if active 'powerline-active2 'powerline-inactive2))
                  (separator-left (intern (format "powerline-%s-%s"
                                                  (powerline-current-separator)
                                                  (car powerline-default-separator-dir))))
                  (separator-right (intern (format "powerline-%s-%s"
                                                   (powerline-current-separator)
                                                   (cdr powerline-default-separator-dir))))
                  (lhs (list ;; (powerline-raw "%*" face0 'l)
                             ;; (powerline-buffer-size face0 'l)
                             (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)))
                             ;; (powerline-raw " ")
                             ;; (funcall separator-left face0 face1)))
                  (rhs (list (powerline-vc face1)
                             (powerline-raw global-mode-string face1 'r)
                             (powerline-raw " " face1)
                             (powerline-raw "%4l" face1 'r)
                             (powerline-raw ":" face1)
                             (powerline-raw "%2c" face1 'r)
                             (funcall separator-right face1 face0)
                             ;; (powerline-raw " ")
                             ;; (powerline-raw "%6p" face0 'r)
                             (powerline-hud face2 face1)))
                             ;; (powerline-fill face0 0)))
                  (center (list (powerline-raw " " face1)
                                (funcall separator-left face1 face2)
                                (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                  (powerline-raw erc-modified-channels-object face2 'l))
                                (powerline-major-mode face2 'l)
                                (powerline-process face2)
                                ;; (powerline-raw " :" face2)
                                ;; (powerline-minor-modes face2 'l)
                                (powerline-raw " " face2)
                                (funcall separator-right face2 face1))))
             (concat (powerline-render lhs)
                     (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                     (powerline-render center)
                     (powerline-fill face1 (powerline-width rhs))
                     (powerline-render rhs)))))))
    (my-powerline-theme)))

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
    (defun on-off-fci-before-company (command)
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

(use-package expand-region
  :bind
  (("A-d" . er/expand-region)
   ("A-D" . er/contract-region)))

(use-package multiple-cursors
  :config
  (progn
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

(use-package ag)

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package swiper)

(use-package counsel
  :bind
  (("A-F" . counsel-ag))
  (("A-M-f" . counsel-projectile-ag)))

(use-package ivy
  :ensure hydra
  :config
  (progn
    (define-key ivy-minibuffer-map "\C-o"
      (defhydra soo-ivy (:hint nil :color pink)
        "
     Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
    ----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
     _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
     ^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
     _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
    "
        ;; arrows
        ("j" ivy-next-line)
        ("k" ivy-previous-line)
        ("l" ivy-alt-done)
        ("h" ivy-backward-delete-char)
        ("g" ivy-beginning-of-buffer)
        ("G" ivy-end-of-buffer)
        ("d" ivy-scroll-up-command)
        ("u" ivy-scroll-down-command)
        ("e" ivy-scroll-down-command)
        ;; actions
        ("q" keyboard-escape-quit :exit t)
        ("C-g" keyboard-escape-quit :exit t)
        ("<escape>" keyboard-escape-quit :exit t)
        ("C-o" nil)
        ("i" nil)
        ("TAB" ivy-alt-done :exit nil)
        ("C-j" ivy-alt-done :exit nil)
        ;; ("d" ivy-done :exit t)
        ("RET" ivy-done :exit t)
        ("C-m" ivy-done :exit t)
        ("f" ivy-call)
        ("c" ivy-toggle-calling)
        ("m" ivy-toggle-fuzzy)
        (">" ivy-minibuffer-grow)
        ("<" ivy-minibuffer-shrink)
        ("w" ivy-prev-action)
        ("s" ivy-next-action)
        ("a" ivy-read-action)
        ("t" (setq truncate-lines (not truncate-lines)))
        ("C" ivy-toggle-case-fold)
        ("o" ivy-occur :exit t)))
    (ivy-mode 1))
  :bind
  (("C-s" . swiper)
   ("A-f" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("A-o" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-x l" . counsel-locate)
   ("C-c C-r" . ivy-resume)
   ("A-b" . ivy-switch-buffer)))

(use-package projectile
  ;; :init
  ;; (setq projectile-enable-caching nil)
  :config
  (projectile-mode))

(use-package counsel-projectile
  :config
  (counsel-projectile-on)
  :bind
  (("A-p" . counsel-projectile-switch-to-buffer)
   ("A-P" . counsel-projectile)))

;; (use-package switch-buffer-functions
;;   :config
;;   (progn
;;     (defun my-header-line-tabs (prev cur)
;;       "Display projectile buffers in the header line"
;;       (setq header-line-format
;;         (seq-map-indexed
;;          (lambda (e i) (if (not (string= "*" (substring e 0 1)))
;;                          (concat "[" (number-to-string i) " " e "]")))
;;          (projectile-project-buffer-names))))
;;     (add-hook 'switch-buffer-functions 'my-header-line-tabs)))

(use-package dumb-jump
  :init
  (progn
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jumpx-prefer-searcher 'ag)
    (global-set-key (kbd "A-R") 'dumb-jump-quick-look)
    (global-set-key (kbd "A-.") 'dumb-jump-go)
    (global-set-key (kbd "A-r") 'dumb-jump-go-prompt))
  :config
  (dumb-jump-mode))

(use-package magit
  :bind
  (("C-x C-z" . magit-status)))

(use-package git-gutter+
  :config
  (global-git-gutter+-mode t))

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

(use-package parinfer
  ;; :ensure paredit
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package cider
  :config
  (setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")
  :bind
  (:map cider-mode-map
   ("A-<return>" . cider-eval-last-sexp)))

(use-package geiser
  :config
  (progn
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
    (put 'match 'scheme-indent-function 1)))


(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown"))

(use-package fish-mode)

(use-package nginx-mode
  :config
  (setq nginx-indent-level tab-width))

(use-package yaml-mode)

(use-package toml-mode)

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset tab-width))

(use-package js2-mode
  :config
  (progn
    (setq js2-basic-offset tab-width)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)))

(use-package dockerfile-mode)
