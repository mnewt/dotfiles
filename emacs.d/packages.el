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
(setq straight-use-package-by-default t)

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

(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; (use-package atom-one-dark-theme
;;   :load-path "straight/build/atom-one-dark-theme"
;;   :config
;;   (progn
;;     (load-theme 'atom-one-dark t)
;;     (if window-system
;;         (progn
;;           ;; blinking is NOT OK
;;           (blink-cursor-mode -1)
;;           (set-cursor-color "#A831A5"))
;;       ;; (custom-set-faces '(default ((t (:background "#21252B"))))
;;       ;;                   '(tabbar-default ((((class color) (min-colors 89)) (:inherit default :foreground "#2F343D" :background "#2F343D" :box (:line-width 6 :color "#2F343D" :style nil)))))
;;       ;;                   '(tabbar-button ((((class color) (min-colors 89)) (:height 1 :box (:line-width 6 :color "#2F343D" :style nil)))))
;;       ;;                   '(tabbar-unselected ((((class color) (min-colors 89)) (:inherit tabbar-button :background "#282C34"))))
;;       ;;                   '(tabbar-modified ((((class color) (min-colors 89)) (:inherit tabbar-unselected))))
;;       ;;                   '(tabbar-selected ((((class color) (min-colors 89)) (:inherit tabbar-button :background "#56B6C2" :box (:line-width 6 :color "#56B6C2" :style nil)))))
;;       ;;                   '(tabbar-separator ((((class color) (min-colors 89)) (:inherit tabbar-selected)))))
;;       (defvar atom-one-dark-colors-alist
;;         '(("atom-one-dark-accent"   . "#528BFF")
;;           ("atom-one-dark-fg"       . "#ABB2BF")
;;           ("atom-one-dark-bg"       . "gray14")
;;           ("atom-one-dark-bg-1"     . "gray13")
;;           ("atom-one-dark-bg-hl"    . "gray13")
;;           ("atom-one-dark-gutter"   . "#666D7A")
;;           ("atom-one-dark-accent"   . "#AEB9F5")
;;           ("atom-one-dark-mono-1"   . "#ABB2BF")
;;           ("atom-one-dark-mono-2"   . "#828997")
;;           ("atom-one-dark-mono-3"   . "#5C6370")
;;           ("atom-one-dark-cyan"     . "#56B6C2")
;;           ("atom-one-dark-blue"     . "#61AFEF")
;;           ("atom-one-dark-purple"   . "#C678DD")
;;           ("atom-one-dark-green"    . "#98C379")
;;           ("atom-one-dark-red-1"    . "#E06C75")
;;           ("atom-one-dark-red-2"    . "#BE5046")
;;           ("atom-one-dark-orange-1" . "#D19A66")
;;           ("atom-one-dark-orange-2" . "#E5C07B")
;;           ("atom-one-dark-gray"     . "#3E4451")
;;           ("atom-one-dark-silver"   . "#AAAAAA")
;;           ("atom-one-dark-black"    . "#0F1011"))
;;         "List of Atom One Dark colors."))))

;; (use-package monokai-theme
;;   :load-path "straight/build/monokai-theme"
;;   :config
;;   (load-theme 'monokai t))

(use-package dracula-theme
  :load-path "straight/build/dracula-theme"
  :config
  (load-theme 'dracula t))

(use-package powerline
  :init
  (progn
    (setq
     powerline-default-separator nil
     vc-handled-backends nil) 
    (defface my-inactive1 '((t (:background "#0E111E" :foreground "#666666" :inherit mode-line-inactive)))
      "Powerline inactive face 1."
      :group 'powerline)
    (defface my-active1 '((t (:background "#545663" :foreground "#c6c6cb" :inherit default)))
      "Powerline active face 1."
      :group 'powerline)
    (set-face-attribute 'mode-line nil
                        :box nil))
  :config
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'my-active1 'my-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (lhs (list (powerline-raw "%*" face1 'l)
                                     (powerline-buffer-id `(mode-line-buffer-id ,face1) 'l)
                                     (powerline-raw " " face1)))
                          (rhs (if active
                                   (list (powerline-raw global-mode-string face1 'r)
                                         (powerline-raw " " face1)
                                         (powerline-raw "%l" face1 'r)
                                         (powerline-raw ":" face1)
                                         (powerline-raw "%c" face1 'r)
                                         (powerline-hud face2 face1))))
                          (center (list (powerline-raw " " face1)
                                        (powerline-major-mode face1 'l)
                                        ;; (powerline-process face2)
                                        (powerline-raw " " face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

;; (use-package switch-buffer-functions
;;   :config
;;   (progn
;;     (defun projectile-header-line-tabs (prev cur)
;;       "Display projectile buffers in the header line"
;;       (if (not (boundp 'projectile-header-line-tabs-list))
;;           (setq projectile-header-line-tabs-list '()))
;;       (let* ((buffer-names (projectile-project-buffer-names))
;;              (tabs-list (seq-intersection projectile-header-line-tabs-list buffer-names))
;;              (name (buffer-name))
;;              (active (powerline-selected-window-active)))
;;         (setq projectile-header-line-tabs-list
;;          (append tabs-list (seq-difference buffer-names tabs-list))))

;;       (setq header-line-format
;;             (seq-map (lambda (s) (concat "[ " s " ]"))
;;                      projectile-header-line-tabs-list)))
;;     (add-hook 'switch-buffer-functions 'projectile-header-line-tabs)))

(use-package help-macro+)

(use-package help+)

(use-package help-fns+)

(use-package help-mode+)

(use-package menu-bar+)

;; (defun tabbar-first-buffer ()
;;   (interactive)
;;   (tabbar-select-tab (first (tabbar-view (tabbar-current-tabset))) (tabbar-current-tabset)))

;; (use-package tabbar
;;   :config
;;   (progn
;;     (tabbar-mode 1)
;;     (setq tabbar-use-images nil)
;;     (set-face-attribute
;;      'tabbar-default nil
;;      :background "gray20"
;;      :foreground "gray20"
;;      :box '(:line-width 1 :color "gray20" :style nil))
;;     (set-face-attribute
;;      'tabbar-unselected nil
;;      :background "gray30"
;;      :foreground "white"
;;      :box '(:line-width 5 :color "gray30" :style nil))
;;     (set-face-attribute
;;      'tabbar-selected nil
;;      :background "gray75"
;;      :foreground "black"
;;      :box '(:line-width 5 :color "gray75" :style nil))
;;     (set-face-attribute
;;      'tabbar-highlight nil
;;      :background "white"
;;      :foreground "black"
;;      :underline nil
;;      :box '(:line-width 5 :color "white" :style nil))
;;     (set-face-attribute
;;      'tabbar-button nil
;;      :box '(:line-width 1 :color "gray20" :style nil))
;;     (set-face-attribute
;;      'tabbar-separator nil
;;      :background "gray20"
;;      :height 1)

;;     ;; Change padding of the tabs
;;     ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
;;     (custom-set-variables
;;      '(tabbar-separator (quote (0.5))))
;;     ;; adding spaces
;;     (defun tabbar-buffer-tab-label (tab)
;;       "Return a label for TAB.
;;     That is, a string used to represent it on the tab bar."
;;       (let ((label  (if tabbar--buffer-show-groups
;;                         (format "[%s]  " (tabbar-tab-tabset tab))
;;                       (format "%s  " (tabbar-tab-value tab)))))
;;         ;; Unless the tab bar auto scrolls to keep the selected tab
;;         ;; visible, shorten the tab label to keep as many tabs as possible
;;         ;; in the visible area of the tab bar.
;;         (if tabbar-auto-scroll-flag
;;             label
;;           (tabbar-shorten
;;            label (max 1 (/ (window-width)
;;                            (length (tabbar-view
;;                                     (tabbar-current-tabset)))))))))
;;    (tabbar-mode 1))
;;   :bind
;;   (("s-1" . tabbar-first-buffer)))

;; (use-package all-the-icons
;;   :config
;;   (all-the-icons-install-fonts))

(use-package which-key
  :config
  (which-key-mode)
  :bind
  (("M-s-˙" . which-key-show-top-level)
   ("M-s-h" . which-key-show-top-level)))

(use-package ace-window
  :bind
  (("M-p" . ace-window)))

(use-package tile
  :config
  (setq tile-cycler
        (tile-strategies :strategies
                         (list (tile-split-n-tall 2)
                               tile-tall
                               (tile-argument-buffer-fetcher :layout tile-master-left)
                               tile-one))))

(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 5)
  :config
  (smooth-scrolling-mode 1))

(use-package mwim
  :bind
  (("M-<left>" . mwim-beginning-of-code-or-line)
   ("M-<right>" . mwim-end-of-code-or-line)))

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

(use-package direx
  :bind
  (("C-x C-j" . direx:jump-to-directory)
   ("M-s-<up>". direx:jump-to-directory)
   ("s-\\" . direx:jump-to-directory)))

(use-package expand-region
  :init
  (setq expand-region-fast-keys-enabled nil)
  :bind
  (("s-d" . er/expand-region)
   ("s-D" . er/contract-region)))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("M-s-m" . mc/edit-lines)
   ("M-s-µ" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("M-s-a"  . mc/mark-all-like-this)
   ("M-s-å"  . mc/mark-all-like-this)))

(use-package origami
  :config
  (global-origami-mode)
  :bind
  (("s-{" . origami-close-node-recursively)
   ("s-}" . origami-open-node-recursively)
   ("s-|" . origami-recursively-toggle-node)
   ("s-+" . origami-show-only-node)
   ("s-=" . origami-open-all-nodes)))

(use-package replace+)

(use-package wgrep)

(use-package wgrep-ag)

(use-package rg
  :config
  (progn
    (rg-enable-default-bindings (kbd "C-r"))
    (add-hook 'rg-mode-hook 'wgrep-ag-setup)))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package ivy-hydra)

(use-package ivy
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          enable-recursive-minibuffers t))
  :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("s-b" . ivy-switch-buffer)
   ("s-B" . ivy-switch-buffer-other-window)
   :map ivy-switch-buffer-map
   ("C-k" . (lambda ()
              (interactive)
              (ivy-set-action 'kill-buffer)
              (ivy-done)))))

(use-package swiper)

(use-package counsel
  :init
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :bind
  (("s-F" . counsel-rg)
   ("s-f" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("s-o" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-x l" . counsel-locate)
   ("M-s-v" . counsel-yank-pop)
   ("M-s-√" . counsel-yank-pop)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package projectile
  :init
  (setq frame-title-format
        '(""
          ;; "%b"
          (:eval
           (when (fboundp 'projectile-project-name)
             (let ((project-name (projectile-project-name)))
               (unless (string= "-" project-name)
                 (format "[%s]" project-name)))))))
  :config
  (projectile-mode)
  :bind
  (("M-s-f" . counsel-projectile-rg)
   ("M-s-ƒ" . counsel-projectile-rg)))

(use-package counsel-projectile
  :init
  (setq counsel-projectile-remove-current-buffer t
        counsel-projectile-remove-current-project t)
  :config
  (counsel-projectile-mode)
  :bind
  (("M-s-p" . counsel-projectile-switch-to-buffer)
   ("M-s-π" . counsel-projectile-switch-to-buffer)
   ("s-p" . counsel-projectile)
   ("s-P" . counsel-projectile-switch-project)
   ("s-t" . counsel-imenu)))

(use-package imenu-anywhere
  :bind
  (("s-r" . ivy-imenu-anywhere)))

(use-package dumb-jump
  :init
  (progn
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-prefer-searcher 'rg))
  :config
  (dumb-jump-mode)
  :bind
  (("s-j" . dumb-jump-go-prompt)
   ("s-." . dumb-jump-go)
   ("s-J" . dumb-jump-quick-look)))

(use-package vkill
  :bind
  (("C-x p" . vkill)))

(use-package magit
  :bind
  (("C-x C-z" . magit-status)))

(use-package git-timemachine)

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package gist)

(use-package git-link)

(use-package git-gutter+
  :config
  (global-git-gutter+-mode t))

;; (use-package goto-last-change
;;   :bind
;;   (("M-," . goto-last-change)))

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
  (("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-d" . company-show-doc-buffer)))

;; (use-package yasnippet
;;   :config
;;   (yas-global-mode 1))

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
      (yas-minor-mode 1)
      ;; This choice of keybinding leaves cider-macroexpand-1 unbound
      (cljr-add-keybindings-with-prefix "C-c C-m"))
    (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)))

(defun sp-sh-post-handler (id action context)
  "Bash post handler.
ID, ACTION, CONTEXT."
  (-let (((&plist :arg arg :enc enc) sp-handler-context))
    (when (equal action 'barf-backward)
      (sp-ruby-delete-indentation 1)
      (indent-according-to-mode)
      (save-excursion
        (sp-backward-sexp) ; move to begining of current sexp
        (sp-backward-sexp arg)
        (sp-ruby-maybe-one-space)))

    (when (equal action 'barf-forward)
      (sp-get enc
        (let ((beg-line (line-number-at-pos :beg-in))
              (end-line (line-number-at-pos :end-in)))
          (sp-forward-sexp arg)
          (sp-ruby-maybe-one-space)
          (when (not (= (line-number-at-pos) beg-line))
            (sp-ruby-delete-indentation -1))
          (indent-according-to-mode))))))

(defun sp-sh-block-post-handler (id action context)
  "Handler for bash block insertions.
ID, ACTION, CONTEXT."
  (when (equal action 'insert)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))
  (sp-sh-post-handler id action context))

(defun sp-sh-pre-handler (id action context)
  "Handler for bash slurp and barf.
ID, ACTION, CONTEXT."
  (let ((enc (plist-get sp-handler-context :enc)))
    (sp-get enc
      (let ((beg-line (line-number-at-pos :beg-in))
            (end-line (line-number-at-pos :end-in)))

        (when (equal action 'slurp-backward)
          (save-excursion
            (sp-forward-sexp)
            (when (looking-at-p ";") (forward-char))
            (sp-ruby-maybe-one-space)
            (when (not (= (line-number-at-pos) end-line))
              (sp-ruby-delete-indentation -1)))
          (while (thing-at-point-looking-at "\\.[[:blank:]\n]*")
            (sp-backward-sexp))
          (when (looking-back "[@$:&?!]")
            (backward-char)
            (when (looking-back "[@&:]")
              (backward-char)))
          (just-one-space)
          (save-excursion
            (if (= (line-number-at-pos) end-line)
                (insert " ")
              (newline))))

        (when (equal action 'barf-backward)
          ;; Barf whole method chains
          (while (thing-at-point-looking-at "[(.:[][\n[:blank:]]*")
            (sp-forward-sexp))
          (if (looking-at-p " *$")
              (newline)
            (save-excursion (newline))))

        (when (equal action 'slurp-forward)
          (save-excursion
            (sp-backward-sexp)
            (when (looking-back "\.") (backward-char))
            (sp-ruby-maybe-one-space)
            (when (not (= (line-number-at-pos) beg-line))
              (if (thing-at-point-looking-at "\\.[[:blank:]\n]*")
                  (progn
                    (forward-symbol -1)
                    (sp-ruby-delete-indentation -1))
                (sp-ruby-delete-indentation))))
          (while (looking-at-p "::") (sp-forward-symbol))
          (when (looking-at-p "[?!;]") (forward-char))
          (if (= (line-number-at-pos) beg-line)
              (insert " ")
            (newline)))

        (when (equal action 'barf-forward)
          (when (looking-back "\\.") (backward-char))
          (while (looking-back "::") (sp-backward-symbol))
          (if (= (line-number-at-pos) end-line)
              (insert " ")
            (if (looking-back "^[[:blank:]]*")
                (save-excursion (newline))
              (newline))))))))

(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)

    ;; https://github.com/Fuco1/smartparens/issues/80
    (defun my-create-newline-and-enter-sexp (&rest _ignored)
      "Open a new brace or bracket expression, with relevant newlines and indent. "
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode))

    (sp-with-modes
     '(sh-mode js2-mode javascript-mode)
     (sp-local-pair "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

    (sp-with-modes
     '(sh-mode)
     (sp-local-pair "do" "done"
                    :when '(("SPC" "RET" "<evil-ret>"))
                    :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                    :actions '(insert navigate)
                    :pre-handlers '(sp-sh-pre-handler)
                    :post-handlers '(sp-sh-block-post-handler))
     (sp-local-pair "then" "fi"
                    :when '(("SPC" "RET" "<evil-ret>"))
                    :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                    :actions '(insert navigate)
                    :pre-handlers '(sp-sh-pre-handler)
                    :post-handlers '(sp-sh-block-post-handler))
     (sp-local-pair "case" "esac"
                    :when '(("SPC" "RET" "<evil-ret>"))
                    :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                    :actions '(insert navigate)
                    :pre-handlers '(sp-sh-pre-handler)
                    :post-handlers '(sp-sh-block-post-handler)))
    (smartparens-global-mode))

  :bind
  (("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("M-C-k" . sp-kill-sexp)))

;; (use-package aggressive-indent
;;   :config
;;   (progn
;;     (global-aggressive-indent-mode 1)
;;     (add-to-list 'aggressive-indent-excluded-modes 'html-mode)))

(use-package paren-face
  :config
  (progn
    (set-face-foreground 'parenthesis "#5C6370")))

;; (use-package paredit
;;   :init
;;   (progn
;;     (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;     (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;;     (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;     (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;;     (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;;     (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;     (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;;     (eldoc-add-command
;;      'paredit-backward-delete
;;      'paredit-close-round)))

(use-package parinfer
  ;; :ensure paredit
  :init
  (progn
    (setq parinfer-extensions
          '(defaults        ; should be included.
             pretty-parens  ; different paren styles for different modes.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))     ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode))
  :bind
  (("C-," . parinfer-toggle-mode)))

(use-package cider
  ;; "(do (require '[figwheel-sidecar.repl-api :as fsra])
  ;;      (fsra/start-figwheel!)
  ;;      (fsra/cljs-repl))"
  :bind
  (:map cider-mode-map
        ("s-<return>" . cider-eval-last-sexp)))

(use-package inf-clojure
  :config
  (defun inf-clojure-start-lumo ()
    (interactive)
    ;; (setq inf-clojure-lein-cmd "lumo -d")
    (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode
              (inf-clojure-minor-mode)
              (inf-clojure "lumo -d")))
  :bind
  (:map inf-clojure-minor-mode-map
        ("s-<return>" . inf-clojure-eval-last-sexp)))

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

(use-package dockerfile-mode)

(use-package docker
  :config
  (docker-global-mode))

(use-package docker-tramp)

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown"))

(use-package fish-mode)

(use-package fish-completion)

(use-package nginx-mode
  :config
  (setq nginx-indent-level tab-width))

(use-package yaml-mode)

(use-package toml-mode)

(use-package web-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (setq web-mode-markup-indent-offset tab-width
          web-mode-css-indent-offset tab-width
          web-mode-code-indent-offset tab-width
          web-mode-enable-current-element-highlight t
          web-mode-enable-current-column-highlight t)))

(use-package js2-mode
  :config
  (progn
    (setq js2-basic-offset tab-width)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)))

(use-package rjsx-mode)

(use-package robe
  :ensure company
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package inf-ruby
  :bind
  (("C-c r r" . inf-ruby)))

(use-package lua-mode)

(use-package sass-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.sss\\'" . sass-mode)))
