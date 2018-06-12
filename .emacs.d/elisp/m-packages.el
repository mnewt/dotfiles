;;; m-packages.el --- Summary --- -*- lexical-binding: t -*-

;;; Commentary:
;;; Here is the configuration for most packages.

;;; Code:

(use-package help-macro+
  :defer 1)

(use-package help+
  :defer 1)

(use-package help-fns+
  :defer 1)

(use-package help-mode+
  :defer 1)

(use-package helpful
  :defer 2
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-h" . helpful-at-point)))

(use-package tldr
  :init
  (global-unset-key (kbd "C-h t"))
  :bind
  (("C-h t t" . tldr)
   ("C-h t u" . tldr-update-docs)))

(use-package info-colors
  :defer 1
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(use-package menu-bar+
  :defer 1)

(use-package which-key
  :defer 1
  :config
  (which-key-mode t)
  :bind
  (("M-s-h" . which-key-show-top-level)))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60)
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package server
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(use-package undo-tree
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode)
  :bind
  (("s-z" . undo-tree-undo)
   ("s-Z" . undo-tree-redo)
   ("s-y" . undo-tree-redo)
   ("M-s-z" . undo-tree-visualize)))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;; (use-package volatile-highlights
;;   :config
;;   (volatile-highlights-mode t))

;; (use-package ace-jump-mode
;;   :bind
;;   (("C-c SPC" . ace-jump-mode)
;;    ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package winner
  :init
  (winner-mode)
  :bind
  (("C-c [" . winner-undo)
   ("s-[" . winner-undo)
   ("C-c ]" . winner-redo)
   ("s-]" . winner-redo)))

(use-package ace-jump-zap
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("C-M-z" . ace-jump-zap-to-char-dwim)))

(use-package ace-window
  :bind
  (("M-o" . ace-window)))

(use-package buffer-move
  :bind
  (("C-H-W" . buf-move-up)
   ("C-H-S" . buf-move-down)
   ("C-H-A" . buf-move-left)
   ("C-H-D" . buf-move-right)))

(use-package mwim
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)
   :map mac-key-mode-map
   ("s-<right>" . mwim-end-of-code-or-line)
   ("s-<left>" . mwim-beginning-of-code-or-line)))

(use-package dired+
  :init
  (defun dired-to-default-directory ()
    "Open directory containing the current file"
    (interactive)
    (dired default-directory))
  
  (defun dired-open-file ()
    "Open file at point in OS default program"
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "open" nil 0 nil file)))
  :config
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-listing-switches "-alh"
        dired-dwim-target t)
  :bind
  (("C-x C-d" . dired-to-default-directory)
   ("C-x d" . dired)
   :map dired-mode-map
   ("C-c o" . dired-open-file)
   ("C-x f" . find-file-literally-at-point)))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

(use-package direx
  :bind
  (("C-x C-j" . direx:jump-to-directory)))

(straight-register-package
 '(vkill :repo "https://github.com/emacsattic/vkill.git"))

(use-package vkill
  :bind
  (("C-c t" . vkill)))

(use-package proc-net
  :bind
  (("C-c n" . list-network-processes)))

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
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-c C-a"  . mc/mark-all-dwim)))

;; (use-package mc-extras
;;   :bind
;;   (:map mc/keymap
;;         ("C-. M-C-f" . mc/mark-next-sexps)
;;         ("C-. M-C-b" . mc/mark-previous-sexps)
;;         ("C-. <" . mc/mark-all-above)
;;         ("C-. >" . mc/mark-all-below)
;;         ("C-. C-d" . mc/remove-current-cursor)
;;         ("C-. C-k" . mc/remove-cursors-at-eol)
;;         ("C-. d" . mc/remove-duplicated-cursors)
;;         ("C-. C-." . mc/freeze-fake-cursors-dwim)
;;         ("C-. ." . mc/move-to-column)
;;         ("C-. =" . mc/compare-chars)))

(use-package origami
  :bind
  (:map origami-mode-map
        ("M-s-]" . origami-close-node-recursively)
        ("M-s-[" . origami-open-node-recursively)
        ("s-|" . origami-recursively-toggle-node)
        ("s-+" . origami-show-only-node)
        ("s-=" . origami-open-all-nodes)))

(use-package wgrep
  :bind
  (("C-c C-p" . wgrep-change-to-wgrep-mode)))

;; some rg features require this
(use-package wgrep-ag)

(use-package rg
  :config
  (progn
    (rg-enable-default-bindings (kbd "C-r"))
    (add-hook 'rg-mode-hook 'wgrep-ag-setup)))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends
        '(company-capf company-gtags company-css company-elisp company-keywords
                       company-semantic company-yasnippet company-files
                       company-dabbrev-code company-dabbrev company-ispell))
  :bind
  (:map prog-mode-map
        ("<tab>" . company-indent-or-complete-common)
        :map company-active-map
        ;; ("C-n" . company-select-next)
        ;; ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer)
        ("M-." . company-show-location)))

(use-package company-shell
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell)))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)
        ;; ivy-use-virtual-buffers t))
  :bind
  (("C-c C-r" . ivy-resume)
   ("s-b" . ivy-switch-buffer)
   ("s-B" . ivy-switch-buffer-other-window)
   :map ivy-switch-buffer-map
   ("C-k" . (lambda ()
              (interactive)
              (ivy-set-action 'kill-buffer)
              (ivy-done)
              (ivy-resume)))))

(use-package ivy-hydra
  :defer 1)

(use-package swiper
  :init
  (defun replace-regexp-entire-buffer (pattern replacement)
    "Perform regular-expression replacement throughout buffer."
    (interactive
     (let ((args (query-replace-read-args "Replace in entire buffer" t)))
       (setcdr (cdr args) nil)    ; remove third value returned from query---args
       args))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (replace-match replacement))))

  (defun ivy--replace-regexp-entire-buffer (replacement)
    (interactive (list (read-from-minibuffer (concat "Replace all occurrences of `" ivy--old-re "\' in entire buffer: "))))
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (replace-regexp-entire-buffer ivy--old-text replacement))
    (ivy-done))
    ;; This isn't the last message displayed so there isn't really a point in displaying it at all
    ;; (message (concat "Replaced `" ivy--old-text "\' with `" replacement"\' across entire buffer.")))

  :bind
  (("C-s" . swiper)
   ("s-5" . replace-regexp-entire-buffer)
   :map ivy-minibuffer-map
   ("s-5" . ivy--replace-regexp-entire-buffer)))

(use-package counsel
  :custom
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :bind
  (("s-F" . counsel-rg)
   ("s-f" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h <tab>" . counsel-info-lookup-symbol)
   ("C-c u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c l" . counsel-locate)
   ("M-s-v" . counsel-yank-pop)
   ("M-Y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line-and-call)
   :map mac-key-mode-map
   ("s-f" . counsel-grep-or-swiper)
   ("s-o" . counsel-find-file)))

(use-package projectile
  :init
  (defun projectile-load-settings (file)
    "https://github.com/jfeltz/projectile-load-settings/blob/master/projectile-load-settings.el
    Load project elisp settings file if they are found in active project root
    directory, or if in the case of undefined root directory, file is
    otherwise path resolvable."
    (interactive "sEnter elisp filename (in project root): ")
    (let
        ((p (concat (projectile-project-root) file)))
      (when (file-exists-p p)
        (load p)
        (message "%s" (concat "Loaded project settings from: " file)) nil)))
  (setq frame-title-format
        '(""
          ;; "%b"
          (:eval
           (when (fboundp 'projectile-project-name)
             (let ((project-name (projectile-project-name)))
               (unless (string= "-" project-name)
                 (format "[%s]" project-name))))))
        projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  :hook
  ((projectile-switch-project . (lambda () (projectile-load-settings "settings.el"))))
  :bind
  (("M-s-f" . counsel-projectile-rg)))

(use-package counsel-projectile
  :init
  (setq counsel-projectile-remove-current-buffer t
        counsel-projectile-remove-current-project t)
  :config
  (counsel-projectile-mode)
  :bind
  (("M-s-p" . counsel-projectile-switch-to-buffer)
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
   ("s-<mouse-1>". dumb-jump-go)
   ("s-J" . dumb-jump-quick-look)))

(use-package magit
  :init
  ;; https://github.com/magit/magit/issues/460#issuecomment-36139308
  (defun git-worktree-link (gitdir worktree)
    (require 'magit)
    (interactive (list (read-directory-name "Gitdir: ")
                     (read-directory-name "Worktree: ")))
    (with-temp-file (expand-file-name ".git" worktree)
      (insert "gitdir: " (file-relative-name gitdir worktree) "\n"))
    (magit-call-git "config" "-f" (expand-file-name "config" gitdir)
                    "core.worktree" (file-relative-name worktree gitdir))
    ;; Configure projectile to only look at tracked files
    (if (boundp 'projectile-git-command)
      (setq projectile-git-command "git ls-files -zc --exclude-standard"))
    (message "Linked worktree: %s from gitdir: %s" worktree gitdir))

  (defun git-worktree-unlink (gitdir worktree)
    (interactive (list (read-directory-name "Gitdir: ")
                       (read-directory-name "Worktree: ")))
    ;; Configure projectile back to default
    (if (boundp 'projectile-git-command)
      (setq projectile-git-command "git ls-files -zco --exclude-standard"))
    ;; This does `git config --unset core.worktree'.  We don't actually
    ;; have to do this and not doing it would have some advantages, but
    ;; might be confusing.
    ;; (magit-set nil "core.worktree")
    ;; This causes an error if this actually is a directory, which is
    ;; a good thing, it saves us from having to do this explicitly :-)
    (delete-file (expand-file-name ".git" worktree)))

  (setq git-home-repo-dir "~/.config/repos")

  (defun git-home-link (repo)
    (interactive (list (completing-read "Link git home repository: "
                                        (directory-files "~/.config/repos" nil "^[^.]")
                                        nil t)))
    (setq repo (expand-file-name repo git-home-repo-dir))
    ;; "Fix" repositories that were created with --bare.
    ;; (let ((default-directory (file-name-as-directory repo)))
    ;;   (magit-set "false" "core.bare"))
    ;; Regular link.
    (git-worktree-link repo (getenv "HOME")))

  (defun git-home-unlink ()
    (interactive)
    (git-worktree-unlink (with-temp-buffer
                           (insert-file-contents (expand-file-name ".git" (getenv "HOME")))
                           (re-search-forward "gitdir: \\(.+\\)")
                           (expand-file-name (match-string 1)))
                         (getenv "HOME")))

  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :bind
  (("C-x g" . magit-status)
   ("C-x C-g" . magit-dispatch-popup)))

;; (use-package git-timemachine
;;   :bind
;;   (("C-x t" . git-timemachine)))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/code"))

(use-package gist
  :bind
  (("C-x M-g" . gist-list)))

(use-package git-link
  :defer 1)

(use-package diff-hl
  :defer 1
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  ((prog-mode markdown-mode) . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode))

;; (Use-Package git-gutter
;;   :config
;;   (global-git-gutter-mode +1)
;;   :bind
;;   (("C-x C-g" . git-gutter)
;;    ("C-x v =" . git-gutter:popup-hunk)
;;    ;; Jump to next/previous hunk
;;    ("C-x p" . git-gutter:previous-hunk)
;;    ("C-x n" . git-gutter:next-hunk)
;;    ;; Stage current hunk
;;    ("C-x v s" . git-gutter:stage-hunk)
;;    ;; Revert current hunk
;;    ("C-x v r" . git-gutter:revert-hunk)
;;    ;; Mark current hunk
;;    ("C-x v SPC" . git-gutter:mark-hunk)))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; (use-package goto-last-change
;;   :bind
;;   (("M-," . goto-last-change)))

(use-package sly
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  :bind
  (:map sly-prefix-map
        ("M-h" . sly-documentation-lookup)))

(use-package sly-company
  :hook
  (sly-mode . sly-company-mode)
  :config
  (add-to-list 'company-backends 'sly-company))

;; (use-package yasnippet
;;   :config
;;   (yas-global-mode 1))

(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode))
  :config
  (add-to-list 'interpreter-mode-alist '("inlein" . clojure-mode))
  (progn
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
      (cider-eval-last-sexp '(1))))
  :hook
  (clojure-mode . turn-on-eldoc-mode)
  (clojurescript-mode . turn-on-eldoc-mode))

(use-package clojure-mode-extra-font-locking
  :defer 1)

(use-package smartparens
  :init
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
  :config
  (progn
    (require 'smartparens-config)
    (setq sp-base-key-bindings 'paredit)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol nil)
    (sp-use-paredit-bindings)

    (show-smartparens-global-mode +1)
    
    ;; https://github.com/Fuco1/smartparens/issues/80
    (defun m-create-newline-and-enter-sexp (&rest _ignored)
      "Open a new brace or bracket expression, with relevant newlines and indent. "
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode))

    (sp-with-modes
        '(sh-mode js2-mode javascript-mode)
      (sp-local-pair "{" nil :post-handlers '((m-create-newline-and-enter-sexp "RET"))))

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
                     :post-handlers '(sp-sh-block-post-handler))))
  :hook
  ((prog-mode conf-mode markdown-mode eshell-mode text-mode) . turn-on-smartparens-mode))

;; Trying out paredit with these 3 packages
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

;; (use-package aggressive-indent
;;   :config
;;   (progn
;;     (global-aggressive-indent-mode 1)
;;     (add-to-list 'aggressive-indent-excluded-modes 'html-mode)))

;; (use-package paren-face
;;   :config
;;   (progn
;;     (set-face-foreground 'parenthesis "#5C6370")))

(use-package parinfer
  :init
  (defun m-parinfer-yank-advice (f &rest args)
    "Make parinfer yanking work OK even when smartparens is enabled"
    (let ((current-mode parinfer--mode))
      ;; Insert something to make sure indent-mode puts parens after point 
      (insert ".")
      (parinfer--invoke-parinfer-instantly (point))
      (parinfer--switch-to-paren-mode)
      (parinfer-backward-delete-char)
      (apply f args)
      (if (eq 'indent current-mode)
          (parinfer--switch-to-indent-mode))))
  
  :custom
  (parinfer-extensions 
   '(defaults        ; should be included.
      pretty-parens   ; different paren styles for different modes.
      smart-tab       ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank))    ; Yank behavior depend on mode.
  
  :config
  (parinfer-strategy-add 'default 'newline-and-indent)

  (advice-add 'parinfer-smart-yank:yank :around #'m-parinfer-yank-advice)
  
  :hook
  ((clojure-mode common-lisp-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode) . parinfer-mode)
  
  :bind
  (:map parinfer-mode-map
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)
        ("C-i" . parinfer--reindent-sexp)
        ("C-M-i" . parinfer-auto-fix)
        ("C-," . parinfer-toggle-mode)
        ;; Don't interfere with smartparens quote handling
        ("\"" . nil)
        ;; Override mac-key-mode
        ("s-v" . parinfer-smart-yank:yank)
        :map parinfer-region-mode-map
        ("C-i" . indent-for-tab-command)
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)))

(use-package dash-at-point
  :config
  (progn
    (add-to-list 'dash-at-point-mode-alist '(clojure-mode . "clojuredocs"))
    (add-to-list 'dash-at-point-mode-alist '(clojurec-mode . "clojuredocs"))
    (add-to-list 'dash-at-point-mode-alist '(clojurescript-mode . "clojuredocs"))
    (add-to-list 'dash-at-point-mode-alist '(sh-mode . "bash"))
    (add-to-list 'dash-at-point-mode-alist '(fish-mode . "fish"))
    (add-to-list 'dash-at-point-mode-alist '(lisp-mode . "lisp"))
    (add-to-list 'dash-at-point-mode-alist '(slime-repl-mode . "lisp"))
    (add-to-list 'dash-at-point-mode-alist '(lisp-interaction-mode . "elisp"))
    (add-to-list 'dash-at-point-mode-alist '(inferior-emacs-lisp-mode . "elisp")))
  :bind
  (("C-c C-d" . dash-at-point)))

;; (use-package counsel-dash
;;   :custom
;;   (counsel-dash-docsets-path "~/Dash/DocSets")
;;   (counsel-dash-common-docsets '("bash")))

(use-package pcre2el
  :defer 1
  :config
  (rxt-global-mode t)
  (pcre-mode t))

(use-package visual-regexp-steroids
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)
   ("C-c m" . vr/mc-mark)))

(use-package elisp-format
  :defer 2)

;; https://github.com/raxod502/straight.el#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(straight-use-package 'org)

(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(use-package cider
  :config
  (add-hook 'cider-repl-mode-hook (lambda () (company-mode nil)))
  :bind
  (:map cider-mode-map
        ("s-<return>" . cider-eval-last-sexp)))

(use-package inf-clojure
  :config
  (defun inf-clojure-start-lumo ()
    (interactive)
    (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode
              (inf-clojure-minor-mode)
              (inf-clojure "lumo -d")))
  (defun reinstate-comint-simple-send ()
    (unless inf-clojure-minor-mode
      (setq-local comint-send-input 'comint-simple-send)))
  :hook
                                        ; `inf-clojure' seems to clobber `comint-send-input' on all comint buffers
  (comint-mode . reinstate-comint-simple-send)
  :bind
  (:map inf-clojure-minor-mode-map
        ("s-<return>" . inf-clojure-eval-last-sexp)
        ("C-c C-k" . inf-clojure-eval-buffer)))

(use-package geiser
  :defer 2
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

(use-package rainbow-mode
  :hook
  ((sass-mode emacs-lisp-mode) . rainbow-mode))
(use-package highlight-escape-sequences
  :defer 1
  :config
  (turn-on-hes-mode))

(use-package hl-todo
  :defer 2
  :config
  (global-hl-todo-mode))

(use-package markdown-mode
  :mode "\\.md\\|markdown\\'"
  :init
  (setq markdown-command "multimarkdown"))

(use-package pinentry
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  (pinentry-start))

(use-package tramp
  :config
  (defun ssh-sudo (path)
    "ssh to host, sudo to root, open dired"
    (interactive "M [User@] Hostname: ")
    (let* ((at (string-match "@" path))
           (colon (string-match ":" path))
           (user (if at
                     (concat (substring path 0 at) "@")
                   ""))
           (host (if at
                     (substring path (+ 1 at))
                   path))
           (dir (if colon
                    (substring path (+ 1 colon))
                  ":/")))
      (find-file (concat "/sshx:" user host "|sudo:root@" host dir))))

  (defun sudo-toggle--add-sudo (f)
    "Add sudo to file path string"
    (if (file-remote-p f)
        (with-parsed-tramp-file-name (expand-file-name f) nil
          (concat "/" method ":"
                  (when user (concat user "@"))
                  host "|sudo:root@" host ":" localname))
      (concat "/sudo:root@localhost:" (expand-file-name f))))

  (defun sudo-toggle--remove-sudo (f)
    "Remove sudo from file path string"
    (cond
     ((string-match-p "/sudo:root@localhost:" f)
      (replace-regexp-in-string (getenv "HOME") "~" (substring f 21)))

     ((string-match-p "|sudo:root@" f)
      (replace-regexp-in-string "|sudo:root@[^:]*" "" f))))

  (defun sudo-toggle ()
    "Reopen the current file, directory, or shell as root.  For
files and dired buffers, the non-sudo buffer is replaced with a
sudo buffer.  For shells, a sudo shell is opened but the non-sudo
shell is left intact."
    (interactive)
    (let* ((position (point))
           (f (expand-file-name (or buffer-file-name default-directory)))
           (newf (if (string-match-p "sudo:" f)
                     (sudo-toggle--remove-sudo f)
                   (sudo-toggle--add-sudo f)))
           ;; so that you don't get method overrides
           (tramp-default-proxies-alist nil))
      (cond ((or buffer-file-name (derived-mode-p 'dired-mode))
             (find-alternate-file newf)
             (goto-char position))
            ((derived-mode-p 'shell-mode)
             (if (string-match-p "*shell/sudo:root@" (buffer-name))
                 (kill-buffer-and-window)
               (with-temp-buffer
                 (cd newf)
                 (shell (format "*shell/sudo:root@%s*"
                                (with-parsed-tramp-file-name newf nil host))))))
            ((derived-mode-p 'eshell-mode)
             (eshell-return-to-prompt)
             (insert (concat "cd " newf))
             (eshell-send-input))
            (t (message "Can't sudo this buffer")))))
  
  ;; Disable file accesses when visiting buffers accessed via tramp for performance reasons
  (defun disable-file-accesses ()
    (when (file-remote-p default-directory)
      (setq-local projectile-mode-line "Projectile")
      (setq-local company-backends (remove 'company-files company-backends))))

  :hook
  (find-file . disable-file-accesses))

(use-package xterm-color
  :hook
  ((shell-mode . (lambda ()
                   (add-hook 'comint-preoutput-filter-functions
                             'xterm-color-filter nil t)))))

(use-package eterm-256color
  :hook
  (term-mode . eterm-256color-mode))

(use-package eshell
  :init
  (setenv "CLICOLOR" "1")
  (setenv "LSCOLORS" "ExFxCxDxBxegedabagacad")
  (setenv "LS_COLORS" "di=36:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34:su=0:sg=0:tw=0:ow=0:")
  (setenv "PAGER" "cat")
  (setenv "MANPAGER" "cat")
  (setenv "BOOT_JVM_OPTIONS" "-client -XX\:+TieredCompilation -XX\:TieredStopAtLevel\=1 -Xmx2g -XX\:+UseConcMarkSweepGC -XX\:+CMSClassUnloadingEnabled -Xverify\:none -XX\:+AggressiveOpts")

  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that directory."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           ;; `eshell' uses this variable as the new buffer name
           (eshell-buffer-name (concat "*eshell: " (car (last (split-string parent "/" t))) "*")))
      (unless (derived-mode-p 'eshell-mode)
        (switch-to-buffer-other-window "*eshell-here-temp*")
        (eshell)
        (kill-buffer "*eshell-here-temp*")
        (insert (concat "ls"))
        (eshell-send-input))))

  (defun eshell-maybe-bol ()
    (interactive)
    (let ((p (point)))
      (eshell-bol)
      (if (= p (point))
          (beginning-of-line))))

  (defun eshell-quit-or-delete-char (arg)
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp))
        (progn
          (eshell-life-is-too-much)
          (ignore-errors
            (delete-window)))
      (delete-forward-char arg)))

  (defun eshell-send-previous-input (&optional arg)
    "Re-run the previous command (or with prefix args the nth command) in last used eshell buffer"
    (interactive "*p")
    (with-current-buffer
        (-first (lambda (b) (eq 'eshell-mode (with-current-buffer b major-mode)))
                (buffer-list))
      (with-selected-window (get-buffer-window)
        (end-of-buffer)
        (recenter 4))
      (eshell-previous-input arg)
      (eshell-send-input)))

  (defun eshell-send-current-line ()
    "Insert text of current line in eshell and execute."
    (interactive)
    (require 'eshell)
    (let ((command (buffer-substring
                    (save-excursion
                      (beginning-of-line)
                      (point))
                    (save-excursion
                      (end-of-line)
                      (point)))))
      (let ((buf (current-buffer)))
        (unless (get-buffer eshell-buffer-name)
          (eshell))
        (display-buffer eshell-buffer-name t)
        (switch-to-buffer-other-window eshell-buffer-name)
        (end-of-buffer)
        (eshell-kill-input)
        (insert command)
        (eshell-send-input)
        (end-of-buffer)
        (switch-to-buffer-other-window buf))))

  (defun source (filename)
    "Update environment variables from a shell source file."
    (interactive "fSource file: ")

    (message "Sourcing environment from `%s'..." filename)
    (with-temp-buffer

      (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))

      (let ((envvar-re "declare -x \\([^=]+\\)=\\(.*\\)$"))
        ;; Remove environment variables
        (while (search-forward-regexp (concat "^-" envvar-re) nil t)
          (let ((var (match-string 1)))
            (message "%s" (prin1-to-string `(setenv ,var nil)))
            (setenv var nil)))

        ;; Update environment variables
        (goto-char (point-min))
        (while (search-forward-regexp (concat "^+" envvar-re) nil t)
          (let ((var (match-string 1))
                (value (read (match-string 2))))
            (message "%s" (prin1-to-string `(setenv ,var ,value)))
            (setenv var value)))))
    (message "Sourcing environment from `%s'... done." filename))

  (defun eshell/exit-code (cmd &rest args)
    "Run PROGRAM with ARGS and return the exit code"
    (with-temp-buffer (apply 'call-process cmd nil (current-buffer) nil args)))

  (defun m-eshell-prompt-function ()
    (concat
     (when (not (eshell-exit-success-p))
       (propertize (concat " " (number-to-string eshell-last-command-status) " ")
                   'face `(:background "red")))
     (propertize (concat " " (replace-regexp-in-string (getenv "HOME") "~" (eshell/pwd)) " ")
                 'face `(:background "cyan" :foreground "black"))
     (propertize "\n() "
                 'face `(:foreground "white" :weight bold))))

  (defun eshell/s (hostname)
    "Change directory to host via tramp"
    (eshell/cd (concat "/sshx:" hostname ":")))

  (defun eshell/info (&optional subject)
    "Invoke `info', optionally opening the Info system to SUBJECT."
    (let ((buf (current-buffer)))
      (Info-directory)
      (if (not (null subject))
          (let ((node-exists (ignore-errors (Info-menu subject))))
            (if (not node-exists)
                (format "No menu item `%s' in node `(dir)Top'." subject))))))

  (defun eshell/init ()
    (source "~/.env")
    (setq eshell-path-env (getenv "PATH"))
    (setenv "TERM" "eterm-color")
    (setenv "EDITOR" "emacsclient")

    (global-key-binding (kbd "M-p") 'eshell-send-previous-input)
    ;; (add-hook sh-mode-hook
    ;;           (lambda () (define-key sh-mode-map (kbd "s-<ret>") 'eshell-send-current-line)))
    (define-key eshell-mode-map (kbd "C-a") 'eshell-maybe-bol)
    (define-key eshell-mode-map (kbd "C-d") 'eshell-quit-or-delete-char)
    (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
    (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history)
    (define-key eshell-mode-map (kbd "C-w") 'eshell/kill-previous-output)
    (define-key eshell-mode-map (kbd "M-w") 'eshell/copy-previous-output)
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
    (add-to-list 'eshell-visual-commands "ncdu")
    (add-to-list 'eshell-visual-commands "nvim")
    (add-to-list 'eshell-visual-commands "ssh")
    (add-to-list 'eshell-visual-commands "tail")
    (add-to-list 'eshell-visual-commands "top")
    (add-to-list 'eshell-visual-commands "vim")
    (eshell/alias "df" "df -h")
    (eshell/alias "whats-my-ip" "curl -s https://diagnostic.opendns.com/myip")
    (eshell/alias "dis" "drill +nocmd +noall +answer")
    (eshell/alias "show-hidden-files" "defaults write com.apple.finder AppleShowAllFiles -bool true; and killall Finder")
    (eshell/alias "hide-hidden-files" "defaults write com.apple.finder AppleShowAllFiles -bool false; and killall Finder")
    (eshell/alias "mergepdf" "/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py"))
  ;; TODO: These break if local has grc but tramp remote doesn't
  ;; (if (= 0 (eshell/exit-code "which" "grc"))
  ;;     (eshell/alias "colourify" "$(which grc) -es --colour=auto")
  ;;   (eshell/alias "configure" "colourify ./configure")
  ;;   (eshell/alias "diff" "colourify diff")
  ;;   (eshell/alias "make" "colourify make")
  ;;   (eshell/alias "gcc" "colourify gcc")
  ;;   (eshell/alias "g++" "colourify g++")
  ;;   (eshell/alias "as" "colourify as")
  ;;   (eshell/alias "gas" "colourify gas")
  ;;   (eshell/alias "ld" "colourify ld")
  ;;   (eshell/alias "netstat" "colourify netstat")
  ;;   (eshell/alias "ping" "colourify ping")
  ;;   (eshell/alias "traceroute" "colourify traceroute")
  ;;   (eshell/alias "tracepath" "colourify -c conf.traceroute tracepath")
  ;;   (eshell/alias "arp" "colourify -c conf.traceroute arp")
  ;;   (eshell/alias "tail" "colourify -c conf.log tail")
  ;;   (eshell/alias "ps" "colourify -c conf.ps ps")
  ;;   (eshell/alias "ifconfig" "colourify -c conf.traceroute ifconfig")
  ;;   (eshell/alias "nmap" "colourify -c conf.nmap nmap")
  ;;   (eshell/alias "lsof" "colourify -c conf.traceroute lsof")
  ;;   (eshell/alias "dig" "colourify -c conf.traceroute dig")
  ;;   (eshell/alias "host" "colourify -c conf.traceroute host")
  ;;   (eshell/alias "drill" "colourify -c conf.traceroute drill")
  ;;   (eshell/alias "curl" "colourify -c conf.curl curl")))

  (defun eshell/kill-previous-output (&optional nth)
    "Copies the output of the previous command to the kill ring.
    When nth is set, it will copy the nth previous command.
    Adapted from http://fasciism.com/2017/01/27/eshell-kill-previous-output/"
    (save-excursion
      ;; Move to the end of the eshell buffer.
      (goto-char (point-max))
      ;; Move to the start of the last prompt.
      (search-backward-regexp eshell-prompt-regexp nil nil nth)
      ;; Move to the start of the line, before the prompt.
      (beginning-of-line)
      ;; Remember this position as the end of the region.
      (let ((end (point)))
        ;; Move to the start of the last prompt.
        (search-backward-regexp eshell-prompt-regexp)
        ;; Move one line below the prompt, where the output begins.
        (next-line)
        ;; Find first line that's not blank.
        (while (looking-at "^[[:space:]]*$")
          (beginning-of-line)
          (next-line))
        (let ((count (count-words-region (point) end)))
          ;; Kill region
          (kill-region (point) end)
          ;; Output stats on what was copied as a sanity check.
          (format "Copied %s words to kill ring." count)))))

  (defun eshell/copy-previous-output (&optional nth)
    "Copies the output of the previous command to the kill ring.
When nth is set, it will copy the nth previous command.
    Stolen from http://fasciism.com/2017/01/27/eshell-kill-previous-output/"
    (save-excursion
      ;; Move to the end of the eshell buffer.
      (goto-char (point-max))
      ;; Move to the start of the last prompt.
      (search-backward-regexp eshell-prompt-regexp nil nil nth)
      ;; Move to the start of the line, before the prompt.
      (beginning-of-line)
      ;; Remember this position as the end of the region.
      (let ((end (point)))
        ;; Move to the start of the last prompt.
        (search-backward-regexp eshell-prompt-regexp)
        ;; Move one line below the prompt, where the output begins.
        (next-line)
        ;; Find first line that's not blank.
        (while (looking-at "^[[:space:]]*$")
          (beginning-of-line)
          (next-line))
        ;; Copy region to kill ring.
        (copy-region-as-kill (point) end)
        ;; Output stats on what was copied as a sanity check.
        (format "Copied %s words to kill ring." (count-words-region (point) end)))))

  :custom
  (eshell-buffer-shorthand t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-error-if-no-glob t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-prefer-lisp-functions t)
  (eshell-prefer-lisp-variables t)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-prompt-function 'm-eshell-prompt-function)
  (eshell-prompt-regexp "^() ")
  (eshell-highlight-prompt nil)
  :hook
  ((eshell-mode . eshell/init)
   (eshell-before-prompt . (lambda ()
                             (setq xterm-color-preserve-properties t))))
  ;;  (sh-mode . (lambda ()
  ;;               (define-key sh-mode-map (kbd "s-<ret>") 'eshell-send-current-line))))
  :bind
  (("s-e" . eshell)
   ("s-E" . eshell-here)))

(use-package esh-autosuggest
  :hook ((eshell-mode . esh-autosuggest-mode))
  :bind (:map esh-autosuggest-active-map
              ("C-e" . company-complete-selection)))

(use-package htmlize
  :bind
  (("C-c h b" . htmlize-buffer)
   ("C-c h f" . htmlize-file)
   ("C-c h m" . htmlize-many-files)
   :map dired-mode-map
   ("C-c h m" . htmlize-many-files-dired)))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker
  :defer 2
  :config
  (docker-global-mode))

(use-package docker-tramp
  :defer 2)

(use-package bash-completion
  :defer 2
  :config
  (bash-completion-setup))

(use-package fish-mode
  :custom (fish-indent-offset tab-width)
  :mode "\\.fish\\'")

(use-package fish-completion
  :defer 2
  :custom (fish-completion-fallback-on-bash-p t)
  :config
  (when (and (executable-find "fish")
             (require 'fish-completion nil t)))
  (global-fish-completion-mode))

(use-package nginx-mode
  :defer 2
  :config
  (setq nginx-indent-level tab-width))

(use-package yaml-mode
  :mode "\\.ya\?ml\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package web-mode
  :mode "\\.html\?\\'"
  :init
  ;; from web-mode FAQ to work with smartparens
  (defun m-web-mode-hook ()
    (setq web-mode-enable-auto-pairing nil))
  (defun sp-web-mode-is-code-context (id action context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))  :mode
  ("\\.phtml\\'"
   "\\.tpl\\.php\\'"
   "\\.[agj]sp\\'"
   "\\.as[cp]x\\'"
   "\\.erb\\'"
   "\\.mustache\\'"
   "\\.djhtml\\'"
   "\\.html?\\'")
  :custom
  (web-mode-markup-indent-offset tab-width
                                 web-mode-css-indent-offset tab-width
                                 web-mode-code-indent-offset tab-width
                                 web-mode-enable-current-element-highlight t
                                 web-mode-enable-current-column-highlight t
                                 web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                                                             ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  :hook
  (web-mode . m-web-mode-hook))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :custom
  (js2-basic-offset tab-width))

(use-package rjsx-mode
  :mode "\\.jsx\\'")

;; (use-package indium
;;   :defer 1)

;; (use-package nodejs-repl
;;   :bind
;;   (:map
;;    js2-mode-map
;;    ("C-x C-e" . nodejs-repl-send-last-expression)
;;    ("C-c C-j" . nodejs-repl-send-line)
;;    ("C-c C-r" . nodejs-repl-send-region)
;;    ("C-c C-l" . nodejs-repl-load-file)
;;    ("C-c C-z" . nodejs-repl-switch-to-repl)))

(use-package know-your-http-well
  :defer 2)

(use-package restclient
  :defer 2)

(use-package company-restclient
  :defer 2
  :config
  (add-to-list 'company-backends 'company-restclient))

;; (use-package robe
;;   :hook ruby-mode
;;   :config
;;   (eval-after-load 'company
;;     '(push 'company-robe company-backends)))

;; (use-package inf-ruby
;;   :defer 1)

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package sass-mode
  :mode "\\.sa?c?ss\\'")

(use-package flycheck
  :bind
  (("C-c ! !" . flycheck-mode)))

(use-package powershell-mode
  :mode "\\.ps1\\'"
  :custom
  (powershell-indent tab-width)
  (powershell-continuation-indent tab-width))

(use-package ios-config-mode
  :mode "\\.cfg\\'")

;; `eval-in-repl' requires a number of other packages so it's best to load it last
(use-package eval-in-repl
  :bind
  (:map
   emacs-lisp-mode-map
   ("C-<return>" . eir-eval-in-ielm)
   :map
   lisp-interaction-mode-map
   ("C-<return>" . eir-eval-in-ielm)
   :map
   Info-mode-map
   ("s-<return>" . eir-eval-in-ielm)
   :map
   python-mode-map
   ("s-<return>" . eir-eval-in-python)
   :map
   ruby-mode-map
   ("s-<return>" . eir-eval-in-ruby)
   :map
   sh-mode-map
   ("s-<return>" . eir-eval-in-shell)))


(provide 'm-packages)
;;; m-packages.el ends here
