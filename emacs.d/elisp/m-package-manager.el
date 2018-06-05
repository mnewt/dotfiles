;; -*- lexical-binding: t -*-

;; Bootstrap straight.el
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
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

;; https://github.com/raxod502/straight.el/issues/41
(setq straight-check-for-modifications 'live)

;; Try to get package.el to work better with straight.el
;; https://github.com/raxod502/straight.el/issues/128
(defun straight--advice-package-installed-p (original-function &rest args)
  "Return t if package is installed via `straight' package manager."
  (or (gethash (symbol-name (car args)) straight--recipe-cache)
      (apply original-function args)))

(advice-add 'package-installed-p :around 'straight--advice-package-installed-p)

;; suggested requires for `use-package'
(eval-when-compile
  (require 'use-package))
;; (require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Properly install seq package
;; https://github.com/raxod502/straight.el/issues/170
(straight-register-package
 '(seq :repo "https://git.savannah.gnu.org/git/emacs/elpa.git" :files ("packages/seq/*.el")))

(use-package seq
  :config
  (require 'seq-25))

(use-package epkg
  :defer 1)

(provide 'm-package-manager)
