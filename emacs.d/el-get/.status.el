((aggressive-indent status "required" recipe
                    (:name aggressive-indent :after
                           (global-aggressive-indent-mode 1)
                           :auto-generated t :type elpa :description "Minor mode to aggressively keep your code always indented" :repo
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           :depends
                           (cl-lib)
                           :minimum-emacs-version
                           (24 1)))
 (auto-complete status "installed" recipe
                (:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
                       (popup fuzzy)
                       :features auto-complete-config :post-init
                       (progn
                         (add-to-list 'ac-dictionary-directories
                                      (expand-file-name "dict" default-directory))
                         (ac-config-default))))
 (better-defaults status "installed" recipe
                  (:name better-defaults :type github :pkgname "technomancy/better-defaults" :description "Fixing weird quirks and poor defaults." :checkout "0.1.3"))
 (buffer-move status "installed" recipe
              (:name buffer-move :after
                     (progn
                       (global-set-key
                        (kbd "<C-S-up>")
                        'buf-move-up)
                       (global-set-key
                        (kbd "<C-S-down>")
                        'buf-move-down)
                       (global-set-key
                        (kbd "<C-S-left>")
                        'buf-move-left)
                       (global-set-key
                        (kbd "<C-S-right>")
                        'buf-move-right))
                     :description "Swap buffers without typing C-x b on each window" :website "https://github.com/lukhas/buffer-move" :type github :pkgname "lukhas/buffer-move"))
 (cider status "installed" recipe
        (:name cider :description "CIDER is a Clojure IDE and REPL." :type github :pkgname "clojure-emacs/cider" :depends
               (seq queue clojure-mode pkg-info spinner)))
 (cl-lib status "installed" recipe
         (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (clj-refactor status "installed" recipe
               (:name clj-refactor :after
                      (progn
                        (defun my-clojure-mode-hook nil
                          (clj-refactor-mode 1)
                          (yas-minor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c C-m"))
                        (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))
                      :description "A collection of simple clojure refactoring functions" :type github :depends
                      (dash s clojure-mode yasnippet paredit multiple-cursors cider edn inflections hydra)
                      :pkgname "magnars/clj-refactor.el"))
 (clojure-mode status "installed" recipe
               (:name clojure-mode :after
                      (progn
                        (require 'clojure-mode-extra-font-locking))
                      :website "https://github.com/clojure-emacs/clojure-mode" :description "Emacs support for the Clojure language." :type github :pkgname "clojure-emacs/clojure-mode"))
 (dash status "installed" recipe
       (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (edn status "installed" recipe
      (:name edn :description "Edn.el is an emacs lisp library for reading and writing the data format edn." :type github :depends
             (dash cl-lib s peg)
             :pkgname "expez/edn.el"))
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :compile
                ("el-get.*\\.el$" "methods/")
                :features el-get :post-init
                (when
                    (memq 'el-get
                          (bound-and-true-p package-activated-list))
                  (message "Deleting melpa bootstrap el-get")
                  (unless package--initialized
                    (package-initialize t))
                  (when
                      (package-installed-p 'el-get)
                    (let
                        ((feats
                          (delete-dups
                           (el-get-package-features
                            (el-get-elpa-package-directory 'el-get)))))
                      (el-get-elpa-delete-package 'el-get)
                      (dolist
                          (feat feats)
                        (unload-feature feat t))))
                  (require 'el-get))))
 (emacs-async status "installed" recipe
              (:name emacs-async :description "Simple library for asynchronous processing in Emacs" :type github :pkgname "jwiegley/emacs-async"))
 (epl status "installed" recipe
      (:name epl :description "EPL provides a convenient high-level API for various package.el versions, and aims to overcome its most striking idiocies." :type github :pkgname "cask/epl"))
 (escreen status "installed" recipe
          (:name escreen :description "Emacs window session manager" :type http :url "http://www.splode.com/~friedman/software/emacs-lisp/src/escreen.el" :prepare
                 (autoload 'escreen-install "escreen" nil t)))
 (find-file-in-project status "installed" recipe
                       (:name find-file-in-project :after
                              (setq ffip-prefer-ido-mode t)
                              :type github :pkgname "technomancy/find-file-in-project" :description "Quick access to project files in Emacs"))
 (fuzzy status "installed" recipe
        (:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (goto-last-change status "installed" recipe
                   (:name goto-last-change :after
                          (progn
                            (global-set-key
                             (kbd "C-x C-/")
                             'goto-last-change))
                          :description "Move point through buffer-undo-list positions" :type emacswiki :load "goto-last-change.el"))
 (hydra status "installed" recipe
        (:name hydra :description "make Emacs bindings that stick around" :type github :depends
               (cl-lib)
               :pkgname "abo-abo/hydra"))
 (inflections status "installed" recipe
              (:name inflections :description "Convert english words between singular and plural" :type elpa))
 (magit status "installed" recipe
        (:name magit :after
               (progn
                 (global-set-key
                  (kbd "C-x C-z")
                  'magit-status))
               :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :branch "master" :minimum-emacs-version "24.4" :depends
               (dash with-editor emacs-async)
               :info "Documentation" :load-path "lisp/" :compile "lisp/" :build
               `(("make" ,(format "EMACSBIN=%s" el-get-emacs)
                  "docs")
                 ("touch" "lisp/magit-autoloads.el"))
               :build/berkeley-unix
               `(("gmake" ,(format "EMACSBIN=%s" el-get-emacs)
                  "docs")
                 ("touch" "lisp/magit-autoloads.el"))
               :build/windows-nt
               (with-temp-file "lisp/magit-autoloads.el" nil)))
 (moe-theme status "installed" recipe
            (:name moe-theme :after
                   (progn
                     (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/moe-theme/")
                     (add-to-list 'load-path "~/.emacs.d/el-get/moe-theme/")
                     (require 'moe-theme)
                     (show-paren-mode t)
                     (setq show-paren-style 'expression)
                     (setq moe-theme-highlight-buffer-id t)
                     (moe-dark))
                   :description "A customizable colorful eye-candy theme for Emacser. Moe, moe, kyun!" :website "https://github.com/kuanyui/moe-theme.el" :type github :pkgname "kuanyui/moe-theme.el" :prepare
                   (add-to-list 'custom-theme-load-path default-directory)))
 (multiple-cursors status "installed" recipe
                   (:name multiple-cursors :description "An experiment in adding multiple cursors to emacs" :type github :pkgname "magnars/multiple-cursors.el"))
 (package status "installed" recipe
          (:name package :description "ELPA implementation (\"package.el\") from Emacs 24" :builtin "24" :type http :url "https://repo.or.cz/w/emacs.git/blob_plain/ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09:/lisp/emacs-lisp/package.el" :features package :post-init
                 (progn
                   (let
                       ((old-package-user-dir
                         (expand-file-name
                          (convert-standard-filename
                           (concat
                            (file-name-as-directory default-directory)
                            "elpa")))))
                     (when
                         (file-directory-p old-package-user-dir)
                       (add-to-list 'package-directory-list old-package-user-dir)))
                   (setq package-archives
                         (bound-and-true-p package-archives))
                   (let
                       ((protocol
                         (if
                             (and
                              (fboundp 'gnutls-available-p)
                              (gnutls-available-p))
                             "https://"
                           (lwarn
                            '(el-get tls)
                            :warning "Your Emacs doesn't support HTTPS (TLS)%s"
                            (if
                                (eq system-type 'windows-nt)
                                ",\n  see https://github.com/dimitri/el-get/wiki/Installation-on-Windows." "."))
                           "http://"))
                        (archives
                         '(("melpa" . "melpa.org/packages/")
                           ("gnu" . "elpa.gnu.org/packages/")
                           ("marmalade" . "marmalade-repo.org/packages/"))))
                     (dolist
                         (archive archives)
                       (add-to-list 'package-archives
                                    (cons
                                     (car archive)
                                     (concat protocol
                                             (cdr archive)))))))))
 (paredit status "installed" recipe
          (:name paredit :description "Minor mode for editing parentheses" :type github :prepare
                 (progn
                   (autoload 'enable-paredit-mode "paredit")
                   (autoload 'disable-paredit-mode "paredit"))
                 :pkgname "emacsmirror/paredit"))
 (parinfer-mode status "installed" recipe
                (:name parinfer-mode :type github :pkgname "edpaget/parinfer-mode" :after
                       (progn
                         (el-get-bundle-load-init "/Users/mnewton/.emacs.d/el-get/bundle-init/_Users_mnewton_.emacs.d_init-1_parinfer-mode.el"))))
 (peg status "installed" recipe
      (:name peg :type emacswiki :description "Parsing Expression Grammars in Emacs Lisp" :website "http://www.emacswiki.org/emacs/download/peg.el"))
 (pkg-info status "installed" recipe
           (:name pkg-info :description "Provide information about Emacs packages." :type github :pkgname "lunaryorn/pkg-info.el" :depends
                  (dash epl)))
 (popup status "installed" recipe
        (:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :submodule nil :depends cl-lib :pkgname "auto-complete/popup-el"))
 (queue status "installed" recipe
        (:name queue :description "Queue data structure" :type elpa))
 (s status "installed" recipe
    (:name s :description "The long lost Emacs string manipulation library." :type github :pkgname "magnars/s.el"))
 (seq status "installed" recipe
      (:name seq :description "Sequence manipulation library for Emacs" :builtin "25" :type github :pkgname "NicolasPetton/seq.el"))
 (smartparens status "installed" recipe
              (:name smartparens :after
                     (require 'smartparens-config)
                     :description "Autoinsert pairs of defined brackets and wrap regions" :type github :pkgname "Fuco1/smartparens" :depends dash))
 (smex status "installed" recipe
       (:name smex :after
              (progn
                (setq smex-save-file "~/.emacs.d/.smex-items")
                (global-set-key
                 (kbd "M-x")
                 'smex)
                (global-set-key
                 (kbd "M-X")
                 'smex-major-mode-commands))
              :description "M-x interface with Ido-style fuzzy matching." :type github :pkgname "nonsequitur/smex" :features smex :post-init
              (smex-initialize)))
 (smooth-scrolling status "installed" recipe
                   (:name smooth-scrolling :after
                          (progn
                            (smooth-scrolling-mode 1)
                            (setq smooth-scroll-margin 5))
                          :description "Make emacs scroll smoothly, keeping the point away from the top and bottom of the current buffer's window in order to keep lines of context around the point visible as much as possible, whilst avoiding sudden scroll jumps which are visually confusing." :type github :pkgname "aspiers/smooth-scrolling" :features smooth-scrolling))
 (spinner status "installed" recipe
          (:name spinner :description "Emacs mode-line spinner for operations in progress." :type github :pkgname "Bruce-Connor/spinner.el"))
 (switch-window status "installed" recipe
                (:name switch-window :description "A *visual* way to choose a window to switch to" :type github :pkgname "dimitri/switch-window" :features switch-window))
 (with-editor status "installed" recipe
              (:name with-editor :description "Use the Emacsclient as $EDITOR" :type github :pkgname "magit/with-editor"))
 (yasnippet status "installed" recipe
            (:name yasnippet :website "https://github.com/capitaomorte/yasnippet.git" :description "YASnippet is a template system for Emacs." :type github :pkgname "capitaomorte/yasnippet" :compile "yasnippet.el" :submodule nil :build
                   (("git" "submodule" "update" "--init" "--" "snippets"))))
 (zencoding-mode status "installed" recipe
                 (:name zencoding-mode :description "Unfold CSS-selector-like expressions to markup" :type github :pkgname "rooney/zencoding" :prepare
                        (defun zencoding-mode-on nil "Tunr on `zencoding-mode' mode."
                               (interactive)
                               (zencoding-mode 1)))))
