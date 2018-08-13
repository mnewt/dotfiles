;;; highlight-things.el --- Highlight interesting things -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: 0.0.1
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: font-lock


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Ideas to filter Eshell output:
;; 1. Eshell feeds each line of program output to
;; `eshell-preoutput-filter-functions'. This is easy but there is no obvious way
;; to handle multi-line semantics.
;; 2. Highlight region from `(eshell-beginning-of-output)' to
;; `(eshell-end-of-output)'. See https://emacs.stackexchange.com/a/5408.
;; 3. Maybe use `org-src-font-lock-fontify-block'.

;; The downsides to #2 and #3 are that the output is not highlighted until after
;; the command finishes.

;; See https://www.emacswiki.org/emacs?SampleMode for mode boilerplate. Note
;; that we are purposefully not using a derived mode because this is not an
;; interactive mode and we don't want to do useless, user-oriented stuff on a
;; temporary buffer. We are going for the most minimal mode possible and it is
;; only intended to be used programmatically so we will eschew the normal
;; boilerplate and mode features like hooks and maps.

;; * TODO: Highlight Eshell command line while it's being typed
;; * TODO: Add minor mode that can replace `hl-todo'
;; * TODO: Finish `hlt-fontify-region' 
;; * TODO: Builtin Eshell commands
;; * TODO: Emacs Lisp functions
;; * TODO: Executables found in $PATH (`hlt-keyword-executable')

;;; Code:

(defcustom hlt-face-property-type 'face
  "The property type to use when adding color properties to a string. Normally, you would want this to be 'face, but `font-lock' can overwrite that. If that is happening, try setting this to 'font-lock-face to trick `font-lock' into leaving the coloring alone."
  :type '(symbol)
  :group 'hlt
  :options '(face font-lock-face))

(defcustom hlt-keyword-executable
  `(,(concat "\\<"
             (regexp-opt '("update" "update-arch" "update-atom" "update-clojure") t)
             "\\>")
    font-lock-keyword-face)
  "Executables found in $PATH."
  :type '(list)
  :group 'hlt)

(defcustom hlt-keyword-ipv4-address
  '("\\b\\(?:\\(?:25[0-5]\\|2[0-4][0-9]\\|[01]?[0-9][0-9]?\\)\\.\\)\\{3\\}\\(?:25[0-5]\\|2[0-4][0-9]\\|[01]?[0-9][0-9]?\\)\\b" font-lock-string-face)
  "IPv4 address."
  :type '(list)
  :group 'hlt)

(defcustom hlt-keyword-ipv6-address
  '("[0-9a-f]*\\(?:::?[0-9a-f]+\\)" font-lock-string-face)
  "IPv6 address."
  :type '(list)
  :group 'hlt)

(defcustom hlt-keyword-human-readable-number
  '("\\<[0-9.]+[kKmMgGtT]\\>" font-lock-builtin-face)
  "Human readable number, like in `ls -h'."
  :type '(list)
  :group 'hlt)

(defcustom hlt-keywords-common
  (list hlt-keyword-ipv4-address
        hlt-keyword-ipv6-address)
  ""
  :type '(list)
  :group 'hlt)

(defcustom hlt-keywords-default '()
  "Keywords that are used when the current command does not have
  a definition in `hlt-keywords-commands'"
  :type '(list)
  :group 'hlt)

(defcustom hlt-keywords-commands
  `(("df" ((,(regexp-opt '("Filesystem" "Size" "Used" "Avail" "Use%" "Mounted on") t)
            font-lock-keyword-face)
           ,hlt-keyword-human-readable-number)))
  "Alist. Keyws are strings representing the currently running
  command. Values are lists of regexp/font-face pairs in standard
  font-lock format."
  :type '(list)
  :group 'hlt)

(defun hlt-mode ()
  "Major mode for highlighting things. It is not intended to be
  used interactively, only programmatically."
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(hlt-font-lock-keywords))
  (setq major-mode 'hlt-mode))

(defun hlt-fontify-string (string &optional keywords)
  "Use `hlt-font-lock-keywords' to fontify the STRING, sort of
like `font-lock-fontify-keywords-region' would, only simpler."
  (dolist (pair (or keywords hlt-font-lock-keywords))
    (let ((re (car pair))
          (face (cdr pair))
          (start 0))
      (while (string-match re string start)
        (add-text-properties (match-beginning 0) (match-end 0)
                             `(,hlt-face-property-type ,face) string)
        (setq start (match-end 0)))))
  string)

(defun hlt-fontify-region (start end &optional keywords)
  "Like `font-lock-fontify-keywords-region' only can be run
without an activated mode."
  ;; Blatantly lie to `font-lock', telling it things have already been set up.
  (let ((font-lock-set-defaults t)
        (font-lock-keywords (or keywords hlt-font-lock-keywords)))
    (font-lock-fontify-keywords-region start end)))

(defun hlt-fontify-string-for-command (command string)
  "Like `hlt-fontify-string' but does special stuff for the specified COMMAND."
  (hlt-fontify-string (alist-get command hlt-keywords-commands nil nil 'string=)))

(defun hlt-eshell-preoutput-filter (string)
  "Add this to `eshell-preoutput-filter-functions' to highlight Eshell output."
  (hlt-fontify-string string
                      (append
                       hlt-keywords-common
                       (or
                        (cadr (assoc eshell-last-command-name hlt-keywords-commands))
                        hlt-keywords-default))))

;; (defun hlt-fontify-text (text)
;;   (with-temp-buffer
;;     (erase-buffer)
;;     (insert text)
;;     ;; TODO: Maybe replace `font-lock.el' machinery with our own, for speed.
;;     (setq major-mode 'hlt-mode)
;;     (set (make-local-variable 'font-lock-defaults) '(hlt-font-lock-keywords))
;;     (font-lock-default-function 'hlt-mode)
;;     ;; I can't think of any use cases for syntactic fontification so we skip it.
;;     (font-lock-fontify-keywords-region (point-min) (point-max) nil)
;;     (if hlt-use-font-lock-face
;;         (hlt-replace-face-with-font-lock-face (buffer-string))
;;       (buffer-string))))

;; (defun hlt-replace-face-with-font-lock-face (text)
;;   "Replace property 'face with 'font-lock-face so that font-lock
;; functions don't strip it."
;;   (let ((pos 0))
;;     (while (setq next (next-single-property-change pos 'face text))
;;       (put-text-property pos next 'font-lock-face (get-text-property pos 'face text) text)
;;       (setq pos next))
;;     (add-text-properties 0 (length text) '(fontified t) text)
;;     text))

(provide 'hlt-mode)

(add-hook 'eshell-preoutput-filter-functions 'hlt-eshell-preoutput-filter)
;; (remove-hook 'eshell-preoutput-filter-functions 'hlt-eshell-preoutput-filter)

(provide 'highlight-things)

;;; highlight-things.el ends here
