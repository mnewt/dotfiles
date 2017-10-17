(defun recompile-emacsd ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

(defun update ()
  (interactive)
  (straight-pull-all)
  (spaceline-compile))
