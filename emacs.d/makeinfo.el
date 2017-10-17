(defvar-local sly-recipe-melpa
  '(sly
     :repo "capitaomorte/sly"
     :fetcher github
     :files ("*.el"
             ("lib" "lib/*")
             ("slynk/backend" "slynk/backend/*") 
             ("slynk" "slynk/*")
             ("contrib" "contrib/*")
             "doc/*.texi"
             "doc/*.info"
             "doc/dir")))

(defvar-local sly-recipe
  (straight--convert-recipe sly-recipe-melpa))

(defun straight--symlink-package (recipe)
  "Symlink the package for the given RECIPE into the build directory.
This deletes any existing files in the relevant subdirectory of
the build directory, creating a pristine set of symlinks."
  (straight--with-plist recipe
      (package local-repo files)
    ;; Remove the existing built package, if necessary.
    (let ((dir (straight--dir "build" package)))
      (when (file-exists-p dir)
        (delete-directory dir 'recursive)))
    ;; Make a new directory for the built package.
    (make-directory (straight--dir "build" package) 'parents)
    ;; Do the linking.
    (dolist (spec (straight-expand-files-directive
                   files
                   (straight--dir "repos" local-repo)
                   (straight--dir "build" package)))
      (cl-destructuring-bind (repo-file . build-file) spec
        (make-directory (file-name-directory build-file) 'parents)
        (make-symbolic-link repo-file build-file)))))

(defun straight--makeinfo-package (recipe)
  "Generate Info manuals and the Info index for the given RECIPE into the build
directory. This overwrites any existing files in the build directory which have
the same name."
  (straight--with-plist recipe
      (package local-repo files)
    (print recipe)))

      


(defun borg-makeinfo (clone)
  "Generate Info manuals and the Info index for the clone named CLONE."
  (dolist (default-directory (borg-info-path clone t))
    (let ((exclude (borg-get-all clone "no-makeinfo")))
      (dolist (texi (directory-files default-directory nil "\\.texi\\'"))
        (let ((info (concat (file-name-sans-extension texi) ".info")))
          (when (and (not (member texi exclude))
                     (or (not (file-exists-p info))
                         (= (process-file "git" nil nil nil
                                          "ls-files" "--error-unmatch" info)
                            1)))
            (let ((cmd (format "makeinfo --no-split %s -o %s" texi info)))
              (message "  Running '%s'..." cmd)
              (borg-silencio "\\`(Shell command succeeded with %s)\\'"
                (shell-command cmd))
              (message "  Running '%s'...done" cmd))))))
    (dolist (info (directory-files default-directory nil "\\.info\\'"))
      (let ((cmd (format "install-info %s --dir=dir" info)))
        (message "  Running '%s'..." cmd)
        (borg-silencio "\\`(Shell command succeeded with %s)\\'"
          (shell-command cmd))))))
(message "  Running '%s'...done" cmd)
