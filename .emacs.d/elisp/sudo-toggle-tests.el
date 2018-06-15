;; -*- lexical-binding: t -*-

(tramp-make-tramp-file-name "sshx" "root" nil "myhost" nil "/my/path")

(toggle-sudo--add-sudo "~/.emacs.d/init.el")

(toggle-sudo--add-sudo "/sshx:mc:")

(toggle-sudo--add-sudo "/ssh:matt@mc:/data/code/")


(toggle-sudo--remove-sudo "/sudo:root@localhost:/Users/mn/.emacs.d/init.el")

(toggle-sudo--remove-sudo "/sshx:mc|sudo:root@mc:/home/matt/")

(toggle-sudo--remove-sudo "/ssh:matt@mc|sudo:root@mc:/data/code/")

