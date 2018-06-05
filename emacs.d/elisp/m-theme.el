;; -*- lexical-binding: t -*-

;; Base faces for modeline
(defface m-inactive0 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 0."
  :group 'powerline)
(defface m-active0 '((t (:inherit default)))
  "Powerline active face 0."
  :group 'powerline)
(defface m-inactive1 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 1."
  :group 'powerline)
(defface m-active1 '((t (:inherit default)))
  "Powerline active face 1."
  :group 'powerline)
(defface m-inactive2 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 2."
  :group 'powerline)
(defface m-active2 '((t (:inherit default)))
  "Powerline active face 2."
  :group 'powerline)
(defface m-inactive3 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 3."
  :group 'powerline)
(defface m-active3 '((t (:inherit default)))
  "Powerline active face 3."
  :group 'powerline)
(defface m-inactive4 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 4."
  :group 'powerline)
(defface m-active4 '((t (:inherit default)))
  "Powerline active face 4."
  :group 'powerline)

(setq-default m-themes '())

(defun activate-theme-common ()
  (set-face-attribute 'mode-line nil
                      :box nil
                      :overline nil
                      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :overline nil
                      :underline nil))

(defun activate-theme (x)
  "Disable current themes and load theme X."
  (let ((theme (if (stringp x) (intern x) x)))
    (condition-case nil
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme theme t)
          (activate-theme-common)
          (funcall (cdr (assoc theme m-themes)))
          (when (fboundp 'powerline-reset) (powerline-reset)))
      (error "Problem loading theme %s" x))))

(defun choose-theme ()
  "Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'."
  (interactive)
  (ivy-read "Load custom theme: "
            (mapcar 'car m-themes)
            :action #'activate-theme
            :caller 'choose-theme))

(use-package dracula-theme
  :load-path "straight/build/dracula-theme"
  :config
  (defun activate-theme-dracula ()
    (setq face-remapping-alist
          '((m-inactive0 :background "#262834" :foreground "#565861")
            (m-active0 :background "#565861" :foreground "#E6E7E8")
            (m-inactive1 :background "#262834" :foreground "#565861")
            (m-active1 :background "#565861" :foreground "#E6E7E8")
            (m-inactive2 :background "#262834" :foreground "#565861")
            (m-active2 :background "#CECFD2" :foreground "#565861")
            (m-inactive3 :background "#565861" :foreground "#9E9FA5")
            (m-active3 :background "#A863C9" :foreground "#FFFFFF")
            (m-inactive4 :background "#565861" :foreground "#9E9FA5")
            (m-active4 :background "#00e5e5" :foreground "#262834")))
    (set-cursor-color "#F60")
    (set-mouse-color "white")
    (set-background-color "#2A2A2A"))
  (add-to-list 'm-themes '(dracula . activate-theme-dracula))
  (activate-theme 'dracula))

;; (use-package monokai-theme
;;   :load-path "straight/build/monokai-theme"
;;   :config
;;   (defun activate-theme-monokai ()
;;     (setq face-remapping-alist
;;           '((m-inactive0 :background "#262834" :foreground "#565861")
;;             (m-active0 :background "#565861" :foreground "#E6E7E8")
;;             (m-inactive1 :background "#262834" :foreground "#565861")
;;             (m-active1 :background "#565861" :foreground "#E6E7E8")
;;             (m-inactive2 :background "#262834" :foreground "#565861")
;;             (m-active2 :background "#CECFD2" :foreground "#565861")
;;             (m-inactive3 :background "#565861" :foreground "#9E9FA5")
;;             (m-active3 :background "#A863C9" :foreground "#FFFFFF")
;;             (m-inactive4 :background "#565861" :foreground "#9E9FA5")
;;             (m-active4 :background "#00e5e5" :foreground "#262834")))
;;     (setq monokai-user-variable-pitch t)
;;     (set-cursor-color "#F60"))
;;   (add-to-list 'm-themes '("monokai" . activate-theme-monokai)))

(use-package solarized-theme
  :load-path "straight/build/solarized-theme"
  :config
  (defun activate-theme-solarized-light ()
    (setq face-remapping-alist
          '((m-inactive0 :background "#EDE8D7" :foreground "#EDE8D7")
            (m-active0 :background "#565861" :foreground "#E6E7E8")
            (m-inactive1 :background "#EDE8D7" :foreground "#EDE8D7")
            (m-active1 :background "#565861" :foreground "#EDE8D7")
            (m-inactive2 :background "#EDE8D7" :foreground "#EDE8D7")
            (m-active2 :background "#CECFD2" :foreground "#565861")
            (m-inactive3 :background "#565861" :foreground "#9E9FA5")
            (m-active3 :background "#A863C9" :foreground "#FFFFFF")
            (m-inactive4 :background "#565861" :foreground "#9E9FA5")
            (m-active4 :background "#00e5e5" :foreground "#262834")))
    (set-mouse-color "black"))
  (add-to-list 'm-themes '(solarized-light . activate-theme-solarized-light)))

;; (use-package doom-themes
;;   ;; :load-path "straight/build/emacs-one-themes"
;;   :config
;;   (defun activate-doom-one ()
;;     (setq doom-themes-enable-bold t
;;           doom-themes-enable-italic t
;;           face-remapping-alist
;;           '((m-inactive0 :background "#262834" :foreground "#565861")
;;             (m-active0 :background "#565861" :foreground "#E6E7E8")
;;             (m-inactive1 :background "#262834" :foreground "#565861")
;;             (m-active1 :background "#565861" :foreground "#E6E7E8")
;;             (m-inactive2 :background "#262834" :foreground "#565861")
;;             (m-active2 :background "#CECFD2" :foreground "#565861")
;;             (m-inactive3 :background "#565861" :foreground "#9E9FA5")
;;             (m-active3 :background "#A863C9" :foreground "#FFFFFF")
;;             (m-inactive4 :background "#565861" :foreground "#9E9FA5")
;;             (m-active4 :background "#00e5e5" :foreground "#262834")))
;;     ;; (set-cursor-color "#F60")
;;     (set-mouse-color "white")
;;     (doom-themes-visual-bell-config))
;;   ;; (set-background-color "#2A2A2A"))
;;   (add-to-list 'm-themes '(doom-one . activate-doom-one))
;;   (add-to-list 'm-themes '(doom-one-light . activate-doom-one))
;;   (add-to-list 'm-themes '(doom-city-lights . activate-doom-one))
;;   (add-to-list 'm-themes '(doom-one . activate-doom-one)))
               
(use-package powerline
  :init
  (progn
    (setq powerline-default-separator nil)
    (set-face-attribute 'mode-line nil
                        :box nil))
  :config
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'm-active0 'm-inactive0))
                          (face1 (if active 'm-active1 'm-inactive1))
                          (face2 (if active 'm-active2 'm-inactive2))
                          (face3 (if active 'm-active3 'm-inactive3))
                          (face4 (if active 'm-active4 'm-inactive4))
                          (center (list (powerline-raw "%*" face1 'l)
                                        ;; (powerline-buffer-id `(mode-line-buffer-id ,face3) 'm)
                                        (powerline-raw (if (file-remote-p default-directory)
                                                           (concat " "
                                                                   (tramp-file-name-host (tramp-dissect-file-name default-directory))
                                                                   " ")
                                                         "")
                                                       face4)
                                        (powerline-raw " " face3)
                                        (powerline-raw (buffer-name) face3 'm)
                                        (powerline-raw " " face3)
                                        (powerline-raw "%*" face1 'r)))
                          (rhs (if active
                                   (list (powerline-raw global-mode-string face0 'r)
                                         (powerline-raw " " face0)
                                         (powerline-raw "%l" face0 'r)
                                         (powerline-raw ":" face0)
                                         (powerline-raw "%c" face0 'r)
                                         (powerline-hud face3 face0))))
                          (lhs (list (powerline-raw " " face1)
                                     (powerline-major-mode face2 'l)
                                     (powerline-raw " " face2)
                                     (powerline-vc face0 'r)
                                     (powerline-raw " " face1)
                                     (powerline-raw (if (eq major-mode 'term-mode)
                                                        (cond
                                                         ((term-in-char-mode) " (char-mode)")
                                                         ((term-in-line-mode) " (line-mode)")
                                                         (t ""))
                                                      "")
                                                    face1)
                                     (powerline-raw " " face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))


(provide 'm-theme)
